-- See bzip2.ads for legal stuff
--
-- Documentation pointers:
--
--   Burrows-Wheeler transform
--     http://en.wikipedia.org/wiki/Burrows%E2%80%93Wheeler_transform
--   MTF Move-To-Front
--     http://fr.wikipedia.org/wiki/Move-To-Front
--
-- Translated on 20-Oct-2009 by (New) P2Ada v. 15-Nov-2006
--
--with Ada.Text_IO;                       use Ada.Text_IO;

with Ada.Unchecked_Deallocation;

package body BZip2 is

  procedure Decompress is

    max_groups    : constant:= 6;
    max_alpha_size: constant:= 258;
    max_code_len  : constant:= 23;
    group_size    : constant:= 50;
    max_selectors : constant:= 2 + (900_000 / group_size);

    sub_block_size: constant:= 100_000;

    type Length_array is array (Integer range <>) of Natural;

    block_randomized: Boolean:= False;
    block_size: Natural;

    use Interfaces;

    type Tcardinal_array is array (Integer range <>) of Unsigned_32;
    type Pcardinal_array is access Tcardinal_array;
    procedure Dispose is new Ada.Unchecked_Deallocation(Tcardinal_array, Pcardinal_array);
    tt: Pcardinal_array;
    tt_count: Natural;

    rle_run_left: Natural:= 0;
    rle_run_data: Unsigned_8:= 0;
    decode_available: Natural:= Natural'Last;
    block_origin: Natural:= 0;
    read_data: Unsigned_8:= 0;
    bits_available: Natural:= 0;
    inuse_count: Natural;
    seq_to_unseq: array (0 .. 255 ) of Natural;
    alpha_size: Natural;
    group_count: Natural;
    --
    selector_count: Natural;
    selector, selector_mtf: array (0 .. max_selectors) of Unsigned_8;
    --
    type Alpha_U32_array is array (0 .. max_alpha_size) of Unsigned_32;
    type Alpha_Nat_array is array (0 .. max_alpha_size) of Natural;

    len  : array (0 .. max_groups) of Alpha_Nat_array;
    limit,
    base ,
    perm : array (0 .. max_groups) of Alpha_U32_array;
    --
    minlens: Length_array(0 .. max_groups);
    cftab: array (0 .. 257) of Natural;
    --
    end_reached: Boolean:= False;

    in_buf: Buffer(1 .. input_buffer_size);
    in_idx: Natural:= in_buf'Last + 1;

    function Read_byte return Unsigned_8 is
      res: Unsigned_8;
    begin
      if in_idx > in_buf'Last then
        Read(in_buf);
        in_idx:= in_buf'First;
      end if;
      res:= in_buf(in_idx);
      in_idx:= in_idx + 1;
      return res;
    end Read_byte;

    procedure hb_create_decode_tables(
       limit, base, perm: in out Alpha_U32_array;
       length           : in     Alpha_Nat_array;
       min_len, max_len : Natural;
       alpha_size       : Integer
    )
    is
      pp, idx: Integer;
      vec: Unsigned_32;
    begin
      pp:=0;
      for i in min_len .. max_len loop
        for j in 0 .. alpha_size-1 loop
          if length(j)=i then
            perm(pp):= Unsigned_32(j);
            pp:= pp + 1;
          end if;
        end loop;
      end loop;
      for i in 0 .. max_code_len-1 loop
        base(i):=0;
        limit(i):=0;
      end loop;
      for i in 0 .. alpha_size-1 loop
        idx:= length(i)+1;
        base(idx):= base(idx) + 1;
      end loop;
      for i in 1 .. max_code_len-1 loop
        base(i):= base(i) + base(i-1);
      end loop;
      vec:=0;
      for i in min_len .. max_len loop
          vec:= vec + base(i+1)-base(i);
          limit(i):= vec-1;
          vec:= vec * 2;
      end loop;
      for i in min_len+1 .. max_len loop
        base(i):=(limit(i-1)+1) * 2 - base(i);
      end loop;
    end hb_create_decode_tables;

    procedure Init is
      magic: String(1..3);
      b: Unsigned_8;
    begin
      --  Read the magic.
      for i in magic'Range loop
        b:= Read_byte;
        magic(i):= Character'Val(b);
      end loop;
      if magic /= "BZh" then
        raise bad_header_magic;
      end if;
      --  Read the block size and allocate the working array.
      b:= Read_byte;
      block_size:= Natural(b) - Character'Pos('0');
      tt:= new Tcardinal_array(0 .. block_size * sub_block_size);
    end Init;

    function get_bits(n: Natural) return Unsigned_8 is
      Result_get_bits : Unsigned_8;
      data: Unsigned_8;
    begin
      if n > bits_available then
        data:= Read_byte;
        Result_get_bits:= Shift_Right(read_data, 8-n) or Shift_Right(data, 8-(n-bits_available));
        read_data:= Shift_Left(data, n-bits_available);
        bits_available:= bits_available + 8;
      else
        Result_get_bits:= Shift_Right(read_data, 8-n);
        read_data:= Shift_Left(read_data, n);
      end if;
      bits_available:= bits_available - n;
      return Result_get_bits;
    end get_bits;

    function get_bits_32(n: Natural) return Unsigned_32 is
    begin
      return Unsigned_32(get_bits(n));
    end get_bits_32;

    function get_boolean return Boolean is
    begin
      return Boolean'Val(get_bits(1));
    end get_boolean;

    function get_byte return Unsigned_8 is
    begin
      return get_bits(8);
    end get_byte;

    function get_cardinal24 return Unsigned_32 is
    begin
      return Shift_Left(get_bits_32(8),16) or Shift_Left(get_bits_32(8),8) or get_bits_32(8);
    end get_cardinal24;

    function get_cardinal return Unsigned_32 is
    begin
      return Shift_Left(get_bits_32(8),24)  or
             Shift_Left(get_bits_32(8),16)  or
             Shift_Left(get_bits_32(8), 8)  or
             get_bits_32(8);
    end get_cardinal;

    --  Receive the mapping table. To save space, the inuse set is stored in pieces
    --  of 16 bits. First 16 bits are stored which pieces of 16 bits are used, then
    --  the pieces follow.
    procedure receive_mapping_table is
      inuse16: array(0 .. 15) of Boolean;
      --* inuse: array(0 .. 255) of Boolean; -- for dump purposes
    begin
      inuse16:= (others => False);
      --  Receive the first 16 bits which tell which pieces are stored.
      for i in 0 .. 15 loop
        inuse16(i):= get_boolean;
      end loop;
      --  Receive the used pieces.
      --* inuse:= (others => False);
      inuse_count:= 0;
      for i in 0 .. 15 loop
        if inuse16(i) then
          for j in 0 .. 15 loop
            if get_boolean then
              --* inuse(16*i+j):= True;
              seq_to_unseq(inuse_count):=16*i+j;
              inuse_count:= inuse_count + 1;
            end if;
          end loop;
        end if;
      end loop;
    end receive_mapping_table;

    --  Receives the selectors.
    procedure receive_selectors is
      j:Unsigned_8;
    begin
      group_count:= Natural(get_bits(3));
      selector_count:= Natural(Shift_Left(get_bits_32(8), 7) or get_bits_32(7));
      for i in 0 .. selector_count-1 loop
        j:=0;
        while get_boolean loop
          j:= j + 1;
          if j > 5 then
            raise data_error;
          end if;
        end loop;
        selector_mtf(i):=j;
      end loop;
    end receive_selectors;

    --  Undo the MTF values for the selectors.
    procedure undo_mtf_values is
      pos: array (0 .. max_groups) of Natural;
      v, tmp: Natural;
    begin
      for w in 0 .. group_count-1 loop
        pos(w):=w;
      end loop;
      for i in 0 .. selector_count-1 loop
        v:= Natural(selector_mtf(i));
        tmp:=pos(v);
        while v/=0 loop
          pos(v):= pos(v-1);
          v:= v - 1;
        end loop;
        pos(0):= tmp;
        selector(i):= Unsigned_8(tmp);
      end loop;
    end undo_mtf_values;

    procedure receive_coding_tables is
      curr: Natural;
    begin
      for t in 0 .. group_count-1 loop
        curr:= Natural(get_bits(5));
        for i in 0 .. alpha_size-1 loop
          loop
            if curr not in 1..20 then
              raise data_error;
            end if;
            exit when not get_boolean;
            if get_boolean then
              curr:= curr - 1;
            else
              curr:= curr + 1;
            end if;
          end loop;
          len(t)(i):=curr;
        end loop;
      end loop;
    end receive_coding_tables;

    --  Builds the Huffman tables.
    procedure make_hufftab is
      minlen, maxlen: Natural;
    begin
      for t in 0 .. group_count-1 loop
        minlen:= 32;
        maxlen:= 0;
        for i in 0 .. alpha_size-1 loop
          if len(t)(i) > maxlen then
            maxlen:= len(t)(i);
          end if;
          if len(t)(i) < minlen then
            minlen:= len(t)(i);
          end if;
        end loop;
        hb_create_decode_tables(
          limit(t), base(t), perm(t), len(t),
          minlen, maxlen, alpha_size
        );
        minlens(t):= minlen;
      end loop;
    end make_hufftab;

    -------------------------
    -- MTF - Move To Front --
    -------------------------

    procedure receive_mtf_values is
      --
      mtfa_size: constant:= 4096;
      mtfl_size: constant:= 16;
      mtfbase: array (0 .. 256 / mtfl_size-1) of Natural;
      mtfa: array (0 .. mtfa_size-1) of Natural;
      --
      procedure init_mtf is
        k: Natural:= mtfa_size-1;
      begin
        for i in reverse 0 .. 256  /  mtfl_size-1 loop
          for j in reverse 0 .. mtfl_size-1 loop
            mtfa(k):= i*mtfl_size + j;
            k:= k - 1;
          end loop;
          mtfbase(i):= k+1;
        end loop;
      end init_mtf;
      --
      group_pos, group_no: Integer;
      gminlen, gsel: Natural;
      --
      function get_mtf_value return Unsigned_32 is
        zn: Natural;
        zvec: Unsigned_32;
      begin
        if group_pos = 0 then
          group_no:= group_no + 1;
          group_pos:= group_size;
          gsel:= Natural(selector(group_no));
          gminlen:= minlens(gsel);
        end if;
        group_pos:= group_pos - 1;
        zn:= gminlen;
        zvec:= get_bits_32(zn);
        while zvec > limit(gsel)(zn) loop
          zn:= zn + 1;
          zvec:= Shift_Left(zvec, 1) or get_bits_32(1);
        end loop;
        return perm(gsel)(Natural(zvec-base(gsel)(zn)));
      end get_mtf_value;
      --
      procedure move_mtf_block is
        j, k: Natural;
      begin
        k:= mtfa_size;
        for i in reverse 0 .. 256  /  mtfl_size-1 loop
          j:= mtfbase(i);
          mtfa(k-16..k-1):= mtfa(j..j+15);
          k:= k - 16;
          mtfbase(i):= k;
        end loop;
      end move_mtf_block;
      --
      run_b: constant:= 1;
      t: Natural;
      next_sym: Unsigned_32;
      es: Natural;
      n, nn: Natural;
      p,q: Natural; -- indexes mtfa
      u,v: Natural; -- indexes mtfbase
      lno, off: Natural;
    begin -- receive_mtf_values
      group_no:= -1;
      group_pos:= 0;
      t:= 0;
      cftab:= (others => 0);
      init_mtf;
      next_sym:= get_mtf_value;
      --
      while Natural(next_sym) /= inuse_count+1 loop
        if next_sym <= run_b then
          es:= 0;
          n:= 0;
          loop
            es:= es + Natural(Shift_Left(next_sym+1, n));
            n:= n + 1;
            next_sym:= get_mtf_value;
            exit when next_sym > run_b;
          end loop;
          n:= seq_to_unseq( mtfa(mtfbase(0)) );
          cftab(n):= cftab(n) + es;
          if t+es > sub_block_size * block_size then
            raise data_error;
          end if;
          while es > 0 loop
            tt(t):= Unsigned_32(n);
            es:= es - 1;
            t:= t + 1;
          end loop;
        else
          nn:= Natural(next_sym - 1);
          if nn < mtfl_size then
            -- Avoid the costs of the general case.
            p:= mtfbase(0);
            q:= p + nn;
            n:= mtfa(q);
            loop
              mtfa(q):= mtfa(q-1);
              q:= q - 1;
              exit when q = p;
            end loop;
            mtfa(q):= n;
          else
            --  General case.
            lno:= nn   /   mtfl_size;
            off:= nn  mod  mtfl_size;
            p:= mtfbase(lno);
            q:= p + off;
            n:= mtfa(q);
            while q /= p loop
              mtfa(q):= mtfa(q-1);
              q:= q - 1;
            end loop;
            u:= mtfbase'First;
            v:= u + lno;
            loop
              mtfa(mtfbase(v)):= mtfa(mtfbase(v-1)+mtfl_size-1);
              v:= v - 1;
              mtfbase(v):= mtfbase(v) - 1;
              exit when v = u;
            end loop;
            mtfa( mtfbase(v) ):= n;
            if mtfbase(v) = 0 then
              move_mtf_block;
            end if;
          end if;
          cftab(seq_to_unseq(n)):= cftab(seq_to_unseq(n)) + 1;
          tt(t):= Unsigned_32(seq_to_unseq(n));
          t:= t + 1;
          if t > sub_block_size * block_size then
            raise data_error;
          end if;
          next_sym:= get_mtf_value;
        end if;
      end loop;
      tt_count:= t;
      --  Setup cftab to facilitate generation of T^(-1).
      t:= 0;
      for i in 0 .. 256 loop
        nn:= cftab(i);
        cftab(i):= t;
        t:= t + nn;
      end loop;
    end receive_mtf_values;

    procedure detransform is
      a: Unsigned_32;
      p,q,r,i255: Natural;
    begin
      a:= 0;
      p:= tt'First;
      q:= p + tt_count;
      while p /= q loop
        i255:= Natural(tt(p) and 16#ff#);
        r:= cftab(i255);
        cftab(i255):= cftab(i255) + 1;
        tt(r):= tt(r) or a;
        a:= a + 256;
        p:= p + 1;
      end loop;
    end detransform;

    -- Cyclic redundancy check to verify uncompressed block data integrity

    package CRC is
      procedure Init( CRC: out Unsigned_32 );
      function  Final( CRC: Unsigned_32 ) return Unsigned_32;
      procedure Update( CRC: in out Unsigned_32; val: Unsigned_8 );
        pragma Inline( Update );
    end CRC;

    package body CRC is

      CRC32_Table :
        constant array( Unsigned_32'(0)..255 ) of Unsigned_32:= (
           16#00000000#, 16#04c11db7#, 16#09823b6e#, 16#0d4326d9#,
           16#130476dc#, 16#17c56b6b#, 16#1a864db2#, 16#1e475005#,
           16#2608edb8#, 16#22c9f00f#, 16#2f8ad6d6#, 16#2b4bcb61#,
           16#350c9b64#, 16#31cd86d3#, 16#3c8ea00a#, 16#384fbdbd#,
           16#4c11db70#, 16#48d0c6c7#, 16#4593e01e#, 16#4152fda9#,
           16#5f15adac#, 16#5bd4b01b#, 16#569796c2#, 16#52568b75#,
           16#6a1936c8#, 16#6ed82b7f#, 16#639b0da6#, 16#675a1011#,
           16#791d4014#, 16#7ddc5da3#, 16#709f7b7a#, 16#745e66cd#,
           16#9823b6e0#, 16#9ce2ab57#, 16#91a18d8e#, 16#95609039#,
           16#8b27c03c#, 16#8fe6dd8b#, 16#82a5fb52#, 16#8664e6e5#,
           16#be2b5b58#, 16#baea46ef#, 16#b7a96036#, 16#b3687d81#,
           16#ad2f2d84#, 16#a9ee3033#, 16#a4ad16ea#, 16#a06c0b5d#,
           16#d4326d90#, 16#d0f37027#, 16#ddb056fe#, 16#d9714b49#,
           16#c7361b4c#, 16#c3f706fb#, 16#ceb42022#, 16#ca753d95#,
           16#f23a8028#, 16#f6fb9d9f#, 16#fbb8bb46#, 16#ff79a6f1#,
           16#e13ef6f4#, 16#e5ffeb43#, 16#e8bccd9a#, 16#ec7dd02d#,
           16#34867077#, 16#30476dc0#, 16#3d044b19#, 16#39c556ae#,
           16#278206ab#, 16#23431b1c#, 16#2e003dc5#, 16#2ac12072#,
           16#128e9dcf#, 16#164f8078#, 16#1b0ca6a1#, 16#1fcdbb16#,
           16#018aeb13#, 16#054bf6a4#, 16#0808d07d#, 16#0cc9cdca#,
           16#7897ab07#, 16#7c56b6b0#, 16#71159069#, 16#75d48dde#,
           16#6b93dddb#, 16#6f52c06c#, 16#6211e6b5#, 16#66d0fb02#,
           16#5e9f46bf#, 16#5a5e5b08#, 16#571d7dd1#, 16#53dc6066#,
           16#4d9b3063#, 16#495a2dd4#, 16#44190b0d#, 16#40d816ba#,
           16#aca5c697#, 16#a864db20#, 16#a527fdf9#, 16#a1e6e04e#,
           16#bfa1b04b#, 16#bb60adfc#, 16#b6238b25#, 16#b2e29692#,
           16#8aad2b2f#, 16#8e6c3698#, 16#832f1041#, 16#87ee0df6#,
           16#99a95df3#, 16#9d684044#, 16#902b669d#, 16#94ea7b2a#,
           16#e0b41de7#, 16#e4750050#, 16#e9362689#, 16#edf73b3e#,
           16#f3b06b3b#, 16#f771768c#, 16#fa325055#, 16#fef34de2#,
           16#c6bcf05f#, 16#c27dede8#, 16#cf3ecb31#, 16#cbffd686#,
           16#d5b88683#, 16#d1799b34#, 16#dc3abded#, 16#d8fba05a#,
           16#690ce0ee#, 16#6dcdfd59#, 16#608edb80#, 16#644fc637#,
           16#7a089632#, 16#7ec98b85#, 16#738aad5c#, 16#774bb0eb#,
           16#4f040d56#, 16#4bc510e1#, 16#46863638#, 16#42472b8f#,
           16#5c007b8a#, 16#58c1663d#, 16#558240e4#, 16#51435d53#,
           16#251d3b9e#, 16#21dc2629#, 16#2c9f00f0#, 16#285e1d47#,
           16#36194d42#, 16#32d850f5#, 16#3f9b762c#, 16#3b5a6b9b#,
           16#0315d626#, 16#07d4cb91#, 16#0a97ed48#, 16#0e56f0ff#,
           16#1011a0fa#, 16#14d0bd4d#, 16#19939b94#, 16#1d528623#,
           16#f12f560e#, 16#f5ee4bb9#, 16#f8ad6d60#, 16#fc6c70d7#,
           16#e22b20d2#, 16#e6ea3d65#, 16#eba91bbc#, 16#ef68060b#,
           16#d727bbb6#, 16#d3e6a601#, 16#dea580d8#, 16#da649d6f#,
           16#c423cd6a#, 16#c0e2d0dd#, 16#cda1f604#, 16#c960ebb3#,
           16#bd3e8d7e#, 16#b9ff90c9#, 16#b4bcb610#, 16#b07daba7#,
           16#ae3afba2#, 16#aafbe615#, 16#a7b8c0cc#, 16#a379dd7b#,
           16#9b3660c6#, 16#9ff77d71#, 16#92b45ba8#, 16#9675461f#,
           16#8832161a#, 16#8cf30bad#, 16#81b02d74#, 16#857130c3#,
           16#5d8a9099#, 16#594b8d2e#, 16#5408abf7#, 16#50c9b640#,
           16#4e8ee645#, 16#4a4ffbf2#, 16#470cdd2b#, 16#43cdc09c#,
           16#7b827d21#, 16#7f436096#, 16#7200464f#, 16#76c15bf8#,
           16#68860bfd#, 16#6c47164a#, 16#61043093#, 16#65c52d24#,
           16#119b4be9#, 16#155a565e#, 16#18197087#, 16#1cd86d30#,
           16#029f3d35#, 16#065e2082#, 16#0b1d065b#, 16#0fdc1bec#,
           16#3793a651#, 16#3352bbe6#, 16#3e119d3f#, 16#3ad08088#,
           16#2497d08d#, 16#2056cd3a#, 16#2d15ebe3#, 16#29d4f654#,
           16#c5a92679#, 16#c1683bce#, 16#cc2b1d17#, 16#c8ea00a0#,
           16#d6ad50a5#, 16#d26c4d12#, 16#df2f6bcb#, 16#dbee767c#,
           16#e3a1cbc1#, 16#e760d676#, 16#ea23f0af#, 16#eee2ed18#,
           16#f0a5bd1d#, 16#f464a0aa#, 16#f9278673#, 16#fde69bc4#,
           16#89b8fd09#, 16#8d79e0be#, 16#803ac667#, 16#84fbdbd0#,
           16#9abc8bd5#, 16#9e7d9662#, 16#933eb0bb#, 16#97ffad0c#,
           16#afb010b1#, 16#ab710d06#, 16#a6322bdf#, 16#a2f33668#,
           16#bcb4666d#, 16#b8757bda#, 16#b5365d03#, 16#b1f740b4#
      );

      procedure Update( CRC: in out Unsigned_32; val: Unsigned_8 ) is
      begin
        CRC :=
          CRC32_Table( 16#FF# and ( Shift_Right(CRC, 24) xor Unsigned_32( val ) ) )
          xor
          Shift_Left( CRC , 8 );
      end Update;

      procedure Init( CRC: out Unsigned_32 ) is
      begin
        CRC:= 16#FFFF_FFFF#;
      end Init;

      function Final( CRC: Unsigned_32 ) return Unsigned_32 is
      begin
        return not CRC;
      end Final;

    end CRC;

    compare_final_CRC: Boolean:= False;
    stored_blockcrc, mem_stored_blockcrc, computed_crc: Unsigned_32;

    -- Decode a new compressed block.
    function decode_block return Boolean is
      magic: String(1 .. 6);
    begin
      for i in 1 .. 6 loop
        magic(i):= Character'Val(get_byte);
      end loop;
      if magic = "1AY&SY" then
        if check_CRC then
          if compare_final_CRC then
            null; -- initialisation is delayed until the rle buffer is empty
          else
            CRC.Init(computed_crc); -- Initialize for next block.
          end if;
        end if;
        stored_blockcrc:= get_cardinal;
        block_randomized:= get_boolean;
        block_origin:= Natural(get_cardinal24);
        --  Receive the mapping table.
        receive_mapping_table;
        alpha_size:= inuse_count + 2;
        --  Receive the selectors.
        receive_selectors;
        --  Undo the MTF values for the selectors.
        undo_mtf_values;
        --  Receive the coding tables.
        receive_coding_tables;
        --  Build the Huffman tables.
        make_hufftab;
        --  Receive the MTF values.
        receive_mtf_values;
        --  Undo the Burrows Wheeler transformation.
        detransform;
        decode_available:= tt_count;
        return True;
      elsif magic = Character'Val(16#17#) & "rE8P" & Character'Val(16#90#) then
        return False;
      else
        raise bad_block_magic;
      end if;
    end decode_block;

    next_rle_idx: Integer:= -2;
    buf: Buffer(1 .. output_buffer_size);
    last: Natural;

    procedure Read is
      shorten: Natural:= 0;

      procedure rle_read is
        rle_len: Natural;
        data: Unsigned_8;
        idx: Integer:= buf'First;
        count: Integer:= buf'Length;
        --
        procedure rle_write is
          pragma Inline(rle_write);
        begin
          loop
            buf(idx):= data;
            idx:= idx + 1;
            count:= count - 1;
            rle_len:= rle_len - 1;
            if check_CRC then
              CRC.Update(computed_crc, data);
              if rle_len = 0 and then compare_final_CRC then
                if CRC.Final(computed_crc) /= mem_stored_blockcrc then
                  raise block_crc_check_failed;
                end if;
                compare_final_CRC:= False;
                CRC.Init(computed_crc); -- Initialize for next block.
              end if;
            end if;
            exit when rle_len = 0 or count = 0;
          end loop;
        end rle_write;
        --
        -- handle extreme cases of data of length 1, 2
        input_dried: exception;
        --
        -- Make next_rle_idx index to the next decoded byte.
        -- If next_rle_idx did index to the last
        -- byte in the current block, decode the next block.
        --
        procedure consume_rle is
          pragma Inline(consume_rle);
        begin
          next_rle_idx:= Integer(Shift_Right(tt(next_rle_idx),8));
          decode_available:= decode_available - 1;
          if decode_available = 0 then
            compare_final_CRC:= True;
            mem_stored_blockcrc:= stored_blockcrc;
            -- ^ There might be a new block when last block's
            --   rle is finally emptied.
            --
            -- ** New block
            if decode_block then
              next_rle_idx:= Natural(Shift_Right(tt(block_origin),8));
            else
              next_rle_idx:= -1;
              end_reached:= True;
            end if;
            -- **
            if end_reached then
              raise input_dried;
            end if;
          end if;
        end consume_rle;
        --
        function rle_byte return Unsigned_8 is
          pragma Inline(rle_byte);
        begin
          return Unsigned_8(tt(next_rle_idx) and 16#FF#);
        end rle_byte;
        --
        function rle_possible return Boolean is
          pragma Inline(rle_possible);
        begin
          return decode_available > 0 and then data = rle_byte;
        end rle_possible;
        --
      begin -- rle_read
        rle_len:= rle_run_left;
        data:= rle_run_data;
        if block_randomized then
          raise randomized_not_yet_implemented;
        end if;
        if rle_len /= 0 then
          rle_write;
          if count = 0 then
            shorten:= 0;
            rle_run_data:= data;
            rle_run_left:= rle_len;
            return;
          end if;
        end if;
        begin
          -- The big loop
          loop
            if decode_available = 0 or end_reached then
              exit;
            end if;
            rle_len:= 1;
            data:= rle_byte;
            consume_rle;
            if rle_possible then
              rle_len:= rle_len + 1;
              consume_rle;
              if rle_possible then
                rle_len:= rle_len + 1;
                consume_rle;
                if rle_possible then
                  consume_rle;
                  rle_len:= rle_len + Natural(rle_byte)+1;
                  consume_rle;
                end if;
              end if;
            end if;
            rle_write;
            exit when count = 0;
          end loop;
        exception
          when input_dried => rle_write;
        end;
        shorten:= count;
        rle_run_data:= data;
        rle_run_left:= rle_len;
      end rle_read;

    begin -- read
      last:= buf'Last;
      if decode_available = Natural'Last then
        --  Initialize the rle process:
        --       - Decode a block
        --       - Initialize pointer.
        if decode_block then
          next_rle_idx:= Natural(Shift_Right(tt(block_origin), 8));
        else
          next_rle_idx:= -1;
          end_reached:= True;
        end if;
      end if;
      rle_read;
      last:= last - shorten;
    end Read;

  begin
    Init;
    loop
      Read;
      Write( buf(1..last) );
      exit when end_reached and rle_run_left = 0;
    end loop;
    Dispose(tt);
  end Decompress;

end BZip2;
