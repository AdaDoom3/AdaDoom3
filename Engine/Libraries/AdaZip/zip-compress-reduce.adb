--
-- "Reduce" method - probabilistic reduction with a Markov chain.
-- See package specification for details.
--

-- Change log:
--
--  7-Feb-2009: GdM: added a cache for LZ77 output to make 2nd phase faster

with Interfaces; use Interfaces;
with Zip.LZ77, Zip.CRC;
with Zip_Streams;

with Ada.Text_IO;                       use Ada.Text_IO;

procedure Zip.Compress.Reduce
 (input,
  output          : in out Zip_Streams.Root_Zipstream_Type'Class;
  input_size_known: Boolean;
  input_size      : File_size_type;
  feedback        : Feedback_proc;
  method          : Reduction_Method;
  CRC             : in out Interfaces.Unsigned_32; -- only updated here
  output_size     : out File_size_type;
  compression_ok  : out Boolean -- indicates compressed < uncompressed
)
is
  reduction_factor: constant Positive:=
    1 + Compression_Method'Pos(method) - Compression_Method'Pos(Reduce_1);
  use Zip_Streams;

  DLE_code: constant:= 144;
  subtype Symbol_range is Integer range 0..255;
  subtype Follower_range is Symbol_range range 0..31;
  -- PKWARE appnote.txt limits to 32 followers.
  -- Above 32, you get "PKUNZIP: (W03) Warning! file has bad table"
  -- Up to 63 is indeed possible and accepted by unzip <=5.12, WinZip
  -- and of course Zip-Ada :-)
  -- Optimum with 63 is extremely rare, the gain on test
  -- files showing a 63 is 0.02%.
  -- Then, we prefer compatibility here.

  Followers: array (Symbol_range, Follower_range) of Symbol_range:=
    (others=> (others=> 0));
  Slen: array (Symbol_range) of Symbol_range;

  -- Bits taken by (x-1) mod 256:
  B_Table: constant array(Symbol_range) of Integer:=
    (  0      => 8,
       1..2   => 1,
       3..4   => 2,
       5..8   => 3,
       9..16  => 4,
      17..32  => 5,
      33..64  => 6,
      65..128 => 7,
     129..255 => 8 );

  ------------------
  -- Buffered I/O --
  ------------------

  --  Define data types needed to implement input and output file buffers

  InBuf, OutBuf: Byte_Buffer(1..buffer_size);

  InBufIdx: Positive;  --  Points to next char in buffer to be read
  OutBufIdx: Positive; --  Points to next free space in output buffer

  MaxInBufIdx: Natural;  --  Count of valid chars in input buffer
  InputEoF: Boolean;     --  End of file indicator

  procedure Read_Block is
  begin
    Zip.BlockRead(
      stream        => input,
      buffer        => InBuf,
      actually_read => MaxInBufIdx
    );
    InputEoF:= MaxInBufIdx = 0;
    InBufIdx := 1;
  end Read_Block;

  -- Exception for the case where compression works but produces
  -- a bigger file than the file to be compressed (data is too "random").
  Compression_unefficient: exception;

  procedure Write_Block is
    amount: constant Integer:= OutBufIdx-1;
  begin
    output_size:= output_size + File_size_type(Integer'Max(0,amount));
    if input_size_known and then output_size >= input_size then
      -- The compression so far is obviously unefficient for that file.
      -- Useless to go further.
      -- Stop immediately before growing the file more than the
      -- uncompressed size.
      raise Compression_unefficient;
    end if;
    Zip.BlockWrite(output, OutBuf(1 .. amount));
    OutBufIdx := 1;
  end Write_Block;

  procedure Put_byte(B : Byte) is
  begin
    OutBuf(OutBufIdx) := B;
    OutBufIdx:= OutBufIdx + 1;
    if OutBufIdx > OutBuf'Last then
      Write_Block;
    end if;
  end Put_byte;

  --------------------------------------------------------------------------

  -----------------
  -- Code buffer --
  -----------------

  Save_byte: Byte;  --  Output code buffer
  Bits_used: Byte;  --  Index into output code buffer

  procedure Flush_output is
  begin
    if Bits_used /= 0 then
      Put_byte(Save_byte);
    end if;
    if OutBufIdx > 1 then
      Write_Block;
    end if;
  end Flush_output;

  procedure Put_code(Code : Byte; Code_size: Natural) is
    Code_work: Byte;
    temp, Save_byte_local, Bits_used_local: Byte;
  begin
    Code_work:= Code;
    temp:= 0;
    Save_byte_local:= Save_byte;
    Bits_used_local:= Bits_used;
    for count in reverse 1 .. Code_size loop
      temp:= 0;
      if Code_work mod 2 = 1 then
        temp:= temp + 1;
      end if;
      Code_work:= Code_work  / 2;
      temp:= Shift_Left(temp, Integer(Bits_used_local));
      Bits_used_local:= Bits_used_local+1;
      Save_byte_local:= Save_byte_local or temp;
      if Bits_used_local = 8 then
        Put_byte(Save_byte_local);
        Save_byte_local:= 0;
        temp:= 0;
        Bits_used_local:= 0;
      end if;
    end loop;
    Save_byte:= Save_byte_local;
    Bits_used:= Bits_used_local;
  end Put_code;

  procedure Show_symbol(S: Symbol_range) is
  begin
    if S in 32..126 then
      Ada.Text_IO.Put(Character'Val(S));
    else
      Ada.Text_IO.Put('{' & Symbol_range'Image(S) & '}');
    end if;
  end Show_symbol;

  procedure Save_Followers is
  begin
    for X in reverse Symbol_range loop
      Put_code(Byte(Slen(X)),6); -- max 2**6 followers per symbol
      for I in 0 .. Slen(X)-1  loop
        Put_code(Byte(Followers(X,I)),8);
      end loop;
    end loop;
  end Save_Followers;

  ----------------------------------------
  -- Probabilistic back-end compression --
  ----------------------------------------

  subtype Count is Integer_32;

  markov_d: array(Symbol_range, Symbol_range) of Count:=
    (others=> (others=> 0));

  -- Build probability of transition from ymbol i to symbol j:
  --
  -- markov(i,j) = P(symbol i is followed by j)
  --             = markov_d(i,j) / sum_k( markov_d(i,k))

  total_line: array(Symbol_range) of Count;
  -- total_line(i) = sum_k( markov_d(i,k))

  order: array(Symbol_range, Symbol_range) of Symbol_range;

  use_probas: constant Boolean:= True;
  trace     : constant Boolean:= False;

  -- We keep the most significant quantiles of markov
  -- to allow a coding shorter than the symbol itself.
  -- Otherwise, why doing all that ;-) ?...

  has_follower: array(Symbol_range, Symbol_range) of Boolean:=
    (others => (others => False));
  follower_pos: array(Symbol_range, Symbol_range) of Follower_range;

  -- follower_pos(a,b) only significant if has_follower(a,b) = True

  procedure Show_partial_markov(ordered: Boolean) is
    subtype subrange is
      Symbol_range range Character'Pos('a')..Character'Pos('i');
    sk, min, max: Symbol_range;
  begin
     if ordered then
       min:= Follower_range'First;
       max:= Follower_range'Last;
     else
       min:= subrange'First;
       max:= subrange'Last;
     end if;
    New_Line;
    for si in subrange loop
      Show_symbol(si);
      Put("| ");
      for sj in min..max loop
        if ordered then -- show top probas
          sk:= order(si,sj);
        else -- show probas in the same subrange
          sk:= sj;
        end if;
        Show_symbol(sk);
        Put(':' & Count'Image(markov_d(si,sj)) & ' ');
      end loop;
      if ordered then
        Put("|" & Count'Image(total_line(si)) & Integer'Image(Slen(si)));
      end if;
      New_Line;
    end loop;
  end Show_partial_markov;

  procedure Build_Followers is

    procedure Swap(a,b: in out Count) is
      pragma Inline(Swap);
      c: Count;
    begin c:= a;a:= b;b:= c; end Swap;

    procedure Swap(a,b: in out Symbol_range) is
      pragma Inline(Swap);
      c: Symbol_range;
    begin c:= a;a:= b;b:= c; end Swap;

    procedure Sort(sym_line: Symbol_range) is
      -- A stupid bubble sort algo, but one working with sets
      -- having big packs of same data (trouble with qsort)...
      swapped: Boolean;
      left: Symbol_range:= Symbol_range'First;
    begin
      loop
        swapped:= False;
        for i in reverse left .. Symbol_range'Last-1 loop
          if markov_d(sym_line, i) < markov_d(sym_line, i+1) then
            Swap( markov_d(sym_line, i), markov_d(sym_line, i+1));
            Swap( order   (sym_line, i), order   (sym_line, i+1));
            swapped:= True;
          end if;
        end loop;
        exit when not swapped;
        left:= left+1;
      end loop;
    end Sort;

    cumul: Count;
    follo: Symbol_range;

    subtype Bit_range is Integer range 0..5;
    -- 6 would be possible, PKWARE appnote.txt limits numb.
    -- of followers to 2**5

    max_follo: constant array(Bit_range) of Integer:=
      -- mostly 2**n - 1
      -- actual follower range is: 0..max_follo(bits)
      ( 0 => -1, -- NB: not 0; range is empty here
        1 =>  1,
        2 =>  3,
        3 =>  7,
        4 => 15,
        5 => 31
    --  6 => 62  -- NB: not 63
      );

    cumul_per_length: array(Bit_range) of Count:= (others => 0);

    exp_size, min_size: Float;
    bits_min: Bit_range;

  begin -- Build_Followers
    if not use_probas then
      Slen:= (others => 0);
      return;
    end if;
    if trace then
      Show_partial_markov(False);
    end if;
    for si in Symbol_range loop
      total_line(si):= 0;
      for sj in Symbol_range loop
        order(si,sj):= sj;
        total_line(si):= total_line(si) + markov_d(si,sj);
      end loop;
      Sort(si);
      cumul:= 0;
      -- Define all possible followers:
      for sj in Follower_range loop
        cumul:= cumul + markov_d(si,sj);
        cumul_per_length(B_Table(sj+1)):= cumul;
        follo:= order(si,sj);
        Followers(si,sj):= follo;
        follower_pos(si, follo):= sj;
      end loop;
      -- Now we decide to which length we are using the followers
      min_size:= Float(total_line(si)) * 8.0;
      -- ^ Size of all codes for no followers at all to symbol si
      bits_min:= 0;
      for bits in 1 .. Bit_range'Last loop
        -- We compute the exact size of reduced output when cutting
        -- the follower range to 0..2**bits-1
        exp_size:=
          Float(cumul_per_length(bits))                * Float(bits+1) +
          -- ^ Coded followers
          Float(total_line(si)-cumul_per_length(bits)) * 9.0 +
          -- ^ All codes outside the follower list will take 8+1 bits
          Float(max_follo(bits)+1) * 8.0;
          -- ^ Also the follower list at the beginning takes place...
        if exp_size < min_size then
          -- It is more efficient to encode si's followers with 'bits' bits.
          min_size:= exp_size;
          bits_min:= bits;
        end if;
      end loop;
      Slen(si):= max_follo(bits_min)+1;
      for sj in 0 .. max_follo(bits_min) loop
        has_follower(si, Followers(si,sj)):= True;
      end loop;
    end loop;
    if trace then
      Show_partial_markov(True);
    end if;
  end Build_Followers;

  --------------------------------
  -- LZ77 front-end compression --
  --------------------------------

  -- Cache for LZ-compressed data, to speedup the 2nd phase:

  LZ_cache_size: constant:= 2**18; -- 256KB
  type LZ_buffer_range is mod LZ_cache_size;
  type LZ_buffer is array(LZ_buffer_range) of Byte; -- circular buffer

  type LZ_cache_type is record
    buf: LZ_buffer;           -- buf's index arithmetic is mod LZ_cache_size
    nxt: LZ_buffer_range:= 0; -- position of next byte to be written
    cnt: Natural:= 0;         -- [0..size]: count of cached bytes
  end record;

  LZ_cache: LZ_cache_type;
  lz77_pos, lz77_size: File_size_type:= 0;
  using_LZ77: Boolean;
  Derail_LZ77: exception;

  -- Possible ranges for LZ distance and length encoding
  -- in the Zip-Reduce format:

  subtype Length_range is
    Integer range 4 .. 2**(8-reduction_factor)+257;

  subtype Distance_range is
    Integer range 1 .. (2**reduction_factor)*256;

  --        max length  max dist
  --    1   385         512
  --    2   321         1024
  --    3   289         2048
  --    4   273         4096

  type Phase_type is (stats, compress);

  generic
    phase: Phase_type;
  procedure Encode;

  procedure Encode is

    X_Percent: Natural;
    Bytes_in   : Natural;   --  Count of input file bytes processed
    user_aborting: Boolean;
    real_pct: constant array(Phase_type) of Integer:= (0, 50);
    PctDone: Natural;

    function Read_byte return Byte is
      b: Byte;
    begin
      b:= InBuf(InBufIdx);
      InBufIdx:= InBufIdx + 1;
      if phase = stats then
        Zip.CRC.Update(CRC, (1=> b));
      end if;
      Bytes_in:= Bytes_in + 1;
      if feedback /= null then
        if Bytes_in = 1 then
          feedback(real_pct(phase), False, user_aborting);
        end if;
        if X_Percent > 0 and then
           ((Bytes_in-1) mod X_Percent = 0
            or Bytes_in = Integer(input_size))
        then
          if input_size_known then
            PctDone := real_pct(phase) + Integer( (50.0 * Float( Bytes_in)) / Float(input_size));
            feedback(PctDone, False, user_aborting);
          else
            feedback(real_pct(phase), False, user_aborting);
          end if;
          if user_aborting then
            raise User_abort;
          end if;
        end if;
      end if;
      return b;
    end Read_byte;

    function More_bytes return Boolean is
    begin
      if InBufIdx > MaxInBufIdx then
        Read_Block;
      end if;
      return not InputEoF;
    end More_bytes;

    upper_shift: constant Integer:= 2**(8-reduction_factor);
    maximum_len_1: constant Integer:= upper_shift - 1;
    maximum_len_1_b: constant Byte:= Byte(maximum_len_1);

    -- LZ77 params
    Look_redfac        : constant array(1..4) of Integer:= (31,63,255,191);
    -- see uza_work.xls for the cooking of these numbers...
    Look_Ahead         : constant Integer:= Look_redfac(reduction_factor);
    String_buffer_size : constant := 2**12; -- 2**n optimizes "mod" to "and"
    Threshold          : constant := 3;

    -- if the DLE coding doesn't fit the format constraints, we
    -- need to decode it as a simple sequence of litterals
    -- before the probabilistic reduction

    type Text_Buffer is array ( 0..String_buffer_size+Look_Ahead-1 ) of Byte;
    Text_Buf: Text_Buffer;
    R: Natural;

    last_b: Symbol_range:= 0;

    -- Raw byte: post LZ77 / DLE coding, pre probabilistic reduction
    procedure Write_raw_byte( b: Byte ) is
      curr_b: constant Symbol_range:= Symbol_range(b);
      follo: Boolean;
    begin
      lz77_pos:= lz77_pos + 1;
      case phase is
        --
        when stats =>
          markov_d(last_b, curr_b):= markov_d(last_b, curr_b) + 1;
          LZ_cache.buf(LZ_cache.nxt):= b;
          LZ_cache.nxt:= LZ_cache.nxt + 1;
          LZ_cache.cnt:= Natural'Min(LZ_cache_size, LZ_cache.cnt + 1);
        when compress => -- Probabilistic reduction
          if Slen(last_b) = 0 then
            -- follower set is empty for this character
            Put_code(b, 8);
          else
            follo:= has_follower(last_b,curr_b);
            Put_code(1-Boolean'Pos(follo), 1);
            if follo then
              Put_code(Byte(follower_pos(last_b,curr_b)), B_Table( Slen(last_b) ));
            else
              Put_code(b, 8);
            end if;
          end if;
      end case;
      last_b:= curr_b;
      if phase = compress and then
         using_LZ77 and then
         (lz77_size - lz77_pos) < File_size_type(LZ_cache.cnt)
        -- We have entered the zone covered by the cache.
      then
        raise Derail_LZ77;
      end if;
    end Write_raw_byte;

    -- Write a normal, "clear-text", character
    procedure Write_normal_byte( b: Byte ) is
    begin
      Write_raw_byte(b);
      if b = DLE_code then
        -- disambiguate situation where the character happens to have
        -- the same 'Pos as the DLE code
        Write_raw_byte(0);
      end if;
      Text_Buf(R):= b;
      R:= (R+1) mod String_buffer_size;
    end Write_normal_byte;

    procedure Write_DL_code( distance, length: Integer ) is
      Copy_start: constant Natural:= (R - distance) mod String_buffer_size;
      len: constant Integer:= length - 3;
      dis: constant Integer:= distance - 1;
      dis_upper: Byte;
    begin
      if distance in Distance_range and length in Length_range then
        Write_raw_byte(DLE_code);
        dis_upper:= Byte((dis / 256) * upper_shift);
        -- Encode length and upper part of distance
        if len < maximum_len_1 then
          Write_raw_byte(Byte(len) + dis_upper);
        else
          Write_raw_byte(maximum_len_1_b + dis_upper);
          Write_raw_byte(Byte(len - maximum_len_1));
        end if;
        -- Encode distance
        Write_raw_byte(Byte(dis mod 256));
        -- Expand in the circular text buffer to have it up to date
        for K in 0..length-1 loop
          Text_Buf(R):= Text_Buf((Copy_start+K) mod String_buffer_size);
          R:= (R+1) mod String_buffer_size;
        end loop;
      else
        -- Cannot encode this distance-length pair, then expand to output :-(
        -- if phase= compress then Put("Aie! (" & distance'img & length'img & ")"); end if;
        for K in 0..length-1 loop
          Write_normal_byte( Text_Buf((Copy_start+K) mod String_buffer_size) );
        end loop;
      end if;
    end Write_DL_code;

    procedure My_LZ77 is
      new LZ77(
        String_buffer_size, Look_Ahead, Threshold,
        Read_byte, More_bytes,
        Write_normal_byte, Write_DL_code
      );

    procedure Finish_Cache is
      i: LZ_buffer_range:= LZ_buffer_range(lz77_pos mod LZ_cache_size);
    begin
      while lz77_pos < lz77_size loop
        Write_raw_byte(LZ_cache.buf(i));
        i:= i + 1;
      end loop;
    end Finish_Cache;

  begin -- Encode
    Read_Block;
    R:= String_buffer_size-Look_Ahead;
    Bytes_in := 0;
    if input_size_known then
      X_Percent := Integer(input_size / 40);
    else
      X_Percent := 0;
    end if;
    using_LZ77:= True;
    My_LZ77;
  exception
    when Derail_LZ77 =>
      using_LZ77:= False;
      Finish_Cache;
      if feedback /= null then
        feedback(100, False, user_aborting);
      end if;
  end Encode;

  procedure Build_stats is new Encode(phase => stats);
  procedure Compress    is new Encode(phase => compress);

  mem: Integer;

begin
  OutBufIdx := 1;
  output_size:= 0;
  mem:= Index(input);
  -- Pass 1: statistics to calibrate the probabilistic expansion
  Build_stats;
  Set_Index(input, mem); -- go back to beginning of message to compress
  Build_Followers;
  -- Pass 2: actual compression
  Save_byte := 0; --  Initialize output bit buffer
  Bits_used := 0;
  Save_Followers;
  lz77_size:= lz77_pos;
  lz77_pos:= 0;
  Compress;
  Flush_output;
  compression_ok:= True;
exception
  when Compression_unefficient =>
    compression_ok:= False;
end Zip.Compress.Reduce;
