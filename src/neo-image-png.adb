
--                                                                                                                    
--                                                              N E O  E N G I N E                                                    
--                                                                                                                    
--                                                      Copyright (C) 2016 Justin Squirek                                          
-- 
-- Neo is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any later version. 
--                                                                                                                    
-- Neo is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.         
--                                                                                                                    
-- You should have received a copy of the GNU General Public License along with Neo. If not, see gnu.org/licenses     
--

separate (Neo.Image) package PNG is

  ------------
  -- Chunks --
  ------------

  type Chunk_Catagory_Kind is (Main_Catagory,           Aux_Catagory,               Public_Catagory,         Private_Catagory);
  type Main_Chunk_Kind     is (Header_Chunk,            Palette_Chunk,              Image_Data_Chunk,        Image_End_Chunk);
  type Aux_Chunk_Kind      is (Background_Chunk,        Chromaticity_Chunk,         Gamma_Chunk,             Histogram_Chunk,
                               ICC_Profile_Chunk,       International_Text_Chunk,   Physical_Size_Chunk,     Significan_Bits_Chunk,
                               Suggested_Palette_Chunk, SRGB_Chunk,                 Text_Chunk,              Time_Modified_Chunk,
                               Transparency_Chunk,      Final_Text_Chunk);
  type Public_Chunk_Kind   is (Offset_Chunk,            Physical_Calibration_Chunk, Physical_Scale_Chunk,    Stereographic_Chunk,     
                               GIF_Graphic_Chunk,       GIF_Extension_Chunk,        Fractal_Chunk);          
  type Private_Chunk_Kind  is (Virtual_Page_Chunk,      Suggested_Palette_Chunk,    Fireworks_prVw_Chunk,    Unknown_cmOD_Chunk,      
                               Unknown_cmPP_Chunk,      Unknown_cpIp_Chunk,         Fireworks_mkBF_Chunk,    Fireworks_mkBS_Chunk,            
                               Fireworks_mkBT_Chunk,    Fireworks_mkTS_Chunk,       Unknown_pcLb_Chunk);
  for Main_Chunk_Kind'Image    use (Header_Chunk            => "IHDR", Palette_Chunk              => "PLTE", Image_Data_Chunk     => "IDAT", Image_End_Chunk       => "IEND",
  for Aux_Chunk_Kind'Image     use (Background_Chunk        => "bKGD", Chromaticity_Chunk         => "cHRM", Gamma_Chunk          => "gAMA", Histogram_Chunk       => "hIST",
                                    ICC_Profile_Chunk       => "iCCP", International_Text_Chunk   => "iTXt", Physical_Size_Chunk  => "pHYs", Significan_Bits_Chunk => "sBIT",
                                    Suggested_Palette_Chunk => "sPLT", SRGB_Chunk                 => "sRGB", Text_Chunk           => "tEXt", Time_Modified_Chunk   => "tIME",
                                    Transparency_Chunk      => "tRNS", Final_Text_Chunk           => "zTXt");
  for Public_Chunk_Kind'Image  use (Offset_Chunk            => "oFFs", Physical_Calibration_Chunk => "pCAL", Physical_Scale_Chunk => "sCAL", Stereographic_Chunk   => "sTER",
                                    GIF_Graphic_Chunk       => "gIFg", GIF_Extension_Chunk        => "gIFx", Fractal_Chunk        => "fRAc");
  for Private_Chunk_Kind'Image use (Virtual_Page_Chunk      => "vpAg", Reduced_Palette_Chunk      => "spAL", Fireworks_prVw_Chunk => "prVw", Unknown_cmOD_Chunk    => "cmOD",
                                    Unknown_cmPP_Chunk      => "cmPP", Unknown_cpIp_Chunk         => "cpIp", Fireworks_mkBF_Chunk => "mkBF", Fireworks_mkBS_Chunk  => "mkBS",
                                    Fireworks_mkBT_Chunk    => "mkBT", Fireworks_mkTS_Chunk       => "mkTS", Unknown_pcLb_Chunk   => "pcLb");
  type Chunk_head (Kind : Chunk_Catagory_Kind := Main_Catagory) is record
      Length : Int_32_Unsigned;
      case Kind is
        when Main_Catagory    => Main_Chunk    : Main_Chunk_Kind;
        when Aux_Catagory     => Aux_Chunk     : Aux_Chunk_Kind;
        when Public_Catagory  => Public_Chunk  : Public_Chunk_Kind;
        when Private_Catagory => Private_Chunk : Private_Chunk_Kind;
      end case;
    end record;

  ------------
  -- Header --
  ------------

  type Header_State is new Controlled with record
      Signature      : Str (1..3);
      Chunk          :
      Width          : Int_32_Unsigned;
      Height         : Int_32_Unsigned;
      Bits_Per_Pixel : Byte;
      Color          : Byte;
      Deflate_Method : Byte;
      Filtering      : Byte;
      Interlaced     : Byte;
    end record;

  procedure Initialize (Header : in out Header_State) is
    begin
      Assert (Header.Signature = "PNG");
      Assert (Header.Width /= 0 and Header.Height /= 0);
      Assert (Header.Defalte_Method = 0);
      Assert (Header.Filtering = 0);
      Assert (Header.Bits_Per_Pixel in (case Color_Kind'Pos (Header.Color) is
                                          when Greyscale_Color        => (1, 2, 4, 8, 16)
                                          when RGB_With_Palette_Color => (1, 2, 4, 8)
                                          when others                 => (8, 16)));
    end;

  -------------
  -- Huffman --
  -------------

  type Length_Code_Pair is record
      Length: Int_32_Natural;
      Code  : Int_32_Natural;
    end record;
  type Huffman_Node is record
      Value      : Int_32_Natural;
      Index_Zero : Int_32_Natural;
      Index_One  : Int_32_Natural; 
    end record;
  type Huff_Node_List is array (1..800) of Huffman_Node;
  type Huffman_Descriptor is array (Int_32_Natural) of Length_Code_Pair;
  type Huffman_Tree is record
      Last : Natural;
      Node : Huffman_Node_list;
    end record;
  procedure Build (Tree : out Huff_tree; Descriptor : Huffman_Descriptor) is
    curr, alloc: Natural;
    code, mask: Unsigned_32;
    begin
      alloc:= root;
      for i in descr'Range loop
        if descr(i).length > 0 then
          curr:= root;
          code:= Unsigned_32(descr(i).code);
          mask:= Shift_Left(Unsigned_32'(1), descr(i).length-1);
          for j in 0..descr(i).length-1 loop
            if (code and mask) /= 0 then
              if t.node(curr).one = nil then
                alloc:= alloc + 1;
                t.node(curr).one:= alloc;
              end if;
              curr:= t.node(curr).one;
            else
              if t.node(curr).zero = nil then
                alloc:= alloc + 1;
                t.node(curr).zero:= alloc;
              end if;
              curr:= t.node(curr).zero;
            end if;
            mask:= Shift_Right(mask, 1);
          end loop;
          t.node(curr).n:= i;
        end if;
      end loop;
      t.last:= alloc;
    end;

  ---------
  -- CRC --
  ---------

  CRC32_Table : array( Unsigned_32'(0)..255 ) of Unsigned_32;
  procedure Prepare_table is
    -- CRC-32 algorithm, ISO-3309
    Seed: constant:= 16#EDB88320#;
    l: Unsigned_32;
  begin
    for i in CRC32_Table'Range loop
      l:= i;
      for bit in 0..7 loop
        if (l and 1) = 0 then
          l:= Shift_Right(l,1);
        else
          l:= Shift_Right(l,1) xor Seed;
        end if;
      end loop;
      CRC32_Table(i):= l;
    end loop;
  end Prepare_table;
  procedure Update( CRC: in out Unsigned_32; InBuf: Byte_array ) is
    local_CRC: Unsigned_32;
  begin
    local_CRC:= CRC ;
    for i in InBuf'Range loop
      local_CRC :=
        CRC32_Table( 16#FF# and ( local_CRC xor Unsigned_32( InBuf(i) ) ) )
        xor
        Shift_Right( local_CRC , 8 );
    end loop;
    CRC:= local_CRC;
  end Update;
  table_empty: Boolean:= True;
  procedure Init( CRC: out Unsigned_32 ) is
  begin
    if table_empty then
      Prepare_table;
      table_empty:= False;
    end if;
    CRC:= 16#FFFF_FFFF#;
  end Init;
  function Final( CRC: Unsigned_32 ) return Unsigned_32 is
  begin
    return not CRC;
  end Final;

  -------------
  -- Inflate --
  -------------  

  --  Size of sliding dictionary and circular output buffer
  wsize: constant:= 16#10000#;
  -- I/O Buffers
  -- > Sliding dictionary for unzipping, and output buffer as well
  slide: Byte_array( 0..wsize );
  slide_index: Integer:= 0; -- Current Position in slide
  Zip_EOF  : constant Boolean:= False;
  crc32val : Unsigned_32;  -- crc calculated from data
  procedure Init_Buffers;
  procedure Read_raw_byte ( bt : out U8 );
    pragma Inline(Read_raw_byte);
  package Bit_buffer is
    procedure Init;
    -- Read at least n bits into the bit buffer, returns the n first bits
    function Read ( n: Natural ) return Integer;
      pragma Inline(Read);
    function Read_U32 ( n: Natural ) return Unsigned_32;
      pragma Inline(Read_U32);
    -- Dump n bits no longer needed from the bit buffer
    procedure Dump ( n: Natural );
      pragma Inline(Dump);
    procedure Dump_to_byte_boundary;
    function Read_and_dump( n: Natural ) return Integer;
      pragma Inline(Read_and_dump);
    function Read_and_dump_U32( n: Natural ) return Unsigned_32;
      pragma Inline(Read_and_dump_U32);
  end Bit_buffer;
  procedure Flush ( x: Natural ); -- directly from slide to output stream
  procedure Flush_if_full(W: in out Integer);
    pragma Inline(Flush_if_full);
  procedure Copy(
    distance, length:        Natural;
    index           : in out Natural );
  pragma Inline(Copy);
  deflate_e_mode: constant Boolean:= False;
  procedure Inflate;

  procedure Init_Buffers is
  begin
    slide_index := 0;
    Bit_buffer.Init;
    CRC32.Init( crc32val );
  end Init_Buffers;

  procedure Read_raw_byte ( bt : out U8 ) is
  begin
    if ch.length = 0 then
      -- We hit the end of a PNG 'IDAT' chunk, so we go to the next one
      -- - in petto, it's strange design, but well...
      -- This "feature" has taken some time (and nerves) to be addressed.
      -- Incidentally, I have reprogrammed the whole Huffman
      -- decoding, and looked at many other wrong places to solve
      -- the mystery.
      -- Out of some intelligent design, there might be an IDAT chunk
      -- boundary anywhere inside the zlib compressed block...
      dummy: U32;
      begin
      Big_endian(image.buffer, dummy); -- ending chunk's CRC
      -- New chunk begins here.
      loop
        Read(image, ch);
        exit when ch.kind /= IDAT or ch.length > 0;
      end loop;
      if ch.kind /= IDAT then
        Raise_Exception(
          error_in_image_data'Identity,
          "PNG additional data chunk must be an IDAT"
        );
      end if;
    end if;
    Buffering.Get_Byte(image.buffer, bt);
    ch.length:= ch.length - 1;
  end Read_raw_byte;

    B : Unsigned_32;
    K : Integer;

    procedure Init is
    begin
      B := 0;
      K := 0;
    end Init;

    procedure Need( n : Natural ) is
      pragma Inline(Need);
      bt: U8;
    begin
      while K < n loop
        Read_raw_byte( bt );
        B:= B or Shift_Left( Unsigned_32( bt ), K );
        K:= K + 8;
      end loop;
    end Need;

    procedure Dump ( n : Natural ) is
    begin
      B := Shift_Right(B, n );
      K := K - n;
    end Dump;

    procedure Dump_to_byte_boundary is
    begin
      Dump ( K mod 8 );
    end Dump_to_byte_boundary;

    function Read_U32 ( n: Natural ) return Unsigned_32 is
    begin
      Need(n);
      return B and (Shift_Left(1,n) - 1);
    end Read_U32;

    function Read ( n: Natural ) return Integer is
    begin
      return Integer(Read_U32(n));
    end Read;

    function Read_and_dump( n: Natural ) return Integer is
      res: Integer;
    begin
      res:= Read(n);
      Dump(n);
      return res;
    end Read_and_dump;

    function Read_and_dump_U32( n: Natural ) return Unsigned_32 is
      res: Unsigned_32;
    begin
      res:= Read_U32(n);
      Dump(n);
      return res;
    end Read_and_dump_U32;

    old_bytes: Natural:= 0;
    -- how many bytes to be resent from last Inflate output
    byte_mem: Byte_array(1..8);

    procedure Flush ( x: Natural ) is
      use Ada.Streams;
    begin
      if full_trace then
        Ada.Text_IO.Put("[Flush..." & Integer'Image(x));
      end if;
      CRC32.Update(crc32val, slide( 0..x-1 ) );
      if old_bytes > 0 then
        declare
          app: constant Byte_array:=
            byte_mem(1..old_bytes) & slide(0..x-1);
        begin
          Output_uncompressed(app, old_bytes);
          -- In extreme cases (x very small), we might have some of
          -- the rejected bytes from byte_mem.
          if old_bytes > 0 then
            byte_mem(1..old_bytes):= app(app'Last-(old_bytes-1)..app'Last);
          end if;
        end;
      else
        Output_uncompressed(slide(0..x-1), old_bytes);
        if old_bytes > 0 then
          byte_mem(1..old_bytes):= slide(x-old_bytes..x-1);
        end if;
      end if;
      if full_trace then
        Ada.Text_IO.Put_Line("finished]");
      end if;
    end Flush;

    procedure Flush_if_full(W: in out Integer) is
    begin
      if W = wsize then
        Flush(wsize);
        W:= 0;
      end if;
    end Flush_if_full;

    -- The copying routines:

    procedure Copy(
        distance, length:        Natural;
        index           : in out Natural )
    is
      source,part,remain: Integer;
    begin
      source:= index - distance;
      remain:= length;
      loop
        --Adjust_to_Slide(source,remain,part, index);
        source:= source mod wsize;
        -- source and index are now in 0..WSize-1
        if  source > index then
          part:= wsize-source;
        else
          part:= wsize-index;
        end if;
        -- NB: part is in 1..WSize (part cannot be 0)
        if part > remain then
          part:= remain;
        end if;
        -- Now part <= remain
        remain:= remain - part;
        -- NB: remain cannot be < 0
        --Copy_range(source, index, part);
        if abs (index - source) < amount then
          -- if source >= index, the effect of copy is
          -- just like the non-overlapping case
          for count in reverse 1..amount loop
            slide(index):= slide(source);
            index := index  + 1;
            source:= source + 1;
          end loop;
        else -- non-overlapping -> copy slice
          slide( index .. index+amount-1 ):=
            slide( source..source+amount-1 );
          index := index  + amount;
          source:= source + amount;
        end if;
        Flush_if_full(index);
        exit when remain = 0;
      end loop;
    end Copy;


    --------[ Method: Inflate ]--------
      CT     : p_HufT_table;  -- current table
      CT_idx : Integer;       -- current table index
      length : Natural;
      E      : Integer;      -- table entry flag/number of extra bits
      W      : Integer:= slide_index;
      -- more local variable for slide index
    begin
      if full_trace then
        Ada.Text_IO.Put_Line("Begin Inflate_codes");
      end if;

      -- inflate the coded data
      main_loop:
      while not Zip_EOF loop
        CT:= Tl.table;
        CT_idx:= UnZ_IO.Bit_buffer.Read(Bl);

        loop
          E := CT(CT_idx).extra_bits;
          exit when E <= 16;
          if E = invalid then
            raise error_in_image_data;
          end if;

          -- then it's a literal
          UnZ_IO.Bit_buffer.Dump( CT(CT_idx).bits );
          E:= E - 16;
          CT:= CT(CT_idx).next_table;
          CT_idx := UnZ_IO.Bit_buffer.Read(E);
        end loop;

        UnZ_IO.Bit_buffer.Dump ( CT(CT_idx).bits );

        case E is
          when 16 =>     -- CTE.N is a Litteral
            slide ( W ) :=  U8( CT(CT_idx).n );
            W:= W + 1;
            UnZ_IO.Flush_if_full(W);

          when 15 =>     -- End of block (EOB)
            if full_trace then
              Ada.Text_IO.Put_Line("Exit  Inflate_codes, e=15 EOB");
            end if;
            exit main_loop;

          when others => -- We have a length/distance

            -- Get length of block to copy:
            length:= CT(CT_idx).n + UnZ_IO.Bit_buffer.Read_and_dump(E);

            -- Decode distance of block to copy:
            CT:= Td.table;
            CT_idx := UnZ_IO.Bit_buffer.Read(Bd);
            loop
              E := CT(CT_idx).extra_bits;
              exit when E <= 16;
              if E = invalid then
                raise error_in_image_data;
              end if;
              UnZ_IO.Bit_buffer.Dump( CT(CT_idx).bits );
              E:= E - 16;
              CT:= CT(CT_idx).next_table;
              CT_idx := UnZ_IO.Bit_buffer.Read(E);
            end loop;
            UnZ_IO.Bit_buffer.Dump( CT(CT_idx).bits );

            UnZ_IO.Copy(
              distance => CT(CT_idx).n + UnZ_IO.Bit_buffer.Read_and_dump(E),
              length   => length,
              index    => W
            );
        end case;
      end loop main_loop;

      slide_index:= W;

      if full_trace then
        Ada.Text_IO.Put_Line("End   Inflate_codes");
      end if;
    end Inflate_Codes;



        -- Copy lengths for literal codes 257..285

        copy_lengths_literal : Length_array( 0..30 ) :=
             (  3,  4,  5,  6,  7,  8,  9, 10, 11, 13, 15, 17, 19, 23, 27, 31,
               35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258, 0, 0 );

        -- Extra bits for literal codes 257..285

        extra_bits_literal : Length_array( 0..30 ) :=
               ( 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2,
                 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0, invalid, invalid );

        -- Copy offsets for distance codes 0..29 (30..31: deflate_e)

        copy_offset_distance : constant Length_array( 0..31 ) :=
             ( 1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193,
               257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145,
               8193, 12289, 16385, 24577, 32769, 49153 );

        -- Extra bits for distance codes

        extra_bits_distance : constant Length_array( 0..31 ) :=
             ( 0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
               7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14 );

        max_dist: Integer:= 29; -- changed to 31 for deflate_e

    procedure Inflate is
      is_last_block: Boolean;
      blocks: Positive:= 1;
    begin
      if deflate_e_mode then
        copy_lengths_literal(28):= 3; -- instead of 258
        extra_bits_literal(28):= 16;  -- instead of 0
        max_dist:= 31;
      end if;
      loop
        Inflate_Block ( is_last_block );
        last_block:= Boolean'Val(UnZ_IO.Bit_buffer.Read_and_dump(1));
        case UnZ_IO.Bit_buffer.Read_and_dump(2) is -- Block type = 0,1,2,3
          when 0 =>      Inflate_stored_block;
            N : Integer;
            if full_trace then
              Ada.Text_IO.Put_Line("Begin Inflate_stored_block");
            end if;
            UnZ_IO.Bit_buffer.Dump_to_byte_boundary;

            -- Get the block length and its complement
            N:= UnZ_IO.Bit_buffer.Read_and_dump( 16 );
            if  N /= Integer(
             (not UnZ_IO.Bit_buffer.Read_and_dump_U32(16))
             and 16#ffff#)
            then
              raise error_in_image_data;
            end if;
            while N > 0  and then not Zip_EOF loop
              -- Read and output the non-compressed data
              N:= N - 1;
              slide ( slide_index ) :=
                U8( UnZ_IO.Bit_buffer.Read_and_dump(8) );
              slide_index:= slide_index + 1;
              UnZ_IO.Flush_if_full(slide_index);
            end loop;
            if full_trace then
              Ada.Text_IO.Put_Line("End   Inflate_stored_block");
            end if;
          when 1 =>      Inflate_fixed_block; declare
            Tl,                        -- literal/length code table
              Td : p_Table_list;            -- distance code table
            Bl, Bd : Integer;          -- lookup bits for tl/bd
            huft_incomplete : Boolean;

            -- length list for HufT_build (literal table)
            L: constant Length_array( 0..287 ):=
              ( 0..143=> 8, 144..255=> 9, 256..279=> 7, 280..287=> 8);

          begin
            if full_trace then
              Ada.Text_IO.Put_Line("Begin Inflate_fixed_block");
            end if;

            -- make a complete, but wrong code set
            Bl := 7;
            HufT_build(
              L, 257, copy_lengths_literal, extra_bits_literal,
              Tl, Bl, huft_incomplete
            );

            -- Make an incomplete code set
            Bd := 5;
            begin
              HufT_build(
                (0..max_dist => 5), 0,
                copy_offset_distance, extra_bits_distance,
                Td, Bd, huft_incomplete
              );
              if huft_incomplete then
                if full_trace then
                  Ada.Text_IO.Put_Line(
                    "td is incomplete, pointer=null: " &
                    Boolean'Image(Td=null)
                  );
                end if;
              end if;
            exception
              when huft_out_of_memory | huft_error =>
                HufT_free( Tl );
                raise error_in_image_data;
            end;

            Inflate_Codes ( Tl, Td, Bl, Bd );

            HufT_free ( Tl );
            HufT_free ( Td );

            if full_trace then
              Ada.Text_IO.Put_Line("End   Inflate_fixed_block");
            end if;
          when 2 =>  --    Inflate_dynamic_block;
            bit_order : constant array ( 0..18 ) of Natural :=
             ( 16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15 );

            Lbits : constant:= 9;
            Dbits : constant:= 6;

            current_length: Natural:= 0;
            defined, number_of_lengths: Natural;

            Tl,                             -- literal/length code tables
              Td : p_Table_list;            -- distance code tables

            CT_dyn_idx : Integer;  -- current table element

            Bl, Bd : Integer;                  -- lookup bits for tl/bd
            Nb : Natural;  -- number of bit length codes
            Nl : Natural;  -- number of literal length codes
            Nd : Natural;  -- number of distance codes

            -- literal/length and distance code lengths
            Ll: Length_array( 0 .. 288+32-1 ):= (others=> 0);

            huft_incomplete : Boolean;

            procedure Repeat_length_code( amount: Natural ) is
            begin
              if defined + amount > number_of_lengths then
                raise error_in_image_data;
              end if;
              for c in reverse 1..amount loop
                Ll ( defined ) := Natural_M32(current_length);
                defined:= defined + 1;
              end loop;
            end Repeat_length_code;

          begin
            if full_trace then
              Ada.Text_IO.Put_Line("Begin Inflate_dynamic_block");
            end if;

            -- Read in table lengths
            Nl := 257 + UnZ_IO.Bit_buffer.Read_and_dump(5);
            Nd :=   1 + UnZ_IO.Bit_buffer.Read_and_dump(5);
            Nb :=   4 + UnZ_IO.Bit_buffer.Read_and_dump(4);

            if Nl > 288 or else Nd > 32 then
              raise error_in_image_data;
            end if;

            -- Read in bit-length-code lengths.
            -- The rest, Ll( Bit_Order( Nb .. 18 ) ), is already = 0
            for J in  0 .. Nb - 1  loop
              Ll ( bit_order( J ) ) := Natural_M32(UnZ_IO.Bit_buffer.Read_and_dump(3));
            end loop;

            -- Build decoding table for trees--single level, 7 bit lookup
            Bl := 7;
            begin
              HufT_build (
                Ll( 0..18 ), 19, empty, empty, Tl, Bl, huft_incomplete
              );
              if huft_incomplete then
                HufT_free(Tl);
                raise error_in_image_data;
              end if;
            exception
              when others =>
                raise error_in_image_data;
            end;

            -- Read in literal and distance code lengths
            number_of_lengths := Nl + Nd;
            defined := 0;
            current_length := 0;

            while  defined < number_of_lengths  loop
              CT_dyn_idx:= UnZ_IO.Bit_buffer.Read(Bl);
              UnZ_IO.Bit_buffer.Dump( Tl.table(CT_dyn_idx).bits );

              case Tl.table(CT_dyn_idx).n is
                when 0..15 =>       -- length of code in bits (0..15)
                  current_length:= Tl.table(CT_dyn_idx).n;
                  Ll (defined) := Natural_M32(current_length);
                  defined:= defined + 1;

                when 16 =>          -- repeat last length 3 to 6 times
                  Repeat_length_code(3 + UnZ_IO.Bit_buffer.Read_and_dump(2));

                when 17 =>          -- 3 to 10 zero length codes
                  current_length:= 0;
                  Repeat_length_code(3 + UnZ_IO.Bit_buffer.Read_and_dump(3));

                when 18 =>          -- 11 to 138 zero length codes
                  current_length:= 0;
                  Repeat_length_code(11 + UnZ_IO.Bit_buffer.Read_and_dump(7));

                when others => null;

              end case;
            end loop;

            HufT_free ( Tl );        -- free decoding table for trees

            -- Build the decoding tables for literal/length codes
            Bl := Lbits;
            begin
              HufT_build (
                Ll( 0..Nl-1 ), 257,
                copy_lengths_literal, extra_bits_literal,
                Tl, Bl, huft_incomplete
              );
              if huft_incomplete then
                HufT_free(Tl);
                raise error_in_image_data;
              end if;
            exception
              when others =>
                raise error_in_image_data;
            end;

            -- Build the decoding tables for distance codes
            Bd := Dbits;
            begin
              HufT_build (
                Ll( Nl..Nl+Nd-1 ), 0,
                copy_offset_distance, extra_bits_distance,
                Td, Bd, huft_incomplete
              );
            exception
              when huft_out_of_memory | huft_error =>
                HufT_free(Tl);
                raise error_in_image_data;
            end;

            -- Decompress until an end-of-block code

            Inflate_Codes ( Tl, Td, Bl, Bd );
            HufT_free ( Tl );
            HufT_free ( Td );

          when others => raise error_in_image_data; -- Bad block type (3)
        end case;
        exit when is_last_block;
        blocks:= blocks+1;
      end loop;
      UnZ_IO.Flush( slide_index );
      slide_index:= 0;
      if some_trace then
        Ada.Text_IO.Put("# blocks:" & Integer'Image(blocks));
      end if;
      crc32val := CRC32.Final( crc32val );
    end Inflate;

  --------------
  -- Unfilter --
  --------------

  type Filter_method_0 is (None, Sub, Up, Average, Paeth);

  current_filter: Filter_method_0;

  function Unfilter (Filter : ; Data : Byte_Array) return Byte_Array is
    f, u
    a,b,c, p,pa,pb,pc,pr: Integer;
    j: Integer:= 0;
    begin
    -- Byte positions (f is the byte to be unfiltered):
    -- c b
    -- a f
  begin
    --
    -- !! find a way to have f99n0g04.png decoded correctly...
    --    seems a filter issue.
    --
    case current_filter is
      when None    =>
        -- Recon(x) = Filt(x)
        u:= f;
      when Sub     =>
        -- Recon(x) = Filt(x) + Recon(a)
        if x > 0 then
          for i in f'Range loop
            u(u'First+j):= f(i) + mem_row_bytes(curr_row)((x-1)*bytes_to_unfilter+j);
            j:= j + 1;
          end loop;
        else
          u:= f;
        end if;
      when Up      =>
        -- Recon(x) = Filt(x) + Recon(b)
        if y > 0 then
          for i in f'Range loop
            u(u'First+j):= f(i) + mem_row_bytes(1-curr_row)(x*bytes_to_unfilter+j);
            j:= j + 1;
          end loop;
        else
          u:= f;
        end if;
      when Average =>
        -- Recon(x) = Filt(x) + floor((Recon(a) + Recon(b)) / 2)
        for i in f'Range loop
          if x > 0 then
            a:= Integer(mem_row_bytes(curr_row)((x-1)*bytes_to_unfilter+j));
          else
            a:= 0;
          end if;
          if y > 0 then
            b:= Integer(mem_row_bytes(1-curr_row)(x*bytes_to_unfilter+j));
          else
            b:= 0;
          end if;
          u(u'First+j):= U8((Integer(f(i)) + (a+b)/2) mod 256);
          j:= j + 1;
        end loop;
      when Paeth   =>
        -- Recon(x) = Filt(x) + PaethPredictor(Recon(a), Recon(b), Recon(c))
        for i in f'Range loop
          if x > 0 then
            a:= Integer(mem_row_bytes(curr_row)((x-1)*bytes_to_unfilter+j));
          else
            a:= 0;
          end if;
          if y > 0 then
            b:= Integer(mem_row_bytes(1-curr_row)(x*bytes_to_unfilter+j));
          else
            b:= 0;
          end if;
          if x > 0 and y > 0 then
            c:= Integer(mem_row_bytes(1-curr_row)((x-1)*bytes_to_unfilter+j));
          else
            c:= 0;
          end if;
          p := a + b - c;
          pa:= abs(p - a);
          pb:= abs(p - b);
          pc:= abs(p - c);
          if pa <= pb and then pa <= pc then
            pr:= a;
          elsif pb <= pc then
            pr:= b;
          else
            pr:= c;
          end if;
          u(u'First+j):= f(i) + U8(pr);
          j:= j + 1;
        end loop;
    end case;
    j:= 0;
    for i in u'Range loop
      mem_row_bytes(curr_row)(x*bytes_to_unfilter+j):= u(i);
      j:= j + 1;
    end loop;
    --  if u'Length /= bytes_to_unfilter then
    --    raise Constraint_Error;
    --  end if;
  end Unfilter_bytes;

  filter_stat: array(Filter_method_0) of Natural:= (others => 0);

  -------------
  -- Display --
  -------------

  procedure Display (data : Byte_array; reject : out Natural) is
    -- Display of pixels coded on 8 bits per channel in the PNG stream
    procedure Out_Pixel_8(br, bg, bb, ba: U8) is
      function Times_257(x: Primary_color_range) return Primary_color_range is
      begin
        return 16 * (16 * x) + x;  --  this is 257 * x, = 16#0101# * x
        --  Numbers 8-bit -> no OA warning at instanciation. Returns x if type Primary_color_range is mod 2**8.
      end;
    begin
      case Primary_color_range'Modulus is
        when 256 =>
          Put_Pixel(
            Primary_color_range(br),
            Primary_color_range(bg),
            Primary_color_range(bb),
            Primary_color_range(ba)
          );
        when 65_536 =>
          Put_Pixel(
            Times_257(Primary_color_range(br)),
            Times_257(Primary_color_range(bg)),
            Times_257(Primary_color_range(bb)),
            Times_257(Primary_color_range(ba))
            -- Times_257 makes max intensity FF go to FFFF
          );
        when others =>
          raise invalid_primary_color_range;
      end case;
    end Out_Pixel_8;

    procedure Out_Pixel_Palette(ix: U8) is
    pragma Inline(Out_Pixel_Palette);
      color_idx: constant Natural:= Integer(ix);
    begin
      Out_Pixel_8(
        image.palette(color_idx).red,
        image.palette(color_idx).green,
        image.palette(color_idx).blue,
        255
      );
    end Out_Pixel_Palette;

    -- Display of pixels coded on 16 bits per channel in the PNG stream
    procedure Out_Pixel_16(br, bg, bb, ba: U16) is
    pragma Inline(Out_Pixel_16);
    begin
      case Primary_color_range'Modulus is
        when 256 =>
          Put_Pixel(
            Primary_color_range(br / 256),
            Primary_color_range(bg / 256),
            Primary_color_range(bb / 256),
            Primary_color_range(ba / 256)
          );
        when 65_536 =>
          Put_Pixel(
            Primary_color_range(br),
            Primary_color_range(bg),
            Primary_color_range(bb),
            Primary_color_range(ba)
          );
        when others =>
          raise invalid_primary_color_range;
      end case;
    end Out_Pixel_16;

  procedure Inc_XY is
    xm, ym: Integer;
    begin
      if x < x_max then
        x:= x + 1;
        if interlaced then
          -- Position of pixels depending on pass:
          -- 1 6 4 6 2 6 4 6
          -- 7 7 7 7 7 7 7 7
          -- 5 6 5 6 5 6 5 6
          -- 7 7 7 7 7 7 7 7
          -- 3 6 4 6 3 6 4 6
          -- 7 7 7 7 7 7 7 7
          -- 5 6 5 6 5 6 5 6
          -- 7 7 7 7 7 7 7 7
          case pass is
            when 1 =>
             Set_X_Y(    x*8, Y_range'Last     - y*8);
            when 2 =>
             Set_X_Y(4 + x*8, Y_range'Last     - y*8);
            when 3 =>
             Set_X_Y(    x*4, Y_range'Last - 4 - y*8);
            when 4 =>
             Set_X_Y(2 + x*4, Y_range'Last     - y*4);
            when 5 =>
             Set_X_Y(    x*2, Y_range'Last - 2 - y*4);
            when 6 =>
             Set_X_Y(1 + x*2, Y_range'Last     - y*2);
            when 7 => null;
          end case;
        end if;
      elsif y < y_max then
        x:= X_range'First; -- New row
        y:= y + 1;
        curr_row:= 1-curr_row; -- swap row index for filtering
        if not interlaced then
          Feedback((y*100)/image.height);
        end if;
      elsif interlaced then -- last row has beed displayed
        x:= X_range'First; -- New row
        for I in Pass..7 loop
          y:= 0;
          case pass is
            when 1 => null;
            when 2 =>
              xm:= (image.width+3)/8 - 1;
              ym:= (image.height+7)/8 - 1;
            when 3 =>
              xm:= (image.width+3)/4 - 1;
              ym:= (image.height+3)/8 - 1;
            when 4 =>
              xm:= (image.width+1)/4 - 1;
              ym:= (image.height+3)/4 - 1;
            when 5 =>
              xm:= (image.width+1)/2 - 1;
              ym:= (image.height+1)/4 - 1;
            when 6 =>
              xm:= (image.width  )/2 - 1;
              ym:= (image.height+1)/2 - 1;
            when 7 =>
              xm:= image.width - 1;
              ym:= image.height/2 - 1;
          end case;
          if xm >=0 and xm <= X_range'Last and ym in Y_range then
            -- This pass is not empty (otherwise, we will continue
            -- to the next one, if any).
            x_max:= xm;
            y_max:= ym;
            exit;
          end if;
        end loop;
      end if;
    end Inc_XY;

    uf: Byte_array(0..15); -- unfiltered bytes for a pixel
    w1, w2: U16;
    i: Integer;

  begin
    -- Depending on the row size, bpp, etc., we can have
    -- several rows, or less than one, being displayed
    -- with the present uncompressed data batch.
    --
    i:= data'First;
    if i > data'Last then
      reject:= 0;
      return; -- data is empty, do nothing
    end if;
    --
    -- Main loop over data
    --
    loop
      if x = X_range'First then -- pseudo-column for filter method
        exit when i > data'Last;
        begin
          current_filter:= Filter_method_0'Val(data(i));
          if some_trace then
            filter_stat(current_filter):= filter_stat(current_filter) + 1;
          end if;
        exception
          when Constraint_Error =>
            Raise_Exception(
              error_in_image_data'Identity,
              "PNG: wrong filter code, row #" &
              Integer'Image(y) & " code:" & U8'Image(data(i))
            );
        end;
        if interlaced then
          case pass is
            when 1..6 =>
              null; -- Set_X_Y for each pixel
            when 7 =>
              Set_X_Y(0, Y_range'Last - 1 - y*2);
          end case;
        else
          Set_X_Y(0, Y_range'Last - y);
        end if;
        i:= i + 1;
      else -- normal pixel
        --
        -- We quit the loop if all data has been used (except for an
        -- eventual incomplete pixel)
        exit when i > data'Last - (bytes_to_unfilter - 1);
        -- NB, for per-channel bpp < 8:
        -- 7.2 Scanlines - some low-order bits of the
        -- last byte of a scanline may go unused.
        case subformat_id is
          when 0 =>
            -----------------------
            -- Type 0: Greyscale --
            -----------------------
            case bits_per_pixel is
              when 1 | 2 | 4  =>
                Unfilter_bytes(data(i..i), uf(0..0));
                i:= i + 1;
                declare
                  b: U8;
                  shift: Integer:= 8 - bits_per_pixel;
                  max: constant U8:= U8(Shift_Left(Unsigned_32'(1), bits_per_pixel)-1);
                  -- Scaling factor to obtain the correct color value on a 0..255 range.
                  -- The division is exact in all cases (bpp=8,4,2,1),
                  -- since 255 = 3 * 5 * 17 and max = 255, 15, 3 or 1.
                  -- This factor ensures: 0 -> 0, max -> 255
                  factor: constant U8:= 255 / max;
                begin
                  -- loop through the number of pixels in this byte:
                  for k in reverse 1..8/bits_per_pixel loop
                    b:= (max and U8(Shift_Right(Unsigned_8(uf(0)), shift))) * factor;
                    shift:= shift - bits_per_pixel;
                    Out_Pixel_8(b, b, b, 255);
                    exit when x >= x_max or k = 1;
                    Inc_XY;
                  end loop;
                end;
              when 8 =>
                -- NB: with bpp as generic param, this case could be merged
                -- into the general 1,2,4[,8] case without loss of performance
                -- if the compiler is smart enough to simplify the code, given
                -- the value of bits_per_pixel.
                -- But we let it here for two reasons:
                --   1) a compiler might be not smart enough
                --   2) it is a very simple case, perhaps helpful for
                --      understanding the algorithm.
                Unfilter_bytes(data(i..i), uf(0..0));
                i:= i + 1;
                Out_Pixel_8(uf(0), uf(0), uf(0), 255);
              when 16 =>
                Unfilter_bytes(data(i..i+1), uf(0..1));
                i:= i + 2;
                w1:= U16(uf(0)) * 256 + U16(uf(1));
                Out_Pixel_16(w1, w1, w1, 65535);
            when others => null; end case;
          when 2 =>
            -----------------
            -- Type 2: RGB --
            -----------------
            case bits_per_pixel is
              when 24 =>
                Unfilter_bytes(data(i..i+2), uf(0..2));
                i:= i + 3;
                Out_Pixel_8(uf(0), uf(1), uf(2), 255);
              when 48 =>
                Unfilter_bytes(data(i..i+5), uf(0..5));
                i:= i + 6;
                Out_Pixel_16(
                  U16(uf(0)) * 256 + U16(uf(1)),
                  U16(uf(2)) * 256 + U16(uf(3)),
                  U16(uf(4)) * 256 + U16(uf(5)),
                  65_535
                );
              when others =>
                null;
            end case;
          when 3 =>
            ------------------------------
            -- Type 3: RGB with palette --
            ------------------------------
            Unfilter_bytes(data(i..i), uf(0..0));
            i:= i + 1;
            case bits_per_pixel is
              when 1 | 2 | 4 =>
                declare
                  shift: Integer:= 8 - bits_per_pixel;
                  max: constant U8:= U8(Shift_Left(Unsigned_32'(1), bits_per_pixel)-1);
                begin
                  -- loop through the number of pixels in this byte:
                  for k in reverse 1..8/bits_per_pixel loop
                    Out_Pixel_Palette(max and U8(Shift_Right(Unsigned_8(uf(0)), shift)));
                    shift:= shift - bits_per_pixel;
                    exit when x >= x_max or k = 1;
                    Inc_XY;
                  end loop;
                end;
              when 8 =>
                -- Same remark for this case (8bpp) as
                -- within Image Type 0 / Greyscale above
                Out_Pixel_Palette(uf(0));
              when others =>
                null;
            end case;
          when 4 =>
            -------------------------------
            -- Type 4: Greyscale & Alpha --
            -------------------------------
            case bits_per_pixel is
              when 16 =>
                Unfilter_bytes(data(i..i+1), uf(0..1));
                i:= i + 2;
                Out_Pixel_8(uf(0), uf(0), uf(0), uf(1));
              when 32 =>
                Unfilter_bytes(data(i..i+3), uf(0..3));
                i:= i + 4;
                w1:= U16(uf(0)) * 256 + U16(uf(1));
                w2:= U16(uf(2)) * 256 + U16(uf(3));
                Out_Pixel_16(w1, w1, w1, w2);
              when others =>
                null; -- undefined in PNG standard
            end case;
          when 6 =>
            ------------------
            -- Type 6: RGBA --
            ------------------
            case bits_per_pixel is
              when 32 =>
                Unfilter_bytes(data(i..i+3), uf(0..3));
                i:= i + 4;
                Out_Pixel_8(uf(0), uf(1), uf(2), uf(3));
              when 64 =>
                Unfilter_bytes(data(i..i+7), uf(0..7));
                i:= i + 8;
                Out_Pixel_16(
                  U16(uf(0)) * 256 + U16(uf(1)),
                  U16(uf(2)) * 256 + U16(uf(3)),
                  U16(uf(4)) * 256 + U16(uf(5)),
                  U16(uf(6)) * 256 + U16(uf(7))
                );
          when others => null; end case;
        when others => null; end case;
      end if;
      Inc_XY;
    end loop;
    -- i is between data'Last-(bytes_to_unfilter-2) and data'Last+1
    reject:= (data'Last + 1) - i;
  end;

  ----------
  -- Load --
  ----------

  function Load return Image_State is
    Result : Image_State;
    generic
      -- These values are invariant through the whole picture,
      -- so we can make them generic parameters. As a result, all
      -- "if", "case", etc. using them at the center of the decoding
      -- are optimized out at compile-time.
      interlaced        : Boolean;
      bits_per_pixel    : Positive;
      bytes_to_unfilter : Positive := Integer'Max(1, bits_per_pixel / 8);
      subformat_id      : Natural;
    procedure Load_specialized is
      subtype Mem_row_bytes_array is Byte_array(0..image.width*8);
      --
      mem_row_bytes: array(0..1) of Mem_row_bytes_array;
      -- We need to memorize two image rows, for un-filtering
      curr_row: Natural:= 1;
      -- either current is 1 and old is 0, or the reverse
      subtype X_range is Integer range -1..image.width-1;
      subtype Y_range is Integer range  0..image.height-1;
      -- X position -1 is for the row's filter methode code
      x: X_range:= X_range'First;
      y: Y_range:= Y_range'First;
      Max_Pixel : 
      x_max: X_range := (if Interlaced then (image.width+7)/8 - 1 else X_range'Last); -- for non-interlaced images: = X_range'Last
      pass: Positive range 1..7:= 1;
      b: U8;
      z_crc, dummy: U32;
    begin -- Load_specialized
      loop
        Read(image, ch);
        if Chunk.Kind = IEND or ch.length > 0 then
          case ch.kind is
            when IEND => exit main_chunk_loop;
            when IDAT => -- 11.2.4 IDAT Image data
              --
              -- NB: the compressed data may hold on several IDAT chunks.
              -- It means that right in the middle of compressed data, you
              -- can have a chunk crc, and a new IDAT header!...
              --
              UnZ_IO.Read_raw_byte(b); -- zlib compression method/flags code
              UnZ_IO.Read_raw_byte(b); -- Additional flags/check bits
              --
              UnZ_IO.Init_Buffers;
              -- ^ we indicate that we have a byte reserve of chunk's length,
              --   minus both zlib header bytes.
              UnZ_Meth.Inflate;
              z_crc:= 0;
              for i in 1..4 loop
                begin
                  UnZ_IO.Read_raw_byte(b);
                exception
                  when error_in_image_data =>
                    -- vicious IEND at the wrong place
                    -- basi4a08.png test image (corrupt imho)
                    exit main_chunk_loop;
                end;
                z_crc:= z_crc * 256 + U32(b);
              end loop;
              -- zlib Check value /= U32 (crc32val)
              -- This check fails despite good decompression is CRC init
              -- value different for zip and zlib? Is it Adler32?
              Big_endian(image.buffer, dummy);
            when tEXt => -- 11.3.4.3 tEXt Textual data
              for i in 1..ch.length loop
                Get_Byte(image.buffer, b);
                if some_trace then
                  if b=0 then -- separates keyword from message
                    Ada.Text_IO.New_Line;
                  else
                    Ada.Text_IO.Put(Character'Val(b));
                  end if;
                end if;
              end loop;
              Big_endian(image.buffer, dummy); -- chunk's CRC
            when others =>
              -- Skip chunk data and CRC
              for i in 1..ch.length + 4 loop
                Get_Byte(image.buffer, b);
              end loop;
          end case;
        end if;
      end loop;
    end;
  procedure Interlaced_1  is new Specialized (True,   1, 1, 3); -- Generics for optimization
  procedure Interlaced_2  is new Specialized (True,   2, 1 ,3);
  procedure Interlaced_4  is new Specialized (True,   4, 1, 3);
  procedure Interlaced_8  is new Specialized (True,   8, 1, 3);
  procedure Interlaced_24 is new Specialized (True,  24, 3, 2);
  procedure Interlaced_32 is new Specialized (True,  32, 4, 6);
  procedure Straight_1    is new Specialized (False,  1, 1, 3);
  procedure Straight_2    is new Specialized (False,  2, 1, 3);
  procedure Straight_4    is new Specialized (False,  4, 1, 3);
  procedure Straight_8    is new Specialized (False,  8, 1, 3);
  procedure Straight_24   is new Specialized (False, 24, 3, 2);
  procedure Straight_32   is new Specialized (False, 32, 4, 6);
  procedure General       is new Specialized (Interlaced        => image.interlaced,
                                              Bits_Per_Pixel    => image.bits_per_pixel,
                                              Bytes_To_Unfilter => Int_32'Max (1, image.bits_per_pixel / 8),
                                              Format            => image.subformat_id);
  Header : Header_State := File'Read (Header);
  Result : Image_State (Header.Width, Header.Height, )  := (
  begin
    ch: Chunk_head;
    n, dummy: U32;
    b, color_type: U8;
    palette: Boolean:= False;
    Buffering.Attach_Stream(image.buffer, image.stream);


    Read(image, ch);
    Assert (ch.kind = IHDR);
    Big_endian_buffered(image.buffer, n);
    Assert (Width /= 0);
    image.width:=  Natural(n);
    Big_endian_buffered(image.buffer, n);
    if n = 0 then
      Raise_Exception(
        error_in_image_data'Identity,
        "PNG image with zero height"
      );
    end if;
    image.height:= Natural(n);
    Get_Byte(image.buffer, b);
    image.bits_per_pixel:= Integer(b);
    Get_Byte(image.buffer, color_type);
    image.subformat_id:= Integer(color_type);
    case color_type is
      when 0 => -- Greyscale
        image.greyscale:= True;
        case image.bits_per_pixel is
          when 1 | 2 | 4 | 8 | 16 =>
            null;
          when others =>
            Raise_Exception(
              error_in_image_data'Identity,
              "PNG, type 0 (greyscale): wrong bit-per-channel depth"
            );
        end case;
      when 2 => -- RGB TrueColor
        case image.bits_per_pixel is
          when 8 | 16 =>
            image.bits_per_pixel:= 3 * image.bits_per_pixel;
          when others =>
            Raise_Exception(
              error_in_image_data'Identity,
              "PNG, type 2 (RGB): wrong bit-per-channel depth"
            );
        end case;
      when 3 => -- RGB with palette
        palette:= True;
        case image.bits_per_pixel is
          when 1 | 2 | 4 | 8 =>
            null;
          when others =>
            Raise_Exception(
              error_in_image_data'Identity,
              "PNG, type 3: wrong bit-per-channel depth"
            );
        end case;
      when 4 => -- Grey & Alpha
        image.greyscale:= True;
        image.transparency:= True;
        case image.bits_per_pixel is
          when 8 | 16 =>
            image.bits_per_pixel:= 2 * image.bits_per_pixel;
          when others =>
            Raise_Exception(
              error_in_image_data'Identity,
              "PNG, type 4 (Greyscale & Alpha): wrong bit-per-channel depth"
            );
        end case;
      when 6 => -- RGBA
        image.transparency:= True;
        case image.bits_per_pixel is
          when 8 | 16 =>
            image.bits_per_pixel:= 4 * image.bits_per_pixel;
          when others =>
            Raise_Exception(
              error_in_image_data'Identity,
              "PNG, type 6 (RGBA): wrong bit-per-channel depth"
            );
        end case;
      when others =>
        Raise_Exception(
          error_in_image_data'Identity,
          "Unknown PNG color type"
        );
    end case;
    Get_Byte(image.buffer, b);
    if b /= 0 then
      Raise_Exception(
        error_in_image_data'Identity,
        "Unknown PNG compression; ISO/IEC 15948:2003" &
        " knows only 'method 0' (deflate)"
      );
    end if;
    Get_Byte(image.buffer, b);
    if b /= 0 then
      Raise_Exception(
        error_in_image_data'Identity,
        "Unknown PNG filtering; ISO/IEC 15948:2003 knows only 'method 0'"
      );
    end if;
    Get_Byte(image.buffer, b);
    image.interlaced:= b = 1; -- Adam7
    Big_endian_buffered(image.buffer, dummy); -- Chunk's CRC
    if palette then
      loop
        Read(image, ch);
        case ch.kind is
          when IEND =>
            Raise_Exception(
              error_in_image_data'Identity,
              "PNG: there must be a palette, found IEND"
            );
          when PLTE =>
            if ch.length rem 3 /= 0 then
              Raise_Exception(
                error_in_image_data'Identity,
                "PNG: palette chunk byte length must be a multiple of 3"
              );
            end if;
            image.palette:= new Color_table(0..Integer(ch.length/3)-1);
            Color_tables.Load_palette(image);
            Big_endian_buffered(image.buffer, dummy); -- Chunk's CRC
            exit;
          when others =>
            -- skip chunk data and CRC
            for i in 1..ch.length + 4 loop
              Get_Byte(image.buffer, b);
            end loop;
        end case;
      end loop;
    end if;
    return (case Image.Format is
              when RGB_Format => (if Image.Bits_Per_Pixel = 24 and Image.Interlaced then Interlaced_24 else General)
              when Palette_Format => (case Image.Bits_Per_Pixel is
                                        when 1 => (if Image.Interlaced then Interlaced_1 else Straight_1)
                                        when 2 => (if Image.Interlaced then Interlaced_2 else Straight_2)
                                        when 4 => (if Image.Interlaced then Interlaced_4 else Straight_4)
                                        when 5 => (if Image.Interlaced then Interlaced_8 else Straight_8))
              when RGBA_Format => (if Image.Bits_Per_Pixel = 32 then
                                     (if Image.Interlaced then Interlaced_24 else Straignt_32)
                                   else General)
              when others => General);
  end;

    with procedure Set_X_Y (x, y: Natural);
    with procedure Put_Pixel (
      red, green, blue : Primary_color_range;
      alpha            : Primary_color_range
    );
