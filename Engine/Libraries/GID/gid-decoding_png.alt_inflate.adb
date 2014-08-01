-- ***********
--
-- Version with an alternative "Inflater" based on simple
-- Huffman tree types. Unfortunately
-- the same "wrong filter" errors, which appeared with
-- the UnZip decoder for some pictures, appear too, at the same
-- places. This leads think the bug is actually not with
-- this part of decompression. And other pictures fail, seemingly on
-- fixed "deflate" blocks, with that alternative. 5-Jun-2010.
--

-- A PNG stream is made of several "chunks" (see type PNG_Chunk_tag).
-- The image itself is contained in the IDAT chunk(s).
--
-- Steps for decoding an image (step numbers are from the ISO standard):
--
-- 10: Inflate deflated data; at each output buffer (slide),
--       process with step 9.
--  9: Read filter code (row begin), or unfilter bytes, go with step 8
--  8: Display pixels these bytes represent;
--       eventually, locate the interlaced image current point
--
with GID.Buffering, GID.Decoding_PNG.Huffman;

with Ada.Text_IO, Ada.Exceptions, Interfaces;

package body GID.Decoding_PNG is

  generic
    type Number is mod <>;
  procedure Big_endian_number(
    from : in out Input_buffer;
    n    :    out Number
  );
    pragma Inline(Big_endian_number);

  procedure Big_endian_number(
    from : in out Input_buffer;
    n    :    out Number
  )
  is
    b: U8;
  begin
    n:= 0;
    for i in 1..Number'Size/8 loop
      Buffering.Get_Byte(from, b);
      n:= n * 256 + Number(b);
    end loop;
  end Big_endian_number;

  procedure Big_endian is new Big_endian_number( U32 );

  use Ada.Exceptions;

  ----------
  -- Read --
  ----------

  procedure Read (image: in out image_descriptor; ch: out Chunk_head) is
    str4: String(1..4);
    b: U8;
  begin
    Big_endian(image.buffer, ch.length);
    for i in str4'Range loop
      Buffering.Get_Byte(image.buffer, b);
      str4(i):= Character'Val(b);
    end loop;
    begin
      ch.kind:= PNG_Chunk_tag'Value(str4);
      if some_trace then
        Ada.Text_IO.Put_Line('[' & str4 & "], length:" & U32'Image(ch.length));
      end if;
    exception
      when Constraint_Error =>
        Raise_exception(
          error_in_image_data'Identity,
          "PNG chunk: " &
          Integer'Image(Character'Pos(str4(1))) &
          Integer'Image(Character'Pos(str4(2))) &
          Integer'Image(Character'Pos(str4(3))) &
          Integer'Image(Character'Pos(str4(4)))
        );
    end;
  end Read;

  package CRC32 is

    use Interfaces;

    procedure Init( CRC: out Unsigned_32 );

    function  Final( CRC: Unsigned_32 ) return Unsigned_32;

    procedure Update( CRC: in out Unsigned_32; InBuf: Byte_array );
    pragma Inline( Update );

  end CRC32;

  package body CRC32 is

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

  end CRC32;

  ----------
  -- Load --
  ----------

  procedure Load (image: in out Image_descriptor) is

    -- !! these constants can be made generic parameters
    bits_per_pixel: constant Positive:= image.bits_per_pixel;
    subformat_id  : constant Natural := image.subformat_id;
    interlaced    : constant Boolean := image.interlaced;

    use GID.Buffering;

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

    x_max: X_range; -- for non-interlaced images: = X_range'Last
    y_max: Y_range; -- for non-interlaced images: = Y_range'Last

    pass: Positive range 1..7:= 1;

    -- Amount of bytes to unfilter at a time
    bytes_to_unfilter: constant Integer:= Integer'Max(1, bits_per_pixel / 8);

    --------------------------
    -- ** 9: Unfiltering ** --
    --------------------------

    type Filter_method_0 is (None, Sub, Up, Average, Paeth);

    current_filter: Filter_method_0;

    procedure Unfilter_bytes(
      f: in  Byte_array;  -- filtered
      u: out Byte_array   -- unfiltered
    )
    is
    pragma Inline(Unfilter_bytes);
      -- c b
      -- a f
      a,b,c,p,pa,pb,pc,pr: Integer;
      j: Integer:= 0;
    begin
      if full_trace and then x = 0 then
        Ada.Text_IO.Put_Line(
          Integer'Image(y) & ": " &
          Filter_method_0'Image(current_filter)
        );
      end if;
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
            u(u'First+j):= f(i) + U8((a+b)/2);
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
            if pa <= pb and pa <= pc then
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
      if u'Length /= bytes_to_unfilter then
        raise constraint_error;-- with "filter mismatch!";
      end if; -- !!
    end Unfilter_bytes;

    filter_stat: array(Filter_method_0) of Natural:= (others => 0);

    -- Output bytes from decompression
    --
    procedure Output_uncompressed(
      data  : in     Byte_array;
      reject:    out Natural
      -- amount of bytes to be resent here next time,
      -- in order to have a full multi-byte pixel
    )
    is
      -- Display of pixels coded on 8 bits per channel in the PNG stream
      procedure Out_Pixel_8(br, bg, bb, ba: U8) is
      pragma Inline(Out_Pixel_8);
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
              16#101# * Primary_color_range(br),
              16#101# * Primary_color_range(bg),
              16#101# * Primary_color_range(bb),
              16#101# * Primary_color_range(ba)
              -- 16#101# because max intensity FF goes to FFFF
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
      pragma Inline(Inc_XY);
      begin
        if x < x_max then
          x:= x + 1;
          if interlaced then
            -- Position of pixels depending on pass:
            --
            --   1 6 4 6 2 6 4 6
            --   7 7 7 7 7 7 7 7
            --   5 6 5 6 5 6 5 6
            --   7 7 7 7 7 7 7 7
            --   3 6 4 6 3 6 4 6
            --   7 7 7 7 7 7 7 7
            --   5 6 5 6 5 6 5 6
            --   7 7 7 7 7 7 7 7
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
              when 7 =>
                null; -- nothing to to, pixel are contiguous
            end case;
          end if;
        else
          x:= X_range'First; -- New row
          if y < y_max then
            y:= y + 1;
            curr_row:= 1-curr_row; -- swap row index for filtering
            if not interlaced then
              Feedback((y*100)/image.height);
            end if;
          elsif interlaced then -- last row has beed displayed
            if pass < 7 then
              pass:= pass + 1;
              y:= 0;
              case pass is
                when 1 =>
                  null;
                when 2 =>
                  x_max:= (image.width+3)/8 - 1;
                  y_max:= (image.height+7)/8 - 1;
                when 3 =>
                  x_max:= (image.width+3)/4 - 1;
                  y_max:= (image.height+3)/8 - 1;
                when 4 =>
                  x_max:= (image.width+1)/4 - 1;
                  y_max:= (image.height+3)/4 - 1;
                when 5 =>
                  x_max:= (image.width+1)/2 - 1;
                  y_max:= (image.height+1)/4 - 1;
                when 6 =>
                  x_max:= (image.width  )/2 - 1;
                  y_max:= (image.height+1)/2 - 1;
                when 7 =>
                  x_max:= image.width - 1;
                  y_max:= image.height/2 - 1;
              end case;
            end if;
          end if;
        end if;
      end Inc_XY;

      uf: Byte_array(0..15); -- unfiltered bytes for a pixel
      w1, w2: U16;
      i: Integer;

    begin
      if some_trace then
        Ada.Text_IO.Put("[UO]");
      end if;
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
              Raise_exception(
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
                    use Interfaces;
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
                      x:= x + 1;
                    end loop;
                  end;
                when 8 =>
                  -- !! with bpp as generic param, this case can be merged
                  -- into the general 1,2,4[,8] case without loss of performance
                  -- if the compiler is smart enough. To be tested first...
                  Unfilter_bytes(data(i..i), uf(0..0));
                  i:= i + 1;
                  Out_Pixel_8(uf(0), uf(0), uf(0), 255);
                when 16 =>
                  Unfilter_bytes(data(i..i+1), uf(0..1));
                  i:= i + 2;
                  w1:= U16(uf(0)) * 256 + U16(uf(1));
                  Out_Pixel_16(w1, w1, w1, 65535);
                when others =>
                  null; -- undefined in PNG standard
              end case;
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
                    use Interfaces;
                    max: constant U8:= U8(Shift_Left(Unsigned_32'(1), bits_per_pixel)-1);
                  begin
                    -- loop through the number of pixels in this byte:
                    for k in reverse 1..8/bits_per_pixel loop
                      Out_Pixel_Palette(max and U8(Shift_Right(Unsigned_8(uf(0)), shift)));
                      shift:= shift - bits_per_pixel;
                      exit when x >= x_max or k = 1;
                      x:= x + 1;
                    end loop;
                  end;
                when 8 =>
                  -- !! with bpp as generic param, this case can be merged
                  -- into the general 1,2,4[,8] case without loss of performance
                  -- if the compiler is smart enough. To be tested first...
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
              -- Type 5: RGBA --
              ------------------
              case bits_per_pixel is
                when 32 =>
                  Unfilter_bytes(data(i..i+3), uf(0..3));
                  i:= i + 4;
                  Out_Pixel_8(uf(0), uf(1), uf(2), uf(3));
                when 64 =>
                  Unfilter_bytes(data(i..i+7), uf(0..7));
                  i:= i + 6;
                  Out_Pixel_16(
                    U16(uf(0)) * 256 + U16(uf(1)),
                    U16(uf(2)) * 256 + U16(uf(3)),
                    U16(uf(4)) * 256 + U16(uf(5)),
                    U16(uf(6)) * 256 + U16(uf(7))
                  );
                when others =>
                  null;
              end case;
            when others =>
              null; -- Unknown - exception already raised at header level
          end case;
        end if;
        Inc_XY;
      end loop;
      -- i is between data'Last-(bytes_to_unfilter-2) and data'Last+1
      reject:= (data'Last + 1) - i;
      if reject > 0 then
        if some_trace then
          Ada.Text_IO.Put("[rj" & Integer'Image(reject) & ']');
        end if;
      end if;
    end Output_uncompressed;

    ---------------------------------------------------------------------
    -- ** 10: Decompression **                                         --
    -- Excerpt and simplification from UnZip.Decompress (Inflate only) --
    ---------------------------------------------------------------------

    --  Size of sliding dictionary and output buffer
    wsize: constant:= 16#10000#;

    --------------------------------------
    -- Specifications of UnZ_* packages --
    --------------------------------------

    use Interfaces;

    package UnZ_Glob is
      -- I/O Buffers
      -- > Sliding dictionary for unzipping, and output buffer as well
      slide: Byte_Array( 0..wsize );
      slide_index: Integer:= 0; -- Current Position in slide
      IDAT_reserve: Natural;
      Zip_EOF  : constant Boolean:= False;
      crc32val : Unsigned_32;  -- crc calculated from data
    end UnZ_Glob;

    package UnZ_IO is

      procedure Init_Buffers(IDAT_reserve: Natural);

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

    end UnZ_IO;

    package UnZ_Meth is
      deflate_e_mode: constant Boolean:= False;
      procedure Inflate;
    end UnZ_Meth;

    ------------------------------
    -- Bodies of UnZ_* packages --
    ------------------------------
    package body UnZ_IO is

      procedure Init_Buffers(IDAT_reserve: Natural) is
      begin
        UnZ_Glob.slide_index := 0;
        Bit_buffer.Init;
        CRC32.Init( UnZ_Glob.crc32val );
        UnZ_Glob.IDAT_reserve:= IDAT_reserve;
      end Init_Buffers;

      procedure Read_raw_byte ( bt : out U8 ) is
        ch: Chunk_head;
        dummy: U32;
      begin
        if UnZ_Glob.IDAT_reserve = 0 then
          -- We hit the end of a PNG 'IDAT' chunk, so we go to the next one
          -- (in petto, it's strange design, but well...).
          -- This "feature" has taken some time (and nerves) to be addressed.
          -- Incidentally, I have reprogrammed the whole Huffman
          -- decoding, and looked at many other wrong places to solve
          -- the mystery.
          Big_endian(image.buffer, dummy); -- ending chunk's CRC
          -- New chunk begins here.
          Read(image, ch);
          if ch.kind /= IDAT then
            Raise_exception(
              error_in_image_data'Identity,
              "PNG additional data chunk must be an IDAT"
            );
          end if;
          UnZ_Glob.IDAT_reserve:= Natural(ch.length);
        end if;
        Buffering.Get_Byte(image.buffer, bt);
        UnZ_Glob.IDAT_reserve:= UnZ_Glob.IDAT_reserve - 1;
      end Read_raw_byte;

      package body Bit_buffer is
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

      end Bit_buffer;

      old_bytes: Natural:= 0;
      -- how many bytes to be resent from last Inflate output
      byte_mem: Byte_array(1..8);

      procedure Flush ( x: Natural ) is
        use Ada.Streams;
      begin
        if full_trace then
          Ada.Text_IO.Put("[Flush..." & Integer'Image(x));
        end if;
        CRC32.Update( UnZ_Glob.crc32val, UnZ_Glob.slide( 0..x-1 ) );
        if old_bytes > 0 then
          declare
            app: constant Byte_array:=
              byte_mem(1..old_bytes) & UnZ_Glob.slide(0..x-1);
          begin
            Output_uncompressed(app, old_bytes);
            -- In extreme cases (x very small), we might have some of
            -- the rejected bytes from byte_mem.
            if old_bytes > 0 then
              byte_mem(1..old_bytes):= app(app'Last-(old_bytes-1)..app'Last);
            end if;
          end;
        else
          Output_uncompressed(UnZ_Glob.slide(0..x-1), old_bytes);
          if old_bytes > 0 then
            byte_mem(1..old_bytes):= UnZ_Glob.slide(x-old_bytes..x-1);
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

      ----------------------------------------------------
      -- Reproduction of sequences in the output slide. --
      ----------------------------------------------------

      -- Internal:

      procedure Adjust_to_Slide(
          source         : in out Integer;
          remain         : in out Natural;
          part           :    out Integer;
          index:                  Integer)
      is
        pragma Inline(Adjust_to_Slide);
      begin
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
      end Adjust_to_Slide;

      procedure Copy_range(source, index: in out Natural; amount: Positive) is
        pragma Inline(Copy_range);
      begin
        if abs (index - source) < amount then
          -- if source >= index, the effect of copy is
          -- just like the non-overlapping case
          for count in reverse 1..amount loop
            UnZ_Glob.slide(index):= UnZ_Glob.slide(source);
            index := index  + 1;
            source:= source + 1;
          end loop;
        else -- non-overlapping -> copy slice
          UnZ_Glob.slide( index .. index+amount-1 ):=
            UnZ_Glob.slide( source..source+amount-1 );
          index := index  + amount;
          source:= source + amount;
        end if;
      end Copy_range;

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
          Adjust_to_Slide(source,remain,part, index);
          Copy_range(source, index, part);
          Flush_if_full(index);
          exit when remain = 0;
        end loop;
      end Copy;

    end UnZ_IO;

    package body UnZ_Meth is

      use GID.Decoding_PNG.Huffman;

      --------[ Method: Inflate ]--------

      procedure Inflate_Codes ( huf_dis, huf_lit_len: Huff_tree ) is
        dis_idx, lit_len_idx: Natural; -- indices in both trees
        val, val_dis, val_len : Natural;
        use Interfaces;

        procedure Get_length is
          v32, base, offset, extra: Unsigned_32;
        begin
          if val <= 264 then
            val_len:= 3 + (val-257);
          elsif val >= 285 then
            val_len:= 258;
          else
            v32:= Unsigned_32(val);
            extra := 1 + (v32-265)/4;
            base  := Shift_Left(8, ((val-265)/4)) + 3;
            offset:= ((v32-265) and 3) * Shift_Left(2, ((val-265)/4));
            val_len:=
              UnZ_IO.Bit_buffer.Read_and_dump(Natural(extra)) +
              Natural(base + offset);
          end if;
        end Get_length;

        procedure Get_distance is
          v32, base, offset, extra: Unsigned_32;
        begin
          if val_dis <= 3 then
            val_dis:= val_dis + 1;
          else
            v32:= Unsigned_32(val_dis);
            base:=   Shift_Left(4, ((val_dis-4)/2))+1;
            offset:= (v32 and 1) * Shift_Left(2, ((val_dis-4)/2));
            extra:=  (v32-2)/2;
            val_dis:=
              UnZ_IO.Bit_buffer.Read_and_dump(Natural(extra)) +
              Natural(base + offset);
          end if;
        end Get_distance;

        w      : Integer:= UnZ_Glob.slide_index;
        -- more local variable for slide index
      begin
        if full_trace then
          Ada.Text_IO.Put_Line("Begin Inflate_codes");
        end if;

        if huf_lit_len.last = nil then
          return;
        end if;
        lit_len_idx:= root;
        -- inflate the coded data
        main_loop:
        while not UnZ_Glob.Zip_EOF loop
          if UnZ_IO.Bit_buffer.Read_and_dump(1) = 0 then
            lit_len_idx:= huf_lit_len.node(lit_len_idx).zero;
          else
            lit_len_idx:= huf_lit_len.node(lit_len_idx).one;
          end if;
          if lit_len_idx = nil then
            raise error_in_image_data;
          end if;
          if huf_lit_len.node(lit_len_idx).zero = nil and then
            huf_lit_len.node(lit_len_idx).one = nil
          then -- tree node
            val:= huf_lit_len.node(lit_len_idx).n;
            case val is
              when 0 .. 255 =>   -- It is a litteral
                UnZ_Glob.slide ( w ) :=  U8( val );
                w:= w + 1;
                UnZ_IO.Flush_if_full(w);
              when 256 =>        -- End of block (EOB)
                if full_trace then
                  Ada.Text_IO.Put_Line("Exit  Inflate_codes, EOB");
                end if;
                exit main_loop;
              when 257 .. 285 => -- We have a LZ length/distance code
                Get_length;
                if huf_dis.last = nil then
                  -- Empty tree for distances ("fixed"), then read directly
                  val_dis:= UnZ_IO.Bit_buffer.Read_and_dump(5);
                else
                  dis_idx:= root;
                  while huf_dis.node(dis_idx).zero /= nil or else
                    huf_dis.node(dis_idx).one /= nil
                  loop
                    if UnZ_IO.Bit_buffer.Read_and_dump(1) = 0 then
                      dis_idx:= huf_dis.node(dis_idx).zero;
                    else
                      dis_idx:= huf_dis.node(dis_idx).one;
                    end if;
                  end loop;
                  val_dis:= huf_dis.node(dis_idx).n;
                end if;
                Get_distance;
                UnZ_IO.Copy(
                  distance => val_dis,
                  length   => val_len,
                  index    => w
                );
              when others =>
                raise error_in_image_data;
            end case;
            lit_len_idx:= root;
          end if;
        end loop main_loop;

        UnZ_Glob.slide_index:= w;

        if full_trace then
          Ada.Text_IO.Put_Line("End   Inflate_codes");
        end if;
      end Inflate_Codes;

      procedure Inflate_stored_block is -- Actually, nothing to inflate
        N : Integer;
      begin
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
        while N > 0  and then not UnZ_Glob.Zip_EOF loop
          -- Read and output the non-compressed data
          N:= N - 1;
          UnZ_Glob.slide ( UnZ_Glob.slide_index ) :=
            U8( UnZ_IO.Bit_buffer.Read_and_dump(8) );
          UnZ_Glob.slide_index:= UnZ_Glob.slide_index + 1;
          UnZ_IO.Flush_if_full(UnZ_Glob.slide_index);
        end loop;
        if full_trace then
          Ada.Text_IO.Put_Line("End   Inflate_stored_block");
        end if;
      end Inflate_stored_block;

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

      procedure Inflate_fixed_block is
        huf_dis, huf_lit_len: Huff_tree;
        descr_lit_len: Huff_descriptor(0..287);
      begin
        if full_trace then
          Ada.Text_IO.Put_Line("Begin Inflate_fixed_block");
        end if;
        -- Make the tree descriptor for LZ distances
        for i in 0 .. 143 loop
          descr_lit_len(i):= (length => 8, code => 16#30#+i);
        end loop;
        for i in 144 .. 255 loop
          descr_lit_len(i):= (length => 9, code => 16#190#+(i-144));
        end loop;
        for i in 256 .. 279 loop
          descr_lit_len(i):= (length => 7, code => i-256);
        end loop;
        for i in 280 .. 287 loop
          descr_lit_len(i):= (length => 8, code => 16#C0#+(i-280));
        end loop;
        -- Build the tree according to the descriptor
        Build(huf_lit_len, descr_lit_len);

        huf_dis.last:= nil;
        Inflate_Codes ( huf_dis, huf_lit_len );

        if full_trace then
          Ada.Text_IO.Put_Line("End   Inflate_fixed_block");
        end if;
      end Inflate_fixed_block;

      procedure Inflate_dynamic_block is

        type H_vector is array(Natural range <>) of Natural;

        procedure Make_dynamic_descriptor(h: out Huff_descriptor; lng: in H_vector) is
          maxLng, code, len: Natural;
        begin
          for i in h'Range loop
            h(i):= (length => lng(i), code => 0);
          end loop;
          maxLng:= 0;
          for i in h'Range loop
            if maxLng < lng(i) then
              maxLng:= lng(i);
            end if;
          end loop;
          declare
            bl_count, next_code: H_vector(0..maxLng):= (others => 0);
          begin
            for i in h'Range loop
              bl_count(lng(i)):= bl_count(lng(i)) + 1;
            end loop;
            code:= 0;
            bl_count(0):= 0;
	      for bits in 1 .. maxLng loop
              code:= (code + bl_count(bits-1)) * 2;
              next_code(bits):= code;
            end loop;
            for n in h'Range loop
              len:= lng(n);
              if len > 0 then
                h(n).code:= next_code(len);
                next_code(len):= next_code(len) + 1; -- !! before or after (++)
              end if;
            end loop;
          end;
        end Make_dynamic_descriptor;

        huf_dis, huf_lit: Huff_tree;

        procedure Decode_dynamic_compression_structure is
          huf_len: Huff_tree;
          len_idx: Natural;
          hLit: Natural;
          hDist: Natural;
          hCLen: Natural;
          nExtr: Natural;
          codeLengthOrder : constant H_vector( 0..18 ) :=
            ( 16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15 );
          codeLengthCode: H_vector( codeLengthOrder'Range ):= (others => 0);

          value,copyLength: Natural;
        begin
          hLit := UnZ_IO.Bit_buffer.Read_and_dump(5);
          hDist:= UnZ_IO.Bit_buffer.Read_and_dump(5);
          hCLen:= UnZ_IO.Bit_buffer.Read_and_dump(4);

          for i in 0 .. hCLen+3 loop
            codeLengthCode(codeLengthOrder(i)):= UnZ_IO.Bit_buffer.Read_and_dump(3);
          end loop;
          declare
            descr_code: Huff_descriptor(codeLengthCode'Range);
          begin
            Make_dynamic_descriptor(descr_code, codeLengthCode);
            Build(huf_len, descr_code);
          end;

          declare
            cl_lit: H_vector(0..(hLit+257)-1);
            cl_dis: H_vector(0..(hDist+1)-1);
            cl_combined: H_vector(0..(hLit+257+hDist+1)-1); -- max 321
            descr_lit: Huff_descriptor( cl_lit'Range );
            descr_dis: Huff_descriptor( cl_dis'Range );
          begin
            nExtr:= 0;
            len_idx:= root;
            while nExtr <= hLit+257+hDist loop
              if UnZ_IO.Bit_buffer.Read_and_dump(1) = 0 then
                len_idx:= huf_len.node(len_idx).zero;
              else
                len_idx:= huf_len.node(len_idx).one;
              end if;
              if len_idx = nil then
                raise error_in_image_data;
              end if;
              if huf_len.node(len_idx).zero = nil and then
                huf_len.node(len_idx).one = nil
              then
                value:= huf_len.node(len_idx).n;
                case value is
                  when 0..15 => -- length of code in bits (0..15)
                    cl_combined(nExtr):= value;
                    nExtr:= nExtr + 1; -- !! ++
                  when 16 =>    -- repeat last length 3 to 6 times
                    for copy in 1 .. 3 + UnZ_IO.Bit_buffer.Read_and_dump(2) loop
                      cl_combined(nExtr):= cl_combined(nExtr-1);
                      nExtr:= nExtr + 1;
                    end loop;
                  when 17 =>    -- 3 to 10 zero length codes
                    for copy in 1 .. 3 + UnZ_IO.Bit_buffer.Read_and_dump(3) loop
                      cl_combined(nExtr):= 0;
                      nExtr:= nExtr + 1;
                    end loop;
                  when 18 =>    -- 11 to 138 zero length codes
                    for copy in 1 .. 11 + UnZ_IO.Bit_buffer.Read_and_dump(7) loop
                      cl_combined(nExtr):= 0;
                      nExtr:= nExtr + 1;
                    end loop;
                  when others =>
                    null; -- or error...!!
                end case;
                len_idx:= root;
              end if;
            end loop;
            cl_lit:= cl_combined(cl_lit'Range);
            cl_dis:= cl_combined(hLit+257..cl_combined'Last);
            -- NB: we could use only cl_combined if Make_dynamic_descriptor uses lng'First
            Make_dynamic_descriptor(descr_lit, cl_lit);
            Build(huf_lit, descr_lit);
            Make_dynamic_descriptor(descr_dis, cl_dis);
            Build(huf_dis, descr_dis);
          end;
        end Decode_dynamic_compression_structure;

      begin
        if full_trace then
          Ada.Text_IO.Put_Line("Begin Inflate_dynamic_block");
        end if;
        Decode_dynamic_compression_structure;
        Inflate_Codes( huf_dis, huf_lit );
        if full_trace then
          Ada.Text_IO.Put_Line("End   Inflate_dynamic_block");
        end if;
      end Inflate_dynamic_block;

      procedure Inflate_Block( last_block: out Boolean ) is
      begin
        last_block:= Boolean'Val(UnZ_IO.Bit_buffer.Read_and_dump(1));
        case UnZ_IO.Bit_buffer.Read_and_dump(2) is -- Block type = 0,1,2,3
          when 0 =>      Inflate_stored_block;
          when 1 =>      Inflate_fixed_block;
          when 2 =>      Inflate_dynamic_block;
          when others => raise error_in_image_data; -- Bad block type (3)
        end case;
      end Inflate_Block;

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
          exit when is_last_block;
          blocks:= blocks+1;
        end loop;
        UnZ_IO.Flush( UnZ_Glob.slide_index );
        UnZ_Glob.slide_index:= 0;
        if some_trace then
          Ada.Text_IO.Put("# blocks:" & Integer'Image(blocks));
        end if;
        UnZ_Glob.crc32val := CRC32.Final( UnZ_Glob.crc32val );
      end Inflate;

    end UnZ_Meth;

    --------------------------------------------------------------------
    -- End of the Decompression part, and of UnZip.Decompress excerpt --
    --------------------------------------------------------------------

    ch: Chunk_head;
    b: U8;
    z_crc, dummy: U32;

  begin -- Load
    if interlaced then
      x_max:= (image.width+7)/8 - 1;
      y_max:= (image.height+7)/8 - 1;
    else
      x_max:= X_range'Last;
      y_max:= Y_range'Last;
    end if;
    loop
      Read(image, ch);
      case ch.kind is
        when IEND => -- 11.2.5 IEND Image trailer
          exit;
        when IDAT => -- 11.2.4 IDAT Image data
          --
          -- NB: the compressed data may hold on several IDAT chunks.
          -- It means that right in the middle of compressed data, you
          -- can have a chunk crc, and a new IDAT header!...
          --
          Get_Byte(image.buffer, b); -- zlib compression method/flags code
          Get_Byte(image.buffer, b); -- Additional flags/check bits
          --
          UnZ_IO.Init_Buffers(IDAT_reserve => Natural(ch.length) - 2);
          -- ^ we indicate that we have a byte reserve of chunk's length,
          --   minus both zlib header bytes.
          UnZ_Meth.Inflate;
          Big_endian(image.buffer, z_crc); -- zlib Check value
          --  if z_crc /= U32(UnZ_Glob.crc32val) then
          --    ada.text_io.put(z_crc 'img &  UnZ_Glob.crc32val'img);
          --    Raise_exception(
          --      error_in_image_data'Identity,
          --      "PNG: deflate stream corrupt"
          --    );
          --  end if;
          --  ** Mystery: this check fail even with images which decompress perfectly
          --  ** Is CRC init value different between zip and zlib ? Is it Adler32 ?
          Big_endian(image.buffer, dummy);
          -- last IDAT chunk's CRC (then, on compressed data)
          --
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
    end loop;
    if some_trace then
      for f in Filter_method_0 loop
        Ada.Text_IO.Put_Line(
          "Filter: " &
          Filter_method_0'Image(f) &
          Integer'Image(filter_stat(f))
        );
      end loop;
    end if;
    Feedback(100);
  end Load;

end GID.Decoding_PNG;
