with Zip.CRC, UnZip.Decompress.Huffman, BZip2;

with Ada.Exceptions, Ada.Streams.Stream_IO, Ada.Text_IO, Interfaces;

package body UnZip.Decompress is

  procedure Decompress_data(
    zip_file            : in out Zip_Streams.Root_Zipstream_Type'Class;
    format              : PKZip_method;
    mode                : Write_mode;
    output_file_name    : String;
    output_memory_access: out p_Stream_Element_Array;
    feedback            : Zip.Feedback_proc;
    explode_literal_tree: Boolean;
    explode_slide_8KB   : Boolean;
    data_descriptor_after_data : Boolean;
    encrypted           : Boolean;
    password            : in out Unbounded_String;
    get_new_password    : Get_password_proc;
    hint                : in out Zip.Headers.Local_File_Header
  )
  is
    -- Disable AdaControl rule for detecting global variables,
    -- they have become local here.
    --## RULE OFF Directly_Accessed_Globals
    --
    -- I/O Buffers sizes
    --  Size of input buffer
    inbuf_size: constant:= 16#8000#;  -- (orig: 16#1000# B =  4 KB)
    --  Size of sliding dictionary and output buffer
    wsize     : constant:= 16#10000#; -- (orig: 16#8000# B = 32 KB)

    ----------------------------------------------------------------------------
    -- Specifications of UnZ_* packages (remain of Info Zip's code structure) --
    ----------------------------------------------------------------------------
    use Interfaces;

    package UnZ_Glob is -- Not global anymore, since local to Decompress_data :-)
      -- I/O Buffers
      -- > Sliding dictionary for unzipping, and output buffer as well
      slide: Zip.Byte_Buffer( 0..wsize );
      slide_index: Integer:= 0; -- Current Position in slide
      -- > Input buffer
      inbuf: Zip.Byte_Buffer( 0 .. inbuf_size - 1 );
      inpos, readpos: Integer;  -- pos. in input buffer, pos. read from file
      compsize,            -- compressed size of file
      reachedsize,         -- number of bytes read from zipfile
      uncompsize,          -- uncompressed size of file
      effective_writes : UnZip.File_size_type;
      -- ^ count of effective bytes written or tested, for feedback only
      percents_done    : Natural;
      crc32val : Unsigned_32;  -- crc calculated from data
      uncompressed_index  : Ada.Streams.Stream_Element_Offset;
    end UnZ_Glob;

    Zip_EOF  : Boolean; -- read over end of zip section for this file

    package UnZ_IO is
      out_bin_file: Ada.Streams.Stream_IO.File_Type;
      out_txt_file: Ada.Text_IO.File_Type;
      last_char   : Character:= ' ';

      procedure Init_Buffers;

      package Decryption is
        procedure Set_mode( crypted: Boolean );
        function Get_mode return Boolean;
        procedure Init( password: String; crc_check: Unsigned_32);
        procedure Decode( b: in out Unsigned_8 );
          pragma Inline(Decode);
      end Decryption;

      procedure Read_raw_byte ( bt : out Unsigned_8 );
        pragma Inline(Read_raw_byte);

      package Bit_buffer is
        procedure Init;
        -- Read at least n bits into the bit buffer, returns the n first bits
        function Read ( n: Natural ) return Integer;
          pragma Inline(Read);
        function Read_U32 ( n: Natural ) return Unsigned_32;
          pragma Inline(Read_U32);
        -- Inverts (NOT operator) the result before masking by n bits
        function Read_inverted ( n: Natural ) return Integer;
          pragma Inline(Read_inverted);
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

      procedure Flush_if_full(W: in out Integer; unflushed: in out Boolean);
        pragma Inline(Flush_if_full);

      procedure Flush_if_full(W: in out Integer);
        pragma Inline(Flush_if_full);

      procedure Copy(
        distance, length:        Natural;
        index           : in out Natural );
      pragma Inline(Copy);

      procedure Copy_or_zero(
        distance, length:        Natural;
        index           : in out Natural;
        unflushed       : in out Boolean );
      pragma Inline(Copy_or_zero);

      procedure Delete_output; -- an error has occured (bad compressed data)

    end UnZ_IO;

    package UnZ_Meth is
      procedure Copy_stored;
      procedure Unshrink;
      subtype Reduction_factor is Integer range 1..4;
      procedure Unreduce( factor: Reduction_factor );
      procedure Explode( literal_tree, slide_8_KB: Boolean );
      deflate_e_mode: Boolean:= False;
      procedure Inflate;
      procedure Bunzip2; -- Nov-2009
    end UnZ_Meth;

    procedure Process_feedback(new_bytes: File_size_type) is
    pragma Inline(Process_feedback);
      new_percents_done: Natural;
      user_aborting: Boolean;
      use Zip;
    begin
      if feedback = null or UnZ_Glob.uncompsize = 0 then
        return; -- no feedback proc. or cannot calculate percentage
      end if;
      UnZ_Glob.effective_writes:= UnZ_Glob.effective_writes + new_bytes;
      new_percents_done:= Natural(
        (100.0 * Float(UnZ_Glob.effective_writes)) / Float(UnZ_Glob.uncompsize)
      );
      if new_percents_done > UnZ_Glob.percents_done then
        feedback(
          percents_done => new_percents_done,
          entry_skipped => False,
          user_abort    => user_aborting
        );
        if user_aborting then
          raise User_abort;
        end if;
        UnZ_Glob.percents_done:= new_percents_done;
      end if;
    end Process_feedback;

    ------------------------------
    -- Bodies of UnZ_* packages --
    ------------------------------
    package body UnZ_IO is

      -- Centralize buffer initialisations - 29-Jun-2001

      procedure Init_Buffers is
      begin
        UnZ_Glob.inpos   :=  0;  -- Input buffer position
        UnZ_Glob.readpos := -1;  -- Nothing read
        UnZ_Glob.slide_index := 0;
        UnZ_Glob.reachedsize      := 0;
        UnZ_Glob.effective_writes := 0;
        UnZ_Glob.percents_done    := 0;
        Zip_EOF := False;
        Zip.CRC.Init( UnZ_Glob.crc32val );
        Bit_buffer.Init;
      end Init_Buffers;

      procedure Read_buffer is
      begin
        if full_trace then
          Ada.Text_IO.Put("[Read_buffer...");
        end if;
        if UnZ_Glob.reachedsize > UnZ_Glob.compsize + 2 then
          -- +2: last code is smaller than requested!
          UnZ_Glob.readpos := UnZ_Glob.inbuf'Length;
          -- Simulates reading -> no blocking
          Zip_EOF := True;
        else
          begin
            Zip.BlockRead(
              stream        => zip_file,
              buffer        => UnZ_Glob.inbuf,
              actually_read => UnZ_Glob.readpos
            );
          exception
            when others => -- I/O error
              UnZ_Glob.readpos := UnZ_Glob.inbuf'Length;
              -- Simulates reading -> CRC error
              Zip_EOF := True;
          end;
          if UnZ_Glob.readpos = 0 then
            UnZ_Glob.readpos := UnZ_Glob.inbuf'Length;
            -- Simulates reading -> CRC error
            Zip_EOF := True;
          end if;

          UnZ_Glob.reachedsize:=
            UnZ_Glob.reachedsize + UnZip.File_size_type(UnZ_Glob.readpos);
          UnZ_Glob.readpos:= UnZ_Glob.readpos - 1;
            -- Reason: index of inbuf starts at 0
        end if;
        UnZ_Glob.inpos:= 0;
        if full_trace then
          Ada.Text_IO.Put_Line("finished]");
        end if;
      end Read_buffer;

      procedure Read_byte_no_decrypt( bt : out Zip.Byte ) is
        pragma Inline( Read_byte_no_decrypt );
      begin
        if UnZ_Glob.inpos > UnZ_Glob.readpos then
          Read_buffer;
        end if;
        bt := UnZ_Glob.inbuf ( UnZ_Glob.inpos );
        UnZ_Glob.inpos := UnZ_Glob.inpos + 1;
      end Read_byte_no_decrypt;

      -- 27-Jun-2001: Decryption - algorithm in Appnote.txt

      package body Decryption is

        type Decrypt_keys is array( 0..2 ) of Unsigned_32;
        keys     : Decrypt_keys;
        decrypt_mode : Boolean;

        procedure Set_mode( crypted: Boolean ) is
        begin
          decrypt_mode:= crypted;
        end Set_mode;

        function Get_mode return Boolean is
        begin
          return decrypt_mode;
        end Get_mode;

        procedure Update_keys( by: Zip.Byte ) is
        begin
          Zip.CRC.Update( keys(0), (0 => by) );
          keys(1) := keys(1) + (keys(0) and 16#000000ff#);
          keys(1) := keys(1) * 134775813 + 1;
          Zip.CRC.Update(
            keys(2),
            (0 => Zip.Byte(Shift_Right( keys(1), 24 )))
          );
        end Update_keys;

        function Decrypt_byte return Zip.Byte is
          temp: Unsigned_16;
        begin
          temp:= Unsigned_16(keys(2) and 16#ffff#) or 2;
          return Zip.Byte(Shift_Right(temp * (temp xor 1), 8));
        end Decrypt_byte;

        procedure Init( password: String; crc_check: Unsigned_32) is
          buffer: array( 0..11 ) of Zip.Byte;
          c: Zip.Byte;
          t: Unsigned_32;
        begin
          -- Step 1 - Initializing the encryption keys

          keys:= (
            0 => 305419896,
            1 => 591751049,
            2 => 878082192
          );

          for i in password'Range loop
            Update_keys( Character'Pos(password(i)) );
          end loop;

          -- Step 2 - Decrypting the encryption header

          for i in buffer'Range loop
            Read_byte_no_decrypt( c );
            c:= c xor Decrypt_byte;
            Update_keys(c);
            buffer(i):= c;
          end loop;

          t:= Zip_Streams.Calendar.Convert(hint.file_timedate);
          if c /= Zip.Byte(Shift_Right( crc_check, 24 )) and not
            -- Dec. 2012. This is a feature of Info-Zip (crypt.c).
            -- Since CRC is only known at the end of a one-way stream
            -- compression, and cannot be written back, they are using a byte of
            -- the time stamp instead. This is not documented in appnote.txt v.6.3.3.
            ( data_descriptor_after_data and c = Zip.Byte(Shift_Right(t, 8) and 16#FF#) )
          then
            raise UnZip.Wrong_password;
          end if;

        end Init;

        procedure Decode( b: in out Zip.Byte ) is
        begin
          if decrypt_mode then
            b:= b xor Decrypt_byte;
            Update_keys(b);
          end if;
        end Decode;

      end Decryption;

      procedure Read_raw_byte ( bt : out Zip.Byte ) is
      begin
        Read_byte_no_decrypt( bt );
        Decryption.Decode(bt);
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
          bt: Zip.Byte;
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

        function Read_inverted ( n: Natural ) return Integer is
        begin
          Need(n);
          return Integer((not B) and (Shift_Left(1,n) - 1));
        end Read_inverted;

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

      procedure Flush ( x: Natural ) is
        use Zip, UnZip, Ada.Streams;
      begin
        if full_trace then
          Ada.Text_IO.Put("[Flush...");
        end if;
        begin
          case mode is
            when write_to_binary_file =>
              BlockWrite(Ada.Streams.Stream_IO.Stream(out_bin_file).all, UnZ_Glob.slide(0..x-1));
            when write_to_text_file =>
              Zip.Write_as_text(
                UnZ_IO.out_txt_file, UnZ_Glob.slide(0..x-1), UnZ_IO.last_char
              );
            when write_to_memory =>
              for i in 0..x-1 loop
                output_memory_access(UnZ_Glob.uncompressed_index):=
                  Ada.Streams.Stream_Element(UnZ_Glob.slide(i));
                UnZ_Glob.uncompressed_index:= UnZ_Glob.uncompressed_index + 1;
              end loop;
            when just_test =>
              null;
          end case;
        exception
          when others =>
            raise UnZip.Write_Error;
        end;
        Zip.CRC.Update( UnZ_Glob.crc32val, UnZ_Glob.slide( 0..x-1 ) );
        Process_Feedback(File_size_type(x));
        if full_trace then
          Ada.Text_IO.Put_Line("finished]");
        end if;
      end Flush;

      procedure Flush_if_full(W: in out Integer; unflushed: in out Boolean) is
      begin
        if W = wsize then
          Flush(wsize);
          W:= 0;
          unflushed:= False;
        end if;
      end Flush_if_full;

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
        if full_trace then
          Ada.Text_IO.Put(
            "(Copy_range: source=" & Integer'Image(source) &
            " index=" & Integer'Image(index) &
            " amount=" & Integer'Image(amount) );
        end if;
        if abs (index - source) < amount then
          if full_trace and then source < index then
            Ada.Text_IO.Put(
              "; replicates" &
              Integer'Image(amount) & " /" & Integer'Image(index-source) &
              " )"
            );
            -- ...times the range source..index-1
          end if;
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
        if full_trace then
          Ada.Text_IO.Put(')');
        end if;
      end Copy_range;

      -- The copying routines:

      procedure Copy(
          distance, length:        Natural;
          index           : in out Natural )
      is
        source,part,remain: Integer;
      begin
        if full_trace or (some_trace and then distance > 32768+3) then
          Ada.Text_IO.Put(
            "DLE(distance=" & Integer'Image(distance) &
            " length=" & Integer'Image(length) & ")"
          );
        end if;
        source:= index - distance;
        remain:= length;
        loop
          Adjust_to_Slide(source,remain,part, index);
          Copy_range(source, index, part);
          Flush_if_full(index);
          exit when remain = 0;
        end loop;
      end Copy;

      procedure Copy_or_zero(
          distance, length:        Natural;
          index           : in out Natural;
          unflushed       : in out Boolean )
      is
        source,part,remain: Integer;
      begin
        source:= index - distance;
        remain:= length;
        loop
          Adjust_to_Slide(source,remain,part, index);
          if unflushed and then index <= source then
            UnZ_Glob.slide( index .. index+part-1 ):= (others=> 0);
            index := index  + part;
            source:= source + part;
          else
            Copy_range(source, index, part);
          end if;
          Flush_if_full(index,unflushed);
          exit when remain = 0;
        end loop;
      end Copy_or_zero;

      procedure Delete_output is -- an error has occured (bad compressed data)
      begin
        if no_trace then -- if there is a trace, we are debugging
          case mode is   --  and want to keep the malformed file
            when write_to_binary_file =>
              Ada.Streams.Stream_IO.Delete( UnZ_IO.out_bin_file );
            when write_to_text_file =>
              Ada.Text_IO.Delete( UnZ_IO.out_txt_file );
            when others =>
              null;
          end case;
        end if;
      end Delete_output;

    end UnZ_IO;

    package body UnZ_Meth is

      --------[ Method: Unshrink ]--------

      -- Original in Pascal written by Christian Ghisler.

      Max_Code  : constant := 8192;
      Max_Stack : constant := 8192;
      Initial_Code_Size : constant := 9;
      Maximum_Code_Size : constant := 13;
      First_Entry       : constant := 257;

      -- Rest of slide=write buffer =766 bytes

      Write_Max : constant := wsize-3*(Max_Code-256)-Max_Stack-2;

      Previous_Code: array ( First_Entry .. Max_Code ) of Integer;
      Actual_Code  : array ( First_Entry .. Max_Code ) of Zip.Byte;

      Next_Free : Integer;      -- Next free code in trie
      Write_Ptr : Integer;      -- Pointer to output buffer

      Writebuf : Zip.Byte_Buffer ( 0..Write_Max );  -- Write buffer

      procedure Unshrink_Flush is
        use Zip, UnZip, Ada.Streams, Ada.Streams.Stream_IO;
      begin
        if full_trace then
          Ada.Text_IO.Put("[Unshrink_Flush]");
        end if;
        begin
          case mode is
            when write_to_binary_file =>
              BlockWrite(Stream(UnZ_IO.out_bin_file).all, Writebuf(0..Write_Ptr-1));
            when write_to_text_file =>
              Zip.Write_as_text(UnZ_IO.out_txt_file, Writebuf(0..Write_Ptr-1), UnZ_IO.last_char);
            when write_to_memory =>
              for I in 0..Write_Ptr-1 loop
                output_memory_access(UnZ_Glob.uncompressed_index):=
                  Stream_Element(Writebuf(I));
                UnZ_Glob.uncompressed_index :=  UnZ_Glob.uncompressed_index + 1;
              end loop;
            when just_test =>
              null;
          end case;
        exception
          when others =>
            raise UnZip.Write_Error;
        end;
        Zip.CRC.Update( UnZ_Glob.crc32val, Writebuf(0 .. Write_Ptr-1) );
        Process_feedback(File_size_type(Write_Ptr));
      end Unshrink_Flush;

      procedure Write_Byte( B: Zip.Byte ) is
      begin
        Writebuf ( Write_Ptr ) := B;
        Write_Ptr:= Write_Ptr + 1;
        if Write_Ptr > Write_Max then
          Unshrink_Flush;
          Write_Ptr := 0;
        end if;
      end Write_Byte;

      procedure Clear_Leaf_Nodes is
        Pc           : Integer;  -- previous code
        Act_Max_Code : Integer;  -- max code to be searched for leaf nodes

      begin
        Act_Max_Code := Next_Free - 1;
        for I in First_Entry .. Act_Max_Code loop
          Previous_Code( I ):=
            Integer( Unsigned_32( Previous_Code(I) ) or 16#8000#);
        end loop;

        for I in First_Entry .. Act_Max_Code loop
          Pc:= Previous_Code(I) mod 16#8000#;
          if  Pc > 256 then
            Previous_Code(Pc):= Previous_Code(Pc) mod 16#8000#;
          end if;
        end loop;

        -- Build new free list
        Pc := - 1;
        Next_Free := - 1;
        for I in First_Entry .. Act_Max_Code loop
          -- Either free before or marked now
          if (Unsigned_32( Previous_Code(I) ) and 16#C000#)  /= 0 then
            -- Link last item to this item
            if Pc = -1 then
              Next_Free := I;
            else
              Previous_Code( Pc ) := -I;
            end if;
            Pc := I;
          end if;
        end loop;

        if Pc /= - 1 then
          Previous_Code ( Pc ) := - Act_Max_Code - 1;
        end if;

      end Clear_Leaf_Nodes;

      procedure Unshrink is
        Incode      : Integer;  -- Code read in
        Last_Incode : Integer;
        Last_Outcode: Zip.Byte;
        Code_Size   : Integer:= Initial_Code_Size; -- Actual code size (9..13)
        Stack       : Zip.Byte_Buffer ( 0..Max_Stack );  -- Stack for output
        Stack_Ptr   : Integer:= Max_Stack;
        New_Code    : Integer;  -- Save new normal code read

        Code_for_Special  : constant:= 256;
        Code_Increase_size: constant:= 1;
        Code_Clear_table  : constant:= 2;


        S: UnZip.File_size_type:= UnZ_Glob.uncompsize;
        -- Fix Jan-2009: replaces a remaining bits counter as Unsigned_*32*...

        procedure Read_Code is
          pragma Inline(Read_Code);
        begin
          Incode := UnZ_IO.Bit_buffer.Read_and_dump( Code_Size );
        end Read_Code;

      begin
        Previous_Code:= (others=> 0);
        Actual_Code  := (others=> 0);
        Stack        := (others=> 0);
        Writebuf     := (others=> 0);

        if UnZ_Glob.compsize = Unsigned_32'Last then
          -- Compressed Size was not in header!
          raise UnZip.Not_supported;
        elsif UnZ_Glob.uncompsize = 0 then
          return; -- compression of a 0-file with Shrink.pas
        end if;

        -- initialize free codes list

        for I in Previous_Code'Range loop
          Previous_Code(I) := - (I+1);
        end loop;

        Next_Free := First_Entry;
        Write_Ptr := 0;

        Read_Code;
        Last_Incode  := Incode;
        Last_Outcode := Zip.Byte( Incode );
        Write_Byte ( Last_Outcode );
        S:= S - 1;

        while S > 0 and then not Zip_EOF loop
          Read_Code;
          if Incode = Code_for_Special then
            Read_Code;
            case Incode is
              when Code_Increase_size =>
                Code_Size:= Code_Size + 1;
                if some_trace then
                  Ada.Text_IO.Put(
                    "[LZW code size ->" & Integer'Image(Code_Size) & ']'
                  );
                end if;
                if  Code_Size > Maximum_Code_Size then
                  raise Zip.Zip_file_Error;
                end if;
              when Code_Clear_table =>
                Clear_Leaf_Nodes;
              when others=>
                raise Zip.Zip_file_Error;
            end case;

          else -- Normal code
            New_Code := Incode;
            if Incode < 256 then          -- Simple char
              Last_Outcode :=  Zip.Byte( Incode );
              Write_Byte ( Last_Outcode );
              S:= S - 1;
            else
              if Previous_Code( Incode ) < 0 then
                Stack( Stack_Ptr ):= Last_Outcode;
                Stack_Ptr:= Stack_Ptr -1;
                Incode := Last_Incode;
              end if;
              while Incode > 256 loop
                -- Test added 11-Dec-2007 for situations
                --     happening on corrupt files:
                if Stack_Ptr < Stack'First or
                   Incode > Actual_Code'Last
                then
                  raise Zip.Zip_file_Error;
                end if;
                Stack( Stack_Ptr ):= Actual_Code( Incode );
                Stack_Ptr:= Stack_Ptr -1;
                Incode := Previous_Code( Incode );
              end loop;

              Last_Outcode := Zip.Byte( Incode mod 256 );
              Write_Byte( Last_Outcode );

              for I in Stack_Ptr+1 .. Max_Stack  loop
                Write_Byte( Stack( I ) );
              end loop;
              S:= S - UnZip.File_size_type(Max_Stack - Stack_Ptr + 1);

              Stack_Ptr := Max_Stack;
            end if;
            Incode := Next_Free;
            if Incode <= Max_Code then
              Next_Free := - Previous_Code( Incode );
              -- Next node in free list
              Previous_Code( Incode ) := Last_Incode;
              Actual_Code  ( Incode ) := Last_Outcode;
            end if;
            Last_Incode := New_Code;
          end if;
        end loop;
        if some_trace then
          Ada.Text_IO.Put("[ Unshrink main loop finished ]");
        end if;
        Unshrink_Flush;
      end Unshrink;

      --------[ Method: Unreduce ]--------

      procedure Unreduce( factor: Reduction_factor ) is

        -- Original slide limit: 16#4000#
        DLE_code: constant:= 144;
        subtype Symbol_range is Integer range 0..255;
        subtype Follower_range is Integer range 0..63; -- Appnote: <= 32 !
        Followers: array (Symbol_range, Follower_range) of Symbol_range:=
          (others=> (others=> 0));
        Slen: array (Symbol_range) of Follower_range;

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

        procedure LoadFollowers is
          list_followers: constant Boolean:= some_trace;
          procedure Show_symbol(S: Symbol_range) is
          begin
            if S in 32..254 then
              Ada.Text_IO.Put(Character'Val(S));
            else
              Ada.Text_IO.Put('{' & Symbol_range'Image(S) & '}');
            end if;
          end Show_symbol;
        begin
          for X in reverse Symbol_range loop
            Slen(X):= UnZ_IO.Bit_buffer.Read_and_dump(6);
            if list_followers then
              Show_symbol(X);
              Ada.Text_IO.Put(" -> (" & Integer'Image(Slen(X)) & ") ");
            end if;
            for I in 0 .. Slen(X)-1  loop
              Followers(X,I):= UnZ_IO.Bit_buffer.Read_and_dump(8);
              if list_followers then
                Show_symbol(Followers(X,I));
              end if;
            end loop;
            if list_followers then
              Ada.Text_IO.New_Line;
            end if;
          end loop;
        end LoadFollowers;

        length,
        char_read,
        last_char: Integer:= 0;
        -- ^ some := 0 are useless, just to calm down ObjectAda 7.2.2
        S: UnZip.File_size_type:= UnZ_Glob.uncompsize;
        -- number of bytes left to decompress
        unflushed: Boolean:= True;
        maximum_AND_mask: constant Unsigned_32:= Shift_Left(1,8-factor) - 1;

        procedure Out_byte(b: Zip.Byte) is
        begin
          S:= S - 1;
          UnZ_Glob.slide(UnZ_Glob.slide_index):= b;
          UnZ_Glob.slide_index:= UnZ_Glob.slide_index + 1;
          UnZ_IO.Flush_if_full(UnZ_Glob.slide_index,unflushed);
        end Out_byte;

        V: Unsigned_32:= 0;
        type State_type is (normal, length_a, length_b, distance);
        state: State_type:= normal;

      begin
        LoadFollowers;

        while S > 0 and then not Zip_EOF loop

          -- 1/ Probabilistic expansion
          if Slen(last_char) = 0 then
            -- follower set is empty for this character
            char_read:= UnZ_IO.Bit_buffer.Read_and_dump(8);
          elsif UnZ_IO.Bit_buffer.Read_and_dump(1) = 0  then
            char_read:= Followers(
              last_char,
              UnZ_IO.Bit_buffer.Read_and_dump(B_Table( Slen(last_char) ))
            );
          else
            char_read:= UnZ_IO.Bit_buffer.Read_and_dump(8);
          end if;

          -- 2/ Expand the resulting Zip.Byte into repeated sequences
          case state is

            when normal =>
              if char_read = DLE_code then
                -- >> Next will be a DLE
                state:= length_a;
              else
                -- >> A single char
                Out_byte(Zip.Byte(char_read));
              end if;

            when length_a =>
              if char_read = 0 then
                -- >> DLE_code & 0 -> was just the Zip.Byte coded DLE_code
                Out_byte(DLE_code);
                state:= normal;
              else
                V:= Unsigned_32(char_read);
                length:= Integer(V and maximum_AND_mask);
                -- The remaining bits of V will be used for the distance
                if length = Integer(maximum_AND_mask) then
                  state:= length_b;
                  -- >> length must be completed before reading distance
                else
                  state:= distance;
                end if;
              end if;

            when length_b =>
              length:= length + char_read;
              state:= distance;

            when distance =>
              length:= length + 3;
              S:= S - UnZip.File_size_type(length);

              UnZ_IO.Copy_or_zero(
                distance   => char_read + 1 + Integer( Shift_Right(V, 8-factor) * 2**8 ),
                length     => length,
                index      => UnZ_Glob.slide_index,
                unflushed  => unflushed
              );
              state:= normal;

          end case;

          last_char:= char_read;  -- store character for next iteration
        end loop;

        UnZ_IO.Flush( UnZ_Glob.slide_index );
      end Unreduce;

      --------[ Method: Explode ]--------

      -- C code by info-zip group, translated to Pascal by Christian Ghisler
      -- based on unz51g.zip

      use UnZip.Decompress.Huffman;

      procedure Get_Tree ( L: out Length_array ) is
        I, K, J, B : Unsigned_32;
        N          : constant Unsigned_32:= L'Length;
        L_Idx      : Integer    := L'First;
        Bytebuf    : Zip.Byte;

      begin
        if full_trace then
          Ada.Text_IO.Put_Line("Begin UnZ_Expl.Get_tree");
        end if;

        UnZ_IO.Read_raw_byte ( Bytebuf );
        I := Unsigned_32(Bytebuf) + 1;
        K := 0;

        loop
          UnZ_IO.Read_raw_byte ( Bytebuf );
          J := Unsigned_32(Bytebuf);
          B := ( J  and  16#0F# ) + 1;
          J := ( J  and  16#F0# ) / 16 + 1;
          if  K + J > N then
            raise Zip.Zip_file_Error;
          end if;

          loop
            L(L_Idx) := Natural(B);
            L_Idx:= L_Idx + 1;
            K:= K + 1;
            J:= J - 1;
            exit when  J = 0;
          end loop;

          I:= I - 1;
          exit when  I = 0;
        end loop;

        if  K /= N then
          raise Zip.Zip_file_Error;
        end if;

        if full_trace then
          Ada.Text_IO.Put_Line("End   UnZ_Expl.Get_tree");
        end if;
      end Get_Tree;

      procedure Explode_Lit ( -- method with 3 trees
        Needed: Integer;
        Tb, Tl, Td : p_Table_list;
        Bb, Bl, Bd : Integer
      )
      is
        S       : Unsigned_32;
        E, N, D : Integer;

        W : Integer:= 0;
        Ct : p_HufT_table; -- current table
        Ci : Natural;                               -- current index
        unflushed: Boolean:= True; -- true while slide not yet unflushed

      begin
        if full_trace then
          Ada.Text_IO.Put_Line("Begin Explode_lit");
        end if;

        UnZ_IO.Bit_buffer.Init;

        S := UnZ_Glob.uncompsize;
        while  S > 0  and  not Zip_EOF  loop
          if UnZ_IO.Bit_buffer.Read_and_dump(1) /= 0 then  -- 1: Litteral
            S:= S - 1;
            Ct:= Tb.table;
            Ci:= UnZ_IO.Bit_buffer.Read_inverted(Bb);

            loop
              E :=  Ct(Ci).extra_bits;
              exit when E <= 16;

              if E = invalid then
                raise Zip.Zip_file_Error;
              end if;

              UnZ_IO.Bit_buffer.Dump( Ct(Ci).bits );
              E:= E - 16;
              Ct:= Ct(Ci).next_table;
              Ci:= UnZ_IO.Bit_buffer.Read_inverted(E);
            end loop;

            UnZ_IO.Bit_buffer.Dump( Ct(Ci).bits );
            UnZ_Glob.slide ( W ):=  Zip.Byte( Ct(Ci).n );
            W:= W + 1;
            UnZ_IO.Flush_if_full(W,unflushed);

          else                                       -- 0: Copy
            D := UnZ_IO.Bit_buffer.Read_and_dump(Needed);
            Ct := Td.table;
            Ci := UnZ_IO.Bit_buffer.Read_inverted(Bd);

            loop
              E := Ct(Ci).extra_bits;
              exit when  E <= 16;

              if E = invalid then
                raise Zip.Zip_file_Error;
              end if;

              UnZ_IO.Bit_buffer.Dump( Ct(Ci).bits );
              E:= E - 16;
              Ct := Ct(Ci).next_table;
              Ci := UnZ_IO.Bit_buffer.Read_inverted(E);
            end loop;

            UnZ_IO.Bit_buffer.Dump( Ct(Ci).bits );
            D := D + Ct(Ci).n;

            Ct := Tl.table;
            Ci := UnZ_IO.Bit_buffer.Read_inverted(Bl);

            loop
              E := Ct(Ci).extra_bits;
              exit when  E <= 16;

              if E = invalid then
                raise Zip.Zip_file_Error;
              end if;

              UnZ_IO.Bit_buffer.Dump( Ct(Ci).bits );
              E:= E - 16;
              Ct := Ct(Ci).next_table;
              Ci := UnZ_IO.Bit_buffer.Read_inverted(E);
            end loop;

            UnZ_IO.Bit_buffer.Dump( Ct(Ci).bits );

            N :=  Ct(Ci).n;
            if  E /= 0 then
              N:= N + UnZ_IO.Bit_buffer.Read_and_dump(8);
            end if;
            S:= S - Unsigned_32(N);

            UnZ_IO.Copy_or_zero(
              distance   => D,
              length     => N,
              index      => W,
              unflushed  => unflushed
            );

          end if;
        end loop;

        UnZ_IO.Flush ( W );
        if Zip_EOF then
          raise UnZip.Read_Error;
        end if;

        if full_trace then
          Ada.Text_IO.Put_Line("End   Explode_lit");
        end if;
      end Explode_Lit;

      procedure Explode_Nolit ( -- method with 2 trees
          Needed: Integer;
          Tl, Td : p_Table_list;
          Bl, Bd : Integer
      )
      is
        S       : Unsigned_32;
        E, N, D : Integer;
        W : Integer:= 0;
        Ct : p_HufT_table; -- current table
        Ci : Natural;                               -- current index
        unflushed: Boolean:= True; -- true while slide not yet unflushed

      begin
        if full_trace then
          Ada.Text_IO.Put_Line("Begin Explode_nolit");
        end if;

        UnZ_IO.Bit_buffer.Init;
        S := UnZ_Glob.uncompsize;
        while  S > 0  and not Zip_EOF  loop
          if UnZ_IO.Bit_buffer.Read_and_dump(1) /= 0 then  -- 1: Litteral
            S:= S - 1;
            UnZ_Glob.slide ( W ):=
              Zip.Byte(UnZ_IO.Bit_buffer.Read_and_dump(8));
            W:= W + 1;
            UnZ_IO.Flush_if_full(W,unflushed);
          else                                       -- 0: Copy
            D:= UnZ_IO.Bit_buffer.Read_and_dump( Needed );
            Ct := Td.table;
            Ci := UnZ_IO.Bit_buffer.Read_inverted(Bd);

            loop
              E := Ct(Ci).extra_bits;
              exit when  E <= 16;

              if E = invalid then
                raise Zip.Zip_file_Error;
              end if;

              UnZ_IO.Bit_buffer.Dump( Ct(Ci).bits );
              E:= E - 16;
              Ct := Ct(Ci).next_table;
              Ci := UnZ_IO.Bit_buffer.Read_inverted(E);
            end loop;

            UnZ_IO.Bit_buffer.Dump( Ct(Ci).bits );

            D :=  D + Ct(Ci).n;
            Ct := Tl.table;
            Ci := UnZ_IO.Bit_buffer.Read_inverted(Bl);

            loop
              E := Ct(Ci).extra_bits;
              exit when  E <= 16;

              if E = invalid then
                raise Zip.Zip_file_Error;
              end if;

              UnZ_IO.Bit_buffer.Dump( Ct(Ci).bits );
              E:= E - 16;
              Ct := Ct(Ci).next_table;
              Ci := UnZ_IO.Bit_buffer.Read_inverted(E);
            end loop;

            UnZ_IO.Bit_buffer.Dump( Ct(Ci).bits );

            N := Ct(Ci).n;
            if  E /= 0 then
              N:= N + UnZ_IO.Bit_buffer.Read_and_dump(8);
            end if;
            S:= S - Unsigned_32(N);

            UnZ_IO.Copy_or_zero(
              distance   => D,
              length     => N,
              index      => W,
              unflushed  => unflushed
            );

          end if;
        end loop;

        UnZ_IO.Flush ( W );
        if Zip_EOF then
          raise UnZip.Read_Error;
        end if;

        if full_trace then
          Ada.Text_IO.Put_Line("End   Explode_nolit");
        end if;

      end Explode_Nolit;

      procedure Explode( literal_tree, slide_8_KB: Boolean ) is

        Tb, Tl, Td : p_Table_list;
        Bb, Bl, Bd : Integer;
        L:  Length_array( 0..255 );
        huft_incomplete : Boolean;

        cp_length_2_trees :
          constant Length_array( 0..63 ) :=
         (  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17,
           18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34,
           35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51,
           52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65 );

        cp_length_3_trees :
          constant Length_array( 0..63 ) :=
         (  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
           19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35,
           36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52,
           53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66 );

        cp_dist_4KB :
          constant Length_array( 0..63 ) :=
         ( 1, 65, 129, 193, 257, 321, 385, 449, 513, 577, 641, 705,
           769, 833, 897, 961, 1025, 1089, 1153, 1217, 1281, 1345, 1409, 1473,
           1537, 1601, 1665, 1729, 1793, 1857, 1921, 1985, 2049, 2113, 2177,
           2241, 2305, 2369, 2433, 2497, 2561, 2625, 2689, 2753, 2817, 2881,
           2945, 3009, 3073, 3137, 3201, 3265, 3329, 3393, 3457, 3521, 3585,
           3649, 3713, 3777, 3841, 3905, 3969, 4033 );

        cp_dist_8KB :
          constant Length_array( 0..63 ) :=
         (    1,  129,  257,  385,  513,  641,  769,  897, 1025, 1153, 1281,
           1409, 1537, 1665, 1793, 1921, 2049, 2177, 2305, 2433, 2561, 2689,
           2817, 2945, 3073, 3201, 3329, 3457, 3585, 3713, 3841, 3969, 4097,
           4225, 4353, 4481, 4609, 4737, 4865, 4993, 5121, 5249, 5377, 5505,
           5633, 5761, 5889, 6017, 6145, 6273, 6401, 6529, 6657, 6785, 6913,
           7041, 7169, 7297, 7425, 7553, 7681, 7809, 7937, 8065 );

        extra :
          constant Length_array( 0..63 ) := ( 0..62 => 0, 63 => 8 );

      begin
        Bl := 7;
        if UnZ_Glob.compsize > 200000 then
          Bd := 8;
        else
          Bd := 7;
        end if;

        if literal_tree then
          Bb := 9;
          Get_Tree ( L );
          begin
            HufT_build( L, 256, empty, empty, Tb, Bb, huft_incomplete );
            if huft_incomplete then
              HufT_free (Tb);
              raise Zip.Zip_file_Error;
            end if;
          exception
            when others =>
              raise Zip.Zip_file_Error;
          end;

          begin
            Get_Tree ( L( 0..63 ) );
          exception
            when others=>
              HufT_free( Tb );
              raise Zip.Zip_file_Error;
          end;

          begin
            HufT_build (
              L( 0..63 ), 0, cp_length_3_trees, extra, Tl, Bl, huft_incomplete
            );
            if huft_incomplete then
              HufT_free(Tl);
              HufT_free(Tb);
              raise Zip.Zip_file_Error;
            end if;
          exception
            when others =>
              HufT_free (Tb);
              raise Zip.Zip_file_Error;
          end;

          begin
            Get_Tree ( L( 0..63 ) );
          exception
            when others=>
              HufT_free( Tb );
              HufT_free( Tl );
              raise Zip.Zip_file_Error;
          end;

          begin
            if slide_8_KB then
              HufT_build (
                L( 0..63 ), 0, cp_dist_8KB, extra, Td, Bd, huft_incomplete
              );
              if huft_incomplete then
                HufT_free(Td);
                HufT_free(Tl);
                HufT_free(Tb);
                raise Zip.Zip_file_Error;
              end if;
              -- Exploding, method: 8k slide, 3 trees
              Explode_Lit ( 7, Tb, Tl, Td, Bb, Bl, Bd );
            else
              HufT_build (
                L( 0..63 ), 0, cp_dist_4KB, extra, Td, Bd, huft_incomplete
              );
              if huft_incomplete then
                HufT_free(Td);
                HufT_free(Tl);
                HufT_free(Tb);
                raise Zip.Zip_file_Error;
              end if;
              -- Exploding, method: 4k slide, 3 trees
              Explode_Lit ( 6, Tb, Tl, Td, Bb, Bl, Bd );
            end if;
          exception
            when  others =>
              HufT_free(Tl);
              HufT_free(Tb);
              raise Zip.Zip_file_Error;
          end;
          HufT_free ( Td );
          HufT_free ( Tl );
          HufT_free ( Tb );

        else         -- No literal tree

          begin
            Get_Tree ( L( 0..63 ) );
          exception
            when others=>
              raise Zip.Zip_file_Error;
          end;

          begin
            HufT_build (
              L( 0..63 ), 0, cp_length_2_trees, extra, Tl, Bl, huft_incomplete
            );
            if huft_incomplete then
              HufT_free(Tl);
              raise Zip.Zip_file_Error;
            end if;
          exception
            when others =>
              raise Zip.Zip_file_Error;
          end;

          begin
            Get_Tree ( L( 0..63 ) );
          exception
            when others=>
              HufT_free (Tl);
              raise Zip.Zip_file_Error;
          end;

          begin
            if slide_8_KB then
              HufT_build (
                L( 0..63 ), 0, cp_dist_8KB, extra, Td, Bd, huft_incomplete
              );
              if huft_incomplete then
                HufT_free(Td);
                HufT_free(Tl);
                raise Zip.Zip_file_Error;
              end if;
              -- Exploding, method: 8k slide, 2 trees
              Explode_Nolit( 7, Tl, Td, Bl, Bd );
            else
              HufT_build (
                L( 0..63 ), 0, cp_dist_4KB, extra, Td, Bd, huft_incomplete
              );
              if huft_incomplete then
                HufT_free(Td);
                HufT_free(Tl);
                raise Zip.Zip_file_Error;
              end if;
              -- Exploding, method: 4k slide, 2 trees
              Explode_Nolit( 6, Tl, Td, Bl, Bd );
            end if;
          exception
            when others=>
              HufT_free(Tl);
              raise Zip.Zip_file_Error;
          end;
          HufT_free ( Td );
          HufT_free ( Tl );
        end if;

      end Explode;

      --------[ Method: Copy stored ]--------

      procedure Copy_stored is
        size: constant UnZip.File_size_type:= UnZ_Glob.compsize;
        read_in, absorbed : UnZip.File_size_type;
      begin
        absorbed:= 0;
        if UnZ_IO.Decryption.Get_mode then
          absorbed:= 12;
        end if;
        while absorbed < size loop
          read_in := size - absorbed;
          if read_in > wsize then
            read_in := wsize;
          end if;
          begin
            for I in 0 .. read_in-1 loop
              UnZ_IO.Read_raw_byte( UnZ_Glob.slide( Natural(I) ) );
            end loop;
          exception
            when others=>
              raise UnZip.Read_Error;
          end;
          begin
            UnZ_IO.Flush ( Natural(read_in) );  -- Takes care of CRC too
          exception
            when User_abort =>
              raise;
            when others =>
              raise UnZip.Write_Error;
          end;
          absorbed:= absorbed + read_in;
        end loop;
      end Copy_stored;

      --------[ Method: Inflate ]--------

      procedure Inflate_Codes ( Tl, Td: p_Table_list; Bl, Bd: Integer ) is
        CTE    : p_HufT;       -- current table element
        length : Natural;
        E      : Integer;      -- table entry flag/number of extra bits
        W      : Integer:= UnZ_Glob.slide_index;
        -- more local variable for slide index
      begin
        if full_trace then
          Ada.Text_IO.Put_Line("Begin Inflate_codes");
        end if;

        -- inflate the coded data
        main_loop:
        while not Zip_EOF loop
          CTE:= Tl.table( UnZ_IO.Bit_buffer.Read(Bl) )'Access;

          loop
            E := CTE.extra_bits;
            exit when E <= 16;
            if E = invalid then
              raise Zip.Zip_file_Error;
            end if;

            -- then it's a literal
            UnZ_IO.Bit_buffer.Dump( CTE.bits );
            E:= E - 16;
            CTE := CTE.next_table( UnZ_IO.Bit_buffer.Read(E) )'Access;
          end loop;

          UnZ_IO.Bit_buffer.Dump ( CTE.bits );

          case E is
            when 16 =>     -- CTE.N is a Litteral
              UnZ_Glob.slide ( W ) :=  Zip.Byte( CTE.n );
              W:= W + 1;
              UnZ_IO.Flush_if_full(W);

            when 15 =>     -- End of block (EOB, code 256)
              if full_trace then
                Ada.Text_IO.Put_Line("Exit  Inflate_codes, e=15 -> EOB");
              end if;
              exit main_loop;

            when others => -- We have a length/distance

              -- Get length of block to copy:
              length:= CTE.n + UnZ_IO.Bit_buffer.Read_and_dump(E);

              -- Decode distance of block to copy:
              CTE := Td.table( UnZ_IO.Bit_buffer.Read(Bd) )'Access;
              loop
                E := CTE.extra_bits;
                exit when E <= 16;
                if E = invalid then
                  raise Zip.Zip_file_Error;
                end if;
                UnZ_IO.Bit_buffer.Dump( CTE.bits );
                E:= E - 16;
                CTE := CTE.next_table( UnZ_IO.Bit_buffer.Read(E) )'Access;
              end loop;
              UnZ_IO.Bit_buffer.Dump( CTE.bits );
              UnZ_IO.Copy(
                distance => CTE.n + UnZ_IO.Bit_buffer.Read_and_dump(E),
                length   => length,
                index    => W
              );
          end case;
        end loop main_loop;

        UnZ_Glob.slide_index:= W;

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
          raise Zip.Zip_file_Error;
        end if;
        while N > 0  and then not Zip_EOF loop
          -- Read and output the non-compressed data
          N:= N - 1;
          UnZ_Glob.slide ( UnZ_Glob.slide_index ) :=
            Zip.Byte( UnZ_IO.Bit_buffer.Read_and_dump(8) );
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
        Tl,                        -- literal/length code table
          Td : p_Table_list;            -- distance code table
        Bl, Bd : Integer;          -- lookup bits for tl/bd
        huft_incomplete : Boolean;

        -- length list for HufT_build (literal table)
        L: constant Length_array( 0..287 ):=
          ( 0..143=> 8, 144..255=> 9, 256..279=> 7, 280..287=> 8);

      begin
        if some_trace then
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
            raise Zip.Zip_file_Error;
        end;

        Inflate_Codes ( Tl, Td, Bl, Bd );

        HufT_free ( Tl );
        HufT_free ( Td );

        if some_trace then
          Ada.Text_IO.Put_Line("End   Inflate_fixed_block");
        end if;
      end Inflate_fixed_block;

      procedure Inflate_dynamic_block is
        bit_order : constant array ( 0..18 ) of Natural :=
         ( 16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15 );

        Lbits : constant:= 9;
        Dbits : constant:= 6;

        current_length: Natural;
        defined, number_of_lengths: Natural;

        Tl,                             -- literal/length code tables
          Td : p_Table_list;            -- distance code tables

        CTE : p_HufT;  -- current table element

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
            raise Zip.Zip_file_Error;
          end if;
          for c in reverse 1..amount loop
            Ll ( defined ) := current_length;
            defined:= defined + 1;
          end loop;
        end Repeat_length_code;

      begin
        if some_trace then
          Ada.Text_IO.Put_Line("Begin Inflate_dynamic_block");
        end if;

        -- Read in table lengths
        Nl := 257 + UnZ_IO.Bit_buffer.Read_and_dump(5);
        Nd :=   1 + UnZ_IO.Bit_buffer.Read_and_dump(5);
        Nb :=   4 + UnZ_IO.Bit_buffer.Read_and_dump(4);

        if Nl > 288 or else Nd > 32 then
          raise Zip.Zip_file_Error;
        end if;

        -- Read in bit-length-code lengths.
        -- The rest, Ll( Bit_Order( Nb .. 18 ) ), is already = 0
        for J in  0 .. Nb - 1  loop
          Ll ( bit_order( J ) ) := UnZ_IO.Bit_buffer.Read_and_dump(3);
        end loop;

        -- Build decoding table for trees--single level, 7 bit lookup
        Bl := 7;
        begin
          HufT_build (
            Ll( 0..18 ), 19, empty, empty, Tl, Bl, huft_incomplete
          );
          if huft_incomplete then
            HufT_free(Tl);
            raise Zip.Zip_file_Error;
          end if;
        exception
          when others =>
            raise Zip.Zip_file_Error;
        end;

        -- Read in literal and distance code lengths
        number_of_lengths := Nl + Nd;
        defined := 0;
        current_length := 0;

        while  defined < number_of_lengths  loop
          CTE:= Tl.table( UnZ_IO.Bit_buffer.Read(Bl) )'Access;
          UnZ_IO.Bit_buffer.Dump( CTE.bits );

          case CTE.n is
            when 0..15 =>       -- length of code in bits (0..15)
              current_length:= CTE.n;
              Ll (defined) := current_length;
              defined:= defined + 1;

            when 16 =>          -- repeat last length 3 to 6 times
              Repeat_length_code(3 + UnZ_IO.Bit_buffer.Read_and_dump(2));

            when 17 =>          -- 3 to 10 zero length codes
              current_length:= 0;
              Repeat_length_code(3 + UnZ_IO.Bit_buffer.Read_and_dump(3));

            when 18 =>          -- 11 to 138 zero length codes
              current_length:= 0;
              Repeat_length_code(11 + UnZ_IO.Bit_buffer.Read_and_dump(7));

            when others =>
              if full_trace then
                Ada.Text_IO.Put_Line(
                  "Illegal length code: " &
                  Integer'Image(CTE.n)
                );
              end if;

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
            raise Zip.Zip_file_Error;
          end if;
        exception
          when others =>
            raise Zip.Zip_file_Error;
        end;

        -- Build the decoding tables for distance codes
        Bd := Dbits;
        begin
          HufT_build (
            Ll( Nl..Nl+Nd-1 ), 0,
            copy_offset_distance, extra_bits_distance,
            Td, Bd, huft_incomplete
          );
          if huft_incomplete then -- do nothing!
            if some_trace then
              Ada.Text_IO.Put_Line("Huffman tree incomplete - PKZIP 1.93a bug workaround");
            end if;
          end if;
        exception
          when huft_out_of_memory | huft_error =>
            HufT_free(Tl);
            raise Zip.Zip_file_Error;
        end;

        -- Decompress until an end-of-block code

        Inflate_Codes ( Tl, Td, Bl, Bd );
        HufT_free ( Tl );
        HufT_free ( Td );

        if some_trace then
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
          when others => raise Zip.Zip_file_Error; -- Bad block type (3)
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
      end Inflate;

      --------[ Method: BZip2 ]--------

      procedure Bunzip2 is
        type BZ_Buffer is array(Natural range <>) of Interfaces.Unsigned_8;
        procedure Read( b: out BZ_Buffer ) is
        begin
          for i in b'Range loop
            exit when Zip_EOF;
            UnZ_IO.Read_raw_byte(b(i));
          end loop;
        end Read;
        procedure Write( b: in BZ_Buffer ) is
        begin
          for i in b'Range loop
            UnZ_Glob.slide ( UnZ_Glob.slide_index ) := b(i);
            UnZ_Glob.slide_index:= UnZ_Glob.slide_index + 1;
            UnZ_IO.Flush_if_full(UnZ_Glob.slide_index);
          end loop;
        end Write;
        package My_BZip2 is new BZip2
          ( Buffer    => BZ_Buffer,
            check_CRC => False, -- Already done by UnZ_IO
            Read      => Read,
            Write     => Write
          );
      begin
        My_BZip2.Decompress;
        UnZ_IO.Flush( UnZ_Glob.slide_index );
      end Bunzip2;

    end UnZ_Meth;

    procedure Process_descriptor(dd: out Zip.Headers.Data_descriptor)
    is
      start: Integer;
      b: Unsigned_8;
      dd_buffer: Zip.Byte_Buffer(1..30);
    begin
      UnZ_IO.Bit_buffer.Dump_to_byte_boundary;
      UnZ_IO.Decryption.Set_mode(False);
      UnZ_IO.Read_raw_byte(b);
      if b = 75 then -- 'K' ('P' is before, this is a Java/JAR bug!)
        dd_buffer(1):= 80;
        dd_buffer(2):= 75;
        start:= 3;
      else
        dd_buffer(1):= b; -- hopefully = 80 (will be checked)
        start:= 2;
      end if;
      for i in start..16 loop
        UnZ_IO.Read_raw_byte( dd_buffer(i) );
      end loop;
      Zip.Headers.Copy_and_check( dd_buffer, dd );
    exception
      when Zip.Headers.Bad_data_descriptor =>
        raise Zip.Zip_file_Error;
    end Process_descriptor;

    work_index: Ada.Streams.Stream_IO.Positive_Count;
    use Zip, UnZ_Meth;

  begin -- Decompress_Data
    output_memory_access:= null;
    -- ^ this is an 'out' parameter, we have to set it anyway
    case mode is
      when write_to_binary_file =>
         Ada.Streams.Stream_IO.Create(UnZ_IO.out_bin_file,Ada.Streams.Stream_IO.Out_File, output_file_name,
                                      Form => To_String (Zip_Streams.Form_For_IO_Open_and_Create));
      when write_to_text_file =>
         Ada.Text_IO.Create(UnZ_IO.out_txt_file, Ada.Text_IO.Out_File, output_file_name,
                               Form => To_String (Zip_Streams.Form_For_IO_Open_and_Create));
      when write_to_memory =>
        output_memory_access:= new
          Ada.Streams.Stream_Element_Array(
            1 .. Ada.Streams.Stream_Element_Offset(hint.dd.uncompressed_size)
          );
        UnZ_Glob.uncompressed_index := output_memory_access'First;
      when just_test =>
        null;
    end case;

    UnZ_Glob.compsize  := hint.dd.compressed_size;
    -- 2008: from TT's version:
    -- Avoid wraparound in read_buffer, when File_size_type'Last is given
    -- as hint.compressed_size (unknown size)
    if UnZ_Glob.compsize > File_size_type'Last - 2 then
      UnZ_Glob.compsize:= File_size_type'Last - 2;
    end if;
    UnZ_Glob.uncompsize:= hint.dd.uncompressed_size;
    UnZ_IO.Init_Buffers;
    UnZ_IO.Decryption.Set_mode( encrypted );
    if encrypted then
      work_index := Ada.Streams.Stream_IO.Positive_Count (Zip_Streams.Index(zip_file));
      password_passes: for p in 1..tolerance_wrong_password loop
        begin
          UnZ_IO.Decryption.Init( To_String(password), hint.dd.crc_32 );
          exit password_passes; -- the current password fits, then go on!
        exception
          when Wrong_password =>
            if p = tolerance_wrong_password then
              raise;
            end if; -- alarm!
            if get_new_password /= null then
              get_new_password( password ); -- ask for a new one
            end if;
        end;
        -- Go back to data beginning:
        begin
          Zip_Streams.Set_Index ( zip_file, Positive(work_index) );
        exception
          when others =>
            raise Read_Error;
        end;
        UnZ_IO.Init_Buffers;
      end loop password_passes;
    end if;

    -- UnZip correct type
    begin
      case format is
        when store    => Copy_stored;
        when shrink   => Unshrink;
        when reduce_1 => Unreduce(1);
        when reduce_2 => Unreduce(2);
        when reduce_3 => Unreduce(3);
        when reduce_4 => Unreduce(4);
        when implode  =>
          UnZ_Meth.Explode( explode_literal_tree, explode_slide_8KB );
        when deflate | deflate_e =>
          UnZ_Meth.deflate_e_mode:= format = deflate_e;
          UnZ_Meth.Inflate;
        when Zip.bzip2 => UnZ_Meth.Bunzip2;
        when others =>
          Ada.Exceptions.Raise_Exception
            (Unsupported_method'Identity,
             "Format (method) " & PKZip_method'Image(format) &
             " is not supported for decompression");
      end case;
    exception
      when others =>
        UnZ_IO.Delete_output;
        raise;
    end;
    UnZ_Glob.crc32val := Zip.CRC.Final( UnZ_Glob.crc32val );
    -- Decompression done !

    if data_descriptor_after_data then -- Sizes and CRC at the end
      declare
       memo_uncomp_size: constant Unsigned_32:=
         hint.dd.uncompressed_size;
      begin
        Process_descriptor(hint.dd); -- CRC is for checking; sizes are for informing user
        if memo_uncomp_size < Unsigned_32'Last and then --
           memo_uncomp_size /= hint.dd.uncompressed_size
        then
          UnZ_IO.Delete_output;
          raise Uncompressed_size_Error;
        end if;
      end;
    end if;

    if hint.dd.crc_32 /= UnZ_Glob.crc32val then
      UnZ_IO.Delete_output;
      raise CRC_Error;
    end if;

    case mode is
      when write_to_binary_file =>
        Ada.Streams.Stream_IO.Close( UnZ_IO.out_bin_file );
      when write_to_text_file =>
        Ada.Text_IO.Close( UnZ_IO.out_txt_file );
      when others =>
        null;
    end case;

  exception

    when others => -- close the file in case of an error, if not yet closed
      case mode is -- or deleted
        when write_to_binary_file =>
          if Ada.Streams.Stream_IO.Is_Open( UnZ_IO.out_bin_file ) then
            Ada.Streams.Stream_IO.Close( UnZ_IO.out_bin_file );
          end if;
        when write_to_text_file =>
          if Ada.Text_IO.Is_Open( UnZ_IO.out_txt_file ) then
            Ada.Text_IO.Close( UnZ_IO.out_txt_file );
          end if;
        when others =>
          null;
      end case;
      raise;

  end Decompress_data;

end UnZip.Decompress;
