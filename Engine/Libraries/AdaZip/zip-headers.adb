with Ada.Streams; use Ada.Streams;
package body Zip.Headers is

  -----------------------------------------------------------
  -- Byte array <-> various integers, with Intel endianess --
  -----------------------------------------------------------

  -- Get numbers with correct trucmuche endian, to ensure
  -- correct header loading on some non-Intel machines

  generic
    type Number is mod <>; -- range <> in Ada83 version (fake Interfaces)
  function Intel_x86_number( b: Byte_Buffer ) return Number;

  function Intel_x86_number( b: Byte_Buffer ) return Number is
    n: Number:= 0;
  begin
    for i in reverse b'Range loop
      n:= n * 256 + Number(b(i));
    end loop;
    return n;
  end Intel_x86_number;

  function Intel_nb is new Intel_x86_number( Unsigned_16 );
  function Intel_nb is new Intel_x86_number( Unsigned_32 );

  -- Put numbers with correct endianess as bytes

  generic
    type Number is mod <>; -- range <> in Ada83 version (fake Interfaces)
    size: Positive;
  function Intel_x86_buffer( n: Number ) return Byte_Buffer;

  function Intel_x86_buffer( n: Number ) return Byte_Buffer is
    b: Byte_Buffer(1..size);
    m: Number:= n;
  begin
    for i in b'Range loop
      b(i):= Unsigned_8(m and 255);
      m:= m / 256;
    end loop;
    return b;
  end Intel_x86_buffer;

  function Intel_bf is new Intel_x86_buffer( Unsigned_16, 2 );
  function Intel_bf is new Intel_x86_buffer( Unsigned_32, 4 );

  -------------------
  -- PK signatures --
  -------------------

  function PK_signature( buf: Byte_Buffer; code: Unsigned_8 ) return Boolean is
  begin
    return buf(buf'First .. buf'First+3) = (16#50#, 16#4B#, code, code+1);
    -- PK12, PK34, ...
  end PK_signature;

  procedure PK_signature( buf: in out Byte_Buffer; code: Unsigned_8 ) is
  begin
    buf(1..4) := (16#50#, 16#4B#, code, code+1); -- PK12, PK34, ...
  end PK_signature;

  -------------------------------------------------------
  -- PKZIP file header, as in central directory - PK12 --
  -------------------------------------------------------
  procedure Read_and_check(
    stream : in out Root_Zipstream_Type'Class;
    header :    out Central_File_Header
  )
  is
    chb: Byte_Buffer( 1..46 );
  begin
    BlockRead(stream, chb);

    if not PK_signature(chb, 1) then
      raise bad_central_header;
    end if;

    header.made_by_version:=                   Intel_nb( chb( 5.. 6) );
    header.short_info.needed_extract_version:= Intel_nb( chb( 7.. 8) );
    header.short_info.bit_flag:=               Intel_nb( chb( 9..10) );
    header.short_info.zip_type:=               Intel_nb( chb(11..12) );
    header.short_info.file_timedate:=
     Zip_Streams.Calendar.Convert(Unsigned_32'(Intel_nb( chb(13..16) )));
    header.short_info.dd.crc_32:=              Intel_nb( chb(17..20) );
    header.short_info.dd.compressed_size:=     Intel_nb( chb(21..24) );
    header.short_info.dd.uncompressed_size:=   Intel_nb( chb(25..28) );
    header.short_info.filename_length:=        Intel_nb( chb(29..30) );
    header.short_info.extra_field_length:=     Intel_nb( chb(31..32) );
    header.comment_length:=                    Intel_nb( chb(33..34) );
    header.disk_number_start:=                 Intel_nb( chb(35..36) );
    header.internal_attributes:=               Intel_nb( chb(37..38) );
    header.external_attributes:=               Intel_nb( chb(39..42) );
    header.local_header_offset:=               Intel_nb( chb(43..46) );

  end Read_and_check;

  procedure Write(
    stream : in out Root_Zipstream_Type'Class;
    header : in     Central_File_Header
  )
  is
    chb: Byte_Buffer( 1..46 );
  begin
    PK_signature(chb, 1);

    chb( 5.. 6):= Intel_bf( header.made_by_version );
    chb( 7.. 8):= Intel_bf( header.short_info.needed_extract_version );
    chb( 9..10):= Intel_bf( header.short_info.bit_flag );
    chb(11..12):= Intel_bf( header.short_info.zip_type );
    chb(13..16):= Intel_bf( Zip_Streams.Calendar.Convert(
                            header.short_info.file_timedate)
                          );
    chb(17..20):= Intel_bf( header.short_info.dd.crc_32 );
    chb(21..24):= Intel_bf( header.short_info.dd.compressed_size );
    chb(25..28):= Intel_bf( header.short_info.dd.uncompressed_size );
    chb(29..30):= Intel_bf( header.short_info.filename_length );
    chb(31..32):= Intel_bf( header.short_info.extra_field_length );
    chb(33..34):= Intel_bf( header.comment_length );
    chb(35..36):= Intel_bf( header.disk_number_start );
    chb(37..38):= Intel_bf( header.internal_attributes );
    chb(39..42):= Intel_bf( header.external_attributes );
    chb(43..46):= Intel_bf( header.local_header_offset );

    BlockWrite(stream, chb);
  end Write;

  -----------------------------------------------------------------------
  -- PKZIP local file header, in front of every file in archive - PK34 --
  -----------------------------------------------------------------------
  procedure Read_and_check(
    stream : in out Root_Zipstream_Type'Class;
    header :    out Local_File_Header
  )
  is
    lhb: Byte_Buffer( 1..30 );
  begin
    BlockRead(stream, lhb);

    if not PK_signature(lhb, 3) then
      raise bad_local_header;
    end if;

    header.needed_extract_version:= Intel_nb( lhb( 5.. 6) );
    header.bit_flag:=               Intel_nb( lhb( 7.. 8) );
    header.zip_type:=               Intel_nb( lhb( 9..10) );
    header.file_timedate:= Zip_Streams.Calendar.Convert(Unsigned_32'(
                                    Intel_nb( lhb(11..14) )
                                  ));
    header.dd.crc_32:=              Intel_nb( lhb(15..18) );
    header.dd.compressed_size:=     Intel_nb( lhb(19..22) );
    header.dd.uncompressed_size:=   Intel_nb( lhb(23..26) );
    header.filename_length:=        Intel_nb( lhb(27..28) );
    header.extra_field_length:=     Intel_nb( lhb(29..30) );

  end Read_and_check;

  procedure Write(
    stream : in out Root_Zipstream_Type'Class;
    header : in     Local_File_Header
  )
  is
    lhb: Byte_Buffer( 1..30 );
  begin
    PK_signature(lhb, 3);

    lhb( 5.. 6):= Intel_bf( header.needed_extract_version );
    lhb( 7.. 8):= Intel_bf( header.bit_flag );
    lhb( 9..10):= Intel_bf( header.zip_type );
    lhb(11..14):= Intel_bf( Zip_Streams.Calendar.Convert(header.file_timedate) );
    lhb(15..18):= Intel_bf( header.dd.crc_32 );
    lhb(19..22):= Intel_bf( header.dd.compressed_size );
    lhb(23..26):= Intel_bf( header.dd.uncompressed_size );
    lhb(27..28):= Intel_bf( header.filename_length );
    lhb(29..30):= Intel_bf( header.extra_field_length );

    BlockWrite(stream, lhb);
  end Write;

  -------------------------------------------
  -- PKZIP end-of-central-directory - PK56 --
  -------------------------------------------
  procedure Copy_and_check(
    buffer  : in     Byte_Buffer;
    the_end :    out End_of_Central_Dir
  )
  is
    o: constant Integer:= buffer'First - 1;
  begin
    if not PK_signature(buffer, 5) then
      raise bad_end;
    end if;

    the_end.disknum:=              Intel_nb( buffer(o+ 5..o+ 6) );
    the_end.disknum_with_start:=   Intel_nb( buffer(o+ 7..o+ 8) );
    the_end.disk_total_entries:=   Intel_nb( buffer(o+ 9..o+10) );
    the_end.total_entries:=        Intel_nb( buffer(o+11..o+12) );
    the_end.central_dir_size:=     Intel_nb( buffer(o+13..o+16) );
    the_end.central_dir_offset:=   Intel_nb( buffer(o+17..o+20) );
    the_end.main_comment_length:=  Intel_nb( buffer(o+21..o+22) );

  end Copy_and_check;

  procedure Read_and_check(
    stream  : in out Root_Zipstream_Type'Class;
    the_end :    out End_of_Central_Dir
  )
  is
    eb: Byte_Buffer( 1..22 );
  begin
    BlockRead(stream, eb);
    Copy_and_check(eb, the_end);
  end Read_and_check;

  procedure Load(
    stream  : in out Root_Zipstream_Type'Class;
    the_end :    out End_of_Central_Dir
  )
  is
    min_end_start: Ada.Streams.Stream_IO.Count;
    use Ada.Streams.Stream_IO;
    max_comment: constant:= 65_535;
    -- In appnote.txt :
    -- .ZIP file comment length        2 bytes
  begin
    -- 20-Jun-2001: abandon search below min_end_start.
    if Size(stream) <= max_comment then
      min_end_start:= 1;
    else
      min_end_start:= Ada.Streams.Stream_IO.Count(Size(stream)) - max_comment;
    end if;
    Zip_Streams.Set_Index(stream, Positive(min_end_start));
    declare
      -- We copy a large chunk of the zip stream's tail into a buffer.
      large_buffer: Byte_Buffer(Integer(min_end_start) .. Natural(Size(stream)));
    begin
      BlockRead(stream, large_buffer);
      for i in reverse Integer(min_end_start) .. Size(stream) - 21 loop
        -- Yes, we must _search_ for the header...
        -- because PKWARE put a variable-size comment _after_ it 8-(
        if PK_signature(large_buffer(i .. i + 3), 5) then
          Copy_and_check( large_buffer(i .. i + 21), the_end );
          -- At this point, the buffer was successfully read, the_end is
          -- is set with its standard contents.
          the_end.offset_shifting:=
          -- This is the real position of the end-of-central-directory block.
          Unsigned_32(i)
          -
          -- This is the theoretical position of the end-of-central-directory,
          -- block. Should coincide with the real position if the zip file
          -- is not appended.
          (
            1 +
            the_end.central_dir_offset +
            the_end.central_dir_size
          );
          Zip_Streams.Set_Index(stream, Positive(i + 22));
          return; -- the_end found and filled -> exit
        end if;
      end loop;
      raise bad_end; -- Definitely no "end-of-central-directory" in this stream
    end;
  end Load;

  procedure Write(
    stream  : in out Root_Zipstream_Type'Class;
    the_end : in     End_of_Central_Dir
  )
  is
    eb: Byte_Buffer( 1..22 );
  begin
    PK_signature(eb, 5);

    eb( 5.. 6):= Intel_bf( the_end.disknum );
    eb( 7.. 8):= Intel_bf( the_end.disknum_with_start );
    eb( 9..10):= Intel_bf( the_end.disk_total_entries );
    eb(11..12):= Intel_bf( the_end.total_entries );
    eb(13..16):= Intel_bf( the_end.central_dir_size );
    eb(17..20):= Intel_bf( the_end.central_dir_offset );
    eb(21..22):= Intel_bf( the_end.main_comment_length );

    BlockWrite(stream, eb);
  end Write;

  ------------------------------------------------------------------
  -- PKZIP data descriptor, after streamed compressed data - PK78 --
  ------------------------------------------------------------------
  procedure Copy_and_check(
    buffer        : in     Byte_Buffer;
    the_data_desc :    out Data_descriptor
  )
  is
  begin
    if not PK_signature(buffer, 7) then
      raise bad_data_descriptor;
    end if;

    the_data_desc.crc_32:=             Intel_nb( buffer(5..8) );
    the_data_desc.compressed_size:=    Intel_nb( buffer(9..12) );
    the_data_desc.uncompressed_size:=  Intel_nb( buffer(13..16) );

  end Copy_and_check;

  procedure Read_and_check(
    stream        : in out Root_Zipstream_Type'Class;
    the_data_desc :    out Data_descriptor
  )
  is
    ddb: Byte_Buffer( 1..16 );
  begin
    BlockRead(stream, ddb);
    Copy_and_check(ddb, the_data_desc);
  end Read_and_check;

  procedure Write(
    stream        : in out Root_Zipstream_Type'Class;
    the_data_desc : in     Data_descriptor
  )
  is
    ddb: Byte_Buffer( 1..16 );
  begin
    PK_signature(ddb, 7);

    ddb( 5.. 8):= Intel_bf( the_data_desc.crc_32 );
    ddb( 9..12):= Intel_bf( the_data_desc.compressed_size );
    ddb(13..16):= Intel_bf( the_data_desc.uncompressed_size );

    BlockWrite(stream, ddb);
  end Write;

end Zip.Headers;
