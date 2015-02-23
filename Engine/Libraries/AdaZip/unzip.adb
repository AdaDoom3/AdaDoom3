with Zip.Headers, UnZip.Decompress;
with Zip_Streams;

with Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Interfaces;                        use Interfaces;

package body UnZip is

  use Ada.Streams, Ada.Strings.Unbounded;

  boolean_to_encoding: constant array(Boolean) of Zip.Zip_name_encoding:=
    (False => Zip.IBM_437, True => Zip.UTF_8);

  --------------------------------------------------
  -- *The* internal 1-file unzipping procedure.   --
  -- Input must be _open_ and won't be _closed_ ! --
  --------------------------------------------------

  procedure UnZipFile (
    zip_file                 : in out Zip_Streams.Root_Zipstream_Type'Class;
    out_name                 : String;
    out_name_encoding        : Zip.Zip_name_encoding;
    name_from_header         : Boolean;
    header_index             : in out Positive;
    hint_comp_size           : File_size_type; -- Added 2007 for .ODS files
    hint_crc_32              : Unsigned_32;    -- Added 2012 for decryption
    feedback                 : Zip.Feedback_proc;
    help_the_file_exists     : Resolve_conflict_proc;
    tell_data                : Tell_data_proc;
    get_pwd                  : Get_password_proc;
    options                  : Option_set;
    password                 : in out Unbounded_String;
    file_system_routines     : FS_routines_type
  )
  is
    work_index: Positive := header_index;
    local_header: Zip.Headers.Local_File_Header;
    data_descriptor_after_data: Boolean;
    method: PKZip_method;

    skip_this_file: Boolean:= False;
    bin_text_mode: constant array(Boolean) of Write_mode:=
      (write_to_binary_file, write_to_text_file);
    mode: constant array(Boolean) of Write_mode:=
      (bin_text_mode( options(extract_as_text) ), just_test);
    actual_mode: Write_mode:= mode( options(test_only) );

    true_packed_size: File_size_type; -- encryption adds 12 to packed size

    the_output_name: Unbounded_String;

    -- 27-Jun-2001 : possibility of trashing directory part of a name
    --               e.g. :  unzipada\uza_src\unzip.ads -> unzip.ads
    function Maybe_trash_dir( n: String ) return String is
      idx: Integer:= n'First - 1;
    begin
      if options( junk_directories ) then
        for i in n'Range loop
          if n(i)= '/' or n(i)='\' then
            idx:= i;
          end if;
        end loop;
        -- idx points on the index just before the interesting part
        return n( idx+1 .. n'Last );
      else
        return n;
      end if;
    end Maybe_trash_dir;

    procedure Set_definitively_named_outfile( composed_name: String ) is
      idx: Integer:= composed_name'First - 1;
      first_in_name: Integer;
    begin
      for i in composed_name'Range loop
        if composed_name(i)= '/' or composed_name(i)='\' then
          idx:= i;
        end if;
      end loop;
      -- idx points on the index just before the name part

      if idx >= composed_name'First and then
         actual_mode in Write_to_file and then
         file_system_routines.Create_Path /= null
      then
        -- Not only the name, also a path.
        -- In that case, we may need to create parts of the path.
        declare
          Directory_Separator: constant Character:= '/';
          -- The '/' separator is also recognized by Windows' routines,
          -- so we can just use it as a standard. See the discussion started
          -- in July 2010 in the Ada Comment mailing list about it
          -- for the 2012 standard.
          path: String:= composed_name(composed_name'First..idx-1);
        begin
          -- Set the file separator recognized by the O.S.
          for i in path'Range loop
            if path(i)='\' or path(i)='/' then
              path(i):= Directory_Separator;
            end if;
          end loop;
          file_system_routines.Create_Path( path );
        end;
      end if;
      -- Now we can create the file itself.
      first_in_name:= composed_name'First;
      --
      the_output_name:=
        To_Unbounded_String(composed_name( first_in_name .. composed_name'Last ));
    end Set_definitively_named_outfile;

    function Full_Path_Name (
      file_name_in_archive : String;
      encoding             : Zip.Zip_name_encoding)
    return String
    is
    begin
       if file_system_routines.Compose_File_Name = null then
          return file_name_in_archive;
       else
          return file_system_routines.Compose_File_Name(file_name_in_archive, encoding);
       end if;
    end Full_Path_Name;

    procedure Set_outfile(
      long_not_composed_name: String;
      encoding              : Zip.Zip_name_encoding
    )
    is
      -- Eventually trash the archived directory structure, then
      -- eventually add/modify/... another one:
      name: constant String:=
        Full_Path_Name( Maybe_trash_dir( long_not_composed_name ), encoding );
    begin
      Set_definitively_named_outfile(name);
    end Set_outfile;

    procedure Set_outfile_interactive(
      long_not_composed_possible_name: String;
      encoding                       : Zip.Zip_name_encoding
    )
    is
      -- Eventually trash the archived directory structure, then
      -- eventually add/modify/... another one:
      possible_name: constant String:=
        Full_Path_Name( Maybe_trash_dir( long_not_composed_possible_name ), encoding );
      -- possible_name may have a different encoding depending on Compose_File_Name...
      new_name : String( 1..1024 );
      new_name_length : Natural;
    begin
      if help_the_file_exists /= null and then Zip.Exists(possible_name) then
        loop
          case current_user_attitude is
            when yes | no | rename_it => -- then ask for this name too
              help_the_file_exists(
                long_not_composed_possible_name, encoding,
                current_user_attitude,
                new_name, new_name_length
              );
            when yes_to_all | none | abort_now =>
              exit; -- nothing to decide: previous decision was definitive
          end case;
          exit when not (
            current_user_attitude = rename_it and then -- new name exists too!
            Zip.Exists( new_name( 1..new_name_length ) )
          );
        end loop;

        -- User has decided.
        case current_user_attitude is
          when yes | yes_to_all =>
            skip_this_file:= False;
            Set_definitively_named_outfile(possible_name);
          when no | none =>
            skip_this_file:= True;
          when rename_it =>
            skip_this_file:= False;
            Set_definitively_named_outfile(new_name( 1..new_name_length ));
          when abort_now =>
            raise User_abort;
        end case;

      else -- no name conflict or non-interactive (help_the_file_exists=null)

        skip_this_file:= False;
        Set_definitively_named_outfile(possible_name);
      end if;
    end Set_outfile_interactive;

    procedure Inform_User(
      name: String;
      comp, uncomp: File_size_type
    )
    is
    begin
      if tell_data /= null  then
        tell_data( name, comp, uncomp, method );
      end if;
    end Inform_User;

    the_name    : String(1..1000);
    the_name_len: Natural;
    use Zip, Zip_Streams;

    actual_feedback: Zip.Feedback_proc;

    dummy: p_Stream_Element_Array;
    encrypted, dummy_bool: Boolean;

  begin
    begin
      Set_Index ( zip_file, work_index);
      Zip.Headers.Read_and_check(zip_file, local_header );
    exception
      when Zip.Headers.bad_local_header =>
        raise;
      when others =>
        raise Read_Error;
    end;

    method:= Zip.Method_from_code(local_header.zip_type);
    if method = unknown then
      Ada.Exceptions.Raise_Exception
        (Unsupported_method'Identity,
         "Format (method) #" & Unsigned_16'Image(local_header.zip_type) &
         " is unknown");
    end if;

    -- calculate offset of data

    work_index :=
       work_index + Positive (Ada.Streams.Stream_IO.Count(
              local_header.filename_length    +
              local_header.extra_field_length +
              Zip.Headers.local_header_length)
       );

    data_descriptor_after_data:= (local_header.bit_flag and 8) /= 0;

    if data_descriptor_after_data then
      -- Sizes and CRC are stored after the data
      -- We set size to avoid getting a sudden Zip_EOF !
      local_header.dd.crc_32            := hint_crc_32;
      local_header.dd.compressed_size   := hint_comp_size;
      local_header.dd.uncompressed_size := File_size_type'Last;
      actual_feedback := null; -- no feedback possible: unknown sizes
    else
      -- Sizes and CRC are stored before the data, inside the local header
      actual_feedback:= feedback; -- use the given feedback procedure
    end if;

    encrypted:= (local_header.bit_flag and 1) /= 0;

    -- 13-Dec-2002
    true_packed_size:= File_size_type(local_header.dd.compressed_size);
    if encrypted then
      true_packed_size:= true_packed_size - 12;
    end if;

    if name_from_header then -- Name from local header is used as output name
      the_name_len:= Natural(local_header.filename_length);
      String'Read(zip_file'Access, the_name(1..the_name_len));
      if not data_descriptor_after_data then
        Inform_User(
          the_name(1..the_name_len),
          true_packed_size,
          File_size_type(local_header.dd.uncompressed_size)
        );
      end if;
      if the_name_len = 0 or else
        (the_name( the_name_len ) = '/' or
         the_name( the_name_len ) = '\' )
      then
        -- This is a directory name (12-feb-2000)
        skip_this_file:= True;
      elsif actual_mode in Write_to_file then
        Set_outfile_interactive(
          the_name(1..the_name_len),
          boolean_to_encoding((local_header.bit_flag and
           Zip.Headers.Language_Encoding_Flag_Bit) /= 0)
        );
      else -- only informational, no need for interaction
        Set_outfile(the_name(1..the_name_len),
          boolean_to_encoding((local_header.bit_flag and
           Zip.Headers.Language_Encoding_Flag_Bit) /= 0)
        );
      end if;
    else -- Output name is given: out_name
      if not data_descriptor_after_data then
        Inform_User(
          out_name,
          true_packed_size,
          File_size_type(local_header.dd.uncompressed_size)
        );
      end if;
      if out_name'Length = 0 or else
        (out_name( out_name'Last ) = '/' or
         out_name( out_name'Last ) = '\' )
      then
        -- This is a directory name, so do not write anything (30-Jan-2012).
        skip_this_file:= True;
      elsif actual_mode in Write_to_file then
        Set_outfile_interactive(out_name, out_name_encoding);
      else -- only informational, no need for interaction
        Set_outfile(out_name, out_name_encoding);
      end if;
    end if;

    if skip_this_file then
      actual_mode := just_test;
    end if;

    if skip_this_file and not data_descriptor_after_data then
      -- We can skip actually since sizes are known.
      if feedback /= null then
        feedback(
          percents_done => 0,
          entry_skipped => True,
          user_abort    => dummy_bool
        );
      end if;
    else
      begin
        Set_Index ( zip_file, work_index ); -- eventually skips the file name
      exception
        when others => raise Read_Error;
      end;
      UnZip.Decompress.Decompress_data(
        zip_file                   => zip_file,
        format                     => method,
        mode                       => actual_mode,
        output_file_name           => To_String(the_output_name),
        output_memory_access       => dummy,
        feedback                   => actual_feedback,
        explode_literal_tree       => (local_header.bit_flag and 4) /= 0,
        explode_slide_8KB          => (local_header.bit_flag and 2) /= 0,
        data_descriptor_after_data => data_descriptor_after_data,
        encrypted                  => encrypted,
        password                   => password,
        get_new_password           => get_pwd,
        hint                       => local_header
      );

      if actual_mode /= just_test then
        begin
          if file_system_routines.Set_Time_Stamp /= null then
            file_system_routines.Set_Time_Stamp(
              To_String(the_output_name),
              Convert(local_header.file_timedate)
            );
          elsif file_system_routines.Set_ZTime_Stamp /= null then
            file_system_routines.Set_ZTime_Stamp(
              To_String(the_output_name),
              local_header.file_timedate
            );
          end if;
        exception
          when Zip_Streams.Calendar.Time_Error =>
            null; -- invalid time, we give up setting the time stamp
        end;
      end if;

      if data_descriptor_after_data then -- Sizes and CRC at the end
        -- Inform after decompression
        Inform_User(
          To_String(the_output_name),
          local_header.dd.compressed_size,
          local_header.dd.uncompressed_size
        );
      end if;

    end if; -- not ( skip_this_file and not data_descriptor )

    -- Set the offset on the next zipped file
    header_index:= header_index + Positive (
      Ada.Streams.Stream_IO.Count(
        File_size_type(
              local_header.filename_length    +
              local_header.extra_field_length +
              Zip.Headers.local_header_length
        ) +
        local_header.dd.compressed_size
      ));

    if data_descriptor_after_data then
      header_index:=
        header_index + Positive (
        Ada.Streams.Stream_IO.Count(Zip.Headers.data_descriptor_length));
    end if;

  end UnZipFile;

  ----------------------------------
  -- Simple extraction procedures --
  ----------------------------------

  -- Extract all files from an archive (from)

  procedure Extract( from                 : String;
                     options              : Option_set:= no_option;
                     password             : String:= "";
                     file_system_routines : FS_routines_type:= null_routines
                ) is
  begin
    Extract( from, null, null, null, null,
             options, password, file_system_routines );
  end Extract;

  procedure Extract( from                 : String;
                     what                 : String;
                     options              : Option_set:= no_option;
                     password             : String:= "";
                     file_system_routines : FS_routines_type:= null_routines
                ) is
  begin
    Extract( from, what, null, null, null, null,
             options, password, file_system_routines );
  end Extract;

  procedure Extract( from                 : String;
                     what                 : String;
                     rename               : String;
                     options              : Option_set:= no_option;
                     password             : String:= "";
                     file_system_routines : FS_routines_type:= null_routines
                ) is
  begin
    Extract( from, what, rename, null, null, null,
             options, password, file_system_routines );
  end Extract;

  procedure Extract( from                 : Zip.Zip_info;
                     options              : Option_set:= no_option;
                     password             : String:= "";
                     file_system_routines : FS_routines_type:= null_routines
                ) is
  begin
    Extract( from, null, null, null, null,
             options, password, file_system_routines );
  end Extract;

  procedure Extract( from                 : Zip.Zip_info;
                     what                 : String;
                     options              : Option_set:= no_option;
                     password             : String:= "";
                     file_system_routines : FS_routines_type:= null_routines
                ) is
  begin
    Extract( from, what, null, null, null, null,
             options, password, file_system_routines );
  end Extract;

  procedure Extract( from                 : Zip.Zip_info;
                     what                 : String;
                     rename               : String;
                     options              : Option_set:= no_option;
                     password             : String:= "";
                     file_system_routines : FS_routines_type:= null_routines
                ) is
  begin
    Extract( from, what, rename, null, null, null,
             options, password, file_system_routines );
  end Extract;

  -- All previous extract call the following ones, with bogus UI arguments

  ------------------------------------------------------------
  -- All previous extraction procedures, for user interface --
  ------------------------------------------------------------

  use Ada.Streams.Stream_IO;

  -- Extract one precise file (what) from an archive (from)

  procedure Extract( from                 : String;
                     what                 : String;
                     feedback             : Zip.Feedback_proc;
                     help_the_file_exists : Resolve_conflict_proc;
                     tell_data            : Tell_data_proc;
                     get_pwd              : Get_password_proc;
                     options              : Option_set:= no_option;
                     password             : String:= "";
                     file_system_routines : FS_routines_type:= null_routines
                )
   is
    use Zip, Zip_Streams;
    zip_file     : File_Zipstream;
                   -- was Unbounded_Stream & file->buffer copy in v.26
    header_index : Positive;
    comp_size    : File_size_type;
    uncomp_size  : File_size_type;
    crc_32       : Unsigned_32;
    work_password: Unbounded_String:= To_Unbounded_String(password);
  begin
    if feedback = null then
      current_user_attitude:= yes_to_all; -- non-interactive
    end if;
    Set_Name (zip_file, from);
    Open (zip_file, In_File);
    Zip.Find_offset(
      file           => zip_file,
      name           => what,
      case_sensitive => options( case_sensitive_match ),
      file_index     => header_index,
      comp_size      => comp_size,
      uncomp_size    => uncomp_size,
      crc_32         => crc_32
    );
    UnZipFile(
      zip_file             => zip_file,
      out_name             => what,
      out_name_encoding    => IBM_437, -- assumption...
      name_from_header     => False,
      header_index         => header_index,
      hint_comp_size       => comp_size,
      hint_crc_32          => crc_32,
      feedback             => feedback,
      help_the_file_exists => help_the_file_exists,
      tell_data            => tell_data,
      get_pwd              => get_pwd,
      options              => options,
      password             => work_password,
      file_system_routines => file_system_routines
    );
    Close(zip_file);
  end Extract;

  -- Extract one precise file (what) from an archive (from),
  -- but save under a new name (rename)

  procedure Extract( from                 : String;
                     what                 : String;
                     rename               : String;
                     feedback             : Zip.Feedback_proc;
                     tell_data            : Tell_data_proc;
                     get_pwd              : Get_password_proc;
                     options              : Option_set:= no_option;
                     password             : String:= "";
                     file_system_routines : FS_routines_type:= null_routines
                )
  is
    use Zip, Zip_Streams;
    zip_file     : aliased File_Zipstream;
                   -- was Unbounded_Stream & file->buffer copy in v.26
    header_index : Positive;
    comp_size    : File_size_type;
    uncomp_size  : File_size_type;
    crc_32       : Unsigned_32;
    work_password: Unbounded_String:= To_Unbounded_String(password);
  begin
    if feedback = null then
      current_user_attitude:= yes_to_all; -- non-interactive
    end if;
    Set_Name (zip_file, from);
    Open (zip_file, In_File);
    Zip.Find_offset(
      file           => zip_file,
      name           => what,
      case_sensitive => options( case_sensitive_match ),
      file_index     => header_index,
      comp_size      => comp_size,
      uncomp_size    => uncomp_size,
      crc_32         => crc_32
    );
    UnZipFile(
      zip_file             => zip_file,
      out_name             => rename,
      out_name_encoding    => IBM_437, -- assumption...
      name_from_header     => False,
      header_index         => header_index,
      hint_comp_size       => comp_size,
      hint_crc_32          => crc_32,
      feedback             => feedback,
      help_the_file_exists => null,
      tell_data            => tell_data,
      get_pwd              => get_pwd,
      options              => options,
      password             => work_password,
      file_system_routines => file_system_routines
    );
    Close(zip_file);
  end Extract;

  -- Extract all files from an archive (from)

  procedure Extract( from                 : String;
                     feedback             : Zip.Feedback_proc;
                     help_the_file_exists : Resolve_conflict_proc;
                     tell_data            : Tell_data_proc;
                     get_pwd              : Get_password_proc;
                     options              : Option_set:= no_option;
                     password             : String:= "";
                     file_system_routines : FS_routines_type:= null_routines
                )
  is
    use Zip, Zip_Streams;
    zip_file     : File_Zipstream;
                   -- was Unbounded_Stream & file->buffer copy in v.26
    header_index : Positive;
    work_password: Unbounded_String:= To_Unbounded_String(password);
  begin
    if feedback = null then
      current_user_attitude:= yes_to_all; -- non-interactive
    end if;
    Set_Name (zip_file, from);
    Open (zip_file, In_File);
    Zip.Find_first_offset(zip_file, header_index); -- >= 13-May-2001
    -- We simply unzip everything sequentially, until the end:
    all_files: loop
      UnZipFile(
        zip_file             => zip_file,
        out_name             => "",
        out_name_encoding    => IBM_437, -- ignored
        name_from_header     => True,
        header_index         => header_index,
        hint_comp_size       => File_size_type'Last,
        --                      ^ no better hint available if comp_size is 0 in local header
        hint_crc_32          => 0, -- 2.0 decryption can fail if data descriptor after data
        feedback             => feedback,
        help_the_file_exists => help_the_file_exists,
        tell_data            => tell_data,
        get_pwd              => get_pwd,
        options              => options,
        password             => work_password,
        file_system_routines => file_system_routines
      );
    end loop all_files;
  exception
    when Zip.Headers.bad_local_header =>
      Close(zip_file); -- normal case: end was hit
    when Zip.Zip_file_open_Error =>
      raise;    -- couldn't open zip file
    when others =>
      Close(zip_file);
      raise;    -- something else wrong
  end Extract;

  -- Extract all files from an archive (from)
  -- Needs Zip.Load(from, ...) prior to the extraction

  procedure Extract( from                 : Zip.Zip_info;
                     feedback             : Zip.Feedback_proc;
                     help_the_file_exists : Resolve_conflict_proc;
                     tell_data            : Tell_data_proc;
                     get_pwd              : Get_password_proc;
                     options              : Option_set:= no_option;
                     password             : String:= "";
                     file_system_routines : FS_routines_type:= null_routines
                )
  is
    procedure Extract_1_file( name: String ) is
    begin
      Extract(from => from,
              what => name,
              feedback => feedback,
              help_the_file_exists => help_the_file_exists,
              tell_data => tell_data,
              get_pwd => get_pwd,
              options => options,
              password => password,
              file_system_routines => file_system_routines
      );
    end Extract_1_file;
    --
    procedure Extract_all_files is new Zip.Traverse( Extract_1_file );
    --
  begin
    Extract_all_files(from);
  end Extract;

  -- Extract one precise file (what) from an archive (from)
  -- Needs Zip.Load(from, ...) prior to the extraction

  procedure Extract( from                 : Zip.Zip_info;
                     what                 : String;
                     feedback             : Zip.Feedback_proc;
                     help_the_file_exists : Resolve_conflict_proc;
                     tell_data            : Tell_data_proc;
                     get_pwd              : Get_password_proc;
                     options              : Option_set:= no_option;
                     password             : String:= "";
                     file_system_routines : FS_routines_type:= null_routines
                ) is

    header_index : Positive;
    comp_size    : File_size_type;
    uncomp_size  : File_size_type;
    crc_32       : Unsigned_32;
    work_password: Unbounded_String:= To_Unbounded_String(password);
    use Zip, Zip_Streams;
    zip_file     : aliased File_Zipstream;
    input_stream : Zipstream_Class_Access;
    use_a_file   : constant Boolean:= Zip.Zip_stream(from) = null;
    name_encoding: Zip.Zip_name_encoding;
  begin
    if use_a_file then
      input_stream:= zip_file'Unchecked_Access;
      Set_Name (zip_file , Zip.Zip_name(from));
      Open (zip_file, Ada.Streams.Stream_IO.In_File);
    else -- use the given stream
      input_stream:= Zip.Zip_stream(from);
    end if;
    if feedback = null then
      current_user_attitude:= yes_to_all; -- non-interactive
    end if;
    Zip.Find_offset(
      info          => from,
      name          => what,
      name_encoding => name_encoding,
      file_index    => Ada.Streams.Stream_IO.Positive_Count(header_index),
      comp_size     => comp_size,
      uncomp_size   => uncomp_size,
      crc_32        => crc_32
    );
    UnZipFile(
      zip_file              => input_stream.all,
      out_name              => what,
      out_name_encoding     => name_encoding,
      name_from_header      => False,
      header_index          => header_index,
      hint_comp_size        => comp_size,
      hint_crc_32           => crc_32,
      feedback              => feedback,
      help_the_file_exists  => help_the_file_exists,
      tell_data             => tell_data,
      get_pwd               => get_pwd,
      options               => options,
      password              => work_password,
      file_system_routines  => file_system_routines
    );
    if use_a_file then
      Close (zip_file);
    end if;
  exception
    when others =>
      if use_a_file and then Is_Open(zip_file) then
        Close (zip_file);
      end if;
      raise;
  end Extract;

  -- Extract one precise file (what) from an archive (from)
  -- but save under a new name (rename)
  -- Needs Zip.Load(from, ...) prior to the extraction

  procedure Extract( from                 : Zip.Zip_info;
                     what                 : String;
                     rename               : String;
                     feedback             : Zip.Feedback_proc;
                     tell_data            : Tell_data_proc;
                     get_pwd              : Get_password_proc;
                     options              : Option_set:= no_option;
                     password             : String:= "";
                     file_system_routines : FS_routines_type:= null_routines
                ) is

    header_index : Positive;
    comp_size    : File_size_type;
    uncomp_size  : File_size_type;
    crc_32       : Unsigned_32;
    work_password: Unbounded_String:= To_Unbounded_String(password);
    use Zip, Zip_Streams;
    zip_file     : aliased File_Zipstream;
    input_stream : Zipstream_Class_Access;
    use_a_file   : constant Boolean:= Zip.Zip_stream(from) = null;
    name_encoding: Zip.Zip_name_encoding;
  begin
    if use_a_file then
      input_stream:= zip_file'Unchecked_Access;
      Set_Name (zip_file , Zip.Zip_name(from));
      Open (zip_file, Ada.Streams.Stream_IO.In_File);
    else -- use the given stream
      input_stream:= Zip.Zip_stream(from);
    end if;
    if feedback = null then
      current_user_attitude:= yes_to_all; -- non-interactive
    end if;
    Zip.Find_offset(
      info          => from,
      name          => what,
      name_encoding => name_encoding,
      file_index    => Ada.Streams.Stream_IO.Positive_Count(header_index),
      comp_size     => comp_size,
      uncomp_size   => uncomp_size,
      crc_32        => crc_32
    );
    UnZipFile(
      zip_file             => input_stream.all,
      out_name             => rename,
      out_name_encoding    => name_encoding, -- assumption: encoding same as name
      name_from_header     => False,
      header_index         => header_index,
      hint_comp_size       => comp_size,
      hint_crc_32          => crc_32,
      feedback             => feedback,
      help_the_file_exists => null,
      tell_data            => tell_data,
      get_pwd              => get_pwd,
      options              => options,
      password             => work_password,
      file_system_routines => file_system_routines
    );
    if use_a_file then
      Close (zip_file);
    end if;
  exception
    when others =>
      if use_a_file and then Is_Open(zip_file) then
        Close (zip_file);
      end if;
      raise;
  end Extract;

end UnZip;
