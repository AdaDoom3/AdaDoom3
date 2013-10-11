--  ________  ___   ______       ______     ___
-- /___..._/  |.|   |.___.\     /. __ .\  __|.|   ____
--    /../    |.|   |.____/     |.|__|.| /....|  __\..\
--  _/../___  |.|   |.|    ===  |..__..||. = .| | = ..|
-- /_______/  |_|  /__|        /__|  |_| \__\_|  \__\_|

-- UnZip
--------
-- This library allows to uncompress deflated, enhanced deflated, bzip2-ed,
-- imploded, reduced, shrunk and stored streams from a Zip archive stream.
--
-- Pure Ada 95 code, 100% portable: OS-, CPU- and compiler- independent.

--  Ada translation and substantial rewriting by Gautier de Montmollin
--    On the web: see the Zip.web constant below.
--  based on Pascal version 2.10 by Abimbola A Olowofoyeku,
--    http://www.greatchief.plus.com/
--  itself based on Pascal version by Christian Ghisler,
--  itself based on C code by Info-Zip group (Mark Adler et al.)
--    http://www.info-zip.org/UnZip.html

-- Technical documentation: read appnote.txt

-- Legal licensing note:

--  Copyright (c) 1999..2010 Gautier de Montmollin

--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:

--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.

--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.

-- NB: this is the MIT License, as found 12-Sep-2007 on the site
-- http://www.opensource.org/licenses/mit-license.php

with Zip;

with Ada.Calendar, Ada.Streams, Ada.Strings.Unbounded;

package UnZip is

  type option is (
    test_only,            -- test .zip file integrity, no write
    junk_directories,     -- ignore directory info -> extract to current one
    case_sensitive_match, -- case sensitive name matching
    extract_as_text       -- files will be written with native line endings
  );

  type Option_set is array( option ) of Boolean;

  no_option: constant Option_set:= ( others=> False );

  -- Ada 2005's Ada.Directories.Create_Path.
  -- For Ada 95 compatibility we pass it as an optional procedure access.
  type Create_Path_proc is access
    procedure (New_Directory : in String;
               Form          : in String := "");

  -- This is system-dependent (or in a future Ada)
  type Set_Time_Stamp_proc is access
    procedure (file_name: String; stamp: Ada.Calendar.Time);

  -- Alternatively, you can use Zip.Time to set file time stamps
  type Set_ZTime_Stamp_proc is access
    procedure (file_name: String; stamp: Zip.Time);
  -- NB: you can use Zip.Convert to change Ada.Calendar.Time from/to Zip.Time
  --     or use our Split to avoid using Ada.Calendar at all.

  -- This is for modifying output file names (e.g. adding a
  -- work directory, modifying the archived path, etc.)
  type Compose_func is access function (
    File_Name     : String;
    Name_encoding : Zip.Zip_name_encoding
  )
  return String;

  -- File System dependent settings
  type FS_routines_type is record
    Create_Path            : Create_Path_proc;
    Set_Time_Stamp         : Set_Time_Stamp_proc;
    Compose_File_Name      : Compose_func;
    Set_ZTime_Stamp        : Set_ZTime_Stamp_proc; -- alt. to Set_Time_Stamp
  end record;

  null_routines: constant FS_routines_type:= (null,null,null,null);


  ----------------------------------
  -- Simple extraction procedures --
  ----------------------------------

  -- Extract all files from an archive (from)

  procedure Extract( from                 : String;
                     options              : Option_set:= no_option;
                     password             : String:= "";
                     file_system_routines : FS_routines_type:= null_routines
                   );

  -- Extract one precise file (what) from an archive (from)

  procedure Extract( from                 : String;
                     what                 : String;
                     options              : Option_set:= no_option;
                     password             : String:= "";
                     file_system_routines : FS_routines_type:= null_routines
                   );

  -- Extract one precise file (what) from an archive (from),
  -- but save under a new name (rename)

  procedure Extract( from                 : String;
                     what                 : String;
                     rename               : String;
                     options              : Option_set:= no_option;
                     password             : String:= "";
                     file_system_routines : FS_routines_type:= null_routines
                   );

  -------------------------------------------------------------------------
  -- Simple extraction procedures without re-searching central directory --
  -------------------------------------------------------------------------

  -- Extract all files from an archive (from)
  -- Needs Zip.Load(from, ...) prior to the extraction

  procedure Extract( from                 : Zip.Zip_info;
                     options              : Option_set:= no_option;
                     password             : String:= "";
                     file_system_routines : FS_routines_type:= null_routines
                   );

  -- Extract one precise file (what) from an archive (from)
  -- Needs Zip.Load(from, ...) prior to the extraction

  procedure Extract( from                 : Zip.Zip_info;
                     what                 : String;
                     options              : Option_set:= no_option;
                     password             : String:= "";
                     file_system_routines : FS_routines_type:= null_routines
                   );

  -- Extract one precise file (what) from an archive (from),
  -- but save under a new name (rename)
  -- Needs Zip.Load(from, ...) prior to the extraction

  procedure Extract( from                 : Zip.Zip_info;
                     what                 : String;
                     rename               : String;
                     options              : Option_set:= no_option;
                     password             : String:= "";
                     file_system_routines : FS_routines_type:= null_routines
                   );

  subtype PKZip_method is Zip.PKZip_method;

  ----------------------------------------------
  -- Extraction procedures for user interface --
  ----------------------------------------------

  -- NB: the *_proc types are accesses to procedures - their usage
  -- may require the non-standard attribute "unrestricted_access",
  -- or some changes.
  -- Read unzipada.adb for details and examples.

  type Name_conflict_intervention is
    ( yes, no, yes_to_all, none, rename_it, abort_now );

  current_user_attitude : Name_conflict_intervention:= yes;
  -- reset to "yes" for a new session (in case of yes_to_all / none state!)

  type Resolve_conflict_proc is access
    procedure ( name            :  in String;
                name_encoding   :  in Zip.Zip_name_encoding;
                action          : out Name_conflict_intervention;
                new_name        : out String;
                new_name_length : out Natural );


  type Get_password_proc is access
    procedure(password: out Ada.Strings.Unbounded.Unbounded_String);

  -- Data sizes in archive
  subtype File_size_type is Zip.File_size_type;

  -- Inform user about some archive data

  type Tell_data_proc is access
    procedure ( name               : String;
                compressed_bytes   : File_size_type;
                uncompressed_bytes : File_size_type;
                method             : PKZip_method );

  -- Extract all files from an archive (from)

  procedure Extract( from                 : String;
                     feedback             : Zip.Feedback_proc;
                     help_the_file_exists : Resolve_conflict_proc;
                     tell_data            : Tell_data_proc;
                     get_pwd              : Get_password_proc;
                     options              : Option_set:= no_option;
                     password             : String:= "";
                     file_system_routines : FS_routines_type:= null_routines
                   );

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
                   );

  -- Extract one precise file (what) from an archive (from),
  -- but save under a new name (rename)

  procedure Extract( from        : String;
                     what        : String;
                     rename      : String;
                     feedback    : Zip.Feedback_proc;
                     tell_data   : Tell_data_proc;
                     get_pwd     : Get_password_proc;
                     options     : Option_set:= no_option;
                     password    : String:= "";
                     file_system_routines : FS_routines_type:= null_routines
                   );

  -- Using Zip_info structure:

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
                   );

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
                   );

  -- Extract one precise file (what) from an archive (from),
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
                   );

  -- Errors

  CRC_Error,
  Uncompressed_size_Error,
  Write_Error,
  Read_Error,
  Wrong_password,
  User_abort,
  Not_supported,
  Unsupported_method,
  Internal_Error : exception;

  tolerance_wrong_password: constant:= 4;
  -- If password is wrong at the Nth attempt, Wrong_password is raised

private

  type Write_mode is
    ( write_to_binary_file,
      write_to_text_file,
      write_to_memory,
      just_test
    );

  subtype Write_to_file is Write_mode
    range write_to_binary_file..write_to_text_file;

  type p_Stream_Element_Array is access all Ada.Streams.Stream_Element_Array;

end UnZip;
