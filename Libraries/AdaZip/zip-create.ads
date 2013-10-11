-- Contributed by ITEC - NXP Semiconductors
-- June 2008
--
-- Change log:
-- ==========
--
-- 30-Oct-2012: GdM: Removed all profiles using Zip_Streams' objects
--                      with accesses (cf 25-Oct's modifications)
-- 26-Oct-2012: GdM: Added Add_Compressed_Stream
-- 25-Oct-2012: GdM: Some procedures using Zip_Streams' objects also with
--                     pointer-free profiles (no more 'access' or access type)
-- 14-Oct-2012: GdM: Added Set procedure for changing compression method
-- 30-Mar-2010: GdM: Added Name function
-- 25-Feb-2010: GdM: Fixed major bottlenecks around Dir_entries
--                     -> 5x faster overall for 1000 files, 356x for 100'000 !
-- 17-Feb-2009: GdM: Added procedure Add_String
-- 10-Feb-2009: GdM: Create / Finish: if Info.Stream is to a file,
--                     the underling file is also created / closed in time
--  4-Feb-2009: GdM: Added procedure Add_File
--

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Zip.Headers; use Zip.Headers;
with Zip.Compress; use Zip.Compress;
with Zip_Streams; use Zip_Streams;

package Zip.Create is

   type Zip_Create_info is private;

   -- Create the Zip archive; create the file if the stream is a file

   procedure Create(Info        : out Zip_Create_info;
                    Z_Stream    : in Zipstream_Class_Access;
                    Name        : String;
                    Compress    : Zip.Compress.Compression_Method:= Zip.Compress.Shrink);

   -- Set a new compression format for the next data to be added to the archive.
   -- Can be useful if data are known to be already compressed - or not.

   procedure Set(Info       : out Zip_Create_info;
                 New_Method : Zip.Compress.Compression_Method);

   function Name(Info: Zip_Create_info) return String;

   -- Add a new entry to a Zip archive, from a general Zipstream

   procedure Add_Stream (Info   : in out Zip_Create_info;
                         Stream : in out Root_Zipstream_Type'Class);

   procedure Add_Stream (Info           : in out Zip_Create_info;
                         Stream         : in out Root_Zipstream_Type'Class;
                         Feedback       : in     Feedback_proc;
                         Compressed_Size:    out Zip.File_size_type;
                         Final_Method   :    out Natural);

   -- Add a new entry to a Zip archive, from an entire file

   procedure Add_File (Info              : in out Zip_Create_info;
                       Name              : String;
                       Name_in_archive   : String:= "";
                       -- default: add the file in the archive
                       -- under the same name
                       Delete_file_after : Boolean:= False;
                       -- practical to delete temporary file after adding
                       Name_encoding     : Zip_name_encoding:= IBM_437;
                       Modification_time : Time:= default_time;
                       Is_read_only      : Boolean:= False;
                       Feedback          : Feedback_proc:= null
   );

   -- Add a new entry to a Zip archive, from a buffer stored in a string

   procedure Add_String (Info              : in out Zip_Create_info;
                         Contents          : String;
                         Name_in_archive   : String;
                         Name_UTF_8_encoded: Boolean:= False
                         -- True if Name is actually UTF-8 encoded (Unicode)
   );

   procedure Add_String (Info              : in out Zip_Create_info;
                         Contents          : Unbounded_String;
                         Name_in_archive   : String;
                         Name_UTF_8_encoded: Boolean:= False
                         -- True if Name is actually UTF-8 encoded (Unicode)
   );

   -- Add a new entry to a Zip archive, copied from another Zip archive.
   -- The stream is set at the beginning of a local header in the archive.

   procedure Add_Compressed_Stream (
     Info           : in out Zip_Create_info;
     Stream         : in out Root_Zipstream_Type'Class;
     Feedback       : in     Feedback_proc
   );

   -- Complete the Zip archive; close the file if the stream is a file

   procedure Finish (Info       : in out Zip_Create_info);

private

   type Dir_entry is record
      head : Zip.Headers.Central_File_Header;
      name : p_String;
   end record;

   type Dir_entries is array (Positive range <>) of Dir_entry;
   type Pdir_entries is access Dir_entries;

   type Zip_Create_info is record
      Stream    : Zipstream_Class_Access;
      Compress  : Zip.Compress.Compression_Method;
      Contains  : Pdir_entries:= null;
      Last_entry: Natural:= 0;
      -- 'Contains' has unused room, to avoid reallocating each time
      Creation_time: Time;
   end record;

end Zip.Create;
