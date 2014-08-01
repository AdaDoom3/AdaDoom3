--  ________  ___   ______       ______     ___
-- /___..._/  |.|   |.___.\     /. __ .\  __|.|   ____
--    /../    |.|   |.____/     |.|__|.| /....|  __\..\
--  _/../___  |.|   |.|    ===  |..__..||. = .| | = ..|
-- /_______/  |_|  /__|        /__|  |_| \__\_|  \__\_|

-- UnZip.Streams
----------------
-- Extracts, as a stream, a file which is has been compressed into a Zip archive.
-- The Zip archive itself (the input) can be a file or a more general stream.
-- This package is resembling Ada.Streams.Stream_IO, to facilitate transition.

with Zip, Zip_Streams;

with Ada.Streams.Stream_IO, Ada.IO_Exceptions;

package UnZip.Streams is

   subtype Stream_Access is Ada.Streams.Stream_IO.Stream_Access;

   type Zipped_File_Type is private;

   -- Opens an input stream for the compressed file named Name stored
   -- in the archive file named Archive_Name. The function Stream(..)
   -- then gives access to the opened stream.

   -- Version: Zip as a file
   procedure Open
     (File           : in out Zipped_File_Type; -- File-in-archive handle
      Archive_Name   : in String;               -- Name of archive file
      Name           : in String;               -- Name of zipped entry
      Password       : in String := "";         -- Decryption password
      Case_sensitive : in Boolean:= False
     );

   -- Version: Zip as a stream
   procedure Open
     (File           : in out Zipped_File_Type; -- File-in-archive handle
      Archive_Stream : in out Zip_Streams.Root_Zipstream_Type'Class; -- Archive's stream
      Name           : in String;               -- Name of zipped entry
      Password       : in String := "";         -- Decryption password
      Case_sensitive : in Boolean:= False
     );

   -- Same as above, but uses a the pre-loaded contents of the archive's
   -- Central Directory; hence Archive_Info is passed instead of
   -- Archive_Name or Archive_Stream.
   -- You need to call Zip.Load( Archive_Info... ) prior to opening the
   -- compressed file.

   procedure Open
     (File           : in out Zipped_File_Type; -- File-in-archive handle
      Archive_Info   : in Zip.Zip_info;         -- Archive's Zip_info
      Name           : in String;               -- Name of zipped entry
      Password       : in String := ""          -- Decryption password
     );

   procedure Close (File : in out Zipped_File_Type);

   function Is_Open     (File : in Zipped_File_Type) return Boolean;
   function End_Of_File (File : in Zipped_File_Type) return Boolean;

   function Stream (File : Zipped_File_Type) return Stream_Access;

   Use_Error    : exception renames Ada.IO_Exceptions.Use_Error;
   End_Error    : exception renames Ada.IO_Exceptions.End_Error;

private

   type UZS_state is (
      uninitialized,
      data_uncompressed, -- In that model, all data is unzipped in one
                         --   time, into memory. If you have a smarter
                         --   idea (small buffer with tasking, write me!)
      end_of_zip         -- We have reached the end, not yet closed
     );

   type p_String is access String;

   type UnZip_Stream_Type is new Ada.Streams.Root_Stream_Type with record
      state        : UZS_state:= uninitialized;
      archive_info : Zip.Zip_info; -- archive info (.zip file, directory)
      delete_info_on_closing : Boolean;
      file_name    : p_String; -- name of zipped file to unzip from archive
      uncompressed : p_Stream_Element_Array; -- whole uncompressed data
      index        : Ada.Streams.Stream_Element_Offset;
   end record;


   procedure Read
     (Stream : in out UnZip_Stream_Type;
      Item   :    out Ada.Streams.Stream_Element_Array;
      Last   :    out Ada.Streams.Stream_Element_Offset);

   procedure Write
     (Stream : in out UnZip_Stream_Type;
      Item   : in     Ada.Streams.Stream_Element_Array);

   type Zipped_File_Type is access UnZip_Stream_Type;

end UnZip.Streams;
