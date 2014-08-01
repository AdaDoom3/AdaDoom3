-- UnZip.Decompress
-------------------
-- Private, internal to the UnZip package.
--
-- Created 9-Mar-2007
--
-- This package includes the decompression algorithms for methods
-- store, reduce, shrink (LZW), implode and inflate.
-- It contains the packages UnZ_IO, UnZ_Glob, UnZ_Infl, UnZ_Olds, UnZ_Misc
-- of previous versions of Zip-Ada.
-- They become local packages inside the Decompress_Data procedure ->
-- previously global variables are now local, one copy per concurrent call.

with Zip.Headers;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Zip_Streams;

private package UnZip.Decompress is

   procedure Decompress_data(
    zip_file                   : in out Zip_Streams.Root_Zipstream_Type'Class;
    -- zip_file must be open and its index is meant
    -- to point to the beginning of compressed data
    format                     : PKZip_method;
    mode                       : Write_mode;
    output_file_name           : String; -- relevant only if mode = write_to_file
    output_memory_access       : out p_Stream_Element_Array; --   = write_to_memory
    feedback                   : Zip.Feedback_proc;
    explode_literal_tree       : Boolean; -- relevant for the "explode" format
    explode_slide_8KB          : Boolean; -- relevant for the "explode" format
    data_descriptor_after_data : Boolean;
    encrypted                  : Boolean;
    password                   : in out Unbounded_String;
    get_new_password           : Get_password_proc; -- if null, initial pwd must fit
    hint                       : in out Zip.Headers.Local_File_Header
    -- Values are known, or smart fakes, and are later corrected if a closing
    -- Data_descriptor is appended to the compressed data (1-pass written
    -- zip files, like JAR, OpenDocument, etc.)
  );

private

  -- Primitive tracing using Ada.Text_IO
  --
  type Trace_type is (none, some_t, full);

  trace: constant Trace_type:= none; -- <== Choice

  no_trace  : constant Boolean:= trace = none;
  some_trace: constant Boolean:= trace >= some_t;
  full_trace: constant Boolean:= trace = full;

end UnZip.Decompress;
