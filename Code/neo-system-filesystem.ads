--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
with
  Neo.Foundation.Text_IO,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Package_Testing;
use
  Neo.Foundation.Text_IO,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Package_Testing;
package Neo.System.Filesystem
  is
  ----------------
  -- Exceptions --
  ----------------
    File_Already_Exists      : Exception;
    Directory_Already_Exists : Exception;
    File_Not_Found           : Exception;
    Directory_Not_Found      : Exception;
  ----------------
  -- Suprograms --
  ----------------
    procedure Test;
    function Get_Freespace(
      Path : in String_2)
      return Integer_8_Natural;
    function Get_File_Seporator
      return String_2;
    function Get_File_Information(
      Path : in String_2)
      return Record_Date;
    function Get_Base_Path
      return String_2;
    function Get_Save_Path
      return String_2;
    function Get_Executable_Path
      return String_2;
    function Get_Disk_Paths
      return Array_Access_String_2;
    function Get_Application_Path(
      Application_Name : in String_2)
      return String_2;  
    function Get_File_List(
      Path : in String_2)
      return Array_Access_String_2;
    function Get_Directory_List(
      Path : in String_2)
      return Array_Access_String_2;
    procedure Create_Directory(
      Path : in String_2);
    procedure Create_File(
      Path : in String_2;
      Data : in Array_Integer_1_Unsigned_C);
    function Search_Subdirectories(
      Path : in String_2)
      return Array_Access_String_2;
    function Is_In_Directory(
      Path : in String_2)
      return Boolean;

-- void      Sys_Mkdir( const char *path );
-- bool      Sys_Rmdir( const char *path );
-- bool      Sys_IsFileWritable( const char *path );

-- enum sysFolder_t {
--   FOLDER_ERROR  = -1,
--   FOLDER_NO   = 0,
--   FOLDER_YES    = 1
-- };

-- // returns FOLDER_YES if the specified path is a folder
-- sysFolder_t   Sys_IsFolder( const char *path );

-- // use fs_debug to verbose Sys_ListFiles
-- // returns -1 if directory was not found (the list is cleared)
-- int       Sys_ListFiles( const char * directory, const char * extension, idList<class idStr> & list );

-- const char *  Sys_EXEPath();
-- const char *  Sys_CWD();

-- const char *  Sys_LaunchPath();

-- #endif

-------
private
-------
  --------------------
  -- Implementation --
  --------------------
  end Neo.System.Filesystem;
