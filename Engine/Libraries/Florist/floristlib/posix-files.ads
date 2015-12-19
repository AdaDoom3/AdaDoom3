------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                           P O S I X . F I L E S                          --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                                                                          --
--  This  file is a component  of FLORIST,  an implementation of the POSIX  --
--  Ada  bindings  for  use with the GNAT Ada compiler and the FSU Gnu Ada  --
--  Runtime Library (GNARL).                                                --
--                                                                          --
--  This package specification contains some text extracted from  IEEE STD  --
--  1003.5: 1990, Information Technology -- POSIX Ada Language  Interfaces  --
--  Part 1: Binding  for  System Application Program Interface, as amended  --
--  by IEEE STD 1003.5b: 1996, Amendment 1: Realtime Extensions, copyright  --
--  1996 by the Institute of Electrical and Electronics Engineers, Inc.     --
--                                                                          --
--  The package specifications in the IEEE standards cited above represent  --
--  only a  portion  of  the  documents  and  are  not to be interpreteted  --
--  outside the context  of  the documents.  The standards must be used in  --
--  conjunction  with  the  package   specifications  in  order  to  claim  --
--  conformance.   The IEEE takes no responsibility for and will assume no  --
--  liability for damages resulting from the reader's misinterpretation of  --
--  said  information resulting from its out-of-context nature.   To order  --
--  copies of the IEEE standards,  please contact the  IEEE Service Center  --
--  at 445 Hoes Lane, PO Box 1331, Piscataway, NJ 08855-1331; via phone at  --
--  1-800-678-IEEE, 908-981-1393; or via fax at 908-981-9667.               --
--                                                                          --
--  These  package  specifications are  distributed in  the hope that they  --
--  will  be useful, but  WITHOUT  ANY  WARRANTY; without even the implied  --
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.        --
--                                                                          --
------------------------------------------------------------------------------

with POSIX,
     POSIX.C,
     POSIX.Permissions,
     POSIX.Process_Identification,
     POSIX.Calendar;
package POSIX.Files is

   --  Operations to Create Files in the File System

   procedure Create_Directory
     (Pathname   : POSIX.Pathname;
      Permission : POSIX.Permissions.Permission_Set);
   procedure Create_FIFO
     (Pathname   : POSIX.Pathname;
      Permission : POSIX.Permissions.Permission_Set);

   --  Operations to remove files from the File System

   procedure Unlink (Pathname : POSIX.Pathname);
   procedure Remove_Directory (Pathname : POSIX.Pathname);

   --  Predicates on files in the File System

   function Is_Block_Special_File (Pathname : POSIX.Pathname)
      return Boolean;
   function Is_Character_Special_File (Pathname : POSIX.Pathname)
      return Boolean;
   function Is_Directory (Pathname : POSIX.Pathname) return Boolean;
   function Is_FIFO (Pathname : POSIX.Pathname) return Boolean;
   --  Is_Symbolic_Link is not in the IEEE standard
   function Is_Symbolic_Link (Pathname : POSIX.Pathname) return Boolean;
   --  .... Change POSIX.5?
   --  Why is this not called Is_Regular_File?  Add renaming decl?
   function Is_File (Pathname : POSIX.Pathname) return Boolean;
   --  Is_Socket is from POSIX.5c [D2]
   function Is_Socket (Pathname : POSIX.Pathname) return Boolean;

   --  Operations to modify File Pathnames

   procedure Link
     (Old_Pathname : POSIX.Pathname;
      New_Pathname : POSIX.Pathname);
   procedure Rename
     (Old_Pathname : POSIX.Pathname;
      New_Pathname : POSIX.Pathname);

   --  Iterating over files within a directory

   type Directory_Entry is limited private;
   function Filename_Of (D_Entry : Directory_Entry) return POSIX.Filename;
   pragma Inline (Filename_Of);
   generic
   with procedure Action
     (D_Entry : Directory_Entry;
      Quit    : in out Boolean);
   procedure For_Every_Directory_Entry
      (Pathname : POSIX.Pathname);

   --  Operations to Update File Status Information

   procedure Change_Owner_And_Group
     (Pathname : POSIX.Pathname;
      Owner    : POSIX.Process_Identification.User_ID;
      Group    : POSIX.Process_Identification.Group_ID);
   procedure Change_Permissions
     (Pathname   : POSIX.Pathname;
      Permission : POSIX.Permissions.Permission_Set);
   procedure Set_File_Times
     (Pathname          : POSIX.Pathname;
      Access_Time       : POSIX.Calendar.POSIX_Time;
      Modification_Time : POSIX.Calendar.POSIX_Time);
   procedure Set_File_Times (Pathname : POSIX.Pathname);

   --  Operations to Determine File Accessibility

   type Access_Mode is (Read_Ok, Write_Ok, Execute_Ok);
   type Access_Mode_Set is array (Access_Mode) of Boolean;
   function Is_Accessible
     (Pathname     : POSIX.Pathname;
      Access_Modes : Access_Mode_Set) return Boolean;
   function Accessibility
     (Pathname     : POSIX.Pathname;
      Access_Modes : Access_Mode_Set) return POSIX.Error_Code;
   function Is_File_Present (Pathname : POSIX.Pathname)
      return Boolean;
   function Existence (Pathname : POSIX.Pathname)
      return POSIX.Error_Code;

private
   type Directory_Entry is new POSIX.C.dirent_ptr;
end POSIX.Files;
