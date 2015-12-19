------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                     P O S I X . F I L E _ S T A T U S                    --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                                                                          --
--             Copyright (C) 1996-1997 Florida State University             --
--                     Copyright (C) 1998-2014, AdaCore                     --
--                                                                          --
--  This file is a component of FLORIST, an  implementation of an  Ada API  --
--  for the POSIX OS services, for use with  the  GNAT  Ada  compiler  and  --
--  the FSU Gnu Ada Runtime Library (GNARL).   The  interface  is intended  --
--  to be close to that specified in  IEEE STD  1003.5: 1990  and IEEE STD  --
--  1003.5b: 1996.                                                          --
--                                                                          --
--  FLORIST is free software;  you can  redistribute  it and/or  modify it  --
--  under terms of the  GNU  General  Public  License as  published by the  --
--  Free Software Foundation;  either version  2, or (at  your option) any  --
--  later version.  FLORIST is distributed  in  the hope  that  it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without  even the implied  warranty  --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR  PURPOSE.  See  the GNU  --
--  General Public License for more details.  You  should have  received a  --
--  copy of the GNU General Public License  distributed  with  GNARL;  see  --
--  file  COPYING.  If not,  write to  the  Free  Software  Foundation, 59  --
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.                   --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
------------------------------------------------------------------------------

with POSIX.Implementation,
     POSIX.Permissions.Implementation,
     Unchecked_Conversion;

package body POSIX.File_Status is

   use POSIX.C;
   use POSIX.Calendar;
   use POSIX.Implementation;
   use POSIX.Permissions.Implementation;

   function To_User_ID is
    new Unchecked_Conversion (uid_t, POSIX.Process_Identification.User_ID);

   function To_Group_ID is
     new Unchecked_Conversion (gid_t, POSIX.Process_Identification.Group_ID);

   pragma Warnings (Off);
   --  Disable warning that the representation of Time values may
   --  change between GNAT versions.

   function Duration_To_POSIX_Time is new Unchecked_Conversion
     (Duration, POSIX_Time);

   pragma Warnings (On);

   function stat (path : char_ptr; buf : stat_ptr) return int;
   pragma Import (C, stat, stat_LINKNAME);

   function lstat (path : char_ptr; buf : stat_ptr) return int;
   pragma Import (C, lstat, lstat_LINKNAME);

   function fstat (fildes : int; buf : stat_ptr) return int;
   pragma Import (C, fstat, fstat_LINKNAME);

   --  We had to redefine these functions in posix-macro.c.
   --  See the file for more info.

   -----------------------
   --  Get_File_Status  --
   -----------------------

   function Get_File_Status (Pathname : POSIX.Pathname)
      return Status is
      S : aliased struct_stat;
      Pathname_With_NUL : POSIX_String := Pathname & NUL;
   begin
      Check (stat
        (Pathname_With_NUL (Pathname_With_NUL'First)'Unchecked_Access,
        S'Unchecked_Access));
      return Status (S);
   end Get_File_Status;

   -----------------------
   --  Get_File_Status  --
   -----------------------

   function Get_File_Status (File : POSIX.IO.File_Descriptor)
      return Status is
      S : aliased struct_stat;
   begin
      Check (fstat (int (File), S'Unchecked_Access));
      return Status (S);
   end Get_File_Status;

   -----------------------
   --  Get_Link_Status  --
   -----------------------

   function Get_Link_Status (Pathname : POSIX.Pathname)
      return Status is
      S : aliased struct_stat;
      Pathname_With_NUL : POSIX_String := Pathname & NUL;
   begin
      Check (lstat
        (Pathname_With_NUL (Pathname_With_NUL'First)'Unchecked_Access,
        S'Unchecked_Access));
      return Status (S);
   end Get_Link_Status;

   -------------------------
   --  Permission_Set_Of  --
   -------------------------

   function Permission_Set_Of (File_Status : Status)
      return POSIX.Permissions.Permission_Set is
   begin
      return Form_Ada_Permission (struct_stat (File_Status).st_mode);
   end Permission_Set_Of;

   ------------------
   --  File_ID_Of  --
   ------------------

   function File_ID_Of (File_Status : Status)
      return File_ID is
   begin
      return  File_ID (struct_stat (File_Status).st_ino);
   end File_ID_Of;

   --------------------
   --  Device_ID_Of  --
   --------------------

   function Device_ID_Of (File_Status : Status)
      return Device_ID is
   begin
      return  Device_ID (struct_stat (File_Status).st_dev);
   end Device_ID_Of;

   ---------------------
   --  Link_Count_Of  --
   ---------------------

   function Link_Count_Of (File_Status : Status)
      return Links is
   begin
      return  Links (struct_stat (File_Status).st_nlink);
   end Link_Count_Of;

   ----------------
   --  Owner_Of  --
   ----------------

   function Owner_Of (File_Status : Status)
      return POSIX.Process_Identification.User_ID is
   begin
      return  To_User_ID (struct_stat (File_Status).st_uid);
   end Owner_Of;

   ----------------
   --  Group_Of  --
   ----------------

   function Group_Of (File_Status : Status)
      return POSIX.Process_Identification.Group_ID is
   begin
      return  To_Group_ID (struct_stat (File_Status).st_gid);
   end Group_Of;

   ---------------
   --  Size_Of  --
   ---------------

   function Size_Of (File_Status : Status) return POSIX.IO_Count is
   begin
      --  We depart from POSIX.5 5.3.2.3 (875) here, since the current
      --  POSIX C standard allows more cases of valid file descriptors
      --  for the st_size field, in particular symbolic links (the pathname
      --  length), shared memory objects, and typed memory objects.

      return IO_Count (struct_stat (File_Status).st_size);
   end Size_Of;

   ---------------------------
   --  Last_Access_Time_Of  --
   ---------------------------

   function Last_Access_Time_Of (File_Status : Status)
      return POSIX_Time is
   begin
      return Duration_To_POSIX_Time (Duration
        (struct_stat (File_Status).st_atime));
   end Last_Access_Time_Of;

   ---------------------------------
   --  Last_Modification_Time_Of  --
   ---------------------------------

   function Last_Modification_Time_Of (File_Status : Status)
      return POSIX_Time is
   begin
      return Duration_To_POSIX_Time
        (Duration (struct_stat (File_Status).st_mtime));
   end Last_Modification_Time_Of;

   ----------------------------------
   --  Last_Status_Change_Time_Of  --
   ----------------------------------

   function Last_Status_Change_Time_Of (File_Status : Status)
      return POSIX_Time is
   begin
      return Duration_To_POSIX_Time
       (Duration (struct_stat (File_Status).st_ctime));
   end Last_Status_Change_Time_Of;

   --------------------
   --  Is_Directory  --
   --------------------

   function s_isdir (mode : mode_t) return int;
   pragma Import (C, s_isdir, "s_isdir");

   function Is_Directory (File_Status : Status)
      return Boolean is
   begin
      return s_isdir (struct_stat (File_Status).st_mode) /= 0;
   end Is_Directory;

   ---------------------------------
   --  Is_Character_Special_File  --
   ---------------------------------

   function s_ischr (mode : mode_t) return int;
   pragma Import (C, s_ischr, "s_ischr");

   function Is_Character_Special_File (File_Status : Status)
      return Boolean is
   begin
      return s_ischr (struct_stat (File_Status).st_mode) /= 0;
   end Is_Character_Special_File;

   -----------------------------
   --  Is_Block_Special_File  --
   -----------------------------

   function s_isblk (mode : mode_t) return int;
   pragma Import (C, s_isblk, "s_isblk");

   function Is_Block_Special_File (File_Status : Status)
      return Boolean is
   begin
      return s_isblk (struct_stat (File_Status).st_mode) /= 0;
   end Is_Block_Special_File;

   ------------------------
   --  Is_Symbolic_Link  --
   ------------------------

   function s_islnk (mode : mode_t) return int;
   pragma Import (C, s_islnk, "s_islnk");

   function Is_Symbolic_Link (File_Status : Status)
      return Boolean is
   begin
      return s_islnk (struct_stat (File_Status).st_mode) /= 0;
   end Is_Symbolic_Link;

   -----------------------
   --  Is_Regular_FIle  --
   -----------------------

   function s_isreg (mode : mode_t) return int;
   pragma Import (C, s_isreg, "s_isreg");

   function Is_Regular_File (File_Status : Status)
      return Boolean is
   begin
      return s_isreg (struct_stat (File_Status).st_mode) /= 0;
   end Is_Regular_File;

   -----------------
   --  Is_Socket  --
   -----------------

   function s_issock (mode : mode_t) return int;
   pragma Import (C, s_issock, "s_issock");

   function Is_Socket (File_Status : Status)
      return Boolean is
   begin
      return s_issock (struct_stat (File_Status).st_mode) /= 0;
   end Is_Socket;

   ---------------
   --  Is_FIFO  --
   ---------------

   function s_isfifo (mode : mode_t) return int;
   pragma Import (C, s_isfifo, "s_isfifo");

   function Is_FIFO (File_Status : Status)
      return Boolean is
   begin
      return s_isfifo (struct_stat (File_Status).st_mode) /= 0;
   end Is_FIFO;

   ------------------------
   --  Is_Shared_Memory  --
   ------------------------

   function s_typeisshm (buf : stat_ptr) return int;
   pragma Import (C, s_typeisshm, "s_typeisshm");

   function Is_Shared_Memory (File_Status : Status)
     return Boolean is
   begin
      return s_typeisshm
        (To_Stat_Ptr (File_Status'Address)) /= 0;
   end Is_Shared_Memory;

   ------------------------
   --  Is_Message_Queue  --
   ------------------------

   function s_typeismq (buf : stat_ptr) return int;
   pragma Import (C, s_typeismq, "s_typeismq");

   function Is_Message_Queue (File_Status : Status)
      return Boolean is
   begin
      return s_typeismq
        (To_Stat_Ptr (File_Status'Address)) /= 0;
   end Is_Message_Queue;

   --------------------
   --  Is_Semaphore  --
   --------------------

   function s_typeissem (buf : stat_ptr) return int;
   pragma Import (C, s_typeissem, "s_typeissem");

   function Is_Semaphore (File_Status : Status)
      return Boolean is
   begin
      return s_typeissem
        (To_Stat_Ptr (File_Status'Address)) /= 0;
   end Is_Semaphore;

end POSIX.File_Status;
