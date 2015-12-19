------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                           P O S I X . F I L E S                          --
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
     POSIX.File_Status,
     POSIX.Permissions.Implementation,
     Unchecked_Conversion;
package body POSIX.Files is

   use POSIX,
       POSIX.C,
       POSIX.Implementation,
       POSIX.Permissions.Implementation;

   -------------------------
   --  Local Subprograms  --
   -------------------------
   pragma Warnings (Off);
   --  Disable warning that the representation of Time values may
   --  change between GNAT versions.

   function To_D_Int is
     new Unchecked_Conversion (POSIX.Calendar.POSIX_Time, D_Int);

   function To_time_t (Time : POSIX.Calendar.POSIX_Time) return time_t;

   function To_time_t (Time : POSIX.Calendar.POSIX_Time) return time_t is
   begin
      return time_t (To_Duration (To_D_Int (Time) / NS_per_S) * NS_per_S);
   end To_time_t;

   pragma Warnings (On);

   function c_access
     (path  : char_ptr;
      amode : int) return int;
   pragma Import (C, c_access, access_LINKNAME);

   function Form_C_access
     (Modes : POSIX.Files.Access_Mode_Set) return int;

   function Form_C_access
     (Modes : POSIX.Files.Access_Mode_Set) return int is
      c_access : Bits := 0;
   begin
      if Modes (Read_Ok) then
         c_access := c_access or R_OK;
      end if;
      if Modes (Write_Ok) then
         c_access := c_access or W_OK;
      end if;
      if Modes (Execute_Ok) then
         c_access := c_access or X_OK;
      end if;
      return int (c_access);
   end Form_C_access;

   ------------------------
   --  Create_Directory  --
   ------------------------

   function mkdir
     (path : char_ptr;
      mode : mode_t) return int;
   pragma Import (C, mkdir, mkdir_LINKNAME);

   procedure Create_Directory
     (Pathname   : POSIX.Pathname;
      Permission : POSIX.Permissions.Permission_Set) is
      Pathname_With_NUL : POSIX_String := Pathname & NUL;
   begin
      Check (mkdir (Pathname_With_NUL
        (Pathname_With_NUL'First)'Unchecked_Access,
        (Form_C_Permission (Permission))));
   end Create_Directory;

   -------------------
   --  Create_FIFO  --
   -------------------

   function mkfifo
     (path : char_ptr;
      mode : mode_t) return int;
   pragma Import (C, mkfifo, mkfifo_LINKNAME);

   procedure Create_FIFO
     (Pathname   : POSIX.Pathname;
      Permission : POSIX.Permissions.Permission_Set) is
      Pathname_With_NUL : POSIX_String := Pathname & NUL;
   begin
      Check (mkfifo (Pathname_With_NUL
        (Pathname_With_NUL'First)'Unchecked_Access,
        (Form_C_Permission (Permission))));
   end Create_FIFO;

   --------------
   --  Unlink  --
   --------------

   function unlink (path : char_ptr) return int;
   pragma Import (C, unlink, unlink_LINKNAME);

   procedure Unlink (Pathname : POSIX.Pathname) is
      Pathname_With_NUL : POSIX_String := Pathname & NUL;
   begin
      Check (unlink (Pathname_With_NUL
       (Pathname_With_NUL'First)'Unchecked_Access));
   end Unlink;

   ------------------------
   --  Remove_Directory  --
   ------------------------

   function rmdir (path : char_ptr) return int;
   pragma Import (C, rmdir, rmdir_LINKNAME);

   procedure Remove_Directory (Pathname : POSIX.Pathname) is
      Pathname_With_NUL : POSIX_String := Pathname & NUL;
   begin
      Check (rmdir (Pathname_With_NUL
       (Pathname_With_NUL'First)'Unchecked_Access));
   end Remove_Directory;

   ------------------------
   --  Is_Symbolic_Link  --
   ------------------------

   function Is_Symbolic_Link (Pathname : POSIX.Pathname) return Boolean is
      stat : POSIX.File_Status.Status;
   begin
      stat := POSIX.File_Status.Get_Link_Status (Pathname);
      return (POSIX.File_Status.Is_Symbolic_Link (stat));
   exception
      when POSIX_Error => return False;
   end Is_Symbolic_Link;

   ---------------
   --  Is_File  --
   ---------------

   function Is_File (Pathname : POSIX.Pathname) return Boolean is
      stat : POSIX.File_Status.Status;
   begin
      stat := POSIX.File_Status.Get_File_Status (Pathname);
      return (POSIX.File_Status.Is_Regular_File (stat));
   exception
      when POSIX_Error => return False;
   end Is_File;

   -----------------
   --  Is_Socket  --
   -----------------

   function Is_Socket (Pathname : POSIX.Pathname) return Boolean is
      stat : POSIX.File_Status.Status;
   begin
      stat := POSIX.File_Status.Get_File_Status (Pathname);
      return (POSIX.File_Status.Is_Socket (stat));
   exception
      when POSIX_Error => return False;
   end Is_Socket;

   --------------------
   --  Is_Directory  --
   --------------------

   function Is_Directory (Pathname : POSIX.Pathname) return Boolean is
      stat : POSIX.File_Status.Status;
   begin
      stat := POSIX.File_Status.Get_File_Status (Pathname);
      return (POSIX.File_Status.Is_Directory (stat));
   exception
      when POSIX_Error => return False;
   end Is_Directory;

   ---------------
   --  Is_FIFO  --
   ---------------

   function Is_FIFO (Pathname : POSIX.Pathname) return Boolean is
      stat : POSIX.File_Status.Status;
   begin
      stat := POSIX.File_Status.Get_File_Status (Pathname);
      return (POSIX.File_Status.Is_FIFO (stat));
   exception
      when POSIX_Error => return False;
   end Is_FIFO;

   ---------------------------------
   --  Is_Character_Special_File  --
   ---------------------------------

   function Is_Character_Special_File
     (Pathname : POSIX.Pathname) return Boolean is
      stat : POSIX.File_Status.Status;
   begin
      stat := POSIX.File_Status.Get_File_Status (Pathname);
      return (POSIX.File_Status.Is_Character_Special_File (stat));
   exception
      when POSIX_Error => return False;
   end Is_Character_Special_File;

   -----------------------------
   --  Is_Block_Special_File  --
   -----------------------------

   function Is_Block_Special_File
     (Pathname : POSIX.Pathname) return Boolean is
      stat : POSIX.File_Status.Status;
   begin
      stat := POSIX.File_Status.Get_File_Status (Pathname);
      return (POSIX.File_Status.Is_Block_Special_File (stat));
   exception
      when POSIX_Error => return False;
   end Is_Block_Special_File;

   ------------
   --  Link  --
   ------------

   function link
     (existing : char_ptr;
      new_name : char_ptr) return int;
   pragma Import (C, link, link_LINKNAME);

   procedure Link
     (Old_Pathname : Pathname;
      New_Pathname : Pathname) is
      Old_Pathname_With_NUL : POSIX_String := Old_Pathname & NUL;
      New_Pathname_With_NUL : POSIX_String := New_Pathname & NUL;
   begin
      Check (link (Old_Pathname_With_NUL
         (Old_Pathname_With_NUL'First)'Unchecked_Access,
        New_Pathname_With_NUL (New_Pathname_With_NUL'First)'Unchecked_Access));
   end Link;

   --------------
   --  Rename  --
   --------------

   function rename
     (old_name : char_ptr;
      new_name : char_ptr) return int;
   pragma Import (C, rename, rename_LINKNAME);

   procedure Rename
     (Old_Pathname : Pathname;
      New_Pathname : Pathname) is
      Old_Pathname_With_NUL : POSIX_String := Old_Pathname & NUL;
      New_Pathname_With_NUL : POSIX_String := New_Pathname & NUL;
   begin
      Check (rename (Old_Pathname_With_NUL
         (Old_Pathname_With_NUL'First)'Unchecked_Access,
        New_Pathname_With_NUL (New_Pathname_With_NUL'First)'Unchecked_Access));
   end Rename;

   -------------------
   --  Filename_Of  --
   -------------------

   function Filename_Of (D_Entry : Directory_Entry)
      return Filename is
   begin
      return Form_POSIX_String
        (To_char_ptr (D_Entry.d_name (1)'Address));
   end Filename_Of;

   ---------------------------------
   --  For_Every_Directory_Entry  --
   ---------------------------------

   function opendir (dirname : char_ptr) return DIR_ptr;
   pragma Import (C, opendir, opendir_LINKNAME);

   function readdir (dirp : DIR_ptr) return dirent_ptr;
   pragma Import (C, readdir, readdir_LINKNAME);

   function closedir (dirp : DIR_ptr) return int;
   pragma Import (C, closedir, closedir_LINKNAME);

   --  ?????
   --  The following needs to be made safe for use in a multitasking
   --  environment.

   --  Clearly, readdir is a problem, since it returns a pointer to a
   --  structure that must be allocated somewhere.  Thus, POSIX provides
   --  readdir_r.  We should probably add conditional compilation code to
   --  Florist posix-files.adb to make use of readdir_r if that is
   --  supported.

   --  Note that we are not required to support safe concurrent use of
   --  multiple iterators on the same directory.  A non-normative note
   --  has been placed in 3.3.5 on lines 19-22 to make this clear.  It
   --  says:

   --   The requirement for tasking safety does not imply any greater
   --   degree of safety for concurrent use than is requird of the
   --   standard Ada libraries by the Ada RM.  That is, unless it is so
   --   specified elsewhere in this standard, operations are [missin "not"
   --   here, which is a typo] necessarily atomic and are not necessarily
   --   safe to execute concurrently on the same data object.

   --  Thus, the thing is to cover the case where readdir is the only
   --  thing available, and it is not safe for concurrent use (even on
   --  different directories).

   procedure For_Every_Directory_Entry (Pathname : POSIX.Pathname) is
      Pathname_With_NUL : POSIX_String := Pathname & NUL;
      dirp : DIR_ptr;
      dirent : dirent_ptr;
      Quit : Boolean := False;
      rc : int;
      pragma Unreferenced (rc);
   begin
      dirp := opendir (Pathname_With_NUL
        (Pathname_With_NUL'First)'Unchecked_Access);
      if dirp = null then
         Raise_POSIX_Error;
      end if;
      loop
         dirent := readdir (dirp);
         exit when dirent = null;
         Action (Directory_Entry (dirent), Quit);
         exit when Quit;
      end loop;
      Check (closedir (dirp));
   exception
      when others =>
         --  Ensure dirp is closed if an exception is raised.
         if dirp /= null then
            --  Do not call Check here, as that function may raise
            --  POSIX_Error and obscure an underlying problem raised
            --  in the procedure Action.
            rc := closedir (dirp);
         end if;
         raise;
   end For_Every_Directory_Entry;

   ------------------------------
   --  Change_Owner_And_Group  --
   ------------------------------

   function chown
     (path  : char_ptr;
      owner : uid_t;
      group : gid_t) return int;
   pragma Import (C, chown, chown_LINKNAME);

   function To_uid_t is new Unchecked_Conversion
     (POSIX.Process_Identification.User_ID, uid_t);
   function To_gid_t is new Unchecked_Conversion
     (POSIX.Process_Identification.Group_ID, gid_t);

   procedure Change_Owner_And_Group
     (Pathname : POSIX.Pathname;
      Owner    : POSIX.Process_Identification.User_ID;
      Group    : POSIX.Process_Identification.Group_ID) is
      Pathname_With_NUL : POSIX_String := Pathname & NUL;
   begin
      Check (chown (Pathname_With_NUL
          (Pathname_With_NUL'First)'Unchecked_Access,
         To_uid_t (Owner), To_gid_t (Group)));
   end Change_Owner_And_Group;

   --------------------------
   --  Change_Permissions  --
   --------------------------

   function chmod
     (path : char_ptr;
      mode : mode_t) return int;
   pragma Import (C, chmod, chmod_LINKNAME);

   procedure Change_Permissions
     (Pathname   : POSIX.Pathname;
      Permission : POSIX.Permissions.Permission_Set) is
      Pathname_With_NUL : POSIX_String := Pathname & NUL;
   begin
      Check (chmod (Pathname_With_NUL
          (Pathname_With_NUL'First)'Unchecked_Access,
         Form_C_Permission (Permission)));
   end Change_Permissions;

   ----------------------
   --  Set_File_Times  --
   ----------------------

   --  There is a problem in the difference between POSIX.1c and POSIX.5
   --  definition of file related times. POSIX.1c requires the accuracy be
   --  in seconds while POSIX.5 requires it to be in POSIX_Time.
   --  To avoid inconsistency, we have implemented POSIX_Time so that
   --  all time values are truncated to the nearest second.

   function utime
     (path   : char_ptr;
      actime : utimbuf_ptr) return int;
   pragma Import (C, utime, utime_LINKNAME);

   procedure Set_File_Times
     (Pathname          : POSIX.Pathname;
      Access_Time       : POSIX.Calendar.POSIX_Time;
      Modification_Time : POSIX.Calendar.POSIX_Time) is
      Pathname_With_NUL : POSIX_String := Pathname & NUL;
      Times : aliased struct_utimbuf;
   begin
      Times.actime := To_time_t (Access_Time);
      Times.modtime := To_time_t (Modification_Time);
      Check (utime (Pathname_With_NUL
          (Pathname_With_NUL'First)'Unchecked_Access,
         Times'Unchecked_Access));
   end Set_File_Times;

   ----------------------
   --  Set_File_Times  --
   ----------------------

   procedure Set_File_Times (Pathname : POSIX.Pathname) is
      Pathname_With_NUL : POSIX_String := Pathname & NUL;
   begin
      Check (utime (Pathname_With_NUL
        (Pathname_With_NUL'First)'Unchecked_Access, null));
   end Set_File_Times;

   ---------------------
   --  Is_Accessible  --
   ---------------------

   function Is_Accessible
     (Pathname : POSIX.Pathname;
      Access_Modes : Access_Mode_Set) return Boolean is
   begin
      return Accessibility (Pathname, Access_Modes) = No_Error;
   end Is_Accessible;

   -----------------------
   --  Accessibilitity  --
   -----------------------

   function Accessibility
     (Pathname : POSIX.Pathname;
      Access_Modes : Access_Mode_Set) return Error_Code is
      Pathname_With_NUL : POSIX_String := Pathname & NUL;
   begin
      if c_access
        (Pathname_With_NUL (Pathname_With_NUL'First)'Unchecked_Access,
         Form_C_access (Access_Modes)) = 0
      then
         return No_Error;
      else
         return Fetch_Errno;
      end if;
   end Accessibility;

   -----------------------
   --  Is_File_Present  --
   -----------------------

   function Is_File_Present
     (Pathname : POSIX.Pathname) return Boolean is
      Pathname_With_NUL : POSIX_String := Pathname & NUL;
   begin
      return c_access (Pathname_With_NUL
        (Pathname_With_NUL'First)'Unchecked_Access, 0) = 0;
   end Is_File_Present;

   -----------------
   --  Existence  --
   -----------------

   function Existence
     (Pathname : POSIX.Pathname) return Error_Code is
   begin
      if Is_File_Present (Pathname) then
         return No_Error;
      else
         return Fetch_Errno;
      end if;
   end Existence;

end POSIX.Files;
