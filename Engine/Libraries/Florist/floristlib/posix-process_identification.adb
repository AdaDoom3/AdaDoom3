------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--          P O S I X . P R O C E S S _ I D E N T I F I C A T I O N         --
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

with POSIX.Implementation;

package body POSIX.Process_Identification is

   use POSIX.C,
       POSIX.Implementation;

   ---------------------
   --  Get_Process_ID --
   ---------------------

   function getpid return pid_t;
   pragma Import (C, getpid, getpid_LINKNAME);

   function Get_Process_ID return Process_ID is
   begin
      return Process_ID (getpid);
   end Get_Process_ID;

   -----------------------------
   --  Get_Parent_Process_ID  --
   -----------------------------

   function Get_Parent_Process_ID return Process_ID is
      function getppid return pid_t;
      pragma Import (C, getppid, getppid_LINKNAME);
   begin
      return Process_ID (getppid);
   end Get_Parent_Process_ID;

   -----------
   -- Image --
   -----------

   function Image (ID : Process_ID)
      return Standard.String is
   begin
      return Process_ID'Image (ID);
   end Image;

   -----------
   -- Value --
   -----------

   function Value (Str : Standard.String)
      return Process_ID is
   begin
      return Process_ID'Value (Str);
   end Value;

   --------------------------
   -- Get_Process_Group_ID --
   --------------------------

   --  The getpgrp takes an argument under BSD but not under POSIX.
   --  We pass it an argument in all cases and hope that the function
   --  call mechanism will not be confused by unexpected arguments.

   function getpgrp (ID : Process_ID) return Process_Group_ID;
   pragma Import (C, getpgrp, "getpgrp");

   function Get_Process_Group_ID return Process_Group_ID is
   begin
      return getpgrp (Get_Process_ID);
   end Get_Process_Group_ID;

   --------------------------
   -- Set_Process_Group_ID --
   --------------------------

   function setpgid (pid : pid_t; pgrp : pid_t) return int;
   pragma Import (C, setpgid, setpgid_LINKNAME);

   procedure Set_Process_Group_ID
     (Process : Process_ID := Get_Process_ID;
      Process_Group : Process_Group_ID := Get_Process_Group_ID) is
   begin
      Check (Process /= Null_Process_ID, Invalid_Argument);
      Check (setpgid (pid_t (Process), pid_t (Process_Group)));
   end Set_Process_Group_ID;

   --------------------------
   -- Create_Process_Group --
   --------------------------

   procedure Create_Process_Group
     (Process : Process_ID; Process_Group : out Process_Group_ID) is
      function setpgid (pid : pid_t; pgrp : pid_t) return int;
      pragma Import (C, setpgid, setpgid_LINKNAME);
   begin
      Check (setpgid (pid_t (Process), 0));
      Process_Group := Process_Group_ID (Process);
   end Create_Process_Group;

   ----------------------
   --  Create_Session  --
   ----------------------

   procedure Create_Session
     (Session_Leader : out Process_Group_ID) is
      function setsid return pid_t;
      pragma Import (C, setsid, setsid_LINKNAME);
   begin
      Session_Leader := Process_Group_ID (setsid);
      if Session_Leader = -1 then
         Raise_POSIX_Error;
      end if;
   end Create_Session;

   -----------
   -- Image --
   -----------

   function Image (ID : Process_Group_ID) return Standard.String
   renames Process_Group_ID'Image;

   -----------
   -- Value --
   -----------

   function Value
     (Str : Standard.String) return Process_Group_ID is
   begin
      return Process_Group_ID'Value (Str);
   end Value;

   ----------------------
   -- Get_Real_User_ID --
   ----------------------

   function Get_Real_User_ID return User_ID is
      function getuid return uid_t;
      pragma Import (C, getuid, getuid_LINKNAME);
   begin
      return User_ID (getuid);
   end Get_Real_User_ID;

   ---------------------------
   -- Get_Effective_user_ID --
   ---------------------------

   function Get_Effective_User_ID return User_ID is
      function geteuid return uid_t;
      pragma Import (C, geteuid, geteuid_LINKNAME);
   begin
      return User_ID (geteuid);
   end Get_Effective_User_ID;

   -----------------
   -- Set_User_ID --
   -----------------

   procedure Set_User_ID (ID : User_ID) is
      function setuid (uid : uid_t) return int;
      pragma Import (C, setuid, setuid_LINKNAME);
   begin
      Check (setuid (uid => uid_t (ID)));
   end Set_User_ID;

   --------------------
   -- Get_Login_Name --
   --------------------

--  .... Consider using getlogin_r if that is supported.
--  Use conditional code, based on configurable constant
--  HAVE_getlogin_r.

   function Get_Login_Name return POSIX.POSIX_String is
      function getlogin return char_ptr;
      pragma Import (C, getlogin, getlogin_LINKNAME);
      Name_Ptr : char_ptr;
   begin
      Name_Ptr := getlogin;
      if Name_Ptr = null then
         Raise_POSIX_Error;
      end if;
      return Form_POSIX_String (Name_Ptr);
   end Get_Login_Name;

   -----------
   -- image --
   -----------

   function Image (ID : User_ID) return Standard.String is
   begin
      return User_ID'Image (ID);
   end Image;

   -----------
   -- Value --
   -----------

   function Value (Str : Standard.String) return User_ID is
   begin
      return User_ID'Value (Str);
   end Value;

   --  User Group Identification

   --  type Group_ID is private;

   -----------------------
   -- Get_Real_Group_ID --
   -----------------------

   function Get_Real_Group_ID return Group_ID is
      function getgid return gid_t;
      pragma Import (C, getgid, getgid_LINKNAME);
   begin
      return Group_ID (getgid);
   end Get_Real_Group_ID;

   ----------------------------
   -- Get_Effective_Group_ID --
   ----------------------------

   function Get_Effective_Group_ID return Group_ID is
      function getegid return gid_t;
      pragma Import (C, getegid, getegid_LINKNAME);
   begin
      return Group_ID (getegid);
   end Get_Effective_Group_ID;

   ------------------
   -- Set_Group_ID --
   ------------------

   procedure Set_Group_ID (ID : Group_ID) is
      function setgid (gid : gid_t) return int;
      pragma Import (C, setgid, setgid_LINKNAME);
   begin
      Check (setgid (gid_t (ID)));
   end Set_Group_ID;

   ----------------
   -- Get_Groups --
   ----------------

   type Access_Group_ID is access all Group_ID;

   function Get_Groups return Group_List is
      function getgroups
        (gidsetsize : int; grouplist : Access_Group_ID) return C.int;
      pragma Import (C, getgroups, getgroups_LINKNAME);
   begin
      loop
         declare
            NGroups_1 : constant int := getgroups (0, null);
            Groups : aliased Group_List (1 .. Integer (NGroups_1));
            NGroups_2 : int;
         begin
            NGroups_2 :=
              getgroups (Groups'Length, Groups (1)'Unchecked_Access);
            Check (NGroups_2);
            if NGroups_1 = NGroups_2 then
               return Groups;
            end if;
         end;
      end loop;
      --  the loop is in case some other process changes the number of
      --  items in the group list,
      --  before the first and second call to getgroups
   end Get_Groups;

   -----------
   -- Image --
   -----------

   function Image (ID : Group_ID) return Standard.String is
   begin
      return Trim_Leading_Blank (Group_ID'Image (ID));
   end Image;

   -----------
   -- Value --
   -----------

   function Value (Str : Standard.String) return Group_ID is
   begin
      return Group_ID (Group_ID'Value (Str));
   end Value;

end POSIX.Process_Identification;
