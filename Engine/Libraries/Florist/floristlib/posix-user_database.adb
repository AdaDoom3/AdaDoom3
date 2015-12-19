------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                   P O S I X . U S E R _ D A T A B A S E                  --
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
     Unchecked_Conversion;

package body POSIX.User_Database is

   use POSIX.C;
   use POSIX.Implementation;

   function To_uid_t is new Unchecked_Conversion
      (POSIX_Process_Identification.User_ID, uid_t);
   function To_User_ID is new Unchecked_Conversion
      (uid_t, POSIX_Process_Identification.User_ID);
   function To_Group_ID is new Unchecked_Conversion
      (gid_t, POSIX_Process_Identification.Group_ID);

   --------------------
   --  User_Name_Of  --
   --------------------

   function User_Name_Of (DB_Item   : User_Database_Item)
      return POSIX.POSIX_String is
   begin
      return Form_POSIX_String (DB_Item.pw_name);
   end User_Name_Of;

   ------------------
   --  User_ID_Of  --
   ------------------
   function User_ID_Of (DB_Item : User_Database_Item)
      return POSIX_Process_Identification.User_ID is
   begin
      return To_User_ID (DB_Item.pw_uid);
   end User_ID_Of;

   -------------------
   --  Group_ID_Of  --
   -------------------
   function Group_ID_Of (DB_Item : User_Database_Item)
      return POSIX_Process_Identification.Group_ID is
   begin
      return To_Group_ID (DB_Item.pw_gid);
   end Group_ID_Of;

   ----------------------------
   --  Initial_Directory_Of  --
   ----------------------------
   function Initial_Directory_Of (DB_Item : User_Database_Item)
      return POSIX.POSIX_String is
   begin
      return Form_POSIX_String (DB_Item.pw_dir);
   end Initial_Directory_Of;

   --------------------------
   --  Initial_Program_Of  --
   --------------------------

   function Initial_Program_Of (DB_Item   : User_Database_Item)
      return POSIX.POSIX_String is
   begin
      return Form_POSIX_String (DB_Item.pw_shell);
   end Initial_Program_Of;

   ------------------------------
   --  Get_User_Database_Item  --
   ------------------------------

   function getpwuid (c_uid : uid_t) return passwd_ptr;
   pragma Import (C, getpwuid, getpwuid_LINKNAME);

   function Get_User_Database_Item
      (UID : POSIX_Process_Identification.User_ID) return User_Database_Item is
      Result : passwd_ptr;
   begin
      Result := getpwuid (To_uid_t (UID));
      if Result = null then
         Raise_POSIX_Error;
      end if;
      return User_Database_Item (Result);
   end Get_User_Database_Item;

   ------------------------------
   --  Get_User_Database_Item  --
   ------------------------------

   function getpwnam (c_name : char_ptr) return passwd_ptr;
   pragma Import (C, getpwnam, getpwnam_LINKNAME);

   function Get_User_Database_Item (Name : POSIX_String)
      return User_Database_Item is
      Result : passwd_ptr;
      Name_With_NUL : POSIX_String := Name & NUL;
   begin
      Result := getpwnam
        (Name_With_NUL (Name_With_NUL'First)'Unchecked_Access);
      if Result = null then
         Raise_POSIX_Error;
      end if;
      return User_Database_Item (Result);
   end Get_User_Database_Item;

end POSIX.User_Database;
