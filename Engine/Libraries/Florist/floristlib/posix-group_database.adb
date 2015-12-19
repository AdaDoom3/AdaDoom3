------------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                  P O S I X . G R O U P _ D A T A B A S E                 --
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

package body POSIX.Group_Database is

   use POSIX.C,
       POSIX.Implementation;

   function To_gid is new Unchecked_Conversion
      (POSIX.Process_Identification.Group_ID, gid_t);
   function To_Group_ID is new Unchecked_Conversion
      (gid_t, POSIX.Process_Identification.Group_ID);

   --  Operations to get information from a Group_Database_Item

   ---------------------
   --  Group_Name_Of  --
   ---------------------

   function Group_Name_Of (DB_Item : Group_Database_Item)
      return POSIX.POSIX_String is
   begin
      if DB_Item = null then
         Raise_POSIX_Error (Invalid_Argument);
      end if;
      return Form_POSIX_String (group_ptr (DB_Item).gr_name);
   end Group_Name_Of;

   -------------------
   --  Group_ID_Of  --
   -------------------

   function Group_ID_Of (DB_Item : Group_Database_Item)
      return POSIX.Process_Identification.Group_ID is
   begin
      if DB_Item = null then
         Raise_POSIX_Error (Invalid_Argument);
      end if;
      return To_Group_ID (group_ptr (DB_Item).gr_gid);
   end Group_ID_Of;

   ------------------------
   --  Group_ID_List_Of  --
   ------------------------

   function Group_ID_List_Of (DB_Item : Group_Database_Item)
      return Group_ID_List is
   begin
      if DB_Item = null then
         Raise_POSIX_Error (Invalid_Argument);
      end if;
      return Group_ID_List (group_ptr (DB_Item).gr_mem);
   end Group_ID_List_Of;

   ------------------------
   --  For_Every_Member  --
   ------------------------

   procedure For_Every_Member (List : Group_ID_List) is
      Quit : Boolean := False;
      P : char_ptr_ptr;
   begin
      P := char_ptr_ptr (List);
      if P = null then
         return;
      end if;
      while P.all /= null loop
         declare
            S : constant POSIX_String := Form_POSIX_String (P.all);
         begin
            Action (S, Quit);
            exit when Quit;
         end;
         Advance (P);
      end loop;
   end For_Every_Member;

   --------------
   --  Length  --
   --------------

   function Length (Member_List : Group_ID_List) return Natural is
      P : char_ptr_ptr := char_ptr_ptr (Member_List);
      Length : Natural := 0;
   begin
      if P = null then
         return 0;
      end if;
      while P.all /= null loop
         Length := Length + 1; Advance (P);
      end loop;
      return Length;
   end Length;

   -------------------------------
   --  Get_Group_Database_Item  --
   -------------------------------

   function getgrgid (gid : gid_t) return group_ptr;
   pragma Import (C, getgrgid, "getgrgid");
   function getgrnam (name : char_ptr) return group_ptr;
   pragma Import (C, getgrnam, "getgrnam");

   function Get_Group_Database_Item
      (GID : POSIX.Process_Identification.Group_ID)
     return Group_Database_Item is
      G : group_ptr;
   begin
      G := getgrgid (To_gid (GID));
      if G = null then
         Raise_POSIX_Error (Invalid_Argument);
      end if;
      return Group_Database_Item (G);
   end Get_Group_Database_Item;

   -------------------------------
   --  Get_Group_Database_Item  --
   -------------------------------

   function Get_Group_Database_Item
      (Name : POSIX_String)
     return Group_Database_Item is
      Name_With_NUL : POSIX_String := Name & NUL;
      G : group_ptr;
   begin
      G := getgrnam (Name_With_NUL (Name_With_NUL'First)'Unchecked_Access);
      if G = null then
         Raise_POSIX_Error (Invalid_Argument);
      end if;
      return Group_Database_Item (G);
   end Get_Group_Database_Item;

end POSIX.Group_Database;
