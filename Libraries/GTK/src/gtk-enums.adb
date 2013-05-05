-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                 Copyright (C) 2000-2013, AdaCore                  --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Unchecked_Conversion;
with Interfaces.C.Strings;

package body Gtk.Enums is

   -------------
   -- Convert --
   -------------

   function Convert (S : String) return System.Address is
      function Internal is new
        Unchecked_Conversion (Interfaces.C.Strings.chars_ptr, System.Address);
   begin
      return Internal (Interfaces.C.Strings.New_String (S));
   end Convert;

   function Convert_Chars_Ptr is new
     Unchecked_Conversion (System.Address, Interfaces.C.Strings.chars_ptr);

   function Convert (S : System.Address) return String is
   begin
      return Interfaces.C.Strings.Value (Convert_Chars_Ptr (S));
   end Convert;

   ----------------------
   -- Free_String_List --
   ----------------------

   procedure Free_String_List (List : in out String_List.Glist) is
      use type String_List.Glist;

      Tmp   : String_List.Glist := List;
      Chars : Interfaces.C.Strings.chars_ptr;

   begin
      while Tmp /= String_List.Null_List loop
         Chars := Convert_Chars_Ptr (String_List.Get_Data_Address (Tmp));
         Interfaces.C.Strings.Free (Chars);
         Tmp := String_List.Next (Tmp);
      end loop;

      String_List.Free (List);
      List := String_List.Null_List;
   end Free_String_List;

   ----------------------
   -- Free_String_List --
   ----------------------

   procedure Free_String_List (List : in out String_SList.GSlist) is
      use type String_SList.GSlist;

      Tmp   : String_SList.GSlist := List;
      Chars : Interfaces.C.Strings.chars_ptr;
   begin
      while Tmp /= String_SList.Null_List loop
         Chars := Convert_Chars_Ptr (String_SList.Get_Data_Address (Tmp));
         Interfaces.C.Strings.Free (Chars);
         Tmp := String_SList.Next (Tmp);
      end loop;

      String_SList.Free (List);
      List := String_SList.Null_List;
   end Free_String_List;

end Gtk.Enums;
