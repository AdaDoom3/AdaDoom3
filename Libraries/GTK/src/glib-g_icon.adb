-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                    Copyright (C) 2010-2013, AdaCore               --
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

with Interfaces.C.Strings;

package body Glib.G_Icon is

   ---------------------
   -- G_Icon_Get_Type --
   ---------------------

   function G_Icon_Get_Type return GType is
      function Internal return GType;
      pragma Import (C, Internal, "g_icon_get_type");
   begin
      return Internal;
   end G_Icon_Get_Type;

   ---------
   -- "=" --
   ---------

   function "=" (Icon1, Icon2 : G_Icon) return Boolean is
      function Internal (Icon1, Icon2 : G_Icon) return Gboolean;
      pragma Import (C, Internal, "g_icon_equal");
   begin
      return Boolean'Val (Internal (Icon1, Icon2));
   end "=";

   ----------
   -- Hash --
   ----------

   function Hash (Icon : G_Icon) return Guint is
      function Internal (Icon : G_Icon) return Guint;
      pragma Import (C, Internal, "g_icon_hash");
   begin
      return Internal (Icon);
   end Hash;

   ---------------
   -- To_String --
   ---------------

   function To_String (Icon : G_Icon) return UTF8_String is
      use Interfaces.C.Strings;

      function Internal (Icon : G_Icon) return chars_ptr;
      pragma Import (C, Internal, "g_icon_to_string");
   begin
      return Value (Internal (Icon));
   end To_String;

   --------------------
   -- New_For_String --
   --------------------

   procedure New_For_String
     (Widget : out G_Icon;
      Str    : String)
   is
      function Internal (Str, Error : System.Address) return G_Icon;
      pragma Import (C, Internal, "g_icon_new_for_string");

      Tmp : constant String := Str & ASCII.NUL;
   begin
      --  We ignore the Error argument.
      Widget := Internal (Tmp'Address, System.Null_Address);
   end New_For_String;

end Glib.G_Icon;
