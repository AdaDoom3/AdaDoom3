-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
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
with Glib; use Glib;

package body Gdk.Keyval is

   --------------
   -- Is_Lower --
   --------------

   function Is_Lower (Keyval : Gdk.Types.Gdk_Key_Type) return Boolean is
      function Internal (Keyval : Gdk.Types.Gdk_Key_Type) return Gboolean;
      pragma Import (C, Internal, "gdk_keyval_is_lower");

   begin
      return Internal (Keyval) /= 0;
   end Is_Lower;

   --------------
   -- Is_Upper --
   --------------

   function Is_Upper (Keyval : Gdk.Types.Gdk_Key_Type) return Boolean is
      function Internal (Keyval : Gdk.Types.Gdk_Key_Type) return Gboolean;
      pragma Import (C, Internal, "gdk_keyval_is_upper");

   begin
      return Internal (Keyval) /= 0;
   end Is_Upper;

   ---------------
   -- From_Name --
   ---------------

   function From_Name (Keyval_Name : String) return Gdk.Types.Gdk_Key_Type is
      function Internal (Keyval_Name : String) return Gdk.Types.Gdk_Key_Type;
      pragma Import (C, Internal, "gdk_keyval_from_name");

   begin
      return Internal (Keyval_Name & ASCII.NUL);
   end From_Name;

   ----------
   -- Name --
   ----------

   function Name (Keyval : Gdk.Types.Gdk_Key_Type) return String is
      function Internal
        (Keyval : Gdk.Types.Gdk_Key_Type)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gdk_keyval_name");

      use type Interfaces.C.Strings.chars_ptr;
      P : constant Interfaces.C.Strings.chars_ptr := Internal (Keyval);
   begin
      if P = Interfaces.C.Strings.Null_Ptr then
         return "";
      else
         return C.Strings.Value (P);
      end if;
   end Name;

end Gdk.Keyval;
