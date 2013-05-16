-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2013, AdaCore                   --
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

--  <c_version>1.3.6</c_version>
--  <group>Gdk, the low-level API</group>

with Glib;
with Gdk.Types;

package Gdk.Keyval is

   type Gdk_Keymap is private;

   function Get_Type return Glib.GType;

   function Name (Keyval : Gdk.Types.Gdk_Key_Type) return String;

   function From_Name (Keyval_Name : String) return Gdk.Types.Gdk_Key_Type;

   function To_Upper
     (Keyval : Gdk.Types.Gdk_Key_Type) return Gdk.Types.Gdk_Key_Type;

   function To_Lower
     (Keyval : Gdk.Types.Gdk_Key_Type) return Gdk.Types.Gdk_Key_Type;

   function Is_Upper (Keyval : Gdk.Types.Gdk_Key_Type) return Boolean;

   function Is_Lower (Keyval : Gdk.Types.Gdk_Key_Type) return Boolean;

   function To_Unicode (Keyval : Gdk.Types.Gdk_Key_Type) return Glib.Gunichar;

private
   pragma Import (C, To_Upper, "gdk_keyval_to_upper");
   pragma Import (C, To_Lower, "gdk_keyval_to_lower");
   pragma Import (C, Get_Type, "gdk_keymap_get_type");
   pragma Import (C, To_Unicode, "gdk_keyval_to_unicode");

   type Gdk_Keymap is new C_Proxy;
end Gdk.Keyval;

--  missing:
--  gdk_keymap_lookup_key
--  gdk_keymap_translate_keyboard_state
--  gdk_keymap_get_entries_for_keyval
--  gdk_keymap_get_entries_for_keycode
--  gdk_keyval_convert_case
--  gdk_unicode_to_keyval
