-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2002 ACT-Europe                 --
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

with Glib; use Glib;

with Gdk.Types;
with Gdk.Window;

package Gdk.Property is

   type Gdk_Prop_Mode is
     (Prop_Mode_Replace, Prop_Mode_Prepend, Prop_Mode_Append);
   pragma Convention (C, Gdk_Prop_Mode);

   function Atom_Intern
     (Atom_Name      : String;
      Only_If_Exists : Boolean := True) return Gdk.Types.Gdk_Atom;
   --  Convert from a string to an atom

   function Atom_Name (Atom : Gdk.Types.Gdk_Atom) return String;
   --  Convert from an atom to a string

   procedure Get
     (Window               : Gdk.Window.Gdk_Window;
      Property             : Gdk.Types.Gdk_Atom;
      The_Type             : Gdk.Types.Gdk_Atom;
      Offset               : Gulong;
      Length               : Gulong;
      Pdelete              : Boolean;
      Actual_Property_Type : out Gdk.Types.Gdk_Atom;
      Actual_Format        : out Gint;
      Data                 : out Guchar_Array_Access;
      Success              : out Boolean);

   procedure Change
     (Window    : Gdk.Window.Gdk_Window;
      Property  : Gdk.Types.Gdk_Atom;
      The_Type  : Gdk.Types.Gdk_Atom;
      Format    : Gint;
      Mode      : Gdk_Prop_Mode;
      Data      : Guchar_Array);

   procedure Delete
     (Window   : Gdk.Window.Gdk_Window;
      Property : Gdk.Types.Gdk_Atom);

private
   pragma Import (C, Delete, "gdk_property_delete");
end Gdk.Property;

--  missing:
--  gdk_text_property_to_text_list
--  gdk_text_property_to_utf8_list
--  gdk_utf8_to_string_target
--  gdk_utf8_to_compound_text
--  gdk_free_text_list
--  gdk_string_to_compound_text
--  gdk_free_compound_text
