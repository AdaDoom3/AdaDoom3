-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2006 AdaCore                    --
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

--  <description>
--  This widget is used internally by gtk+, and is likely not very useful to
--  end-users.
--  This is a widget that has no visual rendering. It is used for reliable
--  pointer grabs and drag-and-drop
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Windows</group>

with Glib.Properties;
with Gtk.Widget;

package Gtk.Invisible is

   type Gtk_Invisible_Record is new Gtk.Widget.Gtk_Widget_Record with private;
   type Gtk_Invisible is access all Gtk_Invisible_Record'Class;

   procedure Gtk_New (Widget : out Gtk_Invisible);

   procedure Initialize (Widget : access Gtk_Invisible_Record'Class);

   function Get_Type return Gtk.Gtk_Type;

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Screen_Property
   --  Type:  Object
   --  Descr: The screen where this window will be displayed
   --
   --  </properties>

   Screen_Property : constant Glib.Properties.Property_Object;

private
   type Gtk_Invisible_Record is new
     Gtk.Widget.Gtk_Widget_Record with null record;

   Screen_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("screen");

   pragma Import (C, Get_Type, "gtk_invisible_get_type");
end Gtk.Invisible;

--  No binding: gtk_invisible_get_screen
--  No binding: gtk_invisible_new_for_screen
--  No binding: gtk_invisible_set_screen
