-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 2006-2013, AdaCore              --
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
--  This package provides a dialog, that you can use a development help in your
--  own application.
--  This dialog allows you to select any widget from your application, and see
--  its properties, or even change them dynamically. This helps in analyzing
--  the effect of properties.
--  </description>
--  <group>Miscellaneous</group>

with Glib.Object;
with Gtk.Widget;

package Gtkada.Properties is

   procedure Popup_Properties_Editor
     (Object : access Glib.Object.GObject_Record'Class);
   --  Popup a dialog to view and edit the properties of Object. If such a
   --  dialog is already displayed for Object, it is made visible.

   function Widget_At
     (Top  : access Gtk.Widget.Gtk_Widget_Record'Class;
      X, Y : Glib.Gint) return Gtk.Widget.Gtk_Widget;
   --  Return the widget at the given coordinates within Top

   function Widget_At_Pointer return Gtk.Widget.Gtk_Widget;
   --  Return the widget below the mouse pointer
   --  ??? See Gdk.Display.Get_Window_At_Pointer

end Gtkada.Properties;
