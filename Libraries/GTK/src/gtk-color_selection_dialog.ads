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
--  The Gtk_Color_Selection_Dialog provides a standard dialog which allows the
--  user to select a color much like the Gtk_File_Selection provides a standard
--  dialog for file selection.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Drawing</group>

with Gtk.Dialog;
with Gtk.Button;
with Gtk.Color_Selection;

package Gtk.Color_Selection_Dialog is

   type Gtk_Color_Selection_Dialog_Record is new
     Gtk.Dialog.Gtk_Dialog_Record with private;
   type Gtk_Color_Selection_Dialog
     is access all Gtk_Color_Selection_Dialog_Record'Class;

   procedure Gtk_New
     (Color_Selection_Dialog : out Gtk_Color_Selection_Dialog;
      Title                  : UTF8_String);
   --  Create a new Color_Selection_Dialog with a specified title.

   procedure Initialize
     (Color_Selection_Dialog : access Gtk_Color_Selection_Dialog_Record'Class;
      Title                  : UTF8_String);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Menu.

   -----------------------------------------------
   -- Functions to get the fields of the dialog --
   -----------------------------------------------

   function Get_Colorsel
     (Color_Selection_Dialog : access Gtk_Color_Selection_Dialog_Record)
      return Gtk.Color_Selection.Gtk_Color_Selection;
   --  Get the Gtk_Color_Selection widget contained within the dialog.
   --  Use this widget and its Gtk.Color_Selection.Get_Color function to gain
   --  access to the selected color. Connect a handler for this widget's
   --  color_changed signal to be notified when the color changes.

   function Get_OK_Button
     (Color_Selection_Dialog : access Gtk_Color_Selection_Dialog_Record)
      return Gtk.Button.Gtk_Button;
   --  Get the OK button widget contained within the dialog.

   function Get_Cancel_Button
     (Color_Selection_Dialog : access Gtk_Color_Selection_Dialog_Record)
      return Gtk.Button.Gtk_Button;
   --  Get the cancel button widget contained within the dialog.

   function Get_Help_Button
     (Color_Selection_Dialog : access Gtk_Color_Selection_Dialog_Record)
      return Gtk.Button.Gtk_Button;
   --  Get the help button widget contained within the dialog.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  </properties>

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  </signals>

private
   type Gtk_Color_Selection_Dialog_Record is new
     Gtk.Dialog.Gtk_Dialog_Record with null record;

   pragma Import (C, Get_Type, "gtk_color_selection_dialog_get_type");
end Gtk.Color_Selection_Dialog;
