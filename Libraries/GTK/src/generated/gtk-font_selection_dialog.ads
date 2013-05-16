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

--  <description>
--  This widget provides a dialog for selecting a font. See also
--  Gtk.Font_Selection.
--
--  </description>
--  <screenshot>gtk-fontsel</screenshot>
--  <group>Selectors</group>
--  <testgtk>create_font_selection.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Font;      use Gdk.Font;
with Glib;          use Glib;
with Glib.Types;    use Glib.Types;
with Gtk.Buildable; use Gtk.Buildable;
with Gtk.Dialog;    use Gtk.Dialog;
with Gtk.Widget;    use Gtk.Widget;

package Gtk.Font_Selection_Dialog is

   type Gtk_Font_Selection_Dialog_Record is new Gtk_Dialog_Record with null record;
   type Gtk_Font_Selection_Dialog is access all Gtk_Font_Selection_Dialog_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Dialog : out Gtk_Font_Selection_Dialog;
       Title  : UTF8_String);
   procedure Initialize
      (Dialog : access Gtk_Font_Selection_Dialog_Record'Class;
       Title  : UTF8_String);
   --  Creates a new Gtk.Font_Selection_Dialog.Gtk_Font_Selection_Dialog.
   --  "title": the title of the dialog window

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_font_selection_dialog_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Apply_Button
      (Dialog : access Gtk_Font_Selection_Dialog_Record)
       return Gtk.Widget.Gtk_Widget;
   pragma Obsolescent (Get_Apply_Button);
   --  Obtains a button. The button doesn't have any function.
   --  Since: gtk+ 2.14
   --  Deprecated since 2.16, Don't use this function.

   function Get_Cancel_Button
      (Dialog : access Gtk_Font_Selection_Dialog_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Gets the 'Cancel' button. for the 'Cancel' button.
   --  Since: gtk+ 2.14

   function Get_Font
      (Dialog : access Gtk_Font_Selection_Dialog_Record)
       return Gdk.Font.Gdk_Font;
   pragma Obsolescent (Get_Font);
   --  Gets the currently-selected font. currently selected font in the
   --  dialog, or null if no font is selected
   --  Deprecated since 2.0, Use Gtk.Font_Selection_Dialog.Get_Font_Name
   --  instead.

   function Get_Font_Name
      (Dialog : access Gtk_Font_Selection_Dialog_Record) return UTF8_String;
   function Set_Font_Name
      (Dialog   : access Gtk_Font_Selection_Dialog_Record;
       Fontname : UTF8_String) return Boolean;
   --  Sets the currently selected font.
   --  "fontname": a font name like "Helvetica 12" or "Times Bold 18"

   function Get_Font_Selection
      (Dialog : access Gtk_Font_Selection_Dialog_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Retrieves the Gtk.Font_Selection.Gtk_Font_Selection widget embedded in
   --  the dialog.
   --  Since: gtk+ 2.22

   function Get_Ok_Button
      (Dialog : access Gtk_Font_Selection_Dialog_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Gets the 'OK' button. for the 'OK' button.
   --  Since: gtk+ 2.14

   function Get_Preview_Text
      (Dialog : access Gtk_Font_Selection_Dialog_Record) return UTF8_String;
   procedure Set_Preview_Text
      (Dialog : access Gtk_Font_Selection_Dialog_Record;
       Text   : UTF8_String);
   --  Sets the text displayed in the preview area.
   --  "text": the text to display in the preview area

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Font_Selection_Dialog_Record, Gtk_Font_Selection_Dialog);
   function "+"
     (Widget : access Gtk_Font_Selection_Dialog_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Font_Selection_Dialog
   renames Implements_Buildable.To_Object;

end Gtk.Font_Selection_Dialog;
