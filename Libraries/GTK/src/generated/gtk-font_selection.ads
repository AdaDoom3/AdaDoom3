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
--  This widget provides a nice way for the user of your application to select
--  fonts. It first searches on your system for the list of fonts available,
--  and displays a set of boxes to select them based on their name, their
--  weight, their size, etc. This widget is provided in two forms, one widget
--  that can be embedded in any container, a Gtk_Font_Selection, whereas the
--  other one comes directly in its own separate window (to be popped up as a
--  dialog).
--
--  Some filters can be applied to the widget, when you want the user to
--  select only a font only among a specific subset (like bitmap or true-type
--  fonts for instance). There are two kinds of filters: a base filter, set in
--  your application and that the user can not change; a user filter that can
--  be modified interactively by the user.
--
--  </description>
--  <screenshot>gtk-fontsel</screenshot>
--  <group>Selectors</group>
--  <testgtk>create_font_selection.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Gdk;             use Gdk;
with Gdk.Font;        use Gdk.Font;
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Box;         use Gtk.Box;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Orientable;  use Gtk.Orientable;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Font_Selection is

   type Gtk_Font_Selection_Record is new Gtk_Vbox_Record with null record;
   type Gtk_Font_Selection is access all Gtk_Font_Selection_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Fontsel : out Gtk_Font_Selection);
   procedure Initialize (Fontsel : access Gtk_Font_Selection_Record'Class);
   --  Creates a new Gtk.Font_Selection.Gtk_Font_Selection.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_font_selection_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Face_List
      (Fontsel : access Gtk_Font_Selection_Record)
       return Gtk.Widget.Gtk_Widget;
   --  This returns the Gtk.Treeview.Gtk_Treeview which lists all styles
   --  available for the selected font. For example, 'Regular', 'Bold', etc.
   --  Since: gtk+ 2.14

   function Get_Family_List
      (Fontsel : access Gtk_Font_Selection_Record)
       return Gtk.Widget.Gtk_Widget;
   --  This returns the Gtk.Treeview.Gtk_Treeview that lists font families,
   --  for example, 'Sans', 'Serif', etc.
   --  Since: gtk+ 2.14

   function Get_Font
      (Fontsel : access Gtk_Font_Selection_Record) return Gdk.Font.Gdk_Font;
   pragma Obsolescent (Get_Font);
   --  Gets the currently-selected font.
   --  Deprecated since 2.0, Use Gtk.Font_Selection.Get_Font_Name instead.

   function Get_Font_Name
      (Fontsel : access Gtk_Font_Selection_Record) return UTF8_String;
   function Set_Font_Name
      (Fontsel  : access Gtk_Font_Selection_Record;
       Fontname : UTF8_String) return Boolean;
   --  Sets the currently-selected font. Note that the Fontsel needs to know
   --  the screen in which it will appear for this to work; this can be
   --  guaranteed by simply making sure that the such font exists or if the
   --  Fontsel doesn't belong to a particular screen yet.
   --  "fontname": a font name like "Helvetica 12" or "Times Bold 18"

   function Get_Preview_Entry
      (Fontsel : access Gtk_Font_Selection_Record)
       return Gtk.Widget.Gtk_Widget;
   --  This returns the Gtk.GEntry.Gtk_Entry used to display the font as a
   --  preview.
   --  Since: gtk+ 2.14

   function Get_Preview_Text
      (Fontsel : access Gtk_Font_Selection_Record) return UTF8_String;
   procedure Set_Preview_Text
      (Fontsel : access Gtk_Font_Selection_Record;
       Text    : UTF8_String);
   --  Sets the text displayed in the preview area. The Text is used to show
   --  how the selected font looks.
   --  "text": the text to display in the preview area

   function Get_Size
      (Fontsel : access Gtk_Font_Selection_Record) return Gint;
   --  The selected font size. or -1 if no font size is selected.
   --  Since: gtk+ 2.14

   function Get_Size_Entry
      (Fontsel : access Gtk_Font_Selection_Record)
       return Gtk.Widget.Gtk_Widget;
   --  This returns the Gtk.GEntry.Gtk_Entry used to allow the user to edit
   --  the font number manually instead of selecting it from the list of font
   --  sizes.
   --  Since: gtk+ 2.14

   function Get_Size_List
      (Fontsel : access Gtk_Font_Selection_Record)
       return Gtk.Widget.Gtk_Widget;
   --  This returns the GtkTreeeView used to list font sizes.
   --  Since: gtk+ 2.14

   ---------------------
   -- Interfaces_Impl --
   ---------------------

   function Get_Orientation
      (Self : access Gtk_Font_Selection_Record)
       return Gtk.Enums.Gtk_Orientation;
   procedure Set_Orientation
      (Self        : access Gtk_Font_Selection_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "Orientable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Font_Selection_Record, Gtk_Font_Selection);
   function "+"
     (Widget : access Gtk_Font_Selection_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Font_Selection
   renames Implements_Buildable.To_Object;

   package Implements_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Font_Selection_Record, Gtk_Font_Selection);
   function "+"
     (Widget : access Gtk_Font_Selection_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Font_Selection
   renames Implements_Orientable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Font_Property
   --  Type: Gdk.Font
   --  Flags: read-write
   --
   --  Name: Font_Name_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --
   --  Name: Preview_Text_Property
   --  Type: UTF8_String
   --  Flags: read-write

   Font_Property : constant Glib.Properties.Property_Boxed;
   Font_Name_Property : constant Glib.Properties.Property_String;
   Preview_Text_Property : constant Glib.Properties.Property_String;

private
   Font_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("font");
   Font_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("font-name");
   Preview_Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("preview-text");
end Gtk.Font_Selection;
