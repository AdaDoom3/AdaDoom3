-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2006-2007 AdaCore                    --
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
--  The Gtk_Font_Button is a button which displays the currently selected font
--  an allows to open a font selection dialog to change the font. It is
--  suitable widget for selecting a font in a preference dialog.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Selectors</group>
--  <screenshot>font-button.png</screenshot>

with Glib.Properties;
with Gtk.Button;

package Gtk.Font_Button is

   type Gtk_Font_Button_Record is new Gtk.Button.Gtk_Button_Record with
     null record;
   type Gtk_Font_Button is access all Gtk_Font_Button_Record'Class;

   procedure Gtk_New (Font_Button : out Gtk_Font_Button);
   procedure Initialize (Font_Button : access Gtk_Font_Button_Record'Class);
   --  Creates a new font picker widget.

   procedure Gtk_New_With_Font
     (Font_Button : out Gtk_Font_Button; Fontname : String);
   procedure Initialize_With_Font
     (Font_Button : access Gtk_Font_Button_Record'Class; Fontname : String);
   --  Creates a new font picker widget.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Font_Button

   function Set_Font_Name
     (Font_Button : access Gtk_Font_Button_Record;
      Fontname    : String) return Boolean;
   function Get_Font_Name
     (Font_Button : access Gtk_Font_Button_Record) return String;
   --  Sets or updates the currently-displayed font in font picker dialog.
   --  Returns the value of Gtk.Font_Selection.Set_Font_Name if the font
   --  selection dialog exists, False otherwise.

   procedure Set_Show_Size
     (Font_Button : access Gtk_Font_Button_Record; Show_Size : Boolean);
   function Get_Show_Size
     (Font_Button : access Gtk_Font_Button_Record) return Boolean;
   --  If Show_Size is True, the font size will be displayed along with the
   --  name of the selected font.

   procedure Set_Show_Style
     (Font_Button : access Gtk_Font_Button_Record; Show_Style : Boolean);
   function Get_Show_Style
     (Font_Button : access Gtk_Font_Button_Record) return Boolean;
   --  Returns whether the name of the font style will be shown in the label.

   procedure Set_Title
     (Font_Button : access Gtk_Font_Button_Record; Title : String);
   function Get_Title
     (Font_Button : access Gtk_Font_Button_Record) return String;
   --  Retrieves the title of the font selection dialog.

   procedure Set_Use_Font
     (Font_Button : access Gtk_Font_Button_Record; Use_Font : Boolean);
   function Get_Use_Font
     (Font_Button : access Gtk_Font_Button_Record) return Boolean;
   --  If Use_Font is True, the font name will be written using the selected
   --  font.

   procedure Set_Use_Size
     (Font_Button : access Gtk_Font_Button_Record; Use_Size : Boolean);
   function Get_Use_Size
     (Font_Button : access Gtk_Font_Button_Record) return Boolean;
   --  If Use_Size is True, the font name will be written using the selected
   --  size.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Font_Name_Property
   --  Type:  String
   --  Descr: The name of the selected font
   --
   --  Name:  Show_Size_Property
   --  Type:  Boolean
   --  Descr: Whether selected font size is shown in the label
   --
   --  Name:  Show_Style_Property
   --  Type:  Boolean
   --  Descr: Whether the selected font style is shown in the label
   --
   --  Name:  Title_Property
   --  Type:  String
   --  Descr: The title of the font selection dialog
   --
   --  Name:  Use_Font_Property
   --  Type:  Boolean
   --  Descr: Whether the label is drawn in the selected font
   --
   --  Name:  Use_Size_Property
   --  Type:  Boolean
   --  Descr: Whether the label is drawn with the selected font size
   --
   --  </properties>

   Font_Name_Property  : constant Glib.Properties.Property_String;
   Show_Size_Property  : constant Glib.Properties.Property_Boolean;
   Show_Style_Property : constant Glib.Properties.Property_Boolean;
   Title_Property      : constant Glib.Properties.Property_String;
   Use_Font_Property   : constant Glib.Properties.Property_Boolean;
   Use_Size_Property   : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "font-set"
   --    procedure Handler (Button : access Gtk_Font_Button_Record'Class);
   --    The font-set signal is emitted when the user selects a font. When
   --    handling this signal, use Get_Font_Name to find out which font was
   --    just selected.
   --    Note that this signal is only emitted when the user changes the font.
   --    If you need to react to programmatic font changes as well, use the
   --    notify::font-name signal.
   --  </signals>

   Signal_Font_Set : constant Glib.Signal_Name := "font-set";

private
   Font_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("font-name");
   Show_Size_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-size");
   Show_Style_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-style");
   Title_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("title");
   Use_Font_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-font");
   Use_Size_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-size");

   pragma Import (C, Get_Type, "gtk_font_button_get_type");
end Gtk.Font_Button;
