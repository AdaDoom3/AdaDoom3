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
--  The Gtk_Color_Button is a button which displays the currently selected
--  color an allows to open a color selection dialog to change the color. It is
--  suitable widget for selecting a color in a preference dialog.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Selectors</group>
--  <screenshot>color-button.png</screenshot>

with Glib.Properties;
with Gdk.Color;
with Gtk.Button;

package Gtk.Color_Button is

   type Gtk_Color_Button_Record is new Gtk.Button.Gtk_Button_Record with
     null record;
   type Gtk_Color_Button is access all Gtk_Color_Button_Record'Class;

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Color_Button.

   procedure Gtk_New (Button : out Gtk_Color_Button);
   procedure Initialize (Button : access Gtk_Color_Button_Record'Class);
   --  Creates a new color button. This returns a widget in the form of a small
   --  button containing a swatch representing the current selected color. When
   --  the button is clicked, a color-selection dialog will open, allowing the
   --  user to select a color. The swatch will be updated to reflect the new
   --  color when the user finishes.

   procedure Gtk_New_With_Color
     (Button : out Gtk_Color_Button;
      Color  : Gdk.Color.Gdk_Color);
   procedure Initialize_With_Color
     (Button : access Gtk_Color_Button_Record'Class;
      Color  : Gdk.Color.Gdk_Color);
   --  Creates a new color button.

   procedure Set_Color
     (Button : access Gtk_Color_Button_Record;
      Color  : Gdk.Color.Gdk_Color);
   function Get_Color
     (Button : access Gtk_Color_Button_Record) return Gdk.Color.Gdk_Color;
   --  Sets the current color to be Color.

   procedure Set_Alpha
     (Button : access Gtk_Color_Button_Record;
      Alpha  : Guint16);
   function Get_Alpha
     (Button : access Gtk_Color_Button_Record)
      return Glib.Guint16;
   --  Sets the current opacity to be Alpha (0 to 65_535).

   procedure Set_Use_Alpha
     (Button    : access Gtk_Color_Button_Record;
      Use_Alpha : Boolean);
   function Get_Use_Alpha
     (Button : access Gtk_Color_Button_Record) return Boolean;
   --  Sets whether or not the color button should use the alpha channel.

   procedure Set_Title
     (Button : access Gtk_Color_Button_Record;
      Title  : String);
   function Get_Title
     (Button : access Gtk_Color_Button_Record) return String;
   --  Sets the title for the color selection dialog.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Alpha_Property
   --  Type:  Uint
   --  Descr: The selected opacity value
   --         (0 fully transparent, 65535 fully opaque)
   --
   --  Name:  Color_Property
   --  Type:  Boxed
   --  Descr: The selected color
   --
   --  Name:  Title_Property
   --  Type:  String
   --  Descr: The title of the color selection dialog
   --
   --  Name:  Use_Alpha_Property
   --  Type:  Boolean
   --  Descr: Whether or not to give the color an alpha value
   --  </properties>

   Alpha_Property     : constant Glib.Properties.Property_Uint;
   Color_Property     : constant Gdk.Color.Property_Gdk_Color;
   Title_Property     : constant Glib.Properties.Property_String;
   Use_Alpha_Property : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "color_set"
   --    procedure Handler (Button : access Gtk_Color_Button_Record'Class);
   --    The color-set signal is emitted when the user selects a color. When
   --    handling this signal, use Get_Color and Get_Alpha to find out which
   --    color was just selected.
   --    Note that this signal is only emitted when the user changes the color.
   --    If you need to react to programmatic color changes as well, use the
   --    notify::color signal.
   --  </signals>

   Signal_Color_Set : constant Glib.Signal_Name := "color_set";

private
   Alpha_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("alpha");
   Color_Property : constant Gdk.Color.Property_Gdk_Color :=
     Gdk.Color.Property_Gdk_Color (Glib.Build ("color"));
   Title_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("title");
   Use_Alpha_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-alpha");

   pragma Import (C, Get_Type, "gtk_color_button_get_type");

end Gtk.Color_Button;
