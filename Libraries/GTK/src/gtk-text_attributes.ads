-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2001-2013, AdaCore                   --
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
--  This package defines the Gtk_Text_Attributes type.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Multiline Text Editor</group>

with Glib;
with Gdk.Color;
with Gtk.Enums;
with Pango.Enums;
with Pango.Font;
with Pango.Tabs;

package Gtk.Text_Attributes is

   type Gtk_Text_Attributes is new Glib.C_Proxy;
   type Gtk_Text_Appearance is new Glib.C_Proxy;

   procedure Gtk_New (Text_Attr : out Gtk_Text_Attributes);
   --  Create a new Gtk_Text_Attributes structure.

   function Get_Type return Glib.GType;
   --  Return the internal type used fro a Gtk_Text_Attributes

   procedure Ref (Values : Gtk_Text_Attributes);
   --  Increase the reference counter of the given Gtk_Text_Attributes
   --  by one (this counter is initially set to 1 when this structure
   --  is created).

   procedure Unref (Values : Gtk_Text_Attributes);
   --  Decrease the reference counter by one. When it reaches zero,
   --  the Gtk_Text_Attributes is automatically deallocated.

   function Copy (Src : Gtk_Text_Attributes) return Gtk_Text_Attributes;
   --  Create a copy of the given Gtk_Text_Attributes structure.

   procedure Copy_Values
     (Src  : Gtk_Text_Attributes;
      Dest : Gtk_Text_Attributes);
   --  Copy the values from Src into Dest so that Dest has the same values
   --  as Src. Free existing values in Dest. Dest's reference counter
   --  is preserved.

   ---------------------
   -- Text appearance --
   ---------------------

   procedure Set_Rise
     (Appearance : Gtk_Text_Appearance;
      Rise       : Gint);
   function Get_Rise
     (Appearance : Gtk_Text_Appearance) return Gint;
   --  Offset of the text above the baseline (or below if negative)

   procedure Set_Underline
     (Appearance : Gtk_Text_Appearance;
      Underline  : Pango.Enums.Underline);
   function Get_Underline
     (Appearance : Gtk_Text_Appearance) return Pango.Enums.Underline;
   --  Set the underline mode

   procedure Set_Strikethrough
     (Appearance : Gtk_Text_Appearance;
      Strikethrough : Boolean);
   function Get_Strikethrough
     (Appearance : Gtk_Text_Appearance) return Boolean;
   --  Whether to strike through the text

   procedure Set_Fg_Color
     (Appearance : Gtk_Text_Appearance;
      Color      : Gdk.Color.Gdk_Color);
   function Get_Fg_Color
     (Appearance : Gtk_Text_Attributes) return Gdk.Color.Gdk_Color;
   --  The color used to display the text

   procedure Set_Bg_Color
     (Appearance : Gtk_Text_Appearance;
      Color      : Gdk.Color.Gdk_Color);
   function Get_Bg_Color
     (Appearance : Gtk_Text_Attributes) return Gdk.Color.Gdk_Color;
   --  The background color for the text

   procedure Set_Fg_Stipple
     (Appearance : Gtk_Text_Appearance;
      Stipple    : Gdk.Gdk_Bitmap);
   function Get_Fg_Stipple
     (Appearance : Gtk_Text_Attributes) return Gdk.Gdk_Bitmap;
   --  The pattern used in the foreground

   procedure Set_Bg_Stipple
     (Appearance : Gtk_Text_Appearance;
      Stipple    : Gdk.Gdk_Bitmap);
   function Get_Bg_Stipple
     (Appearance : Gtk_Text_Attributes) return Gdk.Gdk_Bitmap;
   --  The pattern used in the background

   ----------------
   -- Attributes --
   ----------------

   procedure Set_Font
     (Text_Attr : Gtk_Text_Attributes;
      Font      : Pango.Font.Pango_Font_Description);
   function Get_Font (Text_Attr : Gtk_Text_Attributes)
     return Pango.Font.Pango_Font_Description;
   --  Return the Pango_Font_Description associated to the given
   --  Gtk_Text_Attributes.

   procedure Set_Justification
     (Text_Attr : Gtk_Text_Attributes;
      Justification : Gtk.Enums.Gtk_Justification);
   function Get_Justification
     (Text_Attr : Gtk_Text_Attributes) return Gtk.Enums.Gtk_Justification;
   --  Set the justification for this attributes

   procedure Set_Direction
     (Text_Attr : Gtk_Text_Attributes;
      Direction : Gtk.Enums.Gtk_Text_Direction);
   function Get_Direction
     (Text_Attr : Gtk_Text_Attributes) return Gtk.Enums.Gtk_Text_Direction;
   --  Set the text direction for this attributes

   procedure Set_Font_Scale
     (Text_Attr : Gtk_Text_Attributes;
      Scale     : Gdouble);
   function Get_Font_Scale
     (Text_Attr : Gtk_Text_Attributes) return Gdouble;
   --  Set the scaling to use for the font

   procedure Set_Left_Margin
     (Text_Attr : Gtk_Text_Attributes;
      Margin    : Gint);
   function Get_Left_Margin
     (Text_Attr : Gtk_Text_Attributes) return Gint;
   --  Set the left margin

   procedure Set_Right_Margin
     (Text_Attr : Gtk_Text_Attributes;
      Margin    : Gint);
   function Get_Right_Margin
     (Text_Attr : Gtk_Text_Attributes) return Gint;
   --  Set the right margin

   procedure Set_Indent
     (Text_Attr : Gtk_Text_Attributes;
      Margin    : Gint);
   function Get_Indent
     (Text_Attr : Gtk_Text_Attributes) return Gint;
   --  Amount to indent the paragraph

   procedure Set_Pixels_Above_Line
     (Text_Attr : Gtk_Text_Attributes;
      Margin    : Gint);
   function Get_Pixels_Above_Line
     (Text_Attr : Gtk_Text_Attributes) return Gint;
   --  Set the number of blank pixels above paragraphs

   procedure Set_Pixels_Below_Line
     (Text_Attr : Gtk_Text_Attributes;
      Margin    : Gint);
   function Get_Pixels_Below_Line
     (Text_Attr : Gtk_Text_Attributes) return Gint;
   --  Set the number of blank pixels below paragraphs

   procedure Set_Pixels_Inside_Wrap
     (Text_Attr : Gtk_Text_Attributes;
      Margin    : Gint);
   function Get_Pixels_Inside_Wrap
     (Text_Attr : Gtk_Text_Attributes) return Gint;
   --  Set the number of pixels between wrapped lines in a paragraph

   procedure Set_Wrap_Mode
     (Text_Attr : Gtk_Text_Attributes;
      Mode      : Gtk.Enums.Gtk_Wrap_Mode);
   function Get_Wrap_Mode
     (Text_Attr : Gtk_Text_Attributes) return Gtk.Enums.Gtk_Wrap_Mode;
   --  Set the wrapping mode

   procedure Set_Invisible
     (Text_Attr : Gtk_Text_Attributes;
      Invisible : Boolean);
   function Get_Invisible (Text_Attr : Gtk_Text_Attributes) return Boolean;
   --  Whether the text is invisible

   procedure Set_Bg_Full_Height
     (Text_Attr : Gtk_Text_Attributes;
      Full_Height : Boolean);
   function Get_Bg_Full_Height
     (Text_Attr : Gtk_Text_Attributes) return Boolean;
   --  Whether the background occupies the full line height rather than just
   --  the area occupied by the text.

   procedure Set_Editable
     (Text_Attr : Gtk_Text_Attributes;
      Editable  : Boolean);
   function Get_Editable
     (Text_Attr : Gtk_Text_Attributes) return Boolean;
   --  Whether the text is editable

   procedure Set_Tabs
     (Text_Attr : Gtk_Text_Attributes;
      Tabs      : Pango.Tabs.Pango_Tab_Array);
   function Get_Tabs
     (Text_Attr : Gtk_Text_Attributes) return Pango.Tabs.Pango_Tab_Array;
   --  Set the default tab stops for paragraphs

   function Get_Appearance
     (Text_Attr : Gtk_Text_Attributes) return Gtk_Text_Appearance;
   --  Return the appearance of the text. This can be modified with the
   --  subprograms above.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  </properties>

private
   pragma Import (C, Get_Type,    "gtk_text_attributes_get_type");
   pragma Import (C, Ref,         "gtk_text_attributes_ref");
   pragma Import (C, Unref,       "gtk_text_attributes_unref");
   pragma Import (C, Copy,        "gtk_text_attributes_copy");
   pragma Import (C, Copy_Values, "gtk_text_attributes_copy_values");
   pragma Import (C, Get_Font,    "ada_text_attributes_get_font");
   pragma Import (C, Set_Font,    "ada_text_attributes_set_font");

   pragma Import
     (C, Set_Justification, "ada_text_attributes_set_justification");
   pragma Import
     (C, Get_Justification, "ada_text_attributes_Get_justification");
   pragma Import (C, Set_Direction, "ada_text_attributes_set_direction");
   pragma Import (C, Get_Direction, "ada_text_attributes_get_direction");
   pragma Import
     (C, Set_Font_Scale, "ada_text_attributes_set_font_scale");
   pragma Import
     (C, Get_Font_Scale, "ada_text_attributes_get_font_scale");
   pragma Import (C, Set_Rise, "ada_text_appearance_set_rise");
   pragma Import (C, Get_Rise, "ada_text_appearance_get_rise");
   pragma Import (C, Set_Underline, "ada_text_appearance_set_underline");
   pragma Import (C, Get_Underline, "ada_text_appearance_get_underline");
   pragma Import (C, Set_Fg_Color, "ada_text_appearance_set_fg_color");
   pragma Import (C, Get_Fg_Color, "ada_text_appearance_get_fg_color");
   pragma Import (C, Set_Bg_Color, "ada_text_appearance_set_bg_color");
   pragma Import (C, Get_Bg_Color, "ada_text_appearance_get_bg_color");
   pragma Import (C, Set_Fg_Stipple, "ada_text_appearance_set_fg_stipple");
   pragma Import (C, Get_Fg_Stipple, "ada_text_appearance_get_fg_stipple");
   pragma Import (C, Set_Bg_Stipple, "ada_text_appearance_set_bg_stipple");
   pragma Import (C, Get_Bg_Stipple, "ada_text_appearance_get_bg_stipple");

   pragma Import (C, Set_Left_Margin, "ada_text_attribute_set_left_margin");
   pragma Import (C, Get_Left_Margin, "ada_text_attribute_get_left_margin");
   pragma Import (C, Set_Right_Margin, "ada_text_attribute_set_right_margin");
   pragma Import (C, Get_Right_Margin, "ada_text_attribute_get_right_margin");
   pragma Import (C, Set_Indent, "ada_text_attribute_set_indent");
   pragma Import (C, Get_Indent, "ada_text_attribute_get_indent");
   pragma Import
     (C, Set_Pixels_Above_Line, "ada_text_attribute_set_pixels_above_line");
   pragma Import
     (C, Get_Pixels_Above_Line, "ada_text_attribute_get_pixels_above_line");
   pragma Import
     (C, Set_Pixels_Below_Line, "ada_text_attribute_set_pixels_below_line");
   pragma Import
     (C, Get_Pixels_Below_Line, "ada_text_attribute_get_pixels_below_line");
   pragma Import
     (C, Set_Pixels_Inside_Wrap, "ada_text_attribute_set_pixels_inside_wrap");
   pragma Import
     (C, Get_Pixels_Inside_Wrap, "ada_text_attribute_get_pixels_inside_wrap");
   pragma Import (C, Set_Wrap_Mode, "ada_text_attribute_set_wrap_mode");
   pragma Import (C, Get_Wrap_Mode, "ada_text_attribute_get_wrap_mode");
   pragma Import (C, Get_Appearance, "ada_text_attribute_get_appearance");
   pragma Import (C, Set_Tabs, "ada_text_attribute_set_tabs");
   pragma Import (C, Get_Tabs, "ada_text_attribute_get_tabs");

end Gtk.Text_Attributes;

--  This package doesn't give access to the "language" property of a
--  Gtk_Text_Attributes
