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
--  A Gtk_Cell_Renderer_Text renders a given text in its cell, using the font,
--  color and style information provided by its properties. The text will be
--  ellipsized if it is too long and the ellipsize property allows it.
--
--  If the mode is CELL_RENDERER_MODE_EDITABLE, the Gtk_Cell_Renderer_Text
--  allows to edit its text using an entry.
--  </description>
--  <c_version>2.14</c_version>
--  <group>Trees and Lists</group>

with Pango.Enums;
with Glib.Properties;
with Gdk.Color;
with Gtk;
with Gtk.Cell_Renderer;

package Gtk.Cell_Renderer_Text is

   type Gtk_Cell_Renderer_Text_Record is
     new Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record with private;
   type Gtk_Cell_Renderer_Text is
     access all Gtk_Cell_Renderer_Text_Record'Class;

   procedure Gtk_New (Widget : out Gtk_Cell_Renderer_Text);
   procedure Initialize (Widget : access Gtk_Cell_Renderer_Text_Record'Class);
   --  Creates or initializes a new renderer

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   procedure Set_Fixed_Height_From_Font
     (Renderer       : access Gtk_Cell_Renderer_Text_Record;
      Number_Of_Rows : Gint);
   --  Sets the height of a renderer to explicitly be determined by the "font"
   --  and "y_pad" property set on it. Further changes in these properties do
   --  not affect the height, so they must be accompanied by a subsequent call
   --  to this function. Using this function is unflexible, and should really
   --  only be used if calculating the size of a cell is too slow (ie, a
   --  massive number of cells displayed). If number_of_rows is -1, then the
   --  fixed height is unset, and the height is determined by the properties
   --  again.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "edited"
   --    procedure Handler
   --     (Widget : access Gtk_Cell_Renderer_Text_Record'Class;
   --       Path : UTF8_String;
   --       New_Text : UTF8_String);
   --
   --    Called when the text has been edited interactively . Note that you
   --    also need to set the attribute "editable" for users to be able to
   --    interactively edit the cell. If you want to take into account the
   --    change, you need to change the value in the model appropriately, for
   --    instance through a call to Set_Value
   --  </signals>

   Signal_Edited : constant Glib.Signal_Name := "edited";

   ----------------
   -- Properties --
   ----------------

   --  The following properties are defined for this cell_renderer :
   --  <properties>
   --
   --  Name:  Attributes_Property
   --  Type:  Boxed
   --  Descr: A list of style attributes to apply to the text of the renderer
   --
   --  Name:  Background_Property
   --  Type:  String
   --  Descr: Background color as a string
   --
   --  Name:  Background_Gdk_Property
   --  Type:  Boxed
   --  Descr: Background color as a GdkColor
   --
   --  Name:  Editable_Property
   --  Type:  Boolean
   --  Descr: Whether the text can be modified by the user
   --
   --  Name:  Ellipsize_Property
   --  Type:  Enum
   --  Descr: The preferred place to ellipsize the string,
   --
   --  Name:  Family_Property
   --  Type:  String
   --  Descr: Name of the font family, e.g. Sans, Helvetica, Times, Monospace
   --
   --  Name:  Font_Property
   --  Type:  String
   --  Descr: Font description as a string
   --
   --  Name:  Font_Desc_Property
   --  Type:  Boxed
   --  Descr: Font description as a PangoFontDescription struct
   --
   --  Name:  Foreground_Property
   --  Type:  String
   --  Descr: Foreground color as a string
   --
   --  Name:  Foreground_Gdk_Property
   --  Type:  Boxed
   --  Descr: Foreground color as a GdkColor
   --
   --  Name:  Language_Property
   --  Type:  String
   --  Descr: The language this text is in, as an ISO code.
   --
   --  Name:  Markup_Property
   --  Type:  String
   --  Descr: Marked up text to render
   --
   --  Name:  Rise_Property
   --  Type:  Int
   --  Descr: Offset of text above the baseline
   --
   --  Name:  Scale_Property
   --  Type:  Double
   --  Descr: Font scaling factor
   --
   --  Name:  Single_Paragraph_Mode_Property
   --  Type:  Boolean
   --  Descr: Whether or not to keep all text in a single paragraph
   --
   --  Name:  Size_Property
   --  Type:  Int
   --  Descr: Font size
   --
   --  Name:  Size_Points_Property
   --  Type:  Double
   --  Descr: Font size in points
   --
   --  Name:  Stretch_Property
   --  Type:  Enum
   --  Descr: Font stretch
   --
   --  Name:  Strikethrough_Property
   --  Type:  Boolean
   --  Descr: Whether to strike through the text
   --
   --  Name:  Style_Property
   --  Type:  Enum
   --  Descr: Font style
   --
   --  Name:  Text_Property
   --  Type:  String
   --  Descr: Text to render
   --
   --  Name:  Underline_Property
   --  Type:  Enum
   --  Descr: Style of underline for this text
   --
   --  Name:  Variant_Property
   --  Type:  Enum
   --  Descr: Font variant
   --
   --  Name:  Weight_Property
   --  Type:  Int
   --  Descr: Font weight
   --
   --  Name:  Width_Chars_Property
   --  Type:  Int
   --  Descr: The desired width of the label, in characters
   --
   --  Name:  Wrap_Mode_Property
   --  Type:  Enum
   --  Descr: How to break the string into multiple lines,
   --
   --  Name:  Wrap_Width_Property
   --  Type:  Int
   --  Descr: The width at which the text is wrapped
   --
   --  Name:  Alignment_Property
   --  Type:  Enum
   --  Descr: How to align the lines
   --
   --  </properties>

   --  Attributes_Property        : constant Glib.Properties.Property_Boxed;
   --  Alignment_Property : constant Glib.Properties.Property_Enum;
   Background_Property            : constant Glib.Properties.Property_String;
   --  Background_Gdk_Property    : constant Glib.Properties.Property_Boxed;
   Editable_Property              : constant Glib.Properties.Property_Boolean;
   --  Ellipsize_Property : constant Glib.Properties.Property_Enum;
   Family_Property                : constant Glib.Properties.Property_String;
   Font_Property                  : constant Glib.Properties.Property_String;
   --  Font_Desc_Property         : constant Glib.Properties.Property_Boxed;
   Foreground_Property            : constant Glib.Properties.Property_String;
   Foreground_Gdk_Property        : constant Gdk.Color.Property_Gdk_Color;
   Language_Property              : constant Glib.Properties.Property_String;
   Markup_Property                : constant Glib.Properties.Property_String;
   Rise_Property                  : constant Glib.Properties.Property_Int;
   Scale_Property                 : constant Glib.Properties.Property_Double;
   Single_Paragraph_Mode_Property : constant Glib.Properties.Property_Boolean;
   Size_Property                  : constant Glib.Properties.Property_Int;
   Size_Points_Property           : constant Glib.Properties.Property_Double;
   --  Stretch_Property               : constant Glib.Properties.Property_Enum;
   Strikethrough_Property         : constant Glib.Properties.Property_Boolean;
   --  Style_Property                 : constant Glib.Properties.Property_Enum;
   Text_Property                  : constant Glib.Properties.Property_String;
   --  Underline_Property         : constant Glib.Properties.Property_Enum;
   --  Variant_Property           : constant Glib.Properties.Property_Enum;
   Weight_Property                : constant Glib.Properties.Property_Int;
   Width_Chars_Property           : constant Glib.Properties.Property_Int;
   Wrap_Mode_Property             : constant Pango.Enums.Property_Wrap_Mode;
   Wrap_Width_Property            : constant Glib.Properties.Property_Int;

   --   Attribute             Type in Model             Mode
   --   =========             =============             ====
   --
   --   "text"                UTF8_String               Read / Write
   --   "markup"              String                    Write
   --   "attributes"          PangoAttrList             Read / Write
   --   "background"          String                    Write
   --   "foreground"          String                    Write
   --   "background_gdk"      Gdk_Color                 Read / Write
   --   "foreground_gdk"      Gdk_Color                 Read / Write
   --   "font"                String                    Read / Write
   --   "font-desc"           Pango_Font_Description    Read / Write
   --   "family"              String                    Read / Write
   --   "style"               PangoStyle                Read / Write
   --   "variant"             PangoVariant              Read / Write
   --   "weight"              Gint                      Read / Write
   --   "stretch"             PangoStretch              Read / Write
   --   "size"                Gint                      Read / Write
   --   "size-points"         Gdouble                   Read / Write
   --   "scale"               Gdouble                   Read / Write
   --   "editable"            Boolean                   Read / Write
   --   "strikethrough"       Boolean                   Read / Write
   --   "underline"           PangoUnderline            Read / Write
   --   "rise"                Gint                      Read / Write
   --   "background-set"      Boolean                   Read / Write
   --   "foreground-set"      Boolean                   Read / Write
   --   "family-set"          Boolean                   Read / Write
   --   "style-set"           Boolean                   Read / Write
   --   "variant-set"         Boolean                   Read / Write
   --   "weight-set"          Boolean                   Read / Write
   --   "stretch-set"         Boolean                   Read / Write
   --   "size-set"            Boolean                   Read / Write
   --   "scale-set"           Boolean                   Read / Write
   --   "editable-set"        Boolean                   Read / Write
   --   "strikethrough-set"   Boolean                   Read / Write
   --   "underline-set"       Boolean                   Read / Write
   --   "rise-set"            Boolean                   Read / Write

private
   type Gtk_Cell_Renderer_Text_Record is
     new Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record with null record;

   --  Alignment_Property : constant Glib.Properties.Property_Enum :=
   --     Glib.Properties.Build ("alignment");
   --  Attributes_Property : constant Glib.Properties.Property_Boxed :=
   --     Glib.Properties.Build ("attributes");
   Background_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("background");
--     Background_Gdk_Property : constant Glib.Properties.Property_Boxed :=
--       Glib.Properties.Build ("background-gdk");
   Editable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("editable");
--     Ellipsize_Property : constant Glib.Properties.Property_Enum :=
--       Glib.Properties.Build ("ellipsize");
   Family_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("family");
   Font_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("font");
--     Font_Desc_Property : constant Glib.Properties.Property_Boxed :=
--       Glib.Properties.Build ("font-desc");
   Foreground_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("foreground");
   Foreground_Gdk_Property : constant Gdk.Color.Property_Gdk_Color :=
     Gdk.Color.Build ("foreground_gdk");
   Language_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("language");
   Markup_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("markup");
   Rise_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("rise");
   Scale_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("scale");
   Single_Paragraph_Mode_Property : constant Glib.Properties.Property_Boolean
     := Glib.Properties.Build ("single-paragraph-mode");
   Size_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("size");
   Size_Points_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("size-points");
--     Stretch_Property : constant Glib.Properties.Property_Enum :=
--       Glib.Properties.Build ("stretch");
   Strikethrough_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("strikethrough");
--     Style_Property : constant Glib.Properties.Property_Enum :=
--       Glib.Properties.Build ("style");
   Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("text");
--     Underline_Property : constant Glib.Properties.Property_Enum :=
--       Glib.Properties.Build ("underline");
--     Variant_Property : constant Glib.Properties.Property_Enum :=
--       Glib.Properties.Build ("variant");
   Weight_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("weight");
   Width_Chars_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("width-chars");
   Wrap_Mode_Property : constant Pango.Enums.Property_Wrap_Mode :=
     Pango.Enums.Property_Wrap_Mode (Glib.Build ("wrap-mode"));
   Wrap_Width_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("wrap-width");

   pragma Import (C, Get_Type,  "gtk_cell_renderer_text_get_type");
end Gtk.Cell_Renderer_Text;
