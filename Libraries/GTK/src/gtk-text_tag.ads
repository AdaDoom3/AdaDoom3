-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                 Copyright (C) 2001-2007 AdaCore                   --
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
--  A tag is a set of properties that can be associated with a range of text.
--  See also Gtk.Text_Attributes. Tags should be in a Gtk_Text_Tag_Table for
--  a given before before they are used in that buffer.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Multiline Text Editor</group>

with Gtk.Enums;
with Pango.Enums;
with Glib.Properties;
with Glib.GSlist;
with Pango.Font;
with Gdk.Color;
with System;
pragma Elaborate_All (Gdk.Color);
pragma Elaborate_All (Glib.GSlist);

package Gtk.Text_Tag is

   type Gtk_Text_Tag_Record is new GObject_Record with private;
   type Gtk_Text_Tag is access all Gtk_Text_Tag_Record'Class;

   function Convert (W : Gtk_Text_Tag) return System.Address;
   function Convert (W : System.Address) return Gtk_Text_Tag;
   package Text_Tag_List is new Glib.GSlist.Generic_SList
     (Gpointer => Gtk_Text_Tag);

   procedure Gtk_New (Widget : out Gtk_Text_Tag; Name : String := "");
   --  Create a new Gtk_Text_Tag.
   --  Newly created tags must be added to the tags table for the buffer you
   --  intend to use them in.
   --     Gtk.Text_Tag_Table.Add (Get_Tag_Table (Buffer), Tag);
   --  See also Gtk.Text_Buffer.Create_Tag which is a more convenient way of
   --  creating a tag.

   procedure Initialize
     (Widget : access Gtk_Text_Tag_Record'Class;
      Name   : String := "");
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Glib.GType;
   --  Return the internal value associated with this widget.

   procedure Set_Priority (Tag : access Gtk_Text_Tag_Record; Priority : Gint);
   function Get_Priority (Tag : access Gtk_Text_Tag_Record) return Gint;
   --  Set the priority of a Gtk_Text_Tag.
   --  Valid priorities start at 0 and go to one less than Table_Size.
   --  Each tag in a table has a unique priority; setting the priority of one
   --  tag shifts the priorities of all the other tags in the table to maintain
   --  a unique priority for each tag. Higher priority tags "win" if two tags
   --  both set the same text attribute. When adding a tag to a tag table, it
   --  will be assigned the highest priority in the table by default; so
   --  normally the precedence of a set of tags is the order in which they were
   --  added to the table, or created with Gtk.Text_Buffer.Create_Tag, which
   --  adds the tag to the buffer's table automatically.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Name_Property
   --  Type:  String
   --  Flags: read-write (construct only)
   --  Descr: Name used to refer to the text tag
   --
   --  Name:  Background_Property
   --  Type:  String
   --  Flags: writable
   --  Descr: Background color as a string
   --
   --  Name:  Background_Gdk_Property
   --  Type:  Gdk_Color
   --  Flags: read-write
   --  Descr: Background color
   --
   --  Name:  Background_Full_Height_Property
   --  Type:  Boolean
   --  Flags: read-write
   --  Descr: Whether the background color fills the entire line height or
   --         only the height of the tagged characters
   --
   --  Name:  Background_Stipple_Property
   --  Type:  Gdk_Pixmap
   --  Flags: read-write
   --  Descr: Bitmap to use as a mask when drawing the text background
   --
   --  Name:  Foreground_Property
   --  Type:  String
   --  Flags: writable
   --  Descr: Foreground color as a string
   --
   --  Name:  Foreground_Gdk_Property
   --  Type:  Gdk_Color
   --  Flags: read-write
   --  Descr: Foreground color
   --
   --  Name:  Foreground_Stipple_Property
   --  Type:  Gdk_Pixmap
   --  Flags: read-write
   --  Descr: Bitmap to use as a mask when drawing the text foreground
   --
   --  Name:  Direction_Property
   --  Type:  Gtk_Text_Direction
   --  Flags: read-write
   --  Descr: Text direction, e.g. right-to-left or left-to-right
   --
   --  Name:  Editable_Property
   --  Type:  Boolean
   --  Flags: read-write
   --  Descr: Whether the text can be modified by the user
   --
   --  Name:  Font_Property
   --  Type:  String
   --  Flags: read-write
   --  Descr: Font description as a string
   --
   --  Name:  Font_Desc_Property
   --  Type:  Pango_Font_Description
   --  Flags: read-write
   --  Descr: Font description
   --
   --  Name:  Family_Property
   --  Type:  String
   --  Flags: read-write
   --  Descr: Name of the font family, e.g. Sans, Helvetica, Times, Monospace
   --
   --  Name:  Style_Property
   --  Type:  Pango.Enums.Style
   --  Flags: read-write
   --  Descr: Font style
   --
   --  Name:  Variant_Property
   --  Type:  Pango_Type_Variant
   --  Flags: read-write
   --  Descr: Font variant
   --
   --  Name:  Weight_Property
   --  Type:  Pango.Enums.Weight
   --  Flags: read-write
   --  Descr: Font weight
   --
   --  Name:  Stretch_Property
   --  Type:  Pango_Type_Strech
   --  Flags: read-write
   --  Descr: Font strech
   --
   --  Name:  Size_Property
   --  Type:  Gint
   --  Flags: read-write
   --  Descr: Font size
   --
   --  Name:  Size_Points_Property
   --  Type:  Gdouble
   --  Flags: read-write
   --  Descr: Font size in points
   --
   --  Name:  Justification_Property
   --  Type:  Gtk_Type_Justification
   --  Flags: read-write
   --  Descr: Left, right, or center justification
   --
   --  Name:  Language_Property
   --  Type:  String
   --  Flags: read-write
   --  Descr: Language engine code to use for rendering the text
   --
   --  Name:  Left_Margin_Property
   --  Type:  Gint
   --  Flags: read-write
   --  Descr: Width of the left margin in pixels
   --
   --  Name:  Right_Margin_Property
   --  Type:  Gint
   --  Flags: read-write
   --  Descr: Width of the right margin in pixels
   --
   --  Name:  Indent_Property
   --  Type:  Gint
   --  Flags: read-write
   --  Descr: Amount to indent the paragraph, in pixels
   --
   --  Name:  Rise_Property
   --  Type:  Gint
   --  Flags: read-write
   --  Descr: Offset of text above the baseline (below the baseline if
   --         rise is negative)
   --
   --  Name:  Pixels_Above_Lines_Property
   --  Type:  Gint
   --  Flags: read-write
   --  Descr: Pixels of blank space above paragraphs
   --
   --  Name:  Pixels_Below_Lines_Property
   --  Type:  Gint
   --  Flags: read-write
   --  Descr: Pixels of blank space below paragraphs
   --
   --  Name:  Strikethrough_Property
   --  Type:  Boolean
   --  Flags: read-write
   --  Descr: Whether to strike through the text
   --
   --  Name:  Underline_Property
   --  Type:  Pango_Type_Underline
   --  Flags: read-write
   --  Descr: Style of underline for this text
   --
   --  Name:  Wrap_Mode_Property
   --  Type:  Gtk_Wrap_Mode
   --  Flags: read-write
   --  Descr: Whether to wrap lines never, at word boundaries, or at
   --         character boundaries
   --
   --  Name:  Tabs_Property
   --  Type:  Pango_Tab_Array
   --  Flags: read-write
   --  Descr: Custom tabs for this text
   --
   --  Name:  Invisible_Property
   --  Type:  Boolean
   --  Flags: read-write
   --  Descr: Whether this text is hidden
   --
   --  Name:  Scale_Property
   --  Type:  Double
   --  Descr: Font size as a scale factor relative to the default font size.
   --         This properly adapts to theme changes etc. so is recommended.
   --         Pango predefines some scales such as PANGO_SCALE_X_LARGE
   --
   --  Name:  Paragraph_Background_Property
   --  Type:  String
   --  Descr: Paragraph background color as a string
   --
   --  Name:  Paragraph_Background_Gdk_Property
   --  Type:  Gdk_Color
   --  Descr: Paragraph background color as a color

   --  The following properties indicate whether a tag modifies some aspect of
   --  text or not. You do not need to modify them explicitely when modifying
   --  one of the above properties, since they will be automatically set to
   --  True when you modify the above.
   --  However, the ones below should be set back to False if you wish to
   --  cancel the effect of a previous modification of a tag.
   --
   --  They all default to False, unless you have modified one of the
   --  properties above. They are all of type boolean, and match the properties
   --  above.
   --
   --  Name: Background_Full_Height_Set_Property
   --  Name: Background_Set_Property
   --  Name: Background_Stipple_Set_Property
   --  Name: Editable_Set_Property
   --  Name: Family_Set_Property
   --  Name: Foreground_Set_Property
   --  Name: Foreground_Stipple_Set_Property
   --  Name: Indent_Set_Property
   --  Name: Inside_Wrap_Set_Property
   --  Name: Invisible_Set_Property
   --  Name: Justification_Set_Property
   --  Name: Language_Set_Property
   --  Name: Left_Margin_Set_Property
   --  Name: Paragraph_Background_Set_Property
   --  Name: Pixels_Above_Lines_Set_Property
   --  Name: Pixels_Below_Lines_Set_Property
   --  Name: Pixels_Inside_Wrap_Set_Property
   --  Name: Right_Margin_Set_Property
   --  Name: Rise_Set_Property
   --  Name: Scale_Set_Property
   --  Name: Size_Set_Property
   --  Name: Stretch_Set_Property
   --  Name: Strikethrough_Set_Property
   --  Name: Style_Set_Property
   --  Name: Tabs_Set_Property
   --  Name: Underline_Set_Property
   --  Name: Variant_Set_Property
   --  Name: Weight_Set_Property
   --  Name: Wrap_Mode_Set_Property
   --
   --  </properties>

   Background_Full_Height_Property : constant Glib.Properties.Property_Boolean;
   Background_Gdk_Property     : constant Gdk.Color.Property_Gdk_Color;
   Background_Property         : constant Glib.Properties.Property_String_WO;
   Background_Stipple_Property : constant Glib.Properties.Property_C_Proxy;
   Direction_Property         : constant Gtk.Enums.Property_Gtk_Text_Direction;
   Editable_Property           : constant Glib.Properties.Property_Boolean;
   Family_Property             : constant Glib.Properties.Property_String;
   Font_Desc_Property          : constant Pango.Font.Property_Font_Description;
   Font_Property               : constant Glib.Properties.Property_String;
   Foreground_Gdk_Property     : constant Gdk.Color.Property_Gdk_Color;
   Foreground_Property         : constant Glib.Properties.Property_String_WO;
   Foreground_Stipple_Property : constant Glib.Properties.Property_C_Proxy;
   Indent_Property             : constant Glib.Properties.Property_Int;
   Inside_Wrap_Property        : constant Glib.Properties.Property_Int;
   Invisible_Property          : constant Glib.Properties.Property_Boolean;
   Justification_Property      : constant Gtk.Enums.Property_Gtk_Justification;
   Language_Property           : constant Glib.Properties.Property_String;
   Left_Margin_Property        : constant Glib.Properties.Property_Int;
   Name_Property               : constant Glib.Properties.Property_String;
   Paragraph_Background_Property  : constant Glib.Properties.Property_String;
   Pixels_Above_Lines_Property : constant Glib.Properties.Property_Int;
   Pixels_Below_Lines_Property : constant Glib.Properties.Property_Int;
   Pixels_Inside_Wrap_Property : constant Glib.Properties.Property_Int;
   Right_Margin_Property       : constant Glib.Properties.Property_Int;
   Rise_Property               : constant Glib.Properties.Property_Int;
   Scale_Property              : constant Glib.Properties.Property_Double;
   Size_Points_Property        : constant Glib.Properties.Property_Double;
   Size_Property               : constant Glib.Properties.Property_Int;
   Stretch_Property            : constant Pango.Enums.Property_Stretch;
   Strikethrough_Property      : constant Glib.Properties.Property_Boolean;
   Style_Property              : constant Pango.Enums.Property_Style;
   Underline_Property          : constant Pango.Enums.Property_Underline;
   Variant_Property            : constant Pango.Enums.Property_Variant;
   Weight_Property             : constant Pango.Enums.Property_Weight;
   Wrap_Mode_Property          : constant Gtk.Enums.Property_Gtk_Wrap_Mode;

   --  Tabs_Property            : constant Pango.Types.Property_Tab_Array;
   --  Paragraph_Background_Gdk_Property :
   --     constant Glib.Properties.Property_Boxed;

   Background_Full_Height_Set_Property : constant
     Glib.Properties.Property_Boolean;
   Background_Set_Property         : constant Glib.Properties.Property_Boolean;
   Background_Stipple_Set_Property : constant Glib.Properties.Property_Boolean;
   Editable_Set_Property           : constant Glib.Properties.Property_Boolean;
   Family_Set_Property             : constant Glib.Properties.Property_Boolean;
   Foreground_Set_Property         : constant Glib.Properties.Property_Boolean;
   Foreground_Stipple_Set_Property : constant Glib.Properties.Property_Boolean;
   Indent_Set_Property             : constant Glib.Properties.Property_Boolean;
   Inside_Wrap_Set_Property        : constant Glib.Properties.Property_Boolean;
   Invisible_Set_Property          : constant Glib.Properties.Property_Boolean;
   Justification_Set_Property      : constant Glib.Properties.Property_Boolean;
   Language_Set_Property           : constant Glib.Properties.Property_Boolean;
   Left_Margin_Set_Property        : constant Glib.Properties.Property_Boolean;
   Paragraph_Background_Set_Property : constant
     Glib.Properties.Property_Boolean;
   Pixels_Above_Lines_Set_Property : constant Glib.Properties.Property_Boolean;
   Pixels_Below_Lines_Set_Property : constant Glib.Properties.Property_Boolean;
   Pixels_Inside_Wrap_Set_Property : constant Glib.Properties.Property_Boolean;
   Right_Margin_Set_Property       : constant Glib.Properties.Property_Boolean;
   Rise_Set_Property               : constant Glib.Properties.Property_Boolean;
   Scale_Set_Property              : constant Glib.Properties.Property_Boolean;
   Size_Set_Property               : constant Glib.Properties.Property_Boolean;
   Stretch_Set_Property            : constant Glib.Properties.Property_Boolean;
   Strikethrough_Set_Property      : constant Glib.Properties.Property_Boolean;
   Style_Set_Property              : constant Glib.Properties.Property_Boolean;
   Tabs_Set_Property               : constant Glib.Properties.Property_Boolean;
   Underline_Set_Property          : constant Glib.Properties.Property_Boolean;
   Variant_Set_Property            : constant Glib.Properties.Property_Boolean;
   Weight_Set_Property             : constant Glib.Properties.Property_Boolean;
   Wrap_Mode_Set_Property          : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "event"
   --    function Handler
   --      (Tag          : access Gtk_Text_Tag_Record'Class;
   --       Event_Object : out GObject;
   --       Event        : Gdk.Event.Gdk_Event;
   --       Iter         : access Gtk.Text_Iter.Gtk_Text_Iter_Record'Class)
   --       return Gint;
   --    ???
   --
   --  </signals>

   Signal_Event : constant Glib.Signal_Name := "event";

private
   type Gtk_Text_Tag_Record is new GObject_Record with null record;

   Background_Full_Height_Property : constant Glib.Properties.Property_Boolean
     := Glib.Properties.Build ("background_full_height");
   Direction_Property         : constant Gtk.Enums.Property_Gtk_Text_Direction
     := Gtk.Enums.Build ("direction");
   Name_Property               : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("name");
   Background_Property         : constant Glib.Properties.Property_String_WO :=
     Glib.Properties.Build ("background");
   Background_Gdk_Property     : constant Gdk.Color.Property_Gdk_Color :=
     Gdk.Color.Build ("background_gdk");
   Background_Stipple_Property : constant Glib.Properties.Property_C_Proxy :=
     Glib.Properties.Build ("background_stipple");
   Foreground_Property         : constant Glib.Properties.Property_String_WO :=
     Glib.Properties.Build ("foreground");
   Foreground_Gdk_Property     : constant Gdk.Color.Property_Gdk_Color :=
     Gdk.Color.Build ("foreground_gdk");
   Foreground_Stipple_Property : constant Glib.Properties.Property_C_Proxy :=
     Glib.Properties.Build ("foreground_stipple");
   Editable_Property           : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("editable");
   Font_Property               : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("font");
   Font_Desc_Property        : constant Pango.Font.Property_Font_Description :=
     Pango.Font.Build ("font_desc");
   Family_Property             : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("family");
   Style_Property              : constant Pango.Enums.Property_Style :=
     Pango.Enums.Build ("style");
   Variant_Property            : constant Pango.Enums.Property_Variant :=
     Pango.Enums.Build ("variant");
   Weight_Property             : constant Pango.Enums.Property_Weight :=
     Pango.Enums.Build ("weight");
   Stretch_Property            : constant Pango.Enums.Property_Stretch :=
     Pango.Enums.Build ("stretch");
   Size_Property               : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("size");
   Size_Points_Property        : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("size_points");
   Justification_Property    : constant Gtk.Enums.Property_Gtk_Justification :=
     Gtk.Enums.Build ("justification");
   Language_Property           : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("language");
   Left_Margin_Property        : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("left_margin");
   Right_Margin_Property       : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("right_margin");
   Indent_Property             : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("indent");
   Rise_Property               : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("rise");
   Pixels_Above_Lines_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("pixels_above_lines");
   Pixels_Below_Lines_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("pixels_below_lines");
   Inside_Wrap_Property        : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("inside_wrap");
   Strikethrough_Property     : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("strikethrough");
   Underline_Property          : constant Pango.Enums.Property_Underline :=
     Pango.Enums.Build ("underline");
   Wrap_Mode_Property          : constant Gtk.Enums.Property_Gtk_Wrap_Mode :=
     Gtk.Enums.Build ("wrap_mode");
   --  Tabs_Property               : constant Pango.Types.Property_Tab_Array :=
   --     Pango.Types.Build ("tabs");
   Invisible_Property          : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("invisible");
   Paragraph_Background_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("paragraph-background");
   Pixels_Inside_Wrap_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("pixels-inside-wrap");
   Scale_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("scale");

   Background_Full_Height_Set_Property : constant
     Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("background_full_height_set");
   Background_Set_Property       : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("background_set");
   Background_Stipple_Set_Property : constant Glib.Properties.Property_Boolean
     := Glib.Properties.Build ("background_stipple_set");
   Foreground_Set_Property       : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("foreground_set");
   Foreground_Stipple_Set_Property : constant Glib.Properties.Property_Boolean
     := Glib.Properties.Build ("foreground_stipple_set");
   Editable_Set_Property         : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("editable_set");
   Family_Set_Property           : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("family_set");
   Style_Set_Property            : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("style_set");
   Variant_Set_Property          : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("variant_set");
   Weight_Set_Property           : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("weight_set");
   Stretch_Set_Property          : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("stretch_set");
   Size_Set_Property             : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("size_set");
   Justification_Set_Property    : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("justification_set");
   Language_Set_Property         : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("language_set");
   Left_Margin_Set_Property      : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("left_margin_set");
   Indent_Set_Property           : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("indent_set");
   Rise_Set_Property             : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("rise_set");
   Pixels_Above_Lines_Set_Property : constant Glib.Properties.Property_Boolean
     := Glib.Properties.Build ("pixels_above_lines_set");
   Pixels_Below_Lines_Set_Property : constant Glib.Properties.Property_Boolean
     := Glib.Properties.Build ("pixels_below_lines_set");
   Inside_Wrap_Set_Property      : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("inside_wrap_set");
   Strikethrough_Set_Property   : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("strike_through_set");
   Right_Margin_Set_Property     : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("right_margin_set");
   Underline_Set_Property        : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("underline_set");
   Wrap_Mode_Set_Property        : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("wrap_mode_set");
   Tabs_Set_Property             : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("tabs_set");
   Invisible_Set_Property        : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("invisible_set");
   Paragraph_Background_Set_Property : constant
     Glib.Properties.Property_Boolean :=
       Glib.Properties.Build ("pagraph-background-set");
   Pixels_Inside_Wrap_Set_Property : constant Glib.Properties.Property_Boolean
     := Glib.Properties.Build ("pixels-inside-wrap-set");
   Scale_Set_Property            : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("scale-set");

   --  Paragraph_Background_Gdk_Property : Glib.Properties.Property_Boxed :=
   --    Glib.Properties.Build ("paragraph-background-gdk");


   pragma Import (C, Get_Type, "gtk_text_tag_get_type");
end Gtk.Text_Tag;

--  The following subprograms have a binding in gtk-text_attributes.ads:
--  No binding: gtk_text_attributes_get_type
--  No binding: gtk_text_attributes_ref
--  No binding: gtk_text_attributes_unref
--  No binding: gtk_text_attributes_copy
--  No binding: gtk_text_attributes_copy_values
--  No binding: gtk_text_attributes_new

--  The following subprogram cannot be bound in the package, since it would
--  generate a dependency cycle:
--  No binding: gtk_text_tag_event
