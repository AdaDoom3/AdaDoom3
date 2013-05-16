-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                Copyright (C) 2002-2005 AdaCore                    --
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
--
--  This package provides a high-level object that is capable of arranging text
--  in a visually correct manner. It supports international character sets,
--  although all strings should be Utf8, supports left-to-right and
--  right-to-left writing systems, is capable of handling multi-line texts, and
--  properly aligns tab characters in the text.
--
--  This is the base type that is used in the standard gtk+ widget for all the
--  widgets that display some text on the screen.
--
--  Since it works directly with Pango.Font.Pango_Font_Description fonts, it is
--  also much better at handling resizing of text, wrapping,... than direct
--  calls to Gdk.Drawable.Draw_Text.
--
--  The idea is that this widget is used to compute the layout of the
--  characters (ie their screen position). It doesn't do any rendering,
--  however, and should be used in conjonction with Gdk.Drawable.Draw_Layout to
--  actually display something on the screen.
--
--  This widget is independent from any specific drawing systems, and might for
--  instance be used to create postscript documents, for direct access to the
--  win32 API,...
--
--  This widget represents one of the fundamental additions to gtk+ 2.0 over
--  what previously existed in the gtk+ 1.x series. It obsoletes the package
--  Gdk.Font, which should only be used for legacy applications.
--
--  </description>
--  <group>Pango, font handling</group>

with Glib;
with Pango.Attributes;
with Pango.Context;
with Pango.Font;
with Pango.Tabs;
with Glib.Object;
with Gdk.Rectangle;
with Gtkada.Types;

package Pango.Layout is

   type Pango_Layout_Record is new Glib.Object.GObject_Record with private;
   type Pango_Layout is access all Pango_Layout_Record'Class;
   --  A widget that allows you to easily display text, including handling of
   --  internationalization, right-to-left writing,...
   --  See the function Gtk.Widget.Create_Pango_Layout for more information on
   --  how to create layouts
   --  Use Glib.Object.Unref to destroy a Pango_Layout
   --  See Gdk.Drawable.Draw_Layout for how to actually display the layout on
   --  the screen.

   type Pango_Alignment is
     (Pango_Align_Left,
      Pango_Align_Center,
      Pango_Align_Right);
   pragma Convention (C, Pango_Alignment);
   --  The alignment for each line of the layout

   type Pango_Wrap_Mode is
     (Pango_Wrap_Word,
      Pango_Wrap_Char);
   pragma Convention (C, Pango_Wrap_Mode);
   --  The wrap mode for a layout

   type Pango_Ellipsize_Mode is
     (Ellipsize_None,
      Ellipsize_Start,
      Ellipsize_Middle,
      Ellipsize_End);
   --  This type describes what sort of (if any) ellipsization should be
   --  applied to a line of text. In the ellipsization process characters are
   --  removed from the text in order to make it fit to a given width and
   --  replaced with an ellipsis.


   -----------------------
   -- Creating a layout --
   -----------------------
   --  A layout can be created in two ways: either from a widget
   --  (Gtk.Widget.Create_Pango_Layout), from which it will inherit the font
   --  and various other attributes, or directly from a Pango_Context.

   procedure Gdk_New
     (Layout : out Pango_Layout;
      Context : access Pango.Context.Pango_Context_Record'Class);
   --  Create a new layout, based on context.

   --------------
   -- Contexts --
   --------------

   function Get_Context (Layout : access Pango_Layout_Record)
      return Pango.Context.Pango_Context;
   --  Return the context of the layout. The returned value is the internal
   --  context itself, so you must Glib.Object.Ref it if you need to keep a
   --  reference. You shouldn't Unref it.

   procedure Set_Font_Description
     (Layout : access Pango_Layout_Record;
      Font   : Pango.Font.Pango_Font_Description);
   --  Change the font used in the layout.
   --  If not font description is set for the layout, the font description from
   --  the layout's context is used.

   procedure Context_Changed (Layout : access Pango_Layout_Record);
   --  Forces recomputation of any state in Layout that might depend
   --  on the layout's context. This function should be called if you make
   --  changes to the context subsequent to creating the layout.

   -----------
   -- Lines --
   -----------

   type Pango_Layout_Line is new Glib.C_Proxy;

   function Get_Line
     (Layout : access Pango_Layout_Record;
      Line   : Natural) return Pango_Layout_Line;
   --  Retrieve a particular line from Layout.
   --  Line must be between 0 and Get_Line_Count (Layout) - 1. null is returned
   --  if the index is out of range.
   --  The layout line can be Ref'ed and retained, but will become invalid if
   --  changes are made to Layout.

   procedure Line_Ref (Line : Pango_Layout_Line);
   --  Increase the reference count of Line by 1.

   procedure Line_Unref (Line : Pango_Layout_Line);
   --  Decrease the reference count of Line by 1. If the result is 0, the line
   --  and all associated memory will be destroyed.

   function Line_Index_To_X
     (Line     : Pango_Layout_Line;
      Index    : Integer;
      Trailing : Integer) return Glib.Gint;
   --  Convert an index within a line to an X position.
   --  Index is the byte offset of a graphem within the layout.
   --  Trailing is an integer indicating the edge of the grapheme to retrieve
   --  the position of. If 0, the trailing edge of the grapheme, otherwise, the
   --  leading of the grapheme.
   --  The returned value is in pango units.

   ----------------------
   -- Getting the size --
   ----------------------
   --  Pango internally stores its sizes in pango units, which are a number of
   --  pixels (device units, when not drawing on the screen) multiplied by
   --  Pango_Scale. There are generally equivalent subprograms to get the sizes
   --  directly in pixels.

   procedure Get_Extents
     (Layout       : access Pango_Layout_Record;
      Ink_Rect     : out Gdk.Rectangle.Gdk_Rectangle;
      Logical_Rect : out Gdk.Rectangle.Gdk_Rectangle);
   --  Compute the logical and ink extents of Layout. Logical extents
   --  are usually what you want for positioning things. The extents
   --  are given in pango units; layout coordinates begin at the
   --  top left corner of the layout.
   --  Logical_Rect is the overall size of the layout, ie it includes Ink_Rect
   --  (where the text is actually drawn) and a small border around it.

   procedure Get_Size
     (Layout : access Pango_Layout_Record;
      Width  : out Glib.Gint;
      Height : out Glib.Gint);
   --  Return the logical size, in pango units, of the layout. This is a
   --  convenience function around Get_Extents.

   procedure Get_Pixel_Extents
     (Layout       : access Pango_Layout_Record;
      Ink_Rect     : out Gdk.Rectangle.Gdk_Rectangle;
      Logical_Rect : out Gdk.Rectangle.Gdk_Rectangle);
   --  Same as Get_Extents, but the returned values are in pixels (or device
   --  units when not drawing on the screen).

   procedure Get_Pixel_Size
     (Layout : access Pango_Layout_Record;
      Width  : out Glib.Gint;
      Height : out Glib.Gint);
   --  Same as Get_Size, but the returned values are in pixels.

   procedure XY_To_Index
     (Layout           : access Pango_Layout_Record;
      X_Pango, Y_Pango : Glib.Gint;
      Byte_Index       : out Integer;
      Trailing         : out Integer;
      Exact            : out Boolean);
   --  Convert from X and Y positions within a layout to the byte index of the
   --  character at that logical position.
   --  X and Y are given in pango units, not pixels.
   --  If the position is not inside the layout, the closest position is
   --  chosen, and Exact is set to False.
   --  Trailing is the position in the grapheme where the user clicked. It will
   --  either be 0 (left side) or the number of characters in the grapheme. In
   --  some character sets, a given character can be represented by multiple
   --  signs on the screen, which is what Trailing relates to.

   ---------------------------
   -- Manipulating the text --
   ---------------------------
   --  When initially created with Gtk.Widget.Create_Pango_Layout, the layout
   --  contains some text. Of course, this text may also be changed later in
   --  the life of the layout.

   procedure Set_Text (Layout : access Pango_Layout_Record; Text : String);
   --  Change the text that the layout displays
   --  Text must be a valid UTF8 string. See Glib.Convert for useful conversion
   --  functions.

   function Get_Text (Layout : access Pango_Layout_Record) return String;
   --  Return the text currently displayed in the layout.
   --  It is more efficient to use the iterators on the layout than get text if
   --  you do not need Ada-specific subprograms to act on the text.

   function Get_Text (Layout : access Pango_Layout_Record)
      return Gtkada.Types.Chars_Ptr;
   --  Same a Get_Text, but return directly the C string, which is more
   --  efficient. The returned value should not be freed or modified.

   function Get_Line_Count (Layout : access Pango_Layout_Record)
      return Glib.Gint;
   --  Return the number of lines in Layout

   procedure Set_Markup
     (Layout : access Pango_Layout_Record;
      Markup : Glib.UTF8_String);
   --  Change the text that layout displays.
   --  Markup must be a valid UTF8 String, and might contain markups as
   --  defined in the pango markup language.

   ------------------------
   -- Layouting the text --
   ------------------------

   procedure Set_Justify
     (Layout : access Pango_Layout_Record; Justify : Boolean);
   --  Set whether or not each complete line should be stretched to fill the
   --  entire width of the layout. This stretching is typically done by adding
   --  whitespace, but for some scripts (such as Arabic), the justification is
   --  done by extending the characters.

   function Get_Justify (Layout : access Pango_Layout_Record) return Boolean;
   --  Return True if each complete line should be stretched to fill the entire
   --  width of the layout.

   procedure Set_Alignment
     (Layout    : access Pango_Layout_Record'Class;
      Alignment : Pango_Alignment);
   --  Set the alignment for the layout (how partial lines are positioned
   --  within the horizontal space available).

   function Get_Alignment (Layout : access Pango_Layout_Record)
     return Pango_Alignment;
   --  Return the alignment for the layout.

   procedure Set_Width
     (Layout : access Pango_Layout_Record; Width : Glib.Gint);
   --  Set the width to which the lines of Layout should be wrapped. No
   --  wrapping will be performed if Width is -1.
   --  Width is given in pango units.

   function Get_Width (Layout : access Pango_Layout_Record) return Glib.Gint;
   --  Return the wrapping width of Layout

   procedure Set_Wrap
     (Layout : access Pango_Layout_Record; Mode : Pango_Wrap_Mode);
   --  Sets the wrap style; the wrap style only has an effect if a width is set
   --  on the layout with pango_layout_set_width(). To turn off wrapping, set
   --  the width to -1.

   function Get_Wrap (Layout : access Pango_Layout_Record)
      return Pango_Wrap_Mode;
   --  Return the current wrap style

   procedure Set_Tabs
     (Layout : access Pango_Layout_Record;
      Tabs   : Pango.Tabs.Pango_Tab_Array);
   --  Sets the tabs to use for Layout, overriding the default tabs (by
   --  default, tabs are every 8 spaces). If Tabs is Null_Tab_Array, the
   --  default tabs are reinstated. tabs is copied into the layout; you must
   --  free your copy of Tabs yourself.

   function Get_Tabs
     (Layout : access Pango_Layout_Record) return Pango.Tabs.Pango_Tab_Array;
   --  Get the current tab array used by Layout. If no tab array
   --  has been set, then the default tabs are in use and Null_Tab_Array is
   --  returned.
   --  Default tabs are every 8 spaces. The return value should be freed with
   --  Pango.Tabs.Free.

   ----------------
   -- Attributes --
   ----------------

   procedure Set_Attributes
     (Layout : access Pango_Layout_Record;
      Attributes : Pango.Attributes.Pango_Attr_List);
   --  Set the text attributes for a layout object.
   --  Passing null removes the current list of attributes

   function Get_Attributes (Layout : access Pango_Layout_Record)
      return Pango.Attributes.Pango_Attr_List;
   --  Get the text attributes from a layout object

private
   type Pango_Layout_Record is new Glib.Object.GObject_Record with null record;

   pragma Import (C, Line_Ref, "pango_layout_line_ref");
   pragma Import (C, Line_Unref, "pango_layout_line_unref");
   pragma Import (C, Line_Index_To_X, "pango_layout_line_index_to_x");
end Pango.Layout;

--  missing:
--  pango_layout_get_context
--  pango_layout_copy
--  pango_layout_set_attributes
--  pango_layout_get_attributes
--  pango_layout_set_markup_with_accel
--  pango_layout_set_indent
--  pango_layout_get_indent
--  pango_layout_set_spacing
--  pango_layout_get_spacing
--  pango_layout_set_single_paragraph_mode
--  pango_layout_get_single_paragraph_mode
--  pango_layout_get_log_attrs
--  pango_layout_index_to_pos
--  pango_layout_get_cursor_pos
--  pango_layout_move_cursor_visually
--  pango_layout_get_lines
--  pango_layout_line_index_to_x
--  pango_layout_line_get_x_ranges
--  pango_layout_line_get_extents
--  pango_layout_line_get_pixel_extents
--  pango_layout_get_iter
--  pango_layout_iter_free
--  pango_layout_iter_get_index
--  pango_layout_iter_get_run
--  pango_layout_iter_get_line
--  pango_layout_iter_at_last_line
--  pango_layout_iter_next_char
--  pango_layout_iter_next_cluster
--  pango_layout_iter_next_run
--  pango_layout_iter_next_line
--  pango_layout_iter_get_char_extents
--  pango_layout_iter_get_cluster_extents
--  pango_layout_iter_get_run_extents
--  pango_layout_iter_get_line_extents
--  pango_layout_iter_get_line_yrange
--  pango_layout_iter_get_layout_extents
--  pango_layout_iter_get_baseline
