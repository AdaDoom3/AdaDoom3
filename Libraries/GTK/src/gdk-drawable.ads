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
--  This package provides support for drawing points, lines, arcs and text onto
--  what are called 'drawables'. Drawables, as the name suggests, are things
--  which support drawing onto them, and are either Gdk_Window or
--  Gdk_Pixmap objects.
--
--  Many of the drawing operations take a Gdk_GC argument, which represents a
--  graphics context. This Gdk_GC contains a number of drawing attributes such
--  as foreground color, background color and line width, and is used to
--  reduce the number of arguments needed for each drawing operation.
--  See Gdk.GC for more information.
--
--  </description>
--  <c_version>1.3.6</c_version>
--  <group>Gdk, the low-level API</group>

with Glib; use Glib;
with Gdk.Types;
with Pango.Layout;

package Gdk.Drawable is

   subtype Gdk_Drawable is Gdk.Gdk_Drawable;
   --  A screen area that can be drawn upon.

   Null_Drawable : constant Gdk_Drawable;

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Gdk_Drawable.

   procedure Get_Size
     (Drawable : Gdk_Drawable;
      Width    : out Gint;
      Height   : out Gint);
   --  Return the width and height of a given drawable.

   procedure Set_Colormap
     (Drawable : Gdk_Drawable; Colormap : Gdk.Gdk_Colormap);

   function Get_Colormap
     (Drawable : Gdk_Drawable) return Gdk.Gdk_Colormap;

   function Get_Visual (Drawable : Gdk_Drawable) return Gdk.Gdk_Visual;

   function Get_Depth (Drawable : Gdk_Drawable) return Gint;

   procedure Ref (Drawable : Gdk_Drawable);

   procedure Unref (Drawable : Gdk_Drawable);

   procedure Draw_Point
     (Drawable : Gdk_Drawable;
      GC       : Gdk.Gdk_GC;
      X        : Gint;
      Y        : Gint);
   --  Draw a point, using the foreground color and other attributes of the Gc.

   procedure Draw_Line
     (Drawable : Gdk_Drawable;
      GC       : Gdk.Gdk_GC;
      X1       : Gint;
      Y1       : Gint;
      X2       : Gint;
      Y2       : Gint);
   --  Draw a line, using the foreground color and other attributes of the Gc.
   --  (X1, Y1) is coordinate of the start point.
   --  (X2, Y2) is coordinate of the end point.

   procedure Draw_Rectangle
     (Drawable : Gdk_Drawable;
      GC       : Gdk.Gdk_GC;
      Filled   : Boolean := False;
      X        : Gint;
      Y        : Gint;
      Width    : Gint;
      Height   : Gint);
   --  Draw a rectangular outline or filled rectangle.
   --  Note that a rectangle drawn filled is 1 pixel smaller in both dimensions
   --  than a rectangle outlined. Calling
   --  Draw_Rectangle (Window, Gc, True, 0, 0, 20, 20) results in a filled
   --  rectangle 20 pixels wide and 20 pixels high. Calling
   --  Draw_Rectangle (Window, Gc, False, 0, 0, 20, 20) results in an outlined
   --  rectangle with corners at (0, 0), (0, 20), (20, 20), and (20, 0), which
   --  makes it 21 pixels wide and 21 pixels high.
   --
   --  (X, Y) represents the coordinate of the top-left edge of the rectangle.

   procedure Draw_Arc
     (Drawable : Gdk_Drawable;
      GC       : Gdk.Gdk_GC;
      Filled   : Boolean := False;
      X        : Gint;
      Y        : Gint;
      Width    : Gint;
      Height   : Gint;
      Angle1   : Gint;
      Angle2   : Gint);
   --  Draws an arc or a filled 'pie slice'.
   --  The arc is defined by the bounding rectangle of the entire ellipse, and
   --  the start and end angles of the part of the ellipse to be drawn.
   --  Filled is True if the arc should be filled, producing a 'pie slice'.
   --  (X, Y) represent the coordinate of the top-left edge of the bounding
   --  rectangle.
   --  Angle1 is the start angle of the arc, relative to the 3 o'clock
   --  position, counter-clockwise, in 1/64ths of a degree.
   --  Angle2 is the end angle of the arc, relative to angle1, in 1/64ths of a
   --  degree.

   procedure Draw_Polygon
     (Drawable : Gdk_Drawable;
      GC       : Gdk.Gdk_GC;
      Filled   : Boolean;
      Points   : Gdk.Types.Gdk_Points_Array);
   --  Draw an outlined or filled polygon.
   --  Filled is True if the polygon should be filled. The polygon is closed
   --  automatically, connecting the last point to the first point if
   --  necessary.
   --  Points is an array of Gdk_Point specifying the points making up the
   --  polygon.

   procedure Draw_Text
     (Drawable    : Gdk_Drawable;
      Font        : Gdk.Gdk_Font;
      GC          : Gdk.Gdk_GC;
      X           : Gint;
      Y           : Gint;
      Text        : UTF8_String);
   --  Draw a string in the given font or fontset.
   --  X is the x coordinate of the left edge of the text.
   --  Y is the y coordinate of the baseline of the text.
   --
   --  You should use Gtk.Widget.Create_Pango_Layout instead to handle
   --  internationalization.

   procedure Draw_Text
     (Drawable    : Gdk_Drawable;
      Font        : Gdk.Gdk_Font;
      GC          : Gdk.Gdk_GC;
      X           : Gint;
      Y           : Gint;
      Wide_Text   : Gdk.Types.Gdk_WString);
   --  Draw a wide string in the given font of fontset.
   --  If the font is a 1-byte font, the string is converted into 1-byte
   --  characters (discarding the high bytes) before output.

   procedure Draw_Drawable
     (Drawable : Gdk_Drawable;
      GC       : Gdk.Gdk_GC;
      Src      : Gdk_Drawable;
      Xsrc     : Gint;
      Ysrc     : Gint;
      Xdest    : Gint;
      Ydest    : Gint;
      Width    : Gint := -1;
      Height   : Gint := -1);
   --  Draw a pixmap, or a part of a pixmap, onto another drawable.
   --  Src is the source Gdk_Drawable to draw.
   --  Xsrc is the left edge of the source rectangle within Src.
   --  Ysrc is the top of the source rectangle within Src.
   --  Xdest is the x coordinate of the destination within Src.
   --  Ydest is the y coordinate of the destination within Src.
   --  Width is the width of the area to be copied, or -1 to make the area
   --  extend to the right edge of the source pixmap.
   --  Height is the height of the area to be copied, or -1 to make the area
   --  extend to the bottom edge of the source pixmap.

   procedure Draw_Layout
     (Drawable : Gdk_Drawable;
      GC       : Gdk.Gdk_GC;
      X        : Gint;
      Y        : Gint;
      Layout   : Pango.Layout.Pango_Layout);
   --  Display the layout and its text in Drawable. This method should be
   --  preferred over Draw_Text.

   procedure Draw_Pixmap
     (Drawable : Gdk_Drawable;
      GC       : Gdk.Gdk_GC;
      Src      : Gdk_Drawable;
      Xsrc     : Gint;
      Ysrc     : Gint;
      Xdest    : Gint;
      Ydest    : Gint;
      Width    : Gint := -1;
      Height   : Gint := -1) renames Draw_Drawable;
   --  Deprecated, use Draw_Drawable instead.

   procedure Draw_Image
     (Drawable : Gdk_Drawable;
      GC       : Gdk.Gdk_GC;
      Image    : Gdk.Gdk_Image;
      Xsrc     : Gint;
      Ysrc     : Gint;
      Xdest    : Gint;
      Ydest    : Gint;
      Width    : Gint := -1;
      Height   : Gint := -1);
   --  Draw a Gdk_Image onto a Drawable.
   --  The depth of the Gdk_Image must match the depth of the Gdk_Drawable.
   --  Image is the Gdk_Image to draw.
   --  Xsrc is the left edge of the source rectangle within Image.
   --  Ysrc is the top of the source rectangle within Image.
   --  Xdest is the x coordinate of the destination within Drawable.
   --  Ydest is the y coordinate of the destination within Drawable.
   --  Width is the width of the area to be copied, or -1 to make the area
   --  extend to the right edge of image.
   --  Height is the height of the area to be copied, or -1 to make the area
   --  extend to the bottom edge of image.

   procedure Draw_Points
     (Drawable : Gdk_Drawable;
      GC       : Gdk.Gdk_GC;
      Points   : Gdk.Types.Gdk_Points_Array);
   --  Draw a number of points.
   --  Use the foreground color and other attributes of the Gc.

   procedure Draw_Segments
     (Drawable : in Gdk_Drawable;
      GC       : in Gdk.Gdk_GC;
      Segs     : in Gdk.Types.Gdk_Segments_Array);
   --  Draw a number of unconnected lines.

   procedure Draw_Lines
     (Drawable : Gdk_Drawable;
      GC       : Gdk.Gdk_GC;
      Points   : Gdk.Types.Gdk_Points_Array);
   --  Draw a series of lines connecting the given points.
   --  The way in which joins between lines are drawn is determined by the
   --  Cap_Style value in the Gdk_GC. This can be set with
   --  Gdk.Gc.Set_Line_Attributes.

   function Get_Image
     (Drawable : Gdk_Drawable;
      X        : Gint;
      Y        : Gint;
      Width    : Gint;
      Height   : Gint) return Gdk_Image;

   function Get_Clip_Region (Drawable : Gdk_Drawable) return Gdk.Gdk_Region;

   function Get_Visible_Region (Drawable : Gdk_Drawable) return Gdk.Gdk_Region;

private
   Null_Drawable : constant Gdk_Drawable := null;
   pragma Import (C, Get_Type, "gdk_drawable_get_type");
   pragma Import (C, Get_Depth, "gdk_drawable_get_depth");
   pragma Import (C, Ref, "gdk_drawable_ref");
   pragma Import (C, Unref, "gdk_drawable_unref");
   pragma Import (C, Get_Size, "gdk_drawable_get_size");
   pragma Import (C, Get_Colormap, "gdk_drawable_get_colormap");
   pragma Import (C, Get_Visual, "gdk_drawable_get_visual");
   pragma Import (C, Set_Colormap, "gdk_drawable_set_colormap");
   pragma Import (C, Draw_Drawable, "gdk_draw_drawable");
   pragma Import (C, Draw_Line, "gdk_draw_line");
   pragma Import (C, Draw_Point, "gdk_draw_point");
   pragma Import (C, Draw_Image, "gdk_draw_image");
   pragma Import (C, Get_Image, "gdk_drawable_get_image");
   pragma Import (C, Get_Clip_Region, "gdk_drawable_get_clip_region");
   pragma Import (C, Get_Visible_Region, "gdk_drawable_get_visible_region");
end Gdk.Drawable;

--  <example>
--  <include>../examples/documentation/draw.adb</include>
--  </example>

--  missing pango related functions:
--  gdk_draw_glyphs
--  gdk_draw_layout_line
--  gdk_draw_layout_line_with_colors
--  gdk_draw_layout_with_colors
