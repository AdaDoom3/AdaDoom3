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

with Interfaces.C;
with System;
with Glib.Object; use Glib.Object;

package body Gdk.Drawable is

   package C renames Interfaces.C;

   --------------
   -- Draw_Arc --
   --------------

   procedure Draw_Arc
     (Drawable : Gdk_Drawable;
      GC       : Gdk.Gdk_GC;
      Filled   : Boolean := False;
      X        : Gint;
      Y        : Gint;
      Width    : Gint;
      Height   : Gint;
      Angle1   : Gint;
      Angle2   : Gint)
   is
      procedure Internal
        (Drawable : Gdk_Drawable;
         GC       : Gdk.Gdk_GC;
         Filled   : Gboolean;
         X, Y     : Gint;
         Width    : Gint;
         Height   : Gint;
         Angle1   : Gint;
         Angle2   : Gint);
      pragma Import (C, Internal, "gdk_draw_arc");

   begin
      Internal (Drawable, GC, Boolean'Pos (Filled),
                X, Y, Width, Height, Angle1, Angle2);
   end Draw_Arc;

   ----------------
   -- Draw_Lines --
   ----------------

   procedure Draw_Lines
     (Drawable : Gdk_Drawable;
      GC       : Gdk.Gdk_GC;
      Points   : Gdk.Types.Gdk_Points_Array)
   is
      procedure Internal
        (Drawable : Gdk_Drawable;
         GC       : Gdk.Gdk_GC;
         Points   : Gdk.Types.Gdk_Points_Array;
         Npoints  : Gint);
      pragma Import (C, Internal, "gdk_draw_lines");

   begin
      Internal (Drawable, GC, Points, Points'Length);
   end Draw_Lines;

   -----------------
   -- Draw_Points --
   -----------------

   procedure Draw_Points
     (Drawable : Gdk_Drawable;
      GC       : Gdk.Gdk_GC;
      Points   : Gdk.Types.Gdk_Points_Array)
   is
      procedure Internal
        (Drawable : Gdk_Drawable;
         GC       : Gdk.Gdk_GC;
         Points   : Gdk.Types.Gdk_Points_Array;
         Npoints  : Gint);
      pragma Import (C, Internal, "gdk_draw_points");

   begin
      Internal (Drawable, GC, Points, Points'Length);
   end Draw_Points;

   ------------------
   -- Draw_Polygon --
   ------------------

   procedure Draw_Polygon
     (Drawable : Gdk_Drawable;
      GC       : Gdk.Gdk_GC;
      Filled   : Boolean;
      Points   : Gdk.Types.Gdk_Points_Array)
   is
      procedure Internal
        (Drawable : Gdk_Drawable;
         GC       : Gdk.Gdk_GC;
         Filled   : Gboolean;
         Points   : Gdk.Types.Gdk_Points_Array;
         Npoints  : Gint);
      pragma Import (C, Internal, "gdk_draw_polygon");

   begin
      Internal (Drawable, GC, Boolean'Pos (Filled), Points, Points'Length);
   end Draw_Polygon;

   --------------------
   -- Draw_Rectangle --
   --------------------

   procedure Draw_Rectangle
     (Drawable : Gdk_Drawable;
      GC       : Gdk.Gdk_GC;
      Filled   : Boolean := False;
      X, Y     : Gint;
      Width    : Gint;
      Height   : Gint)
   is
      procedure Internal
        (Drawable : Gdk_Drawable;
         GC       : Gdk.Gdk_GC;
         Filled   : Gboolean;
         X, Y     : Gint;
         Width    : Gint;
         Height   : Gint);
      pragma Import (C, Internal, "gdk_draw_rectangle");

   begin
      Internal (Drawable, GC, Boolean'Pos (Filled), X, Y, Width, Height);
   end Draw_Rectangle;

   -------------------
   -- Draw_Segments --
   -------------------

   procedure Draw_Segments
     (Drawable : Gdk.Drawable.Gdk_Drawable;
      GC       : Gdk.Gdk_GC;
      Segs     : Gdk.Types.Gdk_Segments_Array)
   is
      procedure Internal
        (Drawable : Gdk.Drawable.Gdk_Drawable;
         GC       : Gdk.Gdk_GC;
         Segs     : Gdk.Types.Gdk_Segments_Array;
         Nsegs    : Gint);
      pragma Import (C, Internal, "gdk_draw_segments");

   begin
      Internal (Drawable, GC, Segs, Segs'Length);
   end Draw_Segments;

   ---------------
   -- Draw_Text --
   ---------------

   procedure Draw_Text
     (Drawable : Gdk_Drawable;
      Font     : Gdk.Gdk_Font;
      GC       : Gdk.Gdk_GC;
      X        : Gint;
      Y        : Gint;
      Text     : UTF8_String)
   is
      procedure Internal
        (Drawable    : Gdk_Drawable;
         Font        : Gdk.Gdk_Font;
         GC          : Gdk.Gdk_GC;
         X           : Gint;
         Y           : Gint;
         Text        : UTF8_String;
         Text_Length : Gint);
      pragma Import (C, Internal, "gdk_draw_text");

   begin
      Internal (Drawable, Font, GC, X, Y, Text, Text'Length);
   end Draw_Text;

   procedure Draw_Text
     (Drawable  : Gdk_Drawable;
      Font      : Gdk.Gdk_Font;
      GC        : Gdk.Gdk_GC;
      X         : Gint;
      Y         : Gint;
      Wide_Text : Gdk.Types.Gdk_WString)
   is
      procedure Internal
        (Drawable    : Gdk_Drawable;
         Font        : Gdk.Gdk_Font;
         GC          : Gdk.Gdk_GC;
         X           : Gint;
         Y           : Gint;
         Text        : C.wchar_array;
         Text_Length : Gint);
      pragma Import (C, Internal, "gdk_draw_text_wc");

   begin
      Internal
        (Drawable, Font, GC, X, Y,
         C.To_C (Item => Wide_Text, Append_Nul => False),
         Wide_Text'Length);
   end Draw_Text;

   -----------------
   -- Draw_Layout --
   -----------------

   procedure Draw_Layout
     (Drawable : Gdk_Drawable;
      GC       : Gdk.Gdk_GC;
      X        : Gint;
      Y        : Gint;
      Layout   : Pango.Layout.Pango_Layout)
   is
      procedure Internal
        (Drawable : Gdk_Drawable;
         GC       : Gdk.Gdk_GC;
         X, Y     : Gint;
         Layout   : System.Address);
      pragma Import (C, Internal, "gdk_draw_layout");
   begin
      Internal (Drawable, GC, X, Y, Get_Object (Layout));
   end Draw_Layout;

end Gdk.Drawable;
