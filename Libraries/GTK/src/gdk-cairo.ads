-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                 Copyright (C) 2010-2013, AdaCore                  --
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
--  Interfacing between Gdk and Cairo.
--  </description>
--
--  <c_version>2.16.6</c_version>
--  <group>Cairo</group>

with Glib;         use Glib;
with Cairo;        use Cairo;
with Gdk.Color;    use Gdk.Color;
with Gdk.Pixbuf;   use Gdk.Pixbuf;

package Gdk.Cairo is

   function Create (Drawable : Gdk_Drawable) return Cairo_Context;
   --  Creates a Cairo context for drawing to Drawable.
   --
   --  Note that due to double-buffering, Cairo contexts created
   --  in a GTK+ expose event handler cannot be cached and reused
   --  between different expose events.
   --
   --  Returns a newly created Cairo context. The result should be freed with
   --  Cairo.Destroy.

   procedure Set_Source_Pixmap
     (Cr       : Cairo_Context;
      Pixmap   : Gdk_Drawable;
      Pixmap_X : Gdouble;
      Pixmap_Y : Gdouble);
   --  Cr: a Cairo_Context
   --  Pixmap: a Gdk_Pixmap
   --  Pixmap_X: X coordinate of location to place upper left corner of Pixmap
   --  Pixmap_Y: Y coordinate of location to place upper left corner of Pixmap
   --
   --  Sets the given pixmap as the source pattern for the Cairo context.
   --  The pattern has an extend mode of CAIRO_EXTEND_NONE and is aligned
   --  so that the origin of Pixmap is Pixmap_X, Pixmap_Y

   procedure Set_Source_Pixbuf
     (Cr       : Cairo_Context;
      Pixbuf   : Gdk_Pixbuf;
      Pixbuf_X : Gdouble;
      Pixbuf_Y : Gdouble);
   --  Cr: a Cairo_Context
   --  Pixbuf: a Gdk_Pixbuf
   --  Pixbuf_X: X coordinate of location to place upper left corner of Pixbuf
   --  Pixbuf_Y: Y coordinate of location to place upper left corner of Pixbuf
   --
   --  Sets the given pixbuf as the source pattern for the Cairo context.
   --  The pattern has an extend mode of CAIRO_EXTEND_NONE and is aligned
   --  so that the origin of Pixbuf is Pixbuf_X, Pixbuf_Y

   procedure Set_Source_Color
     (Cr       : Cairo_Context;
      Color    : Gdk_Color);
   --  Set the specified Color as the source of Cr.

private
   pragma Import (C, Set_Source_Pixmap, "gdk_cairo_set_source_pixmap");
   pragma Import (C, Set_Source_Color, "gdk_cairo_set_source_color");
end Gdk.Cairo;
