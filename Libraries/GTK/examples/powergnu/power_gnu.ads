-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 2000                            --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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

with Gdk.Pixbuf;
with Gtk.Drawing_Area;

package Power_GNU is

   --------------------------
   --  The type below is a special drawing area that displays the
   --  associated image in it automatically, and destroys the image
   --  when the widget is destroyed.
   ---------------------------

   type Image_Drawing_Record is new Gtk.Drawing_Area.Gtk_Drawing_Area_Record
     with private;
   type Image_Drawing is access all Image_Drawing_Record'Class;
   --  A special type of drawing area that can be associated with
   --  an image.

   procedure Gtk_New
     (Draw : out Image_Drawing);
   --  Create a new Image

   procedure Initialize
     (Draw : access Image_Drawing_Record'Class);

   procedure Set_Image
     (Draw  : Image_Drawing;
      Image : String);
   --  Set the current image to display in Draw
   --  Image is the file name of the image
   --  The image will be resized to the current size of Draw

private
   type Image_Drawing_Record is new Gtk.Drawing_Area.Gtk_Drawing_Area_Record
   with record
      Orig : Gdk.Pixbuf.Gdk_Pixbuf;  --  The image loaded from disk
      Pix  : Gdk.Pixbuf.Gdk_Pixbuf;  --  The currently displayed (scaled) image
   end record;
end Power_GNU;
