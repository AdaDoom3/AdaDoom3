-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
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

package body Gdk.Bitmap is

   ----------------------
   -- Create_From_Data --
   ----------------------

   procedure Create_From_Data
     (Bitmap : out Gdk_Bitmap;
      Window : Gdk.Window.Gdk_Window;
      Data   : String;
      Width  : Gint;
      Height : Gint)
   is
      function Internal
        (Window : Gdk.Window.Gdk_Window;
         Data   : String;
         Width  : Gint;
         Height : Gint) return Gdk_Bitmap;
      pragma Import (C, Internal, "gdk_bitmap_create_from_data");

   begin
      Bitmap := Internal (Window, Data & ASCII.NUL, Width, Height);
   end Create_From_Data;

   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New
     (Bitmap : out Gdk_Bitmap;
      Window : Gdk.Window.Gdk_Window;
      Width  : Gint;
      Height : Gint)
   is
      function Internal
        (Window : Gdk.Window.Gdk_Window;
         Width  : Gint;
         Height : Gint;
         Depth  : Gint) return Gdk_Bitmap;
      pragma Import (C, Internal, "gdk_pixmap_new");

   begin
      Bitmap := Internal (Window, Width, Height, 1);
   end Gdk_New;

end Gdk.Bitmap;
