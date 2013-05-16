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

package body Gdk.Image is

   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New
     (Image      : out Gdk_Image;
      Image_Type : Gdk_Image_Type;
      Visual     : Gdk.Gdk_Visual;
      Width      : Gint;
      Height     : Gint)
   is
      function Internal
        (Image_Type : Gdk_Image_Type;
         Visual     : Gdk.Gdk_Visual;
         Width      : Gint;
         Height     : Gint) return Gdk_Image;
      pragma Import (C, Internal, "gdk_image_new");

   begin
      Image := Internal (Image_Type, Visual, Width, Height);
   end Gdk_New;

   ---------
   -- Get --
   ---------

   procedure Get
     (Image    : out Gdk_Image;
      Drawable : Gdk.Gdk_Drawable;
      X        : Gint;
      Y        : Gint;
      Width    : Gint;
      Height   : Gint)
   is
      function Internal
        (Drawable : Gdk.Gdk_Drawable;
         X        : Gint;
         Y        : Gint;
         Width    : Gint;
         Height   : Gint) return Gdk_Image;
      pragma Import (C, Internal, "gdk_image_get");

   begin
      Image := Internal (Drawable, X, Y, Width, Height);
   end Get;

end Gdk.Image;
