-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2007 AdaCore                    --
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

with System;

package body Gdk.Rgb is

   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New (Cmap : out Gdk_Rgb_Cmap; Colors : Glib.Guint32_Array) is
      function Internal
        (Colors : System.Address; N_Colors : Integer) return Gdk_Rgb_Cmap;
      pragma Import (C, Internal, "gdk_rgb_cmap_new");

   begin
      Cmap := Internal (Colors'Address, Colors'Length);
   end Gdk_New;

   --------------------
   -- Draw_Rgb_Image --
   --------------------

   procedure Draw_Rgb_Image
     (Drawable      : Gdk.Drawable.Gdk_Drawable;
      GC            : Gdk.GC.Gdk_GC;
      X, Y          : Glib.Gint;
      Width, Height : Glib.Gint;
      Dith          : Gdk_Rgb_Dither;
      Rgb_Buf       : Rgb_Buffer;
      Rowstride     : Glib.Gint)
   is
      procedure Internal
        (Drawable      : Gdk.Drawable.Gdk_Drawable;
         GC            : Gdk.GC.Gdk_GC;
         X, Y          : Glib.Gint;
         Width, Height : Glib.Gint;
         Dith          : Gdk_Rgb_Dither;
         Rgb_Buf       : System.Address;
         Rowstride     : Glib.Gint);
      pragma Import (C, Internal, "gdk_draw_rgb_image");

   begin
      Internal
        (Drawable, GC, X, Y, Width, Height, Dith, Rgb_Buf'Address, Rowstride);
   end Draw_Rgb_Image;

   procedure Draw_Rgb_Image
     (Drawable      : Gdk.Drawable.Gdk_Drawable;
      GC            : Gdk.GC.Gdk_GC;
      X, Y          : Glib.Gint;
      Width, Height : Glib.Gint;
      Dith          : Gdk_Rgb_Dither;
      Rgb_Buf       : Unchecked_Rgb_Buffer;
      Rowstride     : Glib.Gint)
   is
      procedure Internal
        (Drawable      : Gdk.Drawable.Gdk_Drawable;
         GC            : Gdk.GC.Gdk_GC;
         X, Y          : Glib.Gint;
         Width, Height : Glib.Gint;
         Dith          : Gdk_Rgb_Dither;
         Rgb_Buf       : System.Address;
         Rowstride     : Glib.Gint);
      pragma Import (C, Internal, "gdk_draw_rgb_image");

   begin
      Internal
        (Drawable, GC, X, Y, Width, Height, Dith, Rgb_Buf'Address, Rowstride);
   end Draw_Rgb_Image;

   ------------------------------
   -- Draw_Rgb_Image_Dithalign --
   ------------------------------

   procedure Draw_Rgb_Image_Dithalign
     (Drawable      : Gdk.Drawable.Gdk_Drawable;
      GC            : Gdk.GC.Gdk_GC;
      X, Y          : Glib.Gint;
      Width, Height : Glib.Gint;
      Dith          : Gdk_Rgb_Dither;
      Rgb_Buf       : Rgb_Buffer;
      Rowstride     : Glib.Gint;
      Xdith, Ydith  : Glib.Gint)
   is
      procedure Internal
        (Drawable      : Gdk.Drawable.Gdk_Drawable;
         GC            : Gdk.GC.Gdk_GC;
         X, Y          : Glib.Gint;
         Width, Height : Glib.Gint;
         Dith          : Gdk_Rgb_Dither;
         Rgb_Buf       : System.Address;
         Rowstride     : Glib.Gint;
         Xdith, Ydith  : Glib.Gint);
      pragma Import (C, Internal, "gdk_draw_rgb_image_dithalign");

   begin
      Internal
        (Drawable, GC, X, Y, Width, Height, Dith,
         Rgb_Buf'Address, Rowstride, Xdith, Ydith);
   end Draw_Rgb_Image_Dithalign;

   procedure Draw_Rgb_Image_Dithalign
     (Drawable      : Gdk.Drawable.Gdk_Drawable;
      GC            : Gdk.GC.Gdk_GC;
      X, Y          : Glib.Gint;
      Width, Height : Glib.Gint;
      Dith          : Gdk_Rgb_Dither;
      Rgb_Buf       : Unchecked_Rgb_Buffer;
      Rowstride     : Glib.Gint;
      Xdith, Ydith  : Glib.Gint)
   is
      procedure Internal
        (Drawable      : Gdk.Drawable.Gdk_Drawable;
         GC            : Gdk.GC.Gdk_GC;
         X, Y          : Glib.Gint;
         Width, Height : Glib.Gint;
         Dith          : Gdk_Rgb_Dither;
         Rgb_Buf       : System.Address;
         Rowstride     : Glib.Gint;
         Xdith, Ydith  : Glib.Gint);
      pragma Import (C, Internal, "gdk_draw_rgb_image_dithalign");

   begin
      Internal
        (Drawable, GC, X, Y, Width, Height, Dith,
         Rgb_Buf'Address, Rowstride, Xdith, Ydith);
   end Draw_Rgb_Image_Dithalign;

   -----------------------
   -- Draw_Rgb_32_Image --
   -----------------------

   procedure Draw_Rgb_32_Image
     (Drawable      : Gdk.Drawable.Gdk_Drawable;
      GC            : Gdk.GC.Gdk_GC;
      X, Y          : Glib.Gint;
      Width, Height : Glib.Gint;
      Dith          : Gdk_Rgb_Dither;
      Rgb_Buf       : Rgb_Buffer;
      Rowstride     : Glib.Gint)
   is
      procedure Internal
        (Drawable      : Gdk.Drawable.Gdk_Drawable;
         GC            : Gdk.GC.Gdk_GC;
         X, Y          : Glib.Gint;
         Width, Height : Glib.Gint;
         Dith          : Gdk_Rgb_Dither;
         Rgb_Buf       : System.Address;
         Rowstride     : Glib.Gint);
      pragma Import (C, Internal, "gdk_draw_rgb_32_image");

   begin
      Internal
        (Drawable, GC, X, Y, Width, Height, Dith, Rgb_Buf'Address, Rowstride);
   end Draw_Rgb_32_Image;

   procedure Draw_Rgb_32_Image
     (Drawable      : Gdk.Drawable.Gdk_Drawable;
      GC            : Gdk.GC.Gdk_GC;
      X, Y          : Glib.Gint;
      Width, Height : Glib.Gint;
      Dith          : Gdk_Rgb_Dither;
      Rgb_Buf       : Unchecked_Rgb_Buffer;
      Rowstride     : Glib.Gint)
   is
      procedure Internal
        (Drawable      : Gdk.Drawable.Gdk_Drawable;
         GC            : Gdk.GC.Gdk_GC;
         X, Y          : Glib.Gint;
         Width, Height : Glib.Gint;
         Dith          : Gdk_Rgb_Dither;
         Rgb_Buf       : System.Address;
         Rowstride     : Glib.Gint);
      pragma Import (C, Internal, "gdk_draw_rgb_32_image");

   begin
      Internal
        (Drawable, GC, X, Y, Width, Height, Dith, Rgb_Buf'Address, Rowstride);
   end Draw_Rgb_32_Image;

   ---------------------------------
   -- Draw_Rgb_32_Image_Dithalign --
   ---------------------------------

   procedure Draw_Rgb_32_Image_Dithalign
     (Drawable      : Gdk.Drawable.Gdk_Drawable;
      GC            : Gdk.GC.Gdk_GC;
      X, Y          : Glib.Gint;
      Width, Height : Glib.Gint;
      Dith          : Gdk_Rgb_Dither;
      Rgb_Buf       : Rgb_Buffer;
      Rowstride     : Glib.Gint;
      Xdith, Ydith  : Glib.Gint)
   is
      procedure Internal
        (Drawable      : Gdk.Drawable.Gdk_Drawable;
         GC            : Gdk.GC.Gdk_GC;
         X, Y          : Glib.Gint;
         Width, Height : Glib.Gint;
         Dith          : Gdk_Rgb_Dither;
         Rgb_Buf       : System.Address;
         Rowstride     : Glib.Gint;
         Xdith, Ydith  : Glib.Gint);
      pragma Import (C, Internal, "gdk_draw_rgb_32_image_dithalign");

   begin
      Internal
        (Drawable, GC, X, Y, Width, Height, Dith,
         Rgb_Buf'Address, Rowstride, Xdith, Ydith);
   end Draw_Rgb_32_Image_Dithalign;

   procedure Draw_Rgb_32_Image_Dithalign
     (Drawable      : Gdk.Drawable.Gdk_Drawable;
      GC            : Gdk.GC.Gdk_GC;
      X, Y          : Glib.Gint;
      Width, Height : Glib.Gint;
      Dith          : Gdk_Rgb_Dither;
      Rgb_Buf       : Unchecked_Rgb_Buffer;
      Rowstride     : Glib.Gint;
      Xdith, Ydith  : Glib.Gint)
   is
      procedure Internal
        (Drawable      : Gdk.Drawable.Gdk_Drawable;
         GC            : Gdk.GC.Gdk_GC;
         X, Y          : Glib.Gint;
         Width, Height : Glib.Gint;
         Dith          : Gdk_Rgb_Dither;
         Rgb_Buf       : System.Address;
         Rowstride     : Glib.Gint;
         Xdith, Ydith  : Glib.Gint);
      pragma Import (C, Internal, "gdk_draw_rgb_32_image_dithalign");

   begin
      Internal
        (Drawable, GC, X, Y, Width, Height, Dith,
         Rgb_Buf'Address, Rowstride, Xdith, Ydith);
   end Draw_Rgb_32_Image_Dithalign;

   ---------------------
   -- Draw_Gray_Image --
   ---------------------

   procedure Draw_Gray_Image
     (Drawable      : Gdk.Drawable.Gdk_Drawable;
      GC            : Gdk.GC.Gdk_GC;
      X, Y          : Glib.Gint;
      Width, Height : Glib.Gint;
      Dith          : Gdk_Rgb_Dither;
      Rgb_Buf       : Rgb_Buffer;
      Rowstride     : Glib.Gint)
   is
      procedure Internal
        (Drawable      : Gdk.Drawable.Gdk_Drawable;
         GC            : Gdk.GC.Gdk_GC;
         X, Y          : Glib.Gint;
         Width, Height : Glib.Gint;
         Dith          : Gdk_Rgb_Dither;
         Rgb_Buf       : System.Address;
         Rowstride     : Glib.Gint);
      pragma Import (C, Internal, "gdk_draw_gray_image");

   begin
      Internal
        (Drawable, GC, X, Y, Width, Height, Dith, Rgb_Buf'Address, Rowstride);
   end Draw_Gray_Image;

   procedure Draw_Gray_Image
     (Drawable      : Gdk.Drawable.Gdk_Drawable;
      GC            : Gdk.GC.Gdk_GC;
      X, Y          : Glib.Gint;
      Width, Height : Glib.Gint;
      Dith          : Gdk_Rgb_Dither;
      Rgb_Buf       : Unchecked_Rgb_Buffer;
      Rowstride     : Glib.Gint)
   is
      procedure Internal
        (Drawable      : Gdk.Drawable.Gdk_Drawable;
         GC            : Gdk.GC.Gdk_GC;
         X, Y          : Glib.Gint;
         Width, Height : Glib.Gint;
         Dith          : Gdk_Rgb_Dither;
         Rgb_Buf       : System.Address;
         Rowstride     : Glib.Gint);
      pragma Import (C, Internal, "gdk_draw_gray_image");

   begin
      Internal
        (Drawable, GC, X, Y, Width, Height, Dith, Rgb_Buf'Address, Rowstride);
   end Draw_Gray_Image;

   ------------------------
   -- Draw_Indexed_Image --
   ------------------------

   procedure Draw_Indexed_Image
     (Drawable      : Gdk.Drawable.Gdk_Drawable;
      GC            : Gdk.GC.Gdk_GC;
      X, Y          : Glib.Gint;
      Width, Height : Glib.Gint;
      Dith          : Gdk_Rgb_Dither;
      Rgb_Buf       : Rgb_Buffer;
      Rowstride     : Glib.Gint;
      Cmap          : Gdk_Rgb_Cmap)
   is
      procedure Internal
        (Drawable      : Gdk.Drawable.Gdk_Drawable;
         GC            : Gdk.GC.Gdk_GC;
         X, Y          : Glib.Gint;
         Width, Height : Glib.Gint;
         Dith          : Gdk_Rgb_Dither;
         Rgb_Buf       : System.Address;
         Rowstride     : Glib.Gint;
         Cmap          : Gdk_Rgb_Cmap);
      pragma Import (C, Internal, "gdk_draw_indexed_image");

   begin
      Internal
        (Drawable, GC, X, Y, Width, Height, Dith,
         Rgb_Buf'Address, Rowstride, Cmap);
   end Draw_Indexed_Image;

   procedure Draw_Indexed_Image
     (Drawable      : Gdk.Drawable.Gdk_Drawable;
      GC            : Gdk.GC.Gdk_GC;
      X, Y          : Glib.Gint;
      Width, Height : Glib.Gint;
      Dith          : Gdk_Rgb_Dither;
      Rgb_Buf       : Unchecked_Rgb_Buffer;
      Rowstride     : Glib.Gint;
      Cmap          : Gdk_Rgb_Cmap)
   is
      procedure Internal
        (Drawable      : Gdk.Drawable.Gdk_Drawable;
         GC            : Gdk.GC.Gdk_GC;
         X, Y          : Glib.Gint;
         Width, Height : Glib.Gint;
         Dith          : Gdk_Rgb_Dither;
         Rgb_Buf       : System.Address;
         Rowstride     : Glib.Gint;
         Cmap          : Gdk_Rgb_Cmap);
      pragma Import (C, Internal, "gdk_draw_indexed_image");

   begin
      Internal
        (Drawable, GC, X, Y, Width, Height, Dith,
         Rgb_Buf'Address, Rowstride, Cmap);
   end Draw_Indexed_Image;

end Gdk.Rgb;
