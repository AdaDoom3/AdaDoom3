-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
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

with Gdk.Cursor;  use Gdk.Cursor;
with Gdk.Display; use Gdk.Display;
with Glib.Object; use Glib.Object;
with Ada.Unchecked_Deallocation;

package body Gdk.Pixbuf is

   type Proxy_Data is record
      Pixels : Guchar_Array_Access;
      --  Access to the Ada allocated memory used by Gdk_New_From_Data.
      --  Strictly, this is not necessary, as this access could be
      --  built from the Pixels parameter of the destroy callback.
      --  However, it's not possible to easily and portably convert an
      --  Address to an access to unconstrained array.
   end record;
   pragma Convention (C, Proxy_Data);
   --  Closure data attached to a pixbuf created from user data.

   type Proxy_Data_Access is access Proxy_Data;
   pragma Convention (C, Proxy_Data_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Proxy_Data, Name => Proxy_Data_Access);

   procedure Destroy_Pixels
     (Pixels : System.Address;
      Data   : Proxy_Data_Access);
   pragma Convention (C, Destroy_Pixels);
   --  Low level, C compatible, callback called when the reference
   --  count of a pixbuf (that was created from user data) reaches 0.

   ---------------
   -- Add_Alpha --
   ---------------

   function Add_Alpha
     (Pixbuf           : Gdk_Pixbuf;
      Substitute_Color : Boolean := False;
      Red              : Guchar := 0;
      Green            : Guchar := 0;
      Blue             : Guchar := 0) return Gdk_Pixbuf
   is
      function Internal
        (Pixbuf           : System.Address;
         Substitute_Color : Gboolean;
         Red              : Guchar;
         Green            : Guchar;
         Blue             : Guchar) return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_add_alpha");

   begin
      return Convert
        (Internal
           (Get_Object (Pixbuf),
            Boolean'Pos (Substitute_Color),
            Red,
            Green,
            Blue));
   end Add_Alpha;

   ---------------
   -- Composite --
   ---------------

   procedure Composite
     (Src           : Gdk_Pixbuf;
      Dest          : Gdk_Pixbuf;
      Dest_X        : Gint;
      Dest_Y        : Gint;
      Dest_Width    : Gint;
      Dest_Height   : Gint;
      Offset_X      : Gdouble := 0.0;
      Offset_Y      : Gdouble := 0.0;
      Scale_X       : Gdouble := 1.0;
      Scale_Y       : Gdouble := 1.0;
      Inter_Type    : Gdk_Interp_Type := Interp_Bilinear;
      Overall_Alpha : Alpha_Range := 128)
   is
      procedure Internal
        (Src           : System.Address;
         Dest          : System.Address;
         Dest_X        : Gint;
         Dest_Y        : Gint;
         Dest_Width    : Gint;
         Dest_Height   : Gint;
         Offset_X      : Gdouble := 0.0;
         Offset_Y      : Gdouble := 0.0;
         Scale_X       : Gdouble := 1.0;
         Scale_Y       : Gdouble := 1.0;
         Inter_Type    : Gdk_Interp_Type := Interp_Bilinear;
         Overall_Alpha : Alpha_Range := 128);
      pragma Import (C, Internal, "gdk_pixbuf_composite");

   begin
      Internal
        (Get_Object (Src),
         Get_Object (Dest),
         Dest_X,
         Dest_Y,
         Dest_Width,
         Dest_Height,
         Offset_X,
         Offset_Y,
         Scale_X,
         Scale_Y,
         Inter_Type,
         Overall_Alpha);
   end Composite;

   ---------------------
   -- Composite_Color --
   ---------------------

   procedure Composite_Color
     (Src           : Gdk_Pixbuf;
      Dest          : Gdk_Pixbuf;
      Dest_X        : Gint;
      Dest_Y        : Gint;
      Dest_Width    : Gint;
      Dest_Height   : Gint;
      Offset_X      : Gdouble := 0.0;
      Offset_Y      : Gdouble := 0.0;
      Scale_X       : Gdouble := 1.0;
      Scale_Y       : Gdouble := 1.0;
      Inter_Type    : Gdk_Interp_Type := Interp_Bilinear;
      Overall_Alpha : Alpha_Range := 128;
      Check_X       : Gint := 0;
      Check_Y       : Gint := 0;
      Check_Size    : Gint := 0;
      Color1        : Guint32 := 0;
      Color2        : Guint32 := 0)
   is
      procedure Internal
        (Src           : System.Address;
         Dest          : System.Address;
         Dest_X        : Gint;
         Dest_Y        : Gint;
         Dest_Width    : Gint;
         Dest_Height   : Gint;
         Offset_X      : Gdouble;
         Offset_Y      : Gdouble;
         Scale_X       : Gdouble;
         Scale_Y       : Gdouble;
         Inter_Type    : Gdk_Interp_Type;
         Overall_Alpha : Alpha_Range;
         Check_X       : Gint;
         Check_Y       : Gint;
         Check_Size    : Gint;
         Color1        : Guint32;
         Color2        : Guint32);
      pragma Import (C, Internal, "gdk_pixbuf_composite_color");

   begin
      Internal
        (Get_Object (Src),
         Get_Object (Dest),
         Dest_X,
         Dest_Y,
         Dest_Width,
         Dest_Height,
         Offset_X,
         Offset_Y,
         Scale_X,
         Scale_Y,
         Inter_Type,
         Overall_Alpha,
         Check_X,
         Check_Y,
         Check_Size,
         Color1,
         Color2);
   end Composite_Color;

   ----------------------------
   -- Composite_Color_Simple --
   ----------------------------

   function Composite_Color_Simple
     (Src           : Gdk_Pixbuf;
      Dest_Width    : Gint;
      Dest_Height   : Gint;
      Inter_Type    : Gdk_Interp_Type := Interp_Bilinear;
      Overall_Alpha : Alpha_Range := 128;
      Color1        : Guint32 := 0;
      Color2        : Guint32 := 0) return Gdk_Pixbuf
   is
      function Internal
        (Src           : System.Address;
         Dest_Width    : Gint;
         Dest_Height   : Gint;
         Inter_Type    : Gdk_Interp_Type := Interp_Bilinear;
         Overall_Alpha : Alpha_Range := 128;
         Color1        : Guint32 := 0;
         Color2        : Guint32 := 0) return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_composite_color_simple");

   begin
      return Convert
        (Internal
           (Get_Object (Src),
            Dest_Width,
            Dest_Height,
            Inter_Type,
            Overall_Alpha,
            Color1,
            Color2));
   end Composite_Color_Simple;

   -------------
   -- Convert --
   -------------

   function Convert (P : System.Address) return Gdk_Pixbuf is
      use type System.Address;

      Stub : Gdk_Pixbuf_Record;

   begin
      if P = System.Null_Address then
         return null;

      else
         return Gdk_Pixbuf (Get_User_Data (P, Stub));
      end if;
   end Convert;

   ----------
   -- Copy --
   ----------

   function Copy (Pixbuf : Gdk_Pixbuf) return Gdk_Pixbuf is
      function Internal (Pixbuf : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_copy");

   begin
      return Convert (Internal (Get_Object (Pixbuf)));
   end Copy;

   ---------------
   -- Copy_Area --
   ---------------

   procedure Copy_Area
     (Src_Pixbuf  : Gdk_Pixbuf;
      Src_X       : Gint;
      Src_Y       : Gint;
      Width       : Gint;
      Height      : Gint;
      Dest_Pixbuf : Gdk_Pixbuf;
      Dest_X      : Gint;
      Dest_Y      : Gint)
   is
      procedure Internal
        (Src_Pixbuf  : System.Address;
         Src_X       : Gint;
         Src_Y       : Gint;
         Width       : Gint;
         Height      : Gint;
         Dest_Pixbuf : System.Address;
         Dest_X      : Gint;
         Dest_Y      : Gint);
      pragma Import (C, Internal, "gdk_pixbuf_copy_area");

   begin
      Internal
        (Get_Object (Src_Pixbuf),
         Src_X,
         Src_Y,
         Width,
         Height,
         Get_Object (Dest_Pixbuf),
         Dest_X,
         Dest_Y);
   end Copy_Area;

   --------------------
   -- Destroy_Pixels --
   --------------------

   procedure Destroy_Pixels
     (Pixels : System.Address;
      Data   : Proxy_Data_Access)
   is
      pragma Unreferenced (Pixels);
      Tmp : Proxy_Data_Access := Data;
   begin
      Free (Data.Pixels);
      Free (Tmp);
   end Destroy_Pixels;

   ----------
   -- Fill --
   ----------

   procedure Fill (Pixbuf : Gdk_Pixbuf; Pixel : Guint32) is
      procedure Internal (Pixbuf : System.Address; Pixel : Guint32);
      pragma Import (C, Internal, "gdk_pixbuf_fill");
   begin
      Internal (Get_Object (Pixbuf), Pixel);
   end Fill;

   -------------
   -- Gdk_New --
   -------------

   function Gdk_New
     (Colorspace      : Gdk_Colorspace := Colorspace_RGB;
      Has_Alpha       : Boolean := False;
      Bits_Per_Sample : Gint := 8;
      Width           : Gint;
      Height          : Gint) return Gdk_Pixbuf
   is
      function Internal
        (Colorspace      : Gdk_Colorspace;
         Has_Alpha       : Gboolean;
         Bits_Per_Sample : Gint;
         Width           : Gint;
         Height          : Gint) return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_new");

   begin
      return Convert
        (Internal
           (Colorspace,
            Boolean'Pos (Has_Alpha),
            Bits_Per_Sample,
            Width,
            Height));
   end Gdk_New;

   -----------------------
   -- Gdk_New_From_Data --
   -----------------------

   function Gdk_New_From_Data
     (Data              : Guchar_Array_Access;
      Colorspace        : Gdk_Colorspace := Colorspace_RGB;
      Has_Alpha         : Boolean := False;
      Bits_Per_Sample   : Gint := 8;
      Width             : Gint;
      Height            : Gint;
      Rowstride         : Gint;
      Auto_Destroy_Data : Boolean := True) return Gdk_Pixbuf
   is
      function Internal
        (Data            : System.Address;
         Colorspace      : Gdk_Colorspace;
         Has_Alpha       : Gboolean;
         Bits_Per_Sample : Gint;
         Width           : Gint;
         Height          : Gint;
         Row_Stride      : Gint;
         Destroy_Fn      : System.Address;
         Destroy_Fn_Data : Proxy_Data_Access) return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_new_from_data");

   begin
      if Auto_Destroy_Data then
         return Convert
           (Internal
              (Data.all'Address,
               Colorspace,
               Boolean'Pos (Has_Alpha),
               Bits_Per_Sample,
               Width,
               Height,
               Rowstride,
               Destroy_Pixels'Address,
               new Proxy_Data'(Pixels => Data)));

      else
         return Convert
           (Internal
              (Data.all'Address,
               Colorspace,
               Boolean'Pos (Has_Alpha),
               Bits_Per_Sample,
               Width,
               Height,
               Rowstride,
               System.Null_Address,
               null));
      end if;
   end Gdk_New_From_Data;

   -----------------------
   -- Gdk_New_From_File --
   -----------------------

   procedure Gdk_New_From_File
     (Pixbuf   : out Gdk_Pixbuf;
      Filename : String;
      Error    : out GError)
   is
      function Internal
        (Filename : String;
         Error    : access GError) return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_new_from_file");

      Err : aliased GError;

   begin
      Pixbuf := Convert (Internal (Filename & ASCII.NUL, Err'Access));
      Error := Err;
   end Gdk_New_From_File;

   -----------------------
   -- Gdk_New_From_File --
   -----------------------

   procedure Gdk_New_From_File
     (Animation : out Gdk_Pixbuf_Animation;
      Filename  : String;
      Error     : out GError)
   is
      function Internal
        (Filename : String;
         Error    : access GError) return Gdk_Pixbuf_Animation;
      pragma Import (C, Internal, "gdk_pixbuf_animation_new_from_file");

      Err : aliased GError;

   begin
      Animation := Internal (Filename & ASCII.NUL, Err'Access);
      Error := Err;
   end Gdk_New_From_File;

   ---------------------------
   -- Gdk_New_From_Xpm_Data --
   ---------------------------

   function Gdk_New_From_Xpm_Data
     (Data : Interfaces.C.Strings.chars_ptr_array) return Gdk_Pixbuf
   is
      function Internal
        (Data : Interfaces.C.Strings.chars_ptr_array) return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_new_from_xpm_data");

   begin
      return Convert (Internal (Data));
   end Gdk_New_From_Xpm_Data;

   -----------------------
   -- Gdk_New_Subpixbuf --
   -----------------------

   function Gdk_New_Subpixbuf
     (Src_Pixbuf : Gdk_Pixbuf;
      Src_X      : Gint;
      Src_Y      : Gint;
      Width      : Gint;
      Height     : Gint) return Gdk_Pixbuf
   is
      function Internal
        (Src_Pixbuf : System.Address;
         Src_X      : Gint;
         Src_Y      : Gint;
         Width      : Gint;
         Height     : Gint) return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_new_subpixbuf");

   begin
      return Convert
        (Internal (Get_Object (Src_Pixbuf), Src_X, Src_Y, Width, Height));
   end Gdk_New_Subpixbuf;

   -------------------------
   -- Get_Bits_Per_Sample --
   -------------------------

   function Get_Bits_Per_Sample (Pixbuf : Gdk_Pixbuf) return Gint is
      function Internal (Pixbuf : System.Address) return Gint;
      pragma Import (C, Internal, "gdk_pixbuf_get_bits_per_sample");

   begin
      return Internal (Get_Object (Pixbuf));
   end Get_Bits_Per_Sample;

   --------------------
   -- Get_Colorspace --
   --------------------

   function Get_Colorspace (Pixbuf : Gdk_Pixbuf) return Gdk_Colorspace is
      function Internal (Pixbuf : System.Address) return Gdk_Colorspace;
      pragma Import (C, Internal, "gdk_pixbuf_get_colorspace");

   begin
      return Internal (Get_Object (Pixbuf));
   end Get_Colorspace;

   -----------------------
   -- Get_From_Drawable --
   -----------------------

   function Get_From_Drawable
     (Dest   : Gdk_Pixbuf;
      Src    : Gdk.Drawable.Gdk_Drawable;
      Cmap   : Gdk.Color.Gdk_Colormap;
      Src_X  : Gint;
      Src_Y  : Gint;
      Dest_X : Gint;
      Dest_Y : Gint;
      Width  : Gint;
      Height : Gint) return Gdk_Pixbuf
   is
      function Internal
        (Dest   : System.Address;
         Src    : Gdk.Drawable.Gdk_Drawable;
         Cmap   : Gdk.Color.Gdk_Colormap;
         Src_X  : Gint;
         Src_Y  : Gint;
         Dest_X : Gint;
         Dest_Y : Gint;
         Width  : Gint;
         Height : Gint) return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_get_from_drawable");

   begin
      if Dest = null then
         return Convert
           (Internal
              (System.Null_Address,
               Src,
               Cmap,
               Src_X,
               Src_Y,
               Dest_X,
               Dest_Y,
               Width,
               Height));

      else
         return Convert
           (Internal
              (Get_Object (Dest),
               Src,
               Cmap,
               Src_X,
               Src_Y,
               Dest_X,
               Dest_Y,
               Width,
               Height));
      end if;
   end Get_From_Drawable;

   -------------------
   -- Get_Has_Alpha --
   -------------------

   function Get_Has_Alpha (Pixbuf : Gdk_Pixbuf) return Boolean is
      function Internal (Pixbuf : System.Address) return Gboolean;
      pragma Import (C, Internal, "gdk_pixbuf_get_has_alpha");

   begin
      return Internal (Get_Object (Pixbuf)) /= 0;
   end Get_Has_Alpha;

   ----------------
   -- Get_Height --
   ----------------

   function Get_Height (Pixbuf : Gdk_Pixbuf) return Gint is
      function Internal (Pixbuf : System.Address) return Gint;
      pragma Import (C, Internal, "gdk_pixbuf_get_height");

   begin
      return Internal (Get_Object (Pixbuf));
   end Get_Height;

   ----------------
   -- Get_Height --
   ----------------

   function Get_Height (Animation : Gdk_Pixbuf_Animation) return Gint is
      function Internal (Animation : Gdk_Pixbuf_Animation) return Gint;
      pragma Import (C, Internal, "gdk_pixbuf_animation_get_height");

   begin
      return Internal (Animation);
   end Get_Height;

   ---------------
   -- Get_Image --
   ---------------

   function Get_Image (Cursor : Gdk.Cursor.Gdk_Cursor) return Gdk_Pixbuf is
      function Internal (Cursor : Gdk.Cursor.Gdk_Cursor) return System.Address;
      pragma Import (C, Internal, "gdk_cursor_get_image");

   begin
      return Convert (Internal (Cursor));
   end Get_Image;

   --------------------
   -- Get_N_Channels --
   --------------------

   function Get_N_Channels (Pixbuf : Gdk_Pixbuf) return Gint is
      function Internal (Pixbuf : System.Address) return Gint;
      pragma Import (C, Internal, "gdk_pixbuf_get_n_channels");

   begin
      return Internal (Get_Object (Pixbuf));
   end Get_N_Channels;

   ----------------
   -- Get_Pixbuf --
   ----------------

   function Get_Pixbuf (Iter : Gdk_Pixbuf_Animation_Iter) return Gdk_Pixbuf is
      function Internal
        (Iter : Gdk_Pixbuf_Animation_Iter) return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_animation_iter_get_pixbuf");

   begin
      return Convert (Internal (Iter));
   end Get_Pixbuf;

   ----------------
   -- Get_Pixels --
   ----------------

   function Get_Pixels
     (Pixbuf : Gdk_Pixbuf) return Gdk.Rgb.Rgb_Buffer_Access
   is
      function Internal
        (Pixbuf : System.Address) return Gdk.Rgb.Rgb_Buffer_Access;
      pragma Import (C, Internal, "gdk_pixbuf_get_pixels");

   begin
      return Internal (Get_Object (Pixbuf));
   end Get_Pixels;

   -------------------
   -- Get_Rowstride --
   -------------------

   function Get_Rowstride (Pixbuf : Gdk_Pixbuf) return Gint is
      function Internal (Pixbuf : System.Address) return Gint;
      pragma Import (C, Internal, "gdk_pixbuf_get_rowstride");

   begin
      return Internal (Get_Object (Pixbuf));
   end Get_Rowstride;

   ----------------------
   -- Get_Static_Image --
   ----------------------

   function Get_Static_Image
     (Animation : Gdk_Pixbuf_Animation) return Gdk_Pixbuf
   is
      function Internal
        (Animation : Gdk_Pixbuf_Animation) return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_animation_get_static_image");

   begin
      return Convert (Internal (Animation));
   end Get_Static_Image;

   ---------------
   -- Get_Width --
   ---------------

   function Get_Width (Pixbuf : Gdk_Pixbuf) return Gint is
      function Internal (Pixbuf : System.Address) return Gint;
      pragma Import (C, Internal, "gdk_pixbuf_get_width");

   begin
      return Internal (Get_Object (Pixbuf));
   end Get_Width;

   ---------------
   -- Get_Width --
   ---------------

   function Get_Width (Animation : Gdk_Pixbuf_Animation) return Gint is
      function Internal (Animation : Gdk_Pixbuf_Animation) return Gint;
      pragma Import (C, Internal, "gdk_pixbuf_animation_get_width");

   begin
      return Internal (Animation);
   end Get_Width;

   ---------
   -- Ref --
   ---------

   procedure Ref (Animation : Gdk_Pixbuf_Animation) is
      procedure Internal (Pixbuf : Gdk_Pixbuf_Animation);
      pragma Import (C, Internal, "gdk_pixbuf_animation_ref");

   begin
      Internal (Animation);
   end Ref;

   procedure Ref (Iter : Gdk_Pixbuf_Animation_Iter) is
      procedure Internal (Iter : Gdk_Pixbuf_Animation_Iter);
      pragma Import (C, Internal, "g_object_ref");

   begin
      Internal (Iter);
   end Ref;

   ----------------------------
   -- Render_Pixmap_And_Mask --
   ----------------------------

   procedure Render_Pixmap_And_Mask
     (Pixbuf          : Gdk_Pixbuf;
      Pixmap          : out Gdk.Pixmap.Gdk_Pixmap;
      Mask            : out Gdk.Bitmap.Gdk_Bitmap;
      Alpha_Threshold : Alpha_Range)
   is
      procedure Internal
        (Pixbuf          : System.Address;
         Pixmap          : out Gdk.Pixmap.Gdk_Pixmap;
         Mask            : out Gdk.Bitmap.Gdk_Bitmap;
         Alpha_Threshold : Alpha_Range);
      pragma Import (C, Internal, "gdk_pixbuf_render_pixmap_and_mask");

   begin
      Internal (Get_Object (Pixbuf), Pixmap, Mask, Alpha_Threshold);
   end Render_Pixmap_And_Mask;

   -----------------------------------------
   -- Render_Pixmap_And_Mask_For_Colormap --
   -----------------------------------------

   procedure Render_Pixmap_And_Mask_For_Colormap
     (Pixbuf          : Gdk_Pixbuf;
      Colormap        : Gdk.Color.Gdk_Colormap;
      Pixmap          : out Gdk.Pixmap.Gdk_Pixmap;
      Mask            : out Gdk.Bitmap.Gdk_Bitmap;
      Alpha_Threshold : Alpha_Range)
   is
      procedure Internal
        (Pixbuf          : System.Address;
         Colormap        : Gdk.Color.Gdk_Colormap;
         Pixmap          : out Gdk.Pixmap.Gdk_Pixmap;
         Mask            : out Gdk.Bitmap.Gdk_Bitmap;
         Alpha_Threshold : Alpha_Range);
      pragma Import
        (C, Internal, "gdk_pixbuf_render_pixmap_and_mask_for_colormap");

   begin
      Internal (Get_Object (Pixbuf), Colormap, Pixmap, Mask, Alpha_Threshold);
   end Render_Pixmap_And_Mask_For_Colormap;

   ----------------------------
   -- Render_Threshold_Alpha --
   ----------------------------

   procedure Render_Threshold_Alpha
     (Pixbuf          : Gdk_Pixbuf;
      Bitmap          : Gdk.Bitmap.Gdk_Bitmap;
      Src_X           : Gint;
      Src_Y           : Gint;
      Dest_X          : Gint;
      Dest_Y          : Gint;
      Width           : Gint;
      Height          : Gint;
      Alpha_Threshold : Alpha_Range)
   is
      procedure Internal
        (Pixbuf          : System.Address;
         Bitmap          : Gdk.Bitmap.Gdk_Bitmap;
         Src_X           : Gint;
         Src_Y           : Gint;
         Dest_X          : Gint;
         Dest_Y          : Gint;
         Width           : Gint;
         Height          : Gint;
         Alpha_Threshold : Alpha_Range);
      pragma Import (C, Internal, "gdk_pixbuf_render_threshold_alpha");

   begin
      Internal
        (Get_Object (Pixbuf),
         Bitmap,
         Src_X,
         Src_Y,
         Dest_X,
         Dest_Y,
         Width,
         Height,
         Alpha_Threshold);
   end Render_Threshold_Alpha;

   ------------------------
   -- Render_To_Drawable --
   ------------------------

   procedure Render_To_Drawable
     (Pixbuf   : Gdk_Pixbuf;
      Drawable : Gdk.Drawable.Gdk_Drawable;
      GC       : Gdk.GC.Gdk_GC;
      Src_X    : Gint;
      Src_Y    : Gint;
      Dest_X   : Gint;
      Dest_Y   : Gint;
      Width    : Gint;
      Height   : Gint;
      Dither   : Gdk.Rgb.Gdk_Rgb_Dither := Gdk.Rgb.Dither_Normal;
      X_Dither : Gint := 0;
      Y_Dither : Gint := 0)
   is
      procedure Internal
        (Pixbuf   : System.Address;
         Drawable : Gdk.Drawable.Gdk_Drawable;
         GC       : Gdk.GC.Gdk_GC;
         Src_X    : Gint;
         Src_Y    : Gint;
         Dest_X   : Gint;
         Dest_Y   : Gint;
         Width    : Gint;
         Height   : Gint;
         Dither   : Gdk.Rgb.Gdk_Rgb_Dither;
         X_Dither : Gint;
         Y_Dither : Gint);
      pragma Import (C, Internal, "gdk_pixbuf_render_to_drawable");

   begin
      Internal
        (Get_Object (Pixbuf),
         Drawable,
         GC,
         Src_X,
         Src_Y,
         Dest_X,
         Dest_Y,
         Width,
         Height,
         Dither,
         X_Dither,
         Y_Dither);
   end Render_To_Drawable;

   ------------------------------
   -- Render_To_Drawable_Alpha --
   ------------------------------

   procedure Render_To_Drawable_Alpha
     (Pixbuf          : Gdk_Pixbuf;
      Drawable        : Gdk.Drawable.Gdk_Drawable;
      Src_X           : Gint;
      Src_Y           : Gint;
      Dest_X          : Gint;
      Dest_Y          : Gint;
      Width           : Gint;
      Height          : Gint;
      Alpha           : Alpha_Mode;
      Alpha_Threshold : Alpha_Range;
      Dither          : Gdk.Rgb.Gdk_Rgb_Dither := Gdk.Rgb.Dither_Normal;
      X_Dither        : Gint := 0;
      Y_Dither        : Gint := 0)
   is
      procedure Internal
        (Pixbuf          : System.Address;
         Drawable        : Gdk.Drawable.Gdk_Drawable;
         Src_X           : Gint;
         Src_Y           : Gint;
         Dest_X          : Gint;
         Dest_Y          : Gint;
         Width           : Gint;
         Height          : Gint;
         Alpha           : Alpha_Mode;
         Alpha_Threshold : Alpha_Range;
         Dither          : Gdk.Rgb.Gdk_Rgb_Dither;
         X_Dither        : Gint;
         Y_Dither        : Gint);
      pragma Import (C, Internal, "gdk_pixbuf_render_to_drawable_alpha");

   begin
      Internal
        (Get_Object (Pixbuf),
         Drawable,
         Src_X,
         Src_Y,
         Dest_X,
         Dest_Y,
         Width,
         Height,
         Alpha,
         Alpha_Threshold,
         Dither,
         X_Dither,
         Y_Dither);
   end Render_To_Drawable_Alpha;

   ---------------------------
   -- Saturate_And_Pixelate --
   ---------------------------

   procedure Saturate_And_Pixelate
     (Src        : Gdk_Pixbuf;
      Dest       : Gdk_Pixbuf;
      Saturation : Gfloat;
      Pixelate   : Boolean := True)
   is
      procedure Internal
        (Src        : System.Address;
         Dest       : System.Address;
         Saturation : Gfloat;
         Pixelate   : Gboolean);
      pragma Import (C, Internal, "gdk_pixbuf_saturate_and_pixelate");

   begin
      Internal
        (Get_Object (Src),
         Get_Object (Dest),
         Saturation,
         Boolean'Pos (Pixelate));
   end Saturate_And_Pixelate;

   ----------
   -- Save --
   ----------

   procedure Save
     (Pixbuf   : Gdk_Pixbuf;
      Filename : String;
      Format   : File_Format;
      Error    : out GError;
      Quality  : Image_Quality := Image_Quality'Last;
      Depth    : Integer := 32)
   is
      procedure Internal
        (Pixbuf   : System.Address;
         Filename : String;
         Format   : String;
         Error    : out GError;
         Term     : System.Address := System.Null_Address);

      procedure Internal
        (Pixbuf   : System.Address;
         Filename : String;
         Format   : String;
         Error    : out GError;
         Key      : String;
         Value    : String);
      pragma Import (C, Internal, "ada_gdk_pixbuf_save");

   begin
      Error := null;

      case Format is
         when JPEG =>
            Internal
              (Get_Object (Pixbuf),
               Filename & ASCII.NUL,
               "jpeg" & ASCII.NUL,
               Error,
               "quality" & ASCII.NUL,
               Image_Quality'Image (Quality) & ASCII.NUL);

         when PNG =>
            Internal
              (Get_Object (Pixbuf),
               Filename & ASCII.NUL,
               "png" & ASCII.NUL,
               Error);

         when ICO =>
            Internal
              (Get_Object (Pixbuf),
               Filename & ASCII.NUL,
               "ico" & ASCII.NUL,
               Error,
               "depth" & ASCII.NUL,
               Integer'Image (Depth));

         when BMP =>
            Internal
              (Get_Object (Pixbuf),
               Filename & ASCII.NUL,
               "bmp" & ASCII.NUL,
               Error);
      end case;
   end Save;

   -----------
   -- Scale --
   -----------

   procedure Scale
     (Src          : Gdk_Pixbuf;
      Dest         : Gdk_Pixbuf;
      Dest_X       : Gint;
      Dest_Y       : Gint;
      Dest_Width   : Gint;
      Dest_Height  : Gint;
      Offset_X     : Gdouble := 0.0;
      Offset_Y     : Gdouble := 0.0;
      Scale_X      : Gdouble := 1.0;
      Scale_Y      : Gdouble := 1.0;
      Inter_Type   : Gdk_Interp_Type := Interp_Bilinear)
   is
      procedure Internal
        (Src          : System.Address;
         Dest         : System.Address;
         Dest_X       : Gint;
         Dest_Y       : Gint;
         Dest_Width   : Gint;
         Dest_Height  : Gint;
         Offset_X     : Gdouble := 0.0;
         Offset_Y     : Gdouble := 0.0;
         Scale_X      : Gdouble := 1.0;
         Scale_Y      : Gdouble := 1.0;
         Inter_Type   : Gdk_Interp_Type);
      pragma Import (C, Internal, "gdk_pixbuf_scale");

   begin
      Internal
        (Get_Object (Src),
         Get_Object (Dest),
         Dest_X,
         Dest_Y,
         Dest_Width,
         Dest_Height,
         Offset_X,
         Offset_Y,
         Scale_X,
         Scale_Y,
         Inter_Type);
   end Scale;

   ------------------
   -- Scale_Simple --
   ------------------

   function Scale_Simple
     (Src           : Gdk_Pixbuf;
      Dest_Width    : Gint;
      Dest_Height   : Gint;
      Inter_Type    : Gdk_Interp_Type := Interp_Bilinear) return Gdk_Pixbuf
   is
      function Internal
        (Src           : System.Address;
         Dest_Width    : Gint;
         Dest_Height   : Gint;
         Inter_Type    : Gdk_Interp_Type) return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_scale_simple");

   begin
      return Convert
        (Internal (Get_Object (Src), Dest_Width, Dest_Height, Inter_Type));
   end Scale_Simple;

   -----------
   -- Unref --
   -----------

   procedure Unref (Animation : Gdk_Pixbuf_Animation) is
      procedure Internal (Pixbuf : Gdk_Pixbuf_Animation);
      pragma Import (C, Internal, "gdk_pixbuf_animation_unref");

   begin
      Internal (Animation);
   end Unref;

   procedure Unref (Iter : Gdk_Pixbuf_Animation_Iter) is
      procedure Internal (Iter : Gdk_Pixbuf_Animation_Iter);
      pragma Import (C, Internal, "g_object_unref");

   begin
      Internal (Iter);
   end Unref;

   ---------------------
   -- Is_Static_Image --
   ---------------------

   function Is_Static_Image
     (Animation : Gdk_Pixbuf_Animation) return Boolean
   is
      function Internal (Animation : Gdk_Pixbuf_Animation) return Gboolean;
      pragma Import (C, Internal, "gdk_pixbuf_animation_is_static_image");

   begin
      return Internal (Animation) /= 0;
   end Is_Static_Image;

   --------------------------------
   -- On_Currently_Loading_Frame --
   --------------------------------

   function On_Currently_Loading_Frame
     (Iter : Gdk_Pixbuf_Animation_Iter) return Boolean
   is
      function Internal
        (Animation : Gdk_Pixbuf_Animation_Iter) return Gboolean;
      pragma Import
        (C, Internal, "gdk_pixbuf_animation_iter_on_currently_loading_frame");

   begin
      return Internal (Iter) /= 0;
   end On_Currently_Loading_Frame;

   -------------
   -- Advance --
   -------------

   function Advance
     (Iter          : Gdk_Pixbuf_Animation_Iter;
      Current_Timer : GTime_Val_Access := null) return Boolean
   is
      function Internal
        (Iter          : Gdk_Pixbuf_Animation_Iter;
         Current_Timer : GTime_Val_Access) return Gboolean;
      pragma Import (C, Internal, "gdk_pixbuf_animation_iter_advance");

   begin
      return Internal (Iter, Current_Timer) /= 0;
   end Advance;

   -------------------------
   -- Gdk_New_From_Pixbuf --
   -------------------------

   procedure Gdk_New_From_Pixbuf
     (Cursor  : out Gdk.Cursor.Gdk_Cursor;
      Display : Gdk.Display.Gdk_Display := Gdk.Display.Get_Default;
      Pixbuf  : Gdk_Pixbuf;
      X       : Glib.Gint;
      Y       : Glib.Gint)
   is
      function Internal
        (D : System.Address; P : System.Address; X, Y : Gint)
         return Gdk_Cursor;
      pragma Import (C, Internal, "gdk_cursor_new_from_pixbuf");

   begin
      Cursor := Internal (Get_Object (Display), Get_Object (Pixbuf), X, Y);
   end Gdk_New_From_Pixbuf;

end Gdk.Pixbuf;
