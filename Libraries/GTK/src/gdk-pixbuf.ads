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

--  <description>
--  This object provides image manipulation routines.
--
--  The following image formats are known, but some depend on external
--  libraries for the proper loading of files (indicated with * in the list):
--     PNG*, JPEG*, TIFF*, GIF, XPM, PNM, Sun raster file (ras), ico,
--     bmp.
--
--  With this package, you can load images from file, display them on the
--  screen, re-scale them and compose them with other images.
--  All the functions fully support alpha channels (opacity).
--
--  Different filters are provided, depending on the quality of output you
--  expect and the speed you need.
--  </description>
--  <c_version>1.3.11</c_version>
--  <group>Gdk, the low-level API</group>
--  <testgtk>create_pixbuf.adb</testgtk>

with Interfaces.C.Strings;
with System;

with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Glib.Object;
with Gdk.Bitmap;
with Gdk.Drawable;
with Gdk.Color;
with Gdk.Cursor;
with Gdk.Display;
with Gdk.GC;
with Gdk.Pixmap;
with Gdk.Rgb;

package Gdk.Pixbuf is

   type Gdk_Pixbuf_Record is new Glib.Object.GObject_Record with private;

   type Gdk_Pixbuf is access all Gdk_Pixbuf_Record'Class;
   --  A very efficient client-side pixmap.
   --  This type can be adapted to all the possible screen depths (number of
   --  bits per pixel), and the algorithms are extremely efficient.
   --  You can also load a pixbuf directly from an external file in one of
   --  the standard image formats.

   Null_Pixbuf : constant Gdk_Pixbuf := null;

   type Gdk_Pixbuf_Animation is new Glib.C_Proxy;
   --  Type used for animations.

   type Gdk_Pixbuf_Animation_Iter is new Glib.C_Proxy;
   --  Type used to iterate through an animation.

   type Alpha_Mode is (Alpha_Bilevel, Alpha_Full);
   --  Alpha compositing mode.
   --  This indicates how the alpha channel (for opacity) is handled when
   --  rendering.
   pragma Convention (C, Alpha_Mode);

   type Gdk_Colorspace is (Colorspace_RGB);
   --  Type of the image.
   --  The only possible value is currently RGB, but extensions will
   --  exist with CMYK, Gray, Lab, ...
   pragma Convention (C, Gdk_Colorspace);

   type Gdk_Interp_Type is
     (Interp_Nearest,
      --  Nearest neighbor. It is the fastest and lowest quality.

      Interp_Tiles,
      --  Accurate simulation of the Postscript image operator
      --  without any interpolation enabled; each pixel is rendered as a tiny
      --  parallelogram of solid color, the edges of which are implemented
      --  with anti-aliasing. It resembles nearest neighbor for enlargement,
      --  and bilinear for reduction.

      Interp_Bilinear,
      --  Bilinear interpolation. For enlargement, it is equivalent to
      --  point-sampling the ideal bilinear-interpolated image. For reduction,
      --  it is equivalent to laying down small tiles and integrating over the
      --  coverage area.

      Interp_Hyper
      --  Filter_Hyper is the highest quality reconstruction function. It is
      --  derived from the hyperbolic filters in Wolberg's "Digital Image
      --  Warping," and is formally defined as the hyperbolic-filter sampling
      --  the ideal hyperbolic-filter interpolated image (the filter is
      --  designed to be idempotent for 1:1 pixel mapping). It is the slowest
      --  and highest quality.
     );
   --  Interpolation methods.
   pragma Convention (C, Gdk_Interp_Type);

   ------------
   -- Errors --
   ------------

   --  Errors defined in the Pixbuf_Error domain:

   Corrupt_Image         : constant := 0;
   --  image data hosed

   Insufficient_Memory   : constant := 1;
   --  no mem to load image

   Bad_Option            : constant := 2;
   --  bad option passed to save routine

   Unknown_Type          : constant := 3;
   --  unsupported image type

   Unsupported_Operation : constant := 4;
   --  unsupported operation (load, save) for image type

   Failed                : constant := 5;
   --  Operation failed.

   type File_Format is (JPEG, PNG, ICO, BMP);
   --  Possible formats when saving a file.

   type Image_Quality is range 0 .. 100;
   --  For a JPEG image only, quality of the image in percentage.

   type Alpha_Range is range 0 .. 255;
   --  Valid values for alpha parameters.
   pragma Convention (C, Alpha_Range);

   --------------
   -- Get_Type --
   --------------

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Gdk_Pixbuf.

   --------------------------
   -- Accessing the fields --
   --------------------------

   function Get_Colorspace (Pixbuf : Gdk_Pixbuf) return Gdk_Colorspace;
   --  Query the color space of a pixbuf.

   function Get_N_Channels (Pixbuf : Gdk_Pixbuf) return Gint;
   --  Number of channels in the image.

   function Get_Has_Alpha (Pixbuf : Gdk_Pixbuf) return Boolean;
   --  Return True if the image has an alpha channel (opacity information).

   function Get_Bits_Per_Sample (Pixbuf : Gdk_Pixbuf) return Gint;
   --  Number of bits per color sample.

   function Get_Pixels (Pixbuf : Gdk_Pixbuf) return Gdk.Rgb.Rgb_Buffer_Access;
   --  Return a pointer to the pixel data of the image.

   function Get_Width (Pixbuf : Gdk_Pixbuf) return Gint;
   --  Return the width of the image in pixels.

   function Get_Height (Pixbuf : Gdk_Pixbuf) return Gint;
   --  Return the height of the image in pixels.

   function Get_Rowstride (Pixbuf : Gdk_Pixbuf) return Gint;
   --  Return the number of bytes between rows in the image data.

   --------------
   -- Creating --
   --------------

   function Gdk_New
     (Colorspace      : Gdk_Colorspace := Colorspace_RGB;
      Has_Alpha       : Boolean := False;
      Bits_Per_Sample : Gint := 8;
      Width           : Gint;
      Height          : Gint) return Gdk_Pixbuf;
   --  Create a blank pixbuf with an optimal row stride and a new buffer.
   --  The buffer is allocated, but not cleared.
   --  The reference counting is initialized to 1.

   function Copy (Pixbuf : Gdk_Pixbuf) return Gdk_Pixbuf;
   --  Copy a pixbuf.

   function Gdk_New_Subpixbuf
     (Src_Pixbuf : Gdk_Pixbuf;
      Src_X      : Gint;
      Src_Y      : Gint;
      Width      : Gint;
      Height     : Gint) return Gdk_Pixbuf;
   --  Create a pixbuf which points to the pixels of another pixbuf

   procedure Gdk_New_From_File
     (Pixbuf   : out Gdk_Pixbuf;
      Filename : String;
      Error    : out GError);
   --  Load an image from file.

   function Gdk_New_From_Data
     (Data              : Guchar_Array_Access;
      Colorspace        : Gdk_Colorspace := Colorspace_RGB;
      Has_Alpha         : Boolean := False;
      Bits_Per_Sample   : Gint := 8;
      Width             : Gint;
      Height            : Gint;
      Rowstride         : Gint;
      Auto_Destroy_Data : Boolean := True) return Gdk_Pixbuf;
   --  Create a pixbuf out of in-memory image data.
   --  Currently only RGB images with 8 bits per sample are supported.
   --  Width and Height must be > 0.
   --  Rowstride is the distance in bytes between row starts.
   --  A typical value is 4*Width when there is an Alpha channel.
   --  If Auto_Destroy_Data is true, passed data will be automatically
   --  freed when the reference count of the pixbuf reaches 1.
   --  Otherwise, data is never freed.

   function Gdk_New_From_Xpm_Data
     (Data : Interfaces.C.Strings.chars_ptr_array) return Gdk_Pixbuf;
   --  Create an image from a XPM data.

   procedure Fill (Pixbuf : Gdk_Pixbuf; Pixel : Guint32);
   --  Fill pixbuf with a given pixel value.

   procedure Save
     (Pixbuf   : Gdk_Pixbuf;
      Filename : String;
      Format   : File_Format;
      Error    : out GError;
      Quality  : Image_Quality := Image_Quality'Last;
      Depth    : Integer := 32);
   --  Save pixbuf to a file.
   --  Quality is only taken into account for JPEG images.
   --  Depth is only taken into account for ICO images and can take the values
   --  16, 24 or 32.
   --  Error is set to null on success, and set to a GError otherwise.

   function Add_Alpha
     (Pixbuf           : Gdk_Pixbuf;
      Substitute_Color : Boolean := False;
      Red              : Guchar := 0;
      Green            : Guchar := 0;
      Blue             : Guchar := 0) return Gdk_Pixbuf;
   --  Add an alpha channel.
   --  Return a newly allocated image copied from Pixbuf, but with an
   --  extra alpha channel.
   --  If Pixbuf already had an alpha channel, the two images have exactly
   --  the same contents.
   --  If Substitute_Color is True, the color (Red, Green, Blue) is
   --  substituted for zero opacity.
   --  If Substitute_Color is False, Red, Green and Blue are ignored, and a
   --  new color is created with zero opacity.

   procedure Copy_Area
     (Src_Pixbuf  : Gdk_Pixbuf;
      Src_X       : Gint;
      Src_Y       : Gint;
      Width       : Gint;
      Height      : Gint;
      Dest_Pixbuf : Gdk_Pixbuf;
      Dest_X      : Gint;
      Dest_Y      : Gint);
   --  Copy a rectangular area from Src_pixbuf to Dest_pixbuf.
   --  Conversion of pixbuf formats is done automatically.

   procedure Saturate_And_Pixelate
     (Src        : Gdk_Pixbuf;
      Dest       : Gdk_Pixbuf;
      Saturation : Gfloat;
      Pixelate   : Boolean := True);
   --  Brighten/darken and optionally make it pixelated-looking.

   ---------------
   -- Rendering --
   ---------------

   procedure Render_Threshold_Alpha
     (Pixbuf          : Gdk_Pixbuf;
      Bitmap          : Gdk.Bitmap.Gdk_Bitmap;
      Src_X           : Gint;
      Src_Y           : Gint;
      Dest_X          : Gint;
      Dest_Y          : Gint;
      Width           : Gint;
      Height          : Gint;
      Alpha_Threshold : Alpha_Range);
   --  Take the opacity values in a rectangular portion of a pixbuf and
   --  thresholds them to produce a bi-level alpha mask that can be used as
   --  a clipping mask for a drawable.
   --  Bitmap is the bitmap where the bilevel mask will be painted to.
   --  Alpha_Threshold are the opacity values below which a pixel will be
   --  painted as zero. All other values will be painted as one.

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
      Y_Dither : Gint := 0);
   --  Render a rectangular portion of a pixbuf to a drawable while using the
   --  specified GC. This is done using Gdk.RGB, so the specified drawable
   --  must have the Gdk.RGB visual and colormap.  Note that this function
   --  will ignore the opacity information for images with an alpha channel;
   --  the GC must already have the clipping mask set if you want transparent
   --  regions to show through.
   --
   --  For an explanation of dither offsets, see the Gdk.RGB documentation.  In
   --  brief, the dither offset is important when re-rendering partial regions
   --  of an image to a rendered version of the full image, or for when the
   --  offsets to a base position change, as in scrolling.  The dither matrix
   --  has to be shifted for consistent visual results.  If you do not have
   --  any of these cases, the dither offsets can be both zero.

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
      Y_Dither        : Gint := 0);
   --  Render a rectangular portion of a pixbuf to a drawable.
   --  This is done using Gdk.RGB, so the specified drawable must have the
   --  Gdk_RGB visual and colormap. When used with Alpha_Bilevel, this function
   --  has to create a bitmap out of the thresholded alpha channel of the
   --  image and, it has to set this bitmap as the clipping mask for the GC
   --  used for drawing.  This can be a significant performance penalty
   --  depending on the size and the complexity of the alpha channel of the
   --  image. If performance is crucial, consider handling the alpha channel
   --  yourself (possibly by caching it in your application) and using
   --  Render_To_Drawable or Gdk.RGB directly instead.
   --
   --  If the image does have opacity information and Alpha_Mode
   --  is Alpha_Bilevel, specifies the threshold value for opacity values

   procedure Render_Pixmap_And_Mask
     (Pixbuf          : Gdk_Pixbuf;
      Pixmap          : out Gdk.Pixmap.Gdk_Pixmap;
      Mask            : out Gdk.Bitmap.Gdk_Bitmap;
      Alpha_Threshold : Alpha_Range);
   procedure Render_Pixmap_And_Mask_For_Colormap
     (Pixbuf          : Gdk_Pixbuf;
      Colormap        : Gdk.Color.Gdk_Colormap;
      Pixmap          : out Gdk.Pixmap.Gdk_Pixmap;
      Mask            : out Gdk.Bitmap.Gdk_Bitmap;
      Alpha_Threshold : Alpha_Range);
   --  Creates a pixmap and a mask bitmap which are returned in the Pixmap
   --  and Mask arguments, respectively, and renders a pixbuf and its
   --  corresponding tresholded alpha mask to them.  This is merely a
   --  convenience function; applications that need to render pixbufs with
   --  dither offsets or to given drawables should use Render_To_Drawable_Alpha
   --  or Render_To_Drawable
   --  The pixmap that is created uses Colormap.
   --  This colormap must match the colormap of the window where the pixmap
   --  will eventually be used or an error will result.

   function Get_From_Drawable
     (Dest   : Gdk_Pixbuf;
      Src    : Gdk.Drawable.Gdk_Drawable;
      Cmap   : Gdk.Color.Gdk_Colormap;
      Src_X  : Gint;
      Src_Y  : Gint;
      Dest_X : Gint;
      Dest_Y : Gint;
      Width  : Gint;
      Height : Gint) return Gdk_Pixbuf;
   --  Transfer image data from a Gdk drawable and converts it to an RGB(A)
   --  representation inside a Gdk_Pixbuf.
   --
   --  If the drawable src is a pixmap, then a suitable colormap must be
   --  specified, since pixmaps are just blocks of pixel data without an
   --  associated colormap.
   --  If the drawable is a window, the Cmap argument will be ignored and the
   --  window's own colormap will be used instead.
   --
   --  If the specified destination pixbuf Dest is Null_Pixbuf, then this
   --  function will create an RGB pixbuf with 8 bits per channel and no
   --  alpha, with the same size specified by the Width and Height
   --  arguments. In this case, the Dest_x and Dest_y arguments must be
   --  specified as 0, otherwise the function will return Null_Pixbuf.  If the
   --  specified destination pixbuf is not Null_Pixbuf and it contains alpha
   --  information, then the filled pixels will be set to full opacity.
   --
   --  If the specified drawable is a pixmap, then the requested source
   --  rectangle must be completely contained within the pixmap, otherwise the
   --  function will return Null_Pixbuf.
   --
   --  If the specified drawable is a window, then it must be viewable, i.e.
   --  all of its ancestors up to the root window must be mapped.  Also, the
   --  specified source rectangle must be completely contained within the
   --  window and within the screen.  If regions of the window are obscured by
   --  non-inferior windows, the contents of those regions are undefined.
   --  The contents of regions obscured by inferior windows of a different
   --  depth than that of the source window will also be undefined.
   --
   --  Return value: The same pixbuf as Dest if it was non-NULL, or a
   --  newly-created pixbuf with a reference count of 1 if no destination
   --  pixbuf was specified.

   -------------
   -- Scaling --
   -------------

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
      Inter_Type   : Gdk_Interp_Type := Interp_Bilinear);
   --  Transform the source image by scaling by Scale_x and Scale_y then
   --  translating by Offset_x and Offset_y.
   --  The image is then rendered in the rectangle (Dest_x, Dest_y,
   --  Dest_width, Dest_height) of the resulting image onto the destination
   --  drawable replacing the previous contents.

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
      Overall_Alpha : Alpha_Range := 128);
   --  Transform the source image by scaling by Scale_X and Scale_Y then
   --  translating by Offset_X and Offset_Y, then composite the rectangle
   --  (Dest_X, Dest_Y, Dest_Width, Dest_Height) of the resulting image onto
   --  the destination drawable.

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
      Color2        : Guint32 := 0);
   --  Transform the source image by scaling by Scale_x and Scale_y then
   --  translating by Offset_x and Offset_y, then composites the rectangle
   --  (Dest_X, Dest_Y, Dest_Width, Dest_Height) of the resulting image with
   --  a checkboard of the colors Color1 and Color2 and renders it onto the
   --  destination drawable.
   --  The origin of checkboard is at (Check_x, Check_y)
   --  Color1 is the color at the upper left of the check.

   function Scale_Simple
     (Src           : Gdk_Pixbuf;
      Dest_Width    : Gint;
      Dest_Height   : Gint;
      Inter_Type    : Gdk_Interp_Type := Interp_Bilinear) return Gdk_Pixbuf;
   --  Scale the Src image to Dest_width x Dest_height and render the result
   --  into a new pixbuf.

   function Composite_Color_Simple
     (Src           : Gdk_Pixbuf;
      Dest_Width    : Gint;
      Dest_Height   : Gint;
      Inter_Type    : Gdk_Interp_Type := Interp_Bilinear;
      Overall_Alpha : Alpha_Range := 128;
      Color1        : Guint32 := 0;
      Color2        : Guint32 := 0) return Gdk_Pixbuf;
   --  Scale Src to Dest_width x Dest_height and composite the result with
   --  a checkboard of colors Color1 and Color2 and render the result into
   --  a new pixbuf.

   -----------------------
   -- Animation support --
   -----------------------

   function Get_Type_Animation return Glib.GType;
   --  Return the internal value associated with a Gdk_Pixbuf_Animation.

   procedure Gdk_New_From_File
     (Animation : out Gdk_Pixbuf_Animation;
      Filename  : String;
      Error     : out GError);
   --  Create a new animation by loading it from a file.
   --  The file format is detected automatically. If the file's format does not
   --  support multi-frame images, then an animation with a single frame will
   --  be created. Possible errors are in the Pixbuf_Error and GFile_Error
   --  domains.
   --  On return, Animation is a newly created animation with a reference count
   --  of 1, or null if any of several error conditions ocurred: the file could
   --  not be opened, there was no loader for the file's format, there was not
   --  enough memory to allocate the image buffer, or the image file contained
   --  invalid data.

   procedure Ref (Animation : Gdk_Pixbuf_Animation);
   --  Increment the reference counting on the animation.

   procedure Unref (Animation : Gdk_Pixbuf_Animation);
   --  Decrement the reference counting on the animation.

   function Get_Width (Animation : Gdk_Pixbuf_Animation) return Gint;
   --  Return the width of the bounding box of a pixbuf animation.

   function Get_Height (Animation : Gdk_Pixbuf_Animation) return Gint;
   --  Return the height of the bounding box of a pixbuf animation.

   function Is_Static_Image (Animation : Gdk_Pixbuf_Animation) return Boolean;
   --  If you load a file with Gdk_New_From_File and it turns out to be a
   --  plain, unanimated image, then this function will return True.
   --  Use Get_Static_Image to retrieve the image.

   function Get_Static_Image
     (Animation : Gdk_Pixbuf_Animation) return Gdk_Pixbuf;
   --  If an animation is really just a plain image (has only one frame),
   --  this function returns that image. If the animation is an animation,
   --  this function returns a reasonable thing to display as a static
   --  unanimated image, which might be the first frame, or something more
   --  sophisticated. If an animation hasn't loaded any frames yet, this
   --  function will return null.

   function Get_Iter
     (Animation  : Gdk_Pixbuf_Animation;
      Start_Time : GTime_Val_Access := null)
      return Gdk_Pixbuf_Animation_Iter;
   --  Get an iterator for displaying an animation. The iterator provides
   --  the frames that should be displayed at a given time.
   --  It should be freed after use with Unref.
   --
   --  Start_Time would normally come from G_Get_Current_Time, and marks the
   --  beginning of animation playback. After creating an iterator, you should
   --  immediately display the pixbuf returned by Get_Pixbuf. Then, you should
   --  install a timeout (with Timeout_Add) or by some other mechanism to
   --  ensure that you'll update the image after Get_Delay_Time milliseconds.
   --  Each time the image is updated, you should reinstall the timeout with
   --  the new, possibly-changed delay time.
   --
   --  As a shortcut, if Start_Time is equal to null, the result of
   --  G_Get_Current_Time will be used automatically.
   --
   --  To update the image (i.e. possibly change the result of Get_Pixbuf to a
   --  new frame of the animation), call Advance.
   --
   --  If you're using Gdk_Pixbuf_Loader, in addition to updating the image
   --  after the delay time, you should also update it whenever you
   --  receive the area_updated signal and On_Currently_Loading_Frame returns
   --  True. In this case, the frame currently being fed into the loader
   --  has received new data, so needs to be refreshed. The delay time for
   --  a frame may also be modified after an area_updated signal, for
   --  example if the delay time for a frame is encoded in the data after
   --  the frame itself. So your timeout should be reinstalled after any
   --  area_updated signal.
   --
   --  A delay time of -1 is possible, indicating "infinite."

   ---------------
   -- Iterators --
   ---------------

   function Get_Type_Animation_Iter return Glib.GType;
   --  Return the internal value associated with a Gdk_Pixbuf_Animation_Iter.

   procedure Ref (Iter : Gdk_Pixbuf_Animation_Iter);
   --  Increment the reference counting on the iterator.

   procedure Unref (Iter : Gdk_Pixbuf_Animation_Iter);
   --  Decrement the reference counting on the iterator.

   function Get_Delay_Time (Iter : Gdk_Pixbuf_Animation_Iter) return Gint;
   --  Return the number of milliseconds the current pixbuf should be displayed
   --  or -1 if the current pixbuf should be displayed forever. Timeout_Add
   --  conveniently takes a timeout in milliseconds, so you can use a timeout
   --  to schedule the next update.

   function Get_Pixbuf (Iter : Gdk_Pixbuf_Animation_Iter) return Gdk_Pixbuf;
   --  Return the current pixbuf which should be displayed.
   --  The pixbuf will be the same size as the animation itself (Get_Width,
   --  Get_Height). This pixbuf should be displayed for Get_Delay_Time
   --  milliseconds. The caller of this function does not own a reference to
   --  the returned pixbuf; the returned pixbuf will become invalid when the
   --  iterator advances to the next frame, which may happen anytime you call
   --  Advance. Copy the pixbuf to keep it (don't just add a reference), as it
   --  may get recycled as you advance the iterator.

   function On_Currently_Loading_Frame
     (Iter : Gdk_Pixbuf_Animation_Iter) return Boolean;
   --  Used to determine how to respond to the area_updated signal on
   --  Gdk_Pixbuf_Loader when loading an animation. area_updated is emitted
   --  for an area of the frame currently streaming in to the loader. So if
   --  you're on the currently loading frame, you need to redraw the screen for
   --  the updated area.

   function Advance
     (Iter          : Gdk_Pixbuf_Animation_Iter;
      Current_Timer : GTime_Val_Access := null) return Boolean;
   --  Possibly advance an animation to a new frame.
   --  Chooses the frame based on the start time passed to Get_Iter.
   --
   --  Current_Time would normally come from G_Get_Current_Time, and
   --  must be greater than or equal to the time passed to Get_Iter,
   --  and must increase or remain unchanged each time Get_Pixbuf is
   --  called. That is, you can't go backward in time; animations only
   --  play forward.
   --
   --  As a shortcut, pass null for the current time and G_Get_Current_Time
   --  will be invoked on your behalf. So you only need to explicitly pass
   --  Current_Time if you're doing something odd like playing the animation
   --  at double speed.
   --
   --  If this function returns False, there's no need to update the animation
   --  display, assuming the display had been rendered prior to advancing;
   --  if True, you need to call Get_Pixbuf and update the display with the new
   --  pixbuf.

   -------------
   -- Cursors --
   -------------

   procedure Gdk_New_From_Pixbuf
     (Cursor  : out Gdk.Cursor.Gdk_Cursor;
      Display : Gdk.Display.Gdk_Display := Gdk.Display.Get_Default;
      Pixbuf  : Gdk_Pixbuf;
      X       : Glib.Gint;
      Y       : Glib.Gint);
   --  Create a cursor from a pixbuf.
   --  Not all GDK backends support RGBA cursors. If they are not supported,
   --  a monochrome approximation will be displayed.
   --  The functions gdk.display.supports_cursor_alpha and
   --  gdk.display.supports_cursor_color can be used to determine whether RGBA
   --  cursors are supported;
   --  gdk.display.get_default_cursor_size and
   --  gdk.display.get_maximal_cursor_size give information about cursor sizes.
   --  On the X backend, support for RGBA cursors requires a sufficently new
   --  version of the X Render extension.

   function Get_Image (Cursor : Gdk.Cursor.Gdk_Cursor) return Gdk_Pixbuf;
   --  Return the image stored in the cursor

   --  <doc_ignore>
   function Convert (P : System.Address) return Gdk_Pixbuf;
   --  </doc_ignore>

private

   type Gdk_Pixbuf_Record is new Glib.Object.GObject_Record with null record;

   pragma Import (C, Get_Type, "gdk_pixbuf_get_type");
   pragma Import (C, Get_Type_Animation, "gdk_pixbuf_animation_get_type");
   pragma Import
     (C, Get_Type_Animation_Iter, "gdk_pixbuf_animation_iter_get_type");
   pragma Import (C, Get_Iter, "gdk_pixbuf_animation_get_iter");
   pragma Import
     (C, Get_Delay_Time, "gdk_pixbuf_animation_iter_get_delay_time");

end Gdk.Pixbuf;
