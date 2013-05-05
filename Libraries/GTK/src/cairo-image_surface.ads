-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                    Copyright (C) 2010-2013, AdaCore               --
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
--  Image surfaces provide the ability to render to memory buffers either
--  allocated by Cairo or by the calling code. The supported image formats are
--  those defined in Cairo_Format.
--  </description>
--
--  <c_version>1.8.8</c_version>
--  <group>Cairo</group>

package Cairo.Image_Surface is

   --  Cairo_Format is used to identify the memory format of
   --  image data.
   --
   --  New entries may be added in future versions.
   type Cairo_Format is
     (Cairo_Format_ARGB32,
      --  Each pixel is a 32-bit quantity, with
      --  alpha in the upper 8 bits, then red, then green, then blue.
      --  The 32-bit quantities are stored native-endian. Pre-multiplied
      --  alpha is used. (That is, 50 transparent red is 0x80800000,
      --  not 0x80ff0000.)

      Cairo_Format_RGB24,
      --  Each pixel is a 32-bit quantity, with
      --  the upper 8 bits unused. Red, Green, and Blue are stored
      --  in the remaining 24 bits in that order.

      Cairo_Format_A8,
      --  Each pixel is a 8-bit quantity holding an alpha value.

      Cairo_Format_A1,
      --  Each pixel is a 1-bit quantity holding
      --  an alpha value. Pixels are packed together into 32-bit
      --  quantities. The ordering of the bits matches the
      --  endianess of the platform. On a big-endian machine, the
      --  first pixel is in the uppermost bit, on a little-endian
      --  machine the first pixel is in the least-significant bit.

      CAIRO_FORMAT_RGB16_565_Deprecated_Do_Not_Use
      --  This value is deprecated
     );
   pragma Convention (C, Cairo_Format);

   type Byte is range 0 .. 255;

   type ARGB32_Data is record
      Alpha : Byte;
      Red   : Byte;
      Green : Byte;
      Blue  : Byte;
   end record;
   --  One pixel in ARGB32 format

   type RGB24_Data is record
      Red   : Byte;
      Green : Byte;
      Blue  : Byte;
   end record;
   --  One pixel in RGB24 format

   type ARGB32_Array is array (Natural range <>) of ARGB32_Data;
   type ARGB32_Array_Access is access ARGB32_Array;

   type RGB24_Array is array (Natural range <>) of RGB24_Data;
   type RGB24_Array_Access is access RGB24_Array;

   type Byte_Array is array (Natural range <>) of Byte;
   type Byte_Array_Access is access Byte_Array;

   function Create
     (Format : Cairo_Format;
      Width  : Gint;
      Height : Gint)
      return   Cairo_Surface;
   --  Format: Format of pixels in the surface to create
   --  Width: Width of the surface, in pixels
   --  Height: Height of the surface, in pixels
   --
   --  Creates an image surface of the specified format and
   --  dimensions. Initially the surface contents are all
   --  0. (Specifically, within each pixel, each color or alpha channel
   --  belonging to format will be 0. The contents of bits within a pixel,
   --  but not belonging to the given format are undefined).
   --
   --  Return value: a pointer to the newly created surface. The caller
   --  owns the surface and should call Cairo.Surface.Destroy when done
   --  with it.
   --
   --  This function always returns a valid pointer, but it will return a
   --  pointer to a "nil" surface if an error such as out of memory
   --  occurs. You can use Cairo.Surface.Status to check for this.

   function Cairo_Format_Stride_For_Width
     (Format : Cairo_Format;
      Width  : Gint)
      return   Gint;
   --  Format: A Cairo_Format value
   --  Width: The desired Width of an image surface to be created.
   --
   --  This function provides a stride value that will respect all
   --  alignment requirements of the accelerated image-rendering code
   --  within cairo.
   --
   --  Return value: the appropriate stride to use given the desired
   --  format and width, or -1 if either the format is invalid or the width
   --  too large.
   --
   --  Since: 1.6

   function Create_For_Data_Generic
     (Data   : System.Address;
      Format : Cairo_Format;
      Width  : Gint;
      Height : Gint;
      Stride : Gint)
      return   Cairo_Surface;
   --  Data: a pointer to a buffer supplied by the application in which
   --      to write contents. This pointer must be suitably aligned for any
   --      kind of variable, (for example, a pointer returned by malloc).
   --  Format: the Format of pixels in the buffer
   --  Width: the Width of the image to be stored in the buffer
   --  Height: the Height of the image to be stored in the buffer
   --  Stride: the number of bytes between the start of rows in the
   --      buffer as allocated. This value should always be computed by
   --      Cairo_Format_Stride_For_Width before allocating the data
   --      buffer.
   --
   --  This function is a low-level binding to the C function: see also
   --  Create_For_Data_[ARGB32|RGB24|A8|A1]
   --
   --  Creates an image surface for the provided pixel data. The output
   --  buffer must be kept around until the Cairo_Surface is destroyed
   --  or Cairo.Surface.Finish is called on the surface.  The initial
   --  contents of data will be used as the initial image contents; you
   --  must explicitly clear the buffer, using, for example,
   --  Cairo.Rectangle and Cairo.Fill if you want it cleared.
   --
   --  Note that the stride may be larger than Width*bytes_per_pixel to provide
   --  proper alignment for each pixel and row. This alignment is required to
   --  allow high-performance rendering within cairo. The correct way to obtain
   --  a legal stride value is to call Cairo_Format_Stride_For_Width with the
   --  desired format and maximum image width value, and the use the resulting
   --  stride value to allocate the data and to create the image surface. See
   --  Cairo_Format_Stride_For_Width for example code.
   --
   --  Return value: a pointer to the newly created surface. The caller
   --  owns the surface and should call Cairo.Surface.Destroy when done
   --  with it.
   --
   --  This function always returns a valid pointer, but it will return a
   --  pointer to a "nil" surface in the case of an error such as out of
   --  memory or an invalid stride value. In case of invalid stride value
   --  the error status of the returned surface will be
   --  Cairo_Status_Invalid_Stride.  You can use
   --  Cairo.Surface.Status to check for this.
   --
   --  See Cairo.Surface.Set_User_Data for a means of attaching a
   --  destroy-notification fallback to the surface if necessary.

   function Create_For_Data_ARGB32
     (Data   : ARGB32_Array_Access;
      Width  : Gint;
      Height : Gint)
      return   Cairo_Surface;
   --  Same as above, working on ARGB32 format.

   function Create_For_Data_RGB24
     (Data   : RGB24_Array_Access;
      Width  : Gint;
      Height : Gint)
      return   Cairo_Surface;
   --  Same as above, working on RGB24 format.

   function Create_For_Data_A8
     (Data   : Byte_Array_Access;
      Width  : Gint;
      Height : Gint)
      return   Cairo_Surface;
   --  Same as above, working on A8 format.

   function Get_Data_Generic (Surface : Cairo_Surface) return System.Address;
   --  Surface: a Cairo_Image_Surface
   --
   --  Get a pointer to the data of the image surface, for direct
   --  inspection or modification.
   --
   --  Return value: a pointer to the image data of this surface or
   --  System.Null_Address if surface is not an image surface, or if
   --  Cairo.Surface.Finish has been called.
   --
   --  Since: 1.2

   function Get_Format (Surface : Cairo_Surface) return Cairo_Format;
   --  Surface: a Cairo_Image_Surface
   --
   --  Get the format of the surface.
   --
   --  Return value: the format of the surface
   --
   --  Since: 1.2

   function Get_Width (Surface : Cairo_Surface) return Gint;
   --  Surface: a Cairo_Image_Surface
   --
   --  Get the width of the image surface in pixels.
   --
   --  Return value: the width of the surface in pixels.

   function Get_Height (Surface : Cairo_Surface) return Gint;
   --  Surface: a Cairo_Image_Surface
   --
   --  Get the height of the image surface in pixels.
   --
   --  Return value: the height of the surface in pixels.

   function Get_Stride (Surface : Cairo_Surface) return Gint;
   --  Surface: a Cairo_Image_Surface
   --
   --  Get the stride of the image surface in bytes
   --
   --  Return value: the stride of the image surface in bytes (or 0 if
   --  surface is not an image surface). The stride is the distance in
   --  bytes from the beginning of one row of the image data to the
   --  beginning of the next row.
   --
   --  Since: 1.2

private

   for Byte'Size use 8;

   --  Representation working with all endiannesses

   BOP  : constant := System.Bit_Order'Pos (System.Default_Bit_Order);
   NBOP : constant := 1 - BOP;

   for ARGB32_Data use record
      Alpha at BOP * 3 + NBOP * 0 range 0 .. 7;
      Red   at BOP * 2 + NBOP * 1 range 0 .. 7;
      Green at BOP * 1 + NBOP * 2 range 0 .. 7;
      Blue  at BOP * 0 + NBOP * 3 range 0 .. 7;
   end record;

   for RGB24_Data use record
      Red   at BOP * 2 + NBOP * 1 range 0 .. 7;
      Green at BOP * 1 + NBOP * 2 range 0 .. 7;
      Blue  at BOP * 0 + NBOP * 3 range 0 .. 7;
   end record;
   for RGB24_Data'Size use 32;

   pragma Import (C, Create, "cairo_image_surface_create");
   pragma Import
     (C, Cairo_Format_Stride_For_Width, "cairo_format_stride_for_width");

   pragma Import
     (C, Create_For_Data_Generic, "cairo_image_surface_create_for_data");

   pragma Import (C, Get_Data_Generic, "cairo_image_surface_get_data");
   pragma Import (C, Get_Format, "cairo_image_surface_get_format");
   pragma Import (C, Get_Width, "cairo_image_surface_get_width");
   pragma Import (C, Get_Height, "cairo_image_surface_get_height");
   pragma Import (C, Get_Stride, "cairo_image_surface_get_stride");

end Cairo.Image_Surface;
