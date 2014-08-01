---------------------------------
-- GID - Generic Image Decoder --
---------------------------------
--
--  Copyright (c) Gautier de Montmollin 2010
--
--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.
--
-- NB: this is the MIT License, as found 2-May-2010 on the site
-- http://www.opensource.org/licenses/mit-license.php

with GID.Headers,
     GID.Decoding_BMP,
     GID.Decoding_GIF,
     GID.Decoding_JPG,
     GID.Decoding_PNG,
     GID.Decoding_TGA;

with Ada.Unchecked_Deallocation;

package body GID is

  -----------------------
  -- Load_image_header --
  -----------------------

  procedure Load_image_header (
    image   :    out Image_descriptor;
    from    : in out Ada.Streams.Root_Stream_Type'Class;
    try_tga :        Boolean:= False
  )
  is
  begin
    image.stream:= from'Unchecked_Access;
    Headers.Load_signature(image, try_tga);
    case image.format is
      when BMP =>
        Headers.Load_BMP_header(image);
      when FITS =>
        Headers.Load_FITS_header(image);
      when GIF =>
        Headers.Load_GIF_header(image);
      when JPEG =>
        Headers.Load_JPEG_header(image);
      when PNG =>
        Headers.Load_PNG_header(image);
      when TGA =>
        Headers.Load_TGA_header(image);
      when TIFF =>
        Headers.Load_TIFF_header(image);
    end case;
  end Load_image_header;

  -----------------
  -- Pixel_width --
  -----------------

  function Pixel_width (image: Image_descriptor) return Positive is
  begin
    return image.width;
  end Pixel_width;

  ------------------
  -- Pixel_height --
  ------------------

  function Pixel_height (image: Image_descriptor) return Positive is
  begin
    return image.height;
  end Pixel_height;

  function Display_orientation (image: Image_descriptor) return Orientation is
  begin
    return image.display_orientation;
  end Display_orientation;

  -------------------------
  -- Load_image_contents --
  -------------------------

  procedure Load_image_contents (
    image     : in out Image_descriptor;
    next_frame:    out Ada.Calendar.Day_Duration
  )
  is
    procedure BMP_Load is
      new Decoding_BMP.Load( Primary_color_range, Set_X_Y, Put_Pixel, Feedback );

    procedure GIF_Load is
      new Decoding_GIF.Load( Primary_color_range, Set_X_Y, Put_Pixel, Feedback, mode );

    procedure JPG_Load is
      new Decoding_JPG.Load( Primary_color_range, Set_X_Y, Put_Pixel, Feedback );

    procedure PNG_Load is
      new Decoding_PNG.Load( Primary_color_range, Set_X_Y, Put_Pixel, Feedback );

    procedure TGA_Load is
      new Decoding_TGA.Load( Primary_color_range, Set_X_Y, Put_Pixel, Feedback );

  begin
    next_frame:= 0.0;
    -- ^ value updated in case of animation and when
    --   current frame is not the last frame
    case image.format is
      when BMP =>
        BMP_Load(image);
      when GIF =>
        GIF_Load(image, next_frame);
      when JPEG =>
        JPG_Load(image, next_frame);
      when PNG =>
        PNG_Load(image);
      when TGA =>
        TGA_Load(image);
      when others =>
        raise known_but_unsupported_image_format;
    end case;
  end Load_image_contents;

  ---------------------------------------
  -- Some informations about the image --
  ---------------------------------------

  function Format (image: Image_descriptor) return Image_format_type is
  begin
    return image.format;
  end Format;

  function Detailed_format (image: Image_descriptor) return String is
  begin
    return Bounded_255.To_String(image.detailed_format);
  end Detailed_format;

  function Subformat (image: Image_descriptor) return Integer is
  begin
    return image.subformat_id;
  end Subformat;

  function Bits_per_pixel (image: Image_descriptor) return Positive is
  begin
    return image.bits_per_pixel;
  end Bits_per_pixel;

  function RLE_encoded (image: Image_descriptor) return Boolean is
  begin
    return image.RLE_encoded;
  end RLE_encoded;

  function Interlaced (image: Image_descriptor) return Boolean is
  begin
    return image.interlaced;
  end Interlaced;

  function Greyscale (image: Image_descriptor) return Boolean is
  begin
    return image.greyscale;
  end Greyscale;

  function Has_palette (image: Image_descriptor) return Boolean is
  begin
    return image.palette /= null;
  end Has_palette;

  function Expect_transparency (image: Image_descriptor) return Boolean is
  begin
    return image.transparency;
  end Expect_transparency;

  procedure Adjust (Object : in out Image_descriptor) is
  begin
    -- Clone the palette
    Object.palette:= new Color_table'(Object.palette.all);
  end Adjust;

  procedure Finalize (Object : in out Image_descriptor) is
    procedure Dispose is
      new Ada.Unchecked_Deallocation(Color_table, p_Color_table);
    procedure Dispose is
      new Ada.Unchecked_Deallocation(
        JPEG_defs.VLC_table,
        JPEG_defs.p_VLC_table
      );
  begin
    -- Deterministic garbage collection
    Dispose(Object.palette);
    for ad in JPEG_defs.VLC_defs_type'Range(1) loop
      for idx in JPEG_defs.VLC_defs_type'Range(2) loop
        Dispose(Object.JPEG_stuff.vlc_defs(ad, idx));
      end loop;
    end loop;
  end Finalize;

end GID;
