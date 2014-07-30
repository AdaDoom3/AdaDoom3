--
-- Color tables, known as "palettes"
--

private package GID.Color_tables is

  -- Load a palette on its defined range, according to
  -- the format and subformats loaded by initial
  -- steps in GID.Load_image_header
  procedure Load_palette (image: in out Image_descriptor);
  -- if image.palette = null, nothing happens.

  -- Convert a RGB value packed in 2 bytes
  -- (15 bit, 5 bit each channel) into a RGB_Color
  -- This is for the TGA format.
  procedure Convert(c, d: in U8; rgb: out RGB_Color);

end GID.Color_tables;
