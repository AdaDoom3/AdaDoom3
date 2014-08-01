private package GID.Decoding_PNG is

  type PNG_Chunk_tag is (
    --
    -- Critical chunks
    --
    IHDR, --    must be the first chunk; it contains the header.
    PLTE, --    contains the palette; list of colors.
    IDAT, --    contains the image, which may be split among multiple IDAT chunks.
    IEND, --    marks the image end.
    --
    -- Ancillary chunks
    --
    bKGD, --    gives the default background color.
    cHRM, --    gives the chromaticity coordinates of the display primaries and white point.
    gAMA, --    specifies gamma.
    hIST, --    can store the histogram, or total amount of each color in the image.
    iCCP, --    is an ICC color profile.
    iTXt, --    contains UTF-8 text, compressed or not, with an optional language tag.
    pHYs, --    holds the intended pixel size and/or aspect ratio of the image.
    sBIT, --    (significant bits) indicates the color-accuracy of the source data.
    sPLT, --    suggests a palette to use if the full range of colors is unavailable.
    sRGB, --    indicates that the standard sRGB color space is used.
    tEXt, --    can store text that can be represented in ISO/IEC 8859-1.
    tIME, --    stores the time that the image was last changed.
    tRNS, --    contains transparency information.
    zTXt, --    contains compressed text with the same limits as tEXt.
    --
    -- Public extentions
    -- PNG Extensions and Register of Public Chunks and Keywords
    --
    oFFs, -- image offset from frame or page origin
    pCAL, -- physical calibration of pixel values
    sCAL, -- physical scale of image subject
    sTER, -- stereographic subimage layout
    gIFg, -- GIF Graphic Control Extension
    gIFx, -- GIF Application Extension
    fRAc, -- fractal image parameters
    --
    -- Private chunks (not defined in the ISO standard)
    --
    vpAg, --    used in ImageMagick to store "virtual page" size
    spAL,
    prVW,
    cmOD,
    cmPP,
    cpIp,
    mkBF,
    mkBS,
    mkBT,
    mkTS,
    pcLb
  );

  type Chunk_head is record
    length: U32;
    kind  : PNG_Chunk_tag;
  end record;

  procedure Read( image: in out image_descriptor; ch: out Chunk_head);

  --------------------
  -- Image decoding --
  --------------------

  generic
    type Primary_color_range is mod <>;
    with procedure Set_X_Y (x, y: Natural);
    with procedure Put_Pixel (
      red, green, blue : Primary_color_range;
      alpha            : Primary_color_range
    );
    with procedure Feedback (percents: Natural);
  --
  procedure Load (image: in out Image_descriptor);

end GID.Decoding_PNG;
