package Neo.File.Image.Texture is
    type Enumerated_Format is( -- "ASTC, PVRTC, ETC1, ETC2, ATC, DXT -- collect them all!"
      S3_Texture_Compression_Format,
      -- 1998 S3 Graphics
      --      http://web.archive.org/web/20030618083605/www.hardwarecentral.com/hardwarecentral/reports/140/1/
      --      (NON-ARCHIVED LINK!) http://www.gamedev.no/projects/MegatextureCompression/324337_324337.pdf
      Valve_Texture_Format,
      -- 2003 Valve Softare
      --      http://web.archive.org/web/20130421141633/https://developer.valvesoftware.com/wiki/Valve_Texture_Format
      PowerVR_Texture_Compression_Format,
      -- 2003 Imagination Technologies
      --      http://web.archive.org/web/20121130180454/http://web.onetel.net.uk/~simonnihal/assorted3d/fenney03texcomp.pdf
      Ericsson_Texture_Compression_1_Format,
      -- 2005 Ericsson Research
      --      (NON-ARCHIVED LINK!) https://code.google.com/p/rg-etc1/
      Ericsson_Texture_Compression_2_Format,
      -- 2007 Ericsson Research
      --      http://web.archive.org/web/20080908035259/http://www.graphicshardware.org/previous/www_2007/presentations/strom-etc2-gh07.pdf
      Android_Texture_Compression_Format,
      -- 2008 Google
      --      http://www.guildsoftware.com/papers/2012.Converting.DXTC.to.ATC.pdf
      Adaptive_Scalable_Texture_Compression_Format);
      -- 2012 ARM
      --      (NON-ARCHIVED LINK!) http://paper.ustor.cn/read/a_i%3D516a503bc624e810046edbac/
    type Record_Texture(Format : Enumerated_Format; Data_Length : in Integer_4_Positive) is record
        Data : Array_Integer_1_Unsigned(1..Data_Length) := (others => 0); -- Dont decode data if the graphics supports direct loading of the compressed format
      end record;
    type Array_Record_Pixel   is array(Positive range <>, Positive range <>)  of Record_Pixel;
    type Array_Record_Graphic is array(Positive range <>);
    procedure Test;
    procedure Save (Path : in String_2; Graphic : in Record_Texture);
  end Neo.File.Image.Texture;
