package Neo.File.Image is
    type Enumerated_Format is( -- "ASTC, PVRTC, ETC1, ETC2, ATC, DXT -- collect them all!"
      -- 1981 International Astronomical Union Flexible Image Transport System Working Group
      --      http://web.archive.org/web/20130215103213/http://fits.gsfc.nasa.gov/standard30/fits_standard30aa.pdf
      Truevision_Graphics_Adapter_Format,
      -- 1984 Truevision
      --      http://web.archive.org/web/20130206052029/http://www.dca.fee.unicamp.br/~martino/disciplinas/ea978/tgaffs.pdf
      Personal_Computer_Exchange_Format,
      -- 1985 ZSoft
      --      http://web.archive.org/web/20100206055706/http://www.qzx.com/pc-gpe/pcx.txt
      Graphics_Interchange_Format,
      -- 1985 CompuServe Information Service, now a subsidiary of America Online
      --      http://web.archive.org/web/20130426053329/http://www.w3.org/Graphics/GIF/spec-gif89a.txt
      Tagged_Image_File_Format,
      -- 1986 Aldus
      --      http://web.archive.org/web/20130430213645/http://partners.adobe.com/public/developer/en/tiff/TIFF6.pdf
      Portable_Pixmap_Format,
      -- 1988 Jef Poskanzer
      --      http://web.archive.org/web/20130517063911/http://netpbm.sourceforge.net/doc/pbm.html
      X_Pixmap_Format,
      -- 1989 Daniel Dardailler and Colas Nahaboo
      --      http://web.archive.org/web/20070227101113/http://www.net.uom.gr/Books/Manuals/xpm-3-paper.pdf
      Bit_Map_Format,
      -- 1990 Microsoft
      --      http://web.archive.org/web/20130424130243/http://en.wikipedia.org/wiki/BMP_file_format
      Joint_Photographic_Experts_Group_Format,
      -- 1992 Joint Photographic Experts Group and Joint Bi-level Image Experts Group
      --      http://web.archive.org/web/20111226041637/http://white.stanford.edu/~brian/psy221/reader/Wallace.JPEG.pdf
      Portable_Network_Graphics_Format,
      -- 1996 Internet Engineering Steering Group
      --      http://web.archive.org/web/20130116130737/http://libpng.org/pub/png/spec/iso/
      S3_Texture_Compression_Format,
      -- 1998 S3 Graphics
      --      http://web.archive.org/web/20030618083605/www.hardwarecentral.com/hardwarecentral/reports/140/1/
      --      (NON-ARCHIVED) http://www.gamedev.no/projects/MegatextureCompression/324337_324337.pdf
      Joint_Photographic_Experts_Group_2000_Format,
      -- 2000 Joint Photographic Experts Group and Joint Bi-level Image Experts Group
      --      http://web.archive.org/web/20130314150613/http://jpeg.org/public/fcd15444-1.pdf
      --      http://web.archive.org/web/20120117162132/http://www.mast.queensu.ca/~web/Papers/lui-project01.pdf
      Valve_Texture_Format,
      -- 2003 Valve Softare
      --      http://web.archive.org/web/20130421141633/https://developer.valvesoftware.com/wiki/Valve_Texture_Format
      PowerVR_Texture_Compression_Format,
      -- 2003 Imagination Technologies
      --      http://web.archive.org/web/20121130180454/http://web.onetel.net.uk/~simonnihal/assorted3d/fenney03texcomp.pdf
      Ericsson_Texture_Compression_1_Format,
      -- 2005 Ericsson Research
      --      (NON-ARCHIVED) https://code.google.com/p/rg-etc1/
      Ericsson_Texture_Compression_2_Format,
      -- 2007 Ericsson Research
      --      http://web.archive.org/web/20080908035259/http://www.graphicshardware.org/previous/www_2007/presentations/strom-etc2-gh07.pdf
      Android_Texture_Compression_Format,
      -- 2008 Google
      --      http://www.guildsoftware.com/papers/2012.Converting.DXTC.to.ATC.pdf
      Adaptive_Scalable_Texture_Compression_Format);
      -- 2012 ARM
      --      (NON-ARCHIVED) http://paper.ustor.cn/read/a_i%3D516a503bc624e810046edbac/
      Flexible_Image_Transport_System_Format,
    type Enumerated_Colors is (Monochrome_Colors, Eight_Colors, Sixteen_Colors, Two_Hundred_Fifty_Six_Per_Colors, Two_Hundred_Fifty_Six_With_Alpha_Per_Colors);
    for Enumerated_Colors use (1, Byte'size / 2, Byte'size,  Byte'size * 2 - 1, Byte'size * 2, Byte'size * 3, Byte'size * 4); -- Its value is the bits per pixel in each color scheme
    type Array_Record_Pixel;
    type Record_Pixel is record
        Color : Record_Color       := (others => <>);
        Alpha : Integer_1_Unsigned := Byte'last;
      end record;
    type Record_Graphic(Format : Enumerated_Format := Graphics_Interchange_Format; Width, Height : Integer_4_Positive) is record
        Pixels : Array_Record_Pixel(1..Width, 1..Height) := (others => <>);
        Colors : Enumerated_Colors                       := Alpha_With_Two_Hundred_Fifty_Six_Per_Colors;
        case Format is
          when Graphics_Interchange_Format =>
            Until_Next : Day_Duration := 0.0;
          when Truevision_Graphics_Adapter_Format =>
            Do_Encode  : Boolean      := False;
          when Joint_Photographic_Experts_Group_Format =>
            Brightness : Boolean      := False;
            Hue        : Boolean      := False;
            Saturation : Boolean      := False;
            Inphase    : Boolean      := False;
            Quadrature : Boolean      := False;
        when others => null; end case;
      end record;
    type Array_Record_Pixel   is array(Positive range <>, Positive range <>)  of Record_Pixel;
    type Array_Record_Graphic is array(Positive range <>);
    procedure Test;
    procedure Save               (Path : in String_2; Graphic : in Record_Graphic);
    procedure Save               (Path : in String_2; Graphic : in Array_Record_Graphic);
    function Load                (Path : in String_2)          return Record_Graphic;
    function Load                (Path : in String_2)          return Array_Record_Graphic;
    function Process_Drop_Sample (Graphic : in Record_Graphic) return Record_Graphic;
    function Resample            (Graphic : in Record_Graphic) return Record_Graphic;
    function Flip_Horizontally   (Graphic : in Record_Graphic) return Record_Graphic;
    function Flip_Vertically     (Graphic : in Record_Graphic) return Record_Graphic;
    function Rotate              (Graphic : in Record_Graphic) return Record_Graphic;
    function Convert             (Graphic : in Record_Graphic; To : in Enumerated_Format) return Stream_Element_Array with pre => Graphic.Format /= To;
    function Process_Mip_Map     (Graphic : in Record_Graphic; Do_Use_Alpha_Specularity, Do_Use_Gamma : in Boolean := False) return Record_Graphic;
    function Blend_Over          (Graphic : in Record_Graphic; Pixel_Count : in Integer_4_Positive; Blend : in Array_Integer_1_Unsigned) return Record_Graphic;
private
    FORMATS : Array_Record_Format(Enumerated_Format'range) :=(      -- Extensions                                          -- Signatures
      Personal_Computer_Exchange_Format            => Create_Format(("PCX")),
      Truevision_Graphics_Adapter_Format           => Create_Format(("TGA", "TPIC", "TARGA")),
      Portable_Pixmap_Format                       => Create_Format(("PPM", "PGM", "PBM", "PNM"),                          ("P1","P2","P3","P4","P5","P6")),
      Flexible_Image_Transport_System_Format       => Create_Format(("FTS", "FITS"),                                       ("SIMPLE"));
      Graphics_Interchange_Format                  => Create_Format(("GIF", "GIFF"),                                       ("GIF87a", "GIF89a")),
      Tagged_Image_File_Format                     => Create_Format(("TIF", "TIFF"),                                       ("II","MM")),
      Bit_Map_Format                               => Create_Format(("BMP", "DIB"),                                        ("DIB", "BM")),
      Joint_Photographic_Experts_Group_Format      => Create_Format(("JPG", "JPEG", "JPE", "JIF", "JFIF", "JFI", "JIFF")), (Character_1'val(16#FF#) & Character_1'val(16#D8#))),                             -- ÿØ
      Joint_Photographic_Experts_Group_2000_Format => Create_Format(("JP2", "J2K", "JPF", "JPX", "JPM", "MJ2"),            (ASCII.NUL & ASCII.NUL & ASCII.NUL & Character_1'val(16#FF#) & "jP"))));          -- jP
      Portable_Network_Graphics_Format             => Create_Format(("PNG"),                                               (Character_1'val(16#89#) & "PNG" & ASCII.CR & ASCII.LF & ASCII.SUB & ASCII.LF))), -- ‰PNG
    package FTS is
        function Load(Path : in String_2) return Array_Record_Graphic;
        procedure Save(Path : in String_2; Graphic : in Array_Record_Graphic);
      end FTS;
    package PCX is
        function Load(Path : in String_2) return Array_Record_Graphic;
        procedure Save(Path : in String_2; Graphic : in Array_Record_Graphic);
      end PCX;
    package BMP is
        function Load(Path : in String_2) return Array_Record_Graphic;
        procedure Save(Path : in String_2; Graphic : in Array_Record_Graphic);
      end BMP;
    package TIF is
        function Load(Path : in String_2) return Array_Record_Graphic;
        procedure Save(Path : in String_2; Graphic : in Array_Record_Graphic);
      end TIF;
    package GIF is
        function Load(Path : in String_2) return Array_Record_Graphic;
        procedure Save(Path : in String_2; Graphic : in Array_Record_Graphic);
      end GIF;
    package PPM is
        function Load(Path : in String_2) return Array_Record_Graphic;
        procedure Save(Path : in String_2; Graphic : in Array_Record_Graphic);
      end PPM;
    package XPM is
        function Load(Path : in String_2) return Array_Record_Graphic;
        procedure Save(Path : in String_2; Graphic : in Array_Record_Graphic);
      end XPM;
    package PNG is
        function Load(Path : in String_2) return Array_Record_Graphic;
        procedure Save(Path : in String_2; Graphic : in Array_Record_Graphic);
      end PNG;
    package TGA is
        function Load(Path : in String_2) return Array_Record_Graphic;
        procedure Save(Path : in String_2; Graphic : in Array_Record_Graphic);
      end TGA;
    package JPG is
        function Load(Path : in String_2) return Array_Record_Graphic;
        procedure Save(Path : in String_2; Graphic : in Array_Record_Graphic);
      end JPEG;
    package JP2 is
        function Load(Path : in String_2) return Array_Record_Graphic;
        procedure Save(Path : in String_2; Graphic : in Array_Record_Graphic);
      end JP2;
    package Instantiation is new Handler(
      Type_To_Handle => Neo.File.Image.Record_Graphic,
      Format         => Neo.File.Image.Enumerated_Format,
      Formats        => Neo.File.Image.FORMATS
      Operations     =>(
        Flexible_Image_Transfer_System_Format        => (FTS.Load'access, FTS.Save'access),
        Truevision_Graphics_Adapter_Format           => (TGA.Load'access, TGA.Save'access),
        Personal_Computer_Exchange_Format            => (PCX.Load'access, PCX.Save'access),
        Graphics_Interchange_Format                  => (TIF.Load'access, TIF.Save'access),
        Tagged_Image_File_Format                     => (GIF.Load'access, GIF.Save'access),
        Portable_Pixmap_Format                       => (PPM.Load'access, PPM.Save'access),
        X_Pixmap_Format                              => (XPM.Load'access, XPM.Save'access),
        Bit_Map_Format                               => (BMP.Load'access, BMP.Save'access),
        Joint_Photographic_Experts_Group_Format      => (PNG.Load'access, PNG.Save'access),
        Portable_Network_Graphics_Format             => (JPG.Load'access, JPG.Save'access),
        Joint_Photographic_Experts_Group_2000_Format => (JP2.Load'access, JP2.Save'access));
  end Neo.File.Image;