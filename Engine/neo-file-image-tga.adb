separate(Neo.File.Image) package body TGA is
  type Enumerated_Color is(Indexed_Color, Grayscale_Color, Multi_Color); for Enumerated_Color use(Indexed_Color => 1, Multi_Color => 2, Grayscale_Color => 3);
  type Record_Header is record
      Identifier_Length : Integer_1_Unsigned  := 0;
      Color_Map_Kind    : Integer_1_Unsigned  := 0;
      Kind              : Enumerated_Color    := Multi_Color;
      Is_Compressed     : Boolean             := False;
      Color_Map_Index   : Integer_2_Unsigned  := 0;
      Color_Map_Length  : Integer_2_Unsigned  := 0;
      Color_Map_Size    : Integer_1_Unsigned  := 0;
      X_Origin          : Integer_2_Unsigned  := 0;
      Y_Origin          : Integer_2_Unsigned  := 0;
      Width             : Integer_2_Unsigned  := 0;
      Height            : Integer_2_Unsigned  := 0;
      Bits_Per_Pixel    : Integer_1_Unsigned  := 0;
      Alpha_Bits        : Integer range 0..15 := 0;
      Flip_Horizonal    : Boolean             := False;
      Flip_Vertical     : Boolean             := False;
      Junk              : Integer range 0..3  := 0;
    end record;
  for Record_Header'size use ;
  for Record_Header use
      Identifier_Length at 0 range Byte'size *  0    ..Byte'size *  1 - 1;
      Color_Map_Kind    at 0 range Byte'size *  1    ..Byte'size *  2 - 1;
      Kind              at 0 range Byte'size *  2    ..Byte'size *  2 + 1;
      Is_Compressed     at 0 range Byte'size *  2 + 3..Byte'size *  2 + 3;
      Color_Map_Index   at 0 range Byte'size *  3    ..Byte'size *  5 - 1;
      Color_Map_Length  at 0 range Byte'size *  5    ..Byte'size *  7 - 1;
      Color_Map_Size    at 0 range Byte'size *  7    ..Byte'size *  8 - 1;
      X_Origin          at 0 range Byte'size *  8    ..Byte'size * 10 - 1;
      Y_Origin          at 0 range Byte'size * 10    ..Byte'size * 12 - 1;
      Width             at 0 range Byte'size * 12    ..Byte'size * 14 - 1;
      Height            at 0 range Byte'size * 14    ..Byte'size * 16 - 1;
      Bits_Per_Pixel    at 0 range Byte'size * 16    ..Byte'size * 17 - 1;
      Alpha_Bits        at 0 range Byte'size * 17    ..Byte'size * 17 + 3;
      Flip_Horizonal    at 0 range Byte'size * 17 + 4..Byte'size * 17 + 4;
      Flip_Vertical     at 0 range Byte'size * 17 + 5..Byte'size * 17 + 5;
      Junk              at 0 range Byte'size * 17 + 6..Byte'size * 17 + 7;
    end record;
  function Load(Path : in String_2) return Array_Record_Graphic is
    Header : Record_Header     := Read(Header);
    Colors : Enumerated_Colors := Enumerated_Colors'val(Header.Bits_Per_Pixel);
    begin
      Skip(Header.Identifier_Length * Byte'size);
      Assert(Header.Alpha_Bits /= Header.Bits_Per_Pixel));
      Assert(not(Header.Color = Multi_Color     and Colors = Alpha_With_Two_Hundred_Fifty_Six_Per_Colors));
      Assert(not(Header.Color = Multi_Color     and Colors < Thirty_Two_Per_Colors));
      Assert(not(Header.Color = Grayscale_Color and Colors = Thirty_Two_Per_Colors));
      Assert(not(Header.Color = Grayscale_Color and Colors /= Sixteen_Colors and (Header.Alpha_Bits /= 8 or Header.Bits_Per_Pixel not in 15..16)));
      Assert(not(Header.Color = Mapped_Color    and Colors /= Sixteen_Colors));
      if Header.Color = Mapped_Color then
        cmap_bytes = (info->colorMapSize + 7 ) / 8;
        tga_cmap = g_new (guchar, info->colorMapLength * cmap_bytes);
        if info->colorMapSize > 24 then convert_cmap = g_new (guchar, info->colorMapLength * 4);
        else info->colorMapLength > 256 then convert_cmap = g_new (guchar, info->colorMapLength * 3);
        else gimp_cmap = g_new (guchar, info->colorMapLength * 3); end if;
        if (cmap_bytes <= 4 && fread (tga_cmap, info->colorMapLength * cmap_bytes, 1, fp) == 1)
          case Color_Map_Size is
            when 32 => bgr2rgb (convert_cmap, tga_cmap, info->colorMapLength, cmap_bytes, 1);
            when 24 => bgr2rgb (convert_cmap, tga_cmap, info->colorMapLength, cmap_bytes, 0);
            when 16 | 15 => upsample (convert_cmap, tga_cmap, info->colorMapLength, cmap_bytes, info->alphaBits);
            when others => raise Currupt with Locali"Unsupported colormap depth: %u";
          end case;
        end if;
      end if;
      declare
      type Integer_Color is mod Colors'val**2;
      Current : Integer_Color := Integer_Color'first;
      Pixel   : Record_Pixel  := (others => <>);
      Pixel_Row : Array_Record_Pixel
      Graphic : Record_Graphic(Truevision_Graphics_Adapter_Format, Width, Height) := (Run_Length_Encode => Run_Length_Encoded = 1, Colors => Colors);
      begin
        for Y in 1..Graphic.Height loop
          if Header.Is_Compressed then

          else
            for X in 1..Graphic.Width loop
              Graphic.Pixels(X, Y) :=(
                Alpha => (if Colors = Alpha_With_Two_Hundred_Fifty_Six_Per_Colors then
                         Integer_1_Unsigned(Shift_Right(Shift_Left(Integer_4_Unsigned(Current), Integer_Color'size / Graphic.Color'val / Byte'size * 3), Integer_Color'size / Graphic.Color'val / Byte'size * 3)) else Byte'last))
                Red   => Integer_1_Unsigned(Shift_Right(Shift_Left(Integer_4_Unsigned(Current), Integer_Color'size / Graphic.Color'val / Byte'size * 2), Integer_Color'size / Graphic.Color'val / Byte'size * 3));;
                Green => Integer_1_Unsigned(Shift_Right(Shift_Left(Integer_4_Unsigned(Current), Integer_Color'size / Graphic.Color'val / Byte'size),     Integer_Color'size / Graphic.Color'val / Byte'size * 3)),
                Blue  => Integer_1_Unsigned(Shift_Right(           Integer_4_Unsigned(Current), Integer_Color'size / Graphic.Color'val / Byte'size * 3)),
            end loop;
          end if;
          if Header.Flip_Horizonal then
            for X in 1..Integer'floor(Graphic.Width / 2) loop
              Graphic.Pixels(Y, X) := Pixel;
              Graphic.Pixels(Y, X) := Graphic.Pixels(Y, Graphic.Width - X + 1);
              Graphic.Pixels(Y, Graphic.Width - X + 1) := Pixel;
            end loop;
          end if;
        end loop;
        if Header.Flip_Vertical then
          for Y in 1..Integer'floor(Graphic.Height / 2) loop
            Graphic.Pixels(Y) := Pixel_Row;
            Graphic.Pixels(Y) := Graphic.Pixels(Graphic.Height - Y + 1);
            Graphic.Pixels(Graphic.Height - Y + 1) := Pixel_Row;
          end loop;
        end if;
        return Graphic;
      end;
    end Load;
  procedure Save(Path : in String_2; Graphic : in Array_Record_Graphic) is
    begin
      raise Unimplemented_Feature;
    end Save;
end TGA;




