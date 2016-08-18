
--                                                                                                                    
--                                                              N E O  E N G I N E                                                    
--                                                                                                                    
--                                                      Copyright (C) 2016 Justin Squirek                                          
-- 
-- Neo is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any later version. 
--                                                                                                                    
-- Neo is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.         
--                                                                                                                    
-- You should have received a copy of the GNU General Public License along with Neo. If not, see gnu.org/licenses     
--

separate (Neo.Image) package body TGA is

  ------------
  -- Header --
  ------------

  type Color_Kind is (Indexed_Color, Grayscale_Color, Multi_Color);
  for Color_Kind use (Indexed_Color => 1, Multi_Color => 2, Grayscale_Color => 3);
  type Header_State is record
      Identifier_Length : Byte;
      Color_Map_Kind    : Byte;
      Kind              : Color_Kind;
      Is_Compressed     : Bool;
      Color_Map_Index   : Int_2_Unsigned;
      Color_Map_Length  : Int_2_Unsigned;
      Color_Map_Size    : Byte;
      X_Origin          : Int_2_Unsigned;
      Y_Origin          : Int_2_Unsigned;
      Width             : Int_2_Unsigned;
      Height            : Int_2_Unsigned;
      Bits_Per_Pixel    : Byte;
      Alpha_Bits        : Int range 0..15;
      Flip_Horizonal    : Bool;
      Flip_Vertical     : Bool;
      Junk              : Int range 0..3;
    end record;
  for Header_State'Size use ;
  for Header_State use
      Identifier_Length at 0 range Byte'Size *  0    ..Byte'Size *  1 - 1;
      Color_Map_Kind    at 0 range Byte'Size *  1    ..Byte'Size *  2 - 1;
      Kind              at 0 range Byte'Size *  2    ..Byte'Size *  2 + 1;
      Is_Compressed     at 0 range Byte'Size *  2 + 3..Byte'Size *  2 + 3;
      Color_Map_Index   at 0 range Byte'Size *  3    ..Byte'Size *  5 - 1;
      Color_Map_Length  at 0 range Byte'Size *  5    ..Byte'Size *  7 - 1;
      Color_Map_Size    at 0 range Byte'Size *  7    ..Byte'Size *  8 - 1;
      X_Origin          at 0 range Byte'Size *  8    ..Byte'Size * 10 - 1;
      Y_Origin          at 0 range Byte'Size * 10    ..Byte'Size * 12 - 1;
      Width             at 0 range Byte'Size * 12    ..Byte'Size * 14 - 1;
      Height            at 0 range Byte'Size * 14    ..Byte'Size * 16 - 1;
      Bits_Per_Pixel    at 0 range Byte'Size * 16    ..Byte'Size * 17 - 1;
      Alpha_Bits        at 0 range Byte'Size * 17    ..Byte'Size * 17 + 3;
      Flip_Horizonal    at 0 range Byte'Size * 17 + 4..Byte'Size * 17 + 4;
      Flip_Vertical     at 0 range Byte'Size * 17 + 5..Byte'Size * 17 + 5;
      Junk              at 0 range Byte'Size * 17 + 6..Byte'Size * 17 + 7;
    end record;

  ----------
  -- Load --
  ----------

  function Load (Path : Str_16) return Graphic_Array is
    Header : Header_State := Read (Header);
    Colors : Color_Kind   := Color_Kind'Val (Header.Bits_Per_Pixel);
    begin
      Skip   (Header.Identifier_Length * Byte'Size);
      Assert (Header.Alpha_Bits /= Header.Bits_Per_Pixel));
      Assert (not (Header.Color = Multi_Color     and Colors = Alpha_With_Two_Hundred_Fifty_Six_Per_Colors));
      Assert (not (Header.Color = Multi_Color     and Colors < Thirty_Two_Per_Colors));
      Assert (not (Header.Color = Grayscale_Color and Colors = Thirty_Two_Per_Colors));
      Assert (not (Header.Color = Grayscale_Color and Colors /= Sixteen_Colors and (Header.Alpha_Bits /= 8 or Header.Bits_Per_Pixel not in 15..16)));
      Assert (not (Header.Color = Mapped_Color    and Colors /= Sixteen_Colors));
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
            when others => raise Currupt with -"Unsupported colormap depth: %u";
          end case;
        end if;
      end if;
      declare
      type Int_Color is mod Colors'val**2;
      Current   : Int_Color    := Int_Color'first;
      Pixel     : Record_Pixel := (others => <>);
      Pixel_Row : Pixel_Array
      Graphic   : Graphic_State (Truevision_Graphics_Adapter_Format, Width, Height) := (Run_Length_Encode => Run_Length_Encoded = 1, Colors => Colors);
      begin
        for Y in 1..Graphic.Height loop
          if Header.Is_Compressed then

          else
            for X in 1..Graphic.Width loop
              Graphic.Pixels (X, Y) :=(
                Alpha => (if Colors = Alpha_With_Two_Hundred_Fifty_Six_Per_Colors then
                         Byte (Shift_Right (Shift_Left (Int_32_Unsigned (Current), Int_Color'Size / Graphic.Color'val / Byte'Size * 3), Integer_Color'Size / Graphic.Color'val / Byte'Size * 3)) else Byte'last))
                Red   => Byte (Shift_Right (Shift_Left (Int_32_Unsigned (Current), Int_Color'Size / Graphic.Color'val / Byte'Size * 2), Integer_Color'Size / Graphic.Color'val / Byte'Size * 3));;
                Green => Byte (Shift_Right (Shift_Left (Int_32_Unsigned (Current), Int_Color'Size / Graphic.Color'val / Byte'Size),     Integer_Color'Size / Graphic.Color'val / Byte'Size * 3)),
                Blue  => Byte (Shift_Right             (Int_32_Unsigned (Current), Int_Color'Size / Graphic.Color'val / Byte'Size * 3)),
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
          for Y in 1..Int_32'Floor (Graphic.Height / 2) loop
            Graphic.Pixels (Y) := Pixel_Row;
            Graphic.Pixels (Y) := Graphic.Pixels (Graphic.Height - Y + 1);
            Graphic.Pixels (Graphic.Height - Y + 1) := Pixel_Row;
          end loop;
        end if;
        return Graphic;
      end;
    end;
  procedure Save (Path : String_2; Graphic : Array_Record_Graphic) is
    begin
      raise Unimplemented_Feature;
    end;
end;



