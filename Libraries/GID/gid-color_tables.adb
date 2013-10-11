with GID.Buffering;

with Ada.Exceptions;

package body GID.Color_tables is

  procedure Convert(c, d: in U8; rgb: out RGB_Color) is
  begin
    rgb.red  := (d and 127) / 4;
    rgb.green:= (d and 3) * 8 + c / 32;
    rgb.blue := c and 31;
    --
    rgb.red  := U8((U16(rgb.red  ) * 255) / 31);
    rgb.green:= U8((U16(rgb.green) * 255) / 31);
    rgb.blue := U8((U16(rgb.blue ) * 255) / 31);
  end Convert;

  procedure Load_palette (image: in out Image_descriptor) is
    c, d: U8;
    use GID.Buffering;
  begin
    if image.palette = null then
      return;
    end if;
    declare
      palette: Color_Table renames image.palette.all;
    begin
      for i in palette'Range loop
        case image.format is
          when BMP =>
            -- order is BGRx
            U8'Read(image.stream, Palette(i).blue);
            U8'Read(image.stream, Palette(i).green);
            U8'Read(image.stream, Palette(i).red);
            U8'Read(image.stream, c);
            -- x discarded
          when GIF | PNG =>
            -- buffered; order is RGB
            Get_Byte(image.buffer, Palette(i).red);
            Get_Byte(image.buffer, Palette(i).green);
            Get_Byte(image.buffer, Palette(i).blue);
          when TGA =>
            case image.subformat_id is -- = palette's bit depth
              when 8 => -- Grey
                U8'Read(image.stream, c);
                Palette(i).Red  := c;
                Palette(i).Green:= c;
                Palette(i).Blue := c;
              when 15 | 16 => -- RGB, 5 bit per channel
                U8'Read(image.stream, c);
                U8'Read(image.stream, d);
                Convert(c, d, Palette(i));
              when 24 | 32 => -- RGB | RGBA, 8 bit per channel
                U8'Read(image.stream, Palette(i).blue);
                U8'Read(image.stream, Palette(i).green);
                U8'Read(image.stream, Palette(i).red);
              when others =>
                null;
            end case;
          when others =>
            Ada.Exceptions.Raise_exception(
              unsupported_image_subformat'Identity,
              "Palette loading not implemented for " &
              Image_format_type'Image(image.format)
            );
        end case;
      end loop;
    end;
  end Load_palette;

end GID.Color_tables;
