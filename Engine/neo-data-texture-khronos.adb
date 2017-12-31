
--                                                                                                                                      --
--                                                         N E O  E N G I N E                                                           --
--                                                                                                                                      --
--                                                 Copyright (C) 2016 Justin Squirek                                                    --
--                                                                                                                                      --
-- Neo is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the --
-- Free Software Foundation, either version 3 of the License, or (at your option) any later version.                                    --
--                                                                                                                                      --
-- Neo is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of                --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.                            --
--                                                                                                                                      --
-- You should have received a copy of the GNU General Public License along with Neo. If not, see gnu.org/licenses                       --
--                                                                                                                                      --

-- Khronos KTX texture loader
separate (Neo.Data.Texture) package body Khronos is

  -----------
  -- Image --
  -----------

  function Load (Path : Str) return Image_State is

    --  http://web.archive.org/web/20160811201320/https://www.khronos.org/opengles/sdk/tools/KTX/file_format_spec/
    type KTX_Header_State is record
        Id : Str_8 (1..12) := (others => NULL_CHAR_8);
        Endianness, Kind, Image_Size, GL_Format, Internal_Format, Base_Format, Width, Height, Depth, Array_Length, Faces, Mipmaps,
        Key_Value_Bytes : Int_Unsigned := 0;
      end record with Pack;

    Head : KTX_Header_State := (others => <>);
    Data : Ada.Streams.Stream_IO.File_Type;             
    begin
      Open (Data, In_File, To_Str_8 (Path));
      KTX_Header_State'Read (Ada.Streams.Stream_IO.Stream (Data), Head);
      
      -- Once the header is loaded we know the size of our resulting image
      declare Image : Image_State := (Khronos_Format, Head.Mipmaps, Head.Array_Length, Head.Faces, Head.Faces, others => <>); begin
      
        -- Skip key-value pairs and assert some magic
        Assert (Head.Id (2..7) = "KTX 11");
        Skip (Data, Int (Head.Key_Value_Bytes));
  
        -- Load color data
        for Level in 1..Head.Mipmaps loop
          for Layer in 1..Head.Array_Length loop
            for Face in 1..Head.Faces loop
              for Element in 1..Head.Width * Head.Height loop
                Byte'Read (Ada.Streams.Stream_IO.Stream (Data), Image.Data (Level, Layer, Face, Element));
              end loop;
            end loop;
          end loop;
  
          -- Skip MIP padding
          Skip (Data, 3 - (Int (Head.Image_Size) + 3) mod 4);
        end loop;
        Close (Data);
        return Image;
      end;
    end;
end;
