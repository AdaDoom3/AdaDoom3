
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

  function Load (Path : Str) return Compressed_Image is

    -- KTX texture format: http://web.archive.org/web/20160811201320/https://www.khronos.org/opengles/sdk/tools/KTX/file_format_spec/
    type KTX_Header_State is record
        Id : Str_8 (1..12) := (others => NULL_CHAR_8);
        Endianness,
        Kind.
        Image_Size.
        GL_Format.
        Internal_Format.
        Base_Format.
        Width.
        Height.
        Depth.
        Array_Length.
        Faces.
        Mipmaps.
        Key_Value_Bytes : Int_Unsigned := 0;
      end record with Pack;

    -- Open the file and load the header
    Data   : File_Type        := Open_File (Path);
    Header : KTX_Header_State := KTX_Header_State'Read (Data);

    -- Use the header to initialize a temp array for data parsing and result 
    Compressed_Image : Compressed_Image_State (Header.Mipmaps,
                                               Header.Array_Length,
                                               Header.Faces, 
                                               Header.Face_Size) := (others => 0);                
    begin
      Skip (Data, Header.Key_Value_Bytes);

      -- Assert the file is a valid KTX texture
      Assert (Id (2..7) = "KTX 11");

      -- Load the texture data
      for Level in 1..Header.Mipmaps loop
        for Layer in 1..Header.Array_Length loop
          for Face in 1..Header.Faces loop
            for Element in 1..Header.Face_Size loop
              Result (Level, Layer, Face, Element) := Byte'Read (Data);
            end loop;
          end loop;
        end loop;

        -- Skip the MIP padding
        Skip (Data, 3 - (Header.Image_Size + 3) mod 4);
      end loop;
      
      -- Shutdown
      Close_File (Data);
      return Result;
    end;
end;
