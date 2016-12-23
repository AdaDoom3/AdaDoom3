
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
        Id              : Str_8 (1..12)   := (others => NULL_CHAR_8);
        Endianness      : Int_32_Unsigned := 0;
        Kind            : Int_32_Unsigned := 0;
        Image_Size      : Int_32_Unsigned := 0;
        GL_Format       : Int_32_Unsigned := 0;
        Internal_Format : Int_32_Unsigned := 0;
        Base_Format     : Int_32_Unsigned := 0;
        Width           : Int_32_Unsigned := 0;
        Height          : Int_32_Unsigned := 0;
        Depth           : Int_32_Unsigned := 0;
        Array_Length    : Int_32_Unsigned := 0;
        Faces           : Int_32_Unsigned := 0;
        Mipmap_Levels   : Int_32_Unsigned := 0;
        Key_Value_Bytes : Int_32_Unsigned := 0;
      end record with Pack;

    -- Open the file and load the header
    -- File   : File_Type        := Open_File (Path);
    -- Header : KTX_Header_State := KTX_Header_State'Read (File);

  --   -- Use the header to initialize a temp array for data parsing and result 
  --   Texture : array (1..Header.Mipmap_Levels, 1..Header.Array_Length, 1..Header.Faces, 1..Header.Face_Size) of Byte;
  --   Result  : Texture_State ();

    begin
  --     if(Header.NumberOfFaces > 1)
  --       if(Header.NumberOfArrayElements > 0) return TARGET_CUBE_ARRAY;
  --       else return TARGET_CUBE;
  --     elsif(Header.NumberOfArrayElements > 0)
  --       if(Header.PixelHeight == 0) return TARGET_1D_ARRAY;
  --       else return TARGET_2D_ARRAY;
  --     else if(Header.PixelHeight == 0) return TARGET_1D;
  --     else if(Header.PixelDepth > 0) return TARGET_3D;
  --     else return TARGET_2D;

  --     -- Assert the file is a valid KTX texture
  --     Assert ("KTX 11");
  --     Assert (this->extent().y >= 1 and this->extent().z == 1)
  --     Assert (Target != TARGET_2D         or (Target = TARGET_2D         and this->layers() = 1  and this->faces() = 1));
  --     Assert (Target != TARGET_2D_ARRAY   or (Target = TARGET_2D_ARRAY   and this->layers() >= 1 and this->faces() = 1));
  --     Assert (Target != TARGET_CUBE       or (Target = TARGET_CUBE       and this->layers() = 1  and this->faces() >= 1));
  --     Assert (Target != TARGET_CUBE_ARRAY or (Target = TARGET_CUBE_ARRAY and this->layers() >= 1 and this->faces() >= 1));

  --     -- Load the texture data
  --     for Level in 1..Header.Mipmap_Levels loop
  --       for Layer in 1..Header.Array_Length loop
  --         for Face in 1..Header.Faces loop
  --           for Element in 1..Header.Face_Size loop
  --             Texture_Data (Level, Layer, Face, Element) := Byte'Read (File);
  --           end loop;
  --         end loop;
  --       end loop;
  --       -- Skip the MIP padding
  --       Set_Position (File, 3 - ((Texture.Image_Size + 3) mod 4));
  --     end loop;
      return Result;
    end;
end;