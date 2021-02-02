
--                                                                                                                               --
--                                                      N E O  E N G I N E                                                       --
--                                                                                                                               --
--                                               Copyright (C) 2020 Justin Squirek                                               --
--                                                                                                                               --
-- Neo is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published --
-- by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.                      --
--                                                                                                                               --
-- Neo is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of         --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.                     --
--                                                                                                                               --
-- You should have received a copy of the GNU General Public License along with Neo. If not, see gnu.org/licenses                --
--                                                                                                                               --

with Neo.API.Vulkan; use Neo.API.Vulkan;

-- Unified texture type definition
package Neo.Data.Texture is

  Unsupported_Version,
  Unsupported_Orientation,
  Unsupported_Compression_Format : Exception;

  --
  -- KTX1 Exporting instructions
  --
  -- We only support textures through KTX1 (preferribly generated through PVRTexTool)
  -- NOTE: When using PVRTexTool the texture needs to be horizontally flipped to have the correct KTXOrienation of "S=r,T=u,R=i"
  --
  -- http://web.archive.org/web/20160811201320/https://www.khronos.org/opengles/sdk/tools/KTX/file_format_spec/
  --
  -- To generate textures from images with PVRTexToolCLI use the following command:
  --
  --   PVRTexToolCLI.exe -m -i "./in.tga" -o "./out.ktx" -flip y -f <FORMAT>
  --
  -- Where <FORMAT> can be:
  --
  --   EAC_R11,      EAC_RG11,
  --   BC1,          BC2,          BC3,
  --   ETC2_RGBA,    ETC2_RGB_A1,  ETC1,
  --   PVRTC1_4_RGB, PVRTC1_2_RGB, PVRTC1_2,   PVRTC1_4,   PVRTC2_2,   PVRTC2_4,
  --   ASTC_4x4,     ASTC_5x4,     ASTC_5x5,   ASTC_6x5,   ASTC_6x6,   ASTC_8x5,   ASTC_8x6,   ASTC_8x8,   ASTC_10x5,  ASTC_10x6,
  --   ASTC_10x8,    ASTC_10x10,   ASTC_12x10, ASTC_12x12, ASTC_3x3x3, ASTC_4x3x3, ASTC_4x4x3, ASTC_4x4x4, ASTC_5x4x4, ASTC_5x5x4,
  --   ASTC_5x5x5,   ASTC_6x5x5,   ASTC_6x6x5, ASTC_6x6x6
  --
  -- or uncompressed:
  --
  --   a8b8g8r8,UI,lRGB
  --

  type Image_State (Data_Size : Int) is record
      Mipmaps,
      Faces,
      Height,
      Width,
      Format : Int_Unsigned_C;
      Data   : Array_Byte (1..Data_Size);
    end record;
  type Ptr_Image_State is access all Image_State;

  procedure Load (Path : Str; Image : in out Ptr_Image_State);
  procedure Free is new Unchecked_Deallocation (Image_State, Ptr_Image_State);
end;
