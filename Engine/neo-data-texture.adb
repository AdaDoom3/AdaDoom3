
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

package body Neo.Data.Texture is

  ----------
  -- Load --
  ----------

  procedure Load (Path : Str; Image : in out Ptr_Image_State) is

    type KTX_Header_State is record
        Id : Str_8 (1..12);
        Endianness,
        Kind,   
        Kind_Size, 
        Format, 
        Internal_Format, 
        Base_Format,
        Width,      
        Height, 
        Depth,     
        Array_Length, 
        Faces, 
        Mipmaps, 
        Key_Value_Bytes : Int_Unsigned_C := 0;
      end record with Pack;

    Data_Size   : Int_Unsigned_C;
    Header      : KTX_Header_State;
    Data        : Ada.Streams.Stream_IO.File_Type; 
    Data_Stream : Ada.Streams.Stream_IO.Stream_Access;
    begin
    
      -- Open the file and load the header
      Open (Data, In_File, To_Str_8 (Path & ".ktx"));
      Data_Stream := Ada.Streams.Stream_IO.Stream (Data);
      KTX_Header_State'Read (Data_Stream, Header);
      
      -- Fixup mipmap values - according to the spec zero mipmaps also happens to mean one mipmap
      if Header.Mipmaps = 0 then Header.Mipmaps := 1; end if;
      
      -- Inspect the header for «KTX 11» to make sure we are working with version 1.0
      if Header.Id /= Char_8'Val (16#AB#) & "KTX 11" & Char_8'Val (16#BB#) & EOL_8 & SUB & LF then
        raise Unsupported_Version;
      end if;
      
      -- Check the orientation
      declare
      Key_Values : Str_8 (1..Int (Header.Key_Value_Bytes)) := (others => NULL_CHAR_8);
      begin
        Str_8'Read (Data_Stream, Key_Values);
        if Index (Key_Values, "S=r,T=d,R=i") = 0 then raise Unsupported_Orientation; end if;
      end;
      
      -- Load the image data size
      Int_Unsigned_C'Read (Data_Stream, Data_Size);
      
      -- PVRTexToolCLI sets the format incorrectly to 0 for uncompressed layouts - assume GL_RGBA8UI. Otherwise, map it from OpenGL
      Header.Internal_Format := (if Header.Internal_Format = 0 then VK_FORMAT_R8G8B8A8_UNORM
                                                               else To_VkFormat (Header.Internal_Format));
      if Header.Internal_Format = VK_FORMAT_UNDEFINED then raise Unsupported_Compression_Format; end if;
      
      -- Grab the image in a single go - no reason for special loops...
      Image := new Image_State'(Int (Data_Size), Header.Mipmaps, Header.Faces, Header.Width, Header.Height, Header.Internal_Format, others => <>);
      Array_Byte'Read (Data_Stream, Image.Data);         
      Close (Data);
    end;
end;









































