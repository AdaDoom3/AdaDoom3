
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

with Vulkan; use Vulkan

package Neo.Engine.Renderer is 

  ------------
  -- Vulkan --
  ------------

  type Material_Kind is (Bump_Material,   Diffuse_Material, Specular_Material, Coverage_Material, Ambient_Material, 
                         Mirror_Material, XRay_Material,    Cube_Material);
  type Light_Kind    is (Fog_Light,       Blend_Light,      Normal_Light);
  type Material_State is new Controlled with record
      Scale            : Matrix_2x3;
      Color            : Pixel_State;
      Opacity_Color    : Real_32_Percent := 100.0;
      Opacity          : Real_32_Percent := 100.0;
      Offset           : Real_32;
      Frame            : Int_32_Positive;
      Frame_Rate       : Int_32_Natural; -- Non-animated materials have a zero frame rate
      Texture          : Str_16_Unbound;
      Fragment_Program : Str_16_Unbound;
      Vertex_Program   : Str_16_Unbound;
      Fragment_Images  : Vector_Str_16_Unbound.Unsafe.Vector;
      Vertex_Args      : Vector_Vertex_Args.Unsafe.Vector;
    end record;
  type Animation_Instance_State is record
       Animation : 
    end record;
  type Mesh_Instance_State (Animated : Bool := True) is record -- will have vulkan stuff
      Location   :
      Rotation   :  
      Animations :
      Mesh       : Str_16_Unbound;
      Skeleton   :
    end record;
  type GPU_State is record
      Version              : Real_32;
      Color_Bits           : Int_4_Positive;
      Depth_Bits           : Int_4_Positive;
      Stencil_Bits         : Int_4_Positive;
      Bits_Per_Pixel       : Int_4_Positive;
      Maximum_Texture_Size : Int_4_Natural;
      Multisamples         : Int_4_Natural;
    end record;
  function Get_GPU return GPU_Specifics;
  Meshes    : Hashed_Mesh.Safe.Map;
  Images    : Hashed_Image.Safe.Map;
  Shaders   : Hashed_Stream.Safe.Map;
  Materials : Hashed_Materials.Safe.Map;

  package Vector_String_2_Unbounded is new Vectors(String_2_Unbounded);
  package Vector_Record_Graphic     is new Vectors(Record_Graphic);
  package Map_Record_Mesh           is new Hashed_Maps(Record_Mesh);
  --package Map_Record_Camera         is new Hashed_Maps(Record_Camera);
  package Map_Record_Animation      is new Hashed_Maps(Record_Animation);
  --package Map_Vector_Record_Graphic is new Hashed_Maps(Vector_Record_Graphic);
  type Record_Material is record
--      Scale             : Array_Matrix_2x3   := (others => (others => 0.0));
      Color             : Record_Pixel       := (others => <>);
      Opacity_Color     : Float_4_Percent    := 100.0;
      Opacity_General   : Float_4_Percent    := 100.0;
      Offset            : Float_4_Real       := 0.0;
      Frame             : Integer_4_Positive := 1;
      Frame_Rate        : Integer_4_Natural  := 0; -- Non-animated materials have a zero frame rate
      Texture           : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
      Fragment_Program  : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
      Vertex_Program    : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
      Fragment_Images   : Vector_String_2_Unbounded.Unprotected.Vector;
--      Vertex_Parameters : Vector_Vertex_Parameter.Unprotected.Vector;
    end record;
--  package Ordered_Map_Record_Material_Stage                 is new Ordered_Maps(Enumerated_Material_Stage, Record_Material);
--  package Map_Ordered_Map_Record_Material_Stage_Unprotected is new Hashed_Maps(Ordered_Map_Record_Material_Stages.Unprotected.Map);
  type Record_Light is record
      Axis   : Array_Record_Coordinate_3D(1..3) := (others => (others => <>));
      Origin : Record_Coordinate_3D             := (others => <>);
      -- Frustum definitions for project lights - should have real plane equations here though...
      Target : Record_Coordinate_3D             := (others => <>);
      Right  : Record_Coordinate_3D             := (others => <>);
      Up     : Record_Coordinate_3D             := (others => <>);
      Start  : Record_Coordinate_3D             := (others => <>);
      Ending : Record_Coordinate_3D             := (others => <>);
    end record;
  type Record_View is record
      Field_Of_View : Record_Coordinate_2D_Degree := (others => <>);
      Origin        : Record_Coordinate_3D        := (others => <>);
      Axis          : Record_Coordinate_3D        := (others => <>);
      Time          : Duration                    := 0.0;
    end record;
--  type Record_Texture is new Controlled with record
--      Is_Compressed : Boolean := False;
--      case Is_Compressed is
--        when True => 
--    end record;
--  type Record_Element(Kind : Enumerated_Element := Scene_Element) is record
--      X      : Integer_4_Natural  := 0;
--      Y      : Integer_4_Natural  := 0;
--      Width  : Integer_4_Positive := 640;
--      Height : Integer_4_Positive := 480;
--      case Kind is
--        when Text_Element =>
--          Size    : Integer_4_Positive := 12;
--          Font    : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
--          Text    : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
--          Color   : Record_Color       := (others => <>);
--          Opacity : Float_4_Percent    := 100.0;
--        when Scene_Element => null;
--          --Surfaces : ; 
--          --Lights   : ;
--          --Views    : ;
--        when Interface_Element =>
--          Material : Record_Material := (others => <>);
--      end case;
--    end record;
  type Record_Specifics is record
      Is_Supported                     : Boolean                     := False;
      Shading_Language                 : Enumerated_Shading_Language := OpenGL_Shading_Language;
      Version                          : Float_4_Real                := 0.0;
      Color_Bits                       : Integer_4_Positive          := 1;
      Depth_Bits                       : Integer_4_Positive          := 1;
      Stencil_Bits                     : Integer_4_Positive          := 1;
      Bits_Per_Pixel                   : Integer_4_Positive          := 1;
      Maximum_Texture_Size             : Integer_4_Natural           := 0;
      Multisamples                     : Integer_4_Natural           := 0;
      Has_Stereo_Pixel_Format          : Boolean                     := False;
      Has_Multitexture                 : Boolean                     := False;
      Has_Direct_State_Access          : Boolean                     := False;
      Has_Texture_Compression          : Boolean                     := False;
      Has_Anisotropic_Filter           : Boolean                     := False;
      --Has_Texture_Level_Of_Detail_Bias : Boolean                     := False;
      Has_Seamless_Cube_Map            : Boolean                     := False;
      Has_RGB_Color_Framebuffer        : Boolean                     := False;
      Has_Vertex_Buffer_Object         : Boolean                     := False;
      Has_Map_Buffer_Range             : Boolean                     := False;
      Has_Vertex_Array_Object          : Boolean                     := False;
      Has_Draw_Elements_Base_Vertex    : Boolean                     := False;
      Has_Uniform_Buffer               : Boolean                     := False;
      --Has_Two_Sided_Stencil            : Boolean                     := False;
      Has_Depth_Bounds_Test            : Boolean                     := False;
      Has_Sync                         : Boolean                     := False;
      Has_Timer_Query                  : Boolean                     := False;
      Has_Occlusion_Query              : Boolean                     := False;
    end record;
  procedure Render_Frame;
  procedure Initialize (Monitor : in Integer_4_Positive);
  procedure Finalize   (Monitor : in Integer_4_Positive);
  function Get_Specifics return Record_Specifics;
--  Elements  : Vector_Record_Element.Protected_Vector;
--  Textures  : Map_Vector_Record_Graphics_Unprotected.Protected_Map;
--  Materials : Map_Ordered_Map_Record_Material_Stage_Unprotected.Protected_Map;
  MAXIMUM_CLIP_PLANES                 : constant Integer_4_Positive := 1;
  MAXIMUM_NUMBER_OF_OCCLUSION_QUERIES : constant Integer_4_Positive := 16#0000_1000#;
end;