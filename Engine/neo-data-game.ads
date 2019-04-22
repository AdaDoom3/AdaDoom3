
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

with Neo.Core.Maps; use Neo.Core.Maps;
with Neo.Engine;    use Neo.Engine;

-- Unified 3D model, shader, and surface type definitions
package Neo.Data.Game is

  -------------
  -- Surface --
  -------------
  --
  -- A Surface defines how materials interact with all of the other portions of a game engine including footsteps and grenade and bullet
  -- damage - what decals are applied at points of impact and what sounds are generated. In addition, physical properties like friction
  -- and sound reverb are included.
  --
  -- Each property of a surface can take any number of variations (thus the use of a vector instead of an array).
  --

  type Surface_Kind is (Flesh_Surface, Nasty_Flesh_Surface, Thin_Wood_Surface,    Dense_Wood_Surface,    Liquid_Surface, Slime_Surface,
                        Paper_Surface, Cardboard_Surface,   Thin_Rock_Surface,    Dense_Rock_Surface,    Gravel_Surface, Asphalt_Surface,
                        Brick_Surface, Dirt_Surface,        Grass_Surface,        Foliage_Surface,       Sand_Surface,   Snow_Surface,
                        Ice_Surface,   Mud_Surface,         Thin_Metal_Surface,   Thick_Metal_Surface,   Chain_Surface,  Grenade_Surface,
                        Can_Surface,   Weapon_Surface,      Thin_Plastic_Surface, Thick_Plastic_Surface, Glass_Surface,  Rubber_Surface,
                        Tile_Surface,  Carpet_Surface);

  type Surface_State is record
      Mass_Per_Cubic_Unit   : Real_Percent := 0.0;
      Friction              : Real_Percent := 0.0;
      Restitution           : Real_Percent := 0.0;
      Noise_Reduction       : Real_Percent := 0.0; -- www.archtoolbox.com/representation/architectural-concepts/acoustics.html
      Physics_Hit_Emitter   : Str_Unbound  := NULL_STR_UNBOUND;
      Bullet_Hole_Emitter   : Str_Unbound  := NULL_STR_UNBOUND;
      Knife_Slash_Emitter   : Str_Unbound  := NULL_STR_UNBOUND;
      Walk_Light_Emitter    : Str_Unbound  := NULL_STR_UNBOUND;
      Explosion_Emitter     : Str_Unbound  := NULL_STR_UNBOUND;
      Walk_Hard_Emitter     : Str_Unbound  := NULL_STR_UNBOUND;
      Explosion_Materials   : Vector_Str_Unbound.Unsafe.Vector;
      Bullet_Hole_Materials : Vector_Str_Unbound.Unsafe.Vector;
      Knife_Slash_Materials : Vector_Str_Unbound.Unsafe.Vector;
      Land_Light_Materials  : Vector_Str_Unbound.Unsafe.Vector;
      Land_Hard_Materials   : Vector_Str_Unbound.Unsafe.Vector;
      Footstep_Materials    : Vector_Str_Unbound.Unsafe.Vector; -- Scale to feet of model ???
    end record;

  -- Returns constant Surface_State for a given Surface_Kind. All surfaces are defined in the body of this package (neo-model.adb).
  -- function Get_Surface (Kind : Surface_Kind) return Surface_State;

  --------------
  -- Material --
  --------------
  --
  -- A Material or "shader" is a grouping of texture maps and settings that define what a given entity, decal, or map element will look
  -- like and how it might be affected by light or camera position.
  --
  -- This implementation of materials offloads all of the work into separate shader programs so no logic has to be contained within the
  -- actual material record.
  --
  -- References.
  --   web.archive.org/web/20150303182930/https://docs.unrealengine.com/latest/INT/Engine/Rendering/Materials/MaterialProperties/
  --   web.archive.org/web/20160305174701/https://www.iddevnet.com/doom3/materials.php
  --

  type Filter_Kind  is (Linear_Filter,  Nearest_Filter);
  type Clamp_Kind   is (No_Clamp,       Zero_Clamp,      Normal_Clamp,          Zero_Alpha_Clamp);
  type Cube_Kind    is (No_Cube,        Image_Cube,      Camera_Cube,           Mirror_Cube,       Skybox_Cube);
  type Domain_Kind  is (Surface_Domain, Decal_Domain,    Post_Process_Domain,   Light_Domain,      Menu_Domain,   No_Domain);
  type Blend_Kind   is (Opaque_Blend,   Masked_Blend,    Additive_Blend,        Modulate_Blend,    Mirror_Blend,  Remote_Blend);
  type Shading_Kind is (Unlit_Shading,  Lit_Shading,     Subsurface_Shading,    Profile_Shading,   Skin_Shading,  Clear_Coat_Shading);
  type Deform_Kind  is (No_Deform,      Sprite_Deform,   Tube_Deform,           Flare_Deform,      Expand_Deform, Move_Deform,
                        Eye_Deform,     Particle_Deform, Small_Particle_Deform, Turbulent_Deform);
  type Sort_Kind    is (Subview_Sort,   GUI_Sort,        Opaque_Sort,           Sky_Sort,          Decal_Sort,    Far_Sort,
                        Medium_Sort,    Close_Sort,      Almost_Near_Sort,      Nearest_Sort,      Post_Sort);

  type Material_State (Domain : Domain_Kind := Surface_Domain) is record
      Has_Smoothed_Tan : Bool            := True;
      Is_Two_Sided     : Bool            := False;
      Is_Ambient       : Bool            := False;
      Is_Opaque        : Bool            := False;
      Sort             : Sort_Kind       := Opaque_Sort;
      Clamp            : Clamp_Kind      := No_Clamp;
      Filter           : Filter_Kind     := Linear_Filter;
      Surface          : Surface_kind    := Thin_Rock_Surface;
      Deform           : Deform_Kind     := No_Deform;
      Color_Mod        : Color_State     := NULL_COLOR;
      Transform        : Transform_4D    := (1.0, 0.0, 0.0, 0.0,
                                             0.0, 1.0, 0.0, 0.0,
                                             0.0, 0.0, 0.0, 0.0); -- ???
      case Domain is
        when No_Domain => null;
        when Menu_Domain => Menu_Id : Str_Unbound := NULL_STR_UNBOUND;
        when others =>
          Base_Color : Str_Unbound := NULL_STR_UNBOUND; -- RGBA
          case Domain is
            when Light_Domain => null;
            when others =>
              -- Blend                : Blend_Kind    := Opaque_Blend;
              Cube                 : Cube_Kind   := No_Cube; -- Cube map textures assume _px, _py, _pz, _nx, _ny, _nz name convention
              Prefilter            : Str_Unbound := NULL_STR_UNBOUND; -- Single channel
              Irradiance           : Str_Unbound := NULL_STR_UNBOUND; -- RGBA
              Specular             : Str_Unbound := NULL_STR_UNBOUND; -- RGBA
              Normal               : Str_Unbound := NULL_STR_UNBOUND; -- Single channel
              Displacement         : Str_Unbound := NULL_STR_UNBOUND; -- Single channel
              Metallic             : Str_Unbound := NULL_STR_UNBOUND; -- Single channel
              Roughness            : Str_Unbound := NULL_STR_UNBOUND; -- Single channel
              -- brdfLUT     : Str_Unbound   := NULL_STR_UNBOUND; -- RGB
              -- albedoMap           : Str_Unbound   := NULL_STR_UNBOUND; -- Single channel
              -- Ambient_Occlusion    : Str_Unbound   := NULL_STR_UNBOUND; -- Single channel
              -- Refraction           : Str_Unbound   := NULL_STR_UNBOUND; -- Single channel
          end case;
      end case;
    end record;
  package Hashed_Material is new Neo.Core.Hashed (Material_State);

  --------------
  -- Geometry --
  --------------

  -- For optimization and ease of use bounded arrays are used instead of pointers for the heirarchy of pointers
  subtype Bone_Index is Int_16_Unsigned;
  BAD_CHILD : constant Bone_Index := Bone_Index'First;

  MAX_GEOMETRY_NAME : constant Positive := 64;
  type Geometry_Name is new Str_8_Super (MAX_GEOMETRY_NAME);
  type Array_Geometry_Name is array (Positive range <>) of Geometry_Name;

  -- ???
  type Bone_State is record
      Name      : Geometry_Name;
      Parent    : Bone_Index;
      Transform : Transform_4D;
    end record;

  type Array_Bone_State            is array (Bone_Index range <>)                      of Bone_State;
  type Array_Bone_Transform        is array (Bone_Index range <>)                      of Transform_4D;
  type Array_Bone_Children         is array (Bone_Index range <>, Bone_Index range <>) of Bone_Index;
  type Array_Bone_Animation_Times  is array (Bone_Index range <>, Positive   range <>) of Real_32;
  type Array_Bone_Animation_Values is array (Bone_Index range <>, Positive   range <>) of Transform_4D;
  type Skeleton_State (Bone_Count, Max_Children : Bone_Index; Animation_Count, Animation_Frames : Positive) is record
      Pose             : Array_Bone_Transform        (1..Bone_Count);
      Bones            : Array_Bone_State            (1..Bone_Count);
      Bone_Children    : Array_Bone_Children         (1..Bone_Count, 1..Max_Children);
      Animation_Times  : Array_Bone_Animation_Times  (1..Bone_Count, 1..Animation_Frames);
      Animation_Values : Array_Bone_Animation_Values (1..Bone_Count, 1..Animation_Frames);
      Animations       : Array_Positive              (1..Animation_Count); -- Must be in acending order
      Animation_Names  : Array_Geometry_Name         (1..Animation_Count);
    end record;
  type Ptr_Skeleton_State is access all Skeleton_State;

  type Vertex_State is record
      Position  : Vector_3D;
      Normal    : Vector_3D;
      Texturing : Vector_2D;
    end record with Pack;

  -- We pack it all pretty tight here without OOP or controlled things for less overhead, one mesh per geometry object
  type Array_Bone_Weight is array (Bone_Index range <>) of Real_32;
  type Array_Bone_Index  is array (Bone_Index range <>) of Bone_Index;
  type Array_Vertex_State is array (Positive range <>) of Vertex_State;
  type Geometry_Object_State (Vertex_Count, Index_Count : Positive; Bone_Influence_Count, Bone_Weight_Count : Bone_Index) is record
      Name             : Geometry_Name;
      --Default_Material : Material_State (Surface_Domain);
      Skeleton         : Ptr_Skeleton_State;
      Visible          : Bool;
      Shadow           : Bool;
      Motion_Blur      : Bool;
      Bone_Influences  : Array_Bone_Index         (1..Bone_Influence_Count);
      Bone_Indicies    : Array_Bone_Index         (1..Bone_Weight_Count);
      Bone_Weights     : Array_Bone_Weight        (1..Bone_Weight_Count);
      Verticies        : Array_Vertex_State       (1..Vertex_Count);
      Indicies         : Array_x2_Int_32_Unsigned (1..Index_Count, 1..3); -- Zero based for reasons
    end record;
  type Ptr_Geometry_Object_State is access all Geometry_Object_State;

  -- A "mesh set" is a series of models that share the same skeleton
  type Array_Geometry_Object is array (Positive range <>) of Ptr_Geometry_Object_State;
  type Mesh_State (Geometry_Count : Positive) is record
      Geometries : Array_Geometry_Object (1..Geometry_Count);
      Name       : Geometry_Name;
      Skeleton   : Ptr_Skeleton_State;
    end record;
  type Ptr_Mesh_State is access all Mesh_State;
  package Hashed_Mesh is new Neo.Core.Hashed (Ptr_Mesh_State);

  -----------
  -- Scene --
  -----------

--    type Node_Kind is (Bone_Node, Basic_Node, Camera_Node, Light_Node, Geometry_Node);
--
--    type Node_State (Kind : Entity_Kind) is record
--        Children  : Vector_Ptr_Node.Unsafe.Vector;
--        Animation : Vector_Ptr_Animation.
--        Transform : Transform_4D;
--        case Kind is
--          when Geometry_Node => Mesh      : Ptr_Mesh_State;
--                                Materials : Vector_Ptr_Material.Unsafe.Vector;
--          when Light_Node    => Light     : Ptr_Light_State;
--          when Camera_Node   => Camera    : Ptr_Camera_State;
--      end record;

  ----------
  -- IO --
  ----------

  -- Load any type of 3D scene data - animations, materials, you name it
  procedure Load_Model (Path : Str);
  --procedure Free_Mesh (Path : Str);
  --function Get_Mesh (Name : Str) return Ptr_Mesh_Set_State;
  --procedure Free (Path : Str);

  -----------
  -- Cache --
  -----------

--   Maps       : Hashed_Map.Safe_Map;
  Meshes     : Hashed_Mesh.Safe_Map;
--   Cameras    : Hashed_Camera.Safe_Map;
  Materials  : Hashed_Material.Safe_Map;
--   Animations : Hashed_Animation.Safe_Map;
end;
