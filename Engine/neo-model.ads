
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

with Neo.Vector;
with Neo.Hashed;
with Neo.Ordered;
with Neo.Parsing; use Neo.Parsing;
with Neo.Math;    use Neo.Math;

package Neo.Model is

  -------------
  -- Formats --
  -------------

  type Format_Kind is (Id_Technology_Format); -- http://web.archive.org/web/20121018035741/http://www.modwiki.net/wiki/Modelling

  ------------
  -- Camera --
  ------------

  -- Camera animation frame
  type Frame_State is record
      Point         : Point_3D;
      Orientation   : Quaternion_4D;
      Field_Of_View : Real;
    end record;
  package Vector_Frame is new Vectors (Frame_State);

  type Camera_State (Cut_Num : Natural := 0; Frame_Num : Positive := 1) is record
      Frame_Rate : Positive;
      Frames     : Vector_Frame.Unsafe_Array (1..Frame_Num);
      case Cut_Num is when 0      => null;
                      when others => Cuts : Array_Positive (1..Cut_Num);
    end record;
  package Hashed_Camera is new Hashed_Maps (Camera_State);

  ---------------
  -- Animation --
  ---------------

  -- Bounding box
  type Bounds_Array is array (1..2) of Point_3D;

  -- Skeletal joint
  type Joint_State is record
      Name        : Str_Unbound;
      Point       : Point_3D;
      Orientation : Quaternion_4D;
    end record;
  package Treed_Joint is new Trees (Joint_State);

  -- Skeleton with pre-computed bounds
  type Frame_State is record
      Skeleton : Treed_Joint.Unsafe.Tree;
      Bounds   : Bounds_Array;
    end record;
  package Vector_Frame is new Vectors (Frame_State);
 
  type Animation_State (Frame_Num : Positive := 1) is record
      Frame_Rate : Positive;
      Frames     : Vector_Frame_State.Unsafe_Array (1..Frame_Num);
    end record;
  package Hashed_Animation is new Hashed_Maps (Animation_State);

  ----------
  -- Mesh --
  ----------

  type Weight_State is record
      Joint  : Treed_Joint.Cursor;
      Point  : Point_3D; 
      Amount : Real_Percent;
    end record;
  package Vector_Weight is new Vectors (Weight_State);

  type Vertex_State (Weight_Num : Natural := 0) is record
      Texture : Point_2D;
      Point   : Point_3D;
      case Weight_Num is
        when 0      => null;
        when others => Weights : Vector_Weight.Unsafe_Array (1..Weight_Num); end case;
    end record;
  package Vector_Vertex is new Vectors (Vector_State);

  type Array_Triangle is array (1..3) of Real;
  package Vector_Triangle is new Vectors (Array_Triangle);

  type Mesh_State (Vert_Num, Tri_Num : Positive := 1) is record
      Material : Str_Unbound;
      Vertices : Vector_Vertex.Unsafe_Array (1..Vert_Num);   
      Indicies : Vector_Triangle.Unsafe_Array (1..Tri_Num);
      Bounds   : Bounds_Array;
    end record;
  package Vector_Group is new Vectors (Mesh_State);

  type Skeletal_Mesh_State (Mesh_Num : Positive := 1) is record
      Groups     : Vector_Group.Unsafe_Array (1..Group_Num);
      Skeleton   : Treed_Joint.Unsafe.Tree;
      Animations : Vector_Str_Unbound.Unsafe.Vector;
    end record;
  package Hashed_Mesh is new Hashed_Maps (Skeletal_Mesh_State);

  -----------
  -- Level --
  -----------
  --
  -- A Level is used to represent a fully populated game world. It is complex consists of many properties and including many data-points
  -- that exist for optimization (like dividing visual spaces within the level into trees for example). A more detailed description
  -- of this process is below.
  --
  -- 
  --

  type Clip_Kind is (No_Clip,    Player_Clip,  Opaque_Clip, Water_Clip,      Solid_Clip,  Monster_Clip,   Moveable_Clip, Bot_Clip,       
                     Blood_Clip, Trigger_Clip, Body_Clip,   Flashlight_Clip, Corpse_Clip, Animation_Clip, Obstacle_Clip);

  type Edge_State is record
      Start, End : Natural;
    end record;
  package Vector_Edge is new Vectors (Edge_State);

  type Side_State is record
      Clip  : Clip_Kind;
      Plane : Real_Plane_3D;
    end record;
  package Vector_Side is new Vectors (Side_State);

  type Brush_State is record
      Bounds : Brush_State;
      Sides  : Vector_Side.Unsafe.Vector;
    end record;
  package Vector_Brush        is Vectors (Brush_State);
  package Vector_Vector_Brush is Vectors (Vector_Brush.Unsafe.Vector);

  type Surface_State is record
      Material : Str_Unbound;
      Bounds   : Bounds_Array;
      Plane    : Plane;
      Edges    : Vector_Natural.Unsafe.Vector;
    end record;
  package Vector_Surface        is new Vectors (Surface_State);
  package Vector_Vector_Surface is new Vectors (Vector_Surface.Unsafe.Vector);

  type Patch_State (Vert_Num, Tri_Num : Positive := 1) is record
      Edge_Verticies : Vector_Edge.Unsafe.Vector;
      Edge_Triangles : Edge_State;
      Mesh           : Mesh_State (Animated => False);
      Subdivision_Y  : Natural;
      Subdivision_X  : Natural;
      Width          : Natural;
      Height         : Natural;
    end record;
  package Vector_Patch is new Vectors (Patch_State);

  type Brush_State is record
      Material : Str_Unbound;
      Texture  : Point_3D; -- !!!
      Origin   : Point_3D;
      Plane    : Plane;
    end record;
  package Vector_Brush        is new Vectors (Brush_State);
  package Vector_Vector_Brush is new Vectors (Vector_Brush.Unsafe.Vector);

  type Collision_Section_State is record
      Partition : Dimension_Kind;
      Distance  : Natural;
    end record;
  package Treed_Collision_Section is new Trees (Collision_Section_State);
  package Treed_Plane_3D          is new Trees (Plane_3D_State);

  type Level_State is record
      Geometry_CRC       : Int_32_Unsigned := 0;
      Edge_Verticies     : Vector_Edge.Unsafe.Vector;
      Patches            : Vector_Patch.Unsafe.Vector;
      Brushes            : Vector_Vector_Brush.Unsafe.Vector;
      Brush_Blocks       : Vector_Vector_Brush.Unsafe.Vector;
      Entities           : Vector_Hashed_Str_Unbound.Unsafe.Vector;
      Collision_Sections : Treed_Collision_Section.Unsafe.Vector;
      Surface_Blocks     : Vector_Vector_Surface.Unsafe.Vector;
      Surface_Sections   : Treed_Plane.Unsafe.Vector;
    end record;
  package Hashed_Level_State is new Hashed_Maps (Level_State);

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
      Noise_Reduction       : Real_Percent; -- https://web.archive.org/web/20160420185754/http://www.archtoolbox.com/representation/architectural-concepts/acoustics.html
      Physics_Hit_Sounds    : Vector_Str_Unbound; 
      Bullet_Hole_Materials : Vector_Str_Unbound;
      Bullet_Hole_Sounds    : Vector_Str_Unbound;
      Knife_Slash_Materials : Vector_Str_Unbound;
      Knife_Slash_Sounds    : Vector_Str_Unbound;
      Explosion_Materials   : Vector_Str_Unbound;
      Explosion_Sounds      : Vector_Str_Unbound;
      Land_Light_Materials  : Vector_Str_Unbound;
      Land_Hard_Materials   : Vector_Str_Unbound;
      Footstep_Materials    : Vector_Str_Unbound; -- Scale to feet of model ???
      Walk_Light_Sounds     : Vector_Str_Unbound;
      Walk_Hard_Sounds      : Vector_Str_Unbound:
    end record;

  -- Returns constant Surface_State for a given Surface_Kind. All surfaces are defined in the body of this package (neo-model.adb).
  function Get_Surface (Kind : Surface_Kind) return Surface_State;

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
  --   https://web.archive.org/web/20150303182930/https://docs.unrealengine.com/latest/INT/Engine/Rendering/Materials/MaterialProperties/
  --   https://web.archive.org/web/20160305174701/https://www.iddevnet.com/doom3/materials.php
  --

  type Domain_Kind   is (Surface_Domain, Decal_Domain,   Post_Process_Domain, Light_Domain,    Menu_Domain);
  type Blend_Kind    is (Opaque_Blend,   Masked_Blend,   Additive_Blend,      Modulate_Blend,  Mirror_Blend, Remote_Blend);
  type Shading_Kind  is (Unlit_Shading,  Lit_Shading,    Subsurface_Shading,  Profile_Shading, Skin_Shading, Clear_Coat_Shading);
  type Cube_Map_Kind is (No_Cube_Map,    Image_Cube_Map, Camera_Cube_Map,     Mirror_Cube_Map);

  type Stage_State (Static : Bool := True) is record
      case Static is
        when True  => Texture : Str_Unbound;
        when False =>
          Fragment_Program  : Str_Unbound;
          Fragment_Textures : Vector_Str_Unbound;
      end case;
    end record;

  type Material_State (Kind : Domain_Kind := Surface_Domain; Static : Bool := True) is record
      Shadow_Self    : Bool;
      Cast_Shadow    : Bool;
      Two_Sided      : Bool;
      Smoothed_Tan   : Bool;
      Transform      : Transform_4D;
      Vertex_Program : Str_Unbound;
      case Kind is
        when Light_Domain => Pattern : Stage_State (Static);
        when Menu_Domain  => Menu_Id : Str_Unbound;
        when others =>
          Blend                : Blend_Kind;  
          Cube_Map             : Cube_Map_Kind; -- When No_Cube_Map or Mirror_Cube_Map the component Cube_Map_Texture is ignored
          Cube_Map_Texture     : Str_Unbound;   -- Assumes _px, _py, _pz, _nx, _ny, _nz for the positive and negative sides
          Base_Color           : Stage_State (Static);
          Metallic             : Stage_State (Static);
          Specular             : Stage_State (Static);
          Roughness            : Stage_State (Static);
          Emissive_Color       : Stage_State (Static);
          Opacity              : Stage_State (Static);
          Opacity_Mask         : Stage_State (Static);
          Normal               : Stage_State (Static);
          Displacement         : Stage_State (Static);
          Subsurface_Color     : Stage_State (Static);
          Clear_Coat           : Stage_State (Static);
          Clear_Coat_Roughness : Stage_State (Static);
          Ambient_Occlusion    : Stage_State (Static);
          Refraction           : Stage_State (Static);
      when others => null; end case;
    end record;
  package Hashed_Material is new Hashed (Material_State);

  --------
  -- IO --
  --------

  function Load (Path : Str) return Mesh_State;
  function Load (Path : Str) return Level_State;
  function Load (Path : Str) return Camera_State;
  function Load (Path : Str) return Material_State;
end;
