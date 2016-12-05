
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

with Neo.Core.Math;   use Neo.Core.Math;
with Neo.Core.Arrays; use Neo.Core.Arrays;
with Neo.Core.Trees;
with Neo.Core.Hashed;
with Neo.Core.Vectors;
with Neo.Core.Ordered;

-- Unified 3D model, shader, and surface type definitions
package Neo.Data.Model is

  -------------
  -- Formats --
  -------------

  type Format_Kind is (Id_Tech_Format,    -- http://web.archive.org/web/20121018035741/http://www.modwiki.net/wiki/Modelling
                       Wavefront_Format); -- https://web.archive.org/web/20160810123453/https://www.cs.utah.edu/~boulos/cs3505/obj_spec.pdf

  ------------
  -- Camera --
  ------------

  type Camera_Frame_State is record
      Point  : Point_3D;
      Orient : Quaternion_4D;
      FOV    : Real;
    end record;
  package Vector_Camera_Frame is new Vectors (Camera_Frame_State);

  type Camera_State is record
      Frame_Rate : Positive;
      Frames     : Vector_Camera_Frame.Unsafe.Vector;
      Cuts       : Vector_Positive.Unsafe.Vector;
    end record;
  package Hashed_Camera is new Hashed (Camera_State);

  ---------------
  -- Animation --
  ---------------

  type Bounding_State is record
      A, B : Point_3D;
    end record;

  type Joint_State is record
      Name   : Str_Unbound;
      Point  : Point_3D;
      Orient : Quaternion_4D;
    end record;
  package Treed_Joint is new Trees (Joint_State);

  type Animation_Frame_State is record
      Bounding : Bounding_State;
      Skeleton : Treed_Joint.Unsafe.Tree;
    end record;
  package Vector_Animation_Frame is new Vectors (Animation_Frame_State);
 
  type Animation_State is record
      Frame_Rate : Positive;
      Frames     : Vector_Animation_Frame.Unsafe.Vector;
    end record;
  package Hashed_Animation is new Hashed (Animation_State);

  ----------
  -- Mesh --
  ----------

  type Weight_State is record
      Joint  : Treed_Joint.Cursor;
      Point  : Point_3D; 
      Amount : Real_Percent;
    end record;
  package Vector_Weight is new Vectors (Weight_State);

  type Vertex_State (Has_Weights : Bool := False) is record
      Texture : Point_2D;
      case Has_Weights is
        when True  =>
          Weights : Vector_Weight.Unsafe.Vector;
        when False =>
          Point  : Point_3D;
          Normal : Point_3D;
      end case;
    end record;
  package Vector_Vertex is new Vectors (Vertex_State); -- Indicies start at 0 !!!

  type Triangle_Array is array (1..3) of Natural;
  package Vector_Triangle is new Vectors (Triangle_Array);

  type Mesh_State is record
      Material : Str_Unbound;
      Vertices : Vector_Vertex.Unsafe.Vector; 
      Indicies : Vector_Triangle.Unsafe.Vector;
      Bounding : Bounding_State;
    end record;
  package Vector_Mesh is new Vectors (Mesh_State);

  type Skeletal_Mesh_State is record
      Groups     : Vector_Mesh.Unsafe.Vector;
      Skeleton   : Treed_Joint.Unsafe.Tree;
      Animations : Vector_Str_Unbound.Unsafe.Vector;
    end record;
  package Hashed_Mesh is new Hashed (Skeletal_Mesh_State);

  -----------
  -- Level --
  -----------

  type Partition_Kind is (Normal_Partition, Area_Partition, Opaque_Partition);
  type Partition_Node_State (Kind : Partition_Kind := Normal_Partition) is record
      Is_Positive : Bool;
      case Kind is
        when Area_Partition   => Area_Id : INteger;
        when Normal_Partition => Plane   : Plane_4D;
        when Opaque_Partition => null;
      end case;
    end record;
  package Treed_Partition_Node is new Trees (Partition_Node_State);

  type Brush_Side_State is record
      Material : Str_Unbound;
      Texture  : Transform_3D;
      Origin   : Point_3D := (0.0, 0.0, 0.0);
      Plane    : Plane_4D;
    end record;
  package Vector_Brush_Side is new Vectors (Brush_Side_State);
  package Vector_Brush      is new Vectors (Vector_Brush_Side.Unsafe.Vector);
  package Hashed_Str_Unbound is new Hashed (Str_Unbound);
  type Entity_State is record
      Origin     : Point_3D;
      Key_Values : Hashed_Str_Unbound.Unsafe.Map;
      Brushes    : Vector_Brush.Unsafe.Vector;
      Patches    : Vector_Mesh.Unsafe.Vector;
    end record;
  package Vector_Entity is new Vectors (Entity_State);

  package Vector_Plane is new Vectors (Plane_4D);
  type Clip_State is record
      Bounding      : Bounding_State;
      Sides         : Vector_Plane.Unsafe.Vector;
      Is_Player     : Bool := False;
      Is_Opaque     : Bool := False;
      Is_Water      : Bool := False;
      Is_Solid      : Bool := False;
      Is_Monster    : Bool := False;
      Is_Moveable   : Bool := False; 
      Is_Bot        : Bool := False;
      Is_Blood      : Bool := False;
      Is_Trigger    : Bool := False;
      Is_Body       : Bool := False;
      Is_Flashlight : Bool := False;
      Is_Corpse     : Bool := False;
      Is_Animation  : Bool := False;
      Is_Obstacle   : Bool := False;
    end record;
  package Vector_Clip is new Vectors (Clip_State);

  type Edge_State is record
      A, B      : Point_3D;
      Internal  : Natural; -- ???
      Num_Users : Natural;
    end record;
  package Vector_Edge is new Vectors (Edge_State);

  type Polygon_State is record
      Material : Str_Unbound;
      Bounding : Bounding_State;
      Plane    : Plane_4D;
      Edges    : Vector_Natural.Unsafe.Vector;
    end record;
  package Vector_Polygon is new Vectors (Polygon_State);

  type Collision_Node_State is record
      Is_Back   : Bool;
      Distance  : Real;
      Dimension : Dimension_Kind;
    end record;
  package Treed_Collision_Node is new Trees (Collision_Node_State);

  type Collision_State is record
      Name     : Str_Unbound;
      Polygons : Vector_Polygon.Unsafe.Vector;
      Clipping : Vector_Clip.Unsafe.Vector;
      Edges    : Vector_Edge.Unsafe.Vector;
      Nodes    : Treed_Collision_Node.Unsafe.Tree;
    end record;
  package Vector_Collision is new Vectors (Collision_State);

  type AI_State is record
      Bounding        : Bounding_State;
      Gravity         : Vector_3D;
      Max_Step_Height : Real;
      Min_Floor_Cos   : Real;
    end record;

  type Level_State is record
      Geometry_CRC : Int_64_Unsigned;
      Collisions   : Vector_Collision.Unsafe.Vector;
      Entities     : Vector_Entity.Unsafe.Vector;
      Partitions   : Treed_Partition_Node.Unsafe.Tree;
      AI           : AI_State;
    end record;
  package Hashed_Level_State is new Hashed (Level_State);

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
      Density               : Real_Percent;
      Friction              : Real_Percent;
      Restitution           : Real_Percent;
      Noise_Reduction       : Real_Percent; -- http://www.archtoolbox.com/representation/architectural-concepts/acoustics.html
      Physics_Hit_Sounds    : Vector_Str_Unbound.Unsafe.Vector; 
      Bullet_Hole_Materials : Vector_Str_Unbound.Unsafe.Vector;
      Bullet_Hole_Sounds    : Vector_Str_Unbound.Unsafe.Vector;
      Knife_Slash_Materials : Vector_Str_Unbound.Unsafe.Vector;
      Knife_Slash_Sounds    : Vector_Str_Unbound.Unsafe.Vector;
      Explosion_Materials   : Vector_Str_Unbound.Unsafe.Vector;
      Explosion_Sounds      : Vector_Str_Unbound.Unsafe.Vector;
      Land_Light_Materials  : Vector_Str_Unbound.Unsafe.Vector;
      Land_Hard_Materials   : Vector_Str_Unbound.Unsafe.Vector;
      Footstep_Materials    : Vector_Str_Unbound.Unsafe.Vector; -- Scale to feet of model ???
      Walk_Light_Sounds     : Vector_Str_Unbound.Unsafe.Vector;
      Walk_Hard_Sounds      : Vector_Str_Unbound.Unsafe.Vector;
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
  --   https://web.archive.org/web/20150303182930/https://docs.unrealengine.com/latest/INT/Engine/Rendering/Materials/MaterialProperties/
  --   https://web.archive.org/web/20160305174701/https://www.iddevnet.com/doom3/materials.php
  --

  type Cube_Map_Kind is (No_Cube_Map,    Image_Cube_Map,  Camera_Cube_Map,       Mirror_Cube_Map);
  type Domain_Kind   is (Surface_Domain, Decal_Domain,    Post_Process_Domain,   Light_Domain,    Menu_Domain);
  type Blend_Kind    is (Opaque_Blend,   Masked_Blend,    Additive_Blend,        Modulate_Blend,  Mirror_Blend,  Remote_Blend);
  type Shading_Kind  is (Unlit_Shading,  Lit_Shading,     Subsurface_Shading,    Profile_Shading, Skin_Shading,  Clear_Coat_Shading);
  type Deform_Kind   is (No_Deform,      Sprite_Deform,   Tube_Deform,           Flare_Deform,    Expand_Deform, Move_Deform,
                         Eye_Deform,     Particle_Deform, Small_Particle_Deform, Turbulent_Deform);

  type Stage_State (Static : Bool := True) is record
      case Static is
        when True  => Texture : Str_Unbound;
        when False =>
          Fragment_Program  : Str_Unbound;
          Fragment_Textures : Vector_Str_Unbound.Unsafe.Vector;
      end case;
    end record;

  type Material_State (Kind : Domain_Kind := Surface_Domain; Static : Bool := True) is record
      Shadow_Self    : Bool := True;
      Cast_Shadow    : Bool := True;
      Two_Sided      : Bool := False;
      Smoothed_Tan   : Bool := True;
      Transform      : Transform_4D;
      Vertex_Program : Str_Unbound;
      Surface        : Surface_kind;
      Deform         : Deform_Kind;
      case Kind is
        when Light_Domain => Pattern : Stage_State (Static);
        when Menu_Domain  => Menu_Id : Str_Unbound;
        when others =>
          Blend                : Blend_Kind;  
          Cube_Map             : Cube_Map_Kind; -- When No_Cube_Map or Mirror_Cube_Map the component Cube_Map_Texture is ignored
          Cube_Map_Texture     : Str_Unbound;   -- Assumes _px, _py, _pz, _nx, _ny, _nz for the positive and negative sides
          Base_Color           : Stage_State (Static); -- RGB
          Metallic             : Stage_State (Static); -- Single channel
          Specular             : Stage_State (Static); -- Single channel
          Roughness            : Stage_State (Static); -- Single channel
          Emissive_Color       : Stage_State (Static); -- RGB
          Opacity              : Stage_State (Static); -- Single channel
          Normal               : Stage_State (Static); -- Single channel
          Displacement         : Stage_State (Static); -- Single channel
          Subsurface_Color     : Stage_State (Static); -- RGB
          Clear_Coat           : Stage_State (Static); -- Single channel
          Clear_Coat_Roughness : Stage_State (Static); -- Single channel
          Ambient_Occlusion    : Stage_State (Static); -- Single channel ???
          Refraction           : Stage_State (Static); -- Single channel ???
      end case;
    end record;
  package Hashed_Material is new Hashed (Material_State);

  --------
  -- IO --
  --------

  function Load (Path : Str) return Level_State;
  function Load (Path : Str) return Camera_State;
  function Load (Path : Str) return Animation_State;
  function Load (Path : Str) return Skeletal_Mesh_State;
  function Load (Path : Str) return Hashed_Material.Unsafe.Map;
end;