
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

-- Unified 3D model, shader, and surface type definitions
package Neo.Data.Model is

  -------------
  -- Formats --
  -------------

  type Format_Kind is (Id_Tech_Format,    -- web.archive.org/web/20121018035741/http://www.modwiki.net/wiki/Modelling
                       Wavefront_Format); -- web.archive.org/web/20160810123453/https://www.cs.utah.edu/~boulos/cs3505/obj_spec.pdf

  ------------
  -- Camera --
  ------------

  type Camera_Frame_State is record
      FOV         : Real          := 0.0;
      Point       : Point_3D      := ZERO_POINT_3D;
      Orientation : Quaternion_4D := ZERO_QUATERNION_4D;
    end record;
  package Vector_Camera_Frame is new Neo.Core.Vectors (Camera_Frame_State);

  type Camera_State is record
      Frame_Rate : Positive := 1;
      Frames     : Vector_Camera_Frame.Unsafe.Vector;
      Cuts       : Vector_Positive.Unsafe.Vector;
    end record;
  package Hashed_Camera is new Neo.Core.Hashed (Camera_State);

  ---------------
  -- Animation --
  ---------------

  type Bounding_State is record
      A, B : Point_3D := ZERO_POINT_3D;
    end record;

  type Joint_State is record
      Name         : Str_8 (1..32) := (others => NULL_CHAR_8);
      Point        : Point_3D      := ZERO_POINT_3D;
      Orientation  : Quaternion_4D := ZERO_QUATERNION_4D;
      Parent_Index : Int_64        := -2;
    end record;
  package Treed_Joint  is new Neo.Core.Trees   (Joint_State);
  package Vector_Joint is new Neo.Core.Vectors (Joint_State);

  type Animation_Frame_State is record
      Bounding : Bounding_State := (others => <>);
      Skeleton : Treed_Joint.Unsafe.Tree;
    end record;
  package Vector_Animation_Frame is new Neo.Core.Vectors (Animation_Frame_State);
 
  type Animation_State is record
      Frame_Rate : Positive := 1;
      Frames     : Vector_Animation_Frame.Unsafe.Vector;
    end record;
  package Hashed_Animation is new Neo.Core.Hashed (Animation_State);

  ----------
  -- Mesh --
  ----------

  type Vertex_Kind is (Animated_Vertex, Static_Vertex);
  
  type Weight_State is record
      Point  : Point_3D     := ZERO_POINT_3D; 
      Amount : Real_Percent := 0.0;
    end record with Size => Real_64'Size * 4; -- Must consist of nothing but Real_64s for GPU processing
  package Vector_Weight is new Neo.Core.Vectors (Weight_State);

  type Indexed_Weight_State is record
      Joint_Index : Natural      := 0; -- Could have used a tree cursor instead of an joint index but that would be ineffecient
      Data        : Weight_State := (others => <>);
    end record;
  package Vector_Indexed_Weight is new Neo.Core.Vectors (Indexed_Weight_State);

  type Animated_Vertex_State is record
      Texture                    : Point_2D        := ZERO_POINT_2D;
      Start_Weight, Weight_Count : Int_64_Unsigned := 1;
      Velocity                   : Real_64         := 0.0;
    end record with Convention => C;
  package Vector_Animated_Vertex is new Neo.Core.Vectors (Animated_Vertex_State); 

  type Static_Vertex_State is record
      Texture  : Point_2D := ZERO_POINT_2D;
      Point    : Point_3D := ZERO_POINT_3D;
      Normal   : Point_3D := ZERO_POINT_3D;
      Velocity : Real_64  := 0.0;
    end record with Convention => C;
  package Vector_Static_Vertex is new Neo.Core.Vectors (Static_Vertex_State); 

  type Triangle_State is record
      A, B, C : Int_Unsigned := 0;
    end record;
  package Vector_Triangle is new Neo.Core.Vectors (Triangle_State);

  type Animated_Surface_State is record
      Material : Str_Unbound := NULL_STR_UNBOUND;
      Vertices : Vector_Animated_Vertex.Unsafe.Vector; -- Starts at 0
      Indicies : Vector_Triangle.Unsafe.Vector;
      Weights  : Vector_Indexed_Weight.Unsafe.Vector; -- Seporated out of animated vertices for GPU skinning
    end record;
  package Vector_Animated_Surface is new Neo.Core.Vectors (Animated_Surface_State);

  type Static_Surface_State is record
      Material : Str_Unbound := NULL_STR_UNBOUND;
      Vertices : Vector_Static_Vertex.Unsafe.Vector; -- Starts at 0
      Indicies : Vector_Triangle.Unsafe.Vector;
      Bounding : Bounding_State;
    end record;
  package Vector_Static_Surface is new Neo.Core.Vectors (Static_Surface_State);

  type Mesh_State (Is_Animated : Bool := False) is record
      case Is_Animated is
        when False => Static_Surfaces : Vector_Static_Surface.Unsafe.Vector;
        when True =>
          Animated_Surfaces : Vector_Animated_Surface.Unsafe.Vector;
          Animations        : Vector_Str_Unbound.Unsafe.Vector;
          Base_Frame        : Animation_Frame_State := (others => <>);
      end case;
    end record;
  package Vector_Mesh is new Neo.Core.Vectors (Mesh_State);
  package Hashed_Mesh is new Neo.Core.Hashed (Mesh_State);
  
  -----------
  -- Level --
  -----------

  type Partition_Kind is (Normal_Partition, Area_Partition, Opaque_Partition);

  type Partition_Node_State (Kind : Partition_Kind := Normal_Partition) is record
      Is_Positive : Bool := True;
      case Kind is
        when Area_Partition   => Area_Id : Int      := 0;
        when Normal_Partition => Plane   : Plane_4D := (others => <>);
      when others => null; end case;
    end record;
  package Treed_Partition_Node is new Neo.Core.Trees (Partition_Node_State);

  type Brush_Side_State is record
      Material : Str_Unbound  := NULL_STR_UNBOUND;
      Texture  : Transform_3D := (others => <>);
      Origin   : Point_3D     := (others => <>);
      Plane    : Plane_4D     := (others => <>);
    end record;
  package Vector_Brush_Side  is new Neo.Core.Vectors (Brush_Side_State);
  package Vector_Brush       is new Neo.Core.Vectors (Vector_Brush_Side.Unsafe.Vector);
  package Hashed_Str_Unbound is new Neo.Core.Hashed (Str_Unbound);

  type Entity_Kind is (
    Character_Entity,
    Camera_Entity,
    AI_Entity,
    Moveable_Entity,
    Information_Entity,
    Speaker_Entity,
    Activator_Entity,
    Animated_Entity,
    Light_Entity,
    Camera_View_Entity,
    Clip_Model_Entity,
    Door_Entity,
    Earthquake_Entity,
    Elevator_Entity,
    Emitter_Entity,
    Explosion_Entity,
    Forcefield_Entity,
    Fracture_Entity,
    Effects_Entity,
    Portal_Entity,
    Remove_Entity);

  type Physics_Kind is (
    Rigid_Body_Physics,
    Soft_Body_Physics,
    No_Physics,
    Simple_Physics);

   type Damage_State is record
      Nothing : Boolean;
      --"damage" "[Damage Ammount]" 
      --"kickDir" "0 0 0" 
      --"mtr_blob" "genericDamage" 
      --"blob_time" "0" 
      --"blob_size" "0" 
      --"blob_offset_x" "0" 
      --"knockback" "0" 
      --"kick_time" "0" 
      --"kick_amplitude" "0" 
      --"dv_time" "100" 
    end record;

  type Entity_State (Kind : Entity_Kind := Entity_Kind'First) is record
      Point       : Point_3D      := (others => <>);
      Orientation : Quaternion_4D := (others => <>);
      Model       : Str_Unbound   := NULL_STR_UNBOUND;
      Visible     : Bool          := False;
      Physics     : Physics_Kind  := No_Physics;
      Health      : Int_64        := 0;
      Key_Values  : Hashed_Str_Unbound.Unsafe.Map;
      Patches     : Vector_Static_Surface.Unsafe.Vector;
      Targets     : Vector_Str_Unbound.Unsafe.Vector;
      Brushes     : Vector_Brush.Unsafe.Vector;
      case Kind is
        when Light_Entity =>
          Is_Broken    : Bool          := False;
          Broken_Model : Str_Unbound   := NULL_STR_UNBOUND;
          Frustum      : Frustum_State := (others => <>); 
          Glare_Material : Str_Unbound := NULL_STR_UNBOUND;

        when Character_Entity =>
          World : Positive      := 1;
          Area  : Positive      := 1;
          --Pose  : Pose_State    := (others => <>);
          View  : Frustum_State := (others => <>); 
          
        when Speaker_Entity =>
          Clip           : Str_Unbound    := NULL_STR_UNBOUND;
          --Position       : Position_State := (others => <>);
          Is_Mute        : Bool           := False;
          Is_Paused      : Bool           := False;
          Is_Playing     : Bool           := False;
          Min_Distance   : Real           := 0.0;
          Max_Distance   : Real           := 0.0;
          Current_Volume : Real_Percent   := 75.0;
          Current_Shake  : Real_Percent   := 75.0;
        when Door_Entity =>
          Lip              : Natural       := 0;
          Locked           : Bool          := False;
          Speed            : Real          := 10.0;
          Move_Orientation : Quaternion_4D := (others => <>);
          Close_Sound      : Str_Unbound   := NULL_STR_UNBOUND;
          Open_Sound       : Str_Unbound   := NULL_STR_UNBOUND;
          Locked_Sound     : Str_Unbound   := NULL_STR_UNBOUND;
        --when Hurt_Entity =>
        --  Damage : Damage_State := (others => <>);
        when others => null;
      end case;
    end record;
  package Hashed_Entity is new Neo.Core.Hashed (Entity_State);

  type Clip_State is record
      Sides         : Vector_Plane.Unsafe.Vector;
      Bounding      : Bounding_State := (others => <>);
      Is_Player     : Bool           := False;
      Is_Opaque     : Bool           := False;
      Is_Water      : Bool           := False;
      Is_Solid      : Bool           := False;
      Is_Monster    : Bool           := False;
      Is_Moveable   : Bool           := False; 
      Is_Bot        : Bool           := False;
      Is_Blood      : Bool           := False;
      Is_Trigger    : Bool           := False;
      Is_Body       : Bool           := False;
      Is_Flashlight : Bool           := False;
      Is_Corpse     : Bool           := False;
      Is_Animation  : Bool           := False;
      Is_Obstacle   : Bool           := False;
    end record;
  package Vector_Clip is new Neo.Core.Vectors (Clip_State);

  type Edge_State is record
      A, B      : Point_3D := (others => <>);
      Internal  : Natural  := 0; -- ???
      Num_Users : Natural  := 0;
    end record;
  package Vector_Edge is new Neo.Core.Vectors (Edge_State);

  type Polygon_State is record
      Material : Str_Unbound    := NULL_STR_UNBOUND;
      Bounding : Bounding_State := (others => <>);
      Plane    : Plane_4D       := (others => <>);
      Edges    : Vector_Natural.Unsafe.Vector;
    end record;
  package Vector_Polygon is new Neo.Core.Vectors (Polygon_State);

  type Collision_Node_State is record
      Is_Back   : Bool           := False;
      Distance  : Real           := 0.0;
      Dimension : Dimension_Kind := X_Dimension;
    end record;
  package Treed_Collision_Node is new Neo.Core.Trees (Collision_Node_State);

  type Collision_State is record
      Name     : Str_Unbound := NULL_STR_UNBOUND;
      Polygons : Vector_Polygon.Unsafe.Vector;
      Clipping : Vector_Clip.Unsafe.Vector;
      Edges    : Vector_Edge.Unsafe.Vector;
      Nodes    : Treed_Collision_Node.Unsafe.Tree;
    end record;
  package Vector_Collision is new Neo.Core.Vectors (Collision_State);

  -- type Area_Kind is (Ladder_Area, Floor_Area, Liquid_Area);
  --
  -- type Area_Side_State is record
  --     Kind  : Area_Kind;
  --     Plane : Plane_3D;
  --   end record;
  -- package Vector_Area is new Neo.Core.Vectors (Area_Side_State);
  --
  -- type Area_State is record
  --     Bounding          : Bounding_State;
  --     Sides             : Vector_Area.Unsafe_Vector;
  --     Center            : Point_3D;
  --     Is_Ledge          : Bool;
  --     Is_Liquid         : Bool;
  --     Is_Walk_Reachable : Bool;
  --     Is_Fly_Reachable  : Bool;
  --     Contains_Water    : Bool;
  --     Contains_Portal   : Bool;
  --     Contains_Obstacle : Bool;
  --     Can_Walk          : Bool;
  --     Can_Crouch        : Bool;
  --     Can_Ledge_Walk    : Bool;
  --     Can_Barrier_Jump  : Bool;
  --     Can_Jump          : Bool;
  --     Can_Fly           : Bool;
  --     Can_Go_By_Special : Bool;
  --     Can_Go_By_Water   : Bool;
  --     Can_Go_By_Air     : Bool;
  --   end record;
  -- package Vector_Area is new Neo.Core.Vectors (Area_State);
  --
  -- type Area_Cluster_State is record
  --   end record;
  --
  -- type AI_State is record
  --     Bounding        : Bounding_State;
  --     Gravity         : Vector_3D;
  --     Max_Step_Height : Real;
  --     Min_Floor_Cos   : Real;
  --     Clusters        : Vector_Cluster.Unsafe.Vector;
  --   end record;

  type Level_State is record
      Geometry_CRC : Int_64_Unsigned := 0;
      Collisions   : Vector_Collision.Unsafe.Vector;
      Entities     : Hashed_Entity.Unsafe.Map;
      Partitions   : Treed_Partition_Node.Unsafe.Tree;
      -- AI : AI_State;
    end record;
  package Hashed_Level_State is new Neo.Core.Hashed (Level_State);

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
  type Cube_Kind    is (No_Cube,        Image_Cube,      Camera_Cube,           Mirror_Cube,     Skybox_Cube);
  type Domain_Kind  is (Surface_Domain, Decal_Domain,    Post_Process_Domain,   Light_Domain,    Menu_Domain);
  type Blend_Kind   is (Opaque_Blend,   Masked_Blend,    Additive_Blend,        Modulate_Blend,  Mirror_Blend,  Remote_Blend);
  type Shading_Kind is (Unlit_Shading,  Lit_Shading,     Subsurface_Shading,    Profile_Shading, Skin_Shading,  Clear_Coat_Shading);
  type Deform_Kind  is (No_Deform,      Sprite_Deform,   Tube_Deform,           Flare_Deform,    Expand_Deform, Move_Deform,
                        Eye_Deform,     Particle_Deform, Small_Particle_Deform, Turbulent_Deform);

  type Material_State (Kind : Domain_Kind := Surface_Domain) is record
      Has_Smoothed_Tan : Bool         := True;
      Is_Two_Sided     : Bool         := False;
      Is_Ambient       : Bool         := False;
      Clamp            : Clamp_Kind   := No_Clamp;
      Filter           : Filter_Kind  := Linear_Filter;
      Surface          : Surface_kind := Thin_Rock_Surface;
      Deform           : Deform_Kind  := No_Deform;
      Color_Mod        : Color_State  := NULL_COLOR;
      Transform        : Transform_4D := (1.0, 0.0, 0.0, 0.0,
                                          0.0, 1.0, 0.0, 0.0,
                                          0.0, 0.0, 0.0, 0.0); -- ???
      case Kind is
        when Menu_Domain  => Menu_Id : Str_Unbound := NULL_STR_UNBOUND;
        when others =>
          Base_Color : Str_Unbound := NULL_STR_UNBOUND; -- RGBA
          case Kind is
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

  --------
  -- IO --
  --------

  function Load (Path : Str) return Level_State;
  function Load (Path : Str) return Camera_State;
  function Load (Path : Str) return Animation_State;
  function Load (Path : Str) return Mesh_State;
  function Load (Path : Str) return Hashed_Material.Unsafe.Map;
end;
