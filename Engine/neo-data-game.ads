
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

-- Unified 3D model, shader, and surface type definitions
package Neo.Data.Game is

  --------------
  -- Material --
  --------------
  --
  -- A Material or "shader" is a grouping of texture maps and settings that define what a given entity, decal, or map element will
  -- look like and how it might be affected by light or camera position.
  --
  -- This implementation of materials offloads all of the work into separate shader programs so no logic has to be contained within
  -- the actual material record.
  --
  -- References.
  --   web.archive.org/web/20150303182930/https://docs.unrealengine.com/latest/INT/Engine/Rendering/Materials/MaterialProperties/
  --   web.archive.org/web/20160305174701/https://www.iddevnet.com/doom3/materials.php
  --
  -- A Surface defines how materials interact with all of the other portions of a game engine including footsteps and grenade and 
  -- bullet damage - what decals are applied at points of impact and what sounds are generated. In addition, physical properties 
  -- like friction and sound reverb are included.
  --
  -- Each property of a surface can take any number of variations (thus the use of a vector instead of an array).
  --

  type Surface_State is record
      Mass_Per_Cubic_Unit   : Real_Percent := 0.0;
      Friction_Dynamic      : Real_Percent := 0.0;
      Friction_Static       : Real_Percent := 0.0;
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
  package Hashed_Surface is new Neo.Core.Hashed (Surface_State);

  type Filter_Kind  is (Linear_Filter,  Nearest_Filter);
  type Clamp_Kind   is (No_Clamp,       Zero_Clamp,      Normal_Clamp,          Zero_Alpha_Clamp);
  type Deform_Kind  is (No_Deform,      Sprite_Deform,   Tube_Deform,           Flare_Deform,      Expand_Deform, Move_Deform,
                        Eye_Deform,     Particle_Deform, Small_Particle_Deform, Turbulent_Deform);
  type Sort_Kind    is (Subview_Sort,   GUI_Sort,        Opaque_Sort,           Sky_Sort,          Decal_Sort,    Far_Sort,
                        Medium_Sort,    Close_Sort,      Almost_Near_Sort,      Nearest_Sort,      Post_Sort);

  package Texture_Name_Str is new Util_Str_Super (256); use Texture_Name_Str;
  type Material_State is record
      Name             : Texture_Name_Str.T;
      Filter           : Filter_Kind := Linear_Filter;
      Clamp            : Clamp_Kind  := No_Clamp;
      Sort             : Sort_Kind;
      Friction_Static  : Real;
      Friction_Dynamic : Real;
      Restitution      : Real;
      Is_Cube          : Bool := False;      -- Cube map textures assume _px, _py, _pz, _nx, _ny, _nz name convention
      Base_Color       : Texture_Name_Str.T; -- RGBA
      Prefilter        : Texture_Name_Str.T; -- Single channel
      Irradiance       : Texture_Name_Str.T; -- RGBA
      Specular         : Texture_Name_Str.T; -- RGBA
      Normal           : Texture_Name_Str.T; -- Single channel
      Displacement     : Texture_Name_Str.T; -- Single channel
      Metallic         : Texture_Name_Str.T; -- Single channel
      Roughness        : Texture_Name_Str.T; -- Single channel
      Transform        : Transform_4D := (1.0, 0.0, 0.0, 0.0,
                                          0.0, 1.0, 0.0, 0.0,
                                          0.0, 0.0, 1.0, 0.0); 
    end record;
  package Hashed_Material is new Neo.Core.Hashed (Material_State);
  type Array_Surface_Materials is array (Positive range <>) of Material_State;

  --------------
  -- Skeleton --
  --------------

  -- Skeleton
  package Geometry_Name_Str is new Util_Str_Super (64); use type Geometry_Name_Str.T;
  subtype Bone_Index is Int_16_Unsigned;
  BAD_CHILD : constant Bone_Index := 0;
  type Bone_State is record
      Name      : Geometry_Name_Str.T;
      Parent    : Bone_Index;
      Transform : Transform_4D;
    end record;
  type Array_Bone_Weight           is array (Bone_Index range <>)                      of Real_32;
  type Array_Bone_Index            is array (Bone_Index range <>)                      of Bone_Index;
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
      Animation_Names  : Geometry_Name_Str.Array_T   (1..Animation_Count);
    end record;
  type Ptr_Skeleton_State is access all Skeleton_State;
  package Hashed_Skeleton is new Neo.Core.Hashed (Ptr_Skeleton_State);
  
  ----------
  -- Mesh --
  ----------

  type Vertex_State is record
      Position           : Point_3D;
      Normal             : Point_3D;
      Texture_Coordinate : Point_2D;
    end record with Pack;
  type Array_Vertex_State is array (Positive range <>) of aliased Vertex_State;
  type Mesh_Segment_State is record
      Name     : Geometry_Name_Str.T;
      Material : Texture_Name_Str.T;
      Index    : Positive; -- Index into the Indicies array for each segment
      Count    : Positive;
    end record;
  type Array_Mesh_Segment is array (Positive range <>) of Mesh_Segment_State;
  type Mesh_State (Vertex_Count, Index_Count, Segment_Count : Positive;
                   Bone_Influence_Count, Bone_Weight_Count  : Bone_Index)
    is record
      Name            : Geometry_Name_Str.T;
      Visible         : Bool;
      Shadow          : Bool;
      Motion_Blur     : Bool;
      Bone_Influences : Array_Bone_Index         (1..Bone_Influence_Count);
      Bone_Indicies   : Array_Bone_Index         (1..Bone_Weight_Count);
      Bone_Weights    : Array_Bone_Weight        (1..Bone_Weight_Count);
      Vertices        : Array_Vertex_State       (1..Vertex_Count);
      Indices         : Array_x2_Int_32_Unsigned (1..Index_Count, 1..3);
      Segments        : Array_Mesh_Segment       (1..Segment_Count); 
      Skeleton        : Ptr_Skeleton_State;
    end record;
  type Ptr_Mesh_State is access all Mesh_State;
  type Array_Mesh is array (Positive range <>) of Ptr_Mesh_State;
  package Hashed_Mesh is new Neo.Core.Hashed (Ptr_Mesh_State);
  package Vector_Mesh is new Neo.Core.Hashed (Ptr_Mesh_State);

  ------------
  -- Entity --
  ------------
  
  type Entity_Kind is (Character_Entity,   
                       Camera_Entity,      
                       AI_Entity,          
                       Moveable_Entity,    
                       Item_Entity,        
                       Hurt_Entity,        
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

  type Entity_State;
  type Ptr_Entity_State is access all Entity_State;
  type Entity_State (Kind : Entity_Kind) is record
      
      -- General information
      Name      : Str_Unbound;
      Color     : Color_State;
      Transform : Transform_4D; 
      Origin    : Point_3D;
      
      Mesh      : Ptr_Mesh_State;
      Mesh_Name : Str_Unbound;
      Visible   : Bool   := False;
      Health    : Int_64 := 100;
      
      case Kind is
        when Moveable_Entity | Item_Entity | Light_Entity =>
          Broken_Model : Str_Unbound := NULL_STR_UNBOUND;          
          case Kind is
            when Moveable_Entity | Item_Entity =>
              Velocity                : Vector_3D; 
              Angular_Velocity        : Vector_3D; 
              Density                 : Integer; 
              Friction                : Integer; 
              Bouncyness              : Integer; 
              Mass                    : Integer; 
              Velocity_Delay          : Integer; 
              Angular_Velocity_Delay  : Integer; 
              Start_Spline_Time       : Integer; 
              Collision_Damage        : Integer; 
              Clip_Shrink             : Bool        := False;
              Allow_Step              : Bool        := False;
              Non_Solid               : Bool        := False;
              No_Drop                 : Bool        := False;
              No_Activation_On_Impact : Bool        := False;
              Non_Pushable            : Bool        := False;
              Unbind_On_Death         : Bool        := False;   
              Explode_On_Death        : Bool        := False;
              Collision_Sound         : Str_Unbound := NULL_STR_UNBOUND;
              Bound_Sound             : Str_Unbound := NULL_STR_UNBOUND;
              Clip_Model              : Str_Unbound := NULL_STR_UNBOUND;
              case Kind is
                when Item_Entity =>
                  Inventory_Name      : Str_Unbound := NULL_STR_UNBOUND;
                  Acquire_Sound       : Str_Unbound := NULL_STR_UNBOUND;
                  Respawn_Sound       : Str_Unbound := NULL_STR_UNBOUND;
                  Model               : Str_Unbound := NULL_STR_UNBOUND;
                  Icon                : Str_Unbound := NULL_STR_UNBOUND;
                  Show_Acquisition    : Bool        := False;
                  Is_Carrable         : Bool        := False;
                  Is_Touchable        : Bool        := False;
                  Easy_Mode_Spawn     : Bool        := False;
                  Medium_Mode_Spawn   : Bool        := False;
                  Hard_Mode_Spawn     : Bool        := False;
                  Show_Objective      : Bool        := False;
              when others => null; end case;
            when Light_Entity =>
              Is_Broken      : Bool          := False;
              Hide_On_Break  : Bool          := False;
              Frustum        : Frustum_State := (others => <>); 
              Glare_Material : Str_Unbound   := NULL_STR_UNBOUND;
              Trigger_Count  : Natural       := 0; -- "how many times light must be triggered to toggle."
          when others => null; end case;
          
        when Character_Entity =>
          View : Frustum_State := (others => <>); 
          
        when Speaker_Entity =>
          Clip           : Str_Unbound  := NULL_STR_UNBOUND;
          Is_Mute        : Bool         := False;
          Is_Paused      : Bool         := False;
          Is_Playing     : Bool         := False;
          Is_Occluded    : Bool         := False; 
          Min_Distance   : Real         := 0.0;  
          Max_Distance   : Real         := 0.0;  
          Trigger_Wait   : Duration     := 0.0;  
          Current_Volume : Real_Percent := 75.0; 
          Current_Shake  : Real_Percent := 75.0;
	
        when Door_Entity =>
          Lip              : Natural       := 0;
          Locked           : Bool          := False;
          Speed            : Real          := 10.0;
          Move_Orientation : Quaternion_4D := (others => <>);
          Close_Sound      : Str_Unbound   := NULL_STR_UNBOUND;
          Open_Sound       : Str_Unbound   := NULL_STR_UNBOUND;
          Locked_Sound     : Str_Unbound   := NULL_STR_UNBOUND;
          
        when Hurt_Entity =>
          Kick_Direction  : Vector_3D := ZERO_VECTOR_3D;
          Damage_To_Apply : Natural   := 0;
          Blob_Time       : Natural   := 0;
          Blob_Size       : Natural   := 0;
          Blob_Offset_X   : Natural   := 0;
        when others => null;
      end case;
    end record;
  package Hashed_Entity is new Neo.Core.Hashed (Ptr_Entity_State);
  procedure Construct_Entity (Item : in out Ptr_Entity_State; Kind : Entity_Kind);
  
  -- Map  
  type Map_State;
  type Ptr_Map_State is access all Map_State;
  type Map_State (Geometry_Count : Positive) is record
      Name      : Geometry_Name_Str.T;
      Entities  : Hashed_Entity.Unsafe.Map;
      Transform : Transform_4D := ZERO_TRANSFORM_4D;
      Geometry  : Array_Mesh (1..Geometry_Count);
      Areas     : Array_Mesh (1..Geometry_Count);
    end record;
  package Safe_Map is new Safe (Ptr_Map_State, null);
  package Hashed_Map is new Neo.Core.Hashed (Ptr_Map_State);
  
  --------
  -- IO --
  --------

  -- Load any type of 3D scene data - animations, materials, you name it
  procedure Load_Map       (Path : Str; Result : in out Ptr_Map_State);
  procedure Load_Mesh      (Path : Str; Result : in out Ptr_Mesh_State);
  --procedure Load_Skeleton  (Path : Str; Result : in out Ptr_Mesh_State);
  procedure Load_Materials (Path : Str; Result : in out Hashed_Material.Unsafe.Map);
end;
