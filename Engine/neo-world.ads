
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

with Neo.Engine;           use Neo.Engine;
with Neo.Data;             use Neo.Data;
with Neo.Data.Game;        use Neo.Data.Game;
with Neo.Data.Texture;     use Neo.Data.Texture;
with Neo.Core;             use Neo.Core;
with Neo.Core.Math;        use Neo.Core.Math;
with Neo.Core.Console;     use Neo.Core.Console;
--with Neo.Core.Compression; use Neo.Core.Compression;
with Neo.Core.Strings;     use Neo.Core.Strings;
with Neo.Core.Arrays;      use Neo.Core.Arrays;
with Neo.Core.Maps;        use Neo.Core.Maps;
with Neo.Core.Hashed;
with Neo.Core.Ordered;
with Neo.Core.Vectors;

package Neo.World is
  
  ------------
  -- Levels --
  ------------
  
  type Level_State is record
      Has_Singleplayer    : Bool := False;
      Has_Deathmatch      : Bool := False;
      Has_Team_Deathmatch : Bool := False;
      Has_Last_Man        : Bool := False;
      Has_Tourney         : Bool := False;
    end record;
  package Hashed_Level is new Neo.Core.Hashed (Level_State);
  
  Levels : Hashed_Level.Safe_Map;  
  
  ------------
  -- Entity --
  ------------

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

--    type Entity_State (Kind : Entity_Kind := Entity_Kind'First) is record
--        Model       : Str_Unbound   := NULL_STR_UNBOUND;
--        Visible     : Bool          := False;
--        Physics     : Physics_Kind  := No_Physics;
--        Health      : Int_64        := 0;
--        case Kind is
--          when Moveable_Entity | Item_Entity | Light_Entity =>
--            Broken_Model : Str_Unbound := NULL_STR_UNBOUND;          
--            case Kind is
--              when Moveable_Entity | Item_Entity =>
--                Density                 :  := ;
--                Friction                :  := ;
--                Bouncyness              :  := ;
--                Mass                    :  := ;
--                Clip_Model              : Str_Unbound := ;
--                Clip_Shrink             : Bool := ;
--                Allow_Step              : Bool := ;
--                Non_Solid               : Bool := ;
--                No_Drop                 : Bool := ;
--                No_Activation_On_Impact : Bool := ;
--                Non_Pushable            : Bool := ;
--                Unbind_On_Death         : Bool := ;   
--                Explode_On_Death        : Bool := ;
--                Velocity                : Vector_3D := ;
--                Angular_Velocity        : Vector_3D := ;
--                Velocity_Delay          :  := ;
--                Angular_Velocity_Delay  :  := ;
--                Start_Spline_Time       :  := ;
--                Collision_Damage        :  := ;
--                Collision_Sound         : Str_Unbound := ;
--                Bound_Sound             : Str_Unbound := ;
--                case Kind is
--                  when Item_Entity =>
--                    Inventory_Name      : Str_Unbound := NULL_STR_UNBOUND;
--                    Inventory_ID        : Positive    := 1;
--                    Acquire_Sound       : Str_Unbound := NULL_STR_UNBOUND;
--                    Respawn_Sound       : Str_Unbound := NULL_STR_UNBOUND;
--                    Show_Acquisition    : Bool        := False;
--                    Is_Carrable         : Bool        := False;
--                    Is_Touchable        : Bool        := False;
--                    Easy_Mode_Spawn     : Bool        := False;
--                    Medium_Mode_Spawn   : Bool        := False;
--                    Hard_Mode_Spawn     : Bool        := False;
--                    Model               : Str_Unbound := NULL_STR_UNBOUND;
--                    Pickup_Trigger_Size : Positive    := 1;
--                    Icon                : Str_Unbound := NULL_STR_UNBOUND;
--                    Show_Objective      : Bool        := False;
--                when others => null; end case;
--              when Light_Entity =>
--                Is_Broken      : Bool          := False;
--                Frustum        : Frustum_State := (others => <>); 
--                Glare_Material : Str_Unbound   := NULL_STR_UNBOUND;
--          	"editor_material texture"		"light shader to use."
--          	"editor_color color"			"light color"
--          	"editor_var shaderParm3"	"shaderParm 3"
--          	"editor_var shaderParm4"	"shaderParm 4"
--          	"editor_var shaderParm5"	"shaderParm 5"
--          	"editor_var shaderParm6"	"shaderParm 6"
--          	"editor_var shaderParm7"	"shaderParm 7"
--          	"editor_var count"			"how many times light must be triggered to toggle."
--          	"editor_var break"			"break when triggered."
--          	"editor_model model"			"model to use."
--          	"editor_model broken"			"model to use when the light is broken (defaults to model name with '_broken' appended to name)"
--          	"editor_var hideModelOnBreak" "hides the model when broken"
--          	"editor_var health"			"amount of damage to recieve before becoming broken.  0 is nonbreakable."
--          	"editor_var target"			"entities to trigger if shot."
--          	"editor_var levels"			"the number of times a light must be triggered until it turns off.  Each time it's triggered, it's dimmed to a lower level."
--          	"editor_var start_off"		"causes the light to be off when the level starts."
--          	"editor_var snd_broken"		"sound shader to use when broken"
--          	"editor_var mtr_broken"		"material to use when broken"
--          	"editor_color color_demonic"	"demonic color to use if in demonic mode."
--          	"editor_material mat_demonic"	"demonic shader to use if in demonic mode."
--          	"editor_var leave_demonic_mat" "leaves the demonic shader when the effect expires.. color is still reverted back."
--            when others => null; end case;
--          when Character_Entity =>
--            World : Positive      := 1;
--            Area  : Positive      := 1;
--            --Pose  : Pose_State    := (others => <>);
--            View  : Frustum_State := (others => <>); 
--            
--          when Speaker_Entity =>
--            Clip           : Str_Unbound    := NULL_STR_UNBOUND;
--            --Position       : Position_State := (others => <>);
--            Is_Mute        : Bool           := False;
--            Is_Paused      : Bool           := False;
--            Is_Playing     : Bool           := False;
--            Min_Distance   : Real           := 0.0;
--            Max_Distance   : Real           := 0.0;
--            Current_Volume : Real_Percent   := 75.0;
--            Current_Shake  : Real_Percent   := 75.0;
--  	"editor_snd s_shader"			"the sound shader or wavefile."
--  	"editor_snd snd_demonic"		"demonic sound shader."
--  	"editor_var overlayDemonic"		"set to 1 overlays the demonic sound instead of replacing it."
--  	"editor_var s_waitfortrigger"	"play on load or play when triggered."
--  	"editor_var s_volume"			"override shader volume level."
--  	"editor_var s_minDistance"		"override shader minDistance."
--  	"editor_var s_maxDistance"		"override shader maxDistance."
--  	"editor_var s_omni"				"force omnidirectional"
--  	"editor_var s_looping"			"force looping"
--  	"editor_var s_occlusion"		"force no portal occlusion"
--  	"editor_var s_global"			"force no falloff at all"
--  	"editor_var s_shakes"			"this entity can make the screen shake when the sound is loud."
--  	"editor_var wait"				"when set > 0, seconds between triggerings.  time between sounds is wait + or - random"
--  	"editor_var random"				"wait variance."
--  	
--          when Door_Entity =>
--            Lip              : Natural       := 0;
--            Locked           : Bool          := False;
--            Speed            : Real          := 10.0;
--            Move_Orientation : Quaternion_4D := (others => <>);
--            Close_Sound      : Str_Unbound   := NULL_STR_UNBOUND;
--            Open_Sound       : Str_Unbound   := NULL_STR_UNBOUND;
--            Locked_Sound     : Str_Unbound   := NULL_STR_UNBOUND;
--          --when Hurt_Entity =>
--          --  Damage : Damage_State := (others => <>);
--          when others => null;
--        end case;
--      end record;
--    package Hashed_Entity is new Neo.Core.Hashed (Entity_State);
  
  -----------
  -- Items --
  -----------
  
  --type Item_State is record
  --  end record;
  
  ---------------
  -- Character --
  ---------------
  
  
    
  ---------------
  -- Particles --
  ---------------
    
  -----------
  -- Sound --
  -----------

  type Sound_Kind is (Foreground_Clip, Background_Clip, Music_Clip, Cinematic_Clip, Dialog_Clip);

  type Sound_State (Kind : Sound_Kind := Foreground_Clip; Do_Loop : Bool := False) is record
      Lead_In_Sample   : Str_Unbound  := NULL_STR_UNBOUND;
      Lead_In_Volume   : Real_Percent := 100.0;
      Max_Volume       : Real_Percent := 100.0;
      Screen_Shake     : Real_Percent := 10.0;
      Fade_Radius_Min  : Real         := 1.0;
      Fade_Radius_Max  : Real         := 2.0;
      Omnidirectional  : Bool         := False;
      No_Occlusion     : Bool         := False;
      Is_Global        : Bool         := True;
      Is_Private       : Bool         := True;
      Use_Center       : Bool         := True;
      Use_Left         : Bool         := True;
      Use_Right        : Bool         := True;
      Use_Back_Left    : Bool         := True;
      Use_Back_Right   : Bool         := True;
      Use_Subwoofer    : Bool         := True;
      case Do_Loop is
        when False =>
          Sample : Str_Unbound := NULL_STR_UNBOUND;
        when True =>
          Samples       : Vector_Str_Unbound.Unsafe.Vector;
          Same_Twice_OK : Bool := False; -- Must have more than 2 samples to be relevant
      end case;
    end record;
  
  -----------------
  -- 
  ---------------
  

  -----------------
  -- Definitions --
  -----------------
  
  PATH_CHARACTER_DEFS : constant Str := PATH_DEFINITIONS & S & "Characters" & S;
  PATH_ITEM_DEFS      : constant Str := PATH_DEFINITIONS & S & "Items"      & S;
  PATH_MAP_DEFS       : constant Str := PATH_DEFINITIONS & S & "Maps"       & S;
  PATH_MATERIAL_DEFS  : constant Str := PATH_DEFINITIONS & S & "Materials"  & S;
  PATH_PARTICLE_DEFS  : constant Str := PATH_DEFINITIONS & S & "Particles"  & S;
  PATH_SOUND_DEFS     : constant Str := PATH_DEFINITIONS & S & "Sounds"     & S;
  PATH_WEAPON_DEFS    : constant Str := PATH_DEFINITIONS & S & "Weapons"    & S;
  PATH_OBJECT_DEFS    : constant Str := PATH_DEFINITIONS & S & "Objects"    & S;
  
  
end;
