
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

with Neo.Engine;       use Neo.Engine;
with Neo.Data;         use Neo.Data;
with Neo.Data.Game;    use Neo.Data.Game;
with Neo.Data.Texture; use Neo.Data.Texture;
with Neo.Core;         use Neo.Core;
with Neo.Core.Math;    use Neo.Core.Math; 
with Neo.Core.Console; use Neo.Core.Console;
with Neo.Core.Strings; use Neo.Core.Strings;
with Neo.Core.Arrays;  use Neo.Core.Arrays;
with Neo.Core.Maps;    use Neo.Core.Maps;
with Neo.Core.Hashed;
with Neo.Core.Ordered;
with Neo.Core.Vectors;

with Ada.Numerics; use Ada.Numerics;
with Ada.Calendar; use Ada.Calendar;

package Neo.World is

  ------------
  -- Camera --
  ------------
  
  CAMERA_ZOOM        : constant Real := 45.0;
  CAMERA_FOV         : constant Real := 45.0;
  CAMERA_YAW         : constant Real := 0.0;
  CAMERA_PITCH       : constant Real := 0.0;
  CAMERA_SPEED       : constant Real := 0.8;
  CAMERA_SENSITIVITY : constant Real := 0.03;
  
  type Direction_Kind is (Forward_Direction, Backward_Direction, Left_Direction, Right_Direction);
  
  protected type Safe_Camera is 
      function View return Matrix_4D;
      procedure Update;
      procedure Set_Position (Val : Vector_3D);
      procedure Set_Yaw      (Val : Real);
      procedure Set_Pitch    (Val : Real);
      procedure Move         (Dir : Direction_Kind; Start_Time : Time);
      procedure Look         (X_Offset, Y_Offset : Int; Clamp : Bool := True);
    private
      Sensitivity  : Real      := CAMERA_SENSITIVITY;
      Zoom         : Real      := CAMERA_ZOOM;
      Move_Speed   : Real      := CAMERA_SPEED;
      Yaw          : Real      := CAMERA_YAW;
      Pitch        : Real      := CAMERA_PITCH;
      Right        : Vector_3D := ZERO_VECTOR_3D;
      Up           : Vector_3D := ZERO_VECTOR_3D;
      Position     : Vector_3D := ZERO_VECTOR_3D;
      Direction    : Vector_3D := ZERO_VECTOR_3D;
      World_Up     : Vector_3D := (0.0, 0.0, -1.0);
    end;
    
  ----------
  -- Data --
  ----------

  In_Game   : Safe_Status;
  Map       : Safe_Map.T;
  Camera    : Safe_Camera;
  Entities  : Hashed_Entity.Safe_Map;
  Maps      : Hashed_Map.Safe_Map;
  Meshes    : Hashed_Mesh.Safe_Map;
  Materials : Hashed_Material.Safe_Map;
  Skeletons : Hashed_Skeleton.Safe_Map;
  
  -----------
  -- Scene --
  -----------
  
  type Level_State is record
      Has_Singleplayer    : Bool := False;
      Has_Deathmatch      : Bool := False;
      Has_Team_Deathmatch : Bool := False;
      Has_Last_Man        : Bool := False;
      Has_Tourney         : Bool := False;
    end record;
  
  type Memory_Kind is (GPU_Memory, CPU_Memory, GPU_And_CPU_Memory);
  
  
  --procedure Load_Map           (Path : Str; Memory : Memory_Kind);
  --procedure Load_Mesh          (Path : Str; Memory : Memory_Kind);
  --procedure Free_Map           (Path : Str; Memory : Memory_Kind);
  --procedure Free_Mesh          (Path : Str; Memory : Memory_Kind);
  --procedure Free_Unused_Meshes (Path : Str; Memory : Memory_Kind);
  
  
  procedure Initialize_Materials;
  
  
  procedure Initialize_Map (Path : Str);
  procedure Finalize_Map;
end;












































