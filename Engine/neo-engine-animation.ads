
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

package Neo.Engine.Animation is

  ---------------
  -- Animation --
  ---------------

  INFINITE_CYCLE : constant Natural := 0;

  type Blend_State is record
      Influence : Real_Percent := 100.0;
      Cycle     : Natural      := INFINITE_CYCLE;
      Frame     : Positive     := 1;
      Animation : Str_Unbound  := NULL_STR_UNBOUND;
      Start     : Duration     := No_Duration;
    end record;
  package Hashed_Blend is new Core.Hashed (Blend_State);

  -- Bone modifiers for generated animations like player-look rotation
  type Bone_State is record
      Joint     : Str_Unbound  := NULL_STR_UNBOUND;
      Transform : Transform_4D := (others => 0.0);
    end record;
  package Hashed_Bone is new Core.Hashed (Bone_State);

  type Pose_State is record
      Blends : Hashed_Blend.Unsafe.Map;
      Bones  : Hashed_Bone.Unsafe.Map;
    end record;

  function Render (Mesh : Skeletal_Mesh_State; Pose : Pose_State) return Treed_Joint.Unsafe.Tree;
end;