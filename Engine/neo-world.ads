
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

with Neo.Engine.Physics; 

generic
  Path : Str;
package Neo.World is

  -----------
  -- World --
  -----------
  
  -- Global data caches
  Meshes      : Hashed_Mesh.Safe.Map;
  Levels      : Hashed_Level.Safe_Map;
  Images      : Hashed_Image.Safe.Map;
  Shaders     : Hashed_Stream.Safe.Map;
  Materials   : Hashed_Materials.Safe.Map;
  UI_Elements : Treed_UI_Element.Safe.Tree;

  type World_State is record
      Start_Time : Time;
      Elapsed    : Duration;
      Level      : Level_State := (others => <>);
    end record;
  package Hashed_World is new Neo.Core.Hashed (World_State);
  
  procedure Run_Frame;
end;
