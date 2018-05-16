
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

package Neo.World is
  
  ------------
  -- Assets --
  ------------
  
  -- Exceptions related when referencing unbuffered assets
  Unknown_Audio_Clip        : Exception;
  Unknown_Sound_Emitter     : Exception;
  Unknown_Level             : Exception;
  Unknown_Mesh              : Exception;
  Unknown_Image_In_Material : Exception;
  Unknown_Material_In_Mesh  : Exception;
  
  -- Vector of material "definitions" containing all shading/render descriptions. It does not represent loaded textures!
  Materials : Hashed_Material.Safe_Map;
  Meshs     : Hashed_Mesh.Safe_Map;

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
end;
