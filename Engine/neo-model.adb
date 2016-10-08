
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

-- Isolate parsers for different formats into separate packages and consolidate them here
package body Neo.File.Model is

  -- Separate packages (one for each Model.Format_Kind
  package Id_Tech is
      function Load (Path : Str) return Mesh;
      function Load (Path : Str) return Level;
      function Load (Path : Str) return Camera;
      function Load (Path : Str) return Animation;
    end;
  package body Id_Tech is separate;

  -- Create the loaders
  package Mesh      is new Handler (Format_Kind, Mesh_State);
  package Level     is new Handler (Format_Kind, Level_State);
  package Camera    is new Handler (Format_Kind, Camera_State);
  package Animation is new Handler (Format_Kind, Animation_State);

  -- Register the formats in the loaders
  package Id_Tech_Mesh      is new Mesh.Format      (Id_Technology_Format, Id_Tech.Load'Access, "md5mesh,bmd5mesh");
  package Id_Tech_Level     is new Level.Format     (Id_Technology_Format, Id_Tech.Load'Access, "proc,cm,map");
  package Id_Tech_Camera    is new Camera.Format    (Id_Technology_Format, Id_Tech.Load'Access, "md5camera");
  package Id_Tech_Animation is new Animation.Format (Id_Technology_Format, Id_Tech.Load'Access, "md5anim");

  -- Redirect internal handlers to public load functions
  function Load (Path : Str) return Mesh_State      renames Mesh.Load;
  function Load (Path : Str) return Level_State     renames Level.Load;
  function Load (Path : Str) return Camera_State    renames Camera.Load;
  function Load (Path : Str) return Animation_State renames Animation.Load;
end;
