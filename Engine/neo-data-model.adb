
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

-- Isolate parsers for different model formats into separate packages and consolidate them here
package body Neo.Data.Model is

  -----------------
  -- Conversions --
  -----------------

  function To_Triangles (Polygon : Vector_Int_32_Natural.Unsafe.Vector) return Vector_Triangle.Unsafe.Vector is
    Result : Vector_Triangle.Unsafe.Vector;
    begin
      return Result; -- 0 (i) (i + 1)  [for i in 1..(n - 2)] 
    --0 1 2
    --0 2 3
    --  for I in 0..Patch_Height loop
    --    for J in 1..Patch_Width loop
    --      Triangle := (if J mod 2 = 0 then (I * Patch_Width + J - 1, I * Patch_Width + J, (I + 1) * Patch_Width + J - 1)
    --                   else (I * Patch_Width + J - 1, (I + 1) * Patch_Width + J - 2, (I + 1) * Patch_Width + J - 1));
    --    end loop;
    --  end loop;
    end;

  --function To_Mesh (Plane : Plane_3D) return Mesh_State is
  --  begin
  --  end;

  --------------
  -- Bounding --
  --------------

  procedure Adjust_Bounding (Point : Point_3D; Bounding : in out Bounding_State) is
    begin
      if Point.X > Bounding.A.X then Bounding.A.X := Point.X; end if;
      if Point.X < Bounding.B.X then Bounding.B.X := Point.X; end if;
      if Point.Y > Bounding.A.Y then Bounding.A.Y := Point.Y; end if;
      if Point.Y < Bounding.B.Y then Bounding.B.Y := Point.Y; end if;
      if Point.Z > Bounding.A.Z then Bounding.A.Z := Point.Z; end if;
      if Point.Z < Bounding.B.Z then Bounding.B.Z := Point.Z; end if;
    end;

  procedure Build_Bounding (Mesh : in out Mesh_State) is
    begin
      for Vertex of Mesh.Vertices loop Adjust_Bounding (Vertex.Point, Mesh.Bounding); end loop;
    end;

  -------------
  -- Loading --
  -------------

  -- Separate packages (one for each Model.Format_Kind)
  package Wavefront is
      function Load (Path : Str) return Vector_Mesh.Unsafe.Vector;
    end;
  package body Wavefront is separate;
  package Id_Tech is
      function Load (Path : Str) return Level_State;
      function Load (Path : Str) return Camera_State;
      function Load (Path : Str) return Animation_State;
      function Load (Path : Str) return Skeletal_Mesh_State;
      function Load (Path : Str) return Hashed_Material.Unsafe.Map;
    end;
  package body Id_Tech is separate;

  -- Create the loaders
  package Mesh          is new Handler (Format_Kind, Vector_Mesh.Unsafe.Vector);
  package Level         is new Handler (Format_Kind, Level_State);
  package Camera        is new Handler (Format_Kind, Camera_State);
  package Animation     is new Handler (Format_Kind, Animation_State);
  package Skeletal_Mesh is new Handler (Format_Kind, Skeletal_Mesh_State);
  package Material      is new Handler (Format_Kind, Hashed_Material.Unsafe.Map);

  -- Register the formats in the loaders
  package Wavefront_Mesh        is new Mesh.Format          (Wavefront_Format, Wavefront.Load, "obj");
  package Id_Tech_Level         is new Level.Format         (Id_Tech_Format,   Id_Tech.Load,   "proc,cm,map,aas48");
  package Id_Tech_Camera        is new Camera.Format        (Id_Tech_Format,   Id_Tech.Load,   "md5camera");
  package Id_Tech_Material      is new Material.Format      (Id_Tech_Format,   Id_Tech.Load,   "mtr");
  package Id_Tech_Animation     is new Animation.Format     (Id_Tech_Format,   Id_Tech.Load,   "md5anim");
  package Id_Tech_Skeletal_Mesh is new Skeletal_Mesh.Format (Id_Tech_Format,   Id_Tech.Load,   "md5mesh");

  -- Redirect internal handlers to public load functions
  function Load (Path : Str) return Vector_Mesh.Unsafe.Vector  renames Mesh.Load;
  function Load (Path : Str) return Level_State                renames Level.Load;
  function Load (Path : Str) return Camera_State               renames Camera.Load;
  function Load (Path : Str) return Animation_State            renames Animation.Load;
  function Load (Path : Str) return Skeletal_Mesh_State        renames Skeletal_Mesh.Load;
  function Load (Path : Str) return Hashed_Material.Unsafe.Map renames Material.Load;

  --------------
  -- Surfaces --
  --------------
  
  -- function Get_Surface (Kind : Surface_Kind) return Surface_State is (return (case Kind is 
end;
