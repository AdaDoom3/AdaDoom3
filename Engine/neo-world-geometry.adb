
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

package body Neo.Engine.Geometry is
  function Create_Cube           (Length, Width, Height : Real_64)                  return Mesh_State;
  function Create_Cylinder       (Radius, Height : Real_64)                         return Mesh_State;
  function Create_Prism          (Triangle_Base, Triangle_Height, Length : Real_64) return Mesh_State;
  function Create_Tetrahedron    (Triangle_Base, Triangle_Height : Real_64)         return Mesh_State;
  function Create_Pyramid        (Height : Real_64)                                 return Mesh_State;
  function Create_Cone           (
  function Create_Round_Frustum  (
  function Create_Square_Frustum (
  procedure Deform_Sprite   (Mesh : in out Mesh_State);
  procedure Deform_Tube     (Mesh : in out Mesh_State);
  procedure Deform_Flare    (Mesh : in out Mesh_State);
  procedure Deform_Expand   (Mesh : in out Mesh_State);
  procedure Deform_Move     (Mesh : in out Mesh_State);
  procedure Deform_Shake    (Mesh : in out Mesh_State);
  procedure Deform_Eye      (Mesh : in out Mesh_State);
  procedure Deform_Particle (Mesh : in out Mesh_State);
end;