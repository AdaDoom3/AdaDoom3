
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

package Neo.World.Geometry is

  ----------------
  -- Primitives --
  ----------------

  --procedure Make_Sphere   (float radius, const PR& pose);
  procedure Make_Box      (const Point& extents, const PR& pose);
  procedure Make_Capsule  (float r, float h, const PR& pose);
  procedure Make_Cylinder (float r, float h, const PR& pose);
  procedure Make_Triangle (A, B, C : Point_3D; Color : Color_State);
  procedure Make_Line     (A, B, C : Point_3D; Color : Color_State);
  procedure Make_Circle (Segments      : Long_Long_Int;
                         Matrix        : Matrix_4D;
                         Color         : Color_State;
                         Radius        : Float;
                         Is_Semicircle : Bool := False);
  procedure Make_Polygon  (Points : Array_Point_3D;
                           Normal : Point_3D;
                           Color  : Color_State);
  procedure Create_Frustum        (
--  DrawWirefameAABB				(const AABB& box, const Point& color);
--  DrawWirefameOBB					(const OBB& box, const Point& color);
--  DrawRaycastData					(udword nb, const PintRaycastData* raycast_data, const PintRaycastHit* hits, const Point& color);
--  DrawRaycastAnyData				(udword nb, const PintRaycastData* raycast_data, const PintBooleanHit* hits, const Point& color);
--  DrawBoxSweepData				(udword nb, const PintBoxSweepData* box_sweep_data, const PintRaycastHit* hits, const Point& color);
--  DrawSphereSweepData				(udword nb, const PintSphereSweepData* sphere_sweep_data, const PintRaycastHit* hits, const Point& color);
--  DrawCapsuleSweepData			(udword nb, const PintCapsuleSweepData* capsule_sweep_data, const PintRaycastHit* hits, const Point& color);
--  DrawConvexSweepData				(udword nb, const PintConvexSweepData* convex_sweep_data, const PintRaycastHit* hits, const Point& color);
--  DrawSphereOverlapAnyData		(udword nb, const PintSphereOverlapData* sphere_overlap_data, const PintBooleanHit* hits, const Point& color);
--  DrawSphereOverlapObjectsData	(udword nb, const PintSphereOverlapData* sphere_overlap_data, const PintOverlapObjectHit* hits, const Point& color);
--  DrawBoxOverlapAnyData			(udword nb, const PintBoxOverlapData* box_overlap_data, const PintBooleanHit* hits, const Point& color);
--  DrawBoxOverlapObjectsData		(udword nb, const PintBoxOverlapData* box_overlap_data, const PintOverlapObjectHit* hits, const Point& color);
--  DrawCapsuleOverlapAnyData		(udword nb, const PintCapsuleOverlapData* capsule_overlap_data, const PintBooleanHit* hits, const Point& color);
--  DrawCapsuleOverlapObjectsData	(udword nb, const PintCapsuleOverlapData* capsule_overlap_data, const PintOverlapObjectHit* hits, const Point& color);

  ------------------
  -- Deformations --
  ------------------

--  procedure Deform_Sprite   (Mesh : in out Mesh_State);
--  procedure Deform_Tube     (Mesh : in out Mesh_State);
--  procedure Deform_Flare    (Mesh : in out Mesh_State);
--  procedure Deform_Expand   (Mesh : in out Mesh_State);
--  procedure Deform_Move     (Mesh : in out Mesh_State);
--  procedure Deform_Shake    (Mesh : in out Mesh_State);
--  procedure Deform_Eye      (Mesh : in out Mesh_State);
--  procedure Deform_Particle (Mesh : in out Mesh_State);
end;
