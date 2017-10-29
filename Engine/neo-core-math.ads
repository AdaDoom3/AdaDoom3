
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

with Ada.Numerics.Generic_Elementary_Functions;
with Neo.Core.Vectors;

-- Based on Foundations of Game Engine Development Vol. 1: http://foundationsofgameenginedev.com/FGED1-code.cpp
package Neo.Core.Math is

  type Dimension_Kind is (X_Dimension, Y_Dimension, Z_Dimension);
  package Elementary_Functions is new Ada.Numerics.Generic_Elementary_Functions (Real_64); use Elementary_Functions;

  -----------
  -- Point --
  -----------

  -- Definitions
  type Point_2D is record X, Y       : Real_64 := 0.0; end record;
  type Point_3D is record X, Y, Z    : Real_64 := 0.0; end record; -- Listing 2.8
  type Point_4D is record X, Y, Z, W : Real_64 := 0.0; end record;
  package Vector_Point_2D is new Vectors (Point_2D);
  package Vector_Point_3D is new Vectors (Point_3D);

  -- 3D Operations
  function "+" (A, B : Point_3D) return Point_3D is ((A.X + B.X, A.Y + B.Y, A.Z + B.Z)); 
  function "-" (A, B : Point_3D) return Point_3D is ((A.X - B.X, A.Y - B.Y, A.Z - B.Z)); 

  ------------
  -- Vector --
  ------------

  -- Definitions
  type Vector_2D is record X, Y       : Real_64 := 0.0; end record; -- Listing 1.1
  type Vector_3D is record X, Y, Z    : Real_64 := 0.0; end record; -- Listing 1.1
  type Vector_4D is record X, Y, Z, W : Real_64 := 0.0; end record;

  -- Conversions
  function To_Vector_3D (P : Point_3D)               return Vector_3D is ((P.X, P.Y, P.Z));
  function To_Vector_3D (V : Vector_4D)              return Vector_3D is ((V.X, V.Y, V.Z));
  function To_Vector_4D (V : Vector_3D; W : Real_64) return Vector_4D is ((V.X, V.Y, V.Z, W));

  -- 3D Operations
  function "*"       (V    : Vector_3D; S : Real_64) return Vector_3D is ((V.X * S, V.Y * S, V.Z * S));           -- Listing 1.2
  function "/"       (V    : Vector_3D; S : Real_64) return Vector_3D is (V * (1.0 / S));                         -- Listing 1.2
  function "-"       (V    : Vector_3D)              return Vector_3D is ((-V.X, -V.Y, -V.Z));                    -- Listing 1.2
  function "+"       (A, B : Vector_3D)              return Vector_3D is ((A.X + B.X, A.Y + B.Y, A.Z + B.Z));     -- Listing 1.3
  function "-"       (A, B : Vector_3D)              return Vector_3D is ((A.X - B.X, A.Y - B.Y, A.Z - B.Z));     -- Listing 1.3
  function Magnitude (V    : Vector_3D)              return Real_64   is (Sqrt (V.X ** 2 + V.Y ** 2 + V.Z ** 2)); -- Listing 1.2
  function Normal    (V    : Vector_3D)              return Vector_3D is (V / Magnitude (V));                     -- Listing 1.2
  function Dot       (A, B : Vector_3D)              return Real_64   is (A.X * B.X + A.Y * B.Y + A.Z * B.Z);     -- Listing 1.6
  function Project   (A, B : Vector_3D)              return Vector_3D is (B * (Dot (A, B) / Dot (B, B)));         -- Listing 1.8
  function Reject    (A, B : Vector_3D)              return Vector_3D is (A - Project (A, B));                    -- Listing 1.8
  function Cross     (A, B : Vector_3D)              return Vector_3D is ((A.Y * B.Z - A.Z * B.Y,                 -- Listing 1.7
                                                                           A.Z * B.X - A.X * B.Z,
                                                                           A.X * B.Y - A.Y * B.X));
 
  -- 4D Operations
  function "*"       (V    : Vector_4D; S : Real_64) return Vector_4D is ((V.X * S, V.Y * S, V.Z * S, V.W * S));
  function "/"       (V    : Vector_4D; S : Real_64) return Vector_4D is (V * (1.0 / S));
  function "-"       (V    : Vector_4D)              return Vector_4D is ((-V.X, -V.Y, -V.Z, -V.W));
  function "+"       (A, B : Vector_4D)              return Vector_4D is ((A.X + B.X, A.Y + B.Y, A.Z + B.Z, A.W + B.W));
  function "-"       (A, B : Vector_4D)              return Vector_4D is ((A.X - B.X, A.Y - B.Y, A.Z - B.Z, A.W - B.W));
  function Magnitude (V    : Vector_4D)              return Real_64   is (Sqrt (V.X ** 2 + V.Y ** 2 + V.Z ** 2 + V.W ** 2));
  function Normal    (V    : Vector_4D)              return Vector_4D is (V / Magnitude (V));
  function Dot       (A, B : Vector_4D)              return Real_64   is (A.X * B.X + A.Y * B.Y + A.Z * B.Z + A.W * B.W);
  function Project   (A, B : Vector_4D)              return Vector_4D is (B * (Dot (A, B) / Dot (B, B)));
  function Reject    (A, B : Vector_4D)              return Vector_4D is (A - Project (A, B));

  ------------
  -- Matrix --
  ------------

  -- Definitions
  type Matrix_2D is record
      XX, YX,
      XY, YY : Real_64 := 0.0;
    end record;
  type Matrix_3D is record -- Listing 1.3
      XX, YX, ZX,
      XY, YY, ZY,
      XZ, YZ, ZZ : Real_64 := 0.0;
    end record;
  type Matrix_4D is record
      XX, YX, ZX, WX,
      XY, YY, ZY, WY,
      XZ, YZ, ZZ, WZ,
      XW, YW, ZW, WW : Real_64 := 0.0;
    end record;

  -- 3D Conversions
  procedure Set_Matrix_3D_X (M : in out Matrix_3D; V : Vector_3D);
  procedure Set_Matrix_3D_Y (M : in out Matrix_3D; V : Vector_3D);
  procedure Set_Matrix_3D_Z (M : in out Matrix_3D; V : Vector_3D);
  function Get_Matrix_3D_X  (M : Matrix_3D)       return Vector_3D is ((M.XX, M.XY, M.XZ));
  function Get_Matrix_3D_Y  (M : Matrix_3D)       return Vector_3D is ((M.YX, M.YY, M.YZ));
  function Get_Matrix_3D_Z  (M : Matrix_3D)       return Vector_3D is ((M.ZX, M.ZY, M.ZZ));
  function To_Matrix_3D     (A, B, C : Vector_3D) return Matrix_3D is ((A.X, B.X, C.X,
                                                                        A.Y, B.Y, C.Y,
                                                                        A.Z, B.Z, C.Z));

  -- 3D Operations
  function Inverse     (M : Matrix_3D) return Matrix_3D; -- Listing 1.10
  function Determinant (M : Matrix_3D) return Real_64 is (M.XX * (M.YY * M.ZZ - M.ZY * M.YZ) + -- Listing 1.9
                                                          M.YX * (M.ZY * M.XZ - M.XY * M.ZZ) +
                                                          M.ZX * (M.XY * M.YZ - M.YY * M.XZ));
  function "*" (M : Matrix_3D; V : Vector_3D) return Vector_3D is ((M.XX * V.X + M.YX * V.Y + M.ZX * V.Z, -- Listing 1.5
                                                                    M.XY * V.X + M.YY * V.Y + M.ZY * V.Z,
                                                                    M.XZ * V.X + M.YZ * V.Y + M.ZZ * V.Z));
  function "*" (A, B : Matrix_3D) return Matrix_3D is ((A.XX * B.XX + A.YX * B.XY + A.ZX * B.XZ, -- Listing 1.5
                                                        A.XX * B.YX + A.YX * B.YY + A.ZX * B.YZ,
                                                        A.XX * B.ZX + A.YX * B.ZY + A.ZX * B.ZZ,
                                                        A.XY * B.XX + A.YY * B.XY + A.ZY * B.XZ,
                                                        A.XY * B.YX + A.YY * B.YY + A.ZY * B.YZ,
                                                        A.XY * B.ZX + A.YY * B.ZY + A.ZY * B.ZZ,
                                                        A.XZ * B.XX + A.YZ * B.XY + A.ZZ * B.XZ,
                                                        A.XZ * B.YX + A.YZ * B.YY + A.ZZ * B.YZ,
                                                        A.XZ * B.ZX + A.YZ * B.ZY + A.ZZ * B.ZZ));

  -- 4D Conversions
  procedure Set_Matrix_4D_X (M : in out Matrix_4D; V : Vector_4D);
  procedure Set_Matrix_4D_Y (M : in out Matrix_4D; V : Vector_4D);
  procedure Set_Matrix_4D_Z (M : in out Matrix_4D; V : Vector_4D);
  procedure Set_Matrix_4D_W (M : in out Matrix_4D; V : Vector_4D);
  function Get_Matrix_4D_X  (M : Matrix_4D)          return Vector_4D is ((M.XX, M.XY, M.XZ, M.XW));
  function Get_Matrix_4D_Y  (M : Matrix_4D)          return Vector_4D is ((M.YX, M.YY, M.YZ, M.YW));
  function Get_Matrix_4D_Z  (M : Matrix_4D)          return Vector_4D is ((M.ZX, M.ZY, M.ZZ, M.ZW));
  function Get_Matrix_4D_W  (M : Matrix_4D)          return Vector_4D is ((M.WX, M.WY, M.WZ, M.WW));
  function Get_Matrix_4D_X  (M : Matrix_4D)          return Vector_3D is ((M.XX, M.XY, M.XZ));
  function Get_Matrix_4D_Y  (M : Matrix_4D)          return Vector_3D is ((M.YX, M.YY, M.YZ));
  function Get_Matrix_4D_Z  (M : Matrix_4D)          return Vector_3D is ((M.ZX, M.ZY, M.ZZ));
  function Get_Matrix_4D_W  (M : Matrix_4D)          return Vector_3D is ((M.WX, M.WY, M.WZ));
  function To_Matrix_4D     (A, B, C, D : Vector_4D) return Matrix_4D is ((A.X, B.X, C.X, D.X,
                                                                           A.Y, B.Y, C.Y, D.Y,
                                                                           A.Z, B.Z, C.Z, D.Z,
                                                                           A.W, B.W, C.W, D.W));

  -- 4D Operations
  function Inverse (M : Matrix_4D) return Matrix_4D; -- Listing 1.11
  function "*" (A : Matrix_4D; B : Vector_4D) return Vector_4D is ((A.XX * B.X + A.YX * B.Y + A.ZX * B.Z + A.WX * B.W,
                                                                    A.XY * B.X + A.YY * B.Y + A.ZY * B.Z + A.WY * B.W,
                                                                    A.XZ * B.X + A.YZ * B.Y + A.ZZ * B.Z + A.WZ * B.W,
                                                                    A.XW * B.X + A.YW * B.Y + A.ZW * B.Z + A.WW * B.W));
  function "*" (A, B : Matrix_4D) return Matrix_4D is ((A.XX * B.XX + A.YX * B.XY + A.ZX * B.XZ,
                                                        A.XX * B.YX + A.YX * B.YY + A.ZX * B.YZ,
                                                        A.XX * B.ZX + A.YX * B.ZY + A.ZX * B.ZZ,
                                                        A.XX * B.WX + A.YX * B.WY + A.ZX * B.WZ,
                                                        A.XY * B.XX + A.YY * B.XY + A.ZY * B.XZ,
                                                        A.XY * B.YX + A.YY * B.YY + A.ZY * B.YZ,
                                                        A.XY * B.ZX + A.YY * B.ZY + A.ZY * B.ZZ,
                                                        A.XY * B.WX + A.YY * B.WY + A.ZY * B.WZ,
                                                        A.XZ * B.XX + A.YZ * B.XY + A.ZZ * B.XZ,
                                                        A.XZ * B.YX + A.YZ * B.YY + A.ZZ * B.YZ,
                                                        A.XZ * B.ZX + A.YZ * B.ZY + A.ZZ * B.ZZ,
                                                        A.XZ * B.WX + A.YZ * B.WY + A.ZZ * B.WZ,
                                                        A.XW * B.XX + A.YZ * B.XY + A.ZW * B.XZ,
                                                        A.XW * B.YX + A.YZ * B.YY + A.ZW * B.YZ,
                                                        A.XW * B.ZX + A.YZ * B.ZY + A.ZW * B.ZZ,
                                                        A.XW * B.WX + A.YZ * B.WY + A.ZW * B.WZ));

  ---------------
  -- Transform --
  ---------------

  -- Definition
  type Transform_3D is record
      XX, YX, ZX,
      XY, YY, ZY : Real_64 := 0.0;
      -- 0.0, 0.0, 1.0
    end record;
  type Transform_4D is record -- Listing 2.9
      XX, YX, ZX, WX,
      XY, YY, ZY, WY,
      XZ, YZ, ZZ, WZ : Real_64 := 0.0;
      -- 0.0, 0.0, 0.0, 1.0
    end record;

  -- Conversions
  procedure Set_Transform_4D_X (H : in out Transform_4D; V : Vector_3D);                    -- Listing 2.9
  procedure Set_Transform_4D_Y (H : in out Transform_4D; V : Vector_3D);                    -- Listing 2.9
  procedure Set_Transform_4D_Z (H : in out Transform_4D; V : Vector_3D);                    -- Listing 2.9
  procedure Set_Transform_4D_W (H : in out Transform_4D; V : Vector_3D);                    -- Listing 2.9
  procedure Set_Translate      (H : in out Transform_4D; P : Point_3D);                     -- Listing 2.9
  function Get_Transform_4D_X  (H : Transform_4D) return Vector_3D is ((H.XX, H.XY, H.XZ)); -- Listing 2.9
  function Get_Transform_4D_Y  (H : Transform_4D) return Vector_3D is ((H.YX, H.YY, H.YZ)); -- Listing 2.9
  function Get_Transform_4D_Z  (H : Transform_4D) return Vector_3D is ((H.ZX, H.ZY, H.ZZ)); -- Listing 2.9
  function Get_Transform_4D_W  (H : Transform_4D) return Vector_3D is ((H.WX, H.WY, H.WZ)); -- Listing 2.9
  function Get_Translate       (H : Transform_4D) return Point_3D  is ((H.WX, H.WY, H.WZ)); -- Listing 2.9
  function To_Matrix_4D        (H : Transform_4D) return Matrix_4D is ((H.XX, H.YX, H.ZX, H.WX,
                                                                        H.XY, H.YY, H.ZY, H.WY,
                                                                        H.XZ, H.YZ, H.ZZ, H.WZ,
                                                                        0.0,  0.0,  0.0,  1.0));
  function To_Transform_4D (M : Matrix_4D) return Transform_4D is ((M.XX, M.YX, M.ZX, M.WX,
                                                                    M.XY, M.YY, M.ZY, M.WY,
                                                                    M.XZ, M.YZ, M.ZZ, M.WZ));
  function To_Transform_4D (A, B, C : Vector_3D; P : Point_3D) return Transform_4D is ((A.X, B.X, C.X, P.X, -- Listing 2.9
                                                                                        A.Y, B.Y, C.Y, P.Y,
                                                                                        A.Z, B.Z, C.Z, P.Z));

  -- Operations
  -- function Transpose  () return Matrix_3D;
  function Rotate     (Angle : Real_64; Kind : Dimension_Kind) return Matrix_3D;    -- Listing 2.1
  function Rotate     (Angle : Real_64; Axis : Vector_3D)      return Matrix_3D;    -- Listing 2.2
  function Reflect    (A : Vector_3D)                          return Matrix_3D;    -- Listing 2.3
  function Involution (A : Vector_3D)                          return Matrix_3D;    -- Listing 2.4
  function Scale      (SX, SY, SZ : Real_64)                   return Matrix_3D;    -- Listing 2.5
  function Scale      (S          : Real_64; A    : Vector_3D) return Matrix_3D;    -- Listing 2.6
  function Skew       (Angle      : Real_64; A, B : Vector_3D) return Matrix_3D;    -- Listing 2.7 
  function Inverse    (H : Transform_4D)                       return Transform_4D; -- Listing 2.9
  function "*" (A, B : Transform_4D) return Transform_4D is ((A.XX * B.XX + A.YX * B.XY + A.ZX * B.XZ, -- Listing 2.9
                                                              A.XX * B.YX + A.YX * B.YY + A.ZX * B.YZ,
                                                              A.XX * B.ZX + A.YX * B.ZY + A.ZX * B.ZZ,
                                                              A.XX * B.WX + A.YX * B.WY + A.ZX * B.WZ + A.WX,
                                                              A.XY * B.XX + A.YY * B.XY + A.ZY * B.XZ,
                                                              A.XY * B.YX + A.YY * B.YY + A.ZY * B.YZ,
                                                              A.XY * B.ZX + A.YY * B.ZY + A.ZY * B.ZZ,
                                                              A.XY * B.WX + A.YY * B.WY + A.ZY * B.WZ + A.WY,
                                                              A.XZ * B.XX + A.YZ * B.XY + A.ZZ * B.XZ,
                                                              A.XZ * B.YX + A.YZ * B.YY + A.ZZ * B.YZ,
                                                              A.XZ * B.ZX + A.YZ * B.ZY + A.ZZ * B.ZZ,
                                                              A.XZ * B.WX + A.YZ * B.WY + A.ZZ * B.WZ + A.WZ));
  function "*" (H : Transform_4D; V : Vector_3D) return Vector_3D is ((H.XX * V.X + H.YX * V.Y + H.ZX * V.Z, -- Listing 2.9
                                                                       H.XY * V.X + H.YY * V.Y + H.ZY * V.Z,
                                                                       H.XZ * V.X + H.YZ * V.Y + H.ZZ * V.Z));
  function "*" (H : Transform_4D; P : Point_3D) return Point_3D is ((H.XX * P.X + H.YX * P.Y + H.ZX * P.Z + H.ZX, -- Listing 2.9
                                                                     H.XY * P.X + H.YY * P.Y + H.ZY * P.Z + H.ZY,
                                                                     H.XZ * P.X + H.YZ * P.Y + H.ZZ * P.Z + H.ZZ));
  function "*" (N : Vector_3D; H : Transform_4D) return Vector_3D is ((N.X * H.XX + N.Y * H.XY + N.Z * H.XZ, -- Listing 3.1
                                                                       N.X * H.YX + N.Y * H.YY + N.Z * H.YZ,
                                                                       N.X * H.ZX + N.Y * H.ZY + N.Z * H.ZZ));

  ----------------
  -- Quaternion --
  ----------------

  -- Definition
  type Quaternion_4D is new Vector_4D; -- Listing 2.10

  -- Conversions
  function To_Vector_3D     (Q : Quaternion_4D)          return Vector_3D     is ((Q.X, Q.Y, Q.Z));    -- Listing 2.10
  function To_Quaternion_4D (V : Vector_3D; S : Real_64) return Quaternion_4D is ((V.X, V.Y, V.Z, S)); -- Listing 2.10
  function To_Quaternion_4D (V : Vector_3D)              return Quaternion_4D;

  -- Operations
  function "*" (Q1, Q2 : Quaternion_4D) return Quaternion_4D is ((Q1.W * Q2.X + Q1.X * Q2.W + Q1.Y * Q2.Z - Q1.Z * Q2.Y, -- Listing 2.10
                                                                  Q1.W * Q2.Y - Q1.X * Q2.Z + Q1.Y * Q2.W + Q1.Z * Q2.X,
                                                                  Q1.W * Q2.Z + Q1.X * Q2.Y - Q1.Y * Q2.X + Q1.Z * Q2.W,
                                                                  Q1.W * Q2.W - Q1.X * Q2.X - Q1.Y * Q2.Y - Q1.Z * Q2.Z));
  function Transform (Q : Quaternion_4D; V : Vector_3D) return Vector_3D; -- Listing 2.11
  function Rotation  (Q : Quaternion_4D)                return Matrix_3D; -- Listing 2.12
  procedure Rotate   (Q : in out Quaternion_4D; M : Matrix_3D);           -- Listing 2.13
  
  -----------
  -- Plane --
  -----------

  -- Definition
  type Plane_4D is new Vector_4D; -- Listing 3.4
  package Vector_Plane is new Vectors (Plane_4D);

  -- Operations 
  function Reflect (F : Plane_4D)                   return Transform_4D;                                           -- Listing 3.5
  function Normal  (F : Plane_4D)                   return Vector_3D is ((F.X, F.Y, F.Z));                         -- Listing 3.4
  function Dot     (F : Plane_4D; V : Vector_3D)    return Real_64   is (F.X * V.X + F.Y * V.Y + F.Z * V.Z);       -- Listing 3.4
  function Dot     (F : Plane_4D; P : Point_3D)     return Real_64   is (F.X * P.X + F.Y * P.Y + F.Z * P.Z + F.W); -- Listing 3.4
  function "*"     (F : Plane_4D; H : Transform_4D) return Plane_4D  is ((F.X * H.XX + F.Y * H.XY + F.Z * H.XZ,    -- Listing 3.9
                                                                          F.X * H.YX + F.Y * H.YY + F.Z * H.YZ,
                                                                          F.X * H.ZX + F.Y * H.ZY + F.Z * H.ZZ,
                                                                          F.X * H.WX + F.Y * H.WY + F.Z * H.WZ + F.W));

  ----------
  -- Line --
  ----------

  -- Definition
  type Line_3D is record -- Listing 3.10
      Direction : Vector_3D := (others => <>);
      Moment    : Vector_3D := (others => <>);
    end record;

  -- Operations
  function Transform (L : Line_3D; H : Transform_4D) return Line_3D; -- Listing 3.11

  -------------
  -- Frustum --
  -------------

  type Frustum_State is record
      Target, Right, Up, Beginning, Ending : Vector_3D := (others => <>);
    end record;

  -------------
  -- Algebra -- Listing 4.1 and Table 4.6
  -------------

  -- {0|0} if p and q are coincident 
  function Contain (P, Q : Point_3D) return Line_3D is (((Q.X - P.X,
                                                          Q.Y - P.Y,
                                                          Q.Z - P.Z),
                                                         (P.Y * Q.Z - P.Z * Q.Y,
                                                          P.Z * Q.X - P.X * Q.Z,
                                                          P.X * Q.Y - P.Y * Q.X)));

  -- [0|0] if p lies on line L
  function Contain (L : Line_3D; P : Point_3D) return Plane_4D is ((L.Direction.Y * P.Z - L.Direction.Z * P.Y + L.Moment.X,
                                                                    L.Direction.Z * P.X - L.Direction.X * P.Z + L.Moment.Y,
                                                                    L.Direction.X * P.Y - L.Direction.Y * P.X + L.Moment.Z,
                                                                   -L.Moment.X    * P.X - L.Moment.Y    * P.Y - L.Moment.Z * P.Z));

  -- [0|0] if all three points are collinear
  function Contain (P, Q, R : Point_3D) return Plane_4D is (Contain (Contain (P, Q), R));

  -- {0|0} if f and a are coincident, {0|m} if f and g are parallel but not coincident
  function Intersect (F, G : Plane_4D) return Line_3D is (((F.Y * G.Z - F.Z * G.Y,
                                                            F.Z * G.X - F.X * G.Z,
                                                            F.X * G.Y - F.Y * G.X),
                                                           (G.X * F.W - F.X * G.W,
                                                            G.Y * F.W - F.Y * G.W,
                                                            G.Z * F.W - F.Z * G.W)));

  -- (0|0) if L lies in the plane f, (p|0) if L is parallel to f but does not lie in f
  function Intersect (L : Line_3D; F : Plane_4D) return Vector_4D is ((L.Moment.Y    * F.Z - L.Moment.Z    * F.Y + L.Direction.X,
                                                                       L.Moment.Z    * F.X - L.Moment.X    * F.Z + L.Direction.Y,
                                                                       L.Moment.X    * F.Y - L.Moment.Y    * F.X + L.Direction.Z,
                                                                      -L.Direction.X * F.X - L.Direction.Y * F.Y - L.Direction.Z * F.Z));

  -- (0|0) if any two planes are coincident or all three planes are parallel, (p|0) if planes all intersect at parallel lines
  function Intersect (F, G, H : Plane_4D) return Vector_4D is (Intersect (Intersect (F, G), H));

  -- 0 if L1 and L2 are coplanar
  function Distance (L1, L2 : Line_3D) return Real_64 is (-(Dot (L1.Direction, L2.Moment) + Dot (L2.Direction, L1.Moment)));

  -- 0 if p lies in the plane f
  function Distance (P : Point_3D; F : Plane_4D) return Real_64 is (P.X * F.X + P.Y * F.Y + P.Z * F.Z + F.W);
end;
