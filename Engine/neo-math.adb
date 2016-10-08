
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

package body Neo.Math is

  ------------
  -- Matrix --
  ------------

  -- 3D Data
  procedure Set_X (M : in out Matrix_3D; V : Vector_3D) is begin M.XX := V.X; M.XY := V.Y; M.XZ := V.Z; end; 
  procedure Set_Y (M : in out Matrix_3D; V : Vector_3D) is begin M.YX := V.X; M.YY := V.Y; M.YZ := V.Z; end; 
  procedure Set_Z (M : in out Matrix_3D; V : Vector_3D) is begin M.ZX := V.X; M.ZY := V.Y; M.ZZ := V.Z; end; 

  -- 4D Data
  procedure Set_X (M : in out Matrix_4D; V : Vector_4D) is begin M.XX := V.X; M.XY := V.Y; M.XZ := V.Z; M.XW := V.W end; 
  procedure Set_Y (M : in out Matrix_4D; V : Vector_4D) is begin M.YX := V.X; M.YY := V.Y; M.YZ := V.Z; M.YW := V.W end; 
  procedure Set_Z (M : in out Matrix_4D; V : Vector_4D) is begin M.WX := V.X; M.WY := V.Y; M.WZ := V.Z; M.WW := V.W end; 

  -- Inverse
  function Inverse (M : Matrix_3D) return Matrix_3D is -- Listing 1.10
    R2      : constant Vector := Cross (M.X, M.Y);
    INV_DET : constant Real   := 1.0 / Dot (R2, M.Z);
    begin
      return (Cross (M.Y, M.Z) * INV_DET, Cross (M.Z, M.X) * INV_DET, R2 * INV_DET);
    end;
  function Inverse (M : Matrix_4D) return Matrix_3D is -- Listing 1.11
    S       : constant Vector_3D := Cross (Get_X (M), Get_Y (M));
    T       : constant Vector_3D := Cross (Get_Z (M), Get_W (W));
    U       : constant Vector_3D := M.X * M.YW - M.Y * M.XW;
    V       : constant Vector_3D := M.X * M.YW - M.Y * M.XW;
    INV_DET : constant Real      := 1.0 / (Dot (S, V) + Dot (T, U));
    begin
      return (To_Vector_4D (Cross (M.Y, V) + T * M.YW, Dot (M.Y, T)),
              To_Vector_4D (Cross (V, M.X) - T * M.XW, Dot (M.X, T)),
              To_Vector_4D (Cross (M.W, U) + S * M.WW, Dot (M.W, S)),
              To_Vector_4D (Cross (U, M.Z) - S * M.ZW, Dot (M.Z, S)));
    end;

  ----------------
  -- Quaternion --
  ----------------

  function Transform (V : Vector_3D; Q : Quaternion) return Vector_3D is -- Listing 2.11
    B  : constant Vector_3D := Q.Get_Vector;
    B2 : constant Real      := B.X ** 2 + B.Y ** 2 + B.Z ** 2;
    begin
      return V * (Q.W ** 2 - B2) + B * (Dot (V, B) * 2.0) + Cross (B, V) * (Q.W * 2.0);
    end;
  function Rotation (Q : Quaternion) return Matrix_3D is -- Listing 2.12
    X2 : constant Real := X ** 2;
    Y2 : constant Real := Y ** 2;
    Z2 : constant Real := Z ** 2;
    YX : constant Real := Y * X;
    ZX : constant Real := Z * X;
    ZY : constant Real := Z * Y; 
    XW : constant Real := X * W;
    YW : constant Real := Y * W;
    ZW : constant Real := Z * W;
    begin
      return (1.0 - 2.0 * (Y2 + Z2), 2.0 * (YX - ZW), 2.0 * (ZX + YW),
              2.0 * (YX + ZW), 1.0 - 2.0 * (X2 + Z2), 2.0 * (ZY - XW),
              2.0 * (ZX - YW), 2.0 * (ZY + XW), 1.0 - 2.0 * (X2 + Y2));
    end;
  procedure Rotate (Q : in out Quaternion; M : Matrix_3D) is -- Listing 2.13
    Sum : constant Real := M.XX + M.YY + M.ZZ;
    F   : Real;
    begin
      if Sum > 0.0 then
        Q.W := Sqrt (Sum + 1.0) * 0.5;
        F   := 0.25 / Q.W;
        Q.X := (M.YZ - M.ZY) * F;
        Q.Y := (M.ZX - M.XZ) * F;
        Q.Z := (M.XY - M.YX) * F;
      elsif M.XX > M.YY and M.XX > M.ZZ then
        Q.X := Sqrt (M.XX - M.YY - M.ZZ + 1.0) * 0.5;
        F   := 0.25 / Q.X;
        Q.Y := (M.XY - M.YX) * F;
        Q.Z := (M.ZX - M.XZ) * F;
        Q.W := (M.YZ - M.ZY) * F;
      elsif M.YY > M.ZZ then
        Q.Y := Sqrt (M.YY - M.XX - M.ZZ + 1.0) * 0.5;
        F   := 0.25 / Q.Y;
        Q.X := (M.XY - M.YX) * F;
        Q.Z := (M.YZ - M.ZY) * F;
        Q.W := (M.ZX - M.XZ) * F;
      else
        Q.Z := Sqrt (M.ZZ - M.XX - M.YY + 1.0) * 0.5;
        F   := 0.25 / Q.Z;
        Q.X := (M.ZX - M.XZ) * F;
        Q.Y := (M.YZ - M.ZY) * F;
        Q.W := (M.XY - M.YX) * F;
      end if;
    end;
  function To_Quaternion_4D (V : Vector_3D) return Quaternion is
    SR   : constant Real := Sin (V.X * 0.5);
    CR   : constant Real := Cos (V.X * 0.5);
    SP   : constant Real := Sin (V.Y * 0.5);
    CP   : constant Real := Cos (V.Y * 0.5);
    SY   : constant Real := Sin (V.Z * 0.5);
    CY   : constant Real := Cos (V.Z * 0.5);
    CPCY : constant Real := CP * CY;
    SPCY : constant Real := SP * CY;
    CPSY : constant Real := CP * SY;
    SPSY : constant Real := SP * SY;
    begin
      return Normal ((SR * CPCY - CR * SPSY,
                      CR * SPCY + SR * CPSY,
                      CR * CPSY - SR * SPCY,
                      CR * CPCY + SR * SPCY))
    end;

  ---------------
  -- Transform --
  ---------------

  -- Data
  procedure Set_X         (T : in out Transform; V : Vector_3D) is begin T.XX := V.X; T.XY := V.Y; T.XZ := V.Z; end;
  procedure Set_Y         (T : in out Transform; V : Vector_3D) is begin T.YX := V.X; T.YY := V.Y; T.YZ := V.Z; end;
  procedure Set_Z         (T : in out Transform; V : Vector_3D) is begin T.ZX := V.X; T.ZY := V.Y; T.ZZ := V.Z; end;
  procedure Set_W         (T : in out Transform; V : Vector_3D) is begin T.WX := V.X; T.WY := V.Y; T.WZ := V.Z; end;
  procedure Set_Translate (T : in out Transform; P : Point_3D)  is begin T.Set_W ((P.X, P.Y, P.Z)); end;

  -- Inverse
  function Inverse (H : Transform) return Transform is -- Listing 2.9
    A       : constant Vector_3D := Get_X (H);
    B       : constant Vector_3D := Get_Y (H);
    C       : constant Vector_3D := Get_Z (H);
    D       : constant Vector_3D := Get_W (H);
    S       :          Vector_3D := Cross (A, B);
    T       :          Vector_3D := Cross (C, D);
    INV_DET : constant Real      := 1.0 / Dot (S, C);
    V       : constant Vector_3D := C * INV_DET;
    R0      : constant Vector_3D := Cross (B, V);
    R1      : constant Vector_3D := Cross (V, A);
    begin
      S := S * INV_DET;
      T := T * INV_DET;
      return (R0.X, R0.Y, R0.Z, -Dot (B, T),
              R1.X, R1.Y, R1.Z,  Dot (A, T),
              S.X,  S.Y,  S.Z,  -Dot (D, S));
    end;

  -- Rotations
  function Rotate (Angle : Real; Kind : Kind_3D) return Matrix_3D is -- Listing 2.1
    C : constant Real := Cos (Angle);
    S : constant Real := Sin (Angle);
    begin
      return (case Kind is
                when XX => ((1.0, 0.0, 0.0), (0.0, C, -S), (0.0, S, C)),
                when YY => ((C, 0.0, S), (0.0, 1.0, 0.0), (-S, 0.0, C)),
                when ZZ => ((C,-S, 0.0), (S, C, 0.0), (0.0, 0.0, 1.0))); 
    end;
  function Rotate (Angle : Real; Axis : Vector_3D) return Matrix_3D is -- Listing 2.2
    C  : constant Real := Cos (Angle);
    S  : constant Real := Sin (Angle);
    V  : constant Real := Axis * (1.0 - C);
    YX : constant Real := Axis.Y * Axis.X;
    ZX : constant Real := Axis.Z * Axis.X;
    ZY : constant Real := Axis.Z * Axis.Y;
    begin
      return ((C + V.X * Axis.X, YX - S * Axis.Z, ZX + S * Axis.Y),
              (YX + S * Axis.X, C + V.Y + Axis.Y, ZY - S * Axis.X),
              (ZX - S * Axis.Y, ZY + S * Axis.X, C + V.Z + Axis.Z));
    end;

  -- Reflections
  function Reflect (A : Vector_3D) return Matrix_3D is -- Listing 2.3
    V  : constant Vector_3D := A * -2.0;
    YX : constant Real      := V.X * A.Y;
    ZX : constant Real      := V.X * A.Z;
    ZY : constant Real      := V.Y * A.Z;
    begin
      return ((V.X * A.X + 1.0, YX, ZX),
              (YX, V.Y * A.Y + 1.0, ZY),
              (ZX, ZY, V.Z * A.Z + 1.0));
    end;
  function Reflect (A : Plane) return Transform is
    V  : constant Vector_3D := Normalize (A) * -2.0;
    YX : constant Real      := V.X * A.Y;
    ZX : constant Real      := V.X * A.Z;
    ZY : constant Real      := V.Y * A.Z;
    begin
      return ((V.X * A.X + 1.0, YX, ZX, V.X * A (W)),
              (YX, V.Y * A.Y + 1.0, ZY, V.Y * A (W)),
              (ZX, ZY, V.Z * A.Z + 1.0, V.Z * A (W)));
    end;

  -- Scalers
  function Scale (SX, SY, SZ : Real) return Matrix_3D is -- Listing 2.5
    begin
      return ((SX, 0.0, 0.0), (0.0, SY, 0.0), (0.0, 0.0, SZ));
    end;
  function Scale (S : Real; A : Vector_3D) return Matrix_3D is -- Listing 2.6
    V  : constant Vector_3D := A - (S - 1.0);
    YX : constant Real      := V.X * A.Y;
    ZX : constant Real      := V.X * A.Z;
    ZY : constant Real      := V.Y * A.Z;
    begin
      return ((V.X * A.X + 1.0, YX, ZX),
              (YX, V.Y * A.Y + 1.0, ZY),
              (ZX, ZY, V.Z * A.Z + 1.0));
    end;

  -- Involution
  function Involution (A : Vector_3D) return Matrix_3D is -- Listing 2.4
    V  : constant Vector_3D := A * 2.0;
    YX : constant Real      := V.X * A.Y;
    ZX : constant Real      := V.X * A.Z;
    ZY : constant Real      := V.Y * A.Z;
    begin
      return ((V.X * A.X - 1.0, YX, ZX),
              (YX, V.Y * A.Y - 1.0, ZY),
              (ZX, ZY, V.Z * A.Z - 1.0));
    end;

  -- Skew
  function Skew (Angle : Real; A, B : Vector_3D) return Matrix_3D is -- Listing 2.7    
    V : constant Vector_3D := A * Tan (Angle);
    begin
      return ((V.X * B.X + 1.0, V.X * B.Y, V.X * B.Z),
              (V.Y * B.X, V.Y * B.Y + 1.0, V.Y * B.Z),
              (V.Z * B.X, V.Z * B.Y, V.Z * B.Z + 1.0));
    end;

  ----------
  -- Line --
  ----------

  function Transform (A : Line; B : Transform) return Line is -- Listing 3.11
    T : constant Point_3D  := Translate (B);
    V : constant Vector_3D := B * A.Direction;
    begin
      return (V, To_Transform ((Cross (B.Y, B.Z), Cross (B.Z, B.X), Cross (B.X, B.Y))) * A.Moment + Cross (T, V));
    end;
end;