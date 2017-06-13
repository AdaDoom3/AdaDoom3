
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

package body Neo.Core.Math is

  ------------
  -- Matrix --
  ------------

  -- 3D Data
  procedure Set_Matrix_3D_X (M : in out Matrix_3D; V : Vector_3D) is begin M.XX := V.X; M.XY := V.Y; M.XZ := V.Z; end; 
  procedure Set_Matrix_3D_Y (M : in out Matrix_3D; V : Vector_3D) is begin M.YX := V.X; M.YY := V.Y; M.YZ := V.Z; end; 
  procedure Set_Matrix_3D_Z (M : in out Matrix_3D; V : Vector_3D) is begin M.ZX := V.X; M.ZY := V.Y; M.ZZ := V.Z; end; 

  -- 4D Data
  procedure Set_Matrix_4D_X (M : in out Matrix_4D; V : Vector_4D) is begin M.XX := V.X; M.XY := V.Y; M.XZ := V.Z; M.XW := V.W; end; 
  procedure Set_Matrix_4D_Y (M : in out Matrix_4D; V : Vector_4D) is begin M.YX := V.X; M.YY := V.Y; M.YZ := V.Z; M.YW := V.W; end; 
  procedure Set_Matrix_4D_Z (M : in out Matrix_4D; V : Vector_4D) is begin M.ZX := V.X; M.ZY := V.Y; M.ZZ := V.Z; M.ZW := V.W; end; 
  procedure Set_Matrix_4D_W (M : in out Matrix_4D; V : Vector_4D) is begin M.WX := V.X; M.WY := V.Y; M.WZ := V.Z; M.WW := V.W; end; 
 
  -- Inverse
  function Inverse (M : Matrix_3D) return Matrix_3D is -- Listing 1.10
    A       : constant Vector_3D := Get_Matrix_3D_X (M);
    B       : constant Vector_3D := Get_Matrix_3D_Y (M);
    C       : constant Vector_3D := Get_Matrix_3D_Z (M);
    R0      : constant Vector_3D := Cross (B, C);
    R1      : constant Vector_3D := Cross (C, A);
    R2      : constant Vector_3D := Cross (A, B);
    INV_DET : constant Real      := 1.0 / Dot (R2, C);
    begin
      return (R0.X * INV_DET, R0.Y * INV_DET, R0.Z * INV_DET,
              R1.X * INV_DET, R1.Y * INV_DET, R1.Z * INV_DET,
              R2.X * INV_DET, R2.Y * INV_DET, R2.Z * INV_DET);
    end;
  function Inverse (M : Matrix_4D) return Matrix_4D is -- Listing 1.11
    A       : constant Vector_3D := Get_Matrix_4D_X (M);
    B       : constant Vector_3D := Get_Matrix_4D_Y (M);
    C       : constant Vector_3D := Get_Matrix_4D_Z (M);
    D       : constant Vector_3D := Get_Matrix_4D_W (M);
    S       :          Vector_3D := Cross (A, B);
    T       :          Vector_3D := Cross (C, D);
    U       :          Vector_3D := A * M.YW - B * M.XW;
    V       :          Vector_3D := C * M.WW - D * M.ZW;
    INV_DET : constant Real      := 1.0 / (Dot (S, V) + Dot (T, U));
    R0, R1, R2, R3 : Vector_3D;
    begin
      S  := S * INV_DET;
      T  := T * INV_DET;
      U  := U * INV_DET;
      V  := V * INV_DET;
      R0 := Cross (B, V) + T * M.YW;
      R1 := Cross (V, A) - T * M.XW;
      R2 := Cross (D, U) + S * M.WW;
      R3 := Cross (U, C) - S * M.ZW;
      return (R0.X, R0.Y, R0.Z, -Dot (B, T),
              R1.X, R1.Y, R1.Z,  Dot (A, T),
              R2.X, R2.Y, R2.Z, -Dot (D, S),
              R3.X, R3.Y, R3.Z,  Dot (C, S));
    end;

  ----------------
  -- Quaternion --
  ----------------

  function Transform (Q : Quaternion_4D; V : Vector_3D) return Vector_3D is -- Listing 2.11
    B  : constant Vector_3D := To_Vector_3D (Q);
    B2 : constant Real      := B.X ** 2 + B.Y ** 2 + B.Z ** 2;
    begin
      return V * (Q.W ** 2 - B2) + B * (Dot (V, B) * 2.0) + Cross (B, V) * (Q.W * 2.0);
    end;
  function Rotation (Q : Quaternion_4D) return Matrix_3D is -- Listing 2.12
    X2 : constant Real := Q.X ** 2;
    Y2 : constant Real := Q.Y ** 2;
    Z2 : constant Real := Q.Z ** 2;
    YX : constant Real := Q.Y * Q.X;
    ZX : constant Real := Q.Z * Q.X;
    ZY : constant Real := Q.Z * Q.Y; 
    XW : constant Real := Q.X * Q.W;
    YW : constant Real := Q.Y * Q.W;
    ZW : constant Real := Q.Z * Q.W;
    begin
      return (1.0 - 2.0 * (Y2 + Z2), 2.0 * (YX - ZW), 2.0 * (ZX + YW),
              2.0 * (YX + ZW), 1.0 - 2.0 * (X2 + Z2), 2.0 * (ZY - XW),
              2.0 * (ZX - YW), 2.0 * (ZY + XW), 1.0 - 2.0 * (X2 + Y2));
    end;
  procedure Rotate (Q : in out Quaternion_4D; M : Matrix_3D) is -- Listing 2.13
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
  function To_Quaternion_4D (V : Vector_3D) return Quaternion_4D is
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
                      CR * CPCY + SR * SPCY));
    end;

  ---------------
  -- Transform --
  ---------------

  -- Data
  procedure Set_Transform_4D_X (H : in out Transform_4D; V : Vector_3D) is begin H.XX := V.X; H.XY := V.Y; H.XZ := V.Z;   end;
  procedure Set_Transform_4D_Y (H : in out Transform_4D; V : Vector_3D) is begin H.YX := V.X; H.YY := V.Y; H.YZ := V.Z;   end;
  procedure Set_Transform_4D_Z (H : in out Transform_4D; V : Vector_3D) is begin H.ZX := V.X; H.ZY := V.Y; H.ZZ := V.Z;   end;
  procedure Set_Transform_4D_W (H : in out Transform_4D; V : Vector_3D) is begin H.WX := V.X; H.WY := V.Y; H.WZ := V.Z;   end;
  procedure Set_Translate      (H : in out Transform_4D; P : Point_3D)  is begin Set_Transform_4D_W (H, (P.X, P.Y, P.Z)); end;

  -- Inverse
  function Inverse (H : Transform_4D) return Transform_4D is -- Listing 2.9
    A       : constant Vector_3D := Get_Transform_4D_X (H);
    B       : constant Vector_3D := Get_Transform_4D_Y (H);
    C       : constant Vector_3D := Get_Transform_4D_Z (H);
    D       : constant Vector_3D := Get_Transform_4D_W (H);
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
  function Rotate (Angle : Real; Kind : Dimension_Kind) return Matrix_3D is -- Listing 2.1
    C : constant Real := Cos (Angle);
    S : constant Real := Sin (Angle);
    begin
      return (case Kind is
                when X_Dimension => (1.0, 0.0, 0.0, 0.0, C, -S, 0.0, S, C),
                when Y_Dimension => (C, 0.0, S, 0.0, 1.0, 0.0, -S, 0.0, C),
                when Z_Dimension => (C,-S, 0.0, S, C, 0.0, 0.0, 0.0, 1.0)); 
    end;
  function Rotate (Angle : Real; Axis : Vector_3D) return Matrix_3D is -- Listing 2.2
    C  : constant Real      := Cos (Angle);
    S  : constant Real      := Sin (Angle);
    V  : constant Vector_3D := Axis * (1.0 - C);
    YX : constant Real      := Axis.Y * Axis.X;
    ZX : constant Real      := Axis.Z * Axis.X;
    ZY : constant Real      := Axis.Z * Axis.Y;
    begin
      return (C + V.X * Axis.X, YX - S * Axis.Z, ZX + S * Axis.Y,
              YX + S * Axis.X, C + V.Y + Axis.Y, ZY - S * Axis.X,
              ZX - S * Axis.Y, ZY + S * Axis.X, C + V.Z + Axis.Z);
    end;

  -- Reflections
  function Reflect (A : Vector_3D) return Matrix_3D is -- Listing 2.3
    V  : constant Vector_3D := A * (-2.0);
    YX : constant Real      := V.X * A.Y;
    ZX : constant Real      := V.X * A.Z;
    ZY : constant Real      := V.Y * A.Z;
    begin
      return (V.X * A.X + 1.0, YX, ZX,
              YX, V.Y * A.Y + 1.0, ZY,
              ZX, ZY, V.Z * A.Z + 1.0);
    end;
  function Reflect (F : Plane_4D) return Transform_4D is -- !!!
    V  : constant Vector_3D := Normal (F) * (-2.0);
    YX : constant Real      := V.X * F.Y;
    ZX : constant Real      := V.X * F.Z;
    ZY : constant Real      := V.Y * F.Z;
    begin
      return (V.X * F.X + 1.0, YX, ZX, V.X * F.W,
              YX, V.Y * F.Y + 1.0, ZY, V.Y * F.W,
              ZX, ZY, V.Z * F.Z + 1.0, V.Z * F.W);
    end;

  -- Scalers
  function Scale (SX, SY, SZ : Real) return Matrix_3D is -- Listing 2.5
    begin
      return (SX, 0.0, 0.0,
              0.0, SY, 0.0,
              0.0, 0.0, SZ);
    end;
  function Scale (S : Real; A : Vector_3D) return Matrix_3D is -- Listing 2.6
    S2 : constant Real      := S - 1.0;
    V  : constant Vector_3D := (A.X - S, A.Y - S, A.Z - S);
    YX : constant Real      := V.X * A.Y;
    ZX : constant Real      := V.X * A.Z;
    ZY : constant Real      := V.Y * A.Z;
    begin
      return (V.X * A.X + 1.0, YX, ZX,
              YX, V.Y * A.Y + 1.0, ZY,
              ZX, ZY, V.Z * A.Z + 1.0);
    end;

  -- Involution
  function Involution (A : Vector_3D) return Matrix_3D is -- Listing 2.4
    V  : constant Vector_3D := A * 2.0;
    YX : constant Real      := V.X * A.Y;
    ZX : constant Real      := V.X * A.Z;
    ZY : constant Real      := V.Y * A.Z;
    begin
      return (V.X * A.X - 1.0, YX, ZX,
              YX, V.Y * A.Y - 1.0, ZY,
              ZX, ZY, V.Z * A.Z - 1.0);
    end;

  -- Skew
  function Skew (Angle : Real; A, B : Vector_3D) return Matrix_3D is -- Listing 2.7    
    V : constant Vector_3D := A * Tan (Angle);
    begin
      return (V.X * B.X + 1.0, V.X * B.Y, V.X * B.Z,
              V.Y * B.X, V.Y * B.Y + 1.0, V.Y * B.Z,
              V.Z * B.X, V.Z * B.Y, V.Z * B.Z + 1.0);
    end;

  ----------
  -- Line --
  ----------

  function Transform (L : Line_3D; H : Transform_4D) return Line_3D is -- Listing 3.11
    T : constant Vector_3D := To_Vector_3D (Get_Translate (H));
    V : constant Vector_3D := H * L.Direction;
    begin
      return (V, To_Matrix_3D (Cross (Get_Transform_4D_Y (H), Get_Transform_4D_Z (H)),
                               Cross (Get_Transform_4D_Z (H), Get_Transform_4D_X (H)),
                               Cross (Get_Transform_4D_X (H), Get_Transform_4D_Y (H))) * L.Moment + Cross (T, V));
    end;
end;