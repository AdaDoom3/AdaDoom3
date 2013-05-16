pragma Warnings (Off);
with Ada.Numerics.Aux; use Ada.Numerics.Aux;
pragma Warnings (On);

package body Trackball is

   package Ana renames Ada.Numerics.Aux;

   TRACKBALLSIZE : constant Float := 0.8;
   --  This size should really be based on the distance from the center of
   --  rotation to the point on the object underneath the mouse.  That point
   --  would then track the mouse as closely as possible.  This is a simple
   --  example, though, so that is left as an Exercise for the Programmer.

   procedure Vsub (Src1, Src2 : Vector; Dst : out Vector) is
   begin
      Dst := (Src1 (0) - Src2 (0),
              Src1 (1) - Src2 (1),
              Src1 (2) - Src2 (2));
   end Vsub;

   procedure Vcross (V1, V2 : Vector; Cross : out Vector) is
   begin
      Cross := (V1 (1) * V2 (2) - V1 (2) * V2 (1),
                V1 (2) * V2 (0) - V1 (0) * V2 (2),
                V1 (0) * V2 (1) - V1 (1) * V2 (0));
   end Vcross;

   function Vlength (V : Vector) return Float is
   begin
      return Float (Sqrt (Ana.Double (V (0) * V (0)
                                      + V (1) * V (1)
                                      + V (2) * V (2))));
   end Vlength;

   procedure Vscale (V : in out Vector; Div : Float) is
   begin
      V := (V (0) * Div,
            V (1) * Div,
            V (2) * Div);
   end Vscale;

   procedure Vnormal (V : in out Vector) is
   begin
      Vscale (V, 1.0 / Vlength (V));
   end Vnormal;

   function Vdot (V1, V2 : Vector) return Float is
   begin
      return V1 (0) * V2 (0) + V1 (1) * V2 (1) + V1 (2) * V2 (2);
   end Vdot;

   procedure Vadd (Src1, Src2 : Vector; Dst : out Vector) is
   begin
      Dst := (Src1 (0) + Src2 (0),
              Src1 (1) + Src2 (1),
              Src1 (2) + Src2 (2));
   end Vadd;

   --  Quaternions always obey: a^2 + b^2 + c^2 + d^2 = 1.0 If they don't add
   --  up to 1.0, dividing by their magnitued will renormalize them.
   --
   --  Note: See the following for more information on quaternions:
   --
   --  Shoemake, K., Animating rotation with quaternion curves, Computer
   --    Graphics 19, No 3 (Proc. SIGGRAPH'85), 245-254, 1985.
   --  Pletinckx, D., Quaternion calculus as a basic tool in computer
   --    graphics, The Visual Computer 5, 2-13, 1989.

   procedure Normalize_Quat (Q : in out Quaternion) is
      Mag : Float;

   begin
      Mag := Q (0) * Q (0) + Q (1) * Q (1) + Q (2) * Q (2) + Q (3) * Q (3);
      Q (0) := Q (0) / Mag;
      Q (1) := Q (1) / Mag;
      Q (2) := Q (2) / Mag;
      Q (3) := Q (3) / Mag;
   end Normalize_Quat;

   -----------------------
   -- Project_To_Sphere --
   -----------------------
   --  Project an x,y pair onto a sphere of radius r OR a hyperbolic sheet if
   --  we are away from the center of the sphere.

   function Project_To_Sphere (R, X, Y : Float) return Float is
      D, T : Float;
   begin
      D := Float (Sqrt (Ana.Double (X * X + Y * Y)));
      if D < R * 0.70710678118654752440 then  --  inside sphere
         return Float (Sqrt (Ana.Double (R * R - D * D)));
      else   --  on hyperbola
         T := R / 1.41421356237309504880;
         return T * T / D;
      end if;
   end Project_To_Sphere;

   ---------------
   -- Trackball --
   ---------------

   --  Ok, simulate a track-ball.  Project the points onto the virtual
   --  trackball, then figure out the axis of rotation, which is the cross
   --  product of P1 P2 and O P1 (O is the center of the ball, 0,0,0) Note:
   --  This is a deformed trackball-- is a trackball in the center, but is
   --  deformed into a hyperbolic sheet of rotation away from the center.
   --  This particular function was chosen after trying out several
   --  variations.

   --  It is assumed that the arguments to this routine are in the range (-1.0
   --  ... 1.0)

   procedure Trackball (Q : out Quaternion;
                        P1x, P1y, P2x, P2y : Float)
   is
      A   : Vector;  --  Axis of rotation
      Phi : Float;   --  How much to rotate about axis
      P1, P2, D : Vector;
      T   : Float;

   begin

      if P1x = P2x and then P1y = P2y then
         Q := (0.0, 0.0, 0.0, 1.0);
         return;
      end if;

      --  First, figure out z-coordinates for project of P1 and P2 to
      --  deformed sphere

      P1 := (P1x, P1y, Project_To_Sphere (TRACKBALLSIZE, P1x, P1y));
      P2 := (P2x, P2y, Project_To_Sphere (TRACKBALLSIZE, P2x, P2y));

      Vcross (P2, P1, Cross => A);

      --  Figure out how much to rotate around that axis
      Vsub (P1, P2, Dst => D);
      T := Vlength (D) / (2.0 * TRACKBALLSIZE);

      --  Avoid problem with out-of-control values
      if T > 1.0 then
         T := 1.0;
      elsif T < -1.0 then
         T := -1.0;
      end if;

      Phi := 2.0 * Float (Asin (Ana.Double (T)));

      Axis_To_Quat (A, Phi, Q);
   end Trackball;

   ---------------
   -- Add_Quats --
   ---------------

   --  Given two rotations, e1 and e2, expressed as quaternion rotations,
   --  figure out the equivalent single rotation and stuff it into dest.
   --  This routine also normalizes the result every RENORMCOUNT times it is
   --  called, to keep error from creeping in.
   --  NOTE: This routine is written so that q1 or q2 may be the same as dest
   --  (or each other).

   Count : Integer := 0;
   RENORMCOUNT : constant Integer := 97;

   procedure Add_Quats (Q1, Q2 : Quaternion;
                        Dest   : out Quaternion)
   is
      Qv1 : constant Vector := Vector (Q1 (0 .. 2));
      Qv2 : constant Vector := Vector (Q2 (0 .. 2));
      T1, T2, T3, Tf : Vector;

   begin
      T1 := Qv1;
      Vscale (T1, Q2 (3));

      T2 := Qv2;
      Vscale (T2, Q1 (3));

      Vcross (Qv1, Qv2, Cross => T3);
      Vadd (T1, T2, Tf);
      Vadd (T3, Tf, Tf);
      Dest := (Tf (0), Tf (1), Tf (2),
               Q1 (3) * Q2 (3) - Vdot (Qv1, Qv2));

      Count := Count + 1;
      if Count > RENORMCOUNT then
         Count := 0;
         Normalize_Quat (Dest);
      end if;
   end Add_Quats;

   ---------------------
   -- Build_Rotmatrix --
   ---------------------
   --  Build a rotation matrix, given a quaternion rotation.

   procedure Build_Rotmatrix (M : out Matrix; Q : Quaternion)
   is
   begin
      M (0, 0) := 1.0 - 2.0 * (Q (1) * Q (1) + Q (2) * Q (2));
      M (0, 1) := 2.0 * (Q (0) * Q (1) - Q (2) * Q (3));
      M (0, 2) := 2.0 * (Q (2) * Q (0) + Q (1) * Q (3));
      M (0, 3) := 0.0;

      M (1, 0) := 2.0 * (Q (0) * Q (1) + Q (2) * Q (3));
      M (1, 1) := 1.0 - 2.0 * (Q (2) * Q (2) + Q (0) * Q (0));
      M (1, 2) := 2.0 * (Q (1) * Q (2) - Q (0) * Q (3));
      M (1, 3) := 0.0;

      M (2, 0) := 2.0 * (Q (2) * Q (0) - Q (1) * Q (3));
      M (2, 1) := 2.0 * (Q (1) * Q (2) + Q (0) * Q (3));
      M (2, 2) := 1.0 - 2.0 * (Q (1) * Q (1) + Q (0) * Q (0));
      M (2, 3) := 0.0;

      M (3, 0) := 0.0;
      M (3, 1) := 0.0;
      M (3, 2) := 0.0;
      M (3, 3) := 1.0;
   end Build_Rotmatrix;

   ------------------
   -- Axis_To_Quat --
   ------------------

   procedure Axis_To_Quat (A : Vector; Phi : Float; Q : out Quaternion) is
      V : Vector;
   begin
      V := A;
      Vnormal (V);
      Vscale (V, Float (Sin (Ana.Double (Phi / 2.0))));
      Q := (V (0), V (1), V (2),
            Float (Cos (Ana.Double (Phi / 2.0))));
   end Axis_To_Quat;

end Trackball;
