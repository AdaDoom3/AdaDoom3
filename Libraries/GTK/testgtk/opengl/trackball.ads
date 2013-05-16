
--  virtual trackball implementation

package Trackball is

   type Quaternion is array (0 .. 3) of Float;
   type Matrix is array (0 .. 3, 0 .. 3) of aliased Float;
   type Vector is array (0 .. 2) of Float;

   procedure Trackball (Q : out Quaternion;
                        P1x, P1y, P2x, P2y : Float);
   --  Pass the x and y coordinates of the last and current positions of
   --  the mouse, scaled so they are from (-1.0 ... 1.0).
   --  The resulting rotation is returned as a quaternion rotation in the
   --  first paramater.

   procedure Add_Quats (Q1, Q2 : Quaternion;
                        Dest   : out Quaternion);
   --  Given two quaternions, add them together to get a third quaternion.
   --  Adding quaternions to get a compound rotation is analagous to adding
   --  translations to get a compound translation.  When incrementally adding
   --  rotations, the first argument here should be the new rotation, the
   --  second and third the total rotation (which will be over-written with
   --  the resulting new total rotation).

   procedure Build_Rotmatrix (M : out Matrix; Q : Quaternion);
   --  A useful function, builds a rotation matrix in Matrix based on given
   --  quaternion.

   procedure Axis_To_Quat (A : Vector; Phi : Float; Q : out Quaternion);
   --  This function computes a quaternion based on an axis (defined by the
   --  given vector) and an angle about which to rotate.  The angle is
   --  expressed in radians.  The result is put into the third argument.

end Trackball;
