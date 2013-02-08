--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
with
  Neo.Library.Vectors,
  Neo.Library.Matrices,
  Neo.Library.Rotations,
  Neo.Library.Quaternion,
  Neo.Foundation.Text_IO,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Package_Testing;
use
  Neo.Library.Vectors,
  Neo.Library.Matrices,
  Neo.Library.Rotations,
  Neo.Library.Quaternion,
  Neo.Foundation.Text_IO,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Package_Testing;
package Neo.Library.Angles
  is
  -------------
  -- Records --
  -------------
    type Record_Angle
      is record
        Pitch : Float_4_Real := 0.0;
        Yaw   : Float_4_Real := 0.0;
        Roll  : Float_4_Real := 0.0;
      end record;
  -----------------
  -- Subprograms --
  -----------------
    function To_Record_Vector_Polar(
      Item : in Record_Vector_3)
      return Record_Vector_Polar;
    function To_Record_Vector_2(
      Item : in Record_Vector_)
      return Record_Vector_2;
    function To_Record_Vector_3(
      Item : in Record_Vector_)
      return Record_Vector_3;
    procedure Test;
    procedure Put(
      Item : in Record_Angle);
    function "-"(
      Left  : in Record_Angle;
      Right : in Record_Angle)
      return Boolean;
    function "-"(
      Item : in Record_Angle)
      return Boolean;
    function "+"(
      Left  : in Record_Angle;
      Right : in Record_Angle)
      return Boolean;
    function "+"(
      Item : in Record_Angle)
      return Boolean;
    function "*"(
      Left  : in Record_Angle;
      Right : in Record_Angle)
      return Boolean;
    function "/"(
      Left  : in Record_Angle;
      Right : in Record_Angle)
      return Boolean;
    function "="(
      Left  : in Record_Angle;
      Right : in Record_Angle)
      return Boolean;
    function Compare(
      Item_A : in Record_Angle;
      Item_B : in Record_Angle;
      ε      : in Float_4_Real)
      return Boolean;
    function Normalize(
      Item   : in Record_Angle;
      Degree : in Float_4_Degree)
      return Record_Angle;
    procedure Normalize(
      Item   : in out Record_Angle;
      Degree : in     Float_4_Degree);
    function Clamp(
      Item    : in Record_Angle;
      Minimum : in Record_Angle;
      Maximum : in Record_Angle)
      return Record_Angle;
    procedure Clamp(
      Item    : in out Record_Angle;
      Minimum : in     Record_Angle;
      Maximum : in     Record_Angle);
    function To_Yaw(
      Item : in Record_Vector_3)
      return Float_4_Real;
    function To_Pitch(
      Item : in Record_Vector_3)
      return Float_4_Real;
    function To_Record_Angle(
      Item : in Record_Vector_3)
      return Record_Angle;
    function To_Record_Vector_3(
      ToVectors( idVec3 *forward, idVec3 *right = NULL, idVec3 *up = NULL ) const;
    function To_Record_Vector_3_Forward(
      Item : in Record_Angle)
      return Record_Vector_3;
    function To_Record_Vector_3_Angular_Velocity(
      Item : in Record_Angle)
      return Record_Vector_3;
    function To_Record_Quaternion(
      Item : in Record_Angle)
      return Record_Quaternion;
    function To_Record_Rotation(
      Item : in Record_Angle)
      return Record_Rotation;
    function To_Record_Matrix_3
      Item : in Record_Angle)
      return Record_Matrix_3;
    function To_Record_Matrix_4
      Item : in Record_Angle)
      return Record_Matrix_3;
  end Neo.Library.Angles;
