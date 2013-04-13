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
  Neo.Library.Mathmatics,
  Neo.Foundation.Text_IO,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Package_Testing;
use
  Neo.Library.Vectors,
  Neo.Library.Mathmatics,
  Neo.Foundation.Text_IO,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Package_Testing;
package Neo.Library.Plucker_Coordinates
  is
  -------------
  -- Records --
  -------------
    type Record_Plucker_Coordinate
      is record
        X : Record_Vector_3;
        Y : Record_Vector_3;
      end record;
  -----------------
  -- Subprograms --
  -----------------
    procedure Test;
    procedure Put(
      Item : in Record_Plucker_COordinate);
    function To_Record_Plucker_Coordinate_From_Line(
      Beginning : in Record_Vector_3;
      Ending    : in Record_Vector_3)
      return Record_Plucker_Coordinate;
    function To_Record_Plucker_Coordinate_From_Ray(
      Beginning : in Record_Vector_3;
      Ending    : in Record_Vector_3)
      return Record_Plucker_Coordinate;
    function Get_Length(
      Item : in Record_Plucker_Coordinate)
      return Float_4_Real;
    function Get_Length_Squared(
      Item : in Record_Plucker_Coordinate)
      return Float_4_Real;
    function Get_Permuted_Inner_Product(
      Item_A : in Record_Plucker_Coordinate;
      Item_B : in Record_Plucker_Coordinate)
      return Float_4_Real;
    function Get_Square_Of_Shortest_Distance_Between_3D_Lines(
      Item_A : in Record_Plucker_Coordinate;
      Item_B : in Record_Plucker_Coordinate)
      return Float_4_Real;
    function Normalize(
      Item : in Record_Plucker_Coordinate)
      return Record_Plucker_Coordinate;
    function Are_Equal(
      Item_A : in Record_Plucker_Coordinate;
      Item_B : in Record_Plucker_Coordinate;
      ε      : in Float_4_Real)
      return Boolean;
    function "-"(
      Item : in Record_Plucker_Coordinate)
      return Record_Plucker_Coordinate;
    function "-"(
      Left  : in Record_Plucker_Coordinate;
      Right : in Record_Plucker_Coordinate)
      return Record_Plucker_Coordinate;
    function "+"(
      Item : in Record_Plucker_Coordinate)
      return Record_Plucker_Coordinate;
    function "+"(
      Left  : in Record_Plucker_Coordinate;
      Right : in Record_Plucker_Coordinate)
      return Record_Plucker_Coordinate;
    function "*"(
      Left  : in Record_Plucker_Coordinate;
      Right : in Record_Plucker_Coordinate)
      return Float_4_Real;
    function "*"(
      Left  : in Record_Plucker_Coordinate;
      Right : in Float_4_Real)
      return Record_Plucker_Coordinate;
    function "*"(
      Left  : in Float_4_Real;
      Right : in Record_Plucker_Coordinate)
      return Record_Plucker_Coordinate;
    function "/"(
      Left  : in Record_Plucker_Coordinate;
      Right : in Float_4_Real)
      return Record_Plucker_Coordinate;
    function "/"(
      Left  : in Float_4_Real;
      Right : in Record_Plucker_Coordinate)
      return Record_Plucker_Coordinate;
    function "="(
      Left  : in Record_Plucker_Coordinate;
      Right : in Record_Plucker_Coordinate)
      return Boolean;
  end Neo.Library.Plucker_Coordinates;
