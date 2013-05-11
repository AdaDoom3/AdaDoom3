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
  Ada.Strings,
  Ada.Text_IO,
  Ada.Wide_Text_IO,
  Ada.Strings.Fixed,
  Ada.Unchecked_Conversion,
  Neo.Foundation.Data_Types;
use
  Ada.Strings,
  Ada.Strings.Fixed,
  Neo.Foundation.Data_Types;
package Neo.Foundation.Text_IO
  is
  ---------------
  -- Accessors --
  ---------------
    type Access_Subprogram_Localize
      is access function(
        Item : in String_2)
        return String_2;
    type Access_Subprogram_Put
      is access procedure(
        Item : in String_2);
    type Access_Subprogram_Get_Line
      is access procedure(
        Item : out String_2;
        Last : out Integer_4_Natural);
    type Access_Subprogram_Skip_Line
      is access procedure(
        Spacing : in Positive_Count := 1);
  -----------------
  -- Subprograms --
  -----------------
    procedure Test;
    procedure Set_Debug_Printing(
      Do_Print_Debugging : in Boolean);
    procedure Set_Catalog_Path(
      Path : in String_2);
    procedure Set_Line_Size(
      Line_Size : in Integer_4_Positive);
    procedure Set_Localize(
      Localize : in Access_Subprogram_Localize);
    procedure Set_Put(
      Put : in Access_Subprogram_Put);
    procedure Set_Get_Line(
      Get_Line : in Access_Subprogram_Get_Line);
    procedure Set_Skip_Line(
      Skip_Line : in Access_Subprogram_Skip_Line);
    procedure Put(
      Item        : in Character_2;
      Do_Localize : in Boolean := False);
    procedure Put(
      Item        : in String_2;
      Do_Localize : in Boolean := False);
    procedure Put_Line(
      Item        : in String_2;
      Do_Localize : in Boolean := False);
    procedure Put_Debug(
      Item        : in Character_2;
      Do_Localize : in Boolean := False);
    procedure Put_Debug(
      Item        : in String_2;
      Do_Localize : in Boolean := False);
    procedure Put_Debug_Line(
      Item        : in String_2;
      Do_Localize : in Boolean := False);
    procedure Get(
      Item : in out Character_2);
    procedure Get_Line(
      Item : in out String_2;
      Last : in out Integer_4_Natural);
    function Get_Line_Size
      return Integer_4_Positive;
    procedure Skip_Line(
      Spacing : in Integer_4_Positive := 1);
    procedure New_Line(
      Lines : in Integer_4_Positive := 1);
    generic
      type Type_Number
        is mod <>;
    function To_Radian_Image(
      Item    : in Type_Number;
      Base    : in Number_Base;
      Spacing : in Integer_4_Positive)
      return String_2;
-------
private
-------
  ---------------
  -- Constants --
  ---------------
    WIDE_IMAGE_BUFFER_SIZE : constant Integer_4_Positive := 256;
    DEFAULT_LINE_SIZE      : constant Integer_4_Positive := 80;
  ---------------
  -- Protected --
  ---------------
    protected type Protected_Input_Output
      is

      private
        Line_Size    : Integer_4_Positive          := DEFAULT_LINE_SIZE;
        Catalog_Path : Access_String_2             := null;
        Localize     : Access_Subprogram_Localize  := null;
        Put          : Access_Subprogram_Put       := Ada.Wide_Text_IO.Put'Access;
        Get_Line     : Access_Subprogram_Get_Line  := Ada.Wide_Text_IO.Get_Line'Access;
        Skip_Line    : Access_Subprogram_Skip_Line := Ada.Wide_Text_IO.Skip_Line'Access;
      end Protected_Input_Output;
  ---------------
  -- Variables --
  ---------------
    Input_Output : Protected_Input_Output;
  end Neo.Foundation.Text_IO;


