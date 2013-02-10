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
  Neo.Foundation.Data_Types,
  Neo.Foundation.Generic_Protected;
use
  Ada.Strings,
  Ada.Strings.Fixed,
  Neo.Foundation.Data_Types;
package Neo.Foundation.Text_IO
  is
  -------------
  -- Numbers --
  -------------
    subtype Integer_Base
      is Ada.Text_IO.Number_Base;
    subtype Integer_Positive_Count
      is Ada.Wide_Text_IO.Positive_Count;
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
        Spacing : in Integer_Positive_Count := 1);
  -----------------
  -- Subprograms --
  -----------------
    procedure Test;
    procedure Set(
      Line_Size : in Integer_4_Positive;
      Localize  : in Access_Subprogram_Localize;
      Put       : in Access_Subprogram_Put;
      Get_Line  : in Access_Subprogram_Get_Line;
      Skip_Line : in Access_Subprogram_Skip_Line);
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
    function Wide_Image(
      Item : in Integer_8_Unsigned;
      Base : in Integer_Base)
      return String_2;
-------
private
-------
  ---------------
  -- Constants --
  ---------------
    WIDE_IMAGE_BUFFER_SIZE : constant Integer_4_Positive := 256;
    DEFAULT_LINE_SIZE      : constant Integer_4_Positive := 80;
  -----------------
  -- Subprograms --
  -----------------
    function Dummy_Localize(
      Item : in String_2)
      return String_2;
  -------------
  -- Records --
  -------------
    type Record_Input_Output
      is record
        Line_Size : Integer_4_Positive          := DEFAULT_LINE_SIZE;
        Localize  : Access_Subprogram_Localize  := null;
        Put       : Access_Subprogram_Put       := Ada.Wide_Text_IO.Put'Access;
        Get_Line  : Access_Subprogram_Get_Line  := Ada.Wide_Text_IO.Get_Line'Access;
        Skip_Line : Access_Subprogram_Skip_Line := Ada.Wide_Text_IO.Skip_Line'Access;
      end record;
  --------------
  -- Packages --
  --------------
    package Protected_Record_Input_Output
      is new Neo.Foundation.Generic_Protected(Record_Input_Output);
    package Integer_8_Unsigned_Text_IO
      is new Ada.Text_IO.Modular_IO(Integer_8_Unsigned);
  ---------------
  -- Variables --
  ---------------
    Protected_Data : Protected_Record_Input_Output.Data;
  end Neo.Foundation.Text_IO;



