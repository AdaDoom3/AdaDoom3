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
  Interfaces,
  Ada.Strings,
  Ada.Wide_Text_IO,
  Ada.Strings.Wide_Fixed,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Generic_Protected;
use
  Interfaces,
  Ada.Strings,
  Ada.Wide_Text_IO,
  Ada.Strings.Wide_Fixed,
  Neo.Foundation.Data_Types;
package Neo.Foundation.Text_IO
  is
  ---------------
  -- Accessors --
  ---------------
    type Access_Function_Localize
      is access function(
        Item : in String_2)
        return String_2;
    type Access_Procedure_Put
      is access procedure(
        Item : in String_2);
    type Access_Function_Get_Line
      is access function
        return String_2;
  -----------------
  -- Subprograms --
  -----------------
    procedure Test;
    procedure Put(
      Item : in Character_1);
    procedure Put(
      Item : in Character_2);
    procedure Put(
      Item : in String_1);
    procedure Put(
      Item : in String_2);
    procedure Put_Line(
      Item : in Character_1);
    procedure Put_Line(
      Item : in Character_2);
    procedure Put_Line(
      Item : in String_1);
    procedure Put_Line(
      Item : in String_2);
    procedure Set(
      Localize  : in Access_Function_Localize;
      Put       : in Access_Procedure_Put;
      Get_Line  : in Access_Function_Get_Line);
    procedure Set_Localizer(
      Localizer : in Access_Function_Localizer);
    procedure Set_Put(
      New_Put : in Access_Put);
    procedure Set_Get_Line(
      New_Get_Line : in Access_Get_Line);
    function Get
      return Character_2;
    function Get_Line
      return String_2;
    procedure Get_Line;
    function Localize(
      Item : in String_2)
      return String_2;
    function Hexadecimal_Image(
      Item : in Integer_4_Unsigned)
      return String_2;
    function Hexadecimal_Image(
      Item : in Integer_2_Unsigned)
      return String_2;
    function Hexadecimal_Image(
      Item : in Integer_1_Unsigned)
      return String_2;
    function Binary_Image(
      Item             : in Integer_4_Unsigned;
      Do_Space_Nibbles : in Boolean := False)
      return String_2;
    function Binary_Image(
      Item             : in Integer_2_Unsigned;
      Do_Space_Nibbles : in Boolean := False)
      return String_2;
    function Binary_Image(
      Item             : in Integer_1_Unsigned;
      Do_Space_Nibbles : in Boolean := False)
      return String_2;
-------
private
-------
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
        Localize : Access_Function_Localize := Dummy_Localize'Access;
        Put      : Access_Procedure_Put     := Ada.Wide_Text_IO.Put'Access;
        Get_Line : Access_Function_Get_Line := Ada.Wide_Text_IO.Get_Line'Access;
      end record;
  --------------
  -- Packages --
  --------------
    package Integer_4_Signed_IO
      is new Ada.Wide_Text_IO.Integer_IO(Integer_4_Signed);
    package Protected_Record_Input_Output
      is new Foundational.Generic_Protected(Record_Input_Output);
  ---------------
  -- Variables --
  ---------------
    Protected_Data : Protected_Record_Input_Output.Data;
  end Neo.Foundation.Text_IO;
