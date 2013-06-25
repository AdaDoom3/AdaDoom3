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
  Ada.Strings.Wide_Unbounded,
  Ada.Text_IO,
  Ada.Wide_Text_IO,
  Ada.Exceptions,
  Ada.Strings.Fixed,
  Ada.Unchecked_Conversion,
  Neo.Foundation.Data_Types;
use
  Ada.Strings,
  Ada.Strings.Fixed,
  Ada.Exceptions,
  Neo.Foundation.Data_Types;
package Neo.Foundation.Output
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
  -----------------
  -- Subprograms --
  -----------------
    procedure Finalize;
    procedure Test;
    procedure Set_Put_Debug(
      Do_Put_Debug : in Boolean);
    procedure Set_Put(
      Do_Put : in Boolean);
    procedure Put(
      Item : in Character_2);
    procedure Put(
      Item : in String_2);
    procedure Put_Line(
      Item : in String_2);
    procedure Put_Debug(
      Item : in Character_2);
    procedure Put_Debug(
      Item : in String_2);
    procedure Put_Debug_Line(
      Item : in String_2);
    function Get_Line_Size
      return Integer_4_Positive;
    function Get_Catalog
      return String_2;
    function Get_Catalog_Path
      return String_2;
    procedure Set_Catalog_Path(
      Path : in String_2); -- Send "" or NULL_STRING_2 to disable
    procedure Set_Line_Size(
      Line_Size : in Integer_4_Positive);
    procedure Set_Localize(
      Localize : in Access_Subprogram_Localize);
    procedure Set_Put(
      Put : in Access_Subprogram_Put);
    procedure New_Line(
      Lines : in Integer_4_Positive := 1);
    generic
      type Type_Number
        is mod <>;
    function To_Radian_Image(
      Item    : in Type_Number;
      Base    : in Ada.Wide_Text_IO.Number_Base;
      Spacing : in Integer_4_Natural := 0) -- To do yet
      return String_2;
    function Localize(
      Item : in String_2)
      return String_2;
    function L(
      Item : in String_2)
      return String_2
      renames Localize;
-------
private
-------
  ---------------
  -- Constants -- The localization failure strings here should be the only untranslated strings in the program
  ---------------
    DO_PUT_LOCALIZE_FAILURE        : constant Boolean            := False;
    FAILED_GET_CATALOG             : constant String_2           := "Attempted to call Get_Catalog when no catalog was created!";
    FAILED_SET_CATALOG_PATH        : constant String_2           := "Failed to create catalog at path: ";
    FAILED_LOCALIZE_PREFIX         : constant String_2           := "Tried to localize string ";
    FAILED_LOCALIZE_POSTFIX        : constant String_2           := "... without having a Localize function set!";
    FAILED_LOCALIZE_PREVIEW_LENGTH : constant Integer_4_Positive := 10;
    RADIAN_IMAGE_STRING_SIZE       : constant Integer_4_Positive := 256;
    DEFAULT_LINE_SIZE              : constant Integer_4_Positive := 80;
  ---------------
  -- Protected --
  ---------------
    protected type Protected_Input_Output
      is
        entry Finalize;
        entry Set_Catalog_Path(
          Path : in String_2);
        entry Put(
          Item : in String_2);
        entry Get_Catalog(
          Item : in out String_2_Unbounded);
        function Localize(
          Item : in String_2)
          return String_2;
        function Do_Put_Debug
          return Boolean;
        function Get_Line_Size
          return Integer_4_Positive;
        function Get_Catalog_Path
          return String_2;
        procedure Set_Put_Debug(
          Do_Put_Debug : in Boolean);
        procedure Set_Put(
          Do_Put : in Boolean);
        procedure Set_Line_Size(
          Line_Size : in Integer_4_Positive);
        procedure Set_Localize(
          Localize : in Access_Subprogram_Localize);
        procedure Set_Put(
          Put : in Access_Subprogram_Put);
      private
        Current_Catalog_Path : String_2_Unbounded         := Ada.Strings.Wide_Unbounded.NULL_UNBOUNDED_WIDE_STRING;
        Current_Put          : Access_Subprogram_Put      := Ada.Wide_Text_IO.Put'access;
        Current_Localize     : Access_Subprogram_Localize := null;
        Current_Line_Size    : Integer_4_Positive         := DEFAULT_LINE_SIZE;
        Current_Do_Put_Debug : Boolean                    := False;
        Current_Do_Put       : Boolean                    := True;
        Is_Working_With_File : Boolean                    := False;
      end Protected_Input_Output;
  ---------------
  -- Variables --
  ---------------
    Input_Output : Protected_Input_Output;
    Catalog      : Ada.Wide_Text_IO.File_Type; -- Should not be used anywhere besides Protected_Input_Output
  end Neo.Foundation.Output;


