with Ada.Strings;
with Ada.Finalization;
with Ada.Locales;                  use Ada.Locales;
with Ada.Strings.Wide_Fixed;       use Ada.Strings.Wide_Fixed;
with Ada.Characters.Handling;      use Ada.Characters.Handling;
with Ada.Wide_Characters.Handling; use Ada.Wide_Characters.Handling;
package Neo.Command is
    Duplicate : Exception;
    Parse     : Exception;
    procedure Test;
    procedure Save_Variables (Path  : in String_2);
    procedure Load_Variables (Path  : in String_2);
    --procedure Load_Language  (Path  : in String_2; Do_Replace_Duplicates : in Boolean := True);
    procedure Handle         (Input : in String_2);
    function Autocomplete    (Input : in String_2; Limit : in Integer_4_Positive := 1) return Array_String_2_Unbounded;
    generic
      Name                 : String_2;
      Description          : String_2;
      type Type_To_Vary is (<>);
      Initial              : Type_To_Vary                                         := Type_To_Vary'first;
      Is_Saved             : Boolean                                              := False;
      Is_User_Settable     : Boolean                                              := True;
      Is_Server_Overridden : Boolean                                              := False;
      Adjust               : access procedure(Prevous, Current : in Type_To_Vary) := null;
    package Variable is
        procedure Set(Value : in Type_To_Vary);
        function Get return Type_To_Vary;
      private
        LOWER_NAME : constant String_2 := To_Lower(Name);
        type Record_Controller is new Ada.Finalization.Controlled with null record;
        overriding procedure Initialize (Controller : in out Record_Controller);
        overriding procedure Finalize   (Controller : in out Record_Controller);
        Controller : Record_Controller;
        procedure Handle_Set (Value : in String_2);
        function Handle_Get  return String_2;
        protected type Protected_Type_To_Vary is
            procedure Set (Item : in Type_To_Vary);
            function Get  return Type_To_Vary;
          private
            Current : Type_To_Vary := Initial;
          end Protected_Type_To_Vary;
        Data : Protected_Type_To_Vary;
      end Variable;
    generic
      Name : String_2;
      with procedure Perform(Parameters : in String_2);
    package Action is
      private
        LOWER_NAME : constant String_2 := To_Lower(Name);
        type Record_Controller is new Ada.Finalization.Controlled with null record;
        overriding procedure Initialize (Controller : in out Record_Controller);
        overriding procedure Finalize   (Controller : in out Record_Controller);
        Controller : Record_Controller;
        procedure Not_A_Formal_Subprogram(Parameters : in String_2) renames Perform;
      end Action;
private
    CURRENT_VALUE                     : constant String_2           := "Current value: ";
    POSSIBLE_VALUES                   : constant String_2           := "Possible values: ";
    INCORRECT_PARAMETER               : constant String_2           := "Incorrect parameter for ";
    VARIABLE_OUT_OF_SCOPE             : constant String_2           := "Variable out of scope: ";
    NO_SUCH_VARIABLE_OR_ACTION        : constant String_2           := "No such variable or action!";
    MAXIMUM_POSSIBLE_VALUES_DISPLAYED : constant Integer_4_Positive := 5;
    type Access_Function_Get      is access function return String_2;
    type Access_Procedure_Set     is access procedure(Item : in String_2);
    type Access_Procedure_Perform is access procedure(Item : in String_2);
    type Record_Variable is record
        Saved_Value : String_2_Unbounded   := NULL_STRING_2_UNBOUNDED;
        Get         : Access_Function_Get  := null;
        Set         : Access_Procedure_Set := null;
      end record;
    package Mapped_Variables is new Maps(Record_Variable);
    package Mapped_Actions   is new Maps(Access_Procedure_Perform);
    package Mapped_Names     is new Maps(String_2_Unbounded);
    Names     : Mapped_Names.Protected_Map;
    Actions   : Mapped_Actions.Protected_Map;
    Variables : Mapped_Variables.Protected_Map;
  end Neo.Command;
