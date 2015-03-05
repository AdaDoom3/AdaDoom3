with Ada.Strings;
with Ada.Finalization;             use Ada.Finalization;
with Ada.Strings.Wide_Fixed;       use Ada.Strings.Wide_Fixed;
with Ada.Characters.Handling;      use Ada.Characters.Handling;
with Ada.Wide_Characters.Handling; use Ada.Wide_Characters.Handling;
package Neo.Command is
  package Vector_String_2_Unbounded is new Vectors(String_2_Unbounded);
  Duplicate : Exception;
  Parse     : Exception;
  procedure Load        (Path : in String_2);
  procedure Handle      (Text : in String_2);
  function Autocomplete (Text : in String_2; Limit : in Integer_4_Positive := 1) return Array_String_2_Unbounded;
  generic
    Name                 : String_2;
    Description          : String_2;
    type Type_To_Vary is (<>);
    Initial              : Type_To_Vary := Type_To_Vary'first;
    Is_Saved             : Boolean      := False;
    Is_User_Settable     : Boolean      := True;
    Is_Server_Overridden : Boolean      := False;
    Adjust               : access function(Prevous, Current : in Type_To_Vary) return Type_To_Vary := null;
  package Variable is
      procedure Next;
      procedure Previous;
      procedure Set (value : in Float_4_Percent);
      procedure Set (Value : in Type_To_Vary);
      function Get  return Type_To_Vary;
    private
      type Record_Controller is new Controlled with null record;
      protected type Protected_Type_To_Vary is
          procedure Set (Value : in Type_To_Vary);
          function Get  return Type_To_Vary;
        private
          Current : Type_To_Vary := Initial;
        end Protected_Type_To_Vary;
      procedure Initialize (Controller : in out Record_Controller);
      procedure Finalize   (Controller : in out Record_Controller);
      procedure Handle_Set (Value : in String_2);
      function Handle_Get  return String_2;
      LOWER_NAME : constant String_2 := To_Lower(Name);
      Controller : Record_Controller;
      Data       : Protected_Type_To_Vary;
    end Variable;
  generic
    Name : String_2;
    with procedure Perform(Parameters : in Array_String_2_Unbounded);
  package Action is
    private
      type Record_Controller is new Controlled with null record;
      procedure Initialize              (Controller : in out Record_Controller);
      procedure Finalize                (Controller : in out Record_Controller);
      procedure Not_A_Formal_Subprogram (Parameters : in Array_String_2_Unbounded) renames Perform;
      LOWER_NAME : constant String_2 := To_Lower(Name);
      Controller : Record_Controller;
    end Action;
private
  CURRENT_VALUE                     : constant String_2           := "Current value: ";
  POSSIBLE_VALUES                   : constant String_2           := "Possible values: ";
  INCORRECT_PARAMETER               : constant String_2           := "Incorrect parameter for ";
  NO_SUCH_VARIABLE_OR_ACTION        : constant String_2           := "No such variable or action!";
  MAXIMUM_POSSIBLE_VALUES_DISPLAYED : constant Integer_4_Positive := 5;
  type Access_Function_Get      is access function return String_2;
  type Access_Procedure_Set     is access procedure(Item : in String_2);
  type Access_Procedure_Perform is access procedure(Parameters : in Array_String_2_Unbounded);
  type Record_Variable is record
      Saved_Value : String_2_Unbounded   := NULL_STRING_2_UNBOUNDED;
      Get         : Access_Function_Get  := null;
      Set         : Access_Procedure_Set := null;
    end record;
  package Hashed_Map_Record_Variable          is new Hashed_Maps(Record_Variable);
  package Hashed_Map_String_2_Unbounded       is new Hashed_Maps(String_2_Unbounded);
  package Hashed_Map_Access_Procedure_Perform is new Hashed_Maps(Access_Procedure_Perform);
  Names     : Hashed_Map_String_2_Unbounded.Protected_Map;
  Actions   : Hashed_Map_Access_Procedure_Perform.Protected_Map;
  Variables : Hashed_Map_Record_Variable.Protected_Map;
end Neo.Command;
