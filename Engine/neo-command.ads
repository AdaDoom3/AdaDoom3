--
--
--
-- In Doom3, "cvars" (or console variables) are for user interaction via the in-game console and used globally
-- throughout the code. The variable package below defines a similar solution, although instead of wrapping the
-- "cvar" functionality in an object a package is used instead. This is because the type of the variable needs
-- to remain undefined.
--
-- Each varible package instantiation represents a single unique variable instance and can be set directly in
-- a task safe way via Get and Set. Optionally the variable package provides a callback procedure "Adjust"
-- which is called every time the variable is set.
--
-- Neo.Command.Handle is capable of setting any instantiated variable package with a string.
--
--
--
with
  Ada.Strings,
  Ada.Strings.Wide_Fixed,
  Ada.Finalization,
  Ada.Characters.Handling,
  Ada.Wide_Characters.Handling;
use
  Ada.Strings.Wide_Fixed,
  Ada.Characters.Handling,
  Ada.Wide_Characters.Handling;
package Neo.Command
  is
  ----------------
  -- Exceptions --
  ----------------
    Duplicate : Exception;
  -----------------
  -- Subprograms --
  -----------------
    procedure Test;
    procedure Load_Variables(
      Path : in String_2);
    procedure Save_Variables(
      Path : in String_2);
    function Autocomplete(
      Input : in String_2;
      Limit : in Integer_4_Positive := 1)
      return Array_String_2_Unbounded;
    procedure Handle( -- Takes a command followed by parameters, or a variable set (syntax "name new_value")
      Input : in String_2)
      with pre => Trim(Input, Both)'length > 0;
  --------------
  -- Packages --
  --------------
    generic
      type Type_To_Vary
        is (<>);
      Initial              : Type_To_Vary;
      Name                 : String_2;
      Description          : String_2 := "No description...";
      Is_Saved             : Boolean  := True;
      Is_User_Settable     : Boolean  := True;
      Is_Server_Overridden : Boolean  := False;
      Adjust : access procedure(
        Prevous : in Type_To_Vary;
        Current : in Type_To_Vary) := null;
    package Variable
      is
        procedure Set(
          Value : in Type_To_Vary);
        function Get
          return Type_To_Vary;
      private
        LOWER_NAME : constant String_2 := To_Lower(Name);
        type Record_Controller
          is new Ada.Finalization.Controlled
          with null record;
        protected type Protected_Type_To_Vary
          is
            function Get
              return Type_To_Vary;
            procedure Set(
              Item : in Type_To_Vary);
          private
            Current : Type_To_Vary := Initial;
          end Protected_Type_To_Vary;
        function Handle_Get
          return String_2;
        procedure Handle_Set(
          Value : in String_2);
        overriding procedure Initialize(
          Controller : in out Record_Controller);
        overriding procedure Finalize(
          Controller : in out Record_Controller);
        Data       : Protected_Type_To_Vary;
        Controller : Record_Controller;
      end Variable;
    generic
      with
        procedure Perform(
          Parameters : in String_2);
      Name : String_2;
    package Action
      is
      private
        LOWER_NAME : constant String_2 := To_Lower(Name);
        type Record_Controller
          is new Ada.Finalization.Controlled
          with null record;
        procedure Not_A_Formal_Subprogram(
          Parameters : in String_2)
          renames Perform;
        overriding procedure Initialize(
          Controller : in out Record_Controller);
        overriding procedure Finalize(
          Controller : in out Record_Controller);
        Controller : Record_Controller;
      end Action;
-------
private
-------
  ---------------
  -- Constants --
  ---------------
    MAXIMUM_POSSIBLE_VALUES_DISPLAYED : constant Integer_4_Positive := 5;
    CURRENT_VALUE                     : constant String_2           := "Current value: ";
    POSSIBLE_VALUES                   : constant String_2           := "Possible values: ";
    INCORRECT_PARAMETER               : constant String_2           := "Incorrect parameter for ";
    NO_SUCH_VARIABLE_OR_COMMAND       : constant String_2           := "No such variable or command!";
  ---------------
  -- Accessors --
  ---------------
    type Access_Function_Get
      is access function
        return String_2;
    type Access_Procedure_Set
      is access procedure(
        Item : in String_2);
    type Access_Procedure_Perform
      is access procedure(
        Item : in String_2);
  -------------
  -- Records --
  -------------
    type Record_Variable
      is record
        Saved_Value : String_2_Unbounded   := NULL_STRING_2_UNBOUNDED;
        Get         : Access_Function_Get  := null;
        Set         : Access_Procedure_Set := null;
      end record;
  --------------
  -- Packages --
  --------------
    package Hashed_Action
      is new Hashed_Maps(Access_Procedure_Perform);
    package Hashed_Variable
      is new Hashed_Maps(Record_Variable);
  ---------------
  -- Variables --
  ---------------
    Actions   : Hashed_Action.Protected_Map;
    Variables : Hashed_Variable.Protected_Map;
  end Neo.Command;
