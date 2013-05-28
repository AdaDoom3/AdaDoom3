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
  Neo.Foundation.Text_IO,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Package_Testing;
use
  Neo.Foundation.Text_IO,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Package_Testing;
package Neo.System.Exception_Handling
  is
  ---------------
  -- Constants --
  ---------------
    MAXIMUM_NUMBER_OF_CONSOLE_BUTTONS : Integer_4_Positive := 4;
  ----------------
  -- Exceptions --
  ----------------
    Too_Many_Buttons_For_Error_Console       : Exception;
    Attempted_To_Alert_Before_Window_Created : Exception;
    Alert_Started_Before_Stop                : Exception;
    Alert_Stopped_Without_Start              : Exception;
    Empty_Is_Okay_Message                    : Exception;
  ------------------
  -- Enumerations --
  ------------------
    type Enumerated_Icon
      is(
      No_Icon,
      Warning_Icon,
      Information_Icon,
      Error_Icon);
    type Enumerated_Buttons
      is(
      Yes_No_Buttons,
      Okay_Button,
      Okay_Cancel_Buttons,
      Retry_Cancel_Buttons);
  -------------
  -- Records --
  -------------
    type Record_Console_Button
      is record
        Caption : String_2(1..6)   := (others => NULL_CHARACTER_2);
        Action  : Access_Procedure := null;
      end record;
  ------------
  -- Arrays --
  ------------
    type Array_Record_Console_Button
      is array(Positive range <>)
      of Record_Console_Button;
  -----------------
  -- Subprograms --
  -----------------
    procedure Test;
    procedure Start_Alert;
    procedure Stop_Alert;
    procedure Spawn_Console(
      Title     : in String_2;
      Text      : in String_2;
      Buttons   : in Array_Record_Console_Button;
      Icon_Path : in String_2 := NULL_STRING_2);
    function Is_Okay(
      Title        : in String_2;
      Message      : in String_2;
      Buttons      : in Enumerated_Buttons := Okay_Button;
      Icon         : in Enumerated_Icon    := No_Icon)
      return Boolean;
    function Is_Alerting
      return Boolean;
-------
private
-------
  ---------------
  -- Variables --
  ---------------
    Alert_Status : Protected_Status;
  --------------------
  -- Implementation --
  --------------------
    package Implementation
      is
        procedure Start_Alert;
        procedure Stop_Alert;
        procedure Spawn_Console(
          Title     : in String_2;
          Text      : in String_2;
          Buttons   : in Array_Record_Console_Button;
          Icon_Path : in String_2 := NULL_STRING_2);
        function Is_Okay(
          Title        : in String_2;
          Message      : in String_2;
          Buttons      : in Enumerated_Buttons;
          Icon         : in Enumerated_Icon)
          return Boolean;
      end Implementation;
  end Neo.System.Exception_Handling;
