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
  ----------------
  -- Exceptions --
  ----------------
    Alert_Started_Before_Stop   : Exception;
    Alert_Stopped_Without_Start : Exception;
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
  -----------------
  -- Subprograms --
  -----------------
    procedure Test;
    procedure Start_Alert;
    procedure Stop_Alert;
    procedure Create_Error_Console(
      Text    : in String_2;
      Buttons : in Array_Console_Buttons);
    function Is_Okay(
      Title        : in String_2;
      Message      : in String_2;
      Buttons      : in Enumerated_Buttons;
      Icon         : in Enumerated_Icon;
      Parent_Title : in String_2 := NULL_STRING_2)
      return Boolean;
    function Is_Alerting
      return Boolean;
-------
private
-------
  ---------------
  -- Protected --
  ---------------
    protected type Protected_Alert_Status
      is
        function Is_Alerting
          return Boolean;
        procedure Set_Is_Alerting(
          New_Status : in Boolean);
      private
        Status : Boolean := False;
      end Protected_Alert_Status;
  ---------------
  -- Variables --
  ---------------
    Alert_Status : Protected_Alert_Status;
  --------------------
  -- Implementation --
  --------------------
    package Implementation
      is
        procedure Start_Alert;
        procedure Stop_Alert;
        procedure Create_Error_Console(
          Text    : in String_2;
          Buttons : in Array_Console_Buttons);
        function Is_Okay(
          Title        : in String_2;
          Message      : in String_2;
          Buttons      : in Enumerated_Buttons;
          Icon         : in Enumerated_Icon;
          Parent_Title : in String_2 := NULL_STRING_2)
          return Boolean;
      end Implementation;
  end Neo.System.Exception_Handling;
