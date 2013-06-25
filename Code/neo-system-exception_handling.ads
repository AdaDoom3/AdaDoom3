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
  Ada.Unchecked_Deallocation,
  Neo.Foundation.Output,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Package_Testing,
  Neo.System.Text;
use
  Neo.Foundation.Output,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Package_Testing,
  Neo.System.Text;
package Neo.System.Exception_Handling
  is
  ----------------
  -- Exceptions --
  ----------------
    Empty_Is_Okay_Message : Exception;
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
    procedure Spawn_Console(
      Icon_Path : in String_2 := NULL_STRING_2;
      Title     : in String_2 := NAME & L(" Console"));
    procedure Set_Alert(
      Status : in Boolean);
    function Is_Alerting
      return Boolean;
    function Is_Okay(
      Title   : in String_2;
      Message : in String_2;
      Buttons : in Enumerated_Buttons := Okay_Button;
      Icon    : in Enumerated_Icon    := No_Icon)
      return Boolean;
-------
private
-------
  -----------
  -- Tasks --
  -----------
    task type Task_Console
      is
        entry Initialize(
          Icon_Path : in String_2;
          Title     : in String_2);
      end Task_Console;
  ---------------
  -- Accessors --
  ---------------
    type Access_Task_Console
      is access all Task_Console;
  -------------
  -- Records --
  -------------
    type Record_Button
      is record
        Message : String_2(1..4)   := (others => NULL_CHARACTER_2);
        Action  : Access_Procedure := null;
      end record;
  -----------------
  -- Subprograms --
  -----------------
    procedure Finalize
      is new Ada.Unchecked_Deallocation(Task_Console, Access_Task_Console);
    procedure Send_Catalog;
    procedure Save_Catalog;
  ---------------
  -- Constants --
  ---------------
    FAILED_IS_OKAY                 : constant String_2                     := "Failed is okay!";
    FAILED_SET_ALERT               : constant String_2                     := "Alert! Failed to start system alert!";
    FAILED_SPAWN_CONSOLE           : constant String_2                     := "Failed to spawn console!";
    FAILED_CONSOLE_ALREADY_SPAWNED : constant String_2                     := "Failed to spawn console because it is already open!";
    CONSOLE_BUTTONS                : constant array(1..3) of Record_Button :=(
      ("Send", Send_Catalog'access),
      ("Save", Save_Catalog'access),
      ("Quit", null)); -- A null Access_Procedure creates an exit button
  ---------------
  -- Variables --
  ---------------
    Console      : Access_Task_Console;
    Alert_Status : Protected_Status;
  --------------------
  -- Implementation --
  --------------------
    package Implementation
      is
        procedure Run_Console(
          Icon_Path : in String_2;
          Title     : in String_2);
        procedure Set_Alert(
          Status : in Boolean);
        function Is_Okay(
          Title   : in String_2;
          Message : in String_2;
          Buttons : in Enumerated_Buttons;
          Icon    : in Enumerated_Icon)
          return Boolean;
      end Implementation;
  end Neo.System.Exception_Handling;
