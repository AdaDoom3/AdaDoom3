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
  Ada.Unchecked_Deallocation;
package Neo.System.Text.Console
  is
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
    procedure Spawn(
      Name : in String_2 := Localize("Console"))
      with pre => Name'length > 0;
    procedure Set_Alert(
      Status : in Boolean);
    function Is_Alerting
      return Boolean;
    function Is_Open
      return Boolean;
    function Is_Okay(
      Name    : in String_2;
      Message : in String_2;
      Buttons : in Enumerated_Buttons := Okay_Button;
      Icon    : in Enumerated_Icon    := No_Icon)
      return Boolean
      with pre => Name'length > 0 and Message'length > 0;
-------
private
-------
  -----------
  -- Tasks --
  -----------
    task type Task_Console
      is
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
    procedure Send_Log;
    procedure Save_Log;
    procedure Copy_Log;
  ---------------
  -- Constants --
  ---------------
    PREFERRED_LOG_HEIGHT_IN_LINES  : constant Integer_4_Unsigned_C         := 120;
    COLOR_BACKGROUND               : constant Record_Color                 := COLOR_NAVY_BLUE;
    COLOR_TEXT                     : constant Record_Color                 := COLOR_YELLOW;
    ERROR_REPORTING_URL            : constant String_2                     := "http://www.google.com";
    LABEL_LOG                      : constant String_2                     := "Log";
    LABEL_ERROR                    : constant String_2                     := "Error";
    LABEL_INPUT_ENTRY              : constant String_2                     := "Input";
    FAILED_IS_OKAY                 : constant String_2                     := "Failed is okay!";
    FAILED_SET_ALERT               : constant String_2                     := "Alert! Failed to start system alert!";
    FAILED_SPAWN_CONSOLE           : constant String_2                     := "Failed to spawn console!";
    FAILED_CONSOLE_ALREADY_SPAWNED : constant String_2                     := "Failed to spawn console because it is busy!";
    CONSOLE_BUTTONS                : constant array(1..4) of Record_Button :=(
      ("Copy", Copy_Log'access),
      ("Save", Save_Log'access),
      ("Send", Send_Log'access),
      ("Quit", null)); -- A null procedure access creates an exit button
  ---------------
  -- Protected --
  ---------------
    protected type Protected_Console
      is
        procedure Initialize;
        procedure Finalize;
        function Is_Open
          return Boolean;
      private
        Console : Access_Task_Console := null;
      end Protected_Console;
  ---------------
  -- Variables --
  ---------------
    Console      : Protected_Console;
    Alert_Status : Protected_Status;
  ------------
  -- Import --
  ------------
    package Import
      is
        procedure Run;
        procedure Set_Alert(
          Status : in Boolean);
        function Is_Okay(
          Name    : in String_2;
          Message : in String_2;
          Buttons : in Enumerated_Buttons;
          Icon    : in Enumerated_Icon)
          return Boolean;
      end Import;
  end Neo.System.Text.Console;
