with Neo.Command;             use Neo.Command;
with Ada.Wide_Text_IO;        use Ada.Wide_Text_IO;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
package Neo.System.Text.Console is
    procedure Test;
    procedure Initialize;
    procedure Finalize;
    function Is_Running return Boolean;
private
    type Record_Button is record
        Message : String_2(1..4)   := (others => NULL_CHARACTER_2); -- All button captions must be exactly 4 characters
        Action  : Access_Procedure := null;
      end record;
    procedure Run;
    procedure Send_Log;
    procedure Save_Log;
    procedure Copy_Log;
    NUMBER_OF_OUTPUT_ROWS  : constant Integer_4_Unsigned_C := 30;
    COLOR_BACKGROUND       : constant Record_Color         := COLOR_NAVY_BLUE;
    COLOR_TEXT             : constant Record_Color         := COLOR_YELLOW;
    ERROR_REPORTING_URL    : constant String_2             := "http://www.google.com";
    PATH_LOGS              : constant String_2             := To_String_2(SPECIFICS.Path) & "\Logs\";
    LABEL_OUTPUT           : constant String_2             := "Output";
    LABEL_ERROR            : constant String_2             := "Error";
    LABEL_INPUT_ENTRY      : constant String_2             := "Input";
    NAME_POSTFIX_CONSOLE   : constant String_2             := " Console";
    FAILED_BROWSE_FOR_SAVE : constant String_2             := "Failed to browse for save!";
    FAILED_SAVE_LOG        : constant String_2             := "Failed to save log!";
    FAILED_SEND_LOG        : constant String_2             := "Failed to send log!";
    FAILED_COPY_LOG        : constant String_2             := "Failed to copy log!";
    FAILED_INITIALIZE      : constant String_2             := "Failed to initialize console!";
    FAILED_ALREADY_OPEN    : constant String_2             := "Failed to initialize console because it is open!";
    FAILED_NOT_OPEN        : constant String_2             := "Failed to finalize console because it is not open!";
    CONSOLE_BUTTONS        : constant array(1..4) of Record_Button := -- A null procedure creates an exit button
      (("Copy", Copy_Log'access), ("Save", Save_Log'access), ("Send", Send_Log'access), ("Quit", null));
    package Console_Threads is new Threads(Run);
    Console : Console_Threads.Protected_Thread;
    generic
      with procedure Save_Log;
      with procedure Send_Log;
      with procedure Copy_Log;
    package Import is
        procedure Run;
      end Import;
  end Neo.System.Text.Console;
