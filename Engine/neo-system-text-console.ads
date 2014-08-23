with Neo.Command;             use Neo.Command;
with Ada.Streams.Stream_IO;   use Ada.Streams.Stream_IO;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
package Neo.System.Text.Console is
  procedure Initialize;
  procedure Finalize;
  procedure Send_Log;
  procedure Save_Log;
  function Is_Running return Boolean;
private
  type Record_Button is record
      Message : String_2(1..4)   := (others => NULL_CHARACTER_2);
      Action  : Access_Procedure := null;
    end record;
  NUMBER_OF_OUTPUT_ROWS  : constant Integer_4_Unsigned_C         := 25;
  COLOR_BACKGROUND       : constant Record_Color                 := COLOR_NAVY_BLUE;
  COLOR_TEXT             : constant Record_Color                 := COLOR_YELLOW;
  ERROR_REPORTING_URL    : constant String_2                     := "http://www.google.com";
  LABEL_OUTPUT           : constant String_2                     := "Output";
  LABEL_ERROR            : constant String_2                     := "Error";
  LABEL_INPUT_ENTRY      : constant String_2                     := "Input";
  NAME_POSTFIX           : constant String_2                     := " Console";
  FAILED_BROWSE_FOR_SAVE : constant String_2                     := "Failed to browse for save!";
  FAILED_INITIALIZE      : constant String_2                     := "Failed to initialize console!";
  FAILED_ALREADY_OPEN    : constant String_2                     := "Failed to initialize console because it is open!";
  FAILED_NOT_OPEN        : constant String_2                     := "Failed to finalize console because it is not open!";
  CONSOLE_BUTTONS        : constant array(1..3) of Record_Button := (("Save", Save_Log'access), ("Send", Send_Log'access), ("Quit", null));
  procedure Run;
  package Task_Main is new Tasks(Run);
  Main_Task : Task_Main.Protected_Task;
  package Import is procedure Run; end Import;
end Neo.System.Text.Console;
