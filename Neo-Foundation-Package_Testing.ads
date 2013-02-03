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
  Neo.Foundational.Data_Types;
use
  Neo.Foundation.Text_IO,
  Neo.Foundation.Data_Types;
package Neo.Foundation.Package_Testing
  is
  -----------------
  -- Subprograms --
  -----------------
    procedure Test;
    procedure Put_Title(
      Title              : in String_2;
      Do_Show_Directions : in Boolean := False);
    procedure Hang_Window;
-------
private
-------
  ---------------
  -- Constants --
  ---------------
    TITLE_LENGTH         : constant Integer_4_Positive := 80;
    TESTING_EXIT_KEY     : constant String_2           := "Q";
    TESTING_SEPORATOR    : constant String_2           := "_";
    TESTING_INPUT_CURSOR : constant String_2           := ">> ";
    TESTING_INSTRUCTIONS : constant String_2           := "Send a " & CONSOLE_EXIT_KEY & " to continue.";
  end Neo.Foundation.Package_Testing;