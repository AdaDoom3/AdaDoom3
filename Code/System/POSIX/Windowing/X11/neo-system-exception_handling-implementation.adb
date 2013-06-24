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
  Neo.Linux;
use
  Neo.Linux;
separate(Neo.System.Exception_Handling)
package body Implementation
  is
  -----------------
  -- Start_Alert --
  -----------------
    procedure Start_Alert
      is
      begin
        raise System_Call_Failure;
      end Start_Alert;
  ----------------
  -- Stop_Alert --
  ----------------
    procedure Stop_Alert
      is
      begin
        raise System_Call_Failure;
      end Stop_Alert;
  --------------------------
  -- Create_Error_Console --
  --------------------------
    procedure Create_Error_Console(
      Text    : in String_2;
      Buttons : in Array_Console_Buttons)
      is
      begin
        raise System_Call_Failure;
      end Create_Error_Console;
  -------------
  -- Is_Okay --
  -------------
    function Is_Okay(
      Title        : in String_2;
      Message      : in String_2;
      Buttons      : in Enumerated_Buttons;
      Icon         : in Enumerated_Icon;
      Parent_Title : in String_2 := NULL_STRING_2)
      return Boolean
      is
      begin
        raise System_Call_Failure;
        return False;
      end Is_Okay;
  end Implementation;
