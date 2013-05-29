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
  -- Spawn_Console --
  --------------------------
    procedure Spawn_Console(
      Title     : in String_2;
      Text      : in String_2;
      Buttons   : in Array_Record_Console_Button;
      Icon_Path : in String_2 := NULL_STRING_2)
      is
      begin
        raise System_Call_Failure;
      end Spawn_Console;
  -------------
  -- Is_Okay --
  -------------
    function Is_Okay(
      Title   : in String_2;
      Message : in String_2;
      Buttons : in Enumerated_Buttons;
      Icon    : in Enumerated_Icon)
      return Boolean
      is
      begin
        raise System_Call_Failure;
        return False;
      end Is_Okay;
  end Implementation;
