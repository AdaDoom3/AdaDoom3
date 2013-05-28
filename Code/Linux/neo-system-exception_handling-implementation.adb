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
  ---------------
  -- Set_Alert --
  ---------------
    procedure Start_Alert
      is
      begin
        raise Unsupported_Feature;
        null;
      end Start_Alert;
  ----------------
  -- Stop_Alert --
  ----------------
    procedure Stop_Alert
      is
      begin
        raise Unsupported_Feature;
        null;
      end Stop_Alert;
  -------------------
  -- Spawn_Console --
  -------------------
    procedure Spawn_Console(
      Title     : in String_2;
      Text      : in String_2;
      Buttons   : in Array_Record_Console_Button;
      Icon_Path : in String_2 := NULL_STRING_2)
      is
      begin
        raise Unsupported_Feature;
        null;
      end Spawn_Console;
  -------------
  -- Is_Okay --
  -------------
    function Is_Okay(
      Title        : in String_2;
      Message      : in String_2;
      Buttons      : in Enumerated_Buttons;
      Icon         : in Enumerated_Icon)
      return Boolean
      is
      begin
        raise Unsupported_Feature;
        return False;
      end Is_Okay;
  end Implementation;
