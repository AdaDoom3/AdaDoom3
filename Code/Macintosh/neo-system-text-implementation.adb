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
  Neo.Macintosh;
use
  Neo.Macintosh;
separate(Neo.System.Text)
package body Implementation
  is
  -------------------
  -- Set_Clipboard --
  -------------------
    procedure Set_Clipboard(
      Text : in String_2)
      is
      begin
        raise System_Call_Failure;
      end Set_Clipboard;
  -------------------
  -- Get_Clipboard --
  -------------------
    function Get_Clipboard
      return String_2
      is
      begin
        raise System_Call_Failure;
        return NULL_STRING_2;
      end Get_Clipboard;
  end Implementation;
