--
--
--
--
--
--
--
-- This might be helpful
-- https://github.com/AdaDoom3/AdaDoom3/blob/testgenprotectremoval/Libraries/SDL/src/video/x11/SDL_x11clipboard.c
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
separate(Neo.System.Text)
package body Implementation
  is
  ------------------
  -- Get_Language --
  ------------------
    function Get_Language
      return Enumerated_Language
      is
      begin
        raise System_Call_Failure;
        return English_Language;
      end Get_Language;
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
