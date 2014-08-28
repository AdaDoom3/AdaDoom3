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
  Neo.Archetype;
use
  Neo.Archetype;
separate(Neo.Command.System.Text)
package body Implementation
  is pragma Source_File_Name("neo-text-implementation.adb");
  ------------------
  -- Get_Language --
  ------------------
    function Get_Language
      return Enumerated_Language
      is
      begin
        raise Unimplemented_Feature;
      end Get_Language;
  -------------------
  -- Set_Clipboard --
  -------------------
    procedure Set_Clipboard(
      Text : in String_2)
      is
      begin
        raise Unimplemented_Feature;
      end Set_Clipboard;
  -------------------
  -- Get_Clipboard --
  -------------------
    function Get_Clipboard
      return String_2
      is
      begin
        raise Unimplemented_Feature;
      end Get_Clipboard;
  end Implementation;
