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
package body Neo.System.Text
  is
  --------------------
  -- Implementation --
  --------------------
    package body Implementation 
      is separate;
  ----------
  -- Test --
  ----------
    procedure Test
      is
      begin
        Display_Title("CHARACTER TEST!");
        Put_Line(To_Lower("ABCD"));
        Hang_Window;
      end Test;
  -----------------------
  -- Create_Media_Name --
  -----------------------
    function Create_Media_Name(
      Item : in String_2)
      return String_2
      is
      begin
        return "";
      end Create_Media_Name;
  -------------------
  -- Get_Clipboard --
  -------------------
    function Get_Clipboard
      return String_2
      renames Implementation.Get_Clipboard;
  -------------------
  -- Set_Clipboard --
  -------------------
    procedure Set_Clipboard(
      Text : in String_2)
      renames Implementation.Set_Clipboard;
  end Neo.System.Text;
