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
        Display_Title("TEXT TEST");
        Put_Line(Create_Media_Name("C:\Program Files\AdaDoom3\main.exe"));
        Put_Line(Create_Media_Name("/usr/bin/asdf/adadoom3/main.?"));
        Put_Line(Create_Media_Name(":usr:home:asdf:adadoom3:main"));
        Put_Line(Get_Clipboard);
        Set_Clipboard("Yes");
        Put_Line("But does it work? " & Get_Clipboard & "!");
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
