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
        Put_Title("TEXT TEST");
        --Put_Line(Create_Media_Name("C:\Program Files\AdaDoom3\main.exe"));
        --Put_Line(Create_Media_Name("/usr/bin/asdf/adadoom3/main.?"));
        --Put_Line(Create_Media_Name(":usr:home:asdf:adadoom3:main"));
        Put_Line(Get_Clipboard);
        Set_Clipboard("Yes");
        Put_Line("But does it work? " & Get_Clipboard & "!");
        Hang_Window;
      end Test;
  -------------------
  -- Get_Clipboard --
  -------------------
    function Get_Clipboard
      return String_2
      is
      begin
        return Implementation.Get_Clipboard;
      exception
        when System_Call_Failure =>
          return NULL_STRING_2;
      end Get_Clipboard;
  -------------------
  -- Set_Clipboard --
  -------------------
    procedure Set_Clipboard(
      Text : in String_2)
      renames Implementation.Set_Clipboard;
  end Neo.System.Text;
