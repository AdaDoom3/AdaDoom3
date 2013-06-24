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
        Put_Line("Language" & ": " & Enumerated_Language'Wide_Image(Get_Language));
        Put_Line(Get_Clipboard);
        Set_Clipboard("Yes");
        Put_Line("But does it work? " & Get_Clipboard & "!");
        Hang_Window;
      end Test;
  ------------------
  -- Get_Language --
  ------------------
    function Get_Language
      return Enumerated_Language
      is
      begin
        return Implementation.Get_Language;
      exception
        when System_Call_Failure =>
          return English_Language;
      end Get_Language;
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
      is
      begin
        Implementation.Set_Clipboard(Text);
      exception
        when System_Call_Failure =>
          null;
      end Set_Clipboard;
end Neo.System.Text;
