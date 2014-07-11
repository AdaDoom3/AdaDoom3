package body Neo.System.Text is
    package body Import is separate;
    procedure Test is
      begin
        Put_Title(Localize("TEXT TEST"));
        Put_Line(Get_Clipboard);
        Set_Clipboard(Localize("Yes"));
        Put_Line(Localize("But does it work? ") & Get_Clipboard & "!");
      end Test;
    function Get_Clipboard return String_2 is
      begin
        return Import.Get_Clipboard;
      exception when Call_Failure =>
        Put_Debug_Line(Localize(FAILED_GET_CLIPBOARD));
        return NULL_STRING_2;
      end Get_Clipboard;
    procedure Set_Clipboard(Item : in String_2) is
      begin
        Import.Set_Clipboard(Item);
      exception when Call_Failure => Put_Debug_Line(Localize(FAILED_SET_CLIPBOARD));
      end Set_Clipboard;
  end Neo.System.Text;
