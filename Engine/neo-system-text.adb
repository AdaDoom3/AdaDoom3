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
  ------------
  -- Import --
  ------------
    package body Import
      is separate;
  ----------
  -- Test --
  ----------
    procedure Test
      is
      begin
        Put_Title(Localize("TEXT TEST"));
        Put_Line(Localize("Language: ") & Enumerated_Language'wide_image(LANGUAGE));
        Put_Line(Get_Clipboard);
        Set_Clipboard(Localize("Yes"));
        Put_Line(Localize("But does it work? ") & Get_Clipboard & "!");
      end Test;
  --------------
  -- Localize --
  --------------
  --  function Localize(
  --    Item : in String_2)
  --    return String_2
  --    is
  --    Result : String_2 := Input_Output.Localize(Item);
  --    begin
  --      if Result = NULL_STRING_2 then
  --        if DO_PUT_LOCALIZE_FAILURE then
  --          Put_Debug_Line(
  --            FAILED_LOCALIZE_PREFIX & Item(Item'first..(
  --              if Item'length >= FAILED_LOCALIZE_PREVIEW_LENGTH then
  --                Item'first + FAILED_LOCALIZE_PREVIEW_LENGTH - 1
  --              else
  --                Item'last)) &
  --            FAILED_LOCALIZE_POSTFIX);
  --        end if;
  --        return Item;
  --      end if;
  --      return Result;
  --   end Localize;
  ------------------
  -- Get_Language --
  ------------------
    function Get_Language
      return Enumerated_Language
      is
      begin
        return Import.Get_Language;
      exception
        when Call_Failure =>
          Put_Debug_Line(FAILED_GET_LANGUAGE); -- Can't localize this string
          return English_Language;
      end Get_Language;
  -------------------
  -- Get_Clipboard --
  -------------------
    function Get_Clipboard
      return String_2
      is
      begin
        return Import.Get_Clipboard;
      exception
        when Call_Failure =>
          Put_Debug_Line(Localize(FAILED_GET_CLIPBOARD));
          return NULL_STRING_2;
      end Get_Clipboard;
  -------------------
  -- Set_Clipboard --
  -------------------
    procedure Set_Clipboard(
      Text : in String_2)
      is
      begin
        Import.Set_Clipboard(Text);
      exception
        when Call_Failure =>
          Put_Debug_Line(Localize(FAILED_SET_CLIPBOARD));
          null;
      end Set_Clipboard;
end Neo.System.Text;
