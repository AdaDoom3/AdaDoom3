separate(Neo.System.Text) package body Import is
  procedure Set_Clipboard(Item : in String_2) is
    begin
      raise Unimplemented_Feature;  
    end Set_Clipboard;
  function Get_Clipboard return String_2 is
    begin
      raise Unimplemented_Feature;  
      return NULL_STRING_2;
    end Get_Clipboard;
end Import;
