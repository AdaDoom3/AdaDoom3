package Neo.System.Text is
  function Get_Clipboard return String_2;
  procedure Set_Clipboard(Item : in String_2);
private
  FAILED_GET_CLIPBOARD : constant String_2 := "Failed get clipboard!";
  FAILED_SET_CLIPBOARD : constant String_2 := "Failed to set clipboard!";
  package Import is
      procedure Set_Clipboard(Item : in String_2);
      function Get_Clipboard return String_2;
    end Import;
end Neo.System.Text;
