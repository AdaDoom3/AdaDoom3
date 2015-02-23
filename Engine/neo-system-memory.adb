package body Neo.System.Memory is
  package body Import is separate;
  package body Manager is
      procedure Unlock (Item : in out Type_To_Manage) is begin Import.Unlock (Item'address, Item'size / Byte'size); exception when Call_Failure => Put_Debug_Line(Localize(FAILED_UNLOCK)); end Unlock;
      procedure Lock   (Item : in out Type_To_Manage) is begin Import.Lock   (Item'address, Item'size / Byte'size); exception when Call_Failure => Put_Debug_Line(Localize(FAILED_LOCK));   end Lock;
    end Manager;
  procedure Set_Byte_Limits (Minimum, Maximum : in Integer_8_Unsigned) is begin Import.Set_Byte_Limits(Minimum, Maximum); exception when Call_Failure => Put_Debug_Line(Localize(FAILED_SET_BYTE_LIMITS));                  end Set_Byte_Limits;
  function Get_State        return Record_State                        is begin return Import.Get_State;                  exception when Call_Failure => Put_Debug_Line(Localize(FAILED_GET_STATE)); return (others => <>); end Get_State;
begin
  Put_Title(Localize("MEMORY"));
  New_Line;
  Put_Line(Localize("Load:")                       & Float_4_Percent'Wide_Image(INITIAL_STATE.Load));
  Put_Line(Localize("Disk total:")                 & Integer_8_Unsigned'Wide_Image(INITIAL_STATE.Number_Of_Disk_Bytes_Total));
  Put_Line(Localize("Disk available:")             & Integer_8_Unsigned'Wide_Image(INITIAL_STATE.Number_Of_Disk_Bytes_Available));
  Put_Line(Localize("Physical total:")             & Integer_8_Unsigned'Wide_Image(INITIAL_STATE.Number_Of_Physical_Bytes_Total));
  Put_Line(Localize("Physical available:")         & Integer_8_Unsigned'Wide_Image(INITIAL_STATE.Number_Of_Physical_Bytes_Available));
  Put_Line(Localize("Page file total:")            & Integer_8_Unsigned'Wide_Image(INITIAL_STATE.Number_Of_Page_File_Bytes_Total));
  Put_Line(Localize("Page file available:")        & Integer_8_Unsigned'Wide_Image(INITIAL_STATE.Number_Of_Page_File_Bytes_Available));
  Put_Line(Localize("Virtual total:")              & Integer_8_Unsigned'Wide_Image(INITIAL_STATE.Number_Of_Virtual_Bytes_Total));
  Put_Line(Localize("Virtual available:")          & Integer_8_Unsigned'Wide_Image(INITIAL_STATE.Number_Of_Virtual_Bytes_Available));
  Put_Line(Localize("Virtual available extended:") & Integer_8_Unsigned'Wide_Image(INITIAL_STATE.Number_Of_Virtual_Bytes_Available_Extended));
  New_Line;
end Neo.System.Memory;
