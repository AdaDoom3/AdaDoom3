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
package body Neo.System.Memory
  is
  --------------------
  -- Implementation --
  --------------------
    package body Implementation
      is separate;
  -------------
  -- Manager --
  -------------
    package body Manager
      is
        procedure Lock(
          Item : in out Type_To_Manage)
          is
          begin
            Implementation.Lock(Item'Address, Item'Size / Byte'Size);
          exception
            when System_Call_Failure =>
              null;
          end Lock;
        procedure Unlock(
          Item : in out Type_To_Manage)
          is
          begin
            Implementation.Unlock(Item'Address, Item'Size / Byte'Size);
          exception
            when System_Call_Failure =>
              null;
          end Unlock;
      end Manager;
  ----------
  -- Test --
  ----------
    procedure Test
      is
      State : Record_State := (others => <>); -- Get_State is called after for Put_Title, instead of here, for testing purposes
      begin
        Put_Title("MEMORY TEST");
        State := Get_State_At_Launch;
        Put_Line("State at launch: ");
        Put_Line("Load: "                       & Float_4_Percent'Wide_Image(State.Load));
        Put_Line("Disk total: "                 & Integer_8_Unsigned'Wide_Image(State.Number_Of_Disk_Bytes_Total));
        Put_Line("Disk available: "             & Integer_8_Unsigned'Wide_Image(State.Number_Of_Disk_Bytes_Available));
        Put_Line("Physical total: "             & Integer_8_Unsigned'Wide_Image(State.Number_Of_Physical_Bytes_Total));
        Put_Line("Physical available: "         & Integer_8_Unsigned'Wide_Image(State.Number_Of_Physical_Bytes_Available));
        Put_Line("Page file total: "            & Integer_8_Unsigned'Wide_Image(State.Number_Of_Page_File_Bytes_Total));
        Put_Line("Page file available: "        & Integer_8_Unsigned'Wide_Image(State.Number_Of_Page_File_Bytes_Available));
        Put_Line("Virtual total: "              & Integer_8_Unsigned'Wide_Image(State.Number_Of_Virtual_Bytes_Total));
        Put_Line("Virtual available: "          & Integer_8_Unsigned'Wide_Image(State.Number_Of_Virtual_Bytes_Available));
        Put_Line("Virtual available extended: " & Integer_8_Unsigned'Wide_Image(State.Number_Of_Virtual_Bytes_Available_Extended));
        New_Line;
        State := Get_State;
        Put_Line("State currently: ");
        Put_Line("Load: "                       & Float_4_Percent'Wide_Image(State.Load));
        Put_Line("Disk total: "                 & Integer_8_Unsigned'Wide_Image(State.Number_Of_Disk_Bytes_Total));
        Put_Line("Disk available: "             & Integer_8_Unsigned'Wide_Image(State.Number_Of_Disk_Bytes_Available));
        Put_Line("Physical total: "             & Integer_8_Unsigned'Wide_Image(State.Number_Of_Physical_Bytes_Total));
        Put_Line("Physical available: "         & Integer_8_Unsigned'Wide_Image(State.Number_Of_Physical_Bytes_Available));
        Put_Line("Page file total: "            & Integer_8_Unsigned'Wide_Image(State.Number_Of_Page_File_Bytes_Total));
        Put_Line("Page file available: "        & Integer_8_Unsigned'Wide_Image(State.Number_Of_Page_File_Bytes_Available));
        Put_Line("Virtual total: "              & Integer_8_Unsigned'Wide_Image(State.Number_Of_Virtual_Bytes_Total));
        Put_Line("Virtual available: "          & Integer_8_Unsigned'Wide_Image(State.Number_Of_Virtual_Bytes_Available));
        Put_Line("Virtual available extended: " & Integer_8_Unsigned'Wide_Image(State.Number_Of_Virtual_Bytes_Available_Extended));
        Hang_Window;
      end Test;
  ---------------------
  -- Set_Byte_Limits --
  ---------------------
    procedure Set_Byte_Limits(
      Minimum : in Integer_8_Unsigned;
      Maximum : in Integer_8_Unsigned)
      is
      begin
        Implementation.Set_Byte_Limits(Minimum, Maximum);
      exception
        when System_Call_Failure =>
          null;
      end Set_Byte_Limits;
  ---------------
  -- Get_State --
  ---------------
    function Get_State
      return Record_State
      is
      begin
        return Implementation.Get_State;
      exception
        when System_Call_Failure =>
          return (others => <>);
      end Get_State;
    LAUNCH_STATE : constant Record_State := Get_State; -- Bit of a hack
  -------------------------
  -- Get_State_At_Launch --
  -------------------------
    function Get_State_At_Launch
      return Record_State
      is
      begin
        return LAUNCH_STATE;
      end Get_State_At_Launch;
  end Neo.System.Memory;

