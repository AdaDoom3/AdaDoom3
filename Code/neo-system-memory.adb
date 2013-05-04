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
    generic
      type Type_To_Manage
        is private;
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
      Status : Record_State := (others => <>);
      begin
        Put_Title("MEMORY TEST");
        Memory := Get_State;
        Put_Line("Load: "                       & Float_4_Percent'Wide_Image(Memory.Load));
        Put_Line("Disk total: "                 & Integer_Address'Wide_Image(Memory.Number_Of_Disk_Bytes_Total));
        Put_Line("Disk available: "             & Integer_Address'Wide_Image(Memory.Number_Of_Disk_Bytes_Available));
        Put_Line("Physical total: "             & Integer_Address'Wide_Image(Memory.Number_Of_Physical_Bytes_Total));
        Put_Line("Physical available: "         & Integer_Address'Wide_Image(Memory.Number_Of_Physical_Bytes_Available));
        Put_Line("Page file total: "            & Integer_Address'Wide_Image(Memory.Number_Of_Page_File_Bytes_Total));
        Put_Line("Page file available: "        & Integer_Address'Wide_Image(Memory.Number_Of_Page_File_Bytes_Available));
        Put_Line("Virtual total: "              & Integer_Address'Wide_Image(Memory.Number_Of_Virtual_Bytes_Total));
        Put_Line("Virtual available: "          & Integer_Address'Wide_Image(Memory.Number_Of_Virtual_Bytes_Available));
        Put_Line("Virtual available extended: " & Integer_Address'Wide_Image(Memory.Number_Of_Virtual_Bytes_Available_Extended));
        Hang_Window;
      end Test;
  ---------------------
  -- Set_Byte_Limits --
  ---------------------
    procedure Set_Byte_Limits(
      Minimum : in Integer_Address;
      Maximum : in Integer_Address)
      with Pre => Minimum < Maximum
      is
      begin
        Implementation.Set_Byte_Limits;
      exception
        when System_Call_Failure =>
          null;
      end Set_Byte_Limits;
  ---------------
  -- Get_State --
  ---------------
    function Get_State
      return Record_Memory
      is
      begin
        return Implementation.Get_State;
      exception
        when System_Call_Failure =>
          return (others => <>);
      end Get_State;
  end Neo.System.Memory;

