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
      Status : Record_Status := (others => <>);
      begin
        Put_Title("MEMORY TEST");
        Memory := Get_Status;
        Put_Line("Load: "                       & Float_4_Percent'Wide_Image(Memory.Load));
        Put_Line("Free space in gigs: "         & Integer_8_Natural'Wide_Image(Memory.Free_Space_In_Gigabytes));
        Put_Line("Physical total: "             & Integer_8_Natural'Wide_Image(Memory.Physical_Total));
        Put_Line("Physical available: "         & Integer_8_Natural'Wide_Image(Memory.Physical_Available));
        Put_Line("Page file total: "            & Integer_8_Natural'Wide_Image(Memory.Page_File_Total));
        Put_Line("Page file available: "        & Integer_8_Natural'Wide_Image(Memory.Page_File_Available));
        Put_Line("Virtual total: "              & Integer_8_Natural'Wide_Image(Memory.Virtual_Total));
        Put_Line("Virtual available: "          & Integer_8_Natural'Wide_Image(Memory.Virtual_Available));
        Put_Line("Virtual available extended: " & Integer_8_Natural'Wide_Image(Memory.Virtual_Available_Extended));
        Hang_Window;
      end Test;
  ---------------------
  -- Set_Byte_Limits --
  ---------------------
    procedure Set_Byte_Limits(
      Minimum : in Integer_4_Unsigned;
      Maximum : in Integer_4_Unsigned)
      is
      begin
        Implementation.Set_Byte_Limits;
      exception
        when System_Call_Failure =>
          null;
      end Set_Byte_Limits;
  ----------------
  -- Get_Status --
  ----------------
    function Get_Status
      return Record_Memory
      is
      begin
        return Implementation.Get_Data;
      exception
        when System_Call_Failure =>
          return (others => <>);
      end Get_Status;
  end Neo.System.Memory;

