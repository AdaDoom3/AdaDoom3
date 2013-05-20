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
with
  Interfaces.C,
  Neo.POSIX;
use
  Interfaces.C,
  Neo.POSIX;
separate(Neo.System.Processor)
package body Implementation_For_Operating_System
  is
  -------------------------
  -- Get_Number_Of_Cores --
  -------------------------
    function Get_Number_Of_Cores
      return Integer_8_Unsigned
      is
      begin
        return Integer_8_Unsigned(Get_System_Configuration(NUMBER_OF_PROCESSORS_ONLINE));
      end Get_Number_Of_Cores;
  ---------------------
  -- Get_Clock_Ticks --
  ---------------------
    function Get_Clock_Ticks
      return Integer_8_Unsigned
      is
      Time : aliased Record_Time_Stamp := (others => <>);
      begin
        if Get_Time_Stamp(Time'Access) = -1 then
          raise System_Call_Failure;
        end if;
        return Integer_8_Unsigned(Time.System);
      end Get_Clock_Ticks;
  ----------------------------
  -- Get_Speed_In_Megahertz -- No POSIX commands for this, so raise an exception and let it be timed by Get_Clock_Ticks
  ----------------------------
    function Get_Speed_In_Megahertz
      return Integer_8_Unsigned
      is
      begin
        raise Unsupported_Feature; 
        return 0;
      end Get_Speed_In_Megahertz;
  end Implementation_For_Operating_System;
