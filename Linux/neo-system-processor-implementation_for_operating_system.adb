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
  System,
  Interfaces.C,
  Neo.Windows;
use
  System,
  Interfaces.C,
  Neo.Windows;
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
        raise System_Call_Failure;
      end Get_Number_Of_Cores;
  ---------------------
  -- Get_Clock_Ticks --
  ---------------------
    function Get_Clock_Ticks
      return Integer_8_Unsigned
      is
      begin
        raise System_Call_Failure;
        return 0;
      end Get_Clock_Ticks;
  ----------------------------
  -- Get_Speed_In_Megahertz --
  ----------------------------
    function Get_Speed_In_Megahertz
      return Integer_8_Unsigned
      is
      begin
        raise System_Call_Failure;
        return 0;
      end Get_Speed_In_Megahertz;
  end Implementation_For_Operating_System;
