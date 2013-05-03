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
  System.Machine_Code;
use
  System.Machine_Code;
separate(Neo.System.Processor)
package Implementation_For_Architecture
  is
  ----------------
  -- Initialize --
  ----------------
    procedure Initialize
      is
      begin
        raise System_Call_Failure;
      end Initialize;
  ----------------
  -- Get_Vendor --
  ----------------
    function Get_Vendor
      return Enumerated_Vendor
      is
      begin
        raise System_Call_Failure;
        return Generic_Vendor;
      end Get_Vendor;
  -------------------
  -- Get_Specifics --
  -------------------
    function Get_Specifics
      return Record_Specifics
      is
      begin
        raise System_Call_Failure;
        return (others => <>);
      end Get_Specifics;
  -------------------------
  -- Get_Number_of_Cores --
  -------------------------
    function Get_Number_of_Cores
      return Integer_8_Unsigned
      is
      begin
        raise System_Call_Failure;
        return 0;
      end Get_Number_of_Cores;
  ----------------------
  -- Check_Exceptions --
  ----------------------
    procedure Check_Exceptions
      is
      begin
        raise System_Call_Failure;
      end Check_Exceptions;
  ------------------
  -- Set_Rounding --
  ------------------
    procedure Set_Rounding(
      Rounding : in Enumerated_Rounding)
      is
      begin
        raise System_Call_Failure;
        return Nearest_Rounding;
      end Set_Rounding;
  -------------------
  -- Set_Precision --
  -------------------
    procedure Set_Precision(
      Precision : in Enumerated_Precision)
      is
      begin
        raise System_Call_Failure;
        return Double_Extended_Precision;
      end Set_Precision;
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
  ---------------
  -- Put_Stack --
  ---------------
    procedure Put_Stack
      is
      begin
        raise System_Call_Failure;
      end Put_Stack;
  ---------------
  -- Put_Trace --
  ---------------
    procedure Put_Trace
      is
      begin
        raise System_Call_Failure;
      end Put_Trace;
  --------------------
  -- Is_Stack_Empty --
  --------------------
    function Is_Stack_Empty
      return Boolean
      is
      begin
        raise System_Call_Failure;
        return True;
      end Is_Stack_Empty;
  -----------------
  -- Clear_Stack --
  -----------------
    procedure Clear_Stack
      is
      begin
        raise System_Call_Failure;
      end Clear_Stack;
  end Implementation_For_Architecture;
