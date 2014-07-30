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
        null;
      end Initialize;
  ----------------
  -- Get_Vendor --
  ----------------
    function Get_Vendor
      return Enumerated_Vendor
      is
      begin
        return ARM_Licenced_Vendor;
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
        raise Unsupported_Feature;
      end Check_Exceptions;
  ------------------
  -- Set_Rounding --
  ------------------
    procedure Set_Rounding(
      Rounding : in Enumerated_Rounding)
      is
      begin
        raise Unsupported_Feature;
        return Nearest_Rounding;
      end Set_Rounding;
  -------------------
  -- Set_Precision --
  -------------------
    procedure Set_Precision(
      Precision : in Enumerated_Precision)
      is
      begin
        raise Unsupported_Feature;
        return Double_Extended_Precision;
      end Set_Precision;
  ---------------------
  -- Get_Clock_Ticks --
  ---------------------
    function Get_Clock_Ticks
      return Integer_8_Unsigned
      is
      begin
        raise Unsupported_Feature;
        return 0;
      end Get_Clock_Ticks;
  ---------------
  -- Put_Stack --
  ---------------
    procedure Put_Stack
      is
      begin
        raise Unsupported_Feature;
      end Put_Stack;
  --------------------
  -- Is_Stack_Empty --
  --------------------
    function Is_Stack_Empty
      return Boolean
      is
      begin
        raise Unsupported_Feature;
        return True;
      end Is_Stack_Empty;
  --------------------------
  -- Compare_And_Exchange --
  --------------------------
    function Compare_And_Exchange(
      Destination : out Integer_4_Unsigned;
      Comparand   : in  Integer_4_Unsigned;
      Item        : in  Integer_4_Unsigned)
      return Integer_4_Unsigned
      is
      begin
        raise System_Call_Failure;
      end Compare_And_Exchange;
  -----------------
  -- Clear_Stack --
  -----------------
    procedure Clear_Stack
      is
      begin
        raise Unsupported_Feature;
      end Clear_Stack;
  end Implementation_For_Architecture;
