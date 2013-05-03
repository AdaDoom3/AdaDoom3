--                                                                                                                      
--                                                                                                                      
--
--
--
--
--
-- Perhaps Clear, Free, and Allocate should be removed and we should implement the functions with "stream types" 
-- in the processor main body instead, to avoid platform dependent stuff.
--
--
--
--
--
--
with
  Interfaces,
  Interfaces.C,
  Neo.Linux;
use
  Interfaces,
  Interfaces.C,
  Neo.Linux;
separate(Neo.System.Memory)
package body Implementation
  is
  ---------
  -- Get --
  ---------
    function Get
      return Record_Memory
      is
      begin
        -- fstatvfs, statvfs
        raise System_Call_Failure;
        return(others => <>);
      end Get;
  ---------------------
  -- Set_Byte_Limits --
  ---------------------
    procedure Set_Byte_Limits(
      Minimum : in Integer_4_Unsigned;
      Maximum : in Integer_4_Unsigned)
      is
      begin
        raise System_Call_Failure;
      end Set_Byte_Limits;
  ----------
  -- Lock --
  ----------
    procedure Lock(
      Location        : in Address;
      Number_Of_Bytes : in Integer_4_Unsigned)
      is
      begin
        raise System_Call_Failure;
      end Lock;
  ------------
  -- Unlock --
  ------------
    procedure Unlock(
      Location        : in Address;
      Number_Of_Bytes : in Integer_4_Unsigned)
      is
      begin
        raise System_Call_Failure;
      end Unlock;
  -----------
  -- Clear --
  -----------
    function Clear(
      Location      : in Address;
      Size          : in Integer_4_Unsigned;
      Initial_Value : in Boolean := CLEARED_MEMORY_VALUE)
      return Address
      is
      begin
        raise System_Call_Failure;
        return NULL_ADDRESS;
      end Clear;
  --------------
  -- Allocate --
  --------------
    function Allocate(
      Size      : in Integer_4_Unsigned;
      Alignment : in Integer_4_Unsigned)
      return Address
      is
      begin
        raise System_Call_Failure;
        return NULL_ADDRESS;
      end Allocate;
  ----------
  -- Free --
  ----------
    procedure Free(
      Data : in Address)
      is
      begin
        raise System_Call_Failure;
        null;
      end Free;
  end Implementation;
