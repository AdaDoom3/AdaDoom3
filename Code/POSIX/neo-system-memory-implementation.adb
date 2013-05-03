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
  ----------------
  -- Get_Status --
  ----------------
    function Get_Status
      return Record_Status
      is
      begin
        -- fstatvfs, statvfs
        raise System_Call_Failure;
        return(others => <>);
      end Get_Status;
  ---------------------
  -- Set_Byte_Limits --
  ---------------------
    procedure Set_Byte_Limits(
      Minimum : in Integer_Address;
      Maximum : in Integer_Address)
      is
      begin
        raise System_Call_Failure;
      end Set_Byte_Limits;
  ----------
  -- Lock --
  ----------
    procedure Lock(
      Location        : in Address;
      Number_Of_Bytes : in Integer_Address)
      is
      begin
        raise System_Call_Failure;
      end Lock;
  ------------
  -- Unlock --
  ------------
    procedure Unlock(
      Location        : in Address;
      Number_Of_Bytes : in Integer_Address)
      is
      begin
        raise System_Call_Failure;
      end Unlock;
  end Implementation;
