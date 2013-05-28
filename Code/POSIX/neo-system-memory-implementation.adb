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
  Neo.POSIX;
use
  Interfaces,
  Interfaces.C,
  Neo.POSIX;
separate(Neo.System.Memory)
package body Implementation
  is
  ---------------
  -- Get_State --
  ---------------
    function Get_State
      return Record_State
      is
      begin
        -- fstatvfs, statvfs
        raise System_Call_Failure;
        return(others => <>);
      end Get_State;
  ---------------------
  -- Set_Byte_Limits --
  ---------------------
    procedure Set_Byte_Limits(
      Minimum : in Integer_8_Unsigned;
      Maximum : in Integer_8_Unsigned)
      is
      begin
        raise System_Call_Failure;
      end Set_Byte_Limits;
  ----------
  -- Lock --
  ----------
    procedure Lock(
      Location        : in Address;
      Number_Of_Bytes : in Integer_8_Unsigned)
      is
      begin
        raise System_Call_Failure;
      end Lock;
  ------------
  -- Unlock --
  ------------
    procedure Unlock(
      Location        : in Address;
      Number_Of_Bytes : in Integer_8_Unsigned)
      is
      begin
        raise System_Call_Failure;
      end Unlock;
  end Implementation;
