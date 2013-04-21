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
        return(
          Physical_Total             => 0,
          Physical_Available         => 0,
          Page_File_Total            => 0,
          Page_File_Available        => 0,
          Virtual_Total              => 0,
          Virtual_Available          => 0,
          Virtual_Available_Extended => 0,
          Load                       => 0.0);
      end Get;
  ---------------------
  -- Set_Byte_Limits --
  ---------------------
    procedure Set_Byte_Limits(
      Minimum : in Integer_4_Unsigned;
      Maximum : in Integer_4_Unsigned)
      is
      begin
        null;
      end Set_Byte_Limits;
  ----------
  -- Lock --
  ----------
    function Lock(
      Location        : in Address;
      Number_Of_Bytes : in Integer_4_Unsigned)
      return Boolean
      is
      begin
        return False;
      end Lock;
  ------------
  -- Unlock --
  ------------
    function Unlock(
      Location        : in Address;
      Number_Of_Bytes : in Integer_4_Unsigned)
      return Boolean
      is
      begin
        return False;
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
        return NULL_ADDRESS;
      end Allocate;
  ----------
  -- Free --
  ----------
    procedure Free(
      Data : in Address)
      is
      begin
        null;
      end Free;
  end Implementation;
