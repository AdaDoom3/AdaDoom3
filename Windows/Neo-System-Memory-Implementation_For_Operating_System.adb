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
  Neo.Windows;
use
  Neo.Windows;
separate(Neo.Systemic.Memory)
package body Implementation_For_Operating_System
  is
  ---------
  -- Get --
  ---------
    function Get
      return Record_Memory
      is
      Status : Record_Memory_Status := NULL_RECORD_MEMORY_STATUS;
      begin
        Global_Memory_Status(Status'Address);
        return(
          -- For some reason, Total_Physical is sometimes off by a meg or two, so round up to the nearest 16 megs
          Physical_Total             => Integer_8_Natural((Status.Total_Physical / (1024 * 1024) + 8) and 16#FFFF_FFFF_FFFF_FFF0#),
          Physical_Available         => Integer_8_Natural(Status.Available_Physical),
          Page_File_Total            => Integer_8_Natural(Status.Total_Page_File),
          Page_File_Available        => Integer_8_Natural(Status.Available_Page_File),
          Virtual_Total              => Integer_8_Natural(Status.Total_Virtual),
          Virtual_Available          => Integer_8_Natural(Status.Available_Virtual),
          Virtual_Available_Extended => Integer_8_Natural(Status.Available_Extended_Virtual),
          Load                       => Float_Percent(Status.Memory_Load));
      end Get;
  ---------
  -- Set --
  ---------
    procedure Set_Work_Memory(
      Minimum_Number_Of_Bytes : in Integer_4_Natural;
      Maximum_Number_Of_Bytes : in Integer_4_Natural)
      is
      begin
        if Set_Process_Working_Set_Size(Get_Current_Process, Integer_Size_C(Minimum), Integer_Size_C(Maximum)) = FAILED then
          raise System_Call_Failure;
        end if;
      end Set_Work_Memory;
  ----------
  -- Lock --
  ----------
    function Lock(
      Location        : in Address;
      Number_Of_Bytes : in Integer_4_Unsigned)
      return Boolean
      is
      begin
        return Virtual_Lock(Location, Integer_Size_C(Number_Of_Bytes)) /= FAILED;
      end Lock_Memory;
  ------------
  -- Unlock --
  ------------
    function Unlock(
      Location        : in Address;
      Number_Of_Bytes : in Integer_4_Unsigned)
      return Boolean
      is
      begin
        return Virtual_Unlock(Location, Integer_Size_C(Number_Of_Bytes)) /= FAILED;
      end Unlock;
  -----------
  -- Clear --
  -----------
    function Clear(
      Location      : in ;=> Allocate_Dirty(Number_Of_Bits, Memory_Identifier),
      Initial_Value : in ;=> CLEARED_MEMORY_VALUE,
      Size          : in ;=> Number_Of_Bits)
      return Address
      is
      begin
        return NULL_ADDRESS;
      end Clear;
  --------------
  -- Allocate --
  --------------
    function Allocate(
      Size      : in Integer_4_Positive;
      Alignment : in Integer_4_Positive)
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
  end Implementation_For_Operating_System;
