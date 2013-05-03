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
  Neo.Windows;
use
  Interfaces,
  Interfaces.C,
  Neo.Windows;
separate(Neo.System.Memory)
package body Implementation
  is
  ---------
  -- Get --
  ---------
    function Get
      return Record_Memory
      is
      Status : Record_Memory_Status := (others => <>);
      begin
        if Global_Memory_Status(Status'Address) = FAILED then
          raise System_Call_Failure;
        end if;
        return(
          -- For some reason, Total_Physical is sometimes off by a meg or two, so round up to the nearest 16 megs
          Physical_Total             => Integer_8_Natural((Status.Total_Physical / (1024 * 1024) + 8) and 16#FFFF_FFFF_FFFF_FFF0#),
          Physical_Available         => Integer_8_Natural(Status.Available_Physical),
          Page_File_Total            => Integer_8_Natural(Status.Total_Page_File),
          Page_File_Available        => Integer_8_Natural(Status.Available_Page_File),
          Virtual_Total              => Integer_8_Natural(Status.Total_Virtual),
          Virtual_Available          => Integer_8_Natural(Status.Available_Virtual),
          Virtual_Available_Extended => Integer_8_Natural(Status.Available_Extended_Virtual),
          Load                       => Float_4_Percent(Status.Memory_Load),
          Freespace_In_Gigabytes     => );
      end Get;
  ---------------------
  -- Set_Byte_Limits --
  ---------------------
    procedure Set_Byte_Limits(
      Minimum : in Integer_4_Unsigned;
      Maximum : in Integer_4_Unsigned)
      is
      begin
        if
        Set_Process_Working_Set_Size(
          Process => Get_Current_Process,
          Minimum => Integer_Size_C(Minimum),
          Maximum => Integer_Size_C(Maximum)) = FAILED
        then
          raise System_Call_Failure;
        end if;
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
        return Virtual_Lock(Location, Integer_Size_C(Number_Of_Bytes)) /= FAILED;
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
        return Virtual_Unlock(Location, Integer_Size_C(Number_Of_Bytes)) /= FAILED;
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
  end Implementation;

