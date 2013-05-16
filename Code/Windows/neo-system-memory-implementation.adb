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
  ----------------
  -- Get_Status --
  ----------------
    function Get_State
      return Record_State
      is
      Status : aliased Record_Memory_Status := (others => <>);
      begin
        if Global_Memory_Status(Status'Unchecked_Access) = FAILED then
          raise System_Call_Failure;
        end if;
        return(
          -- For some reason (on old systems?), Total_Physical is sometimes off by a meg or two, so round up to the nearest 16 megs
          Number_Of_Physical_Bytes_Total             => Integer_Address((Status.Total_Physical / (1024 * 1024) + 8) and 16#FFFF_FFFF_FFFF_FFF0#),
          Number_Of_Physical_Bytes_Available         => Integer_Address(Status.Available_Physical),
          Number_Of_Disk_Bytes_Total                 => Integer_8_Unsigned(0),
          Number_Of_Disk_Bytes_Available             => Integer_8_Unsigned(0),
          Number_Of_Page_File_Bytes_Total            => 0,--Integer_Address(Status.Total_Page_File),
          Number_Of_Page_File_Bytes_Available        => 0,--Integer_Address(Status.Available_Page_File),
          Number_Of_Virtual_Bytes_Total              => 0,--Integer_Address(Status.Total_Virtual),
          Number_Of_Virtual_Bytes_Available          => 0,--Integer_Address(Status.Available_Virtual),
          Number_Of_Virtual_Bytes_Available_Extended => 0,--Integer_Address(Status.Available_Extended_Virtual),
          Load                                       => Float_4_Percent(Status.Memory_Load));
      end Get_State;
  ---------------------
  -- Set_Byte_Limits --
  ---------------------
    procedure Set_Byte_Limits(
      Minimum : in Integer_Address;
      Maximum : in Integer_Address)
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
    procedure Lock(
      Location        : in Address;
      Number_Of_Bytes : in Integer_Address)
      is
      begin
        if Virtual_Lock(Location, Integer_Size_C(Number_Of_Bytes)) = FAILED then
          raise System_Call_Failure;
        end if;
      end Lock;
  ------------
  -- Unlock --
  ------------
    procedure Unlock(
      Location        : in Address;
      Number_Of_Bytes : in Integer_Address)
      is
      begin
        if Virtual_Unlock(Location, Integer_Size_C(Number_Of_Bytes)) = FAILED then
          raise System_Call_Failure;
        end if;
      end Unlock;
  end Implementation;

