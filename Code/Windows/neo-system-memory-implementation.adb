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
    function Get_Status
      return Record_Status
      is
      Status : Record_Status := (others => <>);
      begin
        if Global_Memory_Status(Status'Access) = FAILED then
          raise System_Call_Failure;
        end if;
        return(
          -- For some reason, Total_Physical is sometimes off by a meg or two, so round up to the nearest 16 megs
          Physical_Total             => Integer_8_Unsigned((Status.Total_Physical / (1024 * 1024) + 8) and 16#FFFF_FFFF_FFFF_FFF0#),
          Physical_Available         => Integer_8_Unsigned(Status.Available_Physical),
          Page_File_Total            => Integer_8_Unsigned(Status.Total_Page_File),
          Page_File_Available        => Integer_8_Unsigned(Status.Available_Page_File),
          Virtual_Total              => Integer_8_Unsigned(Status.Total_Virtual),
          Virtual_Available          => Integer_8_Unsigned(Status.Available_Virtual),
          Virtual_Available_Extended => Integer_8_Unsigned(Status.Available_Extended_Virtual),
          Disk_Total                 => Integer_8_Unsigned(),
          Disk_Available             => Integer_8_Unsigned(),
          Load                       => Float_4_Percent(Status.Memory_Load));
      end Get_Status;
  ---------------------
  -- Set_Byte_Limits --
  ---------------------
    procedure Set_Byte_Limits(
      Minimum : in Integer_Address;
      Maximum : in Integer_Address)
      with Pre => Minimum < Maximum
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

