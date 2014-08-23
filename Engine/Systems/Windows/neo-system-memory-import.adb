with Neo.Windows;  use Neo.Windows;
with Interfaces.C; use Interfaces.C;
with Interfaces;   use Interfaces;
separate(Neo.System.Memory) package body Import is
  function Get_State return Record_State is
    Status                         : aliased Record_Memory_Status := (others => <>);
    Number_Of_Disk_Bytes_Available : aliased Integer_8_Unsigned_C := 0;
    Number_Of_Disk_Bytes_Total     : aliased Integer_8_Unsigned_C := 0;
    begin
      Assert(Global_Memory_Status(Status'unchecked_access));
      Assert(Get_Disk_Free_Space(
        Directory                  => null,
        Free_Bytes_Available       => Number_Of_Disk_Bytes_Available'unchecked_access,
        Total_Number_Of_Bytes      => Number_Of_Disk_Bytes_Total'unchecked_access,
        Total_Number_Of_Free_Bytes => null));
      return( -- Round up Status.Total_Physical to the nearest 16 megabytes due to inaccurate results on test machines
        Number_Of_Physical_Bytes_Total             => Integer_8_Unsigned((Status.Total_Physical / 1024**2 + Byte'size) and 16#FFFF_FFFF_FFFF_FFF0#),
        Number_Of_Physical_Bytes_Available         => Integer_8_Unsigned(Status.Available_Physical),
        Number_Of_Disk_Bytes_Total                 => Integer_8_Unsigned(Number_Of_Disk_Bytes_Total),
        Number_Of_Disk_Bytes_Available             => Integer_8_Unsigned(Number_Of_Disk_Bytes_Available),
        Number_Of_Page_File_Bytes_Total            => Integer_8_Unsigned(Status.Total_Page_File),
        Number_Of_Page_File_Bytes_Available        => Integer_8_Unsigned(Status.Available_Page_File),
        Number_Of_Virtual_Bytes_Total              => Integer_8_Unsigned(Status.Total_Virtual),
        Number_Of_Virtual_Bytes_Available          => Integer_8_Unsigned(Status.Available_Virtual),
        Number_Of_Virtual_Bytes_Available_Extended => Integer_8_Unsigned(Status.Available_Extended_Virtual),
        Load                                       => Float_4_Percent(Status.Memory_Load));
    end Get_State;
  procedure Set_Byte_Limits (Minimum, Maximum : in Integer_8_Unsigned)                       is begin Assert(Set_Process_Working_Set_Size(Get_Current_Process, Integer_Size_C(Minimum), Integer_Size_C(Maximum))); end Set_Byte_Limits;
  procedure Unlock          (Location : in Address; Number_Of_Bytes : in Integer_8_Unsigned) is begin Assert(Virtual_Unlock(Location, Integer_Size_C(Number_Of_Bytes)));                                           end Unlock;
  procedure Lock            (Location : in Address; Number_Of_Bytes : in Integer_8_Unsigned) is begin Assert(Virtual_Lock(Location, Integer_Size_C(Number_Of_Bytes)));                                             end Lock;
end Import;
