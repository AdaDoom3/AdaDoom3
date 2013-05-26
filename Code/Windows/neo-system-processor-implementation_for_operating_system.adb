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
  System,
  System.Address_Image,
  Interfaces.C,
  Neo.Windows;
use
  System,
  Interfaces.C,
  Neo.Windows;
separate(Neo.System.Processor)
package body Implementation_For_Operating_System
  is
  -------------------------
  -- Get_Number_Of_Cores --
  -------------------------
    function Get_Number_Of_Cores
      return Integer_8_Unsigned
      is
      Number_Of_Information_Records : aliased Integer_4_Unsigned_C := 0;
      Result                        :         Integer_8_Unsigned   := 0;
      begin
        if
        Get_Version < Windows_2_6_System or else( -- Get_Core_Information requires XP SP3 or later
          Get_Core_Information(
            Buffer        => null,
            Return_Length => Number_Of_Information_Records'unchecked_access) = FAILED and then
          Get_Last_Error /= ERROR_INSUFFICIENT_BUFFER)
        then
          ------------------
          Use_Affinity_Mask:
          ------------------
            declare
            Process_Affinity : aliased Integer_Address := 0;
            System_Affinity  : aliased Integer_Address := 0;
            begin
              if
              Get_Process_Affinity_Mask(
                Process               => Get_Current_Process,
                Process_Affinity_Mask => Process_Affinity'unchecked_access,
                System_Affinity_Mask  => System_Affinity'unchecked_access) = FAILED
              then
                raise System_Call_Failure;
              end if;
              if Process_Affinity = 0 and System_Affinity = 0 then
                return 1;
              end if;
              -- Process_Affinity is not set correctly, but System_Affinity seemed right on test machines
              while System_Affinity > 0 loop
                Result          := Result + 1;
                System_Affinity := Integer_Address(Shift_Right(Integer_8_Unsigned(System_Affinity), 1));
              end loop;
              return Result;
            end Use_Affinity_Mask;
        else
          -------------------------
          Use_Get_Core_Information:
          -------------------------
            declare
            Information : Access_Array_Record_Core_Information :=
              new Array_Record_Core_Information(1..Integer(Number_Of_Information_Records));
            begin
              if
              Get_Core_Information(
                Buffer        => Information,
                Return_Length => Number_Of_Information_Records'unchecked_access) = FAILED
              then
                raise System_Call_Failure;
              end if;
              for I in Information.all'range loop
                if Information(I).Relationship = CORES_SHARE_SINGLE_PROCESSOR then
                  for J in 0..Information(I).Processor_Mask'size - 1 loop
                    Result := Result +(
                      if (Information(I).Processor_Mask and 2**J) > 0 then
                        1
                      else
                        0);
                  end loop;
                end if;
              end loop;
            end Use_Get_Core_Information;
          end if;
        return Result;
      end Get_Number_Of_Cores;
  ---------------------
  -- Get_Clock_Ticks --
  ---------------------
    function Get_Clock_Ticks
      return Integer_8_Unsigned
      is
      Ticks : aliased Integer_8_Unsigned_C := 0;
      begin
        if Query_Performance_Counter(Ticks'unchecked_access) = FAILED then
          raise System_Call_Failure;
        end if;
        return Integer_8_Unsigned(Ticks);
      end Get_Clock_Ticks;
  ----------------------------
  -- Get_Speed_In_Megahertz --
  ----------------------------
    function Get_Speed_In_Megahertz
      return Integer_8_Unsigned
      is
      Result : aliased Integer_8_Unsigned_C := 0;
      begin
        if Query_Performance_Frequency(Result'unchecked_access) /= FAILED then
          return Integer_8_Unsigned(Result);
        end if;
        ---------------------
        Look_In_The_Registry:
        ---------------------
          declare
          Length   : aliased Integer_4_Unsigned_C := 1;
          Speed    : aliased Integer_4_Unsigned_C := 0;
          Key      : aliased Address              := NULL_ADDRESS;
          Do_Raise :         Boolean              := False;
          begin
            if
            Registry_Open_Key(
              Key     => HKEY_LOCAL_MACHINE,
              Sub_key => To_String_2_C("HARDWARE\DESCRIPTION\System\CentralProcessor\0"),
              Options => 0,
              Desired => KEY_READ,
              Result  => Key) /= NO_ERROR
            then
              raise System_Call_Failure;
            end if;
            if
            Registry_Query_Value(
              Key        => Key,
              Value_Name => To_String_2_C("~MHz"),
              Reserved   => null,
              Kind       => null,
              Data       => Speed'address,
              Data_Size  => Length'unchecked_access) /= NO_ERROR and then
            Registry_Query_Value(
              Key        => Key,
              Value_Name => To_String_2_C("~Mhz"),
              Reserved   => null,
              Kind       => null,
              Data       => Speed'address,
              Data_Size  => Length'unchecked_access) /= NO_ERROR and then
            Registry_Query_Value(
              Key        => Key,
              Value_Name => To_String_2_C("~mhz"),
              Reserved   => null,
              Kind       => null,
              Data       => Speed'address,
              Data_Size  => Length'unchecked_access) /= NO_ERROR
            then
              Do_Raise := True;
            end if;
            if Registry_Close_Key(Key) /= 0 then
              null;
            end if;
            if Do_Raise then
              raise System_Call_Failure;
            end if;
            return Integer_8_Unsigned(Speed * 1_000_000);
          end Look_In_The_Registry;
      end Get_Speed_In_Megahertz;
  end Implementation_For_Operating_System;

