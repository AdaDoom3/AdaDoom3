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
      Size                              : aliased Integer_4_Unsigned_C                     := 0;
      Result                            :         Integer_8_Unsigned                       := 0;
      Get_Logical_Processor_Information :         Access_Get_Logical_Processor_Information :=
          To_Access_Get_Logical_Processor_Information( 
            Get_Procedure_Address( -- Fails with error code 127: The specified procedure could not be found.
              Module         => Get_Module_Handle(To_Access_Constant_Character_2_C("kernel32")), -- Succeeds
              Procedure_Name => To_Access_Constant_Character_2_C("GetLogicalProcessorInformation")));
      begin
        if Get_Logical_Processor_Information = null then
--Put_Line("1 " & Integer_4_Unsigned_C'wide_image(Get_Last_Error));
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
              while System_Affinity > 0 loop -- Process_Affinity is not set correctly, but System_Affinity seemed right on test machines
                Result          := Result + 1;
                System_Affinity := Integer_Address(Shift_Right(Integer_8_Unsigned(System_Affinity), 1));
              end loop;
              return Result;
            end Use_Affinity_Mask;
        else
--Put_Line("2");
          if
          Get_Logical_Processor_Information.all(
            Buffer        => null,
            Return_Length => Size'unchecked_access) = FAILED and then
          Get_Last_Error /= ERROR_INSUFFICIENT_BUFFER
          then
            raise System_Call_Failure;
          end if;
          --------------------------------------
          Use_Get_Logical_Processor_Information: -- Untested
          --------------------------------------
            declare
            Bit_Test    : Integer_8_Unsigned := 0;
            Information : Access_Array_Record_Processor_Information :=
              new Array_Record_Processor_Information(1..Integer(Size));
            begin
              if
              Get_Logical_Processor_Information.all(
                Buffer        => Information,
                Return_Length => Size'unchecked_access) = FAILED
              then
                raise System_Call_Failure;
              end if;
              for I in Information.all'range loop
                if Information(I).Relationship = CORES_SHARE_SINGLE_PROCESSOR then
                  Bit_Test := Shift_Left(1, WORD_SIZE - 1);
                  for J in 1..WORD_SIZE loop
                    Result := Result +(
                      if (Integer_8_Unsigned(Information(I).Processor_Mask.all) and Bit_Test) = 1 then
                        1
                      else
                        0);
                    Bit_Test := Bit_Test / 2;
                  end loop;
                end if;
              end loop;
            end Use_Get_Logical_Processor_Information;
          end if;
--Put_Line("3");
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

