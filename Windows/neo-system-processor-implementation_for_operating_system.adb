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
WITH
  System,
  Interfaces.C,
  Neo.Windows;
USE
  System,
  Interfaces.C,
  Neo.Windows;
SEPARATE(Neo.System.Processor)
PACKAGE BODY Implementation_For_Operating_System
  IS
  -------------------------
  -- Get_Number_Of_Cores --
  -------------------------
    FUNCTION Get_Number_Of_Cores
      RETURN Integer_8_Unsigned
      IS
      --Function_Get_Logical_Processor_Information :         Access_Get_Logical_Processor_Information := null;
      Result                                     :         Integer_4_Natural                        := 0; 
      Size                                       : ALIASED Integer_4_Unsigned_C                     := 0;
      BEGIN
        RAISE System_Call_Failure; RETURN 1;
        -- Function_Get_Logical_Processor_Information := 
        --   To_Access_Get_Logical_Processor_Information(
        --     Get_Procedure_Address(
        --       Module         => Get_Module_Handle(To_String_2_C("kernel32")),
        --       Procedure_Name => To_String_2_C("GetLogicalProcessorInformation")));
        -- IF Function_Get_Logical_Processor_Information = null THEN
        --   IF USE_64_BIT THEN
        --     -------------
        --     Extend_DWORD:
        --     -------------
        --       DECLARE
        --       Process_Affinity : Integer_8_Unsigned_C := 0;
        --       System_Affinity  : Integer_8_Unsigned_C := 0;
        --       BEGIN
        --         IF
        --         Get_Process_Affinity_Mask(
        --           Process               => Get_Current_Process,
        --           Process_Affinity_Mask => Process_Affinity'Address,
        --           System_Affinity_Mask  => System_Affinity'Address) = FAILED
        --         THEN
        --           RAISE System_Call_Failure;
        --         END IF;
        --         IF Process_Affinity = 0 and System_Affinity = 0 THEN
        --           Result.All := 1;
        --           RETURN True;
        --         END IF;
        --         while System_Affinity > 0 loop -- Process_Affinity IS not set correctly, but System_Affinity seemed right on test machines
        --           Result          := Result + 1;
        --           System_Affinity := Integer_8_Unsigned_C(Shift_Right(Integer_8_Unsigned(System_Affinity), 1));
        --         END loop;
        --         Result.All := Integer_4_Positive(Result);
        --         RETURN True;
        --       END Extend_DWORD;
        --   else
        --     ------------
        --     Leave_DWORD:
        --     ------------
        --       DECLARE
        --       Process_Affinity : Integer_4_Unsigned_C := 0;
        --       System_Affinity  : Integer_4_Unsigned_C := 0;
        --       BEGIN
        --         IF
        --         Get_Process_Affinity_Mask(
        --           Process               => Get_Current_Process,
        --           Process_Affinity_Mask => Process_Affinity'Address,
        --           System_Affinity_Mask  => System_Affinity'Address) = FAILED
        --         THEN
        --           RAISE System_Call_Failure;
        --         END IF;
        --         IF Process_Affinity = 0 and System_Affinity = 0 THEN
        --           Result.All := 1;
        --           RETURN Result;
        --         END IF;
        --         while Process_Affinity > 0 loop
        --           Result           := Result + 1;
        --           Process_Affinity := Integer_4_Unsigned_C(Shift_Right(Integer_4_Unsigned(Process_Affinity), 1));
        --         END loop;
        --         RETURN Result;
        --       END Leave_DWORD;
        --   END IF;
        -- END IF;
        -- IF 
        -- Function_Get_Logical_Processor_Information.All(
        --   Buffer        => null,
        --   Return_Length => Size'Access) = FAILED and THEN
        -- Get_Last_Error /= ERROR_INSUFFICIENT_BUFFER
        -- THEN
        --   RAISE System_Call_Failure;
        -- END IF;
        -- --------------------------------------
        -- Use_Get_Logical_Processor_Information:
        -- --------------------------------------
        --   DECLARE
        --   Information : ALIASED Array_Record_Logical_Processor_Information(1..Integer_4_Signed(Size)) :=(
        --     others => NULL_RECORD_LOGICAL_PROCESSOR_INFORMATION);
        --   BEGIN
        --     IF
        --     Function_Get_Logical_Processor_Information.All(
        --       Buffer        => Information'Access,
        --       Return_Length => Size'Access) = FAILED
        --     THEN
        --       RAISE System_Call_Failure;
        --     END IF;
        --     for I in Information'Range loop
        --       null;
        --       --IF Information.Relationship = localRelationProcessorCore THEN
        --       --  Result.All := Result.All + Count_Set_Bits(ptr->ProcessorMask);
        --       --END IF;
        --     END loop;
        --   END Use_Get_Logical_Processor_Information;
        -- RETURN Result;
      END Get_Number_Of_Cores;
  ---------------------
  -- Get_Clock_Ticks --
  ---------------------
    FUNCTION Get_Clock_Ticks
      RETURN Integer_8_Unsigned
      IS
      Ticks : ALIASED Integer_8_Unsigned_C := 0;
      BEGIN
        IF Query_Performance_Counter(Ticks'Address) = FAILED THEN
          RAISE System_Call_Failure;
        END IF;
        RETURN Integer_8_Unsigned(Ticks);
      END Get_Clock_Ticks;
  ----------------------------
  -- Get_Speed_In_Megahertz --
  ----------------------------
    FUNCTION Get_Speed_In_Megahertz
      RETURN Integer_8_Unsigned
      IS
      Result : ALIASED Integer_8_Unsigned_C := 0;
      BEGIN
        IF Query_Performance_Frequency(Result'Address) /= FAILED THEN
          RETURN Integer_8_Unsigned(Result);
        END IF;
        ---------------------
        Look_In_The_Registry:
        ---------------------
          DECLARE
          Key      : Address              := NULL_ADDRESS;
          Length   : Integer_4_Unsigned_C := 1;
          Speed    : Integer_4_Unsigned_C := 0;
          Do_Raise : Boolean              := False; 
          BEGIN
            IF
            Registry_Open_Key(
              Key     => HKEY_LOCAL_MACHINE,
              Sub_key => To_String_2_C("HARDWARE\DESCRIPTION\System\CentralProcessor\0"),
              Options => 0,
              Desired => KEY_READ,
              Result  => Key'Address) /= NO_ERROR
            THEN
              RAISE System_Call_Failure;
            END IF;
            IF
            Registry_Query_Value(
              Key        => Key,
              Value_Name => To_String_2_C("~MHz"),
              Reserved   => NULL_ADDRESS,
              Kind       => NULL_ADDRESS,
              Data       => Speed'Address,
              Data_Size  => Length'Address) /= NO_ERROR and THEN
            Registry_Query_Value(
              Key        => Key,
              Value_Name => To_String_2_C("~Mhz"),
              Reserved   => NULL_ADDRESS,
              Kind       => NULL_ADDRESS,
              Data       => Speed'Address,
              Data_Size  => Length'Address) /= NO_ERROR and THEN
            Registry_Query_Value(
              Key        => Key,
              Value_Name => To_String_2_C("~mhz"),
              Reserved   => NULL_ADDRESS,
              Kind       => NULL_ADDRESS,
              Data       => Speed'Address,
              Data_Size  => Length'Address) /= NO_ERROR
            THEN
              Do_Raise := True;
            END IF;
            IF Registry_Close_Key(Key) /= NO_ERROR THEN
              null;
            END IF;
            IF Do_Raise THEN
              RAISE System_Call_Failure;
            END IF;
            RETURN Integer_8_Unsigned(Speed * 1_000_000);
          END Look_In_The_Registry;
      END Get_Speed_In_Megahertz;
  END Implementation_For_Operating_System;

