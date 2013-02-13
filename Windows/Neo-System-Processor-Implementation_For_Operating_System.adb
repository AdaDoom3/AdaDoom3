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
separate(Neo.System.Processor)
package body Implementation_For_Operating_System
  is
  -------------------------
  -- Get_Number_Of_Cores --
  -------------------------
    function Get_Number_Of_Cores
      return Integer_8_Unsigned
      is
      --Function_Get_Logical_Processor_Information :         Access_Get_Logical_Processor_Information := null;
      Result                                     :         Integer_4_Natural                        := 0; 
      Size                                       : aliased Integer_4_Unsigned_C                     := 0;
      begin
        raise System_Call_Failure; return 1;
        -- Function_Get_Logical_Processor_Information := 
        --   To_Access_Get_Logical_Processor_Information(
        --     Get_Procedure_Address(
        --       Module         => Get_Module_Handle(To_String_2_C("kernel32")),
        --       Procedure_Name => To_String_2_C("GetLogicalProcessorInformation")));
        -- if Function_Get_Logical_Processor_Information = null then
        --   if USE_64_BIT then
        --     -------------
        --     Extend_DWORD:
        --     -------------
        --       declare
        --       Process_Affinity : Integer_8_Unsigned_C := 0;
        --       System_Affinity  : Integer_8_Unsigned_C := 0;
        --       begin
        --         if
        --         Get_Process_Affinity_Mask(
        --           Process               => Get_Current_Process,
        --           Process_Affinity_Mask => Process_Affinity'Address,
        --           System_Affinity_Mask  => System_Affinity'Address) = FAILED
        --         then
        --           raise System_Call_Failure;
        --         end if;
        --         if Process_Affinity = 0 and System_Affinity = 0 then
        --           Result.All := 1;
        --           return True;
        --         end if;
        --         while System_Affinity > 0 loop -- Process_Affinity is not set correctly, but System_Affinity seemed right on test machines
        --           Result          := Result + 1;
        --           System_Affinity := Integer_8_Unsigned_C(Shift_Right(Integer_8_Unsigned(System_Affinity), 1));
        --         end loop;
        --         Result.All := Integer_4_Positive(Result);
        --         return True;
        --       end Extend_DWORD;
        --   else
        --     ------------
        --     Leave_DWORD:
        --     ------------
        --       declare
        --       Process_Affinity : Integer_4_Unsigned_C := 0;
        --       System_Affinity  : Integer_4_Unsigned_C := 0;
        --       begin
        --         if
        --         Get_Process_Affinity_Mask(
        --           Process               => Get_Current_Process,
        --           Process_Affinity_Mask => Process_Affinity'Address,
        --           System_Affinity_Mask  => System_Affinity'Address) = FAILED
        --         then
        --           raise System_Call_Failure;
        --         end if;
        --         if Process_Affinity = 0 and System_Affinity = 0 then
        --           Result.All := 1;
        --           return Result;
        --         end if;
        --         while Process_Affinity > 0 loop
        --           Result           := Result + 1;
        --           Process_Affinity := Integer_4_Unsigned_C(Shift_Right(Integer_4_Unsigned(Process_Affinity), 1));
        --         end loop;
        --         return Result;
        --       end Leave_DWORD;
        --   end if;
        -- end if;
        -- if 
        -- Function_Get_Logical_Processor_Information.All(
        --   Buffer        => null,
        --   Return_Length => Size'Access) = FAILED and then
        -- Get_Last_Error /= ERROR_INSUFFICIENT_BUFFER
        -- then
        --   raise System_Call_Failure;
        -- end if;
        -- --------------------------------------
        -- Use_Get_Logical_Processor_Information:
        -- --------------------------------------
        --   declare
        --   Information : aliased Array_Record_Logical_Processor_Information(1..Integer_4_Signed(Size)) :=(
        --     others => NULL_RECORD_LOGICAL_PROCESSOR_INFORMATION);
        --   begin
        --     if
        --     Function_Get_Logical_Processor_Information.All(
        --       Buffer        => Information'Access,
        --       Return_Length => Size'Access) = FAILED
        --     then
        --       raise System_Call_Failure;
        --     end if;
        --     for I in Information'Range loop
        --       null;
        --       --if Information.Relationship = localRelationProcessorCore then
        --       --  Result.All := Result.All + Count_Set_Bits(ptr->ProcessorMask);
        --       --end if;
        --     end loop;
        --   end Use_Get_Logical_Processor_Information;
        -- return Result;
      end Get_Number_Of_Cores;
  ---------------------
  -- Get_Clock_Ticks --
  ---------------------
    function Get_Clock_Ticks
      return Integer_8_Unsigned
      is
      Ticks : aliased Integer_8_Unsigned_C := 0;
      begin
        if Query_Performance_Counter(Ticks'Address) = FAILED then
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
        if Query_Performance_Frequency(Result'Address) = FAILED then
          ---------------------
          Look_In_The_Registry:
          ---------------------
            declare
            Key    : Address              := NULL_ADDRESS;
            Length : Integer_4_Unsigned_C := 1;
            Speed  : Integer_4_Unsigned_C := 0;
            Dummy  : Integer_4_Signed_C   := 0;
            begin
              if Registry_Open_Key(
                Key     => HKEY_LOCAL_MACHINE,
                Sub_key => To_String_2_C("HARDWARE\DESCRIPTION\System\CentralProcessor\0"),
                Options => 0,
                Desired => KEY_READ,
                Result  => Key'Address) = NO_ERROR then
                if Registry_Query_Value(
                  Key        => Key,
                  Value_Name => To_String_2_C("~MHz"),
                  Reserved   => NULL_ADDRESS,
                  Kind       => NULL_ADDRESS,
                  Data       => Speed'Address,
                  Data_Size  => Length'Address) /= NO_ERROR and then
                Registry_Query_Value(
                  Key        => Key,
                  Value_Name => To_String_2_C("~Mhz"),
                  Reserved   => NULL_ADDRESS,
                  Kind       => NULL_ADDRESS,
                  Data       => Speed'Address,
                  Data_Size  => Length'Address) /= NO_ERROR and then
                Registry_Query_Value(
                  Key        => Key,
                  Value_Name => To_String_2_C("~mhz"),
                  Reserved   => NULL_ADDRESS,
                  Kind       => NULL_ADDRESS,
                  Data       => Speed'Address,
                  Data_Size  => Length'Address) /= NO_ERROR
                then
                  Dummy := Registry_Close_Key(Key);
                  raise System_Call_Failure;
                end if;
                Dummy := Registry_Close_Key(Key);
                return Integer_8_Unsigned(Speed * 1_000_000);
              end if;
              raise System_Call_Failure;
            end Look_In_The_Registry;
        end if;
        return Integer_8_Unsigned(Result);
      end Get_Speed_In_Megahertz;
  end Implementation_For_Operating_System;
