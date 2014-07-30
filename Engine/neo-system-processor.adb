package body Neo.System.Processor is
    procedure Test is
      begin
        Put_Title(Localize("PROCESSOR TEST"));
        Put_Stack;
        Check_Exceptions;
        Set_Rounding(Nearest_Rounding);
        Set_Precision(Double_Extended_Precision);
        if Is_Stack_Empty then
          Put_Line(Localize("Stack is empty!"));
        else
          Put_Stack;
          Clear_Stack;
          if Is_Stack_Empty then Put_Line(Localize("Stack was cleared successfully")); end if;
        end if;
        Put_Line(Localize("Clock ticks:")        & Integer_8_Unsigned'wide_image(Get_Clock_Ticks));
        Put_Line(Localize("Sleep 0.1..."));      delay 0.1;
        Put_Line(Localize("Clock ticks:")        & Integer_8_Unsigned'wide_image(Get_Clock_Ticks));
        Put_Line(Localize("Sleep 0.025..."));    delay 0.025;
        Put_Line(Localize("Clock ticks:")        & Integer_8_Unsigned'wide_image(Get_Clock_Ticks));
        Put_Line(Localize("Speed in megahertz:") & Integer_8_Unsigned'wide_image(SPECIFICS.Speed_In_Megahertz));
        Put_Line(Localize("Vendor: ")            & Enumerated_Vendor 'wide_image(SPECIFICS.Vendor));
        case SPECIFICS.Vendor is
          when Advanced_RISC_Machines_Vendor =>
            if SPECIFICS.Has_NEON                                     then Put_Line(Localize("Has NEON"));      end if;
            if SPECIFICS.Has_Vector_Floating_Point                    then Put_Line(Localize("Has VFP"));       end if;
          when Apple_IBM_Motorola_Vendor =>
            if SPECIFICS.Has_Vector_Multimedia_Instructions           then Put_Line(Localize("Has VMI"));       end if;
            if SPECIFICS.Has_Vector_Scalar_Instructions               then Put_Line(Localize("Has VSI"));       end if;
            if SPECIFICS.Has_Altivec_Additional_Registers             then Put_Line(Localize("Has VMX128"));    end if;
            if SPECIFICS.Has_Altivec                                  then Put_Line(Localize("Has Altivec"));   end if;
          when Intel_Vendor | Advanced_Micro_Devices_Vendor =>
            if SPECIFICS.Vendor = Advanced_Micro_Devices_Vendor then
              if SPECIFICS.Has_3DNow                                  then Put_Line(Localize("Has 3DNow!"));    end if;
              if SPECIFICS.Has_3DNow_Supplement                       then Put_Line(Localize("Has 3DNow!+"));   end if;
              if SPECIFICS.Has_Streaming_SIMD_Extensions_4_Supplement then Put_Line(Localize("Has SSE4a"));     end if;
              if SPECIFICS.Has_Multi_Media_Extensions_Supplement      then Put_Line(Localize("Has MMX+"));      end if;
            end if;
            if SPECIFICS.Has_Context_ID_Manager                       then Put_Line(Localize("Has INVPCID"));   end if;
            if SPECIFICS.Has_Population_Count                         then Put_Line(Localize("Has POPCNT"));    end if;
            if SPECIFICS.Has_Leading_Zero_Count                       then Put_Line(Localize("Has LZCNT"));     end if;
            if SPECIFICS.Has_Carryless_Multiplication_Of_Two_64_Bit   then Put_Line(Localize("Has PCLMULQDQ")); end if;
            if SPECIFICS.Has_Extended_States_Enabled                  then Put_Line(Localize("Has OSXSAVE"));   end if;
            if SPECIFICS.Has_Half_Precision_Floating_Point_Convert    then Put_Line(Localize("Has F16C"));      end if;
            if SPECIFICS.Has_High_Precision_Convert                   then Put_Line(Localize("Has CVT16"));     end if;
            if SPECIFICS.Has_Advanced_Encryption_Service              then Put_Line(Localize("Has AES"));       end if;
            if SPECIFICS.Has_Advanced_State_Operations                then Put_Line(Localize("Has FXSR"));      end if;
            if SPECIFICS.Has_Extended_Operation_Support               then Put_Line(Localize("Has XOP"));       end if;
            if SPECIFICS.Has_Hyperthreading                           then Put_Line(Localize("Has HTT"));       end if;
            if SPECIFICS.Has_Conditional_Move                         then Put_Line(Localize("Has CMOV"));      end if;
            if SPECIFICS.Has_Multi_Media_Extensions                   then Put_Line(Localize("Has MMX"));       end if;
            if SPECIFICS.Has_Fused_Multiply_Add_3                     then Put_Line(Localize("Has FMA3"));      end if;
            if SPECIFICS.Has_Fused_Multiply_Add_4                     then Put_Line(Localize("Has FMA4"));      end if;
            if SPECIFICS.Has_Streaming_SIMD_Extensions_1              then Put_Line(Localize("Has SSE"));       end if;
            if SPECIFICS.Has_Streaming_SIMD_Extensions_2              then Put_Line(Localize("Has SSE2"));      end if;
            if SPECIFICS.Has_Streaming_SIMD_Extensions_3              then Put_Line(Localize("Has SSE3"));      end if;
            if SPECIFICS.Has_Streaming_SIMD_Extensions_3_Supplement   then Put_Line(Localize("Has SSSE3"));     end if;
            if SPECIFICS.Has_Streaming_SIMD_Extensions_4_1            then Put_Line(Localize("Has SSE4.1"));    end if;
            if SPECIFICS.Has_Streaming_SIMD_Extensions_4_2            then Put_Line(Localize("Has SSE4.2"));    end if;
            if SPECIFICS.Has_Bit_Manipulation_Extensions_1            then Put_Line(Localize("Has BMI1"));      end if;
            if SPECIFICS.Has_Bit_Manipulation_Extensions_2            then Put_Line(Localize("Has BMI2"));      end if;
            if SPECIFICS.Has_Advanced_Vector_Extensions_1             then Put(Localize("Has AVX"));
              if not SPECIFICS.Has_Advanced_Vector_Extensions_Enabled then Put_Line(Localize(", but it's disabled")); else New_Line; end if;
            end if;
            if SPECIFICS.Has_Advanced_Vector_Extensions_2             then Put(Localize("Has AVX2"));
              if not SPECIFICS.Has_Advanced_Vector_Extensions_Enabled then Put_Line(Localize(", but it's disabled")); else New_Line; end if;
            end if;
          when others => null;
        end case;
      exception when Unsupported => Put_Debug_Line(Localize("Unsupported feature!"));
      end Test;
    package body Import is separate;
    procedure Put_Stack                                            renames Import.Put_Stack;
    procedure Clear_Stack                                          renames Import.Clear_Stack;
    procedure Check_Exceptions                                     renames Import.Check_Exceptions;
    procedure Set_Rounding   (Rounding  : in Enumerated_Rounding)  renames Import.Set_Rounding;
    procedure Set_Precision  (Precision : in Enumerated_Precision) renames Import.Set_Precision;
    function Get_Clock_Ticks return Integer_8_Unsigned             renames Import.Get_Clock_Ticks;
    function Is_Stack_Empty  return Boolean                        renames Import.Is_Stack_Empty;
    function Get_Specifics return Record_Specifics is
      Specifics : Record_Specifics := Import.Get_Specifics;
      begin
        Specifics.Speed_In_Megahertz := Get_Clock_Ticks;
        delay SECONDS_FOR_PROCESSOR_SPEED_TIMING;
        Specifics.Speed_In_Megahertz := (Get_Clock_Ticks - Specifics.Speed_In_Megahertz) * Integer_8_Unsigned(1.0 / SECONDS_FOR_PROCESSOR_SPEED_TIMING);
        return Specifics;
      end Get_Specifics;
  end Neo.System.Processor;