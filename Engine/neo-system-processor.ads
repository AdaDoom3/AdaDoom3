with Interfaces;          use Interfaces;
with Interfaces.C;        use Interfaces.C;
with System;              use System;
with System.Machine_Code; use System.Machine_Code;
package Neo.System.Processor is
  pragma Suppress(Elaboration_Check);
  Denormalized_Operand : Exception;
  Invalid_Operation    : Exception;
  Numeric_Underflow    : Exception;
  Numeric_Overflow     : Exception;
  Divide_By_Zero       : Exception;
  Inexact_Result       : Exception;
  Stack_Fault          : Exception;
  type Enumerated_Precision is (Single_Precision, Double_Precision, Double_Extended_Precision); for Enumerated_Precision use (24, 53, 64);
  type Enumerated_Rounding  is (Up_Rounding, Down_Rounding, Nearest_Rounding, Truncate_Rounding);
  type Enumerated_Vendor    is (Unknown_Vendor,
    Intel_Vendor,
    -- 1985 x86, x86-64
    --      http://web.archive.org/web/20130402202112/http://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-software-developer-vol-2a-manual.pdf
    Advanced_Micro_Devices_Vendor,
    -- 1985 x86, x86-64
    --      http://web.archive.org/web/20130123012528/http://developer.amd.com/resources/documentation-articles/developer-guides-manuals/
    Advanced_RISC_Machines_Vendor,
    -- 1985 ARM
    --      http://web.archive.org/web/20081030041403/http://infocenter.arm.com/help/index.jsp
    Apple_IBM_Motorola_Vendor);
    -- 1992 PowerPC
    --      http://web.archive.org/web/20110811041906/https://www-01.ibm.com/chips/techlib/techlib.nsf/techdocs/852569B20050FF778525699600741775/$file/prg.pdf
  type Record_Specifics(Vendor : Enumerated_Vendor) is record
      Speed_In_Megahertz : Integer_8_Unsigned := 0;
      case Vendor is
        when Advanced_RISC_Machines_Vendor =>
          Has_NEON                                       : Boolean := False;
          Has_Vector_Floating_Point                      : Boolean := False;
        when Apple_IBM_Motorola_Vendor =>
          Has_Vector_Multimedia_Instructions             : Boolean := False;
          Has_Vector_Scalar_Instructions                 : Boolean := False;
          Has_Altivec_Additional_Registers               : Boolean := False;
          Has_Altivec                                    : Boolean := False;
        when Intel_Vendor | Advanced_Micro_Devices_Vendor =>
          Has_Multi_Media_Extensions                     : Boolean := False;
          Has_Streaming_SIMD_Extensions_1                : Boolean := False;
          Has_Streaming_SIMD_Extensions_2                : Boolean := False;
          Has_Streaming_SIMD_Extensions_3                : Boolean := False;
          Has_Streaming_SIMD_Extensions_3_Supplement     : Boolean := False;
          Has_Streaming_SIMD_Extensions_4_1              : Boolean := False;
          Has_Streaming_SIMD_Extensions_4_2              : Boolean := False;
          Has_Carryless_Multiplication_Of_Two_64_Bit     : Boolean := False;
          Has_Advanced_Vector_Extensions_Enabled         : Boolean := False;
          Has_Advanced_Vector_Extensions_1               : Boolean := False;
          Has_Advanced_Vector_Extensions_2               : Boolean := False;
          Has_Advanced_Encryption_Service                : Boolean := False;
          Has_Advanced_State_Operations                  : Boolean := False;
          Has_Bit_Manipulation_Extensions_1              : Boolean := False;
          Has_Bit_Manipulation_Extensions_2              : Boolean := False;
          Has_Fused_Multiply_Add_3                       : Boolean := False;
          Has_Fused_Multiply_Add_4                       : Boolean := False;
          Has_Hyperthreading                             : Boolean := False;
          Has_High_Precision_Convert                     : Boolean := False;
          Has_Half_Precision_Floating_Point_Convert      : Boolean := False;
          Has_Extended_States_Enabled                    : Boolean := False;
          Has_Population_Count                           : Boolean := False;
          Has_Context_ID_Manager                         : Boolean := False;
          Has_Conditional_Move                           : Boolean := False;
          Has_Leading_Zero_Count                         : Boolean := False;
          Has_Extended_Operation_Support                 : Boolean := False;
          case Vendor is
            when Advanced_Micro_Devices_Vendor =>
              Has_Streaming_SIMD_Extensions_4_Supplement : Boolean := False;
              Has_3DNow                                  : Boolean := False;
              Has_3DNow_Supplement                       : Boolean := False;
              Has_Multi_Media_Extensions_Supplement      : Boolean := False;
            when others => null;
          end case;
        when Unknown_Vendor => null;
      end case;
    end record;
  procedure Put_Stack;
  procedure Clear_Stack;
  procedure Check_Exceptions;
  procedure Set_Rounding   (Rounding  : in Enumerated_Rounding);
  procedure Set_Precision  (Precision : in Enumerated_Precision);
  function Get_Specifics   return Record_Specifics; -- Delays SECONDS_FOR_PROCESSOR_SPEED_TIMING
  function Get_Clock_Ticks return Integer_8_Unsigned;
  function Is_Stack_Empty  return Boolean;
  SPECIFICS : constant Neo.System.Processor.Record_Specifics := Get_Specifics;
private
  SECONDS_FOR_PROCESSOR_SPEED_TIMING : constant Duration            := 0.1;
  REQUIREMENTS_FOR_AVX               : constant Record_Requirements := (Linux_3_System, Windows_2_6_1_System, Macintosh_10_6_System);
  package Import is
      procedure Put_Stack;
      procedure Clear_Stack;
      procedure Check_Exceptions;
      procedure Set_Rounding   (Rounding  : in Enumerated_Rounding);
      procedure Set_Precision  (Precision : in Enumerated_Precision);
      function Get_Specifics   return Record_Specifics; -- Ignore setting Speed_In_Megahertz
      function Get_Clock_Ticks return Integer_8_Unsigned;
      function Is_Stack_Empty  return Boolean;
    end Import;
end Neo.System.Processor;
