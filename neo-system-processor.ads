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
  System.Multiprocessors,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Build_Options,
  Neo.Foundation.Package_Testing;
USE
  System.Multiprocessors,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Build_Options,
  Neo.Foundation.Package_Testing;
PACKAGE Neo.System.Processor
  IS
  ----------------
  -- EXCEPTIONS --
  ----------------
    Invalid_Operation    : Exception;
    Denormalized_Operand : Exception;
    Divide_By_Zero       : Exception;
    Numeric_Overflow     : Exception;
    Numeric_Underflow    : Exception;
    Inexact_Result       : Exception;
    Stack_Fault          : Exception;
  ------------------
  -- ENUMERATIONS --
  ------------------
    TYPE Enumerated_Vendor
      IS(
      Generic_Vendor,
      ARM_Licenced_Vendor,
      Intel_Vendor,
      Apple_IBM_Motorola_Vendor,
      Advanced_Micro_Devices_Vendor);
    TYPE Enumerated_Precision
      IS(
      Single_Precision,           -- 24 bits
      Double_Precision,           -- 53 bits
      Double_Extended_Precision); -- 64 bits
    TYPE Enumerated_Rounding
      IS(
      Up_Rounding,
      Down_Rounding,
      Nearest_Rounding,
      Truncate_Rounding);
  -------------
  -- RECORDS --
  -------------
    TYPE Record_Specifics(
      Vendor : Enumerated_Vendor)
      IS RECORD
        CASE Vendor IS
          WHEN ARM_Licenced_Vendor =>
            Has_NEON                                       : Boolean := False;
            Has_Vector_Floating_Point                      : Boolean := False;
          WHEN Apple_IBM_Motorola_Vendor =>
            Has_Vector_Multimedia_Instructions             : Boolean := False;
            Has_Vector_Scalar_Instructions                 : Boolean := False;
            Has_Altivec_Additional_Registers               : Boolean := False;
            Has_Altivec                                    : Boolean := False;
          WHEN Intel_Vendor | Advanced_Micro_Devices_Vendor =>
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
            CASE Vendor IS
              WHEN Advanced_Micro_Devices_Vendor =>
                Has_Streaming_SIMD_Extensions_4_Supplement : Boolean := False;
                Has_3DNow                                  : Boolean := False;
                Has_3DNow_Supplement                       : Boolean := False;
                Has_Multi_Media_Extensions_Supplement      : Boolean := False;
              WHEN OTHERS =>
                NULL;
            END CASE;
          WHEN OTHERS =>
            NULL;
        END CASE;
      END RECORD;
  -----------------
  -- SUBPROGRAMS --
  -----------------
    PROCEDURE Initialize
      WITH Inline;
    PROCEDURE Test
      WITH Inline;
    PROCEDURE Check_Exceptions
      WITH Inline;
    PROCEDURE Clear_Stack
      WITH Inline;
    FUNCTION Is_Stack_Empty
      RETURN Boolean
      WITH Inline;
    PROCEDURE Put_Stack
      WITH Inline;
    PROCEDURE Put_Trace
      WITH Inline;
    PROCEDURE Set_Rounding(
      Rounding : IN Enumerated_Rounding)
      WITH Inline;
    PROCEDURE Set_Precision(
      Precision : IN Enumerated_Precision)
      WITH Inline;
    FUNCTION Get_Number_Of_Cores
      RETURN Integer_8_Unsigned
      WITH Inline;
    FUNCTION Get_Speed_In_Megahertz
      RETURN Integer_8_Unsigned
      WITH Inline;
    FUNCTION Get_Vendor
      RETURN Enumerated_Vendor
      WITH Inline;
    FUNCTION Get_Specifics
      RETURN Record_Specifics
      WITH Inline;
    FUNCTION Get_Clock_Ticks
      RETURN Integer_8_Unsigned
      WITH Inline;
-------
PRIVATE
-------
  ---------------
  -- CONSTANTS --
  ---------------
    TRACE_LIMIT : CONSTANT Integer_4_Signed := 1_000;
  --------------------
  -- IMPLEMENTATION --
  --------------------
    PACKAGE Implementation_For_Compiler
      IS
        PROCEDURE Put_Trace
          WITH Inline;
      END Implementation_For_Compiler;
    PACKAGE Implementation_For_Operating_System
      IS
        FUNCTION Get_Clock_Ticks
          RETURN Integer_8_Unsigned
          WITH Inline;
        FUNCTION Get_Number_of_Cores
          RETURN Integer_8_Unsigned
          WITH Inline;
        FUNCTION Get_Speed_In_Megahertz
          RETURN Integer_8_Unsigned
          WITH Inline;
      END Implementation_For_Operating_System;
    PACKAGE Implementation_For_Architecture
      IS
        PROCEDURE Initialize
          WITH Inline;
        FUNCTION Get_Vendor
          RETURN Enumerated_Vendor
          WITH Inline;
        FUNCTION Get_Specifics
          RETURN Record_Specifics
          WITH Inline;
        FUNCTION Get_Number_of_Cores
          RETURN Integer_8_Unsigned
          WITH Inline;
        PROCEDURE Check_Exceptions
          WITH Inline;
        PROCEDURE Set_Rounding(
          Rounding : IN Enumerated_Rounding)
          WITH Inline;
        PROCEDURE Set_Precision(
          Precision : IN Enumerated_Precision)
          WITH Inline;
        FUNCTION Get_Clock_Ticks
          RETURN Integer_8_Unsigned
          WITH Inline;
        PROCEDURE Put_Stack
          WITH Inline;
        PROCEDURE Put_Trace
          WITH Inline;
        FUNCTION Is_Stack_Empty
          RETURN Boolean
          WITH Inline;
        PROCEDURE Clear_Stack
          WITH Inline;
      END Implementation_For_Architecture;
  END Neo.System.Processor;
