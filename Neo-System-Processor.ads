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
  Neo.Foundation.Data_Types,
  Neo.Foundation.Build_Options,
  Neo.Foundation.Package_Testing;
use
  Neo.Foundation.Data_Types,
  Neo.Foundation.Build_Options,
  Neo.Foundation.Package_Testing;
package Neo.System.Processor
  is
  ----------------
  -- Exceptions --
  ----------------
    Invalid_Operation    : Exception;
    Denormalized_Operand : Exception;
    Divide_By_Zero       : Exception;
    Numeric_Overflow     : Exception;
    Numeric_Underflow    : Exception;
    Inexact_Result       : Exception;
    Stack_Fault          : Exception;
  ------------------
  -- Enumerations --
  ------------------
    type Enumerated_Vendor
      is(
      Generic_Vendor,
      Intel_Vendor,
      Apple_IBM_Motorola_Vendor,
      Advanced_Micro_Devices_Vendor);
    type Enumerated_Precision
      is(
      Single_Precision,           -- 24 bits
      Double_Precision,           -- 53 bits
      Double_Extended_Precision); -- 64 bits
    type Enumerated_Rounding
      is(
      Nearest_Rounding,
      Truncate_Rounding,
      Down_Rounding,
      Up_Rounding);
  -------------
  -- Records --
  -------------
    type Record_Extensions
      is record
        Has_Altivec                                : Boolean := False;
        Has_3DNow                                  : Boolean := False;
        Has_3DNow_Supplement                       : Boolean := False;
        Has_Multi_Media_Extensions                 : Boolean := False;
        Has_Multi_Media_Extensions_Supplement      : Boolean := False;
        Has_Streaming_SIMD_Extensions_1            : Boolean := False;
        Has_Streaming_SIMD_Extensions_2            : Boolean := False;
        Has_Streaming_SIMD_Extensions_3            : Boolean := False;
        Has_Streaming_SIMD_Extensions_3_Supplement : Boolean := False;
        Has_Streaming_SIMD_Extensions_4_1          : Boolean := False;
        Has_Streaming_SIMD_Extensions_4_2          : Boolean := False;
        Has_Streaming_SIMD_Extensions_4_Supplement : Boolean := False;
        Has_Carryless_Multiplication_Of_Two_64_Bit : Boolean := False;
        Has_Advanced_Vector_Extensions_Enabled     : Boolean := False;
        Has_Advanced_Vector_Extensions_1           : Boolean := False;
        Has_Advanced_Vector_Extensions_2           : Boolean := False;
        Has_Advanced_Encryption_Service            : Boolean := False;
        Has_Advanced_State_Operations              : Boolean := False;
        Has_Bit_Manipulation_Extensions_1          : Boolean := False;
        Has_Bit_Manipulation_Extensions_2          : Boolean := False;
        Has_Fused_Multiply_Add_3                   : Boolean := False;
        Has_Fused_Multiply_Add_4                   : Boolean := False;
        Has_Hyperthreading                         : Boolean := False;
        Has_High_Precision_Convert                 : Boolean := False;
        Has_Half_Precision_Floating_Point_Convert  : Boolean := False;
        Has_Extended_States_Enabled                : Boolean := False;
        Has_Population_Count                       : Boolean := False;
        Has_Context_ID_Manager                     : Boolean := False;
        Has_Conditional_Move                       : Boolean := False;
        Has_Leading_Zero_Count                     : Boolean := False;
        Has_Extended_Operation_Support             : Boolean := False;
      end record;
  -----------------
  -- Subprograms --
  -----------------
    procedure Initialize;
    procedure Test;
    procedure Check_Exceptions;
    procedure Clear_Stack;
    function Is_Stack_Empty
      return Boolean;
    procedure Put_Stack;
    procedure Put_Trace;
    procedure Set_Rounding(
      Rounding : in Enumerated_Rounding);
    procedure Set_Precision(
      Precision : in Enumerated_Precision);
    function Get_Number_Of_Cores
      return Integer_8_Unsigned;
    function Get_Speed_In_Megahertz
      return Integer_8_Unsigned;
    function Get_Vendor
      return Enumerated_Vendor;
    function Get_Extensions
      return Record_Extensions;
    function Get_Clock_Ticks
      return Integer_8_Unsigned;
-------
private
-------
  ---------------
  -- Constants --
  ---------------
    TRACE_LIMIT : constant Integer_4_Signed := 1_000;
  --------------------
  -- Implementation --
  --------------------
    package Implementation_For_Compiler
      is
        procedure Put_Trace;
      end Implementation_For_Compiler;
    package Implementation_For_Operating_System
      is
        function Get_Clock_Ticks
          return Integer_8_Unsigned;
        function Get_Number_of_Cores
          return Integer_8_Unsigned;
        function Get_Speed_In_Megahertz
          return Integer_8_Unsigned;
      end Implementation_For_Operating_System;
    package Implementation_For_Architecture
      is
        procedure Initialize;
        function Get_Vendor
          return Enumerated_Vendor;
        function Get_Extensions
          return Record_Extensions;
        function Get_Number_of_Cores
          return Integer_8_Unsigned;
        function Get_Speed_In_Megahertz
          return Integer_8_Unsigned;
        procedure Check_Exceptions;
        procedure Set_Rounding(
          Rounding  : in Enumerated_Rounding);
        procedure Set_Precision(
          Precision : in Enumerated_Precision);
        function Get_Clock_Ticks
          return Integer_8_Unsigned;
        procedure Put_Stack;
        procedure Put_Trace;
        function Is_Stack_Empty
          return Boolean;
        procedure Clear_Stack;
      end Implementation_For_Architecture;
  end Neo.System.Processor;
