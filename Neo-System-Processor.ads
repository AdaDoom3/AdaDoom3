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
  System.Machine_Code,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Build_Options,
  Neo.Foundation.Package_Testing,
  Neo.Foundation.Package_Controller;
use
  Interfaces,
  System.Machine_Code,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Build_Options,
  Neo.Foundation.Package_Testing;
package Neo.System.Processor
  is
  ---------------
  -- Constants --
  ---------------
    CALLSTACK_TRACE_LIMIT : constant Integer_4_Signed := 1_000;
  ----------------
  -- Exceptions --
  ----------------
    Primary_Implementation_Failure : Exception;
    Invalid_Operation              : Exception;
    Denormalized_Operand           : Exception;
    Divide_By_Zero                 : Exception;
    Numeric_Overflow               : Exception;
    Numeric_Underflow              : Exception;
    Inexact_Result                 : Exception;
    Stack_Fault                    : Exception;
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
    type Record_Processor
      is record
        Vendor                                     : Enumerated_Vendor    := Generic_Vendor;
        Speed_In_Megahertz                         : Integer_8_Positive   := 1;
        Number_Of_Logical_Processors               : Integer_8_Positive   := 1;
        Rounding                                   : Enumerated_Rounding  := Nearest_Rounding;
        Precision                                  : Enumerated_Precision := Double_Extended_Precision;
        Has_Altivec                                : Boolean              := False;
        Has_3DNow                                  : Boolean              := False;
        Has_3DNow_Supplement                       : Boolean              := False;
        Has_Multi_Media_Extensions                 : Boolean              := False;
        Has_Multi_Media_Extensions_Supplement      : Boolean              := False;
        Has_Streaming_SIMD_Extensions_1            : Boolean              := False;
        Has_Streaming_SIMD_Extensions_2            : Boolean              := False;
        Has_Streaming_SIMD_Extensions_3            : Boolean              := False;
        Has_Streaming_SIMD_Extensions_3_Supplement : Boolean              := False;
        Has_Streaming_SIMD_Extensions_4_1          : Boolean              := False;
        Has_Streaming_SIMD_Extensions_4_2          : Boolean              := False;
        Has_Streaming_SIMD_Extensions_4_Supplement : Boolean              := False;
        Has_Carryless_Multiplication_Of_Two_64_Bit : Boolean              := False;
        Has_Advanced_Vector_Extensions_Enabled     : Boolean              := False;
        Has_Advanced_Vector_Extensions_1           : Boolean              := False;
        Has_Advanced_Vector_Extensions_2           : Boolean              := False;
        Has_Advanced_Encryption_Service            : Boolean              := False;
        Has_Advanced_State_Operations              : Boolean              := False;
        Has_Bit_Manipulation_Extensions_1          : Boolean              := False;
        Has_Bit_Manipulation_Extensions_2          : Boolean              := False;
        Has_Fused_Multiply_Add_3                   : Boolean              := False;
        Has_Fused_Multiply_Add_4                   : Boolean              := False;
        Has_Hyperthreading                         : Boolean              := False;
        Has_High_Precision_Convert                 : Boolean              := False;
        Has_Half_Precision_Floating_Point_Convert  : Boolean              := False;
        Has_Processor_Extended_States_Enabled      : Boolean              := False;
        Has_Population_Count                       : Boolean              := False;
        Has_Context_ID_Manager                     : Boolean              := False;
        Has_Conditional_Move                       : Boolean              := False;
        Has_Leading_Zero_Count                     : Boolean              := False;
        Has_Denormals_Are_Zero                     : Boolean              := False;
        Has_Flush_To_Zero                          : Boolean              := False;
        Has_Extended_Operation_Support             : Boolean              := False;
      end record;
  -----------------
  -- Subprograms --
  -----------------
    procedure Initialize;
    procedure Test;
    procedure Put;
    procedure Put_Call_Stack;
    procedure Set(
      Rounding  : in Enumerated_Rounding;
      Precision : in Enumerated_Precision);
    procedure Set_Rounding(
      Rounding : in Enumerated_Rounding);
    procedure Set_Precision(
      Precision : in Enumerated_Precision);
    function Get
      return Record_Processor;
    function Get_Clock_Ticks
      return Integer_8_Unsigned;
    function Is_Stack_Empty
      return Boolean;
    procedure Check_Exceptions;
    procedure Clear_Stack;
-------
private
-------
  --------------------
  -- Implementation --
  --------------------
    package Implementation_For_Operating_System
      is
        function Get_Clock_Ticks
          return Integer_8_Natural;
        function Get_Number_Of_Processors
          return Integer_8_Positive;
        function Get_Speed_In_Megahertz(
          return Integer_8_Positive;
      end Implementation_For_Operating_System;
    package Implementation_For_Compiler
      is
        function Put_Call_Stack
          return Boolean;
      end Implementation_For_Compiler;
    generic
      with
        function Get_Clock_Ticks
          return Integer_8_Natural;
      with
        function Get_Number_Of_Processors
          return Integer_8_Positive;
      with
        function Get_Speed_In_Megahertz(
          return Integer_8_Positive;
      with
        function Put_Call_Stack
          return Boolean;
    package Implementation
      is
        function Initialize
          return Record_Processor;
        procedure Check_Exceptions(
          Processor : in Record_Processor);
        function Set_Rounding(
          Processor : in Record_Processor;
          Rounding  : in Enumerated_Rounding)
          return Record_Processor;
        function Set_Precision(
          Processor : in Record_Processor;
          Precision : in Enumerated_Precision)
          return Record_Processor;
        function Get_Clock_Ticks
          return Integer_8_Unsigned;
        procedure Put_State(
          Processor : in Record_Processor);
        procedure Put_Call_Stack;
        function Is_Stack_Empty
          return Boolean;
        procedure Clear_Stack;
      end Implementation;
    package body Implementation_For_Operating_System
      is separate;
    package body Implementation_For_Compiler
      is separate;
    package body Implementation
      is separate;
    package Instantiated_Implementation
      is new Implementation(
        Get_Clock_Ticks          => Implementation_For_Operating_System.Get_Clock_Ticks,
        Get_Number_Of_Processors => Implementation_For_Operating_System.Get_Number_Of_Processors,
        Get_Speed_In_Gigahertz   => Implementation_For_Operating_System.Get_Speed_In_Gigahertz,
        Put_Call_Stack           => Implementation_For_Compiler.Put_Call_Stack);
  --------------
  -- Packages --
  --------------
    package Protected_Record_Processor
      is new Neo.Foundation.Generic_Protected(Record_Processor);
  ---------------
  -- Variables --
  ---------------
    Protected_Data : Protected_Record_Processor.Data;
  end Neo.System.Processor;