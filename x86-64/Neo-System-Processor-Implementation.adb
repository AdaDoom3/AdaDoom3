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
  Interfaces,
  Interfaces.C,
  System,
  System.Machine_Code;
USE
  Interfaces,
  Interfaces.C,
  System,
  System.Machine_Code;
SEPARATE(Neo.System.Processor)
PACKAGE BODY Implementation_For_Architecture
  IS
  ----------------
  -- EXCEPTIONS --
  ----------------
    CPUID_Is_Not_Supported : Exception;
  -------------
  -- NUMBERS --
  -------------
    TYPE Integer_Bit_Field_Element
      IS NEW Integer_4_Signed
      RANGE 0..31;
  ------------------
  -- ENUMERATIONS --
  ------------------
    TYPE Enumerated_Register
      IS(
      EAX_Register,
      EBX_Register,
      ECX_Register,
      EDX_Register);
  -------------
  -- RECORDS --
  -------------
    TYPE Record_Value
      IS RECORD
        Function_ID : Integer_4_Unsigned        := 0;
        Register    : Enumerated_Register       := EAX_Register;
        Start_Bit   : Integer_Bit_Field_Element := 0;
        End_Bit     : Integer_Bit_Field_Element := 0;
      END RECORD;
    TYPE Record_Feature
      IS RECORD
        Function_ID : Integer_4_Unsigned        := 0;
        Register    : Enumerated_Register       := EAX_Register;
        Bit         : Integer_Bit_Field_Element := 0;
      END RECORD;
    TYPE Record_x86_Environment
      IS RECORD
        Control_Word    : Integer_2_Unsigned := 0;
        Unused_A        : Integer_2_Unsigned := 0;
        Status_Word     : Integer_2_Unsigned := 0;
        Unused_B        : Integer_2_Unsigned := 0;
        Tags            : Integer_2_Unsigned := 0;
        Unused_C        : Integer_2_Unsigned := 0;
        Program_Counter : Integer_4_Unsigned := 0;
        CS_Selector     : Integer_2_Unsigned := 0;
        Operation_Code  : Integer_2_Unsigned := 0; -- 11 bits + 5 unused
        Data_Offset     : Integer_4_Unsigned := 0;
        Data_Selector   : Integer_2_Unsigned := 0;
        Unused_D        : Integer_2_Unsigned := 0;
      END RECORD;
      FOR Record_x86_Environment'Size
        USE 224;
  ---------------
  -- CONSTANTS --
  ---------------
    DO_CRASH_ON_INEXACT_RESULT       : CONSTANT Boolean            := False;
    DO_CRASH_ON_DIVIDE_BY_ZERO       : CONSTANT Boolean            := False;
    DO_CRASH_ON_NUMERIC_OVERFLOW     : CONSTANT Boolean            := False;
    DO_CRASH_ON_INVALID_OPERATION    : CONSTANT Boolean            := False;
    DO_CRASH_ON_NUMERIC_UNDERFLOW    : CONSTANT Boolean            := False;
    DO_CRASH_ON_DENORMALIZED_OPERAND : CONSTANT Boolean            := False;
    TO_ESI                           : CONSTANT String_1           := "s";
    TO_EDI                           : CONSTANT String_1           := "d";
    TO_EAX                           : CONSTANT String_1           := "a";
    TO_EBX                           : CONSTANT String_1           := "b";
    TO_ECX                           : CONSTANT String_1           := "c";
    TO_EDX                           : CONSTANT String_1           := "d";
    FROM_EAX                         : CONSTANT String_1           := "=a";
    FROM_EBX                         : CONSTANT String_1           := "=b";
    FROM_ECX                         : CONSTANT String_1           := "=c";
    FROM_EDX                         : CONSTANT String_1           := "=d";
    VENDOR_INTEL                     : CONSTANT String_1           := "GenuineIntel";
    VENDOR_ADVANCED_MICRO_DEVICES    : CONSTANT String_1           := "AuthenticAMD";
    INTEL_SSE3                       : CONSTANT Record_Feature     := (16#0000_0001#, ECX_Register,  0);
    INTEL_PCLMULQDQ                  : CONSTANT Record_Feature     := (16#0000_0001#, ECX_Register,  1);
    INTEL_SSSE3                      : CONSTANT Record_Feature     := (16#0000_0001#, ECX_Register,  9);
    INTEL_FMA3                       : CONSTANT Record_Feature     := (16#0000_0001#, ECX_Register, 12);
    INTEL_SSE4_1                     : CONSTANT Record_Feature     := (16#0000_0001#, ECX_Register, 19);
    INTEL_SSE4_2                     : CONSTANT Record_Feature     := (16#0000_0001#, ECX_Register, 20);
    INTEL_POPCNT                     : CONSTANT Record_Feature     := (16#0000_0001#, ECX_Register, 23);
    INTEL_AES                        : CONSTANT Record_Feature     := (16#0000_0001#, ECX_Register, 25);
    INTEL_OSXSAVE                    : CONSTANT Record_Feature     := (16#0000_0001#, ECX_Register, 27);
    INTEL_AVX                        : CONSTANT Record_Feature     := (16#0000_0001#, ECX_Register, 28);
    INTEL_F16C                       : CONSTANT Record_Feature     := (16#0000_0001#, ECX_Register, 29);
    INTEL_CMOV                       : CONSTANT Record_Feature     := (16#0000_0001#, EDX_Register, 15);
    INTEL_MMX                        : CONSTANT Record_Feature     := (16#0000_0001#, EDX_Register, 23);
    INTEL_FXSR                       : CONSTANT Record_Feature     := (16#0000_0001#, EDX_Register, 24);
    INTEL_SSE                        : CONSTANT Record_Feature     := (16#0000_0001#, EDX_Register, 25);
    INTEL_SSE2                       : CONSTANT Record_Feature     := (16#0000_0001#, EDX_Register, 26);
    INTEL_HTT                        : CONSTANT Record_Feature     := (16#0000_0001#, EDX_Register, 28);
    INTEL_BMI1                       : CONSTANT Record_Feature     := (16#0000_0007#, EBX_Register,  3);
    INTEL_AVX2                       : CONSTANT Record_Feature     := (16#0000_0007#, EBX_Register,  5);
    INTEL_BMI2                       : CONSTANT Record_Feature     := (16#0000_0007#, EBX_Register,  8);
    INTEL_INVPCID                    : CONSTANT Record_Feature     := (16#0000_0007#, EBX_Register, 10);
    AMD_ABM                          : CONSTANT Record_Feature     := (16#8000_0001#, ECX_Register,  5);
    AMD_SSE4_A                       : CONSTANT Record_Feature     := (16#8000_0001#, ECX_Register,  6);
    AMD_XOP                          : CONSTANT Record_Feature     := (16#8000_0001#, ECX_Register, 11);
    AMD_FMA4                         : CONSTANT Record_Feature     := (16#8000_0001#, ECX_Register, 16);
    AMD_CVT16                        : CONSTANT Record_Feature     := (16#8000_0001#, ECX_Register, 19);
    AMD_MMX_PLUS                     : CONSTANT Record_Feature     := (16#8000_0001#, EDX_Register, 22);
    AMD_3DNOW_PLUS                   : CONSTANT Record_Feature     := (16#8000_0001#, EDX_Register, 30);
    AMD_3DNOW                        : CONSTANT Record_Feature     := (16#8000_0001#, EDX_Register, 31);
    AMD_CPU_COUNT                    : CONSTANT Record_Value       := (16#8000_0008#, ECX_Register,  0,  7); -- One plus this value
    INTEL_CPU_COUNT                  : CONSTANT Record_Value       := (16#0000_0001#, EBX_Register, 16, 23);
    INTEL_APIC                       : CONSTANT Record_Value       := (16#0000_0001#, EBX_Register, 24, 31);
    PROLOGUE_SIGNATURE               : CONSTANT Integer_4_Unsigned := 16#00EC_8B55#;
  -------------------
  -- Execute_CPUID --
  -------------------
    FUNCTION Execute_CPUID(
      Function_ID : IN Integer_4_Unsigned;
      Register    : IN Enumerated_Register)
      RETURN Integer_4_Unsigned
      IS
      A, B, C, D : Integer_4_Unsigned := 0;
      BEGIN
        Asm(
          Template => "cpuid",
          Volatile => True,
          Inputs   => Integer_4_Unsigned'Asm_Input(TO_EAX, Function_ID),
          Outputs  =>(
            Integer_4_Unsigned'Asm_Output(FROM_EAX, A),
            Integer_4_Unsigned'Asm_Output(FROM_EBX, B),
            Integer_4_Unsigned'Asm_Output(FROM_ECX, C),
            Integer_4_Unsigned'Asm_Output(FROM_EDX, D)));
        CASE Register IS
          WHEN EAX_Register =>
            RETURN A;
          WHEN EBX_Register =>
            RETURN B;
          WHEN ECX_Register =>
            RETURN C;
          WHEN EDX_Register =>
            RETURN D;
        END CASE;
      END Execute_CPUID;
  ----------------
  -- Is_Enabled --
  ----------------
    FUNCTION Is_Enabled(
      Feature : IN Record_Feature)
      RETURN Boolean
      IS
      BEGIN
        RETURN (Execute_CPUID(Feature.Function_ID, Feature.Register) AND 2**Integer(Feature.Bit)) /= 0;
      END Is_Enabled;
  ---------------
  -- Get_Value --
  ---------------
    FUNCTION Get_Value(
      Value : IN Record_Value)
      RETURN Integer_8_Unsigned
      IS
      BEGIN
        RETURN
          Integer_8_Unsigned(
            Shift_Right(
              Value =>
                Shift_Left(
                  Value  => Execute_CPUID(Value.Function_ID, Value.Register),
                  Amount => Integer(Integer_Bit_Field_Element'Last - Value.End_Bit)),
              Amount => Integer(Integer_Bit_Field_Element'Last - (Value.End_Bit - Value.Start_Bit))));
      END Get_Value;
  ----------------
  -- Get_Vendor --
  ----------------
    FUNCTION Get_Vendor
      RETURN Enumerated_Vendor
      IS
      TYPE Array_String_Segment
        IS ARRAY(1..4)
        OF Character;
      B, D, C : Array_String_Segment := (OTHERS => Ascii.Nul);
      BEGIN
        Asm(
          Template => "cpuid",
          Volatile => True,
          Inputs   => Integer_4_Unsigned'Asm_Input(TO_EAX, 0),
          Outputs  =>(
            Array_String_Segment'Asm_Output(FROM_EBX, B),
            Array_String_Segment'Asm_Output(FROM_ECX, C),
            Array_String_Segment'Asm_Output(FROM_EDX, D)));
        IF String_1(B) & String_1(D) & String_1(C) = VENDOR_INTEL THEN
          RETURN Intel_Vendor;
        END IF;
        RETURN Advanced_Micro_Devices_Vendor;
      END Get_Vendor;
  -------------------------
  -- Get_Number_Of_Cores --
  -------------------------
    FUNCTION Get_Number_Of_Cores
      RETURN Integer_8_Unsigned
      IS
      BEGIN
        IF Get_Vendor = Intel_Vendor THEN
          RETURN Get_Value(INTEL_CPU_COUNT); -- Inaccurate
        END IF;
        RETURN Get_Value(AMD_CPU_COUNT) + 1;
      END Get_Number_Of_Cores;
  ----------------------------
  -- Get_Speed_In_Megahertz --
  ----------------------------
    FUNCTION Get_Speed_In_Megahertz
      RETURN Integer_8_Unsigned
      IS
      Start : Integer_8_Unsigned := 0;
      BEGIN
        Start := Get_Clock_Ticks;
        DELAY 1.0;
        RETURN Get_Clock_Ticks - Start;
      END Get_Speed_In_Megahertz;
  ----------------
  -- Initialize --
  ----------------
    PROCEDURE Initialize
      IS
      Data : ALIASED Integer_4_Unsigned := 0;
      BEGIN
        IF NOT USE_64_BIT THEN
          Asm( -- Check FOR cpuid
            --------------------------------------------
            "   pushfl                    " & END_LINE &
            "   popl   %%eax              " & END_LINE &
            "   test   $0x00200000, %%eax " & END_LINE &
            "   jz     set21              " & END_LINE &
            "   and    $0xffdfffff, %%eax " & END_LINE &
            "   pushl  %%eax              " & END_LINE &
            "   popfl                     " & END_LINE &
            "   pushfl                    " & END_LINE &
            "   popl   %%eax              " & END_LINE &
            "   test   $0x00200000, %%eax " & END_LINE &
            "   jz     good               " & END_LINE &
            "   jmp    error              " & END_LINE &
            --------------------------------------------
            " set21:                      " & END_LINE &
            "   or     $0x00200000, %%eax " & END_LINE &
            "   pushl  %%eax              " & END_LINE &
            "   popfl                     " & END_LINE &
            "   pushfl                    " & END_LINE &
            "   popl   %%eax              " & END_LINE &
            "   test   $0x00200000, %%eax " & END_LINE &
            "   jnz    good               " & END_LINE &
            "   jmp    error              " & END_LINE &
            --------------------------------------------
            " error:                      " & END_LINE &
            "   movl   $0x00000000, %%eax " & END_LINE &
            "   jmp    done               " & END_LINE &
            --------------------------------------------
            " good:                       " & END_LINE &
            "   movl   $0x00000001, %%eax " & END_LINE &
            --------------------------------------------
            " done:                       " & END_LINE ,
            --------------------------------------------
            Volatile => True,
            Outputs  => Integer_4_Unsigned'Asm_Output(FROM_EAX, Data));
          IF Data = 0 THEN
            RAISE CPUID_Is_Not_Supported;
          END IF;
        END IF;
        IF Is_Enabled(INTEL_FXSR) THEN
          --------------------------
          Enable_Denormals_Are_Zero:
          --------------------------
            declare
            TYPE Array_Save_Area
              IS ARRAY(1..512)
              OF Integer_1_Unsigned;
              FOR Array_Save_Area'Alignment
                USE 16;
            Save_Area : ALIASED Array_Save_Area := (OTHERS => 0);
            BEGIN
              Asm(
                ----------------------------------------
                " fxsave (%%eax)          " & END_LINE &
                " movl   28(%%eax), %%ebx " & END_LINE ,
                ----------------------------------------
                Inputs   => Address'Asm_Input(TO_EAX, Save_Area'Address),
                Outputs  => Integer_4_Unsigned'Asm_Output(FROM_EBX, Data),
                Volatile => True);
              IF (Data AND 16#20#) /= 0 THEN
                Asm(
                  -----------------------------------------
                  " stmxcsr (%%eax)          " & END_LINE &
                  " movl    (%%eax), %%ebx   " & END_LINE &
                  " or      $0x40,   %%bx    " & END_LINE &
                  " movl    %%ebx,   (%%eax) " & END_LINE &
                  " ldmxcsr (%%eax)          " & END_LINE ,
                  -----------------------------------------
                  Inputs   => Address'Asm_Input(TO_EAX, Data'Address),
                  Volatile => True);
              END IF;
            END Enable_Denormals_Are_Zero;
          Asm(
            -----------------------------------------
            " stmxcsr (%%eax)          " & END_LINE &
            " movl    (%%eax), %%ebx   " & END_LINE &
            " or      $0x8000, %%ebx   " & END_LINE &
            " movl    %%ebx,   (%%eax) " & END_LINE &
            " ldmxcsr (%%eax)          " & END_LINE ,
            -----------------------------------------
            Inputs   => Address'Asm_Input(TO_EAX, Data'Address),
            Volatile => True);
        END IF;
        ---------------
        Set_Exceptions:
        ---------------
          declare
          Exception_Mask :         Integer_4_Unsigned := 0;
          Other_Data     : ALIASED Integer_2_Unsigned := 0;
          BEGIN
            IF NOT DO_CRASH_ON_INEXACT_RESULT THEN
              Exception_Mask := 16#0000_1000#;
            END IF;
            IF NOT DO_CRASH_ON_NUMERIC_UNDERFLOW THEN
              Exception_Mask := Exception_Mask OR 16#0000_0800#;
            END IF;
            IF NOT DO_CRASH_ON_NUMERIC_OVERFLOW THEN
              Exception_Mask := Exception_Mask OR 16#0000_0400#;
            END IF;
            IF NOT DO_CRASH_ON_DIVIDE_BY_ZERO THEN
              Exception_Mask := Exception_Mask OR 16#0000_0200#;
            END IF;
            IF NOT DO_CRASH_ON_DENORMALIZED_OPERAND THEN
              Exception_Mask := Exception_Mask OR 16#0000_0100#;
            END IF;
            IF NOT DO_CRASH_ON_INVALID_OPERATION THEN
              Exception_Mask := Exception_Mask OR 16#0000_0080#;
            END IF;
            IF Is_Enabled(INTEL_FXSR) THEN
              Asm(
                ---------------------------------------------
                " stmxcsr (%%eax)              " & END_LINE &
                " movl    (%%eax),     %%ebx   " & END_LINE &
                " and     $0xffffe07f, %%ebx   " & END_LINE &
                " or      %%ecx,       %%ebx   " & END_LINE &
                " movl    %%ebx,       (%%eax) " & END_LINE &
                " ldmxcsr (%%eax)              " & END_LINE ,
                ---------------------------------------------
                Volatile => True,
                Inputs   =>(
                  Address           'Asm_Input(TO_EAX, Data'Address),
                  Integer_4_Unsigned'Asm_Input(TO_ECX, Exception_Mask)));
            END IF;
            Exception_Mask := Shift_Right(Exception_Mask, 7); 
            Asm(
              ----------------------------------------
              " fnstcw (%%eax)          " & END_LINE &
              " movw   (%%eax), %%bx    " & END_LINE &
              " and    $0xffc0, %%bx    " & END_LINE &
              " or     %%cx,    %%bx    " & END_LINE &
              " movw   %%bx,    (%%eax) " & END_LINE &
              " fldcw  (%%eax)          " & END_LINE ,
              ----------------------------------------
              Volatile => True,
              Inputs   =>(
                Address           'Asm_Input(TO_EAX, Other_Data'Address),
                Integer_4_Unsigned'Asm_Input(TO_ECX, Exception_Mask)));
          END Set_Exceptions;
      END Initialize;
  --------------------
  -- Get_Extensions --
  --------------------
    FUNCTION Get_Extensions
      RETURN Record_Extensions
      IS
      Extensions :         Record_Extensions  := (OTHERS => <>);
      Data       : ALIASED Integer_4_Unsigned := 0;
      BEGIN
        IF Get_Vendor = Advanced_Micro_Devices_Vendor THEN
          Extensions.Has_3DNow                                  := Is_Enabled(AMD_3DNOW);
          Extensions.Has_3DNow_Supplement                       := Is_Enabled(AMD_3DNOW_PLUS);
          Extensions.Has_Multi_Media_Extensions_Supplement      := Is_Enabled(AMD_MMX_PLUS);
          Extensions.Has_Streaming_SIMD_Extensions_4_Supplement := Is_Enabled(AMD_SSE4_A);
        END IF;
        Extensions.Has_Extended_States_Enabled                := Is_Enabled(INTEL_OSXSAVE);
        Extensions.Has_Population_Count                       := Is_Enabled(INTEL_POPCNT);
        Extensions.Has_Multi_Media_Extensions                 := Is_Enabled(INTEL_MMX);
        Extensions.Has_Streaming_SIMD_Extensions_1            := Is_Enabled(INTEL_SSE);
        Extensions.Has_Streaming_SIMD_Extensions_2            := Is_Enabled(INTEL_SSE2);
        Extensions.Has_Streaming_SIMD_Extensions_3            := Is_Enabled(INTEL_SSE3);
        Extensions.Has_Streaming_SIMD_Extensions_3_Supplement := Is_Enabled(INTEL_SSSE3);
        Extensions.Has_Streaming_SIMD_Extensions_4_1          := Is_Enabled(INTEL_SSE4_1);
        Extensions.Has_Streaming_SIMD_Extensions_4_2          := Is_Enabled(INTEL_SSE4_2);
        Extensions.Has_Carryless_Multiplication_Of_Two_64_Bit := Is_Enabled(INTEL_PCLMULQDQ);
        Extensions.Has_Bit_Manipulation_Extensions_1          := Is_Enabled(INTEL_BMI1);
        Extensions.Has_Bit_Manipulation_Extensions_2          := Is_Enabled(INTEL_BMI2);
        Extensions.Has_Advanced_Vector_Extensions_1           := Is_Enabled(INTEL_AVX);
        Extensions.Has_Advanced_Vector_Extensions_2           := Is_Enabled(INTEL_AVX2);
        Extensions.Has_Advanced_Encryption_Service            := Is_Enabled(INTEL_AES);
        Extensions.Has_Half_Precision_Floating_Point_Convert  := Is_Enabled(INTEL_F16C);
        Extensions.Has_Conditional_Move                       := Is_Enabled(INTEL_CMOV);
        Extensions.Has_Hyperthreading                         := Is_Enabled(INTEL_HTT);
        Extensions.Has_Advanced_State_Operations              := Is_Enabled(INTEL_FXSR);
        Extensions.Has_Fused_Multiply_Add_3                   := Is_Enabled(INTEL_FMA3);
        Extensions.Has_Fused_Multiply_Add_4                   := Is_Enabled(AMD_FMA4);
        Extensions.Has_High_Precision_Convert                 := Is_Enabled(AMD_CVT16);
        Extensions.Has_Extended_Operation_Support             := Is_Enabled(AMD_XOP);
        Extensions.Has_Leading_Zero_Count                     := Is_Enabled(AMD_ABM);
        IF
        Extensions.Has_Streaming_SIMD_Extensions_4_2 AND THEN
        Is_Newer_Than(Linux_2_7_System, Macintosh_10_6_System, Windows_2_6_1_System)
        THEN
          Asm( -- Check if the operating system will save the YMM registers
            Template => "xgetbv",
            Inputs   => Integer_4_Unsigned'Asm_Input(TO_ECX, 0),
            Outputs  => Integer_4_Unsigned'Asm_Output(FROM_EAX, Data),
            Volatile => True);
          Extensions.Has_Advanced_Vector_Extensions_Enabled := (Data AND 16#0000_0006#) > 0;
        END IF;
        RETURN Extensions;
      END Get_Extensions;
  ----------------------
  -- Check_Exceptions --
  ----------------------
    PROCEDURE Check_Exceptions
      IS
      Data          : ALIASED Integer_4_Unsigned := 0;
      Data_From_x87 : ALIASED Integer_2_Unsigned := 0;
      BEGIN
        IF Is_Enabled(INTEL_FXSR) THEN
          Asm(
            Template => "stmxcsr (%%eax)",
            Inputs   => Address'Asm_Input(TO_EAX, Data'Address),
            Volatile => True);
        END IF;
        Asm(
          Template => "fnstsw (%%eax)",
          Inputs   => Address'Asm_Input(TO_EAX, Data_From_x87'Address),
          Volatile => True);
        Data := (Data AND 16#0000_003F#) AND Integer_4_Unsigned(Data_From_x87);
        IF Data /= 0 THEN -- Raise the first error found
          ---------------------
          Clear_Exception_Bits:
          ---------------------
            declare
            Data_From_SIMD  :         Integer_4_Unsigned     := 0;
            x86_Environment : ALIASED Record_x86_Environment := (OTHERS => <>);
            BEGIN
              IF Is_Enabled(INTEL_FXSR) THEN
                Asm(
                  ---------------------------------------------
                  " stmxcsr (%%eax)              " & END_LINE &
                  " movl    (%%eax),     %%ebx   " & END_LINE &
                  " and     $0xffffffc0, %%ebx   " & END_LINE &
                  " movl    %%ebx,       (%%eax) " & END_LINE &
                  " ldmxcsr (%%eax)              " & END_LINE ,
                  ---------------------------------------------
                  Volatile => True,
                  Inputs   => Address'Asm_Input(TO_EAX, Data_From_SIMD'Address));
              END IF;
              Asm(
                Template => "fnstenv (%%eax)",
                Volatile => True,
                Inputs   => Address'Asm_Input(TO_EAX, x86_Environment'Address));
              -- Clear 6 exception bits plus stack fault
              x86_Environment.Status_Word := x86_Environment.Status_Word AND 16#FF80#; 
              Asm(
                Template => "fldenv (%%eax)",
                Volatile => True,
                Inputs   => Address'Asm_Input(TO_EAX, x86_Environment'Address));
              Asm(
                Template => "fnclex",
                Volatile => True); 
            END Clear_Exception_Bits;
          IF (Data AND 16#0000_0001#) /= 0 THEN
            RAISE Invalid_Operation;
          END IF;
          IF (Data AND 16#0000_0002#) /= 0 THEN
            RAISE Denormalized_Operand;
          END IF;
          IF (Data AND 16#0000_0004#) /= 0 THEN
            RAISE Divide_By_Zero;
          END IF;
          IF (Data AND 16#0000_0008#) /= 0 THEN
            RAISE Numeric_Overflow;
          END IF;
          IF (Data AND 16#0000_0010#) /= 0 THEN
            RAISE Numeric_Underflow;
          END IF;
          IF (Data AND 16#0000_0020#) /= 0 THEN
            RAISE Inexact_Result;
          END IF;
          IF (Data and 16#0000_0040#) /= 0 THEN
            RAISE Stack_Fault;
          END IF;
        END IF;
      END Check_Exceptions;
  ------------------
  -- Set_Rounding --
  ------------------
    PROCEDURE Set_Rounding(
      Rounding  : IN Enumerated_Rounding)
      IS
      Other_Data    : ALIASED Integer_2_Unsigned := 0;
      Data          : ALIASED Integer_4_Unsigned := 0;
      Rounding_Mask :         Integer_4_Unsigned := 0;
      BEGIN
        CASE Rounding IS
          WHEN Nearest_Rounding =>
            Rounding_Mask := 16#0000#;
          WHEN Down_Rounding =>
            Rounding_Mask := 16#2000#;
          WHEN Up_Rounding =>
            Rounding_Mask := 16#4000#;
          WHEN Truncate_Rounding =>
            Rounding_Mask := 16#6000#;
        END CASE;
        IF Is_Enabled(INTEL_FXSR) THEN
          Asm(
            ---------------------------------------------
            " stmxcsr (%%eax)              " & END_LINE &
            " movl    (%%eax),     %%ebx   " & END_LINE &
            " and     $0xffff9fff, %%ebx   " & END_LINE &
            " or      %%cx,        %%bx    " & END_LINE &
            " movl    %%ebx,       (%%eax) " & END_LINE &
            " ldmxcsr (%%eax)              " & END_LINE ,
            ---------------------------------------------
            Volatile => True,
            Inputs   =>(
              Address           'Asm_Input(TO_EAX, Data'Address),
              Integer_4_Unsigned'Asm_Input(TO_ECX, Rounding_Mask)));
        END IF;
        Rounding_Mask := Shift_Right(Rounding_Mask, 3); 
        Asm(
          ----------------------------------------
          " fnstcw (%%eax)          " & END_LINE &
          " movw   (%%eax), %%bx    " & END_LINE &
          " and    $0xf3ff, %%bx    " & END_LINE &
          " or     %%cx,    %%bx    " & END_LINE &
          " movw   %%bx,    (%%eax) " & END_LINE &
          " fldcw  (%%eax)          " & END_LINE ,
          ----------------------------------------
          Volatile => True,
          Inputs   =>(
            Address           'Asm_Input(TO_EAX, Other_Data'Address),
            Integer_4_Unsigned'Asm_Input(TO_ECX, Rounding_Mask)));
      END Set_Rounding;
  -------------------
  -- Set_Precision --
  -------------------
    PROCEDURE Set_Precision(
      Precision : IN Enumerated_Precision)
      IS
      Blank_Memory   : ALIASED Integer_2_Unsigned := 0;
      Precision_Mask :         Integer_4_Unsigned := 0;
      BEGIN
        CASE Precision IS
          WHEN Single_Precision =>
            Precision_Mask := 16#0000#;
          WHEN Double_Precision =>
            Precision_Mask := 16#0200#;
          WHEN Double_Extended_Precision =>
            Precision_Mask := 16#0300#;
        END CASE;
        Asm(
          ----------------------------------------
          " fnstcw (%%eax)          " & END_LINE &
          " movw   (%%eax), %%bx    " & END_LINE &
          " and    $0xfcff, %%bx    " & END_LINE &
          " or     %%cx,    %%bx    " & END_LINE &
          " movw   %%bx,    (%%eax) " & END_LINE &
          " fldcw  (%%eax)          " & END_LINE ,
          ----------------------------------------
          Volatile => True,
          Inputs   =>(
            Address           'Asm_Input(TO_EAX, Blank_Memory'Address),
            Integer_4_Unsigned'Asm_Input(TO_ECX, Precision_Mask)));
      END Set_Precision;
  --------------------
  -- Get_Clock_Tics --
  --------------------
    FUNCTION Get_Clock_Ticks
      RETURN Integer_8_Unsigned
      IS
      Low_Part  : Integer_4_Unsigned := 0;
      High_Part : Integer_4_Unsigned := 0;
      BEGIN
        Asm(
          ----------------------
          " cpuid " & END_LINE &
          " rdtsc " & END_LINE ,
          ----------------------
          Volatile => True,
          Outputs  =>(
            Integer_4_Unsigned'Asm_Output(FROM_EAX, Low_Part),
            Integer_4_Unsigned'Asm_Output(FROM_EDX, High_Part)));
        RETURN Shift_Left(Integer_8_Unsigned(High_Part), 32) + Integer_8_Unsigned(Low_Part);
      END Get_Clock_Ticks;
  --------------------
  -- Is_Stack_Empty --
  --------------------
    FUNCTION Is_Stack_Empty
      RETURN Boolean
      IS
      Result :         Integer_4_Unsigned     := 0;
      Data   : ALIASED Record_x86_Environment := (OTHERS => <>); 
      BEGIN
        Asm(
          ---------------------------------------------
          "   fnstenv (%%eax)            " & END_LINE &
          "   movl    8(%%eax),    %%eax " & END_LINE &
          "   xor     $0xffffffff, %%eax " & END_LINE &
          "   and     $0x0000ffff, %%eax " & END_LINE &
          "   jz      empty              " & END_LINE &
          "   movl    $0x00000000, %%eax " & END_LINE &
          "   jmp     finished           " & END_LINE &
          ---------------------------------------------
          " empty:                       " & END_LINE &
          "   movl    $0x00000001, %%eax " & END_LINE &
          ---------------------------------------------
          " finished:                    " & END_LINE ,
          ---------------------------------------------
          Volatile => True,
          Inputs   => Address'Asm_Input(TO_EAX, Data'Address),
          Outputs  => Integer_4_Unsigned'Asm_Output(FROM_EAX, Result));
        RETURN Result /= 0;
      END Is_Stack_Empty;
  -----------------
  -- Clear_Stack --
  -----------------
    PROCEDURE Clear_Stack
      IS
      Data : ALIASED Record_x86_Environment := (OTHERS => <>);
      BEGIN
        Asm(
          ---------------------------------------------
          "   fnstenv (%%eax)            " & END_LINE &
          "   movl    8(%%eax),    %%eax " & END_LINE &
          "   xor     $0xffffffff, %%eax " & END_LINE &
          "   movl    $0x0000c000, %%edx " & END_LINE &
          ---------------------------------------------
          " emptystack:                  " & END_LINE &
          "   movl    %%eax,       %%ecx " & END_LINE &
          "   and     %%ecx,       %%edx " & END_LINE &
          "   jz      cease              " & END_LINE &
          "   fstp    %%st               " & END_LINE &
          "   shr     $2,          %%edx " & END_LINE &
          "   jmp     emptystack         " & END_LINE &
          ---------------------------------------------
          " cease:                       " & END_LINE ,
          ---------------------------------------------
          Volatile => True,
          Inputs   => Address'Asm_Input(TO_EAX, Data'Address));
      END Clear_Stack;
  ---------------
  -- Put_Trace --
  ---------------
    PROCEDURE Put_Trace
      IS
      Value_At_Position : Access_Integer_4_Unsigned        := NULL;
      Data              : Integer_4_Unsigned               := 0;
      J                 : Integer_4_Signed                 := 0;
      Base_Pointer      : Address                          := NULL_ADDRESS;
      Mid_Point         : Address                          := NULL_ADDRESS;
      Traces            : ARRAY(1..TRACE_LIMIT) OF Address := (OTHERS => NULL_ADDRESS);
      BEGIN
        NULL;
      --   Asm(
      --     Template => "movl %%ebp, %%eax",
      --     Volatile => True,
      --     Outputs  => Address'Asm_Output(FROM_EAX, Base_Pointer));
      --   Base_Pointer := NULL_ADDRESS;-- *(long*) *(long*) Base_Pointer;
      --   FOR I IN Traces'RANGE LOOP
      --     Asm(
      --       ----------------------------------------
      --       "   movl  (%%eax), %%ecx  " & END_LINE &
      --       "   test  %%ecx,   %%ecx  " & END_LINE &
      --       "   jz    terminus        " & END_LINE &
      --       ----------------------------------------
      --       "   movl  4(%%eax), %%eax " & END_LINE &
      --       "   test  %%eax,    %%eax " & END_LINE &
      --       "   jz    terminus        " & END_LINE &
      --       ----------------------------------------
      --       "   movl  $0x1,     %%ebx " & END_LINE &
      --       ----------------------------------------
      --       " terminus:               " & END_LINE ,
      --       ----------------------------------------
      --       Volatile => True,
      --       Inputs   => Address'Asm_Input(TO_EAX, Base_Pointer),
      --       Outputs  =>(
      --         Address           'Asm_Output(FROM_EAX, Mid_Point),
      --         Integer_4_Unsigned'Asm_Output(FROM_EBX, Data)));
      --     IF Data = C_TRUE THEN
      --       Data := To_Integer_4_Unsigned(Mid_Point); -- Becomes index
      --       LOOP
      --         Value_At_Position := To_Access_Integer_4_Unsigned(To_Address(Data));
      --         exit WHEN (Value_At_Position.All AND PROLOGUE_MASK) = PROLOGUE_SIGNATURE;
      --         Data := Data - 1;
      --       END LOOP;
      --       Traces(I) := To_Address(Data);
      --     else
      --       J := I;
      --       exit;
      --     END IF;
      --   END LOOP;
      --   Base_Pointer := NULL_ADDRESS;-- *(long*) Base_Pointer;
      --   while J < Traces'Length LOOP
      --     Traces(J) := 0;
      --     J := J + 1;
      --   END LOOP;
      --   FOR I IN reverse 1..Traces'Size LOOP
      --     Put(Integer_4_Signed'Wide_Image(I));
      --     Put_Line(": " & To_Hex(To_Integer_4_Unsigned(Traces.All(I))));
      --   END LOOP;
      -- END Traverse_Prologue_Signatures;
      END Put_Trace;
  ---------------
  -- Put_Stack --
  ---------------
    PROCEDURE Put_Stack
      IS
      FUNCTION To_Image
        IS NEW To_Radian_Image(Integer_4_Unsigned);
      FUNCTION To_Image
        IS NEW To_Radian_Image(Integer_2_Unsigned);
      Number_Of_Values     :         Integer_4_Unsigned          := 0;
      Data_From_Extensions : ALIASED Integer_4_Unsigned          := 0;
      Stack                : ALIASED ARRAY(1..8) OF Float_8_Real := (OTHERS => 0.0);
      Environment          : ALIASED Record_x86_Environment      := (OTHERS => <>);
      BEGIN
        Asm(
          ---------------------------------------------
          "   movl    %%eax,       %%esi " & END_LINE &
          "   fnstenv (%%esi)            " & END_LINE &
          "   movl    8(%%esi),    %%esi " & END_LINE &
          "   xor     $0xffffffff, %%esi " & END_LINE &
          "   movl    $0x0000c000, %%edx " & END_LINE &
          "   xor     %%eax,       %%eax " & END_LINE &
          "   movl    %%esi,       %%ecx " & END_LINE &
          "   and     %%edx,       %%ecx " & END_LINE &
          "   jz      fin                " & END_LINE &
          ---------------------------------------------
          "   fst     (%%edi)            " & END_LINE &
          "   inc     %%eax              " & END_LINE &
          "   shr     $2,          %%edx " & END_LINE &
          "   movl    %%esi,       %%ecx " & END_LINE &
          "   and     %%edx,       %%ecx " & END_LINE &
          "   jz      fin                " & END_LINE &
          ---------------------------------------------
          "   fxch    %%st(1)            " & END_LINE &
          "   fst     8(%%edi)           " & END_LINE &
          "   inc     %%eax              " & END_LINE &
          "   fxch    %%st(1)            " & END_LINE &
          "   shr     $2,          %%edx " & END_LINE &
          "   movl    %%esi,       %%ecx " & END_LINE &
          "   and     %%edx,       %%ecx " & END_LINE &
          "   jz      fin                " & END_LINE &
          ---------------------------------------------
          "   fxch    %%st(2)            " & END_LINE &
          "   fst     16(%%edi)          " & END_LINE &
          "   inc     %%eax              " & END_LINE &
          "   fxch    %%st(2)            " & END_LINE &
          "   shr     $2,          %%edx " & END_LINE &
          "   movl    %%esi,       %%ecx " & END_LINE &
          "   and     %%edx,       %%ecx " & END_LINE &
          "   jz      fin                " & END_LINE &
          ---------------------------------------------
          "   fxch    %%st(3)            " & END_LINE &
          "   fst     24(%%edi)          " & END_LINE &
          "   inc     %%eax              " & END_LINE &
          "   fxch    %%st(3)            " & END_LINE &
          "   shr     $2,          %%edx " & END_LINE &
          "   movl    %%esi,       %%ecx " & END_LINE &
          "   and     %%edx,       %%ecx " & END_LINE &
          "   jz      fin                " & END_LINE &
          ---------------------------------------------
          "   fxch    %%st(4)            " & END_LINE &
          "   fst     32(%%edi)          " & END_LINE &
          "   inc     %%eax              " & END_LINE &
          "   fxch    %%st(4)            " & END_LINE &
          "   shr     $2,          %%edx " & END_LINE &
          "   movl    %%esi,       %%ecx " & END_LINE &
          "   and     %%edx,       %%ecx " & END_LINE &
          "   jz      fin                " & END_LINE &
          ---------------------------------------------
          "   fxch    %%st(5)            " & END_LINE &
          "   fst     40(%%edi)          " & END_LINE &
          "   inc     %%eax              " & END_LINE &
          "   fxch    %%st(5)            " & END_LINE &
          "   shr     $2,          %%edx " & END_LINE &
          "   movl    %%esi,       %%ecx " & END_LINE &
          "   and     %%edx,       %%ecx " & END_LINE &
          "   jz      fin                " & END_LINE &
          ---------------------------------------------
          "   fxch    %%st(6)            " & END_LINE &
          "   fst     48(%%edi)          " & END_LINE &
          "   inc     %%eax              " & END_LINE &
          "   fxch    %%st(6)            " & END_LINE &
          "   shr     $2,          %%edx " & END_LINE &
          "   movl    %%esi,       %%ecx " & END_LINE &
          "   and     %%edx,       %%ecx " & END_LINE &
          "   jz      fin                " & END_LINE &
          ---------------------------------------------
          "   fxch    %%st(7)            " & END_LINE &
          "   fst     56(%%edi)          " & END_LINE &
          "   inc     %%eax              " & END_LINE &
          "   fxch    %%st(7)            " & END_LINE &
          ---------------------------------------------
          " fin:                         " & END_LINE ,
          ---------------------------------------------
          Volatile => True,
          Inputs =>(
            Address'Asm_Input(TO_EAX, Environment'Address),
            Address'Asm_Input(TO_EDI, Stack'Address)),
          Outputs =>
            Integer_4_Unsigned'Asm_Output(FROM_EAX, Number_Of_Values));
        IF Number_Of_Values <= Stack'Size THEN
          FOR I IN 1..Integer(Number_Of_Values) LOOP
            Put_Line("Stack" & Integer'Wide_Image(I) & ": " & Float_8_Real'Wide_Image(Stack(I)));
          END LOOP;
        END IF;
        IF Is_Enabled(INTEL_FXSR) THEN
          Asm(
            Template => "stmxcsr (%%eax)",
            Volatile => True,
            Inputs   => Address'Asm_Input(TO_EAX, Data_From_Extensions'Address));
          Put_Line("Extensions: " & To_Image(Data_From_Extensions, 2));
        END IF;
        Put_Line("Control word: "    & To_Image(Environment.Control_Word,   16));
        Put_Line("Status word: "     & To_Image(Environment.Status_Word,     2));
        Put_Line("Selector: "        & To_Image(Environment.CS_Selector,     2));
        Put_Line("Tags: "            & To_Image(Environment.Tags,            2));
        Put_Line("Data offset: "     & To_Image(Environment.Data_Offset,     2));
        Put_Line("Data selector: "   & To_Image(Environment.Data_Selector,   2));
        Put_Line("Operation code: "  & To_Image(Environment.Operation_Code,  2));
        Put_Line("Program counter: " & To_Image(Environment.Program_Counter, 2));
      END Put_Stack;
  END Implementation_For_Architecture;
