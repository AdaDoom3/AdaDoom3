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
  Interfaces.C,
  System,
  System.Machine_Code;
use
  Interfaces,
  Interfaces.C,
  System,
  System.Machine_Code;
separate(Neo.System.Processor)
package body Implementation_For_Architecture
  is
  ----------------
  -- Exceptions --
  ----------------
    CPUID_Is_Not_Supported : Exception;
  -------------
  -- Numbers --
  -------------
    type Integer_Bit_Field_Element
      is new Integer_4_Signed
      RANGE 0..31;
  ------------------
  -- Enumerations --
  ------------------
    type Enumerated_Register
      is(
      EAX_Register,
      EBX_Register,
      ECX_Register,
      EDX_Register);
  -------------
  -- Records --
  -------------
    type Record_Value
      is record
        Function_ID : Integer_4_Unsigned        := 0;
        Register    : Enumerated_Register       := EAX_Register;
        Start_Bit   : Integer_Bit_Field_Element := 0;
        End_Bit     : Integer_Bit_Field_Element := 0;
      end record;
    type Record_Feature
      is record
        Function_ID : Integer_4_Unsigned        := 0;
        Register    : Enumerated_Register       := EAX_Register;
        Bit         : Integer_Bit_Field_Element := 0;
      end record;
    type Record_x86_Environment
      is record
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
      end record
      with Size => 28 * Byte'Size;
  ---------------
  -- Constants --
  ---------------
    DO_CRASH_ON_INEXACT_RESULT       : constant Boolean            := False;
    DO_CRASH_ON_DIVIDE_BY_ZERO       : constant Boolean            := False;
    DO_CRASH_ON_NUMERIC_OVERFLOW     : constant Boolean            := False;
    DO_CRASH_ON_inVALID_OPERATION    : constant Boolean            := False;
    DO_CRASH_ON_NUMERIC_UNDERFLOW    : constant Boolean            := False;
    DO_CRASH_ON_DENORMALIZED_OPERAND : constant Boolean            := False;
    TO_ESI                           : constant String_1           := "s";
    TO_EDI                           : constant String_1           := "d";
    TO_EAX                           : constant String_1           := "a";
    TO_EBX                           : constant String_1           := "b";
    TO_ECX                           : constant String_1           := "c";
    TO_EDX                           : constant String_1           := "d";
    FROM_EAX                         : constant String_1           := "=a";
    FROM_EBX                         : constant String_1           := "=b";
    FROM_ECX                         : constant String_1           := "=c";
    FROM_EDX                         : constant String_1           := "=d";
    VENDOR_INTEL                     : constant String_1           := "GenuineIntel";
    VENDOR_ADVANCED_MICRO_DEVICES    : constant String_1           := "AuthenticAMD";
    INTEL_SSE3                       : constant Record_Feature     := (16#0000_0001#, ECX_Register,  0);
    INTEL_PCLMULQDQ                  : constant Record_Feature     := (16#0000_0001#, ECX_Register,  1);
    INTEL_SSSE3                      : constant Record_Feature     := (16#0000_0001#, ECX_Register,  9);
    INTEL_FMA3                       : constant Record_Feature     := (16#0000_0001#, ECX_Register, 12);
    INTEL_SSE4_1                     : constant Record_Feature     := (16#0000_0001#, ECX_Register, 19);
    INTEL_SSE4_2                     : constant Record_Feature     := (16#0000_0001#, ECX_Register, 20);
    INTEL_POPCNT                     : constant Record_Feature     := (16#0000_0001#, ECX_Register, 23);
    INTEL_AES                        : constant Record_Feature     := (16#0000_0001#, ECX_Register, 25);
    INTEL_OSXSAVE                    : constant Record_Feature     := (16#0000_0001#, ECX_Register, 27);
    INTEL_AVX                        : constant Record_Feature     := (16#0000_0001#, ECX_Register, 28);
    INTEL_F16C                       : constant Record_Feature     := (16#0000_0001#, ECX_Register, 29);
    INTEL_CMOV                       : constant Record_Feature     := (16#0000_0001#, EDX_Register, 15);
    INTEL_MMX                        : constant Record_Feature     := (16#0000_0001#, EDX_Register, 23);
    INTEL_FXSR                       : constant Record_Feature     := (16#0000_0001#, EDX_Register, 24);
    INTEL_SSE                        : constant Record_Feature     := (16#0000_0001#, EDX_Register, 25);
    INTEL_SSE2                       : constant Record_Feature     := (16#0000_0001#, EDX_Register, 26);
    INTEL_HTT                        : constant Record_Feature     := (16#0000_0001#, EDX_Register, 28);
    INTEL_BMI1                       : constant Record_Feature     := (16#0000_0007#, EBX_Register,  3);
    INTEL_AVX2                       : constant Record_Feature     := (16#0000_0007#, EBX_Register,  5);
    INTEL_BMI2                       : constant Record_Feature     := (16#0000_0007#, EBX_Register,  8);
    INTEL_inVPCID                    : constant Record_Feature     := (16#0000_0007#, EBX_Register, 10);
    AMD_ABM                          : constant Record_Feature     := (16#8000_0001#, ECX_Register,  5);
    AMD_SSE4_A                       : constant Record_Feature     := (16#8000_0001#, ECX_Register,  6);
    AMD_XOP                          : constant Record_Feature     := (16#8000_0001#, ECX_Register, 11);
    AMD_FMA4                         : constant Record_Feature     := (16#8000_0001#, ECX_Register, 16);
    AMD_CVT16                        : constant Record_Feature     := (16#8000_0001#, ECX_Register, 19);
    AMD_MMX_PLUS                     : constant Record_Feature     := (16#8000_0001#, EDX_Register, 22);
    AMD_3DNOW_PLUS                   : constant Record_Feature     := (16#8000_0001#, EDX_Register, 30);
    AMD_3DNOW                        : constant Record_Feature     := (16#8000_0001#, EDX_Register, 31);
    AMD_CPU_COUNT                    : constant Record_Value       := (16#8000_0008#, ECX_Register,  0,  7); -- One plus this value
    INTEL_CPU_COUNT                  : constant Record_Value       := (16#0000_0001#, EBX_Register, 16, 23);
    INTEL_APIC                       : constant Record_Value       := (16#0000_0001#, EBX_Register, 24, 31);
    PROLOGUE_SIGNATURE               : constant Integer_4_Unsigned := 16#00EC_8B55#;
  -------------------
  -- Execute_CPUID --
  -------------------
    function Execute_CPUID(
      Function_ID : in Integer_4_Unsigned;
      Register    : in Enumerated_Register)
      return Integer_4_Unsigned
      is
      A, B, C, D : Integer_4_Unsigned := 0;
      begin
        Asm(
          Volatile => True,
          Inputs   => Integer_4_Unsigned'Asm_Input(TO_EAX, Function_ID),
          Template => "cpuid",
          Outputs  =>(
            Integer_4_Unsigned'Asm_Output(FROM_EAX, A),
            Integer_4_Unsigned'Asm_Output(FROM_EBX, B),
            Integer_4_Unsigned'Asm_Output(FROM_ECX, C),
            Integer_4_Unsigned'Asm_Output(FROM_EDX, D)));
        case Register is
          when EAX_Register =>
            return A;
          when EBX_Register =>
            return B;
          when ECX_Register =>
            return C;
          when EDX_Register =>
            return D;
        end case;
      end Execute_CPUID;
  ----------------
  -- Is_Enabled --
  ----------------
    function Is_Enabled(
      Feature : in Record_Feature)
      return Boolean
      is
      begin
        return (Execute_CPUID(Feature.Function_ID, Feature.Register) and 2**Integer(Feature.Bit)) /= 0;
      end Is_Enabled;
  ---------------
  -- Get_Value --
  ---------------
    function Get_Value(
      Value : in Record_Value)
      return Integer_8_Unsigned
      is
      begin
        return
          Integer_8_Unsigned(
            Shift_Right(
              Amount => Integer(Integer_Bit_Field_Element'Last - (Value.End_Bit - Value.Start_Bit)),
              Value  =>
                Shift_Left(
                  Amount => Integer(Integer_Bit_Field_Element'Last - Value.End_Bit),
                  Value  => Execute_CPUID(Value.Function_ID, Value.Register))));
      end Get_Value;
  ----------------
  -- Get_Vendor --
  ----------------
    function Get_Vendor
      return Enumerated_Vendor
      is
      type Array_String_Segment
        is array(1..4)
        of Character;
      B, D, C : Array_String_Segment := (others => Ascii.Nul);
      begin
        Asm(
          Volatile => True,
          Inputs   => Integer_4_Unsigned'Asm_Input(TO_EAX, 0),
          Template => "cpuid",
          Outputs  =>(
            Array_String_Segment'Asm_Output(FROM_EBX, B),
            Array_String_Segment'Asm_Output(FROM_ECX, C),
            Array_String_Segment'Asm_Output(FROM_EDX, D)));
        if String_1(B) & String_1(D) & String_1(C) = VENDOR_INTEL then
          return Intel_Vendor;
        end if;
        return Advanced_Micro_Devices_Vendor;
      end Get_Vendor;
  -------------------------
  -- Get_Number_Of_Cores --
  -------------------------
    function Get_Number_Of_Cores
      return Integer_8_Unsigned
      is
      begin
        if Get_Vendor = Intel_Vendor then
          raise Unsupported_Feature;--return Get_Value(INTEL_CPU_COUNT); -- Inaccurate
        end if;
        return Get_Value(AMD_CPU_COUNT) + 1;
      end Get_Number_Of_Cores;
  ----------------
  -- Initialize --
  ----------------
    procedure Initialize
      is
      Data : aliased Integer_4_Unsigned := 0;
      begin
        if Address'Size = 32 then
          Asm( -- Check for cpuid
            Volatile => True,
            Template =>
              ------------------------------------------
              " pushfl                    " & END_LINE & -- Get original extended flags
              " popl   %%eax              " & END_LINE &
              " movl   %%eax,       %%ecx " & END_LINE &
              " xorl   $0x00200000, %%eax " & END_LINE & -- Flip identifier bit in the extended flags
              " pushl  %%eax              " & END_LINE & -- Save new extended flag value on stack
              " popfl                     " & END_LINE & -- Replace current value
              " pushfl                    " & END_LINE & -- Get new extended flags
              " popl   %%eax              " & END_LINE & -- Store new in EAX register
              " xorl   %%ecx,       %%eax " & END_LINE , -- Can we toggle the identifier bit?
              ------------------------------------------
            Outputs  => Integer_4_Unsigned'Asm_Output(FROM_EAX, Data));
          if Data = 0 then -- No we cannot, no cpuid command supported (processor is probably 80486)
            raise CPUID_Is_Not_Supported;
          end if;
        end if;
        if Is_Enabled(INTEL_FXSR) then
          --------------------------
          Enable_Denormals_Are_Zero:
          --------------------------
            declare
            type Array_Save_Area
              is array(1..512)
              of Integer_1_Unsigned
              with Alignment => 16;
            Save_Area : aliased Array_Save_Area := (others => 0);
            begin
              Asm(
                Volatile => True,
                Inputs   => Address'Asm_Input(TO_EAX, Save_Area'Address),
                Template =>
                  ----------------------------------------
                  " fxsave (%%eax)          " & END_LINE &
                  " movl   28(%%eax), %%ebx " & END_LINE ,
                  ----------------------------------------
                Outputs  => Integer_4_Unsigned'Asm_Output(FROM_EBX, Data));
              if (Data and 16#20#) /= 0 then
                Asm(
                  Volatile => True,
                  Inputs   => Address'Asm_Input(TO_EAX, Data'Address),
                  Template =>
                    -----------------------------------------
                    " stmxcsr (%%eax)          " & END_LINE &
                    " movl    (%%eax), %%ebx   " & END_LINE &
                    " or      $0x40,   %%bx    " & END_LINE &
                    " movl    %%ebx,   (%%eax) " & END_LINE &
                    " ldmxcsr (%%eax)          " & END_LINE );
                    -----------------------------------------
              end if;
            end Enable_Denormals_Are_Zero;
          Asm(
            Volatile => True,
            Inputs   => Address'Asm_Input(TO_EAX, Data'Address),
            Template =>
              -----------------------------------------
              " stmxcsr (%%eax)          " & END_LINE &
              " movl    (%%eax), %%ebx   " & END_LINE &
              " or      $0x8000, %%ebx   " & END_LINE &
              " movl    %%ebx,   (%%eax) " & END_LINE &
              " ldmxcsr (%%eax)          " & END_LINE );
              -----------------------------------------
        end if;
        ---------------
        Set_Exceptions:
        ---------------
          declare
          Exception_Mask :         Integer_4_Unsigned := 0;
          Other_Data     : aliased Integer_2_Unsigned := 0;
          begin
            if not DO_CRASH_ON_INEXACT_RESULT then
              Exception_Mask := 16#0000_1000#;
            end if;
            if not DO_CRASH_ON_NUMERIC_UNDERFLOW then
              Exception_Mask := Exception_Mask OR 16#0000_0800#;
            end if;
            if not DO_CRASH_ON_NUMERIC_OVERFLOW then
              Exception_Mask := Exception_Mask OR 16#0000_0400#;
            end if;
            if not DO_CRASH_ON_DIVIDE_BY_ZERO then
              Exception_Mask := Exception_Mask OR 16#0000_0200#;
            end if;
            if not DO_CRASH_ON_DENORMALIZED_OPERAND then
              Exception_Mask := Exception_Mask OR 16#0000_0100#;
            end if;
            if not DO_CRASH_ON_inVALID_OPERATION then
              Exception_Mask := Exception_Mask OR 16#0000_0080#;
            end if;
            if Is_Enabled(INTEL_FXSR) then
              Asm(
                Volatile => True,
                Inputs   =>(
                  Address           'Asm_Input(TO_EAX, Data'Address),
                  Integer_4_Unsigned'Asm_Input(TO_ECX, Exception_Mask)),
                Template =>
                  ---------------------------------------------
                  " stmxcsr (%%eax)              " & END_LINE &
                  " movl    (%%eax),     %%ebx   " & END_LINE &
                  " and     $0xffffe07f, %%ebx   " & END_LINE &
                  " or      %%ecx,       %%ebx   " & END_LINE &
                  " movl    %%ebx,       (%%eax) " & END_LINE &
                  " ldmxcsr (%%eax)              " & END_LINE );
                  ---------------------------------------------
            end if;
            Exception_Mask := Shift_Right(Exception_Mask, 7);
            Asm(
              Volatile => True,
              Inputs   =>(
                Address           'Asm_Input(TO_EAX, Other_Data'Address),
                Integer_4_Unsigned'Asm_Input(TO_ECX, Exception_Mask)),
              Template =>
                ----------------------------------------
                " fnstcw (%%eax)          " & END_LINE &
                " movw   (%%eax), %%bx    " & END_LINE &
                " and    $0xffc0, %%bx    " & END_LINE &
                " or     %%cx,    %%bx    " & END_LINE &
                " movw   %%bx,    (%%eax) " & END_LINE &
                " fldcw  (%%eax)          " & END_LINE );
                ----------------------------------------
          end Set_Exceptions;
      end Initialize;
  -------------------
  -- Get_Specifics --
  -------------------
    function Get_Specifics
      return Record_Specifics
      is
      Specifics :         Record_Specifics(Get_Vendor);-- := (others => <>);
      Data      : aliased Integer_4_Unsigned           := 0;
      begin
        if Specifics.Vendor = Advanced_Micro_Devices_Vendor then
          Specifics.Has_3DNow                                  := Is_Enabled(AMD_3DNOW);
          Specifics.Has_3DNow_Supplement                       := Is_Enabled(AMD_3DNOW_PLUS);
          Specifics.Has_Multi_Media_Extensions_Supplement      := Is_Enabled(AMD_MMX_PLUS);
          Specifics.Has_Streaming_SIMD_Extensions_4_Supplement := Is_Enabled(AMD_SSE4_A);
        end if;
        Specifics.Has_Extended_States_Enabled                := Is_Enabled(INTEL_OSXSAVE);
        Specifics.Has_Population_Count                       := Is_Enabled(INTEL_POPCNT);
        Specifics.Has_Multi_Media_Extensions                 := Is_Enabled(INTEL_MMX);
        Specifics.Has_Streaming_SIMD_Extensions_1            := Is_Enabled(INTEL_SSE);
        Specifics.Has_Streaming_SIMD_Extensions_2            := Is_Enabled(INTEL_SSE2);
        Specifics.Has_Streaming_SIMD_Extensions_3            := Is_Enabled(INTEL_SSE3);
        Specifics.Has_Streaming_SIMD_Extensions_3_Supplement := Is_Enabled(INTEL_SSSE3);
        Specifics.Has_Streaming_SIMD_Extensions_4_1          := Is_Enabled(INTEL_SSE4_1);
        Specifics.Has_Streaming_SIMD_Extensions_4_2          := Is_Enabled(INTEL_SSE4_2);
        Specifics.Has_Carryless_Multiplication_Of_Two_64_Bit := Is_Enabled(INTEL_PCLMULQDQ);
        Specifics.Has_Bit_Manipulation_Extensions_1          := Is_Enabled(INTEL_BMI1);
        Specifics.Has_Bit_Manipulation_Extensions_2          := Is_Enabled(INTEL_BMI2);
        Specifics.Has_Advanced_Vector_Extensions_1           := Is_Enabled(INTEL_AVX);
        Specifics.Has_Advanced_Vector_Extensions_2           := Is_Enabled(INTEL_AVX2);
        Specifics.Has_Advanced_Encryption_Service            := Is_Enabled(INTEL_AES);
        Specifics.Has_Half_Precision_Floating_Point_Convert  := Is_Enabled(INTEL_F16C);
        Specifics.Has_Conditional_Move                       := Is_Enabled(INTEL_CMOV);
        Specifics.Has_Hyperthreading                         := Is_Enabled(INTEL_HTT);
        Specifics.Has_Advanced_State_Operations              := Is_Enabled(INTEL_FXSR);
        Specifics.Has_Fused_Multiply_Add_3                   := Is_Enabled(INTEL_FMA3);
        Specifics.Has_Fused_Multiply_Add_4                   := Is_Enabled(AMD_FMA4);
        Specifics.Has_High_Precision_Convert                 := Is_Enabled(AMD_CVT16);
        Specifics.Has_Extended_Operation_Support             := Is_Enabled(AMD_XOP);
        Specifics.Has_Leading_Zero_Count                     := Is_Enabled(AMD_ABM);
        if
        Specifics.Has_Streaming_SIMD_Extensions_4_2 and then
        Is_Newer_Than(Linux_2_7_System, Macintosh_10_6_System, Windows_2_6_1_System)
        then
          Asm( -- Check if the operating system will save the YMM registers
            Volatile => True,
            Inputs   => Integer_4_Unsigned'Asm_Input(TO_ECX, 0),
            Template => "xgetbv",
            Outputs  => Integer_4_Unsigned'Asm_Output(FROM_EAX, Data));
          Specifics.Has_Advanced_Vector_Extensions_Enabled := (Data and 16#0000_0006#) > 0;
        end if;
        return Specifics;
      end Get_Specifics;
  ----------------------
  -- Check_Exceptions --
  ----------------------
    procedure Check_Exceptions
      is
      Data          : aliased Integer_4_Unsigned := 0;
      Data_From_x87 : aliased Integer_2_Unsigned := 0;
      begin
        if Is_Enabled(INTEL_FXSR) then
          Asm(
            Volatile => True,
            Inputs   => Address'Asm_Input(TO_EAX, Data'Address),
            Template => "stmxcsr (%%eax)");
        end if;
        Asm(
          Volatile => True,
          Inputs   => Address'Asm_Input(TO_EAX, Data_From_x87'Address),
          Template => "fnstsw (%%eax)");
        Data := (Data and 16#0000_003F#) and Integer_4_Unsigned(Data_From_x87);
        if Data /= 0 then -- raise the first error found
          ---------------------
          Clear_Exception_Bits:
          ---------------------
            declare
            Data_From_SIMD  :         Integer_4_Unsigned     := 0;
            x86_Environment : aliased Record_x86_Environment := (others => <>);
            begin
              if Is_Enabled(INTEL_FXSR) then
                Asm(
                  Volatile => True,
                  Inputs   => Address'Asm_Input(TO_EAX, Data_From_SIMD'Address),
                  Template =>
                    ---------------------------------------------
                    " stmxcsr (%%eax)              " & END_LINE &
                    " movl    (%%eax),     %%ebx   " & END_LINE &
                    " and     $0xffffffc0, %%ebx   " & END_LINE &
                    " movl    %%ebx,       (%%eax) " & END_LINE &
                    " ldmxcsr (%%eax)              " & END_LINE );
                    ---------------------------------------------
              end if;
              Asm(
                Volatile => True,
                Inputs   => Address'Asm_Input(TO_EAX, x86_Environment'Address),
                Template => "fnstenv (%%eax)");
              -- Clear 6 exception bits plus stack fault
              x86_Environment.Status_Word := x86_Environment.Status_Word and 16#FF80#;
              Asm(
                Volatile => True,
                Inputs   => Address'Asm_Input(TO_EAX, x86_Environment'Address),
                Template => "fldenv (%%eax)");
              Asm(
                Volatile => True,
                Template => "fnclex");
            end Clear_Exception_Bits;
          if (Data and 16#0000_0001#) /= 0 then
            raise Invalid_Operation;
          end if;
          if (Data and 16#0000_0002#) /= 0 then
            raise Denormalized_Operand;
          end if;
          if (Data and 16#0000_0004#) /= 0 then
            raise Divide_By_Zero;
          end if;
          if (Data and 16#0000_0008#) /= 0 then
            raise Numeric_Overflow;
          end if;
          if (Data and 16#0000_0010#) /= 0 then
            raise Numeric_Underflow;
          end if;
          if (Data and 16#0000_0020#) /= 0 then
            raise Inexact_Result;
          end if;
          if (Data and 16#0000_0040#) /= 0 then
            raise Stack_Fault;
          end if;
        end if;
      end Check_Exceptions;
  ------------------
  -- Set_Rounding --
  ------------------
    procedure Set_Rounding(
      Rounding  : in Enumerated_Rounding)
      is
      Other_Data    : aliased Integer_2_Unsigned := 0;
      Data          : aliased Integer_4_Unsigned := 0;
      Rounding_Mask :         Integer_4_Unsigned := 0;
      begin
        case Rounding is
          when Nearest_Rounding =>
            Rounding_Mask := 16#0000#;
          when Down_Rounding =>
            Rounding_Mask := 16#2000#;
          when Up_Rounding =>
            Rounding_Mask := 16#4000#;
          when Truncate_Rounding =>
            Rounding_Mask := 16#6000#;
        end case;
        if Is_Enabled(INTEL_FXSR) then
          Asm(
            Volatile => True,
            Inputs   =>(
              Address           'Asm_Input(TO_EAX, Data'Address),
              Integer_4_Unsigned'Asm_Input(TO_ECX, Rounding_Mask)),
            Template =>
              ---------------------------------------------
              " stmxcsr (%%eax)              " & END_LINE &
              " movl    (%%eax),     %%ebx   " & END_LINE &
              " and     $0xffff9fff, %%ebx   " & END_LINE &
              " or      %%cx,        %%bx    " & END_LINE &
              " movl    %%ebx,       (%%eax) " & END_LINE &
              " ldmxcsr (%%eax)              " & END_LINE );
              ---------------------------------------------
        end if;
        Rounding_Mask := Shift_Right(Rounding_Mask, 3);
        Asm(
          Volatile => True,
          Inputs   =>(
            Address           'Asm_Input(TO_EAX, Other_Data'Address),
            Integer_4_Unsigned'Asm_Input(TO_ECX, Rounding_Mask)),
          Template => 
            ----------------------------------------
            " fnstcw (%%eax)          " & END_LINE &
            " movw   (%%eax), %%bx    " & END_LINE &
            " and    $0xf3ff, %%bx    " & END_LINE &
            " or     %%cx,    %%bx    " & END_LINE &
            " movw   %%bx,    (%%eax) " & END_LINE &
            " fldcw  (%%eax)          " & END_LINE );
            ----------------------------------------
      end Set_Rounding;
  -------------------
  -- Set_Precision --
  -------------------
    procedure Set_Precision(
      Precision : in Enumerated_Precision)
      is
      Blank_Memory   : aliased Integer_2_Unsigned := 0;
      Precision_Mask :         Integer_4_Unsigned := 0;
      begin
        case Precision is
          when Single_Precision =>
            Precision_Mask := 16#0000#;
          when Double_Precision =>
            Precision_Mask := 16#0200#;
          when Double_Extended_Precision =>
            Precision_Mask := 16#0300#;
        end case;
        Asm(
          Volatile => True,
          Inputs   =>(
            Address           'Asm_Input(TO_EAX, Blank_Memory'Address),
            Integer_4_Unsigned'Asm_Input(TO_ECX, Precision_Mask)),
          Template => 
            ----------------------------------------
            " fnstcw (%%eax)          " & END_LINE &
            " movw   (%%eax), %%bx    " & END_LINE &
            " and    $0xfcff, %%bx    " & END_LINE &
            " or     %%cx,    %%bx    " & END_LINE &
            " movw   %%bx,    (%%eax) " & END_LINE &
            " fldcw  (%%eax)          " & END_LINE );
            ----------------------------------------
      end Set_Precision;
  --------------------
  -- Get_Clock_Tics --
  --------------------
    function Get_Clock_Ticks
      return Integer_8_Unsigned
      is
      Low_Part  : Integer_4_Unsigned := 0;
      High_Part : Integer_4_Unsigned := 0;
      begin
        Asm(
          Volatile => True,
          Template =>
            ----------------------
            " cpuid " & END_LINE &
            " rdtsc " & END_LINE ,
            ----------------------
          Outputs  =>(
            Integer_4_Unsigned'Asm_Output(FROM_EAX, Low_Part),
            Integer_4_Unsigned'Asm_Output(FROM_EDX, High_Part)));
        return Shift_Left(Integer_8_Unsigned(High_Part), 32) + Integer_8_Unsigned(Low_Part);
      end Get_Clock_Ticks;
  --------------------
  -- Is_Stack_Empty --
  --------------------
    function Is_Stack_Empty
      return Boolean
      is
      Result :         Integer_4_Unsigned     := 0;
      Data   : aliased Record_x86_Environment := (others => <>);
      begin
        Asm(
          Volatile => True,
          Inputs   => Address'Asm_Input(TO_EAX, Data'Address),
          Template => 
            ---------------------------------------------
            "   fnstenv (%%eax)            " & END_LINE &
            "   movl    8(%%eax),    %%eax " & END_LINE &
            "   xor     $0xffffffff, %%eax " & END_LINE &
            "   and     $0x0000ffff, %%eax " & END_LINE &
            "   jz      1f                 " & END_LINE &
            "   movl    $0x00000000, %%eax " & END_LINE &
            "   jmp     2f                 " & END_LINE &
            ---------------------------------------------
            " 1:                           " & END_LINE &
            "   movl    $0x00000001, %%eax " & END_LINE &
            ---------------------------------------------
            " 2:                           " & END_LINE ,
            ---------------------------------------------
          Outputs  => Integer_4_Unsigned'Asm_Output(FROM_EAX, Result));
        return Result /= 0;
    end Is_Stack_Empty;
  -----------------
  -- Clear_Stack --
  -----------------
    procedure Clear_Stack
      is
      Data : aliased Record_x86_Environment := (others => <>);
      begin
        Asm(
          Volatile => True,
          Inputs   => Address'Asm_Input(TO_EAX, Data'Address),
          Template => 
            ---------------------------------------------
            "   fnstenv (%%eax)            " & END_LINE &
            "   movl    8(%%eax),    %%eax " & END_LINE &
            "   xor     $0xffffffff, %%eax " & END_LINE &
            "   movl    $0x0000c000, %%edx " & END_LINE &
            ---------------------------------------------
            " 1:                           " & END_LINE &
            "   movl    %%eax,       %%ecx " & END_LINE &
            "   and     %%ecx,       %%edx " & END_LINE &
            "   jz      2f                 " & END_LINE &
            "   fstp    %%st               " & END_LINE &
            "   shr     $2,          %%edx " & END_LINE &
            "   jmp     1b                 " & END_LINE &
            ---------------------------------------------
            " 2:                           " & END_LINE );
            ---------------------------------------------
      end Clear_Stack;
  ---------------
  -- Put_Trace --
  ---------------
    procedure Put_Trace
      is
      Value_At_Position : Access_Integer_4_Unsigned        := null;
      Data              : Integer_4_Unsigned               := 0;
      J                 : Integer_4_Signed                 := 0;
      Base_Pointer      : Address                          := NULL_ADDRESS;
      Mid_Point         : Address                          := NULL_ADDRESS;
      Traces            : array(1..TRACE_LIMIT) of Address := (others => NULL_ADDRESS);
      begin
        null;
      --   Asm(
      --     Template => "movl %%ebp, %%eax",
      --     Volatile => True,
      --     Outputs  => Address'Asm_Output(FROM_EAX, Base_Pointer));
      --   Base_Pointer := NULL_ADDRESS;-- *(long*) *(long*) Base_Pointer;
      --   for I in Traces'RANGE loop
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
      --     if Data = C_TRUE then
      --       Data := To_Integer_4_Unsigned(Mid_Point); -- Becomes index
      --       loop
      --         Value_At_Position := To_Access_Integer_4_Unsigned(To_Address(Data));
      --         exit when (Value_At_Position.All and PROLOGUE_MASK) = PROLOGUE_SIGNATURE;
      --         Data := Data - 1;
      --       end loop;
      --       Traces(I) := To_Address(Data);
      --     else
      --       J := I;
      --       exit;
      --     end if;
      --   end loop;
      --   Base_Pointer := NULL_ADDRESS;-- *(long*) Base_Pointer;
      --   while J < Traces'Length loop
      --     Traces(J) := 0;
      --     J := J + 1;
      --   end loop;
      --   for I in reverse 1..Traces'Size loop
      --     Put(Integer_4_Signed'Wide_Image(I));
      --     Put_Line(": " & To_Hex(To_Integer_4_Unsigned(Traces.All(I))));
      --   end loop;
      -- end Traverse_Prologue_Signatures;
      end Put_Trace;
  ---------------
  -- Put_Stack --
  ---------------
    procedure Put_Stack
      is
      function To_Image
        is new To_Radian_Image(Integer_4_Unsigned);
      function To_Image
        is new To_Radian_Image(Integer_2_Unsigned);
      Number_Of_Values     : aliased Integer_4_Unsigned          := 0;
      Data_From_Extensions : aliased Integer_4_Unsigned          := 0;
      Stack                : aliased array(1..8) of Float_8_Real := (others => 0.0);
      Environment          : aliased Record_x86_Environment      := (others => <>);
      begin
-- SOMETHING IN THIS ASM BLOCK CAUSES AN ERROR:
--    Execution terminated by unhandled exception
--    Exception name: PROGRAM_ERROR
--    Message: EXCEPTION_ACCESS_VIOLATION
--          Asm(
--            ---------------------------------------------
--            "   movl    %%eax,       %%esi " & END_LINE &
--            "   fnstenv (%%esi)            " & END_LINE &
--            "   movl    8(%%esi),    %%esi " & END_LINE &
--            "   xor     $0xffffffff, %%esi " & END_LINE &
--            "   movl    $0x0000c000, %%edx " & END_LINE &
--            "   xor     %%eax,       %%eax " & END_LINE &
--            "   movl    %%esi,       %%ecx " & END_LINE &
--            "   and     %%edx,       %%ecx " & END_LINE &
--            "   jz      fin                " & END_LINE &
--            ---------------------------------------------
--            "   fst     (%%edi)            " & END_LINE &
--            "   inc     %%eax              " & END_LINE &
--            "   shr     $2,          %%edx " & END_LINE &
--            "   movl    %%esi,       %%ecx " & END_LINE &
--            "   and     %%edx,       %%ecx " & END_LINE &
--            "   jz      fin                " & END_LINE &
--            ---------------------------------------------
--            "   fxch    %%st(1)            " & END_LINE &
--            "   fst     8(%%edi)           " & END_LINE &
--            "   inc     %%eax              " & END_LINE &
--            "   fxch    %%st(1)            " & END_LINE &
--            "   shr     $2,          %%edx " & END_LINE &
--            "   movl    %%esi,       %%ecx " & END_LINE &
--            "   and     %%edx,       %%ecx " & END_LINE &
--            "   jz      fin                " & END_LINE &
--            ---------------------------------------------
--            "   fxch    %%st(2)            " & END_LINE &
--            "   fst     16(%%edi)          " & END_LINE &
--            "   inc     %%eax              " & END_LINE &
--            "   fxch    %%st(2)            " & END_LINE &
--            "   shr     $2,          %%edx " & END_LINE &
--            "   movl    %%esi,       %%ecx " & END_LINE &
--            "   and     %%edx,       %%ecx " & END_LINE &
--            "   jz      fin                " & END_LINE &
--            ---------------------------------------------
--            "   fxch    %%st(3)            " & END_LINE &
--            "   fst     24(%%edi)          " & END_LINE &
--            "   inc     %%eax              " & END_LINE &
--            "   fxch    %%st(3)            " & END_LINE &
--            "   shr     $2,          %%edx " & END_LINE &
--            "   movl    %%esi,       %%ecx " & END_LINE &
--            "   and     %%edx,       %%ecx " & END_LINE &
--            "   jz      fin                " & END_LINE &
--            ---------------------------------------------
--            "   fxch    %%st(4)            " & END_LINE &
--            "   fst     32(%%edi)          " & END_LINE &
--            "   inc     %%eax              " & END_LINE &
--            "   fxch    %%st(4)            " & END_LINE &
--            "   shr     $2,          %%edx " & END_LINE &
--            "   movl    %%esi,       %%ecx " & END_LINE &
--            "   and     %%edx,       %%ecx " & END_LINE &
--            "   jz      fin                " & END_LINE &
--            ---------------------------------------------
--            "   fxch    %%st(5)            " & END_LINE &
--            "   fst     40(%%edi)          " & END_LINE &
--            "   inc     %%eax              " & END_LINE &
--            "   fxch    %%st(5)            " & END_LINE &
--            "   shr     $2,          %%edx " & END_LINE &
--            "   movl    %%esi,       %%ecx " & END_LINE &
--            "   and     %%edx,       %%ecx " & END_LINE &
--            "   jz      fin                " & END_LINE &
--            ---------------------------------------------
--            "   fxch    %%st(6)            " & END_LINE &
--            "   fst     48(%%edi)          " & END_LINE &
--            "   inc     %%eax              " & END_LINE &
--            "   fxch    %%st(6)            " & END_LINE &
--            "   shr     $2,          %%edx " & END_LINE &
--            "   movl    %%esi,       %%ecx " & END_LINE &
--            "   and     %%edx,       %%ecx " & END_LINE &
--            "   jz      fin                " & END_LINE &
--            ---------------------------------------------
--            "   fxch    %%st(7)            " & END_LINE &
--            "   fst     56(%%edi)          " & END_LINE &
--            "   inc     %%eax              " & END_LINE &
--            "   fxch    %%st(7)            " & END_LINE &
--            ---------------------------------------------
--            " fin:                         " & END_LINE ,
--            ---------------------------------------------
--            Volatile => True,
--            Inputs =>(
--              Address'Asm_Input(TO_EAX, Environment'Address),
--              Address'Asm_Input(TO_EDI, Stack'Address)),
--            Outputs =>
--  	    Integer_4_Unsigned'Asm_Output(FROM_EAX, Number_Of_Values));
GOTO ENDING;
        if Number_Of_Values <= Stack'Size then
          for I in 1..Integer(Number_Of_Values) loop
            Put_Line("Stack" & Integer'Wide_Image(I) & ": " & Float_8_Real'Wide_Image(Stack(I)));
          end loop;
        end if;
        if Is_Enabled(INTEL_FXSR) then
          Asm(
            Template => "stmxcsr (%%eax)",
            Volatile => True,
            Inputs   => Address'Asm_Input(TO_EAX, Data_From_Extensions'Address));
          Put_Line("Specifics: " & To_Image(Data_From_Extensions, 2));
        end if;
        Put_Line("Control word: "    & To_Image(Environment.Control_Word,   16));
        Put_Line("Status word: "     & To_Image(Environment.Status_Word,     2));
        Put_Line("Selector: "        & To_Image(Environment.CS_Selector,     2));
        Put_Line("Tags: "            & To_Image(Environment.Tags,            2));
        Put_Line("Data offset: "     & To_Image(Environment.Data_Offset,     2));
        Put_Line("Data selector: "   & To_Image(Environment.Data_Selector,   2));
        Put_Line("Operation code: "  & To_Image(Environment.Operation_Code,  2));
        Put_Line("Program counter: " & To_Image(Environment.Program_Counter, 2));
<<ENDING>>
Put_Line( "Put_Stack has been disabled due to ASM bug." );
      end Put_Stack;
  end Implementation_For_Architecture;
