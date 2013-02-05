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
separate(Neo.System.Processor)
package body Implementation_For_Architecture
  is
  -------------
  -- Numbers --
  -------------
    type Integer_Bit_Field_Element
      is new Integer range 0..31;
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
    type Record_Feature
      is record
        Function_ID : Integer_4_Unsigned        := 0;
        Bit         : Integer_Bit_Field_Element := 0;
        Register    : Enumerated_Register       := EAX_Register;
      end record;
    type Record_Value
      is record
        Function_ID : Integer_4_Unsigned        := 0;
        Start_Bit   : Integer_Bit_Field_Element := 0;
        End_Bit     : Integer_Bit_Field_Element := 0;
        Register    : Enumerated_Register       := EAX_Regsiter;
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
      end record;
      for Record_x86_Environment'Size
        use 224;
  ---------------
  -- Constants --
  ---------------
    DO_CRASH_ON_INEXACT_RESULT       : constant Boolean            := False;
    DO_CRASH_ON_DIVIDE_BY_ZERO       : constant Boolean            := False;
    DO_CRASH_ON_NUMERIC_OVERFLOW     : constant Boolean            := False;
    DO_CRASH_ON_INVALID_OPERATION    : constant Boolean            := False;
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
    INTEL_SSE3                       : constant Record_Feature     := (16#0000_0001#,  0, ECX_Register);
    INTEL_PCLMULQDQ                  : constant Record_Feature     := (16#0000_0001#,  1, ECX_Register);
    INTEL_SSSE3                      : constant Record_Feature     := (16#0000_0001#,  9, ECX_Register);
    INTEL_FMA3                       : constant Record_Feature     := (16#0000_0001#, 12, ECX_Register);
    INTEL_SSE4_1                     : constant Record_Feature     := (16#0000_0001#, 19, ECX_Register);
    INTEL_SSE4_2                     : constant Record_Feature     := (16#0000_0001#, 20, ECX_Register);
    INTEL_POPCNT                     : constant Record_Feature     := (16#0000_0001#, 23, ECX_Register);
    INTEL_AES                        : constant Record_Feature     := (16#0000_0001#, 25, ECX_Register);
    INTEL_OSXSAVE                    : constant Record_Feature     := (16#0000_0001#, 27, ECX_Register);
    INTEL_AVX                        : constant Record_Feature     := (16#0000_0001#, 28, ECX_Register);
    INTEL_F16C                       : constant Record_Feature     := (16#0000_0001#, 29, ECX_Register);
    INTEL_CMOV                       : constant Record_Feature     := (16#0000_0001#, 15, EDX_Register);
    INTEL_MMX                        : constant Record_Feature     := (16#0000_0001#, 23, EDX_Register);
    INTEL_FXSR                       : constant Record_Feature     := (16#0000_0001#, 24, EDX_Register);
    INTEL_SSE                        : constant Record_Feature     := (16#0000_0001#, 25, EDX_Register);
    INTEL_SSE2                       : constant Record_Feature     := (16#0000_0001#, 26, EDX_Register);
    INTEL_HTT                        : constant Record_Feature     := (16#0000_0001#, 28, EDX_Register);
    INTEL_BMI1                       : constant Record_Feature     := (16#0000_0007#,  3, EBX_Register);
    INTEL_AVX2                       : constant Record_Feature     := (16#0000_0007#,  5, EBX_Register);
    INTEL_BMI2                       : constant Record_Feature     := (16#0000_0007#,  8, EBX_Register);
    INTEL_INVPCID                    : constant Record_Feature     := (16#0000_0007#, 10, EBX_Register);
    AMD_ABM                          : constant Record_Feature     := (16#8000_0001#,  5, ECX_Register);
    AMD_SSE4_A                       : constant Record_Feature     := (16#8000_0001#,  6, ECX_Register);
    AMD_XOP                          : constant Record_Feature     := (16#8000_0001#, 11, ECX_Register);
    AMD_FMA4                         : constant Record_Feature     := (16#8000_0001#, 16, ECX_Register);
    AMD_CVT16                        : constant Record_Feature     := (16#8000_0001#, 19, ECX_Register);
    AMD_MMX_PLUS                     : constant Record_Feature     := (16#8000_0001#, 22, EDX_Register);
    AMD_3DNOW_PLUS                   : constant Record_Feature     := (16#8000_0001#, 30, EDX_Register);
    AMD_3DNOW                        : constant Record_Feature     := (16#8000_0001#, 31, EDX_Register);
    AMD_CPU_COUNT                    : constant Record_Value       := (16#8000_0008#,  0,  7, ECX_Register); -- One plus this value
    INTEL_CPU_COUNT                  : constant Record_Value       := (16#0000_0001#, 16, 23, EBX_Register);
    INTEL_APIC                       : constant Record_Value       := (16#0000_0001#, 24, 31, EBX_Register);
    PROLOGUE_SIGNATURE               : constant Integer_4_Unsigned := 16#00EC_8B55#;
  ----------------
  -- Initialize --
  ----------------
    function Initialize
      return Record_Processor
      is
      -----------------------
      function Execute_CPUID(
      -----------------------
        Function_ID : in Integer_4_Unsigned;
        Register    : in Enumerated_Register)
        return Integer_4_Unsigned
        is
        A, B, C, D : Integer_4_Unsigned := 0;
        begin
          Asm(
            Template => "cpuid",
            Volatile => True,
            Inputs   => Integer_4_Unsigned'Asm_Input(TO_EAX, Function_ID),
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
      -----------------------
      function Check_Feature(
      -----------------------
        Feature : in Record_Feature)
        return Boolean
        is
        begin
          return(Execute_CPUID(Feature.Function_ID, Feature.Register) and 2**Integer(Feature.Bit)) /= 0;
        end Check_Feature;
      ---------------------
      function Check_Value(
      ---------------------
        Value : in Record_Value)
        return Integer_4_Signed
        is
        begin
          return
            Integer(
              Shift_Right(
                Shift_Left(
                  Value  => Execute_CPUID(Value.Function_ID, Value.Register),
                  Amount => Integer(Integer_Bit_Field_Element'Last - Value.End_Bit)),
                Integer(Integer_Bit_Field_Element'Last - Value.End_Bit - Value.Start_Bit)));
        end Check_Value;
      -------------------
      function Get_Vendor
      -------------------
        return String_1
        is
        type Array_String_Segment
          is array (1..4) of Character;
        B, D, C : Array_String_Segment := (others => Ascii.Nul);
        begin
          Asm(
            Template => "cpuid",
            Volatile => True,
            Inputs   => Integer_4_Unsigned'Asm_Input(TO_EAX, 0),
            Outputs  =>(
              Array_String_Segment'Asm_Output(FROM_EBX, B),
              Array_String_Segment'Asm_Output(FROM_ECX, C),
              Array_String_Segment'Asm_Output(FROM_EDX, D)));
          return String_1(B) & String_1(D) & String_1(C);
        end Get_Vendor;
      Processor : Record_Processor   := <>;
      Data      : Integer_4_Unsigned := 0;
      Did_Fail  : Boolean            := False;
      -----
      begin
      -----
        if not USE_64_BIT then
          Asm(
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
        end if;
        if USE_64_BIT or Data /= 0 then
          ---------------------
          Find_Processor_Speed:
          ---------------------
            declare
            begin
              Processor.Speed_In_Megahertz := Get_Speed_In_Megahertz;
            exception
              when System_Call_Failure =>
                ---------------
                Time_Processor:
                ---------------
                  declare
                  Start : Integer_8_Unsigned := 0;
                  begin
                    Start := Get_Clock_Ticks;
                    Sleep(1_000);
                    Processor.Speed_In_Megahertz := (Get_Clock_Ticks - Start) / 1_000;
                  end Time_Processor;
            end Find_Processor_Speed;
          end if;
          --------------------------
          Find_Number_Of_Processors:
          --------------------------
            declare
            begin
              Processor.Number_Of_Processors := Get_Number_Of_Processors;
            exception
              when System_Call_Failure =>
                Did_Fail := True;
            end Find_Number_Of_Processors;
          if Get_Vendor = VENDOR_ADVANCED_MICRO_DEVICES then
            Processor.Vendor                                     := Advanced_Micro_Devices_Vendor;
            Processor.Has_3DNow                                  := Check_Feature(AMD_3DNOW);
            Processor.Has_3DNow_Supplement                       := Check_Feature(AMD_3DNOW_PLUS);
            Processor.Has_Multi_Media_Extensions_Supplement      := Check_Feature(AMD_MMX_PLUS);
            Processor.Has_Streaming_SIMD_Extensions_4_Supplement := Check_Feature(AMD_SSE4_A);
            if Did_Fail then
              Processor.Number_Of_Processors := Check_Value(AMD_CPU_COUNT) + 1;
            end if;
          else
            Processor.Vendor := Intel_Vendor;
            if Did_Fail then
              Processor.Number_Of_Processors := Check_Value(INTEL_CPU_COUNT); -- Inaccurate
            end if;
          end if;
          Processor.Has_Leading_Zero_Count                     := Check_Feature(AMD_ABM);
          Processor.Has_Extended_Operation_Support             := Check_Feature(AMD_XOP);
          Processor.Has_High_Precision_Convert                 := Check_Feature(AMD_CVT16);
          Processor.Has_Fused_Multiply_Add_4                   := Check_Feature(AMD_FMA4);
          Processor.Has_Fused_Multiply_Add_3                   := Check_Feature(INTEL_FMA3);
          Processor.Has_Population_Count                       := Check_Feature(INTEL_POPCNT);
          Processor.Has_Processor_Extended_States_Enabled      := Check_Feature(INTEL_OSXSAVE);
          Processor.Has_Multi_Media_Extensions                 := Check_Feature(INTEL_MMX);
          Processor.Has_Streaming_SIMD_Extensions_1            := Check_Feature(INTEL_SSE);
          Processor.Has_Streaming_SIMD_Extensions_2            := Check_Feature(INTEL_SSE2);
          Processor.Has_Streaming_SIMD_Extensions_3            := Check_Feature(INTEL_SSE3);
          Processor.Has_Streaming_SIMD_Extensions_3_Supplement := Check_Feature(INTEL_SSSE3);
          Processor.Has_Streaming_SIMD_Extensions_4_1          := Check_Feature(INTEL_SSE4_1);
          Processor.Has_Streaming_SIMD_Extensions_4_2          := Check_Feature(INTEL_SSE4_2);
          Processor.Has_Carryless_Multiplication_Of_Two_64_Bit := Check_Feature(INTEL_PCLMULQDQ);
          Processor.Has_Bit_Manipulation_Extensions_1          := Check_Feature(INTEL_BMI1);
          Processor.Has_Bit_Manipulation_Extensions_2          := Check_Feature(INTEL_BMI2);
          Processor.Has_Advanced_Vector_Extensions_1           := Check_Feature(INTEL_AVX);
          Processor.Has_Advanced_Vector_Extensions_2           := Check_Feature(INTEL_AVX2);
          Processor.Has_Advanced_Encryption_Service            := Check_Feature(INTEL_AES);
          Processor.Has_Half_Precision_Floating_Point_Convert  := Check_Feature(INTEL_F16C);
          Processor.Has_Conditional_Move                       := Check_Feature(INTEL_CMOV);
          Processor.Has_Hyperthreading                         := Check_Feature(INTEL_HTT);
          Processor.Has_Advanced_State_Operations              := Check_Feature(INTEL_FXSR);
          if
          Processor.Has_Streaming_SIMD_Extensions_4_2 and then(
          Get_Operating_System_Version = Mach__Version or
          Get_Operating_System_Version = Windows_2_6_1_Version or
          Get_Operating_System_Version = Linux_2_7_Version)
          then
            Asm(
              Template => "xgetbv",
              Inputs   => Integer_4_Unsigned'Asm_Input(TO_ECX, 0),
              Outputs  => Integer_4_Unsigned'Asm_Output(FROM_EAX, Data),
              Volatile => True);
            Processor.Has_Advanced_Vector_Extensions_Enabled := (Data and 16#0000_0006#) /= 16#0000_0006#;
          end if;
          if Processor.Has_Advanced_State_Operations then
            --------------------------
            Enable_Denormals_Are_Zero:
            --------------------------
              declare
              type Array_Save_Area
                is array (1..512)
                of Integer_1_Unsigned;
                for Array_Save_Area'Alignment use 16;
              Save_Area : aliased Array_Save_Area := (others => 0);
              begin
                Asm(
                  ----------------------------------------
                  " fxsave (%%eax)          " & END_LINE &
                  " movl   28(%%eax), %%ebx " & END_LINE ,
                  ----------------------------------------
                  Inputs   => Address'Asm_Input(TO_EAX, Save_Area'Address),
                  Outputs  => Integer_4_Unsigned'Asm_Output(FROM_EBX, Data),
                  Volatile => True);
                if (Data and 16#20#) /= 0 then
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
                  Processor.Has_Denormals_Are_Zero := True;
                end if;
              end Enable_Denormals_Are_Zero;
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
            Processor.Has_Flush_To_Zero := True;
          end if;
        end if;
        ---------------
        Set_Exceptions:
        ---------------
          declare
          Other_Data     : aliased Integer_2_Unsigned := 0;
          Exception_Mask :         Integer_4_Unsigned := 0;
          begin
            if not DO_CRASH_ON_INEXACT_RESULT then
              Exception_Mask := Exception_Mask or 16#1000#;
            end if;
            if not DO_CRASH_ON_NUMERIC_UNDERFLOW then
              Exception_Mask := Exception_Mask or 16#0800#;
            end if;
            if NOT DO_CRASH_ON_NUMERIC_OVERFLOW then
              Exception_Mask := Exception_Mask or 16#0400#;
            end if;
            if not DO_CRASH_ON_DIVIDE_BY_ZERO then
              Exception_Mask := Exception_Mask or 16#0200#;
            end if;
            if not DO_CRASH_ON_DENORMALIZED_OPERAND then
              Exception_Mask := Exception_Mask or 16#0100#;
            end if;
            if not DO_CRASH_ON_INVALID_OPERATION then
              Exception_Mask := Exception_Mask or 16#0080#;
            end if;
            if Processor.Has_Advanced_State_Operations then
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
            end if;
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
          end Set_Exceptions;
        return Processor;
      end Initialize;
  ----------------------
  -- Check_Exceptions --
  ----------------------
    procedure Check_Exceptions(
      Processor : in Record_Processor)
      is
      Data          : Integer_4_Unsigned := 0;
      Data_From_x87 : Integer_2_Unsigned := 0;
      begin
        if Processor.Has_Advanced_State_Operations then
          Asm(
            Template => "stmxcsr (%%eax)",
            Inputs   => Integer_4_Unsigned'Asm_Input(TO_EAX, To_Integer_4_Unsigned(Data'Address)),
            Volatile => True);
        end if;
        Asm(
          Template => "fnstsw (%%eax)",
          Inputs   => Integer_4_Unsigned'Asm_Input(TO_EAX, To_Integer_4_Unsigned(Data_From_x87'Address)),
          Volatile => True);
        Data := (Data and 16#3F#) and Integer_4_Unsigned(Data_From_x87);
        if Data /= 0 then -- Raise the first error found
          ---------------------
          Clear_Exception_Bits:
          ---------------------
            declare
            Data_From_SIMD  : Integer_4_Unsigned     := 0;
            x86_Environment : Record_x86_Environment := <>;
            begin
              if Processor.Has_Advanced_State_Operations then
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
              end if;
              Asm(
                Template => "fnstenv (%%eax)",
                Volatile => True,
                Inputs   => Integer_4_Unsigned'Asm_Input(TO_EAX, To_Integer_4_Unsigned(x86_Environment'Address)));
              x86_Environment.Status_Word := x86_Environment.Status_Word and 16#FF80#; -- Clear 6 exception bits plus stack fault
              Asm(
                Template => "fldenv (%%eax)",
                Volatile => True,
                Inputs   => Integer_4_Unsigned'Asm_Input(TO_EAX, To_Integer_4_Unsigned(x86_Environment'Address)));
              Asm(
                Template => "fnclex",
                Volatile => True); 
            end Clear_Exception_Bits;
          if (Data and 16#01#) /= 0 then
            raise Invalid_Operation;
          end if;
          if (Data and 16#02#) /= 0 then
            raise Denormalized_Operand;
          end if;
          if (Data and 16#04#) /= 0 then
            raise Divide_By_Zero;
          end if;
          if (Data and 16#08#) /= 0 then
            raise Numeric_Overflow;
          end if;
          if (Data and 16#10#) /= 0 then
            raise Numeric_Underflow;
          end if;
          if (Data and 16#20#) /= 0 then
            raise Inexact_Result;
          end if;
          if (Data and 16#40#) /= 0 then
            raise Stack_Fault;
          end if;
        end if;
      end Check_Exceptions;
  ------------------
  -- Set_Rounding --
  ------------------
    function Set_Rounding(
      Processor : in out Record_Processor;
      Rounding  : in     Enumerated_Rounding)
      return Record_Processor
      is
      Data          : Integer_4_Unsigned := 0;
      Rounding_Mask : Integer_4_Unsigned := 0;
      Other_Data    : Integer_2_Unsigned := 0;
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
        if Processor.Has_Advanced_State_Operations then
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
        end if;
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
        Processor.Rounding := Rounding;
      end Set_Rounding;
  -------------------
  -- Set_Precision --
  -------------------
    function Set_Precision(
      Processor : in out Record_Processor;
      Precision : in     Enumerated_Precision)
      return Record_Processor
      is
      Blank_Memory   : Integer_2_Unsigned := 0;
      Precision_Mask : Integer_4_Unsigned := 0;
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
        Processor.Precision := Precision;
      end Set_Precision;
  --------------------
  -- Get_Clock_Tics --
  --------------------
    function Get_Clock_Ticks
      return Integer_8_Unsigned
      is
      begin
        return Get_Clock_Ticks;
      exception
        when System_Call_Failure =>
          --------------------------
          Get_It_By_Way_Of_Assembly:
          --------------------------
            declare
            Low_Part  : Integer_4_Unsigned := 0;
            High_Part : Integer_4_Unsigned := 0;
            begin
              Asm(
                ----------------------
                " cpuid " & END_LINE &
                " rdtsc " & END_LINE ,
                ----------------------
                Volatile => True,
                Outputs  =>(
                  Integer_4_Unsigned'Asm_Output(FROM_EAX, Low_Part),
                  Integer_4_Unsigned'Asm_Output(FROM_EDX, High_Part)));
              return Shift_Left(Integer_8_Unsigned(High_Part), 32) + Integer_8_Unsigned(Low_Part);
            end Get_It_By_Way_Of_Assembly;
      end Get_Clock_Ticks;
  --------------------
  -- Is_Stack_Empty --
  --------------------
    function Is_Stack_Empty
      return Boolean
      is
      Result : Integer_4_Unsigned     := 0;
      Data   : Record_x86_Environment := <>; 
      begin
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
          Inputs   => Integer_4_Unsigned'Asm_Input(TO_EAX, To_Integer_4_Unsigned(Data'Address)),
          Outputs  => Integer_4_Unsigned'Asm_Output(FROM_EAX, Result));
        return Result /= 0;
      end Is_Stack_Empty;
  -----------------
  -- Clear_Stack --
  -----------------
    procedure Clear_Stack
      is
      Data : Record_x86_Environment := <>;
      begin
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
          Inputs   => Integer_4_Unsigned'Asm_Input(TO_EAX, To_Integer_4_Unsigned(Data'Address)));
      end Clear_Stack;
  --------------------
  -- Put_Call_Stack --
  --------------------
    procedure Put_Call_Stack
      is
      begin
        Put_Call_Stack;
      exception
        when System_Call_Failure => 
          ----------------------------------------------------------
          Find_Subprogram_Addresses_Via_Prologue_Signature_Traverse:
          ----------------------------------------------------------
            declare
            Value_At_Position : Access_Integer_4_Unsigned := null;
            Data              : Integer_4_Unsigned        := 0;
            J                 : Integer_4_Signed          := 0;
            Base_Pointer      : Address                   := NULL_ADDRESS;
            Mid_Point         : Address                   := NULL_ADDRESS;
            Call_Stack:
              array(1..CALLBACK_TRACE_LIMIT)
              of Address := (others => NULL_ADDRESS);
            begin
              Put_Line("Call stack:");
              Asm(
                Template => "movl %%ebp, %%eax",
                Volatile => True,
                Outputs  => Address'Asm_Output(FROM_EAX, Base_Pointer));
              Base_Pointer := NULL_ADDRESS;-- *(long*) *(long*) Base_Pointer;
              for I in Call_Stack'Range loop
                Asm(
                  ----------------------------------------
                  "   movl  (%%eax), %%ecx  " & END_LINE &
                  "   test  %%ecx,   %%ecx  " & END_LINE &
                  "   jz    terminus        " & END_LINE &
                  ----------------------------------------
                  "   movl  4(%%eax), %%eax " & END_LINE &
                  "   test  %%eax,    %%eax " & END_LINE &
                  "   jz    terminus        " & END_LINE &
                  ----------------------------------------
                  "   movl  $0x1,     %%ebx " & END_LINE &
                  ----------------------------------------
                  " terminus:               " & END_LINE ,
                  ----------------------------------------
                  Volatile => True,
                  Inputs   => Address'Asm_Input(TO_EAX, Base_Pointer),
                  Outputs  =>(
                    Address           'Asm_Output(FROM_EAX, Mid_Point),
                    Integer_4_Unsigned'Asm_Output(FROM_EBX, Data)));
                if Data = C_TRUE then
                  Data := To_Integer_4_Unsigned(Mid_Point); -- Becomes index
                  loop
                    Value_At_Position := To_Access_Integer_4_Unsigned(To_Address(Data));
                    exit when (Value_At_Position.All and PROLOGUE_MASK) = PROLOGUE_SIGNATURE;
                    Data := Data - 1;
                  end loop;
                  Call_Stack(I) := To_Address(Data);
                else
                  J := I;
                  exit;
                end if;
              end loop;
              Base_Pointer := NULL_ADDRESS;-- *(long*) Base_Pointer;
              while J < Call_Stack'Length loop
                Call_Stack(J) := 0;
                J := J + 1;
              end loop;
              for I in reverse 1..Call_Stack'Size loop
                Put(Integer_4_Signed'Wide_Image(I));
                Put_Line(": " & To_Hex(To_Integer_4_Unsigned(Call_Stack.All(I))));
              end loop;
            end Find_Subprogram_Addresses_Via_Prologue_Signature_Traverse;
      end Put_Call_Stack;
  ---------------
  -- Put_State --
  ---------------
    procedure Put_State(
      Processor : in Record_Processor)
      is
      Number_Of_Values     :         Integer_4_Unsigned           := 0;
      Data_From_Extensions :         Integer_4_Unsigned           := 0;
      Stack                : aliased array (1..8) of Float_8_Real := (others => 0.0);
      Environment          : aliased Record_x86_Environment       := <>;
      begin
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
          Inputs   =>(
            Address'Asm_Input(TO_EAX, Environment'Address),
            Address'Asm_Input(TO_EDI, Stack'Address)),
          Outputs  =>
            Integer_4_Unsigned'Asm_Output(FROM_EAX, Number_Of_Values));
        if Number_Of_Values <= Stack'Size then
          for I in 1..Integer(Number_Of_Values) loop
            Put_Line("Stack" & Integer'Wide_Image(I) & ": " & Float_8_Real'Wide_Image(Stack(I)));
          end loop;
        end if;
        if Processor.Has_Advanced_State_Operations then
          Asm(
            Template => "stmxcsr (%%eax)",
            Volatile => True,
            Inputs   => Address'Asm_Input(TO_EAX, Data_From_Extensions'Address));
          Put_Line("Extensions: " & Binary_Image(Data_From_Extensions, True));
        end if;
        Put_Line("Control word: "    & Hexadecimal_Image(Environment.Control_Word));
        Put_Line("Status word: "     & Binary_Image(Environment.Status_Word,     True));
        Put_Line("Selector: "        & Binary_Image(Environment.CS_Selector,     True));
        Put_Line("Tags: "            & Binary_Image(Environment.Tags,            True));
        Put_Line("Data offset: "     & Binary_Image(Environment.Data_Offset,     True));
        Put_Line("Data selector: "   & Binary_Image(Environment.Data_Selector,   True));
        Put_Line("Operation code: "  & Binary_Image(Environment.Operation_Code,  True));
        Put_Line("Program counter: " & Binary_Image(Environment.Program_Counter, True));
      end Put_State;
  end Implementation_For_Architecture;
