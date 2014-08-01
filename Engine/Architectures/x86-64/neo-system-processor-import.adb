separate(Neo.System.Processor) package body Import is
    CPUID_Is_Not_Supported : Exception;
    type Enumerated_Register is (EAX_Register, EBX_Register, ECX_Register, EDX_Register);
    type Record_x86_Environment is record
        Control_Word    : Integer_2_Unsigned := 0;
        Unused_A        : Integer_2_Unsigned := 0;
        Status_Word     : Integer_2_Unsigned := 0;
        Unused_B        : Integer_2_Unsigned := 0;
        Tags            : Integer_2_Unsigned := 0;
        Unused_C        : Integer_2_Unsigned := 0;
        Program_Counter : Integer_4_Unsigned := 0;
        CS_Selector     : Integer_2_Unsigned := 0;
        Operation_Code  : Integer_2_Unsigned := 0; -- Consists of 11 used + 5 unused bits
        Data_Offset     : Integer_4_Unsigned := 0;
        Data_Selector   : Integer_2_Unsigned := 0;
        Unused_D        : Integer_2_Unsigned := 0;
      end record with size => 28 * Byte'size;
    TO_ESI   : constant String_1 := "s";
    TO_EDI   : constant String_1 := "d";
    TO_EAX   : constant String_1 := "a";
    TO_EBX   : constant String_1 := "b";
    TO_ECX   : constant String_1 := "c";
    TO_EDX   : constant String_1 := "d";
    FROM_EAX : constant String_1 := "=a";
    FROM_EBX : constant String_1 := "=b";
    FROM_ECX : constant String_1 := "=c";
    FROM_EDX : constant String_1 := "=d";
    function Get_Specifics return Record_Specifics is
      function Get_Vendor return Enumerated_Vendor is -- "Oh, such a sweet sweet trick, just not very useful. :)"
        Vendor : String_1(1..12) := (others => NULL_CHARACTER_1);
        begin
          Asm(
            Volatile => True,
            Inputs   => Integer_4_Unsigned'Asm_Input(TO_EAX, 0),
            Template => "cpuid",
            Outputs  =>(
              String_1'Asm_Output(FROM_EBX, Vendor(1..4)),
              String_1'Asm_Output(FROM_EDX, Vendor(5..8)),
              String_1'Asm_Output(FROM_ECX, Vendor(9..12))));
          return(
            if    Vendor = "AuthenticAMD" then Advanced_Micro_Devices_Vendor
            elsif Vendor = "GenuineIntel" then Intel_Vendor
            else                               Unknown_Vendor);
        end Get_Vendor;
      function Is_Enabled(Function_ID : in Integer_4_Unsigned; Register : in Enumerated_Register; Bit : in Integer_4_Natural) return Boolean with pre => Bit < Integer_4_Unsigned'size;
      function Is_Enabled(Function_ID : in Integer_4_Unsigned; Register : in Enumerated_Register; Bit : in Integer_4_Natural) return Boolean is
        Data : array(Enumerated_Register'range) of Integer_4_Unsigned := (others => 0);
        begin
          Asm(
            Volatile => True,
            Inputs   => Integer_4_Unsigned'Asm_Input(TO_EAX, Function_ID),
            Template => "cpuid",
            Outputs  =>(
              Integer_4_Unsigned'Asm_Output(FROM_EAX, Data(EAX_Register)),
              Integer_4_Unsigned'Asm_Output(FROM_EBX, Data(EBX_Register)),
              Integer_4_Unsigned'Asm_Output(FROM_ECX, Data(ECX_Register)),
              Integer_4_Unsigned'Asm_Output(FROM_EDX, Data(EDX_Register))));
          return (Data(Register) and 2**Bit) /= 0;
        end Is_Enabled;
      Specifics :         Record_Specifics(Get_Vendor);
      Data      : aliased Integer_4_Unsigned := 0;
      begin
        if Specifics.Vendor = Advanced_Micro_Devices_Vendor then
          Specifics.Has_3DNow                                  := Is_Enabled(16#8000_0001#, EDX_Register, 31); -- 3DNow!
          Specifics.Has_3DNow_Supplement                       := Is_Enabled(16#8000_0001#, EDX_Register, 30); -- 3DNow!+
          Specifics.Has_Multi_Media_Extensions_Supplement      := Is_Enabled(16#8000_0001#, EDX_Register, 22); -- MMX+
          Specifics.Has_Streaming_SIMD_Extensions_4_Supplement := Is_Enabled(16#8000_0001#, ECX_Register,  6); -- SSE4a
        end if;
        Specifics.Has_Extended_States_Enabled                  := Is_Enabled(16#0000_0001#, ECX_Register, 27); -- OSXSAVE
        Specifics.Has_Population_Count                         := Is_Enabled(16#0000_0001#, ECX_Register, 23); -- POPCNT
        Specifics.Has_Multi_Media_Extensions                   := Is_Enabled(16#0000_0001#, EDX_Register, 23); -- MMX
        Specifics.Has_Streaming_SIMD_Extensions_1              := Is_Enabled(16#0000_0001#, EDX_Register, 25); -- SSE
        Specifics.Has_Streaming_SIMD_Extensions_2              := Is_Enabled(16#0000_0001#, EDX_Register, 26); -- SSE2
        Specifics.Has_Streaming_SIMD_Extensions_3              := Is_Enabled(16#0000_0001#, ECX_Register,  0); -- SSE3
        Specifics.Has_Streaming_SIMD_Extensions_3_Supplement   := Is_Enabled(16#0000_0001#, ECX_Register,  9); -- SSSE3
        Specifics.Has_Streaming_SIMD_Extensions_4_1            := Is_Enabled(16#0000_0001#, ECX_Register, 19); -- SSE4.1
        Specifics.Has_Streaming_SIMD_Extensions_4_2            := Is_Enabled(16#0000_0001#, ECX_Register, 20); -- SSE4.2
        Specifics.Has_Carryless_Multiplication_Of_Two_64_Bit   := Is_Enabled(16#0000_0001#, ECX_Register,  1); -- PCLMULQDQ
        Specifics.Has_Bit_Manipulation_Extensions_1            := Is_Enabled(16#0000_0007#, EBX_Register,  3); -- BMI1
        Specifics.Has_Bit_Manipulation_Extensions_2            := Is_Enabled(16#0000_0007#, EBX_Register,  8); -- BMI2
        Specifics.Has_Advanced_Vector_Extensions_1             := Is_Enabled(16#0000_0001#, ECX_Register, 28); -- AVX
        Specifics.Has_Advanced_Vector_Extensions_2             := Is_Enabled(16#0000_0007#, EBX_Register,  5); -- AVX2
        Specifics.Has_Advanced_Encryption_Service              := Is_Enabled(16#0000_0001#, ECX_Register, 25); -- AES
        Specifics.Has_Half_Precision_Floating_Point_Convert    := Is_Enabled(16#0000_0001#, ECX_Register, 29); -- F16C
        Specifics.Has_Conditional_Move                         := Is_Enabled(16#0000_0001#, EDX_Register, 15); -- CMOV
        Specifics.Has_Hyperthreading                           := Is_Enabled(16#0000_0001#, EDX_Register, 28); -- HTT
        Specifics.Has_Advanced_State_Operations                := Is_Enabled(16#0000_0001#, EDX_Register, 24); -- FXSR
        Specifics.Has_Fused_Multiply_Add_3                     := Is_Enabled(16#0000_0001#, ECX_Register, 12); -- FMA3
        Specifics.Has_Fused_Multiply_Add_4                     := Is_Enabled(16#8000_0001#, ECX_Register, 16); -- FMA4
        Specifics.Has_High_Precision_Convert                   := Is_Enabled(16#8000_0001#, ECX_Register, 19); -- CVT16
        Specifics.Has_Extended_Operation_Support               := Is_Enabled(16#8000_0001#, ECX_Register, 11); -- XOP
        Specifics.Has_Leading_Zero_Count                       := Is_Enabled(16#8000_0001#, ECX_Register,  5); -- ABM
        if Specifics.Has_Streaming_SIMD_Extensions_4_2 and then Is_Supported(REQUIREMENTS_FOR_AVX) then
          Asm( -- Check if the operating system will save the YMM registers
            Volatile => True,
            Inputs   => Integer_4_Unsigned'Asm_Input(TO_ECX, 0),
            Template => "xgetbv",
            Outputs  => Integer_4_Unsigned'Asm_Output(FROM_EAX, Data));
          Specifics.Has_Advanced_Vector_Extensions_Enabled := (Data and 16#0000_0006#) > 0;
        end if;
        return Specifics;
      end Get_Specifics;
    procedure Check_Exceptions is
      Data          : aliased Integer_4_Unsigned := 0;
      Data_From_x87 : aliased Integer_2_Unsigned := 0;
      begin
        if SPECIFICS.Has_Advanced_State_Operations then
          Asm(
            Volatile => True,
            Inputs   => Address'Asm_Input(TO_EAX, Data'address),
            Template => "stmxcsr (%%eax)");
        end if;
        Asm(
          Volatile => True,
          Inputs   => Address'Asm_Input(TO_EAX, Data_From_x87'address),
          Template => "fnstsw (%%eax)");
        Data := (Data and 16#0000_003F#) and Integer_4_Unsigned(Data_From_x87);
        if Data /= 0 then -- Raise the first error found
          Clear_Exception_Bits:
            declare
            Data_From_SIMD  :         Integer_4_Unsigned     := 0;
            x86_Environment : aliased Record_x86_Environment := (others => <>);
            begin
              if SPECIFICS.Has_Advanced_State_Operations then
                Asm(
                  Volatile => True,
                  Inputs   => Address'Asm_Input(TO_EAX, Data_From_SIMD'address),
                  Template =>
                    -----------------------------------------------
                    " stmxcsr (%%eax)              " & END_LINE_1 &
                    " movl    (%%eax),     %%ebx   " & END_LINE_1 &
                    " and     $0xffffffc0, %%ebx   " & END_LINE_1 &
                    " movl    %%ebx,       (%%eax) " & END_LINE_1 &
                    " ldmxcsr (%%eax)              " & END_LINE_1);
                    -----------------------------------------------
              end if;
              Asm(
                Volatile => True,
                Inputs   => Address'Asm_Input(TO_EAX, x86_Environment'address),
                Template => "fnstenv (%%eax)");
              -- Clear 6 exception bits plus stack fault
              x86_Environment.Status_Word := x86_Environment.Status_Word and 16#FF80#;
              Asm(
                Volatile => True,
                Inputs   => Address'Asm_Input(TO_EAX, x86_Environment'address),
                Template => "fldenv (%%eax)");
              Asm(
                Volatile => True,
                Template => "fnclex");
            end Clear_Exception_Bits;
          if (Data and 16#0000_0001#) /= 0 then raise Invalid_Operation;    end if;
          if (Data and 16#0000_0002#) /= 0 then raise Denormalized_Operand; end if;
          if (Data and 16#0000_0004#) /= 0 then raise Divide_By_Zero;       end if;
          if (Data and 16#0000_0008#) /= 0 then raise Numeric_Overflow;     end if;
          if (Data and 16#0000_0010#) /= 0 then raise Numeric_Underflow;    end if;
          if (Data and 16#0000_0020#) /= 0 then raise Inexact_Result;       end if;
          if (Data and 16#0000_0040#) /= 0 then raise Stack_Fault;          end if;
        end if;
      end Check_Exceptions;
    procedure Set_Rounding(Rounding : in Enumerated_Rounding) is
      Other_Data    : aliased Integer_2_Unsigned := 0;
      Data          : aliased Integer_4_Unsigned := 0;
      Rounding_Mask :         Integer_4_Unsigned :=(
        case Rounding is
          when Nearest_Rounding  => 16#0000#,
          when Down_Rounding     => 16#2000#,
          when Up_Rounding       => 16#4000#,
          when Truncate_Rounding => 16#6000#);
      begin
        if SPECIFICS.Has_Advanced_State_Operations then
          Asm(
            Volatile => True,
            Inputs   =>(
              Address'Asm_Input(TO_EAX, Data'address),
              Integer_4_Unsigned'Asm_Input(TO_ECX, Rounding_Mask)),
            Template =>
              -----------------------------------------------
              " stmxcsr (%%eax)              " & END_LINE_1 &
              " movl    (%%eax),     %%ebx   " & END_LINE_1 &
              " and     $0xffff9fff, %%ebx   " & END_LINE_1 &
              " or      %%cx,        %%bx    " & END_LINE_1 &
              " movl    %%ebx,       (%%eax) " & END_LINE_1 &
              " ldmxcsr (%%eax)              " & END_LINE_1);
              -----------------------------------------------
        end if;
        Rounding_Mask := Shift_Right(Rounding_Mask, 3);
        Asm(
          Volatile => True,
          Inputs   =>(
            Address'Asm_Input(TO_EAX, Other_Data'address),
            Integer_4_Unsigned'Asm_Input(TO_ECX, Rounding_Mask)),
          Template =>
            ------------------------------------------
            " fnstcw (%%eax)          " & END_LINE_1 &
            " movw   (%%eax), %%bx    " & END_LINE_1 &
            " and    $0xf3ff, %%bx    " & END_LINE_1 &
            " or     %%cx,    %%bx    " & END_LINE_1 &
            " movw   %%bx,    (%%eax) " & END_LINE_1 &
            " fldcw  (%%eax)          " & END_LINE_1);
            ------------------------------------------
      end Set_Rounding;
    procedure Set_Precision(Precision : in Enumerated_Precision) is
      Blank_Memory : aliased Integer_2_Unsigned := 0;
      begin
        Asm(
          Volatile => True,
          Inputs   =>(
            Address           'Asm_Input(TO_EAX, Blank_Memory'address),
            Integer_4_Unsigned'Asm_Input(TO_ECX,(
              case Precision is
                when Single_Precision          => 16#0000#,
                when Double_Precision          => 16#0200#,
                when Double_Extended_Precision => 16#0300#))),
          Template =>
            ------------------------------------------
            " fnstcw (%%eax)          " & END_LINE_1 &
            " movw   (%%eax), %%bx    " & END_LINE_1 &
            " and    $0xfcff, %%bx    " & END_LINE_1 &
            " or     %%cx,    %%bx    " & END_LINE_1 &
            " movw   %%bx,    (%%eax) " & END_LINE_1 &
            " fldcw  (%%eax)          " & END_LINE_1);
            ------------------------------------------
      end Set_Precision;
    function Get_Clock_Ticks return Integer_8_Unsigned is
      Low_Part  : Integer_4_Unsigned := 0;
      High_Part : Integer_4_Unsigned := 0;
      begin
        Asm(
          Volatile => True,
          Template =>
            ----------------------
            " cpuid " & END_LINE_1 &
            " rdtsc " & END_LINE_1 ,
            ----------------------
          Outputs  =>(
            Integer_4_Unsigned'Asm_Output(FROM_EAX, Low_Part),
            Integer_4_Unsigned'Asm_Output(FROM_EDX, High_Part)));
        return Shift_Left(Integer_8_Unsigned(High_Part), Integer_4_Unsigned'size) + Integer_8_Unsigned(Low_Part);
      end Get_Clock_Ticks;
    function Is_Stack_Empty return Boolean is
      Result :         Integer_4_Unsigned     := 0;
      Data   : aliased Record_x86_Environment := (others => <>);
      begin
        Asm(
          Volatile => True,
          Inputs   => Address'Asm_Input(TO_EAX, Data'address),
          Template =>
            ---------------------------------------------
            " fnstenv (%%eax)            " & END_LINE_1 &
            " movl    8(%%eax),    %%eax " & END_LINE_1 &
            " xor     $0xffffffff, %%eax " & END_LINE_1 &
            " and     $0x0000ffff, %%eax " & END_LINE_1 ,
            ---------------------------------------------
          Outputs  => Integer_4_Unsigned'Asm_Output(FROM_EAX, Result));
        return Result = 0;
    end Is_Stack_Empty;
    procedure Clear_Stack is
      Data : aliased Record_x86_Environment := (others => <>);
      begin
        Asm(
          Volatile => True,
          Inputs   => Address'Asm_Input(TO_EAX, Data'address),
          Template =>
            -----------------------------------------------
            "   fnstenv (%%eax)            " & END_LINE_1 &
            "   movl    8(%%eax),    %%eax " & END_LINE_1 &
            "   xor     $0xffffffff, %%eax " & END_LINE_1 &
            "   movl    $0x0000c000, %%edx " & END_LINE_1 &
            -----------------------------------------------
            " 1:                           " & END_LINE_1 &
            "   movl    %%eax,       %%ecx " & END_LINE_1 &
            "   and     %%ecx,       %%edx " & END_LINE_1 &
            "   jz      1f                 " & END_LINE_1 &
            "   fstp    %%st               " & END_LINE_1 &
            "   shr     $2,          %%edx " & END_LINE_1 &
            "   jmp     1b                 " & END_LINE_1 &
            -----------------------------------------------
            " 1:                           " & END_LINE_1);
            -----------------------------------------------
      end Clear_Stack;
    procedure Put_Stack is
      function To_String_2 is new Generic_To_String_2(Integer_4_Unsigned);
      function To_String_2 is new Generic_To_String_2(Integer_2_Unsigned);
      Number_Of_Values     : aliased Integer_4_Unsigned          := 0;
      Data_From_Extensions : aliased Integer_4_Unsigned          := 0;
      Stack                : aliased array(1..8) of Float_8_Real := (others => 0.0);
      Environment          : aliased Record_x86_Environment      := (others => <>);
-- The assembly block in this procedure raises an EXCEPTION_ACCESS_VIOLATION PROGRAM_ERROR, so it may be disabled
Disable_Put_Stack : Exception;
      begin
-- Comment/uncomment the below line to disable/enable
if Neo.System.SPECIFICS.Bit_Size >= 64 then Put_Line("Put_Stack is disabled due to an assembly block bug on 64 bit computers."); raise Disable_Put_Stack; end if;
        Asm(
          Volatile => True,
          Inputs =>(
            Address'Asm_Input(TO_EAX, Environment'address),
            Address'Asm_Input(TO_EDI, Stack'address)),
          Template =>
            -----------------------------------------------
            "   movl    %%eax,       %%esi " & END_LINE_1 &
            "   fnstenv (%%esi)            " & END_LINE_1 &
            "   movl    8(%%esi),    %%esi " & END_LINE_1 &
            "   xor     $0xffffffff, %%esi " & END_LINE_1 &
            "   movl    $0x0000c000, %%edx " & END_LINE_1 &
            "   xor     %%eax,       %%eax " & END_LINE_1 &
            "   movl    %%esi,       %%ecx " & END_LINE_1 &
            "   and     %%edx,       %%ecx " & END_LINE_1 &
            "   jz      1f                 " & END_LINE_1 &
            -----------------------------------------------
            "   fst     (%%edi)            " & END_LINE_1 &
            "   inc     %%eax              " & END_LINE_1 &
            "   shr     $2,          %%edx " & END_LINE_1 &
            "   movl    %%esi,       %%ecx " & END_LINE_1 &
            "   and     %%edx,       %%ecx " & END_LINE_1 &
            "   jz      1f                 " & END_LINE_1 &
            -----------------------------------------------
            --
            --   for I in 1..7 loop
            --     fxch %%st(I)
            --     fst  I * 8(%%edi)
            --     inc  %%eax
            --     fxch %%st(I)
            --     if I < 7 then
            --       shr  $2,    %%edx
            --       movl %%esi, %%ecx
            --       and  %%edx, %%ecx
            --       jz   1f
            --     end if
            --   end loop
            -- 1:
            --
            -----------------------------------------------
            "   fxch    %%st(1)            " & END_LINE_1 &
            "   fst     8(%%edi)           " & END_LINE_1 &
            "   inc     %%eax              " & END_LINE_1 &
            "   fxch    %%st(1)            " & END_LINE_1 &
            "   shr     $2,          %%edx " & END_LINE_1 &
            "   movl    %%esi,       %%ecx " & END_LINE_1 &
            "   and     %%edx,       %%ecx " & END_LINE_1 &
            "   jz      1f                 " & END_LINE_1 &
            -----------------------------------------------
            "   fxch    %%st(2)            " & END_LINE_1 &
            "   fst     16(%%edi)          " & END_LINE_1 &
            "   inc     %%eax              " & END_LINE_1 &
            "   fxch    %%st(2)            " & END_LINE_1 &
            "   shr     $2,          %%edx " & END_LINE_1 &
            "   movl    %%esi,       %%ecx " & END_LINE_1 &
            "   and     %%edx,       %%ecx " & END_LINE_1 &
            "   jz      1f                 " & END_LINE_1 &
            -----------------------------------------------
            "   fxch    %%st(3)            " & END_LINE_1 &
            "   fst     24(%%edi)          " & END_LINE_1 &
            "   inc     %%eax              " & END_LINE_1 &
            "   fxch    %%st(3)            " & END_LINE_1 &
            "   shr     $2,          %%edx " & END_LINE_1 &
            "   movl    %%esi,       %%ecx " & END_LINE_1 &
            "   and     %%edx,       %%ecx " & END_LINE_1 &
            "   jz      1f                 " & END_LINE_1 &
            -----------------------------------------------
            "   fxch    %%st(4)            " & END_LINE_1 &
            "   fst     32(%%edi)          " & END_LINE_1 &
            "   inc     %%eax              " & END_LINE_1 &
            "   fxch    %%st(4)            " & END_LINE_1 &
            "   shr     $2,          %%edx " & END_LINE_1 &
            "   movl    %%esi,       %%ecx " & END_LINE_1 &
            "   and     %%edx,       %%ecx " & END_LINE_1 &
            "   jz      1f                 " & END_LINE_1 &
            -----------------------------------------------
            "   fxch    %%st(5)            " & END_LINE_1 &
            "   fst     40(%%edi)          " & END_LINE_1 &
            "   inc     %%eax              " & END_LINE_1 &
            "   fxch    %%st(5)            " & END_LINE_1 &
            "   shr     $2,          %%edx " & END_LINE_1 &
            "   movl    %%esi,       %%ecx " & END_LINE_1 &
            "   and     %%edx,       %%ecx " & END_LINE_1 &
            "   jz      1f                 " & END_LINE_1 &
            -----------------------------------------------
            "   fxch    %%st(6)            " & END_LINE_1 &
            "   fst     48(%%edi)          " & END_LINE_1 &
            "   inc     %%eax              " & END_LINE_1 &
            "   fxch    %%st(6)            " & END_LINE_1 &
            "   shr     $2,          %%edx " & END_LINE_1 &
            "   movl    %%esi,       %%ecx " & END_LINE_1 &
            "   and     %%edx,       %%ecx " & END_LINE_1 &
            "   jz      1f                 " & END_LINE_1 &
            -----------------------------------------------
            "   fxch    %%st(7)            " & END_LINE_1 &
            "   fst     56(%%edi)          " & END_LINE_1 &
            "   inc     %%eax              " & END_LINE_1 &
            "   fxch    %%st(7)            " & END_LINE_1 &
            -----------------------------------------------
            " 1:                           " & END_LINE_1 ,
            -----------------------------------------------
          Outputs =>
            Integer_4_Unsigned'Asm_Output(FROM_EAX, Number_Of_Values));
        if Number_Of_Values <= Stack'size then
          for I in 1..Integer(Number_Of_Values) loop Put_Line("Stack" & Integer'wide_image(I) & ":" & Float_8_Real'wide_image(Stack(I))); end loop;
        end if;
        if SPECIFICS.Has_Advanced_State_Operations then
          Asm(
            Template => "stmxcsr (%%eax)",
            Volatile => True,
            Inputs   => Address'Asm_Input(TO_EAX, Data_From_Extensions'address));
          Put_Line("Specifics: " & To_String_2(Data_From_Extensions, 2));
        end if;
        Put_Line("Control word: "    & To_String_2(Environment.Control_Word,   16));
        Put_Line("Status word: "     & To_String_2(Environment.Status_Word,     2));
        Put_Line("Selector: "        & To_String_2(Environment.CS_Selector,     2));
        Put_Line("Tags: "            & To_String_2(Environment.Tags,            2));
        Put_Line("Data offset: "     & To_String_2(Environment.Data_Offset,     2));
        Put_Line("Data selector: "   & To_String_2(Environment.Data_Selector,   2));
        Put_Line("Operation code: "  & To_String_2(Environment.Operation_Code,  2));
        Put_Line("Program counter: " & To_String_2(Environment.Program_Counter, 2));
exception when Disable_Put_Stack => null;
      end Put_Stack;
  begin
    declare Data : aliased Integer_4_Unsigned := 0; begin
      if WORD_SIZE < 32 then
        raise CPUID_Is_Not_Supported;
      elsif WORD_SIZE = 32 then
        Asm( -- Check for cpuid
          Volatile => True,
          Template =>
            --------------------------------------
	            --" pushfl              " & END_LINE_1 &
            --" orl     %1, (%%esp) " & END_LINE_1 &
            --" popfl               " & END_LINE_1 &
            --" pushfl              " & END_LINE_1 &
            --" pop     %0          " & END_LINE_1 ,
            ---------------------------------------
            --------------------------------------------
            " pushfl                    " & END_LINE_1 & -- Get original extended flags
            " popl   %%eax              " & END_LINE_1 &
            " movl   %%eax,       %%ecx " & END_LINE_1 &
            " xorl   $0x00200000, %%eax " & END_LINE_1 & -- Flip identifier bit in the extended flags
            " pushl  %%eax              " & END_LINE_1 & -- Save new extended flag value on stack
            " popfl                     " & END_LINE_1 & -- Replace current value
            " pushfl                    " & END_LINE_1 & -- Get new extended flags
            " popl   %%eax              " & END_LINE_1 & -- Store new in EAX register
            " xorl   %%ecx,       %%eax " & END_LINE_1 , -- Can we toggle the identifier bit?
            --------------------------------------------
          Outputs  => Integer_4_Unsigned'Asm_Output(FROM_EAX, Data));
        if Data = 0 then -- No we cannot, cpuid command isn't supported (processor is probably 80486)
          raise CPUID_Is_Not_Supported;
        end if;
      end if;
      if SPECIFICS.Has_Advanced_State_Operations then
        Enable_Denormals_Are_Zero:
          declare
          type Array_Save_Area        is array(1..512) of Integer_1_Unsigned  with Alignment => 16;
          type Access_Array_Save_Area is access all Array_Save_Area;
          Save_Area : aliased Array_Save_Area := (others => 0);
          begin
            Asm(
              Volatile => True,
              Inputs   => Access_Array_Save_Area'Asm_Input(TO_EAX, Save_Area'access),
              Template =>
                ------------------------------------------
                " fxsave (%%eax)          " & END_LINE_1 &
                " movl   28(%%eax), %%ebx " & END_LINE_1 ,
                ------------------------------------------
              Outputs  => Integer_4_Unsigned'Asm_Output(FROM_EBX, Data));
            if (Data and 16#20#) /= 0 then
              Asm(
                Volatile => True,
                Inputs   => Address'Asm_Input(TO_EAX, Data'address),
                Template =>
                  -------------------------------------------
                  " stmxcsr (%%eax)          " & END_LINE_1 &
                  " movl    (%%eax), %%ebx   " & END_LINE_1 &
                  " or      $0x40,   %%bx    " & END_LINE_1 &
                  " movl    %%ebx,   (%%eax) " & END_LINE_1 &
                  " ldmxcsr (%%eax)          " & END_LINE_1);
                  -------------------------------------------
            end if;
          end Enable_Denormals_Are_Zero;
        Asm(
          Volatile => True,
          Inputs   => Address'Asm_Input(TO_EAX, Data'address),
          Template =>
            -------------------------------------------
            " stmxcsr (%%eax)          " & END_LINE_1 &
            " movl    (%%eax), %%ebx   " & END_LINE_1 &
            " or      $0x8000, %%ebx   " & END_LINE_1 &
            " movl    %%ebx,   (%%eax) " & END_LINE_1 &
            " ldmxcsr (%%eax)          " & END_LINE_1);
            -------------------------------------------
      end if;
      declare
        Other_Data     : aliased          Integer_2_Unsigned := 0;
        EXCEPTION_MASK : aliased constant Integer_4_Unsigned := 16#0000_1F00#; -- Do not crash on any error
        begin
          if SPECIFICS.Has_Advanced_State_Operations then
            Asm(
              Volatile => True,
              Inputs   =>(
                Address'Asm_Input(TO_EAX, Data'address),
                Integer_4_Unsigned'Asm_Input(TO_ECX, EXCEPTION_MASK)),
              Template =>
                -----------------------------------------------
                " stmxcsr (%%eax)              " & END_LINE_1 &
                " movl    (%%eax),     %%ebx   " & END_LINE_1 &
                " and     $0xffffe07f, %%ebx   " & END_LINE_1 &
                " or      %%ecx,       %%ebx   " & END_LINE_1 &
                " movl    %%ebx,       (%%eax) " & END_LINE_1 &
                " ldmxcsr (%%eax)              " & END_LINE_1);
                -----------------------------------------------
          end if;
          Asm(
            Volatile => True,
            Inputs   =>(
              Address'Asm_Input(TO_EAX, Other_Data'address),
              Integer_4_Unsigned'Asm_Input(TO_ECX, Shift_Right(EXCEPTION_MASK, 7))),
            Template =>
              ------------------------------------------
              " fnstcw (%%eax)          " & END_LINE_1 &
              " movw   (%%eax), %%bx    " & END_LINE_1 &
              " and    $0xffc0, %%bx    " & END_LINE_1 &
              " or     %%cx,    %%bx    " & END_LINE_1 &
              " movw   %%bx,    (%%eax) " & END_LINE_1 &
              " fldcw  (%%eax)          " & END_LINE_1);
              ------------------------------------------
        end;
    end;
  end Import;
