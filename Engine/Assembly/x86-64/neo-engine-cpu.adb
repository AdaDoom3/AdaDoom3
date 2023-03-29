
--                                                                                                                                                          --
--                                                                   N E O  E N G I N E                                                                     --
--                                                                                                                                                          --
--                                                            Copyright (C) 202X Justin Squirek                                                             --
--                                                                                                                                                          --
-- Neo is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published  by the Free Software      --
-- Foundation, either version 3 of the License, or (at your option) any later version.                                                                      --
--                                                                                                                                                          --
-- Neo is distributed in the hope that it will be useful, but without any warranty; without even the implied warranty of merchantability or fitness for a   --
-- particular purpose. See the GNU General Public License for more details.                                                                                 --
--                                                                                                                                                          --

separate (Neo.Engine) package body CPU is
  E : constant Str_8 := EOL_8; -- Abbreviation for convience

  ---------------
  -- Registers --
  ---------------

  type Register_Kind is (EAX_Register, EBX_Register, ECX_Register, EDX_Register);

  -- String "Templates" for Asm calls
  TO_ESI   : constant Str_8 := "s";
  TO_EDI   : constant Str_8 := "d";
  TO_EAX   : constant Str_8 := "a";
  TO_EBX   : constant Str_8 := "b";
  TO_ECX   : constant Str_8 := "c";
  TO_EDX   : constant Str_8 := "d";
  FROM_EAX : constant Str_8 := "=a";
  FROM_EBX : constant Str_8 := "=b";
  FROM_ECX : constant Str_8 := "=c";
  FROM_EDX : constant Str_8 := "=d";

  type x86_Environment_State is record
      Control_Word    : Int_16_Unsigned := 0;
      Unused_A        : Int_16_Unsigned := 0;
      Status_Word     : Int_16_Unsigned := 0;
      Unused_B        : Int_16_Unsigned := 0;
      Tags            : Int_16_Unsigned := 0;
      Unused_C        : Int_16_Unsigned := 0;
      Program_Counter : Int_Unsigned    := 0;
      CS_Selector     : Int_16_Unsigned := 0;
      Operation_Code  : Int_16_Unsigned := 0; -- Consists of 11 used + 5 unused bits
      Data_Offset     : Int_Unsigned    := 0;
      Data_Selector   : Int_16_Unsigned := 0;
      Unused_D        : Int_16_Unsigned := 0;
    end record with size => 28 * Byte'size;

  -------------
  -- Get_CPU --
  -------------

  function Get_CPU_Internal return CPU_State is

    -- Obtain the vender name string
    function Get_Vendor return Vendor_Kind is -- "Oh, such a sweet sweet trick, just not very useful. :)"
      Vendor : Str_8 (1..12) := (others => NULL_CHAR_8);
      begin
        Asm (Volatile => True,
             Inputs   => Int_Unsigned'Asm_Input (TO_EAX, 0),
             Template => "cpuid",
             Outputs  => (Str_8'Asm_Output (FROM_EBX, Vendor (1..4)),
                          Str_8'Asm_Output (FROM_EDX, Vendor (5..8)),
                          Str_8'Asm_Output (FROM_ECX, Vendor (9..12))));
        return (if    Vendor = "AuthenticAMD" then Advanced_Micro_Devices_Vendor
                elsif Vendor = "GenuineIntel" then Intel_Vendor
                else Unknown_Vendor);
      end;

    
    function Is_Enabled (Function_ID : Int_Unsigned; Register : Register_Kind; Bit : Natural) return Bool is
      Data : array (Register_Kind'Range) of Int_Unsigned :=  (others => 0);
      begin
        Asm (Volatile => True,
             Inputs   => Int_Unsigned'Asm_Input (TO_EAX, Function_ID),
             Template => "cpuid",
             Outputs  => (Int_Unsigned'Asm_Output (FROM_EAX, Data (EAX_Register)),
                          Int_Unsigned'Asm_Output (FROM_EBX, Data (EBX_Register)),
                          Int_Unsigned'Asm_Output (FROM_ECX, Data (ECX_Register)),
                          Int_Unsigned'Asm_Output (FROM_EDX, Data (EDX_Register))));

        -- Make sure its enabled
        return (Data (Register) and 2**Bit) /= 0;
      end;

    
    Result : CPU_State (Get_Vendor);
    Data   : aliased Int_Unsigned := 0;

    -- Start of Get_CPU
    begin
      if Result.Vendor = Advanced_Micro_Devices_Vendor then
        Result.Has_3DNow                                  := Is_Enabled (16#8000_0001#, EDX_Register, 31); -- 3DNow!
        Result.Has_3DNow_Supplement                       := Is_Enabled (16#8000_0001#, EDX_Register, 30); -- 3DNow!+
        Result.Has_Multi_Media_Extensions_Supplement      := Is_Enabled (16#8000_0001#, EDX_Register, 22); -- MMX+
        Result.Has_Streaming_SIMD_Extensions_4_Supplement := Is_Enabled (16#8000_0001#, ECX_Register,  6); -- SSE4a
      end if;
      Result.Has_Extended_States_Enabled                  := Is_Enabled (16#0000_0001#, ECX_Register, 27); -- OSXSAVE
      Result.Has_Population_Count                         := Is_Enabled (16#0000_0001#, ECX_Register, 23); -- POPCNT
      Result.Has_Multi_Media_Extensions                   := Is_Enabled (16#0000_0001#, EDX_Register, 23); -- MMX
      Result.Has_Streaming_SIMD_Extensions_1              := Is_Enabled (16#0000_0001#, EDX_Register, 25); -- SSE
      Result.Has_Streaming_SIMD_Extensions_2              := Is_Enabled (16#0000_0001#, EDX_Register, 26); -- SSE2
      Result.Has_Streaming_SIMD_Extensions_3              := Is_Enabled (16#0000_0001#, ECX_Register,  0); -- SSE3
      Result.Has_Streaming_SIMD_Extensions_3_Supplement   := Is_Enabled (16#0000_0001#, ECX_Register,  9); -- SSSE3
      Result.Has_Streaming_SIMD_Extensions_4_1            := Is_Enabled (16#0000_0001#, ECX_Register, 19); -- SSE4.1
      Result.Has_Streaming_SIMD_Extensions_4_2            := Is_Enabled (16#0000_0001#, ECX_Register, 20); -- SSE4.2
      Result.Has_Carryless_Multiplication_Of_Two_64_Bit   := Is_Enabled (16#0000_0001#, ECX_Register,  1); -- PCLMULQDQ
      Result.Has_Bit_Manipulation_Extensions_1            := Is_Enabled (16#0000_0007#, EBX_Register,  3); -- BMI1
      Result.Has_Bit_Manipulation_Extensions_2            := Is_Enabled (16#0000_0007#, EBX_Register,  8); -- BMI2
      Result.Has_Advanced_Vector_Extensions_1             := Is_Enabled (16#0000_0001#, ECX_Register, 28); -- AVX
      Result.Has_Advanced_Vector_Extensions_2             := Is_Enabled (16#0000_0007#, EBX_Register,  5); -- AVX2
      Result.Has_Advanced_Encryption_Service              := Is_Enabled (16#0000_0001#, ECX_Register, 25); -- AES
      Result.Has_Half_Precision_Floating_Point_Convert    := Is_Enabled (16#0000_0001#, ECX_Register, 29); -- F16C
      Result.Has_Conditional_Move                         := Is_Enabled (16#0000_0001#, EDX_Register, 15); -- CMOV
      Result.Has_Hyperthreading                           := Is_Enabled (16#0000_0001#, EDX_Register, 28); -- HTT
      Result.Has_Advanced_State_Operations                := Is_Enabled (16#0000_0001#, EDX_Register, 24); -- FXSR
      Result.Has_Fused_Multiply_Add_3                     := Is_Enabled (16#0000_0001#, ECX_Register, 12); -- FMA3
      Result.Has_Fused_Multiply_Add_4                     := Is_Enabled (16#8000_0001#, ECX_Register, 16); -- FMA4
      Result.Has_High_Precision_Convert                   := Is_Enabled (16#8000_0001#, ECX_Register, 19); -- CVT16
      Result.Has_Extended_Operation_Support               := Is_Enabled (16#8000_0001#, ECX_Register, 11); -- XOP
      Result.Has_Leading_Zero_Count                       := Is_Enabled (16#8000_0001#, ECX_Register,  5); -- ABM

      -- Check if the operating system will save the YMM registers
      if Result.Has_Streaming_SIMD_Extensions_4_2 then
        Asm (Volatile => True,
             Inputs   => Int_Unsigned'Asm_Input (TO_ECX, 0),
             Template => "xgetbv",
             Outputs  => Int_Unsigned'Asm_Output (FROM_EAX, Data));
        Result.Has_Advanced_Vector_Extensions_Enabled :=  (Data and 16#0000_0006#) > 0;
      end if;

      
      return Result;
    end;

  -- Constant info to remove overhead
  INFORMATION_INTERNAL : constant CPU_State := Get_CPU_Internal;
  function Get_CPU return CPU_State is (INFORMATION_INTERNAL);

  ----------------------
  -- Check_Exceptions --
  ----------------------

  procedure Check_Exceptions is
    Data          : aliased Int_Unsigned := 0;
    Data_From_x87 : aliased Int_16_Unsigned := 0;
    begin

      
      if Get_CPU.Has_Advanced_State_Operations then
        Asm (Volatile => True,
             Inputs   => Ptr'Asm_Input (TO_EAX, Data'Address),
             Template => "stmxcsr (%%eax)");
      end if;

      
      Asm (Volatile => True,
           Inputs   => Ptr'Asm_Input (TO_EAX, Data_From_x87'Address),
           Template => "fnstsw (%%eax)");
      Data := (Data and 16#0000_003F#) and Int_Unsigned (Data_From_x87);

      -- Raise the first error found
      if Data /= 0 then

        -- Clear exception state in CPU
        declare
        Data_From_SIMD  :         Int_Unsigned          := 0;
        x86_Environment : aliased x86_Environment_State := (others => <>);
        begin

          -- Clear ???
          if Get_CPU.Has_Advanced_State_Operations then
            Asm (Volatile => True,
                 Inputs   => Ptr'Asm_Input (TO_EAX, Data_From_SIMD'Address),
                 Template => --------------------------------------
                             " stmxcsr (%%eax)              " & E &
                             " movl    (%%eax),     %%ebx   " & E &
                             " and     $0xffffffc0, %%ebx   " & E &
                             " movl    %%ebx,       (%%eax) " & E &
                             " ldmxcsr (%%eax)              " & E);
                             --------------------------------------
          end if;

          -- Clear 6 exception bits plus stack fault
          Asm (Volatile => True,
               Inputs   => Ptr'Asm_Input (TO_EAX, x86_Environment'Address),
               Template => "fnstenv (%%eax)");
          x86_Environment.Status_Word := x86_Environment.Status_Word and 16#FF80#;
          Asm (Volatile => True,
               Inputs   => Ptr'Asm_Input (TO_EAX, x86_Environment'Address),
               Template => "fldenv (%%eax)");
          Asm (Volatile => True, Template => "fnclex");
        end;

        -- Raise the associated Ada exception
        if (Data and 16#0000_0001#) /= 0 then raise Invalid_Operation;    end if;
        if (Data and 16#0000_0002#) /= 0 then raise Denormalized_Operand; end if;
        if (Data and 16#0000_0004#) /= 0 then raise Divide_By_Zero;       end if;
        if (Data and 16#0000_0008#) /= 0 then raise Numeric_Overflow;     end if;
        if (Data and 16#0000_0010#) /= 0 then raise Numeric_Underflow;    end if;
        if (Data and 16#0000_0020#) /= 0 then raise Inexact_Result;       end if;
        if (Data and 16#0000_0040#) /= 0 then raise Stack_Fault;          end if;
      end if;
    end;

  --------------
  -- Settings --
  --------------

  
  procedure Set_Rounding (Val : Rounding_Kind) is
    Other_Data    : aliased Int_16_Unsigned := 0;
    Data          : aliased Int_Unsigned    := 0;
    Rounding_Mask :         Int_Unsigned    := (case Val is when Nearest_Rounding  => 16#0000#,
                                                            when Down_Rounding     => 16#2000#,
                                                            when Up_Rounding       => 16#4000#,
                                                            when Truncate_Rounding => 16#6000#);
    begin
      if Get_CPU.Has_Advanced_State_Operations then
        Asm (Volatile => True,
             Inputs   => (Ptr'Asm_Input (TO_EAX, Data'Address),
                          Int_Unsigned'Asm_Input (TO_ECX, Rounding_Mask)),
             Template => ------------------------------------------
                         " stmxcsr (%%eax)              " & E &
                         " movl    (%%eax),     %%ebx   " & E &
                         " and     $0xffff9fff, %%ebx   " & E &
                         " or      %%cx,        %%bx    " & E &
                         " movl    %%ebx,       (%%eax) " & E &
                         " ldmxcsr (%%eax)              " & E);
                         ------------------------------------------
      end if;
      Rounding_Mask := Shift_Right (Rounding_Mask, 3);
      Asm (Volatile => True,
           Inputs   => (Ptr'Asm_Input (TO_EAX, Other_Data'Address),
                        Int_Unsigned'Asm_Input (TO_ECX, Rounding_Mask)),
           Template => -------------------------------------
                       " fnstcw (%%eax)          " & E &
                       " movw   (%%eax), %%bx    " & E &
                       " and    $0xf3ff, %%bx    " & E &
                       " or     %%cx,    %%bx    " & E &
                       " movw   %%bx,    (%%eax) " & E &
                       " fldcw  (%%eax)          " & E);
                       -------------------------------------
    end;

  
  procedure Set_Precision (Val : Precision_Kind) is
    Blank_Memory : aliased Int_16_Unsigned := 0;
    begin
      Asm (Volatile => True,
           Inputs   => (Address     'Asm_Input (TO_EAX, Blank_Memory'Address),
                        Int_Unsigned'Asm_Input (TO_ECX, (case Val is when Single_Precision          => 16#0000#,
                                                                     when Double_Precision          => 16#0200#,
                                                                     when Double_Extended_Precision => 16#0300#))),
           Template => -------------------------------------
                       " fnstcw (%%eax)          " & E &
                       " movw   (%%eax), %%bx    " & E &
                       " and    $0xfcff, %%bx    " & E &
                       " or     %%cx,    %%bx    " & E &
                       " movw   %%bx,    (%%eax) " & E &
                       " fldcw  (%%eax)          " & E);
                       -------------------------------------
    end;

  -----------
  -- Stack --
  -----------

  function Is_Stack_Empty return Bool is
    Result :         Int_Unsigned          := 0;
    Data   : aliased x86_Environment_State := (others => <>);
    begin
      Asm (Volatile => True,
           Inputs   => Ptr'Asm_Input (TO_EAX, Data'Address),
           Outputs  => Int_Unsigned'Asm_Output (FROM_EAX, Result),
           Template => ----------------------------------------
                       " fnstenv (%%eax)            " & E &
                       " movl    8 (%%eax),   %%eax " & E &
                       " xor     $0xffffffff, %%eax " & E &
                       " and     $0x0000ffff, %%eax " & E);
                       ----------------------------------------
      return Result = 0;
    end;

  
  procedure Clear_Stack is
    Data : aliased x86_Environment_State := (others => <>);
    begin
      Asm (Volatile => True,
           Inputs   => Ptr'Asm_Input (TO_EAX, Data'Address),
           Template => ------------------------------------------
                       "   fnstenv (%%eax)            " & E &
                       "   movl    8 (%%eax),   %%eax " & E &
                       "   xor     $0xffffffff, %%eax " & E &
                       "   movl    $0x0000c000, %%edx " & E &
                       ------------------------------------------
                       " 1:                           " & E &
                       "   movl    %%eax,       %%ecx " & E &
                       "   and     %%ecx,       %%edx " & E &
                       "   jz      1f                 " & E &
                       "   fstp    %%st               " & E &
                       "   shr     $2,          %%edx " & E &
                       "   jmp     1b                 " & E &
                       ------------------------------------------
                       " 1:                           " & E);
                       ------------------------------------------
    end;

  
  procedure Put_Stack is

    -- Conversion functions to print numbers in different bases
    function To_Str is new Generic_To_Str_16_Int (Int_Unsigned);
    function To_Str is new Generic_To_Str_16_Int (Int_16_Unsigned);

    -- Stack temporaries
    Number_Of_Values     : aliased Int_Unsigned          := 0;
    Data_From_Extensions : aliased Int_Unsigned          := 0;
    Environment          : aliased x86_Environment_State := (others => <>);
    Stack                : aliased array (1..8) of Real  := (others => 0.0);

    -- Start of Put_Stack
    begin
      Asm (Volatile => True,
           Inputs   => (Ptr'Asm_Input (TO_EAX, Environment'Address),
                        Ptr'Asm_Input (TO_EDI, Stack (Stack'First)'Address)),
           Outputs  => Int_Unsigned'Asm_Output (FROM_EAX, Number_Of_Values),
           Template => ------------------------------------------
                       "   movl    %%eax,       %%esi " & E &
                       "   fnstenv (%%esi)            " & E &
                       "   movl    8 (%%esi),   %%esi " & E &
                       "   xor     $0xffffffff, %%esi " & E &
                       "   movl    $0x0000c000, %%edx " & E &
                       "   xor     %%eax,       %%eax " & E &
                       "   movl    %%esi,       %%ecx " & E &
                       "   and     %%edx,       %%ecx " & E &
                       "   jz      1f                 " & E &
                       ------------------------------------------
                       "   fst     (%%edi)            " & E &
                       "   inc     %%eax              " & E &
                       "   shr     $2,          %%edx " & E &
                       "   movl    %%esi,       %%ecx " & E &
                       "   and     %%edx,       %%ecx " & E &
                       "   jz      1f                 " & E &
                       ------------------------------------------
                       --
                       --   for I in 1..7 loop
                       --     fxch %%st (I)
                       --     fst  I * 8 (%%edi)
                       --     inc  %%eax
                       --     fxch %%st (I)
                       --     if I < 7 then
                       --       shr  $2,    %%edx
                       --       movl %%esi, %%ecx
                       --       and  %%edx, %%ecx
                       --       jz   1f
                       --     end if
                       --   end loop
                       -- 1:
                       --
                       ------------------------------------------
                       "   fxch    %%st (1)           " & E &
                       "   fst     8 (%%edi)          " & E &
                       "   inc     %%eax              " & E &
                       "   fxch    %%st (1)           " & E &
                       "   shr     $2,          %%edx " & E &
                       "   movl    %%esi,       %%ecx " & E &
                       "   and     %%edx,       %%ecx " & E &
                       "   jz      1f                 " & E &
                       ------------------------------------------
                       "   fxch    %%st (2)           " & E &
                       "   fst     16 (%%edi)         " & E &
                       "   inc     %%eax              " & E &
                       "   fxch    %%st (2)           " & E &
                       "   shr     $2,          %%edx " & E &
                       "   movl    %%esi,       %%ecx " & E &
                       "   and     %%edx,       %%ecx " & E &
                       "   jz      1f                 " & E &
                       ------------------------------------------
                       "   fxch    %%st (3)           " & E &
                       "   fst     24 (%%edi)         " & E &
                       "   inc     %%eax              " & E &
                       "   fxch    %%st (3)           " & E &
                       "   shr     $2,          %%edx " & E &
                       "   movl    %%esi,       %%ecx " & E &
                       "   and     %%edx,       %%ecx " & E &
                       "   jz      1f                 " & E &
                       ------------------------------------------
                       "   fxch    %%st (4)           " & E &
                       "   fst     32 (%%edi)         " & E &
                       "   inc     %%eax              " & E &
                       "   fxch    %%st (4)           " & E &
                       "   shr     $2,          %%edx " & E &
                       "   movl    %%esi,       %%ecx " & E &
                       "   and     %%edx,       %%ecx " & E &
                       "   jz      1f                 " & E &
                       ------------------------------------------
                       "   fxch    %%st (5)           " & E &
                       "   fst     40 (%%edi)         " & E &
                       "   inc     %%eax              " & E &
                       "   fxch    %%st (5)           " & E &
                       "   shr     $2,          %%edx " & E &
                       "   movl    %%esi,       %%ecx " & E &
                       "   and     %%edx,       %%ecx " & E &
                       "   jz      1f                 " & E &
                       ------------------------------------------
                       "   fxch    %%st (6)           " & E &
                       "   fst     48 (%%edi)         " & E &
                       "   inc     %%eax              " & E &
                       "   fxch    %%st (6)           " & E &
                       "   shr     $2,          %%edx " & E &
                       "   movl    %%esi,       %%ecx " & E &
                       "   and     %%edx,       %%ecx " & E &
                       "   jz      1f                 " & E &
                       ------------------------------------------
                       "   fxch    %%st (7)           " & E &
                       "   fst     56 (%%edi)         " & E &
                       "   inc     %%eax              " & E &
                       "   fxch    %%st (7)           " & E &
                       ------------------------------------------
                       " 1:                           " & E);
                       ------------------------------------------

      -- Put the actual stack values
      if Number_Of_Values <= Stack'Size then
        for I in 1..Integer (Number_Of_Values) loop
          Line ("Stack" & Integer'wide_image (I) & ":" & Real'wide_image (Stack (I)));
        end loop;
      end if;

      -- Put AMD specific registers
      if Get_CPU.Has_Advanced_State_Operations then
        Asm (Template => "stmxcsr  (%%eax)",
             Volatile => True,
             Inputs   => Ptr'Asm_Input (TO_EAX, Data_From_Extensions'Address));
        Line ("Result: " & To_Str (Data_From_Extensions, 2));
      end if;

      -- Put general info
      Line ("Control word: "    & To_Str (Environment.Control_Word,   16));
      Line ("Status word: "     & To_Str (Environment.Status_Word,     2));
      Line ("Selector: "        & To_Str (Environment.CS_Selector,     2));
      Line ("Tags: "            & To_Str (Environment.Tags,            2));
      Line ("Data offset: "     & To_Str (Environment.Data_Offset,     2));
      Line ("Data selector: "   & To_Str (Environment.Data_Selector,   2));
      Line ("Operation code: "  & To_Str (Environment.Operation_Code,  2));
      Line ("Program counter: " & To_Str (Environment.Program_Counter, 2));
    end;

------------------------
-- Executable Section --
------------------------

-- Start of Neo.Engine.CPU's executable section
begin

  -- Temporary to hold returned assembly output
  declare Data : aliased Int_Unsigned := 0; begin

    -- Ready the CPU
    if Get_CPU.Has_Advanced_State_Operations then

      -- Enable denormals are zero
      declare
      type Array_Save_Area is array (1..512) of Byte with Alignment => 16;
      Save_Area : aliased Array_Save_Area := (others => 0);
      begin
        Asm (Volatile => True,
             Inputs   => Ptr'Asm_Input (TO_EAX, Save_Area (Save_Area'First)'Address),
             Outputs  => Int_Unsigned'Asm_Output (FROM_EBX, Data),
             Template => ----------------------------------
                         " fxsave (%%eax)           " & E &
                         " movl   28 (%%eax), %%ebx " & E);
                         ----------------------------------

        -- Test one of the things
        if (Data and 16#20#) /= 0 then
          Asm (Volatile => True,
               Inputs   => Ptr'Asm_Input (TO_EAX, Data'Address),
               Template => ----------------------------------
                           " stmxcsr (%%eax)          " & E &
                           " movl    (%%eax), %%ebx   " & E &
                           " or      $0x40,   %%ebx   " & E &
                           " movl    %%ebx,   (%%eax) " & E &
                           " ldmxcsr (%%eax)          " & E);
                           ----------------------------------
        end if;
      end;
      Asm (Volatile => True,
           Inputs   => Ptr'Asm_Input (TO_EAX, Data'Address),
           Template => ----------------------------------
                       " stmxcsr (%%eax)          " & E &
                       " movl    (%%eax), %%ebx   " & E &
                       " or      $0x8000, %%ebx   " & E &
                       " movl    %%ebx,   (%%eax) " & E &
                       " ldmxcsr (%%eax)          " & E);
                       ----------------------------------
    end if;


    declare
    Other_Data     : aliased          Int_16_Unsigned := 0;
    EXCEPTION_MASK : aliased constant Int_Unsigned    := 16#0000_1F00#; -- Do not crash on any error
    begin
      if Get_CPU.Has_Advanced_State_Operations then
        Asm (Volatile => True,
             Inputs   => (Ptr'Asm_Input (TO_EAX, Data'Address),
                          Int_Unsigned'Asm_Input (TO_ECX, EXCEPTION_MASK)),
             Template => --------------------------------------
                         " stmxcsr (%%eax)              " & E &
                         " movl    (%%eax),     %%ebx   " & E &
                         " and     $0xffffe07f, %%ebx   " & E &
                         " or      %%ecx,       %%ebx   " & E &
                         " movl    %%ebx,       (%%eax) " & E &
                         " ldmxcsr (%%eax)              " & E);
                         --------------------------------------
      end if;
      Asm (Volatile => True,
           Inputs   => (Ptr'Asm_Input (TO_EAX, Other_Data'Address),
                        Int_Unsigned'Asm_Input (TO_ECX, Shift_Right (EXCEPTION_MASK, 7))),
           Template => ---------------------------------
                       " fnstcw (%%eax)          " & E &
                       " movw   (%%eax), %%bx    " & E &
                       " and    $0xffc0, %%bx    " & E &
                       " or     %%cx,    %%bx    " & E &
                       " movw   %%bx,    (%%eax) " & E &
                       " fldcw  (%%eax)          " & E);
                       ---------------------------------
    end;
  end;
end;
