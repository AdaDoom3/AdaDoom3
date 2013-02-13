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
package body Neo.System.Processor
  is
  --------------------
  -- Implementation --
  --------------------
    package body Implementation_For_Compiler
      is separate;
    package body Implementation_For_Architecture
      is separate;
    package body Implementation_For_Operating_System
      is separate;
  ----------
  -- Test --
  ----------
    procedure Test
      is
      -----------
      procedure A
      -----------
        is
        begin
          Put_Trace;
        end A;
      -----------
      procedure B
      -----------
        is
        begin
          A;
        end B;
      -----------
      procedure C
      -----------
        is
        begin
          B;
        end C;
      -----------
      procedure D
      -----------
        is
        begin
          C;
        end D;
      Extensions : constant Record_Extensions := Get_Extensions;
      begin
        Put_Title("PROCESSOR TEST");
        Set_Precision(Double_Extended_Precision);
        Set_Rounding(Nearest_Rounding);
        Check_Exceptions;
        Put_Line("Vendor: "      & Enumerated_Vendor'Wide_Image(Get_Vendor));
        Put_Line("Clock ticks: " & Integer_8_Unsigned'Wide_Image(Get_Clock_Ticks));
        Put_Line("Sleep...");
        delay 0.5;
        Put_Line("Clock ticks: " & Integer_8_Unsigned'Wide_Image(Get_Clock_Ticks));
        Put_Line("Sleep...");
        delay 1.0;
        Put_Line("Clock ticks: "          & Integer_8_Unsigned'Wide_Image(Get_Clock_Ticks));
        Put_Line("Number of Extensionss:" & Integer_8_Unsigned'Wide_Image(Get_Number_Of_Cores));
        Put_Line("Speed in megahertz:"    & Integer_8_Unsigned'Wide_Image(Get_Speed_In_Megahertz));
        case Get_Vendor is
          when Apple_IBM_Motorola_Vendor =>
            Put_Line("A PowerPC processor was detected for some reason, are you using a debugger?");
          when Generic_Vendor =>
            Put_Line("Unrecognized CPU, but apperently x86-64");
          when Advanced_Micro_Devices_Vendor =>
            Put_Line("Advanced Micro Devices");
          when Intel_Vendor =>
            Put_Line("Intel");
        end case;
        if Extensions.Has_3DNow then
          Put_Line("Has 3DNow!");
        end if;
        if Extensions.Has_3DNow_Supplement then
          Put_Line("Has 3DNow!+");
        end if;
        if Extensions.Has_Multi_Media_Extensions then
          Put_Line("Has MMX");
        end if;
        if Extensions.Has_Multi_Media_Extensions_Supplement then
          Put_Line("Has MMX+");
        end if;
        if Extensions.Has_Fused_Multiply_Add_3 then
          Put_Line("Has FMA3");
        end if;
        if Extensions.Has_Fused_Multiply_Add_4 then
          Put_Line("Has FMA4");
        end if;
        if Extensions.Has_Streaming_SIMD_Extensions_1 then
          Put_Line("Has SSE");
        end if;
        if Extensions.Has_Streaming_SIMD_Extensions_2 then
          Put_Line("Has SSE2");
        end if;
        if Extensions.Has_Streaming_SIMD_Extensions_3 then
          Put_Line("Has SSE3");
        end if;
        if Extensions.Has_Streaming_SIMD_Extensions_3_Supplement then
          Put_Line("Has SSSE3");
        end if;
        if Extensions.Has_Streaming_SIMD_Extensions_4_Supplement then
          Put_Line("Has SSE4a");
        end if;
        if Extensions.Has_Streaming_SIMD_Extensions_4_1 then
          Put_Line("Has SSE4.1");
        end if;
        if Extensions.Has_Streaming_SIMD_Extensions_4_2 then
          Put_Line("Has SSE4.2");
        end if;
        if Extensions.Has_Bit_Manipulation_Extensions_1 then
          Put_Line("Has BMI1");
        end if;
        if Extensions.Has_Bit_Manipulation_Extensions_2 then
          Put_Line("Has BMI2");
        end if;
        if Extensions.Has_Advanced_Vector_Extensions_1 then
          Put_Line("Has AVX");
          if not Extensions.Has_Advanced_Vector_Extensions_Enabled then
            Put_Line(", but it's disabled");
          else
            New_Line;
          end if;
        end if;
        if Extensions.Has_Advanced_Vector_Extensions_2 then
          Put("Has AVX2");
          if not Extensions.Has_Advanced_Vector_Extensions_Enabled then
            if not Extensions.Has_Advanced_Vector_Extensions_1 then
              Put_Line(", but it's disabled");
            else
              Put_Line(", but it's also disabled");
            end if;
          else
            New_Line;
          end if;
        end if;
        if Extensions.Has_Context_ID_Manager then
          Put_Line("Has INVPCID");
        end if;
        if Extensions.Has_Population_Count then
          Put_Line("Has POPCNT");
        end if;
        if Extensions.Has_Leading_Zero_Count then
          Put_Line("Has LZCNT");
        end if;
        if Extensions.Has_Carryless_Multiplication_Of_Two_64_Bit then
          Put_Line("Has PCLMULQDQ");
        end if;
        if Extensions.Has_Extended_States_Enabled then
          Put_Line("Has OSXSAVE");
        end if;
        if Extensions.Has_Half_Precision_Floating_Point_Convert then
          Put_Line("Has F16C");
        end if;
        if Extensions.Has_High_Precision_Convert then
          Put_Line("Has CVT16");
        end if;
        if Extensions.Has_Advanced_Encryption_Service then
          Put_Line("Has AES");
        end if;
        if Extensions.Has_Advanced_State_Operations then
          Put_Line("Has FXSR");
        end if;
        if Extensions.Has_Extended_Operation_Support then
          Put_Line("Has XOP");
        end if;
        if Extensions.Has_Hyperthreading then
          Put_Line("Has HTT");
        end if;
        if Extensions.Has_Conditional_Move then
          Put_Line("Has CMOV");
        end if;
        if Is_Stack_Empty then
          Put_Line("Stack is empty!");
        else
          Put_Stack;
          Clear_Stack;
          if Is_Stack_Empty then
            Put_Line("Stack was cleared successfully");
          end if;
        end if;
        Put_Stack;
        D;
        Hang_Window;
    end Test;
  ----------------
  -- Initialize --
  ----------------
    procedure Initialize
      renames Implementation_For_Architecture.Initialize;
  ----------------------
  -- Check_Exceptions --
  ----------------------
    procedure Check_Exceptions
      renames Implementation_For_Architecture.Check_Exceptions;
  -----------------
  -- Clear_Stack --
  -----------------
    procedure Clear_Stack
      renames Implementation_For_Architecture.Clear_Stack;
  --------------------
  -- Is_Stack_Empty --
  --------------------
    function Is_Stack_Empty
      return Boolean
      renames Implementation_For_Architecture.Is_Stack_Empty;
  ---------------
  -- Put_Stack --
  ---------------
    procedure Put_Stack
      renames Implementation_For_Architecture.Put_Stack;
  ------------------
  -- Set_Rounding --
  ------------------
    procedure Set_Rounding(
      Rounding : in Enumerated_Rounding)
      renames Implementation_For_Architecture.Set_Rounding;
  -------------------
  -- Set_Precision --
  -------------------
    procedure Set_Precision(
      Precision : in Enumerated_Precision)
      renames Implementation_For_Architecture.Set_Precision;
  --------------------
  -- Get_Extensions --
  --------------------
    function Get_Extensions
      return Record_Extensions
      renames Implementation_For_Architecture.Get_Extensions;
  ----------------
  -- Get_Vendor --
  ----------------
    function Get_Vendor
      return Enumerated_Vendor
      renames Implementation_For_Architecture.Get_Vendor;
  --------------------
  -- Get_Clock_Tics --
  --------------------
    function Get_Clock_Ticks
      return Integer_8_Unsigned
      is
      begin
        return Implementation_For_Operating_System.Get_Clock_Ticks;
      exception
        when System_Call_Failure =>
          return Implementation_For_Architecture.Get_Clock_Ticks;
      end Get_Clock_Ticks;
  -------------------------
  -- Get_Number_Of_Cores --
  -------------------------
    function Get_Number_Of_Cores
      return Integer_8_Unsigned
      is
      begin
        return Implementation_For_Operating_System.Get_Number_Of_Cores;
      exception
        when System_Call_Failure =>
          return Implementation_For_Architecture.Get_Number_Of_Cores;
      end Get_Number_Of_Cores;
  ----------------------------
  -- Get_Speed_In_Megahertz --
  ----------------------------
    function Get_Speed_In_Megahertz
      return Integer_8_Unsigned
      is
      begin
        return Implementation_For_Operating_System.Get_Speed_In_Megahertz;
      exception
        when System_Call_Failure =>
          return Implementation_For_Architecture.Get_Speed_In_Megahertz;
      end Get_Speed_In_Megahertz;
  ---------------
  -- Put_Trace --
  ---------------
    procedure Put_Trace
      is
      begin
        Implementation_For_Compiler.Put_Trace;
      exception
        when System_Call_Failure =>
          Implementation_For_Architecture.Put_Trace;
      end Put_Trace;
  end Neo.System.Processor;
