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
PACKAGE BODY Neo.System.Processor
  IS
  --------------------
  -- IMPLEMENTATION --
  --------------------
    PACKAGE BODY Implementation_For_Compiler
      IS SEPARATE;
    PACKAGE BODY Implementation_For_Architecture
      IS SEPARATE;
    PACKAGE BODY Implementation_For_Operating_System
      IS SEPARATE;
  ----------
  -- Test --
  ----------
    PROCEDURE Test
      IS
      -----------
      PROCEDURE A
      -----------
        IS
        BEGIN
          Put_Trace;
        END A;
      -----------
      PROCEDURE B
      -----------
        IS
        BEGIN
          A;
        END B;
      -----------
      PROCEDURE C
      -----------
        IS
        BEGIN
          B;
        END C;
      -----------
      PROCEDURE D
      -----------
        IS
        BEGIN
          C;
        END D;
      Specifics : CONSTANT Record_Specifics := Get_Specifics;
      BEGIN
        Put_Title("PROCESSOR TEST");
        Set_Precision(Double_Extended_Precision);
        Set_Rounding(Nearest_Rounding);
        Check_Exceptions;
        Put_Line("Clock ticks: " & Integer_8_Unsigned'Wide_Image(Get_Clock_Ticks));
        Put_Line("Sleep...");
        DELAY 0.5;
        Put_Line("Clock ticks: " & Integer_8_Unsigned'Wide_Image(Get_Clock_Ticks));
        Put_Line("Sleep...");
        DELAY 1.0;
        Put_Line("Clock ticks: "       & Integer_8_Unsigned'Wide_Image(Get_Clock_Ticks));
        Put_Line("Number of cores:"    & Integer_8_Unsigned'Wide_Image(Get_Number_Of_Cores));
        Put_Line("Speed in megahertz:" & Integer_8_Unsigned'Wide_Image(Get_Speed_In_Megahertz));
        Put_Line("Vendor: "            & Enumerated_Vendor'Wide_Image(Specifics.Vendor));
        CASE Specifics.Vendor IS
          WHEN ARM_Licenced_Vendor =>
            IF Specifics.Has_NEON THEN
              Put_Line("Has NEON");
            END IF;
            IF Specifics.Has_Vector_Floating_Point THEN
              Put_Line("Has VFP");
            END IF;
          WHEN Apple_IBM_Motorola_Vendor =>
            IF Specifics.Has_Vector_Multimedia_Instructions THEN
              Put_Line("Has VMI");
            END IF;
            IF Specifics.Has_Vector_Scalar_Instructions THEN
              Put_Line("Has VSI");
            END IF;
            IF Specifics.Has_Altivec_Additional_Registers THEN
              Put_Line("Has VMX128");
            END IF;
            IF Specifics.Has_Altivec THEN
              Put_Line("Has Altivec");
            END IF;
          WHEN Intel_Vendor | Advanced_Micro_Devices_Vendor =>
            IF Specifics.Advanced_Micro_Devices_Vendor THEN
              IF Specifics.Has_3DNow THEN
                Put_Line("Has 3DNow!");
              END IF;
              IF Specifics.Has_3DNow_Supplement THEN
                Put_Line("Has 3DNow!+");
              END IF;
              IF Specifics.Has_Streaming_SIMD_Extensions_4_Supplement THEN
                Put_Line("Has SSE4a");
              END IF;
              IF Specifics.Has_Multi_Media_Extensions_Supplement THEN
                Put_Line("Has MMX+");
              END IF;
            END IF;
            IF Specifics.Has_Multi_Media_Extensions THEN
              Put_Line("Has MMX");
            END IF;
            IF Specifics.Has_Fused_Multiply_Add_3 THEN
              Put_Line("Has FMA3");
            END IF;
            IF Specifics.Has_Fused_Multiply_Add_4 THEN
              Put_Line("Has FMA4");
            END IF;
            IF Specifics.Has_Streaming_SIMD_Extensions_1 THEN
              Put_Line("Has SSE");
            END IF;
            IF Specifics.Has_Streaming_SIMD_Extensions_2 THEN
              Put_Line("Has SSE2");
            END IF;
            IF Specifics.Has_Streaming_SIMD_Extensions_3 THEN
              Put_Line("Has SSE3");
            END IF;
            IF Specifics.Has_Streaming_SIMD_Extensions_3_Supplement THEN
              Put_Line("Has SSSE3");
            END IF;
            IF Specifics.Has_Streaming_SIMD_Extensions_4_1 THEN
              Put_Line("Has SSE4.1");
            END IF;
            IF Specifics.Has_Streaming_SIMD_Extensions_4_2 THEN
              Put_Line("Has SSE4.2");
            END IF;
            IF Specifics.Has_Bit_Manipulation_Extensions_1 THEN
              Put_Line("Has BMI1");
            END IF;
            IF Specifics.Has_Bit_Manipulation_Extensions_2 THEN
              Put_Line("Has BMI2");
            END IF;
            IF Specifics.Has_Advanced_Vector_Extensions_1 THEN
              Put("Has AVX");
              IF NOT Specifics.Has_Advanced_Vector_Extensions_Enabled THEN
                Put_Line(", but it's disabled");
              ELSE
                New_Line;
              END IF;
            END IF;
            IF Specifics.Has_Advanced_Vector_Extensions_2 THEN
              Put("Has AVX2");
              IF NOT Specifics.Has_Advanced_Vector_Extensions_Enabled THEN
                IF NOT Specifics.Has_Advanced_Vector_Extensions_1 THEN
                  Put_Line(", but it's disabled");
                ELSE
                  Put_Line(", but it's also disabled");
                END IF;
              ELSE
                New_Line;
              END IF;
            END IF;
            IF Specifics.Has_Context_ID_Manager THEN
              Put_Line("Has INVPCID");
            END IF;
            IF Specifics.Has_Population_Count THEN
              Put_Line("Has POPCNT");
            END IF;
            IF Specifics.Has_Leading_Zero_Count THEN
              Put_Line("Has LZCNT");
            END IF;
            IF Specifics.Has_Carryless_Multiplication_Of_Two_64_Bit THEN
              Put_Line("Has PCLMULQDQ");
            END IF;
            IF Specifics.Has_Extended_States_Enabled THEN
              Put_Line("Has OSXSAVE");
            END IF;
            IF Specifics.Has_Half_Precision_Floating_Point_Convert THEN
              Put_Line("Has F16C");
            END IF;
            IF Specifics.Has_High_Precision_Convert THEN
              Put_Line("Has CVT16");
            END IF;
            IF Specifics.Has_Advanced_Encryption_Service THEN
              Put_Line("Has AES");
            END IF;
            IF Specifics.Has_Advanced_State_Operations THEN
              Put_Line("Has FXSR");
            END IF;
            IF Specifics.Has_Extended_Operation_Support THEN
              Put_Line("Has XOP");
            END IF;
            IF Specifics.Has_Hyperthreading THEN
              Put_Line("Has HTT");
            END IF;
            IF Specifics.Has_Conditional_Move THEN
              Put_Line("Has CMOV");
            END IF;
          WHEN OTHERS =>
            NULL;
        END CASE;         
        IF Is_Stack_Empty THEN
          Put_Line("Stack IS empty!");
        ELSE
          Put_Stack;
          Clear_Stack;
          IF Is_Stack_Empty THEN
            Put_Line("Stack was cleared successfully");
          END IF;
        END IF;
        Put_Stack;
        D;
        Hang_Window;
      EXCEPTION
        WHEN Unsupported_Feature =>
          Put_Line("Unsupported feature!");
          Hang_Window;
      END Test;
  ----------------
  -- Initialize --
  ----------------
    PROCEDURE Initialize
      RENAMES Implementation_For_Architecture.Initialize;
  ----------------------
  -- Check_Exceptions --
  ----------------------
    PROCEDURE Check_Exceptions
      RENAMES Implementation_For_Architecture.Check_Exceptions;
  -----------------
  -- Clear_Stack --
  -----------------
    PROCEDURE Clear_Stack
      RENAMES Implementation_For_Architecture.Clear_Stack;
  --------------------
  -- Is_Stack_Empty --
  --------------------
    FUNCTION Is_Stack_Empty
      RETURN Boolean
      RENAMES Implementation_For_Architecture.Is_Stack_Empty;
  ------------------
  -- Set_Rounding --
  ------------------
    PROCEDURE Set_Rounding(
      Rounding : IN Enumerated_Rounding)
      RENAMES Implementation_For_Architecture.Set_Rounding;
  -------------------
  -- Set_Precision --
  -------------------
    PROCEDURE Set_Precision(
      Precision : IN Enumerated_Precision)
      RENAMES Implementation_For_Architecture.Set_Precision;
  --------------------
  -- Get_Extensions --
  --------------------
    FUNCTION Get_Extensions
      RETURN Record_Extensions
      RENAMES Implementation_For_Architecture.Get_Extensions;
  ----------------
  -- Get_Vendor --
  ----------------
    FUNCTION Get_Vendor
      RETURN Enumerated_Vendor
      RENAMES Implementation_For_Architecture.Get_Vendor;
  --------------------
  -- Get_Clock_Tics --
  --------------------
    FUNCTION Get_Clock_Ticks
      RETURN Integer_8_Unsigned
      IS
      BEGIN
        RETURN Implementation_For_Operating_System.Get_Clock_Ticks;
      EXCEPTION
        WHEN Unsupported_Feature | System_Call_Failure =>
          RETURN Implementation_For_Architecture.Get_Clock_Ticks;
      END Get_Clock_Ticks;
  -------------------------
  -- Get_Number_Of_Cores --
  -------------------------
    FUNCTION Get_Number_Of_Cores
      RETURN Integer_8_Unsigned
      IS
      BEGIN
        RETURN Implementation_For_Operating_System.Get_Number_Of_Cores;
      EXCEPTION
        WHEN Unsupported_Feature | System_Call_Failure =>
          -------------
          Try_Assembly:
          -------------
            DECLARE
            BEGIN
              RETURN Implementation_For_Architecture.Get_Number_Of_Cores;
            EXCEPTION
              WHEN Unsupported_Feature =>
                RETURN Integer_8_Unsigned(Number_Of_CPUs); -- How reliable is this?
            END Try_Assembly;
      END Get_Number_Of_Cores;
  ----------------------------
  -- Get_Speed_In_Megahertz --
  ----------------------------
    FUNCTION Get_Speed_In_Megahertz
      RETURN Integer_8_Unsigned
      IS
      BEGIN
        RETURN Implementation_For_Operating_System.Get_Speed_In_Megahertz;
      EXCEPTION
        WHEN Unsupported_Feature | System_Call_Failure =>
          ---------------
          Time_Processor:
          ---------------
            DECLARE
            Start : Integer_8_Unsigned := 0;
            BEGIN
              Start := Get_Clock_Ticks;
              DELAY 0.1;
              RETURN (Get_Clock_Ticks - Start) * 10;
            END Time_Processor;
      END Get_Speed_In_Megahertz;
  ---------------
  -- Put_Stack --
  ---------------
    PROCEDURE Put_Stack
      RENAMES Implementation_For_Architecture.Put_Stack;
  ---------------
  -- Put_Trace --
  ---------------
    PROCEDURE Put_Trace
      IS
      BEGIN
        Put_Line("Trace:");
        Implementation_For_Compiler.Put_Trace;
      EXCEPTION
        WHEN Unsupported_Feature | System_Call_Failure =>
          Implementation_For_Architecture.Put_Trace;
      END Put_Trace;
  END Neo.System.Processor;

