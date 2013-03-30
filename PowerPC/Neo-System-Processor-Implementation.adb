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
  ------------------
  -- ENUMERATIONS --
  ------------------
    TYPE Enumerated_Processor_Version
      IS(
      Power_6_Compliant_Version,
      Power_7_Compliant_Version,
      Power_7_Plus_Compliant_Version,
      Power_8_Compliant_Version,
      Motorola_601_Version,
      Motorola_602_Version,
      Motorola_603_Version,
      Motorola_603E_Version,
      Motorola_603EV_Version,
      Motorola_604_Version,
      Motorola_604EV_Version,
      Motorola_620_Version,
      Motorola_750_Version,              -- Nintendo Wii
      Motorola_7400_Version,
      Motorola_7410_Version,
      Motorola_7450_Version,
      Motorola_7455_Version,
      Motorola_7457_Version,
      Motorola_7447A_Version,
      Motorola_7448_Version,
      Motorola_8240_Version,
      Motorola_8245_Version,
      IBM_750FX_Version,
      IBM_970_Version,
      IBM_970FX_Version,
      IBM_970GX_Version,
      IBM_970MP_Version,
      IBM_Cell_Broadband_Engine_Version, -- Sony Playstation3
      IBM_Xenon_Version,                 -- Microsoft Xbox360
      Freescale_E500V1_Core_Version,
      Freescale_E500V2_Core_Version); 
    FOR Enumerated_Processor_Version
      USE(
      Power_6_Compliant_Version         => 16#0F00#,
      Power_7_Compliant_Version         => 16#003F#,
      Power_7_Plus_Compliant_Version    => 16#004A#,
      Power_8_Compliant_Version         => 16#004B#,
      Motorola_601_Version              => 16#0001#,
      Motorola_602_Version              => 16#0005#,
      Motorola_603_Version              => 16#0003#,
      Motorola_603E_Version             => 16#0006#,
      Motorola_603EV_Version            => 16#0007#,
      Motorola_604_Version              => 16#0004#,
      Motorola_604EV_Version            => 16#0009#,
      Motorola_620_Version              => 16#0014#,
      Motorola_750_Version              => 16#0008#,
      Motorola_7400_Version             => 16#000C#,
      Motorola_7410_Version             => 16#800C#,
      Motorola_7450_Version             => 16#8000#,
      Motorola_7455_Version             => 16#8001#,
      Motorola_7457_Version             => 16#8002#,
      Motorola_7447A_Version            => 16#8003#,
      Motorola_7448_Version             => 16#8004#,
      Motorola_8240_Version             => 16#0081#,
      Motorola_8245_Version             => 16#8081#,
      IBM_750FX_Version                 => 16#7000#,
      IBM_970_Version                   => 16#0039#,
      IBM_970FX_Version                 => 16#003C#,
      IBM_970MP_Version                 => 16#0044#,
      IBM_970GX_Version                 => 16#0045#,
      IBM_Cell_Broadband_Engine_Version => 16#0070#,
      IBM_Xenon_Version                 => 16#0071#,
      Freescale_E500V1_Core_Version     => 16#8020#,
      Freescale_E500V2_Core_Version     => 16#8021#);
  -------------
  -- RECORDS --
  -------------
    TYPE Record_Version
      IS RECORD
        Major : Integer_2_Unsigned := 0;
        Minor : Integer_2_Unsigned := 0;
      END RECORD;
    FOR Record_Version'Size
      USE 32;
    TYPE Record_Processor
      IS RECORD
        Integer_8_Unsigned : Integer_8_Unsigned := 1;
        Extensions         : Record_Extensions  := (OTHERS => <>);
      END RECORD;
  ---------------
  -- CONSTANTS --
  ---------------
    POWERPC_PROCESSORS : CONSTANT ARRAY(Enumerated_Processor_Version'Range) OF Record_Extensions :=(
      IBM_Cell_Broadband_Engine_Version =>(
        Core_Guess => 8,
        Extensions =>(
          --Has_Vector_Multimedia_Instructions => True, ???
          --Has_Vector_Scalar_Instructions     => True, ???
          Has_Altivec                        => True,
          OTHERS                             => False)),
      IBM_Xenon_Version =>( 
        Core_Guess => 3,
        Extensions =>(
          --Has_Vector_Multimedia_Instructions => True, ???
          --Has_Vector_Scalar_Instructions     => True, ???
          Has_Altivec                        => True,
          Has_Altivec_Additional_Registers   => True,
          OTHERS                             => False), 
      Power_8_Compliant_Version =>(
        Core_Guess => 16, -- ???
        Extensions =>(
          Has_Altivec                        => True,
          Has_Vector_Multimedia_Instructions => True,
          Has_Vector_Scalar_Instructions     => True,
          OTHERS                             => False),
      Power_7_Plus_Compliant_Version =>(
        Core_Guess => 8,
        Extensions =>(
          Has_Altivec                        => True,
          Has_Vector_Multimedia_Instructions => True,
          Has_Vector_Scalar_Instructions     => True,
          OTHERS                             => False),
      Power_7_Compliant_Version =>(
        Core_Guess => 8,
        Extensions =>(
          Has_Altivec                        => True,
          Has_Vector_Multimedia_Instructions => True,
          Has_Vector_Scalar_Instructions     => True,
          OTHERS                             => False),
      Power_6_Compliant_Version =>(
        Core_Guess => 2,
        Extensions =>(
          Has_Altivec                        => True,
          Has_Vector_Multimedia_Instructions => True,
          OTHERS                             => False),
      IBM_970_Version =>(
        Core_Guess => 1,
        Extensions =>(
          Has_Altivec                        => True,
          Has_Vector_Multimedia_Instructions => True,
          OTHERS                             => False),
      IBM_970FX_Version =>(
        Core_Guess => 1,
        Extensions =>(
          Has_Altivec                        => True,
          Has_Vector_Multimedia_Instructions => True,
          OTHERS                             => False),
      IBM_970GX_Version =>(
        Core_Guess => 1,
        Extensions =>(
          Has_Altivec                        => True,
          Has_Vector_Multimedia_Instructions => True,
          OTHERS                             => False),
      IBM_970MP_Version =>(
        Core_Guess => 2,
        Extensions =>(
          Has_Altivec                        => True,
          Has_Vector_Multimedia_Instructions => True,
          OTHERS                             => False),
      Motorola_7400_Version =>(
        Core_Guess => 1,
        Extensions =>(
          Has_Altivec                        => True,
          OTHERS                             => False),
      Motorola_7410_Version =>(
        Core_Guess => 1,
        Extensions =>(
          Has_Altivec                        => True,
          OTHERS                             => False),
      Motorola_7450_Version =>(
        Core_Guess => 1,
        Extensions =>(
          Has_Altivec                        => True,
          OTHERS                             => False),
      Motorola_7455_Version =>(
        Core_Guess => 1,
        Extensions =>(
          Has_Altivec                        => True,
          OTHERS                             => False),
      Motorola_7457_Version =>(
        Core_Guess => 1,
        Extensions =>(
          Has_Altivec                        => True,
          OTHERS                             => False),
      Motorola_7447A_Version =>(
        Core_Guess => 1,
        Extensions =>(
          Has_Altivec                        => True,
          OTHERS                             => False),
      Motorola_7448_Version =>(
        Core_Guess => 1,
        Extensions =>(
          Has_Altivec                        => True,
          OTHERS                             => False),
      OTHERS =>(
        Core_Guess => 1,
        Extensions =>(
          OTHERS                             => False));
  ----------------
  -- Initialize --
  ----------------
    PROCEDURE Initialize
      IS
      BEGIN
        NULL;
      END Initialize;
  ----------------
  -- Get_Vendor --
  ----------------
    FUNCTION Get_Vendor
      RETURN Enumerated_Vendor
      IS
      BEGIN
        RETURN Apple_IBM_Motorola;
      END Get_Vendor;
  -----------------
  -- Get_Version --
  -----------------
    FUNCTION Get_Version
      RETURN Enumerated_Version
      IS
      Version : ALIASED Record_Version := (OTHERS => <>);
      BEGIN
        Asm(
          Template => "mfpvr %%r0",
          Volatile => True,
          Inputs   => Access_Record_Version'Asm_Input(TO_R0, Version'Access));
        RETURN Enumerated_Processor_Version'Val(Version.Major);
      EXCEPTION
        WHEN  => 
          RAISE Unsupported_Feature;
      END Get_Version;
  -------------------------
  -- Get_Number_Of_Cores --
  -------------------------
    FUNCTION Get_Number_Of_Cores
      RETURN Integer_8_Unsigned
      IS
      BEGIN
        RETURN POWERPC_PROCESSORS(Get_Version).Core_Guess;
      END Get_Number_Of_Cores;
  --------------------
  -- Get_Extensions --
  --------------------
    FUNCTION Get_Extensions
      RETURN Record_Extensions
      IS
      BEGIN
        RETURN POWERPC_PROCESSORS(Get_Version).Extensions;
      END Get_Extensions;
  ----------------------
  -- Check_Exceptions --
  ----------------------
    PROCEDURE Check_Exceptions
      IS
      BEGIN
        RAISE Unsupported_Feature; -- Should be done
      END Check_Exceptions;
  ------------------
  -- Set_Rounding --
  ------------------
    PROCEDURE Set_Rounding(
      Rounding : IN Enumerated_Rounding)
      IS
      BEGIN
        IF NOT POWERPC_PROCESSORS(Get_Version).Has_Altivec OR Rounding /= Round_To_Nearest THEN
          RAISE Unsupported_Feature;
        END IF;
      END Set_Rounding;
  -------------------
  -- Set_Precision --
  -------------------
    PROCEDURE Set_Precision(
      Precision : IN Enumerated_Precision)
      IS
      BEGIN
        IF NOT POWERPC_PROCESSORS(Get_Version).Has_Altivec OR Precision /= Single_Precision THEN
          RAISE Unsupported_Feature;
        END IF;
      END Set_Precision;
  --------------------
  -- Get_Clock_Tics --
  --------------------
    FUNCTION Get_Clock_Ticks
      RETURN Integer_8_Unsigned
      IS
--      Result : Integer_4_Unsigned := 0;
      BEGIN
        RAISE Unsupported_Feature; -- Should be done
--         CASE Get_Version IS
--           WHEN 
--             Motorola_7400_Version  |
--             Motorola_7410_Version  |
--             Motorola_7450_Version  |
--             Motorola_7455_Version  |
--             Motorola_7457_Version  |
--             Motorola_7447A_Version |
--             Motorola_7448_Version  |
--             Motorola_750_Version   |
--             IBM_750FX_Version      =>
--               Asm(
--                 -----------------------------------------
--                 " mtspr %%mmcr0, %%mmcr0fc " & END_LINE &
--                 " mtspr %%pmc1,  0         " & END_LINE &
--                 " mtspr %%mmcr0, 1 << 6 " & END_LINE ,
--                 " mfspr 0x3b9 * 1000) + 4999 " & END_LINE &
--                 " mtspr SPR_MMCR0, 0x80000000 " & END_LINE &
--                 " mtmsr msr " & END_LINE ,
--                 --------------------------
--                 Volitile => True,
--                 Inputs =>
--                 Outputs =>);
--           WHEN
--             IBM_750FX_Version |
--             IBM_970FX_Version |
--             IBM_970GX_Version |
--             IBM_970MP_Version =>
--               Asm(
--                 ----------------------------------------
--                 " isync                   " & END_LINE &
--                 " mtspr 0x31b, 0x80000000 " & END_LINE &
--                 " isync                   " & END_LINE &
--                 " mtspr 0x31e, 0          " & END_LINE &
--                 " mtspr 0x312, 0          " & END_LINE &
--                 " mtspr 0x313, 0          " & END_LINE &
--                 " mtspr 0x31b, 0x300c0000 " & END_LINE &
--                 " isync                   " & END_LINE &
--                 " sync                    " & END_LINE &
--                 " mtspr 0x31b, 0x80000000; " & END_LINE &
--                 " (mfspr(SPR_970PMC1) * 1000) + 4999" & END_LINE &
--                 " mtmsr(msr) " & END_LINE ,
--                 --------------------------
--                 Volitile => True,
--                 Inputs =>
--                 Outputs =>);
--           WHEN OTHERS =>
--             RAISE Unsupported_Feature;
--         END CASE;
         RETURN 0;        
      END Get_Clock_Ticks;
  --------------------
  -- Is_Stack_Empty --
  --------------------
    FUNCTION Is_Stack_Empty
      RETURN Boolean
      IS
      BEGIN
        RAISE Unsupported_Feature;
      END Is_Stack_Empty;
  -----------------
  -- Clear_Stack --
  -----------------
    PROCEDURE Clear_Stack
      IS
      BEGIN
        RAISE Unsupported_Feature;
      END Clear_Stack;
  ---------------
  -- Put_Trace --
  ---------------
    PROCEDURE Put_Trace
      IS
      BEGIN
        RAISE Unsupported_Feature;
      END Put_Trace;
 --------------------------
 -- Compare_And_Exchange --
 --------------------------
   FUNCTION Compare_And_Exchange(
      Destination : OUT Integer_4_Unsigned;
      Comparand   : IN  Integer_4_Unsigned;
      Item        : IN  Integer_4_Unsigned)
      RETURN Integer_4_Unsigned
      IS
      BEGIN
        RAISE Unsupported_Feature; -- Should be done
      END Compare_And_Exchange;
  ---------------
  -- Put_Stack --
  ---------------
    PROCEDURE Put_Stack
      IS
      BEGIN
        RAISE Unsupported_Feature;
      END Put_Stack;
  END Implementation_For_Architecture;

