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
      Motorola_601_Version,
      Motorola_602_Version,
      Motorola_603_Version,
      Motorola_603E_Version,
      Motorola_603EV_Version,
      Motorola_604_Version,
      Motorola_604EV_Version,
      Motorola_620_Version,
      Motorola_750_Version,
      IBM_750FX_Version,
      IBM_970_Version,
      IBM_970FX_Version,
      IBM_970GX_Version,
      IBM_970MP_Version,
      Motorola_7400_Version,
      Motorola_7410_Version,
      Motorola_7450_Version,
      Motorola_7455_Version,
      Motorola_7457_Version,
      Motorola_7447A_Version,
      Motorola_7448_Version,
      Motorola_8240_Version,
      Motorola_8245_Version,
      Freescale_E500V1_Core_Version,
      Freescale_E500V2_Core_Version,
      IBM_Cell_Broadband_Engine_Version,
      IBM_Xenon_Version);
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
      IBM_750FX_Version                 => 16#7000#,
      IBM_970FX_Version                 => 16#003C#,
      IBM_970GX_Version                 => 16#0045#,
      IBM_970MP_Version                 => 16#0044#,
      Motorola_7400_Version             => 16#000C#,
      Motorola_7410_Version             => 16#800C#,
      Motorola_7450_Version             => 16#8000#,
      Motorola_7455_Version             => 16#8001#,
      Motorola_7457_Version             => 16#8002#,
      Motorola_7447A_Version            => 16#8003#,
      Motorola_7448_Version             => 16#8004#,
      Motorola_8240_Version             => 16#0081#,
      Motorola_8245_Version             => 16#8081#,
      Freescale_E500V1_Core_Version     => 16#8020#,
      Freescale_E500V2_Core_Version     => 16#8021#,
      IBM_Cell_Broadband_Engine_Version => 16#0070#;
      IBM_Xenon_Version                 => 16#0071#);
  -------------
  -- RECORDS --
  -------------
    TYPE Record_Version
      IS RECORD
        Major : Integer_2_Unsigned := 0;
        Minor : Integer_2_Unsigned := 0;
      END RECORD;
    FOR Record_Version'Size
      USE 2 * Integer_2_Unsigned'Size;
  ---------------
  -- CONSTANTS --
  ---------------
    POWERPC_PROCESSORS : CONSTANT ARRAY(Enumerated_Processor_Version'Range) OF Record_Extensions :=(
      Power_6_Compliant_Version =>(
        Has_Altivec                        => True,
        Has_Vector_Multimedia_Instructions => True,
        OTHERS                             => False),
      Power_7_Compliant_Version =>(
        Has_Altivec                        => True,
        Has_Vector_Multimedia_Instructions => True,
        Has_Vector_Scalar_Instructions     => True,
        OTHERS                             => False),
      Power_7_Plus_Compliant_Version =>(
        Has_Altivec                        => True,
        Has_Vector_Multimedia_Instructions => True,
        Has_Vector_Scalar_Instructions     => True,
        OTHERS                             => False),
      Power_8_Compliant_Version =>(
        Has_Altivec                        => True,
        Has_Vector_Multimedia_Instructions => True,
        Has_Vector_Scalar_Instructions     => True,
        OTHERS                             => False),
      IBM_970_Version =>(
        Has_Altivec                        => True,
        Has_Vector_Multimedia_Instructions => True,
        OTHERS                             => False),
      IBM_970FX_Version =>(
        Has_Altivec                        => True,
        Has_Vector_Multimedia_Instructions => True,
        OTHERS                             => False),
      IBM_970GX_Version =>(
        Has_Altivec                        => True,
        Has_Vector_Multimedia_Instructions => True,
        OTHERS                             => False),
      IBM_970MP_Version =>(
        Has_Altivec                        => True,
        Has_Vector_Multimedia_Instructions => True,
        OTHERS                             => False),
      Motorola_7400_Version =>(
        Has_Altivec                        => True,
        OTHERS                             => False),
      Motorola_7410_Version =>(
        Has_Altivec                        => True,
        OTHERS                             => False),
      Motorola_7450_Version =>(
        Has_Altivec                        => True,
        OTHERS                             => False),
      Motorola_7455_Version =>(
        Has_Altivec                        => True,
        OTHERS                             => False),
      Motorola_7457_Version =>(
        Has_Altivec                        => True,
        OTHERS                             => False),
      Motorola_7447A_Version =>(
        Has_Altivec                        => True,
        OTHERS                             => False),
      Motorola_7448_Version =>(
        Has_Altivec                        => True,
        OTHERS                             => False),
      IBM_Cell_Broadband_Engine_Version =>(
        Has_Altivec                        => True,
        OTHERS                             => False),
      IBM_Xenon_Version =>(
        Has_Altivec                        => True,
        Has_Altivec_Additional_Registers   => True,
        OTHERS                             => False), 
      OTHERS =>(
        OTHERS                             => False));
  -------------------------
  -- Get_Number_Of_Cores --
  -------------------------
    FUNCTION Get_Number_Of_Cores
      RETURN Integer_8_Unsigned
      IS
      BEGIN
        RAISE Unsupported_Feature;
      END Get_Number_Of_Cores;
  ----------------------------
  -- Get_Speed_In_Megahertz --
  ----------------------------
    FUNCTION Get_Speed_In_Megahertz
      RETURN Integer_8_Unsigned
      IS
      BEGIN
      END Get_Speed_In_Megahertz;
  ----------------
  -- Initialize --
  ----------------
    PROCEDURE Initialize
      IS
      BEGIN
        NULL;
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
        RETURN Extensions;
      END Get_Extensions;
  ----------------------
  -- Check_Exceptions --
  ----------------------
    PROCEDURE Check_Exceptions
      IS
      BEGIN

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

      END Set_Precision;
  --------------------
  -- Get_Clock_Tics --
  --------------------
    FUNCTION Get_Clock_Ticks
      RETURN Integer_8_Unsigned
      IS
      BEGIN
      END Get_Clock_Ticks;
  --------------------
  -- Is_Stack_Empty --
  --------------------
    FUNCTION Is_Stack_Empty
      RETURN Boolean
      IS
      BEGIN
      END Is_Stack_Empty;
  -----------------
  -- Clear_Stack --
  -----------------
    PROCEDURE Clear_Stack
      IS
      BEGIN

      END Clear_Stack;
  ---------------
  -- Put_Trace --
  ---------------
    PROCEDURE Put_Trace
      IS
      BEGIN
        NULL;
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

      END Compare_And_Exchange;
  ---------------
  -- Put_Stack --
  ---------------
    PROCEDURE Put_Stack
      IS
      BEGIN
  
      END Put_Stack;
  END Implementation_For_Architecture;

