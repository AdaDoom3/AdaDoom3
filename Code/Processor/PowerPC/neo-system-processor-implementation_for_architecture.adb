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
  ------------------
  -- Enumerations --
  ------------------
    type Enumerated_Processor_Version
      is(
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
    for Enumerated_Processor_Version
      use(
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
  -- Records --
  -------------
    type Record_Version
      is record
        Major : Integer_2_Unsigned := 0;
        Minor : Integer_2_Unsigned := 0;
      end record;
    for Record_Version'Size
      use 32;
    type Record_Processor
      is record
        Core_Guess : Integer_8_Unsigned := 1;
        Extensions : Record_Extensions  := (others => <>);
      end record;
  ---------------
  -- Constants --
  ---------------
    POWERPC_PROCESSORS : constant array(Enumerated_Processor_Version'Range) of Record_Extensions :=(
      IBM_Cell_Broadband_Engine_Version =>(
        Core_Guess => 8,
        Extensions =>(
          --Has_Vector_Multimedia_Instructions => True, ???
          --Has_Vector_Scalar_Instructions     => True, ???
          Has_Altivec                        => True,
          others                             => False)),
      IBM_Xenon_Version =>(
        Core_Guess => 3,
        Extensions =>(
          --Has_Vector_Multimedia_Instructions => True, ???
          --Has_Vector_Scalar_Instructions     => True, ???
          Has_Altivec                        => True,
          Has_Altivec_Additional_Registers   => True,
          others                             => False),
      Power_8_Compliant_Version =>(
        Core_Guess => 16, -- ???
        Extensions =>(
          Has_Altivec                        => True,
          Has_Vector_Multimedia_Instructions => True,
          Has_Vector_Scalar_Instructions     => True,
          others                             => False),
      Power_7_Plus_Compliant_Version =>(
        Core_Guess => 8,
        Extensions =>(
          Has_Altivec                        => True,
          Has_Vector_Multimedia_Instructions => True,
          Has_Vector_Scalar_Instructions     => True,
          others                             => False),
      Power_7_Compliant_Version =>(
        Core_Guess => 8,
        Extensions =>(
          Has_Altivec                        => True,
          Has_Vector_Multimedia_Instructions => True,
          Has_Vector_Scalar_Instructions     => True,
          others                             => False),
      Power_6_Compliant_Version =>(
        Core_Guess => 2,
        Extensions =>(
          Has_Altivec                        => True,
          Has_Vector_Multimedia_Instructions => True,
          others                             => False),
      IBM_970_Version =>(
        Core_Guess => 1,
        Extensions =>(
          Has_Altivec                        => True,
          Has_Vector_Multimedia_Instructions => True,
          others                             => False),
      IBM_970FX_Version =>(
        Core_Guess => 1,
        Extensions =>(
          Has_Altivec                        => True,
          Has_Vector_Multimedia_Instructions => True,
          others                             => False),
      IBM_970GX_Version =>(
        Core_Guess => 1,
        Extensions =>(
          Has_Altivec                        => True,
          Has_Vector_Multimedia_Instructions => True,
          others                             => False),
      IBM_970MP_Version =>(
        Core_Guess => 2,
        Extensions =>(
          Has_Altivec                        => True,
          Has_Vector_Multimedia_Instructions => True,
          others                             => False),
      Motorola_7400_Version =>(
        Core_Guess => 1,
        Extensions =>(
          Has_Altivec                        => True,
          others                             => False),
      Motorola_7410_Version =>(
        Core_Guess => 1,
        Extensions =>(
          Has_Altivec                        => True,
          others                             => False),
      Motorola_7450_Version =>(
        Core_Guess => 1,
        Extensions =>(
          Has_Altivec                        => True,
          others                             => False),
      Motorola_7455_Version =>(
        Core_Guess => 1,
        Extensions =>(
          Has_Altivec                        => True,
          others                             => False),
      Motorola_7457_Version =>(
        Core_Guess => 1,
        Extensions =>(
          Has_Altivec                        => True,
          others                             => False),
      Motorola_7447A_Version =>(
        Core_Guess => 1,
        Extensions =>(
          Has_Altivec                        => True,
          others                             => False),
      Motorola_7448_Version =>(
        Core_Guess => 1,
        Extensions =>(
          Has_Altivec                        => True,
          others                             => False),
      others =>(
        Core_Guess => 1,
        Extensions =>(
          others                             => False));
  ----------------
  -- Initialize --
  ----------------
    procedure Initialize
      is
      begin
        null;
      end Initialize;
  ----------------
  -- Get_Vendor --
  ----------------
    function Get_Vendor
      return Enumerated_Vendor
      is
      begin
        return Apple_IBM_Motorola;
      end Get_Vendor;
  -----------------
  -- Get_Version --
  -----------------
    function Get_Version
      return Enumerated_Version
      is
      Version : aliased Record_Version := (others => <>);
      begin
        Asm(
          Template => "mfpvr %%r0",
          Volatile => True,
          Inputs   => Access_Record_Version'asm_input(TO_R0, Version'access));
        return Enumerated_Processor_Version'val(Version.Major);
      exception
        when  =>
          raise Unsupported_Feature;
      end Get_Version;
  -------------------------
  -- Get_Number_Of_Cores --
  -------------------------
    function Get_Number_Of_Cores
      return Integer_8_Natural
      is
      begin
        raise System_Call_Failure;
        return 1;
        --return POWERPC_PROCESSORS(Get_Version).Core_Guess;
      end Get_Number_Of_Cores;
  -------------------
  -- Get_Specifics --
  -------------------
    function Get_Specifics
      return Record_Specifics
      is
      begin
        raise System_Call_Failure;
        return (others => <>);
        --return POWERPC_PROCESSORS(Get_Version).Extensions;
      end Get_Specifics;
  ----------------------
  -- Check_Exceptions --
  ----------------------
    procedure Check_Exceptions
      is
      begin
        raise System_Call_Failure;
      end Check_Exceptions;
  ------------------
  -- Set_Rounding --
  ------------------
    procedure Set_Rounding(
      Rounding : in Enumerated_Rounding)
      is
      begin
        raise System_Call_Failure;
        --if not POWERPC_PROCESSORS(Get_Version).Has_Altivec or Rounding /= Round_To_Nearest THEN
        --  raise Unsupported_Feature;
        --end if;
      end Set_Rounding;
  -------------------
  -- Set_Precision --
  -------------------
    procedure Set_Precision(
      Precision : in Enumerated_Precision)
      is
      begin
        raise System_Call_Failure;
        --if not POWERPC_PROCESSORS(Get_Version).Has_Altivec or Precision /= Single_Precision THEN
        --  raise Unsupported_Feature;
        --end if;
      end Set_Precision;
  --------------------
  -- Get_Clock_Tics --
  --------------------
    function Get_Clock_Ticks
      return Integer_8_Unsigned
      is
--      Result : Integer_4_Unsigned := 0;
      begin
        raise Unsupported_Feature; -- Should be done
--         case Get_Version is
--           when
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
--           when
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
--           when others =>
--             raise Unsupported_Feature;
--         end case;
         return 0;
      end Get_Clock_Ticks;
  --------------------
  -- Is_Stack_Empty --
  --------------------
    function Is_Stack_Empty
      return Boolean
      is
      begin
        raise System_Call_Failure;
      end Is_Stack_Empty;
  -----------------
  -- Clear_Stack --
  -----------------
    procedure Clear_Stack
      is
      begin
        raise Unsupported_Feature;
      end Clear_Stack;
  --------------------------
  -- Compare_And_Exchange --
  --------------------------
    function Compare_And_Exchange(
      Destination : out Integer_4_Unsigned;
      Comparand   : in  Integer_4_Unsigned;
      Item        : in  Integer_4_Unsigned)
      return Integer_4_Unsigned
      is
      begin
        raise System_Call_Failure;
      end Compare_And_Exchange;
  ---------------
  -- Put_Stack --
  ---------------
    procedure Put_Stack
      is
      begin
        raise Unsupported_Feature;
      end Put_Stack;
  end Implementation_For_Architecture;
