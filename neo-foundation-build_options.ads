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
package Neo.Foundation.Build_Options
  is
  -----------------------------------------------------------------------




  -- Must be consistant with the usage/omission of 64 bit compile flag --

                  USE_64_BIT : constant Boolean := False;




  -----------------------------------------------------------------------
  ------------------
  -- Enumerations --
  ------------------
    type Enumerated_System
      is(
      Unknown_System,
      Linux_System, -- PowerPC?, x86-64?
      Linux_2_System,
      Linux_2_1_System,
      Linux_2_2_System,
      Linux_2_3_System,
      Linux_2_4_System,
      Linux_2_5_System,
      Linux_2_6_System,
      Linux_2_7_System, -- AVX
      Linux_2_8_System,
      Linux_2_9_System,
      Linux_3_System,
      Linux_3_1_System,
      Linux_3_2_System,
      Linux_3_3_System,
      Linux_3_4_System,
      Linux_3_5_System,
      Linux_3_6_System,
      Windows_System, -- x86-64
      Windows_1_4_A_System,
      Windows_1_4_B_System,
      Windows_1_4_10_A_System,
      Windows_1_4_10_B_System,
      Windows_1_4_90_System,
      Windows_2_System,
      Windows_2_5_System,
      Windows_2_5_1_System,
      Windows_2_6_System, -- Aero™
      Windows_2_6_1_System, -- AVX
      Windows_2_6_2_System,
      Macintosh_System, -- PowerPC
      Macintosh_8_5_System,
      Macintosh_8_5_1_System,
      Macintosh_8_6_System,
      Macintosh_9_System,
      Macintosh_9_0_1_System,
      Macintosh_9_0_2_System,
      Macintosh_9_0_3_System,
      Macintosh_9_0_4_System,
      Macintosh_9_1_System,
      Macintosh_9_2_System,
      Macintosh_9_2_1_System,
      Macintosh_9_2_2_System,
      Macintosh_10_System,
      Macintosh_10_1_System,
      Macintosh_10_2_System,
      Macintosh_10_3_System,
      Macintosh_10_4_System,
      Macintosh_10_4_7_System, -- x86-64
      Macintosh_10_5_System,
      Macintosh_10_5_8_System, -- PPC RIP
      Macintosh_10_6_System,
      Macintosh_10_6_8_System, -- AVX
      Macintosh_10_7_System,
      Macintosh_10_8_System);
    subtype Enumerated_Linux_System
      is Enumerated_System
      range Linux_System..Linux_3_6_System;
    subtype Enumerated_Windows_System
      is Enumerated_System
      range Windows_System..Windows_2_6_2_System;
    subtype Enumerated_Macintosh_System
      is Enumerated_System
      range Macintosh_System..Macintosh_10_8_System;
  end Neo.Foundation.Build_Options;
