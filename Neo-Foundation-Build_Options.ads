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
    type Enumerated_Operating_System_Version
      is(
      Unknown_Version,
      Linux_Version, -- PowerPC?, x86-64?
      Linux_2_Version,
      Linux_2_1_Version,
      Linux_2_2_Version,
      Linux_2_3_Version,
      Linux_2_4_Version,
      Linux_2_5_Version,
      Linux_2_6_Version,
      Linux_2_7_Version, -- AVX
      Linux_2_8_Version,
      Linux_2_9_Version,
      Linux_3_Version,
      Linux_3_1_Version,
      Linux_3_2_Version,
      Linux_3_3_Version,
      Linux_3_4_Version,
      Linux_3_5_Version,
      Linux_3_6_Version,
      Windows_Version, -- x86-64
      Windows_1_4_A_Version,
      Windows_1_4_B_Version,
      Windows_1_4_10_A_Version,
      Windows_1_4_10_B_Version,
      Windows_1_4_90_Version,
      Windows_2_Version,
      Windows_2_5_Version,
      Windows_2_5_1_Version,
      Windows_2_6_Version, -- Aero™
      Windows_2_6_1_Version, -- AVX
      Windows_2_6_2_Version,
      Macintosh_Version, -- PowerPC
      Macintosh_8_5_Version,
      Macintosh_8_5_1_Version,
      Macintosh_8_6_Version,
      Macintosh_9_Version,
      Macintosh_9_0_1_Version,
      Macintosh_9_0_2_Version,
      Macintosh_9_0_3_Version,
      Macintosh_9_0_4_Version,
      Macintosh_9_1_Version,
      Macintosh_9_2_Version,
      Macintosh_9_2_1_Version,
      Macintosh_9_2_2_Version,
      Macintosh_10_Version,
      Macintosh_10_1_Version,
      Macintosh_10_2_Version,
      Macintosh_10_3_Version,
      Macintosh_10_4_Version,
      Macintosh_10_4_7_Version, -- x86-64
      Macintosh_10_5_Version,
      Macintosh_10_5_8_Version, -- PPC RIP
      Macintosh_10_6_Version,
      Macintosh_10_6_8_Version, -- AVX
      Macintosh_10_7_Version,
      Macintosh_10_8_Version);
    subtype Enumerated_Linux_Version
      is Enumerated_Operating_System_Version
      Linux_Version..Linux_3_6_Version;
    subtype Enumerated_Windows_Version
      is Enumerated_Operating_System_Version
      Windows_Version..Windows_2_6_2_Version;
    subtype Enumerated_Macintosh_Version
      is Enumerated_Operating_System_Version
      Macintosh_Version..Macintosh_10_8_Version;
  end Neo.Foundation.Build_Options;