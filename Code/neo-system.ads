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
  System,
  Neo.Foundation.Text_IO,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Package_Testing;
use
  System,
  Neo.Foundation.Text_IO,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Package_Testing;
package Neo.System
  is
  ----------------
  -- Exceptions --
  ----------------
    System_Call_Failure : Exception;
    Unsupported_Feature : Exception;
  ---------------
  -- Constants --
  ---------------
    DEFAULT_USERNAME : constant String_2 := "Unnamed";
  ------------------
  -- Enumerations --
  ------------------
    type Enumerated_Language
      is(
      Arabic_Language,
      Basque_Language,
      Catalan_Language,
      Simplified_Chinese_Language,
      Traditional_Chinese_Language,
      Czech_Language,
      Danish_Language,
      Dutch_Language,
      English_Language,
      Finnish_Langauge,
      French_Langauge,
      German_Language,
      Greek_Language,
      Hebrew_Language,
      Hungarian_Language,
      Italian_Language,
      Japanese_Language,
      Korean_Language,
      Norwegian_Language,
      Polish_Language,
      Portuguese_Language,
      Brazilian_Portuguese_Language,
      Russian_Language,
      Slovakian_Language,
      Slovenian_Language,
      Spanish_Language,
      Swedish_Language,
      Turkish_Language);
    type Enumerated_System
      is(
      Unknown_System,
      Linux_System,
      Linux_2_System, -- x86-64
      Linux_2_1_System,
      Linux_2_2_System,
      Linux_2_3_System,
      Linux_2_4_System,
      Linux_2_5_System,
      Linux_2_6_System,
      Linux_3_System, -- AVX
      Linux_3_1_System,
      Linux_3_2_System,
      Linux_3_3_System,
      Linux_3_4_System,
      Linux_3_5_System,
      Linux_3_6_System,
      Linux_3_7_System,
      Linux_3_8_System,
      Linux_3_9_System,
      Windows_System,
      Windows_1_System,
      Windows_1_4_System,
      Windows_1_4_A_System, -- x86-64
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
      Macintosh_System,
      Macintosh_8_5_System, -- PowerPC
      Macintosh_8_6_System,
      Macintosh_9_0_System,
      Macintosh_9_1_System,
      Macintosh_9_2_System,
      Macintosh_10_System,
      Macintosh_10_1_System,
      Macintosh_10_2_System,
      Macintosh_10_3_System,
      Macintosh_10_4_System,
      Macintosh_10_5_System, -- x86-64
      Macintosh_10_6_System,
      Macintosh_10_7_System, -- AVX
      Macintosh_10_8_System);
    subtype Enumerated_Linux_System
      is Enumerated_System
      range Linux_System..Linux_3_9_System;
    subtype Enumerated_Windows_System
      is Enumerated_System
      range Windows_System..Windows_2_6_2_System;
    subtype Enumerated_Macintosh_System
      is Enumerated_System
      range Macintosh_System..Macintosh_10_8_System;
  ----------------
  -- Suprograms --
  ----------------
    procedure Test;
    function Get_Version
      return Enumerated_System;
    function Get_Username
      return String_2;
    function Is_Newer_Than(
      Linux     : in Enumerated_Linux_System;
      Macintosh : in Enumerated_Macintosh_System;
      Windows   : in Enumerated_Windows_System)
      return Boolean;
    procedure Open_Webpage(
      Path : in String_2);
    procedure Execute_Application(
      Do_Quit         : in Boolean;
      Executable_Path : in String_2);
    function Is_Running_In_Emulated_32_Bit_Environment -- Currently only here for Windows® keyboard input
      return Boolean;
-------
private
-------
  --------------------
  -- Implementation --
  --------------------
    package Implementation
      is
        function Is_Running_In_Emulated_32_Bit_Environment
          return Boolean;
        function Get_Version
          return Enumerated_System;
        function Get_Username
          return String_2;
        procedure Open_Webpage(
          Path : in String_2);
        procedure Execute_Application(
          Do_Quit         : in Boolean;
          Executable_Path : in String_2);
      end Implementation;
  end Neo.System;

