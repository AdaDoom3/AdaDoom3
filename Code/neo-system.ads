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
  Neo.Foundation.Text_IO,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Build_Options,
  Neo.Foundation.Package_Testing;
use
  Neo.Foundation.Text_IO,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Build_Options,
  Neo.Foundation.Package_Testing;
package Neo.System
  is
  ----------------
  -- Exceptions --
  ----------------
    System_Call_Failure : Exception;
    Unsupported_Feature : Exception;
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
  -----------------
  -- Subprograms --
  -----------------
    procedure Test;
    function Get_Language
      return Enumerated_Language;
    function Get_Version
      return Enumerated_System;
    function Get_Username
      return String_2;
    procedure Open_Webpage(
      Path : in String_2);
    procedure Execute_Application(
      Do_Quit         : in Boolean;
      Executable_Path : in String_2);
    function Is_Newer_Than(
      Linux     : in Enumerated_Linux_System;
      Macintosh : in Enumerated_Macintosh_System;
      Windows   : in Enumerated_Windows_System)
      return Boolean;
    function Is_Running_In_64_Bit_Environment
      return Boolean;
-------
private
-------
  --------------------
  -- Implementation --
  --------------------
    package Implementation
      is
        function Is_Running_In_64_Bit_Environment
          return Boolean;
        function Get_Language
          return Enumerated_Language;
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

