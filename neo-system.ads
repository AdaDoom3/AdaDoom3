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
  Neo.Foundation.Text_IO,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Build_Options,
  Neo.Foundation.Package_Testing;
USE
  Neo.Foundation.Text_IO,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Build_Options,
  Neo.Foundation.Package_Testing;
PACKAGE Neo.System
  IS
  ----------------
  -- EXCEPTIONS --
  ----------------
    System_Call_Failure : Exception;
  ------------------
  -- ENUMERATIONS --
  ------------------
    TYPE Enumerated_Language
      IS(
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
  -- SUBPROGRAMS --
  -----------------
    PROCEDURE Test;
    FUNCTION Get_Language
      RETURN Enumerated_Language;
    FUNCTION Get_Version
      RETURN Enumerated_System;
    FUNCTION Get_Username
      RETURN String_2;
    PROCEDURE Open_Webpage(
      Path : IN String_2);
    PROCEDURE Execute_Application(
      Do_Quit         : IN Boolean;
      Executable_Path : IN String_2);
    FUNCTION Is_Newer_Than(
      Linux     : IN Enumerated_Linux_System;
      Macintosh : IN Enumerated_Macintosh_System;
      Windows   : IN Enumerated_Windows_System)
      RETURN Boolean;
    FUNCTION Is_Running_In_64_Bit_Environment
      RETURN Boolean;
-------
PRIVATE
-------
  --------------------
  -- IMPLEMENTATION --
  --------------------
    PACKAGE Implementation
      IS
        FUNCTION Is_Running_In_64_Bit_Environment
          RETURN Boolean;
        FUNCTION Get_Language
          RETURN Enumerated_Language;
        FUNCTION Get_Version
          RETURN Enumerated_System;
        FUNCTION Get_Username
          RETURN String_2;
        PROCEDURE Open_Webpage(
          Path : IN String_2);
        PROCEDURE Execute_Application(
          Do_Quit         : IN Boolean;
          Executable_Path : IN String_2);
      END Implementation;
  END Neo.System;

