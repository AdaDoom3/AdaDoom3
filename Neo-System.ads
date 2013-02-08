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
  ---------------
  -- Constants --
  ---------------
    DO_RETURN_NEWER_THAN_INCOMPATIBLE_VERSION : constant Boolean := False;
  ----------------
  -- Exceptions --
  ----------------
    System_Call_Failure : Exception;
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
  -------------
  -- Records --
  -------------
    type Record_System
      is record
        System   : Enumerated_System                   := Unknown_System;
        Language : Enumerated_Language                 := English_Language;
        Version  : Enumerated_Operating_System_Version := Unknown_Version;
        Username : Access_String_2                     := null;
      end record;
  -----------------
  -- Subprograms --
  -----------------
    procedure Test;
    procedure Put;
    function Get
      return Record_System;
    procedure Open_Webpage(
      Path : in String_2);
    procedure Execute_Application(
      Do_Quit         : in Boolean;
      Executable_Path : in String_2);
-------
private
-------
  --------------------
  -- Implementation --
  --------------------
    package Implementation_For_Operating_System
      is
        function Get
          return Record_System;
        procedure Open_Webpage(
          Path : in String_2);
        procedure Execute_Application(
          Do_Quit         : in Boolean;
          Executable_Path : in String_2);
      end Implementation;
    package body Implementation_For_Operating_System
      is separate;
    package Implementation
      renames Implementation_For_Operating_System;
  end Neo.System;
