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
  Interfaces.C,
  Ada.Strings,
  Ada.Strings.Wide_Fixed,
  Neo.Linux;
use
  System,
  Interfaces.C,
  Ada.Strings,
  Ada.Strings.Wide_Fixed,
  Neo.Linux;
separate(Neo.System)
package body Implementation
  is
  -----------------
  -- Get_Version --
  -----------------
    function Get_Version
      return Enumerated_System
      is
      begin
        return Linux_System;
      end Get_Version;
  ------------------
  -- Get_Language --
  ------------------
    function Get_Language
      return Enumerated_Language
      is
      begin
        return English_Language;
      end Get_Language;
  ------------------
  -- Get_Username --
  ------------------
    function Get_Username
      return String_2
      is
      begin
        return "Unimplemented";
      end Get_Username;
  ------------------
  -- Open_Webpage --
  ------------------
    procedure Open_Webpage(
      Path : in String_2)
      is
      begin
        null;
      end Open_Webpage;
  -------------------------
  -- Execute_Application --
  -------------------------
    procedure Execute_Application(
      Do_Quit         : in Boolean;
      Executable_Path : in String_2)
      is
      begin
        null;
      end Execute_Application;
  --------------------------------------
  -- Is_Running_In_64_Bit_Environment --
  --------------------------------------
    function Is_Running_In_64_Bit_Environment
      return Boolean
      is
      begin
        return False;
      end Is_Running_In_64_Bit_Environment;
  end Implementation;
