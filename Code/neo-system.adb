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
package body Neo.System
  is
  --------------------
  -- Implementation --
  --------------------
    package body Implementation
      is separate;
  ----------
  -- Test --
  ----------
    procedure Test
      is
      begin
        Put_Title("SYSTEM TEST");
        Put_Line("Language " & Enumerated_Language'Wide_Image(Get_Language));
        Put_Line("Version "  & Enumerated_System'Wide_Image(Get_Version));
        Put_Line("Username """ & Get_Username & """");
        Open_Webpage("http://www.google.com");
        Execute_Application(False, "C:\Windows\System32\taskmgr.exe");
        --Put_Line("Newer than Windows XP is " & Boolean'Wide_Image(Is_Newer_Than(Windows_2_5_1_System)));
        Hang_Window;
      end Test;
  -------------------
  -- Is_Newer_Than --
  -------------------
    function Is_Newer_Than(
      Linux     : in Enumerated_Linux_System;
      Macintosh : in Enumerated_Macintosh_System;
      Windows   : in Enumerated_Windows_System)
      return Boolean
      is
      Current_System : Enumerated_System := Get_Version;
      begin
        -- if Operating_System in Enumerated_Linux_System'Range then
        -- elsif Operating_System in Enumerated_Windows_System'Range then
        -- elsif Operating_System in Enumerated_Macintosh_System'Range then
        -- end if;
        return True;
      end Is_Newer_Than;
  -----------------------------------------------
  -- Is_Running_In_Emulated_32_Bit_Environment --
  -----------------------------------------------
    function Is_Running_In_Emulated_32_Bit_Environment
      return Boolean
      is
      begin
        if Address'Size = 64 THEN
          return True;
        end if;
        return Implementation.Is_Running_In_Emulated_32_Bit_Environment;
      end Is_Running_In_Emulated_32_Bit_Environment;
  ------------------
  -- Get_Language --
  ------------------
    function Get_Language
      return Enumerated_Language
      renames Implementation.Get_Language;
  -----------------
  -- Get_Version --
  -----------------
    function Get_Version
      return Enumerated_System
      renames Implementation.Get_Version;
  ------------------
  -- Get_Username --
  ------------------
    function Get_Username
      return String_2
      renames Implementation.Get_Username;
  ------------------
  -- Open_Webpage --
  ------------------
    procedure Open_Webpage(
      Path : in String_2)
      renames Implementation.Open_Webpage;
  -------------------------
  -- Execute_Application --
  -------------------------
    procedure Execute_Application(
      Do_Quit         : in Boolean;
      Executable_Path : in String_2)
      renames Implementation.Execute_Application;
  end Neo.System;
