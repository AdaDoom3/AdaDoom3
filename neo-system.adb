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
PACKAGE BODY Neo.System
  IS
  --------------------
  -- Implementation --
  --------------------
    PACKAGE BODY Implementation
      IS SEPARATE;
  ----------
  -- Test --
  ----------
    PROCEDURE Test
      IS
      BEGIN
        Put_Title("SYSTEM TEST");
        Put_Line("Language " & Enumerated_Language'Wide_Image(Get_Language));
        Put_Line("Version "  & Enumerated_System'Wide_Image(Get_Version));
        Put_Line("Username """ & Get_Username & """");
        Open_Webpage("http://www.google.com");
        Execute_Application(False, "C:\Windows\System32\taskmgr.exe");
        --Put_Line("Newer than Windows XP IS " & Boolean'Wide_Image(Is_Newer_Than(Windows_2_5_1_System)));
        Hang_Window;
      END Test;
  -------------------
  -- Is_Newer_Than --
  -------------------
    FUNCTION Is_Newer_Than(
      Linux     : IN Enumerated_Linux_System;
      Macintosh : IN Enumerated_Macintosh_System;
      Windows   : IN Enumerated_Windows_System)
      RETURN Boolean
      IS
      Current_System : Enumerated_System := Get_Version;
      BEGIN
        -- if Operating_System IN Enumerated_Linux_System'Range then
        -- elsif Operating_System IN Enumerated_Windows_System'Range then
        -- elsif Operating_System IN Enumerated_Macintosh_System'Range then
        -- END if;
        RETURN True;
      END Is_Newer_Than;
  --------------------------------------
  -- Is_Running_In_64_Bit_Environment --
  --------------------------------------
    FUNCTION Is_Running_In_64_Bit_Environment
      RETURN Boolean
      IS
      BEGIN
        IF USE_64_BIT THEN
          RETURN True;
        END IF;
        RETURN Implementation.Is_Running_In_64_Bit_Environment;
      END Is_Running_In_64_Bit_Environment;
  ------------------
  -- Get_Language --
  ------------------
    FUNCTION Get_Language
      RETURN Enumerated_Language
      RENAMES Implementation.Get_Language;
  -----------------
  -- Get_Version --
  -----------------
    FUNCTION Get_Version
      RETURN Enumerated_System
      RENAMES Implementation.Get_Version;
  ------------------
  -- Get_Username --
  ------------------
    FUNCTION Get_Username
      RETURN String_2
      RENAMES Implementation.Get_Username;
  ------------------
  -- Open_Webpage --
  ------------------
    PROCEDURE Open_Webpage(
      Path : IN String_2)
      RENAMES Implementation.Open_Webpage;
  -------------------------
  -- Execute_Application --
  -------------------------
    PROCEDURE Execute_Application(
      Do_Quit         : IN Boolean;
      Executable_Path : IN String_2)
      RENAMES Implementation.Execute_Application;
  END Neo.System;
