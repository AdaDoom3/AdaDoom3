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
        Put_Line("Version "  & Enumerated_Operating_System'Wide_Image(Get_Operating_System));
        Put_Line("Username """ & Get_Username & """");
        Open_Webpage("http://www.google.com");
        Execute_Application(False, "C:\Windows\System32\taskmgr.exe");
        Put_Line("Newer than Windows XP is " & Boolean'Wide_Image(Is_Newer_Than(Windows_2_5_1_System)));
        Hang_Window;
      end Test;
  ------------------
  -- Get_Language --
  ------------------
    function Get_Language
      return Enumerated_Language
      renames Implementation.Get_Language;
  --------------------------
  -- Get_Operating_System --
  --------------------------
    function Get_Operating_System
      return Enumerated_Operating_System
      renames Implementation.Get_Operating_System;
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
  -------------------
  -- Is_Newer_Than --
  -------------------
    function Is_Newer_Than(
      Operating_System : in Enumerated_Operating_System)
      return Boolean
      is
      Current_System : Enumerated_Operating_System := Get_Operating_System;
      begin
        -- if Operating_System in Enumerated_Linux_System'Range then
        -- elsif Operating_System in Enumerated_Windows_System'Range then
        -- elsif Operating_System in Enumerated_Macintosh_System'Range then

        -- end if;
        return False;
      end Is_Newer_Than;
  end Neo.System;
