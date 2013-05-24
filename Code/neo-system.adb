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
        Put_Line("Version: "  & Enumerated_System'Wide_Image(Get_Version));
        Put_Line("Username: """ & Get_Username & """");
        Put_Line("Application bit size: " & Integer_4_Positive'Wide_Image(Get_Application_Bit_Size));
        Put_Line("System bit size: " & Integer_4_Positive'Wide_Image(Get_Operating_System_Bit_Size));
        Open_Webpage("http://www.google.com");
        Execute_Application(False, "C:\Windows\System32\taskmgr.exe");
        Put_Line("OS supports AVX assembly is " & Boolean'Wide_Image(
          Is_Newer_Than(Linux_3_System, Macintosh_10_7_System, Windows_2_6_1_System)));
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
        if Current_System in Enumerated_Linux_System'range then
          return Current_System >= Linux;
        elsif Current_System in Enumerated_Windows_System'range then
          return Current_System >= Windows;
        elsif Current_System in Enumerated_Macintosh_System'range then
          return Current_System >= Macintosh;
        end if;
        return False;
      end Is_Newer_Than;
  ------------------------------
  -- Get_Application_Bit_Size --
  ------------------------------
    function Get_Application_Bit_Size
      return Integer_4_Positive
      is
      begin
        return Address'Size;
      end Get_Application_Bit_Size;
  -----------------------------------
  -- Get_Operating_System_Bit_Size --
  -----------------------------------
    function Get_Operating_System_Bit_Size
      return Integer_4_Positive
      is
      begin
        return Implementation.Get_Operating_System_Bit_Size;
      exception
        when System_Call_Failure =>
          return Get_Application_Bit_Size;
      end Get_Operating_System_Bit_Size;
  -----------------
  -- Get_Version --
  -----------------
    function Get_Version
      return Enumerated_System
      is
      begin
        return Implementation.Get_Version;
      exception
        when System_Call_Failure =>
          return Unknown_System;
      end Get_Version;
  ------------------
  -- Get_Username --
  ------------------
    function Get_Username
      return String_2
      is
      begin
        return Implementation.Get_Username;
      exception
        when System_Call_Failure =>
          return DEFAULT_USERNAME;
      end Get_Username;
  ------------------
  -- Open_Webpage --
  ------------------
    procedure Open_Webpage(
      Path : in String_2)
      is
      begin
        Implementation.Open_Webpage(Path);
      exception
        when System_Call_Failure =>
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
        Implementation.Execute_Application(Do_Quit, Executable_Path);
      exception
        when System_Call_Failure =>
          null;
      end Execute_Application;
  end Neo.System;

