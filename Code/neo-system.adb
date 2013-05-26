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
        Put_Line("Application bit size: " & Integer_4_Signed'Wide_Image(WORD_SIZE));
        Put_Line("System bit size: " & Integer_4_Positive'Wide_Image(Get_Bit_Size));
        Open_Webpage("http://www.google.com");
        if Get_Version in Enumerated_Windows_System'range then
          Put_Line("Launching task manager...");
          Execute_Application("taskmgr");
        elsif Get_Version in Enumerated_Linux_System'range then
          Put_Line("Launching ???...");
          null;--Execute_Application("???");
        elsif Get_Version in Enumerated_Macintosh_System'range then
          Put_Line("Launching ???...");
          null;--Execute_Application("???");
        end if;
        Put_Line("Supports AVX feature is " & Boolean'Wide_Image(Is_Feature_Supported(REQUIREMENTS_FOR_AVX)));
        Hang_Window;
      end Test;
  --------------------------
  -- Is_Feature_Supported --
  --------------------------
    function Is_Feature_Supported(
      Feature_Requirements : in Record_Feature_Requirements)
      return Boolean
      is
      Current_System : Enumerated_System := Get_Version;
      begin
        if Current_System in Enumerated_Linux_System'range then
          return Current_System >= Feature_Requirements.Minimum_Linux;
        elsif Current_System in Enumerated_Windows_System'range then
          return Current_System >= Feature_Requirements.Minimum_Windows;
        elsif Current_System in Enumerated_Macintosh_System'range then
          return Current_System >= Feature_Requirements.Minimum_Macintosh;
        end if;
        return False;
      end Is_Feature_Supported;
  ------------------
  -- Get_Bit_Size --
  ------------------
    function Get_Bit_Size
      return Integer_4_Positive
      is
      begin
        return Implementation.Get_Bit_Size;
      exception
        when System_Call_Failure =>
          return Address'Size;
      end Get_Bit_Size;
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
          Put_Line("Could not open " & Path);
      end Open_Webpage;
  -------------------------
  -- Execute_Application --
  -------------------------
    procedure Execute_Application(
      Executable_Path : in String_2;
      Do_Quit         : in Boolean := False)
      is
      begin
        Implementation.Execute_Application(Executable_Path, Do_Quit);
      exception
        when System_Call_Failure =>
          null;
        when others =>
          Put_Line("Unknown failure");
      end Execute_Application;
  end Neo.System;

