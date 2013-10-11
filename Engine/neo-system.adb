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
  ------------
  -- Import --
  ------------
    package body Import
      is separate;
  --------------------
  -- Protected_Data --
  --------------------
    protected body Protected_Data
      is
        procedure Set_Icon_Path(
          Path : in String_2)
          is
          begin
            Current_Path := To_String_2_Unbounded(Path);
          end Set_Icon_Path;
        procedure Set_Name(
          Name : in String_2)
          is
          begin
            Current_Name := To_String_2_Unbounded(Name);
          end Set_Name;
        function Get_Name
          return String_2
          is
          begin
            return To_String_2(Current_Name);
          end Get_Name;
        function Get_Icon_Path
          return String_2
          is
          begin
            return To_String_2(Current_Path);
          end Get_Icon_Path;
      end Protected_Data;
  ----------
  -- Test --
  ----------
    procedure Test
      is
      begin
        Put_Title(Localize("SYSTEM TEST"));
        Put_Line(Localize("Version: ")              & Enumerated_System'wide_image(SPECIFICS.Version));
        Put_Line(Localize("Username: ")             & To_String_2(SPECIFICS.Username));
        Put_Line(Localize("Application bit size: ") & Integer_4_Signed'wide_image(WORD_SIZE));
        Put_Line(Localize("System bit size: ")      & Integer_4_Positive'wide_image(SPECIFICS.Bit_Size));
        if SPECIFICS.Version in Enumerated_Windows_System'range then
          Put_Line(Localize("Launching task manager..."));
          Execute_Application("taskmgr");
        elsif SPECIFICS.Version in Enumerated_Linux_System'range then
          Put_Line(Localize("Launching ???..."));
          --Execute_Application("???");
        elsif SPECIFICS.Version in Enumerated_Macintosh_System'range then
          Put_Line(Localize("Launching ???..."));
          --Execute_Application("???");
        end if;
        Put_Line(Localize("Opening ") & "google.com...");
        Open_Webpage("http://www.google.com");
      end Test;
  -------------------
  -- Set_Icon_Path --
  -------------------
    procedure Set_Icon_Path(
      Path : in String_2)
      is
      begin
        Data.Set_Icon_Path(Path);
      end Set_Icon_Path;
  --------------
  -- Set_Name --
  --------------
    procedure Set_Name(
      Name : in String_2)
      is
      begin
        Data.Set_Name(Name);
      end Set_Name;
  --------------
  -- Get_Name --
  --------------
    function Get_Name
      return String_2
      is
      begin
        return Data.Get_Name;
      end Get_Name;
  -------------------
  -- Get_Icon_Path --
  -------------------
    function Get_Icon_Path
      return String_2
      is
      begin
        return Data.Get_Icon_Path;
      end Get_Icon_Path;
  -----------------
  -- Browse_Path --
  -----------------
    function Browse_Path
      return String_2
      renames Import.Browse_Path;
  --------------------------
  -- Is_Feature_Supported --
  --------------------------
    function Is_Feature_Supported(
      Feature_Requirements : in Record_Feature_Requirements)
      return Boolean
      is
      begin
        if SPECIFICS.Version in Enumerated_Linux_System'range then
          return SPECIFICS.Version >= Feature_Requirements.Minimum_Linux;
        elsif SPECIFICS.Version in Enumerated_Windows_System'range then
          return SPECIFICS.Version >= Feature_Requirements.Minimum_Windows;
        elsif SPECIFICS.Version in Enumerated_Macintosh_System'range then
          return SPECIFICS.Version >= Feature_Requirements.Minimum_Macintosh;
        end if;
        return False;
      end Is_Feature_Supported;
  -------------------
  -- Get_Specifics --
  -------------------
    function Get_Specifics
      return Record_Specifics
      is
      begin
        return Import.Get_Specifics;
      exception
        when Call_Failure =>
          Put_Debug_Line(Localize(FAILED_GET_SPECIFICS));
          return (others => <>);
      end Get_Specifics;
  --------------------
  -- Get_Last_Error --
  --------------------
    function Get_Last_Error
      return String_2
      is
      begin
        return Localize(SYSTEM_ERROR_NUMBER) & Trim(Integer_4_Unsigned'wide_image(Import.Get_Last_Error_Number), Both);
      end Get_Last_Error;
  ------------------
  -- Open_Webpage --
  ------------------
    procedure Open_Webpage(
      Path : in String_2)
      is
      begin
        Import.Open_Webpage(Path);
      exception
        when others =>
          Put_Debug_Line(Localize(FAILED_OPEN_WEBPAGE) & Path);
      end Open_Webpage;
  -------------------------
  -- Execute_Application --
  -------------------------
    procedure Execute_Application(
      Executable_Path : in String_2;
      Do_Fullscreen   : in Boolean := False)
      is
      begin
        Import.Execute_Application(Executable_Path, Do_Fullscreen);
      exception
        when others =>
          Put_Debug_Line(Localize(FAILED_EXECUTE_APPLICATION) & Executable_Path);
      end Execute_Application;
  end Neo.System;

