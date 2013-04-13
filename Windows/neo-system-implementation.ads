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
  System,
  Interfaces.C,
  Ada.Strings,
  Ada.Strings.Wide_Fixed,
  Neo.Windows;
USE
  System,
  Interfaces.C,
  Ada.Strings,
  Ada.Strings.Wide_Fixed,
  Neo.Windows;
SEPARATE(Neo.System)
PACKAGE BODY Implementation
  IS
  ---------------
  -- Constants --
  ---------------
    STRING_BUFFER_SIZE_FOR_GET_USERNAME : CONSTANT Integer_4_Signed := 1024;
  -----------------
  -- Get_Version --
  -----------------
    FUNCTION Get_Version
      RETURN Enumerated_Operating_System
      IS
      Version_Information : Record_Version_Information := (OTHERS => <>);
      BEGIN
        if Get_Version(Version_Information'Address) = FAILED THEN
          Put_Line(Integer_4_Unsigned_C'Wide_Image(Get_Last_Error));
          RAISE System_Call_Failure;
        END if;
        CASE Version_Information.Platform_Identifier IS
          WHEN 1 =>
            CASE Version_Information.Major IS
              WHEN 4 =>
                CASE Version_Information.Minor IS
                  WHEN 0 =>
                    CASE Version_Information.Service_Pack(2) IS
                      WHEN 'B' | 'C' =>
                        RETURN Windows_1_4_B_System;
                      WHEN OTHERS =>
                        RETURN Windows_1_4_A_System;
                    END CASE;
                  WHEN 10 =>
                    CASE Version_Information.Service_Pack(2) IS
                      WHEN 'A' =>
                        RETURN Windows_1_4_10_B_System;
                      WHEN OTHERS =>
                        RETURN Windows_1_4_10_A_System;
                    END CASE;
                  WHEN 90 =>
                    RETURN Windows_1_4_90_System;
                  WHEN OTHERS =>
                    NULL;
                END CASE;
              WHEN OTHERS =>
                NULL;
            END CASE;
          WHEN 2 =>
            CASE Version_Information.Major IS
              WHEN 0..4 =>
                RETURN Windows_2_System;
              WHEN 5 =>
                CASE Version_Information.Minor IS
                  WHEN 0 =>
                    RETURN Windows_2_5_System;
                  WHEN 1 =>
                    RETURN Windows_2_5_1_System;
                  WHEN OTHERS =>
                    NULL;
                END CASE;
              WHEN 6 =>
                CASE Version_Information.Minor IS
                  WHEN 0 =>
                    RETURN Windows_2_6_System;
                  WHEN 1 =>
                    RETURN Windows_2_6_1_System;
                  WHEN 2 =>
                    RETURN Windows_2_6_2_System;
                  WHEN OTHERS =>
                    NULL;
                END CASE;
              WHEN OTHERS =>
                NULL;
            END CASE;
          WHEN OTHERS =>
            NULL;
        END CASE;
        RETURN Windows_System;
      END Get_Version;
  ------------------
  -- Get_Language --
  ------------------
    FUNCTION Get_Language
      RETURN Enumerated_Language
      IS
      BEGIN
        CASE Get_System_Default_Language IS
          WHEN LANGUAGE_ARABIC =>
            RETURN Arabic_Language;
          WHEN LANGUAGE_BASQUE =>
            RETURN Basque_Language;
          WHEN LANGUAGE_CATALAN =>
            RETURN Catalan_Language;
          WHEN LANGUAGE_CHINESE_SIMPLIFIED =>
            RETURN Simplified_Chinese_Language;
          WHEN LANGUAGE_CHINESE_TRADITIONAL =>
            RETURN Traditional_Chinese_Language;
          WHEN LANGUAGE_CZECH =>
            RETURN Czech_Language;
          WHEN LANGUAGE_DANISH =>
            RETURN Danish_Language;
          WHEN LANGUAGE_DUTCH =>
            RETURN Dutch_Language;
          WHEN LANGUAGE_ENGLISH =>
            RETURN English_Language;
          WHEN LANGUAGE_FINNISH =>
            RETURN Finnish_Langauge;
          WHEN LANGUAGE_FRENCH =>
            RETURN French_Langauge;
          WHEN LANGUAGE_GERMAN =>
            RETURN German_Language;
          WHEN LANGUAGE_GREEK =>
            RETURN Greek_Language;
          WHEN LANGUAGE_HEBREW =>
            RETURN Hebrew_Language;
          WHEN LANGUAGE_HUNGARIAN =>
            RETURN Hungarian_Language;
          WHEN LANGUAGE_ITALIAN =>
            RETURN Italian_Language;
          WHEN LANGUAGE_JAPANESE =>
            RETURN Japanese_Language;
          WHEN LANGUAGE_KOREAN =>
            RETURN Korean_Language;
          WHEN LANGUAGE_NORWEGIAN =>
            RETURN Norwegian_Language;
          WHEN LANGUAGE_POLISH =>
            RETURN Polish_Language;
          WHEN LANGUAGE_PORTUGUESE =>
            RETURN Portuguese_Language;
          WHEN LANGUAGE_PORTUGUESE_BRAZIL =>
            RETURN Brazilian_Portuguese_Language;
          WHEN LANGUAGE_RUSSIAN =>
            RETURN Russian_Language;
          WHEN LANGUAGE_SLOVAKIAN =>
            RETURN Slovakian_Language;
          WHEN LANGUAGE_SLOVENIAN =>
            RETURN Slovenian_Language;
          WHEN LANGUAGE_SPANISH =>
            RETURN Spanish_Language;
          WHEN LANGUAGE_SWEDISH =>
            RETURN Swedish_Language;
          WHEN LANGUAGE_TURKISH =>
            RETURN Turkish_Language;
          WHEN OTHERS =>
            RETURN English_Language;
        END CASE;
      END Get_Language;
  ------------------
  -- Get_Username --
  ------------------
    FUNCTION Get_Username
      RETURN String_2
      IS
      Username : String_2(1..STRING_BUFFER_SIZE_FOR_GET_USERNAME) := (OTHERS => ' ');
      Size     : Integer_4_Signed_C                               := Username'Size / 8;
      BEGIN
        if Get_User_Name(Username'Address, Size'Address) = FAILED THEN
          RAISE System_Call_Failure;
        END if;
        if Username(1) = ' ' THEN
          RAISE System_Call_Failure;
        END if;
        Username(Integer_4_Signed(Size)) := ' ';
        RETURN Trim(Username, Both);
      END Get_Username;
  ------------------
  -- Open_Webpage --
  ------------------
    PROCEDURE Open_Webpage(
      Path : IN String_2)
      IS
      Window : Address := NULL_ADDRESS;
      BEGIN
        NULL;
        -- if
        -- Shell_Execute(
        --   Window       => NULL_ADDRESS,
        --   Operation    => To_Access_Constant_Character_2_C("open"),
        --   File         => To_Access_Constant_Character_2_C(Path),
        --   Directory    => NULL,
        --   Parameters   => NULL,
        --   Show_Command => MAKE_WINDOW_RESTORE) < To_Address(32)
        -- THEN
        --   Put_Line("  Open_Webpage: Could not open " & Path);
        --   RETURN;
        -- END if;
        -- Put_Line("  Open_Webpage: " & Path);
        -- Window := Get_Foreground_Window;
        -- if Window /= NULL_ADDRESS and THEN Show_Window(Window, MAKE_WINDOW_FULLSCREEN) = FAILED THEN
        --   NULL; -- RAISE System_Call_Failure;
        -- END if;
      END Open_Webpage;
  -------------------------
  -- Execute_Application --
  -------------------------
    PROCEDURE Execute_Application(
      Do_Quit         : IN Boolean;
      Executable_Path : IN String_2)
      IS
      BEGIN
        NULL;
        --   if
        --   Create_Process(
        --      => NULL_ADDRESS,
        --      => Path_Orgin,
        --      => NULL_ADDRESS,
        --      => NULL_ADDRESS,
        --      => C_FALSE,
        --      => 0,
        --      => NULL_ADDRESS,
        --      => NULL_ADDRESS,
        --      => Startup_Information'Address,
        --      => Process_Information'Address) = FAILED
        --   THEN
        -- Executable_Name : IN String_2;
        -- Do_Quit         : IN Boolean)
        -- IS
        -- TCHAR       szPathOrig[_MAX_PATH];
        -- STARTUPINFO     si;
        -- PROCESS_INFORMATION pi;
        -- BEGIN
        --   ZeroMemory( &si, sizeof(si) );
        --   si.cb = sizeof(si);
        --   strncpy( szPathOrig, exePath, _MAX_PATH );
        --   if( !CreateProcess( NULL, szPathOrig, NULL, NULL, FALSE, 0, NULL, NULL, &si, &pi ) ) {
        --         common->Error( "Could not start process: '%s' ", szPathOrig );
        --       RETURN;
        --   }
        --   if ( doexit ) {
        --     cmdSystem->BufferCommandText( CMD_EXEC_APPEND, "quit\n" );
        --   }
      END Execute_Application;
  --------------------------------------
  -- Is_Running_In_64_Bit_Environment --
  --------------------------------------
    FUNCTION Is_Running_In_64_Bit_Environment
      RETURN Boolean
      IS
      BEGIN
      END Is_Running_In_64_Bit_Environment;
  END Implementation;

