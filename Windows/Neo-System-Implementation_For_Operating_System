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
  Neo.Windows;
use
  Neo.Windows;
separate(Neo.System)
package body Implementation_For_Operating_System
  is
  ---------------
  -- Give_Test --
  ---------------
    procedure Give_Test
      is
      Memory : Record_Memory := NULL_RECORD_MEMORY;
      begin
        Display_Title("SYSTEM TEST");
        case Get_Default_Language is
          when Arabic_Language =>
            Put_Line(TAB & "Arabic");
          when Basque_Language =>
            Put_Line(TAB & "Basque");
          when Catalan_Language =>
            Put_Line(TAB & "Catalan");
          when Simplified_Chinese_Language =>
            Put_Line(TAB & "Simplified Chinese");
          when Traditional_Chinese_Language =>
            Put_Line(TAB & "Traditional Chinese");
          when Czech_Language =>
            Put_Line(TAB & "Czech");
          when Danish_Language =>
            Put_Line(TAB & "Danish");
          when Dutch_Language =>
            Put_Line(TAB & "Dutch");
          when English_Language =>
            Put_Line(TAB & "English");
          when Finnish_Langauge =>
            Put_Line(TAB & "Finnish");
          when French_Langauge =>
            Put_Line(TAB & "French");
          when German_Language =>
            Put_Line(TAB & "German");
          when Greek_Language =>
            Put_Line(TAB & "Greek");
          when Hebrew_Language =>
            Put_Line(TAB & "Hebrew");
          when Hungarian_Language =>
            Put_Line(TAB & "Hungarian");
          when Italian_Language =>
            Put_Line(TAB & "Italian");
          when Japanese_Language =>
            Put_Line(TAB & "Japanese");
          when Korean_Language =>
            Put_Line(TAB & "Korean");
          when Norwegian_Language =>
            Put_Line(TAB & "Norwegian");
          when Polish_Language =>
            Put_Line(TAB & "Polish");
          when Portuguese_Language =>
            Put_Line(TAB & "Portuguese");
          when Brazilian_Portuguese_Language =>
            Put_Line(TAB & "Brazilian Portuguese");
          when Russian_Language =>
            Put_Line(TAB & "Russian");
          when Slovakian_Language =>
            Put_Line(TAB & "Slovakian");
          when Slovenian_Language =>
            Put_Line(TAB & "Slovenian");
          when Spanish_Language =>
            Put_Line(TAB & "Spanish");
          when Swedish_Language =>
            Put_Line(TAB & "Swedish");
          when Turkish_Language =>
            Put_Line(TAB & "Turkish");
        end case;
        Memory := Get_Memory_Status;
        Text_IO.Put_Line.All(TAB & "Load"                       & Float_Percent'Wide_Image(Memory.Load));
        Text_IO.Put_Line.All(TAB & "Physical_Total"             & Integer_8_Natural'Wide_Image(Memory.Physical_Total));
        Text_IO.Put_Line.All(TAB & "Physical_Available"         & Integer_8_Natural'Wide_Image(Memory.Physical_Available));
        Text_IO.Put_Line.All(TAB & "Page_File_Total"            & Integer_8_Natural'Wide_Image(Memory.Page_File_Total));
        Text_IO.Put_Line.All(TAB & "Page_File_Available"        & Integer_8_Natural'Wide_Image(Memory.Page_File_Available));
        Text_IO.Put_Line.All(TAB & "Virtual_Total"              & Integer_8_Natural'Wide_Image(Memory.Virtual_Total));
        Text_IO.Put_Line.All(TAB & "Virtual_Available"          & Integer_8_Natural'Wide_Image(Memory.Virtual_Available));
        Text_IO.Put_Line.All(TAB & "Virtual_Available_Extended" & Integer_8_Natural'Wide_Image(Memory.Virtual_Available_Extended));
        Text_IO.Put_Line.All(TAB & Get_Clipboard_Data);
        Set_Clipboard_Data("The quick brown fox jumps over the lazy dog.");
        --Open_Webpage("http://www.google.com");
        -- Cause an error
        --Put_Line(Get_Last_Error);
        while Is_Okay("test", "Would you like to continue an infinite loop?", Yes_No_Buttons, Error_Icon) loop
          null;
        end loop;
      end Give_Test;
  ------------------
  -- Get_Username --
  ------------------
    function Get_Username
      return String_2
      is
      Username : String_2(1..1024)  := (others => ' ');
      Size     : Integer_4_Signed_C := Username'Size / 8;
      begin
        if Get_User_Name(Username'Address, Size'Address) = FAILED then
          raise System_Call_Failure;
        end if;
        if Username(1) = ' ' then
          raise System_Call_Failure;
        end if;
        Username(Size - 1) := ' ';
        return Trim(Username, Both);
      end Get_Username;
  -----------
  -- Sleep --
  -----------
    procedure Sleep(
      Milliseconds : in Integer_4_Positive)
      is
      begin
        if Sleep(Integer_4_Unsigned_C(Milliseconds), C_FALSE) then
          raise System_Call_Failure;
        end if;
      end Sleep;
  --------------------------
  -- Get_Operating_System --
  --------------------------
    function Get_Operating_System
      return Enumerated_Operating_System
      is
      begin
        return Microsoft_System;
      end Get_Operating_System;
  -------------
  -- Is_Okay --
  -------------
    function Is_Okay(
      Title        : in String_2;
      Message      : in String_2;
      Buttons      : in Enumerated_Buttons;
      Icon         : in Enumerated_Icon;
      Parent_Title : in String_2 := NULL_STRING_2)
      return Boolean
      is
      User_Interaction : Integer_4_Signed_C   := 0;
      Kind             : Integer_4_Unsigned_C := 0;
      begin
        case Buttons is
          when Okay_Button =>
            Kind := BUTTON_OKAY;
          when Yes_No_Buttons =>
            Kind := BUTTONS_YES_NO;
          when Okay_Cancel_Buttons =>
            Kind := BUTTONS_CANCEL_OKAY;
          when Retry_Cancel_Buttons =>
            Kind := BUTTONS_CANCEL_RETRY;
        end case;
        case Icon is
          when Warning_Icon =>
            Kind := Kind or ICON_WARNING;
          when Information_Icon =>
            Kind := Kind or ICON_INFORMATION;
          when Error_Icon =>
            Kind := Kind or ICON_ERROR;
          when No_Icon =>
            null;
        end case;
        User_Interaction :=
          Message_Box(
            Window  => Find_Window(To_String_2_C(Parent_Title), NULL_ADDRESS),
            Caption => To_String_2_C(Title),
            Text    => To_String_2_C(Message),
            Kind    => Kind); 
        if
        User_Interaction = PRESSED_OKAY  or
        User_Interaction = PRESSED_RETRY or
        User_Interaction = PRESSED_YES
        then
          return True;
        end if;
        return False;
      end Is_Okay;
  ---------
  -- Get --
  ---------
    function Get
      return 
      is
      Version_Information : Record_Version_Information := NULL_RECORD_VERSION_INFORMATION;
      begin
        if Get_Version(Version_Information'Address) = FAILED then
          raise System_Call_Failure;
        end if;
        case Version_Information.Platform_Identifier is
          when 1 =>
            case Version_Information.Major is
              when 4 =>
                case Version_Information.Minor is
                  when 0 =>
                    case Version_Information.Service_Pack(2) is
                      when 'B' | 'C'
                        return Windows_1_4_B_Version;
                      when others =>
                        return Windows_1_4_A_Version;
                    end case;
                  when 10 =>
                    case Version_Information.Service_Pack(2) is
                      when 'A' =>
                        return Windows_1_4_10_B_Version;
                      when others =>
                        return Windows_1_4_10_A_Version;
                    end case;
                  when 90 =>
                    return Windows_1_4_90_Version;
                  when others =>
                    null;
                end case;
              when others =>
                null;
            end case;
          when 2 =>
            case Version_Information.Major is
              when 0..4 =>
                return Windows_2_Version;
              when 5 =>
                case Version_Information.Minor is
                  when 0 =>
                    return Windows_2_5_Version;
                  when 1 =>
                    return Windows_2_5_1_Version;
                  when others =>
                    null;
                end case;
              when 6 =>
                case Version_Information.Minor is
                  when 0 =>
                    return Windows_2_6_Version;
                  when 1 =>
                    return Windows_2_6_1_Version;
                  when 2 =>
                    return Windows_2_6_2_Version;
                  when others =>
                    null;
                end case;
              when others =>
                null;
            end case;
          when others =>
            null;
        end case;
        return Windows_Version;
        case Get_System_Default_Language is
          when LANGUAGE_ARABIC =>
            return Arabic_Language;
          when LANGUAGE_BASQUE =>
            return Basque_Language;
          when LANGUAGE_CATALAN =>
            return Catalan_Language;
          when LANGUAGE_CHINESE_SIMPLIFIED =>
            return Simplified_Chinese_Language;
          when LANGUAGE_CHINESE_TRADITIONAL =>
            return Traditional_Chinese_Language;
          when LANGUAGE_CZECH =>
            return Czech_Language;
          when LANGUAGE_DANISH =>
            return Danish_Language;
          when LANGUAGE_DUTCH =>
            return Dutch_Language;
          when LANGUAGE_ENGLISH =>
            return English_Language;
          when LANGUAGE_FINNISH =>
            return Finnish_Langauge;
          when LANGUAGE_FRENCH =>
            return French_Langauge;
          when LANGUAGE_GERMAN =>
            return German_Language;
          when LANGUAGE_GREEK =>
            return Greek_Language;
          when LANGUAGE_HEBREW =>
            return Hebrew_Language;
          when LANGUAGE_HUNGARIAN =>
            return Hungarian_Language;
          when LANGUAGE_ITALIAN =>
            return Italian_Language;
          when LANGUAGE_JAPANESE =>
            return Japanese_Language;
          when LANGUAGE_KOREAN =>
            return Korean_Language;
          when LANGUAGE_NORWEGIAN =>
            return Norwegian_Language;
          when LANGUAGE_POLISH =>
            return Polish_Language;
          when LANGUAGE_PORTUGUESE =>
            return Portuguese_Language;
          when LANGUAGE_PORTUGUESE_BRAZIL =>
            return Brazilian_Portuguese_Language;
          when LANGUAGE_RUSSIAN =>
            return Russian_Language;
          when LANGUAGE_SLOVAKIAN =>
            return Slovakian_Language;
          when LANGUAGE_SLOVENIAN =>
            return Slovenian_Language;
          when LANGUAGE_SPANISH =>
            return Spanish_Language;
          when LANGUAGE_SWEDISH =>
            return Swedish_Language;
          when LANGUAGE_TURKISH =>
            return Turkish_Language;
          when others =>
            return English_Language;
        end case;
      end Get;
  ------------------------
  -- Get_File_Seporator --
  ------------------------
    function Get_File_Seporator
      return String_2
      is
      begin
        return "\";
      end Get_File_Seporator;
  ---------------------
  -- Get_System_Path --
  ---------------------
    function Get_System_Path
      return String_2
      is
      begin
        return "C:\";
      end Get_System_Path;
  ------------------
  -- Open_Webpage --
  ------------------
    procedure Open_Webpage( -- This doesn't work
      Path : in String_2)
      is
      Window : Address := NULL_ADDRESS;
      begin
        null;
        -- if
        -- Shell_Execute(
        --   Window       => NULL_ADDRESS,
        --   Operation    => To_Access_Constant_Character_2_C("open"),
        --   File         => To_Access_Constant_Character_2_C(Path),
        --   Directory    => null,
        --   Parameters   => null,
        --   Show_Command => MAKE_WINDOW_RESTORE) < To_Address(32)
        -- then
        --   Put_Line("  Open_Webpage: Could not open " & Path);
        --   return;
        -- end if;
        -- Put_Line("  Open_Webpage: " & Path);
        -- Window := Get_Foreground_Window;
        -- if Window /= NULL_ADDRESS and then Show_Window(Window, MAKE_WINDOW_FULLSCREEN) = FAILED then
        --   null; -- raise System_Call_Failure;
        -- end if;
      end Open_Webpage;

  -------------------
  -- Start_Process --
  -------------------
    procedure Start_Process(
      Do_Quit         : in Boolean;
      Executable_Name : in String_2;
      Executable_Path : in String_2 := NULL_STRING_2)
      is
      begin
        if
        Create_Process(
           => NULL_ADDRESS,
           => Path_Orgin,
           => NULL_ADDRESS,
           => NULL_ADDRESS,
           => C_FALSE,
           => 0,
           => NULL_ADDRESS,
           => NULL_ADDRESS,
           => Startup_Information'Address,
           => Process_Information'Address) = FAILED
        then
  --     Executable_Name : in String_2;
  --     Do_Quit         : in Boolean)
  --     is
  --     TCHAR       szPathOrig[_MAX_PATH];
  --     STARTUPINFO     si;
  --     PROCESS_INFORMATION pi;
  --     begin
  --       ZeroMemory( &si, sizeof(si) );
  --       si.cb = sizeof(si);
  --       strncpy( szPathOrig, exePath, _MAX_PATH );
  --       if( !CreateProcess( NULL, szPathOrig, NULL, NULL, FALSE, 0, NULL, NULL, &si, &pi ) ) {
  --             common->Error( "Could not start process: '%s' ", szPathOrig );
  --           return;
  --       }
  --       if ( doexit ) {
  --         cmdSystem->BufferCommandText( CMD_EXEC_APPEND, "quit\n" );
  --       }
  --     end Start_Process;
      end Start_Process;
  ----------------
  -- Stop_Alert --
  ----------------
    procedure Stop_Alert
      is
      begin
        null;
      end Stop_Alert;
  -----------------
  -- Start_Alert --
  -----------------
    procedure Start_Alert
      is
      begin
        null;
      end Start_Alert;
  end Implementation_For_Operating_System;
