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
  Neo.Windows;
use
  System,
  Interfaces.C,
  Ada.Strings,
  Ada.Strings.Wide_Fixed,
  Neo.Windows;
separate(Neo.System)
package body Implementation
  is
  ---------------
  -- Constants --
  ---------------
    STRING_BUFFER_SIZE_FOR_GET_USERNAME : constant Integer_4_Signed := 1024;
  -----------------
  -- Get_Version --
  -----------------
    function Get_Version
      return Enumerated_System
      is
      Version_Information : Record_Version_Information := (others => <>);
      begin
        if Get_Version(Version_Information'Address) = FAILED then
          Put_Line(Integer_4_Unsigned_C'Wide_Image(Get_Last_Error));
          raise System_Call_Failure;
        end if;
        case Version_Information.Platform_Identifier is
          when 1 =>
            case Version_Information.Major is
              when 4 =>
                case Version_Information.Minor is
                  when 0 =>
                    case Version_Information.Service_Pack(2) is
                      when 'B' | 'C' =>
                        return Windows_1_4_B_System;
                      when others =>
                        return Windows_1_4_A_System;
                    end case;
                  when 10 =>
                    case Version_Information.Service_Pack(2) is
                      when 'A' =>
                        return Windows_1_4_10_B_System;
                      when others =>
                        return Windows_1_4_10_A_System;
                    end case;
                  when 90 =>
                    return Windows_1_4_90_System;
                  when others =>
                    null;
                end case;
              when others =>
                null;
            end case;
          when 2 =>
            case Version_Information.Major is
              when 0..4 =>
                return Windows_2_System;
              when 5 =>
                case Version_Information.Minor is
                  when 0 =>
                    return Windows_2_5_System;
                  when 1 =>
                    return Windows_2_5_1_System;
                  when others =>
                    null;
                end case;
              when 6 =>
                case Version_Information.Minor is
                  when 0 =>
                    return Windows_2_6_System;
                  when 1 =>
                    return Windows_2_6_1_System;
                  when 2 =>
                    return Windows_2_6_2_System;
                  when others =>
                    null;
                end case;
              when others =>
                null;
            end case;
          when others =>
            null;
        end case;
        return Windows_System;
      end Get_Version;
  ------------------
  -- Get_Username --
  ------------------
    function Get_Username
      return String_2
      is
      Username : String_2(1..STRING_BUFFER_SIZE_FOR_GET_USERNAME) := (others => ' ');
      Size     : Integer_4_Signed_C                               := Username'Size / 8;
      begin
        if Get_User_Name(Username'Address, Size'Address) = FAILED then
          raise System_Call_Failure;
        end if;
        if Username(1) = ' ' then
          raise System_Call_Failure;
        end if;
        Username(Integer_4_Signed(Size)) := ' ';
        return Trim(Username, Both);
      end Get_Username;
  ------------------
  -- Open_Webpage --
  ------------------
    procedure Open_Webpage(
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
  -------------------------
  -- Execute_Application --
  -------------------------
    procedure Execute_Application(
      Do_Quit         : in Boolean;
      Executable_Path : in String_2)
      is
      begin
        null;
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
        --   then
        -- Executable_Name : in String_2;
        -- Do_Quit         : in Boolean)
        -- is
        -- TCHAR       szPathOrig[_MAX_PATH];
        -- STARTUPINFO     si;
        -- PROCESS_INFORMATION pi;
        -- begin
        --   ZeroMemory( &si, sizeof(si) );
        --   si.cb = sizeof(si);
        --   strncpy( szPathOrig, exePath, _MAX_PATH );
        --   if( !CreateProcess( null, szPathOrig, null, null, FALSE, 0, null, null, &si, &pi ) ) {
        --         common->Error( "Could not start process: '%s' ", szPathOrig );
        --       return;
        --   }
        --   if ( doexit ) {
        --     cmdSystem->BufferCommandText( CMD_EXEC_APPEND, "quit\n" );
        --   }
      end Execute_Application;
  -----------------------------------------------
  -- Is_Running_In_Emulated_32_Bit_Environment --
  -----------------------------------------------
    function Is_Running_In_Emulated_32_Bit_Environment
      return Boolean
      is
      begin
        return False;
      end Is_Running_In_Emulated_32_Bit_Environment;
  end Implementation;

