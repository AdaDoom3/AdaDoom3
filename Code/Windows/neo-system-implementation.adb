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
  ----------------
  -- Exceptions --
  ----------------
    Executable_Path_Exceeds_Maximum_Length : Exception;
  -----------------
  -- Get_Version --
  -----------------
    function Get_Version
      return Enumerated_System
      is
      Version_Information : aliased Record_Version_Information := (others => <>);
      begin
        if Get_Version(Version_Information'unchecked_access) = FAILED then
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
      Length : aliased Integer_4_Signed_C := 0;
      begin
        if
        Get_Username(null, Length'unchecked_access) = FAILED and then
        Get_Last_Error /= ERROR_INSUFFICIENT_BUFFER
        then
          raise System_Call_Failure;
        end if;
        ---------------------------------
        Create_Username_Buffer_And_Fetch:
        ---------------------------------
          declare
          Username : aliased Access_String_2_C  := new String_2_C(1..Integer_Size_C(Length));
          begin
            Length       := Username.all'size / Byte'size;
      	    Username.all := (others => ' ');
            if Get_Username(Username, Length'unchecked_access) = FAILED then
              raise System_Call_Failure;
            end if;
            if Username(1) = ' ' then
              raise System_Call_Failure;
            end if;
            return To_String_2(Username.all);
          end Create_Username_Buffer_And_Fetch;
      end Get_Username;
  ------------------
  -- Open_Webpage --
  ------------------
    procedure Open_Webpage(
      Path : in String_2)
      is
      Window : Address := NULL_ADDRESS;
      begin
        raise System_Call_Failure;
        --if
        --Shell_Execute( -- Fails with error code 2: ERROR_FILE_NOT_FOUND!
        --  Window       => NULL_ADDRESS,
        --  Operation    => To_Access_Constant_Character_2_C("open"),
        --  File         => To_Access_Constant_Character_2_C(Path),
        --  Directory    => null,
        --  Parameters   => null,
        --  Show_Command => MAKE_WINDOW_RESTORE) <= 32
        --then
        --  Put_Last_Error;
        --  raise System_Call_Failure;
        --end if;
        --Window := Get_Foreground_Window;
        --if Window /= NULL_ADDRESS and then Show_Window(Window, MAKE_WINDOW_FULLSCREEN) = FAILED then
        --  raise System_Call_Failure;
        --end if;
      end Open_Webpage;
  -------------------------
  -- Execute_Application --
  -------------------------
    procedure Execute_Application(
      Executable_Path : in String_2;
      Do_Quit         : in Boolean)
      is
      Startup_Information : aliased Record_Startup_Information := (others => <>);
      Process_Information : aliased Record_Process_Information := (others => <>);
      begin
        if Executable_Path'length > MAXIMUM_PATH_LENGTH - 1 then
          raise Executable_Path_Exceeds_Maximum_Length;
        end if;
        if
        Create_Process(
          Application_Name    => null,
          Command_Line        => To_Access_Character_2_C(Executable_Path),
          Process_Attributes  => null,
          Thread_Attributes   => null,
          Inherit_Handles     => C_FALSE,
          Creation_Flags      => 0,
          Environment         => NULL_ADDRESS,
          Current_Directory   => null,
          Startup_Information => Startup_Information'unchecked_access,
          Process_Information => Process_Information'unchecked_access) = FAILED
        then
          raise System_Call_Failure;
        end if;
        --if Do_Quit then
        --  cmdSystem->BufferCommandText(CMD_EXEC_APPEND, "quit" & END_LINE);
        --end if;
      end Execute_Application;
  ------------------
  -- Get_Bit_Size --
  ------------------
    function Get_Bit_Size
      return Integer_4_Positive
      is
      Result : aliased Integer_4_Signed_C := 0;
      begin
        if WORD_SIZE = 64 then
          return 64;
        elsif Is_Running_In_Emulated_32_Bit(Get_Current_Process, Result'unchecked_access) = FAILED then
          raise System_Call_Failure;
        end if;
        return(
          if Result = C_TRUE then
            64
          else
            32);
      end Get_Bit_Size;
  end Implementation;

