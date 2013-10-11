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
  Neo.Link.Windows;
use
  System,
  Interfaces.C,
  Ada.Strings,
  Ada.Strings.Wide_Fixed,
  Neo.Link.Windows;
separate(Neo.System)
package body Import
  is
  ----------------
  -- Exceptions --
  ----------------
    Executable_Path_Exceeds_Maximum_Length : Exception;
  ---------------------------
  -- Get_Last_Error_Number --
  ---------------------------
    function Get_Last_Error_Number
      return Integer_4_Unsigned
      is
      begin
        return Integer_4_Unsigned(Neo.Link.Windows.Get_Last_Error);
      end Get_Last_Error_Number;
  -------------------
  -- Get_Specifics --
  -------------------
    function Get_Specifics
      return Record_Specifics
      is
      Buffer : aliased Integer_4_Signed_C := 0;
      begin
        if
        Get_Username(null, Buffer'unchecked_access) = FAILED and then
        Get_Last_Error /= ERROR_INSUFFICIENT_BUFFER
        then
          raise Call_Failure;
        end if;
        ----------------
        Create_Username:
        ----------------
          declare
          Username            : aliased Access_String_2_C          := new String_2_C(1..Integer_Size_C(Buffer));
          Version_Information : aliased Record_Version_Information := (others => <>);
          begin
            if Get_Username(Username, Buffer'unchecked_access) = FAILED then
              raise Call_Failure;
            end if;
            if Is_Running_In_Emulated_32_Bit(Get_Current_Process, Buffer'unchecked_access) = FAILED then
              raise Call_Failure;
            end if;
            if Get_Version(Version_Information'unchecked_access) = FAILED then
              raise Call_Failure;
            end if;
            return(
              Username => To_String_2_Unbounded(To_String_2(Username.all)),
              Bit_Size => (if Buffer = C_TRUE then 64 else 32),
              Version  =>(
                case Version_Information.Platform_Identifier is
                  when 1 =>(
                    case Version_Information.Major is
                      when 4 =>(
                        case Version_Information.Minor is
                          when 0 =>(
                            case Version_Information.Service_Pack(2) is
                              when 'B' | 'C' =>
                                Windows_1_4_B_System,
                              when others =>
                                Windows_1_4_A_System),
                          when 10 =>(
                            case Version_Information.Service_Pack(2) is
                              when 'A' =>
                                Windows_1_4_10_B_System,
                              when others =>
                                Windows_1_4_10_A_System),
                          when 90 =>
                            Windows_1_4_90_System,
                          when others =>
                            Windows_1_4_System),
                      when others =>
                        Windows_1_System),
                  when 2 =>(
                    case Version_Information.Major is
                      when 5 =>(
                        case Version_Information.Minor is
                          when 1 =>
                            Windows_2_5_1_System,
                          when others =>
                            Windows_2_5_System),
                      when 6 =>(
                        case Version_Information.Minor is
                          when 1 =>
                            Windows_2_6_1_System,
                          when 2 =>
                            Windows_2_6_2_System,
                          when others =>
                            Windows_2_6_System),
                      when others =>
                        Windows_2_System),
                  when others =>
                    Windows_System));
          end Create_Username;
      end Get_Specifics;
  ------------------
  -- Open_Webpage --
  ------------------
    procedure Open_Webpage(
      Path : in String_2)
      is
      begin
        Execute_Application("explorer " & Path, True);
      end Open_Webpage;
  -----------------
  -- Browse_Path --
  -----------------
    function Browse_Path
      return String_2
      is
      begin
        return NULL_STRING_2;
      end Browse_Path;
  -------------------------
  -- Execute_Application --
  -------------------------
    procedure Execute_Application(
      Executable_Path : in String_2;
      Do_Fullscreen   : in Boolean)
      is
      Startup_Information : aliased Record_Startup_Information := (others => <>);
      Process_Information : aliased Record_Process_Information := (others => <>);
      begin
        if Executable_Path'length > MAXIMUM_PATH_LENGTH then
          raise Executable_Path_Exceeds_Maximum_Length;
        end if;
        if Do_Fullscreen then
          Startup_Information.Show_Window := Integer_2_Unsigned_C(MAKE_WINDOW_FULLSCREEN);
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
          raise Call_Failure;
        end if;
      end Execute_Application;
  end Import;

