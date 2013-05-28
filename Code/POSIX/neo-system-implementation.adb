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
  Neo.POSIX;
use
  System,
  Interfaces.C,
  Ada.Strings,
  Ada.Strings.Wide_Fixed,
  Neo.POSIX;
separate(Neo.System)
package body Implementation
  is
  -----------------
  -- Get_Version --
  -----------------
    function Get_Version
      return Enumerated_System
      is
      Name : aliased Record_Unix_Name := (others => <>);
      begin
        if Get_Unix_Name(Name'access) /= SUCCESS then
          raise System_Call_Failure;
        end if;
        if
        Name.System(
          Name.System'First..
          Name.System'First + UNIX_NAME_LINUX'Length) = UNIX_NAME_LINUX
        then
          case Name.Version(Name.Version'First) is
            when '2' =>
              case Name.Version(Name.Version'First + 2) is
                when '4'    => return Linux_2_4_System;
                when '5'    => return Linux_2_5_System;
                when '6'    => return Linux_2_6_System;
                when others => return Linux_2_System;
               end case;
            when '3' =>
              case Name.Version(Name.Version'First + 2) is
                when '1'    => return Linux_3_1_System;
                when '2'    => return Linux_3_2_System;
                when '3'    => return Linux_3_3_System;
                when '4'    => return Linux_3_4_System;
                when '5'    => return Linux_3_5_System;
                when '6'    => return Linux_3_6_System;
                when '7'    => return Linux_3_7_System;
                when '8'    => return Linux_3_8_System;
                when '9'    => return Linux_3_9_System;
                when others => return Linux_3_System;
              end case;
            when others =>
              return Linux_System;
          end case;
        elsif
        Name.System(
          Name.System'First..
          Name.System'First + UNIX_NAME_MACINTOSH'Length) = UNIX_NAME_MACINTOSH
        then
          case Name.Version(Name.Version'First) is
            when '8' =>
              case Name.Version(Name.Version'First + 2) is
                when '5'    => return Macintosh_8_5_System;
                when '6'    => return Macintosh_8_6_System;
                when others => return Macintosh_8_System;
              end case;
            when '9' =>
              case Name.Version(Name.Version'First + 4) is
                when '1'    => return Macintosh_9_1_System;
                when '2'    => return Macintosh_9_2_System;
                when others => return Macintosh_9_System;
              end case;
            when '1' =>
              if Name.Version(Name.Version'First + 1) = '0' then
                case Name.Version(Name.Version'First + 3) is
                  when '1'    => return Macintosh_10_1_System;
                  when '2'    => return Macintosh_10_2_System;
                  when '3'    => return Macintosh_10_3_System;
                  when '4'    => return Macintosh_10_4_System;
                  when '5'    => return Macintosh_10_5_System;
                  when '6'    => return Macintosh_10_6_System;
                  when '7'    => return Macintosh_10_7_System;
                  when '8'    => return Macintosh_10_8_System;
                  when others => return Macintosh_10_System;
                end case;
              end if;
            when others =>
              return Macintosh_System;
          end case;
        else
          raise System_Call_Failure;
        end if;
      end Get_Version;
  ------------------
  -- Get_Username --
  ------------------
    function Get_Username
      return String_2
      is
      --Password : access Record_Password := Get_Password(Get_User);--getpwuid(geteuid); -- struct passwd
      begin
      --  if Password = null then
      --    raise System_Call_Failure;
      --  end if;
      --  return Trim(Password.Name, Both);
        raise Unsupported_Feature;
        return DEFAULT_USERNAME;
      end Get_Username;
  ------------------
  -- Open_Webpage --
  ------------------
    procedure Open_Webpage(
      Path : in String_2)
      is
      begin
        raise Unsupported_Feature;
      end Open_Webpage;
  -------------------------
  -- Execute_Application --
  -------------------------
    procedure Execute_Application(
      Executable_Path : in String_2;
      Do_Fullscreen   : in Boolean)
      is
      begin
        raise Unsupported_Feature;
      end Execute_Application;
  ------------------
  -- Get_Bit_Size --
  ------------------
    function Get_Bit_Size
      return Integer_4_Positive
      is
      begin
        raise System_Call_Failure;
        return WORD_SIZE;
      end Get_Bit_Size;
  end Implementation;
