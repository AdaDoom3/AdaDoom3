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
  Neo.Linux;
use
  System,
  Interfaces.C,
  Ada.Strings,
  Ada.Strings.Wide_Fixed,
  Neo.Linux;
separate(Neo.System)
package body Implementation
  is

  --  Return Linux system enumeration type for given release string.
  function To_Linux_System (
    Release : String)
    return Enumerated_System
  is
     Major  : constant Character := Release (Release'First);
     Minor  : constant Character := Release (Release'First + 2);
     System : Enumerated_System;
  begin
     case Major is
       when '2' =>
         case Minor is
           when '4'    => System := Linux_2_4_System;
           when '5'    => System := Linux_2_5_System;
           when '6'    => System := Linux_2_6_System;
           when others => System := Linux_2_System;
          end case;
       when '3' =>
         case Minor is
           when '0'    => System := Linux_3_0_System;
           when '1'    => System := Linux_3_1_System;
           when '2'    => System := Linux_3_2_System;
           when '3'    => System := Linux_3_3_System;
           when '4'    => System := Linux_3_4_System;
           when '5'    => System := Linux_3_5_System;
           when '6'    => System := Linux_3_6_System;
           when '7'    => System := Linux_3_7_System;
           when '8'    => System := Linux_3_8_System;
           when '9'    => System := Linux_3_9_System;
           when others => System := Linux_3_System;
         end case;
       when others =>
         System := Linux_System;
      end case;
      return System;
    end To_Linux_System;
  -----------------
  -- Get_Version --
  -----------------
    function Get_Version
      return Enumerated_System
      is
        Data   : aliased Utsname_Type;
        Result : Interfaces.C.int;
      begin
        Result := Uname (Buf => Data'Access);
        if Result /= 0 then
          raise System_Call_Failure;
        end if;
        return To_Linux_System (Release => Interfaces.C.To_Ada (Item => Data.Release));
      end Get_Version;
  ------------------
  -- Get_Language --
  ------------------
    function Get_Language
      return Enumerated_Language
      is
      begin
        return English_Language;
      end Get_Language;
  ------------------
  -- Get_Username --
  ------------------
    function Get_Username
      return String_2
      is
      begin
        return "Unimplemented";
      end Get_Username;
  ------------------
  -- Open_Webpage --
  ------------------
    procedure Open_Webpage(
      Path : in String_2)
      is
      begin
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
        null;
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
