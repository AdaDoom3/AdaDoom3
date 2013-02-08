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
  ----------
  -- Test --
  ----------
    procedure Test
      is
      begin
        Display_Title("SYSTEM TEST");
        Put;
        Hang_Window;
      end Test;
  ---------
  -- Put --
  ---------
    procedure Put
      is
      System : Record_System := Get;
      begin
        Put_Line("Username: " & System.Username.All);
        Put_Line("Version: "  & System.Version'Image);
        Put_Line("Language: " & System.Language'Image);
      end Put;
  ---------
  -- Get --
  ---------
    function Get
      return Record_System
      renames Implementation.Get;
  ------------------
  -- Open_Webpage --
  ------------------
    procedure Open_Webpage(
      Path : in String_2)
      renames Implementation.Open_Webpage;
  -------------
  -- Process --
  -------------
    procedure Process(
      Do_Quit         : in Boolean;
      Executable_Path : in String_2)
      renames Implementation.Process;
  -------------------
  -- Is_Newer_Than --
  -------------------
    function Is_Newer_Than(
      Version : in Enumerated_Operating_System_Version)
      return Boolean
      is
      begin
        
      end Is_Newer_Than;
    function Is_Newer_Than(
      Possible_Version : in String_2)
      return Boolean
      is
      begin
        for Version in Enumerated_Operating_System_Version loop
          if Version'Image = Possible_Version then
            exit;
          end if;
          if Version = Enumerated_Operating_System_Version'Last loop
            raise Invalid_Compatibility_Check;
          end loop;
        end loop;
        return Is_Newer_Than(Version);
      end Is_Newer_Than;
  end Neo.System;
