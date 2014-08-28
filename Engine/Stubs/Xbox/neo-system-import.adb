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
  Neo.Archetype;
use
  Neo.Archetype;
separate(Neo.System)
package body Implementation
  is
  ---------------------------
  -- Get_Last_Error_Number --
  ---------------------------
    function Get_Last_Error_Number
      return Integer_4_Unsigned
      is
      begin
        raise Unimplemented_Feature;
      end Get_Last_Error_Number;
  -----------------
  -- Get_Version --
  -----------------
    function Get_Version
      return Enumerated_System
      is
      begin
        raise Unimplemented_Feature;
      end Get_Version;
  ------------------
  -- Get_Username --
  ------------------
    function Get_Username
      return String_2
      is
      begin
        raise Unimplemented_Feature;
      end Get_Username;
  ------------------
  -- Open_Webpage --
  ------------------
    procedure Open_Webpage(
      Path : in String_2)
      is
      begin
        raise Unimplemented_Feature;
      end Open_Webpage;
  -------------------------
  -- Execute_Application --
  -------------------------
    procedure Execute_Application(
      Executable_Path : in String_2;
      Do_Fullscreen   : in Boolean)
      is
      begin
        raise Unimplemented_Feature;
      end Execute_Application;
  ------------------
  -- Get_Bit_Size --
  ------------------
    function Get_Bit_Size
      return Integer_4_Positive
      is
      begin
        raise Unimplemented_Feature;
      end Get_Bit_Size;
  end Implementation;

