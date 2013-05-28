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
  Interfaces.C,
  Neo.Foundation.Data_Types;
use
  Interfaces.C,
  Neo.Foundation.Data_Types;
package Neo.Posix
  is
  ---------------
  -- Constants --
  ---------------
    SUCCESS                     : constant Integer_4_Signed_C := 0;
    NUMBER_OF_PROCESSORS_ONLINE : constant Integer_4_Signed_C := 84;
    UNIX_NAME_LINUX             : constant String_1_C         := "Linux";
    UNIX_NAME_MACINTOSH         : constant String_1_C         := "Darwin";
  -------------
  -- Records --
  -------------
    type Record_Unix_Name
      is record
        System  : String_1_C(1..65) := (others => NULL_CHARACTER_1_C);
        Node    : String_1_C(1..65) := (others => NULL_CHARACTER_1_C);
        Release : String_1_C(1..65) := (others => NULL_CHARACTER_1_C);
        Version : String_1_C(1..65) := (others => NULL_CHARACTER_1_C);
        Machine : String_1_C(1..65) := (others => NULL_CHARACTER_1_C);
        Domain  : String_1_C(1..65) := (others => NULL_CHARACTER_1_C);
      end record
      with Size => 6 * 65 * Byte'Size;
      pragma Convention(C, Record_Unix_Name);
    type Record_Time_Stamp
      is record
        User            : Integer_8_Unsigned_C := 0; --clock_t
        System          : Integer_8_Unsigned_C := 0;
        Children_User   : Integer_8_Unsigned_C := 0;
        Children_System : Integer_8_Unsigned_C := 0;
      end record;
      pragma Convention(C, Record_Time_Stamp);
  -----------------
  -- Subprograms --
  -----------------
    function Get_Unix_Name(
      Buffer : access Record_Unix_Name)
      return Integer_4_Signed_C;
    function Get_System_Configuration(
      Name : Integer_4_Signed_C)
      return Long;
    function Get_Time_Stamp(
      Buffer : access Record_Time_Stamp)
      return Integer_8_Unsigned_C;
-------
private
-------
  ----------------
  -- Directives --
  ----------------
    pragma Import(C, Get_System_Configuration, "sysconf");
    pragma Import(C, Get_Unix_Name,            "uname");
    pragma Import(C, Get_Time_Stamp,           "times");
  end Neo.Posix;
