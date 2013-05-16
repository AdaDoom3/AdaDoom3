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
  Neo.Foundation.Data_Types;
use
  Neo.Foundation.Data_Types;
package Neo.Posix
  is
  ---------------
  -- Constants --
  ---------------
    SUCCESS                     : constant Integer_4_Signed_C := 0;
    NUMBER_OF_PROCESSORS_ONLINE : constant Integer_4_Signed_C := 84;
    UNIX_NAME_STRING_SIZE       : constant Integer_4_Signed   := 65;
    UNIX_NAME_LINUX             : constant String_1_C         := "Linux";
    UNIX_NAME_MACINTOSH         : constant String_1_C         := "Darwin";
  -------------
  -- Records --
  -------------
    type Record_Unix_Name
      is record
        System  : String_1_C(1..UNIX_NAME_STRING_SIZE) := (others => NULL_CHARACTER_1);
        Node    : String_1_C(1..UNIX_NAME_STRING_SIZE) := (others => NULL_CHARACTER_1);
        Release : String_1_C(1..UNIX_NAME_STRING_SIZE) := (others => NULL_CHARACTER_1);
        Version : String_1_C(1..UNIX_NAME_STRING_SIZE) := (others => NULL_CHARACTER_1);
        Machine : String_1_C(1..UNIX_NAME_STRING_SIZE) := (others => NULL_CHARACTER_1);
        Domain  : String_1_C(1..UNIX_NAME_STRING_SIZE) := (others => NULL_CHARACTER_1);
      end record;
      --with
        --Convention => C,
      --  Size       => X * Byte'Size;
      for Record_Unix_Name'Size use X * Byte'Size;
      --pragma(C, Record_Unix_Name);
    type Record_Time_Stamp
      is record
        User            : Integer_8_Unsigned_C := 0; --clock_t
        System          : Integer_8_Unsigned_C := 0; 
        Children_User   : Integer_8_Unsigned_C := 0; 
        Children_System : Integer_8_Unsigned_C := 0; 
      end record;
      --pragma(C, Record_Time_Stamp);
      --with Convention => C;
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
    pragma Import(C, Get_Time_Spent,           "times");
  end Neo.Posix;
