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
package Neo.Linux
  is
  ---------------
  -- Constants --
  ---------------
    SC_NPROCESSORS_ONLN : constant Integer_4_Signed_C := 84;
    UTSNAME_LENGTH      : constant                    := 65;
  -----------
  -- Types --
  -----------
    type Utsname_Type is record
      Sysname    : Interfaces.C.char_array (1 .. UTSNAME_LENGTH);
      Nodename   : Interfaces.C.char_array (1 .. UTSNAME_LENGTH);
      Release    : Interfaces.C.char_array (1 .. UTSNAME_LENGTH);
      Version    : Interfaces.C.char_array (1 .. UTSNAME_LENGTH);
      Machine    : Interfaces.C.char_array (1 .. UTSNAME_LENGTH);
      Domainname : Interfaces.C.char_array (1 .. UTSNAME_LENGTH);
    end record;
  -----------------
  -- Subprograms --
  -----------------
    function Sysconf(
      Name : Integer_4_Signed_C)
      return Long;

    function Uname(
      Buf : access Utsname_Type)
      return Interfaces.C.int;

-------
private
-------
  ----------------
  -- Directives --
  ----------------
    pragma Import(C, Sysconf, "sysconf");
    pragma Import(C, Uname,   "uname");
  end Neo.Linux;
