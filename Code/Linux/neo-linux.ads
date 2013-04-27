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
    SC_NPROCESSORS_ONLN : constant := 84;
  -----------------
  -- Subprograms --
  -----------------
    function Sysconf(
      Name : Integer_4_Signed_C)
      return Long;
-------
private
-------
  ----------------
  -- Directives --
  ----------------
    pragma Import(C, Sysconf, "sysconf");
  end Neo.Linux;
