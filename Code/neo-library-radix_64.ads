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
  Neo.Foundation.Output,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Package_Testing;
use
  Neo.Foundation.Output,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Package_Testing;
package Neo.Library.Radix_64
  is
  -----------------
  -- Subprograms --
  -----------------
    procedure Test;
    function Encode(
      Data : in Array_Integer_1_Unsigned)
      return String_1;
    function Encode(
      Data : in Array_Integer_1_Unsigned)
      return String_2;
    function Decode(
      Data : in String_1)
      return Array_Integer_1_Unsigned;
    function Decode(
      Data : in String_2)
      return Array_Integer_1_Unsigned;
-------
private
-------
  ---------------
  -- Constants --
  ---------------
    DATA_TERMINATION_FLAG : constant Character_1 := '=';
    SIXTET_TO_RADIX_64:
      constant String_1(1..64) :=
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";
    RADIX_64_TO_SIXTET:
      constant array (1..256)
      of Integer_4_Unsigned :=(
      Character_1'pos(SIXTET_TO_RADIX_64(1))  =>  1,
      Character_1'pos(SIXTET_TO_RADIX_64(2))  =>  2,
      Character_1'pos(SIXTET_TO_RADIX_64(3))  =>  3,
      Character_1'pos(SIXTET_TO_RADIX_64(4))  =>  4,
      Character_1'pos(SIXTET_TO_RADIX_64(5))  =>  5,
      Character_1'pos(SIXTET_TO_RADIX_64(6))  =>  6,
      Character_1'pos(SIXTET_TO_RADIX_64(7))  =>  7,
      Character_1'pos(SIXTET_TO_RADIX_64(8))  =>  8,
      Character_1'pos(SIXTET_TO_RADIX_64(9))  =>  9,
      Character_1'pos(SIXTET_TO_RADIX_64(10)) => 10,
      Character_1'pos(SIXTET_TO_RADIX_64(11)) => 11,
      Character_1'pos(SIXTET_TO_RADIX_64(12)) => 12,
      Character_1'pos(SIXTET_TO_RADIX_64(13)) => 13,
      Character_1'pos(SIXTET_TO_RADIX_64(14)) => 14,
      Character_1'pos(SIXTET_TO_RADIX_64(15)) => 15,
      Character_1'pos(SIXTET_TO_RADIX_64(16)) => 16,
      Character_1'pos(SIXTET_TO_RADIX_64(17)) => 17,
      Character_1'pos(SIXTET_TO_RADIX_64(18)) => 18,
      Character_1'pos(SIXTET_TO_RADIX_64(19)) => 19,
      Character_1'pos(SIXTET_TO_RADIX_64(20)) => 20,
      Character_1'pos(SIXTET_TO_RADIX_64(21)) => 21,
      Character_1'pos(SIXTET_TO_RADIX_64(22)) => 22,
      Character_1'pos(SIXTET_TO_RADIX_64(23)) => 23,
      Character_1'pos(SIXTET_TO_RADIX_64(24)) => 24,
      Character_1'pos(SIXTET_TO_RADIX_64(25)) => 25,
      Character_1'pos(SIXTET_TO_RADIX_64(26)) => 26,
      Character_1'pos(SIXTET_TO_RADIX_64(27)) => 27,
      Character_1'pos(SIXTET_TO_RADIX_64(28)) => 28,
      Character_1'pos(SIXTET_TO_RADIX_64(29)) => 29,
      Character_1'pos(SIXTET_TO_RADIX_64(30)) => 30,
      Character_1'pos(SIXTET_TO_RADIX_64(31)) => 31,
      Character_1'pos(SIXTET_TO_RADIX_64(32)) => 32,
      Character_1'pos(SIXTET_TO_RADIX_64(33)) => 33,
      Character_1'pos(SIXTET_TO_RADIX_64(34)) => 34,
      Character_1'pos(SIXTET_TO_RADIX_64(35)) => 35,
      Character_1'pos(SIXTET_TO_RADIX_64(36)) => 36,
      Character_1'pos(SIXTET_TO_RADIX_64(37)) => 37,
      Character_1'pos(SIXTET_TO_RADIX_64(38)) => 38,
      Character_1'pos(SIXTET_TO_RADIX_64(39)) => 39,
      Character_1'pos(SIXTET_TO_RADIX_64(40)) => 40,
      Character_1'pos(SIXTET_TO_RADIX_64(41)) => 41,
      Character_1'pos(SIXTET_TO_RADIX_64(42)) => 42,
      Character_1'pos(SIXTET_TO_RADIX_64(43)) => 43,
      Character_1'pos(SIXTET_TO_RADIX_64(44)) => 44,
      Character_1'pos(SIXTET_TO_RADIX_64(45)) => 45,
      Character_1'pos(SIXTET_TO_RADIX_64(46)) => 46,
      Character_1'pos(SIXTET_TO_RADIX_64(47)) => 47,
      Character_1'pos(SIXTET_TO_RADIX_64(48)) => 48,
      Character_1'pos(SIXTET_TO_RADIX_64(49)) => 49,
      Character_1'pos(SIXTET_TO_RADIX_64(50)) => 50,
      Character_1'pos(SIXTET_TO_RADIX_64(51)) => 51,
      Character_1'pos(SIXTET_TO_RADIX_64(52)) => 52,
      Character_1'pos(SIXTET_TO_RADIX_64(53)) => 53,
      Character_1'pos(SIXTET_TO_RADIX_64(54)) => 54,
      Character_1'pos(SIXTET_TO_RADIX_64(55)) => 55,
      Character_1'pos(SIXTET_TO_RADIX_64(56)) => 56,
      Character_1'pos(SIXTET_TO_RADIX_64(57)) => 57,
      Character_1'pos(SIXTET_TO_RADIX_64(58)) => 58,
      Character_1'pos(SIXTET_TO_RADIX_64(59)) => 59,
      Character_1'pos(SIXTET_TO_RADIX_64(60)) => 60,
      Character_1'pos(SIXTET_TO_RADIX_64(61)) => 61,
      Character_1'pos(SIXTET_TO_RADIX_64(62)) => 62,
      Character_1'pos(SIXTET_TO_RADIX_64(63)) => 63,
      Character_1'pos(SIXTET_TO_RADIX_64(64)) => 64,
      others                                  => 0);
  end Neo.Library.Radix_64;
