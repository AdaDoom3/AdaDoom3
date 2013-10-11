--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     IEEE_754.Generic_Double_Precision           Luebeck            --
--  Interface                                      Summer, 2008       --
--                                                                    --
--                                Last revision :  11:26 27 Jul 2008  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--

generic
   type Number is digits <>;
package IEEE_754.Generic_Double_Precision is
   pragma Pure (IEEE_754.Generic_Double_Precision);
--
-- Float_64 -- 64-bit double-precision IEEE 754 float. The memory layout
--             is big endian, i.e. the byte containing the number's sign
-- and  the  most  significant  bits  of the exponent is the first array
-- element.  The  byte  containing  the  least  significant  bits of the
-- mantissa is the last array element.
--
   type Float_64 is array (1..8) of Byte;
   Positive_Infinity : constant Float_64;
   Positive_Zero     : constant Float_64;
   Negative_Infinity : constant Float_64;
   Negative_Zero     : constant Float_64;
--
-- From_IEEE -- Conversion from 32-bit single precision IEEE 754 float
--
--    Value - The argument
--
-- Returns :
--
--    The corresponding floating-point number
--
-- Exceptions :
--
--    Not_A_Number_Error      - Not a number
--    Positive_Overflow_Error - Positive infinity or too big positive
--    Negative_Overflow_Error - Negative infinity or too big negative
--
   function From_IEEE (Value : Float_64) return Number;
--
-- Is_NaN -- NaN test
--
--    Value - The argument
--
-- Returns :
--
--    True if Value is an IEEE NaN
--
   function Is_NaN (Value : Float_64) return Boolean;
--
-- Is_Negative -- IEEE sign test
--
--    Value - The argument
--
-- Returns :
--
--    True if Value has an IEEE sign
--
   function Is_Negative (Value : Float_64) return Boolean;
--
-- Is_Real -- Value test
--
--    Value - The argument
--
-- This function tests if Value represents a real number. Infinities and
-- NaN are not numbers. Both zeros are considered numbers.
--
-- Returns :
--
--    True if Value represents a real number
--
   function Is_Real (Value : Float_64) return Boolean;
--
-- To_IEEE -- Conversion to 32-bit single precision IEEE 754 float
--
--    Value - The argument
--
-- The value  to  big  for  normalized  representation  results  in  the
-- corresponding IEEE infinities. Too small values  are  represented  as
-- IEEE zero.
--
-- Returns :
--
--    The corresponding IEEE 754 representation
--
   function To_IEEE (Value : Number) return Float_64;
private
   pragma Inline (Is_NaN);
   pragma Inline (Is_Negative);
   pragma Inline (Is_Real);

   Positive_Infinity : constant Float_64 := (16#7F#,16#F0#,others => 0);
   Positive_Zero     : constant Float_64 := (others => 0);
   Negative_Infinity : constant Float_64 := (16#FF#,16#F8#,others => 0);
   Negative_Zero     : constant Float_64 := (16#80#,others => 0);

end IEEE_754.Generic_Double_Precision;
