--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     IEEE_754.Generic_Double_Precision           Luebeck            --
--  Implementation                                 Summer, 2008       --
--                                                                    --
--                                Last revision :  22:28 15 Feb 2009  --
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

package body IEEE_754.Generic_Double_Precision is

   Exponent_Bias  : constant := 2**10 - 1;
   Exponent_First : constant := -51;
   Exponent_Last  : constant := 2**11 - 1;
   Fraction_Bits  : constant := 52;
   Mantissa_Bits  : constant := 53;

   function Exponent (Value : Float_64) return Integer is
      pragma Inline (Exponent);
   begin
      return
         Integer
         (  Shift_Left  (Unsigned_16 (Value (1)) and 16#7F#, 4)
         or Shift_Right (Unsigned_16 (Value (2)), 4)
         );
   end Exponent;

   function Mantissa (Value : Float_64) return Unsigned_64 is
      pragma Inline (Mantissa);
   begin
      return
      (  Unsigned_64 (Value (8))
      or Shift_Left (Unsigned_64 (Value (7)), 8  )
      or Shift_Left (Unsigned_64 (Value (6)), 2*8)
      or Shift_Left (Unsigned_64 (Value (5)), 3*8)
      or Shift_Left (Unsigned_64 (Value (4)), 4*8)
      or Shift_Left (Unsigned_64 (Value (3)), 5*8)
      or Shift_Left (Unsigned_64 (Value (2)) and 16#0F#, 6*8)
      or 2 ** Fraction_Bits
      );
   end Mantissa;

   procedure Normalize
             (  Value    : Number;
                Mantissa : out Unsigned_64;
                Exponent : out Integer
             )  is
      pragma Inline (Normalize);
   begin
      if Number'Machine_Radix = 2 then
         --
         -- The machine  radix  is  binary.  We  can  use  the  hardware
         -- representation  attributes  in order to get the exponent and
         -- the fraction.
         --
         Exponent := Number'Exponent (Value) - Mantissa_Bits;
         Mantissa := Unsigned_64 (Number'Scaling (Value, -Exponent));
      else
         --
         -- OK, this gets more tricky. The number is normalized to be in
         -- the range 2**53 > X >= 2**52, by multiplying to  the  powers
         -- of  two.  Some optimization is made to factor out the powers
         -- 2**(2**n)). Though we do not use powers bigger than 30.
         --
         declare
            Accum : Number  := Value;
            Shift : Integer;
         begin
            Exponent := 0;
            if Accum < 2.0**Fraction_Bits then
               Shift := 24;
               while Shift > 0 loop
                  if Accum < 2.0**(Mantissa_Bits - Shift) then
                     Accum    := Accum * 2.0**Shift;
                     Exponent := Exponent - Shift;
                  else
                     Shift := Shift / 2;
                  end if;
               end loop;
            elsif Accum >= 2.0**Mantissa_Bits then
               Shift := 8;
               while Shift > 0 loop
                  if Accum >= 2.0**(Fraction_Bits + Shift) then
                     Accum    := Accum / 2.0**Shift;
                     Exponent := Exponent + Shift;
                  else
                     Shift := Shift / 2;
                  end if;
               end loop;
            end if;
            Mantissa := Unsigned_64 (Accum);
         end;
      end if;
   end Normalize;

   function From_IEEE (Value : Float_64) return Number is
   begin
      if (  0 = (Value (1) and 16#7F#)
         and then
            Value (2) = 0
         and then
            Value (3) = 0
         and then
            Value (4) = 0
         and then
            Value (5) = 0
         and then
            Value (6) = 0
         and then
            Value (7) = 0
         and then
            Value (8) = 0
         )
      then
         return 0.0;
      end if;
      declare
         Power    : Integer := Exponent (Value);
         Fraction : Unsigned_64 := Mantissa (Value);
         Result   : Number;
      begin
         if Power = Exponent_Last then
            if Fraction /= 2#1000_0000_0000# then
               raise Not_A_Number_Error;
            elsif Value (1) > 127 then
               raise Negative_Overflow_Error;
            else
               raise Positive_Overflow_Error;
            end if;
         elsif Power = 0 then -- Denormalized number
            Fraction := Fraction and 16#0F_FF_FF_FF_FF_FF_FF_FF#;
            Power    := Exponent_First - Exponent_Bias;
            if Number'Machine_Radix = 2 then
               Result := Number'Scaling (Number (Fraction), Power);
            else
               Result := Number (Fraction) * 2.0 ** Power;
            end if;
         else -- Normalized number
            Power := Power - Exponent_Bias - Fraction_Bits;
            if Number'Machine_Radix = 2 then
               Result := Number'Scaling (Number (Fraction), Power);
            else
               Result := Number (Fraction) * 2.0 ** Power;
            end if;
         end if;
         if Value (1) > 127 then
            return -Result;
         else
            return Result;
         end if;
      exception
         when Constraint_Error =>
            if Value (1) > 127 then
               raise Negative_Overflow_Error;
            else
               raise Positive_Overflow_Error;
            end if;
      end;
   end From_IEEE;

   function Is_NaN (Value : Float_64) return Boolean is
   begin
      return
      (  Exponent (Value) = Exponent_Last
      and then
         Mantissa (Value) /= 2 ** Fraction_Bits
      );
   end Is_NaN;

   function Is_Negative (Value : Float_64) return Boolean is
   begin
      return Value (1) > 127;
   end Is_Negative;

   function Is_Real (Value : Float_64) return Boolean is
   begin
      return Exponent (Value) < Exponent_Last;
   end Is_Real;

   function To_IEEE (Value : Number) return Float_64 is
   begin
      if Value = 0.0 then
         return (others => 0);
      end if;
      declare
         Exponent : Integer;
         Fraction : Unsigned_64;
         Sign     : Byte := 0;
      begin
         if Value > 0.0 then
            Normalize (Value, Fraction, Exponent);
         else
            Normalize (-Value, Fraction, Exponent);
            Sign := 2**7;
         end if;
         Exponent := Exponent + Exponent_Bias + Fraction_Bits;
         if Exponent < Exponent_First then
            -- Underflow, resuls in zero
            return (others => 0);
         elsif Exponent >= Exponent_Last then
            -- Overflow, results in infinities
            if Sign = 0 then
               return Positive_Infinity;
            else
               return Negative_Infinity;
            end if;
         elsif Exponent <= 0 then -- Denormalized
            Fraction := Shift_Right (Fraction, 1 - Exponent);
            Exponent := 0;
         end if;
         return
         (  Sign or Byte (Exponent / 2**4),
            (  Byte (Shift_Right (Fraction, 8*6) and 16#0F#)
            or Shift_Left (Byte (Exponent mod 2**4), 4)
            ),
            Byte (Shift_Right (Fraction, 8*5) and 16#FF#),
            Byte (Shift_Right (Fraction, 8*4) and 16#FF#),
            Byte (Shift_Right (Fraction, 8*3) and 16#FF#),
            Byte (Shift_Right (Fraction, 8*2) and 16#FF#),
            Byte (Shift_Right (Fraction, 8  ) and 16#FF#),
            Byte (Fraction                    and 16#FF#)
         );
      end;
   end To_IEEE;

end IEEE_754.Generic_Double_Precision;
