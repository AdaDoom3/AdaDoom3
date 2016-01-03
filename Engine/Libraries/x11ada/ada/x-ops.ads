-- $Source: /home/harp/1/proto/monoBANK/xbind/x-ops.ads,v $ 
-- $Revision: 1.1 $ $Date: 96/04/05 13:54:26 $ $Author: mg $ 

with Ada.Unchecked_Conversion;

package X.Ops is

    subtype Uchar  is X.Unsigned_Char;
    subtype Ushort is X.Unsigned_Short;
    subtype Uint   is X.Unsigned_Int;
    subtype Ulong  is X.Unsigned_Long;

    -- unchecked conversions between signed and unsigned types --------------
    function To_Unsigned is new Ada.Unchecked_Conversion (Char,  UChar);
    function To_Unsigned is new Ada.Unchecked_Conversion (Short, Ushort);
    function To_Unsigned is new Ada.Unchecked_Conversion (Int,   Uint);
    function To_Unsigned is new Ada.Unchecked_Conversion (Long,  Ulong);

    function To_Signed is new Ada.Unchecked_Conversion (UChar,  Char);
    function To_Signed is new Ada.Unchecked_Conversion (UShort, Short);
    function To_Signed is new Ada.Unchecked_Conversion (UInt,   Int);
    function To_Signed is new Ada.Unchecked_Conversion (ULong,  Long);

    -- and, or, xor, not -----------------------------------------------------
    function "and" (I1, I2: Uchar)  return Uchar  renames Interfaces.C."and";
    function "and" (I1, I2: Ushort) return Ushort renames Interfaces.C."and";
    function "and" (I1, I2: Uint)   return Uint   renames Interfaces.C."and";
    function "and" (I1, I2: Ulong)  return Ulong  renames Interfaces.C."and";

    function "and" (I1, I2: Char)  return Char;
	pragma Inline("and");
    function "and" (I1, I2: Short) return Short;
	pragma Inline("and");
    function "and" (I1, I2: Int)   return Int;
	pragma Inline("and");
    function "and" (I1, I2: Long)  return Long;
	pragma Inline("and");

    function "or"  (I1, I2: Uchar)  return Uchar  renames Interfaces.C."or";
    function "or"  (I1, I2: Ushort) return Ushort renames Interfaces.C."or";
    function "or"  (I1, I2: Uint)   return Uint   renames Interfaces.C."or";
    function "or"  (I1, I2: Ulong)  return Ulong  renames Interfaces.C."or";

    function "or"  (I1, I2: Char)  return Char;
	pragma Inline("or");
    function "or"  (I1, I2: Short) return Short;
	pragma Inline("or");
    function "or"  (I1, I2: Int)   return Int;
	pragma Inline("or");
    function "or"  (I1, I2: Long)  return Long;
	pragma Inline("or");

    function "xor"  (I1, I2: Uchar)  return Uchar  renames Interfaces.C."xor";
    function "xor"  (I1, I2: Ushort) return Ushort renames Interfaces.C."xor";
    function "xor"  (I1, I2: Uint)   return Uint   renames Interfaces.C."xor";
    function "xor"  (I1, I2: Ulong)  return Ulong  renames Interfaces.C."xor";

    function "xor"  (I1, I2: Char)  return Char;
	pragma Inline("xor");
    function "xor"  (I1, I2: Short) return Short;
	pragma Inline("xor");
    function "xor"  (I1, I2: Int)   return Int;
	pragma Inline("xor");
    function "xor"  (I1, I2: Long)  return Long;
	pragma Inline("xor");

    function "not" (I1: Uchar)  return Uchar  renames Interfaces.C."not";
    function "not" (I1: Ushort) return Ushort renames Interfaces.C."not";
    function "not" (I1: Uint)   return Uint   renames Interfaces.C."not";
    function "not" (I1: Ulong)  return Ulong  renames Interfaces.C."not";

    function "not" (I1: Char)  return Char;
	pragma Inline("not");
    function "not" (I1: Short) return Short;
	pragma Inline("not");
    function "not" (I1: Int)   return Int;
	pragma Inline("not");
    function "not" (I1: Long)  return Long;
	pragma Inline("not");

    -- Shift left and right ---------------------------------------------------
    function Shift_Left (Value: Uchar;  Amount: Natural) return Uchar;
	pragma Inline(Shift_Left);

    function Shift_Left (Value: Ushort; Amount: Natural) return Ushort;
	pragma Inline(Shift_Left);

    function Shift_Left (Value: Uint;   Amount: Natural) return Uint;
	pragma Inline(Shift_Left);

    function Shift_Left (Value: Ulong;  Amount: Natural) return Ulong;
	pragma Inline(Shift_Left);


    function Shift_Left (Value: Char;  Amount: Natural) return Char;
	pragma Inline(Shift_Left);

    function Shift_Left (Value: Short; Amount: Natural) return Short;
	pragma Inline(Shift_Left);

    function Shift_Left (Value: Int;   Amount: Natural) return Int;
	pragma Inline(Shift_Left);

    function Shift_Left (Value: Long;  Amount: Natural) return Long;
	pragma Inline(Shift_Left);


    function Shift_Right (Value: Uchar;  Amount: Natural) return Uchar;
	pragma Inline(Shift_Right);

    function Shift_Right (Value: Ushort; Amount: Natural) return Ushort;
	pragma Inline(Shift_Right);

    function Shift_Right (Value: Uint;   Amount: Natural) return Uint;
	pragma Inline(Shift_Right);

    function Shift_Right (Value: Ulong;  Amount: Natural) return Ulong;
	pragma Inline(Shift_Right);


    function Shift_Right (Value: Char;  Amount: Natural) return Char;
	pragma Inline(Shift_Right);

    function Shift_Right (Value: Short; Amount: Natural) return Short;
	pragma Inline(Shift_Right);

    function Shift_Right (Value: Int;   Amount: Natural) return Int;
	pragma Inline(Shift_Right);

    function Shift_Right (Value: Long;  Amount: Natural) return Long;
	pragma Inline(Shift_Right);


    function Shift_Right_Arithmetic (Value: Uchar;  Amount: Natural) 
	return Uchar;
	pragma Inline(Shift_Right_Arithmetic);

    function Shift_Right_Arithmetic (Value: Ushort; Amount: Natural) 
	return Ushort;
	pragma Inline(Shift_Right_Arithmetic);

    function Shift_Right_Arithmetic (Value: Uint;   Amount: Natural) 
	return Uint;
	pragma Inline(Shift_Right_Arithmetic);

    function Shift_Right_Arithmetic (Value: Ulong;  Amount: Natural) 
	return Ulong;
	pragma Inline(Shift_Right_Arithmetic);


    function Shift_Right_Arithmetic (Value: Char;  Amount: Natural) 
	return Char;
	pragma Inline(Shift_Right_Arithmetic);

    function Shift_Right_Arithmetic (Value: Short; Amount: Natural) 
	return Short;
	pragma Inline(Shift_Right_Arithmetic);

    function Shift_Right_Arithmetic (Value: Int;   Amount: Natural) 
	return Int;
	pragma Inline(Shift_Right_Arithmetic);

    function Shift_Right_Arithmetic (Value: Long;  Amount: Natural) 
	return Long;
	pragma Inline(Shift_Right_Arithmetic);

end X.Ops;
