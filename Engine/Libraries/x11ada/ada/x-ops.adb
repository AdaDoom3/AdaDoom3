-- $Source: /home/harp/1/proto/monoBANK/xbind/x-ops.adb,v $ 
-- $Revision: 1.1 $ $Date: 96/04/05 13:54:25 $ $Author: mg $ 

with Ada.Unchecked_Conversion;
with Interfaces;

package body X.Ops is

    function "and" (I1, I2: Char)  return Char is
    begin
	return To_Signed(To_Unsigned(I1) and To_Unsigned(I2));
    end "and";

    function "and" (I1, I2: Short) return Short is
    begin
	return To_Signed(To_Unsigned(I1) and To_Unsigned(I2));
    end "and";

    function "and" (I1, I2: Int)   return Int is
    begin
	return To_Signed(To_Unsigned(I1) and To_Unsigned(I2));
    end "and";

    function "and" (I1, I2: Long)  return Long is
    begin
	return To_Signed(To_Unsigned(I1) and To_Unsigned(I2));
    end "and";


    function "or"  (I1, I2: Char)  return Char is
    begin
	return To_Signed(To_Unsigned(I1) or To_Unsigned(I2));
    end "or";

    function "or"  (I1, I2: Short) return Short is
    begin
	return To_Signed(To_Unsigned(I1) or To_Unsigned(I2));
    end "or";

    function "or"  (I1, I2: Int)   return Int is
    begin
	return To_Signed(To_Unsigned(I1) or To_Unsigned(I2));
    end "or";

    function "or"  (I1, I2: Long)  return Long is
    begin
	return To_Signed(To_Unsigned(I1) or To_Unsigned(I2));
    end "or";


    function "xor"  (I1, I2: Char)  return Char is
    begin
	return To_Signed(To_Unsigned(I1) xor To_Unsigned(I2));
    end "xor";

    function "xor"  (I1, I2: Short) return Short is
    begin
	return To_Signed(To_Unsigned(I1) xor To_Unsigned(I2));
    end "xor";

    function "xor"  (I1, I2: Int)   return Int is
    begin
	return To_Signed(To_Unsigned(I1) xor To_Unsigned(I2));
    end "xor";

    function "xor"  (I1, I2: Long)  return Long is
    begin
	return To_Signed(To_Unsigned(I1) xor To_Unsigned(I2));
    end "xor";


    function "not" (I1: Char)  return Char is
    begin
	return To_Signed(not To_Unsigned(I1));
    end "not";

    function "not" (I1: Short) return Short is
    begin
	return To_Signed(not To_Unsigned(I1));
    end "not";

    function "not" (I1: Int)   return Int is
    begin
	return To_Signed(not To_Unsigned(I1));
    end "not";

    function "not" (I1: Long)  return Long is
    begin
	return To_Signed(not To_Unsigned(I1));
    end "not";


    package I renames Interfaces;

    -- this would have to change on a machine with 64 bit integers.

    function U8  is new Ada.Unchecked_Conversion (Uchar,  I.Unsigned_8);
    function U16 is new Ada.Unchecked_Conversion (Ushort, I.Unsigned_16);
    function U32 is new Ada.Unchecked_Conversion (Uint,   I.Unsigned_32);
    function U32 is new Ada.Unchecked_Conversion (Ulong,  I.Unsigned_32);

    function UC is new Ada.Unchecked_Conversion (I.Unsigned_8,  Uchar);
    function US is new Ada.Unchecked_Conversion (I.Unsigned_16, Ushort);
    function UI is new Ada.Unchecked_Conversion (I.Unsigned_32, Uint);
    function UL is new Ada.Unchecked_Conversion (I.Unsigned_32, Ulong);

    function Shift_Left (Value: Uchar; Amount: Natural) 
	return Unsigned_Char is
    begin
	return UC(I.Shift_Left(U8(Value), Amount));
    end Shift_Left;

    function Shift_Left (Value: Ushort; Amount: Natural) 
	return Unsigned_Short is
    begin
	return US(I.Shift_Left(U16(Value), Amount));
    end Shift_Left;

    function Shift_Left (Value: Uint; Amount: Natural) 
	return Unsigned_Int is
    begin
	return UI(I.Shift_Left(U32(Value), Amount));
    end Shift_Left;

    function Shift_Left (Value: Ulong; Amount: Natural) 
	return Unsigned_Long is
    begin
	return UL(I.Shift_Left(U32(Value), Amount));
    end Shift_Left;


    function Shift_Right (Value: Uchar; Amount: Natural) 
	return Unsigned_Char is
    begin
	return UC(I.Shift_Right(U8(Value), Amount));
    end Shift_Right;

    function Shift_Right (Value: Ushort; Amount: Natural) 
	return Unsigned_Short is
    begin
	return US(I.Shift_Right(U16(Value), Amount));
    end Shift_Right;

    function Shift_Right (Value: Uint; Amount: Natural) 
	return Unsigned_Int is
    begin
	return UI(I.Shift_Right(U32(Value), Amount));
    end Shift_Right;

    function Shift_Right (Value: Ulong; Amount: Natural) 
	return Unsigned_Long is
    begin
	return UL(I.Shift_Right(U32(Value), Amount));
    end Shift_Right;


    function Shift_Right_Arithmetic (Value: Uchar; Amount: Natural) 
	return Unsigned_Char is
    begin
	return UC(I.Shift_Right_Arithmetic(U8(Value), Amount));
    end Shift_Right_Arithmetic;

    function Shift_Right_Arithmetic (Value: Ushort; Amount: Natural) 
	return Ushort is
    begin
	return US(I.Shift_Right_Arithmetic(U16(Value), Amount));
    end Shift_Right_Arithmetic;

    function Shift_Right_Arithmetic (Value: Uint; Amount: Natural) 
	return Unsigned_Int is
    begin
	return UI(I.Shift_Right_Arithmetic(U32(Value), Amount));
    end Shift_Right_Arithmetic;

    function Shift_Right_Arithmetic (Value: Ulong; Amount: Natural) 
	return Unsigned_Long is
    begin
	return UL(I.Shift_Right_Arithmetic(U32(Value), Amount));
    end Shift_Right_Arithmetic;

    function Shift_Left (Value: Char;  Amount: Natural) return Char is
    begin
	return To_Signed(Shift_Left(To_Unsigned(Value), Amount));
    end Shift_Left;

    function Shift_Left (Value: Short; Amount: Natural) return Short is
    begin
	return To_Signed(Shift_Left(To_Unsigned(Value), Amount));
    end Shift_Left;

    function Shift_Left (Value: Int;   Amount: Natural) return Int is
    begin
	return To_Signed(Shift_Left(To_Unsigned(Value), Amount));
    end Shift_Left;

    function Shift_Left (Value: Long;  Amount: Natural) return Long is
    begin
	return To_Signed(Shift_Left(To_Unsigned(Value), Amount));
    end Shift_Left;


    function Shift_Right (Value: Char;  Amount: Natural) return Char is
    begin
	return To_Signed(Shift_Right(To_Unsigned(Value), Amount));
    end Shift_Right;

    function Shift_Right (Value: Short; Amount: Natural) return Short is
    begin
	return To_Signed(Shift_Right(To_Unsigned(Value), Amount));
    end Shift_Right;

    function Shift_Right (Value: Int;   Amount: Natural) return Int is
    begin
	return To_Signed(Shift_Right(To_Unsigned(Value), Amount));
    end Shift_Right;

    function Shift_Right (Value: Long;  Amount: Natural) return Long is
    begin
	return To_Signed(Shift_Right(To_Unsigned(Value), Amount));
    end Shift_Right;


    function Shift_Right_Arithmetic (Value: Char;  Amount: Natural) 
	return Char is
    begin
	return To_Signed(Shift_Right_Arithmetic(To_Unsigned(Value), Amount));
    end Shift_Right_Arithmetic;

    function Shift_Right_Arithmetic (Value: Short; Amount: Natural) 
	return Short is
    begin
	return To_Signed(Shift_Right_Arithmetic(To_Unsigned(Value), Amount));
    end Shift_Right_Arithmetic;

    function Shift_Right_Arithmetic (Value: Int;   Amount: Natural) 
	return Int is
    begin
	return To_Signed(Shift_Right_Arithmetic(To_Unsigned(Value), Amount));
    end Shift_Right_Arithmetic;

    function Shift_Right_Arithmetic (Value: Long;  Amount: Natural) 
	return Long is
    begin
	return To_Signed(Shift_Right_Arithmetic(To_Unsigned(Value), Amount));
    end Shift_Right_Arithmetic;

end X.Ops;
