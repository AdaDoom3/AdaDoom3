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
  System,
  Interfaces,
  Interfaces.C,
  Ada.Text_IO,
  Ada.Strings.Fixed,
  Ada.Strings.Wide_Fixed,
  Ada.Unchecked_Conversion,
  Ada.Unchecked_Deallocation;
use
  System,
  Interfaces,
  Interfaces.C,
  Ada.Strings.Fixed,
  Ada.Strings.Wide_Fixed;
package Neo.Foundation.Data_Types
  is
  -------------
  -- Numbers --
  -------------
    subtype Character_1_C
      is Interfaces.C.Char;
    subtype Character_1
      is Character;
    subtype Character_2_C
      is Interfaces.C.WChar_T;
    subtype Character_2
      is Wide_Character;
    subtype Integer_1_Unsigned_C
      is Interfaces.C.Unsigned_Char;
    subtype Integer_1_Unsigned
      is Unsigned_8;
    subtype Integer_1_Signed_C
      is Interfaces.C.Char;
    subtype Integer_1_Signed
      is Short_Short_Integer;
    subtype Integer_1_Natural
      is Integer_1_Unsigned;
    subtype Integer_1_Positive
      is Integer_1_Unsigned
      range 1..Integer_1_Unsigned'Last;
    subtype Integer_2_Unsigned_C
      is Interfaces.C.Unsigned_Short;
    subtype Integer_2_Unsigned
      is Unsigned_16;
    subtype Integer_2_Signed_C
      is Interfaces.C.Short;
    subtype Integer_2_Signed
      is Short_Integer;
    subtype Integer_2_Natural
      is Integer_2_Unsigned;
    subtype Integer_2_Positive
      is Integer_1_Unsigned
      range 1..Integer_1_Unsigned'Last;
    subtype Integer_4_Unsigned_C
      is Interfaces.C.Unsigned;
    subtype Integer_4_Unsigned
      is Unsigned_32;
    subtype Integer_4_Signed_C 
      is Interfaces.C.Int;
    subtype Integer_4_Signed
      is Integer;
    subtype Integer_4_Natural
      is Natural;
    subtype Integer_4_Positive
      is Positive;
    subtype Integer_8_Unsigned
      is Unsigned_64;
    subtype Integer_8_Unsigned_C
      is Integer_8_Unsigned;
    subtype Integer_8_Signed
      is Long_Long_Integer;
    subtype Integer_8_Signed_C
      is Integer_8_Signed;
    subtype Integer_8_Natural
      is Integer_8_Unsigned;
    subtype Integer_8_Positive
      is Integer_8_Unsigned
      range 1..Integer_8_Unsigned'Last;
    subtype Integer_Size_C
      is Interfaces.C.Size_T;
    subtype Float_8_Real_C
      is Interfaces.C.Double;
    subtype Float_8_Real
      is Long_Float;
    subtype Float_8_Natural
      is Float_8_Real
      range 0.0..Float_8_Real'Last;
    subtype Float_8_Positive
      is Float_8_Real
      range 1.0..Float_8_Real'Last;
    subtype Float_8_Percent
      is Float_8_Real
      range 0.0..100.0;
    subtype Float_8_Degree
      is Float_8_Real
      range 1.0..360.0;
    subtype Float_4_Real_C
      is Interfaces.C.C_Float;
    subtype Float_4_Real
      is Float;
    subtype Float_4_Natural
      is Float_4_Real
      range 0.0..Float_4_Real'Last;
    subtype Float_4_Positive
      is Float_4_Real
      range 1.0..Float_4_Real'Last;
    subtype Float_4_Percent
      is Float_4_Real
      range 0.0..100.0;
    subtype Float_4_Degree
      is Float_4_Real
      range 1.0..360.0;
  -------------
  -- Strings --
  -------------
    subtype String_1
      is String;
    subtype String_2
      is Wide_String;
    subtype String_1_C
      is Interfaces.C.Char_Array;
    subtype String_2_C      
      is Interfaces.C.WChar_Array;
  ---------------
  -- Accessors --
  ---------------
    subtype Access_Address
      is System.Address;
    type Access_String_2
      is access all String_2;
    type Access_Procedure
      is access procedure;
    type Access_String_1_C
      is access all String_1_C;
    type Access_String_1
      is access all String_1;
    type Access_String_2_C
      is access all String_2_C;
    type Access_Constant_Character_1_C
      is access constant Character_1_C;
    type Access_Character_1_C
      is access all Interfaces.C.Char;
    type Access_Constant_Character_2_C
      is access constant Character_2_C;
    type Access_Character_2_C
      is access all Character_2_C;
    type Access_Integer_4_Unsigned_C
      is access all Integer_4_Unsigned_C;
    type Access_Integer_4_Unsigned
      is access all Integer_4_Unsigned;
    type Access_Integer_4_Signed_C
      is access all Integer_4_Signed_C;
    type Access_Integer_2_Unsigned_C
      is access all Integer_2_Unsigned_C;
  ------------
  -- Arrays --
  ------------
    type Array_Address
      is array(Positive range <>)
      of Address;
    type Array_Access_String_2
      is array(Positive range <>)
      of Access_String_2;
    type Array_Character_1_C
      is array(Positive range <>)
      of Character_1_C;
    type Array_Character_1
      is array(Positive range <>)
      of Character_1;
    type Array_Character_2_C
      is array(Positive range <>)
      of Character_2_C;
    type Array_Character_2
      is array(Positive range <>)
      of Character_2;
    type Array_Integer_1_Unsigned_C
      is array(Positive range <>)
      of Integer_1_Unsigned_C;
    type Array_Integer_1_Unsigned
      is array(Positive range <>)
      of Integer_1_Unsigned;
    type Array_Integer_1_Signed_C
      is array(Positive range <>)
      of Integer_1_Signed_C;
    type Array_Integer_1_Signed
      is array(Positive range <>)
      of Integer_1_Signed;
    type Array_Integer_1_Natural
      is array(Positive range <>)
      of Integer_1_Natural;
    type Array_Integer_1_Positive
      is array(Positive range <>)
      of Integer_1_Positive;
    type Array_Integer_2_Unsigned_C
      is array(Positive range <>)
      of Integer_2_Unsigned_C;
    type Array_Integer_2_Unsigned
      is array(Positive range <>)
      of Integer_2_Unsigned;
    type Array_Integer_2_Signed_C
      is array(Positive range <>)
      of Integer_2_Signed_C;
    type Array_Integer_2_Signed
      is array(Positive range <>)
      of Integer_2_Signed;
    type Array_Integer_2_Natural
      is array(Positive range <>)
      of Integer_2_Natural;
    type Array_Integer_2_Positive
      is array(Positive range <>)
      of Integer_2_Positive;
    type Array_Integer_4_Unsigned_C
      is array(Positive range <>)
      of Integer_4_Unsigned_C;
    type Array_Integer_4_Unsigned
      is array(Positive range <>)
      of Integer_4_Unsigned;
    type Array_Integer_4_Signed_C 
      is array(Positive range <>)
      of Integer_4_Signed_C ;
    type Array_Integer_4_Signed
      is array(Positive range <>)
      of Integer_4_Signed;
    type Array_Integer_4_Natural
      is array(Positive range <>)
      of Integer_4_Natural;
    type Array_Integer_4_Positive
      is array(Positive range <>)
      of Integer_4_Positive;
    type Array_Integer_8_Unsigned
      is array(Positive range <>)
      of Integer_8_Unsigned;
    type Array_Integer_8_Unsigned_C
      is array(Positive range <>)
      of Integer_8_Unsigned_C;
    type Array_Integer_8_Signed
      is array(Positive range <>)
      of Integer_8_Signed;
    type Array_Integer_8_Signed_C
      is array(Positive range <>)
      of Integer_8_Signed_C;
    type Array_Integer_8_Natural
      is array(Positive range <>)
      of Integer_8_Natural;
    type Array_Integer_8_Positive
      is array(Positive range <>)
      of Integer_8_Positive;
    type Array_Integer_Size_C
      is array(Positive range <>)
      of Integer_Size_C;
    type Array_Access_Address
      is array(Positive range <>)
      of Access_Address;
    type Array_Float_8_Real_C
      is array(Positive range <>)
      of Float_8_Real_C;
    type Array_Float_8_Real
      is array(Positive range <>)
      of Float_8_Real;
    type Array_Float_8_Natural
      is array(Positive range <>)
      of Float_8_Natural;
    type Array_Float_8_Positive
      is array(Positive range <>)
      of Float_8_Positive;
    type Array_Float_8_Percent
      is array(Positive range <>)
      of Float_8_Percent;
    type Array_Float_8_Degree
      is array(Positive range <>)
      of Float_8_Degree;
    type Array_Float_4_Real_C
      is array(Positive range <>)
      of Float_4_Real_C;
    type Array_Float_4_Real
      is array(Positive range <>)
      of Float_4_Real;
    type Array_Float_4_Natural
      is array(Positive range <>)
      of Float_4_Natural;
    type Array_Float_4_Positive
      is array(Positive range <>)
      of Float_4_Positive;
    type Array_Float_4_Percent
      is array(Positive range <>)
      of Float_4_Percent;
    type Array_Float_4_Degree
      is array(Positive range <>)
      of Float_4_Degree;
  ---------------------
  -- Array Accessors --
  ---------------------
    type Access_Array_Address
      is access all Array_Address;
    type Access_Array_Access_String_2
      is access all Array_Access_String_2;
    type Access_Array_Character_1_C
      is access all Array_Character_1_C;
    type Access_Array_Character_1
      is access all Array_Character_1;
    type Access_Array_Character_2_C
      is access all Array_Character_2_C;
    type Access_Array_Character_2
      is access all Array_Character_2;
    type Access_Array_Integer_1_Unsigned_C
      is access all Array_Integer_1_Unsigned_C;
    type Access_Array_Integer_1_Unsigned
      is access all Array_Integer_1_Unsigned;
    type Access_Array_Integer_1_Signed_C
      is access all Array_Integer_1_Signed_C;
    type Access_Array_Integer_1_Signed
      is access all Array_Integer_1_Signed;
    type Access_Array_Integer_1_Natural
      is access all Array_Integer_1_Natural;
    type Access_Array_Integer_1_Positive
      is access all Array_Integer_1_Positive;
    type Access_Array_Integer_2_Unsigned_C
      is access all Array_Integer_2_Unsigned_C;
    type Access_Array_Integer_2_Unsigned
      is access all Array_Integer_2_Unsigned;
    type Access_Array_Integer_2_Signed_C
      is access all Array_Integer_2_Signed_C;
    type Access_Array_Integer_2_Signed
      is access all Array_Integer_2_Signed;
    type Access_Array_Integer_2_Natural
      is access all Array_Integer_2_Natural;
    type Access_Array_Integer_2_Positive
      is access all Array_Integer_2_Positive;
    type Access_Array_Integer_4_Unsigned_C
      is access all Array_Integer_4_Unsigned_C;
    type Access_Array_Integer_4_Unsigned
      is access all Array_Integer_4_Unsigned;
    type Access_Array_Integer_4_Signed_C 
      is access all Array_Integer_4_Signed_C;
    type Access_Array_Integer_4_Signed
      is access all Array_Integer_4_Signed;
    type Access_Array_Integer_4_Natural
      is access all Array_Integer_4_Natural;
    type Access_Array_Integer_4_Positive
      is access all Array_Integer_4_Positive;
    type Access_Array_Integer_8_Unsigned
      is access all Array_Integer_8_Unsigned;
    type Access_Array_Integer_8_Unsigned_C
      is access all Array_Integer_8_Unsigned_C;
    type Access_Array_Integer_8_Signed
      is access all Array_Integer_8_Signed;
    type Access_Array_Integer_8_Signed_C
      is access all Array_Integer_8_Signed_C;
    type Access_Array_Integer_8_Natural
      is access all Array_Integer_8_Natural;
    type Access_Array_Integer_8_Positive
      is access all Array_Integer_8_Positive;
    type Access_Array_Integer_Size_C
      is access all Array_Integer_Size_C;
    type Access_Array_Access_Address
      is access all Array_Access_Address;
    type Access_Array_Float_8_Real_C
      is access all Array_Float_8_Real_C;
    type Access_Array_Float_8_Real
      is access all Array_Float_8_Real;
    type Access_Array_Float_8_Natural
      is access all Array_Float_8_Natural;
    type Access_Array_Float_8_Positive
      is access all Array_Float_8_Positive;
    type Access_Array_Float_8_Percent
      is access all Array_Float_8_Percent;
    type Access_Array_Float_8_Degree
      is access all Array_Float_8_Degree;
    type Access_Array_Float_4_Real_C
      is access all Array_Float_4_Real_C;
    type Access_Array_Float_4_Real
      is access all Array_Float_4_Real;
    type Access_Array_Float_4_Natural
      is access all Array_Float_4_Natural;
    type Access_Array_Float_4_Positive
      is access all Array_Float_4_Positive;
    type Access_Array_Float_4_Percent
      is access all Array_Float_4_Percent;
    type Access_Array_Float_4_Degree
      is access all Array_Float_4_Degree;
  ---------------
  -- Constants --
  ---------------
    C_TRUE           : constant Integer_4_Signed_C := 1;
    C_FALSE          : constant Integer_4_Signed_C := 0;
    NULL_CHARACTER_1 : constant Character_1        := Character_1'Val(0);
    NULL_CHARACTER_2 : constant Character_2        := Character_2'Val(0);
    NULL_STRING_1    : constant String_1           := "" & NULL_CHARACTER_1;
    NULL_STRING_2    : constant String_2           := "" & NULL_CHARACTER_2;
    END_LINE         : constant String_1(1..2)     := Ascii.CR & Ascii.LF;
  -----------------
  -- Subprograms --
  -----------------
    procedure Finalize
      is new Ada.Unchecked_Deallocation(String_2, Access_String_2);
    function To_Unchecked_Address
      is new Ada.Unchecked_Conversion(Integer_4_Unsigned_C, Address);
    function To_Unchecked_Address  
      is new Ada.Unchecked_Conversion(Integer_4_Signed, Address);
    function To_Unchecked_Address
      is new Ada.Unchecked_Conversion(Integer_4_Unsigned, Address);
    function To_Unchecked_Access_Character_1_C
      is new Ada.Unchecked_Conversion(Address, Access_Character_1_C);
    function To_Unchecked_Access_Character_2_C
      is new Ada.Unchecked_Conversion(Address, Access_Character_2_C);
    function To_Unchecked_Access_Constant_Character_1_C
      is new Ada.Unchecked_Conversion(Address, Access_Constant_Character_1_C);
    function To_Unchecked_Access_Constant_Character_2_C
      is new Ada.Unchecked_Conversion(Address, Access_Constant_Character_2_C);
    function To_Unchecked_Access_Constant_Character_2_C
      is new Ada.Unchecked_Conversion(Integer_4_Unsigned, Access_Constant_Character_2_C);
    function To_Unchecked_Access_Integer_2_Unsigned_C
      is new Ada.Unchecked_Conversion(Address, Access_Integer_2_Unsigned_C);
    function To_Unchecked_Access_Integer_2_Unsigned_C
      is new Ada.Unchecked_Conversion(Integer_4_Unsigned, Access_Integer_2_Unsigned_C);
    function To_Unchecked_Access_Integer_4_Unsigned
      is new Ada.Unchecked_Conversion(Address, Access_Integer_4_Unsigned);
    function To_Unchecked_Float_4_Real
      is new Ada.Unchecked_Conversion(Float_4_Real, Integer_4_Unsigned);
    function To_Unchecked_Integer_2_Signed
      is new Ada.Unchecked_Conversion(Integer_2_Unsigned, Integer_2_Signed);
    function To_Unchecked_Integer_4_Signed_C     
      is new Ada.Unchecked_Conversion(Address, Integer_4_Signed_C);
    function To_Unchecked_Integer_4_Signed
      is new Ada.Unchecked_Conversion(Address, Integer_4_Signed);
    function To_Unchecked_Integer_4_Signed
      is new Ada.Unchecked_Conversion(Integer_4_Unsigned, Integer_4_Signed);
    function To_Unchecked_Integer_4_Unsigned_C
      is new Ada.Unchecked_Conversion(Address, Integer_4_Unsigned_C);
    function To_Unchecked_Integer_4_Unsigned
      is new Ada.Unchecked_Conversion(Address, Integer_4_Unsigned);
    function To_Unchecked_Integer_4_Unsigned
      is new Ada.Unchecked_Conversion(Access_Integer_2_Unsigned_C, Integer_4_Unsigned);
    function To_Unchecked_Integer_4_Unsigned
      is new Ada.Unchecked_Conversion(Access_Constant_Character_2_C, Integer_4_Unsigned);
    function To_Unchecked_Integer_4_Unsigned
      is new Ada.Unchecked_Conversion(Integer_4_Signed_C, Integer_4_Unsigned);
    function To_Unchecked_Integer_4_Unsigned
      is new Ada.Unchecked_Conversion(Float_4_Real, Integer_4_Unsigned);
    function To_Integer_4_Signed_C(
      Item : in String_1_C) 
      return Integer_4_Signed_C;
    function To_Access_Character_1_C(
      Item : in String_1_C) 
      return Access_Character_1_C;
    function To_Access_Character_2_C(
      Item : in String_2)
      return Access_Character_2_C;
    function To_Access_Constant_Character_1_C(
      Item : in String_1_C) 
      return Access_Constant_Character_1_C;
    function To_Access_Constant_Character_2_C(
      Item : in String_2)
      return Access_Constant_Character_2_C;
    function To_String_1_C(
      Item : in String_1) 
      return String_1_C;
    function To_String_1_C(
      Item : in String_2) 
      return String_1_C;
    function To_String_1(
      Item : in String_1_C) 
      return String_1;
    function To_String_1(
      Item : in String_2) 
      return String_1;
    function To_String_2_C(
      Item : in String_2)
      return String_2_C;
    function To_String_2(
      Item : in String_1_C)
      return String_2;      
    function To_String_2(
      Item : in String_1)
      return String_2;
    function To_String_2(
      Item : in String_2_C)
      return String_2;
    function To_String_2(
      Item : in Access_Constant_Character_2_C)
      return String_2;
    function Is_High_Order_Byte_First
      return Boolean;
    function Is_Low_Order_Byte_First
      return Boolean;
    function Is_Big_Endian
      return Boolean
      renames Is_High_Order_Byte_First;
    function Is_Little_Endian
      return Boolean
      renames Is_Low_Order_Byte_First;
-------
private
-------
  -------------
  -- Records --
  -------------
    type Record_Endian_Test(
      Do_Initialize_To_Split_Default_Over_Unsplit_Default : Boolean := True)
      is record
        case Do_Initialize_To_Split_Default_Over_Unsplit_Default is
          when True =>
            Split : Array_Integer_1_Unsigned(1..2) := (1, 0);
          when False =>
            Unsplit : Integer_2_Unsigned;
        end case;
      end record;
      pragma Unchecked_Union(Record_Endian_Test);
      for Record_Endian_Test'Size
        use 2*8;
  ---------------
  -- Variables --
  ---------------
    Endian_Test : Record_Endian_Test;
  ---------------
  -- Constants --
  ---------------
    DOES_MACHINE_ORDER_LOW_BYTE_FIRST : constant Boolean := Endian_Test.Unsplit = 1;
  end Neo.Foundation.Data_Types;
