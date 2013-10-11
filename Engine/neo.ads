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
  Ada.Calendar,
  Ada.Exceptions,
  Ada.Text_IO,
  Ada.Wide_Text_IO,
  Ada.Streams,
  Ada.Strings.Fixed,
  Ada.Strings.Wide_Fixed,
  Ada.Strings.Wide_Unbounded,
  Ada.Strings.Wide_Unbounded.Wide_Hash,
  Ada.Unchecked_Conversion,
  Ada.Unchecked_Deallocation,
  Ada.Containers.Hashed_Maps;
use
  System,
  Interfaces,
  Interfaces.C,
  Ada.Calendar,
  Ada.Exceptions,
  --Ada.Text_IO,
  --Ada.Wide_Text_IO,
  Ada.Streams,
  Ada.Strings,
  Ada.Strings.Fixed,
  Ada.Strings.Wide_Fixed,
  Ada.Strings.Wide_Unbounded;
package Neo
  is
  ----------------
  -- Directives --
  ----------------
    pragma Suppress(Elaboration_Check);
  ----------------
  -- Exceptions --
  ----------------
    Unimplemented_Feature                                      : Exception;
    Timer_Started_Without_Being_Stopped                        : Exception;
    Timer_Stopped_Without_Being_Started                        : Exception;
    Title_Is_Too_Long_To_Fit_On_A_Single_Line                  : Exception;
    Attempted_To_Decode_Non_Valid_Base_64_Digit_To_Data_Stream : Exception;
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
    subtype String_2_Unbounded
      is Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
  ----------------
  -- Characters --
  ----------------
    subtype Character_1_C
      is Interfaces.C.Char;
    subtype Character_1
      is Character;
    subtype Character_2_C
      is Interfaces.C.WChar_T;
    subtype Character_2
      is Wide_Character;
  -------------
  -- Numbers --
  -------------
    type Integer_Address
      is mod MEMORY_SIZE;
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
      range 1..Integer_1_Unsigned'last;
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
      range 1..Integer_1_Unsigned'last;
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
      range 1..Integer_8_Unsigned'last;
    subtype Integer_Size_C
      is Interfaces.C.Size_T;
    subtype Float_8_Real_C
      is Interfaces.C.Double;
    subtype Float_8_Real
      is Long_Float;
    subtype Float_8_Natural
      is Float_8_Real
      range 0.0..Float_8_Real'last;
    subtype Float_8_Positive
      is Float_8_Real
      range 1.0..Float_8_Real'last;
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
      range 0.0..Float_4_Real'last;
    subtype Float_4_Positive
      is Float_4_Real
      range 1.0..Float_4_Real'last;
    subtype Float_4_Percent
      is Float_4_Real
      range 0.0..100.0;
    subtype Float_4_Degree
      is Float_4_Real
      range 1.0..360.0;
    subtype Byte
      is Integer_1_Unsigned;
  ---------------
  -- Accessors --
  ---------------
    type Access_Function_Localize
      is access function(
        Item : in String_2)
        return String_2;
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
    type Access_Integer_Address
      is access all Integer_Address;
    type Access_Integer_8_Unsigned_C
      is access all Integer_8_Unsigned_C;
    type Access_Integer_4_Unsigned_C
      is access all Integer_4_Unsigned_C;
    type Access_Integer_4_Unsigned
      is access all Integer_4_Unsigned;
    type Access_Integer_4_Signed_C
      is access all Integer_4_Signed_C;
    type Access_Integer_2_Unsigned_C
      is access all Integer_2_Unsigned_C;
    type Access_Integer_1_Unsigned_C
      is access all Integer_1_Unsigned_C;
  ------------------------
  -- Not Null Accessors --
  ------------------------
    type Not_Null_Access_Procedure_Put
      is not null access procedure(
        Item : in String_2);
    type Not_Null_Access_String_2
      is not null access all String_2;
    type Not_Null_Access_Constant_String_2
      is not null access constant String_2;
    type Not_Null_Access_Stream_Element_Array
      is not null access all Stream_Element_Array;
  ------------
  -- Arrays --
  ------------
    type Array_Duration
      is array(Positive range <>)
      of Duration;
    type Array_Address
      is array(Positive range <>)
      of Address;
    type Array_Access_String_2
      is array(Positive range <>)
      of Access_String_2;
    type Array_String_2_Unbounded
      is array(Positive range <>)
      of String_2_Unbounded;
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
    type Not_Null_Access_Array_Not_Null_Access_String_2
      is not null access Not_Null_Access_String_2;
  ---------------
  -- Protected --
  ---------------
    protected type Protected_Status
      is
        function Is_Doing_Something
          return Boolean;
        procedure Set_Is_Doing_Something(
          New_Status : in Boolean);
      private
        Status : Boolean := False;
      end Protected_Status;
  -------------
  -- Records --
  -------------
    type Record_Color
      is record
        Red   : Integer_1_Unsigned := 16#FF#;
        Green : Integer_1_Unsigned := 16#FF#;
        Blue  : Integer_1_Unsigned := 16#FF#;
      end record;
    type Record_Timer
      is private;
  -----------------
  -- Subprograms --
  -----------------
    procedure Test;
    procedure Hang(
      Hang_Time : in Integer_4_Signed := 3);
    --
    -- Timer
    --
    procedure Start(
      Timer : in out Record_Timer);
    procedure Stop(
      Timer : in out Record_Timer);
    function Get_Duration(
      Timer : in Record_Timer)
      return Duration;
    --
    -- Input and output
    --
    function Localize(
      Item : in String_2)
      return String_2;
    procedure Put(
      Item : in Character_2);
    procedure Put(
      Item : in String_2);
    procedure Put_Title(
      Title : in String_2);
    procedure Put_Line(
      Item : in String_2);
    procedure Put_Debug(
      Item : in Character_2);
    procedure Put_Debug(
      Item : in String_2);
    procedure Put_Debug_Line(
      Item : in String_2);
    procedure New_Line(
      Lines : in Integer_4_Positive := 1);
    function Get_Line_Size
      return Integer_4_Positive;
    function Get_Log
      return String_2;
    function Get_Input_Entry
      return String_2;
    function Get_Error
      return String_2;
    function Get_Extension(
      Path : in String_2)
      return String_2;
    procedure Set_Error(
      Text : in String_2);
    procedure Set_Input_Entry(
      Item : in String_2);
    procedure Set_Do_Put_Debug(
      Do_Put_Debug : in Boolean);
    procedure Set_Line_Size(
      Line_Size : in Integer_4_Positive);
    procedure Set_Put(
      Put : in Not_Null_Access_Procedure_Put);
    generic
      type Type_Number
        is mod <>;
    function To_Radian_Image(
      Item    : in Type_Number;
      Base    : in Ada.Wide_Text_IO.Number_Base;
      Spacing : in Integer_4_Natural := 0) -- To do yet
      return String_2;
    --
    -- String manipulation
    --
--    function To_Stream(
--      Data : in String_1)
--      return Stream_Element_Array;
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
    function To_String_2_Unbounded(
      Item : in String_2)
      return String_2_Unbounded
      renames Ada.Strings.Wide_Unbounded.To_Unbounded_Wide_String;
    function To_String_2(
      Item : in String_2_Unbounded)
      return String_2
      renames Ada.Strings.Wide_Unbounded.To_Wide_String;
    function To_String_2(
      Item : in Access_Constant_Character_2_C)
      return String_2;
    function To_Access_Character_1_C(
      Item : in String_1_C)
      return Access_Character_1_C;
    function To_Access_Character_2_C(
      Item : in String_2)
      return Access_Character_2_C;
    function To_Access_Constant_Character_1_C(
      Item : in String_1)
      return Access_Constant_Character_1_C;
    function To_Access_Constant_Character_2_C(
      Item : in String_2)
      return Access_Constant_Character_2_C;
    --
    -- Endian conversion
    --
    function To_Low_Order_Byte_First(
      Item : in Stream_Element_Array)
      return Stream_Element_Array;
    function To_Low_Order_Byte_Then_Low_Order_Bit_First(
      Item : in Stream_Element_Array)
      return Stream_Element_Array;
    function To_Low_Order_Byte_Then_High_Order_Bit_First(
      Item : in Stream_Element_Array)
      return Stream_Element_Array;
    function To_High_Order_Byte_First(
      Item : in Stream_Element_Array)
      return Stream_Element_Array;
    function To_High_Order_Byte_Then_Low_Order_Bit_First(
      Item : in Stream_Element_Array)
      return Stream_Element_Array;
    function To_High_Order_Byte_Then_High_Order_Bit_First(
      Item : in Stream_Element_Array)
      return Stream_Element_Array;
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
    --
    -- Unchecked conversion
    --
    function To_Unchecked_Address
      is new Ada.Unchecked_Conversion(Integer_Address, Address);
    function To_Unchecked_Address
      is new Ada.Unchecked_Conversion(Access_Constant_Character_2_C, Address);
    function To_Unchecked_Access_Character_1_C
      is new Ada.Unchecked_Conversion(Address, Access_Character_1_C);
    function To_Unchecked_Access_Character_2_C
      is new Ada.Unchecked_Conversion(Address, Access_Character_2_C);
    function To_Unchecked_Access_Constant_Character_1_C
      is new Ada.Unchecked_Conversion(Address, Access_Constant_Character_1_C);
    function To_Unchecked_Access_Constant_Character_2_C
      is new Ada.Unchecked_Conversion(Address, Access_Constant_Character_2_C);
    function To_Unchecked_Access_Constant_Character_2_C
      is new Ada.Unchecked_Conversion(Integer_Address, Access_Constant_Character_2_C);
    function To_Unchecked_Access_Integer_2_Unsigned_C
      is new Ada.Unchecked_Conversion(Address, Access_Integer_2_Unsigned_C);
    function To_Unchecked_Access_Integer_2_Unsigned_C
      is new Ada.Unchecked_Conversion(Integer_Address, Access_Integer_2_Unsigned_C);
    function To_Unchecked_Integer_4_Signed_C
      is new Ada.Unchecked_Conversion(Integer_Address, Integer_4_Signed_C);
    function To_Unchecked_Integer_4_Unsigned_C
      is new Ada.Unchecked_Conversion(Integer_Address, Integer_4_Unsigned_C);
    function To_Unchecked_Integer_4_Unsigned_C
      is new Ada.Unchecked_Conversion(Integer_4_Signed_C, Integer_4_Unsigned_C);
    function To_Unchecked_Access_Integer_4_Unsigned
      is new Ada.Unchecked_Conversion(Address, Access_Integer_4_Unsigned);
    function To_Unchecked_Float_4_Real
      is new Ada.Unchecked_Conversion(Float_4_Real, Integer_4_Unsigned);
    function To_Unchecked_Integer_2_Signed
      is new Ada.Unchecked_Conversion(Integer_2_Unsigned, Integer_2_Signed);
    function To_Unchecked_Integer_Address
      is new Ada.Unchecked_Conversion(Address, Integer_Address);
    function To_Unchecked_Integer_Address
      is new Ada.Unchecked_Conversion(Access_Integer_2_Unsigned_C, Integer_Address);
    function To_Unchecked_Integer_Address
      is new Ada.Unchecked_Conversion(Access_Constant_Character_2_C, Integer_Address);
    function To_Unchecked_Integer_Address
      is new Ada.Unchecked_Conversion(Integer_4_Signed_C, Integer_Address);
    function To_Unchecked_Integer_Address
      is new Ada.Unchecked_Conversion(Integer_4_Unsigned_C, Integer_Address);
    function To_Unchecked_Integer_4_Signed
      is new Ada.Unchecked_Conversion(Integer_4_Unsigned, Integer_4_Signed);
    function To_Unchecked_Integer_4_Address
      is new Ada.Unchecked_Conversion(Access_Integer_2_Unsigned_C, Integer_Address);
    function To_Unchecked_Integer_4_Address
      is new Ada.Unchecked_Conversion(Access_Constant_Character_2_C, Integer_Address);
    function To_Unchecked_Integer_4_Unsigned
      is new Ada.Unchecked_Conversion(Integer_4_Signed_C, Integer_4_Unsigned);
    function To_Unchecked_Integer_4_Unsigned
      is new Ada.Unchecked_Conversion(Float_4_Real, Integer_4_Unsigned);
  ---------------
  -- Constants --
  ---------------
    START_TIME              : constant Time               := Clock;
    CHARACTER_2_REPLACEMENT : constant Character_1        := '~';
    COLOR_BLACK             : constant Record_Color       := (16#00#, 16#00#, 16#00#);
    COLOR_WHITE             : constant Record_Color       := (16#FF#, 16#FF#, 16#FF#);
    COLOR_RED               : constant Record_Color       := (16#FF#, 16#00#, 16#00#);
    COLOR_GREEN             : constant Record_Color       := (16#00#, 16#FF#, 16#00#);
    COLOR_BLUE              : constant Record_Color       := (16#00#, 16#00#, 16#FF#);
    COLOR_YELLOW            : constant Record_Color       := (16#FF#, 16#FF#, 16#00#);
    COLOR_ORANGE            : constant Record_Color       := (16#FF#, 16#A5#, 16#00#);
    COLOR_VIOLET            : constant Record_Color       := (16#EE#, 16#82#, 16#EE#);
    COLOR_PURPLE            : constant Record_Color       := (16#80#, 16#00#, 16#80#);
    COLOR_PINK              : constant Record_Color       := (16#FF#, 16#C0#, 16#CB#);
    COLOR_AQUA              : constant Record_Color       := (16#00#, 16#FF#, 16#FF#);
    COLOR_SKY_BLUE          : constant Record_Color       := (16#87#, 16#CE#, 16#EB#);
    COLOR_LIGHT_BLUE        : constant Record_Color       := (16#AD#, 16#D8#, 16#E6#);
    COLOR_MAGENTA           : constant Record_Color       := (16#FF#, 16#00#, 16#FF#);
    COLOR_IVORY             : constant Record_Color       := (16#FF#, 16#FF#, 16#F0#);
    COLOR_BEIGE             : constant Record_Color       := (16#F5#, 16#F5#, 16#DC#);
    COLOR_WHEAT             : constant Record_Color       := (16#F5#, 16#DE#, 16#B3#);
    COLOR_TAN               : constant Record_Color       := (16#D2#, 16#B4#, 16#8C#);
    COLOR_KHAKI             : constant Record_Color       := (16#C3#, 16#B0#, 16#91#);
    COLOR_SILVER            : constant Record_Color       := (16#C0#, 16#C0#, 16#C0#);
    COLOR_GRAY              : constant Record_Color       := (16#80#, 16#80#, 16#80#);
    COLOR_CHARCOAL          : constant Record_Color       := (16#46#, 16#46#, 16#46#);
    COLOR_NAVY_BLUE         : constant Record_Color       := (16#00#, 16#00#, 16#80#);
    COLOR_ROYAL_BLUE        : constant Record_Color       := (16#08#, 16#4C#, 16#9E#);
    COLOR_CYAN              : constant Record_Color       := (16#00#, 16#FF#, 16#FF#);
    COLOR_AQUAMARINE        : constant Record_Color       := (16#7F#, 16#FF#, 16#D4#);
    COLOR_TEAL              : constant Record_Color       := (16#00#, 16#80#, 16#80#);
    COLOR_FOREST_GREEN      : constant Record_Color       := (16#22#, 16#8B#, 16#22#);
    COLOR_OLIVE             : constant Record_Color       := (16#80#, 16#80#, 16#00#);
    COLOR_CHARTREUSE        : constant Record_Color       := (16#7F#, 16#FF#, 16#00#);
    COLOR_LIME              : constant Record_Color       := (16#BF#, 16#FF#, 16#00#);
    COLOR_GOLDEN            : constant Record_Color       := (16#FF#, 16#D7#, 16#00#);
    COLOR_GOLDENROD         : constant Record_Color       := (16#DA#, 16#A5#, 16#20#);
    COLOR_CORAL             : constant Record_Color       := (16#FF#, 16#7F#, 16#50#);
    COLOR_SALMON            : constant Record_Color       := (16#FA#, 16#80#, 16#72#);
    COLOR_HOT_PINK          : constant Record_Color       := (16#FC#, 16#0F#, 16#C0#);
    COLOR_FUCHSIA           : constant Record_Color       := (16#FF#, 16#77#, 16#FF#);
    COLOR_PUCE              : constant Record_Color       := (16#CC#, 16#88#, 16#99#);
    COLOR_MAUVE             : constant Record_Color       := (16#E0#, 16#B0#, 16#FF#);
    COLOR_LAVENDER          : constant Record_Color       := (16#B5#, 16#7E#, 16#DC#);
    COLOR_PLUM              : constant Record_Color       := (16#84#, 16#31#, 16#79#);
    COLOR_INDIGO            : constant Record_Color       := (16#4B#, 16#00#, 16#82#);
    COLOR_MAROON            : constant Record_Color       := (16#80#, 16#00#, 16#00#);
    COLOR_CRIMSON           : constant Record_Color       := (16#DC#, 16#14#, 16#3C#);
    NULL_CHARACTER_1        : constant Character_1        := ASCII.NUL;
    NULL_CHARACTER_2        : constant Character_2        := Character_2'val(Character_1'pos(NULL_CHARACTER_1));
    NULL_CHARACTER_1_C      : constant Character_1_C      := Interfaces.C.NUL;
    NULL_CHARACTER_2_C      : constant Character_2_C      := Character_2_C'val(Character_1_C'pos(NULL_CHARACTER_1_C));
    NULL_STRING_1           : constant String_1           := "";
    NULL_STRING_2           : constant String_2           := "";
    NULL_STRING_2_UNBOUNDED : constant String_2_Unbounded := To_String_2_Unbounded(NULL_STRING_2);
    END_LINE_1              : constant String_1           := ASCII.CR & ASCII.LF;
    END_LINE_2              : constant String_2           := To_String_2(END_LINE_1);
    C_TRUE                  : constant Integer_4_Signed_C := 1;
    C_FALSE                 : constant Integer_4_Signed_C := 0;
    WORD_SIZE_IMAGE         : constant String_1           :=( -- Trim(Integer'image(WORD_SIZE), Both); -- Must be static expression for Linker_Options pragma
      case WORD_SIZE is
        when 8      => "8",
        when 16     => "16",
        when 32     => "32",
        when 64     => "64",
        when others => "???");
  --------------
  -- Packages --
  --------------
    generic
      type Type_To_Hash
        is private;
    package Hashed_Maps
      is
        package Actual
          is new Ada.Containers.Hashed_Maps(
            Key_Type        => String_2_Unbounded,
            Element_Type    => Type_To_Hash,
            Hash            => Wide_Hash,
            Equivalent_Keys => "=");
        protected type Protected_Map
          is
            procedure Insert(
              Key      : in String_2;
              New_Item : in Type_To_Hash);
            procedure Replace(
              Key      : in String_2;
              New_Item : in Type_To_Hash);
            function Find(
              Key : in String_2)
              return Actual.Cursor;
            procedure Delete(
              Key : in String_2);
            function Element(
              Key : in String_2)
              return Type_To_Hash;
            function Has_Element(
              Key : in String_2)
              return Boolean;
          private
            Data : Actual.Map;
          end Protected_Map;
      end Hashed_Maps;
--    generic
--      type Type_To_Vector
--        is private;
--    package Vector
--      is
--        package Actual
--          is new Ada.Containers.Vector(
--            Equivalent_Keys => "=");
--        protected type Protected_Vector
--          is
--          private
--            Data : Actual.Vector;
--          end Protected_Vector;
--      end Vector;
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
      for Record_Endian_Test'size
        use 2 * Byte'size;
    type Record_Timer
      is record
        Start      : Time;
        Last       : Duration := 0.0;
        Is_Stopped : Boolean  := False;
      end record;
  -------------
  -- Numbers --
  -------------
    type Integer_Base64
      is mod 2**6
      with Size => 6;
  ------------
  -- Arrays --
  ------------
    type Array_Integer_Base64
      is array(Positive range <>)
      of Integer_Base64;
  ---------------
  -- Accessors --
  ---------------
    type Access_Array_Integer_Base64
      is access all Array_Integer_Base64;
  -----------------
  -- Subprograms --
  -----------------
--    function To_Not_Null_Access_Stream_Element_Array
--      is new Ada.Unchecked_Conversion(Access_Array_Integer_Base64, Not_Null_Access_Stream_Element_Array);
  ---------------
  -- Constants --
  ---------------
    DO_PUT_LOCALIZE_FAILURE           : constant Boolean            := False;
    ENDIAN_TEST                       : constant Record_Endian_Test := (others => <>);
    DOES_MACHINE_ORDER_LOW_BYTE_FIRST : constant Boolean            := ENDIAN_TEST.Unsplit = 1;
    HANG_DELAY                        : constant Duration           := 3.0;
    HANG_INDICATORS_DRAWN_PER_SECOND  : constant Float_4_Real       := 0.5;
    FAILED_LOCALIZE_PREVIEW_LENGTH    : constant Integer_4_Positive := 10;
    RADIAN_IMAGE_STRING_SIZE          : constant Integer_4_Positive := 256;
    DEFAULT_LINE_SIZE                 : constant Integer_4_Positive := 80;
    FAILED_SAVE_LOG                   : constant String_2           := "Failed to save log to path: ";
    HANG_INDICATOR                    : constant String_2           := "_";
    TESTING_INPUT_HANG_INCREMENT      : constant String_2           := ">";
    TESTING_SEPARATOR                 : constant Character_2        := '_';
    BASE64_TERMINATOR                 : constant Character_1        := '=';
    BASE64_ALPHABET                   : constant array(Integer_Base64'range) of Character_1 :=(
      'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
      'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z',
      '0','1','2','3','4','5','6','7','8','9','+','/');
  ---------------
  -- Protected --
  ---------------
    protected type Protected_Input_Output
      is
        procedure Put(
          Item : in String_2);
        function Localize(
          Item : in String_2)
          return String_2;
        function Do_Put_Debug
          return Boolean;
        function Get_Line_Size
          return Integer_4_Positive;
        function Get_Number_Of_Lines
          return Integer_4_Natural;
        function Get_Log
          return String_2;
        function Get_Error
          return String_2;
        function Get_Input_Entry
          return String_2;
        procedure Set_Input_Entry(
          Input_Entry : in String_2);
        procedure Set_Error(
          Error : in String_2);
        procedure Set_Do_Put_Debug(
          Do_Put_Debug : in Boolean);
        procedure Set_Line_Size(
          Line_Size : in Integer_4_Positive);
        procedure Set_Put(
          Put : in Not_Null_Access_Procedure_Put);
        procedure Set_Localize(
          Localize : in Access_Function_Localize);
      private
        Current_Localize     : Access_Function_Localize      := null;
        Current_Put          : Not_Null_Access_Procedure_Put := Ada.Wide_Text_IO.Put'access;
        Current_Input_Entry  : String_2_Unbounded            := NULL_STRING_2_UNBOUNDED;
        Current_Log          : String_2_Unbounded            := NULL_STRING_2_UNBOUNDED;
        Current_Error        : String_2_Unbounded            := NULL_STRING_2_UNBOUNDED;
        Current_Line_Size    : Integer_4_Positive            := DEFAULT_LINE_SIZE;
        Current_Line_Count   : Integer_4_Natural             := 0;
        Current_Do_Put_Debug : Boolean                       := False;
      end Protected_Input_Output;
  ---------------
  -- Variables --
  ---------------
    Input_Output : Protected_Input_Output;
  end Neo;
