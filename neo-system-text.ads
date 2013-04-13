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
  Ada.Finalization,
  Neo.Foundation.Text_IO,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Package_Testing;
use
  Ada.Finalization,
  Neo.Foundation.Text_IO,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Package_Testing;
package Neo.System.Text
  is
  -----------
  -- Types --
  -----------
    type Record_String_UTF8
      is private;
  ----------------
  -- Suprograms --
  ----------------
    procedure Test;
    function Get(
      Item  : in Record_String_UTF8;
      Index : in Integer_4_Positive)
      return Character_2;
    function Get_Length(
      Item : in Record_String_UTF8)
      return Integer_4_Positive;
    function Get_Clipboard
      return String_2;
    procedure Set_Clipboard(
      Text : in String_2);
    function Create_Media_Name(
      Item : in String_1)
      return String_1;
    function Create_Media_Name(
      Item : in String_1)
      return String_2;
    function Create_Media_Name(
      Item : in String_2)
      return String_1;
    function Create_Media_Name(
      Item : in String_2)
      return String_2;
    function To_Lower(
      Item : in Character_1)
      return Character_1;
    function To_Lower(
      Item : in Character_1)
      return Character_2;
    function To_Lower(
      Item : in Character_2)
      return Character_1;
    function To_Lower(
      Item : in Character_2)
      return Character_2;
    function To_Lower(
      Item : in String_1)
      return String_1;
    function To_Lower(
      Item : in String_1)
      return String_2;
    function To_Lower(
      Item : in String_2)
      return String_1;
    function To_Lower(
      Item : in String_2)
      return String_2;
    procedure To_Lower(
      Item : in out Character_1);
    procedure To_Lower(
      Item : in out Character_2);
    procedure To_Lower(
      Item : in out String_1);
    procedure To_Lower(
      Item : in out String_2);
    function Is_Lower(
      Item : in Character_1)
      return Boolean;
    function Is_Lower(
      Item : in Character_2)
      return Boolean;
    function Is_Lower(
      Item : in String_1)
      return Boolean;
    function Is_Lower(
      Item : in String_2)
      return Boolean;
    function To_Upper(
      Item : in Character_1)
      return Character_2;
    function To_Upper(
      Item : in Character_2)
      return Character_1;
    function To_Upper(
      Item : in Character_2)
      return Character_2;
    function To_Upper(
      Item : in String_1)
      return String_2;
    function To_Upper(
      Item : in String_2)
      return String_1;
    function To_Upper(
      Item : in String_2)
      return String_2;
    procedure To_Upper(
      Item : in out Character_1);
    procedure To_Upper(
      Item : in out Character_2);
    procedure To_Upper(
      Item : in out String_1);
    procedure To_Upper(
      Item : in out String_2);
    function Is_Upper(
      Item : in Character_1)
      return Boolean;
    function Is_Upper(
      Item : in Character_2)
      return Boolean;
    function Is_Upper(
      Item : in String_1)
      return Boolean;
    function Is_Upper(
      Item : in String_2)
      return Boolean;
    function Is_Alphabetic(
      Item : in Character_1)
      return Boolean;
    function Is_Alphabetic(
      Item : in Character_2)
      return Boolean;
    function Is_Alphabetic(
      Item : in String_1)
      return Boolean;
    function Is_Alphabetic(
      Item : in String_2)
      return Boolean;
    function Is_Alphanumeric(
      Item : in Character_1)
      return Boolean;
    function Is_Alphanumeric(
      Item : in Character_2)
      return Boolean;
    function Is_Alphanumeric(
      Item : in String_1)
      return Boolean;
    function Is_Alphanumeric(
      Item : in String_2)
      return Boolean;
    function Is_Letter(
      Item : in Character_1)
      return Boolean;
      renames Is_Alphabetic;
    function Is_Letter(
      Item : in Character_2)
      return Boolean;
      renames Is_Alphabetic;
    function Is_Punctuation(
      Item : in Character_1)
      return Boolean;
    function Is_Punctuation(
      Item : in Character_2)
      return Boolean;
    function Is_Punctuation(
      Item : in String_1)
      return Boolean;
    function Is_Punctuation(
      Item : in String_2)
      return Boolean;
    function Is_Printable(
      Item : in Character_1)
      return Boolean;
    function Is_Printable(
      Item : in Character_2)
      return Boolean;
    function Is_Printable(
      Item : in String_1)
      return Boolean;
    function Is_Printable(
      Item : in String_2)
      return Boolean;
    function Is_White_Space(
      Item : in Character_1)
      return Boolean;
    function Is_White_Space(
      Item : in Character_2)
      return Boolean;
    function Is_White_Space(
      Item : in String_1)
      return Boolean;
    function Is_White_Space(
      Item : in String_2)
      return Boolean;
    function Is_Control(
      Item : in Character_1)
      return Boolean;
    function Is_Control(
      Item : in Character_2)
      return Boolean;
    function Is_Control(
      Item : in String_1)
      return Boolean;
    function Is_Control(
      Item : in String_2)
      return Boolean;
    function Is_Hexadecimal_Digit(
      Item : in Character_1) 
      return Boolean;
    function Is_Hexadecimal_Digit(
      Item : in Character_2) 
      return Boolean;
    function Is_Hexadecimal_Digit(
      Item : in String_1) 
      return Boolean;
    function Is_Hexadecimal_Digit(
      Item : in String_2) 
      return Boolean;
    function Is_Decimal_Digit(
      Item : in Character_1) 
      return Boolean;
    function Is_Decimal_Digit(
      Item : in Character_2) 
      return Boolean
    function Is_Decimal_Digit(
      Item : in String_1) 
      return Boolean
    function Is_Decimal_Digit(
      Item : in String_2) 
      return Boolean
    function Is_Digit(
      Item : in Character_1) 
      return Boolean
      renames Is_Decimal_Digit;
    function Is_Digit(
      Item : in Character_2) 
      return Boolean
      renames Is_Decimal_Digit;
    function Is_Digit(
      Item : in String_1) 
      return Boolean
      renames Is_Decimal_Digit;
    function Is_Digit(
      Item : in String_2) 
      return Boolean
      renames Is_Decimal_Digit;
-------
private
-------
  ---------------
  -- Constants --
  ---------------
    MEDIA_NAME_NON_CONFORMANT             : constant String_2    := "|:\";
    MEDIA_NAME_NON_CONFORMANT_REPLACEMENT : constant Character_1 := '/';
    MEDIA_NAME_EXTENSION_SEPORATOR        : constant Character_1 := '.';
    REPLACEMENT_FOR_EXTENDED_CHARACTER    : constant Character_1 := '~'; 
  --------------------
  -- Implementation --
  --------------------
    package Implementation
      is
        procedure Set_Clipboard(
          Text : in String_2);
        function Get_Clipboard
          return String_2;
        function To_Lower(
          Item : in Character_2)
          return Character_2;
        function To_Upper(
          Item : in Character_2)
          return Character_2;
        function Is_Lower(
          Item : in Character_2)
          return Boolean;
        function Is_Upper(
          Item : in Character_2)
          return Boolean;
        function Is_Printable(
          Item : in Character_2)
          return Boolean;
        function Is_White_Space(
          Item : in Character_2)
          return Boolean;
        function Is_Control(
          Item : in Character_2);
        function Is_Alphabetic(
          Item : in Character_2)
          return Boolean;
        function Is_Alphanumeric(
          Item : in Character_2)
          return Boolean;
        function Is_Punctuation(
          Item : in Character_2)
          return Boolean;
      end Implementation;
    package body Implementation 
      is separate;
  end Neo.System.Text;
