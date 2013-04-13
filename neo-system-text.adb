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
package body Neo.System.Text
  is
  ----------
  -- Test --
  ----------
    procedure Test
      is
      begin
        Display_Title("CHARACTER TEST!");
        Put_Line(To_Lower("ABCD"));
        Hang_Window;
      end Test;
  -----------------------
  -- Create_Media_Name --
  -----------------------
    function Create_Media_Name(
      Item : in String_1)
      return String_1
      is
      begin
        return "";
      end Create_Media_Name;
    function Create_Media_Name(
      Item : in String_1)
      return String_2
      is
      begin
        return "";
      end Create_Media_Name;
    function Create_Media_Name(
      Item : in String_2)
      return String_1
      is
      begin
        return "";
      end Create_Media_Name;
    function Create_Media_Name(
      Item : in String_2)
      return String_2
      is
      begin
        return "";
      end Create_Media_Name;
  --------------
  -- To_Lower --
  --------------
    function To_Lower(
      Item : in Character_1)
      return Character_2
      is
      begin
        return To_Lower(To_Character_2(Item));
      end To_Lower;
    function To_Lower(
      Item : in Character_2)
      return Character_1
      is
      begin
        return To_Character_1(To_Lower(Item));
      end To_Lower;
    function To_Lower(
      Item : in Character_2)
      return Character_2
      renames Implementation.To_Lower;
    function To_Lower(
      Item : in String_1)
      return String_2
      is
      Result : String_2(Item'First..Item'Last) := (others => NULL_CHARACTER_2);
      begin
        for I in Result loop

        end loop;
      end To_Lower;
    function To_Lower(
      Item : in String_2)
      return String_1
      is
      begin
        return "";
      end To_Lower;
    function To_Lower(
      Item : in String_2)
      return String_2
      is
      begin
        return "";
      end To_Lower;
    procedure To_Lower(
      Item : in out Character_1)
      is
      begin
        null;
      end To_Lower;
    procedure To_Lower(
      Item : in out Character_2)
      is
      begin
        null;
      end To_Lower;
    procedure To_Lower(
      Item : in out String_1)
      is
      begin
        null;
      end To_Lower;
    procedure To_Lower(
      Item : in out String_2)
      is
      begin
        null;
      end To_Lower;
  --------------
  -- Is_Lower --
  --------------
    function Is_Lower(
      Item : in Character_1)
      return Boolean
      is
      begin
        return Is_Lower(To_Character_2(Item));
      end Is_Lower;
    function Is_Lower(
      Item : in Character_2)
      return Boolean
      is
      begin
        return Implementation.Is_Lower(Item);
      end Is_Lower;
    function Is_Lower(
      Item : in String_1)
      return Boolean
      is
      begin
        return Is_Lower(To_String_2(Item));
      end Is_Lower;
    function Is_Lower(
      Item : in String_2)
      return Boolean
      is
      begin
        for I in String_2 loop
          if not Is_Lower(Item(I)) then

          end if;
        end loop;

      end Is_Lower;
  --------------
  -- To_Upper --
  --------------
    function To_Upper(
      Item : in Character_1)
      return Character_2
      is
      begin
        return ' ';
      end To_Upper;
    function To_Upper(
      Item : in Character_2)
      return Character_1
      is
      begin
        if Character_2'Pos(Item) > Character_1'Pos(Character_1'Last) then
          return REPLACEMENT_FOR_EXTENDED_CHARACTER;
        end if;
        return ' ';
      end To_Upper;
    function To_Upper(
      Item : in Character_2)
      return Character_2
      is
      begin
        return ' ';
      end To_Upper;
    function To_Upper(
      Item : in String_1)
      return String_2
      is
      begin
        return "";
      end To_Upper;
    function To_Upper(
      Item : in String_2)
      return String_1
      is
      begin
        return "";
      end To_Upper;
    function To_Upper(
      Item : in String_2)
      return String_2
      is
      begin
        return "";
      end To_Upper;
    procedure To_Upper(
      Item : in out Character_1)
      is
      begin
        null;
      end To_Upper;
    procedure To_Upper(
      Item : in out Character_2)
      is
      begin
        null;
      end To_Upper;
    procedure To_Upper(
      Item : in out String_1)
      is
      begin
        null;
      end To_Upper;
    procedure To_Upper(
      Item : in out String_2)
      is
      begin
        null;
      end To_Upper;
  --------------
  -- Is_Upper --
  --------------
    function Is_Upper(
      Item : in Character_1)
      return Boolean
      is
      begin
        return True;
      end Is_Upper;
    function Is_Upper(
      Item : in Character_2)
      return Boolean
      is
      begin
        return True;
      end Is_Upper;
    function Is_Upper(
      Item : in String_1)
      return Boolean
      is
      begin
        return True;
      end Is_Upper;
    function Is_Upper(
      Item : in String_2)
      return Boolean
      is
      begin
        return True;
      end Is_Upper;
  -------------------
  -- Is_Alphabetic --
  -------------------
    function Is_Alphabetic(
      Item : in Character_1)
      return Boolean
      is
      begin
        return True;
      end Is_Alphabetic;
    function Is_Alphabetic(
      Item : in Character_2)
      return Boolean
      is
      begin
        return True;
      end Is_Alphabetic;
    function Is_Alphabetic(
      Item : in String_1)
      return Boolean
      is
      begin
        return True;
      end Is_Alphabetic;
    function Is_Alphabetic(
      Item : in String_2)
      return Boolean
      is
      begin
        return True;
      end Is_Alphabetic;
  ---------------------
  -- Is_Alphanumeric --
  ---------------------
    function Is_Alphanumeric(
      Item : in Character_1)
      return Boolean
      is
      begin
        return True;
      end Is_Alphanumeric;
    function Is_Alphanumeric(
      Item : in Character_2)
      return Boolean
      is
      begin
        return True;
      end Is_Alphanumeric;
    function Is_Alphanumeric(
      Item : in String_1)
      return Boolean
      is
      begin
        return True;
      end Is_Alphanumeric;
    function Is_Alphanumeric(
      Item : in String_2)
      return Boolean
      is
      begin
        return True;
      end Is_Alphanumeric;
  ------------------
  -- Is_Printable --
  ------------------
    function Is_Printable(
      Item : in Character_1)
      return Boolean
      is
      begin
        return False;
      end Is_Printable;
    function Is_Printable(
      Item : in Character_2)
      return Boolean
      is
      begin
        return False;
      end Is_Printable;
    function Is_Printable(
      Item : in String_1)
      return Boolean
      is
      begin
        return False;
      end Is_Printable;
    function Is_Printable(
      Item : in String_2)
      return Boolean
      is
      begin
        return False;
      end Is_Printable;
  --------------------
  -- Is_Punctuation --
  --------------------
    function Is_Punctuation(
      Item : in Character_1)
      return Boolean
      is
      begin
        return False;
      end Is_Punctuation;
    function Is_Punctuation(
      Item : in Character_2)
      return Boolean
      is
      begin
        return False;
      end Is_Punctuation;
    function Is_Punctuation(
      Item : in String_1)
      return Boolean
      is
      begin
        return False;
      end Is_Punctuation;
    function Is_Punctuation(
      Item : in String_2)
      return Boolean
      is
      begin
        return False;
      end Is_Punctuation;
  --------------------
  -- Is_White_Space --
  --------------------
    function Is_White_Space(
      Item : in Character_1)
      return Boolean
      is
      begin
        return False;
      end Is_White_Space;
    function Is_White_Space(
      Item : in Character_2)
      return Boolean
      is
      begin
        return False;
      end Is_White_Space;
    function Is_White_Space(
      Item : in String_1)
      return Boolean
      is
      begin
        return False;
      end Is_White_Space;
    function Is_White_Space(
      Item : in String_2)
      return Boolean
      is
      begin
        return False;
      end Is_White_Space;
  ----------------
  -- Is_Control --
  ----------------
    function Is_Control(
      Item : in Character_1)
      return Boolean
      is
      begin
        return False;
      end Is_Control;
    function Is_Control(
      Item : in Character_2)
      return Boolean
      is
      begin
        return False;
      end Is_Control;
    function Is_Control(
      Item : in String_1)
      return Boolean
      is
      begin
        return False;
      end Is_Control;
    function Is_Control(
      Item : in String_2)
      return Boolean
      is
      begin
        return False;
      end Is_Control;
  --------------------------
  -- Is_Hexadecimal_Digit --
  --------------------------
    function Is_Hexadecimal_Digit(
      Item : in Character_1)
      return Boolean
      is
      begin
        return False;
      end Is_Hexadecimal_Digit;
    function Is_Hexadecimal_Digit(
      Item : in Character_2)
      return Boolean
      is
      begin
        return False;
      end Is_Hexadecimal_Digit;
    function Is_Hexadecimal_Digit(
      Item : in String_1)
      return Boolean
      is
      begin
        return False;
      end Is_Hexadecimal_Digit;
    function Is_Hexadecimal_Digit(
      Item : in String_2)
      return Boolean
      is
      begin
        return False;
      end Is_Hexadecimal_Digit;
  ----------------------
  -- Is_Decimal_Digit --
  ----------------------
    function Is_Decimal_Digit(
      Item : in Character_1)
      return Boolean
      is
      begin
        return False;
      end Is_Decimal_Digit;
    function Is_Decimal_Digit(
      Item : in Character_2)
      return Boolean
      is
      begin
        return False;
      end Is_Decimal_Digit;
    function Is_Decimal_Digit(
      Item : in String_1)
      return Boolean
      is
      begin
        return False;
      end Is_Decimal_Digit;
    function Is_Decimal_Digit(
      Item : in String_2)
      return Boolean
      is
      begin
        return False;
      end Is_Decimal_Digit;
  end Neo.System.Text;
