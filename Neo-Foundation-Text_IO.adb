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
package body Neo.Foundation.Text_IO
  is
  ----------
  -- Test --
  ----------
    procedure Test
      is
      begin
        null;
      end Test;
  ---------
  -- Set --
  ---------
    procedure Set(
      Localize  : in Access_Localize;
      Put       : in Access_Put;
      Get_Line  : in Access_Get_Line;
      Localize  : in Access_Localizer)
      is
      begin
        Protected_Data.Set(New_State);
      end Set;
  -------------
  -- Set_Put --
  -------------
    procedure Set_Put(
      New_Put : in Access_Procedure_In_String_2)
      is
      Intput_Output : Record_Input_Output := Get;
      begin
        Set(
          Line_Size => Intput_Output.Line_Size,
          Put       => New_Put,
          Put_Line  => Intput_Output.Put_Line,
          New_Line  => Intput_Output.New_Line,
          Get_Line  => Intput_Output.Get_Line);
      end Set_Put;
  ------------------
  -- Set_Put_Line --
  ------------------
    procedure Set_Put_Line(
      New_Put_Line : in Access_Procedure_In_String_2)
      is
      Intput_Output : Record_Input_Output := Get;
      begin
        Set(
          Line_Size => Intput_Output.Line_Size,
          Put       => Intput_Output.Put,
          Put_Line  => New_Put_Line,
          New_Line  => Intput_Output.New_Line,
          Get_Line  => Intput_Output.Get_Line);
      end Set_Put_Line;
  ------------------
  -- Set_Get_Line --
  ------------------
    procedure Set_Get_Line(
      New_Get_Line : in Access_Procedure_In_Out_String_2_In_Out_Integer_4_Natural)
      is
      Intput_Output : Record_Input_Output := Get;
      begin
        Set(
          Line_Size => Intput_Output.Line_Size,
          Put       => Intput_Output.Put,
          Put_Line  => Intput_Output.Put_Line,
          New_Line  => Intput_Output.New_Line,
          Get_Line  => New_Get_Line);
      end Set_Get_Line;
  ---------
  -- Put --
  ---------
    procedure Put(
      Item : in Character_1)
      is
      begin
      end Put;
    procedure Put(
      Item : in Character_2)
      is
      begin
      end Put;
    procedure Put(
      Item : in String_1)
      is
      begin
      end Put;
    procedure Put(
      Item : in String_2)
      is
      begin
      end Put;
  --------------
  -- Put_Line --
  --------------
    procedure Put_Line(
      Item : in Character_1)
      is
      begin
      end Put_Line;
    procedure Put_Line(
      Item : in Character_2)
      is
      begin
      end Put_Line;
    procedure Put_Line(
      Item : in String_1)
      is
      begin
      end Put_Line;
    procedure Put_Line(
      Item : in String_2)
      is
      begin
      end Put_Line;
  ---------
  -- Get --
  ---------
    function Get
      return Character_2
      is
      begin
        return ' ';
      end Get;
  --------------
  -- Get_Line --
  --------------
    function Get_Line
      return String_2
      is
      begin
        return "";
      end Get_Line;
    procedure Get_Line
      is
      begin
      end Get_Line;
  -----------------------
  -- Hexadecimal_Image --
  -----------------------
    function Hexadecimal_Image(
      Item : in Integer_4_Unsigned)
      return String_2
      is
      Result      : String_2(1..12)    := (others => ' ');
      First_Digit : Integer_4_Positive := 1; 
      begin 
        Integer_4_Signed_IO.Put(Result, Integer(Item), 12);
        Result(12) := ' ';
        for I in 1..Result'Length loop
          if Result(I) = '#' then
            Result(I - 2..I) := "   ";
            exit;
          end if;
        end loop;
        return Trim(Result, Both);
      end Hexadecimal_Image;
    function Hexadecimal_Image(
      Item : in Integer_2_Unsigned)
      return String_2
      is
      begin
        return Hexadecimal_Image(Integer_4_Unsigned(Item));
      end Hexadecimal_Image;
    function Hexadecimal_Image(
      Item : in Integer_1_Unsigned)
      return String_2
      is
      begin
        return Hexadecimal_Image(Integer_4_Unsigned(Item));
      end Hexadecimal_Image;
  ------------------
  -- Binary_Image --
  ------------------
    function Binary_Image(
      Item             : in Integer_4_Unsigned;
      Do_Space_Nibbles : in Boolean := False)
      return String_2
      is
      I      : Integer_4_Unsigned := 16#8000_0000#;
      K      : Integer_4_Signed   := 1;
      Result : String_2(1..39)    := (others => ' ');
      begin
        while (I and Item) = 0 loop
          I := Shift_Right(I, 1);
          K := K + 1;
          if Do_Space_Nibbles and then K mod 5 = 0 then
            K := K + 1;
          end if;
          if I = 0 then
            return "0";
          end if;
        end loop;
        for J in K..Result'Length loop
          if Do_Space_Nibbles and then J mod 5 = 0 then
            Result(J) := ' ';
          else
            if (I and Item) > 0 then
              Result(J) := '1';
            else
              Result(J) := '0';
            end if;
            I := Shift_Right(I, 1);
            exit when I <= 0;
          end if;
        end loop;
        return Trim(Result, Both);
      end Binary_Image;
    function Binary_Image(
      Item             : in Integer_2_Unsigned;
      Do_Space_Nibbles : in Boolean := False)
      return String_2
      is
      begin
        return Binary_Image(Integer_4_Unsigned(Item), Do_Space_Nibbles);
      end Binary_Image;
    function Binary_Image(
      Item             : in Integer_1_Unsigned;
      Do_Space_Nibbles : in Boolean := False)
      return String_2
      is
      begin
        return Binary_Image(Integer_4_Unsigned(Item), Do_Space_Nibbles);
      end Binary_Image;
  end Neo.Foundation.Text_IO;