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
      Item    : String_2(1..10)    := (others => NULL_CHARACTER_2);
      Last    : Integer_4_Natural  := 0;
      Unicode : Character_2        := Character_2'Val(512);
      Data    : Integer_4_Unsigned := 16#2345_6789#;
      function To_Image
        is new To_Radian_Image(Integer_4_Unsigned);
      begin
        Put_Line("Test of Neo.Foundation.Text_IO");
        Set_Skip_Line(Ada.Wide_Text_IO.Skip_Line'Access);
        --Set_Localize(Dummy_Localize'Access);
        Set_Put(Ada.Wide_Text_IO.Put'Access);
        Set_Get_Line(Ada.Wide_Text_IO.Get_Line'Access);
        Set_Data(
          Line_Size => DEFAULT_LINE_SIZE,
          Localize  => null,--Dummy_Localize'Access,
          Put       => Ada.Wide_Text_IO.Put'Access,
          Get_Line  => Ada.Wide_Text_IO.Get_Line'Access,
          Skip_Line => Ada.Wide_Text_IO.Skip_Line'Access);
        Put("Test of ");
        Put("Put");
        Put('.');
        Put(Unicode);
        New_Line;
        Put_Line("Test of Put_Line." & Unicode);
        Put("Test of Get: ");
        Get(Unicode);
        Put_Line("Result is " & Unicode & "");
        Put("Test of Get_Line: ");
        Get_Line(Item, Last);
        Put_Line("Result is " & Item);
        New_Line;
        New_Line(2);
        Put_Line("Number printing:");
        Put_Line("""" & To_Image(Data, 16) & """");
        Put_Line("""" & To_Image(Data,  2) & """");
        Put_Line("""" & To_Image(Data, 10) & """");
      end Test;
  --------------
  -- Set_Data --
  --------------
    procedure Set_Data(
      Line_Size : in Integer_4_Positive;
      Localize  : in Access_Subprogram_Localize;
      Put       : in Access_Subprogram_Put;
      Get_Line  : in Access_Subprogram_Get_Line;
      Skip_Line : in Access_Subprogram_Skip_Line)
      is
      begin
        Protected_Data.Set((
          Line_Size => Line_Size,
          Localize  => Localize,
          Put       => Put,
          Get_Line  => Get_Line,
          Skip_Line => Skip_Line));
      end Set_Data;
  -------------------
  -- Set_Line_Size --
  -------------------
    procedure Set_Line_Size(
      Line_Size : in Integer_4_Positive)
      is
      Input_Output : Record_Input_Output := Protected_Data.Get;
      begin
        Set_Data(
          Line_Size => Line_Size,
          Localize  => Input_Output.Localize,
          Put       => Input_Output.Put,
          Get_Line  => Input_Output.Get_Line,
          Skip_Line => Input_Output.Skip_Line);
      end Set_Line_Size;
  -------------
  -- Set_Put --
  -------------
    procedure Set_Put(
      Put : in Access_Subprogram_Put)
      is
      Input_Output : Record_Input_Output := Protected_Data.Get;
      begin
        Set_Data(
          Line_Size => Input_Output.Line_Size,
          Localize  => Input_Output.Localize,
          Put       => Put,
          Get_Line  => Input_Output.Get_Line,
          Skip_Line => Input_Output.Skip_Line);
      end Set_Put;
  ------------------
  -- Set_Get_Line --
  ------------------
    procedure Set_Get_Line(
      Get_Line : in Access_Subprogram_Get_Line)
      is
      Input_Output : Record_Input_Output := Protected_Data.Get;
      begin
        Set_Data(
          Line_Size => Input_Output.Line_Size,
          Localize  => Input_Output.Localize,
          Put       => Input_Output.Put,
          Get_Line  => Get_Line,
          Skip_Line => Input_Output.Skip_Line);
      end Set_Get_Line;
  ------------------
  -- Set_Localize --
  ------------------
    procedure Set_Localize(
      Localize : in Access_Subprogram_Localize)
      is
      Input_Output : Record_Input_Output := Protected_Data.Get;
      begin
        Set_Data(
          Line_Size => Input_Output.Line_Size,
          Localize  => Localize,
          Put       => Input_Output.Put,
          Get_Line  => Input_Output.Get_Line,
          Skip_Line => Input_Output.Skip_Line);
      end Set_Localize;
  -------------------
  -- Set_Skip_Line --
  -------------------
    procedure Set_Skip_Line(
      Skip_Line : in Access_Subprogram_Skip_Line)
      is
      Input_Output : Record_Input_Output := Protected_Data.Get;
      begin
        Set_Data(
          Line_Size => Input_Output.Line_Size,
          Localize  => Input_Output.Localize,
          Put       => Input_Output.Put,
          Get_Line  => Input_Output.Get_Line,
          Skip_Line => Skip_Line);
      end Set_Skip_Line;
  ---------
  -- Put --
  ---------
    procedure Put(
      Item        : in Character_2;
      Do_Localize : in Boolean := False)
      is
      begin
        Put(Item & "", Do_Localize);
      end Put;
    procedure Put(
      Item        : in String_2;
      Do_Localize : in Boolean := False)
      is
      begin
        if Protected_Data.Get.Put /= null then
          if Protected_Data.Get.Localize /= null and then Do_Localize then
            Protected_Data.Get.Put.All(Protected_Data.Get.Localize(Item));
          else
            Protected_Data.Get.Put.All(Item);
          end if;
        end if;
      end Put;
  --------------
  -- Put_Line --
  --------------
    procedure Put_Line(
      Item        : in String_2;
      Do_Localize : in Boolean := False)
      is
      begin
        Put(Item, Do_Localize);
        New_Line;
      end Put_Line;
  ---------
  -- Get --
  ---------
    procedure Get(
      Item : in out Character_2)
      is
      Line_Of_Text : String_2(1..1)    := (others => NULL_CHARACTER_2);
      Last         : Integer_4_Natural := 0;
      begin
        while Last = 0 loop
          Get_Line(Line_Of_Text, Last);
        end loop;
        Skip_Line;
        Item := Line_Of_Text(Line_Of_Text'First);
      end Get;
  --------------
  -- Get_Line --
  --------------
    procedure Get_Line(
      Item : in out String_2;
      Last : in out Integer_4_Natural)
      is
      begin
        if Protected_Data.Get.Get_Line /= null then
          Protected_Data.Get.Get_Line.All(Item, Last);
        end if;
      end Get_Line;
  ------------------
  -- Get_Line_Size --
  -------------------
    function Get_Line_Size
      return Integer_4_Positive
      is
      begin
        return Protected_Data.Get.Line_Size;
      end Get_Line_Size;
  ---------------
  -- Skip_Line --
  ---------------
    procedure Skip_Line(
      Spacing : in Integer_4_Positive := 1)
      is
      begin
        if Protected_Data.Get.Skip_Line /= null then
          Protected_Data.Get.Skip_Line.All(Integer_Positive_Count(Spacing));
        end if;
      end Skip_Line;
  --------------
  -- New_Line --
  --------------
    procedure New_Line(
      Lines : in Integer_4_Positive := 1)
      is
      begin
        for I in 1..Lines loop
          Put(To_String_2(END_LINE));
        end loop;
      end New_Line;
  ---------------------
  -- To_Radian_Image --
  ---------------------
    function To_Radian_Image(
      Item : in Type_Number;
      Base : in Integer_Base)
      return String_2
      is
      package Type_Number_Text_IO
        is new Ada.Text_IO.Modular_IO(Type_Number);
      Input : String_1(1..WIDE_IMAGE_BUFFER_SIZE) := (others => NULL_CHARACTER_1);
      begin
        Type_Number_Text_IO.Put(Input, Item, Base);
        if Base = 10 then
          return To_String_2(Trim(Input, Both));
        end if;
        ----------------
        Remove_Notation:
        ----------------
          declare
          Trimmed_Input : String_1 := Trim(Input, Both);
          begin
            return
              To_String_2(
                Tail(
                  Source => Head(Trimmed_Input, Trimmed_Input'Last - 1),
                  Count  => Trimmed_Input'Last - 2 - Trim(Integer_Base'Image(Base), Both)'Length));
          end Remove_Notation;
      end To_Radian_Image;
  end Neo.Foundation.Text_IO;


