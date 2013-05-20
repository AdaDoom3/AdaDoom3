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
  ----------------------------
  -- Protected_Input_Output --
  ----------------------------
    protected body Protected_Input_Output
      is
        procedure Put(
          Item        : in String_2;
          Do_Localize : in Boolean := False)
          is
          begin
            if Current_Put /= null then
              if Current_Localize /= null and then Do_Localize then
                Current_Put.all(Current_Localize.all(Item));
              else
                Current_Put.all(Item);
              end if;
            end if;
          end Put;
        procedure Put_Debug(
          Item        : in String_2;
          Do_Localize : in Boolean := False)
          is
          begin
            if Current_Do_Print_Debugging then
              Put(Item, Do_Localize);
            end if;
          end Put_Debug;
        procedure Skip_Line(
          Spacing : in Integer_4_Positive := 1)
          is
          begin
            Current_Skip_Line.all(Ada.Wide_Text_IO.Positive_Count(Spacing));
          end Skip_Line;
        procedure Get_Line(
          Item : in out String_2;
          Last : in out Integer_4_Natural)
          is
          begin
            Current_Get_Line.all(Item, Last);
          end Get_Line;
        function Get_Line_Size
          return Integer_4_Positive
          is
          begin
            return Current_Line_Size;
          end Get_Line_Size;
        procedure Set_Debug_Printing(
          Do_Print_Debugging : in Boolean)
          is
          begin
            Current_Do_Print_Debugging := Do_Print_Debugging;
          end Set_Debug_Printing;
        procedure Set_Catalog_Path(
          Path : in String_2)
          is
          begin
            Current_Catalog_Path := To_Unbounded_Wide_String(Path);
          end Set_Catalog_Path;
        procedure Set_Line_Size(
          Line_Size : in Integer_4_Positive)
          is
          begin
            Current_Line_Size := Line_Size;
          end Set_Line_Size;
        procedure Set_Localize(
          Localize : in Access_Subprogram_Localize)
          is
          begin
            Current_Localize := Localize;
          end Set_Localize;
        procedure Set_Put(
          Put : in Access_Subprogram_Put)
          is
          begin
            Current_Put := Put;
          end Set_Put;
        procedure Set_Get_Line(
          Get_Line : in Access_Subprogram_Get_Line)
          is
          begin
            Current_Get_Line := Get_Line;
          end Set_Get_Line;
        procedure Set_Skip_Line(
          Skip_Line : in Access_Subprogram_Skip_Line)
          is
          begin
            Current_Skip_Line := Skip_Line;
          end Set_Skip_Line;
      end Protected_Input_Output;
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
  -------------------
  -- Set_Line_Size --
  -------------------
    procedure Set_Line_Size(
      Line_Size : in Integer_4_Positive)
      is
      begin
        Input_Output.Set_Line_Size(Line_Size);
      end Set_Line_Size;
  -------------
  -- Set_Put --
  -------------
    procedure Set_Put(
      Put : in Access_Subprogram_Put)
      is
      begin
        Input_Output.Set_Put(Put);
      end Set_Put;
  ------------------
  -- Set_Get_Line --
  ------------------
    procedure Set_Get_Line(
      Get_Line : in Access_Subprogram_Get_Line)
      is
      begin
        Input_Output.Set_Get_Line(Get_Line);
      end Set_Get_Line;
  ------------------
  -- Set_Localize --
  ------------------
    procedure Set_Localize(
      Localize : in Access_Subprogram_Localize)
      is
      begin
      	Input_Output.Set_Localize(Localize);
      end Set_Localize;
  -------------------
  -- Set_Skip_Line --
  -------------------
    procedure Set_Skip_Line(
      Skip_Line : in Access_Subprogram_Skip_Line)
      is
      begin
      	Input_Output.Set_Skip_Line(Skip_Line);
      end Set_Skip_Line;
  ----------------------
  -- Set_Catalog_Path --
  ----------------------
    procedure Set_Catalog_Path(
      Path : in String_2)
      is
      begin
      	Input_Output.Set_Catalog_Path(Path);
      end Set_Catalog_Path;
  ------------------------
  -- Set_Debug_Printing --
  ------------------------
    procedure Set_Debug_Printing(
      Do_Print_Debugging : in Boolean)
      is
      begin
        Input_Output.Set_Debug_Printing(Do_Print_Debugging);
      end Set_Debug_Printing;
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
      	Input_Output.Put(Item, Do_Localize);
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
  ---------------
  -- Put_Debug --
  ---------------
    procedure Put_Debug(
      Item        : in Character_2;
      Do_Localize : in Boolean := False)
      is
      begin
        Put_Debug(Item & "", Do_Localize);
      end Put_Debug;
    procedure Put_Debug(
      Item        : in String_2;
      Do_Localize : in Boolean := False)
      is
      begin
        Input_Output.Put_Debug(Item, Do_Localize);
      end Put_Debug;
  --------------------
  -- Put_Debug_Line --
  --------------------
    procedure Put_Debug_Line(
      Item        : in String_2;
      Do_Localize : in Boolean := False)
      is
      begin
        Put_Debug(Item, Do_Localize);
        New_Line;
      end Put_Debug_Line;
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
      	Input_Output.Get_Line(Item, Last);
      end Get_Line;
  ------------------
  -- Get_Line_Size --
  -------------------
    function Get_Line_Size
      return Integer_4_Positive
      is
      begin
      	return Input_Output.Get_Line_Size;
      end Get_Line_Size;
  ---------------
  -- Skip_Line --
  ---------------
    procedure Skip_Line(
      Spacing : in Integer_4_Positive := 1)
      is
      begin
      	Input_Output.Skip_Line(Spacing);
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
      Item    : in Type_Number;
      Base    : in Ada.Wide_Text_IO.Number_Base;
      Spacing : in Integer_4_Natural := 0)
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
                  Count  => Trimmed_Input'Last - 2 - Trim(Ada.Wide_Text_IO.Number_Base'Image(Base), Both)'Length));
          end Remove_Notation;
      end To_Radian_Image;
  end Neo.Foundation.Text_IO;


