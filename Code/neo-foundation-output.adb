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
package body Neo.Foundation.Output
  is
  ----------------------------
  -- Protected_Input_Output --
  ----------------------------
    procedure Put_Failed_Get_Catalog -- Hack, avoids error with protected object calling itself
      with inline;
    procedure Put_Failed_Get_Catalog
      is
      begin
        Put_Debug_Line(L(FAILED_GET_CATALOG));
      end Put_Failed_Get_Catalog;
    protected body Protected_Input_Output
      is
        entry Finalize
          when not Is_Working_With_File
          is
          begin
            Is_Working_With_File := True;
            if Ada.Wide_Text_IO.Is_Open(Catalog) then
              Ada.Wide_Text_IO.Close(Catalog);
            end if;
            Is_Working_With_File := False;
          end Finalize;
        entry Set_Catalog_Path(
          Path : in String_2)
          when not Is_Working_With_File
          is
          begin
            Is_Working_With_File := True;
            if Ada.Wide_Text_IO.Is_Open(Catalog) then
              Ada.Wide_Text_IO.Close(Catalog);
            end if;
            -----------------
            Try_To_Open_Path:
            -----------------
              begin
                Ada.Wide_Text_IO.Open(Catalog, Ada.Wide_Text_IO.Out_File, To_String_1(Path));
              exception
                when others => -- File does not exist, create it
                  Ada.Wide_Text_IO.Create(Catalog, Ada.Wide_Text_IO.Out_File, To_String_1(Path));
              end Try_To_Open_Path;
            Current_Catalog_Path := To_String_2_Unbounded(Path);
            Is_Working_With_File := False;
          exception
            when others =>
              Put_Debug_Line(L(FAILED_SET_CATALOG_PATH) & Path);
              Is_Working_With_File := False;
          end Set_Catalog_Path;
        entry Put(
          Item : in String_2)
          when not Is_Working_With_File
          is
          begin
            Is_Working_With_File := True;
            if Current_Put /= null and then Current_Do_Put then
              Current_Put.all(Item);
            end if;
            if Ada.Wide_Text_IO.Is_Open(Catalog) then
              Ada.Wide_Text_IO.Put(Catalog, Item);
            end if;
            Is_Working_With_File := False;
          end Put;
        entry Get_Catalog(
          Item : in out String_2_Unbounded)
          when not Is_Working_With_File
          is
          begin
            Is_Working_With_File := True;
            if not Ada.Wide_Text_IO.Is_Open(Catalog) then
              Put_Failed_Get_Catalog;
              Is_Working_With_File := False;
            else
              Ada.Wide_Text_IO.Reset(Catalog, Ada.Wide_Text_IO.In_File);
              loop
                Ada.Strings.Wide_Unbounded.Append(Item, Ada.Wide_Text_IO.Get_Line(Catalog) & To_String_2(END_LINE));
              end loop;
            end if;
          exception
            when Ada.Wide_Text_IO.End_Error =>
              Ada.Wide_Text_IO.Reset(Catalog, Ada.Wide_Text_IO.Out_File);
              Ada.Wide_Text_IO.Put(Catalog, To_String_2(Item)); -- There has to be a better way to append a file than to open it and rewrite the contents...
              Is_Working_With_File := False;
            when Error: others =>
              Is_Working_With_File := False;
              Reraise_Occurrence(Error);
          end Get_Catalog;
        function Localize(
          Item : in String_2)
          return String_2
          is
          begin
            if Current_Localize = null then
              return NULL_STRING_2;
            end if;
            return Current_Localize.all(Item);
          end Localize;
        function Do_Put_Debug
          return Boolean
          is
          begin
            return Current_Do_Put_Debug;
          end Do_Put_Debug;
        function Get_Catalog_Path
          return String_2
          is
          begin
            return To_String_2(Current_Catalog_Path);
          end Get_Catalog_Path;
        procedure Set_Put(
          Do_Put : in Boolean)
          is
          begin
            Current_Do_Put := Do_Put;
          end Set_Put;
        procedure Set_Put_Debug(
          Do_Put_Debug : in Boolean)
          is
          begin
            Current_Do_Put_Debug := Do_Put_Debug;
          end Set_Put_Debug;
        function Get_Line_Size
          return Integer_4_Positive
          is
          begin
            return Current_Line_Size;
          end Get_Line_Size;
        procedure Set_Debug_Put(
          Do_Put_Debug : in Boolean)
          is
          begin
            Current_Do_Put_Debug := Do_Put_Debug;
          end Set_Debug_Put;
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
      end Protected_Input_Output;
  ----------
  -- Test --
  ----------
    procedure Test
      is
      Item    : String_2(1..10)    := (others => NULL_CHARACTER_2);
      Last    : Integer_4_Natural  := 0;
      Unicode : Character_2        := Character_2'val(512);
      Data    : Integer_4_Unsigned := 16#2345_6789#;
      function To_Image
        is new To_Radian_Image(Integer_4_Unsigned);
      begin
        Put_Line(L("Test of Neo.Foundation.Output"));
        --Set_Localize(Dummy_Localize'access);
        Set_Put(Ada.Wide_Text_IO.Put'access);
        Put(L("Test of "));
        Put("Put");
        Put('.');
        Put(Unicode);
        New_Line;
        Put_Line(L("Test of Put_Line.") & Unicode);
        Put(L("Test of Get: "));
        Put_Line(L("Result: ") & Unicode & "");
        Put_Line(L("Result: ") & Item);
        New_Line;
        New_Line(2);
        Put_Line(L("Number Put: "));
        Put_Line("""" & To_Image(Data, 16) & """");
        Put_Line("""" & To_Image(Data,  2) & """");
        Put_Line("""" & To_Image(Data, 10) & """");
        Put_Line(L("The current catalog (" & Get_Catalog_Path & ": "));
        Put_Line(Get_Catalog);
      end Test;
  --------------
  -- Finalize --
  --------------
    procedure Finalize
      is
      begin
        Input_Output.Finalize;
      end Finalize;
  --------------
  -- Localize --
  --------------
    function Localize(
      Item : in String_2)
      return String_2
      is
      Result : String_2 := Input_Output.Localize(Item);
      begin
        if Result = NULL_STRING_2 then
          if DO_PUT_LOCALIZE_FAILURE then
            Put_Debug(
              FAILED_LOCALIZE_PREFIX &
              Item(Item'first..(
                if Item'length >= FAILED_LOCALIZE_PREVIEW_LENGTH then
                  Item'first + FAILED_LOCALIZE_PREVIEW_LENGTH - 1
                else
                  Item'last)) &
              FAILED_LOCALIZE_POSTFIX);
          end if;
          return Item;
        end if;
        return Result;
      end Localize;
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
  -- Set_Localize --
  ------------------
    procedure Set_Localize(
      Localize : in Access_Subprogram_Localize)
      is
      begin
      	Input_Output.Set_Localize(Localize);
      end Set_Localize;
  -------------------
  -- Set_Put_Debug --
  -------------------
    procedure Set_Put_Debug(
      Do_Put_Debug : in Boolean)
      is
      begin
        Input_Output.Set_Put_Debug(Do_Put_Debug);
      end Set_Put_Debug;
  -------------
  -- Set_Put --
  -------------
    procedure Set_Put(
      Do_Put : in Boolean)
      is
      begin
        Input_Output.Set_Put(Do_Put);
      end Set_Put;
  ----------------------
  -- Set_Catalog_Path --
  ----------------------
    procedure Set_Catalog_Path(
      Path : in String_2)
      is
      begin
        Input_Output.Set_Catalog_Path(Path);
      end Set_Catalog_Path;
  -----------------
  -- Get_Catalog --
  -----------------
    function Get_Catalog
      return String_2
      is
      Result : String_2_Unbounded := Ada.Strings.Wide_Unbounded.NULL_UNBOUNDED_WIDE_STRING;
      begin
        Input_Output.Get_Catalog(Result);
        return To_String_2(Result);
      end Get_Catalog;
  ----------------------
  -- Get_Catalog_Path --
  ----------------------
    function Get_Catalog_Path
      return String_2
      is
      begin
        return Input_Output.Get_Catalog_Path;
      end Get_Catalog_Path;
  ---------
  -- Put --
  ---------
    procedure Put(
      Item : in Character_2)
      is
      begin
        Put(Item & "");
      end Put;
    procedure Put(
      Item : in String_2)
      is
      begin
      	Input_Output.Put(Item);
      end Put;
  --------------
  -- Put_Line --
  --------------
    procedure Put_Line(
      Item : in String_2)
      is
      begin
        Put(Item);
        New_Line;
      end Put_Line;
  ---------------
  -- Put_Debug --
  ---------------
    procedure Put_Debug(
      Item : in Character_2)
      is
      begin
        Put_Debug(Item & "");
      end Put_Debug;
    procedure Put_Debug(
      Item : in String_2)
      is
      begin
        if Input_Output.Do_Put_Debug then
          Input_Output.Put(Item);
        end if;
      end Put_Debug;
  --------------------
  -- Put_Debug_Line --
  --------------------
    procedure Put_Debug_Line(
      Item : in String_2)
      is
      begin
        Put_Debug(Item);
        New_Line;
      end Put_Debug_Line;
  -------------------
  -- Get_Line_Size --
  -------------------
    function Get_Line_Size
      return Integer_4_Positive
      is
      begin
      	return Input_Output.Get_Line_Size;
      end Get_Line_Size;
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
      Input : String_1(1..RADIAN_IMAGE_STRING_SIZE) := (others => NULL_CHARACTER_1);
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
                  Source => Head(Trimmed_Input, Trimmed_Input'last - 1),
                  Count  => Trimmed_Input'last - 2 - Trim(Ada.Wide_Text_IO.Number_Base'Image(Base), Both)'Length));
          end Remove_Notation;
      end To_Radian_Image;
  end Neo.Foundation.Output;


