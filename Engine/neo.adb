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
package body Neo
  is
  -----------------
  -- Hashed_Maps --
  -----------------
    package body Hashed_Maps
      is
        protected body Protected_Map
          is
            -----------------
            procedure Insert(
            -----------------
              Key      : in String_2;
              New_Item : in Type_To_Hash)
              is
              begin
                Actual.Insert(Data, To_String_2_Unbounded(Key), New_Item);
              end Insert;
            ------------------
            procedure Replace(
            ------------------
              Key      : in String_2;
              New_Item : in Type_To_Hash)
              is
              begin
                Actual.Replace(Data, To_String_2_Unbounded(Key), New_Item);
              end Replace;
            --------------
            function Find(
            --------------
              Key : in String_2)
              return Actual.Cursor
              is
              begin
                return Actual.Find(Data, To_String_2_Unbounded(Key));
              end Find;
            -----------------
            procedure Delete(
            -----------------
              Key : in String_2)
              is
              begin
                Actual.Delete(Data, To_String_2_Unbounded(Key));
              end Delete;
            -----------------
            function Element(
            -----------------
              Key : in String_2)
              return Type_To_Hash
              is
              begin
                return Actual.Element(Data, To_String_2_Unbounded(Key));
              end Element;
            ---------------------
            function Has_Element(
            ---------------------
              Key : in String_2)
              return Boolean
              is
              begin
                return Actual.Has_Element(Actual.Find(Data, To_String_2_Unbounded(Key)));
              end Has_Element;
          end Protected_Map;
      end Hashed_Maps;
  ------------
  -- Vector --
  ------------
  ----------------------
  -- Protected_Status --
  ----------------------
    protected body Protected_Status
      is
        ---------------------------
        function Is_Doing_Something
        ---------------------------
          return Boolean
          is
          begin
            return Status;
          end Is_Doing_Something;
        ---------------------------------
        procedure Set_Is_Doing_Something(
        ---------------------------------
          New_Status : in Boolean)
          is
          begin
            Status := New_Status;
          end Set_Is_Doing_Something;
      end Protected_Status;
  ----------------------------
  -- Protected_Input_Output --
  ----------------------------
    protected body Protected_Input_Output
      is
        --------------
        procedure Put(
        --------------
          Item : in String_2)
          is
          begin
            Current_Log := Current_Log & To_String_2_Unbounded(Item);
            Current_Put.all(Item);
          end Put;
        ------------------
        function Localize(
        ------------------
          Item : in String_2)
          return String_2
          is
          begin
            return(
              if Current_Localize = null then
                Item
              else
                Current_Localize.all(Item));
          end Localize;
        ---------------------
        function Do_Put_Debug
        ---------------------
          return Boolean
          is
          begin
            return Current_Do_Put_Debug;
          end Do_Put_Debug;
        ---------------------------
        procedure Set_Do_Put_Debug(
        ---------------------------
          Do_Put_Debug : in Boolean)
          is
          begin
            Current_Do_Put_Debug := Do_Put_Debug;
          end Set_Do_Put_Debug;
        -----------------------
        procedure Set_Localize(
        -----------------------
          Localize : in Access_Function_Localize)
          is
          begin
            Current_Localize := Localize;
          end Set_Localize;
        ------------------
        procedure Set_Put(
        ------------------
          Put : in Not_Null_Access_Procedure_Put)
          is
          begin
            Current_Put := Put;
          end Set_Put;
        --------------------------
        procedure Set_Input_Entry(
        --------------------------
          Input_Entry : in String_2)
          is
          begin
            Current_Input_Entry := Current_Input_Entry & To_String_2_Unbounded(Input_Entry);
          end Set_Input_Entry;
        --------------------
        procedure Set_Error(
        --------------------
          Error : in String_2)
          is
          begin
            Current_Error := Current_Error & To_String_2_Unbounded(Error);
          end Set_Error;
        --------------------
        function Get_Log
        --------------------
          return String_2
          is
          begin
            return To_String_2(Current_Log);
          end Get_Log;
        ------------------
        function Get_Error
        ------------------
          return String_2
          is
          begin
            return To_String_2(Current_Error);
          end Get_Error;
        ------------------------
        function Get_Input_Entry
        ------------------------
          return String_2
          is
          begin
            return To_String_2(Current_Input_Entry);
          end Get_Input_Entry;
        ----------------------
        function Get_Line_Size
        ----------------------
          return Integer_4_Positive
          is
          begin
            return Current_Line_Size;
          end Get_Line_Size;
        ----------------------------
        function Get_Number_Of_Lines
        ----------------------------
          return Integer_4_Natural
          is
          begin
            return Current_Line_Count;
          end Get_Number_Of_Lines;
        ------------------------
        procedure Set_Line_Size(
        ------------------------
          Line_Size : in Integer_4_Positive)
          is
          begin
            Current_Line_Size := Line_Size;
          end Set_Line_Size;
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
        Put_Line(Localize("BASE TEST"));
        Set_Put(Ada.Wide_Text_IO.Put'access);
        Put(Localize("Test of "));
        Put("Put");
        Put('.');
        Put(Unicode);
        New_Line;
        Put_Line(Localize("Test of Put_Line.") & Unicode);
        Put_Line(Localize("Result: ") & Unicode & "");
        Put_Line(Localize("Result: ") & Item);
        New_Line;
        New_Line(2);
        Put_Line(Localize("Number Put: "));
        Put_Line("""" & To_Image(Data, 16) & """");
        Put_Line("""" & To_Image(Data,  2) & """");
        Put_Line("""" & To_Image(Data, 10) & """");
        Put_Line(Get_Log);
      end Test;
  -----------
  -- Start --
  -----------
    procedure Start(
      Timer : in out Record_Timer)
      is
      begin
        if not Timer.Is_Stopped then
          raise Timer_Started_Without_Being_Stopped;
        end if;
        Timer :=(
          Is_Stopped => False,
          Start      => Clock,
          others     => <>);
      end Start;
  ----------
  -- Stop --
  ----------
    procedure Stop(
      Timer : in out Record_Timer)
      is
      begin
        if Timer.Is_Stopped then
          raise Timer_Stopped_Without_Being_Started;
        end if;
        Timer :=(
          Is_Stopped => True,
          Last       => Timer.Start - Clock,
          others     => <>);
      end Stop;
  ------------------
  -- Get_Duration --
  ------------------
    function Get_Duration(
      Timer : in Record_Timer)
      return Duration
      is
      begin
        return(
          if Timer.Is_Stopped then
            Timer.Last
          else
            Clock - Timer.Start);
      end Get_Duration;
  --------------
  -- Localize --
  --------------
    function Localize(
      Item : in String_2)
      return String_2
      is
      begin
        return Input_Output.Localize(Item);
      end Localize;
  ----------
  -- Hang --
  ----------
    procedure Hang(
      Hang_Time : in Integer_4_Signed := 3)
      is
      begin
        New_Line;
        for I in 1..Hang_Time loop
          delay 1.0;
          Put(TESTING_INPUT_HANG_INCREMENT);
        end loop;
      end Hang;
  --------------
  -- New_Line --
  --------------
    procedure New_Line(
      Lines : in Integer_4_Positive := 1)
      is
      begin
        for I in 1..Lines loop
          Put(END_LINE_2);
        end loop;
      end New_Line;
  ---------------------
  -- Set_Input_Entry --
  ---------------------
    procedure Set_Input_Entry(
      Item : in String_2)
      is
      begin
        Input_Output.Set_Input_Entry(Item);
      end Set_Input_Entry;
  ----------------------
  -- Set_Do_Put_Debug --
  ----------------------
    procedure Set_Do_Put_Debug(
      Do_Put_Debug : in Boolean)
      is
      begin
        Input_Output.Set_Do_Put_Debug(Do_Put_Debug);
      end Set_Do_Put_Debug;
  ---------------
  -- Put_Title --
  ---------------
    procedure Put_Title(
      Title : in String_2)
      is
      Space_Count : Integer_4_Signed := 0;
      Count       : Integer_4_Signed := Title'Length * 3 - Title'Length / 3;
      begin
        if Title'Length > Get_Line_Size then
          raise Title_Is_Too_Long_To_Fit_On_A_Single_Line;
        end if;
        New_Line;
        for I in 1..Get_Line_Size loop
          Put(TESTING_SEPARATOR);
        end loop;
        New_Line(2);
        for I in 1..Title'Length loop
          if Title(I) = ' ' then
            Space_Count := Space_Count + 1;
          end if;
        end loop;
        if Count + Space_Count >= Get_Line_Size then
          for I in 1..(Get_Line_Size / 2 - (Title'Length) / 2 - 1) loop
            Put(" ");
          end loop;
          Put(Title);
        else
          for I in 1..(Get_Line_Size / 2 - (Count + Space_Count) / 2 - 1) loop
            Put(" ");
          end loop;
          for I in 1..Title'Length loop
            Put(Title(I) & "  ");
            if Title(I) = ' ' then
              Put(" ");
            end if;
          end loop;
        end if;
        New_Line;
        for I in 1..Get_Line_Size loop
          Put(TESTING_SEPARATOR);
        end loop;
        New_Line(2);
      end Put_Title;
  ---------
  -- Put --
  ---------
    procedure Put(
      Item : in String_2)
      is
      begin
        Input_Output.Put(Item);
      end Put;
    --------------
    procedure Put(
    --------------
      Item : in Character_2)
      is
      begin
        Put(Item & "");
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
      Item : in String_2)
      is
      begin
        if Input_Output.Do_Put_Debug then
          Input_Output.Put(Item);
        end if;
      end Put_Debug;
    --------------------
    procedure Put_Debug(
    --------------------
      Item : in Character_2)
      is
      begin
        Put_Debug(Item & "");
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
      Put : in Not_Null_Access_Procedure_Put)
      is
      begin
        Input_Output.Set_Put(Put);
      end Set_Put;
  ------------------
  -- Set_Localize --
  ------------------
    procedure Set_Localize(
      Localize : in Access_Function_Localize)
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
        Input_Output.Set_Do_Put_Debug(Do_Put_Debug);
      end Set_Put_Debug;
  --------------
  -- Save_Log --
  --------------
    procedure Save_Log(
      Path : in String_2 := NULL_STRING_2)
      is
      begin
        null;--Input_Output.Get_Log;
      end Save_Log;
  ---------------
  -- Set_Error --
  ---------------
    procedure Set_Error(
      Text : in String_2)
      is
      begin
       null;
      end Set_Error;
  ---------------
  -- Get_Error --
  ---------------
    function Get_Error
      return String_2
      is
      begin
        return "";
      end Get_Error;
  ---------------------
  -- Get_Input_Entry --
  ---------------------
    function Get_Input_Entry
      return String_2
      is
      begin
        return Input_Output.Get_Input_Entry;
      end Get_Input_Entry;
  -------------
  -- Get_Log --
  -------------
    function Get_Log
      return String_2
      is
      begin
        return Input_Output.Get_Log;
      end Get_Log;
  -------------------
  -- Get_Line_Size --
  -------------------
    function Get_Line_Size
      return Integer_4_Positive
      is
      begin
        return Input_Output.Get_Line_Size;
      end Get_Line_Size;
  -------------------
  -- Get_Extension --
  -------------------
    function Get_Extension(
      Path : in String_2)
      return String_2
      is
      begin
        for I in reverse Path'range loop
          if Path(I) = '.' then
            exit when I = Path'last;
            return Path(I + 1..Path'last);
          end if;
        end loop;
        return NULL_STRING_2;
      end Get_Extension;
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
          return To_String_2(Ada.Strings.Fixed.Trim(Input, Both));
        end if;
        ----------------
        Remove_Notation:
        ----------------
          declare
          Trimmed_Input : String_1 := Ada.Strings.Fixed.Trim(Input, Both);
          begin
            return
              To_String_2(
                Ada.Strings.Fixed.Tail(
                  Source => Ada.Strings.Fixed.Head(Trimmed_Input, Trimmed_Input'last - 1),
                  Count  => Trimmed_Input'last - 2 - Ada.Strings.Fixed.Trim(Ada.Wide_Text_IO.Number_Base'Image(Base), Both)'length));
          end Remove_Notation;
      end To_Radian_Image;
  ---------------
  -- To_Stream --
  ---------------
--    function To_Stream(
--      Data : in String_1)
--      return Stream_Element_Array
--      is
--      Buffer : Array_Integer_Base64(Data'range) := 0;
--      Last   : Integer_4_Signed                 := Data'last;
--      begin
--        for I in Data'range loop
--          for J in BASE64_ALPHABET'range loop
--            if BASE64_ALPHABET(J) = Data(I) then
--              Buffer(I) := J;
--              if
--              (I = Data'last - 2 and then Data(I + 1..I + 2) = (BASE64_TERMINATION, BASE64_TERMINATION)) or
--              (I = Data'last - 1 and then Data(I + 1) = BASE64_TERMINATION)
--              then
--                Last := I;
--                exit;
--              end if;
--            end if;
--            if J = BASE64_ALPHABET'last then
--              raise Attempted_To_Decode_Non_Valid_Base_64_Digit_To_Data_Stream;
--            end if;
--          end loop;
--        end loop;
--        return To_Not_Null_Access_Stream_Element_Array(Buffer'access).all(Data'first..Data'first + (Last - Data'first) / 4 * 3);
--      end To_Stream;
  -----------------
  -- To_String_1 --
  -----------------
--      function To_String_1(
--        Data : in not null access Root_Stream_Type'class)
--        return String_1
--        is
--        Current  : Array_Integer_1_Unsigned(1..2) := (others => 0);
--        Result   : String_1(1..Data'size * 2)     := (others => ' '); -- This could be sized better
--        I        : Stream_Element_Offset          := Data'First;
--        J        : Integer_4_Signed               := 1;
--        begin
--          while I <= Data'last loop
--            if J + 4 > Result'last + 1 then
--              return Trim(Result, Both);
--            end if;
--            Current(1) := Byte(Data(I));
--            Result(J)  := BASE64_ALPHABET(Shift_Right(Current(1), 2));
--            if I = Data'last then
--              Result(J + 1..J + 3) := BASE64_ALPHABET(Shift_Left(Current(1) and 16#03#, 4)) & BASE64_ALPHABET'last & BASE64_ALPHABET'last;
--              return Trim(Result, Both);
--            end if;
--            Current(2)    := Byte(Data(I + 1));
--            Result(J + 1) := BASE64_ALPHABET(Shift_Left(Current(1) and 16#03#, 4) or Shift_Right(Current(2), 4));
--            if I = Data'last - 1 then
--              Result(J + 2..J + 3) := BASE64_ALPHABET(Shift_Left(Current(2) and 16#0F#, 2)) & BASE64_ALPHABET'last;
--              return Trim(Result, Both);
--            end if;
--            Current(1):= Byte(Data(I + 2));
--            Result(J + 2..J + 3) :=
--              BASE64_ALPHABET(Shift_Left(Current(2) and 16#0F#, 2) or Shift_Right(Current(1), 6)) &
--              BASE64_ALPHABET(Current(1) and 16#3F#);
--            J := J + 4;
--            I := I + 3;
--          end loop;
--        end To_String_1;
  -----------------
  -- To_String_1 --
  -----------------
    function To_String_1(
      Item : in String_1_C)
      return String_1
      is
      begin
        return To_Ada(Item);
      end To_String_1;
    ---------------------
    function To_String_1(
    ---------------------
      Item : in String_2)
      return String_1
      is
      Result : String_1(Item'range) := (others => NULL_CHARACTER_1);
      begin
        for I in Result'range loop
          Result(I) :=(
            if Character_2'pos(Item(I)) > Character_1'pos(Character_1'last) then
              CHARACTER_2_REPLACEMENT
            else
              Character_1'val(Character_2'pos(Item(I))));
        end loop;
        return Result;
      end To_String_1;
  -------------------
  -- To_String_2_C --
  -------------------
    function To_String_2_C(
      Item : in String_1_C)
      return String_2_C
      is
      Result : String_2_C(Item'range) := (others => NULL_CHARACTER_2_C);
      begin
        for I in Result'range loop
          Result(I) := Character_2_C'val(Character_1_C'pos(Item(I)));
        end loop;
        return Result;
      end To_String_2_C;
    -----------------------
    function To_String_2_C(
    -----------------------
      Item : in String_2)
      return String_2_C
      is
      begin
        return To_C(Item & NULL_CHARACTER_2);
      end To_String_2_C;
  -----------------
  -- To_String_2 --
  -----------------
    function To_String_2(
      Item : in String_1_C)
      return String_2
      is
      begin
        return To_String_2(To_String_2_C(Item));
      end To_String_2;
    ---------------------
    function To_String_2(
    ---------------------
      Item : in String_2_C)
      return String_2
      is
      Last   : Integer_Size_C     := Item'first;
      Buffer : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
      begin
        for I in Item'range loop
          if Item(I) = NULL_CHARACTER_2_C then
            return To_String_2(Buffer);
          end if;
          Buffer := Buffer & Character_2'val(Character_2_C'pos(Item(Integer_Size_C(I))));
        end loop;
        return To_String_2(Buffer);
      end To_String_2;
    ---------------------
    function To_String_2(
    ---------------------
      Item : in String_1)
      return String_2
      is
      Result : String_2(Item'range) := (others => NULL_CHARACTER_2);
      begin
        for I in Item'range loop
          Result(I) := Character_2'val(Character_1'pos(Item(I)));
        end loop;
        return Result;
      end To_String_2;
    ---------------------
    function To_String_2(
    ---------------------
      Item : in Access_Constant_Character_2_C)
      return String_2
      is
      Length  : Integer_4_Signed              := 0;
      Buffer  : String_2_Unbounded            := NULL_STRING_2_UNBOUNDED;
      Pointer : Access_Constant_Character_2_C := Item;
      begin
        while Pointer.all /= NULL_CHARACTER_2_C loop
          Length  := Length + 1;
          Buffer  := Buffer & Character_2(Pointer.all);
          Pointer :=
            To_Unchecked_Access_Constant_Character_2_C( -- "Don't do pointer arithmetic.  Seriously."
              To_Unchecked_Address(
                To_Unchecked_Integer_Address(Pointer) + Character_2_C'size / Byte'size));
        end loop;
        return To_String_2(Buffer);
      exception
        when others => -- Buffer overflow!
          return "";
      end To_String_2;
  -------------------
  -- To_String_1_C --
  -------------------
    function To_String_1_C(
      Item : in String_1)
      return String_1_C
      is
      begin
        return To_C(Item);
      end To_String_1_C;
    -----------------------
    function To_String_1_C(
    -----------------------
      Item : in String_2)
      return String_1_C
      is
      begin
        return To_String_1_C(To_String_1(Item));
      end To_String_1_C;
  -----------------------------
  -- To_Access_Character_1_C --
  -----------------------------
    function To_Access_Character_1_C(
      Item : in String_1_C)
      return Access_Character_1_C
      is
      begin
        return To_Unchecked_Access_Character_1_C(Item(Item'first)'address);
      end To_Access_Character_1_C;
  -----------------------------
  -- To_Access_Character_2_C --
  -----------------------------
    function To_Access_Character_2_C(
      Item : in String_2)
      return Access_Character_2_C
      is
      begin
        return To_Unchecked_Access_Character_2_C(To_String_2_C(Item)'address);
      end To_Access_Character_2_C;
  --------------------------------------
  -- To_Access_Constant_Character_2_C --
  --------------------------------------
    function To_Access_Constant_Character_2_C(
      Item : in String_2_C)
      return Access_Constant_Character_2_C
      is
      begin
        return To_Unchecked_Access_Constant_Character_2_C(Item(Item'first)'address);
      end To_Access_Constant_Character_2_C;
    ------------------------------------------
    function To_Access_Constant_Character_2_C(
    ------------------------------------------
      Item : in String_2)
      return Access_Constant_Character_2_C
      is
      begin
        return To_Access_Constant_Character_2_C(To_C(Item));
      end To_Access_Constant_Character_2_C;
  --------------------------------------
  -- To_Access_Constant_Character_1_C --
  --------------------------------------
    function To_Access_Constant_Character_1_C2(
      Item : in String_1_C)
      return Access_Constant_Character_1_C
      is
      begin
        return To_Unchecked_Access_Constant_Character_1_C(Item(Item'first)'address);
      end To_Access_Constant_Character_1_C2;
    ------------------------------------------
    function To_Access_Constant_Character_1_C(
    ------------------------------------------
      Item : in String_1)
      return Access_Constant_Character_1_C
      is
      begin
        return To_Access_Constant_Character_1_C2(To_C(Item));
      end To_Access_Constant_Character_1_C;
  -----------------------------
  -- To_Low_Order_Byte_First --
  -----------------------------
    function To_Low_Order_Byte_First(
      Item : in Stream_Element_Array)
      return Stream_Element_Array
      is
      begin
        return Item;
      end To_Low_Order_Byte_First;
  ------------------------------------------------
  -- To_Low_Order_Byte_Then_Low_Order_Bit_First --
  ------------------------------------------------
    function To_Low_Order_Byte_Then_Low_Order_Bit_First(
      Item : in Stream_Element_Array)
      return Stream_Element_Array
      is
      begin
        return Item;
      end To_Low_Order_Byte_Then_Low_Order_Bit_First;
  -------------------------------------------------
  -- To_Low_Order_Byte_Then_High_Order_Bit_First --
  -------------------------------------------------
    function To_Low_Order_Byte_Then_High_Order_Bit_First(
      Item : in Stream_Element_Array)
      return Stream_Element_Array
      is
      begin
        return Item;
      end To_Low_Order_Byte_Then_High_Order_Bit_First;
  ------------------------------
  -- To_High_Order_Byte_First --
  ------------------------------
    function To_High_Order_Byte_First(
      Item : in Stream_Element_Array)
      return Stream_Element_Array
      is
      begin
        return Item;
      end To_High_Order_Byte_First;
  -------------------------------------------------
  -- To_High_Order_Byte_Then_Low_Order_Bit_First --
  -------------------------------------------------
    function To_High_Order_Byte_Then_Low_Order_Bit_First(
      Item : in Stream_Element_Array)
      return Stream_Element_Array
      is
      begin
        return Item;
      end To_High_Order_Byte_Then_Low_Order_Bit_First;
  --------------------------------------------------
  -- To_High_Order_Byte_Then_High_Order_Bit_First --
  --------------------------------------------------
    function To_High_Order_Byte_Then_High_Order_Bit_First(
      Item : in Stream_Element_Array)
      return Stream_Element_Array
      is
      begin
        return Item;
      end To_High_Order_Byte_Then_High_Order_Bit_First;
  -----------------------------
  -- Is_Low_Order_Byte_First --
  -----------------------------
    function Is_Low_Order_Byte_First
      return Boolean
      is
      begin
        return DOES_MACHINE_ORDER_LOW_BYTE_FIRST;
      end Is_Low_Order_Byte_First;
  ------------------------------
  -- Is_High_Order_Byte_First --
  ------------------------------
    function Is_High_Order_Byte_First
      return Boolean
      is
      begin
        return not Is_Low_Order_Byte_First;
      end Is_High_Order_Byte_First;
  end Neo;
