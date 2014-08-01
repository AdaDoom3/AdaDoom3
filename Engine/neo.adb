package body Neo is
    procedure Test is
      function To_String_2 is new Generic_To_String_2(Integer_4_Unsigned);
      Item    : String_2(1..10)    := (others => NULL_CHARACTER_2);
      Last    : Integer_4_Natural  := 0;
      Unicode : Character_2        := Character_2'val(512);
      Data    : Integer_4_Unsigned := 16#2345_6789#;
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
        Put_Line(Localize("Number Put: ") & To_String_2(Data, 2) & ", " & To_String_2(Data, 16) & ", " & To_String_2(Data, 10));
        Put_Line(Get_Log);
      end Test;
    package body Tasks is
        task body Task_Unsafe is begin accept Initialize(Id : in out Task_Id) do Id := Current_Task; end Initialize; Run; Put_Line("End?"); end Task_Unsafe;
        protected body Protected_Task is
            procedure Initialize               is begin Current_Task := new Task_Unsafe; Current_Task.Initialize(Current_Id); Data.Set_Number_Of_Tasks(Data.Get_Number_Of_Tasks + 1); end Initialize;
            procedure Finalize                 is begin Abort_Task(Current_Id); Finalize(Current_Task); Current_Task := null; Data.Set_Number_Of_Tasks(Data.Get_Number_Of_Tasks - 1);      end Finalize;
            function Is_Running return Boolean is begin return Current_Task /= null;                                                                                                  end Is_Running;
          end Protected_Task;
      end Tasks;
    package body Ordered_Maps is
        protected body Protected_Map is
            procedure Clear                                                        is begin Unsafe.Clear(Current_Data);                                end Clear;
            procedure Set        (Data : in Unsafe.Map)                            is begin Current_Data := Data;                                      end Set;
            procedure Next       (Position : in out Cursor)                        is begin Position := Unsafe.Next(Position);                         end Next;
            procedure Delete     (Position : in out Cursor)                        is begin Unsafe.Delete(Current_Data, Position);                     end Delete;
            procedure Replace    (Position : in Cursor; New_Item : in Type_To_Map) is begin Unsafe.Replace_Element(Current_Data, Position, New_Item);  end Replace;
            procedure Insert     (Key : in Type_To_Key; New_Item : in Type_To_Map) is begin Unsafe.Insert(Current_Data, Key, New_Item);                end Insert;
            procedure Replace    (Key : in Type_To_Key; New_Item : in Type_To_Map) is begin Unsafe.Replace(Current_Data, Key, New_Item);               end Replace;
            procedure Delete     (Key : in Type_To_Key)                            is begin Unsafe.Delete(Current_Data, Key);                          end Delete;
            function Element     (Key : in Type_To_Key) return Type_To_Map         is begin return Unsafe.Element(Current_Data, Key);                  end Element;
            function Element     (Position : in Cursor) return Type_To_Map         is begin return Unsafe.Element(Position);                           end Element;
            function Has_Element (Key : in Type_To_Key) return Boolean             is begin return Unsafe.Has_Element(Unsafe.Find(Current_Data, Key)); end Has_Element;
            function Has_Element (Position : in Cursor) return Boolean             is begin return Unsafe.Has_Element(Position);                       end Has_Element;
            function Key         (Position : Cursor)    return Type_To_Key         is begin return Unsafe.Key(Position);                               end Key;
            function Get                                return Unsafe.Map          is begin return Current_Data;                                       end Get;
            function First                              return Cursor              is begin return Unsafe.First(Current_Data);                         end First;
          end Protected_Map;
      end Ordered_Maps;
    package body Hashed_Maps is
        protected body Protected_Map is
            procedure Clear                                                        is begin Unsafe.Clear(Current_Data);                                                       end Clear;
            procedure Set        (Data : in Unsafe.Map)                            is begin Current_Data := Data;                                                             end Set;
            procedure Next       (Position : in out Cursor)                        is begin Position := Unsafe.Next(Position);                                                end Next;
            procedure Delete     (Position : in out Cursor)                        is begin Unsafe.Delete(Current_Data, Position);                                            end Delete;
            procedure Replace    (Position : in Cursor; New_Item : in Type_To_Map) is begin Unsafe.Replace_Element(Current_Data, Position, New_Item);                         end Replace;
            procedure Insert     (Key : in String_2;    New_Item : in Type_To_Map) is begin Unsafe.Insert(Current_Data, To_String_2_Unbounded(Key), New_Item);                end Insert;
            procedure Replace    (Key : in String_2;    New_Item : in Type_To_Map) is begin Unsafe.Replace(Current_Data, To_String_2_Unbounded(Key), New_Item);               end Replace;
            procedure Delete     (Key : in String_2)                               is begin Unsafe.Delete(Current_Data, To_String_2_Unbounded(Key));                          end Delete;
            function Element     (Key : in String_2)    return Type_To_Map         is begin return Unsafe.Element(Current_Data, To_String_2_Unbounded(Key));                  end Element;
            function Element     (Position : in Cursor) return Type_To_Map         is begin return Unsafe.Element(Position);                                                  end Element;
            function Has_Element (Key : in String_2)    return Boolean             is begin return Unsafe.Has_Element(Unsafe.Find(Current_Data, To_String_2_Unbounded(Key))); end Has_Element;
            function Has_Element (Position : in Cursor) return Boolean             is begin return Unsafe.Has_Element(Position);                                              end Has_Element;
            function Key         (Position : Cursor)    return String_2            is begin return To_String_2(Unsafe.Key(Position));                                         end Key;
            function Get                                return Unsafe.Map          is begin return Current_Data;                                                              end Get;
            function First                              return Cursor              is begin return Unsafe.First(Current_Data);                                                end First;
          end Protected_Map;
      end Hashed_Maps;
    package body Vectors is
        protected body Protected_Vector is
            procedure Clear                                                                                                         is begin Unsafe.Clear(Current_Data);                                       end Clear;
            procedure Set        (Data : in Unsafe.Vector)                                                                          is begin Current_Data := Data;                                             end Set;
            procedure Next       (Position : in out Cursor)                                                                         is begin Unsafe.Next(Position);                                            end Next;
            procedure Replace    (Position : in Cursor;           New_Item : in Type_To_Vector)                                     is begin Unsafe.Replace_Element(Current_Data, Position, New_Item);         end Replace; 
            procedure Append                                     (New_Item : in Type_To_Vector; Count : in Integer_4_Positive := 1) is begin Unsafe.Append(Current_Data, New_Item, Count_Type(Count));         end Append;
            procedure Prepend                                    (New_Item : in Type_To_Vector; Count : in Integer_4_Positive := 1) is begin Unsafe.Prepend(Current_Data, New_Item, Count_Type(Count));        end Prepend;
            procedure Insert     (Before : in Integer_4_Positive; New_Item : in Type_To_Vector; Count : in Integer_4_Positive := 1) is begin Unsafe.Insert(Current_Data, Before, New_Item, Count_Type(Count)); end Insert;
            procedure Delete     (Index  : in Integer_4_Positive;                               Count : in Integer_4_Positive := 1) is begin Unsafe.Delete(Current_Data, Count);                               end Delete;
            function Element     (Index  : in Integer_4_Positive) return Type_To_Vector                                             is begin return Unsafe.Element(Current_Data, Index);                       end Element;
            function Element     (Position : in Cursor)           return Type_To_Vector                                             is begin return Unsafe.Element(Position);                                  end Element;
            function Has_Element (Position : in Cursor)           return Boolean                                                    is begin return Unsafe.Has_Element(Position);                              end Has_Element;
            function Length                                       return Integer_4_Positive                                         is begin return Integer_4_Positive(Unsafe.Length(Current_Data));           end Length;
            function Get                                          return Unsafe.Vector                                              is begin return Current_Data;                                              end Get;
            function First                                        return Cursor                                                     is begin return Unsafe.First(Current_Data);                                end First;
          end Protected_Vector;
      end Vectors;
    protected body Protected_Status is
        function Is_Doing_Something      return Boolean       is begin return Status;   end Is_Doing_Something;
        procedure Set_Is_Doing_Something (Value : in Boolean) is begin Status := Value; end Set_Is_Doing_Something;
      end Protected_Status;
    protected body Protected_Data is
        procedure Set_Do_Put_Debug    (Value : in Boolean)                            is begin Current_Do_Put_Debug := Value;                                                 end Set_Do_Put_Debug;
        procedure Set_Line_Size       (Value : in Integer_4_Positive)                 is begin Current_Line_Size := Value;                                                    end Set_Line_Size;
        procedure Set_Number_of_Tasks (Value : in Integer_4_Positive)                 is begin Current_Number_Of_Tasks := Value;                                              end Set_Number_Of_Tasks;
        procedure Set_Put             (Value : in Access_Procedure_Put)               is begin Current_Put := Value;                                                          end Set_Put;
        procedure Set_Localize        (Value : in Access_Function_Localize)           is begin Current_Localize := Value;                                                     end Set_Localize;
        procedure Set_Input_Entry     (Value : in String_2)                           is begin Current_Input_Entry := To_String_2_Unbounded(Value);                           end Set_Input_Entry;
        procedure Set_Errors          (Value : in String_2)                           is begin Current_Errors := Current_Errors & To_String_2_Unbounded(Value);               end Set_Errors;
        function Localize             (Item  : in String_2) return String_2           is begin return (if Current_Localize = null then Item else Current_Localize.all(Item)); end Localize;
        function Get_Log                                    return String_2           is begin return To_String_2(Current_Log);                                               end Get_Log;
        function Get_Errors                                 return String_2           is begin return To_String_2(Current_Errors);                                            end Get_Errors;
        function Get_Input_Entry                            return String_2           is begin return To_String_2(Current_Input_Entry);                                       end Get_Input_Entry;
        function Do_Put_Debug                               return Boolean            is begin return Current_Do_Put_Debug;                                                   end Do_Put_Debug;
        function Get_Line_Size                              return Integer_4_Positive is begin return Current_Line_Size;                                                      end Get_Line_Size;
        function Get_Number_Of_Lines                        return Integer_8_Natural  is begin return Current_Number_Of_Lines;                                                end Get_Number_Of_Lines;
        function Get_Number_Of_Tasks                        return Integer_4_Positive is begin return Current_Number_Of_Tasks;                                                end Get_Number_Of_Tasks;
        procedure Put(Item : in String_2) is
          Count : Integer_4_Natural := 0;
          begin
            Current_Log := Current_Log & To_String_2_Unbounded(Item);
            for I in Item'range loop
              if Item(I) = Character_2'val(Character_1'pos(ASCII.CR)) then Count := Count + 1; end if;
            end loop;
            Current_Number_Of_Lines := Current_Number_Of_Lines + 1;
            if Current_Put /= null then Current_Put.all(Item); end if;
          end Put;
      end Protected_Data;
    function Get_Duration(Timer : in Record_Timer) return Duration is begin return (if Timer.Is_Stopped then Timer.Last else Clock - Timer.Start); end Get_Duration;
    procedure Start(Timer : in out Record_Timer) is
      begin
        if not Timer.Is_Stopped then raise Timer_Started_Without_Being_Stopped; end if;
        Timer := (Is_Stopped => False, Start => Clock, others => <>);
      end Start;
    procedure Stop(Timer : in out Record_Timer) is
      begin
        if Timer.Is_Stopped then raise Timer_Stopped_Without_Being_Started; end if;
        Timer := (Is_Stopped => True, Last => Timer.Start - Clock, others => <>);
      end Stop;
    procedure New_Line         (Count : in Integer_4_Positive := 1)            is begin for I in 1..Count loop Put(END_LINE_2); end loop; end New_Line;
    procedure Put_Debug        (Item  : in Character_2)                        is begin Put_Debug(Item & "");                             end Put_Debug;
    procedure Set_Input_Entry  (Value : in String_2)                           is begin Data.Set_Input_Entry(Value);                      end Set_Input_Entry;
    procedure Set_Errors       (Value : in String_2)                           is begin Data.Set_Errors(Value);                           end Set_Errors;
    procedure Set_Do_Put_Debug (Value : in Boolean)                            is begin Data.Set_Do_Put_Debug(Value);                     end Set_Do_Put_Debug; 
    procedure Set_Line_Size    (Value : in Integer_4_Positive)                 is begin Data.Set_Line_Size(Value);                        end Set_Line_Size;
    procedure Set_Put          (Value : in Access_Procedure_Put)               is begin Data.Set_Put(Value);                              end Set_Put;
    procedure Put              (Item  : in Character_2)                        is begin Put(Item & "");                                   end Put;                  
    procedure Put              (Item  : in String_2)                           is begin Data.Put(Item);                                   end Put;
    procedure Put_Line         (Item  : in String_2)                           is begin Put(Item); New_Line;                              end Put_Line;
    procedure Put_Debug        (Item  : in String_2)                           is begin if Data.Do_Put_Debug then Data.Put(Item); end if; end Put_Debug;
    procedure Put_Debug_Line   (Item  : in String_2)                           is begin Put_Debug(Item); New_Line;                        end Put_Debug_Line;                  
    function Localize          (Item  : in String_2) return String_2           is begin return Data.Localize(Item);                       end Localize;
    function Get_Extension     (Path  : in String_2) return String_2           is begin return Path(Index(Path, ".") + 1..Path'last);     end Get_Extension;
    function Get_Log                                 return String_2           is begin return Data.Get_Log;                              end Get_Log;
    function Get_Input_Entry                         return String_2           is begin return Data.Get_Input_Entry;                      end Get_Input_Entry; 
    function Get_Errors                              return String_2           is begin return Data.Get_Errors;                           end Get_Errors;
    function Get_Number_Of_Lines                     return Integer_8_Natural  is begin return Data.Get_Number_Of_Lines;                  end Get_Number_Of_Lines;
    function Get_Line_Size                           return Integer_4_Positive is begin return Data.Get_Line_Size;                        end Get_Line_Size;
    procedure Put_Title(Item : in String_2) is
      Space_Count : Integer_4_Signed := 0;
      Count       : Integer_4_Signed := Item'length * 3 - Item'length / 3;
      begin
        if Item'length > Get_Line_Size then raise Title_Is_Too_Long; end if;
        New_Line;
        for I in 1..Get_Line_Size loop Put(TESTING_SEPARATOR); end loop;
        New_Line(2);
        for I in 1..Item'length loop
          if Item(I) = ' ' then  Space_Count := Space_Count + 1; end if;
        end loop;
        if Count + Space_Count >= Get_Line_Size then
          for I in 1..(Get_Line_Size / 2 - (Item'length) / 2 - 1) loop Put(" "); end loop;
          Put(Item);
        else
          for I in 1..(Get_Line_Size / 2 - (Count + Space_Count) / 2 - 1) loop Put(" "); end loop;
          for I in 1..Item'length loop
            Put(Item(I) & "  ");
            if Item(I) = ' ' then  Put(" "); end if;
          end loop;
        end if;
        New_Line;
        for I in 1..Get_Line_Size loop Put(TESTING_SEPARATOR); end loop;
        New_Line(2);
      end Put_Title;
    function To_String_1_C                     (Item : in String_1)           return String_1_C                    is begin return To_C(Item, True);                                                     end To_String_1_C;
    function To_String_1                       (Item : in String_1_C)         return String_1                      is begin return To_Ada(Item, True);                                                   end To_String_1;
    function To_String_2_C                     (Item : in String_2_Unbounded) return String_2_C                    is begin return To_String_2_C(To_String_2(Item));                                     end To_String_2_C;
    function To_String_2_C                     (Item : in String_2)           return String_2_C                    is begin return To_C(Item & NULL_CHARACTER_2);                                        end To_String_2_C;
    function To_String_2                       (Item : in String_1_C)         return String_2                      is begin return To_String_2(To_String_1(Item));                                       end To_String_2;
    --function To_String_1_C                     (Item : in String_2)           return String_1_C                    is begin return To_String_1_C(To_String_1(Item));                                     end To_String_1_C;
    function To_Access_Character_1_C           (Item : in String_1_C)         return Access_Character_1_C          is begin return To_Unchecked_Access_Character_1_C(Item(Item'first)'address);          end To_Access_Character_1_C;
    function To_Access_Character_2_C           (Item : in String_2)           return Access_Character_2_C          is begin return To_Unchecked_Access_Character_2_C(To_String_2_C(Item)'address);       end To_Access_Character_2_C;
    function To_Access_Constant_Character_2_C  (Item : in String_2_C)         return Access_Constant_Character_2_C is begin return To_Unchecked_Access_Constant_Character_2_C(Item(Item'first)'address); end To_Access_Constant_Character_2_C;
    function To_Access_Constant_Character_2_C  (Item : in String_2)           return Access_Constant_Character_2_C is begin return To_Access_Constant_Character_2_C(To_C(Item));                         end To_Access_Constant_Character_2_C;
    function To_Access_Constant_Character_1_C2 (Item : in String_1_C)         return Access_Constant_Character_1_C is begin return To_Unchecked_Access_Constant_Character_1_C(Item(Item'first)'address); end To_Access_Constant_Character_1_C2;
    function To_Access_Constant_Character_1_C  (Item : in String_1)           return Access_Constant_Character_1_C is begin return To_Access_Constant_Character_1_C2(To_C(Item));                        end To_Access_Constant_Character_1_C;
    function To_String_1(Item : in String_2) return String_1 is
      Result : String_1(Item'range) := (others => NULL_CHARACTER_1);
      begin
        for I in Result'range loop
          Result(I) := (if Character_2'pos(Item(I)) > Character_1'pos(Character_1'last) then CHARACTER_2_REPLACEMENT else Character_1'val(Character_2'pos(Item(I))));
        end loop;
        return Result;
      end To_String_1;
    function Generic_To_String_2(Item : in Type_Number; Base : in Integer_4_Positive; Spacing : in Integer_4_Natural := 0) return String_2 is
      package Type_Number_Text_IO is new Ada.Text_IO.Modular_IO(Type_Number);
      Input : String_1(1..RADIAN_IMAGE_STRING_SIZE) := (others => NULL_CHARACTER_1);
      begin
        Type_Number_Text_IO.Put(Input, Item, Ada.Wide_Text_IO.Number_Base(Base));
        if Base = 10 then return To_String_2(Ada.Strings.Fixed.Trim(Input, Both)); end if;
        return To_String_2(Input(Input'first - 1..Input'last - 2 - Ada.Strings.Fixed.Trim(Integer_4_Positive'image(Base), Both)'length));
      end Generic_To_String_2;
    function To_String_2_C(Item : in String_1_C) return String_2_C is
      Result : String_2_C(Item'range) := (others => NULL_CHARACTER_2_C);
      begin
        for I in Result'range loop Result(I) := Character_2_C'val(Character_1_C'pos(Item(I))); end loop;
        return Result;
      end To_String_2_C;
    function To_String_2(Item : in String_2_C) return String_2 is
      Last   : Integer_Size_C     := Item'first;
      Buffer : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
      begin
        for I in Item'range loop
          if Item(I) = NULL_CHARACTER_2_C then return To_String_2(Buffer); end if;
          Buffer := Buffer & Character_2'val(Character_2_C'pos(Item(Integer_Size_C(I))));
        end loop;
        return To_String_2(Buffer);
      end To_String_2;
    function To_String_2(Item : in String_1) return String_2 is
      Result : String_2(Item'range) := (others => NULL_CHARACTER_2);
      begin
        for I in Item'range loop Result(I) := Character_2'val(Character_1'pos(Item(I))); end loop;
        return Result;
      end To_String_2;
    function To_String_2(Item : in Access_Constant_Character_2_C) return String_2 is
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
      exception when others => raise Storage_Error;
      end To_String_2;
  begin
    null; -- Create directories if absent
  end Neo;
