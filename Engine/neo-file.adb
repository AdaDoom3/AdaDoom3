package body Neo.File is
  procedure Save(Name : in String_2; Item : in Array_Stream_Element) is
    begin
      null;
    end Save;
  function Load(Name : in String_2) return Array_Stream_Element is
    Junk : Stream_Element;
    begin
      return (Junk, Junk);
    end Load;
  function Get_Extension(Name : in String_2) return String_2 is
    Extension_Text : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
    begin
      for I in reverse Name'range loop
        if Name(I) = '.' and I /= Name'length then return To_Lower(Name(I + 1..Name'last)); end if;
      end loop;
      raise Unknown;
    end Get_Extension;
  function Build_Path(Name : in String_2) return String_2 is
    begin
      return PATH_ASSETS & SPECIFICS.Separator & Get_Extension(Name) & SPECIFICS.Separator & Name;
    end Build_Path;
  package body Handler is
      package body Format is
          procedure Initialize(Item : in out Record_Controller) is
            begin
              if Formats.Has_Element(Kind) then raise Duplicate_Format; end if;
              Formats.Insert(Kind, (Save, Load, To_String_2_Unbounded(To_Lower(Extensions))));
            end Initialize;
          procedure Finalize(Item : in out Record_Controller) is
            begin
              Formats.Delete(Kind);
            end Finalize;
        end Format;
      procedure Save(Name : in String_2; Item : in Type_To_Handle) is
        begin
          Formats.Element(Match_Extension(Name)).Save(Build_Path(Name), Item);
        end Save;
      function Load(Name : in String_2) return Type_To_Handle is
        begin
          return Formats.Element(Match_Extension(Name)).Load(Build_Path(Name));
        end Load;
      function Match_Extension(Value : in String_2) return Enumerated_Format is
        Current_Format : Ordered_Map_Record_Format.Cursor := Formats.First;
        Format         : Record_Format                    := (others => <>);
        begin
          while Formats.Has_Element(Current_Format) loop
            Format := Formats.Element(Current_Format);
            for Extension of Split(To_String_2(Format.Extensions), ",") loop
              if Extension = Get_Extension(Value) then return Formats.Key(Current_Format); end if;
            end loop;
            Formats.Next(Current_Format);
          end loop;
          raise Unsupported;
        end Match_Extension;
    end Handler;
  package body Parser is
      procedure Skip_Set(Starting, Ending : in String_2) is Junk : String_2_Unbounded := Next_Set(Starting, Ending); begin null; end Skip_Set;
      procedure Skip(Number_To_Skip : in Integer_4_Positive := 1) is
        Junk : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
        begin
          for I in 1..Number_To_Skip loop Junk := Next;Put_Line(Junk); end loop;
        end Skip;
      function At_End return Boolean is
        begin
          return (if Row > Data.Last_Index then True else False);
        end At_End;
      procedure Assert(Text : in String_2) is
        begin
          Put_Line("Assert: " & Text & "  " & Slice(Data.Element(Row), Column, Length(Data.Element(Row))));
          if Index(Slice(Data.Element(Row), Column, Length(Data.Element(Row))), Text) /= Column then raise Unlexable; end if;
          Column := Column + Text'length - 1;
          Seek;
        end Assert;
      function Peek return String_2_Unbounded is
        Previous_Column : Integer_4_Positive := Column;
        Previous_Row    : Integer_4_Positive := Row;
        Result          : String_2_Unbounded := Next;
        begin
          Row    := Previous_Row;
          Column := Previous_Column;
          return Result;
        end Peek;
      procedure Seek is
        begin
          Column := Column + 1;
          while not At_End loop
            loop
              if Column > Length(Data.Element(Row)) or Data.Element(Row) = NULL_STRING_2_UNBOUNDED then
                Column := 1;
                Row    := Row + 1;
                exit;
              end if;
              if Index(Slice(Data.Element(Row), Column, Length(Data.Element(Row))), Separator) /= Column then return;
              --elsif not Do_Ignore_Multiple then
              --  Column := Column + Separator'length;
              --  return;
              end if;
              Column := Column + 1;
            end loop;
          end loop;
        end Seek;
      function Next return String_2_Unbounded is
        Last   : Integer_4_Positive := Column + 1;
        Result : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
        begin
          while Last <= Length(Data.Element(Row)) loop
            if Index(Slice(Data.Element(Row), Column, Last), Separator) = Last then
              --Last := Last - 1;
              exit;
            end if;
            exit when Last = Length(Data.Element(Row));
            Last := Last + 1;
          end loop;
          if Last = Column then return NULL_STRING_2_UNBOUNDED; end if;
          Result := To_String_2_Unbounded(Slice(Data.Element(Row), Column, Last));
          Column := Last;
          Seek;
          return Result;
        end Next;
      function Next_Number return Float_8_Real is
        Entered_Digit : Boolean            := False;
        Found_Decimal : Boolean            := False;
        Found_Sign    : Boolean            := False;
        Result        : Float_8_Real       := 0.0;
        Last          : Integer_4_Positive := Column + 1;
        begin
          while Last <= Length(Data.Element(Row)) loop
            if not Is_Digit(Element(Data.Element(Row), Last)) then
              case Element(Data.Element(Row), Last) is
                when '-' | '+' =>
                  if Found_Sign or Entered_Digit then
                    Last := Last - 1;
                    exit;
                  end if;
                  Found_Sign := True;                    
                when '.' =>
                  if Found_Decimal then
                    Last := Last - 1;
                    exit;
                  end if;
                  Entered_Digit := True;
                  Found_Decimal := True;
                when others =>
                  Last := Last - 1;
                  exit;
              end case;
            else 
              Entered_Digit := True;
            end if;
            exit when Last = Length(Data.Element(Row));
            Last := Last + 1;
          end loop;
          if Last = Column then raise Unlexable; end if;
          Result := Float_8_Real'wide_value(Slice(Data.Element(Row), Column, Last));
          Column := Last;
          Seek;
          return Result;
        end Next_Number;
      function Next_Set(Starting, Ending : in String_2) return String_2_Unbounded is
        Last   : Integer_4_Positive := Column + 1;
        Result : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
        begin
          Assert(Starting);
          while not At_End loop
            loop
              if Column > Length(Data.Element(Row)) then
                Column := 1;
                Row    := Row + 1;
                Result := Result & END_LINE_2;
                exit;
              end if;
              if Index(Slice(Data.Element(Row), Column, Length(Data.Element(Row))), Ending) = Column then
                Column := Column + Separator'length;
                Seek;
                return Result;
              end if;
              Result := Result & Element(Data.Element(Row), Column);
              Column := Column + 1;
            end loop;
          end loop;
          raise Unlexable;
        end Next_Set;
    begin
      declare
      Raw_Data : File_Type;
      I        : Integer_4_Natural := 0;
      begin
        if Separator = NULL_STRING_2 then raise Unlexable; end if;
        Open(Raw_Data, In_File, To_String_1(To_String_2(Neo.System.SPECIFICS.Path) & SPECIFICS.Separator & Path));
        while not End_Of_File(Raw_Data) loop
          Data.Append(To_String_2_Unbounded(Get_Line(Raw_Data)));
          if Comment /= NULL_STRING_2 then
            I := Index(Data.Last_Element, Comment);
            if I = 1 then Data.Replace_Element(Data.Last_Index, NULL_STRING_2_UNBOUNDED); 
            elsif I /= 0 then Data.Replace_Element(Data.Last_Index, To_String_2_Unbounded(Slice(Data.Last_Element, 1, I - 1))); end if;
          end if;
          if Do_Convert_Tabs then
            loop
              I := Index(Data.Last_Element, TAB_2);
              exit when I = 0;
              Data.Replace_Element(Data.Last_Index, Overwrite(Data.Last_Element, I, " "));
            end loop;
          end if;
        end loop;
        Close(Raw_Data);
        Seek;
      end;
    end Parser;
end Neo.File;
