
--                                                                                                                    
--                                                 A D A  D O O M  III                                                    
--                                                                                                                    
--                                         Copyright (C) 2016 Justin Squirek                                          
-- 
-- AdaDoom3 is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. 
--                                                                                                                    
-- AdaDoom3 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.         
--                                                                                                                    
-- You should have received a copy of the GNU General Public License along with AdaDoom3. If not, see gnorg/licenses     
--

package body Doom is

  ------------
  -- Timing --
  ------------

  Timer_Error : Exception;
  function Get_Duration (Timer : Timer_State) return Duration is ((if Timer.Is_Stopped then Timer.Last else Clock - Timer.Start));
  procedure Start (Timer : in out Timer_State) is
    begin
      if not Timer.Is_Stopped then raise Timer_Error with "Started without being stopped"; end if;
      Timer := (Is_Stopped => False, Start => Clock, others => <>);
    end;
  procedure Stop (Timer : in out Timer_State) is
    begin
      if Timer.Is_Stopped then raise Timer_Error with "Stopped without being started"; end if;
      Timer := (Is_Stopped => True, Last => Timer.Start - Clock, others => <>);
    end;

  ------------
  -- Status --
  ------------

  protected body Protected_Status is
      function Is_Doing_Something      return Bool    is (Status);
      procedure Set_Is_Doing_Something (Value : Bool) is begin Status := Value; end;
    end;

  ----------------
  -- Containers --
  ----------------

  package body Trees is
      protected body Protected_Tree is
          procedure Clear                                                                              is begin Clear (Data); end;
          procedure Iterate      (Parent : Cursor; Process : not null access procedure (Pos : Cursor)) is begin Iterate (Data, Process); end;
          procedure Back_Iterate (Parent : Cursor; Process : not null access procedure (Pos : Cursor)) is begin Reverse_Iterate_Children (Parent, Process); end;
          procedure Prepend      (Parent : Cursor;              New_Item : Tree_T)                     is begin Prepend_Child (Data, Parent, New_Item, Count_Type (Count)); end;
          procedure Append       (Parent : Cursor;              New_Item : Tree_T)                     is begin Append_Child (Data, Parent, New_Item, Count_Type (Count)); end;
          procedure Insert       (Parent, Before, Pos : Cursor; New_Item : Tree_T)                     is begin ; end;
          procedure Splice       (Parent, Before, Pos : Cursor; Source_Tree : in out Tree)             is begin ; end;
          procedure Copy_Subtree (Parent, Before, Pos : Cursor)                                        is begin ; end;
        end;
    end;
  package body Ordered_Maps is
      protected body Protected_Map is
          procedure Clear                                         is begin Clear (Data);                                end;
          procedure Set     (Map : Map)                           is begin Data     := Map;                             end;
          procedure Next    (Position : in out Cursor)            is begin Position := Next (Position);                 end;
          procedure Delete  (Position : in out Cursor)            is begin Delete           (Data, Position);           end;
          procedure Replace (Position : Cursor; New_Item : Map_T) is begin Replace_Element  (Data, Position, New_Item); end;
          procedure Replace (Key : Key_T;       New_Item : Map_T) is begin Replace          (Data, Key, New_Item);      end;
          procedure Insert  (Key : Key_T;       New_Item : Map_T) is begin Insert           (Data, Key, New_Item);      end;
          procedure Delete  (Key : Key_T)                         is begin Delete           (Data, Key);                end;
        end;
    end;
  package body Hashed_Maps is
      protected body Protected_Map is
          procedure Clear                                         is begin Clear (Data);                                               end;
          procedure Set     (Map : Map)                           is begin Data     := Map;                                            end;
          procedure Next    (Position : in out Cursor)            is begin Position := Next (Position);                                end;
          procedure Delete  (Position : in out Cursor)            is begin Delete           (Data, Position);                          end;
          procedure Replace (Position : Cursor; New_Item : Map_T) is begin Replace_Element  (Data, Position, New_Item);                end;
          procedure Replace (Key : Str_16;      New_Item : Map_T) is begin Replace          (Data, To_Str_16_Complex (Key), New_Item); end;
          procedure Insert  (Key : Str_16;      New_Item : Map_T) is begin Insert           (Data, To_Str_16_Complex (Key), New_Item); end;
          procedure Delete  (Key : Str_16)                        is begin Delete           (Data, To_Str_16_Complex (Key));           end;
        end;
    end;
  package body Vectors is
      protected body Protected_Vector is
          procedure Clear                                                                              is begin Clear (Data);                                                   end;
          procedure Set     (Vector   : Vector)                                                        is begin Data := Vector;                                                 end;
          procedure Next    (Position : in out Cursor)                                                 is begin Next            (Position);                                     end;
          procedure Replace (Position :        Cursor; New_Item : Vec_T)                               is begin Replace_Element (Data, Position, New_Item);                     end; 
          procedure Append                            (New_Item : Vec_T; Count : Int_32_Positive := 1) is begin Append          (Data,           New_Item, Count_Type (Count)); end;
          procedure Prepend                           (New_Item : Vec_T; Count : Int_32_Positive := 1) is begin Prepend         (Data,           New_Item, Count_Type (Count)); end;
          procedure Insert  (Before : Int_32_Positive; New_Item : Vec_T; Count : Int_32_Positive := 1) is begin Insert          (Data, Before,   New_Item, Count_Type (Count)); end;
          procedure Delete  (Index  : Int_32_Positive;                   Count : Int_32_Positive := 1) is begin Delete          (Data, Count);                                  end;
        end;
    end;

  -----------------
  -- Conversions --
  -----------------

  function To_Char_8 (Item : Char_16) return Char_8 is
    begin
      return (if Char_16'Pos (Item) > Char_8'Pos (Char_8'Last) then CHARACTER_16_REPLACEMENT else Char_8'Val (Char_16'Pos (Item)));
    end;
  function To_Str_8 (Item : Str_16) return Str_8 is
    Result : Str_8 (Item'Range);
    begin
      for Element in Item (I)'Range loop Result.Append (To_Char_8 (Element)); end loop;
      return Result;
    end;
  function To_Str_16_C (Item : Str_8_C) return Str_16_C is
    Result : Str_16_C (Item'Range);
    begin
      for Element in Item'Range loop Result.Append (Char_16_C'Val (Char_8_C'Pos (Element)); end loop;
      return Result;
    end;
  function To_Str_16 (Item : Str_8) return Str_16 is
    Result : Str_16 (Item'Range) := (others => NULL_CHAR_16);
    begin
      for I in Item'Range loop Result (I) := Char_16'Val (Char_8'Pos (Item (I))); end loop;
      return Result;
    end;
  function To_Str_16 (Item : Str_16_C) return Str_16 is
    Last   : Int_Size_C := Item'First;
    Buffer : Str_16_Complex;
    begin
      for I in Item'Range loop
        exit when Item (I) = NULL_CHAR_16_C;
        Buffer := Buffer & Char_16'Val (Char_16_C'Pos (Item (Int_Size_C (I))));
      end loop;
      return To_Str_16 (Buffer);
    end;
  function To_Str_16 (Item : Ptr_Const_Char_16_C) return Str_16 is
    Length : Int_32_Signed;
    Buffer : Str_16_Complex;
    begin
      while Item.All /= NULL_CHAR_16_C loop
        Length := Length + 1;
        Buffer := Buffer & Char_16 (Item.All);
        Item   := To_Ptr_Const_Char_16_C (To_Address (To_Int_Address (Item) + Char_16_C'Size / Int_8_Unsigned'Size));
      end loop;
      return To_Str_16 (Buffer);
    end;          
  function To_Str_8 (Item : Ptr_Const_Char_8_C) return Str_8 is 
    Length : Int_32_Signed;
    Buffer : Str_8_Complex;
    begin
      while Item.All /= NULL_CHAR_8_C loop
        Length := Length + 1;
        Buffer := Buffer & Char_8 (Item.All);
        Item   := To_Ptr_Const_Char_8_C (To_Address (To_Int_Address (Item) + Char_8_C'Size / Int_8_Unsigned'Size));
      end loop;
      return To_Str_8 (Buffer);
    end;
  function Generic_To_Str_16 (Item : Num_T; Base : Int_32_Positive; Do_Pad_Zeros : Bool := True) return Str_16 is
    package Num_T_Text_IO is new Ada.Wide_Text_IO.Modular_IO (Num_T); use Num_T_Text_IO;
    Buffer : Str_16_Complex;
    Input  : Str_16 (1..RADIAN_IMAGE_STRING_SIZE);
    begin
      Put (Input, Item, Number_Base (Base));
      if Base = 10 then return Trim (Input, Both); end if;
      Buffer := To_Str_16_Complex (Trim (Input, Both));
      Delete (Buffer, 1, Trim (Base'Img, Both)'Length + 1);
      Delete (Buffer, Length (Buffer), Length (Buffer));
      if Item /= Num_T'Last and Do_Pad_Zeros then
        for I in 1..Generic_To_Str_16 (Num_T'Last, Base)'Length - Length (Buffer) loop Insert (Buffer, 1, "0"); end loop;
      end if;
      return To_Str_16 (Buffer);
    end;

  ---------
  -- Str --
  ---------

  function Split (Item : Str_16; On : Str_16 := " ") return Array_Str_16_Complex is
    package Vector_Str_16_Complex is new Ada.Containers.Indefinite_Vectors (Int_32_Positive, Str_16_Complex);
    function Internal (Item : Str_16; On : Str_16 := " ") return Vector_Str_16_Complex.Vector is
      Result    : Vector_Str_16_Complex.Vector;
      TRIMMED   : constant Str_16 := Trim (Item, Both);
      REMAINDER : constant Natural   := Index (TRIMMED, On);
      begin
        if REMAINDER = 0 then
          Result.Append (To_Str_16_Complex (TRIMMED));
          return Result;
        else Result.Append (To_Str_16_Complex (Trim (TRIMMED (TRIMMED'First..REMAINDER - 1), Both))); end if;
        Result.Append (Internal (TRIMMED (REMAINDER..TRIMMED'Last), On));
        return Result;
      end;
    Internal_Result : Vector_Str_16_Complex.Vector;
    begin
      Internal_Result := Internal (Item, On);
      declare
      Result : Array_Str_16_Complex (1..Int_32_Signed (Internal_Result.Length));
      begin
        for I in Result'Range loop Result (I) := Internal_Result.Element (I); end loop;
        return Result;
      end;
    end;

  -------------
  -- Parsing --
  -------------

  procedure Save (Name : Str_16; Item : Array_Stream_Element) is
    begin
      null;
    end;
  function Load (Name : Str_16) return Array_Stream_Element is
    Junk : Stream_Element;
    begin
      return (Junk, Junk);
    end;
  function Get_Extension (Name : Str_16) return Str_16 is
    Extension_Text : Str_16_Complex := NULL_STRING_16_UNBOUNDED;
    begin
      for I in reverse Name'Range loop
        if Name (I) = '.' and I /= Name'length then return To_Lower (Name (I + 1..Name'Last)); end if;
      end loop;
      raise Unknown;
    end;
  package body Handler is
      procedure Save (Name : Str_16; Item : T) is
        begin
          Formats.Element (Match_Extension (Name)).Save (Build_Path (Name), Item);
        end Save;
      function Load (Name : Str_16) return T is
        begin
          return Formats.Element (Match_Extension (Name)).Load (Build_Path (Name));
        end Load;
      function Match_Extension (Value : Str_16) return Enumerated_Format is

            Format := Formats.Element (Current);
            for Extension of Split (To_Str_16 (Format.Extensions), ",") loop
              if Extension = Get_Extension (Value) then return Formats.Key (Current); end if;
            end loop;
          raise Unsupported;
        end;
      package body Format is
          procedure Initialize (Item : in out Record_Controller) is
            begin
              if Formats.Has_Element (Kind) then raise Duplicate_Format; end if;
              Formats.Insert (Kind, (Save, Load, To_Str_16_Complex (To_Lower (Extensions))));
            end Initialize;
          procedure Finalize (Item : in out Record_Controller) is
            begin
              Formats.Delete (Kind);
            end Finalize;
        end Format;
    end Handler;
  package body Parser is
      function At_End return Bool is ((if Row > Data.Last_Index then True else False));
      procedure Skip_Set (Starting, Ending : Str_16) is Junk : Str_16_Complex := Next_Set (Starting, Ending); begin null; end;
      procedure Skip (Number_To_Skip : Int_32_Positive := 1) is
        Junk : Str_16_Complex;
        begin
          for I in 1..Number_To_Skip loop Junk := Next; end loop;
        end;
      procedure Assert (Text : Str_16) is
        begin
          if Index (Slice (Data.Element (Row), Column, Length (Data.Element (Row))), Text) /= Column then raise Invalid; end if;
          Column := Column + Text'length - 1;
          Seek;
        end;
      function Peek return Str_16_Complex is
        Previous_Column : Int_32_Positive := Column;
        Previous_Row    : Int_32_Positive := Row;
        Result          : Str_16_Complex := Next;
        begin
          Row    := Previous_Row;
          Column := Previous_Column;
          return Result;
        end;
      procedure Seek is
        begin
          Column := Column + 1;
          while not At_End loop
            loop
              if Column > Length (Data.Element (Row)) or else Data.Element (Row) = NULL_STRING_16_UNBOUNDED then
                Column := 1;
                Row    := Row + 1;
                exit;
              end if;
              if Index (Slice (Data.Element (Row), Column, Length (Data.Element (Row))), Separator) /= Column then return;
              --if Length (Data.Element (Row)) - Column > Separator'length and then Slice (Data.Element (Row), Column, Column + Separator'length) = Separator then return;
              elsif not Do_Ignore_Multiple then
                Column := Column + Separator'length;
                return;
              end if;
              Column := Column + 1;
            end loop;
          end loop;
        end;
      function Next return Str_16_Complex is
        Result : Str_16_Complex := To_Str_16_Complex (Slice (Data.Element (Row), Column, Length (Data.Element (Row))));
        I      : Int_32_Natural  := Index (Result, Separator);
        begin
          if I /= 0 then Delete (Result, I, Length (Result)); end if;
          Column := Column + Length (Result);
          Seek;
          return Result;
        end;
      function Next_Number return Float_64_Real is
        Found_Decimal : Bool := False;
        Found_Digit   : Bool := False;
        Found_Sign    : Bool := False;
        Result        : Str_16_Complex;
        begin
          while Column <= Length (Data.Element (Row)) loop
            if not Is_Digit (Element (Data.Element (Row), Column)) then
              case Element (Data.Element (Row), Column) is
                when '-' | '+' => exit when Found_Digit or Found_Sign;
                                  Found_Sign := True;                    
                when '.' =>       exit when Found_Decimal;
                                  Found_Digit   := True;
                                  Found_Decimal := True;
                when others =>    exit;
              end case;
            elsif not Found_Digit then Found_Digit := True; end if;
            Result := Result & Element (Data.Element (Row), Column);
            Column := Column + 1;
          end loop;
          if Result = NULL_STRING_16_UNBOUNDED then raise Invalid; end if;
          Seek;
          return Float_64_Real'Wide_Value (To_Str_16 (Result));
        end;
      function Next_Set (Starting, Ending : Str_16) return Str_16_Complex is
        Result : Str_16_Complex;
        Buffer : Str_16_Complex;
        I      : Int_32_Natural;
        begin
          Assert (Starting);
          while not At_End loop
            Buffer := To_Str_16_Complex (Slice (Data.Element (Row), Column, Length (Data.Element (Row))));
            I      := Index (Buffer, Ending);
            if I = 0 then
              Column := 1;
              Row    := Row + 1;
              Result := Result & Buffer & END_LINE_16;
            else
              Buffer := Delete (Buffer, I, Length (Buffer));
              Column := Column + Length (Buffer) + Ending'length;
              Seek;
              return Result & Buffer;
            end if;
          end loop;
          raise Invalid;
        end;
    begin
      declare
      Raw_Data             : File_Type;
      In_Multiline_Comment : Bool := False;
      begin
        Open (Raw_Data, In_File, Path);
        while not End_Of_File (Raw_Data) loop
          Data.Append (To_Str_16_Complex (Get_Line (Raw_Data)));
          if Comment_Singleline /= NULL_STRING_16 and then Index (Data.Last_Element, Comment_Singleline) /= 0 then
            Data.Replace_Element (Data.Last_Index, Head (Data.Last_Element, Index (Data.Last_Element, Comment_Singleline) - 1));
          end if;
          if Comment_Multiline /= (others => NULL_STRING_16) then
            if In_Multiline_Comment and then Index (Data.Last_Element, Comment_Multiline'Last) + Length (Comment_Multiline'Last) /= Data.Last_Index then
              In_Multiline_Comment := False;
              Data.Replace_Element (Data.Last_Index, Head (Data.Last_Element, Index (Data.Last_Element, Comment_Multiline'Last)));
            elsif Index (Data.Last_Element, Comment_Multiline'First) /= 0 then
              In_Multiline_Comment := True;
              Data.Replace_Element (Data.Last_Index, Tail (Data.Last_Element, Index (Data.Last_Element, Comment_Multiline'Last)));
            else Data.Replace_Element (Data.Last_Index, NULL_STRING_16_UNBOUNDED); end if;
          end if;
          if Tab_Replacement /= NULL_STRING_16 then
            while Index (Data.Last_Element, TAB_16) /= 0 loop Data.Replace_Element (Data.Last_Index, Overwrite (Data.Last_Element, Index (Data.Last_Element, TAB_16), " ")); end loop;
          end if;
        end loop;
        Close (Raw_Data);
        Seek;
      end;
    end;
end;