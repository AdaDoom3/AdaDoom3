
--                                                                                                                                      --
--                                                         N E O  E N G I N E                                                           --
--                                                                                                                                      --
--                                                 Copyright (C) 2016 Justin Squirek                                                    --
--                                                                                                                                      --
-- Neo is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the --
-- Free Software Foundation, either version 3 of the License, or (at your option) any later version.                                    --
--                                                                                                                                      --
-- Neo is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of                --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.                            --
--                                                                                                                                      --
-- You should have received a copy of the GNU General Public License along with Neo. If not, see gnu.org/licenses                       --
--                                                                                                                                      --

package body Neo is

  ----------------
  -- Containers --
  ----------------

  package body Trees is
      protected body Safe_Tree is
          procedure Clear                                                                                         is begin null; end;
          procedure Replace         (Pos    : Cursor; Item : Tree_T)                                              is begin null; end;
          procedure Query           (Pos    : Cursor; Process : not null access procedure (Item : Tree_T))        is begin null; end;
          procedure Update          (Pos    : Cursor; Process : not null access procedure (Item : in out Tree_T)) is begin null; end;
          procedure Iterate_Subtree (Pos    : Cursor; Process : not null access procedure (Pos : Cursor))         is begin null; end;
          procedure Iterate                          (Process : not null access procedure (Pos : Cursor))         is begin null; end;
          procedure Iterate         (Parent : Cursor; Process : not null access procedure (Pos : Cursor))         is begin null; end;
          procedure Iterate_Back    (Parent : Cursor; Process : not null access procedure (Pos : Cursor))         is begin null; end;
          procedure Set             (Source : Unsafe.Tree)                                                        is begin null; end;
          procedure Move            (Source : in out Unsafe.Tree)                                                 is begin null; end;
          procedure Delete_Leaf     (Pos    : in out Cursor)                                                      is begin null; end;
          procedure Delete_Subtree  (Pos    : in out Cursor)                                                      is begin null; end;
          procedure Swap            (I, J   : Cursor)                                                             is begin null; end;
          procedure Delete          (Parent         : Cursor)                                                     is begin null; end;
          procedure Prepend         (Parent         : Cursor; Item : Tree_T; Count : Int_32_Positive := 1)        is begin null; end;
          procedure Append          (Parent         : Cursor; Item : Tree_T; Count : Int_32_Positive := 1)        is begin null; end;
          procedure Insert          (Parent, Before : Cursor; Item : Tree_T; Count : Int_32_Positive := 1)        is begin null; end;
          procedure Splice_Subtree  (Parent, Before, Pos           : Cursor)                                      is begin null; end;
          procedure Splice          (Parent, Before, Source_Parent : Cursor)                                      is begin null; end;
          procedure Copy_Subtree    (Parent, Before, Source        : Cursor)                                      is begin null; end;
          procedure Next            (Pos    : in out Cursor)                                                      is begin null; end;
          procedure Previous        (Pos    : in out Cursor)                                                      is begin null; end;
          function Subtree_Nodes    (Pos    : Cursor)                return Int_32_Positive                       is (1);
          function Depth            (Pos    : Cursor)                return Int_32_Positive                       is (1);
          function "="              (L, R   : Cursor)                return Bool                                  is (True);
          function Is_Root          (Pos    : Cursor)                return Bool                                  is (True);
          function Is_Leaf          (Pos    : Cursor)                return Bool                                  is (True);
          function Has              (Pos    : Cursor)                return Bool                                  is (True);
          function Has              (Item   : Tree_T)                return Bool                                  is (True);
          function Equals           (Item   : Unsafe.Tree)           return Bool                                  is (True);
          function Find             (Item   : Tree_T)                return Cursor                                is (NO_ELEMENT);
          function Get              (Pos    : Cursor)                return Tree_T                                is (Unsafe.Element (Pos));
          function Find_In_Subtree  (Pos    : Cursor; Item : Tree_T) return Cursor                                is (NO_ELEMENT);
          function Ancestor_Find    (Pos    : Cursor; Item : Tree_T) return Cursor                                is (NO_ELEMENT);
          function Child_Count      (Parent        : Cursor)         return Int_32_Positive                       is (1);
          function Child_Depth      (Parent, Child : Cursor)         return Int_32_Positive                       is (1);
          function Parent           (Pos    : Cursor)                return Cursor                                is (NO_ELEMENT);
          function First            (Parent : Cursor)                return Cursor                                is (NO_ELEMENT);
          function First            (Parent : Cursor)                return Tree_T                                is (Unsafe.Element (Parent));
          function Last             (Parent : Cursor)                return Tree_T                                is (Unsafe.Element (Parent));
          function Last             (Parent : Cursor)                return Cursor                                is (NO_ELEMENT);
          function Is_Empty                                          return Bool                                  is (True);
          function Node_Count                                        return Int_32_Positive                       is (1);
          function Root                                              return Cursor                                is (NO_ELEMENT);
          function Get                                               return Unsafe.Tree                           is (This);
        end;
    end;
  package body Vectors is
      protected body Safe_Vector is
          procedure Clear                                                                          is begin Unsafe.Clear           (This);                            end;
          procedure Set     (Val : Unsafe.Vector)                                                  is begin This := Val;                                              end;
          procedure Next    (Pos : in out Cursor)                                                  is begin Unsafe.Next            (Pos);                             end;
          procedure Replace (Pos :        Cursor;      Item : Vec_T)                               is begin Unsafe.Replace_Element (This, Pos, Item);                 end;
          procedure Append                            (Item : Vec_T; Count : Int_32_Positive := 1) is begin Unsafe.Append          (This, Item,  Count_Type (Count)); end;
          procedure Prepend                           (Item : Vec_T; Count : Int_32_Positive := 1) is begin Unsafe.Prepend         (This, Item,  Count_Type (Count)); end;
          procedure Insert  (Before : Int_32_Positive; Item : Vec_T; Count : Int_32_Positive := 1) is begin Unsafe.Insert          (This, Before, Item);              end;
          procedure Delete  (Index  : Int_32_Positive;               Count : Int_32_Positive := 1) is begin Unsafe.Delete          (This, Index, Count_Type (Count)); end;
          function Has      (Pos    : Cursor)          return Bool                                 is (Unsafe.Has_Element          (Pos));
          function Get      (Pos    : Cursor)          return Vec_T                                is (Unsafe.Element              (Pos));
          function Get      (Index  : Int_32_Positive) return Vec_T                                is (Unsafe.Element              (This, Index));
          function Get                                 return Unsafe.Vector                        is (This);
          function First                               return Cursor                               is (Unsafe.First                (This));
          function Length                              return Int_32_Positive                      is (Int_32_Positive             (Unsafe.Length (This)));
        end;
    end;
  package body Hashed is
      protected body Safe_Map is
          procedure Clear                                    is begin Unsafe.Clear           (This);                                end;
          procedure Set     (Val : Unsafe.Map)               is begin This := Val;                                                  end;
          procedure Next    (Pos : in out Cursor)            is begin Unsafe.Next            (Pos);                                 end;
          procedure Delete  (Pos : in out Cursor)            is begin Unsafe.Delete          (This, Pos);                           end;
          procedure Delete  (Key : Str)                      is begin Unsafe.Delete          (This, To_Str_16_Unbound (Key));       end;
          procedure Replace (Pos : Cursor; Item : Map_T)     is begin Unsafe.Replace_Element (This, Pos, Item);                     end;
          procedure Replace (Key : Str; Item : Map_T)        is begin Unsafe.Replace         (This, To_Str_16_Unbound (Key), Item); end;
          procedure Insert  (Key : Str; Item : Map_T)        is begin Unsafe.Insert          (This, To_Str_16_Unbound (Key), Item); end;
          function Has      (Key : Str) return Bool          is (Unsafe.Contains             (This, To_Str_16_Unbound (Key)));
          function Has      (Pos : Cursor) return Bool       is (Unsafe.Has_Element          (Pos));
          function Key      (Pos : Cursor) return Str        is (To_Str_16 (Unsafe.Key       (Pos)));
          function Get      (Key : Str) return Map_T         is (Unsafe.Element              (This, To_Str_16_Unbound (Key)));
          function Get      (Pos : Cursor) return Map_T      is (Unsafe.Element              (Pos));
          function Get                     return Unsafe.Map is (This);
          function First                   return Cursor     is (Unsafe.First                (This));
        end;
    end;
  package body Ordered is
      protected body Safe_Map is
          procedure Clear                                    is begin Unsafe.Clear           (This);            end;         
          procedure Set     (Val : Unsafe.Map)               is begin This := Val;                              end;
          procedure Next    (Pos : in out Cursor)            is begin Unsafe.Next            (Pos);             end;
          procedure Delete  (Pos : in out Cursor)            is begin Unsafe.Delete          (This, Pos);       end;
          procedure Delete  (Key : Key_T)                    is begin Unsafe.Delete          (This, Key);       end;
          procedure Replace (Pos : Cursor; Item : Map_T)     is begin Unsafe.Replace_Element (This, Pos, Item); end;
          procedure Replace (Key : Key_T;  Item : Map_T)     is begin Unsafe.Replace         (This, Key, Item); end;
          procedure Insert  (Key : Key_T;  Item : Map_T)     is begin Unsafe.Insert          (This, Key, Item); end;
          function Has      (Key : Key_T)  return Bool       is (Unsafe.Contains             (This, Key));
          function Has      (Pos : Cursor) return Bool       is (Unsafe.Has_Element          (Pos));
          function Key      (Pos : Cursor) return Key_T      is (Unsafe.Key                  (Pos));
          function Get      (Pos : Cursor) return Map_T      is (Unsafe.Element              (Pos));
          function Get      (Key : Key_T)  return Map_T      is (Unsafe.Element              (This, Key));
          function Get                     return Unsafe.Map is (This);
          function First                   return Cursor     is (Unsafe.First                (This));
        end;
    end;

  ------------
  -- Timing --
  ------------

  Timer_Error : Exception;
  START_TIME  : Time := Clock;
  function Get_Start_Time                     return Time     is (START_TIME);
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

  protected body Safe_Status is
      function Occupied  return Bool    is (Status);
      procedure Occupied (Value : Bool) is begin Status := Value; end;
    end;

  ---------------
  -- Debugging --
  ---------------

  procedure Assert (Val : Int_16_Unsigned_C) is begin Assert (Val /= 0);        end;
  procedure Assert (Val : Int_32_Unsigned_C) is begin Assert (Val /= 0);        end;
  procedure Assert (Val : Int_32_C)          is begin Assert (Val /= 0);        end;
  procedure Assert (Val : Ptr)               is begin Assert (Val /= NULL_PTR); end;
  procedure Assert (Val : Bool)              is begin if not Val then raise Program_Error; end if; end;

  -- Null procedures - should be replaced when GNAT 17 is released
  procedure Ignore (Val : Bool)              is begin null; end;
  procedure Ignore (Val : Ptr)               is begin null; end;
  procedure Ignore (Val : Int_Ptr)           is begin null; end;
  procedure Ignore (Val : Int_C)             is begin null; end;
  procedure Ignore (Val : Int_16_Unsigned_C) is begin null; end;
  procedure Ignore (Val : Int_32_Unsigned_C) is begin null; end;
  
  -----------------
  -- Conversions --
  -----------------

  -- Character conversions
  function To_Char_8 (Item : Char_16) return Char_8 is
    (if Char_16'Pos (Item) > Char_8'Pos (Char_8'Last) then CHAR_16_REPLACEMENT else Char_8'Val (Char_16'Pos (Item)));

  -- String conversions
  function To_Str_8 (Item : Str) return Str_8 is
    Result : Str_8 (Item'First..Item'Length);
    begin
      for I in Item'Range loop Result (I) := To_Char_8 (Item (I)); end loop;
      return Result;
    end;
  function To_Str_16_C (Item : Str_8_C) return Str_16_C is
    Result : Str_16_C (Item'First..Item'Length);
    begin
      for I in Item'Range loop Result (I) := Char_16_C'Val (Char_8_C'Pos (Item (I))); end loop;
      return Result;
    end;
  function To_Str_16 (Item : Str_8) return Str is
    Result : Str (Item'First..Item'Length);
    begin
      for I in Item'Range loop Result (I) := Char_16'Val (Char_8'Pos (Item (I))); end loop;
      return Result;
    end;
  function To_Str_16 (Item : Str_16_C) return Str is
    Last   : Int_Size_C := Item'First;
    Buffer : Str_16_Unbound;
    begin
      for I in Item'Range loop
        exit when Item (I) = NULL_CHAR_16_C;
        Buffer := Buffer & Char_16'Val (Char_16_C'Pos (Item (Int_Size_C (I))));
      end loop;
      return To_Str_16 (Buffer);
    end;
  function To_Str_16 (Item : Ptr_Const_Char_16_C) return Str is
    Length : Int_32 := 0;
    Buffer : Str_16_Unbound;
    Temp   : Ptr_Const_Char_16_C := Item;
    begin
      while Temp.All /= NULL_CHAR_16_C loop
        Length := Length + 1;
        Buffer := Buffer & Char_16 (Temp.All);
        Temp   := To_Ptr_Const_Char_16_C (To_Ptr (To_Int_Ptr (Temp) + Char_16_C'Size / Int_8_Unsigned'Size));
      end loop;
      return To_Str_16 (Buffer);
    end;          
  function To_Str_8 (Item : Ptr_Const_Char_8_C) return Str_8 is 
    Length : Int_32 := 0;
    Buffer : Str_8_Unbound;
    Temp   : Ptr_Const_Char_8_C := Item;
    begin
      while Temp.All /= NULL_CHAR_8_C loop
        Length := Length + 1;
        Buffer := Buffer & Char_8 (Temp.All);
        Temp   := To_Ptr_Const_Char_8_C (To_Ptr (To_Int_Ptr (Temp) + Char_8_C'Size / Int_8_Unsigned'Size));
      end loop;
      return To_Str_8 (Buffer);
    end;

  -- Integer to string with a changable base (e.g. decimal to binary or hex)
  function Generic_To_Str_16 (Item : Num_T; Base : Int_32_Positive; Do_Pad_Zeros : Bool := True) return Str is
    package Num_T_Text_IO is new Ada.Wide_Text_IO.Modular_IO (Num_T);
    Buffer : Str_16_Unbound;
    Input  : Str (1..4096);
    begin
      Num_T_Text_IO.Put (Input, Item, Ada.Wide_Text_IO.Number_Base (Base));
      if Base = 10 then return Trim (Input, Both); end if;
      Buffer := To_Str_16_Unbound (Trim (Input, Both));
      Delete (Buffer, 1, Trim (Base'Img, Both)'Length + 1);
      Delete (Buffer, Length (Buffer), Length (Buffer));
      if Item /= Num_T'Last and Do_Pad_Zeros then
        for I in 1..Generic_To_Str_16 (Num_T'Last, Base)'Length - Length (Buffer) loop Insert (Buffer, 1, "0"); end loop;
      end if;
      return To_Str_16 (Buffer);
    end;

  -- Split a string into an array of strings based on a separator (e.g. comma)
  function Split (Item : Str; On : Str := " ") return Array_Str_16_Unbound is
    package Vector_Str_16_Unbound is new Ada.Containers.Indefinite_Vectors (Int_32_Positive, Str_16_Unbound);
    function Internal (Item : Str; On : Str := " ") return Vector_Str_16_Unbound.Vector is
      Result    : Vector_Str_16_Unbound.Vector;
      TRIMMED   : constant Str  := Trim (Item, Both);
      REMAINDER : constant Natural := Index (TRIMMED, On);
      begin
        if REMAINDER = 0 then
          Result.Append (To_Str_16_Unbound (TRIMMED));
          return Result;
        else Result.Append (To_Str_16_Unbound (Trim (TRIMMED (TRIMMED'First..REMAINDER - 1), Both))); end if;
        Result.Append (Internal (TRIMMED (REMAINDER..TRIMMED'Last), On));
        return Result;
      end;
    Internal_Result : Vector_Str_16_Unbound.Vector;
    begin
      Internal_Result := Internal (Item, On);
      declare
      Result : Array_Str_16_Unbound (1..Int_32 (Internal_Result.Length));
      begin
        for I in Result'Range loop Result (I) := Internal_Result.Element (I); end loop;
        return Result;
      end;
    end;

  -------------
  -- Parsing -- This needs comments...
  -------------

  procedure Save (Path : Str; Item : Stream_Element_Array) is
    begin
      null;
    end;
  function Load (Path : Str) return Stream_Element_Array is
    Junk : Stream_Element;
    begin
      return (Junk, Junk);
    end;
  package body Handler is
      Unsupported, Duplicate_Format : Exception;
      type Format_State is record
          Save       : access procedure (Path : Str; Item : T);
          Load       : access function  (Path : Str) return T;
          Extensions : Str_16_Unbound;
        end record;
      package Ordered_Format is new Ordered (Format_T, Format_State);
      Formats : Ordered_Format.Safe_Map;
      function Match_Extension (Val : Str) return Format_T is
        Bad_Extension : Exception;
        Val_Extension : Str_16_Unbound := NULL_STR_16_UNBOUND;
        begin
          for I in reverse Val'Range loop
            if Val (I) = '.' and I /= Val'length then 
              Val_Extension := To_Str_16_Unbound (To_Lower (Val (I + 1..Val'Last)));
              exit;
            elsif I = Val'First then raise Bad_Extension; end if;
          end loop;
          for Format of Formats.Get loop
            for Extension of Split (To_Str_16 (Format.Extensions), ",") loop
              if Extension = Val_Extension then return Format_T'Wide_Value (To_Str_16 (Extension)); end if;
            end loop;
          end loop;
          raise Unsupported;
        end;
      procedure Save (Path : Str; Item : T) is begin        Formats.Get (Match_Extension (Path)).Save (Path, Item); end;
      function Load  (Path : Str) return T  is begin return Formats.Get (Match_Extension (Path)).Load (Path);       end;
      package body Format is
          type Controller_State is new Controlled with null record;
          procedure Finalize   (Item : in out Controller_State);
          procedure Initialize (Item : in out Controller_State);
          procedure Finalize   (Item : in out Controller_State) is begin Formats.Delete (Kind); end;
          procedure Initialize (Item : in out Controller_State) is
            begin
              if Formats.Has (Kind) then raise Duplicate_Format; end if;
              Formats.Insert (Kind, (Save, Load, To_Str_16_Unbound (To_Lower (Extensions))));
            end;
        end;
    end;
  package body Parser is
      package Vector_Str_16_Unbound is new Vectors (Str_16_Unbound);
      This    : Vector_Str_16_Unbound.Unsafe.Vector;
      Row     : Int_32_Positive := 1;
      Column  : Int_32_Natural  := 0;
      Invalid : Exception;
      procedure Seek is
        begin
          Column := Column + 1;
          while not At_EOF loop
            loop
              if Column > Length (This.Element (Row)) or else This.Element (Row) = NULL_STR_16_UNBOUND then
                Column := 1;
                Row    := Row + 1;
                exit;
              end if;
              if Index (Slice (This.Element (Row), Column, Length (This.Element (Row))), "" & Separator) /= Column then return; end if;
              Column := Column + 1;
            end loop;
          end loop;
        end;
      function Next_Set  (Ending : Str) return Str_16_Unbound is      (Next_Set ("" & Element (This.Element (Row), Column), Ending));
      procedure Skip_Set (Ending : Str)                       is begin Skip_Set ("" & Element (This.Element (Row), Column), Ending); end;
      procedure Skip_Line                                     is begin Row := Row + 1; Column := 1;                                  end;
      function Next_Line return Str_16_Unbound is
        Result : Str_16_Unbound := To_Str_16_Unbound (Slice (This.Element (Row), Column, Length (This.Element (Row))));
        begin
          Parser.Skip_Line;
          return Result;
        end;
      function At_EOL return Bool is
        Previous_Column : Int_32_Positive := Column;
        Previous_Row    : Int_32_Positive := Row;
        Result          : Str_16_Unbound  := Next;
        begin
          Column          := Previous_Column;
          Previous_Column := Row;
          Row             := Previous_Row;
          return Previous_Column /= Row;
        end;
      function At_EOF return Bool is ((if Row > This.Last_Index then True else False));
      procedure Skip_Set (Starting, Ending : Str) is Junk : Str_16_Unbound := Next_Set (Starting, Ending); begin null; end;
      procedure Skip (Amount : Int_32_Positive := 1) is
        Junk : Str_16_Unbound;
        begin
          for I in 1..Amount loop Junk := Next; end loop;
        end;
      procedure Assert (Text : Str) is
        begin
          if Index (Slice (This.Element (Row), Column, Length (This.Element (Row))), Text) /= Column then raise Invalid; end if;
          Column := Column + Text'length - 1;
          Seek;
        end;
      function Peek return Str_16_Unbound is
        Previous_Column : Int_32_Positive := Column;
        Previous_Row    : Int_32_Positive := Row;
        Result          : Str_16_Unbound  := Next;
        begin
          Row    := Previous_Row;
          Column := Previous_Column;
          return Result;
        end;
      function Next return Str_16_Unbound is
        Result : Str_16_Unbound := To_Str_16_Unbound (Slice (This.Element (Row), Column, Length (This.Element (Row))));
        I      : Int_32_Natural := Index (Result, "" & Separator);
        begin
          if I /= 0 then Delete (Result, I, Length (Result)); end if;
          Column := Column + Length (Result);
          Seek;
          return Result;
        end;
      function Next return Real_64 is
        Found_Decimal : Bool := False;
        Found_Digit   : Bool := False;
        Found_Sign    : Bool := False;
        Result        : Str_16_Unbound;
        begin
          while Column <= Length (This.Element (Row)) loop
            if not Is_Digit (Element (This.Element (Row), Column)) then
              case Element (This.Element (Row), Column) is
                when '-' | '+' => exit when Found_Digit or Found_Sign;
                                  Found_Sign := True;                    
                when '.' =>       exit when Found_Decimal;
                                  Found_Digit   := True;
                                  Found_Decimal := True;
                when others =>    exit;
              end case;
            elsif not Found_Digit then Found_Digit := True; end if;
            Result := Result & Element (This.Element (Row), Column);
            Column := Column + 1;
          end loop;
          if Result = NULL_STR_16_UNBOUND then raise Invalid; end if;
          Seek;
          return Real_64'Wide_Value (To_Str_16 (Result));
        end;
      function Next_Set (Starting, Ending : Str) return Str_16_Unbound is
        Result : Str_16_Unbound;
        Buffer : Str_16_Unbound;
        I      : Int_32_Natural;
        begin
          Assert (Starting);
          while not At_EOF loop
            Buffer := To_Str_16_Unbound (Slice (This.Element (Row), Column, Length (This.Element (Row))));
            I      := Index (Buffer, Ending);
            if I = 0 then
              Column := 1;
              Row    := Row + 1;
              Result := Result & Buffer & EOL_16;
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
      Data                  : File_Type;
      In_Multiline_Comment  : Bool := False;
      Trimmed_Comment_Start : Str  := Trim (Comment_Start, Both);
      Trimmed_Comment_End   : Str  := Trim (Comment_End,   Both);
      Trimmed_Comment       : Str  := Trim (Comment,       Both);
      begin
        Assert ((if Trimmed_Comment_Start = NULL_STR_16 then Trimmed_Comment_End = NULL_STR_16
                 else Trimmed_Comment_End /= NULL_STR_16));
        Open (Data, In_File, To_Str_8 (Path));
        while not End_Of_File (Data) loop
          This.Append (To_Str_16_Unbound (Get_Line (Data)));
          if Trimmed_Comment /= NULL_STR_16 and then Index (This.Last_Element, Trimmed_Comment) /= 0 then
            This.Replace_Element (This.Last_Index,
                                  Head (This.Last_Element,
                                  Index (This.Last_Element, Trimmed_Comment) - 1));
          end if;
          if Trimmed_Comment_Start /= NULL_STR_16 then
            if In_Multiline_Comment and then
              Index (This.Last_Element, Trimmed_Comment_End) + Trimmed_Comment_End'Length /= This.Last_Index
            then
              In_Multiline_Comment := False;
              This.Replace_Element (This.Last_Index,
                                    Head (This.Last_Element,
                                    Index (This.Last_Element, Trimmed_Comment_End)));
            elsif Index (This.Last_Element, Trimmed_Comment_Start) /= 0 then
              In_Multiline_Comment := True;
              This.Replace_Element (This.Last_Index,
                                    Tail (This.Last_Element,
                                    Index (This.Last_Element, Trimmed_Comment_End)));
            else This.Replace_Element (This.Last_Index, NULL_STR_16_UNBOUND); end if;
          end if;
          if Tab_Replacement /= NULL_CHAR_16 then
            while Index (This.Last_Element, TAB_16) /= 0 loop
              This.Replace_Element (This.Last_Index,
                                    Overwrite (This.Last_Element,
                                    Index (This.Last_Element, TAB_16), " "));
            end loop;
          end if;
        end loop;
        Close (Data);
        Seek;
      end;
    end;

  --------
  -- IO --
  --------

  -- Hidden protected structure of task-safe output
  protected type Safe_IO is
      function Log          return Str_16_Unbound;
      function Log          return Str;
      function Input_Entry  return Str;
      function Line_Size    return Int_32_Positive;
      function Lines        return Int_64_Natural;
      procedure Input_Entry (Val  : Str);
      procedure Line_Size   (Val  : Int_32_Positive);
      procedure Put         (Item : Str);
    private
      Current_Put         : Ptr_Procedure_Put;-- := Ada.Wide_Text_IO.Put'Access;
      Current_Log         : Str_16_Unbound;
      Current_Input_Entry : Str_16_Unbound;
      Current_Lines       : Int_64_Natural;
      Current_Tasks       : Int_32_Positive;
      Current_Line_Size   : Int_32_Positive := 80;
    end;
  protected body Safe_IO is
      function Log          return Str_16_Unbound   is (Current_Log);
      function Log          return Str              is (To_Str_16 (Current_Log));
      function Input_Entry  return Str              is (To_Str_16 (Current_Input_Entry));
      function Line_Size    return Int_32_Positive  is (Current_Line_Size);
      function Lines        return Int_64_Natural   is (Current_Lines);
      procedure Line_Size   (Val : Int_32_Positive) is begin Current_Line_Size   := Val;                     end;
      procedure Input_Entry (Val : Str)             is begin Current_Input_Entry := To_Str_16_Unbound (Val); end;
      procedure Put (Item : Str) is
        Count : Int_64_Natural := 0;
        begin
          Current_Log := Current_Log & To_Str_16_Unbound (Item);
          if Current_Put /= null then Current_Put.All (Item); end if;
          for Get of Item loop
            if Get = Char_16'Val (Char_8'Pos (ASCII.CR)) then Count := Count + 1; end if;
          end loop;
          Current_Lines := Current_Lines + Count;
        end;
    end;
  IO : Safe_IO;

  -- Public subprograms that wrap the protected type
  procedure Line        (Count : Int_32_Positive := 1)       is begin for I in 1..Count loop Put (EOL_16); end loop; end;
  procedure Input_Entry (Val   : Str)                        is begin IO.Input_Entry (Val);                          end;
  procedure Line_Size   (Val   : Int_32_Positive)            is begin IO.Line_Size   (Val);                          end;
  procedure Put         (Val   : Ptr_Procedure_Put)          is begin null;                                          end;
  procedure Put         (Item  : Str_16_Unbound)             is begin Put            (To_Str_16 (Item));             end;
  procedure Put         (Item  : Char_16)                    is begin Put            (Item & "");                    end;                  
  procedure Put         (Item  : Str)                        is begin IO.Put         (Item);                         end;
  procedure Line        (Item  : Char_16)                    is begin Line           (Item & "");                    end;    
  procedure Line        (Item  : Str_16_Unbound)             is begin Line           (To_Str_16 (Item));             end;
  procedure Line        (Item  : Str)                        is begin Put            (Item); Line;                   end;
  function Extension    (Path  : Str) return Str             is (Path (Index (Path, ".") + 1..Path'Last));
  function Log                        return Str             is (IO.Log);
  function Input_Entry                return Str             is (IO.Input_Entry); 
  function Lines                      return Int_64_Natural  is (IO.Lines);
  function Line_Size                  return Int_32_Positive is (IO.Line_Size);

  -- Localization loading and translation
  function Initialize_Localization return Hashed_Localization.Unsafe.Map is
    Result : Hashed_Localization.Unsafe.Map;
    begin
      return Result;
    end;
  FAILED_LOCALIZE_PREVIEW_SIZE : constant Int_32_Positive := 10;
  LOCALIZATION : constant Hashed_Localization.Unsafe.Map := Initialize_Localization;
  function Localize (Item : Str) return Str is
    Result : Str := Item;
    begin
      if LOCALIZATION.Has_Element (Item) then = NULL_Str_16 then
        return Item;
      end if;
      return Result;
    end;

  -------------
  -- Command --
  -------------

  -- Comment prefix for configuration files
  SINGLE_LINE_COMMENT : constant Str := "--"; 

  -- Maximum cvar values displayable after query
  MAX_VALUES_DISPLAYABLE : constant Int_32_Positive := 5; 

  -- Internal data structure used to register command instantiations globally for commandline
  type Command_Callback is access procedure (Args : Array_Str_16_Unbound);

  -- Internal data structure used to register cvar instantiations globally for commandline  
  type CVar_State is record 
      Val : Str_16_Unbound; -- A cvar that goes out of scope is not forgotten
      Get : access function  return Str;
      Set : access procedure (Val : Str);
    end record;

  -- Global maps
  package Hashed_CVar    is new Hashed (CVar_State);
  package Hashed_Command is new Hashed (Command_Callback);
  Commands : Hashed_Command.Safe_Map;
  CVars    : Hashed_CVar.Safe_Map;

  -- CVar defintion
  package body CVar is
      Duplicate, Parse : Exception;

      -- Forward declarations to add finalization via a "controller"
      type Control_State is new Controlled with null record;
      procedure Initialize (Control : in out Control_State);
      procedure Finalize   (Control : in out Control_State);

      -- Internal protected type to maintain task safety
      protected type Safe_Var_T is
          procedure Set (Val : Var_T);
          function Get  return Var_T;
        private
          Current : Var_T;
        end;
      protected body Safe_Var_T is
          function Get return Var_T is (Current);
          procedure Set (Val : Var_T) is begin Current := Val; end;
        end;
      This : Safe_Var_T;

      -- Get
      function Get return Var_T is (This.Get);
      function Handle_Get return Str is
        Vals : Str_16_Unbound := To_Str_16_Unbound (Trim (Var_T'First'Img, Both));
        begin
          if Var_T'Pos (Var_T'Last) - Var_T'Pos (Var_T'First) > MAX_VALUES_DISPLAYABLE then
            Vals := Vals & ".." & To_Str_16 (Trim (Var_T'Last'Img, Both));
          else
            for I in Var_T'Val (Var_T'Pos (Var_T'first) + 1)..Var_T'Last loop
              Vals := Vals & ", " & To_Str_16_Unbound (Trim (I'Img, Both));
            end loop;
          end if;
          return Help & EOL_16 &
                 "Current value: " & To_Str_16 (Trim (This.Get'Img, Both)) & EOL_16 &
                 "Possible values: " & To_Str_16 (Vals);
        end;

      -- Set
      procedure Set (Val : Var_T) is begin This.Set (Val); end;
      procedure Handle_Set (Val : Str) is
        begin
          if not Settable then
            Line (Name & " is not settable!");
            return;
          end if;
          Set (Var_T'Wide_Value (Val));
        exception when Constraint_Error =>
          for I in Var_T'Range loop
            if Val = To_Str_16 (I'Img) then
              Set (I);
              exit;
            elsif I = Var_T'Last then
              Line ("Incorrect parameter for cvar """ & Name & """: " & Val);
              Line (Handle_Get);
            end if;
          end loop;
        end;

      -- Initialization and finalization
      procedure Initialize (Control : in out Control_State) is
        begin
          if Commands.Has (Name) then
            if CVars.Get (Name).Set /= null or CVars.Get (Name).Get /= null then raise Duplicate; end if;
            CVars.Replace (Name, (Val => CVars.Get (Name).Val,
                                  Get => Handle_Get'Unrestricted_Access,
                                  Set => Handle_Set'Unrestricted_Access));
            Handle_Set (To_Str (CVars.Get (Name).Val));
          else
            CVars.Insert (Name, (Val => NULL_STR_16_UNBOUND,
                                 Get => Handle_Get'Unrestricted_Access,
                                 Set => Handle_Set'Unrestricted_Access));
            Set (Initial);
          end if;
        end;
      procedure Finalize (Control : in out Control_State) is
        begin
          if Settable then CVars.Replace (Name, (Val => To_Str_16_Unbound (Trim (This.Get'Img, Both)),
                                                 Get => null,
                                                 Set => null));
          else CVars.Delete (Name); end if;
        end;
      Control : Control_State;
    end;

  -- Command
  package body Command is
      Duplicate, Parse : Exception;

      -- Controller
      type Control_State is new Controlled with null record;
      procedure Finalize   (Control : in out Control_State);
      procedure Initialize (Control : in out Control_State);

      -- Initialization and finalization
      procedure Informal   (Args : Array_Str_16_Unbound) renames Callback;
      procedure Finalize   (Control : in out Control_State) is begin Commands.Delete (Name); end;
      procedure Initialize (Control : in out Control_State) is
        begin
          if Commands.Has (Name) then raise Duplicate; end if;
          Commands.Insert (Name, Informal'Unrestricted_Access);
        end;
      Control : Control_State;
    end;

  -- Parsing
  procedure Submit (Text : Str) is
    Tokens : Array_Str_16_Unbound := Split (Text);
    CMD    : constant Str := To_Str_16 (Tokens (1));
    begin
      if Commands.Has (CMD) then Commands.Get (CMD).All (Tokens (2..Tokens'Length));
      elsif CVars.Has (CMD) then
        if Tokens'Length = 1 then
          if CVars.Get (CMD).Get /= null then Line (CVars.Get (CMD).Get.All); end if;
        elsif CVars.Get (CMD).Set /= null then CVars.Get (CMD).Set.All (To_Str_16 (Tokens (2))); end if;
      else raise Constraint_Error; end if;
    exception when others => Line ("No such cvar or command!"); end;
  function Autocomplete (Text : Str) return Array_Str_16_Unbound is
    begin
      return (NULL_STR_16_UNBOUND, NULL_STR_16_UNBOUND);
      --for CVar in CVars loop
      --  if Key (CVar) ()
      --end loop;
    end;

  -- Configuration
  procedure Initialize_Configuration is
    begin
      null;
    end;
  procedure Finalize_Configuration is
    begin
      null;--for Command of Commands.Get loop Put_Line (Command.Save.All); end loop;
    end;
end;