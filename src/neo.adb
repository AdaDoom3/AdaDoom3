
--                                                                                                                    
--                                                              N E O  E N G I N E                                                    
--                                                                                                                    
--                                                      Copyright (C) 2016 Justin Squirek                                          
-- 
-- Neo is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any later version. 
--                                                                                                                    
-- Neo is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.         
--                                                                                                                    
-- You should have received a copy of the GNU General Public License along with Neo. If not, see gnu.org/licenses     
--

package body Neo is

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

  protected body Safe_Status is
      function Occupied  return Bool    is (Status);
      procedure Occupied (Value : Bool) is begin Status := Value; end;
    end;

  ----------------
  -- Containers --
  ----------------

  package body Trees is
      protected body Tree is
          procedure Clear                                                                                            is begin  end;
          procedure Replace         (Pos    : Cursor; New_Item : Tree_T)                                             is begin  end;
          procedure Query           (Pos    : Cursor; Process : not null access procedure (Element : Tree_T))        is begin  end;
          procedure Update          (Pos    : Cursor; Process : not null access procedure (Element : in out Tree_T)) is begin  end;
          procedure Iterate_Subtree (Pos    : Cursor; Process : not null access procedure (Pos : Cursor))            is begin  end;
          procedure Iterate                          (Process : not null access procedure (Pos : Cursor))            is begin  end;
          procedure Iterate         (Parent : Cursor; Process : not null access procedure (Pos : Cursor))            is begin  end;
          procedure Iterate_Back    (Parent : Cursor; Process : not null access procedure (Pos : Cursor))            is begin  end;
          procedure Set             (Source : Unsafe.Tree)                                                           is begin  end;
          procedure Move            (Source : in out Unsafe.Tree)                                                    is begin  end;
          procedure Delete_Leaf     (Pos    : in out Cursor)                                                         is begin  end;
          procedure Delete_Subtree  (Pos    : in out Cursor)                                                         is begin  end;
          procedure Swap            (I, J   : Cursor)                                                                is begin  end;
          procedure Delete          (Parent         : Cursor)                                                        is begin  end;
          procedure Prepend         (Parent         : Cursor; New_Item : Tree_T; Count : Int_32_Positive := 1)       is begin  end;
          procedure Append          (Parent         : Cursor; New_Item : Tree_T; Count : Int_32_Positive := 1)       is begin  end;
          procedure Insert          (Parent, Before : Cursor; New_Item : Tree_T; Count : Int_32_Positive := 1)       is begin  end;
          procedure Splice_Subtree  (Parent, Before, Pos           : Cursor)                                         is begin  end;
          procedure Splice          (Parent, Before, Source_Parent : Cursor)                                         is begin  end;
          procedure Copy_Subtree    (Parent, Before, Source        : Cursor)                                         is begin  end;
          procedure Next            (Pos    : in out Cursor)                                                         is begin  end;
          procedure Previous        (Pos    : in out Cursor)                                                         is begin  end;
          function Subtree_Nodes    (Pos    : Cursor)                return Int_32_Positive                          is ();
          function Depth            (Pos    : Cursor)                return Int_32_Positive                          is ();
          function "="              (L, R   : Cursor)                return Bool                                     is ();
          function Is_Root          (Pos    : Cursor)                return Bool                                     is ();
          function Is_Leaf          (Pos    : Cursor)                return Bool                                     is ();
          function Has              (Pos    : Cursor)                return Bool                                     is ();
          function Has              (Item   : Tree_T)                return Bool                                     is ();
          function Equals           (Item   : Unsafe.Tree)           return Bool                                     is ();
          function Find             (Item   : Tree_T)                return Cursor                                   is ();
          function Element          (Pos    : Cursor)                return Tree_T                                   is ();
          function Find_In_Subtree  (Pos    : Cursor; Item : Tree_T) return Cursor                                   is ();
          function Ancestor_Find    (Pos    : Cursor; Item : Tree_T) return Cursor                                   is ();
          function Child_Count      (Parent        : Cursor)         return Int_32_Positive                          is ();
          function Child_Depth      (Parent, Child : Cursor)         return Int_32_Positive                          is ();
          function Parent           (Pos    : Cursor)                return Cursor                                   is ();
          function First            (Parent : Cursor)                return Cursor                                   is ();
          function First            (Parent : Cursor)                return Tree_T                                   is ();
          function Last             (Parent : Cursor)                return Tree_T                                   is ();
          function Last             (Parent : Cursor)                return Cursor                                   is ();
          function Is_Empty                                          return Bool                                     is ();
          function Node_Count                                        return Int_32_Positive                          is ();
          function Root                                              return Cursor                                   is ();
          function Get                                               return Unsafe.Tree                              is ();
        private
          This : Unsafe.Tree;
        end;
    end;
  package body Vectors is
      protected body Vector is
          procedure Clear                                                                          is begin Unsafe.Clear           (This); end;
          procedure Set     (Val : Unsafe.Vector)                                                  is begin This := Val;           end;
          procedure Next    (Pos : in out Cursor)                                                  is begin Unsafe.Next            (Pos) end;
          procedure Replace (Pos :        Cursor;      Item : Vec_T)                               is begin Unsafe.Replace_Element (This, Pos, Item); end;
          procedure Append                            (Item : Vec_T; Count : Int_32_Positive := 1) is begin Unsafe.Append          (This, Item, Unsafe.Count_Type (Count)) end;
          procedure Prepend                           (Item : Vec_T; Count : Int_32_Positive := 1) is begin Unsafe.Prepend         (This, Item, Unsafe.Count_Type (Count)) end;
          procedure Insert  (Before : Int_32_Positive; Item : Vec_T; Count : Int_32_Positive := 1) is begin Unsafe.Insert          (This, Before, Item) end;
          procedure Delete  (Index  : Int_32_Positive;               Count : Int_32_Positive := 1) is begin Unsafe.Delete          (This, Index, Count_Type (Count)) end;
          function Has      (Pos    : Cursor)          return Bool                                 is (Unsafe.Has_Element (Cursor));
          function Get      (Pos    : Cursor)          return Bool                                 is (Unsafe.Element ());
          function Get      (Pos    : Cursor)          return Vec_T                                is ();
          function Get      (Index  : Int_32_Positive) return Vec_T                                is ();
          function Get                                 return Unsafe.Vector                        is ();
          function First                               return Cursor                               is ();
          function Length                              return Int_32_Positive                      is ();
        private
          This : Unsafe.Vector;
        end;
    end;
  package body Hashed is
      protected body Map is
          procedure Clear                                    is begin Unsafe.Clear           (This);            end;
          procedure Set     (Val : Unsafe.Map)               is begin This := Val;                              end;
          procedure Next    (Pos : in out Cursor)            is begin Unsafe.Next            (Pos);             end;
          procedure Delete  (Pos : in out Cursor)            is begin Unsafe.Delete          (This, Pos);       end;
          procedure Delete  (Key : Str_16)                   is begin Unsafe.Delete          (This, Key);       end;
          procedure Replace (Pos : Cursor; Item : Map_T)     is begin Unsafe.Replace_Element (This, Pos, Item); end;
          procedure Replace (Key : Str_16; Item : Map_T)     is begin Unsafe.Replace         (This, Key, Item); end;
          procedure Insert  (Key : Str_16; Item : Map_T)     is begin Unsafe.Insert          (This, Key, Item); end;
          function Has      (Key : Str_16) return Bool       is (Unsafe.Contains             (This, Key));
          function Has      (Pos : Cursor) return Bool       is (Unsafe.Has_Element          (Pos));
          function Key      (Pos : Cursor) return Str_16     is (Unsafe.Key                  (Pos));
          function Get      (Key : Str_16) return Map_T      is (Unsafe.Element              (This, Key));
          function Get      (Pos : Cursor) return Map_T      is (Unsafe.Element              (Pos));
          function Get                     return Unsafe.Map is (This);
          function First                   return Cursor     is (Unsafe.First                (This));
        private
          This : Unsafe.Map;
        end;
    end;
  package body Ordered is
      protected body Map is
          procedure Clear                                    is begin Unsafe.Clear           (This);            end;         
          procedure Set     (Val : Unsafe.Map                is begin This := Val;                              end;
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
        private
          This : Unsafe.Map;
        end;
    end;

  ---------------
  -- Debugging --
  ---------------

  procedure Assert (Val : Int_32_C) is begin Assert (Val /= C_FALSE);      end;
  procedure Assert (Val : Address)  is begin Assert (Val /= NULL_ADDRESS); end;
  procedure Assert (Val : Bool)     is begin pragma Assert (Val);          end;
  
  -----------------
  -- Conversions --
  -----------------

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
    Buffer : Str_16_Unbound;
    begin
      for I in Item'Range loop
        exit when Item (I) = NULL_CHAR_16_C;
        Buffer := Buffer & Char_16'Val (Char_16_C'Pos (Item (Int_Size_C (I))));
      end loop;
      return To_Str_16 (Buffer);
    end;
  function To_Str_16 (Item : Ptr_Const_Char_16_C) return Str_16 is
    Length : Int_32;
    Buffer : Str_16_Unbound;
    begin
      while Item.All /= NULL_CHAR_16_C loop
        Length := Length + 1;
        Buffer := Buffer & Char_16 (Item.All);
        Item   := To_Ptr_Const_Char_16_C (To_Address (To_Int_Address (Item) + Char_16_C'Size / Int_8_Unsigned'Size));
      end loop;
      return To_Str_16 (Buffer);
    end;          
  function To_Str_8 (Item : Ptr_Const_Char_8_C) return Str_8 is 
    Length : Int_32;
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
    Buffer : Str_16_Unbound;
    Input  : Str_16 (1..RADIAN_IMAGE_STRING_SIZE);
    begin
      Put (Input, Item, Number_Base (Base));
      if Base = 10 then return Trim (Input, Both); end if;
      Buffer := To_Str_16_Unbound (Trim (Input, Both));
      Delete (Buffer, 1, Trim (Base'Img, Both)'Length + 1);
      Delete (Buffer, Length (Buffer), Length (Buffer));
      if Item /= Num_T'Last and Do_Pad_Zeros then
        for I in 1..Generic_To_Str_16 (Num_T'Last, Base)'Length - Length (Buffer) loop Insert (Buffer, 1, "0"); end loop;
      end if;
      return To_Str_16 (Buffer);
    end;
  function Split (Item : Str_16; On : Str_16 := " ") return Array_Str_16_Unbound is
    package Vector_Str_16_Unbound is new Ada.Containers.Indefinite_Vectors (Int_32_Positive, Str_16_Unbound);
    function Internal (Item : Str_16; On : Str_16 := " ") return Vector_Str_16_Unbound.Vector is
      Result    : Vector_Str_16_Unbound.Vector;
      TRIMMED   : constant Str_16  a2Q  19:= Trim (Item, Both);
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
    Extension_Text : Str_16_Unbound := NULL_STRING_16_UNBOUNDED;
    begin
      for I in reverse Name'Range loop
        if Name (I) = '.' and I /= Name'length then return To_Lower (Name (I + 1..Name'Last)); end if;
      end loop;
      raise Unknown;
    end;
  package body Handler is
      procedure Save (Name : Str_16; Item : T) is begin        Formats.Element (Match_Extension (Name)).Save (Build_Path (Name), Item); end;
      function Load  (Name : Str_16) return T  is begin return Formats.Element (Match_Extension (Name)).Load (Build_Path (Name));       end;
      function Match_Extension (Value : Str_16) return Enumerated_Format is
            Format := Formats.Element (Current);
            for Extension of Split (To_Str_16 (Format.Extensions), ",") loop
              if Extension = Get_Extension (Value) then return Formats.Key (Current); end if;
            end loop;
          raise Unsupported;
        end;
      package body Format is
          procedure Finalize   (Item : in out Record_Controller) is begin Formats.Delete (Kind); end;
          procedure Initialize (Item : in out Record_Controller) is
            begin
              if Formats.Has_Element (Kind) then raise Duplicate_Format; end if;
              Formats.Insert (Kind, (Save, Load, To_Str_16_Unbound (To_Lower (Extensions))));
            end;
        end;
    end;
  package body Parser is
      function Next_Set  (Ending : Str_16) return Str_16_Unbound is      (Next_Set (Element (This.Element (Row), Column), Ending));
      procedure Skip_Set (Ending : Str_16) return Str_16_Unbound is begin Skip_Set (Element (This.Element (Row), Column), Ending); end;
      function Next_Line return Str_16_Unbound is
        Result : Str_2_Unbound;
        begin

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
      procedure Skip_Set (Starting, Ending : Str_16) is Junk : Str_16_Unbound := Next_Set (Starting, Ending); begin null; end;
      procedure Skip (Number_To_Skip : Int_32_Positive := 1) is
        Junk : Str_16_Unbound;
        begin
          for I in 1..Number_To_Skip loop Junk := Next; end loop;
        end;
      procedure Assert (Text : Str_16) is
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
      procedure Seek is
        begin
          Column := Column + 1;
          while not At_EOF loop
            loop
              if Column > Length (This.Element (Row)) or else This.Element (Row) = NULL_STRING_16_UNBOUNDED then
                Column := 1;
                Row    := Row + 1;
                exit;
              end if;
              if Index (Slice (This.Element (Row), Column, Length (This.Element (Row))), Separator) /= Column then return;
              --if Length (This.Element (Row)) - Column > Separator'length and then Slice (This.Element (Row), Column, Column + Separator'length) = Separator then return;
              elsif not Do_Ignore_Multiple then
                Column := Column + Separator'length;
                return;
              end if;
              Column := Column + 1;
            end loop;
          end loop;
        end;
      function Next return Str_16_Unbound is
        Result : Str_16_Unbound := To_Str_16_Unbound (Slice (This.Element (Row), Column, Length (This.Element (Row))));
        I      : Int_32_Natural := Index (Result, Separator);
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
          if Result = NULL_STRING_16_UNBOUNDED then raise Invalid; end if;
          Seek;
          return Float_64_Real'Wide_Value (To_Str_16 (Result));
        end;
      function Next_Set (Starting, Ending : Str_16) return Str_16_Unbound is
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
      Raw_This             : File_Type;
      In_Multiline_Comment : Bool := False;
      begin
        Open (Raw_This, In_File, Path);
        while not End_Of_File (Raw_This) loop
          This.Append (To_Str_16_Unbound (Get_Line (Raw_This)));
          if Comment_Singleline /= NULL_STRING_16 and then Index (This.Last_Element, Comment_Singleline) /= 0 then
            This.Replace_Element (This.Last_Index, Head (This.Last_Element, Index (This.Last_Element, Comment_Singleline) - 1));
          end if;
          if Comment_Multiline /= (others => NULL_STRING_16) then
            if In_Multiline_Comment and then Index (This.Last_Element, Comment_Multiline'Last) + Length (Comment_Multiline'Last) /= This.Last_Index then
              In_Multiline_Comment := False;
              This.Replace_Element (This.Last_Index, Head (This.Last_Element, Index (This.Last_Element, Comment_Multiline'Last)));
            elsif Index (This.Last_Element, Comment_Multiline'First) /= 0 then
              In_Multiline_Comment := True;
              This.Replace_Element (This.Last_Index, Tail (This.Last_Element, Index (This.Last_Element, Comment_Multiline'Last)));
            else This.Replace_Element (This.Last_Index, NULL_STRING_16_UNBOUNDED); end if;
          end if;
          if Tab_Replacement /= NULL_STRING_16 then
            while Index (This.Last_Element, TAB_16) /= 0 loop
              This.Replace_Element (This.Last_Index, Overwrite (This.Last_Element, Index (This.Last_Element, TAB_16), " "));
            end loop;
          end if;
        end loop;
        Close (Raw_This);
        Seek;
      end;
    end;
end;