
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

package body Neo.Core is
  package body Neo.Core.Vectors is
    protected body Safe_Vector is
        procedure Set     (Val : Unsafe.Vector)                                      is begin This := Val;                                       end;
        procedure Set     (Val : Unsafe_Array)                                       is begin Set (To_Unsafe_Vector (Val));                      end;
        procedure Clear                                                              is begin This.Clear;                                        end;
        procedure Next    (Pos : in out Cursor)                                      is begin Unsafe.Next           (Pos);                       end;
        procedure Replace (Pos :        Cursor; Item : Vec_T)                        is begin This.Replace_Element  (Pos, Item);                 end;
        procedure Append                       (Item : Vec_T; Count : Positive := 1) is begin This.Append           (Item,  Count_Type (Count)); end;
        procedure Prepend                      (Item : Vec_T; Count : Positive := 1) is begin This.Prepend          (Item,  Count_Type (Count)); end;
        procedure Insert  (Before : Positive;   Item : Vec_T; Count : Positive := 1) is begin This.Insert           (Before, Item);              end;
        procedure Delete  (Index  : Positive;                 Count : Positive := 1) is begin This.Delete           (Index, Count_Type (Count)); end;
        function Has      (Pos    : Cursor)    return Boolean                        is (Unsafe.Has_Element         (Pos));
        function Get      (Pos    : Cursor)    return Vec_T                          is (Unsafe.Element             (Pos));
        function Get      (Index  : Positive)  return Vec_T                          is (This                       (Index));
        function Get                           return Unsafe.Vector                  is (This);
        function First                         return Cursor                         is (This.First);
        function Length                        return Positive                       is (Positive                   (This.Length));
        function To_Array                      return Unsafe_Array                   is (To_Unsafe_Array            (This));
      end;

    -- Vector and array conversions
    function To_Unsafe_Vector (Item : Unsafe_Array) return Unsafe.Vector is
      Result : Unsafe.Vector;
      begin
        for I of Item loop Result.Append (I); end loop;
        return Result;
      end;
    function To_Unsafe_Array (Item : Unsafe.Vector) return Unsafe_Array is
      Result : Unsafe_Array (1..Integer (Item.Length));
      begin
        for I in 1..Integer (Item.Length) loop Result (I) := Item.Element (I); end loop;
        return Result;
      end;
  end;


  package body Neo.Core.Trees is
    protected body Safe_Tree is
        procedure Set             (Source : Unsafe.Tree)                                                        is begin This := Source;                                                             end;
        procedure Clear                                                                                         is begin This.Clear;                                                                 end;
        procedure Replace         (Pos    : Cursor; Item : Tree_T)                                              is begin This.Replace_Element            (Pos, Item);                                end;
        procedure Query           (Pos    : Cursor; Process : not null access procedure (Item : Tree_T))        is begin Unsafe.Query_Element            (Pos,    Process);                          end;
        procedure Update          (Pos    : Cursor; Process : not null access procedure (Item : in out Tree_T)) is begin This.Update_Element             (Pos,    Process);                          end;
        procedure Iterate_Subtree (Pos    : Cursor; Process : not null access procedure (Pos : Cursor))         is begin Unsafe.Iterate_Subtree          (Pos,    Process);                          end;
        procedure Iterate                          (Process : not null access procedure (Pos : Cursor))         is begin This.Iterate                            (Process);                          end;
        procedure Iterate         (Parent : Cursor; Process : not null access procedure (Pos : Cursor))         is begin Unsafe.Iterate_Children         (Parent, Process);                          end;
        procedure Iterate_Back    (Parent : Cursor; Process : not null access procedure (Pos : Cursor))         is begin Unsafe.Reverse_Iterate_Children (Parent, Process);                          end;
        procedure Move            (Source : in out Unsafe.Tree)                                                 is begin This.Move                       (Source);                                   end;
        procedure Delete_Leaf     (Pos    : in out Cursor)                                                      is begin This.Delete_Leaf                (Pos);                                      end;
        procedure Delete_Subtree  (Pos    : in out Cursor)                                                      is begin This.Delete_Subtree             (Pos);                                      end;
        procedure Swap            (I, J   : Cursor)                                                             is begin This.Swap                       (I, J);                                     end;
        procedure Delete          (Parent         : Cursor)                                                     is begin This.Delete_Children            (Parent);                                   end;
        procedure Prepend         (Parent         : Cursor; Item : Tree_T; Count : Positive := 1)               is begin This.Prepend_Child              (Parent,         Item, Count_Type (Count)); end;
        procedure Append          (Parent         : Cursor; Item : Tree_T; Count : Positive := 1)               is begin This.Append_Child               (Parent,         Item, Count_Type (Count)); end;
        procedure Insert          (Parent, Before : Cursor; Item : Tree_T; Count : Positive := 1)               is begin This.Insert_Child               (Parent, Before, Item, Count_Type (Count)); end;
        procedure Splice_Subtree  (Parent, Before, Pos           : Cursor)                                      is begin This.Splice_Subtree             (Parent, before, Pos);                      end;
        procedure Splice_Children (Parent, Before, Source_Parent : Cursor)                                      is begin This.Splice_Children            (Parent, Before, Source_Parent);            end;
        procedure Copy_Subtree    (Parent, Before, Source        : Cursor)                                      is begin This.Copy_Subtree               (Parent, Before, Source);                   end;
        procedure Next            (Pos    : in out Cursor)                                                      is begin Unsafe.Next_Sibling             (Pos);                                      end;
        procedure Previous        (Pos    : in out Cursor)                                                      is begin Unsafe.Previous_Sibling         (Pos);                                      end;
        function Subtree_Nodes    (Pos    : Cursor)                return Positive                              is (Positive (Unsafe.Subtree_Node_Count  (Pos)));
        function Depth            (Pos    : Cursor)                return Positive                              is (Positive (Unsafe.Depth               (Pos)));
        function "="              (L, R   : Cursor)                return Bool                                  is (Unsafe.Equal_Subtree                 (L, R));
        function Is_Root          (Pos    : Cursor)                return Bool                                  is (Unsafe.Is_Root                       (Pos));
        function Is_Leaf          (Pos    : Cursor)                return Bool                                  is (Unsafe.Is_Leaf                       (Pos));
        function Has              (Pos    : Cursor)                return Bool                                  is (Unsafe.Has_Element                   (Pos));
        function Equals           (Item   : Unsafe.Tree)           return Bool                                  is (This."="                             (Item));
        function Find             (Item   : Tree_T)                return Cursor                                is (This.Find                            (Item));
        function Get              (Pos    : Cursor)                return Tree_T                                is (Unsafe.Element                       (Pos));
        function Find_In_Subtree  (Pos    : Cursor; Item : Tree_T) return Cursor                                is (Unsafe.Find_In_Subtree               (Pos, Item));
        function Ancestor_Find    (Pos    : Cursor; Item : Tree_T) return Cursor                                is (Unsafe.Ancestor_Find                 (Pos, Item));
        function Child_Count      (Parent        : Cursor)         return Positive                              is (Positive (Unsafe.Child_Count         (Parent)));
        function Child_Depth      (Parent, Child : Cursor)         return Positive                              is (Positive (Unsafe.Child_Depth         (Parent, Child)));
        function Parent           (Pos    : Cursor)                return Cursor                                is (Unsafe.Parent                        (Pos));
        function First            (Parent : Cursor)                return Cursor                                is (Unsafe.First_Child                   (Parent));
        function First            (Parent : Cursor)                return Tree_T                                is (Unsafe.First_Child_Element           (Parent));
        function Last             (Parent : Cursor)                return Tree_T                                is (Unsafe.Last_Child_Element            (Parent));
        function Last             (Parent : Cursor)                return Cursor                                is (Unsafe.Last_Child                    (Parent));
        function Node_Count                                        return Positive                              is (Positive (This.Node_Count));
        function Root                                              return Cursor                                is (This.Root);
        function Is_Empty                                          return Bool                                  is (This.Is_Empty);
        function Get                                               return Unsafe.Tree                           is (This);
      end;
  end;

  package body Neo.Core.Ordered is
    protected body Safe_Map is       
        procedure Set     (Val : Unsafe.Map)               is begin This := Val;                      end;
        procedure Clear                                    is begin This.Clear;                       end;  
        procedure Next    (Pos : in out Cursor)            is begin Unsafe.Next          (Pos);       end;
        procedure Delete  (Pos : in out Cursor)            is begin This.Delete          (Pos);       end;
        procedure Delete  (Key : Key_T)                    is begin This.Delete          (Key);       end;
        procedure Replace (Pos : Cursor; Item : Map_T)     is begin This.Replace_Element (Pos, Item); end;
        procedure Replace (Key : Key_T;  Item : Map_T)     is begin This.Replace         (Key, Item); end;
        procedure Insert  (Key : Key_T;  Item : Map_T)     is begin This.Insert          (Key, Item); end;
        function Has      (Key : Key_T)  return Bool       is (This.Contains             (Key));
        function Has      (Pos : Cursor) return Bool       is (Unsafe.Has_Element        (Pos));
        function Key      (Pos : Cursor) return Key_T      is (Unsafe.Key                (Pos));
        function Get      (Pos : Cursor) return Map_T      is (Unsafe.Element            (Pos));
        function Get      (Key : Key_T)  return Map_T      is (This                      (Key));
        function Get                     return Unsafe.Map is (This);
        function First                   return Cursor     is (This.First);
        function Length                  return Natural    is (Natural (This.Length));
      end;
  end;

  -----------
  -- Stack --
  -----------

  -- Called by all tasks (including the main execution) during an error to report exception and trace information
  procedure Put_Stack is
    Traces : Tracebacks_Array (1..128);
    Result : Str_Unbound := NULL_STR_UNBOUND;
    Skip   : Bool        := False;
    Length : Natural     := 0;
    begin
      Call_Chain (Traces, Length);
      
      -- Change line endings
      for Item of Symbolic_Traceback (Traces) loop
        if not Skip and Item = '0' then Skip := True;
        elsif Item = ASCII.LF then
          if Skip then Skip := False;
          else Result := Result & EOL; end if;
        elsif not Skip then Result := Result & To_Str (Item); end if;
      end loop;
      Line (Result);
    end;
    
  -----------
  -- Split --
  -----------

  -- Could this be done cleanly without vectors ???
  function Split (Item : Str; On : Str := " ") return Vector_Str_16_Unbound.Unsafe.Vector is
    Result    : Vector_Str_16_Unbound.Unsafe.Vector; use Vector_Str_16_Unbound.Unsafe;
    TRIMMED   : constant Str     := Trim (Item, Both);
    REMAINDER : constant Natural := Index (TRIMMED, On);
    begin
      if REMAINDER = 0 then return Result & To_Str_Unbound (TRIMMED);
      else Result.Append (To_Str_Unbound (Trim (TRIMMED (TRIMMED'First..REMAINDER - 1), Both))); end if;
      return Result & (Split (TRIMMED (REMAINDER..TRIMMED'Last), On));
    end;
  function Split (Item : Str; On : Str := " ") return Array_Str_Unbound is (Vector_Str_16_Unbound.To_Unsafe_Array (Split (Item, On)));

  -------------
  -- Replace --
  -------------

  procedure Replace (Item : in out Str_Unbound; From, To : Str) is begin Item := Replace (Item, From, To); end;
  function Replace (Item : Str_Unbound; From, To : Str) return Str_Unbound is
    I      : Natural     := 0;
    Result : Str_Unbound := Item;
    begin
      loop
        I := Index (Result, From);
        exit when I = 0;
        Overwrite (Result, I, To);
      end loop;
      return Result;
    end;

  -----------
  -- Count --
  -----------

  function Count (Text : Str; Item : Char) return Natural is
    Result : Natural := 0;
    begin
      for C of Text loop
        if C = To_Char_16 (ASCII.CR) then
          Result := Result + 1;
        end if;
      end loop;
      return Result;
    end;
end;