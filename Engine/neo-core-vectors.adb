
--                                                                                                                               --
--                                                      N E O  E N G I N E                                                       --
--                                                                                                                               --
--                                               Copyright (C) 2020 Justin Squirek                                               --
--                                                                                                                               --
-- Neo is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published --
-- by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.                      --
--                                                                                                                               --
-- Neo is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of         --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.                     --
--                                                                                                                               --
-- You should have received a copy of the GNU General Public License along with Neo. If not, see gnu.org/licenses                --
--                                                                                                                               --

package body Neo.Core.Vectors is
  protected body Safe_Vector is
      procedure Set     (Val : Unsafe.Vector)                                      is begin This := Val;                                       end;
      procedure Set     (Val : Unsafe_Array)                                       is begin Set (To_Unsafe_Vector (Val));                      end;
      procedure Clear                                                              is begin This.Clear;                                        end;
      procedure Next    (Pos : in out Cursor)                                      is begin Unsafe.Next           (Pos);                       end;
      procedure Replace (Pos :        Cursor; Item : Vec_T)                        is begin This.Replace_Element  (Pos, Item);                 end;
      procedure Replace (Pos : Positive;      Item : Vec_T)                        is begin This.Replace_Element  (Pos, Item);                 end;
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
