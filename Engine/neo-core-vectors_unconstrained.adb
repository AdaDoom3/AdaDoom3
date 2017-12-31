
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

package body Neo.Core.Vectors_Unconstrained is
  protected body Safe_Vector is
      procedure Set     (Val : Unsafe.Vector)                                      is begin This := Val;                                       end;
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
    end;
end;
