
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

package body Neo.Core.Hashed is
  protected body Safe_Map is
      procedure Set     (Val : Unsafe.Map)                      is begin This := Val;                                       end;
      procedure Clear                                           is begin This.Clear;                                        end;
      procedure Next    (Pos : in out Cursor)                   is begin Unsafe.Next          (Pos);                        end;
      procedure Delete  (Pos : in out Cursor)                   is begin This.Delete          (Pos);                        end;
      procedure Delete  (Key : Str)                             is begin This.Delete          (To_Str_Unbound (Key));       end;
      procedure Replace (Pos : Cursor; Item : Map_T)            is begin This.Replace_Element (Pos, Item);                  end;
      procedure Replace (Key : Str; Item : Map_T)               is begin This.Replace         (To_Str_Unbound (Key), Item); end;
      procedure Insert  (Key : Str; Item : Map_T)               is begin This.Insert          (To_Str_Unbound (Key), Item); end;
      function Has      (Key : Str)    return Bool              is (This.Contains             (To_Str_Unbound (Key)));
      function Has      (Pos : Cursor) return Bool              is (Unsafe.Has_Element        (Pos));
      function Key      (Pos : Cursor) return Str               is (To_Str (Unsafe.Key        (Pos)));
      function Get      (Key : Str)    return Map_T             is (This                      (To_Str_Unbound (Key)));
      function Get      (Pos : Cursor) return Map_T             is (Unsafe.Element            (Pos));
      function Get                     return Unsafe.Map        is (This);
      function Keys                    return Array_Str_Unbound is (Keys                      (This));
      function First                   return Cursor            is (This.First);
      function Length                  return Natural           is (Natural (This.Length));
    end;

  -- Sorted key conversions
  function Keys (Val : Unsafe.Map) return Array_Str_Unbound is
    Result : Array_Str_Unbound (1..Int (Val.Length));
    J      : Int := Result'First;
    begin
      for I in Val.Iterate loop
        Result (J) := Unsafe.Key (I);
        J := J + 1;
      end loop;
      Sort (Result);
      return Result;
    end;
end;