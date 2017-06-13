
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
