
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

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Wide_Unbounded.Wide_Hash;
with Neo.Core.Arrays; use Neo.Core.Arrays;

generic
  type Map_T is private;
package Neo.Core.Hashed is

  -----------------
  -- Hashed Maps --
  -----------------

  -- Base type
  package Unsafe is new Ada.Containers.Hashed_Maps (Str_Unbound, Map_T, Wide_Hash, "=");
  subtype Cursor is Unsafe.Cursor;
  NO_ELEMENT : constant Cursor     := Unsafe.NO_ELEMENT;
  EMPTY_MAP  : constant Unsafe.Map := Unsafe.EMPTY_MAP;

  -- Fetch a sorted list of keys
  function Keys (Val : Unsafe.Map) return Array_Str_Unbound;

  -- Wrapped type
  protected type Safe_Map is
      procedure Clear;
      procedure Set     (Val : Unsafe.Map);
      procedure Next    (Pos : in out Cursor);
      procedure Delete  (Pos : in out Cursor);
      procedure Delete  (Key : Str);
      procedure Delete  (Key : Str_Unbound);
      procedure Replace (Pos : Cursor;      Item : Map_T);
      procedure Replace (Key : Str;         Item : Map_T);
      procedure Replace (Key : Str_Unbound; Item : Map_T);
      procedure Insert  (Key : Str;         Item : Map_T);
      procedure Insert  (Key : Str_Unbound; Item : Map_T);
      function Has      (Key : Str)         return Bool;
      function Has      (Key : Str_Unbound) return Bool;
      function Has      (Pos : Cursor)      return Bool;
      function Key      (Pos : Cursor)      return Str;
      function Get      (Key : Str)         return Map_T;
      function Get      (Key : Str_Unbound) return Map_T;
      function Get      (Pos : Cursor)      return Map_T;
      function Get                          return Unsafe.Map;
      function Keys                         return Array_Str_Unbound;
      function First                        return Cursor;
      function Length                       return Natural;
    private
      This : Unsafe.Map;
    end;
end;
