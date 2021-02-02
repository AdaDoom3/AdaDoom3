
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

with Neo.Core.Arrays; use Neo.Core.Arrays;

-- Miscellaneous string utilities
package Neo.Core.Strings is

  -----------
  -- Split --
  -----------

  function Split_Vec (Item : Str; On : Str) return Vector_Str_16_Unbound.Unsafe.Vector;
  function Split     (Item : Str; On : Str) return Array_Str_Unbound is
    (Vector_Str_16_Unbound.To_Unsafe_Array (Split_Vec (Item, On)));

  function Split_Vec_On_Whitespace (Item : Str) return Vector_Str_16_Unbound.Unsafe.Vector;
  function Split_On_Whitespace     (Item : Str) return Array_Str_Unbound is
    (Vector_Str_16_Unbound.To_Unsafe_Array (Split_Vec_On_Whitespace (Item)));

  -------------
  -- Replace --
  -------------

  function Replace (Item : Str_Unbound; From : Str; To : Str) return Str_Unbound;
  function Replace (Item : Str;         From : Str; To : Str) return Str is (S (Replace (U (Item), From, To)));
  procedure Replace (Item : in out Str_Unbound; From : Str; To : Str);

  ----------
  -- Trim --
  ----------

  procedure Trim (Item : in out Str_Unbound);

  ------------------------
  -- Number Conversions --
  ------------------------

  function To_Str is new Generic_To_Str_16_Real (Real_64);
  function To_Str_16_Int is new Generic_To_Str_16_Int (Int_Unsigned_C);

  --------------------
  -- Util_Str_Super --
  --------------------

  generic
    Max_Length : Positive;
  package Util_Str_Super is
    subtype T is Str_16_Super (Max_Length);
    NULL_STR : constant T := To_Super_String ("", Max_Length);
    type Array_T is array (Positive range <>) of T;
    function S              (Val : Str) return T         is (To_Super_String (Val, Max_Length));
    function To_Str         (Val : T) return Str         is (To_Str_16 (Val));
    function To_Str_Unbound (Val : T) return Str_Unbound is (U (To_Str (Val)));
    function S              (Val : T) return Str         renames To_Str;
    function U              (Val : T) return Str_Unbound renames To_Str_Unbound;
  end;

  generic
    Max_Length : Positive;
  package Util_Str_8_Super is
    subtype T is Str_8_Super (Max_Length);
    NULL_STR : constant T := To_Super_String ("", Max_Length);
    type Array_T is array (Positive range <>) of T;
    function S              (Val : Str) return T           is (To_Super_String (To_Str_8 (Val), Max_Length));
    function To_Str         (Val : T)   return Str         is (To_Str_16 (To_Str_8 (Val)));
    function To_Str_Unbound (Val : T)   return Str_Unbound is (U (To_Str (Val)));
    function S              (Val : T)   return Str         renames To_Str;
    function U              (Val : T)   return Str_Unbound renames To_Str_Unbound;
  end;
end;
