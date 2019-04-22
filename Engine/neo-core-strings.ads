
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

with Neo.Core.Arrays; use Neo.Core.Arrays;

-- Miscellaneous string utilities
package Neo.Core.Strings is

  -----------
  -- Split --
  -----------

  function Split_Vec (Item : Str; On : Str) return Vector_Str_16_Unbound.Unsafe.Vector;
  function Split     (Item : Str; On : Str) return Array_Str_Unbound is (Vector_Str_16_Unbound.To_Unsafe_Array (Split_Vec (Item, On)));

  function Split_Vec_On_Whitespace (Item : Str) return Vector_Str_16_Unbound.Unsafe.Vector;
  function Split_On_Whitespace     (Item : Str) return Array_Str_Unbound is (Vector_Str_16_Unbound.To_Unsafe_Array (Split_Vec_On_Whitespace (Item)));

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
end;
