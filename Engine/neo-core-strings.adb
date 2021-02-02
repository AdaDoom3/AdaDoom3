
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

-- Needs cleanup...
package body Neo.Core.Strings is

  -----------
  -- Split --
  -----------

  -- Could this be done cleanly without vectors ???
  function Split_Vec (Item : Str; On : Str) return Vector_Str_16_Unbound.Unsafe.Vector is
    Result    : Vector_Str_16_Unbound.Unsafe.Vector; use Vector_Str_16_Unbound.Unsafe;
    TRIMMED   : constant Str     := Trim (Item, Both);
    REMAINDER : constant Natural := Index (Item, On);
    begin
      if REMAINDER = 0 then return Result & To_Str_Unbound (Item);
      else Result.Append (To_Str_Unbound (Trim (TRIMMED (TRIMMED'First..REMAINDER - 1), Both))); end if;
      return Result & (Split_Vec (TRIMMED (REMAINDER + 1..TRIMMED'Last), On));
    end;

  -- This is sh!t
  function Split_Vec_On_Whitespace (Item : Str) return Vector_Str_16_Unbound.Unsafe.Vector is
    package WL1 renames Ada.Characters.Wide_Latin_1;
    Result    : Vector_Str_16_Unbound.Unsafe.Vector; use Vector_Str_16_Unbound.Unsafe;
    WHITESPACE_CHARS : constant array (1..5) of Char := (' ', WL1.HT, WL1.LF, WL1.CR, WL1.VT);
    TRIMMED   : constant Str     := Trim (Item, Both);
    Current   :          Natural := Natural'Last;
    Test      :          Natural := 0;
    begin
      for Char of WHITESPACE_CHARS loop
        Test := Index (TRIMMED, To_Str (Char));
        if Test /= 0 and Test < Current then Current := Test; end if;
      end loop;
      if Current = Natural'Last then return Result & To_Str_Unbound (TRIMMED);
      else Result.Append (To_Str_Unbound (Trim (TRIMMED (TRIMMED'First..Current - 1), Both))); end if;
      return Result & (Split_Vec_On_Whitespace (TRIMMED (Current + 1..TRIMMED'Last)));
    end;

  ----------
  -- Trim --
  ----------

  procedure Trim (Item : in out Str_Unbound) is
    package WL1 renames Ada.Characters.Wide_Latin_1;
    I : Natural := 1;
    begin
      while I <= Length (Item) and then Element (Item, I) in ' ' | WL1.HT | WL1.LF | WL1.CR | WL1.VT loop
        I := I + 1;
      end loop;
      if I /= 1 then Tail (Item, Length (Item) - I); end if;
      I := Length (Item);
      while I > 0 and then Element (Item, I) in ' ' | WL1.HT | WL1.LF | WL1.CR | WL1.VT loop
        I := I - 1;
      end loop;
      if I /= Length (Item) then Head (Item, Length (Item) - I); end if;
    end;

  -------------
  -- Replace --
  -------------

  procedure Replace (Item : in out Str_Unbound; From : Str; To : Str) is begin Item := Replace (Item, From, To); end;
  function Replace (Item : Str_Unbound; From : Str; To : Str) return Str_Unbound is
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
end;
