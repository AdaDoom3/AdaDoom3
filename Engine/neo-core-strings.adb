
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

package body Neo.Core.Strings is

  -----------
  -- Split --
  -----------

  -- Could this be done cleanly without vectors ???
  function Split_Vec (Item : Str; On : Str := " ") return Vector_Str_16_Unbound.Unsafe.Vector is
    Result    : Vector_Str_16_Unbound.Unsafe.Vector; use Vector_Str_16_Unbound.Unsafe;
    TRIMMED   : constant Str     := Trim (Item, Both);
    REMAINDER : constant Natural := Index (TRIMMED, On);
    begin
      if REMAINDER = 0 then return Result & To_Str_Unbound (TRIMMED);
      else Result.Append (To_Str_Unbound (Trim (TRIMMED (TRIMMED'First..REMAINDER - 1), Both))); end if;
      return Result & (Split_Vec (TRIMMED (REMAINDER..TRIMMED'Last), On));
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
