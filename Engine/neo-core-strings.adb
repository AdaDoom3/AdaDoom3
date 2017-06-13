
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

  --------------------
  -- C String Array --
  --------------------

  function To_C_String_Array (Item : Array_Str_Unbound) return C_String_Array is
    Result : C_String_Array (Item'Length);-- := (Controlled with Data => new Array_Ptr_Str_8_C (Item'Range));
    begin
      for I in Result.Data'Range loop
        Result.Data (I) := new Str_8_C'(To_Str_8_C (S (Item (I))));
        Result.Ptrs (I) := Result.Data (I)(1)'Unchecked_Access;
      end loop;
      return Result;
    end;
  --procedure Finalize (Item : in out C_String_Array) is
  --  procedure Free is new Unchecked_Deallocation (Str_8_C, Ptr_Str_8_C);
  --  procedure Free is new Unchecked_Deallocation (Array_Ptr_Str_8_C, Ptr_Array_Ptr_Str_8_C);
  --  begin
  --    for I in Item.Data'Range loop Free (Item.Data (I)); end loop;
  --    Free (Item.Data);
  --  end;

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
  --function Split (Item : Str; On : Str := " ") return Vector_Str_16_Unbound.Unsafe.Vector;
  function Split (Item : Str; On : Str := " ") return Array_Str_Unbound is (Vector_Str_16_Unbound.To_Unsafe_Array (Split (Item, On)));

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
