
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

package body Neo.Arrays is

  -------------
  -- Sorting --
  -------------

  procedure Sort (Item : in out Array_Str_Unbound) is procedure Internal_Sort is new Ada.Containers.Generic_Array_Sort (Positive, Str_Unbound, Array_Str_Unbound, "<"); begin Internal_Sort (Item); end;
  procedure Sort (Item : in out Array_Int)         is procedure Internal_Sort is new Ada.Containers.Generic_Array_Sort (Positive, Int,         Array_Int,         "<"); begin Internal_Sort (Item); end;
  procedure Sort (Item : in out Array_Int_64)      is procedure Internal_Sort is new Ada.Containers.Generic_Array_Sort (Positive, Int_64,      Array_Int_64,      "<"); begin Internal_Sort (Item); end;
  procedure Sort (Item : in out Array_Real)        is procedure Internal_Sort is new Ada.Containers.Generic_Array_Sort (Positive, Real,        Array_Real,        "<"); begin Internal_Sort (Item); end;
  procedure Sort (Item : in out Array_Real_64)     is procedure Internal_Sort is new Ada.Containers.Generic_Array_Sort (Positive, Real_64,     Array_Real_64,     "<"); begin Internal_Sort (Item); end;
end;