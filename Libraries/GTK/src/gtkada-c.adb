-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 2006-2013, AdaCore              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

package body Gtkada.C is

   ----------------------
   -- Unbounded_Arrays --
   ----------------------

   package body Unbounded_Arrays is

      --------------
      -- To_Array --
      --------------

      function To_Array
        (Arr : Unbounded_Array_Access; N : Index) return T_Array
      is
      begin
         if Arr = null then
            return (Index'Val (1) .. Index'Val (0) => Null_T);
         else
            declare
               Result : T_Array (Index'Val (1) .. N);
            begin
               for R in Index'Val (1) .. N loop
                  Result (R) := Arr (R);
               end loop;
               return Result;
            end;
         end if;
      end To_Array;
   end Unbounded_Arrays;

end Gtkada.C;
