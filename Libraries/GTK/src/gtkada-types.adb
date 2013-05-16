-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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

package body Gtkada.Types is

   use Interfaces.C.Strings;

   ---------
   -- "+" --
   ---------

   function "+" (S1, S2 : String) return Chars_Ptr_Array is
   begin
      return (0 => New_String (S1), 1 => New_String (S2));
   end "+";

   function "+" (S1 : Chars_Ptr_Array; S2 : String) return Chars_Ptr_Array is
   begin
      return S1 + New_String (S2);
   end "+";

   function "+" (S1 : Chars_Ptr_Array; S2 : Chars_Ptr)
     return Chars_Ptr_Array
   is
      use type Interfaces.C.size_t;

      Result : Chars_Ptr_Array (S1'First .. S1'Last + 1);

   begin
      Result (S1'Range) := S1;
      Result (S1'Last + 1) := S2;
      return Result;
   end "+";

   function "+" (S1 : Chars_Ptr; S2 : String) return Chars_Ptr_Array is
   begin
      return (0 => S1, 1 => New_String (S2));
   end "+";

   ----------
   -- Free --
   ----------

   procedure Free (A : in out Chars_Ptr_Array) is
   begin
      for J in A'Range loop
         Interfaces.C.Strings.Free (A (J));
      end loop;
   end Free;

   function Null_Array return Chars_Ptr_Array is
   begin
      return (1 .. 0 => Null_Ptr);
   end Null_Array;

end Gtkada.Types;
