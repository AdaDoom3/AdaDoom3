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

with Glib;                 use Glib;
with GNAT.Strings;         use GNAT.Strings;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Gtkada.Bindings is

   --------------------
   -- String_Or_Null --
   --------------------

   function String_Or_Null
     (S : String) return Interfaces.C.Strings.chars_ptr is
   begin
      if S = "" then
         return Null_Ptr;
      else
         return New_String (S);
      end if;
   end String_Or_Null;

   --------------------
   -- To_String_List --
   --------------------

   function To_String_List
     (C : Interfaces.C.Strings.chars_ptr_array) return String_List
   is
      Count : Natural := 0;
   begin
      while C (size_t (Count)) /= Null_Ptr loop
         Count := Count + 1;
      end loop;

      return To_String_List (C, Gint (Count));
   end To_String_List;

   --------------------
   -- To_String_List --
   --------------------

   function To_String_List
     (C : ICS.chars_ptr_array; N : Gint) return GNAT.Strings.String_List
   is
      Result : String_List (1 .. Natural (N));
   begin
      for R in Result'Range loop
         Result (R) := new String'(Value (C (size_t (R) - 1)));
      end loop;
      return Result;
   end To_String_List;

   ----------------------
   -- From_String_List --
   ----------------------

   function From_String_List
     (C : String_List) return Interfaces.C.Strings.chars_ptr_array
   is
      Result : Interfaces.C.Strings.chars_ptr_array (0 .. C'Length);
   begin
      for S in C'Range loop
         Result (size_t (S - C'First)) := New_String (C (S).all);
      end loop;
      Result (Result'Last) := Null_Ptr;
      return Result;
   end From_String_List;

   ------------------
   -- To_Chars_Ptr --
   ------------------

   function To_Chars_Ptr
     (C : chars_ptr_array_access) return ICS.chars_ptr_array
   is
      Count : size_t := 0;
   begin
      while C (Count) /= Null_Ptr loop
         Count := Count + 1;
      end loop;

      declare
         Result : chars_ptr_array (0 .. Count - 1);
      begin
         for J in Result'Range loop
            Result (J) := C (J);
         end loop;
         return Result;
      end;
   end To_Chars_Ptr;

   -------------------
   -- To_Gint_Array --
   -------------------

--     function To_Gint_Array
--       (Arr : Unbounded_Gint_Array_Access; N : Gint) return Glib.Gint_Array
--     is
--     begin
--        if Arr = null then
--           return (1 .. 0 => 0);
--        else
--           declare
--              Result : Glib.Gint_Array (1 .. Natural (N));
--           begin
--              for R in 0 .. Natural (N - 1) loop
--                 Result (R + 1) := Arr (R);
--              end loop;
--              return Result;
--           end;
--        end if;
--     end To_Gint_Array;

   -----------------------------------
   -- To_Gint_Array_Zero_Terminated --
   -----------------------------------

   function To_Gint_Array_Zero_Terminated
     (Arr : Gint_Arrays.Unbounded_Array_Access) return Glib.Gint_Array
   is
      Count : Natural := 0;
   begin
      while Arr (Count) /= 0 loop
         Count := Count + 1;
      end loop;

      declare
         Result : Gint_Array (1 .. Count);
      begin
         for R in Result'Range loop
            Result (R) := Arr (R - 1);
         end loop;
         return Result;
      end;
   end To_Gint_Array_Zero_Terminated;

   --------------------
   -- To_Point_Array --
   --------------------

--     function To_Point_Array
--       (Arr : Unbounded_Points_Array_Access; N : Glib.Gint)
--        return Gdk.Types.Gdk_Points_Array
--     is
--     begin
--        if Arr = null then
--           return (1 .. 0 => (0, 0));
--        else
--           declare
--              Result : Gdk_Points_Array (1 .. Natural (N));
--           begin
--              for R in 0 .. Natural (N - 1) loop
--                 Result (R + 1) := Arr (R);
--              end loop;
--              return Result;
--           end;
--        end if;
--     end To_Point_Array;

   -------------------
   -- To_Atom_Array --
   -------------------

--     function To_Atom_Array
--       (Arr : Unbounded_Atom_Array_Access; N : Glib.Gint)
--        return Gdk.Types.Gdk_Atom_Array
--     is
--     begin
--        if Arr = null then
--           return (1 .. 0 => Gdk.Types.Gdk_None);
--        else
--           declare
--              Result : Gdk_Atom_Array (1 .. Natural (N));
--           begin
--              for R in 0 .. Natural (N - 1) loop
--                 Result (R + 1) := Arr (R);
--              end loop;
--              return Result;
--           end;
--        end if;
--     end To_Atom_Array;

   --------------------
   -- To_Color_Array --
   --------------------

--     function To_Color_Array
--       (Arr : Unbounded_Color_Array_Access; N : Glib.Gint)
--        return Gdk.Color.Gdk_Color_Array
--     is
--     begin
--        if Arr = null then
--           return (1 .. 0 => Null_Color);
--        else
--           declare
--              Result : Gdk_Color_Array (1 .. Natural (N));
--           begin
--              for R in 0 .. Natural (N - 1) loop
--                 Result (R + 1) := Arr (R);
--              end loop;
--              return Result;
--           end;
--        end if;
--     end To_Color_Array;

   --------------------
   -- To_Pspec_Array --
   --------------------

--     function To_Pspec_Array
--       (Arr : Unbounded_Pspec_Array_Access; N : Glib.Gint)
--        return Glib.Param_Spec_Array
--     is
--     begin
--        if Arr = null then
--           return (1 .. 0 => null);
--        else
--           declare
--              Result : Param_Spec_Array (1 .. Natural (N));
--           begin
--              for R in 0 .. Natural (N - 1) loop
--                 Result (R + 1) := Arr (R);
--              end loop;
--              return Result;
--           end;
--        end if;
--     end To_Pspec_Array;

   ------------------------
   -- To_Signal_Id_Array --
   ------------------------

--     function To_Signal_Id_Array
--       (Arr : Unbounded_Signal_Id_Array_Access; N : Glib.Guint)
--        return Glib.Object.Signal_Id_Array
--     is
--     begin
--        if Arr = null then
--           return (1 .. 0 => Null_Signal_Id);
--        else
--           declare
--              Result : Signal_Id_Array (1 .. N);
--           begin
--              for R in 0 .. N - 1 loop
--                 Result (R + 1) := Arr (Natural (R));
--              end loop;
--              return Result;
--           end;
--        end if;
--     end To_Signal_Id_Array;

   --------------------
   -- To_GType_Array --
   --------------------

--     function To_GType_Array
--       (Arr : Unbounded_GType_Array_Access; N : Glib.Guint)
--        return Glib.GType_Array
--     is
--     begin
--        if Arr = null then
--           return (1 .. 0 => GType_Invalid);
--        else
--           declare
--              Result : GType_Array (1 .. N);
--           begin
--              for R in 0 .. N - 1 loop
--                 Result (R + 1) := Arr (Natural (R));
--              end loop;
--              return Result;
--           end;
--        end if;
--     end To_GType_Array;

end Gtkada.Bindings;
