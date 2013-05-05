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

--  This package is purely internal to GtkAda.
--  It was moved out of Gtkada.Bindings for elaboration circularity issues

with Ada.Unchecked_Conversion;
with System;

package Gtkada.C is

   -------------
   --  Arrays --
   -------------
   --  The following functions ease bindings: when a C function returns an
   --  array or a pointer to an array, it returns a C array, ie which doesn't
   --  contain bounds. The size of the array is generally reported separately.
   --  The following definitions are suitable for use internally in the
   --  binding, but should not, when possible, be made visble to the user,
   --  since they don't behave like usual Ada arrays ('Last is irrelevant for
   --  instance).
   --
   --  For instance, if a C function has the following profile:
   --      gint* get_sizes (GtkIconTheme* theme);  --  0 terminated array
   --  when the binding is:
   --      function Internal (Theme) return Unbounded_Gint_Array_Access;
   --  and you need to compute for yourself the number of elements, and
   --  return a Glib.Gint_Array to the user.
   --
   --  If the C function has the following profile:
   --      gboolean get_attach_points (theme, GdkPoint** p, gint* n);
   --  then the binding is:
   --      function Internal (Theme  : System.Address;
   --                         Points : out Unbounded_Points_Array_Access;
   --                         N      : access Gint) return Gboolean;
   --  and you do the following:
   --      R : aliased Unbounded_Points_Array_Access;
   --      N : aliased Gint;
   --      Tmp : constant Gboolean :=
   --         Internal (.., R'Unchecked_Access, N'Unchecked_Access);
   --      Result : Gdk_Points_Array (1 .. Natural (N)) :=
   --         To_Gint_Array (R, Natural (N));
   --      Free (R);
   --      return Result;

   generic
      type T is private;
      Null_T : T;
      type Index is (<>);
      type T_Array is array (Index range <>) of T;
   package Unbounded_Arrays is
      type Unbounded_Array
        is array (Index range Index'Val (1) .. Index'Last) of T;
      pragma Convention (C, Unbounded_Array);
      type Unbounded_Array_Access is access Unbounded_Array;

      procedure G_Free (Arr : Unbounded_Array_Access);

      function To_Array
        (Arr : Unbounded_Array_Access; N : Index) return T_Array;

      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Unbounded_Array_Access);

   private
      pragma Import (C, G_Free, "g_free");
   end Unbounded_Arrays;

end Gtkada.C;
