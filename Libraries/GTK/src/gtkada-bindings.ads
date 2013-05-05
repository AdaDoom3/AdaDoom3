-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                 Copyright (C) 2006-2013, AdaCore                  --
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

--  This is a unit purely internal to GtkAda, to ease binding and avoid code
--  duplication.
--  Do not use in your own applications, since the interface might change from
--  release to release.
--  See also Gtkada.Types

--  with Gdk.Color;
--  with Gdk.Types;
with Glib;
--  with Glib.Object;
with Gtkada.C;
with GNAT.Strings;
with Interfaces.C.Strings;

package Gtkada.Bindings is
   package ICS renames Interfaces.C.Strings;

   -------------
   -- Strings --
   -------------

   function String_Or_Null (S : String) return ICS.chars_ptr;
   --  Return Null_Ptr if S is the empty string, or a newly allocated string
   --  otherwise. This is intended mostly for the binding itself.

   type chars_ptr_array_access
     is access ICS.chars_ptr_array (Interfaces.C.size_t);
   pragma Convention (C, chars_ptr_array_access);
   --  Suitable for a C function that returns a gchar**

   procedure g_strfreev (Str_Array : in out chars_ptr_array_access);
   --  Thin binding to C function of the same name.  Frees a null-terminated
   --  array of strings, and the array itself.  If called on a null value,
   --  simply return.

   function To_String_List
     (C : ICS.chars_ptr_array) return GNAT.Strings.String_List;
   --  Converts C into a String_List. Returned value must be freed by caller,
   --  as well as C. C is NULL terminated.

   function To_String_List
     (C : ICS.chars_ptr_array; N : Glib.Gint)
      return GNAT.Strings.String_List;
   --  Converts C into a String_List. N is the number of elements in C.
   --  Returned value must be freed by caller, as well as C.

   function From_String_List
     (C : GNAT.Strings.String_List) return ICS.chars_ptr_array;
   --  Converts C into a chars_ptr_array. Returned value must be freed by
   --  caller, as well as C.

   function To_Chars_Ptr
     (C : chars_ptr_array_access) return ICS.chars_ptr_array;
   --  Return a bounded array that contains the same strings as C (so you
   --  shouldn't free C). 'Last applies to the result, whereas it doesn't to C.

   ------------
   -- Arrays --
   ------------
   --  See Gtkada.C for more information.
   --  The packages that are commented out are instanciated in various,
   --  possibly duplicated places. This is because of elaboration circularity
   --  issues.

   package Gint_Arrays is new Gtkada.C.Unbounded_Arrays
     (Glib.Gint, 0, Natural, Glib.Gint_Array);
--     package Points_Arrays is new Gtkada.C.Unbounded_Arrays
--       (Gdk.Types.Gdk_Point, (0, 0), Positive, Gdk.Types.Gdk_Points_Array);
--     package Atom_Arrays is new Gtkada.C.Unbounded_Arrays
--       (Gdk.Types.Gdk_Atom, Gdk.Types.Gdk_None,
--        Natural, Gdk.Types.Gdk_Atom_Array);
   package Pspec_Arrays is new Gtkada.C.Unbounded_Arrays
     (Glib.Param_Spec, null, Natural, Glib.Param_Spec_Array);
--     package Signal_Id_Arrays is new Gtkada.C.Unbounded_Arrays
--       (Glib.Signal_Id, Glib.Null_Signal_Id, Glib.Guint,
--        Glib.Object.Signal_Id_Array);
   package GType_Arrays is new Gtkada.C.Unbounded_Arrays
     (Glib.GType, Glib.GType_None, Glib.Guint, Glib.GType_Array);
--     package Color_Arrays is new Gtkada.C.Unbounded_Arrays
--       (Gdk.Color.Gdk_Color, Gdk.Color.Null_Color, Natural,
--        Gdk.Color.Gdk_Color_Array);

--     type Unbounded_Gint_Array is array (Natural) of Glib.Gint;
--     pragma Convention (C, Unbounded_Gint_Array);
--     type Unbounded_Gint_Array_Access is access Unbounded_Gint_Array;
--     procedure G_Free (Arr : in out Unbounded_Gint_Array_Access);
--     function To_Gint_Array
--       (Arr : Unbounded_Gint_Array_Access; N : Glib.Gint)
--        return Glib.Gint_Array;
   function To_Gint_Array_Zero_Terminated
     (Arr : Gint_Arrays.Unbounded_Array_Access)
      return Glib.Gint_Array;
   --  Converts Arr, stopping at the first 0 encountered

--     type Unbounded_Points_Array is array (Natural) of Gdk.Types.Gdk_Point;
--     pragma Convention (C, Unbounded_Points_Array);
--     type Unbounded_Points_Array_Access is access Unbounded_Points_Array;
--     procedure G_Free (Arr : in out Unbounded_Points_Array_Access);
--     function To_Point_Array
--       (Arr : Unbounded_Points_Array_Access; N : Glib.Gint)
--        return Gdk.Types.Gdk_Points_Array;

--     type Unbounded_Atom_Array is array (Natural) of Gdk.Types.Gdk_Atom;
--     pragma Convention (C, Unbounded_Atom_Array);
--     type Unbounded_Atom_Array_Access is access Unbounded_Atom_Array;
--     procedure G_Free (Arr : in out Unbounded_Atom_Array_Access);
--     function To_Atom_Array
--       (Arr : Unbounded_Atom_Array_Access; N : Glib.Gint)
--        return Gdk.Types.Gdk_Atom_Array;

--     type Unbounded_Pspec_Array is array (Natural) of Glib.Param_Spec;
--     pragma Convention (C, Unbounded_Pspec_Array);
--     type Unbounded_Pspec_Array_Access is access Unbounded_Pspec_Array;
--     procedure G_Free (Arr : in out Unbounded_Pspec_Array_Access);
--     function To_Pspec_Array
--       (Arr : Unbounded_Pspec_Array_Access; N : Glib.Gint)
--        return Glib.Param_Spec_Array;

--     type Unbounded_Signal_Id_Array is array (Natural) of Glib.Signal_Id;
--     pragma Convention (C, Unbounded_Signal_Id_Array);
--   type Unbounded_Signal_Id_Array_Access is access Unbounded_Signal_Id_Array;
--     procedure G_Free (Arr : in out Unbounded_Signal_Id_Array_Access);
--     function To_Signal_Id_Array
--       (Arr : Unbounded_Signal_Id_Array_Access; N : Glib.Guint)
--        return Glib.Object.Signal_Id_Array;

--     type Unbounded_GType_Array is array (Natural) of Glib.GType;
--     pragma Convention (C, Unbounded_GType_Array);
--     type Unbounded_GType_Array_Access is access Unbounded_GType_Array;
--     procedure G_Free (Arr : in out Unbounded_GType_Array_Access);
--     function To_GType_Array
--       (Arr : Unbounded_GType_Array_Access; N : Glib.Guint)
--        return Glib.GType_Array;

--     type Unbounded_Color_Array is array (Natural) of Gdk.Color.Gdk_Color;
--     pragma Convention (C, Unbounded_Color_Array);
--     type Unbounded_Color_Array_Access is access Unbounded_Color_Array;
--     procedure G_Free (Arr : in out Unbounded_Color_Array_Access);
--     function To_Color_Array
--       (Arr : Unbounded_Color_Array_Access; N : Glib.Gint)
--        return Gdk.Color.Gdk_Color_Array;
--     function Convert is new Ada.Unchecked_Conversion
--       (System.Address, Unbounded_Color_Array_Access);

private
--   pragma Import (C, g_free, "g_free");
   pragma Import (C, g_strfreev, "g_strfreev");
end Gtkada.Bindings;
