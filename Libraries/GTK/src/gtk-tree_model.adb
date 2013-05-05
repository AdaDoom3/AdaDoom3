-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                 Copyright (C) 2001-2013, AdaCore                  --
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

with Interfaces.C.Strings;  use Interfaces.C.Strings;
with System;                use System;
with Gtk;                   use Gtk;
with Gtkada.Types;

with Glib.Type_Conversion_Hooks;

package body Gtk.Tree_Model is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Tree_Model_Record);
   pragma Warnings (Off, Type_Conversion);

   ---------
   -- "=" --
   ---------

   function "=" (Left : Gtk_Tree_Iter; Right : Gtk_Tree_Iter) return Boolean is
   begin
      if Left.Stamp = Right.Stamp then
         if Left.Stamp = 0 then
            --  Stamp = 0 means the iterator is null iterator, we need not to
            --  compare other fields in this case.

            return True;

         else
            return Left.User_Data = Right.User_Data
                     and then Left.User_Data2 = Right.User_Data2
                     and then Left.User_Data3 = Right.User_Data3;
         end if;

      else
         return False;
      end if;
   end "=";

   ---------------
   -- Get_Flags --
   ---------------

   function Get_Flags
     (Model : access Gtk_Tree_Model_Record) return Tree_Model_Flags
   is
      function Internal (M : System.Address) return Tree_Model_Flags;
      pragma Import (C, Internal, "gtk_tree_model_get_flags");
   begin
      return Internal (Get_Object (Model));
   end Get_Flags;

   -------------
   -- Gtk_New --
   -------------

   function Gtk_New (Path : String := "") return Gtk_Tree_Path is
      function Internal1 (Path : String) return Gtk_Tree_Path;
      pragma Import (C, Internal1, "gtk_tree_path_new_from_string");

      function Internal2 return Gtk_Tree_Path;
      pragma Import (C, Internal2, "gtk_tree_path_new");

   begin
      if Path = "" then
         return Internal2;
      else
         return Internal1 (Path & ASCII.NUL);
      end if;
   end Gtk_New;

   ---------------
   -- To_String --
   ---------------

   function To_String (Path : Gtk_Tree_Path) return String   is
      function Internal (Path   : Gtk_Tree_Path) return chars_ptr;
      pragma Import (C, Internal, "gtk_tree_path_to_string");

      Result : chars_ptr;
   begin
      Result := Internal (Path);

      declare
         S : constant String := Interfaces.C.Strings.Value (Result);
      begin
         Free (Result);
         return S;
      end;
   end To_String;

   -------------------
   -- Get_Tree_Path --
   -------------------

   function Get_Tree_Path (Val : Glib.Values.GValue) return Gtk_Tree_Path is

      function To_Gtk_Tree_Path is
        new Ada.Unchecked_Conversion (System.Address, Gtk_Tree_Path);

   begin
      return To_Gtk_Tree_Path (Glib.Values.Get_Address (Val));
   end Get_Tree_Path;

   -----------------
   -- Get_Indices --
   -----------------

   function Get_Indices (Path : Gtk_Tree_Path) return Gint_Array is
      Depth : constant Integer := Integer (Get_Depth (Path));

      subtype Result_Array is Gint_Array (0 .. Depth - 1);
      type Result_Array_Access is access all Result_Array;
      pragma Convention (C, Result_Array_Access);

      function Internal (Path : Gtk_Tree_Path) return Result_Array_Access;
      pragma Import (C, Internal, "gtk_tree_path_get_indices");

   begin
      --  Do not free the result of gtk_tree_path_get_indices since this is
      --  not a copy, but the currently used data.

      return Internal (Path).all;
   end Get_Indices;

   ----------
   -- Prev --
   ----------

   function Prev (Path : Gtk_Tree_Path) return Boolean is
      function Internal (Path : Gtk_Tree_Path) return Gint;
      pragma Import (C, Internal, "gtk_tree_path_prev");
   begin
      return Internal (Path) /= 0;
   end Prev;

   --------
   -- Up --
   --------

   function Up (Path : Gtk_Tree_Path) return Boolean is
      function Internal (Path : Gtk_Tree_Path) return Gint;
      pragma Import (C, Internal, "gtk_tree_path_up");
   begin
      return Internal (Path) /= 0;
   end Up;

   -----------------
   -- Is_Ancestor --
   -----------------

   function Is_Ancestor (Path, Descendant : Gtk_Tree_Path) return Boolean is
      function Internal (Path, Descendant : Gtk_Tree_Path) return Gint;
      pragma Import (C, Internal, "gtk_tree_path_is_ancestor");
   begin
      return Internal (Path, Descendant) /= 0;
   end Is_Ancestor;

   -------------------
   -- Is_Descendant --
   -------------------

   function Is_Descendant (Path, Ancestor : Gtk_Tree_Path) return Boolean is
      function Internal (Path, Ancestor : Gtk_Tree_Path) return Gint;
      pragma Import (C, Internal, "gtk_tree_path_is_descendant");
   begin
      return Internal (Path, Ancestor) /= 0;
   end Is_Descendant;

   -------------
   -- Gtk_New --
   -------------

   function Gtk_New
     (Model : access Gtk_Tree_Model_Record;  Path : Gtk_Tree_Path)
      return Gtk_Tree_Row_Reference
   is
      function Internal
        (Model  : System.Address; Path : Gtk_Tree_Path)
         return Gtk_Tree_Row_Reference;
      pragma Import (C, Internal, "gtk_tree_row_reference_new");
   begin
      return Internal (Get_Object (Model), Path);
   end Gtk_New;

   -----------
   -- Valid --
   -----------

   function Valid (Reference : Gtk_Tree_Row_Reference) return Boolean is
      function Internal (Reference : Gtk_Tree_Row_Reference) return Gint;
      pragma Import (C, Internal, "gtk_tree_row_reference_valid");
   begin
      return Internal (Reference) /= 0;
   end Valid;

   -------------------
   -- Get_N_Columns --
   -------------------

   function Get_N_Columns
     (Tree_Model : access Gtk_Tree_Model_Record) return Gint
   is
      function Internal (Tree_Model : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_tree_model_get_n_columns");
   begin
      return Internal (Get_Object (Tree_Model));
   end Get_N_Columns;

   ---------------------
   -- Get_Column_Type --
   ---------------------

   function Get_Column_Type
     (Tree_Model : access Gtk_Tree_Model_Record; Index : Gint) return GType
   is
      function Internal (Tree_Model : System.Address; Index : Gint)
         return GType;
      pragma Import (C, Internal, "gtk_tree_model_get_column_type");
   begin
      return Internal (Get_Object (Tree_Model), Index);
   end Get_Column_Type;

   -------------------
   -- Get_Tree_Iter --
   -------------------

   procedure Get_Tree_Iter
     (Val  : Glib.Values.GValue;
      Iter : out Gtk_Tree_Iter)
   is
      procedure Internal (Source : System.Address; Dest : out Gtk_Tree_Iter);
      pragma Import (C, Internal, "ada_tree_iter_copy");

   begin
      Internal (Glib.Values.Get_Address (Val), Iter);
   end Get_Tree_Iter;

   -------------------
   -- Get_Tree_Iter --
   -------------------

   function Get_Tree_Iter (Val : Glib.Values.GValue) return Gtk_Tree_Iter is
      Result : Gtk_Tree_Iter;
   begin
      Get_Tree_Iter (Val, Result);
      return Result;
   end Get_Tree_Iter;

   ----------------
   -- To_Address --
   ----------------

   function To_Address (Iter : Gtk_Tree_Iter) return System.Address is
   begin
      return Iter'Address;
   end To_Address;

   --------------
   -- Get_Iter --
   --------------

   function Get_Iter
     (Tree_Model : access Gtk_Tree_Model_Record;
      Path       : Gtk_Tree_Path) return Gtk_Tree_Iter
   is
      function Internal
        (Tree_Model, Iter : System.Address; Path : Gtk_Tree_Path) return Gint;
      pragma Import (C, Internal, "gtk_tree_model_get_iter");

      Iter : aliased Gtk_Tree_Iter;

   begin
      if Internal (Get_Object (Tree_Model), Iter'Address, Path) /= 0 then
         return Iter;
      else
         return Null_Iter;
      end if;
   end Get_Iter;

   --------------------------
   -- Get_Iter_From_String --
   --------------------------

   function Get_Iter_From_String
     (Tree_Model : access Gtk_Tree_Model_Record; Path_String : String)
      return Gtk_Tree_Iter
   is
      function Internal
        (Tree_Model, Iter : System.Address; Str : String) return Gint;
      pragma Import (C, Internal, "gtk_tree_model_get_iter_from_string");

      Iter : aliased Gtk_Tree_Iter;

   begin
      if Internal
        (Get_Object (Tree_Model), Iter'Address, Path_String & ASCII.NUL) /= 0
      then
         return Iter;
      else
         return Null_Iter;
      end if;
   end Get_Iter_From_String;

   --------------------
   -- Get_Iter_First --
   --------------------

   function Get_Iter_First
     (Tree_Model : access Gtk_Tree_Model_Record) return Gtk_Tree_Iter
   is
      function Internal (Tree_Model, Iter : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_tree_model_get_iter_first");

      Iter : aliased Gtk_Tree_Iter;

   begin
      if Internal (Get_Object (Tree_Model), Iter'Address) /= 0 then
         return Iter;
      else
         return Null_Iter;
      end if;
   end Get_Iter_First;

   --------------
   -- Get_Path --
   --------------

   function Get_Path
     (Reference : Gtk_Tree_Row_Reference) return Gtk_Tree_Path
   is
      function Internal
        (Reference : Gtk_Tree_Row_Reference) return Gtk_Tree_Path;
      pragma Import (C, Internal, "gtk_tree_row_reference_get_path");

   begin
      return Internal (Reference);
   end Get_Path;

   function Get_Path
     (Tree_Model : access Gtk_Tree_Model_Record; Iter : Gtk_Tree_Iter)
      return Gtk_Tree_Path
   is
      function Internal
        (Tree_Model : System.Address; Iter : Gtk_Tree_Iter)
         return Gtk_Tree_Path;
      pragma Import (C, Internal, "gtk_tree_model_get_path");

   begin
      return Internal (Get_Object (Tree_Model), Iter);
   end Get_Path;

   ---------------
   -- Get_Value --
   ---------------

   procedure Get_Value
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record;
      Iter       : Gtk_Tree_Iter;
      Column     : Gint;
      Value      : out Glib.Values.GValue)
   is
      procedure Internal
        (Tree_Model : System.Address;
         Iter       : Gtk_Tree_Iter;
         Column     : Gint;
         Value      : out Glib.Values.GValue);
      pragma Import (C, Internal, "gtk_tree_model_get_value");
      --  Windows used to crash when calling this directly. It might be because
      --  we didn't have the right version of gtk+. Better to bind directly,
      --  though, in case gtk+ is changed.

   begin
      Internal (Get_Object (Tree_Model), Iter, Column, Value);
   end Get_Value;

   ----------
   -- Next --
   ----------

   procedure Next (Path : Gtk_Tree_Path) is
      procedure Internal (Path : Gtk_Tree_Path);
      pragma Import (C, Internal, "gtk_tree_path_next");

   begin
      Internal (Path);
   end Next;

   procedure Next
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record;
      Iter       : in out Gtk_Tree_Iter)
   is
      function Internal (Tree_Model, Iter : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_tree_model_iter_next");

      Local_Iter : aliased Gtk_Tree_Iter := Iter;

   begin
      if Internal (Get_Object (Tree_Model), Local_Iter'Address) = 0 then
         Iter := Null_Iter;
      else
         Iter := Local_Iter;
      end if;
   end Next;

   --------------
   -- Children --
   --------------

   function Children
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record;
      Parent     : Gtk_Tree_Iter) return Gtk_Tree_Iter
   is
      function Internal
        (Tree_Model : System.Address;
         Iter       : System.Address;
         Parent     : Gtk_Tree_Iter) return Gint;
      pragma Import (C, Internal, "gtk_tree_model_iter_children");

      Iter : aliased Gtk_Tree_Iter;

   begin
      if Internal (Get_Object (Tree_Model), Iter'Address, Parent) /= 0 then
         return Iter;
      else
         return Null_Iter;
      end if;
   end Children;

   ---------------
   -- Has_Child --
   ---------------

   function Has_Child
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record;
      Iter       : Gtk_Tree_Iter) return Boolean
   is
      function Internal
        (Tree_Model : System.Address; Iter : Gtk_Tree_Iter) return Gint;
      pragma Import (C, Internal, "gtk_tree_model_iter_has_child");

   begin
      return Internal (Get_Object (Tree_Model), Iter) /= 0;
   end Has_Child;

   ----------------
   -- N_Children --
   ----------------

   function N_Children
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record;
      Iter       : Gtk_Tree_Iter := Null_Iter) return Gint
   is
      function Internal (Tree_Model, Iter : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_tree_model_iter_n_children");

   begin
      if Iter = Null_Iter then
         return Internal (Get_Object (Tree_Model), System.Null_Address);
      else
         return Internal (Get_Object (Tree_Model), Iter'Address);
      end if;
   end N_Children;

   ---------------
   -- Nth_Child --
   ---------------

   function Nth_Child
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record;
      Parent     : Gtk_Tree_Iter;
      N          : Gint) return Gtk_Tree_Iter
   is
      function Internal
        (Tree_Model, Iter, Parent : System.Address; N : Gint) return Gint;
      pragma Import (C, Internal, "gtk_tree_model_iter_nth_child");

      Iter : aliased Gtk_Tree_Iter;
      P    : System.Address := System.Null_Address;

   begin
      if Parent /= Null_Iter then
         P := Parent'Address;
      end if;

      if Internal (Get_Object (Tree_Model), Iter'Address, P, N) /= 0 then
         return Iter;
      else
         return Null_Iter;
      end if;
   end Nth_Child;

   ------------
   -- Parent --
   ------------

   function Parent
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record;
      Child      : Gtk_Tree_Iter) return Gtk_Tree_Iter
   is
      function Internal
        (Tree_Model : System.Address;
         Iter       : System.Address;
         Child      : Gtk_Tree_Iter) return Gint;
      pragma Import (C, Internal, "gtk_tree_model_iter_parent");

      Iter : aliased Gtk_Tree_Iter;

   begin
      if Internal (Get_Object (Tree_Model), Iter'Address, Child) /= 0 then
         return Iter;
      else
         return Null_Iter;
      end if;
   end Parent;

   --------------
   -- Ref_Node --
   --------------

   procedure Ref_Node
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record;
      Iter       : Gtk_Tree_Iter)
   is
      procedure Internal
        (Tree_Model : System.Address;
         Iter       : Gtk_Tree_Iter);
      pragma Import (C, Internal, "gtk_tree_model_ref_node");

   begin
      Internal (Get_Object (Tree_Model), Iter);
   end Ref_Node;

   ----------------
   -- Unref_Node --
   ----------------

   procedure Unref_Node
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record;
      Iter       : Gtk_Tree_Iter)
   is
      procedure Internal
        (Tree_Model : System.Address;
         Iter       : Gtk_Tree_Iter);
      pragma Import (C, Internal, "gtk_tree_model_unref_node");

   begin
      Internal (Get_Object (Tree_Model), Iter);
   end Unref_Node;

   -------------
   -- Get_Int --
   -------------

   function Get_Int
     (Tree_Model : access Gtk_Tree_Model_Record;
      Iter       : Gtk_Tree_Iter;
      Column     : Gint) return Gint
   is
      procedure Internal
        (Tree_Model : System.Address;
         Iter       : Gtk_Tree_Iter;
         Column     : Gint;
         Value      : out Gint);
      pragma Import (C, Internal, "ada_gtk_tree_model_get_int");

      A : Gint;

   begin
      Internal (Get_Object (Tree_Model), Iter, Column, A);
      return A;
   end Get_Int;

   -----------------
   -- Get_Boolean --
   -----------------

   function Get_Boolean
     (Tree_Model : access Gtk_Tree_Model_Record;
      Iter       : Gtk_Tree_Iter;
      Column     : Gint) return Boolean
   is
      procedure Internal
        (Tree_Model : System.Address;
         Iter       : Gtk_Tree_Iter;
         Column     : Gint;
         Value      : out Gboolean);
      pragma Import (C, Internal, "ada_gtk_tree_model_get_int");

      B : Gboolean;

   begin
      Internal (Get_Object (Tree_Model), Iter, Column, B);
      return B /= 0;
   end Get_Boolean;

   ----------------
   -- Get_Object --
   ----------------

   function Get_Object
     (Tree_Model : access Gtk_Tree_Model_Record;
      Iter       : Gtk_Tree_Iter;
      Column     : Gint) return Glib.Object.GObject
   is
      procedure Internal
        (Tree_Model : System.Address;
         Iter       : Gtk_Tree_Iter;
         Column     : Gint;
         Value      : out System.Address);
      pragma Import (C, Internal, "ada_gtk_tree_model_get_ptr");

      Value : System.Address;
      Stub  : Glib.Object.GObject_Record;

   begin
      Internal (Get_Object (Tree_Model), Iter, Column, Value);
      return Get_User_Data (Value, Stub);
   end Get_Object;

   -----------------
   -- Get_C_Proxy --
   -----------------

   function Get_C_Proxy
     (Tree_Model : access Gtk_Tree_Model_Record;
      Iter       : Gtk_Tree_Iter;
      Column     : Gint) return Glib.C_Proxy
   is
      procedure Internal
        (Tree_Model : System.Address;
         Iter       : Gtk_Tree_Iter;
         Column     : Gint;
         Value      : out Glib.C_Proxy);
      pragma Import (C, Internal, "ada_gtk_tree_model_get_ptr");

      Value : Glib.C_Proxy;
   begin
      Internal (Get_Object (Tree_Model), Iter, Column, Value);
      return Value;
   end Get_C_Proxy;

   -----------------
   -- Get_Address --
   -----------------

   function Get_Address
     (Tree_Model : access Gtk_Tree_Model_Record;
      Iter       : Gtk_Tree_Iter;
      Column     : Gint) return System.Address
   is
      procedure Internal
        (Tree_Model : System.Address;
         Iter       : Gtk_Tree_Iter;
         Column     : Gint;
         Value      : out System.Address);
      pragma Import (C, Internal, "ada_gtk_tree_model_get_ptr");

      Value : System.Address;
   begin
      Internal (Get_Object (Tree_Model), Iter, Column, Value);
      return Value;
   end Get_Address;

   ----------------
   -- Get_String --
   ----------------

   function Get_String
     (Tree_Model : access Gtk_Tree_Model_Record;
      Iter       : Gtk_Tree_Iter;
      Column     : Gint) return UTF8_String
   is
      procedure Internal
        (Tree_Model : System.Address;
         Iter       : Gtk_Tree_Iter;
         Column     : Gint;
         Value      : access chars_ptr);
      pragma Import (C, Internal, "ada_gtk_tree_model_get_ptr");

      A : aliased chars_ptr := Null_Ptr;

   begin
      Internal (Get_Object (Tree_Model), Iter, Column, A'Access);

      if A = Null_Ptr then
         return "";
      else
         declare
            S : constant String := Interfaces.C.Strings.Value (A);
         begin
            Gtkada.Types.g_free (A);
            return S;
         end;
      end if;
   end Get_String;

   -----------------
   -- Row_Changed --
   -----------------

   procedure Row_Changed
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Path       : Gtk_Tree_Path;
      Iter       : Gtk_Tree_Iter)
   is
      procedure Internal
        (Tree_Model : System.Address;
         Path       : Gtk_Tree_Path;
         Iter       : Gtk_Tree_Iter);
      pragma Import (C, Internal, "gtk_tree_model_row_changed");

   begin
      Internal (Get_Object (Tree_Model), Path, Iter);
   end Row_Changed;

   ------------------
   -- Row_Inserted --
   ------------------

   procedure Row_Inserted
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Path       : Gtk_Tree_Path;
      Iter       : Gtk_Tree_Iter)
   is
      procedure Internal
        (Tree_Model : System.Address;
         Path       : Gtk_Tree_Path;
         Iter       : Gtk_Tree_Iter);
      pragma Import (C, Internal, "gtk_tree_model_row_inserted");

   begin
      Internal (Get_Object (Tree_Model), Path, Iter);
   end Row_Inserted;

   ---------------------------
   -- Row_Has_Child_Toggled --
   ---------------------------

   procedure Row_Has_Child_Toggled
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Path       : Gtk_Tree_Path;
      Iter       : Gtk_Tree_Iter)
   is
      procedure Internal
        (Tree_Model : System.Address;
         Path       : Gtk_Tree_Path;
         Iter       : Gtk_Tree_Iter);
      pragma Import (C, Internal, "gtk_tree_model_row_has_child_toggled");

   begin
      Internal (Get_Object (Tree_Model), Path, Iter);
   end Row_Has_Child_Toggled;

   -----------------
   -- Row_Deleted --
   -----------------

   procedure Row_Deleted
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Path       : Gtk_Tree_Path)
   is
      procedure Internal
        (Tree_Model : System.Address;
         Path       : Gtk_Tree_Path);
      pragma Import (C, Internal, "gtk_tree_model_row_deleted");

   begin
      Internal (Get_Object (Tree_Model), Path);
   end Row_Deleted;

   --------------------
   -- Rows_Reordered --
   --------------------

   procedure Rows_Reordered
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Path       : Gtk_Tree_Path;
      Iter       : Gtk_Tree_Iter;
      New_Order  : Gint_Array)
   is
      procedure Internal
        (Tree_Model : System.Address;
         Path       : Gtk_Tree_Path;
         Iter       : Gtk_Tree_Iter;
         New_Order  : System.Address);
      pragma Import (C, Internal, "gtk_tree_model_rows_reordered");

   begin
      Internal (Get_Object (Tree_Model), Path, Iter, New_Order'Address);
   end Rows_Reordered;

   --------------------------
   -- Get_String_From_Iter --
   --------------------------

   function Get_String_From_Iter
     (Tree_Model : access Gtk_Tree_Model_Record;
      Iter       : Gtk_Tree_Iter)
      return String
   is
      function Internal
        (Tree_Model : System.Address;
         Iter       : Gtk_Tree_Iter)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_tree_model_get_string_from_iter");
      Str : chars_ptr := Internal (Get_Object (Tree_Model), Iter);
      Result : constant String := Value (Str);
   begin
      Free (Str);
      return Result;
   end Get_String_From_Iter;

   ---------------
   -- Get_Model --
   ---------------

   function Get_Model
     (Reference : Gtk_Tree_Row_Reference) return Gtk_Tree_Model
   is
      function Internal
        (Reference : Gtk_Tree_Row_Reference) return System.Address;
      pragma Import (C, Internal, "gtk_tree_row_reference_get_model");
      Stub : Gtk_Tree_Model_Record;
   begin
      return Gtk_Tree_Model (Get_User_Data (Internal (Reference), Stub));
   end Get_Model;

   -------------
   -- Foreach --
   -------------

   procedure Foreach
     (Model     : access Gtk_Tree_Model_Record;
      Func      : Gtk_Tree_Model_Foreach_Func;
      User_Data : System.Address)
   is
      type Foreach_Data is record
         Func      : Gtk_Tree_Model_Foreach_Func;
         User_Data : System.Address;
         Model     : Gtk_Tree_Model;
      end record;

      function Foreach_Proxy
        (C_Model : System.Address;
         Path    : Gtk_Tree_Path;
         Iter    : Gtk_Tree_Iter;
         Data    : access Foreach_Data) return Gboolean;
      pragma Convention (C, Foreach_Proxy);

      function Foreach_Proxy
        (C_Model : System.Address;
         Path    : Gtk_Tree_Path;
         Iter    : Gtk_Tree_Iter;
         Data    : access Foreach_Data) return Gboolean
      is
         pragma Unreferenced (C_Model);
      begin
         return Boolean'Pos
           (Data.Func (Data.Model, Path, Iter, Data.User_Data));
      end Foreach_Proxy;

      procedure Internal
        (Model     : System.Address;
         Func      : System.Address;
         User_Data : System.Address);
      pragma Import (C, Internal, "gtk_tree_model_foreach");

      Data : aliased Foreach_Data := (Func, User_Data, Model.all'Access);

   begin
      Internal (Get_Object (Model), Foreach_Proxy'Address, Data'Address);
   end Foreach;

end Gtk.Tree_Model;
