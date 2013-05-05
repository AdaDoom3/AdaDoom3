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

with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk; use Gtk;
with System;
with Ada.Unchecked_Conversion;

with Glib.Type_Conversion_Hooks;

package body Gtk.Tree_Store is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Tree_Store_Record);
   pragma Warnings (Off, Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Tree_Store : out Gtk_Tree_Store;
      Types      : GType_Array) is
   begin
      Tree_Store := new Gtk_Tree_Store_Record;
      Initialize (Tree_Store, Types);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Tree_Store : access Gtk_Tree_Store_Record'Class;
      Types      : GType_Array)
   is
      function Internal
        (N_Columns : Gint;
         Types     : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_store_newv");

   begin
      Set_Object (Tree_Store, Internal (Types'Length, Types'Address));
   end Initialize;

   -----------------
   -- Generic_Set --
   -----------------

   package body Generic_Set is

      pragma Warnings (Off);
      --  Disable warnings on strict aliasing, since there's no aliasing
      --  issue here.

      function To_Address is new
        Ada.Unchecked_Conversion (Data_Type_Access, System.Address);

      function To_Access is new
        Ada.Unchecked_Conversion (System.Address, Data_Type_Access);

      pragma Warnings (On);

      procedure Set
        (Tree_Store : access Gtk_Tree_Store_Record'Class;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
         Column     : Gint;
         Value      : Data_Type_Access)
      is
         procedure Internal
           (Tree_Store : System.Address;
            Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
            Column     : Gint;
            Value      : System.Address);
         pragma Import (C, Internal, "ada_gtk_tree_store_set_ptr");

      begin
         Internal
           (Get_Object (Tree_Store), Iter, Column, To_Address (Value));
      end Set;

      function Get
        (Tree_Store : access Gtk_Tree_Store_Record'Class;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
         Column     : Gint) return Data_Type_Access
      is
         procedure Internal
           (Tree_Store : System.Address;
            Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
            Column     : Gint;
            Value      : out System.Address);
         pragma Import (C, Internal, "ada_gtk_tree_model_get_ptr");

         Value : System.Address;
      begin
         Internal
           (Get_Object (Tree_Store), Iter, Column, Value);
         return To_Access (Value);
      end Get;

   end Generic_Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Address    : System.Address)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
         Column     : Gint;
         Value      : System.Address);
      pragma Import (C, Internal, "ada_gtk_tree_store_set_ptr");

   begin
      Internal (Get_Object (Tree_Store), Iter, Column, Address);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : UTF8_String)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
         Column     : Gint;
         Value      : UTF8_String);
      pragma Import (C, Internal, "ada_gtk_tree_store_set_ptr");

   begin
      Internal (Get_Object (Tree_Store), Iter, Column, Value & ASCII.NUL);
   end Set;

   procedure Set
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : Gint)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
         Column     : Gint;
         Value      : Gint);
      pragma Import (C, Internal, "ada_gtk_tree_store_set_int");

   begin
      Internal (Get_Object (Tree_Store), Iter, Column, Value);
   end Set;

   procedure Set
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : Boolean)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
         Column     : Gint;
         Value      : Gboolean);
      pragma Import (C, Internal, "ada_gtk_tree_store_set_int");

   begin
      Internal (Get_Object (Tree_Store), Iter, Column, Boolean'Pos (Value));
   end Set;

   procedure Set
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : Glib.C_Proxy)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
         Column     : Gint;
         Value      : Glib.C_Proxy);
      pragma Import (C, Internal, "ada_gtk_tree_store_set_ptr");

   begin
      Internal (Get_Object (Tree_Store), Iter, Column, Value);
   end Set;

   procedure Set
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : Glib.Object.GObject)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
         Column     : Gint;
         Value      : System.Address);
      pragma Import (C, Internal, "ada_gtk_tree_store_set_ptr");

   begin
      if Value = null then
         Internal (Get_Object (Tree_Store), Iter, Column, System.Null_Address);

      else
         Internal (Get_Object (Tree_Store), Iter, Column, Get_Object (Value));
      end if;
   end Set;

   ----------------------
   -- Set_Column_Types --
   ----------------------

   procedure Set_Column_Types
     (Tree_Store : access Gtk_Tree_Store_Record;
      Types      : GType_Array)
   is
      procedure Internal
        (Tree_Store : System.Address;
         N_Columns  : Gint;
         Types      : System.Address);
      pragma Import (C, Internal, "gtk_tree_store_set_column_types");

   begin
      Internal (Get_Object (Tree_Store), Types'Length, Types'Address);
   end Set_Column_Types;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : Glib.Values.GValue)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
         Column     : Gint;
         Value      : Glib.Values.GValue);
      pragma Import (C, Internal, "gtk_tree_store_set_value");

   begin
      Internal (Get_Object (Tree_Store), Iter, Column, Value);
   end Set_Value;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter);
      pragma Import (C, Internal, "gtk_tree_store_remove");

   begin
      Internal (Get_Object (Tree_Store), Iter);
      Iter := Gtk.Tree_Model.Null_Iter;
   end Remove;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter;
      Parent     : Gtk.Tree_Model.Gtk_Tree_Iter;
      Position   : Gint)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter;
         Parent     : System.Address;
         Position   : Gint);
      pragma Import (C, Internal, "gtk_tree_store_insert");

   begin
      if Parent = Null_Iter then
         Internal
           (Get_Object (Tree_Store), Iter, System.Null_Address, Position);
      else
         Internal (Get_Object (Tree_Store), Iter, Parent'Address, Position);
      end if;
   end Insert;

   -------------------
   -- Insert_Before --
   -------------------

   procedure Insert_Before
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter;
      Parent     : Gtk.Tree_Model.Gtk_Tree_Iter;
      Sibling    : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter;
         Parent     : System.Address;
         Sibling    : System.Address);
      pragma Import (C, Internal, "gtk_tree_store_insert_before");

      P, S : System.Address := System.Null_Address;

   begin
      if Parent /= Null_Iter then
         P := Parent'Address;
      end if;

      if Sibling /= Null_Iter then
         S := Sibling'Address;
      end if;

      Internal (Get_Object (Tree_Store), Iter, P, S);
   end Insert_Before;

   ------------------
   -- Insert_After --
   ------------------

   procedure Insert_After
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter;
      Parent     : Gtk.Tree_Model.Gtk_Tree_Iter;
      Sibling    : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter;
         Parent     : System.Address;
         Sibling    : System.Address);
      pragma Import (C, Internal, "gtk_tree_store_insert_after");

      P, S : System.Address := System.Null_Address;

   begin
      if Parent /= Null_Iter then
         P := Parent'Address;
      end if;

      if Sibling /= Null_Iter then
         S := Sibling'Address;
      end if;

      Internal (Get_Object (Tree_Store), Iter, P, S);
   end Insert_After;

   -------------
   -- Prepend --
   -------------

   procedure Prepend
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter;
      Parent     : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter;
         Parent     : System.Address);
      pragma Import (C, Internal, "gtk_tree_store_prepend");

   begin
      if Parent = Null_Iter then
         Internal (Get_Object (Tree_Store), Iter, System.Null_Address);
      else
         Internal (Get_Object (Tree_Store), Iter, Parent'Address);
      end if;
   end Prepend;

   ------------
   -- Append --
   ------------

   procedure Append
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter;
      Parent     : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter;
         Parent     : System.Address);
      pragma Import (C, Internal, "gtk_tree_store_append");

   begin
      if Parent = Null_Iter then
         Internal (Get_Object (Tree_Store), Iter, System.Null_Address);
      else
         Internal (Get_Object (Tree_Store), Iter, Parent'Address);
      end if;
   end Append;

   -----------------
   -- Is_Ancestor --
   -----------------

   function Is_Ancestor
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Descendant : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is
      function Internal
        (Tree_Store : System.Address;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
         Descendant : Gtk.Tree_Model.Gtk_Tree_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_tree_store_is_ancestor");

   begin
      return Internal (Get_Object (Tree_Store), Iter, Descendant) /= 0;
   end Is_Ancestor;

   ----------------
   -- Iter_Depth --
   ----------------

   function Iter_Depth
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter) return Gint
   is
      function Internal
        (Tree_Store : System.Address;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter) return Gint;
      pragma Import (C, Internal, "gtk_tree_store_iter_depth");

   begin
      return Internal (Get_Object (Tree_Store), Iter);
   end Iter_Depth;

   -----------
   -- Clear --
   -----------

   procedure Clear (Tree_Store : access Gtk_Tree_Store_Record) is
      procedure Internal (Tree_Store : System.Address);
      pragma Import (C, Internal, "gtk_tree_store_clear");

   begin
      Internal (Get_Object (Tree_Store));
   end Clear;

   -----------------
   -- Freeze_Sort --
   -----------------

   function Freeze_Sort
     (Tree : access Gtk.Tree_Store.Gtk_Tree_Store_Record'Class)
      return Gint
   is
      function Internal (Tree : System.Address) return Gint;
      pragma Import (C, Internal, "ada_gtk_tree_view_freeze_sort");
   begin
      return Internal (Get_Object (Tree));
   end Freeze_Sort;

   ---------------
   -- Thaw_Sort --
   ---------------

   procedure Thaw_Sort
     (Tree : access Gtk.Tree_Store.Gtk_Tree_Store_Record'Class;
      Column_Id : Gint)
   is
      procedure Internal (Tree : System.Address; Id : Gint);
      pragma Import (C, Internal, "ada_gtk_tree_view_thaw_sort");
   begin
      Internal (Get_Object (Tree), Column_Id);
   end Thaw_Sort;

   -------------------
   -- Iter_Is_Valid --
   -------------------

   function Iter_Is_Valid
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk_Tree_Iter)
      return Boolean
   is
      function Internal
        (Tree_Store : System.Address;
         Iter       : Gtk_Tree_Iter)
         return Gboolean;
      pragma Import (C, Internal, "gtk_tree_store_iter_is_valid");
   begin
      return Boolean'Val (Internal (Get_Object (Tree_Store), Iter));
   end Iter_Is_Valid;

   ----------------
   -- Move_After --
   ----------------

   procedure Move_After
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk_Tree_Iter;
      Position   : Gtk_Tree_Iter)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Iter       : Gtk_Tree_Iter;
         Position   : Gtk_Tree_Iter);
      pragma Import (C, Internal, "gtk_tree_store_move_after");
   begin
      Internal (Get_Object (Tree_Store), Iter, Position);
   end Move_After;

   -----------------
   -- Move_Before --
   -----------------

   procedure Move_Before
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk_Tree_Iter;
      Position   : Gtk_Tree_Iter)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Iter       : Gtk_Tree_Iter;
         Position   : Gtk_Tree_Iter);
      pragma Import (C, Internal, "gtk_tree_store_move_before");
   begin
      Internal (Get_Object (Tree_Store), Iter, Position);
   end Move_Before;

   -------------
   -- Reorder --
   -------------

   procedure Reorder
     (Tree_Store : access Gtk_Tree_Store_Record;
      Parent     : Gtk_Tree_Iter;
      New_Order  : Gint_Array)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Parent     : System.Address;
         New_Order  : System.Address);
      pragma Import (C, Internal, "gtk_tree_store_reorder");
   begin
      if Parent = Null_Iter then
         Internal (Get_Object (Tree_Store), System.Null_Address,
                   New_Order (New_Order'First)'Address);
      else
         Internal (Get_Object (Tree_Store), Parent'Address,
                   New_Order (New_Order'First)'Address);
      end if;
   end Reorder;

   ----------
   -- Swap --
   ----------

   procedure Swap
     (Tree_Store : access Gtk_Tree_Store_Record;
      A          : Gtk_Tree_Iter;
      B          : Gtk_Tree_Iter)
   is
      procedure Internal
        (Tree_Store : System.Address;
         A          : Gtk_Tree_Iter;
         B          : Gtk_Tree_Iter);
      pragma Import (C, Internal, "gtk_tree_store_swap");
   begin
      Internal (Get_Object (Tree_Store), A, B);
   end Swap;

end Gtk.Tree_Store;
