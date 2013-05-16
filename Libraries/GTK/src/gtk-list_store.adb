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

with System;

with Gtk;            use Gtk;
with Gtk.Tree_Model; use Gtk.Tree_Model;

with Glib.Type_Conversion_Hooks;

package body Gtk.List_Store is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_List_Store_Record);
   pragma Warnings (Off, Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (List_Store : out Gtk_List_Store;
      Types      : GType_Array) is
   begin
      List_Store := new Gtk_List_Store_Record;
      Initialize (List_Store, Types);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (List_Store : access Gtk_List_Store_Record'Class;
      Types      : GType_Array)
   is
      function Internal
        (N_Columns : Gint; Types : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_list_store_newv");

   begin
      Set_Object (List_Store, Internal (Types'Length, Types'Address));
   end Initialize;

   ----------------------
   -- Set_Column_Types --
   ----------------------

   procedure Set_Column_Types
     (List_Store : access Gtk_List_Store_Record;
      Types      : GType_Array)
   is
      procedure Internal
        (List_Store : System.Address;
         N_Columns  : Gint;
         Types      : System.Address);
      pragma Import (C, Internal, "gtk_list_store_set_column_types");

   begin
      Internal (Get_Object (List_Store), Types'Length, Types'Address);
   end Set_Column_Types;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (List_Store : access Gtk_List_Store_Record;
      Iter       : Gtk_Tree_Iter;
      Column     : Gint;
      Value      : Glib.Values.GValue)
   is
      procedure Internal
        (List_Store : System.Address;
         Iter       : Gtk_Tree_Iter;
         Column     : Gint;
         Value      : Glib.Values.GValue);
      pragma Import (C, Internal, "gtk_list_store_set_value");

   begin
      Internal (Get_Object (List_Store), Iter, Column, Value);
   end Set_Value;

   ---------
   -- Set --
   ---------

   procedure Set
     (Tree_Store : access Gtk_List_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : UTF8_String)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
         Column     : Gint;
         Value      : UTF8_String);
      pragma Import (C, Internal, "ada_gtk_list_store_set_ptr");

   begin
      Internal (Get_Object (Tree_Store), Iter, Column, Value & ASCII.NUL);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Tree_Store : access Gtk_List_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : Gint)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
         Column     : Gint;
         Value      : Gint);
      pragma Import (C, Internal, "ada_gtk_list_store_set_int");

   begin
      Internal (Get_Object (Tree_Store), Iter, Column, Value);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Tree_Store : access Gtk_List_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : Boolean)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
         Column     : Gint;
         Value      : Gboolean);
      pragma Import (C, Internal, "ada_gtk_list_store_set_boolean");

   begin
      Internal (Get_Object (Tree_Store), Iter, Column, Boolean'Pos (Value));
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Tree_Store : access Gtk_List_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : Gdk.Pixbuf.Gdk_Pixbuf)
   is
      procedure Internal
        (Tree_Store : System.Address;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
         Column     : Gint;
         Value      : System.Address);
      pragma Import (C, Internal, "ada_gtk_list_store_set_pixbuf");

   begin
      Internal (Get_Object (Tree_Store), Iter, Column, Get_Object (Value));
   end Set;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (List_Store : access Gtk_List_Store_Record;
      Iter       : in out Gtk_Tree_Iter)
   is
      procedure Internal
        (List_Store : System.Address;
         Iter       : in out Gtk_Tree_Iter);
      pragma Import (C, Internal, "gtk_list_store_remove");

   begin
      Internal (Get_Object (List_Store), Iter);
   end Remove;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (List_Store : access Gtk_List_Store_Record;
      Iter       : in out Gtk_Tree_Iter;
      Position   : Gint)
   is
      procedure Internal
        (List_Store : System.Address;
         Iter       : in out Gtk_Tree_Iter;
         Position   : Gint);
      pragma Import (C, Internal, "gtk_list_store_insert");

   begin
      Internal (Get_Object (List_Store), Iter, Position);
   end Insert;

   -------------------
   -- Insert_Before --
   -------------------

   procedure Insert_Before
     (List_Store : access Gtk_List_Store_Record;
      Iter       : in out Gtk_Tree_Iter;
      Sibling    : Gtk_Tree_Iter)
   is
      procedure Internal
        (List_Store : System.Address;
         Iter       : in out Gtk_Tree_Iter;
         Sibling    : System.Address);
      pragma Import (C, Internal, "gtk_list_store_insert_before");

      Addr : System.Address := System.Null_Address;

   begin
      if Sibling /= Null_Iter then
         Addr := Sibling'Address;
      end if;

      Internal (Get_Object (List_Store), Iter, Addr);
   end Insert_Before;

   ------------------
   -- Insert_After --
   ------------------

   procedure Insert_After
     (List_Store : access Gtk_List_Store_Record;
      Iter       : in out Gtk_Tree_Iter;
      Sibling    : Gtk_Tree_Iter)
   is
      procedure Internal
        (List_Store : System.Address;
         Iter       : in out Gtk_Tree_Iter;
         Sibling    : System.Address);
      pragma Import (C, Internal, "gtk_list_store_insert_after");

      Addr : System.Address := System.Null_Address;

   begin
      if Sibling /= Null_Iter then
         Addr := Sibling'Address;
      end if;

      Internal (Get_Object (List_Store), Iter, Addr);
   end Insert_After;

   -------------
   -- Prepend --
   -------------

   procedure Prepend
     (List_Store : access Gtk_List_Store_Record;
      Iter       : in out Gtk_Tree_Iter)
   is
      procedure Internal
        (List_Store : System.Address;
         Iter       : in out Gtk_Tree_Iter);
      pragma Import (C, Internal, "gtk_list_store_prepend");

   begin
      Internal (Get_Object (List_Store), Iter);
   end Prepend;

   ------------
   -- Append --
   ------------

   procedure Append
     (List_Store : access Gtk_List_Store_Record;
      Iter       : in out Gtk_Tree_Iter)
   is
      procedure Internal
        (List_Store : System.Address;
         Iter       : in out Gtk_Tree_Iter);
      pragma Import (C, Internal, "gtk_list_store_append");

   begin
      Internal (Get_Object (List_Store), Iter);
   end Append;

   -----------
   -- Clear --
   -----------

   procedure Clear (List_Store : access Gtk_List_Store_Record) is
      procedure Internal (List_Store : System.Address);
      pragma Import (C, Internal, "gtk_list_store_clear");

   begin
      Internal (Get_Object (List_Store));
   end Clear;

   -------------------------
   -- Insert_With_Valuesv --
   -------------------------

   procedure Insert_With_Valuesv
     (List_Store : access Gtk_List_Store_Record;
      Iter       : in out Gtk_Tree_Iter;
      Position   : Gint;
      Columns    : Gint_Array;
      Values     : Glib.Values.GValue_Array)
   is
      procedure Internal
        (List_Store : System.Address;
         Iter       : in out Gtk_Tree_Iter;
         Position   : Gint;
         Columns    : System.Address;
         Values     : System.Address;
         N_Values   : Gint);
      pragma Import (C, Internal, "gtk_list_store_insert_with_valuesv");
   begin
      Internal (Get_Object (List_Store), Iter, Position,
                Columns (Columns'First)'Address, Values (Values'First)'Address,
                Values'Length);
   end Insert_With_Valuesv;

   -------------------
   -- Iter_Is_Valid --
   -------------------

   function Iter_Is_Valid
     (List_Store : access Gtk_List_Store_Record;
      Iter       : Gtk_Tree_Iter)
      return Boolean
   is
      function Internal
        (List_Store : System.Address;
         Iter       : Gtk_Tree_Iter)
         return Gboolean;
      pragma Import (C, Internal, "gtk_list_store_iter_is_valid");
   begin
      return Boolean'Val (Internal (Get_Object (List_Store), Iter));
   end Iter_Is_Valid;

   ----------------
   -- Move_After --
   ----------------

   procedure Move_After
     (Store    : access Gtk_List_Store_Record;
      Iter     : Gtk_Tree_Iter;
      Position : Gtk_Tree_Iter)
   is
      procedure Internal
        (Store    : System.Address;
         Iter     : Gtk_Tree_Iter;
         Position : Gtk_Tree_Iter);
      pragma Import (C, Internal, "gtk_list_store_move_after");
   begin
      Internal (Get_Object (Store), Iter, Position);
   end Move_After;

   -----------------
   -- Move_Before --
   -----------------

   procedure Move_Before
     (Store    : access Gtk_List_Store_Record;
      Iter     : Gtk_Tree_Iter;
      Position : Gtk_Tree_Iter)
   is
      procedure Internal
        (Store    : System.Address;
         Iter     : Gtk_Tree_Iter;
         Position : Gtk_Tree_Iter);
      pragma Import (C, Internal, "gtk_list_store_move_before");
   begin
      Internal (Get_Object (Store), Iter, Position);
   end Move_Before;

   -------------
   -- Reorder --
   -------------

   procedure Reorder
     (Store     : access Gtk_List_Store_Record;
      New_Order : Gint_Array)
   is
      procedure Internal
        (Store     : System.Address;
         New_Order : System.Address);
      pragma Import (C, Internal, "gtk_list_store_reorder");
   begin
      Internal (Get_Object (Store), New_Order (New_Order'First)'Address);
   end Reorder;

   ----------
   -- Swap --
   ----------

   procedure Swap
     (Store : access Gtk_List_Store_Record;
      A     : Gtk_Tree_Iter;
      B     : Gtk_Tree_Iter)
   is
      procedure Internal
        (Store : System.Address;
         A     : Gtk_Tree_Iter;
         B     : Gtk_Tree_Iter);
      pragma Import (C, Internal, "gtk_list_store_swap");
   begin
      Internal (Get_Object (Store), A, B);
   end Swap;

end Gtk.List_Store;
