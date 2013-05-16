-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2006-2013, AdaCore                   --
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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Tree_Model;  use Gtk.Tree_Model;
with System;          use System;

package body Gtk.Tree_Sortable is

   type Gtk_Tree_Iter_Access is access all Gtk_Tree_Iter;
   function To_Iter is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Tree_Iter_Access);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Tree_Iter_Compare_Func, System.Address);

   function Compare_Func_Wrapper
     (Model, A, B : System.Address;
      Data : Gtk_Tree_Iter_Compare_Func) return Gint;
   pragma Convention (C, Compare_Func_Wrapper);
   --  Internal wrapper for a Gtk_Tree_Iter_Compare_Func

   ------------------------
   -- Get_Sort_Column_Id --
   ------------------------

   procedure Get_Sort_Column_Id
     (Sortable        : Gtk_Tree_Sortable;
      Sort_Column_Id  : out Gint;
      Order           : out Gtk.Enums.Gtk_Sort_Type)
   is
      function Internal
        (Sortable       : Gtk_Tree_Sortable;
         Sort_Column_Id : access Gint;
         Order          : access Gtk_Sort_Type)
         return Gboolean;
      pragma Import (C, Internal, "gtk_tree_sortable_get_sort_column_id");

      Id  : aliased Gint;
      Ord : aliased Gtk_Sort_Type;
      Tmp : constant Gboolean :=
        Internal (Sortable, Id'Access, Ord'Access);
      pragma Unreferenced (Tmp);
   begin
      Sort_Column_Id := Id;
      Order          := Ord;
   end Get_Sort_Column_Id;

   --------------------------
   -- Compare_Func_Wrapper --
   --------------------------

   function Compare_Func_Wrapper
     (Model, A, B : System.Address;
      Data        : Gtk_Tree_Iter_Compare_Func) return Gint
   is
      Stub : Gtk_Tree_Model_Record;
      AI : constant Gtk_Tree_Iter_Access := To_Iter (A);
      BI : constant Gtk_Tree_Iter_Access := To_Iter (B);

   begin
      return Data
        (Gtk_Tree_Model (Get_User_Data (Model, Stub)),
         AI.all, BI.all);
   end Compare_Func_Wrapper;

   ---------------------------
   -- Set_Default_Sort_Func --
   ---------------------------

   procedure Set_Default_Sort_Func
     (Sortable  : Gtk_Tree_Sortable;
      Sort_Func : Gtk_Tree_Iter_Compare_Func)
   is
      procedure Internal
        (Sortable  : Gtk_Tree_Sortable;
         Sort_Func : System.Address;
         User_Data : System.Address;
         Destroy   : System.Address := System.Null_Address);
      pragma Import (C, Internal, "gtk_tree_sortable_set_default_sort_func");
   begin
      Internal
        (Sortable, Compare_Func_Wrapper'Address, To_Address (Sort_Func));
   end Set_Default_Sort_Func;

   ---------------------------
   -- Has_Default_Sort_Func --
   ---------------------------

   function Has_Default_Sort_Func
     (Sortable : Gtk_Tree_Sortable) return Boolean
   is
      function Internal (Sortable : Gtk_Tree_Sortable) return Gboolean;
      pragma Import (C, Internal, "gtk_tree_sortable_has_default_sort_func");
   begin
      return Boolean'Val (Internal (Sortable));
   end Has_Default_Sort_Func;

   -------------------
   -- Set_Sort_Func --
   -------------------

   procedure Set_Sort_Func
     (Sortable       : Gtk_Tree_Sortable;
      Sort_Column_Id : Gint;
      Sort_Func      : Gtk_Tree_Iter_Compare_Func)
   is
      procedure Internal
        (Sortable       : Gtk_Tree_Sortable;
         Sort_Column_Id : Gint;
         Sort_Func      : System.Address;
         User_Data      : System.Address;
         Destroy        : System.Address := System.Null_Address);
      pragma Import (C, Internal, "gtk_tree_sortable_set_sort_func");
   begin
      Internal
        (Sortable, Sort_Column_Id,
         Compare_Func_Wrapper'Address,
         To_Address (Sort_Func));
   end Set_Sort_Func;

   -------------------
   -- Compare_Funcs --
   -------------------

   package body Compare_Funcs is

      procedure Free is new Ada.Unchecked_Deallocation
        (Data_Type, Data_Type_Access);
      procedure Free is new Ada.Unchecked_Deallocation
        (Data_Type_Record, Data_Type_Record_Access);

      ---------------------------
      -- Set_Default_Sort_Func --
      ---------------------------

      procedure Set_Default_Sort_Func
        (Sortable  : Gtk_Tree_Sortable;
         Sort_Func : Gtk_Tree_Iter_Compare_Func;
         User_Data : Data_Type;
         Destroy   : Destroy_Notify := null)
      is
         procedure Internal
           (Sortable  : Gtk_Tree_Sortable;
            Sort_Func : System.Address;
            User_Data : Data_Type_Record_Access;
            Destroy   : System.Address);
         pragma Import
           (C, Internal, "gtk_tree_sortable_set_default_sort_func");
      begin
         Internal
           (Sortable, Internal_Compare_Func'Address,
            new Data_Type_Record'
              (Func => Sort_Func,
               Destroy => Destroy,
               Data    => new Data_Type'(User_Data)),
            Internal_Destroy_Notify'Address);
      end Set_Default_Sort_Func;

      -------------------
      -- Set_Sort_Func --
      -------------------

      procedure Set_Sort_Func
        (Sortable       : Gtk_Tree_Sortable;
         Sort_Column_Id : Gint;
         Sort_Func      : Gtk_Tree_Iter_Compare_Func;
         User_Data      : Data_Type;
         Destroy        : Destroy_Notify := null)
      is
         procedure Internal
           (Sortable  : Gtk_Tree_Sortable;
            Sort_Column_Id : Gint;
            Sort_Func : System.Address;
            User_Data : Data_Type_Record_Access;
            Destroy   : System.Address);
         pragma Import
           (C, Internal, "gtk_tree_sortable_set_sort_func");
      begin
         Internal
           (Sortable, Sort_Column_Id, Internal_Compare_Func'Address,
            new Data_Type_Record'
              (Func => Sort_Func,
               Destroy => Destroy,
               Data    => new Data_Type'(User_Data)),
            Internal_Destroy_Notify'Address);
      end Set_Sort_Func;

      -----------------------------
      -- Internal_Destroy_Notify --
      -----------------------------

      procedure Internal_Destroy_Notify (Data : Data_Type_Record_Access) is
         D : Data_Type_Record_Access := Data;
      begin
         if Data.Destroy /= null then
            Data.Destroy (Data.Data.all);
         end if;
         Free (Data.Data);
         Free (D);
      end Internal_Destroy_Notify;

      ---------------------------
      -- Internal_Compare_Func --
      ---------------------------

      function Internal_Compare_Func
        (Model : System.Address;
         A, B  : System.Address;
         Data  : Data_Type_Record_Access) return Gint
      is
         Stub : Gtk_Tree_Model_Record;
         AI : constant Gtk_Tree_Iter_Access := To_Iter (A);
         BI : constant Gtk_Tree_Iter_Access := To_Iter (B);
      begin
         return Data.Func
           (Gtk_Tree_Model (Get_User_Data (Model, Stub)),
            AI.all, BI.all, Data.Data.all);
      end Internal_Compare_Func;

   end Compare_Funcs;

end Gtk.Tree_Sortable;
