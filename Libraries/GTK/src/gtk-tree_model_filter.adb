-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Glib.Values;      use Glib.Values;
with Gtk.Tree_Model;   use Gtk.Tree_Model;
with System;           use System;

with Glib.Type_Conversion_Hooks;

package body Gtk.Tree_Model_Filter is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Tree_Model_Filter_Record);
   pragma Warnings (Off, Type_Conversion);

   type Gtk_Tree_Iter_Access is access all Gtk_Tree_Iter;
   function To_Iter is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Tree_Iter_Access);

   function Visible_Func_Wrapper
     (Model, Iter : System.Address; Data : Gtk_Tree_Model_Filter_Visible_Func)
      return Gboolean;
   pragma Convention (C, Visible_Func_Wrapper);
   --  Internal wrapper for a Gtk_Tree_Model_Filter_Visible_Func.

   procedure Modify_Func_Wrapper
     (Model, Iter : System.Address;
      Value       : out Glib.Values.GValue;
      Column      : Gint;
      Data        : Gtk_Tree_Model_Filter_Modify_Func);
   pragma Convention (C, Modify_Func_Wrapper);
   --  Internal wrapper for a Gtk_Tree_Model_Filter_Modify_Func.

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Model       : out Gtk_Tree_Model_Filter;
      Child_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Root        : Gtk.Tree_Model.Gtk_Tree_Path := null)
   is
   begin
      Model := new Gtk_Tree_Model_Filter_Record;
      Initialize (Model, Child_Model, Root);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Model       : access Gtk_Tree_Model_Filter_Record'Class;
      Child_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Root        : Gtk.Tree_Model.Gtk_Tree_Path := null)
   is
      function Internal
        (Child_Model : System.Address;
         Root        : Gtk_Tree_Path) return System.Address;
      pragma Import (C, Internal, "gtk_tree_model_filter_new");
   begin
      Set_Object
        (Model, Internal (Get_Object (Child_Model), Root));
   end Initialize;

   ---------------
   -- Get_Model --
   ---------------

   function Get_Model
     (Filter : access Gtk_Tree_Model_Filter_Record)
      return Gtk.Tree_Model.Gtk_Tree_Model
   is
      function Internal (Filter : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_model_filter_get_model");
      Stub : Gtk_Tree_Model_Record;
   begin
      return Gtk_Tree_Model
        (Get_User_Data (Internal (Get_Object (Filter)), Stub));
   end Get_Model;

   --------------------------------
   -- Convert_Child_Iter_To_Iter --
   --------------------------------

   procedure Convert_Child_Iter_To_Iter
     (Filter      : access Gtk_Tree_Model_Filter_Record;
      Filter_Iter : out Gtk.Tree_Model.Gtk_Tree_Iter;
      Child_Iter  : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      pragma Warnings (Off, Filter_Iter);
      procedure Internal
        (Filter      : System.Address;
         Filter_Iter : Gtk_Tree_Iter;
         Child_Iter  : Gtk_Tree_Iter);
      pragma Import
        (C, Internal, "gtk_tree_model_filter_convert_child_iter_to_iter");
   begin
      Internal (Get_Object (Filter), Filter_Iter, Child_Iter);
   end Convert_Child_Iter_To_Iter;

   --------------------------------
   -- Convert_Child_Path_To_Path --
   --------------------------------

   function Convert_Child_Path_To_Path
     (Filter     : access Gtk_Tree_Model_Filter_Record;
      Child_Path : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Path
   is
      function Internal
        (TreeFilter : System.Address;
         Child_Path : Gtk_Tree_Path) return Gtk_Tree_Path;
      pragma Import (C, Internal,
                     "gtk_tree_model_filter_convert_child_path_to_path");
   begin
      return Internal (Get_Object (Filter), Child_Path);
   end Convert_Child_Path_To_Path;

   --------------------------------
   -- Convert_Iter_To_Child_Iter --
   --------------------------------

   procedure Convert_Iter_To_Child_Iter
     (Filter      : access Gtk_Tree_Model_Filter_Record;
      Child_Iter  : out Gtk.Tree_Model.Gtk_Tree_Iter;
      Filter_Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      pragma Warnings (Off, Child_Iter);
      procedure Internal
        (Filter      : System.Address;
         Child_Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
         Sorted_Iter : Gtk.Tree_Model.Gtk_Tree_Iter);
      pragma Import
        (C, Internal, "gtk_tree_model_filter_convert_iter_to_child_iter");
   begin
      Internal (Get_Object (Filter), Child_Iter, Filter_Iter);
   end Convert_Iter_To_Child_Iter;

   --------------------------------
   -- Convert_Path_To_Child_Path --
   --------------------------------

   function Convert_Path_To_Child_Path
     (Filter      : access Gtk_Tree_Model_Filter_Record;
      Filter_Path : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Path
   is
      function Internal
        (Filter      : System.Address;
         Filter_Path : Gtk_Tree_Path) return Gtk_Tree_Path;
      pragma Import
        (C, Internal, "gtk_tree_model_filter_convert_path_to_child_path");
   begin
      return Internal (Get_Object (Filter), Filter_Path);
   end Convert_Path_To_Child_Path;

   ------------------------
   -- Set_Visible_Column --
   ------------------------

   procedure Set_Visible_Column
     (Filter : access Gtk_Tree_Model_Filter_Record; Column : Gint)
   is
      procedure Internal (Filter : System.Address; Column : Gint);
      pragma Import (C, Internal, "gtk_tree_model_filter_set_visible_column");
   begin
      Internal (Get_Object (Filter), Column);
   end Set_Visible_Column;

   --------------------------
   -- Visible_Func_Wrapper --
   --------------------------

   function Visible_Func_Wrapper
     (Model, Iter : System.Address; Data : Gtk_Tree_Model_Filter_Visible_Func)
      return Gboolean
   is
      Stub : Gtk_Tree_Model_Record;
      It   : constant Gtk_Tree_Iter_Access := To_Iter (Iter);
   begin
      return Boolean'Pos
        (Data (Gtk_Tree_Model (Get_User_Data (Model, Stub)), It.all));
   end Visible_Func_Wrapper;

   ----------------------
   -- Set_Visible_Func --
   ----------------------

   procedure Set_Visible_Func
     (Filter  : access Gtk_Tree_Model_Filter_Record;
      Func    : Gtk_Tree_Model_Filter_Visible_Func)
   is
      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Tree_Model_Filter_Visible_Func, System.Address);
      procedure Internal
        (Filter  : System.Address;
         Func    : System.Address;
         Data    : System.Address;
         Destroy : System.Address := System.Null_Address);
      pragma Import (C, Internal, "gtk_tree_model_filter_set_visible_func");
   begin
      Internal
        (Get_Object (Filter), Visible_Func_Wrapper'Address, To_Address (Func));
   end Set_Visible_Func;

   -------------------
   -- Visible_Funcs --
   -------------------

   package body Visible_Funcs is
      procedure Free is new Ada.Unchecked_Deallocation
        (Data_Type, Data_Type_Access);
      procedure Free is new Ada.Unchecked_Deallocation
        (Data_Type_Record, Data_Type_Record_Access);

      ----------------------
      -- Set_Visible_Func --
      ----------------------

      procedure Set_Visible_Func
        (Filter  : access Gtk_Tree_Model_Filter_Record'Class;
         Func    : Gtk_Tree_Model_Filter_Visible_Func;
         Data    : Data_Type;
         Destroy : Destroy_Notify := null)
      is
         procedure Internal
           (Filter  : System.Address;
            Func    : System.Address;
            Data    : Data_Type_Record_Access;
            Destroy : System.Address);
         pragma Import (C, Internal, "gtk_tree_model_filter_set_visible_func");
      begin
         Internal
           (Get_Object (Filter),
            Internal_Filter_Visible_Func'Address,
            new Data_Type_Record'
              (Func    => Func,
               Destroy => Destroy,
               Data    => new Data_Type'(Data)),
            Internal_Destroy_Notify'Address);
      end Set_Visible_Func;

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

      ----------------------------------
      -- Internal_Filter_Visible_Func --
      ----------------------------------

      function Internal_Filter_Visible_Func
        (Model : System.Address;
         Iter  : System.Address;
         Data  : Data_Type_Record_Access) return Gboolean
      is
         Stub : Gtk_Tree_Model_Record;
         It   : constant Gtk_Tree_Iter_Access := To_Iter (Iter);
      begin
         return Boolean'Pos
           (Data.Func
              (Gtk_Tree_Model (Get_User_Data (Model, Stub)),
               It.all,
               Data.Data.all));
      end Internal_Filter_Visible_Func;
   end Visible_Funcs;

   --------------
   -- Refilter --
   --------------

   procedure Refilter (Filter : access Gtk_Tree_Model_Filter_Record) is
      procedure Internal (Filter : System.Address);
      pragma Import (C, Internal, "gtk_tree_model_filter_refilter");
   begin
      Internal (Get_Object (Filter));
   end Refilter;

   -------------------------
   -- Modify_Func_Wrapper --
   -------------------------

   procedure Modify_Func_Wrapper
     (Model, Iter : System.Address;
      Value       : out Glib.Values.GValue;
      Column      : Gint;
      Data        : Gtk_Tree_Model_Filter_Modify_Func)
   is
      Stub : Gtk_Tree_Model_Filter_Record;
      M    : constant Gtk_Tree_Model_Filter :=
        Gtk_Tree_Model_Filter (Get_User_Data (Model, Stub));
      It   : constant Gtk_Tree_Iter_Access := To_Iter (Iter);
   begin
      Data (M, It.all, Value, Column);
   end Modify_Func_Wrapper;

   ---------------------
   -- Set_Modify_Func --
   ---------------------

   procedure Set_Modify_Func
     (Filter    : access Gtk_Tree_Model_Filter_Record;
      Types     : Glib.GType_Array;
      Func      : Gtk_Tree_Model_Filter_Modify_Func)
   is
      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Tree_Model_Filter_Modify_Func, System.Address);
      procedure Internal
        (Filter    : System.Address;
         N_Columns : Gint;
         Types     : System.Address;
         Func      : System.Address;
         Data      : System.Address;
         Destroy   : System.Address := System.Null_Address);
      pragma Import (C, Internal, "gtk_tree_model_filter_set_modify_func");
   begin
      Internal (Get_Object (Filter), Types'Length, Types (Types'First)'Address,
                Modify_Func_Wrapper'Address, To_Address (Func));
   end Set_Modify_Func;

   ------------------
   -- Modify_Funcs --
   ------------------

   package body Modify_Funcs is

      procedure Free is new Ada.Unchecked_Deallocation
        (Data_Type, Data_Type_Access);
      procedure Free is new Ada.Unchecked_Deallocation
        (Data_Type_Record, Data_Type_Record_Access);

      ---------------------
      -- Set_Modify_Func --
      ---------------------

      procedure Set_Modify_Func
        (Filter    : access Gtk_Tree_Model_Filter_Record'Class;
         Types     : Glib.GType_Array;
         Func      : Gtk_Tree_Model_Filter_Modify_Func;
         Data      : Data_Type;
         Destroy   : Destroy_Notify := null)
      is
         procedure Internal
           (Filter    : System.Address;
            N_Columns : Gint;
            Types     : System.Address;
            Func      : System.Address;
            Data      : Data_Type_Record_Access;
            Destroy   : System.Address);
         pragma Import (C, Internal, "gtk_tree_model_filter_set_modify_func");
      begin
         Internal
           (Get_Object (Filter), Types'Length, Types (Types'First)'Address,
            Internal_Filter_Modify_Func'Address,
            new Data_Type_Record'
              (Func    => Func,
               Destroy => Destroy,
               Data    => new Data_Type'(Data)),
            Internal_Destroy_Notify'Address);
      end Set_Modify_Func;

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

      ---------------------------------
      -- Internal_Filter_Modify_Func --
      ---------------------------------

      procedure Internal_Filter_Modify_Func
        (Model  : System.Address;
         Iter   : System.Address;
         Value  : out Glib.Values.GValue;
         Column : Gint;
         Data   : Data_Type_Record_Access)
      is
         Stub : Gtk_Tree_Model_Filter_Record;
         It   : constant Gtk_Tree_Iter_Access := To_Iter (Iter);
      begin
         Data.Func
           (Gtk_Tree_Model_Filter (Get_User_Data (Model, Stub)),
            It.all, Value, Column, Data.Data.all);
      end Internal_Filter_Modify_Func;
   end Modify_Funcs;

   -----------------
   -- Clear_Cache --
   -----------------

   procedure Clear_Cache (Filter : access Gtk_Tree_Model_Filter_Record) is
      procedure Internal (Filter : System.Address);
      pragma Import (C, Internal, "gtk_tree_model_filter_clear_cache");
   begin
      Internal (Get_Object (Filter));
   end Clear_Cache;
end Gtk.Tree_Model_Filter;
