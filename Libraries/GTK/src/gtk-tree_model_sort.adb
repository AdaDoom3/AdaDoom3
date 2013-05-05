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

with Gtk;             use Gtk;
with Gtk.Tree_Model;  use Gtk.Tree_Model;
with System;

with Glib.Type_Conversion_Hooks;

package body Gtk.Tree_Model_Sort is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Tree_Model_Sort_Record);
   pragma Warnings (Off, Type_Conversion);

   ---------------
   -- Get_Model --
   ---------------

   function Get_Model
     (Tree_Model : access Gtk_Tree_Model_Sort_Record)
      return Gtk.Tree_Model.Gtk_Tree_Model
   is
      function Internal (Tree_Model : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_model_sort_get_model");

      Stub : Gtk.Tree_Model.Gtk_Tree_Model_Record;

   begin
      return Gtk.Tree_Model.Gtk_Tree_Model
        (Get_User_Data_Fast (Internal (Get_Object (Tree_Model)), Stub));
   end Get_Model;

   --------------------------------
   -- Convert_Child_Path_To_Path --
   --------------------------------

   function Convert_Child_Path_To_Path
     (Tree_Model_Sort : access Gtk_Tree_Model_Sort_Record;
      Child_Path      : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Path
   is
      function Internal
        (Tree_Model_Sort : System.Address;
         Child_Path      : Gtk_Tree_Path)
         return Gtk.Tree_Model.Gtk_Tree_Path;
      pragma Import (C, Internal,
                     "gtk_tree_model_sort_convert_child_path_to_path");

   begin
      return Internal (Get_Object (Tree_Model_Sort), Child_Path);
   end Convert_Child_Path_To_Path;

   --------------------------------
   -- Convert_Child_Iter_To_Iter --
   --------------------------------

   procedure Convert_Child_Iter_To_Iter
     (Tree_Model_Sort : access Gtk_Tree_Model_Sort_Record;
      Sort_Iter       : out Gtk.Tree_Model.Gtk_Tree_Iter;
      Child_Iter      : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      pragma Warnings (Off, Sort_Iter);
      procedure Internal
        (Tree_Model_Sort : System.Address;
         Sort_Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
         Child_Iter      : Gtk.Tree_Model.Gtk_Tree_Iter);
      pragma Import
        (C, Internal, "gtk_tree_model_sort_convert_child_iter_to_iter");

   begin
      Internal (Get_Object (Tree_Model_Sort), Sort_Iter, Child_Iter);
   end Convert_Child_Iter_To_Iter;

   --------------------------------
   -- Convert_Path_To_Child_Path --
   --------------------------------

   function Convert_Path_To_Child_Path
     (Tree_Model_Sort : access Gtk_Tree_Model_Sort_Record;
      Sorted_Path     : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Path
   is
      function Internal
        (Tree_Model_Sort : System.Address;
         Sorted_Path     : Gtk.Tree_Model.Gtk_Tree_Path)
         return Gtk.Tree_Model.Gtk_Tree_Path;
      pragma Import
        (C, Internal, "gtk_tree_model_sort_convert_path_to_child_path");

   begin
      return Internal (Get_Object (Tree_Model_Sort), Sorted_Path);
   end Convert_Path_To_Child_Path;

   --------------------------------
   -- Convert_Iter_To_Child_Iter --
   --------------------------------

   procedure Convert_Iter_To_Child_Iter
     (Tree_Model_Sort : access Gtk_Tree_Model_Sort_Record;
      Child_Iter      : out Gtk.Tree_Model.Gtk_Tree_Iter;
      Sorted_Iter     : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      pragma Warnings (Off, Child_Iter);

      procedure Internal
        (Tree_Model_Sort : System.Address;
         Child_Iter      : Gtk.Tree_Model.Gtk_Tree_Iter;
         Sorted_Iter     : Gtk.Tree_Model.Gtk_Tree_Iter);
      pragma Import
        (C, Internal, "gtk_tree_model_sort_convert_iter_to_child_iter");

   begin
      Internal (Get_Object (Tree_Model_Sort), Child_Iter, Sorted_Iter);
   end Convert_Iter_To_Child_Iter;

   -----------------------------
   -- Reset_Default_Sort_Func --
   -----------------------------

   procedure Reset_Default_Sort_Func
     (Tree_Model_Sort : access Gtk_Tree_Model_Sort_Record)
   is
      procedure Internal (Tree_Model_Sort : System.Address);
      pragma Import (C, Internal,
                     "gtk_tree_model_sort_reset_default_sort_func");
   begin
      Internal (Get_Object (Tree_Model_Sort));
   end Reset_Default_Sort_Func;

   -----------------
   -- Clear_Cache --
   -----------------

   procedure Clear_Cache (Tree_Model_Sort : access Gtk_Tree_Model_Sort_Record)
   is
      procedure Internal (Tree_Model_Sort : System.Address);
      pragma Import (C, Internal, "gtk_tree_model_sort_clear_cache");
   begin
      Internal (Get_Object (Tree_Model_Sort));
   end Clear_Cache;

   -------------------
   -- Iter_Is_Valid --
   -------------------

   function Iter_Is_Valid
     (Tree_Model_Sort : access Gtk_Tree_Model_Sort_Record;
      Iter            : Gtk_Tree_Iter)
      return Boolean
   is
      function Internal
        (Tree_Model_Sort : System.Address;
         Iter            : Gtk_Tree_Iter)
         return Gboolean;
      pragma Import (C, Internal, "gtk_tree_model_sort_iter_is_valid");
   begin
      return Boolean'Val (Internal (Get_Object (Tree_Model_Sort), Iter));
   end Iter_Is_Valid;

   ------------------------
   -- Gtk_New_With_Model --
   ------------------------

   procedure Gtk_New_With_Model
     (Sort_Model  : out Gtk_Tree_Model_Sort;
      Child_Model : access Gtk_Tree_Model_Record'Class)
   is
   begin
      Sort_Model := new Gtk_Tree_Model_Sort_Record;
      Initialize_With_Model (Sort_Model, Child_Model);
   end Gtk_New_With_Model;

   ---------------------------
   -- Initialize_With_Model --
   ---------------------------

   procedure Initialize_With_Model
     (Sort_Model  : access Gtk_Tree_Model_Sort_Record'Class;
      Child_Model : access Gtk_Tree_Model_Record'Class)
   is
      function Internal
        (Child_Model : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_model_sort_new_with_model");
   begin
      Set_Object (Sort_Model, Internal (Get_Object (Child_Model)));
   end Initialize_With_Model;

end Gtk.Tree_Model_Sort;
