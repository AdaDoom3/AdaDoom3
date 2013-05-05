-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2001-2006 AdaCore                    --
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

--  <description>
--  The Gtk_Tree_Model_Sort is a model which implements the Gtk_Tree_Sortable
--  interface. It does not hold any data itself, but rather is created with
--  child model and proxies its data. It has identical column types to this
--  child model, and the changes in the child are propagated. The primary
--  purpose of this model is to provide a way to sort a different model without
--  modifying it. Note that the sort function used by Gtk_Tree_Model_Sort is
--  not guaranteed to be stable.
--
--  The use of this is best demonstrated through an example. In the following
--  sample code we create two Gtk_Tree_View widgets each with a view of the
--  same data. As the model is wrapped here by a Gtk_Tree_Model_Sort, the two
--  Gtk_Tree_Views can each sort their view of the data without affecting the
--  other. By contrast, if we simply put the same model in each widget, then
--  sorting the first would sort the second.
--
--  declare
--     Tree_View1, Tree_View2   : Gtk_Tree_View;
--     Sort_Model1, Sort_Model2 : Gtk_Tree_Model_Sort;
--     Child_Model              : Gtk_Tree_Model;
--  begin
--    Child_Model := Get_My_Model;  --  Your own implementation
--
--    --  Create the first tree
--    Gtk_New_With_Model (Sort_Model1, Child_Model);
--    Gtk_New (Tree_View1, Sort_Model1);
--    Set_Sort_Column_Id (Sort_Model1, COLUMN1, Sort_Ascending);
--
--    --  Create the second tree
--    Gtk_New_With_Model (Sort_Model2, Child_Model);
--    Gtk_New (Tree_View2, Sort_Model2);
--    Set_Sort_Column_Id (Sort_Model2, COLUMN1, Sort_Descending);
--  end;
--
--  To demonstrate how to access the underlying child model from the sort
--  model, the next example will be a callback for the Gtk_Tree_Selection
--  "changed" signal. In this callback, we get a string from COLUMN_1 of the
--  model. We then modify the string, find the same selected row on the child
--  model, and change the row there.
--
--  procedure Selection_Changed
--    (Selection : access Gtk_Tree_Selection_Record'Class)
--  is
--     Sort_Model, Child_Model : Gtk_Tree_Model;
--     Sort_Iter, Child_Iter   : Gtk_Tree_Iter;
--  begin
--     --  Get the currently selected row and the model
--     Get_Selected (Selection, Sort_Model, Sort_Iter);
--     if Sort_Iter = Null_Iter then
--        return;
--     end if;
--
--     --  Lookup the current value on the selected row
--     declare
--       Some_Data : constant String :=
--                     Get_String (Sort_Model, Sort_Iter, COLUMN1);
--     begin
--        --  Get an iterator on the child model instead of the sort model
--        Convert_Iter_To_Child_Iter (Sort_Model, Child_Iter, Sort_Iter);
--
--        --  Get the child model and change the value in the row
--        --  In this example, the model is a Gtk_List_Store, but it could be
--        --  anything
--        Child_Model := Get_Model (Gtk_Sort_Model (Sort_Model));
--        Set (Ctk_List_Store (Child_Model), Child_Iter, COLUMN1, "data");
--     end;
--  end Selection_Changed;
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Trees and Lists</group>

with Glib.Properties;
with Glib.Types;
with Gtk;
with Gtk.Tree_Dnd;
with Gtk.Tree_Model;
with Gtk.Tree_Sortable;

package Gtk.Tree_Model_Sort is

   type Gtk_Tree_Model_Sort_Record is
     new Gtk.Tree_Model.Gtk_Tree_Model_Record with null record;
   type Gtk_Tree_Model_Sort is access all Gtk_Tree_Model_Sort_Record'Class;

   procedure Gtk_New_With_Model
     (Sort_Model  : out Gtk_Tree_Model_Sort;
      Child_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class);
   procedure Initialize_With_Model
     (Sort_Model  : access Gtk_Tree_Model_Sort_Record'Class;
      Child_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class);
   --  Creates or initialized a new sortable tree model, with Child_Model as
   --  the child model.
   --  Any change in Child_Model is reflected into Sort_Model

   function Get_Type return Glib.GType;
   --  Return the internal type associated with a Gtk_Tree_Model_Sort.

   function Get_Model
     (Tree_Model : access Gtk_Tree_Model_Sort_Record)
      return Gtk.Tree_Model.Gtk_Tree_Model;
   --  Return the model the Gtk_Tree_Model_Sort is sorting.

   function Convert_Child_Path_To_Path
     (Tree_Model_Sort : access Gtk_Tree_Model_Sort_Record;
      Child_Path      : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Path;
   --  Convert Child_Path to a path relative to Tree_Model_Sort.
   --  That is, Child_Path points to a path in the child model.
   --  The returned path will point to the same row in the sorted model.
   --  If Child_Path isn't a valid path on the child model, then Null
   --  is returned.
   --  The returned value must be freed with Path_Free.

   procedure Convert_Child_Iter_To_Iter
     (Tree_Model_Sort : access Gtk_Tree_Model_Sort_Record;
      Sort_Iter       : out Gtk.Tree_Model.Gtk_Tree_Iter;
      Child_Iter      : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Set Sort_Iter to point to the row in Tree_Model_Sort that
   --  corresponds to the row pointed at by Child_Iter.

   function Convert_Path_To_Child_Path
     (Tree_Model_Sort : access Gtk_Tree_Model_Sort_Record;
      Sorted_Path     : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Path;
   --  Convert Sort_Path to a path on the child model of Tree_Model_Sort.
   --  That is, Sort_Path points ot a location in Tree_Model_Sort.
   --  The returned path will point to the same location in the model
   --  not being sorted.

   procedure Convert_Iter_To_Child_Iter
     (Tree_Model_Sort : access Gtk_Tree_Model_Sort_Record;
      Child_Iter      : out Gtk.Tree_Model.Gtk_Tree_Iter;
      Sorted_Iter     : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Set Child_Iter to point to the row pointed to by Sorted_Iter.

   procedure Reset_Default_Sort_Func
     (Tree_Model_Sort : access Gtk_Tree_Model_Sort_Record);
   --  This resets the default sort function to be in the 'unsorted' state.
   --  That is, it is in the same order as the child model. It will re-sort the
   --  model to be in the same order as the child model only if the
   --  Gtk_Tree_Model_Sort is in 'unsorted' state.

   procedure Clear_Cache (Tree_Model_Sort : access Gtk_Tree_Model_Sort_Record);
   --  This function should almost never be called. It clears the
   --  tree_model_sort of any cached iterators that haven't been reffed with
   --  gtk.tree_model.ref_node. This might be useful if the child model being
   --  sorted is static (and doesn't change often) and there has been a lot of
   --  unreffed access to nodes. As a side effect of this function, all
   --  unreffed iters will be invalid.

   function Iter_Is_Valid
     (Tree_Model_Sort : access Gtk_Tree_Model_Sort_Record;
      Iter            : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   --  WARNING: this function is slow. Only use if for debugging and/or
   --  testing purposes.
   --  Checks if the given iter is a valid iter for this model.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Tree_Sortable"
   --    This interface allows you to specify your own sort function
   --
   --  - "Gtk_Tree_Drag_Source"
   --    This interface allows this widget to act as a dnd source

   package Implements_Tree_Sortable is new Glib.Types.Implements
     (Gtk.Tree_Sortable.Gtk_Tree_Sortable,
      Gtk_Tree_Model_Sort_Record,
      Gtk_Tree_Model_Sort);
   function "+"
     (Model : access Gtk_Tree_Model_Sort_Record'Class)
      return Gtk.Tree_Sortable.Gtk_Tree_Sortable
      renames Implements_Tree_Sortable.To_Interface;
   function "-"
     (Sortable : Gtk.Tree_Sortable.Gtk_Tree_Sortable)
      return Gtk_Tree_Model_Sort
      renames Implements_Tree_Sortable.To_Object;
   --  Converts to and from the Gtk_Tree_Sortable interface

   package Implements_Drag_Source is new Glib.Types.Implements
     (Gtk.Tree_Dnd.Gtk_Tree_Drag_Source,
      Gtk_Tree_Model_Sort_Record,
      Gtk_Tree_Model_Sort);
   function "+"
     (Model : access Gtk_Tree_Model_Sort_Record'Class)
      return Gtk.Tree_Dnd.Gtk_Tree_Drag_Source
      renames Implements_Drag_Source.To_Interface;
   function "-"
     (Drag_Source : Gtk.Tree_Dnd.Gtk_Tree_Drag_Source)
      return Gtk_Tree_Model_Sort
      renames Implements_Drag_Source.To_Object;
   --  Converts to and from the Gtk_Tree_Drag_Source interface

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  </signals>

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Model_Property
   --  Type:  Object
   --  Descr: The model for the TreeModelSort to sort
   --
   --  </properties>

   Model_Property : constant Glib.Properties.Property_Object;

private
   Model_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("model");

   pragma Import (C, Get_Type, "gtk_tree_model_sort_get_type");
end Gtk.Tree_Model_Sort;
