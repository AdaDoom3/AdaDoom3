-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2001-2013, AdaCore                   --
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
--  This package implements a specific model to store your data in. It is
--  basically similar to a small database, in that each field can contain any
--  number of columns.
--
--  Each column can contain a different type of data, specified when the model
--  is created.
--
--  Adding new values in the model is done as in the example at the end.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Trees and Lists</group>

with Glib.Types;
with Glib.Values; use Glib.Values;
with Gtk;
with Gtk.Tree_Dnd;
with Gtk.Tree_Model;
with Gtk.Tree_Sortable;

package Gtk.Tree_Store is

   type Gtk_Tree_Store_Record is
     new Gtk.Tree_Model.Gtk_Tree_Model_Record with null record;
   type Gtk_Tree_Store is access all Gtk_Tree_Store_Record'Class;

   procedure Gtk_New
     (Tree_Store : out Gtk_Tree_Store;
      Types      : GType_Array);
   --  Create a new tree store using Types to fill the columns.

   procedure Initialize
     (Tree_Store : access Gtk_Tree_Store_Record'Class;
      Types      : GType_Array);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Glib.GType;
   --  Return the internal value associated with this widget.

   procedure Set_Column_Types
     (Tree_Store : access Gtk_Tree_Store_Record;
      Types      : GType_Array);
   --  This function is meant primarily for GObjects that inherit from
   --  Gtk_Tree_Store, and should only be used when constructing a new
   --  Gtk_Tree_Store. It will not function after a row has been added, or a
   --  method on the Gtk_Tree_Model interface is called.

   procedure Set_Value
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : Glib.Values.GValue);
   --  Set a new value in the model. The value is added in the column Column,
   --  and in the line Iter.
   --  This is the most general of the Set procedures, since it allows you to
   --  control what should be done when the cell is freed (useful for instance
   --  for reference-controlled types). In particular, you would create a
   --  GValue of a special type derived from Boxed (see Glib.Value.Set_Boxed).
   --
   --  The type of the column must be of the type stored in the GValue itself.
   --  Referencing the example given for Set_Boxed, this would be the value
   --  in "Typ".

   generic
      type Data_Type is private;
   package Generic_Set is
      type Data_Type_Access is access all Data_Type;

      procedure Set
        (Tree_Store : access Gtk_Tree_Store_Record'Class;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
         Column     : Gint;
         Value      : Data_Type_Access);
      --  Generic procedure used to store access objects in the model.
      --  For GObject and all of its descendents (including all widgets),
      --  you should use the Set procedure below that takes a GObject as
      --  parameter.
      --
      --  Please see the example at the end for more information on how to
      --  create your own Set procedures adapted to your model. Also consider
      --  using Set_Value for complex cases

      function Get
        (Tree_Store : access Gtk_Tree_Store_Record'Class;
         Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
         Column     : Gint) return Data_Type_Access;
      --  Generic procedure used to get access objects back from the model.
      --  For GObject and all of its descendents (including all widgets),
      --  you should use the Get_Object function defined in Gtk-Tree_Model
      --  that returns a GObject.

   end Generic_Set;

   procedure Set
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : UTF8_String);
   --  Same as Generic_Set, but tailored to use with a string.

   procedure Set
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : Boolean);
   --  Same as Generic_Set, but tailored to use with a boolean.

   procedure Set
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : Gint);
   --  Same as Generic_Set, but tailored to use with an integer.

   procedure Set
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : Glib.C_Proxy);
   --  Same as Generic_Set, but tailored for Gdk types.

   procedure Set
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Address    : System.Address);
   --  Same as Generic_Set, for a generic address

   procedure Set
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : Glib.Object.GObject);
   --  Same as Generic_Set, but tailored to objects/widgets.

   procedure Remove
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Remove Iter from Tree_Store.
   --  After being removed, Iter is set to Null_Iter.

   procedure Insert
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter;
      Parent     : Gtk.Tree_Model.Gtk_Tree_Iter;
      Position   : Gint);
   --  Create a new row at Position.
   --  If parent is non-null, then the row will be made a child of Parent.
   --  Otherwise, the row will be created at the toplevel. If Position is
   --  larger than the number of rows at that level, then the new row will be
   --  inserted to the end of the list. Iter will be changed to point to this
   --  new row. The row will be empty before this function is called. To fill
   --  in values, you need to call Set_Value.

   procedure Insert_Before
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter;
      Parent     : Gtk.Tree_Model.Gtk_Tree_Iter;
      Sibling    : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Insert a new row before Sibling.
   --  If Sibling is Null_Iter, then the row will be appended to the beginning
   --  of the Parent's children. If Parent and Sibling are Null_Iter, then the
   --  row will be appended to the toplevel. If both Sibling and Parent are
   --  set, then Parent must be the parent of Sibling. When Sibling is set,
   --  Parent is optional. Iter will be changed to point to this new row. The
   --  row will be empty after this function is called. To fill in values, you
   --  need to call Set_Value.

   procedure Insert_After
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter;
      Parent     : Gtk.Tree_Model.Gtk_Tree_Iter;
      Sibling    : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Insert a new row after Sibling.
   --  If Sibling is Null_Iter, then the row will be prepended to the beginning
   --  of the Parent's children. If Parent and Sibling are Null_Iter, then the
   --  row will be prepended to the toplevel. If both Sibling and Parent are
   --  set, then Parent must be the parent of Sibling. When Sibling is set,
   --  Parent is optional. Iter will be changed to point to this new row. The
   --  row will be empty after this function is called. To fill in values, you
   --  need to call Set_Value.

   procedure Prepend
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter;
      Parent     : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Prepend a new row to Tree_Store.
   --  If Parent is non-null, then it will prepend the new row before the first
   --  child of Parent, otherwise it will prepend a row to the top level. Iter
   --  will be changed to point to this new row. The row will be empty after
   --  this function is called. To fill in values, you need to call Set_Value.
   --  The efficiency of this procedure is O(N).

   procedure Append
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter;
      Parent     : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Append a new row to Tree_Store.
   --  If Parent is non-null, then it will append the new row after the last
   --  child of Parent, otherwise it will append a row to the top level. Iter
   --  will be changed to point to this new row. The row will be empty after
   --  this function is called. To fill in values, you need to call Set_Value.
   --  The efficiency of this procedure is O(N^2).

   function Is_Ancestor
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Descendant : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   --  Return True if Iter is an ancestor of Descendant.
   --  That is, Iter is the parent (or grandparent or great-grandparent) of
   --  Descendant.

   function Iter_Depth
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter) return Gint;
   --  Returns the depth of Iter.
   --  This will be 0 for anything on the root level, 1 for anything down a
   --  level, etc.

   function Iter_Is_Valid
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Boolean;
   --  WARNING: This function is slow. Only use it for debugging and/or testing
   --  purposes.
   --  Checks if the given iter is a valid iter for Tree_Store.

   procedure Move_After
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Position   : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Moves the row pointed to by Iter to the position after Position. Iter
   --  and Position should be in the same level. Note that this function only
   --  works with unsorted stores. If Position is Null_Iter, Iter will be
   --  moved to the start of the level.

   procedure Move_Before
     (Tree_Store : access Gtk_Tree_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Position   : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Moves the row pointed to by Iter to the position before Position. Iter
   --  and Position should be in the same level. Note that this function only
   --  works with unsorted stores. If Position is Null_Iter, Iter will be

   procedure Clear (Tree_Store : access Gtk_Tree_Store_Record);
   --  Removes all rows from Tree_Store

   procedure Reorder
     (Tree_Store : access Gtk_Tree_Store_Record;
      Parent     : Gtk.Tree_Model.Gtk_Tree_Iter;
      New_Order  : Glib.Gint_Array);
   --  Reorders the children of Parent to follow the order indicated by
   --  New_order. Note that this function only works with unsorted stores. New
   --  order is an array of integers mapping the new position of each child to
   --  its old position before the re-ordering,
   --  i.e. New_order[newpos] = oldpos
   --  If Parent is Null_Iter, it also reorders the root nodes

   procedure Swap
     (Tree_Store : access Gtk_Tree_Store_Record;
      A          : Gtk.Tree_Model.Gtk_Tree_Iter;
      B          : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Swaps the rows pointed to by A and B (in the same level). Note that this
   --  function only works with unsorted stores.

   ---------------------------
   -- Sorting Freeze / Thaw --
   ---------------------------

   --  Note: the following two functions are not part of the Gtk+ API, but
   --  are provided by GtkAda.

   function Freeze_Sort
     (Tree : access Gtk.Tree_Store.Gtk_Tree_Store_Record'Class)
      return Gint;
   --  Freeze the sorting in the tree view, and returns the current
   --  sort_column_id, which should be used when thawing. (See Thaw_Sort)

   procedure Thaw_Sort
     (Tree      : access Gtk.Tree_Store.Gtk_Tree_Store_Record'Class;
      Column_Id : Gint);
   --  Thaw a frozen tree_view. Column_Id should be the value returned by
   --  the corresponding call to Freeze_Sort.

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
   --
   --  - "Gtk_Tree_Drag_Dest"
   --    This interface allows this widget to act as a dnd destination

   package Implements_Tree_Sortable is new Glib.Types.Implements
     (Gtk.Tree_Sortable.Gtk_Tree_Sortable,
      Gtk_Tree_Store_Record,
      Gtk_Tree_Store);
   function "+"
     (Model : access Gtk_Tree_Store_Record'Class)
      return Gtk.Tree_Sortable.Gtk_Tree_Sortable
      renames Implements_Tree_Sortable.To_Interface;
   function "-"
     (Sortable : Gtk.Tree_Sortable.Gtk_Tree_Sortable)
      return Gtk_Tree_Store
      renames Implements_Tree_Sortable.To_Object;
   --  Converts to and from the Gtk_Tree_Sortable interface

   package Implements_Drag_Source is new Glib.Types.Implements
     (Gtk.Tree_Dnd.Gtk_Tree_Drag_Source,
      Gtk_Tree_Store_Record,
      Gtk_Tree_Store);
   function "+"
     (Model : access Gtk_Tree_Store_Record'Class)
      return Gtk.Tree_Dnd.Gtk_Tree_Drag_Source
      renames Implements_Drag_Source.To_Interface;
   function "-"
     (Drag_Source : Gtk.Tree_Dnd.Gtk_Tree_Drag_Source)
      return Gtk_Tree_Store
      renames Implements_Drag_Source.To_Object;
   --  Converts to and from the Gtk_Tree_Drag_Source interface

   package Implements_Drag_Dest is new Glib.Types.Implements
     (Gtk.Tree_Dnd.Gtk_Tree_Drag_Dest,
      Gtk_Tree_Store_Record,
      Gtk_Tree_Store);
   function "+"
     (Model : access Gtk_Tree_Store_Record'Class)
      return Gtk.Tree_Dnd.Gtk_Tree_Drag_Dest
      renames Implements_Drag_Dest.To_Interface;
   function "-"
     (Drag_Dest : Gtk.Tree_Dnd.Gtk_Tree_Drag_Dest)
      return Gtk_Tree_Store
      renames Implements_Drag_Dest.To_Object;
   --  Converts to and from the Gtk_Tree_Drag_Source interface

private
   pragma Import (C, Get_Type, "gtk_tree_store_get_type");
end Gtk.Tree_Store;

--  <example>
--  Adding a new line in the model:
--
--  declare
--     Iter  : Gtk_Text_Iter;
--     Value : Glib.Values.GValue;
--  begin
--     Append (Model, Iter, Null_Iter);
--
--     --  First method:
--
--     Init (Value, GType_String);
--     Set_String (Value, "foo");
--     Set_Value (Model, Iter, 0, Value);
--     Unref (Value);
--
--     --  Second method:
--
--     Set (Model, Iter, 0, "foo");
--  end;
--
--  </example>

--  <example>
--  Defining your own Set function for your model: This can be done by directly
--  importing the C function, with the appropriate number of parameters.
--  Remember that you are passing data directly to C, thus you need to end
--  strings with ASCII.NUL
--
--  procedure My_Set
--     (Tree_Store : access Gtk_Tree_Store_Record'Class;
--      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
--      Column1 : Gint; Value1 : UTF8_String;
--      Column2 : Gint; Value2 : Boolean)
--  is
--      procedure Set_String
--        (Tree : System.Address;
--         Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
--         Column : Gint; Value : UTF8_String);
--      pragma Import (C, Set_String, "ada_gtk_tree_store_set_ptr");
--
--      procedure Set_Int
--        (Tree : System.Address;
--         Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
--         Column : Gint; Value : Gint);
--      pragma Import (C, Internal, "ada_gtk_tree_store_set_int");
--   begin
--      Internal
--        (Get_Object (Tree_Store), Iter, Column1, Value1 & ASCII.NUL);
--      Internal
--        (Get_Object (Tree_Store), Iter, Column2, Boolean'Pos (Value2));
--   end Set;
--
--  </example>

--  No binding: gtk_tree_store_new
--  No binding: gtk_tree_store_set
--  No binding: gtk_tree_store_set_valist
