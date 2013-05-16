-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--               Copyright (C) 2001-2013, AdaCore                    --
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
--  The type Gtk_Tree_Model defined in this model defines an abstract interface
--  to represent sets of data that will be displayed in a Gtk_Tree_View.
--  Various default implementations are provided  in the Gtk.Tree_Store and
--  Gtk.List_Store packages.
--
--  Data are considered as being organized into a tree-like structure.
--
--  This package also defines a number of other types to manipulate these
--  models:
--
--  A Gtk_Tree_Path is a textual pointer to a specific row/node in the
--  model. It is a column separate list of numbers, that indicate the index of
--  the child they point to.
--  For instance, "10:4:0" would points to the first (0) child of the fifth (4)
--  child of the eleventh child of the root. The depth of this path is 3.
--
--  A Gtk_Tree_Iter is similar to a path, but is a direct pointer to the actual
--  data. It is also more efficient to use than paths.
--
--  A Gtk_Row_Reference is an object that tracks model changes, so that it
--  always refere to the same row. A Gtk_Tree_Path refers to a position in the
--  model, not a fixed row.
--
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Trees and Lists</group>

with Ada.Unchecked_Conversion;
with Glib.Glist;
with Glib.Object;
with Glib.Values;

package Gtk.Tree_Model is

   type Gtk_Tree_Model_Record is new Glib.Object.GObject_Record
     with private;
   type Gtk_Tree_Model is access all Gtk_Tree_Model_Record'Class;
   --  This is an abstract interface

   type Gtk_Tree_Path is new Glib.C_Proxy;
   type Gtk_Tree_Iter is private;

   type Tree_Model_Flags is mod 2 ** 32;
   Tree_Model_Iters_Persist : constant Tree_Model_Flags;
   Tree_Model_List_Only     : constant Tree_Model_Flags;

   -----------------
   -- Tree models --
   -----------------

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Gtk_Tree_Model.

   function Get_Flags (Model : access Gtk_Tree_Model_Record)
      return Tree_Model_Flags;
   --  Return a set of flags supported by this interface. The flags
   --  supported should not change during the lifecycle of the tree_model.
   --  The flags should not change in the lifetime of the model.

   function Get_N_Columns (Tree_Model : access Gtk_Tree_Model_Record)
     return Gint;
   --  Return the number of columns supported by Tree_Model.

   function Get_Column_Type
     (Tree_Model : access Gtk_Tree_Model_Record; Index : Gint) return GType;
   --  Return the type of the Index-th column in the model.

   type Gtk_Tree_Model_Foreach_Func is access function
     (Model     : access Gtk_Tree_Model_Record'Class;
      Path      : Gtk_Tree_Path;
      Iter      : Gtk_Tree_Iter;
      User_Data : System.Address) return Boolean;

   procedure Foreach
     (Model     : access Gtk_Tree_Model_Record;
      Func      : Gtk_Tree_Model_Foreach_Func;
      User_Data : System.Address);
   --  Calls func on each node in model in a depth-first fashion. If func
   --  returns True, then the tree ceases to be walked, and Foreach returns.

   ------------------------
   -- Paths manipulation --
   ------------------------

   function Gtk_New (Path : String := "") return Gtk_Tree_Path;
   --  Create a new Gtk_Tree_Path from a path string.
   --  Path should have the format described above, like "10:4:0". If it is the
   --  empty string, then a Gtk_Tree_Path of depth 0 is returned.
   --  The memory allocated for the path must be freed explicitely by calling
   --  Path_Free below.

   function Gtk_New_First return Gtk_Tree_Path;
   --  Return a new path pointed to the first row in the model. The string
   --  representation is "0"

   function Path_Get_Type return Glib.GType;
   --  Return the internal type used for Gtk_Tree_Path

   function To_String (Path : Gtk_Tree_Path) return String;
   --  Generate a string representation of the path.
   --  This string is a colon-separated list of numbers, as described above.

   function Get_Tree_Path (Val : Glib.Values.GValue) return Gtk_Tree_Path;
   --  Extract the path from the given GValue.

   procedure Append_Index (Path : Gtk_Tree_Path; Index : Gint);
   --  Append a new index to a path.
   --  As a result, the depth of the path is increased. See Path_Up for the
   --  opposite operation.

   procedure Prepend_Index (Path : Gtk_Tree_Path; Index : Gint);
   --  Prepend a new index to a path.  As a result, the depth of the path is
   --  increased.

   function Get_Depth (Path : Gtk_Tree_Path) return Gint;
   --  Return the current depth of Path.

   function Get_Indices (Path : Gtk_Tree_Path) return Glib.Gint_Array;
   --  Return the list of indices from the path. This is an array of integers,
   --  each representing a node in a tree, as described in the path format.

   procedure Path_Free (Path : Gtk_Tree_Path);
   --  Free the memory allocated for Path.

   function Copy (Path : Gtk_Tree_Path) return Gtk_Tree_Path;
   --  Create a new Gtk_Tree_Path as a copy of Path. The memory allocated for
   --  the new path must be freed by a call to Path_Free.

   function Compare (A, B : Gtk_Tree_Path) return Gint;
   --  Compare two paths.  If A appears before B in a tree, then -1 is
   --  returned.  If B appears before A, then 1 is returned.  If the two nodes
   --  are equal, then 0 is returned.

   procedure Next (Path : Gtk_Tree_Path);
   --  Move the Path to point to the next node at the current depth. In effect,
   --  it increments the last indice of the path. Note that the path might
   --  become invalid if there is no more node at this depth.

   function Prev (Path : Gtk_Tree_Path) return Boolean;
   --  Move Path to point to the previous node at the current depth,
   --  if it exists.
   --  Return True if Path has a previous node, and the move was made. If it
   --  returns False, then Path has not been changed.

   function Up (Path : Gtk_Tree_Path) return Boolean;
   --  Moves the Path to point to it's parent node, if it has a parent.
   --  Return True if Path has a parent, and the move was made.
   --  In practice, the depth of Path is decreased by 1.

   procedure Down (Path : Gtk_Tree_Path);
   --  Moves Path to point to the first child of the current path.

   function Is_Ancestor (Path, Descendant : Gtk_Tree_Path) return Boolean;
   --  Return True if Descendant is contained inside Path.

   function Is_Descendant (Path, Ancestor : Gtk_Tree_Path) return Boolean;
   --  Return True if Path is contained inside Ancestor.

   function Convert is new Ada.Unchecked_Conversion
     (Gtk_Tree_Path, System.Address);
   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Tree_Path);
   package Gtk_Tree_Path_List is new Glib.Glist.Generic_List (Gtk_Tree_Path);

   --------------------------------
   -- Row_Reference manipulation --
   --------------------------------

   type Gtk_Tree_Row_Reference is new Glib.C_Proxy;

   function Gtk_New
     (Model : access Gtk_Tree_Model_Record;
      Path  : Gtk_Tree_Path)
      return Gtk_Tree_Row_Reference;
   --  Create a row reference based on Path. This reference will keep pointing
   --  to the node pointed to by Path, so long as it exists.  It listens to
   --  all signals on model, and updates it's path appropriately.  If Path
   --  isn't a valid path in Model, then null is returned.

   function Row_Reference_Get_Type return Glib.GType;
   --  Return the internal type used for row reference.

   function Get_Path (Reference : Gtk_Tree_Row_Reference) return Gtk_Tree_Path;
   --  Return the path that Reference currently points to.
   --  null is returned if Reference is no longer valid.
   --  The caller must free the returned path.

   function Valid (Reference : Gtk_Tree_Row_Reference) return Boolean;
   --  Return True if Reference is non null and is still valid.

   function Row_Reference_Copy
     (Ref : Gtk_Tree_Row_Reference) return Gtk_Tree_Row_Reference;
   --  Return a newly allocated copy of Ref

   procedure Row_Reference_Free (Reference : Gtk_Tree_Row_Reference);
   --  Free the memory occupied by Reference.

   function Get_Model
     (Reference : Gtk_Tree_Row_Reference) return Gtk_Tree_Model;
   --  Returns the model which Reference is monitoring in order to
   --  appropriately the path.

   ---------------
   -- Iterators --
   ---------------
   --  ??? Need to be able to access the user_data fields, so that new models
   --  can define their own iterators

   Null_Iter : constant Gtk_Tree_Iter;

   function "=" (Left : Gtk_Tree_Iter; Right : Gtk_Tree_Iter) return Boolean;

   function Iter_Get_Type return Glib.GType;
   --  Return the internal type used for iterators

   procedure Iter_Copy (Source : Gtk_Tree_Iter; Dest : out Gtk_Tree_Iter);
   --  Create a copy of Source.
   --  You can also copy tree iters simply by using the ":=" Ada construct.

   procedure Set_Tree_Iter
     (Val  : in out Glib.Values.GValue;
      Iter : Gtk_Tree_Iter);
   --  Set the value of the given GValue to Iter.
   --  Note that Iter is stored by reference, which means no copy of Iter
   --  is made. Iter should remain allocated as long as Val is being used.

   procedure Get_Tree_Iter
     (Val  : Glib.Values.GValue;
      Iter : out Gtk_Tree_Iter);
   --  Extract the iterator from the given GValue.
   --  Note that the iterator returned is a copy of the iterator referenced
   --  by the give GValue. Modifying the iterator returned does not modify
   --  the iterator referenced by the GValue.

   function Get_Tree_Iter (Val : Glib.Values.GValue) return Gtk_Tree_Iter;
   --  Extract the iterator from the given GValue.

   function To_Address (Iter : Gtk_Tree_Iter) return System.Address;
   --  Return address of the specified iterator.

   function Get_Iter
     (Tree_Model : access Gtk_Tree_Model_Record;
      Path       : Gtk_Tree_Path) return Gtk_Tree_Iter;
   --  Return an iterator pointing to Path.
   --  Null_Iter is returned if Path was invalid or no iterator could be set.

   function Get_Iter_From_String
     (Tree_Model  : access Gtk_Tree_Model_Record;
      Path_String : String) return Gtk_Tree_Iter;
   --  Return an iterator pointing to Path_String.
   --  Null_Iter is returned if Path was invalid or no iterator could be set.

   function Get_String_From_Iter
     (Tree_Model : access Gtk_Tree_Model_Record;
      Iter       : Gtk_Tree_Iter) return String;
   --  Generates a string representation of the iter. This string is a ':'
   --  separated list of numbers. For example, "4:10:0:3" would be an
   --  acceptable return value for this string.

   function Get_Iter_First
     (Tree_Model : access Gtk_Tree_Model_Record) return Gtk_Tree_Iter;
   --  Return an iterator pointing to the root of Tree_Model.
   --  Null_Iter is returned if Tree_Model is empty.

   function Get_Path
     (Tree_Model : access Gtk_Tree_Model_Record;
      Iter       : Gtk_Tree_Iter) return Gtk_Tree_Path;
   --  Return a newly created Gtk_Tree_Path referenced by Iter.
   --  This path must be freed with Path_Free.

   procedure Next
     (Tree_Model : access Gtk_Tree_Model_Record;
      Iter       : in out Gtk_Tree_Iter);
   --  Sets Iter to point to the node following it at the current level.
   --  If there is none, Iter is set to Null_Iter.

   function Children
     (Tree_Model : access Gtk_Tree_Model_Record;
      Parent     : Gtk_Tree_Iter) return Gtk_Tree_Iter;
   --  Return the first child of Parent.
   --  If Parent has no children, return Null_Iter.
   --  Parent will remain a valid node after this function has been called.

   function Has_Child
     (Tree_Model : access Gtk_Tree_Model_Record;
      Iter       : Gtk_Tree_Iter) return Boolean;
   --  Return True if Iter has children, False otherwise.

   function N_Children
     (Tree_Model : access Gtk_Tree_Model_Record;
      Iter       : Gtk_Tree_Iter := Null_Iter) return Gint;
   --  Return the number of children that Iter has.
   --  As a special case, if Iter is Null_Iter, then the number of toplevel
   --  nodes is returned.

   function Nth_Child
     (Tree_Model : access Gtk_Tree_Model_Record;
      Parent     : Gtk_Tree_Iter;
      N          : Gint) return Gtk_Tree_Iter;
   --  Return the child of Parent, using the given index.
   --  The First index is 0. If Index is too big, or Parent has no children,
   --  return Null_Iter.
   --  If Parent is Null_Iter, then the nth root node is set.

   function Parent
     (Tree_Model : access Gtk_Tree_Model_Record;
      Child      : Gtk_Tree_Iter) return Gtk_Tree_Iter;
   --  Return the parent of Child.
   --  If Child is at the toplevel, and doesn't have a parent, then Null_Iter
   --  is returned.

   procedure Ref_Node
     (Tree_Model : access Gtk_Tree_Model_Record;
      Iter       : Gtk_Tree_Iter);
   --  Let the tree reference the node.
   --  This is an optional method for models to implement.
   --  To be more specific, models may ignore this call as it exists primarily
   --  for performance reasons. This function is primarily meant as a way for
   --  views to let caching model know when nodes are being displayed (and
   --  hence, whether or not to cache that node). For example, a file-system
   --  based model would not want to keep the entire file-hierarchy in memory,
   --  just the sections that are currently being displayed by every current
   --  view.

   procedure Unref_Node
     (Tree_Model : access Gtk_Tree_Model_Record;
      Iter       : Gtk_Tree_Iter);
   --  Let the tree unref the node.
   --  This is an optional method for models to implement. To be more specific,
   --  models may ignore this call as it exists primarily for performance
   --  reasons. For more information on what this means, please see
   --  Tree_Model_Ref_Node. Please note that nodes that are deleted are not
   --  unreferenced.

   procedure Get_Value
     (Tree_Model : access Gtk_Tree_Model_Record;
      Iter       : Gtk_Tree_Iter;
      Column     : Gint;
      Value      : out Glib.Values.GValue);
   --  Get a value from the model, at column Column and line Iter.
   --  Value must be freed by the caller.

   function Get_Int
     (Tree_Model : access Gtk_Tree_Model_Record;
      Iter       : Gtk_Tree_Iter;
      Column     : Gint) return Gint;
   --  Get the int value of one cell in the row referenced by Iter.

   function Get_Boolean
     (Tree_Model : access Gtk_Tree_Model_Record;
      Iter       : Gtk_Tree_Iter;
      Column     : Gint) return Boolean;
   --  Get the boolean value of one cell in the row referenced by Iter.

   function Get_Object
     (Tree_Model : access Gtk_Tree_Model_Record;
      Iter       : Gtk_Tree_Iter;
      Column     : Gint) return Glib.Object.GObject;
   --  Get the object value of one cell in the row referenced by Iter.

   function Get_C_Proxy
     (Tree_Model : access Gtk_Tree_Model_Record;
      Iter       : Gtk_Tree_Iter;
      Column     : Gint) return Glib.C_Proxy;
   --  Get the address value of one cell in the row referenced by Iter.

   function Get_String
     (Tree_Model : access Gtk_Tree_Model_Record;
      Iter       : Gtk_Tree_Iter;
      Column     : Gint) return UTF8_String;
   --  Get the string stored at a specific location in the model.

   function Get_Address
     (Tree_Model : access Gtk_Tree_Model_Record;
      Iter       : Gtk_Tree_Iter;
      Column     : Gint) return System.Address;
   --  Get the pointer stored at a specific location in the model.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "row_changed"
   --    procedure Handler
   --       (Tree_Model : access Gtk_Tree_Model_Record'Class;
   --        Path       : Gtk_Tree_Path;
   --        Iter       : Gtk_Tree_Iter);
   --    This signal should be emitted every time the contents of a row (any
   --    column) has changed. This forces the tree_view to refresh the display.
   --
   --  - "row_inserted"
   --    procedure Handler
   --      (Tree_Model : access Gtk_Tree_Model_Record'Class;
   --       Path       : Gtk_Tree_Path;
   --       Iter       : Gtk_Tree_Iter);
   --    This signal should be emitted every time a new row has been inserted.
   --
   --  - "row_has_child_toggled"
   --    procedure Handler
   --      (Tree_Model : access Gtk_Tree_Model_Record'Class;
   --       Path       : Gtk_Tree_Path;
   --       Iter       : Gtk_Tree_Iter);
   --    This should be emitted by models after the child state of a node
   --    changes.
   --
   --  - "row_deleted"
   --    procedure Handler
   --      (Tree_Model : access Gtk_Tree_Model_Record'Class;
   --       Path       : Gtk_Tree_Path);
   --    This should be emitted by models after the child state of a node
   --    changes.
   --
   --  - "rows_reordered"
   --    procedure Handler
   --      (Tree_Model : access Gtk_Tree_Model_Record'Class;
   --       Path       : Gtk_Tree_Path;
   --       Iter       : Gtk_Tree_Iter;
   --       New_Order  : Gint_Array);
   --   This should be emitted when the rows have been reordered
   --
   --  </signals>

   procedure Row_Changed
     (Tree_Model : access Gtk_Tree_Model_Record'Class;
      Path       : Gtk_Tree_Path;
      Iter       : Gtk_Tree_Iter);
   --  Emit the "row_changed" signal.

   procedure Row_Inserted
     (Tree_Model : access Gtk_Tree_Model_Record'Class;
      Path       : Gtk_Tree_Path;
      Iter       : Gtk_Tree_Iter);
   --  Emit the "row_inserted" signal.

   procedure Row_Has_Child_Toggled
     (Tree_Model : access Gtk_Tree_Model_Record'Class;
      Path       : Gtk_Tree_Path;
      Iter       : Gtk_Tree_Iter);
   --  Emit the "row_has_child_toggled" signal.

   procedure Row_Deleted
     (Tree_Model : access Gtk_Tree_Model_Record'Class;
      Path       : Gtk_Tree_Path);
   --  Emit the "row_has_child_toggled" signal.

   procedure Rows_Reordered
     (Tree_Model : access Gtk_Tree_Model_Record'Class;
      Path       : Gtk_Tree_Path;
      Iter       : Gtk_Tree_Iter;
      New_Order  : Gint_Array);
   --  Emit the "rows_reordered" signal

   Signal_Row_Changed           : constant Glib.Signal_Name :=
                                    "row_changed";
   Signal_Row_Inserted          : constant Glib.Signal_Name :=
                                    "row_inserted";
   Signal_Row_Has_Child_Toggled : constant Glib.Signal_Name :=
                                    "row_has_child_toggled";
   Signal_Row_Deleted           : constant Glib.Signal_Name :=
                                    "row_deleted";
   Signal_Rows_Reordered        : constant Glib.Signal_Name :=
                                    "rows_reordered";

private
   pragma Convention (C, Tree_Model_Flags);

   pragma Convention (C, To_Address);
   --  Note: To_Address needs a pass-by-reference semantic to work properly
   --  On some ABIs (e.g. IA64), Gtk_Tree_Iter is passed by copy, since it's
   --  a "small enough" record.

   Tree_Model_Iters_Persist : constant Tree_Model_Flags := 2 ** 0;
   Tree_Model_List_Only     : constant Tree_Model_Flags := 2 ** 1;

   type Gtk_Tree_Model_Record is new Glib.Object.GObject_Record
     with null record;

   type Gtk_Tree_Iter is record
      Stamp      : Gint;
      User_Data  : System.Address;
      User_Data2 : System.Address;
      User_Data3 : System.Address;
   end record;
   pragma Convention (C, Gtk_Tree_Iter);

   Null_Iter : constant Gtk_Tree_Iter :=
     (0, System.Null_Address, System.Null_Address, System.Null_Address);

   pragma Import (C, Get_Type,      "gtk_tree_model_get_type");
   pragma Import (C, Row_Reference_Free, "gtk_tree_row_reference_free");
   pragma Import (C, Append_Index,  "gtk_tree_path_append_index");
   pragma Import (C, Prepend_Index, "gtk_tree_path_prepend_index");
   pragma Import (C, Get_Depth,     "gtk_tree_path_get_depth");
   pragma Import (C, Path_Free,     "gtk_tree_path_free");
   pragma Import (C, Copy,          "gtk_tree_path_copy");
   pragma Import (C, Compare,       "gtk_tree_path_compare");
   pragma Import (C, Down,          "gtk_tree_path_down");
   pragma Import (C, Iter_Get_Type, "gtk_tree_iter_get_type");
   pragma Import (C, Path_Get_Type, "gtk_tree_path_get_type");
   pragma Import (C, Gtk_New_First, "gtk_tree_path_new_first");
   pragma Import (C, Iter_Copy,     "ada_tree_iter_copy");
   pragma Import (C, Row_Reference_Copy, "gtk_tree_row_reference_copy");
   pragma Import
     (C, Row_Reference_Get_Type, "gtk_tree_row_reference_get_type");

   pragma Import (C, Set_Tree_Iter, "g_value_set_pointer");
   --  External binding: g_value_set_pointer

end Gtk.Tree_Model;

--  This function is not intended to be used by applications anyway
--  No binding: gtk_tree_iter_copy
--  No binding: gtk_tree_iter_free

--  variable number of arguments, no convenient binding
--  No binding: gtk_tree_model_get
--  No binding: gtk_tree_model_get_valist
--  No binding: gtk_tree_path_new_from_indices

--  Not needed by most applications, in fact, only for low-level monitoring:
--  No binding: gtk_tree_row_reference_deleted
--  No binding: gtk_tree_row_reference_inserted
--  No binding: gtk_tree_row_reference_new_proxy
--  No binding: gtk_tree_row_reference_reordered
