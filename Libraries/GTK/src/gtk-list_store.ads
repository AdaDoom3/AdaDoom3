-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                      Copyright (C) 2001-2006                      --
--                               AdaCore                             --
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
--  This package implements a specific model to store your data in. Each
--  item in the list will correspond to one line in the tree view. Multiple
--  columns can be displayed for each line.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Trees and Lists</group>

with Gdk.Pixbuf;
with Glib.Types;
with Glib.Values;
with Gtk;
with Gtk.Tree_Dnd;
with Gtk.Tree_Model;
with Gtk.Tree_Sortable;

package Gtk.List_Store is

   type Gtk_List_Store_Record is
     new Gtk.Tree_Model.Gtk_Tree_Model_Record with null record;
   type Gtk_List_Store is access all Gtk_List_Store_Record'Class;

   procedure Gtk_New
     (List_Store : out Gtk_List_Store;
      Types      : GType_Array);
   --  Creates a new list store using Types to fill the columns.

   procedure Initialize
     (List_Store : access Gtk_List_Store_Record'Class;
      Types      : GType_Array);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   procedure Set_Column_Types
     (List_Store : access Gtk_List_Store_Record;
      Types      : GType_Array);

   procedure Set_Value
     (List_Store : access Gtk_List_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : Glib.Values.GValue);
   --  Set the data in the cell specified by Iter and Column.
   --  The type of Value must be convertible to the type of the column.

   procedure Set
     (Tree_Store : access Gtk_List_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : UTF8_String);
   --  Same as above, for an UTF8 string.

   procedure Set
     (Tree_Store : access Gtk_List_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : Gint);
   --  Same as above, for a Gint.

   procedure Set
     (Tree_Store : access Gtk_List_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : Gdk.Pixbuf.Gdk_Pixbuf);
   --  Same as above for a pixbuf

   procedure Set
     (Tree_Store : access Gtk_List_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Gint;
      Value      : Boolean);
   --  Same as above for a boolean

   procedure Remove
     (List_Store : access Gtk_List_Store_Record;
      Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Remove the given row from the list store.
   --  After being removed, Iter is set to be the next valid row, or
   --  invalidated if it pointed to the last row in List_Store.

   procedure Insert
     (List_Store : access Gtk_List_Store_Record;
      Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter;
      Position   : Gint);
   --  Create a new row at Position.
   --  Iter will be changed to point to this new row.
   --  If Position is larger than the number of rows on the list, then the new
   --  row will be appended to the list. The row will be empty before this
   --  function is called. To fill in values, you need to call Set_Value.

   procedure Insert_Before
     (List_Store : access Gtk_List_Store_Record;
      Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter;
      Sibling    : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Insert a new row before Sibling.
   --  If Sibling is Null_Iter, then the row will be appended to the end of the
   --  list. Iter will be changed to point to this new row. The row will be
   --  empty before this function is called. To fill in values, you need to
   --  call Set_Value.

   procedure Insert_After
     (List_Store : access Gtk_List_Store_Record;
      Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter;
      Sibling    : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Insert a new row after Sibling.
   --  If Sibling is Null_Iter, then the row will be prepended to the beginning
   --  of the list. Iter will be changed to point to this new row. The row will
   --  be empty after this function is called. To fill in values, you need to
   --  call Set_Value.

   procedure Insert_With_Valuesv
     (List_Store : access Gtk_List_Store_Record;
      Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter;
      Position   : Glib.Gint;
      Columns    : Glib.Gint_Array;
      Values     : Glib.Values.GValue_Array);
   --  Creates a new row at Position. Iter will be changed to point to this new
   --  row. If Position is larger than the number of rows on the list, then the
   --  new row will be appended to the list. The row will be filled with the
   --  values given to this function.
   --  Using this function is more efficient that calling Insert and then
   --  Set for each column, since that will not emit the rows_reordered signal
   --  when the model is sorted.

   procedure Prepend
     (List_Store : access Gtk_List_Store_Record;
      Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Prepend a new row to List_Store.
   --  Iter will be changed to point to this new row. The row will be empty
   --  after this function is called. To fill in values, you need to call
   --  Set_Value.

   procedure Append
     (List_Store : access Gtk_List_Store_Record;
      Iter       : in out Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Append a new row to List_Store.
   --  Iter will be changed to point to this new row. The row will be empty
   --  after this function is called. To fill in values, you need to call
   --  Set_Value.

   procedure Clear (List_Store : access Gtk_List_Store_Record);
   --  Remove all the rows in List_Store.

   function Iter_Is_Valid
     (List_Store : access Gtk_List_Store_Record;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Boolean;
   --  WARNING: This function is slow. Only use it for debugging and/or testing
   --  purposes.
   --  Checks if the given iter is a valid iter for List_Store.

   procedure Move_After
     (Store    : access Gtk_List_Store_Record;
      Iter     : Gtk.Tree_Model.Gtk_Tree_Iter;
      Position : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Moves the row pointed to by Iter to the position after Position. Note
   --  that this function only works with unsorted stores. If Position is
   --  Null_Iter, Iter will be moved to the start of the list.

   procedure Move_Before
     (Store    : access Gtk_List_Store_Record;
      Iter     : Gtk.Tree_Model.Gtk_Tree_Iter;
      Position : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Moves the row pointed to by Iter to the position before Position. Note
   --  that this function only works with unsorted stores. If Position is
   --  Null_Iter, Iter will be moved to the end of the list.

   procedure Reorder
     (Store     : access Gtk_List_Store_Record;
      New_Order : Glib.Gint_Array);
   --  Reorders Store to follow the order indicated by New_order. Note that
   --  this function only works with unsorted stores.
   --  New_Order is an array of integers mapping the new position of each child
   --  to its old position before the re-ordering,
   --  i.e. New_Order[newpos] = oldpos

   procedure Swap
     (Store : access Gtk_List_Store_Record;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Swaps the rwos pointed to by A and B. Note that this function only works
   --  with unsorted stores.

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
      Gtk_List_Store_Record,
      Gtk_List_Store);
   function "+"
     (Model : access Gtk_List_Store_Record'Class)
      return Gtk.Tree_Sortable.Gtk_Tree_Sortable
      renames Implements_Tree_Sortable.To_Interface;
   function "-"
     (Sortable : Gtk.Tree_Sortable.Gtk_Tree_Sortable)
      return Gtk_List_Store
      renames Implements_Tree_Sortable.To_Object;
   --  Converts to and from the Gtk_Tree_Sortable interface

   package Implements_Drag_Source is new Glib.Types.Implements
     (Gtk.Tree_Dnd.Gtk_Tree_Drag_Source,
      Gtk_List_Store_Record,
      Gtk_List_Store);
   function "+"
     (Model : access Gtk_List_Store_Record'Class)
      return Gtk.Tree_Dnd.Gtk_Tree_Drag_Source
      renames Implements_Drag_Source.To_Interface;
   function "-"
     (Drag_Source : Gtk.Tree_Dnd.Gtk_Tree_Drag_Source)
      return Gtk_List_Store
      renames Implements_Drag_Source.To_Object;
   --  Converts to and from the Gtk_Tree_Drag_Source interface

   package Implements_Drag_Dest is new Glib.Types.Implements
     (Gtk.Tree_Dnd.Gtk_Tree_Drag_Dest,
      Gtk_List_Store_Record,
      Gtk_List_Store);
   function "+"
     (Model : access Gtk_List_Store_Record'Class)
      return Gtk.Tree_Dnd.Gtk_Tree_Drag_Dest
      renames Implements_Drag_Dest.To_Interface;
   function "-"
     (Drag_Dest : Gtk.Tree_Dnd.Gtk_Tree_Drag_Dest)
      return Gtk_List_Store
      renames Implements_Drag_Dest.To_Object;
   --  Converts to and from the Gtk_Tree_Drag_Source interface

private

   pragma Import (C, Get_Type, "gtk_list_store_get_type");
end Gtk.List_Store;

--  No binding: gtk_list_store_new
--  No binding: gtk_list_store_set
--  No binding: gtk_list_store_set_valist
--  No binding: gtk_list_store_insert_with_values
