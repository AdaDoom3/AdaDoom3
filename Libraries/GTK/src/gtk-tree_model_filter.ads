-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2006 AdaCore                         --
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
--  A Gtk_Tree_Model_Filter is a tree model which wraps another tree model, and
--  can do the following things:
--
--  - Filter specific rows, based on data from a "visible column", a column
--    storing booleans indicating whether the row should be filtered or not, or
--    based on the return value of a "visible function", which gets a model,
--    iter and user_data and returns a boolean indicating whether the row
--    should be filtered or not.
--
--  - Modify the "appearance" of the model, using a modify function. This is
--    extremely powerful and allows for just changing some values and also for
--    creating a completely different model based on the given child model.
--
--  - Set a different root node, also known as a "virtual root". You can pass
--    in a Gtk_Tree_Path indicating the root node for the filter at
--    construction time.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Trees and Lists</group>
--  <testgtk>create_tree_filter.adb</testgtk>

with Glib.Types;
with Glib.Values;
with Gtk.Tree_Dnd;
with Gtk.Tree_Model;

package Gtk.Tree_Model_Filter is
   type Gtk_Tree_Model_Filter_Record is
     new Gtk.Tree_Model.Gtk_Tree_Model_Record with null record;
   type Gtk_Tree_Model_Filter is access all Gtk_Tree_Model_Filter_Record'Class;

   procedure Gtk_New
     (Model       : out Gtk_Tree_Model_Filter;
      Child_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Root        : Gtk.Tree_Model.Gtk_Tree_Path := null);
   procedure Initialize
     (Model       : access Gtk_Tree_Model_Filter_Record'Class;
      Child_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Root        : Gtk.Tree_Model.Gtk_Tree_Path := null);
   --  Creates a new tree model, with Child_Model as the child_model
   --  and Root as the virtual root (or the same root as Child_Model by
   --  default).

   function Get_Type return Glib.GType;
   --  Returns the internal type used for a Gtk_Tree_Model_Filter

   -----------------
   -- Child model --
   -----------------
   --  The tree model filter wraps another model, and offers functions to
   --  convert from one to the other. Generally speaking, you can change data
   --  on either of the two models, and these changes will be reflected
   --  graphically automatically.

   function Get_Model
     (Filter : access Gtk_Tree_Model_Filter_Record)
      return Gtk.Tree_Model.Gtk_Tree_Model;
   --  Returns a pointer to the child model of Filter.

   procedure Convert_Child_Iter_To_Iter
     (Filter      : access Gtk_Tree_Model_Filter_Record;
      Filter_Iter : out Gtk.Tree_Model.Gtk_Tree_Iter;
      Child_Iter  : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Sets Filter_Iter to point to the row in Filter that corresponds to the
   --  row pointed at by Child_Iter.

   function Convert_Child_Path_To_Path
     (Filter     : access Gtk_Tree_Model_Filter_Record;
      Child_Path : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Path;
   --  Converts Child_Path to a path relative to Filter. That is, Child_Path
   --  points to a path in the child model. The returned path will point to the
   --  same row in the filtered model. If Child_Path isn't a valid path on the
   --  child model, then null is returned.
   --  The returned value must be freed with Path_Free.

   procedure Convert_Iter_To_Child_Iter
     (Filter      : access Gtk_Tree_Model_Filter_Record;
      Child_Iter  : out Gtk.Tree_Model.Gtk_Tree_Iter;
      Filter_Iter : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Sets Child_Iter to point to the row pointed to by Filter_Iter.

   function Convert_Path_To_Child_Path
     (Filter      : access Gtk_Tree_Model_Filter_Record;
      Filter_Path : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Path;
   --  Converts Filter_Path to a path on the child model of Filter. That is,
   --  Filter_Path points to a location in Filter. The returned path will point
   --  to the same location in the model not being filtered. If Filter_Path
   --  does not point to a location in the child model, null is returned.
   --  The returned value must be freed with Path_Free.

   --------------------------
   --  Changing visibility --
   --------------------------
   --  One of the capabilities of a Gtk_Tree_Model_Filter is to hide some of
   --  the rows of its child model, so that they are not visible on the screen.

   procedure Set_Visible_Column
     (Filter : access Gtk_Tree_Model_Filter_Record; Column : Gint);
   --  Sets Column of the child_model to be the column where Filter should
   --  look for visibility information. Columns should be a column of type
   --  GType_Boolean, where True means that a row is visible, and False
   --  if not.

   type Gtk_Tree_Model_Filter_Visible_Func is access function
     (Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   --  Called for each row in the model to decide whether or not it should be
   --  visible. True indicates the row should be made visible.
   --  Model is the child model, and Iter points into it.

   procedure Set_Visible_Func
     (Filter  : access Gtk_Tree_Model_Filter_Record;
      Func    : Gtk_Tree_Model_Filter_Visible_Func);
   --  Sets the visible function used when filtering the Filter to be Func. The
   --  function should return True if the given row should be visible and False
   --  otherwise.
   --  If the condition calculated by the function changes over time (e.g.
   --  because it depends on some global parameters), you must call Refilter to
   --  keep the visibility information of the model uptodate.

   generic
      type Data_Type (<>) is private;
   package Visible_Funcs is
      type Gtk_Tree_Model_Filter_Visible_Func is access function
        (Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
         Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
         Data  : Data_Type) return Boolean;

      type Destroy_Notify is access procedure (Data : in out Data_Type);
      --  Destroys the memory allocated for Data

      procedure Set_Visible_Func
        (Filter  : access Gtk_Tree_Model_Filter_Record'Class;
         Func    : Gtk_Tree_Model_Filter_Visible_Func;
         Data    : Data_Type;
         Destroy : Destroy_Notify := null);
      --  Same as above, but the application can pass addition data to the
      --  function

   private
      --  <doc_ignore>
      type Data_Type_Access is access Data_Type;
      type Data_Type_Record is record
         Func    : Gtk_Tree_Model_Filter_Visible_Func;
         Destroy : Destroy_Notify;
         Data    : Data_Type_Access;
      end record;
      type Data_Type_Record_Access is access Data_Type_Record;
      pragma Convention (C, Data_Type_Record_Access);

      procedure Internal_Destroy_Notify (Data : Data_Type_Record_Access);
      pragma Convention (C, Internal_Destroy_Notify);

      function Internal_Filter_Visible_Func
        (Model : System.Address;
         Iter  : System.Address;
         Data  : Data_Type_Record_Access) return Gboolean;
      pragma Convention (C, Internal_Filter_Visible_Func);

      --  </doc_ignore>
   end Visible_Funcs;

   procedure Refilter (Filter : access Gtk_Tree_Model_Filter_Record);
   --  Emits row_changed for each row in the child model, which causes
   --  the filter to re-evaluate whether a row is visible or not.

   --------------------------------
   -- Modifying displayed values --
   --------------------------------
   --  The other capability of a Gtk_Tree_Model_Filter is to modify on the fly
   --  the displayed value (ie we do not display directly what is in the child
   --  model, but change the value in memory, not in the model, on the fly)

   type Gtk_Tree_Model_Filter_Modify_Func is access procedure
     (Model  : access Gtk_Tree_Model_Filter_Record'Class;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Value  : out Glib.Values.GValue;
      Column : Gint);
   --  A function which calculates display values from raw values in the model.
   --  It must fill value with the display value for the column column in the
   --  row indicated by iter.
   --  Since this function is called for each data access, it's not a
   --  particularly efficient operation.
   --  Value has already been initializes to the right type (ie the one defined
   --  in Set_Modify_Func for this column). Iter references the filter model,
   --  not the child model. When implementating this procedure, make sure you
   --  do not call Get_String, Get_Int,... on Model itself, since that would
   --  create a recursion. You must apply all operations to Get_Model (Model),
   --  after converting Iter to a Child_Iter through Convert_Iter_To_Child_Iter

   procedure Set_Modify_Func
     (Filter    : access Gtk_Tree_Model_Filter_Record;
      Types     : Glib.GType_Array;
      Func      : Gtk_Tree_Model_Filter_Modify_Func);
   --  Types can be used to override the column types that will be made visible
   --  to the parent model/view.
   --  Func is used to specify the modify function. The modify function will
   --  get called for *each* data access, the goal of the modify function is to
   --  return the data which should be displayed at the location specified
   --  using the parameters of the modify function.

   generic
      type Data_Type (<>) is private;
   package Modify_Funcs is
      type Gtk_Tree_Model_Filter_Modify_Func is access procedure
        (Model     : access Gtk_Tree_Model_Filter_Record'Class;
         Iter      : Gtk.Tree_Model.Gtk_Tree_Iter;
         Value     : out Glib.Values.GValue;
         Column    : Gint;
         User_Data : Data_Type);

      type Destroy_Notify is access procedure (Data : in out Data_Type);
      --  Destroys the memory allocated for Data

      procedure Set_Modify_Func
        (Filter    : access Gtk_Tree_Model_Filter_Record'Class;
         Types     : Glib.GType_Array;
         Func      : Gtk_Tree_Model_Filter_Modify_Func;
         Data      : Data_Type;
         Destroy   : Destroy_Notify := null);
      --  Same as above, but the application can pass extra data to the
      --  function.

   private
      --  <doc_ignore>
      type Data_Type_Access is access Data_Type;
      type Data_Type_Record is record
         Func    : Gtk_Tree_Model_Filter_Modify_Func;
         Destroy : Destroy_Notify;
         Data    : Data_Type_Access;
      end record;
      type Data_Type_Record_Access is access Data_Type_Record;
      pragma Convention (C, Data_Type_Record_Access);

      procedure Internal_Destroy_Notify (Data : Data_Type_Record_Access);
      pragma Convention (C, Internal_Destroy_Notify);

      procedure Internal_Filter_Modify_Func
        (Model  : System.Address;
         Iter   : System.Address;
         Value  : out Glib.Values.GValue;
         Column : Gint;
         Data   : Data_Type_Record_Access);
      pragma Convention (C, Internal_Filter_Modify_Func);
      --  </doc_ignore>
   end Modify_Funcs;

   ----------
   -- Misc --
   ----------

   procedure Clear_Cache (Filter : access Gtk_Tree_Model_Filter_Record);
   --  This function should almost never be called. It clears the Filter of any
   --  cached iterators that haven't been reffed with Gtk.Tree_Model.Ref_Node.
   --  This might be useful if the child model being filtered is static (and
   --  doesn't change often) and there has been a lot of unreffed access to
   --  nodes. As a side effect of this function, all unrefed iters will be
   --  invalid.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Tree_Drag_Source"
   --    This interface allows this widget to act as a dnd source

   package Implements_Drag_Source is new Glib.Types.Implements
     (Gtk.Tree_Dnd.Gtk_Tree_Drag_Source,
      Gtk_Tree_Model_Filter_Record,
      Gtk_Tree_Model_Filter);
   function "+"
     (Model : access Gtk_Tree_Model_Filter_Record'Class)
      return Gtk.Tree_Dnd.Gtk_Tree_Drag_Source
      renames Implements_Drag_Source.To_Interface;
   function "-"
     (Drag_Source : Gtk.Tree_Dnd.Gtk_Tree_Drag_Source)
      return Gtk_Tree_Model_Filter
      renames Implements_Drag_Source.To_Object;
   --  Converts to and from the Gtk_Tree_Drag_Source interface

private
   pragma Import (C, Get_Type, "gtk_tree_model_filter_get_type");
end Gtk.Tree_Model_Filter;
