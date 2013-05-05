-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2001-2007 AdaCore                    --
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
--  The Gtk_Tree_Selection object is a helper object to manage the selection
--  for a Gtk_Tree_View widget. The Gtk_Tree_Selection object is automatically
--  created when a new Gtk_Tree_View widget is created, and cannot exist
--  independentally of this widget. The primary reason the Gtk_Tree_Selection
--  objects exists is for cleanliness of code and API. That is, there is no
--  conceptual reason all these functions could not be methods on the
--  Gtk_Tree_View widget instead of separate function.
--
--  The Gtk_Tree_Selection object is gotten from a Gtk_Tree_View by calling
--  Gtk.Tree_View.Get_Selection. It can be manipulated to check the selection
--  status of the tree, as well as select and deselect individual rows.
--  Selection is done completely view side. As a result, multiple views of the
--  same model can have completely different selections. Additionally, you
--  cannot change the selection of a row on the model that is not currently
--  displayed by the view without expanding its parents first.
--
--  One of the important things to remember when monitoring the selection of a
--  view is that the "changed" signal is mostly a hint. That is, it may only
--  emit one signal when a range of rows is selected. Additionally, it may on
--  occasion emit a "changed" signal when nothing has happened (mostly as a
--  result of programmers calling select_row on an already selected row).
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Trees and Lists</group>

with Glib.Object;
with Gtk;
with Gtk.Enums;      use Gtk.Enums;
with Gtk.Tree_Model;
with Gtk.Widget;     use Gtk.Widget;

package Gtk.Tree_Selection is

   type Gtk_Tree_Selection_Record is
     new Glib.Object.GObject_Record with private;
   type Gtk_Tree_Selection is access all Gtk_Tree_Selection_Record'Class;

   function Get_Type return Glib.GType;
   --  Return the internal type associated with Gtk_Tree_Selection.

   procedure Set_Mode
     (Selection : access Gtk_Tree_Selection_Record'Class;
      The_Type  : Gtk_Selection_Mode);
   function Get_Mode
     (Selection : access Gtk_Tree_Selection_Record'Class)
      return Gtk_Selection_Mode;
   --  Set the selection mode of the Selection.
   --  If the previous type was Gtk_Selection_Multiple,
   --  then the anchor is kept selected, if it was  previously selected.

   function Get_Tree_View
     (Selection : access Gtk_Tree_Selection_Record'Class)
      return Gtk.Widget.Gtk_Widget;
   --  Return the tree view associated with Selection.

   function Count_Selected_Rows
     (Selection : access Gtk_Tree_Selection_Record) return Gint;
   --  Returns the number of rows that have been selected.

   procedure Get_Selected
     (Selection : access Gtk_Tree_Selection_Record'Class;
      Model     : out Gtk.Tree_Model.Gtk_Tree_Model;
      Iter      : out Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Set Iter to the currently selected node if Selection
   --  is set to Gtk_Selection_Single or Gtk_Selection_Browse.
   --  Iter is set to Null_Iter if no node is currently selected.
   --  Model is filled with the current model as a convenience. This function
   --  will not work if Selection is set to Gtk_Selection_Multiple.

   procedure Get_Selected_Rows
     (Selection : access Gtk_Tree_Selection_Record;
      Model     : out Gtk.Tree_Model.Gtk_Tree_Model;
      Path_List : out Gtk.Tree_Model.Gtk_Tree_Path_List.Glist);
   --  Creates a list of path of all selected rows. Additionally, if you are
   --  planning on modifying the model after calling this function, you may
   --  want to convert the returned list into a list of Gtk_Tree_Row_Reference.
   --
   --  You must free the resulting list by calling Path_Free on each item, and
   --  then freeing the list itself.

   generic
      type Data_Type is private;
   package Selection_Foreach is

      type Data_Type_Access is access all Data_Type;

      type Foreach_Func is access procedure
        (Model : Gtk.Tree_Model.Gtk_Tree_Model;
         Path  : Gtk.Tree_Model.Gtk_Tree_Path;
         Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
         Data  : Data_Type_Access);

      procedure Selected_Foreach
        (Selection : access Gtk_Tree_Selection_Record'Class;
         Func      : Foreach_Func;
         Data      : Data_Type_Access);
      --  Call Func for each selected node.

   end Selection_Foreach;

   procedure Select_Path
     (Selection : access Gtk_Tree_Selection_Record'Class;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path);
   procedure Unselect_Path
     (Selection : access Gtk_Tree_Selection_Record'Class;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path);
   --  Selects or unselects the row at path.

   function Path_Is_Selected
     (Selection : access Gtk_Tree_Selection_Record'Class;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path)
      return Boolean;
   --  Return True if the row pointed to by path is currently selected.
   --  If path does not point to a valid location, False is returned

   procedure Select_Iter
     (Selection : access Gtk_Tree_Selection_Record'Class;
      Iter      : Gtk.Tree_Model.Gtk_Tree_Iter);
   procedure Unselect_Iter
     (Selection : access Gtk_Tree_Selection_Record'Class;
      Iter      : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Selects or unselects the row pointed to by the specified iterator.

   function Iter_Is_Selected
     (Selection : access Gtk_Tree_Selection_Record'Class;
      Iter      : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Boolean;
   --  Return True if the row pointed to by path is currently selected.

   procedure Select_All (Selection : access Gtk_Tree_Selection_Record'Class);
   procedure Unselect_All (Selection : access Gtk_Tree_Selection_Record'Class);
   --  Selects or unselects all the nodes.
   --  Selection must be set to Gtk_Selection_Multiple mode.

   procedure Select_Range
     (Selection  : access Gtk_Tree_Selection_Record'Class;
      Start_Path : Gtk.Tree_Model.Gtk_Tree_Path;
      End_Path   : Gtk.Tree_Model.Gtk_Tree_Path);
   procedure Unselect_Range
     (Selection  : access Gtk_Tree_Selection_Record;
      Start_Path : Gtk.Tree_Model.Gtk_Tree_Path;
      End_Path   : Gtk.Tree_Model.Gtk_Tree_Path);
   --  Selects or unselects a range of nodes, determined by Start_Path and
   --  End_Path inclusive

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "changed"
   --    procedure Handler
   --      (Widget : access Gtk_Tree_Selection_Record'Class'Class);
   --    Emitted whenever the selection has (possibly) changed. Please note
   --    that this signal is mostly a hint. It may only be emitted once when a
   --    range of rows are selected, and it may occasionally be emitted when
   --    nothing has happened.
   --
   --  </signals>

   Signal_Changed : constant Glib.Signal_Name := "changed";

private
   type Gtk_Tree_Selection_Record is
     new Glib.Object.GObject_Record with null record;

   pragma Import (C, Get_Type, "gtk_tree_selection_get_type");

end Gtk.Tree_Selection;

--   missing:
--
--    procedure Set_Select_Function
--      (Selection : access Gtk_Tree_Selection_Record'Class;
--       Func      : Gtk_Tree_Selection_Func;
--       Data      : gpointer;
--       Destroy   : Gtk_Destroy_Notify);
--
--    function Get_User_Data
--      (Selection : access Gtk_Tree_Selection_Record'Class) return gpointer;
