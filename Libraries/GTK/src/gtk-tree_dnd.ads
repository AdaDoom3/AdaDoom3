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

--  <description>
--  GTK+ supports Drag-and-Drop in tree views with a high-level and a low-level
--  API.
--
--  The low-level API consists of the GTK+ DND API, augmented by some treeview
--  utility functions: Gtk.Tree_View.Set_Drag_Dest_Row,
--  Gtk.Tree_View.Get_Drag_Dest_Row, Gtk.Tree_View.Get_Dest_Row_At_Pos,
--  Gtk.Tree_View.Create_Row_Drag_Icon, Set_Row_Drag_Data and
--  Get_Row_Drag_Data. This API leaves a lot of flexibility, but
--  nothing is done automatically, and implementing advanced features like
--  hover-to-open-rows or autoscrolling on top of this API is a lot of work.
--
--  On the other hand, if you write to the high-level API, then all the
--  bookkeeping of rows is done for you, as well as things like hover-to-open
--  and auto-scroll, but your models have to implement the Gtk_Tree_Drag_Source
--  and Gtk_Tree_Drag_Dest interfaces.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Trees and Lists</group>

with Glib.Types;
with Gtk.Selection;
with Gtk.Tree_Model;

package Gtk.Tree_Dnd is
   type Gtk_Tree_Drag_Source is new Glib.Types.GType_Interface;
   type Gtk_Tree_Drag_Dest   is new Glib.Types.GType_Interface;

   function Drag_Dest_Get_Type   return GType;
   function Drag_Source_Get_Type return GType;
   --  Return the low-level types associated with the interfaces

   function Drag_Dest_Drag_Data_Received
     (Drag_Dest      : Gtk_Tree_Drag_Dest;
      Dest           : Gtk.Tree_Model.Gtk_Tree_Path;
      Selection_Data : Gtk.Selection.Selection_Data)
      return Boolean;
   --  Asks the Drag_Dest to insert a row before the path Dest,
   --  deriving the contents of the row from Selection_Data. If Dest is
   --  outside the tree so that inserting before it is impossible, False
   --  will be returned. Also, False may be returned if the new row is
   --  not created for some model-specific reason.  Should robustly handle
   --  a Dest no longer found in the model!

   function Drag_Dest_Row_Drop_Possible
     (Drag_Dest      : Gtk_Tree_Drag_Dest;
      Dest_Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      Selection_Data : Gtk.Selection.Selection_Data)
      return Boolean;
   --  Determines whether a drop is possible before the given Dest_Path,
   --  at the same depth as Dest_Path. i.e., can we drop the data in
   --  Selection_Data at that location. Dest_Path does not have to
   --  exist; the return value will almost certainly be False if the
   --  parent of Dest_Path doesn't exist, though.

   function Drag_Source_Drag_Data_Delete
     (Drag_Source : Gtk_Tree_Drag_Source;
      Path        : Gtk.Tree_Model.Gtk_Tree_Path)
      return Boolean;
   --  Asks the Drag_Source to delete the row at Path, because
   --  it was moved somewhere else via drag-and-drop. Returns False
   --  if the deletion fails because Path no longer exists, or for
   --  some model-specific reason. Should robustly handle a Path no
   --  longer found in the model!

   function Drag_Source_Drag_Data_Get
     (Drag_Source    : Gtk_Tree_Drag_Source;
      Path           : Gtk.Tree_Model.Gtk_Tree_Path;
      Selection_Data : Gtk.Selection.Selection_Data)
      return Boolean;
   --  Asks the Drag_Source to fill in Selection_Data with a
   --  representation of the row at Path. Get_Target (Selection_Data) gives
   --  the required type of the data.  Should robustly handle a Path no
   --  longer found in the model!

   function Drag_Source_Row_Draggable
     (Drag_Source : Gtk_Tree_Drag_Source;
      Path        : Gtk.Tree_Model.Gtk_Tree_Path)
      return Boolean;
   --  Asks the Drag_Source whether a particular row can be used as
   --  the source of a DND operation. If the source doesn't implement
   --  this interface, the row is assumed draggable.

   procedure Get_Row_Drag_Data
     (Selection_Data : Gtk.Selection.Selection_Data;
      Tree_Model     : out Gtk.Tree_Model.Gtk_Tree_Model;
      Path           : out Gtk.Tree_Model.Gtk_Tree_Path;
      Success        : out Boolean);
   --  Obtains a Tree_Model and Path from selection data of target type
   --  GTK_TREE_MODEL_ROW. Normally called from a drag_data_received handler.
   --
   --  This function can only be used if Selection_Data originates from the
   --  same process that's calling this function, because a pointer to the tree
   --  model is being passed around. If you aren't in the same process, then
   --  you'll get memory corruption. In the Gtk_Tree_Drag_Dest
   --  drag_data_received handler, you can assume that selection data of type
   --  GTK_TREE_MODEL_ROW is in from the current process.
   --
   --  The returned path must be freed with

   function Set_Row_Drag_Data
     (Selection_Data : Gtk.Selection.Selection_Data;
      Tree_Model     : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Path           : Gtk.Tree_Model.Gtk_Tree_Path)
      return Boolean;
   --  Sets selection data of target type GTK_TREE_MODEL_ROW. Normally used
   --  in a drag_data_get handler.

private
   pragma Import (C, Drag_Dest_Get_Type, "gtk_tree_drag_dest_get_type");
   pragma Import (C, Drag_Source_Get_Type, "gtk_tree_drag_source_get_type");

end Gtk.Tree_Dnd;



