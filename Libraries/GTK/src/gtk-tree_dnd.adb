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

with Gtk.Selection;   use Gtk.Selection;
with Gtk.Tree_Model;  use Gtk.Tree_Model;

package body Gtk.Tree_Dnd is

   ----------------------------------
   -- Drag_Dest_Drag_Data_Received --
   ----------------------------------

   function Drag_Dest_Drag_Data_Received
     (Drag_Dest      : Gtk_Tree_Drag_Dest;
      Dest           : Gtk_Tree_Path;
      Selection_Data : Gtk.Selection.Selection_Data)
      return Boolean
   is
      function Internal
        (Drag_Dest      : Gtk_Tree_Drag_Dest;
         Dest           : Gtk_Tree_Path;
         Selection_Data : Gtk.Selection.Selection_Data)
         return Gboolean;
      pragma Import (C, Internal, "gtk_tree_drag_dest_drag_data_received");
   begin
      return Boolean'Val (Internal (Drag_Dest, Dest, Selection_Data));
   end Drag_Dest_Drag_Data_Received;

   ---------------------------------
   -- Drag_Dest_Row_Drop_Possible --
   ---------------------------------

   function Drag_Dest_Row_Drop_Possible
     (Drag_Dest      : Gtk_Tree_Drag_Dest;
      Dest_Path      : Gtk_Tree_Path;
      Selection_Data : Gtk.Selection.Selection_Data)
      return Boolean
   is
      function Internal
        (Drag_Dest      : Gtk_Tree_Drag_Dest;
         Dest_Path      : Gtk_Tree_Path;
         Selection_Data : Gtk.Selection.Selection_Data)
         return Gboolean;
      pragma Import (C, Internal, "gtk_tree_drag_dest_row_drop_possible");
   begin
      return Boolean'Val (Internal (Drag_Dest, Dest_Path, Selection_Data));
   end Drag_Dest_Row_Drop_Possible;

   ----------------------------------
   -- Drag_Source_Drag_Data_Delete --
   ----------------------------------

   function Drag_Source_Drag_Data_Delete
     (Drag_Source : Gtk_Tree_Drag_Source;
      Path        : Gtk_Tree_Path)
      return Boolean
   is
      function Internal
        (Drag_Source : Gtk_Tree_Drag_Source;
         Path        : Gtk_Tree_Path)
         return Gboolean;
      pragma Import (C, Internal, "gtk_tree_drag_source_drag_data_delete");
   begin
      return Boolean'Val (Internal (Drag_Source, Path));
   end Drag_Source_Drag_Data_Delete;

   -------------------------------
   -- Drag_Source_Drag_Data_Get --
   -------------------------------

   function Drag_Source_Drag_Data_Get
     (Drag_Source    : Gtk_Tree_Drag_Source;
      Path           : Gtk_Tree_Path;
      Selection_Data : Gtk.Selection.Selection_Data)
      return Boolean
   is
      function Internal
        (Drag_Source    : Gtk_Tree_Drag_Source;
         Path           : Gtk_Tree_Path;
         Selection_Data : Gtk.Selection.Selection_Data)
         return Gboolean;
      pragma Import (C, Internal, "gtk_tree_drag_source_drag_data_get");
   begin
      return Boolean'Val (Internal (Drag_Source, Path, Selection_Data));
   end Drag_Source_Drag_Data_Get;

   -------------------------------
   -- Drag_Source_Row_Draggable --
   -------------------------------

   function Drag_Source_Row_Draggable
     (Drag_Source : Gtk_Tree_Drag_Source;
      Path        : Gtk_Tree_Path)
      return Boolean
   is
      function Internal
        (Drag_Source : Gtk_Tree_Drag_Source;
         Path        : Gtk_Tree_Path)
         return Gboolean;
      pragma Import (C, Internal, "gtk_tree_drag_source_row_draggable");
   begin
      return Boolean'Val (Internal (Drag_Source, Path));
   end Drag_Source_Row_Draggable;

   -----------------------
   -- Get_Row_Drag_Data --
   -----------------------

   procedure Get_Row_Drag_Data
     (Selection_Data : Gtk.Selection.Selection_Data;
      Tree_Model     : out Gtk.Tree_Model.Gtk_Tree_Model;
      Path           : out Gtk.Tree_Model.Gtk_Tree_Path;
      Success        : out Boolean)
   is
      function Internal
        (Selection_Data : Gtk.Selection.Selection_Data;
         Model          : access System.Address;
         Path           : access Gtk_Tree_Path) return Gboolean;
      pragma Import (C, Internal, "gtk_tree_get_row_drag_data");
      M : aliased System.Address;
      P : aliased Gtk_Tree_Path;
      Stub : Gtk_Tree_Model_Record;
   begin
      Success := Boolean'Val
        (Internal (Selection_Data, M'Unchecked_Access, P'Unchecked_Access));
      Path := P;
      Tree_Model := Gtk_Tree_Model (Get_User_Data (M, Stub));
   end Get_Row_Drag_Data;

   -----------------------
   -- Set_Row_Drag_Data --
   -----------------------

   function Set_Row_Drag_Data
     (Selection_Data : Gtk.Selection.Selection_Data;
      Tree_Model     : access Gtk_Tree_Model_Record'Class;
      Path           : Gtk_Tree_Path)
      return Boolean
   is
      function Internal
        (Selection_Data : Gtk.Selection.Selection_Data;
         Tree_Model     : System.Address;
         Path           : Gtk_Tree_Path)
         return Gboolean;
      pragma Import (C, Internal, "gtk_tree_set_row_drag_data");
   begin
      return Boolean'Val
        (Internal (Selection_Data, Get_Object (Tree_Model), Path));
   end Set_Row_Drag_Data;

end Gtk.Tree_Dnd;
