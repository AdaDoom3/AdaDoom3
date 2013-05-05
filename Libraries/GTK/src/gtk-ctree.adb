-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                 Copyright (C) 2000-2013, AdaCore                  --
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

with System;
with Unchecked_Deallocation;
with Gtk.Style;    use Gtk.Style;

with Ada.Unchecked_Conversion;

with Glib.Type_Conversion_Hooks;

package body Gtk.Ctree is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Ctree_Record);
   pragma Warnings (Off, Type_Conversion);

   Compare_Drag_Func_Key : constant String :=
                             "_GtkAda_Ctree_Compare_Drag_Func" & ASCII.NUL;

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Ctree_Compare_Drag_Func, System.Address);
   function From_Address is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Ctree_Compare_Drag_Func);
   --
   --  The key that will be used to store the address of the Ada
   --  Compare_Drag_Func function.

   --  Local Declarations...
   --

   function C_Compare_Drag_Func
     (Ctree       : in System.Address;
      Source_Node : in Gtk_Ctree_Node;
      New_Parent  : in Gtk_Ctree_Node;
      New_Sibling : in Gtk_Ctree_Node) return Gboolean;
   pragma Convention (C, C_Compare_Drag_Func);

   --------------
   -- Collapse --
   --------------

   procedure Collapse (Ctree : access Gtk_Ctree_Record;
                       Node  : in     Gtk_Ctree_Node)
   is
      procedure Internal (Ctree : in System.Address;
                          Node  : in Gtk_Ctree_Node);
      pragma Import (C, Internal, "gtk_ctree_collapse");
   begin
      Internal (Get_Object (Ctree), Node);
   end Collapse;

   ------------------------
   -- Collapse_Recursive --
   ------------------------

   procedure Collapse_Recursive
     (Ctree : access Gtk_Ctree_Record;
      Node  : in     Gtk_Ctree_Node := null)
   is
      procedure Internal (Ctree : in System.Address;
                          Node  : in Gtk_Ctree_Node);
      pragma Import (C, Internal, "gtk_ctree_collapse_recursive");
   begin
      Internal (Get_Object (Ctree), Node);
   end Collapse_Recursive;

   -----------------------
   -- Collapse_To_Depth --
   -----------------------

   procedure Collapse_To_Depth
     (Ctree : access Gtk_Ctree_Record;
      Node  : in     Gtk_Ctree_Node := null;
      Depth : in     Gint)
   is
      procedure Internal (Ctree : in System.Address;
                          Node  : in Gtk_Ctree_Node;
                          Depth : in Gint);
      pragma Import (C, Internal, "gtk_ctree_collapse_to_depth");
   begin
      Internal (Get_Object (Ctree), Node, Depth);
   end Collapse_To_Depth;

   ------------
   -- Expand --
   ------------

   procedure Expand (Ctree : access Gtk_Ctree_Record;
                     Node  : in     Gtk_Ctree_Node)
   is
      procedure Internal (Ctree : in System.Address;
                          Node  : in Gtk_Ctree_Node);
      pragma Import (C, Internal, "gtk_ctree_expand");
   begin
      Internal (Get_Object (Ctree), Node);
   end Expand;

   ----------------------
   -- Expand_Recursive --
   ----------------------

   procedure Expand_Recursive
     (Ctree : access Gtk_Ctree_Record;
      Node  : in Gtk_Ctree_Node := null)
   is
      procedure Internal (Ctree : in System.Address;
                          Node  : in Gtk_Ctree_Node);
      pragma Import (C, Internal, "gtk_ctree_expand_recursive");
   begin
      Internal (Get_Object (Ctree), Node);
   end Expand_Recursive;

   ---------------------
   -- Expand_To_Depth --
   ---------------------

   procedure Expand_To_Depth
     (Ctree : access Gtk_Ctree_Record;
      Node  : in     Gtk_Ctree_Node := null;
      Depth : in     Gint)
   is
      procedure Internal (Ctree : in System.Address;
                          Node  : in Gtk_Ctree_Node;
                          Depth : in Gint);
      pragma Import (C, Internal, "gtk_ctree_expand_to_depth");
   begin
      Internal (Get_Object (Ctree), Node, Depth);
   end Expand_To_Depth;

   ----------
   -- Find --
   ----------

   function Find (Ctree : access Gtk_Ctree_Record;
                  Node  : in     Gtk_Ctree_Node;
                  Child : in     Gtk_Ctree_Node) return Boolean is
      function Internal (Ctree : in System.Address;
                         Node  : in Gtk_Ctree_Node;
                         Child : in Gtk_Ctree_Node) return Gboolean;
      pragma Import (C, Internal, "gtk_ctree_find");
   begin
      return Internal (Get_Object (Ctree), Node, Child) /= 0;
   end Find;

   -------------------
   -- Find_Node_Ptr --
   -------------------

   function Find_Node_Ptr (Ctree     : access Gtk_Ctree_Record;
                           Ctree_Row : in     Gtk_Ctree_Row)
     return Gtk_Ctree_Node is
      function Internal (Ctree     : in System.Address;
                         Ctree_Row : in Gtk_Ctree_Row) return Gtk_Ctree_Node;
      pragma Import (C, Internal, "gtk_ctree_find_node_ptr");
   begin
      return Internal (Get_Object (Ctree), Ctree_Row);
   end Find_Node_Ptr;

   ------------------------
   -- Get_Expander_Style --
   ------------------------

   function Get_Expander_Style (Ctree : access Gtk_Ctree_Record)
                                return         Gtk_Ctree_Expander_Style
   is
      function Internal (Ctree : in System.Address) return Gint;
      pragma Import (C, Internal, "ada_ctree_get_expander_style");
   begin
      return Gtk_Ctree_Expander_Style'Val (Internal (Get_Object (Ctree)));
   end Get_Expander_Style;

   --------------------
   -- Get_Line_Style --
   --------------------

   function Get_Line_Style (Ctree : access Gtk_Ctree_Record)
                            return         Gtk_Ctree_Line_Style
   is
      function Internal (Ctree : in System.Address) return Gint;
      pragma Import (C, Internal, "ada_ctree_get_line_style");
   begin
      return Gtk_Ctree_Line_Style'Val (Internal (Get_Object (Ctree)));
   end Get_Line_Style;

   -------------------
   -- Get_Node_Info --
   -------------------

   procedure Get_Node_Info
     (Ctree         : access Gtk_Ctree_Record;
      Node          : in     Gtk_Ctree_Node;
      Text          :    out Interfaces.C.Strings.chars_ptr;
      Spacing       :    out Guint8;
      Pixmap_Closed :    out Gdk.Pixmap.Gdk_Pixmap;
      Mask_Closed   :    out Gdk.Bitmap.Gdk_Bitmap;
      Pixmap_Opened :    out Gdk.Pixmap.Gdk_Pixmap;
      Mask_Opened   :    out Gdk.Bitmap.Gdk_Bitmap;
      Is_Leaf       :    out Boolean;
      Expanded      :    out Boolean;
      Success       :    out Boolean)
   is
      function Internal
        (Ctree         : in System.Address;
         Node          : in Gtk_Ctree_Node;
         Text          : in System.Address;
         Spacing       : in System.Address;
         Pixmap_Closed : in System.Address;
         Mask_Closed   : in System.Address;
         Pixmap_Opened : in System.Address;
         Mask_Opened   : in System.Address;
         Is_Leaf       : in System.Address;
         Expanded      : in System.Address) return Gint;
      pragma Import (C, Internal, "gtk_ctree_get_node_info");

      Tmp_Is_Leaf, Tmp_Expanded : Gboolean;
      Tmp_Pixmap_Closed : aliased Gdk.Pixmap.Gdk_Pixmap;
      Tmp_Mask_Closed : aliased Gdk.Bitmap.Gdk_Bitmap;
      Tmp_Pixmap_Opened : aliased Gdk.Pixmap.Gdk_Pixmap;
      Tmp_Mask_Opened : aliased Gdk.Bitmap.Gdk_Bitmap;

   begin
      Success := Internal
        (Get_Object (Ctree),
         Node,
         Text'Address,
         Spacing'Address,
         Tmp_Pixmap_Closed'Address,
         Tmp_Mask_Closed'Address,
         Tmp_Pixmap_Opened'Address,
         Tmp_Mask_Opened'Address,
         Tmp_Is_Leaf'Address,
         Tmp_Expanded'Address) /= 0;
      Pixmap_Closed := Tmp_Pixmap_Closed;
      Mask_Closed := Tmp_Mask_Closed;
      Pixmap_Opened := Tmp_Pixmap_Opened;
      Mask_Opened := Tmp_Mask_Opened;
      Is_Leaf := Tmp_Is_Leaf /= 0;
      Expanded := Tmp_Expanded /= 0;
   end Get_Node_Info;

   -------------------
   -- Get_Node_List --
   -------------------

   function Get_Node_List
     (Ctree : access Gtk_Ctree_Record) return Node_List.Glist
   is
      function Internal (Ctree : in System.Address) return System.Address;
      pragma Import (C, Internal, "ada_clist_get_row_list");
      --  Note: We need to extract "GTK_CLIST (Ctree)->row_list", which
      --  explains why the imported C routine applies to a clist.
      --  Note also that the row_list is actually a GtkCtreeNode list...
      List : Node_List.Glist;

   begin
      Node_List.Set_Object (List, Internal (Get_Object (Ctree)));
      return List;
   end Get_Node_List;

   ------------------
   -- Get_Row_List --
   ------------------

   function Get_Row_List
     (Ctree : access Gtk_Ctree_Record) return Row_List.Glist
   is
      function Internal (Ctree : in System.Address) return System.Address;
      pragma Import (C, Internal, "ada_clist_get_row_list");
      --  Note: We need to extract "GTK_CLIST (Ctree)->row_list", which
      --  explains why the imported C routine applies to a clist. Also, we
      --  have to override the Get_Row_List function inherited from Clist
      --  because we need to return a list of Gtk_Ctree_Row (The inherited
      --  Get_Row_List would return a list of Gtk_Clist_Row).
      List : Row_List.Glist;

   begin
      Row_List.Set_Object (List, Internal (Get_Object (Ctree)));
      return List;
   end Get_Row_List;

   -------------------
   -- Get_Selection --
   -------------------

   function Get_Selection
     (Ctree : access Gtk_Ctree_Record) return Node_List.Glist
   is
      function Internal (Widget : in System.Address) return System.Address;
      pragma Import (C, Internal, "ada_clist_get_selection");
      List : Node_List.Glist;

   begin
      Node_List.Set_Object (List, Internal (Get_Object (Ctree)));
      return List;
   end Get_Selection;

   -------------------
   -- Get_Show_Stub --
   -------------------

   function Get_Show_Stub (Ctree : access Gtk_Ctree_Record) return Boolean
   is
      function Internal (Ctree : in System.Address) return Gboolean;
      pragma Import (C, Internal, "ada_ctree_get_show_stub");
   begin
      return Internal (Get_Object (Ctree)) /= 0;
   end Get_Show_Stub;

   ---------------------
   -- Get_Tree_Column --
   ---------------------

   function Get_Tree_Column (Widget : access Gtk.Ctree.Gtk_Ctree_Record'Class)
                             return          Gint
   is
      function Internal (Widget : in System.Address) return Gint;
      pragma Import (C, Internal, "ada_ctree_get_tree_column");
   begin
      return Internal (Get_Object (Widget));
   end Get_Tree_Column;

   ----------------
   -- Get_Indent --
   ----------------

   function Get_Indent (Widget : access Gtk.Ctree.Gtk_Ctree_Record'Class)
     return Gint
   is
      function Internal (Widget : in System.Address) return Gint;
      pragma Import (C, Internal, "ada_ctree_get_tree_indent");
   begin
      return Internal (Get_Object (Widget));
   end Get_Indent;

   -----------------
   -- Get_Spacing --
   -----------------

   function Get_Spacing (Widget : access Gtk.Ctree.Gtk_Ctree_Record'Class)
     return Gint
   is
      function Internal (Widget : in System.Address) return Gint;
      pragma Import (C, Internal, "ada_ctree_get_tree_spacing");
   begin
      return Internal (Get_Object (Widget));
   end Get_Spacing;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget      :    out Gtk_Ctree;
                      Titles      : in     Chars_Ptr_Array;
                      Tree_Column : in     Gint := 0) is
   begin
      Widget := new Gtk_Ctree_Record;
      Initialize (Widget, Titles, Tree_Column);
   end Gtk_New;

   procedure Gtk_New (Widget      :    out Gtk_Ctree;
                      Columns     : in     Gint;
                      Tree_Column : in     Gint := 0)
   is
   begin
      Widget := new Gtk_Ctree_Record;
      Initialize (Widget, Columns, Tree_Column);
   end Gtk_New;

   ----------------
   -- Gtk_Select --
   ----------------

   procedure Gtk_Select (Ctree : access Gtk_Ctree_Record;
                         Node  : in     Gtk_Ctree_Node)
   is
      procedure Internal (Ctree : in System.Address;
                          Node  : in Gtk_Ctree_Node);
      pragma Import (C, Internal, "gtk_ctree_select");
   begin
      Internal (Get_Object (Ctree), Node);
   end Gtk_Select;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget      : access Gtk_Ctree_Record'Class;
                         Titles      : in     Chars_Ptr_Array;
                         Tree_Column : in     Gint := 0)
   is
      function Internal (Columns     : in Gint;
                         Tree_Column : in Gint;
                         Titles      : in System.Address)
                         return           System.Address;
      pragma Import (C, Internal, "gtk_ctree_new_with_titles");
   begin
      Set_Object (Widget,
                  Internal (Titles'Length, Tree_Column, Titles'Address));
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget      : access Gtk_Ctree_Record'Class;
                         Columns     : in     Gint;
                         Tree_Column : in     Gint := 0)
   is
      function Internal (Columns     : in Gint;
                         Tree_Column : in Gint)
                         return           System.Address;
      pragma Import (C, Internal, "gtk_ctree_new");
   begin
      Set_Object (Widget, Internal (Columns, Tree_Column));
   end Initialize;

   -----------------
   -- Insert_Node --
   -----------------

   function Insert_Node
     (Ctree         : access Gtk_Ctree_Record;
      Parent        : in     Gtk_Ctree_Node;
      Sibling       : in     Gtk_Ctree_Node;
      Text          : in     Chars_Ptr_Array;
      Spacing       : in     Guint8;
      Pixmap_Closed : in     Gdk.Pixmap.Gdk_Pixmap;
      Mask_Closed   : in     Gdk.Bitmap.Gdk_Bitmap;
      Pixmap_Opened : in     Gdk.Pixmap.Gdk_Pixmap;
      Mask_Opened   : in     Gdk.Bitmap.Gdk_Bitmap;
      Is_Leaf       : in     Boolean;
      Expanded      : in     Boolean)
      return Gtk_Ctree_Node
   is
      function Internal
        (Ctree         : in System.Address;
         Parent        : in Gtk_Ctree_Node;
         Sibling       : in Gtk_Ctree_Node;
         Text          : in Chars_Ptr_Array;
         Spacing       : in Guint8;
         Pixmap_Closed : in Gdk.Pixmap.Gdk_Pixmap;
         Mask_Closed   : in Gdk.Bitmap.Gdk_Bitmap;
         Pixmap_Opened : in Gdk.Pixmap.Gdk_Pixmap;
         Mask_Opened   : in Gdk.Bitmap.Gdk_Bitmap;
         Is_Leaf       : in Gint;
         Expanded      : in Gint)
         return Gtk_Ctree_Node;
      pragma Import (C, Internal, "gtk_ctree_insert_node");

   begin
      return Internal (Get_Object (Ctree),
                       Parent,
                       Sibling,
                       Text,
                       Spacing,
                       Pixmap_Closed,
                       Mask_Closed,
                       Pixmap_Opened,
                       Mask_Opened,
                       To_Gint (Is_Leaf),
                       To_Gint (Expanded));
   end Insert_Node;

   -----------------
   -- Is_Ancestor --
   -----------------

   function Is_Ancestor (Ctree  : access Gtk_Ctree_Record;
                         Node   : in     Gtk_Ctree_Node;
                         Child  : in     Gtk_Ctree_Node)
                         return Boolean
   is
      function Internal (Ctree  : in System.Address;
                         Node   : in Gtk_Ctree_Node;
                         Child  : in Gtk_Ctree_Node)
                         return Gboolean;
      pragma Import (C, Internal, "gtk_ctree_is_ancestor");

   begin
      return Internal (Get_Object (Ctree), Node, Child) /= 0;
   end Is_Ancestor;

   ----------------
   -- Is_Created --
   ----------------

   function Is_Created (Node : in Gtk_Ctree_Node) return Boolean is
   begin
      return Node /= null;
   end Is_Created;

   -----------------
   -- Is_Hot_Spot --
   -----------------

   function Is_Hot_Spot (Ctree  : access Gtk_Ctree_Record;
                         X      : in     Gint;
                         Y      : in     Gint)
                         return          Boolean
   is
      function Internal (Ctree  : in System.Address;
                         X      : in Gint;
                         Y      : in Gint)
                         return      Gboolean;
      pragma Import (C, Internal, "gtk_ctree_is_hot_spot");
   begin
      return Internal (Get_Object (Ctree), X, Y) /= 0;
   end Is_Hot_Spot;

   -----------------
   -- Is_Viewable --
   -----------------

   function Is_Viewable (Ctree  : access Gtk_Ctree_Record;
                         Node   : in     Gtk_Ctree_Node)
                         return Boolean
   is
      function Internal (Ctree  : in System.Address;
                         Node   : in Gtk_Ctree_Node)
                         return Gboolean;
      pragma Import (C, Internal, "gtk_ctree_is_viewable");

   begin
      return Internal (Get_Object (Ctree), Node) /= 0;
   end Is_Viewable;

   ----------
   -- Last --
   ----------

   function Last (Ctree  : access Gtk_Ctree_Record;
                  Node   : in     Gtk_Ctree_Node)
                  return          Gtk_Ctree_Node
   is
      function Internal
        (Ctree  : in System.Address;
         Node   : in Gtk_Ctree_Node) return Gtk_Ctree_Node;
      pragma Import (C, Internal, "gtk_ctree_last");

   begin
      return Internal (Get_Object (Ctree), Node);
   end Last;

   ----------
   -- Move --
   ----------

   procedure Move (Ctree       : access Gtk_Ctree_Record;
                   Node        : in     Gtk_Ctree_Node;
                   New_Parent  : in     Gtk_Ctree_Node;
                   New_Sibling : in     Gtk_Ctree_Node)
   is
      procedure Internal (Ctree       : in System.Address;
                          Node        : in Gtk_Ctree_Node;
                          New_Parent  : in Gtk_Ctree_Node;
                          New_Sibling : in Gtk_Ctree_Node);
      pragma Import (C, Internal, "gtk_ctree_move");

   begin
      Internal (Get_Object (Ctree), Node, New_Parent, New_Sibling);
   end Move;

   -------------------------
   -- Node_Get_Cell_Style --
   -------------------------

   function Node_Get_Cell_Style (Ctree  : access Gtk_Ctree_Record;
                                 Node   : in     Gtk_Ctree_Node;
                                 Column : in     Gint)
                                 return          Gtk.Style.Gtk_Style
   is
      function Internal (Ctree  : in System.Address;
                         Node   : in Gtk_Ctree_Node;
                         Column : in Gint)
                         return System.Address;
      pragma Import (C, Internal, "gtk_ctree_node_get_cell_style");
      Stub : Gtk_Style_Record;
   begin
      return Gtk_Style
        (Get_User_Data (Internal (Get_Object (Ctree), Node, Column), Stub));
   end Node_Get_Cell_Style;

   ------------------------
   -- Node_Get_Cell_Type --
   ------------------------

   pragma Warnings (Off); --  Gtk.CList is obsolescent
   function Node_Get_Cell_Type (Ctree  : access Gtk_Ctree_Record;
                                Node   : in     Gtk_Ctree_Node;
                                Column : in     Gint)
                                return Gtk.Clist.Gtk_Cell_Type
   is
      function Internal (Ctree  : in System.Address;
                         Node   : in Gtk_Ctree_Node;
                         Column : in Gint) return Gint;
      pragma Import (C, Internal, "gtk_ctree_node_get_cell_type");

   begin
      return Gtk.Clist.Gtk_Cell_Type'Val
        (Internal (Get_Object (Ctree), Node, Column));
   end Node_Get_Cell_Type;
   pragma Warnings (On);

   ---------------------
   -- Node_Get_Pixmap --
   ---------------------

   procedure Node_Get_Pixmap
     (Ctree   : access Gtk_Ctree_Record;
      Node    : in     Gtk_Ctree_Node;
      Column  : in     Gint;
      Pixmap  :    out Gdk.Pixmap.Gdk_Pixmap;
      Mask    :    out Gdk.Bitmap.Gdk_Bitmap;
      Success :    out Boolean)
   is
      function Internal
        (Ctree  : in System.Address;
         Node   : in Gtk_Ctree_Node;
         Column : in Gint;
         Pixmap : in System.Address;
         Mask   : in System.Address) return Gint;
      pragma Import (C, Internal, "gtk_ctree_node_get_pixmap");

      Pixmap_Address : aliased Gdk.Pixmap.Gdk_Pixmap;
      Mask_Address : aliased Gdk.Bitmap.Gdk_Bitmap;

   begin
      Success := Internal
        (Get_Object (Ctree), Node, Column,
         Pixmap_Address'Address, Mask_Address'Address) /= 0;
      Pixmap := Pixmap_Address;
      Mask := Mask_Address;
   end Node_Get_Pixmap;

   ----------------------
   -- Node_Get_Pixtext --
   ----------------------

   procedure Node_Get_Pixtext
     (Ctree   : access Gtk_Ctree_Record;
      Node    : in     Gtk_Ctree_Node;
      Column  : in     Gint;
      Text    :    out Interfaces.C.Strings.chars_ptr;
      Spacing :    out Guint8;
      Pixmap  :    out Gdk.Pixmap.Gdk_Pixmap;
      Mask    :    out Gdk.Bitmap.Gdk_Bitmap;
      Success :    out Boolean)
   is
      function Internal
        (Ctree   : in System.Address;
         Node    : in Gtk_Ctree_Node;
         Column  : in Gint;
         Text    : in System.Address;
         Spacing : in System.Address;
         Pixmap  : in System.Address;
         Mask    : in System.Address) return Gint;
      pragma Import (C, Internal, "gtk_ctree_node_get_pixtext");

      Pixmap_Address : aliased Gdk.Pixmap.Gdk_Pixmap;
      Mask_Address : aliased Gdk.Bitmap.Gdk_Bitmap;

   begin
      Success := Internal
        (Get_Object (Ctree), Node, Column, Text'Address, Spacing'Address,
         Pixmap_Address'Address, Mask_Address'Address) /= 0;
      Pixmap := Pixmap_Address;
      Mask := Mask_Address;
   end Node_Get_Pixtext;

   ------------------------
   -- Node_Get_Row_Style --
   ------------------------

   function Node_Get_Row_Style
     (Ctree  : access Gtk_Ctree_Record;
      Node   : in     Gtk_Ctree_Node)
      return Gtk.Style.Gtk_Style
   is
      function Internal (Ctree  : in System.Address;
                         Node   : in Gtk_Ctree_Node)
                        return System.Address;
      pragma Import (C, Internal, "gtk_ctree_node_get_row_style");
      Stub : Gtk_Style_Record;
   begin
      return Gtk_Style
        (Get_User_Data (Internal (Get_Object (Ctree), Node), Stub));
   end Node_Get_Row_Style;

   -------------------------
   -- Node_Get_Selectable --
   -------------------------

   function Node_Get_Selectable (Ctree  : access Gtk_Ctree_Record;
                                 Node   : in     Gtk_Ctree_Node)
                                 return          Boolean
   is
      function Internal (Ctree  : in System.Address;
                         Node   : in Gtk_Ctree_Node)
                         return      Gint;
      pragma Import (C, Internal, "gtk_ctree_node_get_selectable");

   begin
      return Internal (Get_Object (Ctree), Node) /= 0;
   end Node_Get_Selectable;

   -------------------
   -- Node_Get_Text --
   -------------------

   function Node_Get_Text (Ctree   : access Gtk_Ctree_Record;
                           Node    : in     Gtk_Ctree_Node;
                           Column  : in     Gint) return UTF8_String
   is
      function Internal
        (Ctree  : in System.Address;
         Node   : in Gtk_Ctree_Node;
         Column : in Gint;
         Text   : access Interfaces.C.Strings.chars_ptr) return Gint;
      pragma Import (C, Internal, "gtk_ctree_node_get_text");

      function Internal2
        (Clist   : in System.Address;
         Node    : in Gtk_Ctree_Node;
         Column  : in Gint;
         Text    : access Interfaces.C.Strings.chars_ptr;
         Spacing : in System.Address;
         Pixmap  : in System.Address;
         Mask    : access System.Address) return Gint;
      pragma Import (C, Internal2, "gtk_ctree_node_get_pixtext");

      Mask : aliased System.Address;
      Text : aliased Interfaces.C.Strings.chars_ptr;
      Success : Gint;

      pragma Warnings (Off); --  Gtk.CList is obsolescent
      use Gtk.Clist;
      pragma Warnings (On);

   begin
      if Node_Get_Cell_Type (Ctree, Node, Column) = Cell_Text then
         Success := Internal (Get_Object (Ctree), Node, Column, Text'Access);
      else
         Success := Internal2
           (Get_Object (Ctree), Node, Column, Text'Access,
            System.Null_Address, System.Null_Address, Mask'Access);
      end if;

      if Success = 0 then
         return "";
      else
         return Interfaces.C.Strings.Value (Text);
      end if;
   end Node_Get_Text;

   ---------------------
   -- Node_Is_Visible --
   ---------------------

   function Node_Is_Visible (Ctree  : access Gtk_Ctree_Record;
                             Node   : in     Gtk_Ctree_Node)
                             return          Gtk_Visibility
   is
      function Internal (Ctree  : in System.Address;
                         Node   : in Gtk_Ctree_Node)
                         return      Gint;
      pragma Import (C, Internal, "gtk_ctree_node_is_visible");
   begin
      return Gtk_Visibility'Val (Internal (Get_Object (Ctree), Node));
   end Node_Is_Visible;

   -----------------
   -- Node_Moveto --
   -----------------

   procedure Node_Moveto (Ctree     : access Gtk_Ctree_Record;
                          Node      : in     Gtk_Ctree_Node;
                          Column    : in     Gint;
                          Row_Align : in     Gfloat := 0.5;
                          Col_Align : in     Gfloat := 0.5)
   is
      procedure Internal (Ctree     : in System.Address;
                          Node      : in Gtk_Ctree_Node;
                          Column    : in Gint;
                          Row_Align : in Gfloat;
                          Col_Align : in Gfloat);
      pragma Import (C, Internal, "gtk_ctree_node_moveto");

   begin
      Internal (Get_Object (Ctree), Node, Column, Row_Align, Col_Align);
   end Node_Moveto;

   --------------
   -- Node_Nth --
   --------------

   function Node_Nth (Ctree  : access Gtk_Ctree_Record;
                      Row    : in     Guint)
                      return          Gtk_Ctree_Node
   is
      function Internal
        (Ctree  : in System.Address;
         Row    : in Guint) return Gtk_Ctree_Node;
      pragma Import (C, Internal, "gtk_ctree_node_nth");

   begin
      return Internal (Get_Object (Ctree), Row);
   end Node_Nth;

   -------------------------
   -- Node_Set_Background --
   -------------------------

   procedure Node_Set_Background (Ctree : access Gtk_Ctree_Record;
                                  Node  : in     Gtk_Ctree_Node;
                                  Color : in     Gdk.Color.Gdk_Color)
   is
      procedure Internal (Ctree : in System.Address;
                          Node  : in Gtk_Ctree_Node;
                          Color : in System.Address);
      pragma Import (C, Internal, "gtk_ctree_node_set_background");
      use type Gdk.Color.Gdk_Color;
      Color_A : System.Address := Color'Address;

   begin
      if Color = Gdk.Color.Null_Color then
         Color_A := System.Null_Address;
      end if;

      Internal (Get_Object (Ctree), Node, Color_A);
   end Node_Set_Background;

   -------------------------
   -- Node_Set_Cell_Style --
   -------------------------

   procedure Node_Set_Cell_Style (Ctree  : access Gtk_Ctree_Record;
                                  Node   : in     Gtk_Ctree_Node;
                                  Column : in     Gint;
                                  Style  : in     Gtk.Style.Gtk_Style)
   is
      procedure Internal (Ctree  : in System.Address;
                          Node   : in Gtk_Ctree_Node;
                          Column : in Gint;
                          Style  : in System.Address);
      pragma Import (C, Internal, "gtk_ctree_node_set_cell_style");

   begin
      Internal (Get_Object (Ctree), Node, Column, Get_Object (Style));
   end Node_Set_Cell_Style;

   -------------------------
   -- Node_Set_Foreground --
   -------------------------

   procedure Node_Set_Foreground (Ctree : access Gtk_Ctree_Record;
                                  Node  : in     Gtk_Ctree_Node;
                                  Color : in     Gdk.Color.Gdk_Color)
   is
      procedure Internal (Ctree : in System.Address;
                          Node  : in Gtk_Ctree_Node;
                          Color : in System.Address);
      pragma Import (C, Internal, "gtk_ctree_node_set_foreground");
      use type Gdk.Color.Gdk_Color;
      Color_A : System.Address := Color'Address;

   begin
      if Color = Gdk.Color.Null_Color then
         Color_A := System.Null_Address;
      end if;

      Internal (Get_Object (Ctree), Node, Color_A);
   end Node_Set_Foreground;

   ---------------------
   -- Node_Set_Pixmap --
   ---------------------

   procedure Node_Set_Pixmap (Ctree  : access Gtk_Ctree_Record;
                              Node   : in     Gtk_Ctree_Node;
                              Column : in     Gint;
                              Pixmap : in     Gdk.Pixmap.Gdk_Pixmap;
                              Mask   : in     Gdk.Bitmap.Gdk_Bitmap)
   is
      procedure Internal (Ctree  : in System.Address;
                          Node   : in Gtk_Ctree_Node;
                          Column : in Gint;
                          Pixmap : in Gdk.Pixmap.Gdk_Pixmap;
                          Mask   : in Gdk.Bitmap.Gdk_Bitmap);
      pragma Import (C, Internal, "gtk_ctree_node_set_pixmap");

   begin
      Internal (Get_Object (Ctree), Node, Column, Pixmap, Mask);
   end Node_Set_Pixmap;

   ----------------------
   -- Node_Set_Pixtext --
   ----------------------

   procedure Node_Set_Pixtext (Ctree   : access Gtk_Ctree_Record;
                               Node    : in     Gtk_Ctree_Node;
                               Column  : in     Gint;
                               Text    : in     UTF8_String;
                               Spacing : in     Guint8;
                               Pixmap  : in     Gdk.Pixmap.Gdk_Pixmap;
                               Mask    : in     Gdk.Bitmap.Gdk_Bitmap)
   is
      procedure Internal (Ctree   : in System.Address;
                          Node    : in Gtk_Ctree_Node;
                          Column  : in Gint;
                          Text    : in UTF8_String;
                          Spacing : in Guint8;
                          Pixmap  : in Gdk.Pixmap.Gdk_Pixmap;
                          Mask    : in Gdk.Bitmap.Gdk_Bitmap);
      pragma Import (C, Internal, "gtk_ctree_node_set_pixtext");

   begin
      Internal
        (Get_Object (Ctree), Node, Column, Text & ASCII.NUL, Spacing,
         Pixmap, Mask);
   end Node_Set_Pixtext;

   ------------------------
   -- Node_Set_Row_Style --
   ------------------------

   procedure Node_Set_Row_Style (Ctree : access Gtk_Ctree_Record;
                                 Node  : in     Gtk_Ctree_Node;
                                 Style : in     Gtk.Style.Gtk_Style)
   is
      procedure Internal (Ctree : in System.Address;
                          Node  : in Gtk_Ctree_Node;
                          Style : in System.Address);
      pragma Import (C, Internal, "gtk_ctree_node_set_row_style");

   begin
      Internal (Get_Object (Ctree), Node, Get_Object (Style));
   end Node_Set_Row_Style;

   -------------------------
   -- Node_Set_Selectable --
   -------------------------

   procedure Node_Set_Selectable (Ctree      : access Gtk_Ctree_Record;
                                  Node       : in Gtk_Ctree_Node;
                                  Selectable : in Boolean := True)
   is
      procedure Internal (Ctree      : in System.Address;
                          Node       : in Gtk_Ctree_Node;
                          Selectable : in Gboolean);
      pragma Import (C, Internal, "gtk_ctree_node_set_selectable");

   begin
      Internal (Get_Object (Ctree), Node, Boolean'Pos (Selectable));
   end Node_Set_Selectable;

   --------------------
   -- Node_Set_Shift --
   --------------------

   procedure Node_Set_Shift (Ctree      : access Gtk_Ctree_Record;
                             Node       : in     Gtk_Ctree_Node;
                             Column     : in     Gint;
                             Vertical   : in     Gint;
                             Horizontal : in     Gint)
   is
      procedure Internal (Ctree      : in System.Address;
                          Node       : in Gtk_Ctree_Node;
                          Column     : in Gint;
                          Vertical   : in Gint;
                          Horizontal : in Gint);
      pragma Import (C, Internal, "gtk_ctree_node_set_shift");

   begin
      Internal (Get_Object (Ctree), Node, Column, Vertical, Horizontal);
   end Node_Set_Shift;

   -------------------
   -- Node_Set_Text --
   -------------------

   procedure Node_Set_Text (Ctree  : access Gtk_Ctree_Record;
                            Node   : in     Gtk_Ctree_Node;
                            Column : in     Gint;
                            Text   : in     UTF8_String)
   is
      procedure Internal (Ctree  : in System.Address;
                          Node   : in Gtk_Ctree_Node;
                          Column : in Gint;
                          Text   : in UTF8_String);
      pragma Import (C, Internal, "gtk_ctree_node_set_text");

   begin
      Internal (Get_Object (Ctree), Node, Column, Text & ASCII.NUL);
   end Node_Set_Text;

   ---------------------------
   -- Real_Select_Recursive --
   ---------------------------

   procedure Real_Select_Recursive (Ctree     : access Gtk_Ctree_Record;
                                    Node      : in     Gtk_Ctree_Node := null;
                                    Do_Select : in     Boolean)
   is
      procedure Internal (Ctree : in System.Address;
                          Node  : in Gtk_Ctree_Node;
                          State : in Gint);
      pragma Import (C, Internal, "gtk_ctree_real_select_recursive");

   begin
      Internal (Get_Object (Ctree), Node, Boolean'Pos (Do_Select));
   end Real_Select_Recursive;

   -----------------
   -- Remove_Node --
   -----------------

   procedure Remove_Node (Ctree : access Gtk_Ctree_Record;
                          Node  : in     Gtk_Ctree_Node)
   is
      procedure Internal (Ctree : in System.Address;
                          Node  : in Gtk_Ctree_Node);
      pragma Import (C, Internal, "gtk_ctree_remove_node");

   begin
      Internal (Get_Object (Ctree), Node);
   end Remove_Node;

   ----------------------
   -- Row_Get_Expanded --
   ----------------------

   function Row_Get_Expanded (Row : in Gtk_Ctree_Row) return Boolean is
      function Internal (Row : in Gtk_Ctree_Row) return Guint;
      pragma Import (C, Internal, "ada_ctree_row_get_expanded");
   begin
      return Internal (Row) /= 0;
   end Row_Get_Expanded;

   ---------------------
   -- Row_Get_Is_Leaf --
   ---------------------

   function Row_Get_Is_Leaf (Row : in Gtk_Ctree_Row) return Boolean is
      function Internal (Row : in Gtk_Ctree_Row) return Guint;
      pragma Import (C, Internal, "ada_ctree_row_get_is_leaf");
   begin
      return Internal (Row) /= 0;
   end Row_Get_Is_Leaf;

   ----------------------
   -- Select_Recursive --
   ----------------------

   procedure Select_Recursive
     (Ctree : access Gtk_Ctree_Record;
      Node  : in     Gtk_Ctree_Node := null)
   is
      procedure Internal (Ctree : in System.Address;
                          Node  : in Gtk_Ctree_Node);
      pragma Import (C, Internal, "gtk_ctree_select_recursive");
   begin
      Internal (Get_Object (Ctree), Node);
   end Select_Recursive;

   ---------------------------
   -- Set_Drag_Compare_Func --
   ---------------------------

   --  Note : Inside Set_Drag_Compare_Func, the type of the data stored
   --         by Set_User_Data should be System.Address. It is assumed
   --         that it is ok to replace it by a Gtk_Ctree_Compare_Drag_Func.
   --
   --         Also, inside the C_Compare_Drag_Func, it is assumed that
   --         it is equivalent to replace the return type of Get_User_Data
   --         by Gtk_Ctree_Compare_Drag_Func (instead of System.Address)
   --
   --         This avoids unnecessary conversions.

   function C_Compare_Drag_Func
     (Ctree       : in System.Address;
      Source_Node : in Gtk_Ctree_Node;
      New_Parent  : in Gtk_Ctree_Node;
      New_Sibling : in Gtk_Ctree_Node) return Gboolean is
      --
      --  The C_Compare_Drag_Func is the "C" function that is stored
      --  in the cmp_func field of the Ctree object. It therefore maps
      --  exactly the profile expected by Gtk+.
      --
      --  What it does, when it is called, is to retrieve the address
      --  of the user provided Ada compare function that is stored in
      --  the user data area, using the Compare_Drag_Func_Key. It then
      --  invokes it to perform the comparison.

      function Get_User_Data
        (Object : in System.Address;
         Key    : in String) return System.Address;
      pragma Import (C, Get_User_Data, "gtk_object_get_data");
      --  External binding: gtk_object_get_data

      Local_Ctree_Stub : Gtk_Ctree_Record;
      Local_Ctree : constant Gtk_Ctree :=
        Gtk_Ctree (Get_User_Data (Ctree, Local_Ctree_Stub));

      Cmp_Func : constant Gtk_Ctree_Compare_Drag_Func := From_Address
        (Get_User_Data (Object => Ctree, Key => Compare_Drag_Func_Key));

   begin
      return Boolean'Pos
        (Cmp_Func (Local_Ctree, Source_Node, New_Parent, New_Sibling));
   end C_Compare_Drag_Func;

   ---------------------------
   -- Set_Drag_Compare_Func --
   ---------------------------

   procedure Set_Drag_Compare_Func
     (Ctree    : access Gtk_Ctree_Record;
      Cmp_Func : in     Gtk_Ctree_Compare_Drag_Func) is

      procedure Internal (Ctree    : in System.Address;
                          Cmp_Func : in System.Address);
      pragma Import (C, Internal, "gtk_ctree_set_drag_compare_func");

      procedure Set_User_Data (Object : in System.Address;
                               Name   : in String;
                               Data   : in System.Address);
      pragma Import (C, Set_User_Data, "gtk_object_set_data");
      --  External binding: gtk_object_set_data

   begin
      if Cmp_Func = null then
         Internal (Get_Object (Ctree), System.Null_Address);
      else
         Set_User_Data
           (Get_Object (Ctree), Compare_Drag_Func_Key,
            To_Address (Cmp_Func)
           );
         Internal (Get_Object (Ctree), C_Compare_Drag_Func'Address);
         --
         --  It is not possible to store directly the Ada Cmp_Func into
         --  the cmp_func field of the Ctree, because the profile of
         --  Cmp_Func is not the same as the profile expected by Gtk+
         --  (Gtk+ uses 'System.Address'es, whereas GtkAda uses nice Ada
         --  structures).
         --
         --  We therefore store the address of the Cmp_Func inside the
         --  user data area using the Compare_Drag_Func_Key, and provide
         --  Gtk+ with an internal compare function that matches the expected
         --  profile. This internal function will then retrieve the address
         --  of our Cmp_Func, transform all the System.Address parameters in
         --  GtkAda structures, and then invoke it.

      end if;
   end Set_Drag_Compare_Func;

   ------------------------
   -- Set_Expander_Style --
   ------------------------

   procedure Set_Expander_Style
     (Ctree          : access Gtk_Ctree_Record;
      Expander_Style : in     Gtk_Ctree_Expander_Style :=
        Ctree_Expander_Square)
   is
      procedure Internal
        (Ctree          : System.Address;
         Expander_Style : Gtk_Ctree_Expander_Style);
      pragma Import (C, Internal, "gtk_ctree_set_expander_style");

   begin
      Internal (Get_Object (Ctree), Expander_Style);
   end Set_Expander_Style;

   ----------------
   -- Set_Indent --
   ----------------

   procedure Set_Indent (Ctree  : access Gtk_Ctree_Record;
                         Indent : in     Gint := 20)
   is
      procedure Internal (Ctree  : in System.Address;
                          Indent : in Gint);
      pragma Import (C, Internal, "gtk_ctree_set_indent");
   begin
      Internal (Get_Object (Ctree), Indent);
   end Set_Indent;

   --------------------
   -- Set_Line_Style --
   --------------------

   procedure Set_Line_Style
     (Ctree      : access Gtk_Ctree_Record;
      Line_Style : in     Gtk_Ctree_Line_Style := Ctree_Lines_Solid)
   is
      procedure Internal
        (Ctree      : System.Address;
         Line_Style : Gtk_Ctree_Line_Style);
      pragma Import (C, Internal, "gtk_ctree_set_line_style");

   begin
      Internal (Get_Object (Ctree), Line_Style);
   end Set_Line_Style;

   -------------------
   -- Set_Node_Info --
   -------------------

   procedure Set_Node_Info (Ctree         : access Gtk_Ctree_Record;
                            Node          : in     Gtk_Ctree_Node;
                            Text          : in     UTF8_String;
                            Spacing       : in     Guint8;
                            Pixmap_Closed : in     Gdk.Pixmap.Gdk_Pixmap;
                            Mask_Closed   : in     Gdk.Bitmap.Gdk_Bitmap;
                            Pixmap_Opened : in     Gdk.Pixmap.Gdk_Pixmap;
                            Mask_Opened   : in     Gdk.Bitmap.Gdk_Bitmap;
                            Is_Leaf       : in     Boolean;
                            Expanded      : in     Boolean)
   is
      procedure Internal (Ctree         : in System.Address;
                          Node          : in Gtk_Ctree_Node;
                          Text          : in UTF8_String;
                          Spacing       : in Guint8;
                          Pixmap_Closed : in Gdk.Pixmap.Gdk_Pixmap;
                          Mask_Closed   : in Gdk.Bitmap.Gdk_Bitmap;
                          Pixmap_Opened : in Gdk.Pixmap.Gdk_Pixmap;
                          Mask_Opened   : in Gdk.Bitmap.Gdk_Bitmap;
                          Is_Leaf       : in Gint;
                          Expanded      : in Gint);
      pragma Import (C, Internal, "gtk_ctree_set_node_info");

   begin
      Internal (Get_Object (Ctree),
                Node,
                Text & ASCII.NUL,
                Spacing,
                Pixmap_Closed,
                Mask_Closed,
                Pixmap_Opened,
                Mask_Opened,
                To_Gint (Is_Leaf),
                To_Gint (Expanded));
   end Set_Node_Info;

   -------------------
   -- Set_Show_Stub --
   -------------------

   procedure Set_Show_Stub (Ctree     : access Gtk_Ctree_Record;
                            Show_Stub : in     Boolean)
   is
      procedure Internal (Ctree     : in System.Address;
                          Show_Stub : in Gint);
      pragma Import (C, Internal, "gtk_ctree_set_show_stub");
   begin
      Internal (Get_Object (Ctree), To_Gint (Show_Stub));
   end Set_Show_Stub;

   -----------------
   -- Set_Spacing --
   -----------------

   procedure Set_Spacing (Ctree   : access Gtk_Ctree_Record;
                          Spacing : in     Gint := 5)
   is
      procedure Internal (Ctree   : in System.Address;
                          Spacing : in Gint);
      pragma Import (C, Internal, "gtk_ctree_set_spacing");
   begin
      Internal (Get_Object (Ctree), Spacing);
   end Set_Spacing;

   ---------------
   -- Sort_Node --
   ---------------

   procedure Sort_Node (Ctree : access Gtk_Ctree_Record;
                        Node  : in     Gtk_Ctree_Node)
   is
      procedure Internal (Ctree : in System.Address;
                          Node  : in Gtk_Ctree_Node);
      pragma Import (C, Internal, "gtk_ctree_sort_node");

   begin
      Internal (Get_Object (Ctree), Node);
   end Sort_Node;

   --------------------
   -- Sort_Recursive --
   --------------------

   procedure Sort_Recursive (Ctree : access Gtk_Ctree_Record;
                             Node  : in     Gtk_Ctree_Node := null)
   is
      procedure Internal (Ctree : in System.Address;
                          Node  : in Gtk_Ctree_Node);
      pragma Import (C, Internal, "gtk_ctree_sort_recursive");

   begin
      Internal (Get_Object (Ctree), Node);
   end Sort_Recursive;

   ----------------------
   -- Toggle_Expansion --
   ----------------------

   procedure Toggle_Expansion (Ctree : access Gtk_Ctree_Record;
                               Node  : in     Gtk_Ctree_Node)
   is
      procedure Internal (Ctree : in System.Address;
                          Node  : in Gtk_Ctree_Node);
      pragma Import (C, Internal, "gtk_ctree_toggle_expansion");

   begin
      Internal (Get_Object (Ctree), Node);
   end Toggle_Expansion;

   --------------------------------
   -- Toggle_Expansion_Recursive --
   --------------------------------

   procedure Toggle_Expansion_Recursive (Ctree : access Gtk_Ctree_Record;
                                         Node  : in     Gtk_Ctree_Node)
   is
      procedure Internal (Ctree : in System.Address;
                          Node  : in Gtk_Ctree_Node);
      pragma Import (C, Internal, "gtk_ctree_toggle_expansion_recursive");

   begin
      Internal (Get_Object (Ctree), Node);
   end Toggle_Expansion_Recursive;

   --------------
   -- Unselect --
   --------------

   procedure Unselect (Ctree : access Gtk_Ctree_Record;
                       Node  : in     Gtk_Ctree_Node)
   is
      procedure Internal (Ctree : in System.Address;
                          Node  : in Gtk_Ctree_Node);
      pragma Import (C, Internal, "gtk_ctree_unselect");

   begin
      Internal (Get_Object (Ctree), Node);
   end Unselect;

   ------------------------
   -- Unselect_Recursive --
   ------------------------

   procedure Unselect_Recursive
     (Ctree : access Gtk_Ctree_Record;
      Node  : in     Gtk_Ctree_Node := null)
   is
      procedure Internal (Ctree : in System.Address;
                          Node  : in Gtk_Ctree_Node);
      pragma Import (C, Internal, "gtk_ctree_unselect_recursive");

   begin
      Internal (Get_Object (Ctree), Node);
   end Unselect_Recursive;

   -----------------
   -- Ctree_Gnode --
   -----------------

   package body Ctree_Gnode is

      ------------------------
      -- C_Ctree_Gnode_Func --
      ------------------------

      function C_Ctree_Gnode_Func
        (C_Ctree : System.Address;
         Depth   : Guint;
         C_Gnode : Glib.Gnodes.Gnode;
         C_Cnode : Gtk_Ctree_Node;
         C_Data  : Ctree_Gnode_Func_Record_Access) return Gboolean
      is
         Stub : Gtk_Ctree_Record;
         Ctree : constant Gtk_Ctree :=
           Gtk_Ctree (Get_User_Data (C_Ctree, Stub));
      begin
         return Boolean'Pos (C_Data.Func (Ctree, Depth, C_Gnode,
                                          C_Cnode, C_Data.Data));
      end C_Ctree_Gnode_Func;

      ---------------------
      -- Export_To_Gnode --
      ---------------------

      function Export_To_Gnode
        (Ctree   : access Gtk_Ctree_Record'Class;
         Parent  : in     Glib.Gnodes.Gnode;
         Sibling : in     Glib.Gnodes.Gnode;
         Node    : in     Gtk_Ctree_Node;
         Func    : in     Gtk_Ctree_Gnode_Func;
         Data    : in     Data_Type_Access) return Glib.Gnodes.Gnode
      is
         function Internal (Ctree   : in System.Address;
                            Parent  : in Glib.Gnodes.Gnode;
                            Sibling : in Glib.Gnodes.Gnode;
                            Node    : in Gtk_Ctree_Node;
                            Func    : in System.Address;
                            Data    : in System.Address)
                           return Glib.Gnodes.Gnode;
         pragma Import (C, Internal, "gtk_ctree_export_to_gnode");

         C_Func_Address : System.Address;
         Local_Data : aliased constant Ctree_Gnode_Func_Record :=
           (Func => Func, Data => Data);

      begin
         if Func = null then
            C_Func_Address := System.Null_Address;
         else
            C_Func_Address := C_Ctree_Gnode_Func'Address;
         end if;

         return Internal
           (Get_Object (Ctree), Parent, Sibling, Node,
            C_Func_Address, Local_Data'Address);
      end Export_To_Gnode;

      function Insert_Gnode (Ctree   : access Gtk_Ctree_Record'Class;
                             Parent  : in     Glib.Gnodes.Gnode;
                             Sibling : in     Glib.Gnodes.Gnode;
                             Node    : in     Gtk_Ctree_Node;
                             Func    : in     Gtk_Ctree_Gnode_Func;
                             Data    : in     Data_Type_Access)
        return Gtk_Ctree_Node is

         function Internal (Ctree   : in System.Address;
                            Parent  : in Glib.Gnodes.Gnode;
                            Sibling : in Glib.Gnodes.Gnode;
                            Node    : in Gtk_Ctree_Node;
                            Func    : in System.Address;
                            Data    : in System.Address)
           return Gtk_Ctree_Node;
         pragma Import (C, Internal, "gtk_ctree_insert_gnode");

         C_Func_Address : System.Address;
         Local_Data : aliased constant Ctree_Gnode_Func_Record :=
           (Func => Func, Data => Data);

      begin
         if Func = null then
            C_Func_Address := System.Null_Address;
         else
            C_Func_Address := C_Ctree_Gnode_Func'Address;
         end if;

         return Internal (Get_Object (Ctree),
                          Parent,
                          Sibling,
                          Node,
                          C_Func_Address,
                          Local_Data'Address);
      end Insert_Gnode;

   end Ctree_Gnode;

   --------------
   -- Row_Data --
   --------------

   package body Row_Data is

      procedure Deallocate_Data_Type is new Unchecked_Deallocation
        (Data_Type, Data_Type_Access);

      ------------------
      -- C_Ctree_Func --
      ------------------

      procedure C_Ctree_Func (C_Ctree : in System.Address;
                              C_Node  : in Gtk_Ctree_Node;
                              C_Data  : in Ctree_Func_Record_Access) is
         Stub : Gtk_Ctree_Record;
         Ctree : constant Gtk_Ctree :=
           Gtk_Ctree (Get_User_Data (C_Ctree, Stub));
      begin
         C_Data.Func (Ctree, C_Node, C_Data.Data);
      end C_Ctree_Func;

      ---------------------
      -- C_Gcompare_Func --
      ---------------------

      function C_Gcompare_Func
        (Row_Data           : in Data_Type_Access;
         Gcompare_Func_Data : in Gcompare_Func_Record_Access) return Gint is
      begin
         if Row_Data /= null then
            return To_Gint (Gcompare_Func_Data.Func
                            (Row_Data.all,
                             Gcompare_Func_Data.Data.all));

         else  --  No data set to this node... Returning false
            return To_Gint (False);
         end if;
      end C_Gcompare_Func;

      ---------------------------
      -- Default_Gcompare_Func --
      ---------------------------

      function Default_Gcompare_Func (A, B : in Data_Type) return Boolean is
      begin
         return A = B;
      end Default_Gcompare_Func;

      ---------------
      -- Free_Data --
      ---------------

      procedure Free_Data (Data : Data_Type_Access) is
         Local_Data : Data_Type_Access := Data;
      begin
         Deallocate_Data_Type (Local_Data);
      end Free_Data;

      ----------------------
      -- Find_By_Row_Data --
      ----------------------

      function Find_By_Row_Data
        (Ctree : access Gtk_Ctree_Record'Class;
         Node  : in     Gtk_Ctree_Node;
         Data  : in     Data_Type) return Gtk_Ctree_Node is
      begin
         return Find_By_Row_Data_Custom
           (Ctree, Node, Data, Default_Gcompare_Func'Access);
      end Find_By_Row_Data;

      --------------------------
      -- Find_All_By_Row_Data --
      --------------------------

      function Find_All_By_Row_Data (Ctree : access Gtk_Ctree_Record'Class;
                                     Node  : in     Gtk_Ctree_Node;
                                     Data  : in     Data_Type)
        return Node_List.Glist is
      begin
         return Find_All_By_Row_Data_Custom
           (Ctree, Node, Data, Default_Gcompare_Func'Access);
      end Find_All_By_Row_Data;

      -----------------------------
      -- Find_By_Row_Data_Custom --
      -----------------------------

      function Find_By_Row_Data_Custom (Ctree : access Gtk_Ctree_Record'Class;
                                        Node  : in     Gtk_Ctree_Node;
                                        Data  : in     Data_Type;
                                        Func  : in     Gcompare_Func)
        return Gtk_Ctree_Node is
         function Internal (Ctree : in System.Address;
                            Node  : in Gtk_Ctree_Node;
                            Data  : in System.Address;
                            Func  : in System.Address) return Gtk_Ctree_Node;
         pragma Import (C, Internal, "gtk_ctree_find_by_row_data_custom");
         Local_Copy : Data_Type_Access := new Data_Type'(Data);
         D : Gcompare_Func_Record := (Func => Func, Data => Local_Copy);
         Result : Gtk_Ctree_Node;

      begin
         Result := Internal
           (Get_Object (Ctree), Node, D'Address, C_Gcompare_Func'Address);
         Deallocate_Data_Type (Local_Copy);
         return Result;
      end Find_By_Row_Data_Custom;

      ---------------------------------
      -- Find_All_By_Row_Data_Custom --
      ---------------------------------

      function Find_All_By_Row_Data_Custom
        (Ctree : access Gtk_Ctree_Record'Class;
         Node  : in     Gtk_Ctree_Node;
         Data  : in     Data_Type;
         Func  : in     Gcompare_Func) return Node_List.Glist
      is
         function Internal (Ctree : in System.Address;
                            Node  : in Gtk_Ctree_Node;
                            Data  : in System.Address;
                            Func  : in System.Address) return System.Address;
         pragma Import (C, Internal, "gtk_ctree_find_all_by_row_data_custom");
         Local_Copy : Data_Type_Access := new Data_Type'(Data);
         D : Gcompare_Func_Record := (Func => Func, Data => Local_Copy);
         Result : Node_List.Glist;

      begin
         Node_List.Set_Object (Result, Internal (Get_Object (Ctree),
                                                 Node,
                                                 D'Address,
                                                 C_Gcompare_Func'Address));
         Deallocate_Data_Type (Local_Copy);
         return Result;
      end Find_All_By_Row_Data_Custom;

      -----------------------
      -- Node_Get_Row_Data --
      -----------------------

      function Node_Get_Row_Data (Ctree : access Gtk_Ctree_Record'Class;
                                  Node  : in     Gtk_Ctree_Node)
                                  return Data_Type
      is
         function Internal
           (Ctree : in System.Address;
            Node  : in Gtk_Ctree_Node) return Data_Type_Access;
         pragma Import (C, Internal, "gtk_ctree_node_get_row_data");

      begin
         return Internal (Get_Object (Ctree), Node).all;
      exception
         when Constraint_Error =>
            raise Gtkada.Types.Data_Error;
      end Node_Get_Row_Data;

      -----------------------
      -- Node_Set_Row_Data --
      -----------------------

      procedure Node_Set_Row_Data (Ctree : access Gtk_Ctree_Record'Class;
                                   Node  : in     Gtk_Ctree_Node;
                                   Data  : in     Data_Type)
      is
         procedure Internal
           (Object  : in System.Address;
            Node    : in Gtk_Ctree_Node;
            Data    : in Data_Type_Access;
            Destroy : in System.Address);
         pragma Import (C, Internal, "gtk_ctree_node_set_row_data_full");

      begin
         Internal
           (Get_Object (Ctree),
            Node, new Data_Type'(Data), Free_Data'Address);
      end Node_Set_Row_Data;

      --------------------
      -- Post_Recursive --
      --------------------

      procedure Post_Recursive (Ctree : access Gtk_Ctree_Record'Class;
                                Node  : in     Gtk_Ctree_Node;
                                Func  : in     Gtk_Ctree_Func;
                                Data  : in     Data_Type_Access) is
         procedure Internal (Ctree : in System.Address;
                             Node  : in Gtk_Ctree_Node;
                             Func  : in System.Address;
                             Data  : in System.Address);
         pragma Import (C, Internal, "gtk_ctree_post_recursive");

         Local_Data : aliased constant Ctree_Func_Record :=
           (Func => Func, Data => Data);

      begin
         Internal
           (Get_Object (Ctree), Node, C_Ctree_Func'Address,
            Local_Data'Address);
      end Post_Recursive;

      -----------------------------
      -- Post_Recursive_To_Depth --
      -----------------------------

      procedure Post_Recursive_To_Depth (Ctree : access Gtk_Ctree_Record'Class;
                                         Node  : in     Gtk_Ctree_Node;
                                         Depth : in     Gint;
                                         Func  : in     Gtk_Ctree_Func;
                                         Data  : in     Data_Type_Access) is
         procedure Internal (Ctree : in System.Address;
                             Node  : in Gtk_Ctree_Node;
                             Depth : in Gint;
                             Func  : in System.Address;
                             Data  : in System.Address);
         pragma Import (C, Internal, "gtk_ctree_post_recursive_to_depth");

         Local_Data : aliased constant Ctree_Func_Record :=
           (Func => Func, Data => Data);

      begin
         Internal
           (Get_Object (Ctree), Node, Depth, C_Ctree_Func'Address,
            Data => Local_Data'Address);
      end Post_Recursive_To_Depth;

      -------------------
      -- Pre_Recursive --
      -------------------

      procedure Pre_Recursive (Ctree : access Gtk_Ctree_Record'Class;
                               Node  : in     Gtk_Ctree_Node;
                               Func  : in     Gtk_Ctree_Func;
                               Data  : in     Data_Type_Access) is
         procedure Internal (Ctree : in System.Address;
                             Node  : in Gtk_Ctree_Node;
                             Func  : in System.Address;
                             Data  : in System.Address);
         pragma Import (C, Internal, "gtk_ctree_pre_recursive");

         Local_Data : aliased constant Ctree_Func_Record :=
           (Func => Func, Data => Data);

      begin
         Internal
           (Get_Object (Ctree), Node, C_Ctree_Func'Address,
            Local_Data'Address);
      end Pre_Recursive;

      ----------------------------
      -- Pre_Recursive_To_Depth --
      ----------------------------

      procedure Pre_Recursive_To_Depth (Ctree : access Gtk_Ctree_Record'Class;
                                        Node  : in     Gtk_Ctree_Node;
                                        Depth : in     Gint;
                                        Func  : in     Gtk_Ctree_Func;
                                        Data  : in     Data_Type_Access) is
         procedure Internal (Ctree : in System.Address;
                             Node  : in Gtk_Ctree_Node;
                             Depth : in Gint;
                             Func  : in System.Address;
                             Data  : in System.Address);
         pragma Import (C, Internal, "gtk_ctree_pre_recursive_to_depth");

         Local_Data : aliased constant Ctree_Func_Record :=
           (Func => Func, Data => Data);

      begin
         Internal
           (Get_Object (Ctree), Node, Depth, C_Ctree_Func'Address,
            Data => Local_Data'Address);
      end Pre_Recursive_To_Depth;
   end Row_Data;

end Gtk.Ctree;
