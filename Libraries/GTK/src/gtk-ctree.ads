-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2007 AdaCore                    --
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
--  This widget is deprecated. Use Gtk.Tree_View instead.
--
--  This widget is similar to Gtk_Clist but it displays a tree with expandable
--  nodes instead of a simple list. Gtk_Tree is a more flexible tree widget
--  (it can have arbitrary widgets in the tree cells), but it is less efficient
--  and is limited to 32768 pixels.
--
--  If you need horizontal or vertical scrolling, you mustn't put this widget
--  in a Gtk_Viewport, and then in a Gtk_Scrolled_Window. Put it directly into
--  a Gtk_Scrolled_Window, or horizontal scrolling will be disabled, and the
--  column headers will disappear when scrolling vertically.
--
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Obsolescent widgets</group>
--  <testgtk>create_ctree.adb</testgtk>

with Interfaces.C.Strings;
with Unchecked_Conversion;

with Gdk.Bitmap;
with Gdk.Color;
with Gdk.Pixmap;

with Glib.Glist;
pragma Elaborate_All (Glib.Glist);
with Glib.Gnodes;

pragma Warnings (Off);  --  Gtk.Clist is obsolescent
with Gtk.Clist;
pragma Warnings (On);
with Gtk.Enums;            use Gtk.Enums;
with Gtk.Style;

with Gtkada.Types;         use Gtkada.Types;

package Gtk.Ctree is
   pragma Obsolescent ("use Gtk.Tree_View instead");
   pragma Elaborate_Body;

   pragma Warnings (Off); --  Gtk.Clist is obsolescent;
   type Gtk_Ctree_Record is new Gtk.Clist.Gtk_Clist_Record with private;
   type Gtk_Ctree is access all Gtk_Ctree_Record'Class;

   type Gtk_Ctree_Row is new Gtk.Clist.Gtk_Clist_Row;
   pragma Warnings (On);
   --  Similar to Clist_Row, but for a Ctree.

   type Gtk_Ctree_Node is new Gdk.C_Proxy;
   --  This type represents a node inside a Ctree.

   --  <doc_ignore>
   Null_Ctree_Node : constant Gtk_Ctree_Node := null;

   type Gtk_Ctree_Line_Style is
     (Ctree_Lines_None,
      --  No line will be drawn in the Ctree

      Ctree_Lines_Solid,
      --  Solid lines will be drawn

      Ctree_Lines_Dotted,
      --  Dotted lines will be drawn

      Ctree_Lines_Tabbed
      --  The tree won't be highlighted by lines but by tabs surrounding nodes
     );
   pragma Convention (C, Gtk_Ctree_Line_Style);
   --  See Gtk.Ctree.Set_Line_Style for more details.

   type Gtk_Ctree_Expander_Style is
     (Ctree_Expander_None,
      --  No pixmap will be drawn, you will have to double-click on the node to
      --  expand it.

      Ctree_Expander_Square,
      --  The pixmap will be a square

      Ctree_Expander_Triangle,
      --  The pixmap will be a triangle

      Ctree_Expander_Circular
      --  The pixmap will be a circle
     );
   --  See Gtk.Ctree.Set_Expander_Style for more details.
   pragma Convention (C, Gtk_Ctree_Expander_Style);

   package Row_List is new Glib.Glist.Generic_List (Gtk_Ctree_Row);

   function Convert is new Unchecked_Conversion
     (Gtk_Ctree_Node, System.Address);
   function Convert is new Unchecked_Conversion
     (System.Address, Gtk_Ctree_Node);
   package Node_List is new Glib.Glist.Generic_List (Gtk_Ctree_Node);
   --  </doc_ignore>

   -----------------------------------
   -- Creation, insertion, deletion --
   -----------------------------------
   --  Elements inside a Gtk_Ctree are not ordered from the top to the bottom
   --  as is the case for Gtk_Clist. Instead, they are put in the ctree by
   --  indicating where in the tree they should be placed. The position of an
   --  element (called a node) is defined by a parent node and a sibling node.
   --  The node will be attached in the parent subtree, on top of the sibling
   --  node.

   procedure Gtk_New (Widget      :    out Gtk_Ctree;
                      Columns     : in     Gint;
                      Tree_Column : in     Gint := 0);
   --  Create a ctree with Columns columns.
   --  Tree_Column indicates in which column the tree will be displayed.

   procedure Initialize (Widget      : access Gtk_Ctree_Record'Class;
                         Columns     : in     Gint;
                         Tree_Column : in     Gint := 0);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Gtk_New (Widget      :    out Gtk_Ctree;
                      Titles      : in     Chars_Ptr_Array;
                      Tree_Column : in     Gint := 0);
   --  Create a ctree with Titles'Length columns.
   --  Titles gives the title of each column.
   --  Tree_Column indicates in which column the tree will be displayed.

   procedure Initialize (Widget      : access Gtk_Ctree_Record'Class;
                         Titles      : in     Chars_Ptr_Array;
                         Tree_Column : in     Gint := 0);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Ctree.

   function Insert_Node (Ctree         : access Gtk_Ctree_Record;
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
                         return                 Gtk_Ctree_Node;
   --  Insert a new node in the Ctree.
   --  Parent is the parent node. If null, the new node is part of the root.
   --  The new node will be inserted right on top of Sibling. If Sibling is
   --  null, then it will be the first node in the subtree.
   --  Text contains the text for each cell of the node. Note that Insert_Node
   --  expects the length of the Text parameter to be equal to the number of
   --  columns of the Ctree.
   --  Spacing is the number of pixels between the lines of the tree and the
   --  text in the same column.
   --  If Is_Leaf is True, then the node won't contain any subtree. If False,
   --  the newly created node can be used as the Parent for further node
   --  creation. In this case, Expanded indicates whether the subtree
   --  associated with this node should be initially visible.
   --  In addition to the "+" or "-" sign indicating whether the subtree is
   --  expanded or not, it is possible to put a pixmap giving this information.
   --  Pixmap_Closed and Mask_Closed represent the image and the mask used when
   --  the subtree is closed; similarly, Pixmap_Opened and Mask_Opened
   --  represent the image and the mask used when the subtree is opened.

   procedure Remove_Node (Ctree : access Gtk_Ctree_Record;
                          Node  : in     Gtk_Ctree_Node);
   --  Remove Node from Ctree.

   -------------------------------------------
   -- Tree, Node and Row basic manipulation --
   -------------------------------------------

   function Get_Tree_Column
     (Widget : access Gtk.Ctree.Gtk_Ctree_Record'Class) return Gint;
   --  Return the Tree_Column attribute of a given Node.
   --  Tree_Column indicates in which column the tree will be displayed.

   function Get_Node_List
     (Ctree : access Gtk_Ctree_Record) return Node_List.Glist;
   --   Return the list of nodes associated with a given Ctree.
   --   Note: you need to extract the nodes with Node_List.Get_Gpointer.

   function Get_Row_List
     (Ctree : access Gtk_Ctree_Record) return Row_List.Glist;
   --  Return the list of rows associated with a given Ctree.

   function Get_Selection
     (Ctree : access Gtk_Ctree_Record) return Node_List.Glist;
   --   Return the list of nodes currently selected.
   --   Extract the nodes with Node_List.Get_Data

   function Node_Get_Row (Node : in Gtk_Ctree_Node) return Gtk_Ctree_Row;
   --  Return the row of a given Node.

   function Row_Get_Children (Row : in Gtk_Ctree_Row) return Gtk_Ctree_Node;
   --  Return the children node of a given Row.

   function Row_Get_Expanded (Row : in Gtk_Ctree_Row) return Boolean;
   --  Return the expanded attribute of a given Row.
   --  Note that Expanded can also be retrieved via Get_Node_Info,
   --  this function is just a quick accessor.

   function Row_Get_Is_Leaf (Row : in Gtk_Ctree_Row) return Boolean;
   --  Return the leaf attribute of a given Row.
   --  Note that Is_Leaf can also be retrieved via Get_Node_Info,
   --  this function is just a quick accessor.

   function Row_Get_Parent (Row : in Gtk_Ctree_Row) return Gtk_Ctree_Node;
   --  Return the parent node of a given Row.

   function Row_Get_Sibling (Row : in Gtk_Ctree_Row) return Gtk_Ctree_Node;
   --  Return the sibling node of a given Row.

   function Is_Created (Node : in Gtk_Ctree_Node) return Boolean;
   --  Return True if Node is different from Null_Ctree_Node

   -----------------------------------------
   -- Querying / finding tree information --
   -----------------------------------------

   function Is_Viewable
     (Ctree  : access Gtk_Ctree_Record;
      Node   : in     Gtk_Ctree_Node)
      return Boolean;
   --  Return True if Node is viewable.
   --  A Node is viewable if all the trees and subtrees containing it are
   --  expanded.

   function Last
     (Ctree  : access Gtk_Ctree_Record;
      Node   : in     Gtk_Ctree_Node)
      return Gtk_Ctree_Node;
   --  Return the last node of a given subtree.
   --  Starting at Node, this function will recursively look for the last
   --  sibling of the last child.
   --  Return an empty node is Node is empty.

   function Find_Node_Ptr
     (Ctree     : access Gtk_Ctree_Record;
      Ctree_Row : in     Gtk_Ctree_Row)
      return Gtk_Ctree_Node;
   --  Return the node corresponding to a given row.

   function Node_Nth (Ctree  : access Gtk_Ctree_Record;
                      Row    : in     Guint)
                      return          Gtk_Ctree_Node;
   --  Return the Node corresponding to the nth row of a given Ctree.
   --  This can be used to retrieve the root node of the tree, by passing 0 for
   --  Row.

   function Find (Ctree : access Gtk_Ctree_Record;
                  Node  : in     Gtk_Ctree_Node;
                  Child : in     Gtk_Ctree_Node) return Boolean;
   --  Recursively search for a given Child in a given subtree.
   --  the subtree is determined by Node. If Node is empty, the search will
   --  occur on the whole tree.
   --  Return True if Child is found, False otherwise.

   function Is_Ancestor
     (Ctree  : access Gtk_Ctree_Record;
      Node   : in     Gtk_Ctree_Node;
      Child  : in     Gtk_Ctree_Node) return Boolean;
   --  Indicate whether Node is an ancestor of Child.
   --  It is assumed that Node is not empty.

   function Is_Hot_Spot
     (Ctree  : access Gtk_Ctree_Record;
      X      : in     Gint;
      Y      : in     Gint) return Boolean;
   --  Return True if the Ctree is centered on (x,y)

   ------------------------------------------------------
   -- Tree signals: move, expand, collapse, (un)select --
   ------------------------------------------------------

   procedure Move (Ctree       : access Gtk_Ctree_Record;
                   Node        : in     Gtk_Ctree_Node;
                   New_Parent  : in     Gtk_Ctree_Node;
                   New_Sibling : in     Gtk_Ctree_Node);
   --  Move a node in a Ctree.
   --  After its creation, a node can be moved.
   --  New_Parent points to the new parent node that will contain Node.
   --  If null, Node will be attached to the root.
   --  New_Sibling indicates under which node Node will be inserted.
   --  If New_Sibling is null, the new node will be the lowest in its branch.

   procedure Expand (Ctree : access Gtk_Ctree_Record;
                     Node  : in     Gtk_Ctree_Node);
   --  Expand the first level of the subtree associated with Node.

   procedure Expand_Recursive
     (Ctree : access Gtk_Ctree_Record;
      Node  : in     Gtk_Ctree_Node := null);
   --  Expand the whole subtree associated with Node.

   procedure Expand_To_Depth
     (Ctree : access Gtk_Ctree_Record;
      Node  : in     Gtk_Ctree_Node := null;
      Depth : in     Gint);
   --  Expand the subtree associated with Node and its descendants until Depth
   --  levels of subtrees have been reached.

   procedure Collapse (Ctree : access Gtk_Ctree_Record;
                       Node  : in     Gtk_Ctree_Node);
   --  Collapse the first level of the subtree associated with Node.

   procedure Collapse_Recursive
     (Ctree : access Gtk_Ctree_Record;
      Node  : in     Gtk_Ctree_Node := null);
   --  Collapse the whole subtree associated with Node.

   procedure Collapse_To_Depth
     (Ctree : access Gtk_Ctree_Record;
      Node  : in     Gtk_Ctree_Node := null;
      Depth : in     Gint);
   --  Collapse the subtree associated with Node and its descendants until
   --  Depth levels of subtrees have been reached.

   procedure Toggle_Expansion (Ctree : access Gtk_Ctree_Record;
                               Node  : in     Gtk_Ctree_Node);
   --  Change the state of the Ctree from expanded to collapsed and the other
   --  way around on one level.

   procedure Toggle_Expansion_Recursive (Ctree : access Gtk_Ctree_Record;
                                         Node  : in     Gtk_Ctree_Node);
   --  Change the state of the Ctree from expanded to collapsed and the other
   --  way around for the whole subtree.

   procedure Gtk_Select (Ctree : access  Gtk_Ctree_Record;
                         Node  : in      Gtk_Ctree_Node);
   --  Select a specified Node, and only this one.

   procedure Select_Recursive
     (Ctree : access Gtk_Ctree_Record;
      Node  : in     Gtk_Ctree_Node := null);
   --  Select a specified Node, and its whole subtree.

   procedure Unselect (Ctree : access Gtk_Ctree_Record;
                       Node  : in     Gtk_Ctree_Node);
   --  Unselect a specified Node, and only this one.

   procedure Unselect_Recursive
     (Ctree : access Gtk_Ctree_Record;
      Node  : in     Gtk_Ctree_Node := null);
   --  Unselect a specified Node, and its whole subtree.

   procedure Real_Select_Recursive (Ctree     : access Gtk_Ctree_Record;
                                    Node      : in     Gtk_Ctree_Node := null;
                                    Do_Select : in     Boolean);
   --  Similar to Select_Recursive or Unselect_Recursive.
   --  If Do_Select is True, equivalent to Select_Recursive.
   --  If Do_Select is False, equivalent to Unselect_Recursive.

   ------------------------------------
   -- Analogs of Gtk_Clist functions --
   ------------------------------------

   procedure Node_Set_Text (Ctree  : access Gtk_Ctree_Record;
                            Node   : in     Gtk_Ctree_Node;
                            Column : in     Gint;
                            Text   : in     UTF8_String);
   --  Set the cell's text, replacing its current contents.
   --  This changes the type of the cell to Cell_Text. The pixmap (if any)
   --  will no longer be displayed.

   function Node_Get_Text (Ctree   : access Gtk_Ctree_Record;
                           Node    : in     Gtk_Ctree_Node;
                           Column  : in     Gint) return UTF8_String;
   --  Return the text contained in cell.
   --  An empty string is returned if Column is invalid or if the Cell did not
   --  contain any text (only a pixmap)

   procedure Node_Set_Pixmap (Ctree  : access Gtk_Ctree_Record;
                              Node   : in     Gtk_Ctree_Node;
                              Column : in     Gint;
                              Pixmap : in     Gdk.Pixmap.Gdk_Pixmap;
                              Mask   : in     Gdk.Bitmap.Gdk_Bitmap);
   --  Set the cell's pixmap, replacing its current contents.
   --  The type of the cell becomes Cell_Pixmap, and the text is no longer
   --  displayed.

   procedure Node_Get_Pixmap (Ctree   : access Gtk_Ctree_Record;
                              Node    : in     Gtk_Ctree_Node;
                              Column  : in     Gint;
                              Pixmap  :    out Gdk.Pixmap.Gdk_Pixmap;
                              Mask    :    out Gdk.Bitmap.Gdk_Bitmap;
                              Success :    out Boolean);
   --  Return the Pixmap contained in a cell.
   --  The type of the cell should be Cell_Pixmap.
   --  The result is meaningful only if Success is True. If the Cell did not
   --  contain a pixmap, Success is set to False.

   procedure Node_Set_Pixtext (Ctree   : access Gtk_Ctree_Record;
                               Node    : in     Gtk_Ctree_Node;
                               Column  : in     Gint;
                               Text    : in     UTF8_String;
                               Spacing : in     Guint8;
                               Pixmap  : in     Gdk.Pixmap.Gdk_Pixmap;
                               Mask    : in     Gdk.Bitmap.Gdk_Bitmap);
   --  Set both the Text and the Pixmap for the cell.
   --  Replace its current contents. The type of the cell becomes Cell_Pixtext,
   --  and both the text and the pixmap are displayed.

   procedure Node_Get_Pixtext (Ctree   : access Gtk_Ctree_Record;
                               Node    : in     Gtk_Ctree_Node;
                               Column  : in     Gint;
                               Text    :    out Interfaces.C.Strings.chars_ptr;
                               Spacing :    out Guint8;
                               Pixmap  :    out Gdk.Pixmap.Gdk_Pixmap;
                               Mask    :    out Gdk.Bitmap.Gdk_Bitmap;
                               Success :    out Boolean);
   --  Return the Text and the Pixmap for the cell.
   --  The result is not meaningful if Success is False.

   procedure Node_Set_Shift (Ctree      : access Gtk_Ctree_Record;
                             Node       : in     Gtk_Ctree_Node;
                             Column     : in     Gint;
                             Vertical   : in     Gint;
                             Horizontal : in     Gint);
   --  Set a horizontal and vertical shift for drawing the content of the cell.
   --  Both shifts can be either positive or negative.
   --  This is particularly useful for indenting items in a columns.

   procedure Set_Node_Info (Ctree         : access Gtk_Ctree_Record;
                            Node          : in     Gtk_Ctree_Node;
                            Text          : in     UTF8_String;
                            Spacing       : in     Guint8;
                            Pixmap_Closed : in     Gdk.Pixmap.Gdk_Pixmap;
                            Mask_Closed   : in     Gdk.Bitmap.Gdk_Bitmap;
                            Pixmap_Opened : in     Gdk.Pixmap.Gdk_Pixmap;
                            Mask_Opened   : in     Gdk.Bitmap.Gdk_Bitmap;
                            Is_Leaf       : in     Boolean;
                            Expanded      : in     Boolean);
   --  Set all the info related to a specific Node.

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
      Success       :    out Boolean);
   --  Return all the info related to a specific Node.

   procedure Node_Set_Selectable (Ctree      : access Gtk_Ctree_Record;
                                  Node       : in     Gtk_Ctree_Node;
                                  Selectable : in     Boolean := True);
   --  Indicate whether the Node can be selected or not.
   --  The default value is True.

   function Node_Get_Selectable (Ctree  : access Gtk_Ctree_Record;
                                 Node   : in     Gtk_Ctree_Node)
                                 return          Boolean;
   --  Return the selectable status of the Node.

   procedure Node_Set_Row_Style (Ctree : access Gtk_Ctree_Record;
                                 Node  : in     Gtk_Ctree_Node;
                                 Style : in     Gtk.Style.Gtk_Style);
   --  Set the default style for the cells in the Node.
   --  This can be overridden for each cell with Node_Set_Cell_Style.

   function Node_Get_Row_Style (Ctree  : access Gtk_Ctree_Record;
                                Node   : in     Gtk_Ctree_Node)
                                return          Gtk.Style.Gtk_Style;
   --  Return the default style used for the Node.

   procedure Node_Set_Cell_Style (Ctree  : access Gtk_Ctree_Record;
                                  Node   : in     Gtk_Ctree_Node;
                                  Column : in     Gint;
                                  Style  : in     Gtk.Style.Gtk_Style);
   --  Set the style (font, color, ...) used for the cell.
   --  This overrides the Node's style.

   function Node_Get_Cell_Style (Ctree  : access Gtk_Ctree_Record;
                                 Node   : in     Gtk_Ctree_Node;
                                 Column : in     Gint)
                                 return          Gtk.Style.Gtk_Style;
   --  Return the style of the cell.

   procedure Node_Set_Foreground (Ctree : access Gtk_Ctree_Record;
                                  Node  : in     Gtk_Ctree_Node;
                                  Color : in     Gdk.Color.Gdk_Color);
   --  Set the foreground color for the Node.
   --  The color must already be allocated.
   --  If no such Node exists in the tree, nothing is done.

   procedure Node_Set_Background (Ctree : access Gtk_Ctree_Record;
                                  Node  : in     Gtk_Ctree_Node;
                                  Color : in     Gdk.Color.Gdk_Color);
   --  Set the background color for the Node.
   --  The color must already be allocated.
   --  If no such Node exists in the tree, nothing is done.

   pragma Warnings (Off);  --  Gtk.Clist is obsolescent
   function Node_Get_Cell_Type (Ctree  : access Gtk_Ctree_Record;
                                Node   : in     Gtk_Ctree_Node;
                                Column : in     Gint)
                                return Gtk.Clist.Gtk_Cell_Type;
   pragma Warnings (On);
   --  Return the type of the cell at Node/Column.
   --  This indicates which of the functions Node_Get_Text. Node_Get_Pixmap,
   --  etc. should be used with this cell.

   procedure Node_Moveto (Ctree     : access Gtk_Ctree_Record;
                          Node      : in     Gtk_Ctree_Node;
                          Column    : in     Gint;
                          Row_Align : in     Gfloat := 0.5;
                          Col_Align : in     Gfloat := 0.5);
   --  Make a Node visible.
   --  Column indicates which column of the Node should be visible, if not
   --  all columns can be displayed.
   --  Row_Align and Col_Align are parameters between 0.0 and 1.0, and
   --  specify how the Node and the Column will be centered in the Ctree
   --  window. 0.0 means a Node on the top, and a Column on the left.

   function Node_Is_Visible
     (Ctree  : access Gtk_Ctree_Record;
      Node   : in     Gtk_Ctree_Node)
      return Gtk_Visibility;
   --  Indicate the visibility of a Node.
   --  Return Visibility_None if the Node is not visible in the Ctree window;
   --  Visibility_Partial if the Node is partially visible; Visibility_Full
   --  if the Node is entirely visible.
   --  This function ignores the fact that Node is in an expanded or collapsed
   --  subtree.

   ------------------------------
   -- Ctree specific functions --
   ------------------------------

   procedure Set_Indent (Ctree  : access Gtk_Ctree_Record;
                         Indent : in     Gint := 20);
   --  Change the indentation of the Ctree.
   --  Each different level of a subtree is indented by a number of pixels.
   --  By default, the indentation is 20 pixels, and can be changed using this
   --  procedure.

   function Get_Indent
     (Widget : access Gtk.Ctree.Gtk_Ctree_Record'Class) return Gint;
   --  Return the indentation of a Ctree.

   procedure Set_Spacing (Ctree   : access Gtk_Ctree_Record;
                          Spacing : in     Gint := 5);
   --  Set the spacing between the tree's icon and the additional pixmap.
   --  The additional pixmap indicates whether the subtree is opened or closed.
   --  The default value is 5 pixels.

   function Get_Spacing
     (Widget : access Gtk.Ctree.Gtk_Ctree_Record'Class) return Gint;
   --  Return the spacing between the tree's icon and the additional pixmap.

   procedure Set_Show_Stub (Ctree     : access Gtk_Ctree_Record;
                            Show_Stub : in     Boolean);
   --  Set the Show_Stub attribute of Ctree.

   function Get_Show_Stub (Ctree : access Gtk_Ctree_Record) return Boolean;
   --  Return the Show_Stub attribute of Ctree.

   procedure Set_Line_Style
     (Ctree      : access Gtk_Ctree_Record;
      Line_Style : in     Gtk_Ctree_Line_Style := Ctree_Lines_Solid);
   --  Change the style of the lines representing the tree of a given Ctree.
   --  By default, solid lines are used.
   --  See the description of Gtk_Ctree_Line_Style for more details of the
   --  possible values.

   function Get_Line_Style
     (Ctree : access Gtk_Ctree_Record) return Gtk_Ctree_Line_Style;
   --  return the style of the lines representing the tree of a given Ctree.

   procedure Set_Expander_Style
     (Ctree          : access Gtk_Ctree_Record;
      Expander_Style : in     Gtk_Ctree_Expander_Style :=
        Ctree_Expander_Square);
   --  Set the way a given Ctree can be expanded.
   --  To expand a subtree, you can either double-click on a node, or click on
   --  the "+/-" icon. This icon is by default included in a square pixmap.
   --  This procedure can change the form of this pixmap.
   --  See the description of Gtk_Ctree_Expander_Style for more details.

   function Get_Expander_Style
     (Ctree : access Gtk_Ctree_Record) return Gtk_Ctree_Expander_Style;
   --  Return the way a given Ctree can be expanded.

   type Gtk_Ctree_Compare_Drag_Func is access
     function (Ctree        : in Gtk_Ctree;
               Source_Node  : in Gtk_Ctree_Node;
               New_Parent   : in Gtk_Ctree_Node;
               New_Sibling  : in Gtk_Ctree_Node) return Boolean;
   --  Function type used in Set_Drag_Compare_Func.

   procedure Set_Drag_Compare_Func
     (Ctree    : access Gtk_Ctree_Record;
      Cmp_Func : in     Gtk_Ctree_Compare_Drag_Func);
   --  Set the drag compare function of a given Ctree.
   --  This function is used when the Ctree receives a dragged data.

   ----------------------------
   -- Tree sorting functions --
   ----------------------------

   procedure Sort_Node (Ctree : access Gtk_Ctree_Record;
                        Node  : in     Gtk_Ctree_Node);
   --  Sort the nodes of a given Ctree.
   --  This procedure only sorts the first level of the tree.

   procedure Sort_Recursive (Ctree : access Gtk_Ctree_Record;
                             Node  : in     Gtk_Ctree_Node := null);
   --  Sort the nodes of a given Ctree recursively.
   --  This procedure sorts the whole tree and subtrees associated with Ctree.
   --  Set Node to null if you want to sort the whole tree starting from its
   --  root.

   --------------------------
   -- Ctree_Gnode handling --
   --------------------------

   --  <doc_ignore>
   --  This package needs to be documented ???

   generic
      type Data_Type (<>) is private;
   package Ctree_Gnode is

      type Data_Type_Access is access all Data_Type;

      type Gtk_Ctree_Gnode_Func is access
        function (Ctree : access Gtk_Ctree_Record'Class;
                  Depth : in     Guint;
                  Gnode : in     Glib.Gnodes.Gnode;
                  Cnode : in     Gtk_Ctree_Node;
                  Data  : in     Data_Type_Access) return Boolean;

      function Export_To_Gnode (Ctree   : access Gtk_Ctree_Record'Class;
                                Parent  : in     Glib.Gnodes.Gnode;
                                Sibling : in     Glib.Gnodes.Gnode;
                                Node    : in     Gtk_Ctree_Node;
                                Func    : in     Gtk_Ctree_Gnode_Func;
                                Data    : in     Data_Type_Access)
        return Glib.Gnodes.Gnode;

      function Insert_Gnode (Ctree   : access Gtk_Ctree_Record'Class;
                             Parent  : in     Glib.Gnodes.Gnode;
                             Sibling : in     Glib.Gnodes.Gnode;
                             Node    : in     Gtk_Ctree_Node;
                             Func    : in     Gtk_Ctree_Gnode_Func;
                             Data    : in     Data_Type_Access)
        return Gtk_Ctree_Node;
   private
      --  <doc_ignore>
      type Ctree_Gnode_Func_Record is record
         Func : Gtk_Ctree_Gnode_Func;
         Data : Data_Type_Access;
      end record;
      type Ctree_Gnode_Func_Record_Access is
        access all Ctree_Gnode_Func_Record;

      function C_Ctree_Gnode_Func
        (C_Ctree : System.Address;
         Depth   : Guint;
         C_Gnode : Glib.Gnodes.Gnode;
         C_Cnode : Gtk_Ctree_Node;
         C_Data  : Ctree_Gnode_Func_Record_Access) return Gboolean;
      pragma Convention (C, C_Ctree_Gnode_Func);
      --  </doc_ignore>
   end Ctree_Gnode;

   --  </doc_ignore>

   -----------------------
   -- Row_Data handling --
   -----------------------

   --  <doc_ignore>
   generic
      type Data_Type (<>) is private;
   package Row_Data is

      type Data_Type_Access is access all Data_Type;
      --  </doc_ignore>

      procedure Node_Set_Row_Data (Ctree : access Gtk_Ctree_Record'Class;
                                   Node  : in     Gtk_Ctree_Node;
                                   Data  : in     Data_Type);
      --  Associate a Data with a Node.

      function Node_Get_Row_Data
        (Ctree : access Gtk_Ctree_Record'Class;
         Node  : in     Gtk_Ctree_Node)
         return Data_Type;
      --  Retrieve a data associated with a Node.
      --  Error Handling:
      --  Gtkada.Types.Data_Error is raised when trying to retrieve
      --  the data from a Node for which no data has been set
      --  (using Node_Set_Row_Data).

      function Find_By_Row_Data
        (Ctree : access Gtk_Ctree_Record'Class;
         Node  : in     Gtk_Ctree_Node;
         Data  : in     Data_Type)
         return Gtk_Ctree_Node;
      --  Find the first node containing a specified Data.
      --  Node is the starting point of the search. If null, the search will
      --  start from the root.
      --  Return the first Node whose associated data is Data, null if none
      --  can be found.

      function Find_All_By_Row_Data
        (Ctree : access Gtk_Ctree_Record'Class;
         Node  : in     Gtk_Ctree_Node;
         Data  : in     Data_Type)
         return Node_List.Glist;
      --  Find all nodes containing a specified Data.
      --  Node is the starting point of the search. If null, the search will
      --  start from the root.

      type Gcompare_Func is access
        function (A, B : in Data_Type) return Boolean;
      --  Function used to compare data types in the functions
      --  Find_[All] By_Row_Data_Custom.

      function Find_By_Row_Data_Custom
        (Ctree : access Gtk_Ctree_Record'Class;
         Node  : in     Gtk_Ctree_Node;
         Data  : in     Data_Type;
         Func  : in     Gcompare_Func)
         return Gtk_Ctree_Node;
      --  Find the first node containing a specified Data.
      --  Similar to Find_By_Row_Data but Func is used to allow a more flexible
      --  (user defined) method to compare two nodes.

      function Find_All_By_Row_Data_Custom
        (Ctree : access Gtk_Ctree_Record'Class;
         Node  : in     Gtk_Ctree_Node;
         Data  : in     Data_Type;
         Func  : in     Gcompare_Func) return Node_List.Glist;
      --  Find all the nodes containing a specified Data.
      --  Similar to Find_All_By_Row_Data but Func is used to allow a more
      --  flexible (user defined) method to compare two nodes.

      type Gtk_Ctree_Func is access
        procedure (Ctree : access Gtk_Ctree_Record'Class;
                   Node  : in     Gtk_Ctree_Node;
                   Data  : in     Data_Type_Access);
      --  Function used by Post/Pre_Recursive functions below.

      procedure Post_Recursive (Ctree : access Gtk_Ctree_Record'Class;
                                Node  : in     Gtk_Ctree_Node;
                                Func  : in     Gtk_Ctree_Func;
                                Data  : in     Data_Type_Access);
      --  Apply Func to each node of a subtree.
      --  Node designates the root of the subtree.
      --  Data will be passed as a parameter to Func.
      --  This procedure will first apply Func to the children nodes.

      procedure Post_Recursive_To_Depth (Ctree : access Gtk_Ctree_Record'Class;
                                         Node  : in     Gtk_Ctree_Node;
                                         Depth : in     Gint;
                                         Func  : in     Gtk_Ctree_Func;
                                         Data  : in     Data_Type_Access);
      --  Apply Func to each node of a subtree until a specified Depth.
      --  Node designates the root of the subtree.
      --  Data will be passed as a parameter to Func.
      --  This function is similar to Post_Recursive except that it
      --  stop at a specified subtree depth.

      procedure Pre_Recursive (Ctree : access Gtk_Ctree_Record'Class;
                               Node  : in     Gtk_Ctree_Node;
                               Func  : in     Gtk_Ctree_Func;
                               Data  : in     Data_Type_Access);
      --  Apply Func to each node of a subtree.
      --  Similar to Post_Recursive but will apply Func to the parent before
      --  applying it to its children.

      procedure Pre_Recursive_To_Depth (Ctree : access Gtk_Ctree_Record'Class;
                                        Node  : in     Gtk_Ctree_Node;
                                        Depth : in     Gint;
                                        Func  : in     Gtk_Ctree_Func;
                                        Data  : in     Data_Type_Access);
      --  Apply Func to each node of a subtree until a specific Depth.
      --  Similar to Post_Recursive_To_Depth but will apply Func to the parent
      --  before applying it to its children.

   private

      --  <doc_ignore>
      function Default_Gcompare_Func (A, B : in Data_Type) return Boolean;
      --
      --  This function needs to be declared in the spec, although it is
      --  only used in the body. Otherwise, the compiler does not allow
      --  to apply the 'Access attribute to it.

      type Ctree_Func_Record is record
         Func : Gtk_Ctree_Func;
         Data : Data_Type_Access;
      end record;
      type Ctree_Func_Record_Access is access all Ctree_Func_Record;

      procedure Free_Data (Data : Data_Type_Access);
      pragma Convention (C, Free_Data);
      --  Note that Data is *not* an in out parameter here, since Free_Data
      --  will be used as a callback, and when called, the original variable
      --  holding the pointer no longer exists.

      procedure C_Ctree_Func (C_Ctree : in System.Address;
                              C_Node  : in Gtk_Ctree_Node;
                              C_Data  : in Ctree_Func_Record_Access);
      pragma Convention (C, C_Ctree_Func);

      type Gcompare_Func_Record is record
         Func : Gcompare_Func;
         Data : Data_Type_Access;
      end record;
      type Gcompare_Func_Record_Access is access all Gcompare_Func_Record;

      function C_Gcompare_Func
        (Row_Data           : in Data_Type_Access;
         Gcompare_Func_Data : in Gcompare_Func_Record_Access) return Gint;
      pragma Convention (C, C_Gcompare_Func);
      --  </doc_ignore>

   end Row_Data;
   --
   --  The previous package implements the Row_Data stuff.
   --  !! Warning !! No type verification is made to check if you are
   --  using the appropriate function Get. This is your own responsability

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --  </properties>

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "tree_select_row"
   --    procedure Handler (Ctree  : access Gtk_Ctree_Record'Class;
   --                       Node   : Gtk_Ctree_Node;
   --                       Column : Gint);
   --
   --    Emitted to request the selection of a node.
   --    Column is the column number where the user clicked.
   --
   --  - "tree_unselect_row"
   --    procedure Handler (Ctree  : access Gtk_Ctree_Record'Class;
   --                       Node   : Gtk_Ctree_Node;
   --                       Column : Gint);
   --
   --    Emitted to request the unselection of a node.
   --
   --  - "tree_expand"
   --    procedure Handler (Ctree  : access Gtk_Clist_Record'Class;
   --                       Node   : Gtk_Ctree_Node);
   --
   --    Emitted when the subtree associated with a Node is expanded.
   --
   --  - "tree_collapse"
   --    procedure Handler (Ctree  : access Gtk_Clist_Record'Class;
   --                       Node   : Gtk_Ctree_Node);
   --
   --    Emitted when the subtree associated with a Node is collapsed.
   --
   --  - "tree_move"
   --    procedure Handler (Ctree       : access Gtk_Clist_Record'Class;
   --                       Node        : Gtk_Ctree_Node);
   --                       New_Parent  : Gtk_Ctree_Node);
   --                       New_Sibling : Gtk_Ctree_Node);
   --
   --    Emitted when a Node is moved (e.g its parent and/or its sibling
   --    changed).
   --
   --  </signals>

   Signal_Change_Focus_Row_Expansion : constant Glib.Signal_Name :=
                                         "change_focus_row_expansion";
   Signal_Tree_Collapse              : constant Glib.Signal_Name :=
                                         "tree_collapse";
   Signal_Tree_Expand                : constant Glib.Signal_Name :=
                                         "tree_expand";
   Signal_Tree_Move                  : constant Glib.Signal_Name :=
                                         "tree_move";
   Signal_Tree_Select_Row            : constant Glib.Signal_Name :=
                                         "tree_select_row";
   Signal_Tree_Unselect_Row          : constant Glib.Signal_Name :=
                                         "tree_unselect_row";

private

   pragma Warnings (Off);  --  Gtk.Clist is obsolescent
   type Gtk_Ctree_Record is new Gtk.Clist.Gtk_Clist_Record with null record;
   pragma Warnings (On);

   pragma Import (C, Get_Type, "gtk_ctree_get_type");
   pragma Import (C, Node_Get_Row, "ada_ctree_node_get_row");
   pragma Import (C, Row_Get_Children, "ada_ctree_row_get_children");
   pragma Import (C, Row_Get_Parent, "ada_ctree_row_get_parent");
   pragma Import (C, Row_Get_Sibling, "ada_ctree_row_get_sibling");

end Gtk.Ctree;

--  These subprograms never had a binding, and are now obsolescent:
--  No binding: gtk_ctree_find_all_by_row_data
--  No binding: gtk_ctree_find_by_row_data
--  No binding: gtk_ctree_node_get_type
--  No binding: gtk_ctree_node_set_row_data
