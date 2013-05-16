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
--  See extended documentation in Gtk.Tree_View_Column and Gtk.Tree_Store.
--  </description>
--  <c_version>2.16.6</c_version>
--  <group>Trees and Lists</group>
--  <testgtk>create_tree_view.adb</testgtk>
--  <screenshot>gtk-tree_view</screenshot>

with Glib.Properties;
with Gdk.Dnd;
with Gdk.Pixmap;
with Gdk.Rectangle;
with Gdk.Types;
with Gdk.Window;
with Gtk;
with Gtk.Adjustment;
with Gtk.Cell_Renderer;
with Gtk.Container;
with Gtk.Enums;
with Gtk.GEntry;
with Gtk.Selection;
with Gtk.Tooltip;
with Gtk.Tree_Model;
with Gtk.Tree_Selection;
with Gtk.Tree_View_Column;
with Interfaces.C.Strings;

package Gtk.Tree_View is

   type Gtk_Tree_View_Record is
     new Gtk.Container.Gtk_Container_Record with private;
   type Gtk_Tree_View is access all Gtk_Tree_View_Record'Class;

   type Gtk_Tree_View_Drop_Position is
     (Tree_View_Drop_Before,
      Tree_View_Drop_After,
      Tree_View_Drop_Into_Or_Before,
      Tree_View_Drop_Into_Or_After);
   pragma Convention (C, Gtk_Tree_View_Drop_Position);

   procedure Gtk_New (Widget : out Gtk_Tree_View);
   procedure Initialize (Widget : access Gtk_Tree_View_Record'Class);
   --  Creates or initializes a new tree view

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   procedure Gtk_New
     (Widget : out Gtk_Tree_View;
      Model  : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class);

   procedure Initialize
     (Widget : access Gtk_Tree_View_Record'Class;
      Model  : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Set_Model
     (Tree_View : access Gtk_Tree_View_Record;
      Model     : Gtk.Tree_Model.Gtk_Tree_Model);
   function Get_Model
     (Tree_View : access Gtk_Tree_View_Record)
      return Gtk.Tree_Model.Gtk_Tree_Model;
   --  Sets the model for a Gtk_Tree_View.  If the Tree_View already has a
   --  model set, it will remove it before setting the new model.
   --  If Model is Null, then it will unset the old model.

   function Get_Selection
     (Tree_View : access Gtk_Tree_View_Record)
      return Gtk.Tree_Selection.Gtk_Tree_Selection;
   --  Gets the Gtk_Tree_Selection associated with Tree_View.

   procedure Set_Hadjustment
     (Tree_View  : access Gtk_Tree_View_Record;
      Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   function Get_Hadjustment
     (Tree_View : access Gtk_Tree_View_Record)
      return Gtk.Adjustment.Gtk_Adjustment;
   --  Sets or gets the Gtk_Adjustment for the current horizontal aspect.

   procedure Set_Vadjustment
     (Tree_View  : access Gtk_Tree_View_Record;
      Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   function Get_Vadjustment
     (Tree_View : access Gtk_Tree_View_Record)
      return Gtk.Adjustment.Gtk_Adjustment;
   --  Sets or Gets the Gtk_Adjustment currently being used for the vertical
   --  aspect.

   function Get_Enable_Tree_Lines
     (Tree_View : access Gtk_Tree_View_Record) return Boolean;
   procedure Set_Enable_Tree_Lines
     (Tree_View : access Gtk_Tree_View_Record;
      Enabled   : Boolean);
   --  Whether to draw lines interconnecting the expanders in Tree_View.
   --  This does not have any visible effects for lists.

   function Get_Grid_Lines
     (Tree_View : access Gtk_Tree_View_Record) return Gtk.Enums.Gtk_Grid_Lines;
   procedure Set_Grid_Lines
     (Tree_View  : access Gtk_Tree_View_Record;
      Grid_Lines : Gtk.Enums.Gtk_Grid_Lines);
   --  Which grid lines to draw in Tree_View.

   function Get_Level_Indentation
     (Tree_View : access Gtk_Tree_View_Record) return Gint;
   procedure Set_Level_Indentation
     (Tree_View   : access Gtk_Tree_View_Record;
      Indentation : Gint);
   --  Sets the amount of extra indentation for child levels to use in
   --  Tree_View in addition to the default indentation.  The value should be
   --  specified in pixels, a value of 0 disables this feature and in this case
   --  only the default indentation will be used.
   --  This does not have any visible effects for lists.

   function Get_Rubber_Banding
     (Tree_View : access Gtk_Tree_View_Record) return Boolean;
   procedure Set_Rubber_Banding
     (Tree_View : access Gtk_Tree_View_Record;
      Enable    : Boolean);
   --  Enables or disables rubber banding in Tree_View.  If the selection mode
   --  is Selection_Multiple, rubber banding will allow the user to select
   --  multiple rows by dragging the mouse.

   function Is_Rubber_Banding_Active
     (Tree_View : access Gtk_Tree_View_Record) return Boolean;
   --  Returns whether a rubber banding operation is currently being done
   --  in Tree_View.

   function Get_Show_Expanders
     (Tree_View : access Gtk_Tree_View_Record) return Boolean;
   procedure Set_Show_Expanders
     (Tree_View : access Gtk_Tree_View_Record;
      Enabled   : Boolean);
   --  Whether to draw and enable expanders and indent child rows in Tree_View.
   --  When disabled there will be no expanders visible in trees and there will
   --  be no way to expand and collapse rows by default.  Also note that hiding
   --  the expanders will disable the default indentation.  You can set a
   --  custom indentation in this case using Set_Level_Indentation.
   --  This does not have any visible effects for lists.

   ----------------------------------
   -- Column and header operations --
   ----------------------------------

   procedure Set_Headers_Visible
     (Tree_View       : access Gtk_Tree_View_Record;
      Headers_Visible : Boolean);
   function Get_Headers_Visible
     (Tree_View : access Gtk_Tree_View_Record) return Boolean;
   --  Returns True if the headers on the Tree_View are visible.

   procedure Columns_Autosize (Tree_View : access Gtk_Tree_View_Record);
   --  Resizes all columns to their optimal width.

   function Get_Headers_Clickable
     (Tree_View : access Gtk_Tree_View_Record) return Boolean;
   procedure Set_Headers_Clickable
     (Tree_View : access Gtk_Tree_View_Record;
      Setting   : Boolean);
   --  Allow the column title buttons to be clicked.

   procedure Set_Rules_Hint
     (Tree_View : access Gtk_Tree_View_Record;
      Setting   : Boolean);
   function Get_Rules_Hint
     (Tree_View : access Gtk_Tree_View_Record) return Boolean;
   --  This function tells GtkAda that the user interface for your
   --  application requires users to read across tree rows and associate
   --  cells with one another. By default, GtkAda will then render the tree
   --  with alternating row colors. Do *not* use it just because you prefer the
   --  appearance of the ruled tree; that's a question for the theme. Some
   --  themes will draw tree rows in alternating colors even when rules are
   --  turned off, and users who prefer that appearance all the time can choose
   --  those themes. You should call this function only as a *semantic*
   --  hint to the theme engine that your tree makes alternating colors
   --  useful from a functional standpoint (since it has lots of columns,
   --  generally).

   -----------------------------
   -- Public Column functions --
   -----------------------------

   function Append_Column
     (Tree_View : access Gtk_Tree_View_Record;
      Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column) return Gint;
   --  Append Column to the list of columns.

   function Remove_Column
     (Tree_View : access Gtk_Tree_View_Record;
      Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column) return Gint;
   --  Remove Column from Tree_View.
   --  Return value: The number of columns in Tree_View after removing.

   function Insert_Column
     (Tree_View : access Gtk_Tree_View_Record;
      Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Position  : Gint := -1) return Gint;
   --  Insert the Column into the Tree_View at Position.
   --  If Position is -1, then the column is inserted at the end.
   --  Return the number of columns in Tree_View after insertion.

   function Insert_Column_With_Data_Func
     (Tree_View : access Gtk_Tree_View_Record;
      Position  : Gint;
      Title     : String;
      Cell      : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Func      : Gtk.Tree_View_Column.Cell_Data_Func)
      return Gint;
   --  Convenience function that inserts a new column into the tree view
   --  with the given cell renderer and a function to set cell renderer
   --  attributes (normally using data from the model). See also
   --  Gtk.Tree_View_Column.Set_Cell_Data_Func and
   --  Gtk.Tree_View_Column.Pack_Start.
   --  If Tree_View has "fixed_height" mode enabled, then Column must have its
   --  "sizing" property set to be TREE_VIEW_COLUMN_FIXED.
   --
   --  Return value: number of columns in the tree view post-insert

   function Get_Column
     (Tree_View : access Gtk_Tree_View_Record;
      N         : Gint)
      return Gtk.Tree_View_Column.Gtk_Tree_View_Column;
   --  Gets the Gtk_Tree_View_Column at the given position in the Tree_View.

   function Get_Tree_View
     (Tree_Column : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record)
      return Gtk_Tree_View;
   --  Returns the Gtk_Tree_View wherein Tree_Column has been inserted.  If
   --  Column is currently not inserted in any tree view, null is
   --  returned.

   function Get_Columns
     (Tree_View : access Gtk_Tree_View_Record)
      return Gtk.Tree_View_Column.Column_List.Glist;
   --  Return a list of all the Gtk_Tree_View_Column s currently in Tree_View.
   --  The returned list must be freed with g_list_free ().

   procedure Move_Column_After
     (Tree_View   : access Gtk_Tree_View_Record;
      Column      : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Base_Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column);
   --  Move Column to be after to Base_Column.  If Base_Column is Null, then
   --  Column is placed in the first position.

   procedure Set_Expander_Column
     (Tree_View : access Gtk_Tree_View_Record;
      Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column);
   --  Set the column to draw the expander arrow at. It must be in Tree_View.
   --  If Column is Null, then the expander arrow is fixed at the first column.

   function Get_Expander_Column
     (Tree_View : access Gtk_Tree_View_Record)
      return Gtk.Tree_View_Column.Gtk_Tree_View_Column;
   --  Return the column that is the current expander column.
   --  This column has the expander arrow drawn next to it.

   procedure Scroll_To_Point
     (Tree_View : access Gtk_Tree_View_Record;
      Tree_X    : Gint;
      Tree_Y    : Gint);
   --  Scroll the tree view such that the top-left corner of the visible
   --  area is Tree_X, Tree_Y, where Tree_X and Tree_Y are specified
   --  in tree window coordinates. The Tree_View must be realized before
   --  this function is called. If it isn't, you probably want to be
   --  using Scroll_To_Cell.

   procedure Scroll_To_Cell
     (Tree_View : access Gtk_Tree_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Use_Align : Boolean;
      Row_Align : Gfloat;
      Col_Align : Gfloat);
   --  Move the alignments of Tree_View to the position specified by Column and
   --  Path. If Column is Null, then no horizontal scrolling occurs. Likewise,
   --  if Path is Null no vertical scrolling occurs. Row_Align determines where
   --  the row is placed, and Col_align determines where Column is placed. Both
   --  are expected to be between 0.0 and 1.0. 0.0 means left/top alignment,
   --  1.0 means right/bottom alignment, 0.5 means center.
   --  If Use_Align is False, then the alignment arguments are ignored, and the
   --  tree does the minimum amount of work to scroll the cell onto the screen.

   procedure Get_Visible_Range
     (Tree_View  : access Gtk_Tree_View_Record;
      Start_Path : out Gtk.Tree_Model.Gtk_Tree_Path;
      End_Path   : out Gtk.Tree_Model.Gtk_Tree_Path;
      Success    : out Boolean);
   --  Sets Start_path and End_path to be the first and last visible path.
   --  Note that there may be invisible paths in between.
   --  The paths should be freed with Free after use.

   procedure Row_Activated
     (Tree_View : access Gtk_Tree_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column);
   --  Activate the cell determined by Path and Column.

   procedure Expand_All (Tree_View : access Gtk_Tree_View_Record);
   --  Recursively expand all nodes in the Tree_View.

   procedure Collapse_All (Tree_View : access Gtk_Tree_View_Record);
   --  Recursively collapse all visible, expanded nodes in Tree_View.

   function Expand_Row
     (Tree_View : access Gtk_Tree_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      Open_All  : Boolean) return Boolean;
   --  Open the row so its children are visible
   --  Return True if the row existed and had children

   procedure Expand_To_Path
     (Tree_View : access Gtk_Tree_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path);
   --  Expands the row at Path. This will also expand all parent rows of
   --  Path as necessary.

   type Gtk_Tree_View_Mapping_Func is access
     procedure (Tree_View : System.Address;  --  Gtk_Tree_View
                Path      : Gtk.Tree_Model.Gtk_Tree_Path;
                User_Data : System.Address);
   pragma Convention (C, Gtk_Tree_View_Mapping_Func);
   --  Function called on each matching row. Since this is low-level, you must
   --  convert Tree_View to a proper Gtk_Tree_View with
   --     Tree := Gtk_Tree_View (Gtk.Widget.Convert (Tree_View));

   procedure Map_Expanded_Rows
     (Tree_View : access Gtk_Tree_View_Record;
      Func      : Gtk_Tree_View_Mapping_Func;
      Data      : System.Address);
   --  Calls Func on all expanded rows.

   function Collapse_Row
     (Tree_View : access Gtk_Tree_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean;
   --  Collapse a row (hides its child rows, if they exist.)

   function Row_Expanded
     (Tree_View : access Gtk_Tree_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean;
   --  Return True if the node pointed to by Path is expanded in Tree_View.

   procedure Set_Fixed_Height_Mode
     (Tree_View : access Gtk_Tree_View_Record; Enable : Boolean);
   function Get_Fixed_Height_Mode
     (Tree_View : access Gtk_Tree_View_Record) return Boolean;
   --  Enables or disables the fixed height mode of tree_view.
   --  Fixed height mode speeds up the rendering by assuming that all
   --  rows have the same height.
   --  Only enable this option if all rows are the same height and all
   --  columns are of type TREE_VIEW_COLUMN_FIXED.

   procedure Set_Hover_Expand
     (Tree_View : access Gtk_Tree_View_Record; Expand : Boolean);
   function Get_Hover_Expand
     (Tree_View : access Gtk_Tree_View_Record) return Boolean;
   --  Enables of disables the hover expansion mode of Tree_view.
   --  Hover expansion makes rows expand or collaps if the pointer
   --  moves over them.

   procedure Set_Hover_Selection
     (Tree_View : access Gtk_Tree_View_Record; Hover : Boolean);
   function Get_Hover_Selection
     (Tree_View : access Gtk_Tree_View_Record) return Boolean;
   --  Enables of disables the hover selection mode of Tree_View.
   --  Hover selection makes the selected row follow the pointer.
   --  Currently, this works only for the selection modes
   --  SELECTION_SINGLE and SELECTION_BROWSE.

   procedure Set_Cursor
     (Tree_View     : access Gtk_Tree_View_Record;
      Path          : Gtk.Tree_Model.Gtk_Tree_Path;
      Focus_Column  : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Start_Editing : Boolean);
   --  Sets the current keyboard focus to be at Path, and selects it.  This is
   --  useful when you want to focus the user's attention on a particular row.
   --  If Column is not Null, then focus is given to that column.
   --  Additionally, if Column is specified, and Start_Editing is True, then
   --  editing should be started in the specified cell.
   --  Keyboard focus is given to the widget after this is called.
   --  Please note that editing can only happen when the widget is realized.

   procedure Get_Cursor
     (Tree_View    : access Gtk_Tree_View_Record;
      Path         : out Gtk.Tree_Model.Gtk_Tree_Path;
      Focus_Column : out Gtk.Tree_View_Column.Gtk_Tree_View_Column);
   --  Fills in Path and Focus_Column with the current path and focus column.
   --  If the cursor isn't currently set, then *path will be Null. If no column
   --  currently has focus, then *focus_column will be Null.

   procedure Set_Cursor_On_Cell
     (Tree_View     : access Gtk_Tree_View_Record;
      Path          : Gtk.Tree_Model.Gtk_Tree_Path;
      Focus_Column  : Gtk.Tree_View_Column.Gtk_Tree_View_Column := null;
      Focus_Cell    : Gtk.Cell_Renderer.Gtk_Cell_Renderer := null;
      Start_Editing : Boolean);
   --  Sets the current keyboard focus to be at Path, and selects it. This is
   --  useful when you want to focus the user's attention on a particular row.
   --  If Focus_Column is not null, then focus is given to the column specified
   --  by it. If Focus_Column and Focus_Cell are not null, and Focus_Column
   --  contains 2 or more editable or activatable cells, then focus is given to
   --  the cell specified by Focus_Cell. Additionally, if Focus_Column is
   --  specified, and Start_Editing is true, then editing should be started in
   --  the specified cell. This function is often followed by
   --  gtk.widget.grab_focus (Tree_View) in order to give keyboard focus to the
   --  widget. Please note that editing can only happen when the widget is
   --  realized.

   function Get_Bin_Window
     (Tree_View : access Gtk_Tree_View_Record) return Gdk.Window.Gdk_Window;
   --  Return the window that Tree_View renders to.
   --  This is used primarily to compare to Get_Window (Event) to confirm that
   --  the event on Tree_View is on the right window.

   type Gtk_Tree_View_Row_Separator_Func is access
     function (Model     : System.Address;
               Iter      : Gtk.Tree_Model.Gtk_Tree_Iter;
               User_Data : System.Address) return Gboolean;
   pragma Convention (C, Gtk_Tree_View_Row_Separator_Func);
   --  This function is used to determine whether a row should be drawn with a
   --  separator. If it returns True, a separator is displayed.
   --  This is a low-level function, since it isn't used very often, and you
   --  should convert the Model to a Gtk_Tree_Model with
   --     declare
   --        Stub : Gtk_Tree_Model_Record;
   --     begin
   --        My_Model := Gtk_Tree_Model (Get_User_Data (Model, Stub));
   --     end;

   procedure Set_Row_Separator_Func
     (Tree_View : access Gtk_Tree_View_Record;
      Func      : Gtk_Tree_View_Row_Separator_Func;
      Data      : System.Address;
      Destroy   : Glib.G_Destroy_Notify_Address := null);
   function Get_Row_Separator_Func
     (Tree_View : access Gtk_Tree_View_Record)
      return Gtk_Tree_View_Row_Separator_Func;
   --  Sets the row separator function, which is used to determine
   --  whether a row should be drawn as a separator. If the row separator
   --  function is NULL, no separators are drawn. This is the default value.

   procedure Get_Path_At_Pos
     (Tree_View : access Gtk_Tree_View_Record;
      X         : Gint;
      Y         : Gint;
      Path      : out Gtk.Tree_Model.Gtk_Tree_Path;
      Column    : out Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Cell_X    : out Gint;
      Cell_Y    : out Gint;
      Row_Found : out Boolean);
   --  Find the path at the point (X, Y) relative to Window.
   --  If Window is null, then the point is found relative to the widget
   --  coordinates. This function is expected to be called after an event.
   --  It is primarily for things like popup menus. Path will be filled
   --  with the Gtk_Tree_Path at that point. It should be freed with
   --  Tree_Path_Free. Column will be filled with the column at that point.
   --  Cell_X and Cell_Y return the coordinates relative to the cell background
   --  (i.e. the background_area passed to gtk_cell_renderer_render()).
   --  This function only works if Tree_View is realized.
   --  Row_Found is set to True if a row exists at that coordinate.

   procedure Get_Cell_Area
     (Tree_View : access Gtk_Tree_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Rect      : out Gdk.Rectangle.Gdk_Rectangle);
   --  Fills the bounding rectangle in tree window coordinates for the cell at
   --  the row specified by Path and the column specified by Column. If Path is
   --  Null, or points to a path not currently displayed, the Y and Height
   --  fields of the rectangle will be filled with 0. If Column is Null,
   --  the X and Width fields will be filled with 0.
   --  The sum of all cell rects does not cover the entire tree;
   --  there are extra pixels in between rows, for example. The
   --  returned rectangle is equivalent to the Cell_Area passed to
   --  gtk_cell_renderer_render().  This function is only valid if Tree_View is
   --  realized.

   procedure Get_Background_Area
     (Tree_View : access Gtk_Tree_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Rect      : out Gdk.Rectangle.Gdk_Rectangle);
   --  Fills the bounding rectangle in tree window coordinates for the cell
   --  at the row specified by Path and the column specified by Column.  If
   --  Path is Null, or points to a node not found in the tree, the Y and
   --  Height fields of the rectangle will be filled with 0. If Column is Null,
   --  the X and Width fields will be filled with 0.
   --  The returned rectangle is equivalent to the Background_Area passed to
   --  Gtk.Cell_Renderer.Render.  These background areas tile to cover the
   --  entire tree window (except for the area used for
   --  header buttons). Contrast with the cell_area, returned by
   --  gtk_tree_view_get_cell_area(), which returns only the cell itself,
   --  excluding surrounding borders and the tree expander area.

   procedure Get_Visible_Rect
     (Tree_View    : access Gtk_Tree_View_Record;
      Visible_Rect : out Gdk.Rectangle.Gdk_Rectangle);
   --  Fills Visible_Rect with the currently-visible region of the
   --  buffer, in tree coordinates. Convert to widget coordinates with
   --  gtk_tree_view_tree_to_widget_coords(). Tree coordinates start at
   --  0,0 for row 0 of the tree, and cover the entire scrollable area of
   --  the tree.

   ------------------------
   -- Coordinate Systems --
   ------------------------

   --  Several different coordinate systems are exposed in the Gtk.Tree_View
   --  API. These are:
   --                   _____________________________
   --               /   | _________________________ |
   --              /    | |                       | |
   --             /     | |     Header_Window     | |
   --             |     | |_______________________| |
   --             |     | _________________________ |
   --             |     | |  ...................  | |    \
   --    widget- -+     | |  :                 :  | |     \
   --  relative   |     | |  :   Bin_Window    :  | |      \
   --             |     | |  :                 :  | |       \
   --             |     | |  :                 :  | |       |
   --             \     | |  :                 :  | |       |
   --              \    | |__:_________________:__| |       +- tree
   --               \   |____:_________________:____|       |  coordinates
   --                        :                 :            |
   --                        :                 :            /
   --                        :                 :           /
   --                        :                 :          /
   --                        :.................:         /
   --
   --  Widget coordinates ------ coordinates relative to the widget (usually
   --                            widget->window).
   --
   --  Bin window coordinates -- coordinates relative to the window that
   --                            Gtk_Tree_View renders to.
   --
   --  Tree coordinates -------- coordinates relative to the entire scrollable
   --                            area of Gtk_Tree_View. These coordinates start
   --                            at (0, 0) for row 0 of the tree.
   --
   --  Several functions are available for converting between the different
   --  coordinate systems. The most common translations are between widget and
   --  bin window coordinates and between bin window and tree coordinates. For
   --  the former you can use Convert_Widget_To_Bin_Window_Coords (and vice
   --  versa), for the latter Convert_Bin_Window_To_Tree_Coords (and vice
   --  versa).

   procedure Widget_To_Tree_Coords
     (Tree_View : access Gtk_Tree_View_Record;
      Wx        : Gint;
      Wy        : Gint;
      Tx        : out Gint;
      Ty        : out Gint);
   pragma Obsolescent; --  Widget_To_Tree_Coords
   --  Converts widget coordinates to coordinates for the
   --  tree window (the full scrollable area of the tree).
   --
   --  Obsolescent; use Convert_Widget_To_Tree_Coords instead.

   procedure Tree_To_Widget_Coords
     (Tree_View : access Gtk_Tree_View_Record;
      Tx        : Gint;
      Ty        : Gint;
      Wx        : out Gint;
      Wy        : out Gint);
   pragma Obsolescent; --  Tree_To_Widget_Coords
   --  Converts tree coordinates (coordinates in full scrollable area of
   --  the tree) to widget coordinates.
   --
   --  Obsolescent; use Convert_Tree_To_Widget_Coords instead.

   procedure Convert_Widget_To_Tree_Coords
     (Tree_View : access Gtk_Tree_View_Record;
      Wx        : Gint;
      Wy        : Gint;
      Tx        : out Gint;
      Ty        : out Gint);
   --  Converts widget coordinates to coordinates for the tree (the full
   --  scrollable area of the tree).

   procedure Convert_Tree_To_Widget_Coords
     (Tree_View : access Gtk_Tree_View_Record;
      Tx        : Gint;
      Ty        : Gint;
      Wx        : out Gint;
      Wy        : out Gint);
   --  Converts tree coordinates (coordinates in full scrollable area of the
   --  tree) to widget coordinates.

   procedure Convert_Bin_Window_To_Tree_Coords
     (Tree_View : access Gtk_Tree_View_Record;
      Bx        : Gint;
      By        : Gint;
      Tx        : out Gint;
      Ty        : out Gint);
   --  Converts bin_window coordinates to coordinates for the
   --  tree (the full scrollable area of the tree).

   procedure Convert_Tree_To_Bin_Window_Coords
     (Tree_View : access Gtk_Tree_View_Record;
      Tx        : Gint;
      Ty        : Gint;
      Bx        : out Gint;
      By        : out Gint);
   --  Converts tree coordinates (coordinates in full scrollable area of the
   --  tree) to bin_window coordinates.

   procedure Convert_Widget_To_Bin_Window_Coords
     (Tree_View : access Gtk_Tree_View_Record;
      Wx        : Gint;
      Wy        : Gint;
      Bx        : out Gint;
      By        : out Gint);
   --  Converts widget coordinates to coordinates for the Bin_Window
   --  (see Get_Bin_Window).

   procedure Convert_Bin_Window_To_Widget_Coords
     (Tree_View : access Gtk_Tree_View_Record;
      Bx        : Gint;
      By        : Gint;
      Wx        : out Gint;
      Wy        : out Gint);
   --  Converts Bin_Window coordinates (see Get_Bin_Window) to widget
   --  relative coordinates.

   ---------------
   -- Searching --
   ---------------

   procedure Set_Enable_Search
     (Tree_View     : access Gtk_Tree_View_Record;
      Enable_Search : Boolean);
   function Get_Enable_Search
     (Tree_View : access Gtk_Tree_View_Record) return Boolean;
   --  If enable_search is set, then the user can type in text to search
   --  through the tree interactively (this is sometimes called "typeahead
   --  find").
   --  Note that even if this is FALSE, the user can still initiate a search
   --  using the "start-interactive-search" key binding.

   procedure Set_Search_Column
     (Tree_View : access Gtk_Tree_View_Record;
      Column    : Gint);
   function Get_Search_Column
     (Tree_View : access Gtk_Tree_View_Record) return Gint;
   --  Sets column as the column where the interactive search code should
   --  search in.
   --  If the sort column is set, users can use the "start-interactive-search"
   --  key binding to bring up search popup. The enable-search property
   --  controls whether simply typing text will also start an interactive
   --  search.
   --  Note that column refers to a column of the model.

   type Gtk_Tree_View_Search_Equal_Func is access
     function (Model  : System.Address;
               Column : Gint;
               Key    : Interfaces.C.Strings.chars_ptr;
               Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
               User_Data : System.Address) return Gboolean;
   pragma Convention (C, Gtk_Tree_View_Search_Equal_Func);
   --  The function used to compare for the interactive search capabilities.
   --  This function should return False on match, similar to C's strcmp().
   --  This is a low-level function, and you should convert the model to a
   --  Gtk_Tree_Model (see Gtk_Tree_View_Row_Separator_Func

   procedure Set_Search_Equal_Func
     (Tree_View         : access Gtk_Tree_View_Record;
      Search_Equal_Func : Gtk_Tree_View_Search_Equal_Func;
      Search_User_Data  : System.Address;
      Search_Destroy    : G_Destroy_Notify_Address := null);
   function Get_Search_Equal_Func
     (Tree_View : access Gtk_Tree_View_Record)
      return Gtk_Tree_View_Search_Equal_Func;
   --  Sets the compare function for the interactive search capabilities

   function Get_Search_Entry
     (Tree_View : access Gtk_Tree_View_Record) return Gtk.GEntry.Gtk_Entry;
   procedure Set_Search_Entry
     (Tree_View : access Gtk_Tree_View_Record;
      The_Entry : access Gtk.GEntry.Gtk_Entry_Record'Class);
   --  Gets/Sets the entry which the interactive search code will use for this
   --  Tree_View.  This is useful when you want to provide a search entry
   --  in our interface at all time at a fixed position.  A null Gtk_Entry will
   --  make the interactive search code use the built-in popup entry again.

   type Gtk_Tree_View_Search_Position_Func is access procedure
     (Tree_View     : System.Address;  --  Get_Object (Gtk_Tree_View_Record)
      Search_Dialog : System.Address;  --  Get_Object (Gtk_Widget'Class)
      User_Data     : System.Address);
   pragma Convention (C, Gtk_Tree_View_Search_Position_Func);

   function Get_Search_Position_Func
     (Tree_View : access Gtk_Tree_View_Record)
      return Gtk_Tree_View_Search_Position_Func;
   procedure Set_Search_Position_Func
     (Tree_View : access Gtk_Tree_View_Record;
      Func      : Gtk_Tree_View_Search_Position_Func;
      Data      : System.Address;
      Destroy   : G_Destroy_Notify);
   --  Gets/Sets the function to use when positioning the search dialog.

   --------------
   -- Tooltips --
   --------------

   procedure Set_Tooltip_Cell
     (Tree_View : access Gtk_Tree_View_Record;
      Tooltip   : access Gtk.Tooltip.Gtk_Tooltip_Record'Class;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      Column    : access
                  Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
      Cell      : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);
   --  Sets the tip area of Tooltip to the area Path, Column and Cell have
   --  in common.  For example, if Path is null and Column is set, the tip
   --  area will be set to the full area covered by Column.  See also
   --  Gtk.Tooltip.Set_Tip_Area.
   --
   --  Note that if Path is not specified and Cell is set and part of a column
   --  containing the expander, the tooltip might not show and hide at the
   --  correct position.  In such cases Path must be set to the current node
   --  under the mouse cursor for this function to operate correctly.
   --
   --  See also Set_Tooltip_Column for a simpler alternative.

   procedure Set_Tooltip_Column
     (Tree_View : access Gtk_Tree_View_Record;
      Column    : Gint);
   function Get_Tooltip_Column
     (Tree_View : access Gtk_Tree_View_Record) return Gint;
   --  If you only plan to have simple (text-only) tooltips on full rows, you
   --  can use this function to have Gtk_Tree_View handle these automatically
   --  for you. Column should be set to the column in tree_view's model
   --  containing the tooltip texts, or -1 to disable this feature.
   --
   --  When enabled, "has-tooltip" will be set to TRUE and tree_view will
   --  connect a "query-tooltip" signal handler.
   --
   --  Note that the signal handler sets the text with
   --  gtk_tooltip_set_markup(), so &, <, etc have to be escaped in the text.

   procedure Get_Tooltip_Context
     (Tree_View     : access Gtk_Tree_View_Record;
      X             : in out Glib.Gint;
      Y             : in out Glib.Gint;
      Keyboard_Mode : Boolean;
      Model         : out Gtk.Tree_Model.Gtk_Tree_Model;
      Path          : out Gtk.Tree_Model.Gtk_Tree_Path;
      Iter          : out Gtk.Tree_Model.Gtk_Tree_Iter;
      Success       : out Boolean);
   --  This function is supposed to be used in a "query-tooltip" signal handler
   --  for Gtk_Tree_View. The x, y and keyboard_tip values which are received
   --  in the signal handler, should be passed to this function without
   --  modification.
   --
   --  The Success indicates whether there is a tree view row at the given
   --  coordinates (True) or not (False) for mouse tooltips. For keyboard
   --  tooltips the row returned will be the cursor row. When True, then any of
   --  model, path and iter which have been provided will be set to point to
   --  that row and the corresponding model. x and y will always be converted
   --  to be relative to tree_view's bin_window if keyboard_tooltip is False.

   procedure Set_Tooltip_Row
     (Tree_View : access Gtk_Tree_View_Record;
      Tooltip   : access Gtk.Tooltip.Gtk_Tooltip_Record'Class;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path);
   --  Sets the tip area of tooltip to be the area covered by the row at path.
   --  See also Set_Tooltip_Column for a simpler alternative. See also
   --  Gtk.Tooltips.Set_Tip_Area.

   ------------------------
   -- Columns reordering --
   ------------------------

   procedure Set_Reorderable
     (Tree_View   : access Gtk_Tree_View_Record;
      Reorderable : Boolean);
   function Get_Reorderable
     (Tree_View : access Gtk_Tree_View_Record) return Boolean;
   --  This function is a convenience function to allow you to reorder models
   --  that support the Gtk_Drag_Source_Iface and the Gtk_Drag_Dest_Iface. Both
   --  Gtk_Tree_Store and Gtk_List_Store support these.
   --  If Reorderable is True, then the user can reorder the model by dragging
   --  and dropping columns.  The developer can listen to these changes by
   --  connecting to the model's signals.
   --  This function does not give you any degree of control over the order
   --  - any reorderering is allowed. If more control is needed, you should
   --  probably handle drag and drop manually.

   type Gtk_Tree_View_Column_Drop_Func is access
     function (Tree_View   : System.Address; --  Gtk_Tree_View
               Column      : System.Address; --  Gtk_Tree_View_Column
               Prev_Column : System.Address; --  Gtk_Tree_View_Column
               Next_Column : System.Address; --  Gtk_Tree_View_Column
               User_Data   : System.Address) return Gboolean;
   pragma Convention (C, Gtk_Tree_View_Column_Drop_Func);
   --  This function is used to determine whether a column may be dropped in
   --  a given location.
   --  This function is called on every column pair in turn at the beginning of
   --  a column drag to determine where a drop can take place. The arguments
   --  are: the tree_view, the column being dragged, the two columns
   --  determining the drop spot, and user_data. If either of the column
   --  arguments for the drop spot are null, then they indicate an edge.
   --
   --  This is a low-level function, and you should use Get_User_Data to
   --  convert to the appropriate GtkAda widgets

   procedure Set_Column_Drag_Function
     (Tree_View : access Gtk_Tree_View_Record;
      Func      : Gtk_Tree_View_Column_Drop_Func;
      User_Data : System.Address;
      Destroy   : Glib.G_Destroy_Notify_Address);
   --  Sets a user function for determining where a column may be dropped.
   --  If Func is set to be %NULL, then Tree_View reverts to the default
   --  behavior of allowing all columns to be dropped everywhere.

   -------------------
   -- Drag-and-drop --
   -------------------

   procedure Enable_Model_Drag_Dest
     (Tree_View : access Gtk_Tree_View_Record;
      Targets   : Gtk.Selection.Target_Entry_Array;
      Actions   : Gdk.Dnd.Drag_Action);
   --  Turns Tree_View into a drop destination for automatic drag-and-drop.
   --  Targets is the table of targets that the drag will support.
   --  Actions is a bitmask of possible actions for a drag to this widget.

   procedure Enable_Model_Drag_Source
     (Tree_View         : access Gtk_Tree_View_Record;
      Start_Button_Mask : Gdk.Types.Gdk_Modifier_Type;
      Targets           : Gtk.Selection.Target_Entry_Array;
      Actions           : Gdk.Dnd.Drag_Action);
   --  Turns Tree_View into a drag source for automatic DND.
   --  Targets is the list of targets that the drag will support.
   --  Actions is the bitmask of possible actions for a drag from this widget.
   --  Start_Button_Mask is the mask of allowed buttons to start the drag.
   --  You need to connect to the usual dnd signals (see gtk-dnd.ads) to
   --  provide the actual data upon request.

   procedure Unset_Rows_Drag_Source (Tree_View : access Gtk_Tree_View_Record);
   --  Undoes the effect of Enable_Model_Drag_Source.

   procedure Unset_Rows_Drag_Dest (Tree_View : access Gtk_Tree_View_Record);
   --  Undoes the effect of Enable_Model_Drag_Dest.

   function Create_Row_Drag_Icon
     (Tree_View : access Gtk_Tree_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gdk.Pixmap.Gdk_Pixmap;
   --  Creates a Gdk_Pixmap representation of the row at path. This image is
   --  used for a drag icon.
   --  The returned pixmap must be freed by the user

   procedure Get_Dest_Row_At_Pos
     (Tree_View : access Gtk_Tree_View_Record;
      Drag_X    : Gint;
      Drag_Y    : Gint;
      Path      : out Gtk.Tree_Model.Gtk_Tree_Path;
      Pos       : out Gtk_Tree_View_Drop_Position;
      Success   : out Boolean);
   --  Determines the destination row for a given position.
   --  (Drag_X, Drag_Y) is the position to determine the destination row for.

   procedure Set_Drag_Dest_Row
     (Tree_View : access Gtk_Tree_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      Pos       : Gtk_Tree_View_Drop_Position);
   procedure Get_Drag_Dest_Row
     (Tree_View : access Gtk_Tree_View_Record;
      Path      : out Gtk.Tree_Model.Gtk_Tree_Path;
      Pos       : out Gtk_Tree_View_Drop_Position);
   --  Sets or gets information about the row that is highlighted for feedback.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.

   --  <properties>
   --  Name:  Enable_Grid_Lines_Property
   --  Type:  Enum
   --  Descr: Whether grid lines should be drawn in the tree view
   --
   --  Name:  Enable_Search_Property
   --  Type:  Boolean
   --  Descr: View allows user to search through columns interactively
   --
   --  Name:  Enable_Tree_Lines_Property
   --  Type:  Boolean
   --  Descr: Whether tree lines should be drawn in the tree view
   --
   --  Name:  Expander_Column_Property
   --  Type:  Object
   --  Descr: Set the column for the expander column
   --
   --  Name:  Fixed_Height_Mode_Property
   --  Type:  Boolean
   --  Descr: Speeds up Gtk_Tree_View by assuming that all rows have the same
   --         height
   --
   --  Name:  Hadjustment_Property
   --  Type:  Object
   --  Descr: Horizontal Adjustment for the widget
   --
   --  Name:  Headers_Clickable_Property
   --  Type:  Boolean
   --  Descr: Column headers respond to click events
   --
   --  Name:  Headers_Visible_Property
   --  Type:  Boolean
   --  Descr: Show the column header buttons
   --
   --  Name:  Hover_Expand_Property
   --  Type:  Boolean
   --  Descr: Whether rows should be expanded/collapsed when the pointer moves
   --         over them
   --
   --  Name:  Hover_Selection_Property
   --  Type:  Boolean
   --  Descr: Whether the selection should follow the pointer
   --
   --  Name:  Level_Indentation_Property
   --  Type:  Int
   --  Descr: Extra indentation for each level
   --
   --  Name:  Model_Property
   --  Type:  Object
   --  Descr: The model for the tree view
   --
   --  Name:  Reorderable_Property
   --  Type:  Boolean
   --  Descr: View is reorderable
   --
   --  Name:  Rubber_Banding_Property
   --  Type:  Boolean
   --  Descr: Whether to enable selection of multiple items by dragging the
   --         mouse pointer
   --
   --  Name:  Rules_Hint_Property
   --  Type:  Boolean
   --  Descr: Set a hint to the theme engine to draw rows in alternating colors
   --
   --  Name:  Search_Column_Property
   --  Type:  Int
   --  Descr: Model column to search through when searching through code
   --
   --  Name:  Show_Expanders_Property
   --  Type:  Boolean
   --  Descr: View has expanders
   --
   --  Name:  Tooltip_Column_Property
   --  Type:  Int
   --  Descr: The column in the model containing the tooltip texts for the rows
   --
   --  Name:  Vadjustment_Property
   --  Type:  Object
   --  Descr: Vertical Adjustment for the widget
   --  </properties>

   Enable_Grid_Lines_Property : constant Glib.Properties.Property_Enum;
   Enable_Search_Property     : constant Glib.Properties.Property_Boolean;
   Enable_Tree_Lines_Property : constant Glib.Properties.Property_Boolean;
   Expander_Column_Property   : constant Glib.Properties.Property_Object;
   Fixed_Height_Mode_Property : constant Glib.Properties.Property_Boolean;
   Hadjustment_Property       : constant Glib.Properties.Property_Object;
   Headers_Clickable_Property : constant Glib.Properties.Property_Boolean;
   Headers_Visible_Property   : constant Glib.Properties.Property_Boolean;
   Hover_Expand_Property      : constant Glib.Properties.Property_Boolean;
   Hover_Selection_Property   : constant Glib.Properties.Property_Boolean;
   Level_Indentation_Property : constant Glib.Properties.Property_Int;
   Model_Property             : constant Glib.Properties.Property_Object;
   Reorderable_Property       : constant Glib.Properties.Property_Boolean;
   Rubber_Banding_Property    : constant Glib.Properties.Property_Boolean;
   Rules_Hint_Property        : constant Glib.Properties.Property_Boolean;
   Search_Column_Property     : constant Glib.Properties.Property_Int;
   Show_Expanders_Property    : constant Glib.Properties.Property_Boolean;
   Tooltip_Column_Property    : constant Glib.Properties.Property_Int;
   Vadjustment_Property       : constant Glib.Properties.Property_Object;

   ----------------------
   -- Style Properties --
   ----------------------
   --  The following properties can be changed through the gtk theme and
   --  configuration files, and retrieved through Gtk.Widget.Style_Get_Property

   --  <style_properties>
   --  Name:  Allow_Rules_Property
   --  Type:  Boolean
   --  Descr: Allow drawing of alternating color rows
   --
   --  Name:  Even_Row_Color_Property
   --  Type:  Boxed
   --  Descr: Color to use for even rows
   --
   --  Name:  Expander_Size_Property
   --  Type:  Int
   --  Descr: Size of the expander arrow
   --
   --  Name:  Grid_Line_Pattern_Property
   --  Type:  String
   --  Descr: Dash pattern used to draw the tree view grid lines
   --
   --  Name:  Grid_Line_Width_Property
   --  Type:  Int
   --  Descr: Width, in pixels, of the tree view grid lines
   --
   --  Name:  Horizontal_Separator_Property
   --  Type:  Int
   --  Descr: Horizontal space between cells.  Must be an even number
   --
   --  Name:  Indent_Expanders_Property
   --  Type:  Boolean
   --  Descr: Make the expanders indented
   --
   --  Name:  Odd_Row_Color_Property
   --  Type:  Boxed
   --  Descr: Color to use for odd rows
   --
   --  Name:  Row_Ending_Details_Property
   --  Type:  Boolean
   --  Descr: Enable extended row background theming
   --
   --  Name:  Tree_Line_Pattern_Property
   --  Type:  String
   --  Descr: Dash pattern used to draw the tree view lines
   --
   --  Name:  Tree_Line_Width_Property
   --  Type:  Int
   --  Descr: Width, in pixels, of the tree view lines
   --
   --  Name:  Vertical_Separator_Property
   --  Type:  Int
   --  Descr: Vertical space between cells.  Must be an even number
   --  </style_properties>

   Allow_Rules_Property          : constant Glib.Properties.Property_Boolean;
   --  Even_Row_Color_Property   : constant Glib.Properties.Property_Boxed;
   Expander_Size_Property        : constant Glib.Properties.Property_Int;
   Grid_Line_Pattern_Property    : constant Glib.Properties.Property_String;
   Grid_Line_Width_Property      : constant Glib.Properties.Property_Int;
   Horizontal_Separator_Property : constant Glib.Properties.Property_Int;
   Indent_Expanders_Property     : constant Glib.Properties.Property_Boolean;
   --  Odd_Row_Color_Property    : constant Glib.Properties.Property_Boxed;
   Row_Ending_Details_Property   : constant Glib.Properties.Property_Boolean;
   Tree_Line_Pattern_Property    : constant Glib.Properties.Property_String;
   Tree_Line_Width_Property      : constant Glib.Properties.Property_Int;
   Vertical_Separator_Property   : constant Glib.Properties.Property_Int;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "set_scroll_adjustments"
   --    procedure Handler (Widget : access Gtk_Tree_View_Record'Class;
   --       Hadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class;
   --       Vadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --
   --  - "row_activated"
   --    procedure Handler (Widget : access Gtk_Tree_View_Record'Class;
   --       Path : Gtk.Tree_Model.Gtk_Tree_Path;
   --       Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column);
   --
   --  - "test_expand_row"
   --    function Handler (Widget : access Gtk_Tree_View_Record'Class;
   --       Iter : access Gtk.Tree_Iter.Gtk_Tree_Iter_Record'Class;
   --       Path : Gtk.Tree_Model.Gtk_Tree_Path)
   --       return Gboolean;
   --
   --  - "test_collapse_row"
   --    function Handler (Widget : access Gtk_Tree_View_Record'Class;
   --       Iter : access Gtk.Tree_Iter.Gtk_Tree_Iter_Record'Class;
   --       Path : Gtk.Tree_Model.Gtk_Tree_Path)
   --       return Gboolean;
   --
   --  - "row_expanded"
   --    procedure Handler (Widget : access Gtk_Tree_View_Record'Class;
   --       Iter : access Gtk.Tree_Iter.Gtk_Tree_Iter_Record'Class;
   --       Path : Gtk.Tree_Model.Gtk_Tree_Path);
   --
   --  - "row_collapsed"
   --    procedure Handler (Widget : access Gtk_Tree_View_Record'Class;
   --       Iter : access Gtk.Tree_Iter.Gtk_Tree_Iter_Record'Class;
   --       Path : Gtk.Tree_Model.Gtk_Tree_Path);
   --
   --  - "columns_changed"
   --    procedure Handler (Widget : access Gtk_Tree_View_Record'Class);
   --
   --  - "move_cursor"
   --    procedure Handler (Widget : access Gtk_Tree_View_Record'Class;
   --       Step : Gtk_Movement_Step;
   --       Count : Gint);
   --
   --  - "select_all"
   --    procedure Handler (Widget : access Gtk_Tree_View_Record'Class);
   --
   --  - "select_cursor_row"
   --    procedure Handler (Widget : access Gtk_Tree_View_Record'Class;
   --       Start_Editing : Boolean);
   --
   --  - "toggle_cursor_row"
   --    procedure Handler (Widget : access Gtk_Tree_View_Record'Class);
   --
   --  - "expand_collapse_cursor_row"
   --    procedure Handler (Widget : access Gtk_Tree_View_Record'Class;
   --       Logical : Boolean;
   --       Expand : Boolean;
   --       Open_All : Boolean);
   --
   --  - "select_cursor_parent"
   --    procedure Handler (Widget : access Gtk_Tree_View_Record'Class);
   --
   --  - "start_interactive_search"
   --    procedure Handler (Widget : access Gtk_Tree_View_Record'Class);
   --
   --  </signals>

   Signal_Columns_Changed            : constant Glib.Signal_Name :=
                                         "columns_changed";
   Signal_Cursor_Changed             : constant Glib.Signal_Name :=
                                         "cursor_changed";
   Signal_Expand_Collapse_Cursor_Row : constant Glib.Signal_Name :=
                                         "expand_collapse_cursor_row";
   Signal_Move_Cursor                : constant Glib.Signal_Name :=
                                         "move_cursor";
   Signal_Row_Activated              : constant Glib.Signal_Name :=
                                         "row_activated";
   Signal_Row_Collapsed              : constant Glib.Signal_Name :=
                                         "row_collapsed";
   Signal_Row_Expanded               : constant Glib.Signal_Name :=
                                         "row_expanded";
   Signal_Select_All                 : constant Glib.Signal_Name :=
                                         "select_all";
   Signal_Select_Cursor_Parent       : constant Glib.Signal_Name :=
                                         "select_cursor_parent";
   Signal_Select_Cursor_Row          : constant Glib.Signal_Name :=
                                         "select_cursor_row";
   Signal_Set_Scroll_Adjustments     : constant Glib.Signal_Name :=
                                         "set_scroll_adjustments";
   Signal_Start_Interactive_Search   : constant Glib.Signal_Name :=
                                         "start_interactive_search";
   Signal_Test_Collapse_Row          : constant Glib.Signal_Name :=
                                         "test_collapse_row";
   Signal_Test_Expand_Row            : constant Glib.Signal_Name :=
                                         "test_expand_row";
   Signal_Toggle_Cursor_Row          : constant Glib.Signal_Name :=
                                         "toggle_cursor_row";
   Signal_Unselect_All               : constant Glib.Signal_Name :=
                                         "unselect_all";

private
   type Gtk_Tree_View_Record is
     new Gtk.Container.Gtk_Container_Record with null record;

   Enable_Grid_Lines_Property : constant Glib.Properties.Property_Enum :=
     Glib.Properties.Build ("enable-grid-lines");
   Enable_Search_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("enable-search");
   Enable_Tree_Lines_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("enable-tree-lines");
   Expander_Column_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("expander-column");
   Fixed_Height_Mode_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("fixed-height-mode");
   Hadjustment_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("hadjustment");
   Headers_Clickable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("headers-clickable");
   Headers_Visible_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("headers-visible");
   Hover_Expand_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("hover-expand");
   Hover_Selection_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("hover-selection");
   Level_Indentation_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("level-indentation");
   Model_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("model");
   Reorderable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("reorderable");
   Rubber_Banding_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("rubber-banding");
   Rules_Hint_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("rules-hint");
   Search_Column_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("search-column");
   Show_Expanders_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-expanders");
   Tooltip_Column_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("tooltip-column");
   Vadjustment_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("vadjustment");

   Allow_Rules_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("allow-rules");
--     Even_Row_Color_Property : constant Glib.Properties.Property_Boxed :=
--       Glib.Properties.Build ("even-row-color");
   Expander_Size_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("expander-size");
   Grid_Line_Pattern_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("grid-line-pattern");
   Grid_Line_Width_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("grid-line-width");
   Horizontal_Separator_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("horizontal-separator");
   Indent_Expanders_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("indent-expanders");
--     Odd_Row_Color_Property : constant Glib.Properties.Property_Boxed :=
--       Glib.Properties.Build ("odd-row-color");
   Row_Ending_Details_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("row-ending-details");
   Tree_Line_Pattern_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("tree-line-pattern");
   Tree_Line_Width_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("tree-line-width");
   Vertical_Separator_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("vertical-separator");

   pragma Import (C, Get_Type, "gtk_tree_view_get_type");
end Gtk.Tree_View;

--  No binding: gtk_tree_view_set_destroy_count_func
--  No binding: gtk_tree_view_insert_column_with_attributes
