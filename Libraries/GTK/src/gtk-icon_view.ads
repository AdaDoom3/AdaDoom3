-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2006-2010 AdaCore                    --
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
--  Gtk_Icon_View provides an alternative view on a list model. It displays the
--  model as a grid of icons with labels. Like Gtk_Tree_View, it allows to
--  select one or multiple items (depending on the selection mode, see
--  Set_Selection_Mode). In addition to selection with the arrow keys,
--  Gtk_Icon_View supports rubberband selection, which is controlled by
--  dragging the pointer.
--  </description>
--  <c_version>2.16.6</c_version>
--  <group>Trees and Lists</group>
--  <screenshot>icon-view</screenshot>
--  <testgtk>create_icon_view.adb</testgtk>

with Glib.Types;
with Gdk.Color;
with Gdk.Dnd;
with Gdk.Types;
with Glib.Properties;
with Gtk.Cell_Layout;
with Gtk.Cell_Renderer;
with Gtk.Container;
with Gtk.Enums;
with Gtk.Selection;
with Gtk.Tooltip;
with Gtk.Tree_Model;

package Gtk.Icon_View is

   type Gtk_Icon_View_Record is new Gtk.Container.Gtk_Container_Record with
     null record;
   type Gtk_Icon_View is access all Gtk_Icon_View_Record'Class;

   type Gtk_Icon_View_Drop_Position is
     (No_Drop,
      Drop_Into,
      Drop_Left,
      Drop_Right,
      Drop_Above,
      Drop_Below);
   --  An enum for determining where a dropped item goes.
   --  If Drop_Into, then the drop item replaces the item.

   procedure Gtk_New    (Icon_View : out Gtk_Icon_View);
   procedure Initialize (Icon_View : access Gtk_Icon_View_Record'Class);
   --  Creates a new Gtk_Icon_View widget

   procedure Gtk_New_With_Model
     (Icon_View : out Gtk_Icon_View;
      Model     : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class);
   procedure Initialize_With_Model
     (Icon_View : access Gtk_Icon_View_Record'Class;
      Model     : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class);
   --  Creates a new Gtk_Icon_View widget with the model Model.

   function Get_Type return GType;
   --  Return the internal type used for a Gtk_Icon_View.

   procedure Set_Column_Spacing
     (Icon_View      : access Gtk_Icon_View_Record;
      Column_Spacing : Glib.Gint);
   function Get_Column_Spacing
     (Icon_View : access Gtk_Icon_View_Record) return Glib.Gint;
   --  Sets the ::column-spacing property which specifies the space
   --  which is inserted between the columns of the icon view.

   procedure Set_Columns
     (Icon_View : access Gtk_Icon_View_Record;
      Columns   : Glib.Gint);
   function Get_Columns
     (Icon_View : access Gtk_Icon_View_Record) return Glib.Gint;
   --  Sets the ::columns property which determines in how many columns the
   --  icons are arranged. If Columns is -1, the number of columns will be
   --  chosen automatically to fill the available area.

   procedure Set_Cursor
     (Icon_View     : access Gtk_Icon_View_Record;
      Path          : Gtk.Tree_Model.Gtk_Tree_Path;
      Cell          : Gtk.Cell_Renderer.Gtk_Cell_Renderer := null;
      Start_Editing : Boolean := False);
   --  Sets the current keyboard focus to be at Path, and selects it. This is
   --  useful when you want to focus the user's attention on a particular item.
   --   If Cell is not null, then focus is given to the cell specified by it.
   --  Additionally, if Start_Editing is True, then editing should be started
   --  in the specified cell.
   --
   --  This function is often followed by Grab_Focus in order to give keyboard
   --  focus to the widget.  Please note that editing can only happen when the
   --  widget is realized.

   procedure Get_Cursor
     (Icon_View     : access Gtk_Icon_View_Record;
      Path          : out Gtk.Tree_Model.Gtk_Tree_Path;
      Cell          : out Gtk.Cell_Renderer.Gtk_Cell_Renderer;
      Cursor_Is_Set : out Boolean);
   --  Fills in Path and Cell with the current cursor path and cell.
   --  If the cursor isn't currently set, then Ppath will be null.
   --  If no cell currently has focus, then Cell will be null.
   --  The returned Path must be freed with Gtk.Tree_Model.Path_Free.
   --  Cursor_Is_Set is set to True if the cursor is set.

   procedure Set_Item_Width
     (Icon_View  : access Gtk_Icon_View_Record;
      Item_Width : Glib.Gint);
   function Get_Item_Width
     (Icon_View : access Gtk_Icon_View_Record) return Glib.Gint;
   --  Sets the ::item-width property which specifies the width to use for each
   --  item. If it is set to -1, the icon view will automatically determine a
   --  suitable item size.

   procedure Set_Margin
     (Icon_View : access Gtk_Icon_View_Record;
      Margin    : Glib.Gint);
   function Get_Margin
     (Icon_View : access Gtk_Icon_View_Record) return Glib.Gint;
   --  Sets the ::margin property which specifies the space which is inserted
   --  at the top, bottom, left and right of the icon view.

   procedure Set_Orientation
     (Icon_View   : access Gtk_Icon_View_Record;
      Orientation : Gtk.Enums.Gtk_Orientation);
   function Get_Orientation
     (Icon_View : access Gtk_Icon_View_Record)
      return Gtk.Enums.Gtk_Orientation;
   --  Sets the ::orientation property which determines whether the labels
   --  are drawn beside the icons instead of below.

   procedure Set_Reorderable
     (Icon_View   : access Gtk_Icon_View_Record;
      Reorderable : Boolean);
   function Get_Reorderable
     (Icon_View : access Gtk_Icon_View_Record) return Boolean;
   --  This function is a convenience function to allow you to reorder models
   --  that support the Gtk_Tree_Drag_Source interface and the
   --  Gtk_Tree_Drag_Dest interface. Both Gtk_Tree_Store and Gtk_List_Store
   --  support these. If Reorderable is True, then the user can reorder the
   --  model by dragging and dropping rows. The developer can listen to these
   --  changes by connecting to the model's row_inserted and row_deleted
   --  signals.
   --
   --  This function does not give you any degree of control over the order --
   --  any reordering is allowed. If more control is needed, you should
   --  probably handle drag and drop manually.

   procedure Set_Row_Spacing
     (Icon_View   : access Gtk_Icon_View_Record;
      Row_Spacing : Glib.Gint);
   function Get_Row_Spacing
     (Icon_View : access Gtk_Icon_View_Record) return Glib.Gint;
   --  Sets the ::row-spacing property which specifies the space
   --  which is inserted between the rows of the icon view.

   procedure Set_Spacing
     (Icon_View : access Gtk_Icon_View_Record;
      Spacing   : Glib.Gint);
   function Get_Spacing
     (Icon_View : access Gtk_Icon_View_Record) return Glib.Gint;
   --  Sets the ::spacing property which specifies the space
   --  which is inserted between the cells (i.e. the icon and
   --  the text) of an item.

   procedure Item_Activated
     (Icon_View : access Gtk_Icon_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path);
   --  Activates the item determined by Path.

   ---------------
   -- Scrolling --
   ---------------

   procedure Get_Visible_Range
     (Icon_View : access Gtk_Icon_View_Record;
      Start_Path : out Gtk.Tree_Model.Gtk_Tree_Path;
      End_Path   : out Gtk.Tree_Model.Gtk_Tree_Path);
   --  Sets Start_Path and End_Path to be the first and last visible path.
   --  Note that there may be invisible paths in between.
   --  Both paths should be freed with Path_Free after use.

   procedure Scroll_To_Path
     (Icon_View : access Gtk_Icon_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      Use_Align : Boolean := True;
      Row_Align : Glib.Gfloat := 0.5;
      Col_Align : Glib.Gfloat := 0.0);
   --  Moves the alignments of Icon_View to the position specified by Path.
   --  Row_Align determines where the row is placed, and Col_Align determines
   --  where column is placed. Both are expected to be between 0.0 and 1.0.
   --  0.0 means left/top alignment, 1.0 means right/bottom alignment, 0.5
   --  means center.
   --
   --  If Use_Align is False, then the alignment arguments are ignored, and the
   --  tree does the minimum amount of work to scroll the item onto the screen.
   --  This means that the item will be scrolled to the edge closest to its
   --  current position. If the item is currently visible on the screen,
   --  nothing is done.
   --
   --  This function only works if the model is set, and Path is a valid row on
   --  the model. If the model changes before the Icon_View is realized, the
   --  centered path will be modified to reflect this change.

   --------------
   -- Tooltips --
   --------------

   procedure Set_Tooltip_Column
     (Icon_View : access Gtk_Icon_View_Record;
      Column    : Gint);
   function Get_Tooltip_Column
     (Icon_View : access Gtk_Icon_View_Record) return Gint;
   --  If you only plan to have simple (text-only) tooltips on full items, you
   --  can use Set_Tooltip_Column to have Gtk_Icon_View handle these
   --  automatically for you. Column should be set to the column in
   --  Icon_View's model containing the tooltip texts, or -1 to disable this
   --  feature.
   --
   --  When enabled, #GtkWidget::has-tooltip will be set to True and
   --  Icon_View will connect a #GtkWidget::query-tooltip signal handler.
   --
   --  Get_Tooltip_Column returns the index of the tooltip column that is
   --  currently being used, or -1 if this is disabled.

   procedure Get_Tooltip_Context
     (Icon_View    : access Gtk_Icon_View_Record;
      X            : in out Gint;
      Y            : in out Gint;
      Keyboard_Tip : Boolean;
      Model        : out Gtk.Tree_Model.Gtk_Tree_Model;
      Path         : out Gtk.Tree_Model.Gtk_Tree_Path;
      Iter         : out Gtk.Tree_Model.Gtk_Tree_Iter;
      Success      : out Boolean);
   --  Icon_View: a Gtk_Icon_View
   --  X: the x coordinate (relative to widget coordinates)
   --  Y: the y coordinate (relative to widget coordinates)
   --  Keyboard_Tip: whether or not this is a keyboard tooltip
   --  Model: a Gtk_Tree_Model
   --  Path: a Gtk_Tree_Path
   --  Iter: a Gtk_Tree_Iter
   --
   --  This subprogram is supposed to be used in a #GtkWidget::query-tooltip
   --  signal handler for Gtk_Icon_View.  The X, Y and Keyboard_Tip values
   --  which are received in the signal handler should be passed to this
   --  subprogram without modification.
   --
   --  The Success value indicates whether there is an icon view item at the
   --  given coordinates (True) or not (False) for mouse tooltips. For keyboard
   --  tooltips the item returned will be the cursor item. When True, then any
   --  of Model, Path and Iter which have been provided will be set to point to
   --  that row and the corresponding model. X and Y will always be converted
   --  to be relative to Icon_View's bin_window if Keyboard_Tip is False.

   procedure Set_Tooltip_Cell
     (Icon_View : access Gtk_Icon_View_Record;
      Tooltip   : access Gtk.Tooltip.Gtk_Tooltip_Record'Class;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      Cell      : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);
   --  Sets the tip area of Tooltip to the area which Cell occupies in
   --  the item pointed to by Path. See also Gtk.Tooltip.Set_Tip_Area.
   --
   --  See also Set_Tooltip_Column for a simpler alternative.

   procedure Set_Tooltip_Item
     (Icon_View : access Gtk_Icon_View_Record;
      Tooltip   : access Gtk.Tooltip.Gtk_Tooltip_Record'Class;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path);
   --  Sets the tip area of Tooltip to be the area covered by the item at Path.
   --
   --  See also Set_Tooltip_Column for a simpler alternative.
   --  See also Gtk.Tooltip.Set_Tip_Area.

   ----------------
   -- Tree Model --
   ----------------

   procedure Set_Model
     (Icon_View : access Gtk_Icon_View_Record;
      Model     : Gtk.Tree_Model.Gtk_Tree_Model := null);
   function Get_Model
     (Icon_View : access Gtk_Icon_View_Record)
      return Gtk.Tree_Model.Gtk_Tree_Model;
   --  Sets the model for a Gtk_Icon_View.  If the Icon_View already has a
   --  model set, it will remove it before setting the new model. If Model is
   --  null, then it will unset the old model.

   procedure Set_Text_Column
     (Icon_View : access Gtk_Icon_View_Record;
      Column    : Glib.Gint);
   function Get_Text_Column
     (Icon_View : access Gtk_Icon_View_Record) return Glib.Gint;
   --  Sets the column with text for Icon_View to be Column. The text
   --  column must be of type GType_String.

   procedure Set_Pixbuf_Column
     (Icon_View : access Gtk_Icon_View_Record; Column    : Glib.Gint);
   function Get_Pixbuf_Column
     (Icon_View : access Gtk_Icon_View_Record) return Glib.Gint;
   --  Sets the column with pixbufs for Icon_View to be Column. The pixbuf
   --  column must be of type Gdk.Pixbuf.Get_Type

   procedure Set_Markup_Column
     (Icon_View : access Gtk_Icon_View_Record;
      Column    : Glib.Gint);
   function Get_Markup_Column
     (Icon_View : access Gtk_Icon_View_Record) return Glib.Gint;
   --  Sets the column with markup information for Icon_View to be
   --  Column. The markup column must be of type GType_String.
   --  If the markup column is set to something, it overrides
   --  the text column set by Set_Text_Column.

   function Get_Path_At_Pos
     (Icon_View : access Gtk_Icon_View_Record;
      X         : Glib.Gint;
      Y         : Glib.Gint) return Gtk.Tree_Model.Gtk_Tree_Path;
   --  Finds the path at the point (X, Y), relative to widget coordinates. See
   --  Get_Item_At_Pos, if you are also interested in the cell at the specified
   --  position.

   procedure Get_Item_At_Pos
     (Icon_View : access Gtk_Icon_View_Record;
      X         : Gint;
      Y         : Gint;
      Path      : out Gtk.Tree_Model.Gtk_Tree_Path;
      Cell      : out Gtk.Cell_Renderer.Gtk_Cell_Renderer;
      Has_Item  : out Boolean);
   --  Finds the path at the point (X, Y), relative to widget coordinates.
   --  In contrast to Get_Path_At_Pos, this function also
   --  obtains the cell at the specified position. The returned path should
   --  be freed with Path_Free.
   --  Has_Item is set to True if an item exists at the specified position.

   procedure Convert_Widget_To_Bin_Window_Coords
     (Icon_View : access Gtk_Icon_View_Record;
      Wx        : Gint;
      Wy        : Gint;
      Bx        : out Gint;
      By        : out Gint);
   --  Icon_View: a Gtk_Icon_View
   --  Wx: X coordinate relative to the widget
   --  Wy: Y coordinate relative to the widget
   --  Bx: bin_window X coordinate
   --  By: bin_window Y coordinate
   --
   --  Converts widget coordinates to coordinates for the bin_window,
   --  as expected by e.g. Get_Path_At_Pos.

   ---------------
   -- Selection --
   ---------------

   procedure Set_Selection_Mode
     (Icon_View : access Gtk_Icon_View_Record;
      Mode      : Gtk.Enums.Gtk_Selection_Mode);
   function Get_Selection_Mode
     (Icon_View : access Gtk_Icon_View_Record)
      return Gtk.Enums.Gtk_Selection_Mode;
   --  Sets the selection mode of the Icon_View.

   procedure Select_All   (Icon_View : access Gtk_Icon_View_Record);
   procedure Unselect_All (Icon_View : access Gtk_Icon_View_Record);
   --  Selects all the icons. Icon_View must has its selection mode set
   --  to Selection_Multiple

   procedure Select_Path
     (Icon_View : access Gtk_Icon_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path);
   procedure Unselect_Path
     (Icon_View : access Gtk_Icon_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path);
   --  Selects the row at Path.

   function Get_Selected_Items
     (Icon_View : access Gtk_Icon_View_Record)
      return Gtk.Tree_Model.Gtk_Tree_Path_List.Glist;
   --  Creates a list of paths of all selected items. Additionally, if you are
   --  planning on modifying the model after calling this function, you may
   --  want to convert the returned list into a list of Gtk_Tree_Row_Reference.
   --  To free the returend value, use:
   --      Foreach (List, Gtk.Tree_Model.Path_Free, Null);
   --      Free (List);

   function Path_Is_Selected
     (Icon_View : access Gtk_Icon_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean;
   --  Returns True if the icon pointed to by Path is currently
   --  selected. If Path does not point to a valid location, False is returned.

   -------------------
   -- Drag and drop --
   -------------------

   function Create_Drag_Icon
     (Icon_View : access Gtk_Icon_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path) return Gdk.Gdk_Pixmap;
   --  Creates a Gdk_Pixmap representation of the item at Path.
   --  This image is used for a drag icon.
   --  The returned value must be Unref'd by the caller.

   procedure Enable_Model_Drag_Dest
     (Icon_View : access Gtk_Icon_View_Record;
      Targets   : Gtk.Selection.Target_Entry_Array;
      Actions   : Gdk.Dnd.Drag_Action);
   procedure Unset_Model_Drag_Dest
     (Icon_View : access Gtk_Icon_View_Record);
   --  Turns Icon_view into a drop destination for automatic DND.
   --  Targets is the list of targets that the drag will support.

   procedure Enable_Model_Drag_Source
     (Icon_View         : access Gtk_Icon_View_Record;
      Start_Button_Mask : Gdk.Types.Gdk_Modifier_Type;
      Targets           : Gtk.Selection.Target_Entry_Array;
      Actions           : Gdk.Dnd.Drag_Action);
   procedure Unset_Model_Drag_Source
     (Icon_View : access Gtk_Icon_View_Record);
   --  Turns Icon_view into a drag source for automatic DND.
   --  Start_Button_Mask is the allowed buttons to start drag.

   procedure Get_Dest_Item_At_Pos
     (Icon_View : access Gtk_Icon_View_Record;
      Drag_X    : Glib.Gint;
      Drag_Y    : Glib.Gint;
      Path      : out Gtk.Tree_Model.Gtk_Tree_Path;
      Pos       : out Gtk_Icon_View_Drop_Position;
      Has_Item  : out Boolean);
   --  Determines the destination item for a given position.
   --  Return value: whether there is an item at the given position.

   procedure Set_Drag_Dest_Item
     (Icon_View : access Gtk_Icon_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      Pos       : Gtk_Icon_View_Drop_Position);
   procedure Get_Drag_Dest_Item
     (Icon_View : access Gtk_Icon_View_Record;
      Path      : out Gtk.Tree_Model.Gtk_Tree_Path;
      Pos       : out Gtk_Icon_View_Drop_Position);
   --  Sets the item that is highlighted for feedback.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Cell_Layout"

   package Implements_Cell_Layout is new Glib.Types.Implements
     (Gtk.Cell_Layout.Gtk_Cell_Layout, Gtk_Icon_View_Record, Gtk_Icon_View);
   function "+"
     (Widget : access Gtk_Icon_View_Record'Class)
      return Gtk.Cell_Layout.Gtk_Cell_Layout
      renames Implements_Cell_Layout.To_Interface;
   function "-"
     (Interf : Gtk.Cell_Layout.Gtk_Cell_Layout)
      return Gtk_Icon_View
      renames Implements_Cell_Layout.To_Object;
   --  Converts to and from the Gtk_Cell_Layout interface

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Column_Spacing_Property
   --  Type:  Int
   --  Descr: Space which is inserted between grid column
   --
   --  Name:  Columns_Property
   --  Type:  Int
   --  Descr: Number of columns to display
   --
   --  Name:  Item_Width_Property
   --  Type:  Int
   --  Descr: The width used for each item
   --
   --  Name:  Margin_Property
   --  Type:  Int
   --  Descr: Space which is inserted at the edges of the icon view
   --
   --  Name:  Markup_Column_Property
   --  Type:  Int
   --  Descr: Model column used to retrieve the text if using Pango markup
   --
   --  Name:  Model_Property
   --  Type:  Object
   --  Descr: The model for the icon view
   --
   --  Name:  Orientation_Property
   --  Type:  Enum
   --  Descr: How the text and icon of each item are positioned relative to
   --         each other
   --
   --  Name:  Pixbuf_Column_Property
   --  Type:  Int
   --  Descr: Model column used to retrieve the icon pixbuf from
   --
   --  Name:  Reorderable_Property
   --  Type:  Boolean
   --  Descr: View is reorderable
   --
   --  Name:  Row_Spacing_Property
   --  Type:  Int
   --  Descr: Space which is inserted between grid rows
   --
   --  Name:  Selection_Mode_Property
   --  Type:  Enum
   --  Descr: The selection mode
   --
   --  Name:  Spacing_Property
   --  Type:  Int
   --  Descr: Space which is inserted between cells of an item
   --
   --  Name:  Text_Column_Property
   --  Type:  Int
   --  Descr: Model column used to retrieve the text from
   --
   --  Name:  Tooltip_Column_Property
   --  Type:  Int
   --  Descr: The column in the model containing the tooltip texts for
   --         the items
   --
   --  </properties>

   Column_Spacing_Property : constant Glib.Properties.Property_Int;
   Columns_Property        : constant Glib.Properties.Property_Int;
   Item_Width_Property     : constant Glib.Properties.Property_Int;
   Margin_Property         : constant Glib.Properties.Property_Int;
   Markup_Column_Property  : constant Glib.Properties.Property_Int;
   Model_Property          : constant Glib.Properties.Property_Object;
   Orientation_Property    : constant Gtk.Enums.Property_Gtk_Orientation;
   Pixbuf_Column_Property  : constant Glib.Properties.Property_Int;
   Reorderable_Property    : constant Glib.Properties.Property_Boolean;
   Row_Spacing_Property    : constant Glib.Properties.Property_Int;
   --  Selection_Mode_Property : constant Glib.Properties.Property_Enum;
   Spacing_Property        : constant Glib.Properties.Property_Int;
   Text_Column_Property    : constant Glib.Properties.Property_Int;
   Tooltip_Column_Property : constant Glib.Properties.Property_Int;

   ----------------------
   -- Style Properties --
   ----------------------
   --  The following properties can be changed through the gtk theme and
   --  configuration files, and retrieved through Gtk.Widget.Style_Get_Property
   --
   --  <style_properties>
   --  Name:  Selection_Box_Alpha_Property
   --  Type:  Uchar
   --  Descr: Opacity of the selection box
   --
   --  Name:  Selection_Box_Color_Property
   --  Type:  Boxed
   --  Descr: Color of the selection box
   --

   Selection_Box_Alpha_Property : constant Glib.Properties.Property_Uchar;
   Selection_Box_Color_Property : constant Gdk.Color.Property_Gdk_Color;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "activate_cursor_item"
   --  - "item_activated"
   --  - "move_cursor"
   --  - "select_all"
   --  - "select_cursor_item"
   --  - "selection_changed"
   --  - "set_scroll_adjustments"
   --  - "toggle_cursor_item"
   --  - "unselect_all"
   --  </signals>

   Signal_Activate_Cursor_Item   : constant Glib.Signal_Name :=
                                     "activate_cursor_item";
   Signal_Item_Activated         : constant Glib.Signal_Name :=
                                     "item_activated";
   Signal_Move_Cursor            : constant Glib.Signal_Name :=
                                     "move_cursor";
   Signal_Select_All             : constant Glib.Signal_Name :=
                                     "select_all";
   Signal_Select_Cursor_Item     : constant Glib.Signal_Name :=
                                     "select_cursor_item";
   Signal_Selection_Changed      : constant Glib.Signal_Name :=
                                     "selection_changed";
   Signal_Set_Scroll_Adjustments : constant Glib.Signal_Name :=
                                     "set_scroll_adjustments";
   Signal_Toggle_Cursor_Item     : constant Glib.Signal_Name :=
                                     "toggle_cursor_item";
   Signal_Unselect_All           : constant Glib.Signal_Name :=
                                     "unselect_all";

private

   Selection_Box_Alpha_Property : constant Glib.Properties.Property_Uchar :=
     Glib.Properties.Build ("selection-box-alpha");
   Selection_Box_Color_Property : constant Gdk.Color.Property_Gdk_Color :=
     Gdk.Color.Property_Gdk_Color (Glib.Build ("selection-box-color"));

   Column_Spacing_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("column-spacing");
   Columns_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("columns");
   Item_Width_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("item-width");
   Margin_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("margin");
   Markup_Column_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("markup-column");
   Model_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("model");
   Orientation_Property    : constant Gtk.Enums.Property_Gtk_Orientation :=
     Gtk.Enums.Property_Gtk_Orientation (Glib.Build ("orientation"));
   Pixbuf_Column_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("pixbuf-column");
   Reorderable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("reorderable");
   Row_Spacing_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("row-spacing");
--     Selection_Mode_Property : constant Glib.Properties.Property_Enum :=
--     Glib.Properties.Build ("selection-mode");
   Spacing_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("spacing");
   Text_Column_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("text-column");
   Tooltip_Column_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("tooltip-column");

   pragma Import (C, Get_Type, "gtk_icon_view_get_type");

end Gtk.Icon_View;

--  binding might be useful later:
--  No binding: gtk_icon_view_selected_foreach
