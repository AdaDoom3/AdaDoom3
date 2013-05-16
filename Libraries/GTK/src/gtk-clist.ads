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
--  This widget displays a multi-column list. Each line is made of
--  a number of column, each being able to display any kind of widget.
--
--  The intersection of a line and a column is called a Cell. Each cell can
--  have a different type (Cell_Text, Cell_Pixmap, Cell_Pixtext), and display
--  its contents depending on this type. For instance, the text is not
--  displayed in the type is Cell_Pixmap.
--  Note that this type is changed dynamically by some of the subprograms
--  below, like Set_Pixmap, Set_Text, ... and Set_Cell_Contents
--
--  This is one of the most powerful widgets in GtkAda, that can be used to
--  display an kind of information. Look also into using Gtk_Ctree, which is
--  a similar widget.
--
--  You can add scrolling in a Gtk_Clist by adding it in a Gtk_Scrolled_Window.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Obsolescent widgets</group>
--  <testgtk>create_clist.adb</testgtk>

with Gdk.Bitmap;
with Gdk.Color;
with Gdk.Pixmap;
with Gdk.Window;
with Glib.Glist;
pragma Elaborate_All (Glib.Glist);
with Gtk.Adjustment;
with Gtk.Container;
with Gtk.Enums;
with Gtk.Style;
with Gtk.Widget;
with Gtkada.Types;
with Unchecked_Conversion;

package Gtk.Clist is
   pragma Obsolescent ("Use Gtk.Tree_View instead");

   type Gtk_Clist_Record is new Gtk.Container.Gtk_Container_Record
     with private;
   type Gtk_Clist is access all Gtk_Clist_Record'Class;

   type Gtk_Clist_Row is new Gdk.C_Proxy;
   --  A row of the clist.
   --  Application-specific data can be associated with each row.
   --  In the following subprograms, rows can also be accessed via their
   --  number, starting from 0.

   type Gtk_Button_Action is new Guint;
   Button_Ignored : constant Gtk_Button_Action := 0;
   Button_Selects : constant Gtk_Button_Action := 1 ** 0;
   Button_Drags   : constant Gtk_Button_Action := 1 ** 1;
   Button_Expands : constant Gtk_Button_Action := 1 ** 2;

   type Gtk_Cell_Type is
     (Cell_Empty,
      Cell_Text,
      Cell_Pixmap,
      Cell_Pixtext,
      Cell_Widget);
   pragma Convention (C, Gtk_Cell_Type);

   type Gtk_Sort_Type is (Ascending, Descending);
   pragma Convention (C, Gtk_Sort_Type);
   --  The order in which the rows should be sorted.

   --  <doc_ignore>
   function Convert is new Unchecked_Conversion
     (Gtk_Clist_Row, System.Address);
   function Convert is new Unchecked_Conversion
     (System.Address, Gtk_Clist_Row);
   package Row_List is new Glib.Glist.Generic_List (Gtk_Clist_Row);

   --  </doc_ignore>

   type Gtk_Clist_Compare_Func is access
     function
       (Clist : access Gtk_Clist_Record'Class;
        Row1  : Gtk_Clist_Row;
        Row2  : Gtk_Clist_Row) return Gint;
   --  Function used when sorting a clist. This function takes two
   --  rows as its arguments, and should return a Gint indicating in which
   --  order the rows are found (-1 if Row1 comes first, 0 if they are equal,
   --  1 if Row2 comes first).

   ------------------------------------------------
   -- Creating a list and setting the attributes --
   ------------------------------------------------

   procedure Gtk_New (Widget : out Gtk_Clist; Columns : in Gint);
   --  Create a list with Columns columns.
   --  Each line will have this exact number of column
   --  The number of columns can not be changed once the widget has been
   --  created.

   procedure Initialize
     (Widget : access Gtk_Clist_Record'Class; Columns : in Gint);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Gtk_New
     (Widget  : out Gtk_Clist;
      Columns : in  Gint;
      Titles  : in  Gtkada.Types.Chars_Ptr_Array);
   --  Create a new list with Columns columns.
   --  The title of the columns is specified in Titles.
   --  The results are undefined (and can raise an exception) if Titles does
   --  not have at least Columns items.

   procedure Initialize
     (Widget  : access Gtk_Clist_Record'Class;
      Columns : in Gint;
      Titles  : in Gtkada.Types.Chars_Ptr_Array);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Clist.

   procedure Set_Hadjustment
     (Clist      : access Gtk_Clist_Record;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment);
   --  Set the horizontal adjustment used for the clist.
   --  Note that such an adjustment is automatically created when the clist
   --  is added to a Gtk_Scrolled_Window. You should rather use
   --  Gtk.Scrolled_Window.Set_Hadjustment if you want to modify the
   --  adjustment.
   --  If there was already such an adjustment, it is unref-ed, and might
   --  be deleted.

   procedure Set_Vadjustment
     (Clist      : access Gtk_Clist_Record;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment);
   --  Set the vertical adjustment used for the clist.
   --  Note that such an adjustment is automatically created when the clist
   --  is added to a Gtk_Scrolled_Window. You should rather use
   --  Gtk.Scrolled_Window.Set_Hadjustment if you want to modify the
   --  adjustment.
   --  If there was already such an adjustment, it is unref-ed, and might
   --  be deleted.

   function Get_Hadjustment
     (Clist  : access  Gtk_Clist_Record) return Gtk.Adjustment.Gtk_Adjustment;
   --  Return the horizontal adjustment used for the clist.
   --  This indicates what position the clist is presently displaying, and
   --  by changing its value, the clist is automatically scrolled horizontally.
   --  This is done automatically when the clist's parent is a
   --  Gtk_Scrolled_Window.

   function Get_Vadjustment
     (Clist  : access Gtk_Clist_Record) return Gtk.Adjustment.Gtk_Adjustment;
   --  Return the vertical adjustment used for the clist.
   --  This indicates what position the clist is presently displaying, and
   --  by changing its value, the clist is automatically scrolled vertically.
   --  This is done automatically when the clist's parent is a
   --  Gtk_Scrolled_Window.

   procedure Set_Selection_Mode
     (Clist : access Gtk_Clist_Record;
      Mode  : in Gtk.Enums.Gtk_Selection_Mode);
   --  Modify the selection mode for the clist.
   --  This indicates whether one or more lines can be selected at the
   --  same time in the clist, and how this selection can done by the
   --  user (does he have to click explicitly on an item, or can he
   --  browse through the clist and select the last item he was on, etc.)
   --
   --  Note that changing the selection mode to Selection_Single or
   --  Selection_Browse will deselect all the items in the clist.

   function Get_Selection_Mode
     (Clist : access Gtk_Clist_Record) return Gtk.Enums.Gtk_Selection_Mode;
   --  Return the selection mode for the clist.

   --  <doc_ignore>
   function Get_Clist_Window
     (Clist : access Gtk_Clist_Record) return Gdk.Window.Gdk_Window;
   --  Returns the scrolling window used in the clist. This function is
   --  kept for backward compatibility reasons, and you probably won't have
   --  to use it.
   --  </doc_ignore>

   --------------------
   -- Visual aspects --
   --------------------

   procedure Freeze (Clist : access Gtk_Clist_Record);
   --  Freeze all visual updates on the list, while you make big changes.
   --  This is more efficient than working on an unfrozen list.

   procedure Thaw (Clist : access Gtk_Clist_Record);
   --  Thaw the list, ie reactivate all the visual updates.
   --  This also forces an immediate refresh of the list.
   --  Note that each Freeze must be followed by a Thaw. The visual updates
   --  are not reactivated until the last Thaw has been emitted, but there is
   --  an immediate refresh every time anyway.

   procedure Set_Shadow_Type
     (Clist    : access Gtk_Clist_Record;
      The_Type : in Gtk.Enums.Gtk_Shadow_Type);
   --  Set the border style of the clist.

   ----------------------------
   -- Modifying the contents --
   ----------------------------

   function Append
     (Clist : access Gtk_Clist_Record;
      Text  : in     Gtkada.Types.Chars_Ptr_Array) return Gint;
   --  Append a new row to the clist, and return the index of the row created.
   --  The row is added at the end of the Clist.
   --  The behavior is undefined if Text does not have at least as many items
   --  as there are columns in the Clist.

   function Prepend
     (Clist : access Gtk_Clist_Record;
      Text  : in     Gtkada.Types.Chars_Ptr_Array) return Gint;
   --  Add a new row at the beginning of the clist, and return its index.
   --  The behavior is undefined if Text does not have at least as many items
   --  as there are columns in the Clist.

   procedure Insert
     (Clist : access Gtk_Clist_Record;
      Row   : in     Gint;
      Text  : in     Gtkada.Types.Chars_Ptr_Array);
   --  Add a new row in the clist.
   --  The row 0 is the first in the clist. If Row is not in the range for
   --  clist, the new row is added at the end. The behavior is undefined if
   --  Text does not have enough items.

   procedure Remove (Clist : access Gtk_Clist_Record; Row : in Gint);
   --  Remove a row from the clist (0 is the first one).

   procedure Clear (Clist : access Gtk_Clist_Record);
   --  Clears the entire list. This is much faster than doing a Remove on each
   --  line.

   procedure Swap_Rows
     (Clist : access Gtk_Clist_Record;
      Row1  : in     Gint;
      Row2  : in     Gint);
   --  Exchange the position of two rows in the clist.

   procedure Row_Move
     (Clist      : access Gtk_Clist_Record;
      Source_Row : in     Gint;
      Dest_Row   : in     Gint);
   --  Move the row at Source_Row to Dest_Row (0 indicates the first row in
   --  the clist)

   procedure Set_Sort_Column
     (Clist  : access Gtk_Clist_Record;
      Column : Gint);
   --  Indicate the column on which to sort the clist.
   --  This column is relevant when you use Sort or Set_Auto_Sort below.
   --  The first column is number 0.

   function Get_Sort_Column (Clist : access Gtk_Clist_Record) return Gint;
   --  Return the column on which the clist is sorted.

   procedure Set_Sort_Type
     (Clist     : access Gtk_Clist_Record;
      Sort_Type : Gtk_Sort_Type);
   --  Indicate in which order the sort should be done on the clist
   --  (ascending or descending).

   function Get_Sort_Type
     (Clist : access Gtk_Clist_Record) return Gtk_Sort_Type;
   --  Return the sort type currently used for the list

   procedure Sort (Clist : access Gtk_Clist_Record);
   --  Sort the lines of the clist, based on the column set by Set_Sort_Column,
   --  and in the order set by Set_Sort_Type.

   procedure Set_Auto_Sort
     (Clist     : access Gtk_Clist_Record;
      Auto_Sort : Boolean);
   --  If Auto_Sort is true, then the clist will be automatically sorted every
   --  time a new line is inserted into the clist.

   procedure Set_Compare_Func
     (Clist : access Gtk_Clist_Record;
      Func  : Gtk_Clist_Compare_Func);
   --  Set the function used when sorting the list. This function takes two
   --  rows as its arguments, and should return a Gint indicating in which
   --  order the rows are found (-1 if Row1 comes first, 0 if they are equal,
   --  1 if Row2 comes last).
   --  Func should be null to restore the default sorting functions.

   -------------
   -- Columns --
   -------------

   function Get_Columns (Clist : access Gtk_Clist_Record) return Gint;
   --  Return the number of columns in the clist.

   procedure Column_Titles_Hide (Clist : access Gtk_Clist_Record);
   --  Hide the column titles for the list.
   --  This is the default behavior if no column titles were given when the
   --  list was created.

   procedure Column_Titles_Show (Clist : access Gtk_Clist_Record);
   --  Show the column titles for the list.
   --  This is the default behavior if some column titles were given when the
   --  list was created.

   procedure Column_Title_Active
     (Clist : access Gtk_Clist_Record;
      Column : in Gint);
   --  Set the column title to be an activate title.
   --  In other words, answer all button presses, highlights when the mouse is
   --  over it, ...

   procedure Column_Title_Passive
     (Clist : access Gtk_Clist_Record;
      Column : in Gint);
   --  Set the column title to be passive.
   --  Act just as a title, and do not react to mouse events.

   procedure Column_Titles_Active (Clist : access Gtk_Clist_Record);
   --  Set all column titles to be active.

   procedure Column_Titles_Passive (Clist : access Gtk_Clist_Record);
   --  Set all column titles to be passive.

   procedure Set_Column_Title
     (Clist  : access Gtk_Clist_Record;
      Column : in Gint;
      Title  : in UTF8_String);
   --  Set the text for the button of the column's title.
   --  See Set_Column_Widget if you want to put a pixmap inside the button.

   function Get_Column_Title
     (Clist  : access Gtk_Clist_Record;
      Column : in Gint) return UTF8_String;
   --  Return the text used for the title's column.
   --  This is a copy of the title, so you can't modify it to automatically
   --  change the column's title.

   procedure Set_Column_Widget
     (Clist  : access Gtk_Clist_Record;
      Column : in     Gint;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Modify the widget used in the Gtk_Button that is the column's title.
   --  By default, this button contains a simple Gtk_Label, which is replaced
   --  by Widget. This is the function to use if you want to put a pixmap
   --  (or a Gtk_Box that contains both a pixmap and some text) in a column's
   --  title.

   function Get_Column_Widget
     (Clist  : access Gtk_Clist_Record;
      Column : in Gint) return Gtk.Widget.Gtk_Widget;
   --  Return the child of the button that makes the column's title.
   --  Unless you changed it with Set_Column_Widget, this will return a
   --  Gtk_Label. Note also that if this widget was not created in Ada, but
   --  transparently by gtk+, you have to 'with' Gtk.Type_Conversion so that
   --  the correct type of the widget is created (See the user's guide for
   --  more information on type conversion).

   procedure Set_Column_Justification
     (Clist         : access Gtk_Clist_Record;
      Column        : in Gint;
      Justification : in Gtk.Enums.Gtk_Justification);
   --  Change the way the text in the whole column is justified.
   --  This function has no effect on the title if you used Set_Column_Widget
   --  before.

   procedure Set_Column_Visibility
     (Clist   : access Gtk_Clist_Record;
      Column  : in Gint;
      Visible : in Boolean);
   --  Modify the visibility of a column.
   --  Note that GtkAda prevents the last remaining visible column to be
   --  hidden. Nothing will be done if you try to hide that last column.
   --  See the example below for an example how to hide all the columns but
   --  one.

   procedure Set_Column_Resizeable
     (Clist    : access Gtk_Clist_Record;
      Column   : in Gint;
      Resizeable : in Boolean);
   --  Set whether the column can be dynamically resized with the mouse.
   --  If Resizeable is true, then the column can be resized by clicking
   --  and dragging the lines that separates the column from the next one.

   procedure Set_Column_Auto_Resize
     (Clist       : access Gtk_Clist_Record;
      Column      : in Gint;
      Auto_Resize : in Boolean);
   --  Set whether the column should automatically be resized to the optimal
   --  size (based on its contents). Note that this operation could slow things
   --  down a lot if you have a lot of items in your list.

   function Columns_Autosize (Clist  : access Gtk_Clist_Record) return Gint;
   --  Set all the columns' width to their optimal size.
   --  Return the total width of the clist after this operation.

   function Optimal_Column_Width
     (Clist : access Gtk_Clist_Record;
      Column : Gint) return Gint;
   --  Return the optimal width for Column, based on its contents.
   --  This is the maximal cell width in the column.

   procedure Set_Column_Width
     (Clist  : access Gtk_Clist_Record;
      Column : in Gint;
      Width  : in Gint);
   --  Set the column width in pixels.
   --  By default, the column's width is chosen from the column's title.

   procedure Set_Column_Min_Width
     (Clist     : access Gtk_Clist_Record;
      Column    : Gint;
      Min_Width : Gint);
   --  Set the minimal width for the column, in pixels.
   --  if Min_Width is negative, there is no limit on the minimal width for
   --  the column.

   procedure Set_Column_Max_Width
     (Clist     : access Gtk_Clist_Record;
      Column    : Gint;
      Max_Width : Gint);
   --  Set the maximal width for the column, in pixels.
   --  If Max_Width is negative, there is no limit on the maximal width for
   --  the column.

   ----------
   -- Rows --
   ----------

   function Get_Rows (Clist : access Gtk_Clist_Record) return Gint;
   --  Return the number of rows in the clist.

   procedure Set_Row_Height
     (Clist  : access Gtk_Clist_Record;
      Height : Gint);
   --  Set the height of the rows, in pixels.
   --  if Height is 0, the chosen height will be the current's font height.

   function Row_Is_Visible
     (Clist : access Gtk_Clist_Record;
      Row   : in Gint) return Gtk.Enums.Gtk_Visibility;
   --  Return the visibility status of the row.

   procedure Set_Foreground
     (Clist : access Gtk_Clist_Record;
      Row   : in Gint;
      Color : in Gdk.Color.Gdk_Color);
   --  Set the foreground color for the row.
   --  The color must already be allocated.
   --  If no such row exists in the list, nothing is done.

   procedure Set_Background
     (Clist : access Gtk_Clist_Record;
      Row   : in Gint;
      Color : in Gdk.Color.Gdk_Color);
   --  Set the background color for the row.
   --  The color must already be allocated.
   --  If no such row exists in the list, nothing is done.

   procedure Set_Row_Style
     (Clist : access Gtk_Clist_Record; Row : Gint;
      Style : in Gtk.Style.Gtk_Style);
   --  Set the default style for the cells in the row. This can be
   --  overridden for each cell with Set_Cell_Style.

   function Get_Row_Style
     (Clist  : access Gtk_Clist_Record;
      Row    : in     Gint) return Gtk.Style.Gtk_Style;
   --  Return the default style used for the row.

   procedure Set_Selectable
     (Clist      : access Gtk_Clist_Record;
      Row        : Gint;
      Selectable : Boolean);
   --  Indicate whether the row can be selected or not.
   --  The default value is True.

   function Get_Selectable
     (Clist : access Gtk_Clist_Record;
      Row   : Gint) return Boolean;
   --  Return the selectable status of the row.

   procedure Select_Row
     (Clist  : access Gtk_Clist_Record;
      Row    : in Gint;
      Column : in Gint);
   --  Emit the signal "select_row". This simulates the user pressing
   --  the mouse on Row, Column on the clist.

   procedure Unselect_Row
     (Clist  : access Gtk_Clist_Record;
      Row    : in Gint;
      Column : in Gint);
   --  Emit the signal "unselect_row", as if the user had clicked on
   --  Row, Column on the clist.

   procedure Undo_Selection (Clist  : access Gtk_Clist_Record);
   --  Undo the last select/unselect operation.

   procedure Get_Selection_Info
     (Clist    : access Gtk_Clist_Record;
      X        : in Gint;
      Y        : in Gint;
      Row      : out Gint;
      Column   : out Gint;
      Is_Valid : out Boolean);
   --  Return the Row/Column corresponding to the coordinates X,Y in the
   --  Row column. The coordinates X,Y are relative to the clist window
   --  (ie 0,0 is the top left corner of the clist).
   --  The result is valid only if Is_Valid is true

   procedure Select_All (Clist : access Gtk_Clist_Record);
   --  Select all the rows in the clist. This only works if the selection
   --  mode allows for multiple rows selected at the same time (extended or
   --  multiple).

   procedure Unselect_All (Clist : access Gtk_Clist_Record);
   --  Deselect all the rows in the clist. If the selection mode is
   --  Browse, then only the current line is deselected.

   function Get_Focus_Row (Clist : access Gtk_Clist_Record) return Gint;
   --  Return the number of the line that currently has the focus.

   function Get_Row_List
     (Clist : access Gtk_Clist_Record) return Row_List.Glist;
   --  Return the list of all the rows in the clist. This might speed up
   --  the access to the rows a little.
   --  You can then use the function Set_Cell_Contents to modify the cells
   --  in the row, and Get_Text or Get_Pixmap to get its contents.

   function Get_Selection
     (Widget : access Gtk_Clist_Record) return Gtk.Enums.Gint_List.Glist;
   --  Return the list of selected rows, by number.

   -----------
   -- Cells --
   -----------

   function Get_Cell_Type
     (Clist  : access Gtk_Clist_Record;
      Row    : in Gint;
      Column : in Gint) return Gtk_Cell_Type;
   --  Return the type of the cell at Row/Column.
   --  This indicates which of the functions Get_Text. Get_Pixmap, etc.
   --  below you can use.

   procedure Set_Text
     (Clist  : access Gtk_Clist_Record;
      Row    : in Gint;
      Column : in Gint;
      Text   : in UTF8_String);
   --  Set the cell's text, replacing its current contents.
   --  This changes the type of the cell to Cell_Text. The pixmap (if any)
   --  will no longer be displayed.

   function Get_Text
     (Clist    : access Gtk_Clist_Record;
      Row      : in Gint;
      Column   : in Gint) return UTF8_String;
   --  Return the text contained in cell. The type of the cell should be
   --  either Cell_Text or Cell_Pixtext.
   --  If there was a problem, a null-length string is returned.
   --  The problem might appear in case the row or the column are
   --  invalid, or if the cell does not contain any text.

   function Get_Text
     (Clist    : access Gtk_Clist_Record;
      Row      : Gtk_Clist_Row;
      Column   : in Gint) return UTF8_String;
   --  Return the text contained in cell. The Row can be obtained from
   --  Get_Row_List, this function speeds up the access a little compared
   --  to the other Get_Text above.

   procedure Set_Pixmap
     (Clist  : access Gtk_Clist_Record;
      Row    : in Gint;
      Column : in Gint;
      Pixmap : in Gdk.Pixmap.Gdk_Pixmap;
      Mask   : in Gdk.Bitmap.Gdk_Bitmap);
   --  Set the cell's pixmap, replacing its current contents.
   --  The type of the cell becomes Cell_Pixmap, and the text is no longer
   --  displayed.

   procedure Get_Pixmap
     (Clist    : access Gtk_Clist_Record;
      Row      : in Gint;
      Column   : in Gint;
      Pixmap   : out Gdk.Pixmap.Gdk_Pixmap;
      Mask     : out Gdk.Bitmap.Gdk_Bitmap;
      Is_Valid : out Boolean);
   --  Return the pixmap contained in a cell. The type of the cell should
   --  be Cell_Pixmap.
   --  The result is meaningful only if Is_Valid is True. If the Cell did not
   --  contain a pixmap, Is_Valid is set to False

   procedure Get_Pixmap
     (Clist    : access Gtk_Clist_Record;
      Row      : in Gtk_Clist_Row;
      Column   : in Gint;
      Pixmap   : out Gdk.Pixmap.Gdk_Pixmap;
      Mask     : out Gdk.Bitmap.Gdk_Bitmap;
      Is_Valid : out Boolean);
   --  Return the pixmap contained in a cell. Row can be obtained directly with
   --  Get_Row_List, and speeds up the access a little compared to the previous
   --  Get_Pixmap function.

   procedure Set_Pixtext
     (Clist   : access Gtk_Clist_Record;
      Row     : in Gint;
      Column  : in Gint;
      Text    : in UTF8_String;
      Spacing : in Guint8;
      Pixmap  : in Gdk.Pixmap.Gdk_Pixmap;
      Mask    : in Gdk.Bitmap.Gdk_Bitmap);
   --  Set both the text and the pixmap for the cell.
   --  Replace its current contents. The type of the cell becomes Cell_Pixtext,
   --  and both the text and the pixmap are displayed.

   procedure Get_Pixtext
     (Clist    : access Gtk_Clist_Record;
      Row      : in Gint;
      Column   : in Gint;
      Spacing  : out Guint8;
      Pixmap   : out Gdk.Pixmap.Gdk_Pixmap;
      Mask     : out Gdk.Bitmap.Gdk_Bitmap;
      Is_Valid : out Boolean);
   --  The result is not meaningful if Is_Valid is False.
   --  The only way to get the string is to use Get_Text, since a String is
   --  an unconstrained type in Ada and is not really convenient to use as an
   --  out parameter.

   procedure Set_Cell_Style
     (Clist  : access Gtk_Clist_Record;
      Row    : in Gint;
      Column : in Gint;
      Style  : in Gtk.Style.Gtk_Style);
   --  Set the style (font, color, ...) used for the cell.
   --  This overrides the row's style.

   function Get_Cell_Style
     (Clist  : access Gtk_Clist_Record;
      Row    : in     Gint;
      Column : in     Gint) return Gtk.Style.Gtk_Style;
   --  Return the style of the cell.

   procedure Set_Shift
     (Clist      : access Gtk_Clist_Record;
      Row        : in Gint;
      Column     : in Gint;
      Vertical   : in Gint;
      Horizontal : in Gint);
   --  Set a horizontal and vertical shift for drawing the content of the cell.
   --  Both shifts can be either positive or negative.
   --  This is particularly useful for indenting items in a columns.

   procedure Set_Cell_Contents
     (Clist     : access Gtk_Clist_Record;
      Row       : Gtk_Clist_Row;
      Column    : Gint;
      Cell_Type : Gtk_Cell_Type;
      Text      : UTF8_String;
      Spacing   : Guint8;
      Pixmap    : Gdk.Pixmap.Gdk_Pixmap;
      Mask      : Gdk.Bitmap.Gdk_Bitmap);
   --  Modify the contents and type of a cell.
   --  Cell_Type indicates what should be displayed in the cell. Note that
   --  if you do not want any string, you should pass an empty string "".
   --  You get Row from Get_Row_List.

   -------------------------
   -- Reordering the list --
   -------------------------

   procedure Set_Reorderable
     (Clist : access Gtk_Clist_Record; Reorderable : Boolean);
   --  Set whether the list can be dynamically reordered by the user.
   --  (using a simple drag-n-drop protocol).

   procedure Set_Use_Drag_Icons
     (Clist : access Gtk_Clist_Record; Use_Icons : Boolean);
   --  Set whether drag icons are shown while the user is reordering the list.
   --  The default value is True.

   procedure Set_Button_Actions
     (Clist         : access Gtk_Clist_Record;
      Button        : Guint;
      Button_Action : Gtk_Button_Action);
   --  Set the action for a specific button on the list.
   --  The default if for the left mouse button to select or drag and item,
   --  the other buttons are ignored.
   --  The Button_Expands action has no effect on a clist.

   procedure Moveto
     (Clist     : access Gtk_Clist_Record;
      Row       : in Gint;
      Column    : in Gint;
      Row_Align : in Gfloat;
      Col_Align : in Gfloat);
   --  Scroll the list so that Row/Column is visible.
   --  If Row is -1, the clist is not scrolled vertically.
   --  If Column is -1, the clist is not scrolled horizontally.
   --  The new location of Row/Column depends on the value of Row_Align and
   --  Col_Align (from 0.0x0.0 (top-left) to 1.0x1.0 (bottom-right), all
   --  intermediate values are possible).

   ---------------
   -- Row_Data --
   ---------------
   --  You can associate one private data with each row in the clist. If you
   --  want to store multiple values, you should create a record type that
   --  contains all the values, and associate with value with the relevant
   --  line in the clist.
   --  This package is the equivalent of Gtk.Widget.User_Data for the Clists.
   --
   --  This is your responsibility to use the Get and Set functions from the
   --  same generic package. However, you can use different packages for
   --  different lines (although this will definitely make things harder to
   --  use!)
   --
   --  Note also that an internal copy of the Data is done, therefore the
   --  "find" functions found in gtk+ have no equivalent in GtkAda, although it
   --  would be enough to write one by iterating over the Row numbers.

   generic
      --  <doc_ignore>
      type Data_Type (<>) is private;
      --  </doc_ignore>
   package Row_Data is
      function Get
        (Object : access Gtk_Clist_Record'Class;
         Row    : in     Gint) return Data_Type;
      --  Get the data associated to a specific row.

      function Get
        (Object : access Gtk_Clist_Record'Class;
         Row    : in     Gtk_Clist_Row) return Data_Type;
      --  Same as above, but acts directly on a row obtained through
      --  Get_Row_List. This is faster for big lists.

      procedure Set
        (Object : access Gtk_Clist_Record'Class;
         Row    : in Gint;
         Data   : in Data_Type);
      --  Modify the data associated with a row

      procedure Set
        (Object : access Gtk_Clist_Record'Class;
         Row    : in Gtk_Clist_Row;
         Data   : in Data_Type);
      --  Same as above but acts directly on a row obtained through
      --  Get_Row_List. This is faster for big lists.

   private
      --  <doc_ignore>
      procedure Free_Data (Data : System.Address);
      --  Free memory associated with Data
      pragma Convention (C, Free_Data);
      --  </doc_ignore>
   end Row_Data;

   --  <doc_ignore>
   procedure Set_Show_Titles (Clist : access Gtk_Clist_Record; Show : Boolean);
   --  If show is true, call Column_Titles_Show. Do nothing otherwise.
   --  This procedure is primarily used by Gate generated code.
   --  </doc_ignore>

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  </properties>

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "select_row"
   --    procedure Handler (Clist  : access Gtk_Clist_Record'Class;
   --                       Row    : Gint;
   --                       Column : Gint;
   --                       Event  : Gdk.Event.Gdk_Event);
   --
   --    Emitted when a row is selected. Column contains the column number in
   --    which the user has clicked, or -1 if the selection was done internally
   --    by GtkAda.
   --    Event will be null if the selection was not triggered by an event, eg
   --    if the row was selected through a call to Select_Row.
   --
   --  - "unselect_row"
   --    procedure Handler (Clist  : access Gtk_Clist_Record'Class;
   --                       Row    : Gint;
   --                       Column : Gint;
   --                       Event  : Gdk.Event.Gdk_Event);
   --
   --    Emitted to request the unselection of a row. Event will be null most
   --    of the time when the event is emitted directly by GtkAda. You should
   --    use Unselect_Row instead.
   --
   --  - "row_move"
   --    procedure Handler (Clist      : access Gtk_Clist_Record'Class;
   --                       Source_Row : Gint;
   --                       Dest_Row   : Gint);
   --
   --    Emitted to request the change of a Source_Row to Dest_Row. You should
   --    use Row_Move instead.
   --
   --  - "click_column"
   --    procedure Handler (Clist  : access Gtk_Clist_Record'Class;
   --                       Column : Gint);
   --
   --    Emitted when the user has clicked on one of the buttons at the top
   --    of a column. The first column has number 0.
   --
   --  - "resize_column"
   --    procedure Handler (Clist  : access Gtk_Clist_Record'Class;
   --                       Column : Gint;
   --                       Width  : Gint);
   --
   --    Emitted to request a new size for a given column. You should use
   --    Set_Column_Width instead.
   --
   --  - "toggle_focus_row"
   --    procedure Handler (Clist  : access Gtk_Clist_Record'Class);
   --
   --    Emitted to request the change of the selection status (selected/
   --    unselected) of the focus row. This signal is not emitted internally
   --    by GtkAda.
   --
   --  - "select_all"
   --    procedure Handler (Clist  : access Gtk_Clist_Record'Class);
   --
   --    Emitted to request the selection of all the rows in the Clist, if the
   --    selection mode allows. You should use Select_All instead.
   --
   --  - "unselect_all"
   --    procedure Handler (Clist  : access Gtk_Clist_Record'Class);
   --
   --    Emitted to request the unselection of all the rows in the Clist, if
   --    the selection mode is different from Browse. You should use
   --    Unselect_All instead.
   --
   --  - "undo_selection"
   --    procedure Handler (Clist  : access Gtk_Clist_Record'Class);
   --
   --    Emitted to request the cancellation of the last select/unselect
   --    operation. You should use Undo_Selection instead.
   --
   --  - "start_selection"
   --    procedure Handler (Clist  : access Gtk_Clist_Record'Class);
   --
   --    Request the start of the selection. This signal is not emitted
   --    internally by GtkAda, but acts as if the user had clicked on the
   --    focus row (the exact visual modification depends on the selection
   --    mode).
   --
   --  - "end_selection"
   --    procedure Handler (Clist  : access Gtk_Clist_Record'Class);
   --
   --    Ends the current selection process. This is never emitted internally
   --    by GtkAda, but acts as if the user had just released the mouse button.
   --
   --  - "toggle_add_mode"
   --    procedure Handler (Clist  : access Gtk_Clist_Record'Class);
   --
   --    Changes the add_mode for the clist (indicates whether the next line
   --    clicked on will be added to the selection or will replace it).
   --    This is never emitted internally by GtkAda.
   --
   --  - "extend_selection"
   --    procedure Handler (Clist       : access Gtk_Clist_Record'Class;
   --                       Scroll_Type : Gtk.Enums.Gtk_Scroll_Type;
   --                       Position    : Gfloat;
   --                       Auto_Start_Selection : Boolean);
   --
   --    Extends the current selection. Position is used only for certain
   --    values of Scroll_Type. It is never emitted internally by GtkAda. It
   --    has no effect if the selection mode is not Extended.
   --
   --  - "scroll_vertical"
   --    procedure Handler (Clist       : access Gtk_Clist_Record'Class;
   --                       Scroll_Type : Gtk.Enums.Gtk_Scroll_Type;
   --                       Position    : Gfloat);
   --
   --    Scrolls the clist vertically. This also modifies the selection.
   --    It is never emitted internally by GtkAda. You should consider using
   --    Moveto instead.
   --
   --  - "scroll_horizontal"
   --    procedure Handler (Clist       : access Gtk_Clist_Record'Class;
   --                       Scroll_Type : Gtk.Enums.Gtk_Scroll_Type;
   --                       Position    : Gfloat);
   --
   --    Scrolls the clist horizontally. This also modifies the selection.
   --    It is never emitted internally by GtkAda. You should consider using
   --    Moveto instead.
   --
   --  - "abort_column_resize"
   --    procedure Handler (Clist       : access Gtk_Clist_Record'Class);
   --
   --    Aborts the current interactive resizing of the column by the user.
   --    This releases the grab done on the pointer. It is never emitted
   --    internally by GtkAda.
   --
   --  </signals>

   Signal_Abort_Column_Resize    : constant Glib.Signal_Name :=
                                     "abort_column_resize";
   Signal_Click_Column           : constant Glib.Signal_Name :=
                                     "click_column";
   Signal_End_Selection          : constant Glib.Signal_Name :=
                                     "end_selection";
   Signal_Extend_Selection       : constant Glib.Signal_Name :=
                                     "extend_selection";
   Signal_Resize_Column          : constant Glib.Signal_Name :=
                                     "resize_column";
   Signal_Row_Move               : constant Glib.Signal_Name :=
                                     "row_move";
   Signal_Scroll_Horizontal      : constant Glib.Signal_Name :=
                                     "scroll_horizontal";
   Signal_Scroll_Vertical        : constant Glib.Signal_Name :=
                                     "scroll_vertical";
   Signal_Select_All             : constant Glib.Signal_Name :=
                                     "select_all";
   Signal_Select_Row             : constant Glib.Signal_Name :=
                                     "select_row";
   Signal_Set_Scroll_Adjustments : constant Glib.Signal_Name :=
                                     "set_scroll_adjustments";
   Signal_Start_Selection        : constant Glib.Signal_Name :=
                                     "start_selection";
   Signal_Toggle_Add_Mode        : constant Glib.Signal_Name :=
                                     "toggle_add_mode";
   Signal_Toggle_Focus_Row       : constant Glib.Signal_Name :=
                                     "toggle_focus_row";
   Signal_Undo_Selection         : constant Glib.Signal_Name :=
                                     "undo_selection";
   Signal_Unselect_All           : constant Glib.Signal_Name :=
                                     "unselect_all";
   Signal_Unselect_Row           : constant Glib.Signal_Name :=
                                     "unselect_row";

private
   type Gtk_Clist_Record is new Gtk.Container.Gtk_Container_Record with record
      Sort_Func : Gtk_Clist_Compare_Func := null;
   end record;

   pragma Import (C, Get_Type, "gtk_clist_get_type");
end Gtk.Clist;

--  <example>
--  <include>../examples/documentation/clist.adb</include>
--  </example>

--  The following subprograms never had a binding, and are now obsolescent:
--  No binding: gtk_clist_find_row_from_data
--  No binding: gtk_clist_set_row_data
