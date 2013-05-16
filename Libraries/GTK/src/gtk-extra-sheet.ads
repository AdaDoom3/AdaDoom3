-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--      Copyright (C) 2000 E. Briot, J. Brobecker and A. Charlet     --
--                Copyright (C) 2000-2006 AdaCore                    --
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
--  A Gtk_Sheet is a table like the one you can find in most spreadsheets.
--  Each cell can contain some text or any kind of widgets.
--  </description>
--  <c_version>gtkextra 2.1.1</c_version>
--  <group>GtkExtra, additional widgets</group>
--  <testgtk>create_sheet.adb</testgtk>
--  <screenshot>gtk-sheet</screenshot>

with Gtk.Adjustment;  use Gtk.Adjustment;
with Gtk.Container;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.GEntry;
with Gdk.Rectangle;
with Gdk.Color;
with Gdk.GC;
with Pango.Font;
with Gtk.Widget;

package Gtk.Extra.Sheet is

   type Gtk_Sheet_Record is new Gtk.Container.Gtk_Container_Record
     with private;
   type Gtk_Sheet is access all Gtk_Sheet_Record'Class;

   type Gtk_Sheet_Range_Record is record
      Row0, Col0 : Gint;  --  Upper-left cell
      Rowi, Coli : Gint;  --  Lower-Right cell
   end record;

   type Gtk_Sheet_Range is access all Gtk_Sheet_Range_Record;
   --  A range in the sheet.
   --  This is a part of the sheet represented by its upper-left cell and
   --  its lower-right cell.
   --  Most operations below apply to such ranges.

   type Gtk_Sheet_Child_Record is
     new Gtk.Widget.Gtk_Widget_Record with private;
   type Gtk_Sheet_Child is access all Gtk_Sheet_Child_Record'Class;
   --  A widget insert in the sheet.
   --  This structure includes both a widget pointer and the position in the
   --  table in which it is put.

   ----------------
   -- Enum types --
   ----------------

   --  <doc_ignore>
   type Sheet_Attr_Type is
     (Sheet_Foreground,
      Sheet_Background,
      Sheet_Font,
      Sheet_Justification,
      Sheet_Border,
      Sheet_Border_Color,
      Sheet_Is_Editable,
      Sheet_Is_Visible);
   --  Attributes that can be set for the sheet.
   --  No function is provided at the Ada level to modify these attributes
   --  directly.
   pragma Convention (C, Sheet_Attr_Type);
   --  </doc_ignore>

   type Sheet_State is
     (Sheet_Normal,
      Sheet_Row_Selected,
      Sheet_Column_Selected,
      Sheet_Range_Selected);
   --  The state of the selection.
   pragma Convention (C, Sheet_State);

   type Gtk_Sheet_Border is new Integer;
   --  Mask that indicates which borders should be visible in a cell.

   No_Border     : constant Gtk_Sheet_Border;
   Left_Border   : constant Gtk_Sheet_Border;
   Right_Border  : constant Gtk_Sheet_Border;
   Top_Border    : constant Gtk_Sheet_Border;
   Bottom_Border : constant Gtk_Sheet_Border;
   All_Borders   : constant Gtk_Sheet_Border;

   -------------------------------
   -- Creation and modification --
   -------------------------------

   procedure Gtk_New
     (Sheet      : out Gtk_Sheet;
      Rows       : Guint;
      Columns    : Guint;
      Title      : UTF8_String := "";
      Entry_Type : Gtk_Type := GType_Invalid);
   --  Create a new sheet with a specific number of rows and columns.
   --  You can fully specify which type the entry used to modify the value of
   --  cells should have. The value of Entry_Type can be found by using one
   --  of the Get_Type subprograms in the GtkAda packages.
   --  The Title is internal, and does not appear on the screen.

   procedure Initialize
     (Sheet      : access Gtk_Sheet_Record'Class;
      Rows       : Guint;
      Columns    : Guint;
      Title      : UTF8_String := "";
      Entry_Type : Gtk_Type := GType_Invalid);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Gtk_New_Browser
     (Sheet   : out Gtk_Sheet;
      Rows    : Guint;
      Columns : Guint;
      Title   : UTF8_String := "");
   --  Create a new sheet browser with a specific number of rows and columns.
   --  This is a standard Gtk_Sheet, except that it is read-only and that its
   --  cells will automatically resize themselves depending on their contents.

   procedure Initialize_Browser (Sheet   : access Gtk_Sheet_Record'Class;
                                 Rows    : in Guint;
                                 Columns : in Guint;
                                 Title   : in UTF8_String := "");
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Sheet.

   procedure Set_Hadjustment
     (Sheet      : access Gtk_Sheet_Record;
      Adjustment : access Gtk_Adjustment_Record'Class);
   --  Change the horizontal adjustment.
   --  It indicates what range of columns is visible.

   procedure Set_Vadjustment
      (Sheet      : access Gtk_Sheet_Record;
       Adjustment : access Gtk_Adjustment_Record'Class);
   --  Change the vertical adjustment.
   --  It indicates what range of rows is visible.

   function Get_Vadjustment
     (Sheet  : access Gtk_Sheet_Record)
      return      Gtk.Adjustment.Gtk_Adjustment;
   --  Return the adjustment used to indicate the range of visible rows.

   function Get_Hadjustment
     (Sheet  : access Gtk_Sheet_Record)
      return      Gtk.Adjustment.Gtk_Adjustment;
   --  Return the adjustment used to indicate the range of visible columns.

   procedure Change_Entry
     (Sheet      : access Gtk_Sheet_Record;
      Entry_Type : Gtk_Type);
   --  Change the type of widget used to interactively modify the value of
   --  the cells.

   function Get_Entry
     (Sheet : access Gtk_Sheet_Record) return Gtk.GEntry.Gtk_Entry;
   --  Return the entry used to modify the content of the cells.
   --  This can be the same widget as Get_Entry_Widget, if set, or some default
   --  widget associated with the specific child otherwise

   function Get_Entry_Widget
     (Sheet : access Gtk_Sheet_Record) return Gtk.Widget.Gtk_Widget;
   --  Return the entry used to modify the content of the cells.

   procedure Set_Title
     (Sheet : access Gtk_Sheet_Record;
      Title : UTF8_String);
   --  Change the title of the sheet.

   procedure Freeze (Sheet : access Gtk_Sheet_Record);
   --  Freeze all visual updates of the sheet, until you thaw it.
   --  The update will occur in a more efficient way.

   procedure Thaw (Sheet : access Gtk_Sheet_Record);
   --  Thaw the sheet, so that visual updates occur again.
   --  Note that you have to call Thaw as many times as you have called
   --  Freeze to actually thaw the widget.

   procedure Moveto
     (Sheet     : access Gtk_Sheet_Record;
      Row       : Gint;
      Column    : Gint;
      Row_Align : Gfloat;
      Col_Align : Gfloat);
   --  Scroll the viewing area to (Row, Column).
   --  (Row_Align, Col_Align) represent the location on the screen that the
   --  cell should appear at. (0.0, 0.0) is at the top-left of the screen,
   --  whereas (1.0, 1.0) is at the bottom-right corner.
   --  If Row or Column is negative, there is no change.

   procedure Set_Background
     (Sheet : access Gtk_Sheet_Record; Color : Gdk.Color.Gdk_Color);
   --  Change the background color of the cells.

   procedure Set_Grid
     (Sheet : access Gtk_Sheet_Record; Color : Gdk.Color.Gdk_Color);
   --  Set the color to use for the grid.

   procedure Show_Grid
     (Sheet : access Gtk_Sheet_Record; Show : Boolean);
   --  Whether the grid should be made visible

   function Grid_Visible (Sheet : access Gtk_Sheet_Record) return Boolean;
   --  Whether the grid is currently visible

   ----------------------------
   -- Selection and Clipping --
   ----------------------------

   function Get_State (Sheet : access Gtk_Sheet_Record) return Sheet_State;
   --  Return the status of the selection in the sheet.

   function Get_Range (Sheet : access Gtk_Sheet_Record) return Gtk_Sheet_Range;
   --  Return the selected range.

   procedure Get_Visible_Range
     (Sheet     : access Gtk_Sheet_Record;
      The_Range : out Gtk_Sheet_Range);
   --  Return the range visible on the screen.

   procedure Set_Selection_Mode
     (Sheet : access Gtk_Sheet_Record;
      Mode  : Gtk.Enums.Gtk_Selection_Mode);
   --  Change the selection mode.

   procedure Select_Column
     (Sheet  : access Gtk_Sheet_Record;
      Column : Gint);
   --  Replace the current selection with a specific column.
   --  The range is highlighted.

   procedure Select_Row
     (Sheet : access Gtk_Sheet_Record;
      Row   : Gint);
   --  Replace the current selection with a specific row.
   --  The range is highlighted.

   procedure Set_Autoresize
     (Sheet : access Gtk_Sheet_Record; Autoresize : Boolean);
   --  Whether cells should automatically resize to fit their contents

   function Autoresize (Sheet : access Gtk_Sheet_Record) return Boolean;
   --  Whether cells automatically resize to fit their contents

   procedure Set_Autoscroll
     (Sheet : access Gtk_Sheet_Record; Autoscroll : Boolean);
   --  Whether the sheet should automatically scroll to show the active cell at
   --  all times.

   function Autoscroll (Sheet : access Gtk_Sheet_Record) return Boolean;
   --  Whether the sheet automatically scrolls to show the active cell at all
   --  times.

   procedure Set_Clip_Text
     (Sheet : access Gtk_Sheet_Record; Clip : Boolean);
   --  Set when the text contained in the cells is automatically clipped to
   --  their width.

   function Clip_Text (Sheet : access Gtk_Sheet_Record) return Boolean;
   --  Whether the text contained in the cells is automatically clipped to
   --  their width.

   procedure Set_Justify_Entry
     (Sheet : access Gtk_Sheet_Record; Justify_Entry : Boolean);
   --  Set when the justification attribute for entries should be taken into
   --  account

   function Justify_Entry (Sheet : access Gtk_Sheet_Record) return Boolean;
   --  Whether the justification attribute is used for entries

   procedure Set_Locked
     (Sheet : access Gtk_Sheet_Record; Locked : Boolean);
   --  If Locked is true, the cells are no longer editable

   function Locked (Sheet : access Gtk_Sheet_Record) return Boolean;
   --  Whether cells are currently read-only

   procedure Select_Range
     (Sheet     : access Gtk_Sheet_Record;
      The_Range : Gtk_Sheet_Range);
   --  Select a new range of cells.

   procedure Unselect_Range (Sheet : access Gtk_Sheet_Record);
   --  Unselect a specific range of cells.
   --  If null is passed, the current selected range is used.

   procedure Clip_Range (Sheet     : access Gtk_Sheet_Record;
                         The_Range : in Gtk_Sheet_Range);
   --  Create a new clip range, which is copied to the clipboard
   --  That range is flashed on the screen.

   procedure Unclip_Range (Sheet : access Gtk_Sheet_Record);
   --  Destroy the clip area.

   function In_Clip (Sheet : access Gtk_Sheet_Record) return Boolean;
   --  Whether a range was copied to the clipboard

   function Set_Active_Cell
     (Sheet  : access Gtk_Sheet_Record;
      Row    : Gint;
      Column : Gint)
      return Boolean;
   --  Set active cell where the entry will be displayed.
   --  Returns FALSE if the current cell can not be deactivated or if the
   --  requested cell can't be activated.
   --  Depending on the value passed to Set_Autoscroll, the sheet might be
   --  scrolled.

   procedure Get_Active_Cell
     (Sheet  : access Gtk_Sheet_Record;
      Row    : out Gint;
      Column : out Gint);
   --  Return the coordinates of the active cell.
   --  This is the cell that the user is currently editing.

   -------------
   -- Columns --
   -------------

   procedure Set_Column_Title
     (Sheet  : access Gtk_Sheet_Record;
      Column : Gint;
      Title  : UTF8_String);
   --  Modify the title of a column.
   --  The first column on the left has the number 0.
   --  Note that this title does not appear on the screen, and can only be
   --  used internally to find a specific column.

   function Get_Column_Title
     (Sheet  : access Gtk_Sheet_Record;
      Column : Gint) return UTF8_String;
   --  Return the title of a specific column.

   procedure Set_Column_Titles_Height
     (Sheet  : access Gtk_Sheet_Record; Height : Guint);
   --  Modify the height of the row in which the column titles appear.

   procedure Column_Button_Add_Label
     (Sheet  : access Gtk_Sheet_Record;
      Column : Gint;
      Label  : UTF8_String);
   --  Modify the label of the button that appears at the top of each column.

   function Column_Button_Get_Label
     (Sheet : access Gtk_Sheet_Record; Column : Gint) return UTF8_String;
   --  Return the label for the button that appears at the top of each column

   procedure Column_Button_Justify
      (Sheet         : access Gtk_Sheet_Record;
       Column        : Gint;
       Justification : Gtk.Enums.Gtk_Justification);
   --  Modify the justification for the label in the column button.

   procedure Show_Column_Titles (Sheet : access Gtk_Sheet_Record);
   --  Show the row in which the column titles appear.

   procedure Hide_Column_Titles (Sheet : access Gtk_Sheet_Record);
   --  Hide the row in which the column titles appear.

   function Column_Titles_Visible
     (Sheet : access Gtk_Sheet_Record) return Boolean;
   --  Whether a special row is added at the top to show the title of the
   --  columns.

   procedure Columns_Set_Sensitivity
     (Sheet     : access Gtk_Sheet_Record;
      Sensitive : Boolean);
   --  Modify the sensitivity of all the columns.
   --  If Sensitive is False, the columns can not be resized dynamically.
   --  This also modifies the sensitivity of the button at the top of the
   --  columns.

   procedure Column_Set_Sensitivity
     (Sheet     : access Gtk_Sheet_Record;
      Column    : Gint;
      Sensitive : Boolean);
   --  Modify the sensitivity of a specific column and its title button.
   --  If Sensitive if False, the column can not be dynamically resized.

   procedure Column_Set_Visibility
     (Sheet   : access Gtk_Sheet_Record;
      Column  : Gint;
      Visible : Boolean);
   --  Change the visibility of a column.

   procedure Columns_Set_Resizable
     (Sheet : access Gtk_Sheet_Record; Resizable : Boolean);
   --  Whether columns are resizable

   function Columns_Resizable (Sheet : access Gtk_Sheet_Record) return Boolean;
   --  Whether columns are resizable

   procedure Column_Label_Set_Visibility
     (Sheet   : access Gtk_Sheet_Record;
      Column  : Gint;
      Visible : Boolean := True);
   --  Change the visibility of the label in a given column.

   procedure Columns_Labels_Set_Visibility
     (Sheet   : access Gtk_Sheet_Record;
      Visible : Boolean := True);
   --  Change the visibility for all the column labels.

   procedure Set_Column_Width
     (Sheet  : access Gtk_Sheet_Record;
      Column : Gint;
      Width  : Guint);
   --  Modify the width in pixels of a specific column.

   function Get_Column_Width (Sheet  : access Gtk_Sheet_Record;
                              Column : in Gint)
                             return Gint;
   --  Return the width in pixels of the Column-nth in Sheet.

   procedure Add_Column
     (Sheet : access Gtk_Sheet_Record;
      Ncols : Guint);
   --  Add some empty columns at the end of the sheet.

   procedure Insert_Columns
     (Sheet : access Gtk_Sheet_Record;
      Col   : Guint;
      Ncols : Guint);
   --  Add Ncols empty columns just before the columns number Col.

   procedure Delete_Columns
     (Sheet : access Gtk_Sheet_Record;
      Col   : Guint;
      Ncols : Guint);
   --  Delete Ncols columns starting from Col.

   procedure Column_Set_Justification
     (Sheet         : access Gtk_Sheet_Record;
      Column        : Gint;
      Justification : Gtk.Enums.Gtk_Justification);
   --  Set the default justification for the cells in the specific column.

   function Get_Columns_Count (Sheet : access Gtk_Sheet_Record) return Guint;
   --  Return the maximum column number of the displayed cells.

   ----------
   -- Rows --
   ----------

   procedure Set_Row_Title
     (Sheet : access Gtk_Sheet_Record;
      Row   : Gint;
      Title : UTF8_String);
   --  Modify the title of a row.
   --  The first row at the top has the number 0.
   --  Note that this title does not appear on the screen, and can only be
   --  used internally to find a specific row.

   function Get_Row_Title
     (Sheet  : access Gtk_Sheet_Record; Row : Gint) return UTF8_String;
   --  Return the title of a specific row.

   procedure Set_Row_Titles_Width
     (Sheet : access Gtk_Sheet_Record; Width : Guint);
   --  Modify the width of the column that has the row titles.

   procedure Row_Button_Add_Label
     (Sheet : access Gtk_Sheet_Record;
      Row   : Gint;
      Label : UTF8_String);
   --  Modify the label of the button that appears on the left of each row.

   function Row_Button_Get_Label
     (Sheet : access Gtk_Sheet_Record; Row : Gint) return UTF8_String;
   --  Return the label for the button that appears on the left of each row.

   procedure Row_Button_Justify
      (Sheet         : access Gtk_Sheet_Record;
       Row           : Gint;
       Justification : Gtk.Enums.Gtk_Justification);
   --  Modify the justification for the label of the row button.

   procedure Show_Row_Titles (Sheet : access Gtk_Sheet_Record);
   --  Show the column in which the row titles appear.

   procedure Hide_Row_Titles (Sheet : access Gtk_Sheet_Record);
   --  Hide the column in which the row titles appear.

   function Row_Titles_Visible
     (Sheet : access Gtk_Sheet_Record) return Boolean;
   --  Whether a special column is added to the left to show the title of the
   --  rows.

   procedure Rows_Set_Sensitivity (Sheet     : access Gtk_Sheet_Record;
                                   Sensitive : in Boolean);
   --  Modify the sensitivity of all the rows.
   --  If Sensitive is False, the rows can not be resized dynamically.
   --  This also modifies the sensitivity of the button at the left of the
   --  row.

   procedure Row_Set_Sensitivity (Sheet     : access Gtk_Sheet_Record;
                                  Row       : in Gint;
                                  Sensitive : in Boolean);
   --  Modify the sensitivity of a specific row and its title button.
   --  If Sensitive if False, the row can not be dynamically resized.

   procedure Row_Set_Visibility (Sheet   : access Gtk_Sheet_Record;
                                 Row     : in Gint;
                                 Visible : in Boolean);
   --  Modify the visibility of a specific row

   procedure Row_Label_Set_Visibility
     (Sheet   : access Gtk_Sheet_Record;
      Row    : in Gint;
      Visible : in Boolean := True);
   --  Change the visibility of the label in a given Row.

   procedure Rows_Labels_Set_Visibility
     (Sheet   : access Gtk_Sheet_Record;
      Visible : Boolean := True);
   --  Change the visibility for all the row labels.

   procedure Rows_Set_Resizable
     (Sheet : access Gtk_Sheet_Record; Resizable : Boolean);
   --  Whether rows are resizable

   function Rows_Resizable (Sheet : access Gtk_Sheet_Record) return Boolean;
   --  Whether rows are resizable

   procedure Set_Row_Height
     (Sheet  : access Gtk_Sheet_Record;
      Row    : Gint;
      Height : Guint);
   --  Set the height in pixels of a specific row.

   function Get_Row_Height (Sheet   : access Gtk_Sheet_Record;
                            Row     : in Gint)
                           return Gint;
   --  Return the height in pixels of the Row-th row in Sheet.

   procedure Add_Row
     (Sheet : access Gtk_Sheet_Record;
      Nrows : Guint);
   --  Append Nrows row at the end of the sheet.

   procedure Insert_Rows
     (Sheet : access Gtk_Sheet_Record;
      Row   : Guint;
      Nrows : Guint);
   --  Add Nrows empty rows just before the row number Row.

   procedure Delete_Rows
     (Sheet : access Gtk_Sheet_Record;
      Row   : Guint;
      Nrows : Guint);
   --  Delete Nrows rows starting from Row.

   function Get_Rows_Count (Sheet : access Gtk_Sheet_Record) return Guint;
   --  Return the maximum row number of displayed cells.

   -----------
   -- Range --
   -----------

   function Range_Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associate with a Gtk_Sheet_Range

   procedure Range_Clear
     (Sheet     : access Gtk_Sheet_Record;
      The_Range : Gtk_Sheet_Range);
   --  Clear the content of the range.

   procedure Range_Delete
     (Sheet     : access Gtk_Sheet_Record;
      The_Range : Gtk_Sheet_Range);
   --  Clear the content of the range and delete all the links (user_data)

   procedure Range_Set_Background
     (Sheet     : access Gtk_Sheet_Record;
      The_Range : Gtk_Sheet_Range;
      Color     : Gdk.Color.Gdk_Color);
   --  Set the background color for the cells in a specific range.

   procedure Range_Set_Foreground
     (Sheet     : access Gtk_Sheet_Record;
      The_Range : Gtk_Sheet_Range;
      Color     : Gdk.Color.Gdk_Color);
   --  Set the foreground color for the cells in a specific range.

   procedure Range_Set_Justification
     (Sheet         : access Gtk_Sheet_Record;
      The_Range     : Gtk_Sheet_Range;
      Justification : Gtk.Enums.Gtk_Justification);
   --  Set the text justification for the cells in the range.

   procedure Range_Set_Editable
     (Sheet     : access Gtk_Sheet_Record;
      The_Range : Gtk_Sheet_Range;
      Editable  : Boolean);
   --  Set whether the cells in the range are editable.

   procedure Range_Set_Visible (Sheet     : access Gtk_Sheet_Record;
                                The_Range : in Gtk_Sheet_Range;
                                Visible   : in Boolean);
   --  Set whether the cells in the range are visible.

   procedure Range_Set_Border
     (Sheet      : access Gtk_Sheet_Record;
      The_Range  : Gtk_Sheet_Range;
      Mask       : Gtk_Sheet_Border;
      Width      : Guint;
      Line_Style : Gdk.GC.Gdk_Line_Style);
   --  Set the style of the border for the cells in the range.

   procedure Range_Set_Border_Color
     (Sheet     : access Gtk_Sheet_Record;
      The_Range : Gtk_Sheet_Range;
      Color     : Gdk.Color.Gdk_Color);
   --  Change the color for the borders of the cells in the range.

   procedure Range_Set_Font
     (Sheet     : access Gtk_Sheet_Record;
      The_Range : Gtk_Sheet_Range;
      Font      : Pango.Font.Pango_Font_Description);
   --  Change the font of the cells in the range.

   -----------
   -- Cells --
   -----------

   procedure Set_Cell
     (Sheet         : access Gtk_Sheet_Record;
      Row           : Gint;
      Col           : Gint;
      Justification : Gtk.Enums.Gtk_Justification;
      Text          : UTF8_String);
   --  Set the cell contents.
   --  Set Text to the empty string to delete the content of the cell.

   procedure Set_Cell_Text
     (Sheet : access Gtk_Sheet_Record;
      Row   : Gint;
      Col   : Gint;
      Text  : UTF8_String);
   --  Set the cell contents.
   --  The justification used is the previous one used in that cell.

   function Cell_Get_Text
     (Sheet  : access Gtk_Sheet_Record;
      Row    : Gint;
      Col    : Gint)
      return UTF8_String;
   --  Return the text put in a specific cell.
   --  The empty string is returned if there is no text in that cell.

   procedure Cell_Clear
     (Sheet : access Gtk_Sheet_Record;
      Row   : Gint;
      Col   : Gint);
   --  Clear the contents of the cell.

   procedure Cell_Delete
     (Sheet : access Gtk_Sheet_Record;
      Row   : Gint;
      Col   : Gint);
   --  Clear the contents of the cell and remove the user data associated
   --  with it.

   function Cell_Get_State (Sheet  : access Gtk_Sheet_Record;
                            Row    : in Gint;
                            Col    : in Gint)
                           return  Gtk.Enums.Gtk_State_Type;
   --  Return the state of the cell (normal or selected).

   procedure Get_Pixel_Info
     (Sheet  : access Gtk_Sheet_Record;
      X      : Gint;
      Y      : Gint;
      Row    : out Gint;
      Column : out Gint);
   --  Return the row and column matching a given pixel on the screen.
   --  Constraint_Error is raised if no such cell exists.

   procedure Get_Cell_Area
     (Sheet  : access Gtk_Sheet_Record;
      Row    : Gint;
      Column : Gint;
      Area   : out Gdk.Rectangle.Gdk_Rectangle);
   --  Get the area of the screen that a cell is mapped to.
   --  Constraint_Error is raised if no such cell exists;

   --------------
   -- Children --
   --------------
   --  A Gtk_Sheet can contain some children, attached to some specific
   --  cells.

   procedure Put
     (Sheet  : access Gtk_Sheet_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      X      : Gint;
      Y      : Gint);
   --  Put a new child at a specific location (in pixels) in the sheet.

   procedure Attach
      (Sheet    : access Gtk_Sheet_Record;
       Widget   : access Gtk.Widget.Gtk_Widget_Record'Class;
       Row      : Gint;
       Col      : Gint;
       Xoptions : Gtk.Enums.Gtk_Attach_Options := Expand or Fill;
       Yoptions : Gtk.Enums.Gtk_Attach_Options := Expand or Fill;
       Xpadding : Gint := 0;
       Ypadding : Gint := 0);
   --  Attach a child to a specific Cell in the sheet.
   --  X_Align and Y_Align should be between 0.0 and 1.0, indicating that
   --  the child should be aligned from the Left (resp. Top) to the Right
   --  (resp. Bottom) of the cell.
   --  If Row or Col is negative, the widget is attached to the row buttons or
   --  column buttons.
   --  Widget will not be moved if the cell is moved.

   procedure Attach_Floating
      (Sheet    : access Gtk_Sheet_Record;
       Widget   : access Gtk.Widget.Gtk_Widget_Record'Class;
       Row      : Gint;
       Col      : Gint);
   --  Attach a child at the current location or (Row, Col).
   --  If the cell is moved because of resizing or other reasons, Widget will
   --  be moved as well.

   procedure Move_Child
      (Sheet  : access Gtk_Sheet_Record;
       Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
       X      : Gint;
       Y      : Gint);
   --  Move a child of the table to a specific location in pixels.
   --  A warning is printed if Widget is not already a child of Sheet.

   function Get_Child_At
     (Sheet  : access Gtk_Sheet_Record;
      Row    : Gint;
      Col    : Gint)
      return Gtk_Sheet_Child;
   --  Return the widget associated with the cell.

   function Get_Widget (Child : Gtk_Sheet_Child) return Gtk.Widget.Gtk_Widget;
   --  Return the widget in the child.

   procedure Button_Attach
     (Sheet   : access Gtk_Sheet_Record;
      Widget  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Row     : Gint;
      Col     : Gint);
   --  Attach a new button in the row or column title.
   --  One of Row or Col must be negative (but only one).
   --  This can be used to modify the standard buttons that appear at the top
   --  of each column, or on the left of each row.

   -----------------------
   -- Links / User_Data --
   -----------------------
   --  You can associate any kind of data with a cell, just like you
   --  can associate user_data with all the widgets.
   --  Note that this uses a generic package, which must be instantiated at
   --  library level since it has internal clean up functions.

   generic
      --  <doc_ignore>
      type Data_Type (<>) is private;
   package Links is
      type Data_Type_Access is access all Data_Type;
      --  </doc_ignore>

      procedure Link_Cell
        (Sheet : access Gtk_Sheet_Record'Class;
         Row   : Gint;
         Col   : Gint;
         Link  : Data_Type);
      --  Associate some user specific data with a given cell.

      function Get_Link
        (Sheet  : access Gtk_Sheet_Record'Class;
         Row    : Gint;
         Col    : Gint)
         return  Data_Type_Access;
      --  Return the user data associated with the cell.
      --  null is returned if the cell has no user data.

   end Links;

   procedure Remove_Link
     (Sheet : access Gtk_Sheet_Record;
      Row   : Gint;
      Col   : Gint);
   --  Delete the user data associated with the cell.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "set_scroll_adjustments"
   --    procedure Handler (Sheet : access Gtk_Sheet_Record'Class;
   --                       Hadj  : Gtk_Adjustement;
   --                       Vadj  : Gtk_Adjustment);
   --
   --    Emitted when the adjustments used to indicate which area of the sheet
   --    is visible are set or changed. This is not called when their value is
   --    changed, only when a new one is set.
   --
   --  - "select_row"
   --    procedure Handler (Sheet : access Gtk_Sheet_Record'Class;
   --                       Row   : Gint);
   --
   --    Emitted when a new row is selected.
   --
   --  - "select_column"
   --    procedure Handler (Sheet  : access Gtk_Sheet_Record'Class;
   --                       Column : Gint);
   --
   --    Emitted when a new column is selected.
   --
   --  - "select_range"
   --    procedure Handler (Sheet     : access Gtk_Sheet_Record'Class;
   --                       The_Range : Gtk_Sheet_Range);
   --
   --    Emitted when a new range of cells is selected.
   --
   --  - "clip_range"
   --    procedure Handler (Sheet      : access Gtk_Sheet_Record'Class;
   --                       Clip_Range : Gtk_Sheet_Range);
   --
   --    Emitted when the clip area is set to a new value.
   --
   --  - "resize_range"
   --    procedure Handler (Sheet     : access Gtk_Sheet_Record'Class;
   --                       Old_Range : Gtk_Sheet_Range;
   --                       New_Range : Gtk_Sheet_Range);
   --
   --    Emitted when the current range of selected cell is resized (ie new
   --    cells are added to it or removed from it).
   --
   --  - "move_range"
   --    procedure Handler (Sheet     : access Gtk_Sheet_Record'Class;
   --                       Old_Range : Gtk_Sheet_Range;
   --                       New_Range : Gtk_Sheet_Range);
   --
   --    Emitted when the current range of selected cell is moved (ie the
   --    top-left cell is changed, but the size is not modified).
   --
   --  - "traverse"
   --    function Handler (Sheet      : access Gtk_Sheet_Record'Class;
   --                      Row        : Gint;
   --                      Column     : Gint;
   --                      New_Row    : Gint_Access;
   --                      New_Column : Gint_Access)
   --                     return Boolean;
   --
   --    Emitted when the user wants to make a new cell active. The coordinates
   --    of the currently active cell are passed in (Row, Column), the
   --    coordinates of the cell that the user would like to select are
   --    passed in (New_Row, New_Column). The callback can modify the new
   --    values, and should return True if the new coordinates are accepted,
   --    False if the selection should be refused.
   --
   --  - "deactivate"
   --    function Handler (Sheet  : access Gtk_Sheet_Record'Class;
   --                      Row    : Gint;
   --                      Column : Gint)
   --                     return Boolean;
   --
   --    Emitted when the user wants to deactivate a specific cell. The
   --    callback should return True if the cell can be deactivated, False
   --    otherwise. See the subprogram Deactivate_Cell.
   --
   --  - "activate"
   --    function Handler (Sheet  : access Gtk_Sheet_Record'Class;
   --                      Row    : Gint;
   --                      Column : Gint)
   --                     return Boolean;
   --
   --    Emitted when the user wants to activate a specific cell. The
   --    callback should return True if the cell can be activated, False
   --    otherwise. See the subprogram Activate_Cell.
   --
   --  - "set_cell"
   --    procedure Handler (Sheet  : access Gtk_Sheet_Record'Class;
   --                       Row    : Gint;
   --                       Column : Gint);
   --
   --    Emitted from Hide_Active_Cell, when the cell is non-empty. ???
   --
   --  - "clear_cell"
   --    procedure Handler (Sheet  : access Gtk_Sheet_Record'Class;
   --                       Row    : Gint;
   --                       Column : Gint);
   --
   --    Emitted when the content of the cell has been deleted (the text is
   --    now the empty string).
   --
   --  - "changed"
   --    procedure Handler (Sheet  : access Gtk_Sheet_Record'Class;
   --                       Row    : Gint;
   --                       Column : Gint);
   --
   --    Emitted when the content of the cell is modified (either the text
   --    itself, or its properties, alignment,...)
   --    A value of -1 for Row or Column means the row title, the column
   --    title, or their intersection.
   --
   --  - "new_column_width"
   --    procedure Handler (Sheet  : access Gtk_Sheet_Record'Class;
   --                       Column : Gint;
   --                       Width  : Guint);
   --
   --    Emitted whenever the width of the column is changed, either by the
   --    user or automatically if the cells should automatically resize
   --    themselves depending on their contents).
   --
   --  - "new_row_height"
   --    procedure Handler (Sheet  : access Gtk_Sheet_Record'Class;
   --                       Row    : Gint;
   --                       Height : Guint);
   --
   --    Emitted whenever the height of the row is changed.
   --  </signals>

private
   type Gtk_Sheet_Record is new Gtk.Container.Gtk_Container_Record
     with null record;
   type Gtk_Sheet_Child_Record is
     new Gtk.Widget.Gtk_Widget_Record with null record;

   pragma Import (C, Get_Type, "gtk_sheet_get_type");
   pragma Import (C, Range_Get_Type, "gtk_sheet_range_get_type");

   No_Border     : constant Gtk_Sheet_Border := 0;
   Left_Border   : constant Gtk_Sheet_Border := 1;
   Right_Border  : constant Gtk_Sheet_Border := 2;
   Top_Border    : constant Gtk_Sheet_Border := 4;
   Bottom_Border : constant Gtk_Sheet_Border := 8;
   All_Borders   : constant Gtk_Sheet_Border := 15;
   pragma Convention (C, Gtk_Sheet_Range);
end Gtk.Extra.Sheet;

--  Unbound:
--    gtk_sheet_get_entry_widget
--    gtk_sheet_get_attributes
