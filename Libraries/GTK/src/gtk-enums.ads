-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2013, AdaCore                   --
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
--  This package contains a number of types that are shared by several
--  widgets in GtkAda.
--  </description>
--  <c_version>2.16.6</c_version>

with Glib;
with Glib.Generic_Properties;  use Glib.Generic_Properties;
pragma Elaborate_All (Glib.Generic_Properties);
with Glib.Glist, Glib.GSlist;
pragma Elaborate_All (Glib.Glist);
pragma Elaborate_All (Glib.GSlist);

with System;

package Gtk.Enums is

   type Gtk_Anchor_Type is
     (Anchor_Center,
      Anchor_North,
      Anchor_North_West,
      Anchor_North_East,
      Anchor_South,
      Anchor_South_East,
      Anchor_South_West,
      Anchor_West,
      Anchor_East);
   --  Gtk_Anchor_Type indicates the exact location of the widget on its
   --  side. Note that not all anchors are relevant for each side.
   --
   --  For instance, if you put a widget on Side_Right, with an anchor of
   --  Anchor_North, Anchor_North_West or Anchor_North_East, the widget will
   --  in fact appear on the upper right side of the remaining space in the
   --  container.
   --
   --  Thus, if a previous child was added on Side_North, then the new child
   --  will only appear on the second line in the container. The order the
   --  children are inserted into the container is important.

   pragma Convention (C, Gtk_Anchor_Type);

   Anchor_N  : Gtk_Anchor_Type renames Anchor_North;
   Anchor_NW : Gtk_Anchor_Type renames Anchor_North_West;
   Anchor_NE : Gtk_Anchor_Type renames Anchor_North_East;
   Anchor_S  : Gtk_Anchor_Type renames Anchor_South;
   Anchor_SW : Gtk_Anchor_Type renames Anchor_South_West;
   Anchor_SE : Gtk_Anchor_Type renames Anchor_South_East;
   Anchor_W  : Gtk_Anchor_Type renames Anchor_West;
   Anchor_E  : Gtk_Anchor_Type renames Anchor_East;

   type Gtk_Arrow_Type is
     (Arrow_Up,
      Arrow_Down,
      Arrow_Left,
      Arrow_Right);
   --  The various types of arrows that can be represented by GtkAda

   type Gtk_Attach_Options is new Glib.Guint32;
   --  The various options used for attaching widgets to tables

   Expand : constant Gtk_Attach_Options := 1;
   Shrink : constant Gtk_Attach_Options := 2;
   Fill   : constant Gtk_Attach_Options := 4;

   type Gtk_Button_Box_Style is
     (Buttonbox_Default_Style,
      Buttonbox_Spread,
      Buttonbox_Edge,
      Buttonbox_Start,
      Buttonbox_End);
   --  The style for button boxes
   --  - Buttonbox_Spread: The children are spread regularly across the box
   --  - Buttonbox_Edge  : Same as Spread, except that the first and last
   --                      children are aligned on the border of the box.
   --  - Buttonbox_Start : The children are put as much to the left (resp. top)
   --                      as possible in the box.
   --  - Buttonbox_End   : The children are put as much to the right
   --                      (resp. bottom) as possible in the box.

   type Gtk_Curve_Type is
     (Curve_Type_Linear,       --  Linear interpolation
      Curve_Type_Spline,       --  Spline interpolation
      Curve_Type_Free);        --  Free form curve
   --  The curve types that can be used in gtk-curve.ads

   type Gtk_Delete_Type is
     (Delete_Chars,
      Delete_Word_Ends,
      Delete_Words,
      Delete_Display_Lines,
      Delete_Display_Line_Ends,
      Delete_Paragraph_Ends,
      Delete_Paragraphs,
      Delete_Whitespace);
   --  The deletion modes used in the text editor.
   --  Delete_Word_Ends will delete only the portion of the word to the
   --  left/right of the cursor if we are in the middle of a word.
   --  Delete_Paragraph_Ends acts like c-k in Emacs: it deletes the text until,
   --  but not including, the end of line.
   --  Delete_Paragraphs acts like c-k in pico: it deletes the whole line.
   --  Delete_Whitespace acts like M-\ in Emacs, and removes all white spaces
   --  surrounding the cursor.

   type Gtk_Direction_Type is
     (Dir_Tab_Forward,
      Dir_Tab_Backward,
      Dir_Up,
      Dir_Down,
      Dir_Left,
      Dir_Right);
   --  Focus movement types

   type Gtk_Expander_Style is
     (Expander_Collapsed,
      Expander_Semi_Collapsed,
      Expander_Semi_Expanded,
      Expander_Expanded);
   --  Expander styles, as seen in trees

   type Gtk_Icon_Size is new Gint;
   Icon_Size_Invalid       : constant Gtk_Icon_Size := 0;
   Icon_Size_Menu          : constant Gtk_Icon_Size := 1;
   Icon_Size_Small_Toolbar : constant Gtk_Icon_Size := 2;
   Icon_Size_Large_Toolbar : constant Gtk_Icon_Size := 3;
   Icon_Size_Button        : constant Gtk_Icon_Size := 4;
   Icon_Size_Dnd           : constant Gtk_Icon_Size := 5;
   Icon_Size_Dialog        : constant Gtk_Icon_Size := 6;
   --  Built-in stock icon sizes. Depending on the context, icons should be
   --  displayed larger or smaller (typically, icons in menus are much smaller
   --  than icons in toolbars for instance).
   --  New custom icon sizes can be created (see gtk-icon_factory.ads)

   type Gtk_Text_Direction is
     (Text_Dir_None,
      Text_Dir_Ltr,
      Text_Dir_Rtl);
   --  The directory in which text should be written (left to right or
   --  right to left).

   type Gtk_Justification is
     (Justify_Left,
      Justify_Right,
      Justify_Center,
      Justify_Fill);
   --  Within a paragraph, text can be justified in various ways: aligned on
   --  the left, aligned on the right, centered, or justified (in which case
   --  the width of the spaces might vary so that the text is aligned on both
   --  sides).

   type Gtk_Menu_Direction_Type is
     (Menu_Dir_Parent,
      Menu_Dir_Child,
      Menu_Dir_Next,
      Menu_Dir_Prev);
   --  Direction where to move the selection.

   type Gtk_Metric_Type is (Pixels, Inches, Centimeters);
   --  The unit to use when you display a ruler at the top of a drawing area.

   type Gtk_Movement_Step is
     (Movement_Logical_Positions, --  move by forw/back graphemes
      Movement_Visual_Positions,  --  move by left/right graphemes
      Movement_Words,             --  move by forward/back words
      Movement_Display_Lines,     --  move up/down lines (wrapped lines)
      Movement_Display_Line_Ends, --  move up/down lines (wrapped lines)
      Movement_Paragraphs,        --  move up/down paragraphs
      Movement_Paragraph_Ends,    --  move to either end of a paragraph
      Movement_Pages,             --  move by pages
      Movement_Buffer_Ends,       --  move to ends of the buffer
      Movement_Horizontal_Pages); --  move horizontally by pages
   --  The various kind of movements that can be performed within an editor.
   --  Gtk+ has builtin knowledge of all of the above.
   --  Note that a paragraph is defined as a new-line ended line.

   type Gtk_Number_Up_Layout is
     (Left_To_Right_Top_To_Bottom,   --  nick=lrtb
      Left_To_Right_Bottom_To_Top,   --  nick=lrbt
      Right_To_Left_Top_To_Bottom,   --  nick=rltb
      Right_To_Left_Bottom_To_Top,   --  nick=rlbt
      Top_To_Bottom_Left_To_Right,   --  nick=tblr
      Top_To_Bottom_Right_To_Left,   --  nick=tbrl
      Bottom_To_Top_Left_To_Right,   --  nick=btlr
      Bottom_To_Top_Right_To_Left);  --  nick=btrl
   --  Used to determine the layout of pages on a sheet when printing multiple
   --  pages per sheet.

   type Gtk_Scroll_Step is
     (Scroll_Steps,
      Scroll_Pages,
      Scroll_Ends,
      Scroll_Horizontal_Steps,
      Scroll_Horizontal_Pages,
      Scroll_Horizontal_Ends);
   --  The behavior of scrollbars for editors

   type Gtk_Orientation is (Orientation_Horizontal, Orientation_Vertical);
   --  Orientation of widgets. Most widgets have no such notion, but for
   --  instance toolbars can display different kind of information depending
   --  on their current orientation

   type Gtk_Page_Orientation is
     (Page_Orientation_Portrait,
      Page_Orientation_Landscape,
      Page_Orientation_Reverse_Portrait,
      Page_Orientation_Reverse_Landscape);
   --  Orientation of a printed page.

   type Gtk_Corner_Type is
     (Corner_Top_Left,
      Corner_Bottom_Left,
      Corner_Top_Right,
      Corner_Bottom_Right);
   --  Type used by Set_Placement below to determine the location of the
   --  child widget with respect to the scrollbars.
   --  Corner_Top_Left means the child is in the top left, with the scrollbars
   --  underneath and to the right.

   type Gtk_Grid_Lines is
     (Grid_Lines_None,
      Grid_Lines_Horizontal,
      Grid_Lines_Vertical,
      Grid_Lines_Both);
   --  Used to indicate which grid lines to draw in a tree view.

   type Gtk_Pack_Type is (Pack_Start, Pack_End);
   --  Whether items should be added at the start or at the end of the list of
   --  children for a widget. This impacts the visual rendering of containers

   type Gtk_Path_Priority_Type is mod 2 ** 32;
   --  Priorities for path lookups

   Path_Prio_Lowest      : constant Gtk_Path_Priority_Type := 0;
   Path_Prio_Gtk         : constant Gtk_Path_Priority_Type := 4;
   Path_Prio_Application : constant Gtk_Path_Priority_Type := 8;
   Path_Prio_Theme       : constant Gtk_Path_Priority_Type := 10;
   Path_Prio_RC          : constant Gtk_Path_Priority_Type := 12;
   Path_Prio_Highest     : constant Gtk_Path_Priority_Type := 15;
   Path_Prio_Mask        : constant Gtk_Path_Priority_Type := 16#0f#;

   type Gtk_Path_Type is (Path_Widget, Path_Widget_Class, Path_Class);
   --  Widget path types

   type Gtk_Policy_Type is (Policy_Always, Policy_Automatic, Policy_Never);
   --  When should scrollbars be made visible in Gtk_Scrolled_Window

   type Gtk_Position_Type is
     (Pos_Left,
      Pos_Right,
      Pos_Top,
      Pos_Bottom);
   --  Use to define the position of children within a container

   type Gtk_Page_Set is
     (Page_Set_All,
      Page_Set_Even,
      Page_Set_Odd);
   --  The set of pages to print

   type Gtk_Print_Duplex is
     (Print_Duplex_Simplex,
      Print_Duplex_Horizontal,
      Print_Duplex_Vertical);
   --  Whether/how to print on both sides of a sheet.

   type Gtk_Print_Pages is
     (Print_Pages_All,
      Print_Pages_Current,
      Print_Pages_Ranges);
   --  Which pages to print.

   type Gtk_Print_Quality is
     (Print_Quality_Low,
      Print_Quality_Normal,
      Print_Quality_High,
      Print_Quality_Draft);
   --  Quality of printed output.

   type Gtk_Relief_Style is (Relief_Normal, Relief_Half, Relief_None);
   --  Explains how the border of widgets should be displayed

   type Gtk_Resize_Mode is
     (Resize_Parent,     --  Pass request to the parent
      Resize_Queue,      --  Queue resizes on this widget
      Resize_Immediate); --  Perform the resizes now
   --  The resizing of widgets is generally done asynchronously, for efficiency
   --  reasons. This can have some impact on the visual rendering of the widget
   --  which might be an issue in some cases.
   --  This type is only used when you are writting your own containers.

   type Gtk_Scroll_Type is
     (Scroll_None,
      Scroll_Jump,
      Scroll_Step_Backward,
      Scroll_Step_Forward,
      Scroll_Page_Backward,
      Scroll_Page_Forward,
      Scroll_Step_Up,
      Scroll_Step_Down,
      Scroll_Page_Up,
      Scroll_Page_Down,
      Scroll_Step_Left,
      Scroll_Step_Right,
      Scroll_Page_Left,
      Scroll_Page_Right,
      Scroll_Start,
      Scroll_End);
   --  How clists should be scrolled

   type Gtk_Selection_Mode is
     (Selection_None,
      Selection_Single,
      Selection_Browse,
      Selection_Multiple);
   --  Indicates what selection is allowed in a tree (no selection allowed, a
   --  single line, a single line when the mouse is released, or multiple
   --  lines).

   type Gtk_Sensitivity_Type is
     (Gtk_Sensitivity_Auto,
      Gtk_Sensitivity_On,
      Gtk_Sensitivity_Off);
   --  Determines how GTK+ handles the sensitivity of stepper arrows at
   --  the end of range widgets.

   type Gtk_Shadow_Type is
     (Shadow_None,
      Shadow_In,
      Shadow_Out,
      Shadow_Etched_In,
      Shadow_Etched_Out);
   --  The type of shadows that can be drawn around widgets

   type Gtk_State_Type is
     (State_Normal,
      State_Active,
      State_Prelight,
      State_Selected,
      State_Insensitive);
   --  Widgets can be in various states. This impacts their visual rendering,
   --  but can also impact whether they react to events or not (they do not
   --  when in State_Insensitive mode).

   type Gtk_Toolbar_Style is
     (Toolbar_Icons,
      Toolbar_Text,
      Toolbar_Both,
      Toolbar_Both_Horiz);
   --  The style of toolbars. Toolbar_Both_Horiz indicates that both icon and
   --  text should be displayed, arranged horizontally.

   type Gtk_Unit is (Pixel, Points, Inch, MM);
   --  Unit of distance measurement.

   type Gtk_Update_Type is
     (Update_Continuous,
      Update_Discontinuous,
      Update_Delayed);
   --  For some widgets, this indicates how often they should be updated

   type Gtk_Visibility is
     (Visibility_None,
      Visibility_Partial,
      Visibility_Full);
   --  Generic visibility flags. This indicate how visible a window currently
   --  is.

   type Gtk_Window_Position is
     (Win_Pos_None,
      Win_Pos_Center,
      Win_Pos_Mouse,
      Win_Pos_Center_Always,
      Win_Pos_Center_On_Parent);
   --  The position at which a new window should be initially displayed on the
   --  screen.

   type Gtk_Window_Type is
     (Window_Toplevel,
      Window_Popup);
   --  GtkAda supports multiple types of windows. They all act as top-level
   --  containers, but the amount of decoration is different. A popup window
   --  has no title bar for instance.

   type Gtk_Wrap_Mode is
     (Wrap_None,
      Wrap_Char,
      Wrap_Word,
      Wrap_Word_Char);
   --  Text wrapping algorithm. This indicates where a text widget is allowed
   --  to break its contents to display multiple lines when a line doesn't fit
   --  on the screen.

   type Gtk_Sort_Type is
     (Sort_Ascending,
      Sort_Descending);
   --  How to sort

   type Gtk_Pack_Direction is
     (Pack_Direction_LTR,
      Pack_Direction_RTL,
      Pack_Direction_TTB,
      Pack_Direction_BTT);
   --  The direction in which children should be packed in their parents
   --  (Left-to-Right, Right-To-Left, Top-To-Bottom or Bottom-To-Top)

   type Gtk_Text_Window_Type is
     (Text_Window_Private,
      Text_Window_Widget,
      Text_Window_Text,
      Text_Window_Left,
      Text_Window_Right,
      Text_Window_Top,
      Text_Window_Bottom);
   --  The various components of a Gtk.Text_View widget

   -------------------------------
   --  Some Glib instantiations --
   -------------------------------

   function Convert (S : String) return System.Address;
   function Convert (S : System.Address) return String;
   function Convert_I (I : Gint) return System.Address;
   function Convert_A (S : System.Address) return Gint;
   pragma Import (C, Convert_I, "convert_i");
   pragma Import (C, Convert_A, "convert_a");

   package String_List is new Glib.Glist.Generic_List (UTF8_String);
   package String_SList is new Glib.GSlist.Generic_SList (UTF8_String);
   --  Warning: when you create this list, new memory gets allocated for
   --  all the strings. You should use the function Free_String_List
   --  instead of Glib.Glist.Free to be sure to free this memory.

   procedure Free_String_List (List : in out String_List.Glist);
   procedure Free_String_List (List : in out String_SList.GSlist);
   --  Free the memory occupied by all the strings in the list, as well
   --  as the memory occupied by the list itself.

   package Gint_List is new
     Glib.Glist.Generic_List (Gint, Convert_I, Convert_A);

   function Convert_UI (I : Guint) return System.Address;
   function Convert_UA (S : System.Address) return Guint;
   pragma Import (C, Convert_UI, "convert_ui");
   pragma Import (C, Convert_UA, "convert_ua");
   package Guint_List is new
     Glib.Glist.Generic_List (Guint, Convert_UI, Convert_UA);

   pragma Convention (C, Gtk_Arrow_Type);
   pragma Convention (C, Gtk_Button_Box_Style);
   pragma Convention (C, Gtk_Curve_Type);
   pragma Convention (C, Gtk_Delete_Type);
   pragma Convention (C, Gtk_Direction_Type);
   pragma Convention (C, Gtk_Grid_Lines);
   pragma Convention (C, Gtk_Path_Type);
   pragma Convention (C, Gtk_Expander_Style);
   pragma Convention (C, Gtk_Text_Direction);
   pragma Convention (C, Gtk_Justification);
   pragma Convention (C, Gtk_Menu_Direction_Type);
   pragma Convention (C, Gtk_Metric_Type);
   pragma Convention (C, Gtk_Movement_Step);
   pragma Convention (C, Gtk_Number_Up_Layout);
   pragma Convention (C, Gtk_Orientation);
   pragma Convention (C, Gtk_Page_Orientation);
   pragma Convention (C, Gtk_Corner_Type);
   pragma Convention (C, Gtk_Pack_Type);
   pragma Convention (C, Gtk_Policy_Type);
   pragma Convention (C, Gtk_Position_Type);
   pragma Convention (C, Gtk_Page_Set);
   pragma Convention (C, Gtk_Print_Duplex);
   pragma Convention (C, Gtk_Print_Pages);
   pragma Convention (C, Gtk_Print_Quality);
   pragma Convention (C, Gtk_Relief_Style);
   pragma Convention (C, Gtk_Resize_Mode);
   pragma Convention (C, Gtk_Scroll_Type);
   pragma Convention (C, Gtk_Selection_Mode);
   pragma Convention (C, Gtk_Sensitivity_Type);
   pragma Convention (C, Gtk_Shadow_Type);
   pragma Convention (C, Gtk_State_Type);
   pragma Convention (C, Gtk_Text_Window_Type);
   pragma Convention (C, Gtk_Toolbar_Style);
   pragma Convention (C, Gtk_Update_Type);
   pragma Convention (C, Gtk_Unit);
   pragma Convention (C, Gtk_Visibility);
   pragma Convention (C, Gtk_Window_Position);
   pragma Convention (C, Gtk_Window_Type);
   pragma Convention (C, Gtk_Wrap_Mode);
   pragma Convention (C, Gtk_Sort_Type);

   ----------------
   -- Properties --
   ----------------
   --  The following packages and types are used to represent properties of
   --  the given type. They are used in the packages that use these properties

   package Relief_Style_Properties is new
     Generic_Internal_Discrete_Property (Gtk_Relief_Style);
   package Resize_Mode_Properties is new
     Generic_Internal_Discrete_Property (Gtk_Resize_Mode);
   package Arrow_Type_Properties is new
     Generic_Internal_Discrete_Property (Gtk_Arrow_Type);
   package Shadow_Type_Properties is new
     Generic_Internal_Discrete_Property (Gtk_Shadow_Type);
   package Update_Type_Properties is new
     Generic_Internal_Discrete_Property (Gtk_Update_Type);
   package Position_Type_Properties is new
     Generic_Internal_Discrete_Property (Gtk_Position_Type);
   package Toolbar_Style_Properties is new
     Generic_Internal_Discrete_Property (Gtk_Toolbar_Style);
   package Justification_Properties is new
     Generic_Internal_Discrete_Property (Gtk_Justification);
   package Orientation_Properties is new
     Generic_Internal_Discrete_Property (Gtk_Orientation);
   package Window_Type_Properties is new
     Generic_Internal_Discrete_Property (Gtk_Window_Type);
   package Window_Position_Properties is new
     Generic_Internal_Discrete_Property (Gtk_Window_Position);
   package Text_Direction_Properties is new
     Generic_Internal_Discrete_Property (Gtk_Text_Direction);
   package Wrap_Mode_Properties is new
     Generic_Internal_Discrete_Property (Gtk_Text_Direction);
   package Policy_Properties is new
     Generic_Internal_Discrete_Property (Gtk_Policy_Type);
   package Pack_Direction_Properties is new
     Generic_Internal_Discrete_Property (Gtk_Pack_Direction);
   package Sort_Type_Properties is new
     Generic_Internal_Discrete_Property (Gtk_Sort_Type);
   package Metric_Type_Properties is new
     Generic_Internal_Discrete_Property (Gtk_Metric_Type);
   package Pack_Type_Properties is new
     Generic_Internal_Discrete_Property (Gtk_Pack_Type);
   package Icon_Size_Properties is new
     Generic_Internal_Discrete_Property (Gtk_Icon_Size);
   package Selection_Mode_Properties is new
     Generic_Internal_Discrete_Property (Gtk_Selection_Mode);
   package Sensitivity_Properties is new
     Generic_Internal_Discrete_Property (Gtk_Sensitivity_Type);
   package BBox_Style_Properties is new
     Generic_Internal_Discrete_Property (Gtk_Button_Box_Style);
   package Curve_Type_Properties is new
     Generic_Internal_Discrete_Property (Gtk_Curve_Type);

   type Property_Gtk_Relief_Style  is new Relief_Style_Properties.Property;
   type Property_Gtk_Resize_Mode   is new Resize_Mode_Properties.Property;
   type Property_Gtk_Arrow_Type    is new Arrow_Type_Properties.Property;
   type Property_Gtk_Shadow_Type   is new Shadow_Type_Properties.Property;
   type Property_Gtk_Update_Type   is new Update_Type_Properties.Property;
   type Property_Gtk_Position_Type is new Position_Type_Properties.Property;
   type Property_Gtk_Toolbar_Style is new Toolbar_Style_Properties.Property;
   type Property_Gtk_Button_Box_Style is new BBox_Style_Properties.Property;
   type Property_Gtk_Justification is new Justification_Properties.Property;
   type Property_Gtk_Orientation   is new Orientation_Properties.Property;
   type Property_Gtk_Window_Type   is new Window_Type_Properties.Property;
   type Property_Gtk_Window_Position is new
     Window_Position_Properties.Property;
   type Property_Gtk_Text_Direction is new Text_Direction_Properties.Property;
   type Property_Gtk_Wrap_Mode     is new Wrap_Mode_Properties.Property;
   type Property_Gtk_Policy_Type   is new Policy_Properties.Property;
   type Property_Pack_Direction    is new Pack_Direction_Properties.Property;
   type Property_Sort_Type         is new Sort_Type_Properties.Property;
   type Property_Metric_Type       is new Metric_Type_Properties.Property;
   type Property_Pack_Type         is new Pack_Type_Properties.Property;
   type Property_Gtk_Icon_Size     is new Icon_Size_Properties.Property;
   type Property_Gtk_Selection_Mode is new Selection_Mode_Properties.Property;
   type Property_Gtk_Sensitivity_Type is new Sensitivity_Properties.Property;
   type Property_Gtk_Curve_Type    is new Curve_Type_Properties.Property;

   -----------------
   -- Obsolescent --
   -----------------
   --  All subprograms below are now obsolescent in gtk+. They might be removed
   --  from future versions of gtk+ (and therefore GtkAda).
   --  To find out whether your code uses any of these, we recommend compiling
   --  with the -gnatwj switch
   --  <doc_ignore>

   type Gtk_Side_Type is (Side_Top, Side_Bottom, Side_Left, Side_Right);
   --  pragma Obsolescent;
   pragma Convention (C, Gtk_Side_Type);

   type Gtk_Match_Type is
     (Match_All, Match_All_Tail, Match_Head, Match_Tail,
      Match_Exact, Match_Last);
   --  pragma Obsolescent;
   pragma Convention (C, Gtk_Match_Type);

   type Gtk_Preview_Type is (Preview_Color, Preview_Grayscale);
   --  pragma Obsolescent;
   pragma Convention (C, Gtk_Preview_Type);

   type Gtk_Submenu_Direction is (Direction_Left, Direction_Right);
   --  pragma Obsolescent;

   type Gtk_Submenu_Placement is (Top_Bottom, Left_Right);
   --  pragma Obsolescent;

   type Gtk_Toolbar_Space_Style is (Toolbar_Space_Empty, Toolbar_Space_Line);
   --  pragma Obsolescent;

   pragma Convention (C, Gtk_Toolbar_Space_Style);

   package Toolbar_Space_Style_Properties is new
     Generic_Internal_Discrete_Property (Gtk_Toolbar_Space_Style);
   type Property_Toolbar_Space_Style is new
     Toolbar_Space_Style_Properties.Property;

   type Gtk_Tree_View_Mode is (Tree_View_Line, Tree_View_Item);
   --  pragma Obsolescent;

   pragma Convention (C, Gtk_Submenu_Direction);
   pragma Convention (C, Gtk_Submenu_Placement);
   pragma Convention (C, Gtk_Tree_View_Mode);
   --  </doc_ignore>

   --  These comments are for the sake of our automatic tool to check whether
   --  the bindings are up-to-date. They indicate that the functions are part
   --  of GtkAda itself, no gtk+
   --  External binding: convert_ui
   --  External binding: convert_ua
   --  External binding: convert_i
   --  External binding: convert_a
end Gtk.Enums;
