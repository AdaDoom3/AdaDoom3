-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                 Copyright (C) 2001-2013, AdaCore                  --
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

with Ada.Unchecked_Conversion;
with System;
with Gdk.Dnd;               use Gdk.Dnd;
with Gdk.Types;             use Gdk.Types;
with Gtk;                   use Gtk;
with Gtk.Cell_Renderer;     use Gtk.Cell_Renderer;
with Gtk.Selection;         use Gtk.Selection;
with Gtk.Tree_Model;        use Gtk.Tree_Model;
with Gtk.Tree_View_Column;  use Gtk.Tree_View_Column;

with Glib.Type_Conversion_Hooks;

package body Gtk.Tree_View is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Tree_View_Record);
   pragma Warnings (Off, Type_Conversion);

   type Gtk_Tree_Iter_Access is access all Gtk_Tree_Iter;
   function To_Iter is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Tree_Iter_Access);

   procedure Internal_Cell_Data_Func
     (Tree_Column, Cell, Model, Iter : System.Address; Data : Cell_Data_Func);
   --  Internal proxy for a Cell_Data_Func.

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Tree_View) is
   begin
      Widget := new Gtk_Tree_View_Record;
      Gtk.Tree_View.Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Tree_View_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_tree_view_new");

   begin
      Set_Object (Widget, Internal);
   end Initialize;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget : out Gtk_Tree_View;
      Model  : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class) is
   begin
      Widget := new Gtk_Tree_View_Record;
      Initialize (Widget, Model);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget : access Gtk_Tree_View_Record'Class;
      Model  : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class)
   is
      function Internal (Model  : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_view_new_with_model");

   begin
      Set_Object (Widget, Internal (Get_Object (Model)));
   end Initialize;

   --------------------
   -- Get_Bin_Window --
   --------------------

   function Get_Bin_Window
     (Tree_View : access Gtk_Tree_View_Record) return Gdk.Window.Gdk_Window
   is
      function Internal
        (Tree_View : System.Address) return Gdk.Window.Gdk_Window;
      pragma Import (C, Internal, "gtk_tree_view_get_bin_window");

   begin
      return Internal (Get_Object (Tree_View));
   end Get_Bin_Window;

   ---------------
   -- Get_Model --
   ---------------

   function Get_Model
     (Tree_View : access Gtk_Tree_View_Record)
      return Gtk.Tree_Model.Gtk_Tree_Model
   is
      function Internal (Tree_View : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_view_get_model");

      Stub : Gtk.Tree_Model.Gtk_Tree_Model_Record;

   begin
      return Gtk.Tree_Model.Gtk_Tree_Model
        (Get_User_Data_Fast (Internal (Get_Object (Tree_View)), Stub));
   end Get_Model;

   ---------------
   -- Set_Model --
   ---------------

   procedure Set_Model
     (Tree_View : access Gtk_Tree_View_Record;
      Model     : Gtk.Tree_Model.Gtk_Tree_Model)
   is
      procedure Internal
        (Tree_View : System.Address;
         Model     : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_set_model");

   begin
      if Model = null then
         Internal (Get_Object (Tree_View), System.Null_Address);
      else
         Internal (Get_Object (Tree_View), Get_Object (Model));
      end if;
   end Set_Model;

   -------------------
   -- Get_Selection --
   -------------------

   function Get_Selection
     (Tree_View : access Gtk_Tree_View_Record)
      return Gtk.Tree_Selection.Gtk_Tree_Selection
   is
      function Internal (Tree_View : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_view_get_selection");

      Stub : Gtk.Tree_Selection.Gtk_Tree_Selection_Record;

   begin
      return Gtk.Tree_Selection.Gtk_Tree_Selection
        (Get_User_Data_Fast (Internal (Get_Object (Tree_View)), Stub));
   end Get_Selection;

   ---------------------
   -- Get_Hadjustment --
   ---------------------

   function Get_Hadjustment
     (Tree_View : access Gtk_Tree_View_Record)
      return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Tree_View : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_view_get_hadjustment");

      Stub : Gtk.Adjustment.Gtk_Adjustment_Record;

   begin
      return Gtk.Adjustment.Gtk_Adjustment
        (Get_User_Data_Fast (Internal (Get_Object (Tree_View)), Stub));
   end Get_Hadjustment;

   ---------------------
   -- Set_Hadjustment --
   ---------------------

   procedure Set_Hadjustment
     (Tree_View  : access Gtk_Tree_View_Record;
      Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Internal
        (Tree_View  : System.Address;
         Adjustment : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_set_hadjustment");

   begin
      Internal (Get_Object (Tree_View), Get_Object (Adjustment));
   end Set_Hadjustment;

   ---------------------
   -- Get_Vadjustment --
   ---------------------

   function Get_Vadjustment
     (Tree_View : access Gtk_Tree_View_Record)
      return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Tree_View : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_view_get_vadjustment");

      Stub : Gtk.Adjustment.Gtk_Adjustment_Record;

   begin
      return Gtk.Adjustment.Gtk_Adjustment
        (Get_User_Data_Fast (Internal (Get_Object (Tree_View)), Stub));
   end Get_Vadjustment;

   ---------------------
   -- Set_Vadjustment --
   ---------------------

   procedure Set_Vadjustment
     (Tree_View  : access Gtk_Tree_View_Record;
      Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Internal
        (Tree_View  : System.Address;
         Adjustment : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_set_vadjustment");

   begin
      Internal (Get_Object (Tree_View), Get_Object (Adjustment));
   end Set_Vadjustment;

   ---------------------------
   -- Get_Enable_Tree_Lines --
   ---------------------------

   function Get_Enable_Tree_Lines
     (Tree_View : access Gtk_Tree_View_Record) return Boolean
   is
      function Internal (Tree_View : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_get_enable_tree_lines");
   begin
      return Boolean'Val (Internal (Get_Object (Tree_View)));
   end Get_Enable_Tree_Lines;

   ---------------------------
   -- Set_Enable_Tree_Lines --
   ---------------------------

   procedure Set_Enable_Tree_Lines
     (Tree_View : access Gtk_Tree_View_Record;
      Enabled   : Boolean)
   is
      procedure Internal
        (Tree_View : System.Address;
         Enabled   : Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_set_enable_tree_lines");
   begin
      Internal (Get_Object (Tree_View), Boolean'Pos (Enabled));
   end Set_Enable_Tree_Lines;

   --------------------
   -- Get_Grid_Lines --
   --------------------

   function Get_Grid_Lines
     (Tree_View : access Gtk_Tree_View_Record) return Gtk.Enums.Gtk_Grid_Lines
   is
      function Internal
        (Tree_View : System.Address) return Gtk.Enums.Gtk_Grid_Lines;
      pragma Import (C, Internal, "gtk_tree_view_get_grid_lines");
   begin
      return Internal (Get_Object (Tree_View));
   end Get_Grid_Lines;

   --------------------
   -- Set_Grid_Lines --
   --------------------

   procedure Set_Grid_Lines
     (Tree_View  : access Gtk_Tree_View_Record;
      Grid_Lines : Gtk.Enums.Gtk_Grid_Lines)
   is
      procedure Internal
        (Tree_View  : System.Address;
         Grid_Lines : Gtk.Enums.Gtk_Grid_Lines);
      pragma Import (C, Internal, "gtk_tree_view_set_grid_lines");
   begin
      Internal (Get_Object (Tree_View), Grid_Lines);
   end Set_Grid_Lines;

   ---------------------------
   -- Get_Level_Indentation --
   ---------------------------

   function Get_Level_Indentation
     (Tree_View : access Gtk_Tree_View_Record) return Gint
   is
      function Internal (Tree_View : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_tree_view_get_level_indentation");
   begin
      return Internal (Get_Object (Tree_View));
   end Get_Level_Indentation;

   ---------------------------
   -- Set_Level_Indentation --
   ---------------------------

   procedure Set_Level_Indentation
     (Tree_View   : access Gtk_Tree_View_Record;
      Indentation : Gint)
   is
      procedure Internal
        (Tree_View   : System.Address;
         Indentation : Gint);
      pragma Import (C, Internal, "gtk_tree_view_set_level_indentation");
   begin
      Internal (Get_Object (Tree_View), Indentation);
   end Set_Level_Indentation;

   ------------------------
   -- Get_Rubber_Banding --
   ------------------------

   function Get_Rubber_Banding
     (Tree_View : access Gtk_Tree_View_Record) return Boolean
   is
      function Internal (Tree_View : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_get_rubber_banding");
   begin
      return Boolean'Val (Internal (Get_Object (Tree_View)));
   end Get_Rubber_Banding;

   ------------------------
   -- Set_Rubber_Banding --
   ------------------------

   procedure Set_Rubber_Banding
     (Tree_View : access Gtk_Tree_View_Record;
      Enable    : Boolean)
   is
      procedure Internal
        (Tree_View : System.Address;
         Enable    : Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_set_rubber_banding");
   begin
      Internal (Get_Object (Tree_View), Boolean'Pos (Enable));
   end Set_Rubber_Banding;

   ------------------------------
   -- Is_Rubber_Banding_Active --
   ------------------------------

   function Is_Rubber_Banding_Active
     (Tree_View : access Gtk_Tree_View_Record) return Boolean
   is
      function Internal (Tree_View : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_is_rubber_banding_active");
   begin
      return Boolean'Val (Internal (Get_Object (Tree_View)));
   end Is_Rubber_Banding_Active;

   ------------------------
   -- Get_Show_Expanders --
   ------------------------

   function Get_Show_Expanders
     (Tree_View : access Gtk_Tree_View_Record) return Boolean
   is
      function Internal (Tree_View : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_get_show_expanders");
   begin
      return Boolean'Val (Internal (Get_Object (Tree_View)));
   end Get_Show_Expanders;

   ------------------------
   -- Set_Show_Expanders --
   ------------------------

   procedure Set_Show_Expanders
     (Tree_View : access Gtk_Tree_View_Record;
      Enabled   : Boolean)
   is
      procedure Internal
        (Tree_View : System.Address;
         Enabled   : Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_set_show_expanders");
   begin
      Internal (Get_Object (Tree_View), Boolean'Pos (Enabled));
   end Set_Show_Expanders;

   -------------------------
   -- Get_Headers_Visible --
   -------------------------

   function Get_Headers_Visible
     (Tree_View : access Gtk_Tree_View_Record) return Boolean
   is
      function Internal (Tree_View : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_get_headers_visible");

   begin
      return Internal (Get_Object (Tree_View)) /= 0;
   end Get_Headers_Visible;

   -------------------------
   -- Set_Headers_Visible --
   -------------------------

   procedure Set_Headers_Visible
     (Tree_View       : access Gtk_Tree_View_Record;
      Headers_Visible : Boolean)
   is
      procedure Internal
        (Tree_View       : System.Address;
         Headers_Visible : Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_set_headers_visible");

   begin
      Internal (Get_Object (Tree_View), Boolean'Pos (Headers_Visible));
   end Set_Headers_Visible;

   ----------------------
   -- Columns_Autosize --
   ----------------------

   procedure Columns_Autosize (Tree_View : access Gtk_Tree_View_Record) is
      procedure Internal (Tree_View : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_columns_autosize");

   begin
      Internal (Get_Object (Tree_View));
   end Columns_Autosize;

   ---------------------------
   -- Get_Headers_Clickable --
   ---------------------------

   function Get_Headers_Clickable
     (Tree_View : access Gtk_Tree_View_Record) return Boolean
   is
      function Internal (Tree_View : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_get_headers_clickable");
   begin
      return Boolean'Val (Internal (Get_Object (Tree_View)));
   end Get_Headers_Clickable;

   ---------------------------
   -- Set_Headers_Clickable --
   ---------------------------

   procedure Set_Headers_Clickable
     (Tree_View : access Gtk_Tree_View_Record;
      Setting   : Boolean)
   is
      procedure Internal
        (Tree_View : System.Address;
         Setting   : Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_set_headers_clickable");

   begin
      Internal (Get_Object (Tree_View), Boolean'Pos (Setting));
   end Set_Headers_Clickable;

   --------------------
   -- Set_Rules_Hint --
   --------------------

   procedure Set_Rules_Hint
     (Tree_View : access Gtk_Tree_View_Record;
      Setting   : Boolean)
   is
      procedure Internal
        (Tree_View : System.Address;
         Setting   : Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_set_rules_hint");

   begin
      Internal (Get_Object (Tree_View), Boolean'Pos (Setting));
   end Set_Rules_Hint;

   --------------------
   -- Get_Rules_Hint --
   --------------------

   function Get_Rules_Hint
     (Tree_View : access Gtk_Tree_View_Record) return Boolean
   is
      function Internal (Tree_View : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_get_rules_hint");

   begin
      return Internal (Get_Object (Tree_View)) /= 0;
   end Get_Rules_Hint;

   -------------------
   -- Append_Column --
   -------------------

   function Append_Column
     (Tree_View : access Gtk_Tree_View_Record;
      Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column) return Gint
   is
      function Internal
        (Tree_View : System.Address;
         Column    : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_tree_view_append_column");

   begin
      return Internal (Get_Object (Tree_View), Get_Object (Column));
   end Append_Column;

   -------------------
   -- Remove_Column --
   -------------------

   function Remove_Column
     (Tree_View : access Gtk_Tree_View_Record;
      Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column) return Gint
   is
      function Internal
        (Tree_View : System.Address;
         Column    : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_tree_view_remove_column");

   begin
      return Internal (Get_Object (Tree_View), Get_Object (Column));
   end Remove_Column;

   -------------------
   -- Insert_Column --
   -------------------

   function Insert_Column
     (Tree_View : access Gtk_Tree_View_Record;
      Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Position  : Gint := -1) return Gint
   is
      function Internal
        (Tree_View : System.Address;
         Column    : System.Address;
         Position  : Gint) return Gint;
      pragma Import (C, Internal, "gtk_tree_view_insert_column");

   begin
      return Internal (Get_Object (Tree_View), Get_Object (Column), Position);
   end Insert_Column;

   ----------------
   -- Get_Column --
   ----------------

   function Get_Column
     (Tree_View : access Gtk_Tree_View_Record;
      N         : Gint)
      return Gtk.Tree_View_Column.Gtk_Tree_View_Column
   is
      function Internal
        (Tree_View : System.Address;
         N         : Gint) return System.Address;
      pragma Import (C, Internal, "gtk_tree_view_get_column");

      Stub :  Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record;

   begin
      return Gtk.Tree_View_Column.Gtk_Tree_View_Column
        (Get_User_Data_Fast (Internal (Get_Object (Tree_View), N), Stub));
   end Get_Column;

   -----------------
   -- Get_Columns --
   -----------------

   function Get_Columns
     (Tree_View : access Gtk_Tree_View_Record)
      return Gtk.Tree_View_Column.Column_List.Glist
   is
      function Internal (Tree_View : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_view_get_columns");

      List : Gtk.Tree_View_Column.Column_List.Glist;

   begin
      Gtk.Tree_View_Column.Column_List.Set_Object
        (List, Internal (Get_Object (Tree_View)));
      return List;
   end Get_Columns;

   -------------------
   -- Get_Tree_View --
   -------------------

   function Get_Tree_View
     (Tree_Column : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record)
      return Gtk_Tree_View
   is
      function Internal (Tree_Column : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_view_column_get_tree_view");
      Stub : Gtk_Tree_View_Record;
   begin
      return Gtk_Tree_View
        (Get_User_Data (Internal (Get_Object (Tree_Column)), Stub));
   end Get_Tree_View;

   -----------------------
   -- Move_Column_After --
   -----------------------

   procedure Move_Column_After
     (Tree_View   : access Gtk_Tree_View_Record;
      Column      : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Base_Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column)
   is
      procedure Internal
        (Tree_View   : System.Address;
         Column      : System.Address;
         Base_Column : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_move_column_after");

   begin
      Internal (Get_Object (Tree_View),
                Get_Object (Column),
                Get_Object (Base_Column));
   end Move_Column_After;

   -------------------------
   -- Set_Expander_Column --
   -------------------------

   procedure Set_Expander_Column
     (Tree_View : access Gtk_Tree_View_Record;
      Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column)
   is
      procedure Internal
        (Tree_View : System.Address;
         Column    : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_set_expander_column");

   begin
      if Column = null then
         Internal (Get_Object (Tree_View), System.Null_Address);
      else
         Internal (Get_Object (Tree_View), Get_Object (Column));
      end if;
   end Set_Expander_Column;

   -------------------------
   -- Get_Expander_Column --
   -------------------------

   function Get_Expander_Column
     (Tree_View : access Gtk_Tree_View_Record)
     return Gtk.Tree_View_Column.Gtk_Tree_View_Column
   is
      function Internal (Tree_View : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_view_get_expander_column");

      Stub : Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record;

   begin
      return Gtk.Tree_View_Column.Gtk_Tree_View_Column
        (Get_User_Data_Fast (Internal (Get_Object (Tree_View)), Stub));
   end Get_Expander_Column;

   ---------------------
   -- Scroll_To_Point --
   ---------------------

   procedure Scroll_To_Point
     (Tree_View : access Gtk_Tree_View_Record;
      Tree_X    : Gint;
      Tree_Y    : Gint)
   is
      procedure Internal
        (Tree_View : System.Address;
         Tree_X    : Gint;
         Tree_Y    : Gint);
      pragma Import (C, Internal, "gtk_tree_view_scroll_to_point");

   begin
      Internal (Get_Object (Tree_View), Tree_X, Tree_Y);
   end Scroll_To_Point;

   --------------------
   -- Scroll_To_Cell --
   --------------------

   procedure Scroll_To_Cell
     (Tree_View : access Gtk_Tree_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Use_Align : Boolean;
      Row_Align : Gfloat;
      Col_Align : Gfloat)
   is
      procedure Internal
        (Tree_View : System.Address;
         Path      : Gtk.Tree_Model.Gtk_Tree_Path;
         Column    : System.Address;
         Use_Align : Gboolean;
         Row_Align : Gfloat;
         Col_Align : Gfloat);
      pragma Import (C, Internal, "gtk_tree_view_scroll_to_cell");

      Column_Address : System.Address;

   begin
      if Column = null then
         Column_Address := System.Null_Address;
      else
         Column_Address := Get_Object (Column);
      end if;

      Internal (Get_Object (Tree_View),
                Path,
                Column_Address,
                Boolean'Pos (Use_Align),
                Row_Align,
                Col_Align);
   end Scroll_To_Cell;

   -------------------
   -- Row_Activated --
   -------------------

   procedure Row_Activated
     (Tree_View : access Gtk_Tree_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column)
   is
      procedure Internal
        (Tree_View : System.Address;
         Path      : Gtk.Tree_Model.Gtk_Tree_Path;
         Column    : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_row_activated");

   begin
      Internal (Get_Object (Tree_View), Path, Get_Object (Column));
   end Row_Activated;

   ----------------
   -- Expand_All --
   ----------------

   procedure Expand_All (Tree_View : access Gtk_Tree_View_Record) is
      procedure Internal (Tree_View : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_expand_all");

   begin
      Internal (Get_Object (Tree_View));
   end Expand_All;

   ------------------
   -- Collapse_All --
   ------------------

   procedure Collapse_All (Tree_View : access Gtk_Tree_View_Record)
   is
      procedure Internal (Tree_View : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_collapse_all");
   begin
      Internal (Get_Object (Tree_View));
   end Collapse_All;

   ----------------
   -- Expand_Row --
   ----------------

   function Expand_Row
     (Tree_View : access Gtk_Tree_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      Open_All  : Boolean) return Boolean
   is
      function Internal
        (Tree_View : System.Address;
         Path      : Gtk.Tree_Model.Gtk_Tree_Path;
         Open_All  : Gboolean) return Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_expand_row");

   begin
      return
         Internal (Get_Object (Tree_View), Path, Boolean'Pos (Open_All)) /= 0;
   end Expand_Row;

   ------------------
   -- Collapse_Row --
   ------------------

   function Collapse_Row
     (Tree_View : access Gtk_Tree_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path)
      return Boolean
   is
      function Internal
        (Tree_View : System.Address;
         Path      : Gtk.Tree_Model.Gtk_Tree_Path) return Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_collapse_row");

   begin
      return Internal (Get_Object (Tree_View), Path) /= 0;
   end Collapse_Row;

   ------------------
   -- Row_Expanded --
   ------------------

   function Row_Expanded
     (Tree_View : access Gtk_Tree_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean
   is
      function Internal
        (Tree_View : System.Address;
         Path      : Gtk.Tree_Model.Gtk_Tree_Path) return Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_row_expanded");

   begin
      return Internal (Get_Object (Tree_View), Path) /= 0;
   end Row_Expanded;

   ---------------------
   -- Set_Reorderable --
   ---------------------

   procedure Set_Reorderable
     (Tree_View   : access Gtk_Tree_View_Record;
      Reorderable : Boolean)
   is
      procedure Internal
        (Tree_View   : System.Address;
         Reorderable : Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_set_reorderable");

   begin
      Internal (Get_Object (Tree_View), Boolean'Pos (Reorderable));
   end Set_Reorderable;

   ---------------------
   -- Get_Reorderable --
   ---------------------

   function Get_Reorderable
     (Tree_View : access Gtk_Tree_View_Record) return Boolean
   is
      function Internal (Tree_View : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_get_reorderable");

   begin
      return Internal (Get_Object (Tree_View)) /= 0;
   end Get_Reorderable;

   ----------------
   -- Set_Cursor --
   ----------------

   procedure Set_Cursor
     (Tree_View     : access Gtk_Tree_View_Record;
      Path          : Gtk.Tree_Model.Gtk_Tree_Path;
      Focus_Column  : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Start_Editing : Boolean)
   is
      procedure Internal
        (Tree_View     : System.Address;
         Path          : Gtk.Tree_Model.Gtk_Tree_Path;
         Focus_Column  : System.Address;
         Start_Editing : Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_set_cursor");

      Column_Address : System.Address;

   begin
      if Focus_Column = null then
         Column_Address := System.Null_Address;
      else
         Column_Address := Get_Object (Focus_Column);
      end if;

      Internal (Get_Object (Tree_View),
                Path,
                Column_Address,
                Boolean'Pos (Start_Editing));
   end Set_Cursor;

   ----------------
   -- Get_Cursor --
   ----------------

   procedure Get_Cursor
     (Tree_View    : access Gtk_Tree_View_Record;
      Path         : out Gtk.Tree_Model.Gtk_Tree_Path;
      Focus_Column : out Gtk.Tree_View_Column.Gtk_Tree_View_Column)
   is
      procedure Internal
        (Tree_View    : System.Address;
         Path         : out Gtk.Tree_Model.Gtk_Tree_Path;
         Focus_Column : out System.Address);
      pragma Import (C, Internal, "gtk_tree_view_get_cursor");

      Local_Column : System.Address;
      Stub         : Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record;

   begin
      Internal (Get_Object (Tree_View), Path, Local_Column);
      Focus_Column := Gtk.Tree_View_Column.Gtk_Tree_View_Column
        (Get_User_Data_Fast (Local_Column, Stub));
   end Get_Cursor;

   ---------------------
   -- Get_Path_At_Pos --
   ---------------------

   procedure Get_Path_At_Pos
     (Tree_View : access Gtk_Tree_View_Record;
      X         : Gint;
      Y         : Gint;
      Path      : out Gtk.Tree_Model.Gtk_Tree_Path;
      Column    : out Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Cell_X    : out Gint;
      Cell_Y    : out Gint;
      Row_Found : out Boolean)
   is
      function Internal
        (Tree_View : System.Address;
         X         : Gint;
         Y         : Gint;
         Path      : access Gtk.Tree_Model.Gtk_Tree_Path;
         Column    : access System.Address;
         Cell_X    : access Gint;
         Cell_Y    : access Gint) return Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_get_path_at_pos");

      Local_X, Local_Y : aliased Gint;

      Local_Column : aliased System.Address;
      Local_Path   : aliased Gtk.Tree_Model.Gtk_Tree_Path;

      Stub         : Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record;

   begin
      Row_Found := Internal
        (Get_Object (Tree_View),
         X, Y,
         Local_Path'Access,
         Local_Column'Access,
         Local_X'Access, Local_Y'Access) /= 0;
      Cell_X := Local_X;
      Cell_Y := Local_Y;

      Column := Gtk.Tree_View_Column.Gtk_Tree_View_Column
        (Get_User_Data_Fast (Local_Column, Stub));

      Path := Local_Path;
   end Get_Path_At_Pos;

   -------------------
   -- Get_Cell_Area --
   -------------------

   procedure Get_Cell_Area
     (Tree_View : access Gtk_Tree_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Rect      : out Gdk.Rectangle.Gdk_Rectangle)
   is
      procedure Internal
        (Tree_View : System.Address;
         Path      : Gtk.Tree_Model.Gtk_Tree_Path;
         Column    : System.Address;
         Rect      : out Gdk.Rectangle.Gdk_Rectangle);
      pragma Import (C, Internal, "gtk_tree_view_get_cell_area");

      Col_Addr : System.Address;
   begin
      if Column = null then
         Col_Addr := System.Null_Address;
      else
         Col_Addr := Get_Object (Column);
      end if;

      Internal (Get_Object (Tree_View), Path, Col_Addr, Rect);
   end Get_Cell_Area;

   -------------------------
   -- Get_Background_Area --
   -------------------------

   procedure Get_Background_Area
     (Tree_View : access Gtk_Tree_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Rect      : out Gdk.Rectangle.Gdk_Rectangle)
   is
      procedure Internal
        (Tree_View : System.Address;
         Path      : Gtk.Tree_Model.Gtk_Tree_Path;
         Column    : System.Address;
         Rect      : out Gdk.Rectangle.Gdk_Rectangle);
      pragma Import (C, Internal, "gtk_tree_view_get_background_area");

      Col_Addr : System.Address;

   begin
      if Column = null then
         Col_Addr := System.Null_Address;
      else
         Col_Addr := Get_Object (Column);
      end if;

      Internal (Get_Object (Tree_View), Path, Col_Addr, Rect);
   end Get_Background_Area;

   ----------------------
   -- Get_Visible_Rect --
   ----------------------

   procedure Get_Visible_Rect
     (Tree_View    : access Gtk_Tree_View_Record;
      Visible_Rect : out Gdk.Rectangle.Gdk_Rectangle)
   is
      procedure Internal
        (Tree_View    : System.Address;
         Visible_Rect : out Gdk.Rectangle.Gdk_Rectangle);
      pragma Import (C, Internal, "gtk_tree_view_get_visible_rect");
   begin
      Internal (Get_Object (Tree_View), Visible_Rect);
   end Get_Visible_Rect;

   ---------------------------
   -- Widget_To_Tree_Coords --
   ---------------------------

   procedure Widget_To_Tree_Coords
     (Tree_View : access Gtk_Tree_View_Record;
      Wx        : Gint;
      Wy        : Gint;
      Tx        : out Gint;
      Ty        : out Gint)
   is
      procedure Internal
        (Tree_View : System.Address;
         Wx        : Gint;
         Wy        : Gint;
         Tx        : out Gint;
         Ty        : out Gint);
      pragma Import (C, Internal, "gtk_tree_view_widget_to_tree_coords");
   begin
      Internal (Get_Object (Tree_View), Wx, Wy, Tx, Ty);
   end Widget_To_Tree_Coords;

   ---------------------------
   -- Tree_To_Widget_Coords --
   ---------------------------

   procedure Tree_To_Widget_Coords
     (Tree_View : access Gtk_Tree_View_Record;
      Tx        : Gint;
      Ty        : Gint;
      Wx        : out Gint;
      Wy        : out Gint)
   is
      procedure Internal
        (Tree_View : System.Address;
         Tx        : Gint;
         Ty        : Gint;
         Wx        : out Gint;
         Wy        : out Gint);
      pragma Import (C, Internal, "gtk_tree_view_tree_to_widget_coords");
   begin
      Internal (Get_Object (Tree_View), Tx, Ty, Wx, Wy);
   end Tree_To_Widget_Coords;

   ---------------------------------------
   -- Convert_Bin_Window_To_Tree_Coords --
   ---------------------------------------

   procedure Convert_Bin_Window_To_Tree_Coords
     (Tree_View : access Gtk_Tree_View_Record;
      Bx        : Gint;
      By        : Gint;
      Tx        : out Gint;
      Ty        : out Gint)
   is
      procedure Internal
        (Tree_View : System.Address;
         Bx        : Gint;
         By        : Gint;
         Tx        : out Gint;
         Ty        : out Gint);
      pragma Import
        (C, Internal, "gtk_tree_view_convert_bin_window_to_tree_coords");
   begin
      Internal (Get_Object (Tree_View), Bx, By, Tx, Ty);
   end Convert_Bin_Window_To_Tree_Coords;

   -----------------------------------------
   -- Convert_Bin_Window_To_Widget_Coords --
   -----------------------------------------

   procedure Convert_Bin_Window_To_Widget_Coords
     (Tree_View : access Gtk_Tree_View_Record;
      Bx        : Gint;
      By        : Gint;
      Wx        : out Gint;
      Wy        : out Gint)
   is
      procedure Internal
        (Tree_View : System.Address;
         Bx        : Gint;
         By        : Gint;
         Wx        : out Gint;
         Wy        : out Gint);
      pragma Import
        (C, Internal, "gtk_tree_view_convert_bin_window_to_widget_coords");
   begin
      Internal (Get_Object (Tree_View), Bx, By, Wx, Wy);
   end Convert_Bin_Window_To_Widget_Coords;

   ---------------------------------------
   -- Convert_Tree_To_Bin_Window_Coords --
   ---------------------------------------

   procedure Convert_Tree_To_Bin_Window_Coords
     (Tree_View : access Gtk_Tree_View_Record;
      Tx        : Gint;
      Ty        : Gint;
      Bx        : out Gint;
      By        : out Gint)
   is
      procedure Internal
        (Tree_View : System.Address;
         Tx        : Gint;
         Ty        : Gint;
         Bx        : out Gint;
         By        : out Gint);
      pragma Import
        (C, Internal, "gtk_tree_view_convert_tree_to_bin_window_coords");
   begin
      Internal (Get_Object (Tree_View), Tx, Ty, Bx, By);
   end Convert_Tree_To_Bin_Window_Coords;

   -----------------------------------
   -- Convert_Tree_To_Widget_Coords --
   -----------------------------------

   procedure Convert_Tree_To_Widget_Coords
     (Tree_View : access Gtk_Tree_View_Record;
      Tx        : Gint;
      Ty        : Gint;
      Wx        : out Gint;
      Wy        : out Gint)
   is
      procedure Internal
        (Tree_View : System.Address;
         Tx        : Gint;
         Ty        : Gint;
         Wx        : out Gint;
         Wy        : out Gint);
      pragma Import
        (C, Internal, "gtk_tree_view_convert_tree_to_widget_coords");
   begin
      Internal (Get_Object (Tree_View), Tx, Ty, Wx, Wy);
   end Convert_Tree_To_Widget_Coords;

   -----------------------------------------
   -- Convert_Widget_To_Bin_Window_Coords --
   -----------------------------------------

   procedure Convert_Widget_To_Bin_Window_Coords
     (Tree_View : access Gtk_Tree_View_Record;
      Wx        : Gint;
      Wy        : Gint;
      Bx        : out Gint;
      By        : out Gint)
   is
      procedure Internal
        (Tree_View : System.Address;
         Wx        : Gint;
         Wy        : Gint;
         Bx        : out Gint;
         By        : out Gint);
      pragma Import
        (C, Internal, "gtk_tree_view_convert_widget_to_bin_window_coords");
   begin
      Internal (Get_Object (Tree_View), Wx, Wy, Bx, By);
   end Convert_Widget_To_Bin_Window_Coords;

   -----------------------------------
   -- Convert_Widget_To_Tree_Coords --
   -----------------------------------

   procedure Convert_Widget_To_Tree_Coords
     (Tree_View : access Gtk_Tree_View_Record;
      Wx        : Gint;
      Wy        : Gint;
      Tx        : out Gint;
      Ty        : out Gint)
   is
      procedure Internal
        (Tree_View : System.Address;
         Wx        : Gint;
         Wy        : Gint;
         Tx        : out Gint;
         Ty        : out Gint);
      pragma Import
        (C, Internal, "gtk_tree_view_convert_widget_to_tree_coords");
   begin
      Internal (Get_Object (Tree_View), Wx, Wy, Tx, Ty);
   end Convert_Widget_To_Tree_Coords;

   ----------------------------
   -- Unset_Rows_Drag_Source --
   ----------------------------

   procedure Unset_Rows_Drag_Source
     (Tree_View : access Gtk_Tree_View_Record)
   is
      procedure Internal (Tree_View : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_unset_rows_drag_source");
   begin
      Internal (Get_Object (Tree_View));
   end Unset_Rows_Drag_Source;

   --------------------------
   -- Unset_Rows_Drag_Dest --
   --------------------------

   procedure Unset_Rows_Drag_Dest (Tree_View : access Gtk_Tree_View_Record) is
      procedure Internal (Tree_View : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_unset_rows_drag_dest");
   begin
      Internal (Get_Object (Tree_View));
   end Unset_Rows_Drag_Dest;

   --------------------------
   -- Create_Row_Drag_Icon --
   --------------------------

   function Create_Row_Drag_Icon
     (Tree_View : access Gtk_Tree_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gdk.Pixmap.Gdk_Pixmap
   is
      function Internal
        (Tree_View : System.Address;
         Path      : Gtk.Tree_Model.Gtk_Tree_Path)
         return Gdk.Pixmap.Gdk_Pixmap;
      pragma Import (C, Internal, "gtk_tree_view_create_row_drag_icon");

   begin
      return Internal (Get_Object (Tree_View), Path);
   end Create_Row_Drag_Icon;

   -----------------------
   -- Set_Enable_Search --
   -----------------------

   procedure Set_Enable_Search
     (Tree_View     : access Gtk_Tree_View_Record;
      Enable_Search : Boolean)
   is
      procedure Internal
        (Tree_View     : System.Address;
         Enable_Search : Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_set_enable_search");

   begin
      Internal (Get_Object (Tree_View), Boolean'Pos (Enable_Search));
   end Set_Enable_Search;

   -----------------------
   -- Get_Enable_Search --
   -----------------------

   function Get_Enable_Search
     (Tree_View : access Gtk_Tree_View_Record) return Boolean
   is
      function Internal (Tree_View : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_get_enable_search");
   begin
      return Internal (Get_Object (Tree_View)) /= 0;
   end Get_Enable_Search;

   -----------------------
   -- Get_Search_Column --
   -----------------------

   function Get_Search_Column
     (Tree_View : access Gtk_Tree_View_Record) return Gint
   is
      function Internal (Tree_View : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_tree_view_get_search_column");
   begin
      return Internal (Get_Object (Tree_View));
   end Get_Search_Column;

   -----------------------
   -- Set_Search_Column --
   -----------------------

   procedure Set_Search_Column
     (Tree_View : access Gtk_Tree_View_Record;
      Column    : Gint)
   is
      procedure Internal
        (Tree_View : System.Address;
         Column    : Gint);
      pragma Import (C, Internal, "gtk_tree_view_set_search_column");

   begin
      Internal (Get_Object (Tree_View), Column);
   end Set_Search_Column;

   ----------------------
   -- Get_Search_Entry --
   ----------------------

   function Get_Search_Entry
     (Tree_View : access Gtk_Tree_View_Record) return Gtk.GEntry.Gtk_Entry
   is
      function Internal (Tree_View : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_view_get_search_entry");
      Stub : Gtk.GEntry.Gtk_Entry_Record;
   begin
      return Gtk.GEntry.Gtk_Entry
        (Get_User_Data (Internal (Get_Object (Tree_View)), Stub));
   end Get_Search_Entry;

   ----------------------
   -- Set_Search_Entry --
   ----------------------

   procedure Set_Search_Entry
     (Tree_View : access Gtk_Tree_View_Record;
      The_Entry : access Gtk.GEntry.Gtk_Entry_Record'Class)
   is
      procedure Internal (Tree_View, The_Entry : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_set_search_entry");
   begin
      Internal (Get_Object (Tree_View), Get_Object (The_Entry));
   end Set_Search_Entry;

   ------------------------------
   -- Get_Search_Position_Func --
   ------------------------------

   function Get_Search_Position_Func
     (Tree_View : access Gtk_Tree_View_Record)
      return Gtk_Tree_View_Search_Position_Func
   is
      function Internal
        (Tree_View : System.Address) return Gtk_Tree_View_Search_Position_Func;
      pragma Import (C, Internal, "gtk_tree_view_get_search_position_func");
   begin
      return Internal (Get_Object (Tree_View));
   end Get_Search_Position_Func;

   ------------------------------
   -- Set_Search_Position_Func --
   ------------------------------

   procedure Set_Search_Position_Func
     (Tree_View : access Gtk_Tree_View_Record;
      Func      : Gtk_Tree_View_Search_Position_Func;
      Data      : System.Address;
      Destroy   : G_Destroy_Notify)
   is
      procedure Internal
        (Tree_View : System.Address;
         Func      : Gtk_Tree_View_Search_Position_Func;
         Data      : System.Address;
         Destroy   : G_Destroy_Notify);
      pragma Import (C, Internal, "gtk_tree_view_set_search_position_func");
   begin
      Internal (Get_Object (Tree_View), Func, Data, Destroy);
   end Set_Search_Position_Func;

   ----------------------
   -- Set_Tooltip_Cell --
   ----------------------

   procedure Set_Tooltip_Cell
     (Tree_View : access Gtk_Tree_View_Record;
      Tooltip   : access Gtk.Tooltip.Gtk_Tooltip_Record'Class;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      Column    : access
                  Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
      Cell      : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
   is
      procedure Internal
        (Tree_View : System.Address;
         Tooltip   : System.Address;
         Path      : Gtk.Tree_Model.Gtk_Tree_Path;
         Column    : System.Address;
         Cell      : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_set_tooltip_cell");
   begin
      Internal
        (Get_Object (Tree_View),
         Get_Object (Tooltip),
         Path,
         Get_Object (Column),
         Get_Object (Cell));
   end Set_Tooltip_Cell;

   ------------------------
   -- Get_Tooltip_Column --
   ------------------------

   function Get_Tooltip_Column
     (Tree_View : access Gtk_Tree_View_Record) return Gint
   is
      function Internal (Tree_View : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_tree_view_get_tooltip_column");

   begin
      return Internal (Get_Object (Tree_View));
   end Get_Tooltip_Column;

   ------------------------
   -- Set_Tooltip_Column --
   ------------------------

   procedure Set_Tooltip_Column
     (Tree_View : access Gtk_Tree_View_Record;
      Column    : Gint)
   is
      procedure Internal
        (Tree_View : System.Address;
         Column    : Gint);
      pragma Import (C, Internal, "gtk_tree_view_set_tooltip_column");

   begin
      Internal (Get_Object (Tree_View), Column);
   end Set_Tooltip_Column;

   -------------------------
   -- Get_Tooltip_Context --
   -------------------------

   procedure Get_Tooltip_Context
     (Tree_View     : access Gtk_Tree_View_Record;
      X             : in out Glib.Gint;
      Y             : in out Glib.Gint;
      Keyboard_Mode : Boolean;
      Model         : out Gtk_Tree_Model;
      Path          : out Gtk_Tree_Path;
      Iter          : out Gtk_Tree_Iter;
      Success       : out Boolean)
   is
      function Internal
        (Tree_View     : System.Address;
         X             : access Glib.Gint;
         Y             : access Glib.Gint;
         Keyboard_Mode : Glib.Gboolean;
         Model         : access System.Address;
         Path          : access Gtk_Tree_Path;
         Iter          : access Gtk_Tree_Iter)
         return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_get_tooltip_context");

      C_X     : aliased Glib.Gint := X;
      C_Y     : aliased Glib.Gint := Y;
      C_Model : aliased System.Address;
      C_Path  : aliased Gtk_Tree_Path;
      C_Iter  : aliased Gtk_Tree_Iter;
      C_Ret   : Glib.Gboolean;

      Stub    : Gtk.Tree_Model.Gtk_Tree_Model_Record;

   begin
      C_Ret :=
        Internal
          (Get_Object (Tree_View),
           C_X'Access,
           C_Y'Access,
           Glib.Gboolean (Glib.To_Gint (Keyboard_Mode)),
           C_Model'Access,
           C_Path'Access,
           C_Iter'Access);

      if C_Ret = 0 then
         Success := False;

      else
         Success := True;
         X := C_X;
         Y := C_Y;
         Model :=
           Gtk.Tree_Model.Gtk_Tree_Model (Get_User_Data_Fast (C_Model, Stub));
         Path := C_Path;
         Iter := C_Iter;
      end if;
   end Get_Tooltip_Context;

   ---------------------
   -- Set_Tooltip_Row --
   ---------------------

   procedure Set_Tooltip_Row
     (Tree_View : access Gtk_Tree_View_Record;
      Tooltip   : access Gtk.Tooltip.Gtk_Tooltip_Record'Class;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      procedure Internal
        (Tree_View : System.Address;
         Tooltip   : System.Address;
         Path      : Gtk_Tree_Path);
      pragma Import (C, Internal, "gtk_tree_view_set_tooltip_row");

   begin
      Internal (Get_Object (Tree_View), Get_Object (Tooltip), Path);
   end Set_Tooltip_Row;

   --------------------
   -- Expand_To_Path --
   --------------------

   procedure Expand_To_Path
     (Tree_View : access Gtk_Tree_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      procedure Internal
        (Tree_View : System.Address;
         Path      : Gtk_Tree_Path);
      pragma Import (C, Internal, "gtk_tree_view_expand_to_path");
   begin
      Internal (Get_Object (Tree_View), Path);
   end Expand_To_Path;

   ---------------------------
   -- Get_Fixed_Height_Mode --
   ---------------------------

   function Get_Fixed_Height_Mode
     (Tree_View : access Gtk_Tree_View_Record) return Boolean
   is
      function Internal (Tree_View : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_get_fixed_height_mode");
   begin
      return Boolean'Val (Internal (Get_Object (Tree_View)));
   end Get_Fixed_Height_Mode;

   ----------------------
   -- Get_Hover_Expand --
   ----------------------

   function Get_Hover_Expand
     (Tree_View : access Gtk_Tree_View_Record) return Boolean
   is
      function Internal (Tree_View : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_get_hover_expand");
   begin
      return Boolean'Val (Internal (Get_Object (Tree_View)));
   end Get_Hover_Expand;

   -------------------------
   -- Get_Hover_Selection --
   -------------------------

   function Get_Hover_Selection
     (Tree_View : access Gtk_Tree_View_Record) return Boolean
   is
      function Internal (Tree_View : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_get_hover_selection");
   begin
      return Boolean'Val (Internal (Get_Object (Tree_View)));
   end Get_Hover_Selection;

   ---------------------------
   -- Set_Fixed_Height_Mode --
   ---------------------------

   procedure Set_Fixed_Height_Mode
     (Tree_View : access Gtk_Tree_View_Record;
      Enable    : Boolean)
   is
      procedure Internal (Tree_View : System.Address; Enable : Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_set_fixed_height_mode");
   begin
      Internal (Get_Object (Tree_View), Boolean'Pos (Enable));
   end Set_Fixed_Height_Mode;

   ----------------------
   -- Set_Hover_Expand --
   ----------------------

   procedure Set_Hover_Expand
     (Tree_View : access Gtk_Tree_View_Record;
      Expand    : Boolean)
   is
      procedure Internal (Tree_View : System.Address; Expand : Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_set_hover_expand");
   begin
      Internal (Get_Object (Tree_View), Boolean'Pos (Expand));
   end Set_Hover_Expand;

   -------------------------
   -- Set_Hover_Selection --
   -------------------------

   procedure Set_Hover_Selection
     (Tree_View : access Gtk_Tree_View_Record;
      Hover     : Boolean)
   is
      procedure Internal (Tree_View : System.Address; Hover : Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_set_hover_selection");
   begin
      Internal (Get_Object (Tree_View), Boolean'Pos (Hover));
   end Set_Hover_Selection;

   ----------------------------
   -- Enable_Model_Drag_Dest --
   ----------------------------

   procedure Enable_Model_Drag_Dest
     (Tree_View : access Gtk_Tree_View_Record;
      Targets   : Gtk.Selection.Target_Entry_Array;
      Actions   : Gdk.Dnd.Drag_Action)
   is
      procedure Internal
        (Tree_View : System.Address;
         Targets   : System.Address;
         N_Targets : Gint;
         Actions   : Gdk.Dnd.Drag_Action);
      pragma Import (C, Internal, "gtk_tree_view_enable_model_drag_dest");
   begin
      Internal (Get_Object (Tree_View), Targets (Targets'First)'Address,
                Targets'Length, Actions);
   end Enable_Model_Drag_Dest;

   ------------------------------
   -- Enable_Model_Drag_Source --
   ------------------------------

   procedure Enable_Model_Drag_Source
     (Tree_View         : access Gtk_Tree_View_Record;
      Start_Button_Mask : Gdk_Modifier_Type;
      Targets           : Gtk.Selection.Target_Entry_Array;
      Actions           : Gdk.Dnd.Drag_Action)
   is
      procedure Internal
        (Tree_View         : System.Address;
         Start_Button_Mask : Gdk_Modifier_Type;
         Targets           : System.Address;
         N_Targets         : Gint;
         Actions           : Drag_Action);
      pragma Import (C, Internal, "gtk_tree_view_enable_model_drag_source");
   begin
      Internal (Get_Object (Tree_View), Start_Button_Mask,
                Targets (Targets'First)'Address, Targets'Length, Actions);
   end Enable_Model_Drag_Source;

   -------------------------
   -- Get_Dest_Row_At_Pos --
   -------------------------

   procedure Get_Dest_Row_At_Pos
     (Tree_View : access Gtk_Tree_View_Record;
      Drag_X    : Gint;
      Drag_Y    : Gint;
      Path      : out Gtk_Tree_Path;
      Pos       : out Gtk_Tree_View_Drop_Position;
      Success   : out Boolean)
   is
      function Internal
        (Tree_View : System.Address;
         Drag_X    : Gint;
         Drag_Y    : Gint;
         Path      : access Gtk_Tree_Path;
         Pos       : access Gtk_Tree_View_Drop_Position)
         return Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_get_dest_row_at_pos");
      Pa : aliased Gtk_Tree_Path;
      Po : aliased Gtk_Tree_View_Drop_Position;
   begin
      Success := Boolean'Val
        (Internal (Get_Object (Tree_View), Drag_X, Drag_Y,
                   Pa'Access, Po'Access));
      Path := Pa;
      Pos  := Po;
   end Get_Dest_Row_At_Pos;

   -----------------------
   -- Get_Drag_Dest_Row --
   -----------------------

   procedure Get_Drag_Dest_Row
     (Tree_View : access Gtk_Tree_View_Record;
      Path      : out Gtk.Tree_Model.Gtk_Tree_Path;
      Pos       : out Gtk_Tree_View_Drop_Position)
   is
      procedure Internal
        (Tree_View : System.Address;
         Path      : access Gtk_Tree_Path;
         Pos       : access Gtk_Tree_View_Drop_Position);
      pragma Import (C, Internal, "gtk_tree_view_get_drag_dest_row");
      Pa : aliased Gtk_Tree_Path;
      Po : aliased Gtk_Tree_View_Drop_Position;
   begin
      Internal (Get_Object (Tree_View), Pa'Access, Po'Access);
      Path := Pa;
      Pos  := Po;
   end Get_Drag_Dest_Row;

   ----------------------------
   -- Set_Row_Separator_Func --
   ----------------------------

   procedure Set_Row_Separator_Func
     (Tree_View : access Gtk_Tree_View_Record;
      Func      : Gtk_Tree_View_Row_Separator_Func;
      Data      : System.Address;
      Destroy   : G_Destroy_Notify_Address := null)
   is
      procedure Internal
        (Tree_View : System.Address;
         Func      : Gtk_Tree_View_Row_Separator_Func;
         Data      : System.Address;
         Destroy   : G_Destroy_Notify_Address);
      pragma Import (C, Internal, "gtk_tree_view_set_row_separator_func");
   begin
      Internal (Get_Object (Tree_View), Func, Data, Destroy);
   end Set_Row_Separator_Func;

   ----------------------------
   -- Get_Row_Separator_Func --
   ----------------------------

   function Get_Row_Separator_Func
     (Tree_View : access Gtk_Tree_View_Record)
      return Gtk_Tree_View_Row_Separator_Func
   is
      function Internal
        (Tree_View : System.Address) return Gtk_Tree_View_Row_Separator_Func;
      pragma Import (C, Internal, "gtk_tree_view_get_row_separator_func");
   begin
      return Internal (Get_Object (Tree_View));
   end Get_Row_Separator_Func;

   -----------------------
   -- Set_Drag_Dest_Row --
   -----------------------

   procedure Set_Drag_Dest_Row
     (Tree_View : access Gtk_Tree_View_Record;
      Path      : Gtk_Tree_Path;
      Pos       : Gtk_Tree_View_Drop_Position)
   is
      procedure Internal
        (Tree_View : System.Address;
         Path      : Gtk_Tree_Path;
         Pos       : Gtk_Tree_View_Drop_Position);
      pragma Import (C, Internal, "gtk_tree_view_set_drag_dest_row");
   begin
      Internal (Get_Object (Tree_View), Path, Pos);
   end Set_Drag_Dest_Row;

   ---------------------------
   -- Get_Search_Equal_Func --
   ---------------------------

   function Get_Search_Equal_Func
     (Tree_View : access Gtk_Tree_View_Record)
      return Gtk_Tree_View_Search_Equal_Func
   is
      function Internal
        (Tree_View : System.Address) return Gtk_Tree_View_Search_Equal_Func;
      pragma Import (C, Internal, "gtk_tree_view_get_search_equal_func");
   begin
      return Internal (Get_Object (Tree_View));
   end Get_Search_Equal_Func;

      ---------------------------
   -- Set_Search_Equal_Func --
   ---------------------------

   procedure Set_Search_Equal_Func
     (Tree_View         : access Gtk_Tree_View_Record;
      Search_Equal_Func : Gtk_Tree_View_Search_Equal_Func;
      Search_User_Data  : System.Address;
      Search_Destroy    : G_Destroy_Notify_Address := null)
   is
      procedure Internal
        (Tree_View         : System.Address;
         Search_Equal_Func : Gtk_Tree_View_Search_Equal_Func;
         Search_User_Data  : System.Address;
         Search_Destroy    : G_Destroy_Notify_Address);
      pragma Import (C, Internal, "gtk_tree_view_set_search_equal_func");
   begin
      Internal (Get_Object (Tree_View), Search_Equal_Func,
                Search_User_Data, Search_Destroy);
   end Set_Search_Equal_Func;

   ------------------------------
   -- Set_Column_Drag_Function --
   ------------------------------

   procedure Set_Column_Drag_Function
     (Tree_View : access Gtk_Tree_View_Record;
      Func      : Gtk_Tree_View_Column_Drop_Func;
      User_Data : System.Address;
      Destroy   : G_Destroy_Notify_Address)
   is
      procedure Internal
        (Tree_View : System.Address;
         Func      : Gtk_Tree_View_Column_Drop_Func;
         User_Data : System.Address;
         Destroy   : G_Destroy_Notify_Address);
      pragma Import (C, Internal, "gtk_tree_view_set_column_drag_function");
   begin
      Internal (Get_Object (Tree_View), Func, User_Data, Destroy);
   end Set_Column_Drag_Function;

   -----------------------
   -- Get_Visible_Range --
   -----------------------

   procedure Get_Visible_Range
     (Tree_View : access Gtk_Tree_View_Record;
      Start_Path, End_Path : out Gtk_Tree_Path;
      Success : out Boolean)
   is
      function Internal
        (Tree_View      : System.Address;
         Start_P, End_P : access Gtk_Tree_Path) return Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_get_visible_range");
      S, E : aliased Gtk_Tree_Path;
   begin
      Success := Boolean'Val
        (Internal (Get_Object (Tree_View), S'Access, E'Access));
      Start_Path := S;
      End_Path   := E;
   end Get_Visible_Range;

   -----------------------------
   -- Internal_Cell_Data_Func --
   -----------------------------

   procedure Internal_Cell_Data_Func
     (Tree_Column, Cell, Model, Iter : System.Address; Data : Cell_Data_Func)
   is
      M_Stub : Gtk.Tree_Model.Gtk_Tree_Model_Record;
      C_Stub : Gtk_Cell_Renderer_Record;
      T_Stub : Gtk_Tree_View_Column_Record;

      T : constant Gtk_Tree_View_Column :=
        Gtk_Tree_View_Column (Get_User_Data_Fast (Tree_Column, T_Stub));
      C : constant Gtk_Cell_Renderer :=
        Gtk_Cell_Renderer (Get_User_Data_Fast (Cell, C_Stub));
      M : constant Gtk_Tree_Model :=
        Gtk_Tree_Model (Get_User_Data_Fast (Model, M_Stub));
      I : constant Gtk_Tree_Iter_Access := To_Iter (Iter);
   begin
      Data (T, C, M, I.all);
   end Internal_Cell_Data_Func;

   ----------------------------------
   -- Insert_Column_With_Data_Func --
   ----------------------------------

   function Insert_Column_With_Data_Func
     (Tree_View : access Gtk_Tree_View_Record;
      Position  : Gint;
      Title     : String;
      Cell      : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Func      : Gtk.Tree_View_Column.Cell_Data_Func)
      return Gint
   is
      function Internal
        (Tree_View : System.Address;
         Position  : Gint;
         Title     : String;
         Cell      : System.Address;
         Func      : System.Address;
         Data      : System.Address;
         Dnotify   : G_Destroy_Notify)
         return Gint;
      pragma Import
        (C, Internal, "gtk_tree_view_insert_column_with_data_func");
   begin
      if Func = null then
         return Internal
           (Get_Object (Tree_View),
            Position, Title & ASCII.NUL,
            Get_Object (Cell),
            System.Null_Address, System.Null_Address, null);
      else
         return Internal
           (Get_Object (Tree_View),
            Position, Title & ASCII.NUL,
            Get_Object (Cell),
            Internal_Cell_Data_Func'Address,
            Func.all'Address, null);
      end if;
   end Insert_Column_With_Data_Func;

   -----------------------
   -- Map_Expanded_Rows --
   -----------------------

   procedure Map_Expanded_Rows
     (Tree_View : access Gtk_Tree_View_Record;
      Func      : Gtk_Tree_View_Mapping_Func;
      Data      : System.Address)
   is
      procedure Internal
        (Tree_View : System.Address;
         Func      : Gtk_Tree_View_Mapping_Func;
         Data      : System.Address);
      pragma Import (C, Internal, "gtk_tree_view_map_expanded_rows");
   begin
      Internal (Get_Object (Tree_View), Func, Data);
   end Map_Expanded_Rows;

   ------------------------
   -- Set_Cursor_On_Cell --
   ------------------------

   procedure Set_Cursor_On_Cell
     (Tree_View     : access Gtk_Tree_View_Record;
      Path          : Gtk.Tree_Model.Gtk_Tree_Path;
      Focus_Column  : Gtk.Tree_View_Column.Gtk_Tree_View_Column := null;
      Focus_Cell    : Gtk.Cell_Renderer.Gtk_Cell_Renderer := null;
      Start_Editing : Boolean)
   is
      procedure Internal
        (Tree_View     : System.Address;
         Path          : Gtk_Tree_Path;
         Focus_Column  : System.Address;
         Focus_Cell    : System.Address;
         Start_Editing : Gboolean);
      pragma Import (C, Internal, "gtk_tree_view_set_cursor_on_cell");
   begin
      Internal
        (Get_Object (Tree_View), Path,
         Get_Object (Focus_Column), Get_Object (Focus_Cell),
         Boolean'Pos (Start_Editing));
   end Set_Cursor_On_Cell;

end Gtk.Tree_View;
