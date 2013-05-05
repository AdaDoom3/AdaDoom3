-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                 Copyright (C) 2006-2013, AdaCore                  --
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

with Gdk.Dnd;             use Gdk.Dnd;
with Gdk.Types;           use Gdk.Types;
with Glib.Properties;     use Glib.Properties;
with Gtk.Cell_Renderer;   use Gtk.Cell_Renderer;
with Gtk.Container;       use Gtk.Container;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Selection;       use Gtk.Selection;
with Gtk.Tree_Model;      use Gtk.Tree_Model;
with System;              use System;

with Glib.Type_Conversion_Hooks;

package body Gtk.Icon_View is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Icon_View_Record);
   pragma Warnings (Off, Type_Conversion);

   -----------------------------------------
   -- Convert_Widget_To_Bin_Window_Coords --
   -----------------------------------------

   procedure Convert_Widget_To_Bin_Window_Coords
     (Icon_View : access Gtk_Icon_View_Record;
      Wx        : Gint;
      Wy        : Gint;
      Bx        : out Gint;
      By        : out Gint)
   is
      procedure Internal
        (Icon_View : System.Address;
         Wx        : Gint;
         Wy        : Gint;
         Bx        : access Gint;
         By        : access Gint);
      pragma Import
        (C, Internal, "gtk_icon_view_convert_widget_to_bin_window_coords");

      Local_Bx : aliased Gint;
      Local_By : aliased Gint;
   begin
      Internal
        (Get_Object (Icon_View), Wx, Wy, Local_Bx'Access, Local_By'Access);
      Bx := Local_Bx;
      By := Local_By;
   end Convert_Widget_To_Bin_Window_Coords;

   ----------------------
   -- Create_Drag_Icon --
   ----------------------

   function Create_Drag_Icon
     (Icon_View : access Gtk_Icon_View_Record;
      Path      : Gtk_Tree_Path)
      return Gdk_Pixmap
   is
      function Internal
        (Icon_View : System.Address;
         Path      : Gtk_Tree_Path) return Gdk_Pixmap;
      pragma Import (C, Internal, "gtk_icon_view_create_drag_icon");
   begin
      return Internal (Get_Object (Icon_View), Path);
   end Create_Drag_Icon;

   ----------------------------
   -- Enable_Model_Drag_Dest --
   ----------------------------

   procedure Enable_Model_Drag_Dest
     (Icon_View : access Gtk_Icon_View_Record;
      Targets   : Target_Entry_Array;
      Actions   : Drag_Action)
   is
      procedure Internal
        (Icon_View : System.Address;
         Targets   : System.Address;
         N_Targets : Gint;
         Actions   : Drag_Action);
      pragma Import (C, Internal, "gtk_icon_view_enable_model_drag_dest");
   begin
      Internal (Get_Object (Icon_View),
                Targets (Targets'First)'Address, Targets'Length,
                Actions);
   end Enable_Model_Drag_Dest;

   ------------------------------
   -- Enable_Model_Drag_Source --
   ------------------------------

   procedure Enable_Model_Drag_Source
     (Icon_View         : access Gtk_Icon_View_Record;
      Start_Button_Mask : Gdk_Modifier_Type;
      Targets           : Target_Entry_Array;
      Actions           : Drag_Action)
   is
      procedure Internal
        (Icon_View         : System.Address;
         Start_Button_Mask : Gdk_Modifier_Type;
         Targets           : System.Address;
         N_Targets         : Gint;
         Actions           : Drag_Action);
      pragma Import (C, Internal, "gtk_icon_view_enable_model_drag_source");
   begin
      Internal (Get_Object (Icon_View), Start_Button_Mask,
                Targets (Targets'First)'Address, Targets'Length,
                Actions);
   end Enable_Model_Drag_Source;

   ------------------------
   -- Get_Column_Spacing --
   ------------------------

   function Get_Column_Spacing
     (Icon_View : access Gtk_Icon_View_Record) return Gint
   is
      function Internal (Icon_View : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_icon_view_get_column_spacing");
   begin
      return Internal (Get_Object (Icon_View));
   end Get_Column_Spacing;

   -----------------
   -- Get_Columns --
   -----------------

   function Get_Columns
     (Icon_View : access Gtk_Icon_View_Record) return Gint
   is
      function Internal (Icon_View : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_icon_view_get_columns");
   begin
      return Internal (Get_Object (Icon_View));
   end Get_Columns;

   ----------------
   -- Get_Cursor --
   ----------------

   procedure Get_Cursor
     (Icon_View     : access Gtk_Icon_View_Record;
      Path          : out Gtk.Tree_Model.Gtk_Tree_Path;
      Cell          : out Gtk.Cell_Renderer.Gtk_Cell_Renderer;
      Cursor_Is_Set : out Boolean)
   is
      function Internal
        (Icon_View : System.Address;
         Path      : access Gtk_Tree_Path;
         Cell      : access System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_icon_view_get_cursor");
      P : aliased Gtk_Tree_Path;
      C : aliased System.Address;
      Stub : Gtk_Cell_Renderer_Record;
   begin
      Cursor_Is_Set := Boolean'Val
        (Internal (Get_Object (Icon_View),
         P'Unchecked_Access,
         C'Unchecked_Access));
      Path := P;
      Cell := Gtk_Cell_Renderer (Get_User_Data (C, Stub));
   end Get_Cursor;

   --------------------------
   -- Get_Dest_Item_At_Pos --
   --------------------------

   procedure Get_Dest_Item_At_Pos
     (Icon_View : access Gtk_Icon_View_Record;
      Drag_X    : Glib.Gint;
      Drag_Y    : Glib.Gint;
      Path      : out Gtk.Tree_Model.Gtk_Tree_Path;
      Pos       : out Gtk_Icon_View_Drop_Position;
      Has_Item  : out Boolean)
   is
      function Internal
        (Icon_View : System.Address;
         Drag_X    : Gint;
         Drag_Y    : Gint;
         Path      : access Gtk_Tree_Path;
         Pos       : access Gtk_Icon_View_Drop_Position)
         return Gboolean;
      pragma Import (C, Internal, "gtk_icon_view_get_dest_item_at_pos");
      P : aliased Gtk_Tree_Path;
      D : aliased Gtk_Icon_View_Drop_Position;
   begin
      Has_Item := Boolean'Val
        (Internal (Get_Object (Icon_View), Drag_X, Drag_Y,
         P'Unchecked_Access, D'Unchecked_Access));
      Path := P;
      Pos  := D;
   end Get_Dest_Item_At_Pos;

   ------------------------
   -- Get_Drag_Dest_Item --
   ------------------------

   procedure Get_Drag_Dest_Item
     (Icon_View : access Gtk_Icon_View_Record;
      Path      : out Gtk_Tree_Path;
      Pos       : out Gtk_Icon_View_Drop_Position)
   is
      procedure Internal
        (Icon_View : System.Address;
         Path      : out Gtk_Tree_Path;
         Pos       : out Gtk_Icon_View_Drop_Position);
      pragma Import (C, Internal, "gtk_icon_view_get_drag_dest_item");
   begin
      Internal (Get_Object (Icon_View), Path, Pos);
   end Get_Drag_Dest_Item;

   ---------------------
   -- Get_Item_At_Pos --
   ---------------------

   procedure Get_Item_At_Pos
     (Icon_View : access Gtk_Icon_View_Record;
      X         : Gint;
      Y         : Gint;
      Path      : out Gtk_Tree_Path;
      Cell      : out Gtk_Cell_Renderer;
      Has_Item  : out Boolean)
   is
      function Internal
        (Icon_View : System.Address;
         X         : Gint;
         Y         : Gint;
         Path      : access Gtk_Tree_Path;
         Cell      : access System.Address)
         return Gboolean;
      pragma Import (C, Internal, "gtk_icon_view_get_item_at_pos");

      P : aliased Gtk_Tree_Path;
      C : aliased System.Address;
      Stub : Gtk_Cell_Renderer_Record;
   begin
      Has_Item := Boolean'Val
        (Internal (Get_Object (Icon_View), X, Y,
                   P'Unchecked_Access, C'Unchecked_Access));
      Path := P;
      Cell := Gtk_Cell_Renderer (Get_User_Data (C, Stub));
   end Get_Item_At_Pos;

   --------------------
   -- Get_Item_Width --
   --------------------

   function Get_Item_Width
     (Icon_View : access Gtk_Icon_View_Record) return Gint
   is
      function Internal (Icon_View : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_icon_view_get_item_width");
   begin
      return Internal (Get_Object (Icon_View));
   end Get_Item_Width;

   ----------------
   -- Get_Margin --
   ----------------

   function Get_Margin
     (Icon_View : access Gtk_Icon_View_Record) return Gint
   is
      function Internal (Icon_View : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_icon_view_get_margin");
   begin
      return Internal (Get_Object (Icon_View));
   end Get_Margin;

   -----------------------
   -- Get_Markup_Column --
   -----------------------

   function Get_Markup_Column
     (Icon_View : access Gtk_Icon_View_Record) return Gint
   is
      function Internal (Icon_View : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_icon_view_get_markup_column");
   begin
      return Internal (Get_Object (Icon_View));
   end Get_Markup_Column;

   ---------------
   -- Get_Model --
   ---------------

   function Get_Model
     (Icon_View : access Gtk_Icon_View_Record) return Gtk_Tree_Model
   is
      function Internal (Icon_View : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_icon_view_get_model");

      Stub : Gtk_Tree_Model_Record;
      M    : constant System.Address := Internal (Get_Object (Icon_View));
   begin
      if M = System.Null_Address then
         return null;
      else
         return Gtk_Tree_Model (Get_User_Data (M, Stub));
      end if;
   end Get_Model;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
     (Icon_View : access Gtk_Icon_View_Record) return Gtk_Orientation
   is
      function Internal (Icon_View : System.Address) return Gtk_Orientation;
      pragma Import (C, Internal, "gtk_icon_view_get_orientation");
   begin
      return Internal (Get_Object (Icon_View));
   end Get_Orientation;

   ---------------------
   -- Get_Path_At_Pos --
   ---------------------

   function Get_Path_At_Pos
     (Icon_View : access Gtk_Icon_View_Record;
      X         : Gint;
      Y         : Gint)
      return Gtk_Tree_Path
   is
      function Internal
        (Icon_View : System.Address;
         X         : Gint;
         Y         : Gint)
         return Gtk_Tree_Path;
      pragma Import (C, Internal, "gtk_icon_view_get_path_at_pos");
   begin
      return Internal (Get_Object (Icon_View), X, Y);
   end Get_Path_At_Pos;

   -----------------------
   -- Get_Pixbuf_Column --
   -----------------------

   function Get_Pixbuf_Column
     (Icon_View : access Gtk_Icon_View_Record) return Gint
   is
      function Internal (Icon_View : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_icon_view_get_pixbuf_column");
   begin
      return Internal (Get_Object (Icon_View));
   end Get_Pixbuf_Column;

   ---------------------
   -- Get_Reorderable --
   ---------------------

   function Get_Reorderable
     (Icon_View : access Gtk_Icon_View_Record) return Boolean
   is
      function Internal (Icon_View : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_icon_view_get_reorderable");
   begin
      return Boolean'Val (Internal (Get_Object (Icon_View)));
   end Get_Reorderable;

   ---------------------
   -- Get_Row_Spacing --
   ---------------------

   function Get_Row_Spacing
     (Icon_View : access Gtk_Icon_View_Record) return Gint
   is
      function Internal (Icon_View : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_icon_view_get_row_spacing");
   begin
      return Internal (Get_Object (Icon_View));
   end Get_Row_Spacing;

   ------------------------
   -- Get_Selected_Items --
   ------------------------

   function Get_Selected_Items
     (Icon_View : access Gtk_Icon_View_Record)
      return Gtk_Tree_Path_List.Glist
   is
      function Internal (Icon_View : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_icon_view_get_selected_items");
      L : Gtk_Tree_Path_List.Glist;
   begin
      Gtk_Tree_Path_List.Set_Object (L, Internal (Get_Object (Icon_View)));
      return L;
   end Get_Selected_Items;

   ------------------------
   -- Get_Selection_Mode --
   ------------------------

   function Get_Selection_Mode
     (Icon_View : access Gtk_Icon_View_Record)
      return Gtk_Selection_Mode
   is
      function Internal (Icon_View : System.Address) return Gtk_Selection_Mode;
      pragma Import (C, Internal, "gtk_icon_view_get_selection_mode");
   begin
      return Internal (Get_Object (Icon_View));
   end Get_Selection_Mode;

   -----------------
   -- Get_Spacing --
   -----------------

   function Get_Spacing
     (Icon_View : access Gtk_Icon_View_Record) return Gint
   is
      function Internal (Icon_View : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_icon_view_get_spacing");
   begin
      return Internal (Get_Object (Icon_View));
   end Get_Spacing;

   ---------------------
   -- Get_Text_Column --
   ---------------------

   function Get_Text_Column
     (Icon_View : access Gtk_Icon_View_Record) return Gint
   is
      function Internal (Icon_View : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_icon_view_get_text_column");
   begin
      return Internal (Get_Object (Icon_View));
   end Get_Text_Column;

   ------------------------
   -- Get_Tooltip_Column --
   ------------------------

   function Get_Tooltip_Column
     (Icon_View : access Gtk_Icon_View_Record) return Gint
   is
      function Internal (Icon_View : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_icon_view_get_tooltip_column");
   begin
      return Internal (Get_Object (Icon_View));
   end Get_Tooltip_Column;

   -------------------------
   -- Get_Tooltip_Context --
   -------------------------

   procedure Get_Tooltip_Context
     (Icon_View    : access Gtk_Icon_View_Record;
      X            : in out Gint;
      Y            : in out Gint;
      Keyboard_Tip : Boolean;
      Model        : out Gtk_Tree_Model;
      Path         : out Gtk_Tree_Path;
      Iter         : out Gtk_Tree_Iter;
      Success      : out Boolean)
   is
      function Internal
        (Icon_View    : System.Address;
         X            : access Gint;
         Y            : access Gint;
         Keyboard_Tip : Gboolean;
         Model        : access System.Address;
         Path         : access Gtk_Tree_Path;
         Iter         : access Gtk_Tree_Iter)
         return Gboolean;
      pragma Import (C, Internal, "gtk_icon_view_get_tooltip_context");

      Local_X     : aliased Gint := X;
      Local_Y     : aliased Gint := Y;
      Local_Model : aliased System.Address;
      Local_Path  : aliased Gtk_Tree_Path;
      Local_Iter  : aliased Gtk_Tree_Iter;

      Stub : Gtk_Tree_Model_Record;
   begin
      Success := Boolean'Val (Internal
        (Get_Object (Icon_View),
         Local_X'Access,
         Local_Y'Access,
         Boolean'Pos (Keyboard_Tip),
         Local_Model'Access,
         Local_Path'Access,
         Local_Iter'Access));

      X     := Local_X;
      Y     := Local_Y;
      Model := Gtk_Tree_Model (Get_User_Data (Local_Model, Stub));
      Path  := Local_Path;
      Iter  := Local_Iter;
   end Get_Tooltip_Context;

   -----------------------
   -- Get_Visible_Range --
   -----------------------

   procedure Get_Visible_Range
     (Icon_View : access Gtk_Icon_View_Record;
      Start_Path : out Gtk.Tree_Model.Gtk_Tree_Path;
      End_Path   : out Gtk.Tree_Model.Gtk_Tree_Path)
   is
      function Internal
        (Icon_View : System.Address;
         S, E : access Gtk_Tree_Path) return Gboolean;
      pragma Import (C, Internal, "gtk_icon_view_get_visible_range");
      S, E : aliased Gtk_Tree_Path;
   begin
      if Boolean'Val
        (Internal
           (Get_Object (Icon_View), S'Unchecked_Access, E'Unchecked_Access))
      then
         Start_Path := S;
         End_Path   := E;
      else
         Start_Path := null;
         End_Path   := null;
      end if;
   end Get_Visible_Range;

   --------------------
   -- Item_Activated --
   --------------------

   procedure Item_Activated
     (Icon_View : access Gtk_Icon_View_Record;
      Path      : Gtk_Tree_Path)
   is
      procedure Internal (Icon_View : System.Address; Path : Gtk_Tree_Path);
      pragma Import (C, Internal, "gtk_icon_view_item_activated");
   begin
      Internal (Get_Object (Icon_View), Path);
   end Item_Activated;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Icon_View : out Gtk_Icon_View) is
   begin
      Icon_View := new Gtk_Icon_View_Record;
      Gtk.Icon_View.Initialize (Icon_View);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Icon_View : access Gtk_Icon_View_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_icon_view_new");
   begin
      Set_Object (Icon_View, Internal);
   end Initialize;

   ------------------------
   -- Gtk_New_With_Model --
   ------------------------

   procedure Gtk_New_With_Model
     (Icon_View : out Gtk_Icon_View;
      Model     : access Gtk_Tree_Model_Record'Class)
   is
   begin
      Icon_View := new Gtk_Icon_View_Record;
      Initialize_With_Model (Icon_View, Model);
   end Gtk_New_With_Model;

   ---------------------------
   -- Initialize_With_Model --
   ---------------------------

   procedure Initialize_With_Model
     (Icon_View : access Gtk_Icon_View_Record'Class;
      Model     : access Gtk_Tree_Model_Record'Class)
   is
      function Internal (Model : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_icon_view_new_with_model");
   begin
      Set_Object (Icon_View, Internal (Get_Object (Model)));
   end Initialize_With_Model;

   ----------------------
   -- Path_Is_Selected --
   ----------------------

   function Path_Is_Selected
     (Icon_View : access Gtk_Icon_View_Record;
      Path      : Gtk_Tree_Path)
      return Boolean
   is
      function Internal
        (Icon_View : System.Address;
         Path      : Gtk_Tree_Path)
         return Gboolean;
      pragma Import (C, Internal, "gtk_icon_view_path_is_selected");
   begin
      return Boolean'Val (Internal (Get_Object (Icon_View), Path));
   end Path_Is_Selected;

   --------------------
   -- Scroll_To_Path --
   --------------------

   procedure Scroll_To_Path
     (Icon_View : access Gtk_Icon_View_Record;
      Path      : Gtk_Tree_Path;
      Use_Align : Boolean := True;
      Row_Align : Gfloat := 0.5;
      Col_Align : Gfloat := 0.0)
   is
      procedure Internal
        (Icon_View : System.Address;
         Path      : Gtk_Tree_Path;
         Use_Align : Gboolean;
         Row_Align : Gfloat;
         Col_Align : Gfloat);
      pragma Import (C, Internal, "gtk_icon_view_scroll_to_path");
   begin
      Internal (Get_Object (Icon_View), Path,
                Boolean'Pos (Use_Align), Row_Align, Col_Align);
   end Scroll_To_Path;

   ----------------
   -- Select_All --
   ----------------

   procedure Select_All (Icon_View : access Gtk_Icon_View_Record) is
      procedure Internal (Icon_View : System.Address);
      pragma Import (C, Internal, "gtk_icon_view_select_all");
   begin
      Internal (Get_Object (Icon_View));
   end Select_All;

   -----------------
   -- Select_Path --
   -----------------

   procedure Select_Path
     (Icon_View : access Gtk_Icon_View_Record;
      Path      : Gtk_Tree_Path)
   is
      procedure Internal
        (Icon_View : System.Address;
         Path      : Gtk_Tree_Path);
      pragma Import (C, Internal, "gtk_icon_view_select_path");
   begin
      Internal (Get_Object (Icon_View), Path);
   end Select_Path;

   ------------------------
   -- Set_Column_Spacing --
   ------------------------

   procedure Set_Column_Spacing
     (Icon_View      : access Gtk_Icon_View_Record;
      Column_Spacing : Gint)
   is
      procedure Internal
        (Icon_View : System.Address; Column_Spacing : Gint);
      pragma Import (C, Internal, "gtk_icon_view_set_column_spacing");
   begin
      Internal (Get_Object (Icon_View), Column_Spacing);
   end Set_Column_Spacing;

   -----------------
   -- Set_Columns --
   -----------------

   procedure Set_Columns
     (Icon_View : access Gtk_Icon_View_Record;
      Columns   : Gint)
   is
      procedure Internal (Icon_View : System.Address; Columns : Gint);
      pragma Import (C, Internal, "gtk_icon_view_set_columns");
   begin
      Internal (Get_Object (Icon_View), Columns);
   end Set_Columns;

   ----------------
   -- Set_Cursor --
   ----------------

   procedure Set_Cursor
     (Icon_View     : access Gtk_Icon_View_Record;
      Path          : Gtk_Tree_Path;
      Cell          : Gtk_Cell_Renderer := null;
      Start_Editing : Boolean := False)
   is
      procedure Internal
        (Icon_View     : System.Address;
         Path          : Gtk_Tree_Path;
         Cell          : System.Address;
         Start_Editing : Gboolean);
      pragma Import (C, Internal, "gtk_icon_view_set_cursor");
      C : System.Address := System.Null_Address;
   begin
      if Cell /= null then
         C := Get_Object (Cell);
      end if;
      Internal (Get_Object (Icon_View), Path, C, Boolean'Pos (Start_Editing));
   end Set_Cursor;

   ------------------------
   -- Set_Drag_Dest_Item --
   ------------------------

   procedure Set_Drag_Dest_Item
     (Icon_View : access Gtk_Icon_View_Record;
      Path      : Gtk_Tree_Path;
      Pos       : Gtk_Icon_View_Drop_Position)
   is
      procedure Internal
        (Icon_View : System.Address;
         Path      : Gtk_Tree_Path;
         Pos       : Gtk_Icon_View_Drop_Position);
      pragma Import (C, Internal, "gtk_icon_view_set_drag_dest_item");
   begin
      Internal (Get_Object (Icon_View), Path, Pos);
   end Set_Drag_Dest_Item;

   --------------------
   -- Set_Item_Width --
   --------------------

   procedure Set_Item_Width
     (Icon_View  : access Gtk_Icon_View_Record; Item_Width : Gint)
   is
      procedure Internal (Icon_View  : System.Address; Item_Width : Gint);
      pragma Import (C, Internal, "gtk_icon_view_set_item_width");
   begin
      Internal (Get_Object (Icon_View), Item_Width);
   end Set_Item_Width;

   ----------------
   -- Set_Margin --
   ----------------

   procedure Set_Margin
     (Icon_View : access Gtk_Icon_View_Record;
      Margin    : Gint)
   is
      procedure Internal (Icon_View : System.Address; Margin : Gint);
      pragma Import (C, Internal, "gtk_icon_view_set_margin");
   begin
      Internal (Get_Object (Icon_View), Margin);
   end Set_Margin;

   -----------------------
   -- Set_Markup_Column --
   -----------------------

   procedure Set_Markup_Column
     (Icon_View : access Gtk_Icon_View_Record;
      Column    : Gint)
   is
      procedure Internal (Icon_View : System.Address; Column : Gint);
      pragma Import (C, Internal, "gtk_icon_view_set_markup_column");
   begin
      Internal (Get_Object (Icon_View), Column);
   end Set_Markup_Column;

   ---------------
   -- Set_Model --
   ---------------

   procedure Set_Model
     (Icon_View : access Gtk_Icon_View_Record;
      Model     : Gtk_Tree_Model := null)
   is
      procedure Internal
        (Icon_View : System.Address;
         Model     : System.Address);
      pragma Import (C, Internal, "gtk_icon_view_set_model");
   begin
      if Model = null then
         Internal (Get_Object (Icon_View), System.Null_Address);
      else
         Internal (Get_Object (Icon_View), Get_Object (Model));
      end if;
   end Set_Model;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
     (Icon_View   : access Gtk_Icon_View_Record;
      Orientation : Gtk_Orientation)
   is
      procedure Internal
        (Icon_View   : System.Address; Orientation : Gtk_Orientation);
      pragma Import (C, Internal, "gtk_icon_view_set_orientation");
   begin
      Internal (Get_Object (Icon_View), Orientation);
   end Set_Orientation;

   -----------------------
   -- Set_Pixbuf_Column --
   -----------------------

   procedure Set_Pixbuf_Column
     (Icon_View : access Gtk_Icon_View_Record;
      Column    : Gint)
   is
      procedure Internal (Icon_View : System.Address; Column : Gint);
      pragma Import (C, Internal, "gtk_icon_view_set_pixbuf_column");
   begin
      Internal (Get_Object (Icon_View), Column);
   end Set_Pixbuf_Column;

   ---------------------
   -- Set_Reorderable --
   ---------------------

   procedure Set_Reorderable
     (Icon_View   : access Gtk_Icon_View_Record;
      Reorderable : Boolean)
   is
      procedure Internal (Icon_View : System.Address; Reorderable : Gboolean);
      pragma Import (C, Internal, "gtk_icon_view_set_reorderable");
   begin
      Internal (Get_Object (Icon_View), Boolean'Pos (Reorderable));
   end Set_Reorderable;

   ---------------------
   -- Set_Row_Spacing --
   ---------------------

   procedure Set_Row_Spacing
     (Icon_View   : access Gtk_Icon_View_Record;
      Row_Spacing : Gint)
   is
      procedure Internal (Icon_View : System.Address; Row_Spacing : Gint);
      pragma Import (C, Internal, "gtk_icon_view_set_row_spacing");
   begin
      Internal (Get_Object (Icon_View), Row_Spacing);
   end Set_Row_Spacing;

   ------------------------
   -- Set_Selection_Mode --
   ------------------------

   procedure Set_Selection_Mode
     (Icon_View : access Gtk_Icon_View_Record;
      Mode      : Gtk_Selection_Mode)
   is
      procedure Internal
        (Icon_View : System.Address; Mode : Gtk_Selection_Mode);
      pragma Import (C, Internal, "gtk_icon_view_set_selection_mode");
   begin
      Internal (Get_Object (Icon_View), Mode);
   end Set_Selection_Mode;

   -----------------
   -- Set_Spacing --
   -----------------

   procedure Set_Spacing
     (Icon_View : access Gtk_Icon_View_Record;
      Spacing   : Gint)
   is
      procedure Internal (Icon_View : System.Address; Spacing : Gint);
      pragma Import (C, Internal, "gtk_icon_view_set_spacing");
   begin
      Internal (Get_Object (Icon_View), Spacing);
   end Set_Spacing;

   ---------------------
   -- Set_Text_Column --
   ---------------------

   procedure Set_Text_Column
     (Icon_View : access Gtk_Icon_View_Record;
      Column    : Gint)
   is
      procedure Internal (Icon_View : System.Address; Column : Gint);
      pragma Import (C, Internal, "gtk_icon_view_set_text_column");
   begin
      Internal (Get_Object (Icon_View), Column);
   end Set_Text_Column;

   ----------------------
   -- Set_Tooltip_Cell --
   ----------------------

   procedure Set_Tooltip_Cell
     (Icon_View : access Gtk_Icon_View_Record;
      Tooltip   : access Gtk.Tooltip.Gtk_Tooltip_Record'Class;
      Path      : Gtk_Tree_Path;
      Cell      : access Gtk_Cell_Renderer_Record'Class)
   is
      procedure Internal
        (Icon_View : System.Address;
         Tooltip   : System.Address;
         Path      : Gtk_Tree_Path;
         Cell      : System.Address);
      pragma Import (C, Internal, "gtk_icon_view_set_tooltip_cell");
   begin
      Internal
        (Get_Object (Icon_View),
         Get_Object (Tooltip),
         Path,
         Get_Object (Cell));
   end Set_Tooltip_Cell;

   ------------------------
   -- Set_Tooltip_Column --
   ------------------------

   procedure Set_Tooltip_Column
     (Icon_View : access Gtk_Icon_View_Record;
      Column    : Gint)
   is
      procedure Internal
        (Icon_View : System.Address;
         Column    : Gint);
      pragma Import (C, Internal, "gtk_icon_view_set_tooltip_column");
   begin
      Internal (Get_Object (Icon_View), Column);
   end Set_Tooltip_Column;

   ----------------------
   -- Set_Tooltip_Item --
   ----------------------

   procedure Set_Tooltip_Item
     (Icon_View : access Gtk_Icon_View_Record;
      Tooltip   : access Gtk.Tooltip.Gtk_Tooltip_Record'Class;
      Path      : Gtk_Tree_Path)
   is
      procedure Internal
        (Icon_View : System.Address;
         Tooltip   : System.Address;
         Path      : Gtk_Tree_Path);
      pragma Import (C, Internal, "gtk_icon_view_set_tooltip_item");
   begin
      Internal
        (Get_Object (Icon_View),
         Get_Object (Tooltip),
         Path);
   end Set_Tooltip_Item;

   ------------------
   -- Unselect_All --
   ------------------

   procedure Unselect_All
     (Icon_View : access Gtk_Icon_View_Record)
   is
      procedure Internal (Icon_View : System.Address);
      pragma Import (C, Internal, "gtk_icon_view_unselect_all");
   begin
      Internal (Get_Object (Icon_View));
   end Unselect_All;

   -------------------
   -- Unselect_Path --
   -------------------

   procedure Unselect_Path
     (Icon_View : access Gtk_Icon_View_Record;
      Path      : Gtk_Tree_Path)
   is
      procedure Internal (Icon_View : System.Address; Path : Gtk_Tree_Path);
      pragma Import (C, Internal, "gtk_icon_view_unselect_path");
   begin
      Internal (Get_Object (Icon_View), Path);
   end Unselect_Path;

   ---------------------------
   -- Unset_Model_Drag_Dest --
   ---------------------------

   procedure Unset_Model_Drag_Dest
     (Icon_View : access Gtk_Icon_View_Record)
   is
      procedure Internal (Icon_View : System.Address);
      pragma Import (C, Internal, "gtk_icon_view_unset_model_drag_dest");
   begin
      Internal (Get_Object (Icon_View));
   end Unset_Model_Drag_Dest;

   -----------------------------
   -- Unset_Model_Drag_Source --
   -----------------------------

   procedure Unset_Model_Drag_Source
     (Icon_View : access Gtk_Icon_View_Record)
   is
      procedure Internal (Icon_View : System.Address);
      pragma Import (C, Internal, "gtk_icon_view_unset_model_drag_source");
   begin
      Internal (Get_Object (Icon_View));
   end Unset_Model_Drag_Source;

end Gtk.Icon_View;
