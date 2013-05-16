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

with Gtk.Enums;           use Gtk.Enums;
with Gtk.Text_Attributes; use Gtk.Text_Attributes;
with Pango.Tabs;          use Pango.Tabs;
with System;

with Glib.Type_Conversion_Hooks;

package body Gtk.Text_View is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Text_View_Record);
   pragma Warnings (Off, Type_Conversion);

   --  See Note (2) in the body of package Gtk.Text_Iter for more details
   --  on why it is safe to apply the 'Address attribute on a Gtk_Text_Iter
   --  parameter.

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget : out Gtk_Text_View;
      Buffer : Gtk.Text_Buffer.Gtk_Text_Buffer := null) is
   begin
      Widget := new Gtk_Text_View_Record;
      Initialize (Widget, Buffer);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget : access Gtk_Text_View_Record'Class;
      Buffer : Gtk.Text_Buffer.Gtk_Text_Buffer)
   is
      function Internal (Buffer : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_text_view_new_with_buffer");

      use type Gtk.Text_Buffer.Gtk_Text_Buffer;

   begin
      if Buffer = null then
         Set_Object (Widget, Internal (System.Null_Address));
      else
         Set_Object (Widget, Internal (Get_Object (Buffer)));
      end if;
   end Initialize;

   ----------------
   -- Set_Buffer --
   ----------------

   procedure Set_Buffer
     (Text_View : access Gtk_Text_View_Record;
      Buffer    : access Gtk.Text_Buffer.Gtk_Text_Buffer_Record'Class)
   is
      procedure Internal (Text_View : System.Address; Buffer : System.Address);
      pragma Import (C, Internal, "gtk_text_view_set_buffer");

   begin
      Internal (Get_Object (Text_View), Get_Object (Buffer));
   end Set_Buffer;

   ----------------
   -- Get_Buffer --
   ----------------

   function Get_Buffer
     (Text_View : access Gtk_Text_View_Record)
      return Gtk.Text_Buffer.Gtk_Text_Buffer
   is
      function Internal (Text_View : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_text_view_get_buffer");

      Stub : Gtk.Text_Buffer.Gtk_Text_Buffer_Record;

   begin
      return Gtk.Text_Buffer.Gtk_Text_Buffer
               (Get_User_Data_Fast (Internal (Get_Object (Text_View)), Stub));
   end Get_Buffer;

   --------------------
   -- Scroll_To_Iter --
   --------------------

   function Scroll_To_Iter
     (Text_View     : access Gtk_Text_View_Record;
      Iter          : Gtk.Text_Iter.Gtk_Text_Iter;
      Within_Margin : Gdouble;
      Use_Align     : Boolean;
      Xalign        : Gdouble;
      Yalign        : Gdouble) return Boolean
   is
      function Internal
        (Text_View     : System.Address;
         Iter          : Gtk.Text_Iter.Gtk_Text_Iter;
         Within_Margin : Gdouble;
         Use_Align     : Gboolean;
         Xalign        : Gdouble;
         Yalign        : Gdouble) return Gboolean;
      pragma Import (C, Internal, "gtk_text_view_scroll_to_iter");

   begin
      return
        Internal
          (Get_Object (Text_View),
           Iter,
           Within_Margin,
           Boolean'Pos (Use_Align),
           Xalign,
           Yalign) /= 0;
   end Scroll_To_Iter;

   --------------------
   -- Scroll_To_Mark --
   --------------------

   procedure Scroll_To_Mark
     (Text_View     : access Gtk_Text_View_Record;
      Mark          : access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class;
      Within_Margin : Gdouble := 0.0;
      Use_Align     : Boolean := False;
      Xalign        : Gdouble := 0.0;
      Yalign        : Gdouble := 0.0)
   is
      procedure Internal
        (Text_View     : System.Address;
         Mark          : System.Address;
         Within_Margin : Gdouble;
         Use_Align     : Gboolean;
         Xalign        : Gdouble;
         Yalign        : Gdouble);
      pragma Import (C, Internal, "gtk_text_view_scroll_to_mark");

   begin
      Internal (Get_Object (Text_View),
                Get_Object (Mark),
                Within_Margin,
                Boolean'Pos (Use_Align),
                Xalign,
                Yalign);
   end Scroll_To_Mark;

   --------------------------
   -- Scroll_Mark_Onscreen --
   --------------------------

   procedure Scroll_Mark_Onscreen
     (Text_View : access Gtk_Text_View_Record;
      Mark      : access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class)
   is
      procedure Internal (Text_View : System.Address; Mark : System.Address);
      pragma Import (C, Internal, "gtk_text_view_scroll_mark_onscreen");

   begin
      Internal (Get_Object (Text_View), Get_Object (Mark));
   end Scroll_Mark_Onscreen;

   ------------------------
   -- Move_Mark_Onscreen --
   ------------------------

   function Move_Mark_Onscreen
     (Text_View : access Gtk_Text_View_Record;
      Mark      : access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class)
      return Boolean
   is
      function Internal
        (Text_View : System.Address;
         Mark      : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_text_view_move_mark_onscreen");

   begin
      return Internal (Get_Object (Text_View), Get_Object (Mark)) /= 0;
   end Move_Mark_Onscreen;

   ---------------------------
   -- Place_Cursor_Onscreen --
   ---------------------------

   function Place_Cursor_Onscreen
     (Text_View : access Gtk_Text_View_Record) return Boolean
   is
      function Internal (Text_View : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_text_view_place_cursor_onscreen");

   begin
      return Internal (Get_Object (Text_View)) /= 0;
   end Place_Cursor_Onscreen;

   ----------------------
   -- Get_Visible_Rect --
   ----------------------

   procedure Get_Visible_Rect
     (Text_View    : access Gtk_Text_View_Record;
      Visible_Rect : out Gdk.Rectangle.Gdk_Rectangle)
   is
      procedure Internal
        (Text_View    : System.Address;
         Visible_Rect : out Gdk.Rectangle.Gdk_Rectangle);
      pragma Import (C, Internal, "gtk_text_view_get_visible_rect");

   begin
      Internal (Get_Object (Text_View), Visible_Rect);
   end Get_Visible_Rect;

   ------------------------
   -- Set_Cursor_Visible --
   ------------------------

   procedure Set_Cursor_Visible
     (Text_View : access Gtk_Text_View_Record;
      Setting   : Boolean := True)
   is
      procedure Internal (Text_View : System.Address; Setting : Gboolean);
      pragma Import (C, Internal, "gtk_text_view_set_cursor_visible");

   begin
      Internal (Get_Object (Text_View), Boolean'Pos (Setting));
   end Set_Cursor_Visible;

   ------------------------
   -- Get_Cursor_Visible --
   ------------------------

   function Get_Cursor_Visible
     (Text_View : access Gtk_Text_View_Record) return Boolean
   is
      function Internal (Text_View : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_text_view_get_cursor_visible");

   begin
      return Internal (Get_Object (Text_View)) /= 0;
   end Get_Cursor_Visible;

   -----------------------
   -- Get_Iter_Location --
   -----------------------

   procedure Get_Iter_Location
     (Text_View : access Gtk_Text_View_Record;
      Iter      : Gtk.Text_Iter.Gtk_Text_Iter;
      Location  : out Gdk.Rectangle.Gdk_Rectangle)
   is
      procedure Internal
        (Text_View : System.Address;
         Iter      : Gtk.Text_Iter.Gtk_Text_Iter;
         Location  : out Gdk.Rectangle.Gdk_Rectangle);
      pragma Import (C, Internal, "gtk_text_view_get_iter_location");

   begin
      Internal (Get_Object (Text_View), Iter, Location);
   end Get_Iter_Location;

   --------------------------
   -- Get_Iter_At_Location --
   --------------------------

   procedure Get_Iter_At_Location
     (Text_View : access Gtk_Text_View_Record;
      Iter      : out Gtk.Text_Iter.Gtk_Text_Iter;
      X         : Gint;
      Y         : Gint)
   is
      procedure Internal
        (Text_View : System.Address;
         Iter      : System.Address;
         X         : Gint;
         Y         : Gint);
      pragma Import (C, Internal, "gtk_text_view_get_iter_at_location");

   begin
      Internal (Get_Object (Text_View), Iter'Address, X, Y);
   end Get_Iter_At_Location;

   --------------------------
   -- Get_Iter_At_Position --
   --------------------------

   procedure Get_Iter_At_Position
     (Text_View : access Gtk_Text_View_Record;
      Iter      : out Gtk.Text_Iter.Gtk_Text_Iter;
      Trailing  : out Gint;
      X         : Gint;
      Y         : Gint)
   is
      procedure Internal
        (Text_View : System.Address;
         Iter      : System.Address;
         Trailing  : out Gint;
         X         : Gint;
         Y         : Gint);
      pragma Import (C, Internal, "gtk_text_view_get_iter_at_position");
   begin
      Internal (Get_Object (Text_View), Iter'Address, Trailing, X, Y);
   end Get_Iter_At_Position;

   ---------------------
   -- Get_Line_Yrange --
   ---------------------

   procedure Get_Line_Yrange
     (Text_View : access Gtk_Text_View_Record;
      Iter      : Gtk.Text_Iter.Gtk_Text_Iter;
      Y         : out Gint;
      Height    : out Gint)
   is
      procedure Internal
        (Text_View : System.Address;
         Iter      : Gtk.Text_Iter.Gtk_Text_Iter;
         Y         : out Gint;
         Height    : out Gint);
      pragma Import (C, Internal, "gtk_text_view_get_line_yrange");

   begin
      Internal (Get_Object (Text_View), Iter, Y, Height);
   end Get_Line_Yrange;

   -------------------
   -- Get_Line_At_Y --
   -------------------

   procedure Get_Line_At_Y
     (Text_View   : access Gtk_Text_View_Record;
      Target_Iter : out Gtk.Text_Iter.Gtk_Text_Iter;
      Y           : Gint;
      Line_Top    : out Gint)
   is
      procedure Internal
        (Text_View   : System.Address;
         Target_Iter : System.Address;
         Y           : Gint;
         Line_Top    : out Gint);
      pragma Import (C, Internal, "gtk_text_view_get_line_at_y");
   begin
      Internal (Get_Object (Text_View), Target_Iter'Address, Y, Line_Top);
   end Get_Line_At_Y;

   -----------------------------
   -- Buffer_To_Window_Coords --
   -----------------------------

   procedure Buffer_To_Window_Coords
     (Text_View : access Gtk_Text_View_Record;
      Win       : Gtk.Enums.Gtk_Text_Window_Type;
      Buffer_X  : Gint;
      Buffer_Y  : Gint;
      Window_X  : out Gint;
      Window_Y  : out Gint)
   is
      procedure Internal
        (Text_View : System.Address;
         Win       : Gtk.Enums.Gtk_Text_Window_Type;
         Buffer_X  : Gint;
         Buffer_Y  : Gint;
         Window_X  : out Gint;
         Window_Y  : out Gint);
      pragma Import (C, Internal, "gtk_text_view_buffer_to_window_coords");

   begin
      Internal
        (Get_Object (Text_View), Win, Buffer_X, Buffer_Y, Window_X, Window_Y);
   end Buffer_To_Window_Coords;

   -----------------------------
   -- Window_To_Buffer_Coords --
   -----------------------------

   procedure Window_To_Buffer_Coords
     (Text_View : access Gtk_Text_View_Record;
      Win       : Gtk.Enums.Gtk_Text_Window_Type;
      Window_X  : Gint;
      Window_Y  : Gint;
      Buffer_X  : out Gint;
      Buffer_Y  : out Gint)
   is
      procedure Internal
        (Text_View : System.Address;
         Win       : Gtk.Enums.Gtk_Text_Window_Type;
         Window_X  : Gint;
         Window_Y  : Gint;
         Buffer_X  : out Gint;
         Buffer_Y  : out Gint);
      pragma Import (C, Internal, "gtk_text_view_window_to_buffer_coords");

   begin
      Internal
        (Get_Object (Text_View), Win, Window_X, Window_Y, Buffer_X, Buffer_Y);
   end Window_To_Buffer_Coords;

   ----------------
   -- Get_Window --
   ----------------

   function Get_Window
     (Text_View : access Gtk_Text_View_Record;
      Win       : Gtk.Enums.Gtk_Text_Window_Type) return Gdk.Window.Gdk_Window
   is
      function Internal
        (Text_View : System.Address;
         Win       : Gtk.Enums.Gtk_Text_Window_Type)
         return Gdk.Window.Gdk_Window;
      pragma Import (C, Internal, "gtk_text_view_get_window");

   begin
      return Internal (Get_Object (Text_View), Win);
   end Get_Window;

   ---------------------
   -- Get_Window_Type --
   ---------------------

   function Get_Window_Type
     (Text_View : access Gtk_Text_View_Record;
      Window    : Gdk.Window.Gdk_Window) return Gtk.Enums.Gtk_Text_Window_Type
   is
      function Internal
        (Text_View : System.Address;
         Window    : Gdk.Window.Gdk_Window) return Gint;
      pragma Import (C, Internal, "gtk_text_view_get_window_type");

   begin
      return Gtk.Enums.Gtk_Text_Window_Type'Val
               (Internal (Get_Object (Text_View), Window));
   end Get_Window_Type;

   ----------------------------
   -- Set_Border_Window_Size --
   ----------------------------

   procedure Set_Border_Window_Size
     (Text_View : access Gtk_Text_View_Record;
      The_Type  : Gtk.Enums.Gtk_Text_Window_Type;
      Size      : Gint)
   is
      procedure Internal
        (Text_View : System.Address;
         The_Type  : Gtk.Enums.Gtk_Text_Window_Type;
         Size      : Gint);
      pragma Import (C, Internal, "gtk_text_view_set_border_window_size");

   begin
      Internal (Get_Object (Text_View), The_Type, Size);
   end Set_Border_Window_Size;

   ---------------------------------
   -- Set_Disable_Scroll_On_Focus --
   ---------------------------------

   procedure Set_Disable_Scroll_On_Focus
     (Text_View : access Gtk_Text_View_Record;
      Set       : Boolean)
   is
      procedure Internal
        (Text_View : System.Address;
         Set       : Gboolean);
      pragma Import (C, Internal, "ada_text_view_set_disable_scroll_on_focus");

   begin
      Internal (Get_Object (Text_View), Boolean'Pos (Set));
   end Set_Disable_Scroll_On_Focus;

   ---------------------------------
   -- Get_Disable_Scroll_On_Focus --
   ---------------------------------

   function Get_Disable_Scroll_On_Focus
     (Text_View : access Gtk_Text_View_Record) return Boolean
   is
      function Internal (Text_View : System.Address) return Gboolean;
      pragma Import (C, Internal, "ada_text_view_get_disable_scroll_on_focus");
   begin
      return Internal (Get_Object (Text_View)) /= 0;
   end Get_Disable_Scroll_On_Focus;

   --------------------------
   -- Forward_Display_Line --
   --------------------------

   procedure Forward_Display_Line
     (Text_View : access Gtk_Text_View_Record;
      Iter      : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Result    : out Boolean)
   is
      function Internal
        (Text_View : System.Address;
         Iter      : Gtk.Text_Iter.Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_view_forward_display_line");

   begin
      Result := Internal (Get_Object (Text_View), Iter) /= 0;
   end Forward_Display_Line;

   ---------------------------
   -- Backward_Display_Line --
   ---------------------------

   procedure Backward_Display_Line
     (Text_View : access Gtk_Text_View_Record;
      Iter      : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Result    : out Boolean)
   is
      function Internal
        (Text_View : System.Address;
         Iter      : Gtk.Text_Iter.Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_view_backward_display_line");

   begin
      Result := Internal (Get_Object (Text_View), Iter) /= 0;
   end Backward_Display_Line;

   ------------------------------
   -- Forward_Display_Line_End --
   ------------------------------

   procedure Forward_Display_Line_End
     (Text_View : access Gtk_Text_View_Record;
      Iter      : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Result    : out Boolean)
   is
      function Internal
        (Text_View : System.Address;
         Iter      : Gtk.Text_Iter.Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_view_forward_display_line_end");

   begin
      Result := Internal (Get_Object (Text_View), Iter) /= 0;
   end Forward_Display_Line_End;

   ---------------------------------
   -- Backward_Display_Line_Start --
   ---------------------------------

   procedure Backward_Display_Line_Start
     (Text_View : access Gtk_Text_View_Record;
      Iter      : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Result    : out Boolean)
   is
      function Internal
        (Text_View : System.Address;
         Iter      : Gtk.Text_Iter.Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_view_backward_display_line_start");

   begin
      Result := Internal (Get_Object (Text_View), Iter) /= 0;
   end Backward_Display_Line_Start;

   -------------------------
   -- Starts_Display_Line --
   -------------------------

   function Starts_Display_Line
     (Text_View : access Gtk_Text_View_Record;
      Iter      : Gtk.Text_Iter.Gtk_Text_Iter) return Boolean
   is
      function Internal
        (Text_View : System.Address;
         Iter      : Gtk.Text_Iter.Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_view_starts_display_line");

   begin
      return Internal (Get_Object (Text_View), Iter) /= 0;
   end Starts_Display_Line;

   -------------------
   -- Move_Visually --
   -------------------

   procedure Move_Visually
     (Text_View : access Gtk_Text_View_Record;
      Iter      : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Count     : Gint;
      Result    : out Boolean)
   is
      function Internal
        (Text_View : System.Address;
         Iter      : Gtk.Text_Iter.Gtk_Text_Iter;
         Count     : Gint) return Gboolean;
      pragma Import (C, Internal, "gtk_text_view_move_visually");

   begin
      Result := Internal (Get_Object (Text_View), Iter, Count) /= 0;
   end Move_Visually;

   -------------------------
   -- Add_Child_At_Anchor --
   -------------------------

   procedure Add_Child_At_Anchor
     (Text_View : access Gtk_Text_View_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Anchor    : access Gtk.Text_Child.Gtk_Text_Child_Anchor_Record'Class)
   is
      procedure Internal
        (Text_View : System.Address;
         Child     : System.Address;
         Anchor    : System.Address);
      pragma Import (C, Internal, "gtk_text_view_add_child_at_anchor");

   begin
      Internal (Get_Object (Text_View),
                Get_Object (Child),
                Get_Object (Anchor));
   end Add_Child_At_Anchor;

   -------------------------
   -- Add_Child_In_Window --
   -------------------------

   procedure Add_Child_In_Window
     (Text_View    : access Gtk_Text_View_Record;
      Child        : access Gtk.Widget.Gtk_Widget_Record'Class;
      Which_Window : Gtk.Enums.Gtk_Text_Window_Type;
      Xpos         : Gint;
      Ypos         : Gint)
   is
      procedure Internal
        (Text_View    : System.Address;
         Child        : System.Address;
         Which_Window : Gtk.Enums.Gtk_Text_Window_Type;
         Xpos         : Gint;
         Ypos         : Gint);
      pragma Import (C, Internal, "gtk_text_view_add_child_in_window");

   begin
      Internal
        (Get_Object (Text_View), Get_Object (Child), Which_Window, Xpos, Ypos);
   end Add_Child_In_Window;

   ----------------
   -- Move_Child --
   ----------------

   procedure Move_Child
     (Text_View : access Gtk_Text_View_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Xpos      : Gint;
      Ypos      : Gint)
   is
      procedure Internal
        (Text_View : System.Address;
         Child     : System.Address;
         Xpos      : Gint;
         Ypos      : Gint);
      pragma Import (C, Internal, "gtk_text_view_move_child");

   begin
      Internal (Get_Object (Text_View), Get_Object (Child), Xpos, Ypos);
   end Move_Child;

   -------------------
   -- Set_Wrap_Mode --
   -------------------

   procedure Set_Wrap_Mode
     (Text_View : access Gtk_Text_View_Record;
      Wrap_Mode : Gtk.Enums.Gtk_Wrap_Mode)
   is
      procedure Internal
        (Text_View : System.Address;
         Wrap_Mode : Gtk.Enums.Gtk_Wrap_Mode);
      pragma Import (C, Internal, "gtk_text_view_set_wrap_mode");

   begin
      Internal (Get_Object (Text_View), Wrap_Mode);
   end Set_Wrap_Mode;

   -------------------
   -- Get_Wrap_Mode --
   -------------------

   function Get_Wrap_Mode
     (Text_View : access Gtk_Text_View_Record)
      return Gtk.Enums.Gtk_Wrap_Mode
   is
      function Internal (Text_View : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_text_view_get_wrap_mode");

   begin
      return Gtk.Enums.Gtk_Wrap_Mode'Val (Internal (Get_Object (Text_View)));
   end Get_Wrap_Mode;

   ------------------
   -- Set_Editable --
   ------------------

   procedure Set_Editable
     (Text_View : access Gtk_Text_View_Record;
      Setting   : Boolean := True)
   is
      procedure Internal (Text_View : System.Address; Setting : Gboolean);
      pragma Import (C, Internal, "gtk_text_view_set_editable");

   begin
      Internal (Get_Object (Text_View), Boolean'Pos (Setting));
   end Set_Editable;

   ------------------
   -- Get_Editable --
   ------------------

   function Get_Editable
     (Text_View : access Gtk_Text_View_Record) return Boolean
   is
      function Internal (Text_View : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_text_view_get_editable");

   begin
      return Internal (Get_Object (Text_View)) /= 0;
   end Get_Editable;

   ----------------------------
   -- Set_Pixels_Above_Lines --
   ----------------------------

   procedure Set_Pixels_Above_Lines
     (Text_View          : access Gtk_Text_View_Record;
      Pixels_Above_Lines : Gint)
   is
      procedure Internal
        (Text_View : System.Address; Pixels_Above_Lines : Gint);
      pragma Import (C, Internal, "gtk_text_view_set_pixels_above_lines");

   begin
      Internal (Get_Object (Text_View), Pixels_Above_Lines);
   end Set_Pixels_Above_Lines;

   ----------------------------
   -- Get_Pixels_Above_Lines --
   ----------------------------

   function Get_Pixels_Above_Lines
     (Text_View : access Gtk_Text_View_Record) return Gint
   is
      function Internal (Text_View : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_text_view_get_pixels_above_lines");
   begin
      return Internal (Get_Object (Text_View));
   end Get_Pixels_Above_Lines;

   ----------------------------
   -- Set_Pixels_Below_Lines --
   ----------------------------

   procedure Set_Pixels_Below_Lines
     (Text_View          : access Gtk_Text_View_Record;
      Pixels_Below_Lines : Gint)
   is
      procedure Internal
        (Text_View : System.Address; Pixels_Below_Lines : Gint);
      pragma Import (C, Internal, "gtk_text_view_set_pixels_below_lines");
   begin
      Internal (Get_Object (Text_View), Pixels_Below_Lines);
   end Set_Pixels_Below_Lines;

   ----------------------------
   -- Get_Pixels_Below_Lines --
   ----------------------------

   function Get_Pixels_Below_Lines
     (Text_View : access Gtk_Text_View_Record) return Gint
   is
      function Internal (Text_View : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_text_view_get_pixels_below_lines");

   begin
      return Internal (Get_Object (Text_View));
   end Get_Pixels_Below_Lines;

   ----------------------------
   -- Set_Pixels_Inside_Wrap --
   ----------------------------

   procedure Set_Pixels_Inside_Wrap
     (Text_View          : access Gtk_Text_View_Record;
      Pixels_Inside_Wrap : Gint)
   is
      procedure Internal
        (Text_View : System.Address; Pixels_Inside_Wrap : Gint);
      pragma Import (C, Internal, "gtk_text_view_set_pixels_inside_wrap");

   begin
      Internal (Get_Object (Text_View), Pixels_Inside_Wrap);
   end Set_Pixels_Inside_Wrap;

   ----------------------------
   -- Get_Pixels_Inside_Wrap --
   ----------------------------

   function Get_Pixels_Inside_Wrap
     (Text_View : access Gtk_Text_View_Record) return Gint
   is
      function Internal (Text_View : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_text_view_get_pixels_inside_wrap");

   begin
      return Internal (Get_Object (Text_View));
   end Get_Pixels_Inside_Wrap;

   -----------------------
   -- Set_Justification --
   -----------------------

   procedure Set_Justification
     (Text_View     : access Gtk_Text_View_Record;
      Justification : Gtk.Enums.Gtk_Justification)
   is
      procedure Internal
        (Text_View     : System.Address;
         Justification : Gtk.Enums.Gtk_Justification);
      pragma Import (C, Internal, "gtk_text_view_set_justification");

   begin
      Internal (Get_Object (Text_View), Justification);
   end Set_Justification;

   -----------------------
   -- Get_Justification --
   -----------------------

   function Get_Justification
     (Text_View : access Gtk_Text_View_Record)
      return Gtk.Enums.Gtk_Justification
   is
      function Internal (Text_View : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_text_view_get_justification");

   begin
      return Gtk.Enums.Gtk_Justification'Val
               (Internal (Get_Object (Text_View)));
   end Get_Justification;

   ---------------------
   -- Set_Left_Margin --
   ---------------------

   procedure Set_Left_Margin
     (Text_View : access Gtk_Text_View_Record; Left_Margin : Gint)
   is
      procedure Internal (Text_View : System.Address; Left_Margin : Gint);
      pragma Import (C, Internal, "gtk_text_view_set_left_margin");

   begin
      Internal (Get_Object (Text_View), Left_Margin);
   end Set_Left_Margin;

   ---------------------
   -- Get_Left_Margin --
   ---------------------

   function Get_Left_Margin
     (Text_View : access Gtk_Text_View_Record) return Gint
   is
      function Internal (Text_View : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_text_view_get_left_margin");

   begin
      return Internal (Get_Object (Text_View));
   end Get_Left_Margin;

   ----------------------
   -- Set_Right_Margin --
   ----------------------

   procedure Set_Right_Margin
     (Text_View : access Gtk_Text_View_Record; Right_Margin : Gint)
   is
      procedure Internal (Text_View : System.Address; Right_Margin : Gint);
      pragma Import (C, Internal, "gtk_text_view_set_right_margin");

   begin
      Internal (Get_Object (Text_View), Right_Margin);
   end Set_Right_Margin;

   ----------------------
   -- Get_Right_Margin --
   ----------------------

   function Get_Right_Margin
     (Text_View : access Gtk_Text_View_Record) return Gint
   is
      function Internal (Text_View : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_text_view_get_right_margin");

   begin
      return Internal (Get_Object (Text_View));
   end Get_Right_Margin;

   ----------------
   -- Set_Indent --
   ----------------

   procedure Set_Indent
     (Text_View : access Gtk_Text_View_Record; Indent : Gint)
   is
      procedure Internal (Text_View : System.Address; Indent : Gint);
      pragma Import (C, Internal, "gtk_text_view_set_indent");

   begin
      Internal (Get_Object (Text_View), Indent);
   end Set_Indent;

   ----------------
   -- Get_Indent --
   ----------------

   function Get_Indent
     (Text_View : access Gtk_Text_View_Record) return Gint
   is
      function Internal (Text_View : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_text_view_get_indent");

   begin
      return Internal (Get_Object (Text_View));
   end Get_Indent;

   --------------
   -- Set_Tabs --
   --------------

   procedure Set_Tabs
     (Text_View : access Gtk_Text_View_Record;
      Tabs      : Pango_Tab_Array)
   is
      procedure Internal (Text_View : System.Address; Tabs : Pango_Tab_Array);
      pragma Import (C, Internal, "gtk_text_view_set_tabs");
   begin
      Internal (Get_Object (Text_View), Tabs);
   end Set_Tabs;

   --------------
   -- Get_Tabs --
   --------------

   function Get_Tabs
     (Text_View : access Gtk_Text_View_Record) return Pango_Tab_Array
   is
      function Internal (Text_View : System.Address) return Pango_Tab_Array;
      pragma Import (C, Internal, "gtk_text_view_get_tabs");
   begin
      return Internal (Get_Object (Text_View));
   end Get_Tabs;

   -------------------
   -- Set_Overwrite --
   -------------------

   procedure Set_Overwrite
     (Text_View : access Gtk_Text_View_Record;
      Overwrite : Boolean)
   is
      procedure Internal
        (Text_View : System.Address;
         Overwrite : Gboolean);
      pragma Import (C, Internal, "gtk_text_view_set_overwrite");
   begin
      Internal (Get_Object (Text_View), Boolean'Pos (Overwrite));
   end Set_Overwrite;

   -------------------
   -- Get_Overwrite --
   -------------------

   function Get_Overwrite
     (Text_View : access Gtk_Text_View_Record)
      return Boolean
   is
      function Internal
        (Text_View : System.Address)
         return Gboolean;
      pragma Import (C, Internal, "gtk_text_view_get_overwrite");
   begin
      return Boolean'Val (Internal (Get_Object (Text_View)));
   end Get_Overwrite;

   ---------------------
   -- Get_Accepts_Tab --
   ---------------------

   function Get_Accepts_Tab
     (Text_View : access Gtk_Text_View_Record)
      return Boolean
   is
      function Internal
        (Text_View : System.Address)
         return Gboolean;
      pragma Import (C, Internal, "gtk_text_view_get_accepts_tab");
   begin
      return Boolean'Val (Internal (Get_Object (Text_View)));
   end Get_Accepts_Tab;

   ---------------------
   -- Set_Accepts_Tab --
   ---------------------

   procedure Set_Accepts_Tab
     (Text_View   : access Gtk_Text_View_Record;
      Accepts_Tab : Boolean)
   is
      procedure Internal
        (Text_View   : System.Address;
         Accepts_Tab : Gboolean);
      pragma Import (C, Internal, "gtk_text_view_set_accepts_tab");
   begin
      Internal (Get_Object (Text_View), Boolean'Pos (Accepts_Tab));
   end Set_Accepts_Tab;

   ----------------------------
   -- Get_Border_Window_Size --
   ----------------------------

   function Get_Border_Window_Size
     (Text_View : access Gtk_Text_View_Record;
      The_Type  : Gtk.Enums.Gtk_Text_Window_Type)
      return Gint
   is
      function Internal
        (Text_View : System.Address;
         The_Type  : Gtk.Enums.Gtk_Text_Window_Type)
         return Gint;
      pragma Import (C, Internal, "gtk_text_view_get_border_window_size");
   begin
      return Internal (Get_Object (Text_View), The_Type);
   end Get_Border_Window_Size;

   ----------------------------
   -- Get_Default_Attributes --
   ----------------------------

   function Get_Default_Attributes
     (Text_View : access Gtk_Text_View_Record)
      return Gtk_Text_Attributes
   is
      function Internal
        (Text_View : System.Address)  return Gtk_Text_Attributes;
      pragma Import (C, Internal, "gtk_text_view_get_default_attributes");
   begin
      return Internal (Get_Object (Text_View));
   end Get_Default_Attributes;

end Gtk.Text_View;
