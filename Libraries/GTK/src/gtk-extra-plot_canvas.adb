-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 2000                            --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--               Copyright (C) 2001-2013, AdaCore                    --
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
with Gdk.Color;           use Gdk.Color;
with Gtk.Extra.Plot_Data; use Gtk.Extra.Plot_Data;
with Glib.Type_Conversion_Hooks;
pragma Elaborate_All (Glib.Type_Conversion_Hooks);

package body Gtk.Extra.Plot_Canvas is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Plot_Canvas_Record);
   pragma Warnings (Off, Type_Conversion);
   --  This package is used to implement a minimal automated type conversion
   --  without having to drag the whole Gtk.Type_Conversion package for the
   --  most common widgets.

   -------------------
   -- Cancel_Action --
   -------------------

   procedure Cancel_Action (Plot_Canvas : access Gtk_Plot_Canvas_Record)
   is
      procedure Internal (Plot_Canvas : System.Address);
      pragma Import (C, Internal, "gtk_plot_canvas_cancel_action");
   begin
      Internal (Get_Object (Plot_Canvas));
   end Cancel_Action;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget        : out Gtk_Plot_Canvas;
      Width         : Gint;
      Height        : Gint;
      Magnification : Gdouble := 1.0)
   is
   begin
      Widget := new Gtk_Plot_Canvas_Record;
      Initialize (Widget, Width, Height, Magnification);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget        : access Gtk_Plot_Canvas_Record'Class;
      Width         : Gint;
      Height        : Gint;
      Magnification : Gdouble := 1.0)
   is
      function Internal (Width         : Gint;
                         Height        : Gint;
                         Magnification : Gdouble)
                        return      System.Address;
      pragma Import (C, Internal, "gtk_plot_canvas_new");
   begin
      Set_Object (Widget, Internal (Width, Height, Magnification));
   end Initialize;

   -----------------------------
   -- Plot_Canvas_Flag_Is_Set --
   -----------------------------

   function Plot_Canvas_Flag_Is_Set
     (Plot_Canvas : access Gtk_Plot_Canvas_Record;
      Flag        : Guint16)
     return Boolean
   is
      function Internal (Canvas : System.Address;
                         Flag   : Guint16)
                        return Guint16;
      pragma Import (C, Internal, "ada_gtk_plot_canvas_flag_is_set");
   begin
      return Internal (Get_Object (Plot_Canvas), Flag) /= 0;
   end Plot_Canvas_Flag_Is_Set;

   ---------------------------
   -- Set_Plot_Canvas_Flags --
   ---------------------------

   procedure Plot_Canvas_Set_Flags
     (Plot_Canvas  : access Gtk_Plot_Canvas_Record;
      Flags        : Guint16)
   is
      procedure Internal (Canvas : System.Address;
                          Flags  : Guint16);
      pragma Import (C, Internal, "ada_gtk_plot_canvas_set_flags");
   begin
      Internal (Get_Object (Plot_Canvas), Flags);
   end Plot_Canvas_Set_Flags;

   -----------------------------
   -- Plot_Canvas_Unset_Flags --
   -----------------------------

   procedure Plot_Canvas_Unset_Flags
     (Plot_Canvas  : access Gtk_Plot_Canvas_Record;
      Flags        : Guint16)
   is
      procedure Internal (Canvas : System.Address;
                          Flags  : Guint16);
      pragma Import (C, Internal, "ada_gtk_plot_canvas_unset_flags");
   begin
      Internal (Get_Object (Plot_Canvas), Flags);
   end Plot_Canvas_Unset_Flags;

   --------------
   -- Set_Size --
   --------------

   procedure Set_Size (Canvas  : access Gtk_Plot_Canvas_Record;
                       Width   : Gint;
                       Height  : Gint)
   is
      procedure Internal (Canvas : System.Address;
                          Width  : Gint;
                          Height : Gint);
      pragma Import (C, Internal, "gtk_plot_canvas_set_size");
   begin
      Internal (Get_Object (Canvas), Width, Height);
   end Set_Size;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (Canvas : access Gtk_Plot_Canvas_Record) is
      procedure Internal (Canvas : System.Address);
      pragma Import (C, Internal, "gtk_plot_canvas_refresh");
   begin
      Internal (Get_Object (Canvas));
   end Refresh;

   --------------
   -- Unselect --
   --------------

   procedure Unselect (Canvas : access Gtk_Plot_Canvas_Record) is
      procedure Internal (Canvas : System.Address);
      pragma Import (C, Internal, "gtk_plot_canvas_unselect");
   begin
      Internal (Get_Object (Canvas));
   end Unselect;

   ---------------------
   -- Get_Active_Item --
   ---------------------

   function Get_Active_Item (Canvas  : access Gtk_Plot_Canvas_Record)
                            return Gtk_Plot_Canvas_Child
   is
      function Internal (Canvas : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_plot_canvas_get_active_item");
      Stub : Gtk_Plot_Canvas_Child_Record;
   begin
      return Gtk_Plot_Canvas_Child
        (Get_User_Data (Internal (Get_Object (Canvas)), Stub));
   end Get_Active_Item;

   ----------------------
   -- Grid_Set_Visible --
   ----------------------

   procedure Grid_Set_Visible (Canvas  : access Gtk_Plot_Canvas_Record;
                               Visible : Boolean)
   is
      procedure Internal (Canvas  : System.Address;
                          Visible : Gint);
      pragma Import (C, Internal, "gtk_plot_canvas_grid_set_visible");
   begin
      Internal (Get_Object (Canvas), Boolean'Pos (Visible));
   end Grid_Set_Visible;

   -------------------
   -- Grid_Set_Step --
   -------------------

   procedure Grid_Set_Step (Canvas : access Gtk_Plot_Canvas_Record;
                            Step   : Gdouble)
   is
      procedure Internal (Canvas : System.Address;
                          Step   : Gdouble);
      pragma Import (C, Internal, "gtk_plot_canvas_grid_set_step");
   begin
      Internal (Get_Object (Canvas), Step);
   end Grid_Set_Step;

   -------------------------
   -- Grid_Set_Attributes --
   -------------------------

   procedure Grid_Set_Attributes
     (Canvas : access Gtk_Plot_Canvas_Record;
      Style  : Gtk.Extra.Plot_Data.Plot_Line_Style;
      Width  : Gint;
      Color  : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Canvas : System.Address;
         Style  : Gtk.Extra.Plot_Data.Plot_Line_Style;
         Width  : Gint;
         Color  : System.Address);
      pragma Import (C, Internal, "gtk_plot_canvas_grid_set_attributes");

      Col : aliased Gdk.Color.Gdk_Color := Color;
      Cola : System.Address := Col'Address;

   begin
      if Color = Gdk.Color.Null_Color then
         Cola := System.Null_Address;
      end if;
      Internal (Get_Object (Canvas), Style, Width, Cola);
   end Grid_Set_Attributes;

   -----------------------
   -- Set_Magnification --
   -----------------------

   procedure Set_Magnification
     (Canvas        : access Gtk_Plot_Canvas_Record;
      Magnification : Gdouble := 1.0)
   is
      procedure Internal (Canvas : System.Address;
                          Magnification : Gdouble);
      pragma Import (C, Internal, "gtk_plot_canvas_set_magnification");
   begin
      Internal (Get_Object (Canvas), Magnification);
   end Set_Magnification;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background
     (Canvas     : access Gtk_Plot_Canvas_Record;
      Background : Gdk.Color.Gdk_Color)
   is
      procedure Internal (Canvas : System.Address; Color  : System.Address);
      pragma Import (C, Internal, "gtk_plot_canvas_set_background");
      Col : aliased Gdk.Color.Gdk_Color := Background;
      Cola : System.Address := Col'Address;
   begin
      if Background = Gdk.Color.Null_Color then
         Cola := System.Null_Address;
      end if;
      Internal (Get_Object (Canvas), Cola);
   end Set_Background;

   ---------------
   -- Get_Pixel --
   ---------------

   procedure Get_Pixel
     (Canvas : access Gtk_Plot_Canvas_Record;
      Px     : Gdouble;
      Py     : Gdouble;
      X      : out Gint;
      Y      : out Gint)
   is
      procedure Internal (Canvas : System.Address;
                          Px     : Gdouble;
                          Py     : Gdouble;
                          X      : out Gint;
                          Y      : out Gint);
      pragma Import (C, Internal, "gtk_plot_canvas_get_pixel");
   begin
      Internal (Get_Object (Canvas), Px, Py, X, Y);
   end Get_Pixel;

   ------------------
   -- Get_Position --
   ------------------

   procedure Get_Position
      (Canvas : access Gtk_Plot_Canvas_Record;
       X      : Gint;
       Y      : Gint;
       Px     : out Gdouble;
       Py     : out Gdouble)
   is
      procedure Internal
         (Canvas : System.Address;
          X      : Gint;
          Y      : Gint;
          Px     : out Gdouble;
          Py     : out Gdouble);
      pragma Import (C, Internal, "gtk_plot_canvas_get_position");
   begin
      Internal (Get_Object (Canvas), X, Y, Px, Py);
   end Get_Position;

   ---------------
   -- Put_Child --
   ---------------

   procedure Put_Child
     (Canvas : access Gtk_Plot_Canvas_Record;
      Child  : access Gtk_Plot_Canvas_Child_Record'Class;
      X1     : Gdouble;
      Y1     : Gdouble;
      X2     : Gdouble := 0.0;
      Y2     : Gdouble := 0.0)
   is
      procedure Internal
        (Canvas : System.Address;
         Child  : System.Address;
         X1, Y1, X2, Y2 : Gdouble);
      pragma Import (C, Internal, "gtk_plot_canvas_put_child");
   begin
      Internal (Get_Object (Canvas), Get_Object (Child), X1, Y1, X2, Y2);
   end Put_Child;

   ----------------
   -- Child_Move --
   ----------------

   procedure Child_Move
     (Canvas : access Gtk_Plot_Canvas_Record;
      Child  : access Gtk_Plot_Canvas_Child_Record'Class;
      X1     : Gdouble;
      Y1     : Gdouble)
   is
      procedure Internal
        (Canvas : System.Address;
         Child  : System.Address;
         X1, Y1 : Gdouble);
      pragma Import (C, Internal, "gtk_plot_canvas_child_move");
   begin
      Internal (Get_Object (Canvas), Get_Object (Child), X1, Y1);
   end Child_Move;

   -----------------------
   -- Child_Move_Resize --
   -----------------------

   procedure Child_Move_Resize
     (Canvas : access Gtk_Plot_Canvas_Record;
      Child  : access Gtk_Plot_Canvas_Child_Record'Class;
      X1     : Gdouble;
      Y1     : Gdouble;
      X2     : Gdouble;
      Y2     : Gdouble)
   is
      procedure Internal
        (Canvas : System.Address;
         Child  : System.Address;
         X1, Y1, X2, Y2 : Gdouble);
      pragma Import (C, Internal, "gtk_plot_canvas_child_move_resize");
   begin
      Internal (Get_Object (Canvas), Get_Object (Child), X1, Y1, X2, Y2);
   end Child_Move_Resize;

   -----------
   -- Paint --
   -----------

   procedure Paint (Canvas : access Gtk_Plot_Canvas_Record) is
      procedure Internal (Canvas : System.Address);
      pragma Import (C, Internal, "gtk_plot_canvas_paint");
   begin
      Internal (Get_Object (Canvas));
   end Paint;

   ------------
   -- Freeze --
   ------------

   procedure Freeze (Canvas : access Gtk_Plot_Canvas_Record) is
      procedure Internal (Canvas : System.Address);
      pragma Import (C, Internal, "gtk_plot_canvas_freeze");
   begin
      Internal (Get_Object (Canvas));
   end Freeze;

   ----------
   -- Thaw --
   ----------

   procedure Thaw (Canvas : access Gtk_Plot_Canvas_Record) is
      procedure Internal (Canvas : System.Address);
      pragma Import (C, Internal, "gtk_plot_canvas_thaw");
   begin
      Internal (Get_Object (Canvas));
   end Thaw;

   ---------------------
   -- Set_Transparent --
   ---------------------

   procedure Set_Transparent
     (Canvas : access Gtk_Plot_Canvas_Record; Transparent : Boolean)
   is
      procedure Internal (Canvas : System.Address; Transparent : Integer);
      pragma Import (C, Internal, "gtk_plot_canvas_set_transparent");
   begin
      Internal (Get_Object (Canvas), Boolean'Pos (Transparent));
   end Set_Transparent;

   -----------------
   -- Transparent --
   -----------------

   function Transparent
     (Canvas : access Gtk_Plot_Canvas_Record) return Boolean
   is
      function Internal (Canvas : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_plot_canvas_transparent");
   begin
      return Boolean'Val (Internal (Get_Object (Canvas)));
   end Transparent;

   ------------------
   -- Remove_Child --
   ------------------

   procedure Remove_Child
     (Canvas : access Gtk_Plot_Canvas_Record;
      Child  : access Gtk_Plot_Canvas_Child_Record'Class)
   is
      procedure Internal (C : System.Address; Child : System.Address);
      pragma Import (C, Internal, "gtk_plot_canvas_remove_child");
   begin
      Internal (Get_Object (Canvas), Get_Object (Child));
   end Remove_Child;

   -------------------
   -- Set_Selection --
   -------------------

   procedure Set_Selection
     (Child     : access Gtk_Plot_Canvas_Child_Record;
      Selection : Plot_Canvas_Selection)
   is
      procedure Internal
        (Child : System.Address; Selection : Plot_Canvas_Selection);
      pragma Import (C, Internal, "gtk_plot_canvas_child_set_selection");
   begin
      Internal (Get_Object (Child), Selection);
   end Set_Selection;

   ------------------------
   -- Set_Selection_Mode --
   ------------------------

   procedure Set_Selection_Mode
     (Child     : access Gtk_Plot_Canvas_Child_Record;
      Mode      : Plot_Canvas_Selection_Mode)
   is
      procedure Internal
        (Child : System.Address; Selection : Plot_Canvas_Selection_Mode);
      pragma Import (C, Internal, "gtk_plot_canvas_child_set_selection_mode");
   begin
      Internal (Get_Object (Child), Mode);
   end Set_Selection_Mode;

   ------------------
   -- Get_Position --
   ------------------

   procedure Get_Position
     (Canvas    : access Gtk_Plot_Canvas_Record;
      Child     : access Gtk_Plot_Canvas_Child_Record'Class;
      X1, Y1    : out Gdouble;
      X2, Y2    : out Gdouble)
   is
      procedure Internal
        (Canvas, Child : System.Address;
         X1, Y1, X2, Y2 : out Gdouble);
      pragma Import (C, Internal, "gtk_plot_canvas_child_get_position");
   begin
      Internal (Get_Object (Canvas), Get_Object (Child), X1, Y1, X2, Y2);
   end Get_Position;

end Gtk.Extra.Plot_Canvas;
