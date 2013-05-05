-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                 Copyright (C) 2011-2013, AdaCore                  --
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

with Ada.Numerics;    use Ada.Numerics;

with Cairo.Pattern;   use Cairo.Pattern;

with Glib;            use Glib;

separate (Gtkada.MDI)
package body Close_Button is

   type Cairo_Color is record
      R, G, B : Gdouble;
   end record;

   function Shade
     (Color : Gdk_Color;
      Value : Gdouble) return Cairo_Color;

   function On_Draw
     (Widget : access Gtk_Widget_Record'Class; Event : Gdk_Event)
      return Boolean;
   --  draws the close button upon expose event

   procedure Rounded_Rectangle
     (Cr         : Cairo_Context;
      X, Y, W, H : Gdouble;
      Radius     : Gdouble);
   --  Draws a rounded rectangle at coordinate X, Y with W and H size.

   procedure Cross
     (Cr            : Cairo_Context;
      W, Size, Thin : Gdouble);
   --  Draws a cross centered on W / 2.0 of current size and thin.

   function On_Tab_Enter
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event_Crossing)
      return Boolean;

   function On_Tab_Leave
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event_Crossing)
      return Boolean;

   function On_Enter
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event_Crossing)
      return Boolean;

   function On_Leave
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event_Crossing)
      return Boolean;

   function On_Mouse_Pressed
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event_Button)
      return Boolean;

   function On_Mouse_Released
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event_Button)
      return Boolean;

   procedure Invalidate (Widget : access Gtk_Widget_Record'Class);
   --  Invalidates the whole widget for queing a redraw

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Button      : out Gtkada_MDI_Close_Button;
      Tab         : access Gtk_Widget_Record'Class;
      Child       : access MDI_Child_Record'Class;
      In_Titlebar : Boolean)
   is
   begin
      Button := new Gtkada_MDI_Close_Button_Record;
      Gtk.Event_Box.Initialize (Button);
      Set_Visible_Window (Button, False);

      Button.Child       := MDI_Child (Child);
      Button.Pressed     := False;
      Button.Over        := False;
      Button.Tab_Over    := False;
      Button.In_Titlebar := In_Titlebar;

      --  In the titlebar, we can go up to 16px as this is the size of the
      --  pixmaps, but we lower this size to 14px to be able to draw the extra
      --  border for the hilight.

      --  In the tab, we keep it small however so that this does not take too
      --  much space.
      if In_Titlebar then
         Button.Default_Size := 14;
      else
         Button.Default_Size := 11;
      end if;

      Set_Size_Request (Button, Button.Default_Size, Button.Default_Size + 4);
      Set_Events
        (Button,
         Get_Events (Button) or Pointer_Motion_Mask or
           Button_Press_Mask or Button_Release_Mask or
           Enter_Notify_Mask or Leave_Notify_Mask);
      Return_Callback.Connect
        (Button, Signal_Expose_Event,
         Return_Callback.To_Marshaller (On_Draw'Access));
      Return_Callback.Connect
        (Button, Signal_Enter_Notify_Event,
         Return_Callback.To_Marshaller (On_Enter'Access));
      Return_Callback.Connect
        (Button, Signal_Leave_Notify_Event,
         Return_Callback.To_Marshaller (On_Leave'Access));
      Return_Callback.Object_Connect
        (Tab, Signal_Enter_Notify_Event,
         Return_Callback.To_Marshaller (On_Tab_Enter'Access),
         Slot_Object => Button);
      Return_Callback.Object_Connect
        (Tab, Signal_Leave_Notify_Event,
         Return_Callback.To_Marshaller (On_Tab_Leave'Access),
         Slot_Object => Button);
      Return_Callback.Connect
        (Button, Signal_Button_Press_Event,
         Return_Callback.To_Marshaller (On_Mouse_Pressed'Access));
      Return_Callback.Connect
        (Button, Signal_Button_Release_Event,
         Return_Callback.To_Marshaller (On_Mouse_Released'Access));
   end Gtk_New;

   -------------
   -- On_Draw --
   -------------

   function On_Draw
     (Widget : access Gtk_Widget_Record'Class; Event : Gdk_Event)
      return Boolean
   is
      pragma Unreferenced (Event);

      Button  : constant Gtkada_MDI_Close_Button :=
                  Gtkada_MDI_Close_Button (Widget);
      Cr      : Cairo_Context;
      Alpha   : Gdouble;
      X, Y    : Gint;
      Width   : Gint;
      Height  : Gint;
      dW      : Gdouble;
      Cross_W : Gdouble;
      Bg      : Gdk_Color;
      Base    : Cairo_Color;
      Lo, Hi  : Cairo_Color;
      Ptrn    : Cairo_Pattern;
      Note    : constant Gtk_Notebook :=
                 Gtk_Notebook (Get_Parent (Button.Child));

   begin
      if not Button.In_Titlebar
        and then not Button.Tab_Over
        and then not Button.Over
      then
         return True;
      end if;

      if Realized_Is_Set (Button) then
         X      := Get_Allocation_X (Button);
         Y      := Get_Allocation_Y (Button);
         Width  := Get_Allocation_Width (Button);
         Height := Get_Allocation_Height (Button);

         dW := Gdouble (Button.Default_Size);

         --  Make sure the button fits in the allocated space
         if dW > Gdouble (Width) then
            dW := Gdouble (Width);
         end if;

         --  Height - 4 : we want at least 1 px margin (so *2) + 1px for the
         --  thin hilight effect at the bottom of the button. We add another px
         --  to center the button (compensate the hilight size).
         if dW > Gdouble (Height - 4) then
            dW := Gdouble (Height - 4);
         end if;

         X := X + Width - Gint (dW);
         Y := Y + (Height - Gint (dW)) / 2;

         Cr := Create (Get_Window (Button));

         Cairo.Set_Line_Width (Cr, 1.0);
         Cairo.Translate (Cr, Gdouble (X), Gdouble (Y));
         Cross_W := dW * 0.7;

         --  Retrieve the parent's actual background color for a nice
         --  transparency effect
         if Button.Child.MDI.Focus_Child = Button.Child then
            Bg := Button.Child.MDI.Focus_Title_Color;
         elsif Button.In_Titlebar
           and then Get_Current_Page (Note) = Page_Num (Note, Button.Child)
         then
            Bg := Button.Child.MDI.Title_Bar_Color;
         else
            Bg := Gtk.Style.Get_Bg (Get_Style (Button.Child), State_Normal);
         end if;

         --  Shade the color according to the button's state
         if Button.Pressed then
            Base := Shade (Bg, 0.5);
            Alpha := 1.0;
         elsif Button.Over then
            Base := Shade (Bg, 0.65);
            Alpha := 1.0;
         else
            Base := Shade (Bg, 0.8);
            Alpha := 0.6;
         end if;

         Lo := Shade (Bg, 0.6);
         Hi := Shade (Bg, 1.25);

         --  Clip the cross
         Cairo.Set_Fill_Rule (Cr, Cairo_Fill_Rule_Even_Odd);
         Cairo.Rectangle
           (Cr, -1.0, -1.0, dW + 2.0, dW + 2.0);
         Cross (Cr, dW, Cross_W, dW / 5.0);
         Cairo.Clip (Cr);
         Cairo.Set_Fill_Rule (Cr, Cairo_Fill_Rule_Winding);

         --  Now actually draw the button

         --  Fill the base color
         Cairo.Set_Source_Rgba (Cr, Base.R, Base.G, Base.B, Alpha);
         Rounded_Rectangle (Cr, 0.0, 0.0, dW, dW, 2.5);
         Cairo.Fill (Cr);

         --  Add some radial shadow to simulate shadow under the cross
         Ptrn := Cairo.Pattern.Create_Radial
           (dW * 0.5, dW * 0.5, 2.0, dW * 0.5, dW * 0.5, Cross_W / 2.0);
         Cairo.Pattern.Add_Color_Stop_Rgba
           (Ptrn, 0.0, Lo.R, Lo.G, Lo.B, Alpha);
         Cairo.Pattern.Add_Color_Stop_Rgba
           (Ptrn, 1.0, Lo.R, Lo.G, Lo.B, 0.0);
         Rounded_Rectangle (Cr, 0.0, 0.0, dW, dW, 2.5);
         Cairo.Set_Source (Cr, Ptrn);
         Cairo.Pattern.Destroy (Ptrn);
         Cairo.Fill (Cr);

         --  Add a hilighted border with height bigger than shadowed border
         --  to just display a thin hilighted border under the button
         Cairo.Set_Source_Rgba (Cr, Hi.R, Hi.G, Hi.B, Alpha);
         Rounded_Rectangle (Cr, 0.5, 0.5, dW - 1.0, dW, 2.5);
         Cairo.Stroke (Cr);

         --  Now add the shadowed border
         Cairo.Set_Source_Rgba (Cr, Lo.R, Lo.G, Lo.B, Alpha);
         Rounded_Rectangle (Cr, 0.5, 0.5, dW - 1.0, dW - 1.0, 2.5);
         Cairo.Stroke (Cr);

         Cairo.Destroy (Cr);
      end if;

      return True;
   end On_Draw;

   -----------
   -- Shade --
   -----------

   function Shade
     (Color : Gdk_Color;
      Value : Gdouble) return Cairo_Color
   is
      Ret : Cairo_Color;
   begin
      Ret :=
        (R => Gdouble (Red (Color)) / 65535.0 * Value,
         G => Gdouble (Green (Color)) / 65535.0 * Value,
         B => Gdouble (Blue (Color)) / 65535.0 * Value);

      if Value > 1.0 then
         if Ret.R > 1.0 then
            Ret.R := 1.0;
         end if;

         if Ret.G > 1.0 then
            Ret.G := 1.0;
         end if;

         if Ret.B > 1.0 then
            Ret.B := 1.0;
         end if;
      end if;

      return Ret;
   end Shade;

   -----------------------
   -- Rounded_Rectangle --
   -----------------------

   procedure Rounded_Rectangle
     (Cr         : Cairo_Context;
      X, Y, W, H : Gdouble;
      Radius     : Gdouble) is
   begin
      Cairo.Move_To (Cr, X + Radius, Y);
      Cairo.Arc
        (Cr, X + W - Radius, Y + Radius, Radius, Pi * 1.5, Pi * 2.0);
      Cairo.Arc
        (Cr, X + W - Radius, Y + H - Radius, Radius, 0.0, Pi * 0.5);
      Cairo.Arc
        (Cr, X + Radius, Y + H - Radius, Radius, Pi * 0.5, Pi);
      Cairo.Arc
        (Cr, X + Radius, Y + Radius, Radius, Pi, Pi * 1.5);
   end Rounded_Rectangle;

   -----------
   -- Cross --
   -----------

   procedure Cross
     (Cr            : Cairo_Context;
      W, Size, Thin : Gdouble)
   is
      Matrix : aliased Cairo_Matrix;
   begin
      Cairo.Get_Matrix (Cr, Matrix'Access);

      --      10+--+9
      --      11|  |8
      --  12 +--+  +--+ 7
      --     |        |
      --   1 +--+  +--+ 6
      --       2|  |5
      --       3+--+4
      --
      --        <-->
      --        Thin
      --
      --     <-------->
      --        Size

      Cairo.Translate (Cr, W / 2.0, W / 2.0);
      Cairo.Rotate (Cr, Pi * 0.25);
      Cairo.Move_To (Cr, -Size / 2.0, -Thin / 2.0); --  1
      Cairo.Line_To (Cr, -Thin / 2.0, -Thin / 2.0); --  2
      Cairo.Line_To (Cr, -Thin / 2.0, -Size / 2.0); --  3
      Cairo.Line_To (Cr, Thin / 2.0, -Size / 2.0);  --  4
      Cairo.Line_To (Cr, Thin / 2.0, -Thin / 2.0);  --  5
      Cairo.Line_To (Cr, Size / 2.0, -Thin / 2.0);  --  6
      Cairo.Line_To (Cr, Size / 2.0, Thin / 2.0);   --  7
      Cairo.Line_To (Cr, Thin / 2.0, Thin / 2.0);   --  8
      Cairo.Line_To (Cr, Thin / 2.0, Size / 2.0);   --  9
      Cairo.Line_To (Cr, -Thin / 2.0, Size / 2.0);  --  10
      Cairo.Line_To (Cr, -Thin / 2.0, Thin / 2.0);  --  11
      Cairo.Line_To (Cr, -Size / 2.0, Thin / 2.0);  --  12
      Cairo.Close_Path (Cr);
      --  Restore the transformation matrix
      Cairo.Set_Matrix (Cr, Matrix'Access);
   end Cross;

   ----------------
   -- Invalidate --
   ----------------

   procedure Invalidate (Widget : access Gtk_Widget_Record'Class) is
   begin
      if Realized_Is_Set (Widget) then
         Invalidate_Rect
           (Gtk.Widget.Get_Window (Widget),
            (Get_Allocation_X (Widget),
             Get_Allocation_Y (Widget),
             Get_Allocation_Width (Widget),
             Get_Allocation_Height (Widget)),
            False);
         Queue_Draw (Widget);
      end if;
   end Invalidate;

   ------------------
   -- On_Tab_Enter --
   ------------------

   function On_Tab_Enter
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event_Crossing)
      return Boolean
   is
      pragma Unreferenced (Event);
   begin
      Gtkada_MDI_Close_Button (Widget).Tab_Over := True;
      Invalidate (Widget);

      return False;
   end On_Tab_Enter;

   ------------------
   -- On_Tab_Leave --
   ------------------

   function On_Tab_Leave
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event_Crossing)
      return Boolean
   is
   begin
      Gtkada_MDI_Close_Button (Widget).Tab_Over := False;

      return On_Leave (Widget, Event);
   end On_Tab_Leave;

   --------------
   -- On_Enter --
   --------------

   function On_Enter
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event_Crossing)
      return Boolean
   is
      pragma Unreferenced (Event);
   begin
      Gtkada_MDI_Close_Button (Widget).Over := True;
      Invalidate (Widget);

      return False;
   end On_Enter;

   --------------
   -- On_Leave --
   --------------

   function On_Leave
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event_Crossing)
      return Boolean
   is
      pragma Unreferenced (Event);
   begin
      Gtkada_MDI_Close_Button (Widget).Over := False;
      Gtkada_MDI_Close_Button (Widget).Pressed := False;
      Invalidate (Widget);

      return False;
   end On_Leave;

   ----------------------
   -- On_Mouse_Pressed --
   ----------------------

   function On_Mouse_Pressed
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event_Button)
      return Boolean
   is
      Button : constant Gtkada_MDI_Close_Button :=
                 Gtkada_MDI_Close_Button (Widget);

   begin
      if Gdk.Event.Get_Button (Event) = 1 and then Button.Over then
         Button.Pressed := True;
         Invalidate (Widget);

         return True;
      end if;

      return False;
   end On_Mouse_Pressed;

   -----------------------
   -- On_Mouse_Released --
   -----------------------

   function On_Mouse_Released
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event_Button)
      return Boolean
   is
      Button : constant Gtkada_MDI_Close_Button :=
                 Gtkada_MDI_Close_Button (Widget);

   begin
      if Button.Pressed and then Gdk.Event.Get_Button (Event) = 1 then
         Close_Child (Button.Child);

         return True;
      end if;

      return False;
   end On_Mouse_Released;

end Close_Button;
