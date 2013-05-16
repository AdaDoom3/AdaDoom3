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

with Ada.Numerics; use Ada.Numerics;

with Glib;         use Glib;

with Cairo;        use Cairo;
with Pango.Cairo;  use Pango.Cairo;
with Gdk.Cairo;    use Gdk.Cairo;

with Gtk.Enums;    use Gtk.Enums;
with Gtk.Style;    use Gtk.Style;

package body Gtkada.Style is

   ------------
   -- To_HSV --
   ------------

   function To_HSV (Color : Cairo_Color) return HSV_Color
   is
      Max, Min, Del       : Gdouble;
      Del_R, Del_G, Del_B : Gdouble;
      Ret                 : HSV_Color;

   begin
      Max := Gdouble'Max (Gdouble'Max (Color.R, Color.G), Color.B);
      Min := Gdouble'Min (Gdouble'Min (Color.R, Color.G), Color.B);
      Del := Max - Min;

      Ret.V := Max;
      Ret.A := Color.Alpha;

      if Del = 0.0 then
         Ret.H := 0.0;
         Ret.S := 0.0;

      else
         Ret.S := Del / Max;

         Del_R := (((Del - Color.R) / 6.0) + (Del / 2.0)) / Del;
         Del_G := (((Del - Color.G) / 6.0) + (Del / 2.0)) / Del;
         Del_B := (((Del - Color.B) / 6.0) + (Del / 2.0)) / Del;

         if Max = Color.R then
            Ret.H := Del_B - Del_G;
         elsif Max = Color.G then
            Ret.H := (1.0 / 3.0) + Del_R - Del_B;
         elsif Max = Color.B then
            Ret.H := (2.0 / 3.0) + Del_G - Del_R;
         end if;

         if Ret.H < 0.0 then
            Ret.H := Ret.H + 1.0;
         end if;

         if Ret.H >= 1.0 then
            Ret.H := Ret.H - 1.0;
         end if;
      end if;

      return Ret;
   end To_HSV;

   --------------
   -- To_Cairo --
   --------------

   function To_Cairo (HSV : HSV_Color) return Cairo_Color
   is
      Var_H, Var_J, Var_1, Var_2, Var_3 : Gdouble;
      Ret  : Cairo_Color;

   begin
      Ret.Alpha := HSV.A;

      if HSV.S = 0.0 then
         Ret.R := HSV.V;
         Ret.G := HSV.V;
         Ret.B := HSV.V;

      else
         if HSV.H = 1.0 then
            Var_H := 0.0;
         else
            Var_H := HSV.H * 6.0;
         end if;

         Var_J := Gdouble'Floor (Var_H);
         Var_1 := HSV.V * (1.0 - HSV.S);
         Var_2 := HSV.V * (1.0 - HSV.S * (Var_H - Var_J));
         Var_3 := HSV.V * (1.0 - HSV.S * (1.0 - (Var_H - Var_J)));

         if Var_J = 0.0 then
            Ret.R := HSV.V;
            Ret.G := Var_3;
            Ret.B := Var_1;
         elsif Var_J = 1.0 then
            Ret.R := Var_2;
            Ret.G := HSV.V;
            Ret.B := Var_1;
         elsif Var_J = 2.0 then
            Ret.R := Var_1;
            Ret.G := HSV.V;
            Ret.B := Var_3;
         elsif Var_J = 3.0 then
            Ret.R := Var_1;
            Ret.G := Var_2;
            Ret.B := HSV.V;
         elsif Var_J = 4.0 then
            Ret.R := Var_3;
            Ret.G := Var_1;
            Ret.B := HSV.V;
         else
            Ret.R := HSV.V;
            Ret.G := Var_1;
            Ret.B := Var_2;
         end if;
      end if;

      return Ret;
   end To_Cairo;

   --------------
   -- To_Cairo --
   --------------

   function To_Cairo (Color : Gdk.Color.Gdk_Color) return Cairo_Color is
   begin
      return (R     => Gdouble (Gdk.Color.Red (Color)) / 65535.0,
              G     => Gdouble (Gdk.Color.Green (Color)) / 65535.0,
              B     => Gdouble (Gdk.Color.Blue (Color)) / 65535.0,
              Alpha => 1.0);
   end To_Cairo;

   -----------
   -- Shade --
   -----------

   function Shade
     (Color : Gdk.Color.Gdk_Color;
      Value : Glib.Gdouble)
      return Cairo_Color
   is
   begin
      return Shade (To_Cairo (Color), Value);
   end Shade;

   -----------
   -- Shade --
   -----------

   function Shade
     (Color : Cairo_Color;
      Value : Glib.Gdouble)
      return Cairo_Color
   is
      HSV : HSV_Color;

   begin
      if Value /= 1.0 then
         HSV := To_HSV (Color);
         HSV.V := Gdouble'Min (1.0, HSV.V * Value);

         return To_Cairo (HSV);
      end if;

      return Color;
   end Shade;

   ----------------------
   -- Set_Source_Color --
   ----------------------

   procedure Set_Source_Color
     (Cr : Cairo.Cairo_Context; Color : Cairo_Color) is
   begin
      Cairo.Set_Source_Rgba (Cr, Color.R, Color.G, Color.B, Color.Alpha);
   end Set_Source_Color;

   -----------------------
   -- Rounded_Rectangle --
   -----------------------

   procedure Rounded_Rectangle
     (Cr         : Cairo.Cairo_Context;
      X, Y, W, H : Glib.Gdouble;
      Radius     : Glib.Gdouble)
   is
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

   -----------------
   -- Draw_Shadow --
   -----------------

   procedure Draw_Shadow
     (Cr                  : Cairo.Cairo_Context;
      Style               : Gtk.Style.Gtk_Style;
      Shadow_Type         : Gtk.Enums.Gtk_Shadow_Type;
      X, Y, Width, Height : Glib.Gint;
      Corner_Radius       : Glib.Gdouble := 0.0)
   is
      Hilight : Cairo_Color;
      Shadow  : Cairo_Color;
      HSV     : HSV_Color;
      X_Thick : constant Gdouble := Gdouble (X_Thickness (Style)) / 4.0;
      Radius  : Gdouble;
      dX, dY, dW, dH : Gdouble;

   begin
      if Shadow_Type = Shadow_None then
         return;
      end if;

      Cairo.Save (Cr);

      HSV := To_HSV (To_Cairo (Get_Bg (Style, State_Normal)));
      HSV.V := 0.3;
      Shadow := To_Cairo (HSV);

      HSV.V := 0.9;
      Hilight := To_Cairo (HSV);

      dX := Gdouble (X) + X_Thick + 0.5;
      dY := Gdouble (Y) + X_Thick + 0.5;
      dW := Gdouble (Width) - 2.0 * X_Thick - 1.0;
      dH := Gdouble (Height) - 2.0 * X_Thick - 1.0;

      if Corner_Radius < X_Thick then
         Radius := X_Thick;
      else
         Radius := Corner_Radius;
      end if;

      if Shadow_Type = Shadow_Out or else Shadow_Type = Shadow_Etched_Out then
         Set_Line_Width (Cr, X_Thick);
         Set_Source_Color (Cr, Hilight);

      else
         if X_Thick < 0.5 then
            Set_Line_Width (Cr, 0.5);
         else
            Set_Line_Width (Cr, X_Thick);
         end if;

         Set_Source_Color (Cr, Shadow);
      end if;

      Rounded_Rectangle
        (Cr, dX, dY, dW, dH, Radius);
      Cairo.Stroke (Cr);

      if Shadow_Type = Shadow_Out or else Shadow_Type = Shadow_Etched_Out then
         if X_Thick < 0.5 then
            Set_Line_Width (Cr, 0.5);
         else
            Set_Line_Width (Cr, X_Thick);
         end if;

         Set_Source_Color (Cr, Shadow);

      else
         Set_Line_Width (Cr, X_Thick);
         Set_Source_Color (Cr, Hilight);
      end if;

      Rounded_Rectangle
        (Cr,
         dX + X_Thick, dY + X_Thick, dW - 2.0 * X_Thick, dH - 2.0 * X_Thick,
         Radius - X_Thick * 1.0);
      Cairo.Stroke (Cr);

      Cairo.Restore (Cr);
   end Draw_Shadow;

   --------------------
   -- Draw_Rectangle --
   --------------------

   procedure Draw_Rectangle
     (Cr                  : Cairo.Cairo_Context;
      Color               : Gdk.Color.Gdk_Color;
      Filled              : Boolean;
      X, Y, Width, Height : Glib.Gint;
      Corner_Radius       : Glib.Gdouble := 0.0) is
   begin
      Draw_Rectangle
        (Cr, To_Cairo (Color), Filled, X, Y, Width, Height, Corner_Radius);
   end Draw_Rectangle;

   --------------------
   -- Draw_Rectangle --
   --------------------

   procedure Draw_Rectangle
     (Cr                  : Cairo.Cairo_Context;
      Color               : Cairo_Color;
      Filled              : Boolean;
      X, Y, Width, Height : Glib.Gint;
      Corner_Radius       : Glib.Gdouble := 0.0)
   is
      dX, dY, dW, dH : Gdouble;
      Line_Width     : Gdouble;

   begin
      dX := Gdouble (X);
      dY := Gdouble (Y);
      dW := Gdouble (Width);
      dH := Gdouble (Height);

      if not Filled then
         Line_Width := Get_Line_Width (Cr) / 2.0;
         dX := dX + Line_Width;
         dY := dY + Line_Width;
         dW := dW - 2.0 * Line_Width;
         dH := dH - 2.0 * Line_Width;
      end if;

      if Corner_Radius = 0.0 then
         Rectangle (Cr, dX, dY, dW, dH);
      else
         Rounded_Rectangle (Cr, dX, dY, dW, dH, Corner_Radius);
      end if;

      Set_Source_Color (Cr, Color);

      if Filled then
         Cairo.Fill (Cr);
      else
         Stroke (Cr);
      end if;
   end Draw_Rectangle;

   ---------------
   -- Draw_Line --
   ---------------

   procedure Draw_Line
     (Cr             : Cairo.Cairo_Context;
      Color          : Gdk.Color.Gdk_Color;
      X1, Y1, X2, Y2 : Glib.Gint) is
   begin
      Draw_Line (Cr, To_Cairo (Color), X1, Y1, X2, Y2);
   end Draw_Line;

   ---------------
   -- Draw_Line --
   ---------------

   procedure Draw_Line
     (Cr             : Cairo.Cairo_Context;
      Color          : Cairo_Color;
      X1, Y1, X2, Y2 : Glib.Gint)
   is
      Line_Width     : Gdouble;
   begin
      Set_Source_Color (Cr, Color);
      Line_Width := Get_Line_Width (Cr) / 2.0;

      Move_To (Cr, Gdouble (X1) + Line_Width, Gdouble (Y1) + Line_Width);
      Line_To (Cr, Gdouble (X2) + Line_Width, Gdouble (Y2) + Line_Width);
      Stroke (Cr);
   end Draw_Line;

   -----------------
   -- Draw_Layout --
   -----------------

   procedure Draw_Layout
     (Cr     : Cairo.Cairo_Context;
      Color  : Gdk.Color.Gdk_Color;
      X, Y   : Glib.Gint;
      Layout : Pango.Layout.Pango_Layout)
   is
   begin
      Draw_Layout (Cr, To_Cairo (Color), X, Y, Layout);
   end Draw_Layout;

   -----------------
   -- Draw_Layout --
   -----------------

   procedure Draw_Layout
     (Cr     : Cairo.Cairo_Context;
      Color  : Cairo_Color;
      X, Y   : Glib.Gint;
      Layout : Pango.Layout.Pango_Layout)
   is
   begin
      Set_Source_Color (Cr, Color);
      Move_To (Cr, Gdouble (X), Gdouble (Y));
      Pango.Cairo.Show_Layout (Cr, Layout);
   end Draw_Layout;

   -----------------
   -- Draw_Pixbuf --
   -----------------

   procedure Draw_Pixbuf
     (Cr     : Cairo.Cairo_Context;
      Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf;
      X, Y   : Glib.Gint)
   is
   begin
      Gdk.Cairo.Set_Source_Pixbuf (Cr, Pixbuf, Gdouble (X), Gdouble (Y));
      Cairo.Paint (Cr);
   end Draw_Pixbuf;

end Gtkada.Style;
