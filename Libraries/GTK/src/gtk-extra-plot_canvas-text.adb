-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
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

with Gtkada.Bindings;      use Gtkada.Bindings;
with Gdk.Color;            use Gdk.Color;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.Extra.Plot_Data;  use Gtk.Extra.Plot_Data;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;

package body Gtk.Extra.Plot_Canvas.Text is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Child    : out Gtk_Plot_Canvas_Text;
      Text     : String;
      Font     : String := "";
      Height   : Gint := 0;
      Angle    : Gtk.Extra.Plot_Data.Plot_Angle := Gtk.Extra.Plot_Data.Angle_0;
      Fg       : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Bg            : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Transparent   : Boolean := True;
      Justification : Gtk.Enums.Gtk_Justification := Gtk.Enums.Justify_Center)
   is
      function Internal
        (Font : chars_ptr; Height : Gint; Angle : Plot_Angle;
         Fg, Bg : System.Address;
         Transparent : Gboolean;
         Justification : Gtk_Justification;
         Text : String) return System.Address;
      pragma Import (C, Internal, "gtk_plot_canvas_text_new");

      F1   : aliased Gdk_Color := Fg;
      B1   : aliased Gdk_Color := Bg;
      F, B : System.Address := System.Null_Address;
      T    : chars_ptr := String_Or_Null (Font);

   begin
      if Fg /= Null_Color then
         F := F1'Address;
      end if;
      if Bg /= Null_Color then
         B := B1'Address;
      end if;

      Child := new Gtk_Plot_Canvas_Text_Record;
      Set_Object
        (Child, Internal
           (T, Height, Angle, F, B,
            Boolean'Pos (Transparent), Justification,
            Text & ASCII.NUL));
      Free (T);
   end Gtk_New;

   --------------------
   -- Set_Attributes --
   --------------------

   procedure Set_Attributes
     (Child         : access Gtk_Plot_Canvas_Text_Record;
      Font          : String := "";
      Height        : Gint;
      Angle         : Gtk.Extra.Plot_Data.Plot_Angle;
      Fg            : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Bg            : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Transparent   : Boolean;
      Justification : Gtk.Enums.Gtk_Justification;
      Text          : String)
   is
      procedure Internal
        (Child         : System.Address;
         Font          : chars_ptr;
         Height        : Gint;
         Angle         : Plot_Angle;
         Fg, Bg        : System.Address;
         Transparent   : Gboolean;
         Justification : Gtk_Justification;
         Text          : String);
      pragma Import (C, Internal, "gtk_plot_canvas_text_set_attributes");
      F1   : aliased Gdk_Color := Fg;
      B1   : aliased Gdk_Color := Bg;
      F, B : System.Address := System.Null_Address;
      T    : chars_ptr := String_Or_Null (Font);
   begin
      if Fg /= Null_Color then
         F := F1'Address;
      end if;
      if Bg /= Null_Color then
         B := B1'Address;
      end if;
      Internal (Get_Object (Child), T, Height, Angle, F, B,
                Boolean'Pos (Transparent), Justification, Text & ASCII.NUL);
      Free (T);
   end Set_Attributes;

end Gtk.Extra.Plot_Canvas.Text;
