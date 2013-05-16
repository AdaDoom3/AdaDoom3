-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                 Copyright (C) 2000-2013, AdaCore                  --
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

with Gdk.Color;            use Gdk.Color;
with Gtk.Extra.Plot_Data;  use Gtk.Extra.Plot_Data;

with Glib.Type_Conversion_Hooks;

package body Gtk.Extra.Plot_Canvas.Ellipse is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Plot_Canvas_Ellipse_Record);
   pragma Warnings (Off, Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Child : out Gtk_Plot_Canvas_Ellipse;
      Style : Gtk.Extra.Plot_Data.Plot_Line_Style;
      Width : Gfloat;
      Fg    : Gdk.Color.Gdk_Color;
      Bg    : Gdk.Color.Gdk_Color;
      Fill  : Boolean)
   is
      function Internal
        (Style : Plot_Line_Style;
         Width : Gfloat;
         Fg    : Gdk_Color;
         Bg    : Gdk_Color;
         Fill  : Gboolean) return System.Address;
      pragma Import (C, Internal, "gtk_plot_canvas_ellipse_new");
   begin
      Child := new Gtk_Plot_Canvas_Ellipse_Record;
      Set_Object (Child, Internal (Style, Width, Fg, Bg, Boolean'Pos (Fill)));
   end Gtk_New;

   --------------------
   -- Set_Attributes --
   --------------------

   procedure Set_Attributes
     (Ellipse : access Gtk_Plot_Canvas_Ellipse_Record;
      Style   : Gtk.Extra.Plot_Data.Plot_Line_Style;
      Width   : Gdouble;
      Fg      : Gdk.Color.Gdk_Color;
      Bg      : Gdk.Color.Gdk_Color;
      Fill    : Boolean)
   is
      procedure Internal
        (Ellipse : System.Address;
         Style   : Plot_Line_Style;
         Width   : Gdouble;
         Fg      : Gdk_Color;
         Bg      : Gdk_Color;
         Fill    : Gboolean);
      pragma Import (C, Internal, "gtk_plot_canvas_ellipse_set_attributes");
   begin
      Internal
        (Get_Object (Ellipse), Style, Width, Fg, Bg, Boolean'Pos (Fill));
   end Set_Attributes;

end Gtk.Extra.Plot_Canvas.Ellipse;
