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

--  <description>
--  A special kind of child that can be put in a Gtk_Plot_Canvas.
--  </description>
--  <c_version>gtkextra 2.1.1</c_version>
--  <group>Plotting Data</group>

with Gdk.Color;
with Gtk.Extra.Plot_Data;

package Gtk.Extra.Plot_Canvas.Line is

   type Gtk_Plot_Canvas_Line_Record is new Gtk_Plot_Canvas_Child_Record
     with null record;
   type Gtk_Plot_Canvas_Line is access all Gtk_Plot_Canvas_Line_Record'Class;

   type Plot_Canvas_Arrow is new Gint;
   Arrow_None   : constant Plot_Canvas_Arrow := 0;
   Arrow_Origin : constant Plot_Canvas_Arrow := 1;
   Arrow_End    : constant Plot_Canvas_Arrow := 2;

   procedure Gtk_New
     (Child      : out Gtk_Plot_Canvas_Line;
      Style      : Gtk.Extra.Plot_Data.Plot_Line_Style;
      Width      : Gfloat;
      Color      : Gdk.Color.Gdk_Color;
      Arrow_Mask : Plot_Canvas_Arrow);
   --  Create a new line child

   function Get_Type return Glib.GType;
   --  Return the internal type used for this child

   procedure Set_Arrow
     (Line   : access Gtk_Plot_Canvas_Line_Record;
      Style  : Gtk.Extra.Plot_Data.Plot_Symbol_Style;
      Width  : Gfloat;
      Length : Gfloat;
      Mask   : Plot_Canvas_Arrow);
   --  Change the arrow style for this line

   procedure Set_Attributes
     (Line  : access Gtk_Plot_Canvas_Line_Record;
      Style : Gtk.Extra.Plot_Data.Plot_Line_Style;
      Width : Gfloat;
      Color : Gdk.Color.Gdk_Color);
   --  Change the attributes of the line

private
   pragma Import (C, Get_Type, "gtk_plot_canvas_line_get_type");
end Gtk.Extra.Plot_Canvas.Line;
