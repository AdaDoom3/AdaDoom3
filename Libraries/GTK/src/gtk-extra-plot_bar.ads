-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--      Copyright (C) 2000 E. Briot, J. Brobecker and A. Charlet     --
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
--  This special type of data set displays itself with bar (also known
--  as histograms).
--  </description>
--  <c_version>gtkextra 2.1.1</c_version>
--  <group>Plotting Data</group>

with Gtk.Extra.Plot_Data;
with Gtk.Enums;

package Gtk.Extra.Plot_Bar is

   type Gtk_Plot_Bar_Record is new Gtk.Extra.Plot_Data.Gtk_Plot_Data_Record
     with private;
   type Gtk_Plot_Bar is access all Gtk_Plot_Bar_Record'Class;

   procedure Gtk_New
     (Bar         : out Gtk_Plot_Bar;
      Orientation : Gtk.Enums.Gtk_Orientation);
   --  Create a new Plot bar.

   procedure Initialize
     (Bar         : access Gtk_Plot_Bar_Record'Class;
      Orientation : Gtk.Enums.Gtk_Orientation);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Bar.

   procedure Set_Width
     (Bar   : access Gtk_Plot_Bar_Record'Class;
      Width : Gdouble);
   --  Set the width of the bars

   function Get_Width (Bar : access Gtk_Plot_Bar_Record'Class) return Gdouble;
   --  Return the width used to draw the bars

private
   type Gtk_Plot_Bar_Record is new Gtk.Extra.Plot_Data.Gtk_Plot_Data_Record
     with null record;
   pragma Import (C, Get_Type, "gtk_plot_bar_get_type");
end Gtk.Extra.Plot_Bar;
