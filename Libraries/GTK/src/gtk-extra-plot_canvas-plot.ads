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

with Gtk.Extra.Plot;
with Gtk.Extra.Plot_Data;

package Gtk.Extra.Plot_Canvas.Plot is

   type Gtk_Plot_Canvas_Plot_Record is new Gtk_Plot_Canvas_Child_Record
     with private;
   type Gtk_Plot_Canvas_Plot is access all Gtk_Plot_Canvas_Plot_Record'Class;

   type Plot_Canvas_Plot_Pos is
     (Plot_Out,
      Plot_In_Plot,
      Plot_In_Legends,
      Plot_In_Title,
      Plot_In_Axis,
      Plot_In_Data,
      Plot_In_Gradient,
      Plot_In_Marker);
   --  The various parts of a plot in which a user can click

   procedure Gtk_New
     (Child    : out Gtk_Plot_Canvas_Plot;
      Plot     : access Gtk.Extra.Plot.Gtk_Plot_Record'Class);
   --  Create a new plot child, wrapping Plot

   function Get_Pos
     (Child : access Gtk_Plot_Canvas_Plot_Record) return Plot_Canvas_Plot_Pos;
   --  Return the position in the plot where the user has last clicked

   function Get_Data
     (Child : access Gtk_Plot_Canvas_Plot_Record)
      return Gtk.Extra.Plot_Data.Gtk_Plot_Data;
   --  Return the data associated with Child

   function Get_Datapoint
     (Child : access Gtk_Plot_Canvas_Plot_Record) return Gint;
   --  Return the point in the plot data that was selected by the user

   type Plot_Canvas_Plot_Flags is mod 2 ** 8;
   Flags_Select_Point : constant Plot_Canvas_Plot_Flags := 2 ** 0;
   Flags_Dnd_Point    : constant Plot_Canvas_Plot_Flags := 2 ** 1;
   --  Flags specific to a Gtk_Plot_Canvas_Plot

   procedure Set_Flags
     (Child : access Gtk_Plot_Canvas_Plot_Record;
      Flags : Plot_Canvas_Plot_Flags);
   procedure Unset_Flags
     (Child : access Gtk_Plot_Canvas_Plot_Record;
      Flags : Plot_Canvas_Plot_Flags);
   --  Set or Unset specific flags from Child

private
   pragma Convention (C, Plot_Canvas_Plot_Pos);

   type Gtk_Plot_Canvas_Plot_Record is new Gtk_Plot_Canvas_Child_Record
     with null record;
end Gtk.Extra.Plot_Canvas.Plot;
