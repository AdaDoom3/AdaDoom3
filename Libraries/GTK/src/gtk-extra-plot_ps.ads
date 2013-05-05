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
--  This package does not implement any new widget.
--  Instead, if provides postscript support for Gtk_Plot widgets, and can
--  create a postscript file from any Gtk_Plot widget.
--  </description>
--  This is a binding to gtkplotprint.h
--  <c_version>gtkextra 2.1.1</c_version>
--  <group>Plotting Data</group>

with Gtk.Extra.Plot;
with Gtk.Extra.Plot_Canvas;  use Gtk.Extra.Plot_Canvas;

package Gtk.Extra.Plot_Ps is

   type Ps_Page_Size is
     (Plot_Letter,
      Plot_Legal,
      Plot_A4,
      Plot_Executive,
      Plot_Custom);
   --  The formats that can be used for paper sizes.
   pragma Convention (C, Ps_Page_Size);

   type Ps_Orientation is (Plot_Portrait, Plot_Landscape);
   --  Portrait format means that the vertical size is longer than
   --  the horizontal size. Landscape is the reverse.
   pragma Convention (C, Ps_Orientation);

   type Ps_Units is (Plot_Inches, Plot_Mm, Plot_Cm, Plot_Pspoints);
   --  Units of measure for paper sizes.
   pragma Convention (C, Ps_Units);

   procedure Plot_Export_Ps
     (Plot        : access Gtk.Extra.Plot.Gtk_Plot_Record'Class;
      Psfile      : String;
      Orientation : Ps_Orientation;
      Epsflag     : Boolean;
      Page_Size   : Ps_Page_Size);
   --  Create a new postscript file PsFile with the content of Plot.
   --  Epsflag should be true if the generated file should be in
   --  Encapsulated Postscript format instead of simple Postscript.

   procedure Plot_Export_Ps_With_Size
     (Plot        : access Gtk.Extra.Plot.Gtk_Plot_Record'Class;
      Psfile      : in String;
      Orientation : in Ps_Orientation;
      Epsflag     : in Boolean;
      Units       : in Ps_Units;
      Width       : in Gint;
      Height      : in Gint);
   --  Create a new postscript file PsFile with the content of Plot.
   --  Epsflag should be true if the generated file should be in
   --  Encapsulated Postscript format instead of simple Postscript.
   --  The page has a custom size.

   procedure Plot_Canvas_Export_Ps
     (Canvas      : access Gtk_Plot_Canvas_Record'Class;
      File_Name   : in String;
      Orientation : in Ps_Orientation;
      Epsflag     : in Boolean;
      Page_Size   : in Ps_Page_Size);
   --  Create a new postscript file PsFile with the content of Canvas.
   --  Every plot on it is exported to the postscript file.
   --  Epsflag should be true if the generated file should be in
   --  Encapsulated Postscript format instead of simple Postscript.

   procedure Plot_Canvas_Export_Ps_With_Size
     (Canvas      : access Gtk_Plot_Canvas_Record'Class;
      File_Name   : in String;
      Orientation : in Ps_Orientation;
      Epsflag     : in Boolean;
      Units       : in Ps_Units;
      Width       : in Gint;
      Height      : in Gint);
   --  Create a new postscript file PsFile with the content of Canvas.
   --  Every plot on it is exported to the postscript file.
   --  Epsflag should be true if the generated file should be in
   --  Encapsulated Postscript format instead of simple Postscript.
   --  The page has a custom size.

   --  Unbound:
   --     gtk_plot_ps_get_type
   --     gtk_plot_ps_new
   --     gtk_plot_ps_new_with_size
   --     gtk_plot_ps_construct
   --     gtk_plot_ps_construct_with_size
   --     gtk_plot_ps_set_size
   --     gtk_plot_ps_set_scale

end Gtk.Extra.Plot_Ps;
