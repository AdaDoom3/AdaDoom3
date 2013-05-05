-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 2000                            --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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

with Gtk.Extra.Plot;        use Gtk.Extra.Plot;
with System;

package body Gtk.Extra.Plot_Ps is

   --------------------
   -- Plot_Export_Ps --
   --------------------

   procedure Plot_Export_Ps
      (Plot        : access Gtk.Extra.Plot.Gtk_Plot_Record'Class;
       Psfile      : in String;
       Orientation : in Ps_Orientation;
       Epsflag     : in Boolean;
       Page_Size   : in Ps_Page_Size)
   is
      procedure Internal
        (Plot        : System.Address;
         Psfile      : String;
         Orientation : Ps_Orientation;
         Epsflag     : Gint;
         Page_Size   : Ps_Page_Size);
      pragma Import (C, Internal, "gtk_plot_export_ps");

   begin
      Internal (Get_Object (Plot),
                Psfile & ASCII.NUL,
                Orientation,
                Boolean'Pos (Epsflag),
                Page_Size);
   end Plot_Export_Ps;

   ------------------------------
   -- Plot_Export_Ps_With_Size --
   ------------------------------

   procedure Plot_Export_Ps_With_Size
      (Plot        : access Gtk.Extra.Plot.Gtk_Plot_Record'Class;
       Psfile      : in String;
       Orientation : in Ps_Orientation;
       Epsflag     : in Boolean;
       Units       : in Ps_Units;
       Width       : in Gint;
       Height      : in Gint)
   is
      procedure Internal
        (Plot        : System.Address;
         Psfile      : String;
         Orientation : Ps_Orientation;
         Epsflag     : Gint;
         Units       : Ps_Units;
         Width       : Gint;
         Height      : Gint);
      pragma Import (C, Internal, "gtk_plot_export_ps_with_size");

   begin
      Internal (Get_Object (Plot),
                Psfile & ASCII.NUL,
                Orientation,
                Boolean'Pos (Epsflag),
                Units,
                Width,
                Height);
   end Plot_Export_Ps_With_Size;

   ---------------------------
   -- Plot_Canvas_Export_Ps --
   ---------------------------

   procedure Plot_Canvas_Export_Ps
      (Canvas      : access Gtk.Extra.Plot_Canvas.Gtk_Plot_Canvas_Record'Class;
       File_Name   : in String;
       Orientation : in Ps_Orientation;
       Epsflag     : in Boolean;
       Page_Size   : in Ps_Page_Size)
   is
      procedure Internal
        (Canvas      : System.Address;
         File_Name   : String;
         Orientation : Ps_Orientation;
         Epsflag     : Gint;
         Page_Size   : Ps_Page_Size);
      pragma Import (C, Internal, "gtk_plot_canvas_export_ps");

   begin
      Internal (Get_Object (Canvas),
                File_Name & ASCII.NUL,
                Orientation,
                Boolean'Pos (Epsflag),
                Page_Size);
   end Plot_Canvas_Export_Ps;

   -------------------------------------
   -- Plot_Canvas_Export_Ps_With_Size --
   -------------------------------------

   procedure Plot_Canvas_Export_Ps_With_Size
      (Canvas      : access Gtk.Extra.Plot_Canvas.Gtk_Plot_Canvas_Record'Class;
       File_Name   : in String;
       Orientation : in Ps_Orientation;
       Epsflag     : in Boolean;
       Units       : in Ps_Units;
       Width       : in Gint;
       Height      : in Gint)
   is
      procedure Internal
        (Canvas      : System.Address;
         File_Name   : String;
         Orientation : Ps_Orientation;
         Epsflag     : Gint;
         Units       : Ps_Units;
         Width       : Gint;
         Height      : Gint);
      pragma Import (C, Internal, "gtk_plot_canvas_export_ps_with_size");

   begin
      Internal (Get_Object (Canvas),
                File_Name & ASCII.NUL,
                Orientation,
                Boolean'Pos (Epsflag),
                Units,
                Width,
                Height);
   end Plot_Canvas_Export_Ps_With_Size;

end Gtk.Extra.Plot_Ps;
