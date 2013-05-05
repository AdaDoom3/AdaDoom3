-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 2000                            --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                     Copyright (C) 2003-2006 AdaCore               --
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

with Glib;                  use Glib;
with Gtk.Box;               use Gtk.Box;
with Gtk.Button;            use Gtk.Button;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Extra.Plot;        use Gtk.Extra.Plot;
with Gtk.Extra.Plot_Data;   use Gtk.Extra.Plot_Data;
with Gtk.Extra.Plot_Canvas; use Gtk.Extra.Plot_Canvas;
with Gtk.Extra.Plot_Canvas.Plot; use Gtk.Extra.Plot_Canvas.Plot;
with Gtk.Extra.Plot_3D;     use Gtk.Extra.Plot_3D;
with Gtk.Extra.Plot_Surface; use Gtk.Extra.Plot_Surface;
with Gtk.Frame;             use Gtk.Frame;
with Gtk.Scrolled_Window;   use Gtk.Scrolled_Window;
with Gtk.Handlers;          use Gtk.Handlers;
with Ada.Numerics.Generic_Elementary_Functions;
with System;

package body Create_Plot_3D is

   package Layout_Cb is new Gtk.Handlers.Callback (Gtk_Plot_Canvas_Record);

   package Double_Numerics is new
     Ada.Numerics.Generic_Elementary_Functions (Gdouble);
   use Double_Numerics;

   procedure Rotatex (Canvas : access Gtk_Plot_Canvas_Record'Class);
   procedure Rotatey (Canvas : access Gtk_Plot_Canvas_Record'Class);
   procedure Rotatez (Canvas : access Gtk_Plot_Canvas_Record'Class);
   --  rotates the plot in one of the usual directions

   function My_Function
     (Plot  : System.Address;
      Set   : Gtk_Plot_Data;
      X, Y  : Gdouble;
      Error : access Gboolean) return Gdouble;
   pragma Convention (C, My_Function);

   Active_Plot : Gtk_Plot_3D;

   -----------------
   -- My_Function --
   -----------------

   function My_Function
     (Plot  : System.Address;
      Set   : Gtk_Plot_Data;
      X, Y  : Gdouble;
      Error : access Gboolean) return Gdouble
   is
      pragma Warnings (Off, Plot);
      pragma Warnings (Off, Set);
   begin
      Error.all := 0;
      return Cos (((X - 0.5) * (X - 0.5) + (Y - 0.5) *(Y - 0.5)) * 24.0)
        / 4.0 + 0.5;
   end My_Function;

   -------------
   -- Rotatex --
   -------------

   procedure Rotatex (Canvas : access Gtk_Plot_Canvas_Record'Class) is
   begin
      Rotate_X (Active_Plot, 10.0);
      Paint (Canvas);
      Refresh (Canvas);
   end Rotatex;

   -------------
   -- Rotatey --
   -------------

   procedure Rotatey (Canvas : access Gtk_Plot_Canvas_Record'Class) is
   begin
      Rotate_Y (Active_Plot, 10.0);
      Paint (Canvas);
      Refresh (Canvas);
   end Rotatey;

   -------------
   -- Rotatez --
   -------------

   procedure Rotatez (Canvas : access Gtk_Plot_Canvas_Record'Class) is
   begin
      Rotate_Z (Active_Plot, 10.0);
      Paint (Canvas);
      Refresh (Canvas);
   end Rotatez;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "See the @bPlot demo@B for more information";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Vbox1       : Gtk_Box;
      Scrollw1    : Gtk_Scrolled_Window;
      Button      : Gtk_Button;
      Plot        : Gtk_Plot_3D;
      Canvas      : Gtk_Plot_Canvas;
      Surface     : Gtk_Plot_Surface;
      Plot_Child  : Gtk_Plot_Canvas_Plot;
   begin
      Set_Label (Frame, "Gtk.Extra.Plot_3D demo");

      Gtk_New_Vbox (Vbox1, False, 0);
      Add (Frame, Vbox1);

      --  Put the Plot in a scrolled window, in case it is too big for
      --  the screen
      Gtk_New (Scrollw1);
      Set_Border_Width (Scrollw1, 0);
      Set_Policy (Scrollw1, Policy_Always, Policy_Always);
      Pack_Start (Vbox1, Scrollw1, True, True, 0);

      --  Create the canvas in which the plot will be drawn
      Gtk_New (Canvas, Gint (Get_Allocation_Width (Frame) - 10),
               Gint (Get_Allocation_Height (Frame) - 10), 1.0);
      Plot_Canvas_Set_Flags (Canvas, Dnd_Flags);
      Add_With_Viewport (Scrollw1, Canvas);

      --  Create the plot
      Gtk_New (Plot, null, 0.7, 0.7);
      Gtk_New (Plot_Child, Plot);
      Put_Child (Canvas, Plot_Child, 0.1, 0.06, 0.9, 0.9);
      Show (Plot);
      Active_Plot := Plot;

      Set_Minor_Ticks (Plot, Axis_X, 1);
      Set_Minor_Ticks (Plot, Axis_Y, 1);
      Show_Ticks (Plot, Plot_Side_Xy, Ticks_Out, Ticks_Out);
      Show_Ticks (Plot, Plot_Side_Xz, Ticks_Out, Ticks_Out);
      Show_Ticks (Plot, Plot_Side_Yz, Ticks_Out, Ticks_Out);
      Corner_Set_Visible (Plot, True);

      --  Create the data set
      Gtk_New (Surface, My_Function'Access);
      Set_Xstep (Surface, 0.025);
      Set_Ystep (Surface, 0.025);
      Set_Legend (Surface, "cos ((r-r\s0\N)\S2\N)");
      Add_Data (Plot, Surface);
      Show (Surface);

      --  Buttons
      Gtk_New (Button, "Rotate X");
      Put (Canvas, Button, 80, 0);
      Layout_Cb.Object_Connect
        (Button, "clicked", Layout_Cb.To_Marshaller (Rotatex'Access), Canvas);

      Gtk_New (Button, "Rotate Y");
      Put (Canvas, Button, 160, 0);
      Layout_Cb.Object_Connect
        (Button, "clicked", Layout_Cb.To_Marshaller (Rotatey'Access), Canvas);

      Gtk_New (Button, "Rotate Z");
      Put (Canvas, Button, 240, 0);
      Layout_Cb.Object_Connect
        (Button, "clicked", Layout_Cb.To_Marshaller (Rotatez'Access), Canvas);

      Show_All (Frame);
   end Run;

end Create_Plot_3D;
