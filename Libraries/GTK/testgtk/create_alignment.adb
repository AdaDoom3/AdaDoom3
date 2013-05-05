-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
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

with Glib;          use Glib;
with Gtk;           use Gtk;
with Gtk.Alignment; use Gtk.Alignment;
with Gtk.Box;       use Gtk.Box;
with Gtk.Button;    use Gtk.Button;
with Gtk.Frame;     use Gtk.Frame;

package body Create_Alignment is

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This demo shows how a @bGtk_Alignment@B widget can be used to"
        & " align and resize your widgets exactly the way you want."
        & ASCII.LF
        & "With this container, you can specify the amount of expansion the"
        & " child will have, as well as its alignment in case it isn't fully"
        & " expanded.";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box1   : Gtk_Box;
      Align  : Gtk_Alignment;
      Button : Gtk_Button;
      Frame2 : Gtk_Frame;

   begin
      Gtk.Frame.Set_Label (Frame, "Alignment");

      Gtk_New_Vbox (Box1, Homogeneous => True, Spacing => 0);
      Add (Frame, Box1);

      Gtk_New (Frame2, "Xalign=0.0 Yalign=0.0 Xscale=0.2 Yscale=0.2");
      Gtk_New (Align,
               Xalign => 0.0,
               Yalign => 0.0,
               Xscale => 0.2,
               Yscale => 0.2);
      Gtk_New (Button, "Button");
      Add (Align, Button);
      Add (Frame2, Align);
      Pack_Start (Box1, Frame2);

      Gtk_New (Frame2, "Xalign=0.5 Yalign=0.5 Xscale=0.2 Yscale=0.2");
      Gtk_New (Align,
               Xalign => 0.5,
               Yalign => 0.5,
               Xscale => 0.2,
               Yscale => 0.2);
      Gtk_New (Button, "Button");
      Add (Align, Button);
      Add (Frame2, Align);
      Pack_Start (Box1, Frame2);

      Gtk_New (Frame2, "Xalign=1.0 Yalign=1.0 Xscale=0.2 Yscale=0.2");
      Gtk_New (Align,
               Xalign => 1.0,
               Yalign => 1.0,
               Xscale => 0.2,
               Yscale => 0.2);
      Gtk_New (Button, "Button");
      Add (Align, Button);
      Add (Frame2, Align);
      Pack_Start (Box1, Frame2);

      Gtk_New (Frame2, "Xalign=0.0 Yalign=0.0 Xscale=0.9 Yscale=0.9");
      Gtk_New (Align,
               Xalign => 0.0,
               Yalign => 0.0,
               Xscale => 0.9,
               Yscale => 0.9);
      Gtk_New (Button, "Button");
      Add (Align, Button);
      Add (Frame2, Align);
      Pack_Start (Box1, Frame2);

      Gtk_New (Frame2, "Xalign=0.2 Yalign=0.9 Xscale=0.5 Yscale=0.5");
      Gtk_New (Align,
               Xalign => 0.2,
               Yalign => 0.9,
               Xscale => 0.5,
               Yscale => 0.5);
      Gtk_New (Button, "Button");
      Add (Align, Button);
      Add (Frame2, Align);
      Pack_Start (Box1, Frame2);

      Show_All (Frame);
   end Run;

end Create_Alignment;

