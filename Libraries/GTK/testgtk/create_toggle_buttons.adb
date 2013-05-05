-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
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

with Glib;               use Glib;
with Gtk;                use Gtk;
with Gtk.Box;            use Gtk.Box;
with Gtk.Toggle_Button;  use Gtk.Toggle_Button;

package body Create_Toggle_Buttons is

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @bGtk_Toggle_Button@B is a button with two possible states,"
        & " on and off. Their state is modified each time the user pressed"
        & " the button. As opposed to @bGtk_Radio_Button@B, "
        & " @bGtk_Toggle_Button@B are not grouped, and multiple buttons can"
        & " be selected at the same time.";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box1, Box2 : Box.Gtk_Box;
      A_Toggle_Button : Toggle_Button.Gtk_Toggle_Button;

   begin
      Set_Label (Frame, "Toggle Buttons");

      Box.Gtk_New_Vbox (Box1, Homogeneous => False, Spacing => 0);
      Add (Container => Frame, Widget => Box1);

      Box.Gtk_New_Vbox (Box2, Homogeneous => False, Spacing => 10);
      Set_Border_Width (Container => Box2, Border_Width => 10);
      Box.Pack_Start (In_Box => Box1, Child => Box2,
                      Expand => False, Fill => False);

      Toggle_Button.Gtk_New (A_Toggle_Button, "button1");
      Box.Pack_Start (In_Box => Box2, Child => A_Toggle_Button,
                      Expand => False, Fill => False);

      Toggle_Button.Gtk_New (A_Toggle_Button, "button2");
      Box.Pack_Start (In_Box => Box2, Child => A_Toggle_Button,
                      Expand => False, Fill => False);

      Toggle_Button.Gtk_New (A_Toggle_Button, "button3");
      Box.Pack_Start (In_Box => Box2, Child => A_Toggle_Button,
                      Expand => False, Fill => False);

      Show_All (Frame);
   end Run;

end Create_Toggle_Buttons;
