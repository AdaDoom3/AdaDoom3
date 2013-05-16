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

with Glib;       use Glib;
with Gtk.Box;    use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Label;  use Gtk.Label;
with Gtk.Main;   use Gtk.Main;
with Gtk.Widget; use Gtk.Widget;
with Gtk;        use Gtk;
with Common;     use Common;

with Ada.Text_IO;

package body Create_Main_Loop is

   ------------------
   -- Loop_Destroy --
   ------------------

   procedure Loop_Destroy (Win : access Gtk_Widget_Record'Class) is
      pragma Warnings (Off, Win);
   begin
      Main_Quit;
   end Loop_Destroy;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This demo creates a second event loop. All the events are now"
        & " processed in this second loop. You start this second loop by"
        & " calling again the @bGtk.Main.Main@B function. The interesting"
        & " side effect is that this procedure call is blocking until"
        & " the procedure @bMain_Quit@B is called. Thus, you can prevent"
        & " your program from exiting a given function until some condition"
        & " is met.";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Label  : Gtk_Label;
      Button : Gtk_Button;
      Box    : Gtk_Box;

   begin
      Set_Label (Frame, "Test Main Loop");

      Gtk_New_Vbox (Box, Homogeneous => False, Spacing => 0);
      Add (Frame, Box);

      Gtk_New (Label, "In recursive main loop...");
      Set_Padding (Label, 20, 20);

      Pack_Start (Box, Label, False, False, 0);

      Gtk_New (Button, "Leave one instance of the main loop");
      Pack_Start (Box, Button, False, False, 0);
      Widget_Handler.Object_Connect
        (Button, "clicked",
         Widget_Handler.To_Marshaller (Loop_Destroy'Access),
         Slot_Object => Box);
      Set_Flags (Button, Can_Default);
      Grab_Default (Button);

      Show_All (Frame);
      Ada.Text_IO.Put_Line
        ("Create_Mainloop: start (and block in the current function)");
      Gtk.Main.Main;
      Ada.Text_IO.Put_Line
        ("Create_Mainloop: done (leave the initial function");
      Ada.Text_IO.Put_Line
        ("Clicking again on the button might leave testgtk itself.");
   end Run;

end Create_Main_Loop;

