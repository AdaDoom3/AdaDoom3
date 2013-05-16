-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                   Copyright (C) 2008-2013, AdaCore                --
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

--  This program demonstrates how to switch between displays under X11.

with GNAT.OS_Lib;

with Gtk.Main;

with Gdk.Display; use Gdk.Display;
with Gdk.Display_Manager; use Gdk.Display_Manager;

with Gtk.Window; use Gtk.Window;
with Gtk.Button; use Gtk.Button;
with Gtkada.Handlers; use Gtkada.Handlers;

with Ada.Text_IO; use Ada.Text_IO;
with Gtk.Widget;

procedure Test_Display_Switch is
   Window : Gtk_Window;
   Button : Gtk_Button;

   procedure On_Click
     (Button : access Gtk.Widget.Gtk_Widget_Record'Class) is
   begin
      Destroy (Window);
      Gtk.Main.Main_Quit;
   end On_Click;

begin
   Put_Line ("Enter the display used to initialize GtkAda: ");

   declare
      S : constant String := Get_Line;
   begin
      Put_Line ("Initializing with '" & S & "'");

      GNAT.OS_Lib.Setenv ("DISPLAY", S);

      --  Initialize GtkAda.
      Gtk.Main.Set_Locale;
      Gtk.Main.Init;

      Put_Line ("Initialization done.");
   end;

   New_Line;

   loop
      Put_Line ("Enter the display used to display the window: ");
      declare
         S    : constant String := Get_Line;
         Disp : Gdk.Display.Gdk_Display;
      begin
         Put_Line ("Displaying on '" & S & "'");

         --  Change the display
         Disp := Gdk.Display.Open (S);
         Gdk.Display_Manager.Set_Default_Display
           (Display_Manager_Get,  Disp);

         --  Create a window with a button
         Gtk_New (Window);
         Gtk_New (Button,
                  "hello on display '"
                  & S &  "' (click to select another display");
         Add (Window, Button);

         --  Connect the button to the callback
         Gtkada.Handlers.Widget_Callback.Connect
           (Button, "clicked", On_Click'Unrestricted_Access);

         --  Show the window
         Show_All (Window);

         --  Launch a main loop.
         Gtk.Main.Main; --  The program waits here until the button is pressed
      end;
   end loop;
end Test_Display_Switch;
