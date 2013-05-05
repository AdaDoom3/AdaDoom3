-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                        Copyright (C) 2000                         --
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

with Hello2_Package; use Hello2_Package;
with Gtk.Main;
with Gtk.Box, Gtk.Window, Gtk.Button; use  Gtk.Box, Gtk.Window, Gtk.Button;

procedure Hello2 is
   Window : Gtk_Window;
   Button : Gtk_Button;
   Box1   : Gtk_Box;

begin
   --  This is called in all GtkAda applications. Arguments are parsed
   --  from the command line and are returned to the application.
   Gtk.Main.Init;

   --  Creates a new window
   Gtk.Window.Gtk_New (Window);

   --  This is a new call, which just sets the title of our new window to
   --  "Hello Buttons!"
   Gtk.Window.Set_Title (Window, "Hello Buttons!");

   --  Here we just set a handler for delete_event that immediately exits GTK.
   Return_Handlers.Connect
     (Window, "delete_event",
      Return_Handlers.To_Marshaller (Delete_Event'Access));

   --  Sets the border width of the window.
   Gtk.Window.Set_Border_Width (Window, 10);

   --  We create a box to pack widgets into. This is described in detail
   --  in the "packing" section. The box is not really visible, it
   --  is just used as a tool to arrange widgets.
   Gtk_New_Hbox (Box1, False, 0);

   --  Put the box into the main window.
   Gtk.Window.Add (Window, Box1);

   --  Creates a new button with the label "Button 1".
   Gtk_New (Button, "Button 1");

   --  Now when the button is clicked, we call the "callback" function
   --  with a pointer to "button 1" as its argument
   Handlers.Connect
     (Button, "clicked", Handlers.To_Marshaller (Hello_Callback'Access),
      new String'("button 1"));

   --  Instead of gtk_container_add, we pack this button into the invisible
   --  box, which has been packed into the window. */
   Pack_Start (Box1, Button, True, True, 0);

   --  Always remember this step, this tells GtkAda that our preparation for
   --  this button is complete, and it can now be displayed.
   Show (Button);

   --  Do these same steps again to create a second button

   --  Creates a new button with the label "Button 2".
   Gtk_New (Button, "Button 2");

   --  Call the same callback function with a different argument,
   --  passing a pointer to "button 2" instead.
   Handlers.Connect
     (Button, "clicked", Handlers.To_Marshaller (Hello_Callback'Access),
      new String'("button 2"));

   --  Instead of gtk_container_add, we pack this button into the invisible
   --  box, which has been packed into the window. */
   Pack_Start (Box1, Button, True, True, 0);

   --  The order in which we show the buttons is not really important, but I
   --  recommend showing the window last, so it all pops up at once.
   Show (Button);

   Show (Box1);

   Show (Window);

   --  All GtkAda applications must have a Main. Control ends here
   --  and waits for an event to occur (like a key press or mouse event).
   Gtk.Main.Main;
end Hello2;
