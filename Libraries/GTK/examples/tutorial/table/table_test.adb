with Table; use Table;
with Gtk.Window, Gtk.Button, Gtk.Table, Gtk.Main;
use  Gtk.Window, Gtk.Button, Gtk.Table;

procedure Table_Test is
   Window : Gtk_Window;
   Button : Gtk_Button;
   Table  : Gtk_Table;

begin
   Gtk.Main.Init;

   --  Create a new window
   Gtk_New (Window);

   --  Set the window title
   Set_Title (Window, "Table");

   --  Set a handler for delete_event that immediately
   --  exits GTK.

   Return_Handlers.Connect
     (Window,
      "delete_event",
      Return_Handlers.To_Marshaller (Delete_Event'Access));

   --  Sets the border width of the window.
   Set_Border_Width (Window, 20);

   --  Create a 2x2 table
   Gtk_New (Table, 2, 2, True);

   --  Put the table in the main window
   Add (Window, Table);

   --  Create first button
   Gtk_New (Button, "button 1");

   --  When the button is clicked, we call the "callback" function
   --  with a pointer to "button 1" as its argument

   Handlers.Connect
     (Button, "clicked", Handlers.To_Marshaller (Callback'Access),
      new String'("button 1"));

   --  Insert button 1 into the upper left quadrant of the table
   Attach_Defaults (Table, Button, 0, 1, 0, 1);

   --  Create second button

   Gtk_New (Button, "button 2");

   --  When the button is clicked, we call the "callback" function
   --  with a pointer to "button 2" as its argument
   Handlers.Connect
     (Button, "clicked",
      Handlers.To_Marshaller (Callback'Access), new String'("button 2"));

   --  Insert button 2 into the upper right quadrant of the table
   Attach_Defaults (Table, Button, 1, 2, 0, 1);

   --  Create "Quit" button
   Gtk_New (Button, "Quit");

   --  When the button is clicked, we call the "Quit" function
   --  and the program exits

   Handlers.Connect
     (Button, "clicked", Handlers.To_Marshaller (Quit'Access),
      new String'("Button Pressed"));

   --  Insert the quit button into the both
   --  lower quadrants of the table
   Attach_Defaults (Table, Button, 0, 2, 1, 2);

   Show_All (Window);

   Gtk.Main.Main;
end Table_Test;
