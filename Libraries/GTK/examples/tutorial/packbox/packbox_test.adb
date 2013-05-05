with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Gtk.Main;
with Gtk.Window, Gtk.Button, Gtk.Box, Gtk.Separator, Gtk.Label, Gtk.Box;
use  Gtk.Window, Gtk.Button, Gtk.Box, Gtk.Separator, Gtk.Label, Gtk.Box;
with Packbox; use Packbox;

procedure Packbox_Test is
   Window     : Gtk_Window;
   Button     : Gtk_Button;
   Box1, Box2 : Gtk_Box;
   Separator  : Gtk_Separator;
   Label      : Gtk_Label;
   Quit_Box   : Gtk_Box;
   Which      : Integer;

begin
   Gtk.Main.Init;

   if Argument_Count /= 1 then
      Put_Line ("usage: packbox_test num, where num is 1, 2, or 3.");
      --  This just does cleanup in GtkAda and exits with an exit status of 1.
      Gtk.Main.Gtk_Exit (1);
   end if;

   Which := Integer'Value (Argument (1));

   --  Create our window

   Gtk_New (Window);

   --  You should always remember to connect the delete_event signal
   --  to the main window. This is very important for proper intuitive
   --  behavior.

   Return_Handlers.Connect
     (Window, "delete_event",
      Return_Handlers.To_Marshaller (Delete_Event'Access));
   Set_Border_Width (Window, 10);

   --  We create a vertical box (Vbox) to pack the horizontal boxes into.
   --  This allows us to stack the horizontal boxes filled with buttons one
   --  on top of the other in this vbox.

   Gtk_New_Vbox (Box => Box1, Homogeneous => False, Spacing => 0);

   --  Which example to show. These correspond to the pictures above.

   case Which is
      when 1 =>
         --  create a new label.

         Gtk_New (Label, "Gtk_New_Hbox (Homogeneous => False, Spacing => 0);");

         --  Align the label to the left side.  We'll discuss this function and
         --  others in the section on Widget Attributes.

         Set_Alignment (Label, 0.0, 0.0);

         --  Pack the label into the vertical box (vbox box1).  Remember that
         -- widgets added to a vbox will be packed one on top of the other in
         -- order.

         Pack_Start
           (In_Box  => Box1,
            Child   => Label,
            Expand  => False,
            Fill    => False,
            Padding => 0);

        --  Call our make box function

        Box2 := Make_Box
          (Homogeneous => False,
           Spacing     => 0,
           Expand      => False,
           Fill        => False,
           Padding     => 0);

        Pack_Start
          (In_Box  => Box1,
           Child   => Box2,
           Expand  => False,
           Fill    => False,
           Padding => 0);

        --  Call our make box function

        Box2 := Make_Box
          (Homogeneous => False,
           Spacing     => 0,
           Expand      => True,
           Fill        => False,
           Padding     => 0);

        Pack_Start
          (In_Box  => Box1,
           Child   => Box2,
           Expand  => False,
           Fill    => False,
           Padding => 0);

        Box2 := Make_Box
          (Homogeneous => False,
           Spacing     => 0,
           Expand      => True,
           Fill        => True,
           Padding     => 0);

        Pack_Start
          (In_Box  => Box1,
           Child   => Box2,
           Expand  => False,
           Fill    => False,
           Padding => 0);

        --  Creates a separator, we'll learn more about these later,
        --  but they are quite simple.

        Gtk_New_Hseparator (Separator);

        --  Pack the separator into the vbox. Remember each of these
        --  widgets is being packed into a vbox, so they'll be stacked
        --  vertically.

        Pack_Start
          (In_Box  => Box1,
           Child   => Separator,
           Expand  => False,
           Fill    => True,
           Padding => 5);

        --  Create another new label.

        Gtk_New (Label, "Gtk_New_Hbox (Homogeneous => True, Spacing => 0);");
        Set_Alignment (Label, 0.0, 0.0);
        Pack_Start (Box1, Label, Expand => False, Fill => False, Padding => 0);

        Box2 := Make_Box (True, 0, True, False, 0);
        Pack_Start (Box1, Box2, Expand => False, Fill => False, Padding => 0);

        Box2 := Make_Box (True, 0, True, True, 0);
        Pack_Start (Box1, Box2, Expand => False, Fill => False, Padding => 0);

        --  Another new separator.
        Gtk_New_Hseparator (Separator);

        Pack_Start (Box1, Separator,
          Expand => False, Fill => True, Padding => 5);

      when 2 =>
         --  Create a new label, remember Box1 is a Vbox as created
         --  near the beginning of Packbox_Test

         Gtk_New (Label,
           "Gtk_New_Hbox (Homogeneous => False, Spacing => 10);");
         Set_Alignment (Label, 0.0, 0.0);
         Pack_Start
           (Box1, Label, Expand => False, Fill => False, Padding => 0);

         Box2 := Make_Box
           (Homogeneous => False,
            Spacing     => 10,
            Expand      => True,
            Fill        => False,
            Padding     => 0);
         Pack_Start (Box1, Box2, Expand => False, Fill => False, Padding => 0);

         Box2 := Make_Box
           (Homogeneous => False,
            Spacing     => 10,
            Expand      => True,
            Fill        => True,
            Padding     => 0);
         Pack_Start (Box1, Box2, Expand => False, Fill => False, Padding => 0);

         Gtk_New_Hseparator (Separator);
         Pack_Start
           (Box1, Separator,
            Expand => False, Fill => True, Padding => 5);

         Gtk_New (Label, "Gtk_New_Hbox (Homogeneous => False, Spacing => 0);");
         Set_Alignment (Label, 0.0, 0.0);
         Pack_Start
           (Box1, Label, Expand => False, Fill => False, Padding => 0);

         Box2 := Make_Box
           (Homogeneous => False,
            Spacing     => 0,
            Expand      => True,
            Fill        => False,
            Padding     => 10);
         Pack_Start (Box1, Box2, Expand => False, Fill => False, Padding => 0);

         Box2 := Make_Box
           (Homogeneous => False,
            Spacing     => 0,
            Expand      => True,
            Fill        => True,
            Padding     => 10);
         Pack_Start (Box1, Box2, Expand => False, Fill => False, Padding => 0);

         Gtk_New_Hseparator (Separator);

         Pack_Start
           (Box1, Separator, Expand => False, Fill => True, Padding => 5);

      when 3 =>
         --  This demonstrates the ability to use Pack_End to
         --  right justify widgets. First, we create a new box as before.

         Box2 := Make_Box
           (Homogeneous => False,
            Spacing     => 0,
            Expand      => False,
            Fill        => False,
            Padding     => 0);

         --  Create the label that will be put at the end.

         Gtk_New (Label, "end");

         --  Pack it using Pack_End, so it is put on the right
         --  side of the Hbox created in the Make_Box() call.

         Pack_End (Box2, Label, Expand => False, Fill => False, Padding => 0);

         --  Pack Box2 into Box1 (the Vbox remember ? :)

         Pack_Start (Box1, Box2, Expand => False, Fill => False, Padding => 0);

         --  A separator for the bottom.

         Gtk_New_Hseparator (Separator);

         --  This explicitly sets the separator to 400 pixels wide by 5 pixels
         --  high. This is so the hbox we created will also be 400 pixels wide,
         --  and the "end" label will be separated from the other labels in the
         --  hbox. Otherwise, all the widgets in the hbox would be packed as
         --  close together as possible.

         Set_Usize (Separator, 400, 5);

         --  pack the separator into the Vbox (Box1) created near the start
         --  of Packbox_Test

         Pack_Start
           (Box1, Separator, Expand => False, Fill => True, Padding => 5);

      when others =>
         raise Program_Error;
   end case;

   --  Create another new Hbox.. remember we can use as many as we need!
   Gtk_New_Hbox (Quit_Box, Homogeneous => False, Spacing => 0);

   --  Our quit button.
   Gtk_New (Button, "Quit");

   --  Setup the signal to terminate the program when the button is clicked
   Handlers.Object_Connect
     (Button, "clicked",
      Handlers.To_Marshaller (Quit'Access),
      Window);

   --  Pack the button into the quitbox.

   Pack_Start (Quit_Box, Button, Expand => True, Fill => False, Padding => 0);

   --  pack the quit box into the Vbox (Box1)
   Pack_Start (Box1, Quit_Box, False, False, 0);

   --  Pack the Vbox (Box1) which now contains all our widgets, into the
   --  main window.
   Add (Window, Box1);

   --  Show the window and all its children at once
   Show_All (Window);

   --  And of course, our main function.
   Gtk.Main.Main;

   --  Control returns here when Gtk.Main.Main_Quit is called, but not when
   --  Gtk_Exit is used.

   Put_Line ("Exiting of Packbox_Test");
end Packbox_Test;
