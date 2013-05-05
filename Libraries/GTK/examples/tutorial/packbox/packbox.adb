with Gtk.Main, Gtk.Button;

package body Packbox is

   use Gtk.Box, Gtk.Button;

   function Delete_Event
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) return Boolean
   is
      pragma Unreferenced (Widget);
   begin
      Gtk.Main.Main_Quit;
      return False;
   end Delete_Event;

   function Make_Box
     (Homogeneous : Boolean;
      Spacing     : Gint;
      Expand      : Boolean;
      Fill        : Boolean;
      Padding     : Guint) return Gtk.Box.Gtk_Hbox
   is
      Box    : Gtk_Box;
      Button : Gtk_Button;

   begin
      --  Create a new hbox with the appropriate homogeneous
      --  and spacing settings

      Gtk_New_Hbox (Box, Homogeneous, Spacing);

      --  Create a series of buttons with the appropriate settings

      Gtk_New (Button, "Gtk.Box.Pack");
      Pack_Start (Box, Button, Expand, Fill, Padding);
      Show (Button);

      Gtk_New (Button, "(Box,");
      Pack_Start (Box, Button, Expand, Fill, Padding);
      Show (Button);

      Gtk_New (Button, "Button,");
      Pack_Start (Box, Button, Expand, Fill, Padding);
      Show (Button);

      --  Create a button with the label depending on the value of
      --  expand.

      Gtk_New (Button, "Expand => " & Boolean'Image (Expand) & ",");

      Pack_Start (Box, Button, Expand, Fill, Padding);
      Show (Button);

      --  This is the same as the button creation for "expand"
      --  above.

      Gtk_New (Button, "Fill => " & Boolean'Image (Fill) & ",");

      Pack_Start (Box, Button, Expand, Fill, Padding);
      Show (Button);

      Gtk_New (Button, "Padding => " & Guint'Image (Padding) & ");");
      Pack_Start (Box, Button, Expand, Fill, Padding);
      Show (Button);

      return Box;
   end Make_Box;

   procedure Quit (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) is
      pragma Unreferenced (Widget);
   begin
      Gtk.Main.Main_Quit;
   end Quit;
end Packbox;
