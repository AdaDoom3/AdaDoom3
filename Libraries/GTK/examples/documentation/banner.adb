--  This example shows how you can display a banner while your application is
--  loading

with Gtk.Window, Gtk.Enums, Gtk.Main, Gtk.Label;
use Gtk.Window,  Gtk.Enums, Gtk.Main, Gtk.Label;

procedure Banner is
   Win   : Gtk_Window;
   Label : Gtk_Label;
begin
   Gtk.Main.Init;

   Gtk_New (Win, Window_Popup);
   Set_Policy (Win,
               Allow_Shrink => False,
               Allow_Grow   => False,
               Auto_Shrink  => False);
   Set_Position (Win, Win_Pos_Center);
   Set_Size_Request (Win, 300, 300);

   Gtk_New (Label,
       "You should show a pixmap instead..."
       & ASCII.LF & "(ctrl-c in terminal to exit)");
   Add (Win, Label);

   Show_All (Win);
   Gtk.Main.Main;
end Banner;
