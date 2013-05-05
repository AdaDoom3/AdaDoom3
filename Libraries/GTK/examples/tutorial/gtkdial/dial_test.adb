with Glib; use Glib;
with Gtk_Dial, Dial_Handlers; use Gtk_Dial, Dial_Handlers;
with Gtk.Main, Gtk.Enums; use Gtk.Enums;
with Gtk.Window, Gtk.Adjustment, Gtk.Frame, Gtk.Box, Gtk.Label;
use Gtk.Window, Gtk.Adjustment, Gtk.Frame, Gtk.Box, Gtk.Label;

procedure Dial_Test is
   Window     : Gtk_Window;
   Adjustment : Gtk_Adjustment;
   Dial       : Gtk_Dial.Gtk_Dial;
   Frame      : Gtk_Frame;
   Vbox       : Gtk_Vbox;
   Label      : Gtk_Label;

begin
   Gtk.Main.Init;

   Gtk_New (Window);
   Set_Title (Window, "Dial");

   Window_Cb.Connect
     (Window, "destroy", Window_Cb.To_Marshaller (Destroy'Access));

   Set_Border_Width (Window, 10);

   Gtk_New_Vbox (Vbox, False, 5);
   Add (Window, Vbox);

   Gtk_New (Frame);
   Set_Shadow_Type (Frame, Shadow_In);
   Pack_Start (Vbox, Frame, True, True, 0);

   Gtk_New (Adjustment, 0.0, -1.0, 1.0, 0.01, 0.1, 0.0);

   Gtk_New (Dial, Adjustment);
   Set_Update_Policy (Dial, Update_Delayed);

   Add (Frame, Dial);

   Gtk_New (Label, "0.00");
   Pack_Start (Vbox, Label, True, True, 0);

   Adjustment_Cb.Connect
     (Adjustment, "value_changed",
      Adjustment_Cb.To_Marshaller (Value_Changed'Access), Label);

   Show_All (Window);

   Gtk.Main.Main;
end Dial_Test;
