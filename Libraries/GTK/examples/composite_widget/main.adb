with Gtk.Box;      use Gtk.Box;
with Gtk.Button;   use Gtk.Button;
with Gtk.Enums;    use Gtk.Enums;
with Gtk.Label;    use Gtk.Label;
with Gtk.Main;     use Gtk.Main;
with Gtk.Handlers; use Gtk.Handlers;
with Gtk.Widget;   use Gtk.Widget;
with Gtk.Window;   use Gtk.Window;

with My_Dialog;    use My_Dialog;

procedure Main is
   Main_W : Gtk_Window;
   Ok     : Gtk_Button;

   package Button_Cb is new Callback (Gtk_Button_Record);
   package Dialog_Cb is new Callback (Gtk_Widget_Record);
   package Main_Cb is new Return_Callback (Gtk_Widget_Record, Boolean);

   function On_Main_Window_Delete_Event
     (Object : access Gtk_Widget_Record'Class) return Boolean;
   --  Callback for delete_event

   procedure Open_Dialog (B : access Gtk_Button_Record'Class);

   function On_Main_Window_Delete_Event
     (Object : access Gtk_Widget_Record'Class) return Boolean
   is
      pragma Unreferenced (Object);
   begin
      Gtk.Main.Gtk_Exit (0);
      return True;
   end On_Main_Window_Delete_Event;

   procedure Open_Dialog (B : access Gtk_Button_Record'Class) is
      pragma Unreferenced (B);

      Dialog : My_Dialog.My_Dialog;
      Button : Gtk_Button;
      Label  : Gtk_Label;

   begin
      Gtk_New (Dialog);
      Set_Border_Width (Dialog, 10);

      Gtk_New (Label, "This dialog widget was completly written in Ada");
      Pack_Start (Dialog.Vbox, Label, True, True, 0);
      Gtk_New (Label, "You can use the standard dialog functions on it");
      Pack_Start (Dialog.Vbox, Label, True, True, 0);
      Gtk_New (Label, "like Set_Border_Width.");
      Pack_Start (Dialog.Vbox, Label, True, True, 0);
      Gtk_New (Label, "No C involved!");
      Pack_Start (Dialog.Vbox, Label, True, True, 0);

      Gtk_New (Button, "Quit");
      Pack_Start (Dialog.Action_Area, Button, True, True, 0);
      Dialog_Cb.Object_Connect
        (Button, "clicked",
         Dialog_Cb.To_Marshaller (Destroy_Cb'Access), Dialog);

      Show_All (Dialog);
   end Open_Dialog;

begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;

   Gtk_New (Main_W, Window_Toplevel);

   Gtk_New (Ok, "Click here to show a dialog");
   Add (Main_W, Ok);
   Button_Cb.Connect (Ok, "clicked",
                      Button_Cb.To_Marshaller (Open_Dialog'Access));
   Show (Ok);

   Main_Cb.Connect
     (Main_W, "delete_event",
      Main_Cb.To_Marshaller (On_Main_Window_Delete_Event'Access));

   Show (Main_W);

   Gtk.Main.Main;
end Main;

