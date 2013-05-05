with Gtk.Enums;  use Gtk.Enums;
with Gtk.Handlers;
with Gtk.Main;   use Gtk.Main;
with Gtk.Window; use Gtk.Window;
with Calendar_Combo; use Calendar_Combo;

procedure Main is
   Win : Gtk_Window;
   Cal : Gtk_Calendar_Combo;

   function On_Main_Window_Delete_Event
     (Object : access Gtk_Window_Record'Class) return Boolean;
   --  Handler for the delete_event signal

   function On_Main_Window_Delete_Event
     (Object : access Gtk_Window_Record'Class)
      return Boolean
   is
      pragma Unreferenced (Object);
   begin
      Gtk.Main.Gtk_Exit (0);
      return True;
   end On_Main_Window_Delete_Event;

   package Window_Cb is new Gtk.Handlers.Return_Callback
     (Gtk_Window_Record, Boolean);

begin
   Gtk.Main.Init;

   Gtk_New (Win, Window_Toplevel);

   Gtk_New (Cal);
   Add (Win, Cal);

   Window_Cb.Connect
     (Win, "delete_event",
      Window_Cb.To_Marshaller (On_Main_Window_Delete_Event'Access));

   Show_All (Win);

   Gtk.Main.Main;
end Main;
