
--  This example shows how you can connect a handler that returns a value to
--  a widget.
--  The example chosen is "delete_event", that returns either True or False,
--  depending on whether we should allow the window to be closed by the window
--  manager or not.

with Gtk.Main;
with Gtk.Rc;
with Gtk.Handlers;
with Gdk.Event;      use Gdk.Event;
with Gtk.Enums;      use Gtk.Enums;
with Gtk.Button;     use Gtk.Button;
with Gtk.Window;     use Gtk.Window;
with Ada.Text_IO;    use Ada.Text_IO;

procedure Delete_Event is

   -------------
   -- Deleted --
   -------------

   function Deleted (B : access Gtk_Window_Record'Class;
                     Event : Gdk_Event)
                    return Boolean
   is
      pragma Unreferenced (Event);
      pragma Unreferenced (B);
   begin
      --  If you return FALSE in the "delete_event" signal handler,
      --  GTK will emit the "destroy" signal. Returning TRUE means
      --  you don't want the window to be destroyed.
      --  This is useful for popping up 'are you sure you want to quit?'
      --  type dialogs.

      Put_Line ("You can not close the window like that...");
      return True;
   end Deleted;

   ------------
   -- Delete --
   ------------

   procedure Delete (B             : access Gtk_Button_Record'Class;
                     To_Be_Deleted : Gtk_Window) is
      pragma Unreferenced (B);
   begin
      Put_Line ("Quit the application and destroy the window.");
      Destroy (To_Be_Deleted);
      Gtk.Main.Main_Quit;
   end Delete;

   package Button_Cb is new Gtk.Handlers.User_Callback
     (Gtk_Button_Record, Gtk_Window);
   package Event_Cb is new Gtk.Handlers.Return_Callback
     (Gtk_Window_Record, Boolean);

   Win : Gtk_Window;
   B   : Gtk_Button;

begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk.Rc.Parse ("testgtkrc");
   Gtk_New (Win, Window_Toplevel);
   Event_Cb.Connect (Win, "delete_event",
                     Event_Cb.To_Marshaller (Deleted'Access));

   Gtk_New (B, "Close");
   Add (Win, B);
   Button_Cb.Connect (B, "clicked",
                      Button_Cb.To_Marshaller (Delete'Access), Win);

   Show_All (Win);

   Put_Line ("Not that you can't simply close the window by clicking on the"
             & " icon at the top");
   Put_Line ("You have to explicitly press the Close button to quit the"
             & " application.");
   Put_Line ("This is implemented through a handler for the delete_event"
             & " callback.");
   Gtk.Main.Main;
end Delete_Event;
