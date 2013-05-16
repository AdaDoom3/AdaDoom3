with Gtk.Adjustment, Gtk.Label, Gtk.Window, Gtk.Handlers;

package Dial_Handlers is

   procedure Value_Changed
     (Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class;
      Label : Gtk.Label.Gtk_Label);

   procedure Destroy (Window : access Gtk.Window.Gtk_Window_Record'Class);

   package Adjustment_Cb is new Gtk.Handlers.User_Callback
     (Gtk.Adjustment.Gtk_Adjustment_Record, Gtk.Label.Gtk_Label);

   package Window_Cb is new Gtk.Handlers.Callback
     (Gtk.Window.Gtk_Window_Record);

end Dial_Handlers;
