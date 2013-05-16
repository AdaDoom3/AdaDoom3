with Glib; use Glib;
with Gtk.Widget;
with Gtk.Box;
with Gtk.Handlers;

package Packbox is

   function Delete_Event
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) return Boolean;
   --  "delete_event" handler.
   --  Quit the main loop.

   function Make_Box
     (Homogeneous : Boolean;
      Spacing     : Gint;
      Expand      : Boolean;
      Fill        : Boolean;
      Padding     : Guint) return Gtk.Box.Gtk_Hbox;
   --  Make a new hbox filled with button-labels. Arguments for the
   --  variables we're interested are passed in to this function.
   --  We do not show the box, but do show everything inside.

   procedure Quit (Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Quit button handler ("clicked" signal).
   --  Quit the main loop.

   package Handlers is new Gtk.Handlers.Callback
     (Gtk.Widget.Gtk_Widget_Record);

   package Return_Handlers is new Gtk.Handlers.Return_Callback
     (Gtk.Widget.Gtk_Widget_Record, Boolean);

end Packbox;
