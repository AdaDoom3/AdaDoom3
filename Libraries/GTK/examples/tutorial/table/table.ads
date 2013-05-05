with Gdk.Event; use Gdk.Event;
with Gtk.Widget, Gtk.Handlers; use Gtk.Widget, Gtk.Handlers;

package Table is

   type String_Access is access all String;

   package Handlers is new Gtk.Handlers.User_Callback
     (Widget_Type => Gtk_Widget_Record,
      User_Type   => String_Access);

   package Return_Handlers is new Gtk.Handlers.Return_Callback
     (Widget_Type => Gtk_Widget_Record,
      Return_Type => Boolean);

   procedure Callback
     (Widget : access Gtk_Widget_Record'Class;
      Data   : String_Access);

   function Delete_Event
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;

   procedure Quit
     (Widget : access Gtk_Widget_Record'Class;
      Data   : String_Access);

end Table;
