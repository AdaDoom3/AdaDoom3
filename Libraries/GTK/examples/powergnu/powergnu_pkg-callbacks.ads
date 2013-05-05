with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Powergnu_Pkg.Callbacks is
   function On_Powergnu_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;
   --  Delete event handler

   function On_Powergnu_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;
   --  Key press event handler

   procedure Load_File
     (Power : access Powergnu_Record'Class;
      Name  : String);
   --  Load image Name from disk

end Powergnu_Pkg.Callbacks;
