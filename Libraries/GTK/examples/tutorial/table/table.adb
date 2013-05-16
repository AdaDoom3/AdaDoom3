with Gtk.Main;

with Ada.Text_IO;
use  Ada.Text_IO;

package body Table is

   procedure Callback
     (Widget : access Gtk_Widget_Record'Class;
      Data   : String_Access)
   is
      pragma Unreferenced (Widget);
   begin
      Put_Line ("Hello again - " & Data.all & " was pressed");
   end Callback;

   function Delete_Event
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      pragma Unreferenced (Widget);
      pragma Unreferenced (Event);
   begin
      Gtk.Main.Main_Quit;
      return False;
   end Delete_Event;

   procedure Quit
     (Widget : access Gtk_Widget_Record'Class;
      Data   : String_Access)
   is
      pragma Unreferenced (Widget);
   begin
      Put_Line ("Quitting - " & Data.all);
      Gtk.Main.Main_Quit;
   end Quit;

end Table;
