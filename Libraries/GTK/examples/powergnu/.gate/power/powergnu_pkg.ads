with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.Status_Bar; use Gtk.Status_Bar;
with Gtk.Button; use Gtk.Button;
package Powergnu_Pkg is

   type Powergnu_Record is new Gtk_Window_Record with record
      Vbox1 : Gtk_Vbox;
      Main_Frame : Gtk_Frame;
      Drawing_Area : Gtk_Drawing_Area;
      Statusbar1 : Gtk_Statusbar;
   end record;
   type Powergnu_Access is access all Powergnu_Record'Class;

   procedure Gtk_New (Powergnu : out Powergnu_Access);
   procedure Initialize (Powergnu : access Powergnu_Record'Class);

   Powergnu : Powergnu_Access;

end Powergnu_Pkg;
