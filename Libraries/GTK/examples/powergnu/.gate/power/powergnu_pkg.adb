with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Power; use Callbacks_Power;
with Powergnu_Pkg.Callbacks; use Powergnu_Pkg.Callbacks;

package body Powergnu_Pkg is

procedure Gtk_New (Powergnu : out Powergnu_Access) is
begin
   Powergnu := new Powergnu_Record;
   Powergnu_Pkg.Initialize (Powergnu);
end Gtk_New;

procedure Initialize (Powergnu : access Powergnu_Record'Class) is
   pragma Suppress (All_Checks);
begin
   Gtk.Window.Initialize (Powergnu, Window_Toplevel);
   Set_Events (Powergnu, 
     Key_Press_Mask);
   Return_Callback.Connect
     (Powergnu, "delete_event", On_Powergnu_Delete_Event'Access);
   Return_Callback.Connect
     (Powergnu, "key_press_event", On_Powergnu_Key_Press_Event'Access);
   Set_Title (Powergnu, "GtkAda Presentation Viewer");
   Set_Policy (Powergnu, True, True, False);
   Set_Position (Powergnu, Win_Pos_None);
   Set_Modal (Powergnu, False);
   Set_Default_Size (Powergnu, 800, 600);

   Gtk_New_Vbox (Powergnu.Vbox1, False, 0);
   Add (Powergnu, Powergnu.Vbox1);

   Gtk_New (Powergnu.Main_Frame);
   Pack_Start (Powergnu.Vbox1, Powergnu.Main_Frame, True, True, 0);
   Set_Shadow_Type (Powergnu.Main_Frame, Shadow_Etched_In);

   Gtk_New (Powergnu.Drawing_Area);
   Add (Powergnu.Main_Frame, Powergnu.Drawing_Area);

   Gtk_New (Powergnu.Statusbar1);
   Pack_Start (Powergnu.Vbox1, Powergnu.Statusbar1, False, False, 0);

end Initialize;

end Powergnu_Pkg;
