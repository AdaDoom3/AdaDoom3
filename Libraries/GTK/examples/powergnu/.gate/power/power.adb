with Gtk; use Gtk;
with Gtk.Main;
with Gtk.Widget; use Gtk.Widget;
with Powergnu_Pkg; use Powergnu_Pkg;

procedure Power is
begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk_New (Powergnu);
   Show_All (Powergnu);
   Gtk.Main.Main;
end Power;
