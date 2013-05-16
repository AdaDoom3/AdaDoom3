with Full_Test; use Full_Test;
with Gtk.Main; use Gtk.Main;

procedure Parameters is
begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Full_Test.Init;
   Gtk.Main.Main;
end Parameters;
