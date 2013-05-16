with Anim_Timeout; use Anim_Timeout;
with Gtk.Main; use Gtk.Main;

procedure Anim2 is
begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Anim_Timeout.Init;
   Gtk.Main.Main;
end Anim2;
