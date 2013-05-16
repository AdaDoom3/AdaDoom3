with Anim_Task; use Anim_Task;
with Gtk.Main; use Gtk.Main;

procedure Anim is
begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Anim_Task.Init;
end Anim;
