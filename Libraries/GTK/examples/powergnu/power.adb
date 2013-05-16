with Gtk; use Gtk;
with Gtk.Main;
with Powergnu_Pkg; use Powergnu_Pkg;
with Powergnu_Pkg.Callbacks; use Powergnu_Pkg.Callbacks;
with Ada.Command_Line; use Ada.Command_Line;

procedure Power is
begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk_New (Powergnu);
   Show_All (Powergnu);

   --  Presentation file on the command line ? Load it

   if Argument_Count = 1 then
      Load_File (Powergnu, Argument (1));
   else
      Load_File (Powergnu, "file.tst");
   end if;

   Gtk.Main.Main;
end Power;
