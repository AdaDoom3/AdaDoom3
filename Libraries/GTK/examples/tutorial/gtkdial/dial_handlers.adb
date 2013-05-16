with Gtk.Main;
with Ada.Float_Text_IO;

package body Dial_Handlers is

   use Gtk.Adjustment, Gtk.Label;

   procedure Value_Changed
     (Adjustment : access Gtk_Adjustment_Record'Class;
      Label : Gtk_Label)
   is
      Buffer : String (1 .. 5) := (others => ' ');
   begin
      Ada.Float_Text_IO.Put
        (Buffer, Float (Get_Value (Adjustment)), 2, 0);
      Set_Text (Label, Buffer);
   end Value_Changed;

   procedure Destroy (Window : access Gtk.Window.Gtk_Window_Record'Class) is
      pragma Unreferenced (Window);
   begin
      Gtk.Main.Gtk_Exit (0);
   end Destroy;

end Dial_Handlers;
