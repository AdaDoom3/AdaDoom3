with Gtk.Separator;
with Gtk.Enums;

package body My_Dialog is

   procedure Gtk_New (Dialog : out My_Dialog) is
   begin
      Dialog := new My_Dialog_Record;
      Initialize (Dialog);
   end Gtk_New;

   procedure Initialize (Dialog : access My_Dialog_Record'Class) is
      Sep : Gtk.Separator.Gtk_Separator;
   begin
      Gtk.Window.Initialize (Dialog, Gtk.Enums.Window_Toplevel);

      Gtk.Box.Gtk_New_Vbox (Dialog.Vbox, False, 0);
      Add (Dialog, Dialog.Vbox);
      Gtk.Box.Show (Dialog.Vbox);

      Gtk.Box.Gtk_New_Hbox (Dialog.Action_Area, True, 5);
      Gtk.Box.Set_Border_Width (Dialog.Action_Area, 10);
      Gtk.Box.Pack_End (Dialog.Vbox, Dialog.Action_Area, False, True, 0);
      Gtk.Box.Show (Dialog.Action_Area);

      Gtk.Separator.Gtk_New_Hseparator (Sep);
      Gtk.Box.Pack_End (Dialog.Vbox, Sep, False, True, 0);
      Gtk.Separator.Show (Sep);
   end Initialize;

end My_Dialog;
