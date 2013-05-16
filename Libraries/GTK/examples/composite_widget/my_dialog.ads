with Gtk.Window;
with Gtk.Box;

package My_Dialog is

   --  This is intended to reprogram the Gtk_Dialog fully in Ada
   --  A dialog is simply a window with two areas

   type My_Dialog_Record is new Gtk.Window.Gtk_Window_Record with
      record
         Vbox        : Gtk.Box.Gtk_Box;
         Action_Area : Gtk.Box.Gtk_Box;
      end record;
   type My_Dialog is access all My_Dialog_Record'Class;

   procedure Gtk_New (Dialog : out My_Dialog);
   procedure Initialize (Dialog : access My_Dialog_Record'Class);

end My_Dialog;
