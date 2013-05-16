with Glib; use Glib;
with Gdk.GC;
with Gtk.Drawing_Area;

package My_Widget is

   --  A simple widget, that appears as a round target, with two zones.
   --  If prints a different message depending on where you click

   type Target_Widget_Record is new Gtk.Drawing_Area.Gtk_Drawing_Area_Record
     with private;
   type Target_Widget is access all Target_Widget_Record'Class;


   procedure Gtk_New (Widget : out Target_Widget);
   procedure Initialize (Widget : access Target_Widget_Record'Class);

private
   type Target_Widget_Record is new
     Gtk.Drawing_Area.Gtk_Drawing_Area_Record with
   record
      Gc_In  : Gdk.GC.Gdk_GC;
      Gc_Out : Gdk.GC.Gdk_GC;
      Radius : Gint;
   end record;

end My_Widget;
