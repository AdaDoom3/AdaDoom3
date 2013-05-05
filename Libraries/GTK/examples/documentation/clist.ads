with Gtk.Clist, Glib;  use Gtk.Clist, Glib;

package Clist is

   procedure Hide_All_But_One (Clist      : access Gtk_Clist_Record'Class;
                               New_Column : Gint);

end Clist;
