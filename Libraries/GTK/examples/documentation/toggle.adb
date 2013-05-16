--  This example creates a toggle button with a pixmap in it

with Gtk.Toggle_Button, Gdk.Pixmap, Gdk.Bitmap, Gtk.Pixmap;
with Gtk.Style, Gtk.Enums;

procedure Toggle is
   Toggle    : Gtk.Toggle_Button.Gtk_Toggle_Button;
   Style     : Gtk.Style.Gtk_Style;
   Pixmap    : Gdk.Pixmap.Gdk_Pixmap;
   Mask      : Gdk.Bitmap.Gdk_Bitmap;
   PixmapWid : Gtk.Pixmap.Gtk_Pixmap;
begin
   --  Do not specify a label
   Gtk.Toggle_Button.Gtk_New (Toggle);

   Style := Gtk.Toggle_Button.Get_Style (Toggle);
   Gdk.Pixmap.Create_From_Xpm
     (Pixmap,
      Gtk.Toggle_Button.Get_Window (Toggle),
      Mask,
      Gtk.Style.Get_Bg (Style, Gtk.Enums.State_Normal),
      "icon.xpm");
   Gtk.Pixmap.Gtk_New (PixmapWid, Pixmap, Mask);

   --  Add the pixmap to the button
   Gtk.Toggle_Button.Add (Toggle, PixmapWid);
end Toggle;
