with Glib;
with Gdk.Window;
with Gdk.Drawable;
with Gdk.GC;
with Gdk.Font;
with Gtk.Drawing_Area;

procedure Draw (Drawing : in out Gtk.Drawing_Area.Gtk_Drawing_Area) is
   Gdkw : Gdk.Window.Gdk_Window;
   GC   : Gdk.GC.Gdk_GC;
   Font : Gdk.Font.Gdk_Font;
   use type Glib.Gint;

begin
   -- Get the Gdk window

   Gdkw := Gtk.Drawing_Area.Get_Window (Drawing);

   -- Clear the window

   Gdk.Window.Clear (Gdkw);

   -- Create a graphic context associated with this window

   Gdk.GC.Gdk_New (GC, Gdkw);

   -- Draw a line in this window

   Gdk.Drawable.Draw_Line
     (Drawable => Gdkw,
      GC => GC,
      X1 =>   0, Y1 =>   0,
      X2 => 100, Y2 => 100);

   -- Draw an arc

   Gdk.Drawable.Draw_Arc
     (Drawable => Gdkw,
      Gc       => GC,
      Filled   => True,
      X        => 100,
      Y        => 100,
      Width    => 200,
      Height   => 100,
      Angle1   => 0 * 64,
      Angle2   => 270 * 64);

   -- Ask for a given font

   Gdk.Font.Load (Font,
                  "-adobe-courier-medium-i-*-*-15-*-*-*-*-*-*-*");
   Gdk.Drawable.Draw_Text
     (Drawable    => Gdkw,
      Font        => Font,
      Gc          => GC,
      X           => 50,
      Y           => 50,
      Text        => "Hello World");
   Gdk.GC.Destroy (GC);
end Draw;
