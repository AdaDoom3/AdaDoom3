
with Glib;              use Glib;
with Double_Buffer;     use Double_Buffer;
with Gdk.Color;         use Gdk.Color;
with Gdk.Drawable;      use Gdk.Drawable;
with Gtk.Drawing_Area;  use Gtk.Drawing_Area;
with Gdk.GC;            use Gdk.GC;
with Gtk.Window;        use Gtk.Window;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Box;           use Gtk.Box;
with Gtk.Handlers;      use Gtk.Handlers;
pragma Elaborate_All (Gtk.Handlers);
with Gtk.Label;         use Gtk.Label;
with Gtk.Widget;        use Gtk.Widget;
with Gtk.Main;          use Gtk.Main;
pragma Elaborate_All (Gtk.Main);

with Ada.Text_IO;   use Ada.Text_IO;

package body Anim_Timeout is

   White_Gc : Gdk.GC.Gdk_GC;
   Black_Gc : Gdk.GC.Gdk_GC;

   X_Pos : Gint := 10;

   package Void_Cb is new Gtk.Handlers.Callback (Gtk_Window_Record);
   package Gint_Timeout is new Gtk.Main.Timeout (Gtk_Drawing_Area);

   ------------------
   -- Draw_Complex --
   ------------------

   procedure Draw_Complex (Pixmap : Gdk_Drawable) is
   begin
      Draw_Rectangle (Pixmap, White_Gc, Filled => True,
                      X     => 0,   Y      => 0,
                      Width => 400, Height => 400);

      for J in Gint'(1) .. 30 loop
         Draw_Rectangle (Pixmap, Black_Gc, Filled => False,
                         X     => X_Pos,  Y      => 30 + J * 2,
                         Width => X_Pos + 100, Height => 100);
         Draw_Rectangle (Pixmap, Black_Gc, Filled => False,
                         X     => X_Pos + 20,  Y      => 60 + J * 2,
                         Width => X_Pos + 60, Height => 80);
         Draw_Rectangle (Pixmap, Black_Gc, Filled => False,
                      X     => X_Pos + 30,  Y      => 50 + J * 2,
                         Width => X_Pos + 80, Height => 90);
         Draw_Rectangle (Pixmap, Black_Gc, Filled => False,
                         X     => X_Pos - 20,  Y      => 120 + J * 2,
                         Width => X_Pos + 80, Height => 190);
      end loop;

      X_Pos := (X_Pos + 1) mod 140;
   end Draw_Complex;

   -------------------------
   -- Draw_Complex_Buffer --
   -------------------------

   function Draw_Complex_Buffer (Area : Gtk_Drawing_Area) return Boolean is
      Buffer : Gtk_Double_Buffer := Gtk_Double_Buffer (Area);
   begin
      Draw_Complex (Get_Pixmap (Buffer));
      Double_Buffer.Draw (Buffer);
      return True;
   end Draw_Complex_Buffer;

   -----------------------
   -- Draw_Complex_Area --
   -----------------------

   function Draw_Complex_Area (Area : Gtk_Drawing_Area) return Boolean is
   begin
      Draw_Complex (Get_Window (Area));
      return True;
   end Draw_Complex_Area;

   ----------
   -- Quit --
   ----------

   procedure Quit (Win : access Gtk_Window_Record'Class) is
      pragma Warnings (Off, Win);
   begin
      Gtk.Main.Gtk_Exit (0);
   end Quit;

   ----------
   -- Init --
   ----------

   procedure Init is
      Win    : Gtk_Window;
      Area   : Gtk_Drawing_Area;
      Buffer : Gtk_Double_Buffer;
      Vbox,
      Hbox   : Gtk_Box;
      Label  : Gtk_Label;
      Id     : Timeout_Handler_Id;

   begin
      Put_Line ("This demo shows how you can use a Double_Buffer widget");
      Put_Line (" to provide flicker-free animations in your applications.");
      Put_Line ("The code is almost the same as with a Gtk_Drawing_Area.");
      Put_Line (" (the drawing routines are exactly the same in this demo)");
      Put_Line (" except that in one case you draw in an off-screen pixmap");
      Put_Line (" That you need to copy to the screen when you are ready.");
      New_Line;
      Put_Line ("The animation is done thanks to GtkAda Timeouts");

      --  Double buffer demo

      Gtk_New (Win, Window_Toplevel);
      Set_Title (Win, "Animation demo");
      Void_Cb.Connect (Win, "destroy", Void_Cb.To_Marshaller (Quit'Access));

      Gtk_New_Hbox (Hbox, Homogeneous => True, Spacing => 10);

      Gtk_New_Vbox (Vbox, Homogeneous => False, Spacing => 0);
      Gtk_New (Label, "With double-buffering");
      Gtk_New (Buffer);
      Set_USize (Buffer, 200, 200);
      Pack_Start (Vbox, Label);
      Pack_Start (Vbox, Buffer);
      Pack_Start (Hbox, Vbox);

      Gtk_New_Vbox (Vbox, Homogeneous => False, Spacing => 0);
      Gtk_New (Label, "No double-buffering");
      Gtk_New (Area);
      Set_USize (Area, 200, 200);
      Pack_Start (Vbox, Label);
      Pack_Start (Vbox, Area);
      Pack_Start (Hbox, Vbox);

      Add (Win, Hbox);

      Show_All (Win);

      --  The window needs to be created before creating the GCs
      Gdk.GC.Gdk_New (White_Gc, Get_Window (Buffer));
      Gdk.GC.Set_Foreground
        (White_Gc, Gdk.Color.White (Gtk.Widget.Get_Default_Colormap));

      Gdk.GC.Gdk_New (Black_Gc, Get_Window (Buffer));
      Gdk.GC.Set_Foreground
        (Black_Gc, Gdk.Color.Black (Gtk.Widget.Get_Default_Colormap));

      Id := Gint_Timeout.Add (10, Draw_Complex_Buffer'Access,
                              Gtk_Drawing_Area (Buffer));
      Id := Gint_Timeout.Add (10, Draw_Complex_Area'Access,
                              Area);
   end Init;

end Anim_Timeout;
