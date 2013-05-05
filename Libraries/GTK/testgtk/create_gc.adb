
with Glib;         use Glib;
with Gdk.GC;       use Gdk.GC;
with Gdk.Color;    use Gdk.Color;
with Gdk.Drawable; use Gdk.Drawable;
with Gdk.Event;    use Gdk.Event;
with Gdk.Font;     use Gdk.Font;
with Gdk.Types;    use Gdk.Types;
with Gtk.Frame;    use Gtk.Frame;
with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.Handlers; use Gtk.Handlers;
with Gtk.Widget;   use Gtk.Widget;

package body Create_Gc is

   package Event_Cb is new Gtk.Handlers.Return_Callback
     (Gtk_Drawing_Area_Record, Boolean);

   GC   : Gdk_GC;
   Font : Gdk_Font;
   Blue : Gdk_Color;
   Red  : Gdk_Color;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This demo shows some of the aspects of the graphic contexts"
        & " that you can set before calling the drawing functions in"
        & " @bGdk.Drawable@B."
        & ASCII.LF
        & "In all theses demos, the foreground is set to a blue color, whereas"
        & " the background is set to red.";
   end Help;

   ------------------------
   -- Draw_Text_And_Line --
   ------------------------

   procedure Draw_Text_And_Line
     (Area      : access Gtk_Drawing_Area_Record'Class;
      Y         : Gint;
      Text      : String;
      Num_Lines : Positive := 1) is
   begin
      Draw_Text
        (Drawable => Get_Window (Area),
         Font     => Font,
         GC       => GC,
         X        => 3,
         Y        => Y,
         Text     => Text);

      if Num_Lines = 1 then
         Draw_Line
           (Drawable => Get_Window (Area),
            GC       => GC,
            X1       => 10,
            Y1       => Y + 7,
            X2       => 200,
            Y2       => Y + 7);

      else
         Draw_Lines
           (Drawable => Get_Window (Area),
            GC       => GC,
            Points   => ((10,  Y + 7),
                         (200, Y + 7),
                         (250, Y + 50)));
      end if;
   end Draw_Text_And_Line;

   ------------
   -- Expose --
   ------------

   function Expose
     (Area  : access Gtk_Drawing_Area_Record'Class;
      Event : Gdk_Event)
      return Boolean
   is
      pragma Warnings (Off, Event);
   begin
      Set_Foreground (GC, Blue);
      Set_Background (GC, Red);

      Set_Line_Attributes (GC,
                           Line_Width => 8,
                           Line_Style => Line_Solid,
                           Cap_Style  => Cap_Butt,
                           Join_Style => Join_Miter);
      Draw_Text_And_Line (Area, 15, "Cap_Style = Cap_Butt");

      Set_Line_Attributes (GC,
                           Line_Width => 8,
                           Line_Style => Line_Solid,
                           Cap_Style  => Cap_Projecting,
                           Join_Style => Join_Miter);
      Draw_Text_And_Line (Area, 45, "Cap_Style = Cap_Projecting");

      Set_Line_Attributes (GC,
                           Line_Width => 8,
                           Line_Style => Line_Solid,
                           Cap_Style  => Cap_Round,
                           Join_Style => Join_Miter);
      Draw_Text_And_Line (Area, 75, "Cap_Style = Cap_Round");

      Set_Line_Attributes (GC,
                           Line_Width => 4,
                           Line_Style => Line_On_Off_Dash,
                           Cap_Style  => Cap_Butt,
                           Join_Style => Join_Miter);
      Draw_Text_And_Line (Area, 105, "Line_Style = Line_On_Off_Dash");

      Set_Line_Attributes (GC,
                           Line_Width => 4,
                           Line_Style => Line_Double_Dash,
                           Cap_Style  => Cap_Butt,
                           Join_Style => Join_Miter);
      Draw_Text_And_Line (Area, 135, "Line_Style = Line_Double_Dash");

      Set_Line_Attributes (GC,
                           Line_Width => 4,
                           Line_Style => Line_On_Off_Dash,
                           Cap_Style  => Cap_Butt,
                           Join_Style => Join_Miter);
      Set_Dashes (GC,
                  Dash_Offset => 0,
                  Dash_List   => (2, 2, 4, 4, 6, 6));
      Draw_Text_And_Line (Area, 180, "Dashes set to (2,2,4,4,6,6)");

      Set_Dashes (GC,
                  Dash_Offset => 0,
                  Dash_List   => (2, 2, 4, 4, 6));
      Draw_Text_And_Line (Area, 210, "Dashes set to (2,2,4,4,6)");

      Set_Line_Attributes (GC,
                           Line_Width => 8,
                           Line_Style => Line_Solid,
                           Cap_Style  => Cap_Butt,
                           Join_Style => Join_Miter);
      Draw_Text_And_Line (Area, 250, "Join_Style = Join_Miter", 2);

      Set_Line_Attributes (GC,
                           Line_Width => 8,
                           Line_Style => Line_Solid,
                           Cap_Style  => Cap_Butt,
                           Join_Style => Join_Round);
      Draw_Text_And_Line (Area, 300, "Join_Style = Join_Round", 2);

      Set_Line_Attributes (GC,
                           Line_Width => 8,
                           Line_Style => Line_Solid,
                           Cap_Style  => Cap_Butt,
                           Join_Style => Join_Bevel);
      Draw_Text_And_Line (Area, 350, "Join_Style = Join_Bevel", 2);

      return False;
   end Expose;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Area    : Gtk_Drawing_Area;
      Success : Boolean;

   begin
      Gtk.Frame.Set_Label (Frame, "Graphic contexts");

      Gtk_New (Area);
      Set_USize (Area, 300, 500);
      Add (Frame, Area);
      Event_Cb.Connect (Area, "expose_event",
                        Event_Cb.To_Marshaller (Expose'Access));

      Show_All (Frame);

      Gdk_New (GC, Get_Window (Area));
      Load (Font, "-*-courier-*-i-*-*-*-130-*-*-*-*-*-*");

      Blue := Gdk.Color.Parse ("Blue");
      Alloc_Color (Colormap   => Gtk.Widget.Get_Default_Colormap,
                   Color      => Blue,
                   Writeable  => False,
                   Best_Match => True,
                   Success    => Success);

      Red := Gdk.Color.Parse ("Red");
      Alloc_Color (Colormap   => Gtk.Widget.Get_Default_Colormap,
                   Color      => Red,
                   Writeable  => False,
                   Best_Match => True,
                   Success    => Success);
   end Run;

end Create_Gc;
