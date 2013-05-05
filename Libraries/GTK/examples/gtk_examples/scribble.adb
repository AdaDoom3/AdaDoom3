with Glib;             use Glib;
with Gdk;
with Gdk.Drawable;     use Gdk.Drawable;
with Gdk.Event;        use Gdk.Event;
with Gdk.Font;         use Gdk.Font;
with Gdk.Pixmap;       use Gdk.Pixmap;
with Gdk.Rectangle;    use Gdk.Rectangle;
with Gdk.Types;        use Gdk.Types;
with Gdk.Window;       use Gdk.Window;
with Gtk;              use Gtk;
with Gtk.Box;          use Gtk.Box;
with Gtk.Button;       use Gtk.Button;
with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Main;         use Gtk.Main;
with Gtk.Rc;
with Gtk.Handlers;     use Gtk.Handlers;
with Gtk.Style;        use Gtk.Style;
with Gtk.Window;       use Gtk.Window;

procedure Scribble is

   Pixmap     : Gdk_Pixmap;
   Font       : Gdk_Font;
   Window     : Gtk_Window;

   package Configured is new Gtk.Handlers.Return_Callback
     (Widget_Type => Gtk_Drawing_Area_Record,
      Return_Type => Boolean);
   package Destroyed is new Gtk.Handlers.Callback
     (Widget_Type => Gtk_Window_Record);

   ---------------------
   -- Configure_Event --
   ---------------------

   function Configure_Event
     (Drawing_Area : access Gtk_Drawing_Area_Record'Class)
      return Boolean
   is
      Win    : Gdk_Window;
      Width  : Gint;
      Height : Gint;

   begin
      Win := Get_Window (Drawing_Area);
      Get_Size (Win, Width, Height);

      Gdk.Pixmap.Gdk_New (Pixmap, Win, Width, Height, -1);
      Draw_Rectangle (Pixmap, Get_White (Get_Style (Drawing_Area)),
                      True, 0, 0, Width, Height);
      return True;
   end Configure_Event;

   ------------------
   -- Expose_Event --
   ------------------

   function Expose_Event
     (Drawing_Area : access Gtk_Drawing_Area_Record'Class;
      Event : in Gdk.Event.Gdk_Event)
      return Boolean
   is
      Area : Gdk_Rectangle := Get_Area (Event);
   begin
      --  Ada.Text_Io.Put_Line("exposing..." &
      --                       Gint'Image(Area.X) &
      --                       Gint'Image(Area.Y) &
      --                       Gint'Image(Gint(Area.Width)) &
      --                       Gint'Image(Gint(Area.Height))&
      --                       Integer'Image(Bogus));

      Draw_Pixmap (Get_Window (Drawing_Area),
                   Get_Fg_GC (Get_Style (Drawing_Area), State_Normal),
                   Pixmap, Area.X, Area.Y, Area.X, Area.Y,
                   Gint (Area.Width), Gint (Area.Height));
      return True;
   end Expose_Event;

   ----------------
   -- Draw_Brush --
   ----------------

   procedure Draw_Brush
     (Widget : access Gtk_Drawing_Area_Record'Class;
      X, Y   :        Gint)
   is
      Message       : String := "Moo";
      Height, Width : Gint;
      Cx, Cy        : Gint;
   begin
      Width := String_Width (Font, Message);
      Height := String_Height (Font, Message);
      Cx := X - Width /2;
      Cy := Y + Height /2;

      Draw_Text (Pixmap, Font, Get_Black (Get_Style (Widget)),
                 Cx, Cy, Message);

      Draw_Arc (Pixmap, Get_Black (Get_Style (Widget)), False,
                X - 15, Y - 15, 2 * 15, 2 * 15, 0, 360 * 64);

      Draw (Widget);
   end Draw_Brush;

   ------------------------
   -- Button_Press_Event --
   ------------------------

   function Button_Press_Event
     (Widget : access Gtk_Drawing_Area_Record'Class;
      Event  : in     Gdk_Event)
      return Boolean
   is
      use type Gdk.Pixmap.Gdk_Pixmap;
   begin
      if Get_Button (Event) = 1 and Pixmap /= Null_Pixmap then
         Draw_Brush (Widget, Gint (Get_X (Event)), Gint (Get_Y (Event)));
      end if;
      return True;
   end Button_Press_Event;

   -------------------------
   -- Motion_Notify_Event --
   -------------------------

   function Motion_Notify_Event
     (Widget : access Gtk_Drawing_Area_Record'Class;
      Event  : in     Gdk_Event)
      return Boolean
   is
      X, Y   : Gint;
      State  : Gdk_Modifier_Type;
      Result : Gdk_Window;

      use type Gdk.Pixmap.Gdk_Pixmap;
   begin
      if Get_Is_Hint (Event) then
         Get_Pointer (Get_Window (Widget), X, Y, State, Result);
      else
         X := Gint (Get_X (Event));
         Y := Gint (Get_Y (Event));
         State := Get_State (Event);
      end if;

      if (State and Button1_Mask) /= 0 and Pixmap /= Null_Pixmap then
         Draw_Brush (Widget, X, Y);
      end if;
      return True;
   end Motion_Notify_Event;

   ---------
   -- Bye --
   ---------

   procedure Bye (Window : access Gtk.Window.Gtk_Window_Record'Class) is
      pragma Warnings (Off, Window);
   begin
      Gtk.Main.Main_Quit;
   end Bye;

   Button : Gtk_Button;
   Vbox : Gtk_Box;
   Drawing_Area : Gtk_Drawing_Area;

begin
   Gtk.Main.Init;
   Gtk.Rc.Parse ("testgtkrc");
   Gtk_New (Window, Window_Toplevel);
   Set_Title (Window, "Test Input");
   Set_Border_Width (Window, Border_Width => 5);
   Destroyed.Connect (Window, "destroy", Destroyed.To_Marshaller (Bye'Access));

   Gtk_New_Vbox (Vbox, Homogeneous => False, Spacing => 0);
   Add (Window, Vbox);
   Show (Vbox);

   Gtk_New (Drawing_Area);
   Size (Drawing_Area, 200, 200);
   Pack_Start (In_Box => Vbox, Child => Drawing_Area);
   Show (Drawing_Area);
   Unrealize (Drawing_Area);

   Set_Events (Drawing_Area, Exposure_Mask or Leave_Notify_Mask or
               Button_Press_Mask or Pointer_Motion_Mask or
               Pointer_Motion_Hint_Mask);

   Configured.Connect (Widget => Drawing_Area,
                       Name   => "expose_event",
                       Marsh  => Configured.To_Marshaller
                          (Expose_Event'Access));

   Configured.Connect (Widget => Drawing_Area,
                       Name   => "motion_notify_event",
                       Marsh  => Configured.To_Marshaller
                          (Motion_Notify_Event'Access));

   Configured.Connect (Widget => Drawing_Area,
                       Name   => "configure_event",
                       Marsh  => Configured.To_Marshaller
                          (Configure_Event'Access));

   Configured.Connect (Widget => Drawing_Area,
                       Name   => "button_press_event",
                       Marsh  => Configured.To_Marshaller
                          (Button_Press_Event'Access));

   Gtk_New (Button, "Done Scribblin'");
   Pack_Start (Vbox, Button, Expand => False, Fill => False);
   Destroyed.Object_Connect (Button,
                             "clicked",
                             Destroyed.To_Marshaller (Bye'Access),
                             Slot_Object => Window);

   Load (Font,
         "-adobe-helvetica-medium-o-normal--14-140-75-75-p-78-iso8859-1");

   Show_All (Window);
   Gtk.Main.Main;
end Scribble;

