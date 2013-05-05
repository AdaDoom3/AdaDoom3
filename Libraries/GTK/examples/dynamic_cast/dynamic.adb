--  This program shows how to dynamically change the label of a
--  button, when the label was created automatically by gtk+.
--  This program will change all the labels contained within the
--  button. We use a universal function for that, that automatically
--  goes through all the children and modifies all the labels.
--
--  This program makes use of the capacity of GtkAda to fully
--  convert a C widget to an Ada widget. This requires the
--  'with Gtk.Type_Conversion' below.

with Glib;         use Glib;
with Gdk.Bitmap;   use Gdk.Bitmap;
with Gdk.Pixmap;   use Gdk.Pixmap;
with Gtk.Box;      use Gtk.Box;
with Gtk.Button;   use Gtk.Button;
with Gtk.Enums;    use Gtk.Enums;
with Gtk.Label;    use Gtk.Label;
with Gtk.Main;     use Gtk.Main;
with Gtk.Handlers; use Gtk.Handlers;
with Gtk.Style;    use Gtk.Style;
with Gtk.Pixmap;   use Gtk.Pixmap;
with Gtk.Widget;   use Gtk.Widget;
with Gtk.Window;   use Gtk.Window;
with Gtk;          use Gtk;

with Gtk.Type_Conversion;
--  This is to enable the exact conversion from C widgets to Ada
--  widgets.

procedure Dynamic is

   Num    : Integer := 0;

   --  This will change the label of the button given in argument.
   --  It will also work if the button contains both a pixmap and
   --  a label. In that case, we simply go recursively through all the
   --  children until we find one or more labels.
   --
   --  Note the following :
   --  The label inside the button was created automatically, by the
   --  C part of gtk+, not from Ada.
   --  The button could also contain a box, or a pixmap. GtkAda creates
   --  automatically the correspondig Ada widget.
   --  It would be possible to dispatch on the 'Tag too for instance.

   procedure Change_Label (Button : access Gtk_Button_Record'Class) is
      use Widget_List;

      --  A button has a single child
      Child : Gtk_Widget := Get_Data (Children (Button));
   begin
      if Child.all in Gtk_Label_Record'Class then
         Set_Text (Gtk_Label (Child), "Num=" & Integer'Image (Num));
         Num := Num + 1;

      elsif Child.all in Gtk_Box_Record'Class then
         declare
            List2  : Gtk.Widget.Widget_List.Glist;
            Child2 : Gtk_Widget;
         begin
            List2 := Children (Gtk_Box (Child));
            while List2 /= Widget_List.Null_List loop
               Child2 := Widget_List.Get_Data (List2);
               if Child2.all in Gtk_Label_Record'Class then
                  Set_Text (Gtk_Label (Child2), "Num=" & Integer'Image (Num));
                  Num := Num + 1;
               end if;
               List2 := Next (List2);
            end loop;
         end;
      end if;
   end Change_Label;

   function On_Main_Window_Delete_Event
     (Object : access Gtk_Window_Record'Class)
      return Boolean
   is
      pragma Unreferenced (Object);
   begin
      Gtk.Main.Gtk_Exit (0);
      return True;
   end On_Main_Window_Delete_Event;

   package Widget_Cb is new Handlers.Callback (Gtk_Button_Record);
   package Window_Cb is new Handlers.Return_Callback
      (Gtk_Window_Record, Boolean);

   Window    : Gtk_Window;
   Box1,
   Box2      : Gtk_Box;
   Button,
   Button1,
   Button2   : Gtk_Button;
   Label     : Gtk_Label;
   Pixmap    : Gdk_Pixmap;
   Mask      : Gdk_Bitmap;
   PixmapWid : Gtk_Pixmap;

begin
   Gtk.Main.Init;
   Gtk.Type_Conversion.Init;

   Gtk_New (Window, Window_Toplevel);
   Set_Title (Window, "button with text");
   Set_Border_Width (Window, Border_Width => 0);
   Realize (Window);

   Gtk_New_Vbox (Box1, False, 0);
   Add (Window, Box1);
   Show (Box1);

   --  Create a button with only a label in it.
   --  The label is created automatically by gtk+

   Gtk_New (Button, "Button 1");
   Pack_Start (Box1, Button, False, False, 0);
   Show (Button);

   --  Create a button with both a pixmap and a label.
   --  We have to create the content of the button (box) ourselves

   Gtk_New (Button2);
   Pack_Start (Box1, Button2, False, False, 0);

   Gtk_New_Vbox (Box2, False, 10);
   Create_From_Xpm (Pixmap, Get_Window (Window), Mask,
                    Get_Bg (Get_Style (Button2), State_Normal), "test.xpm");
   Gtk_New (PixmapWid, Pixmap, Mask);
   Add (Box2, PixmapWid);

   Gtk_New (Label, "Button 2");
   Add (Box2, Label);
   Show (Label);
   Gtk_New (Label, "Button 2.2");
   Add (Box2, Label);
   Show (Label);

   Add (Button2, Box2);
   Show (PixmapWid);
   Show (Box2);
   Show (Button2);

   --  Create the two action buttons

   Gtk_New (Button1, "Change Button 1");
   Widget_Cb.Object_Connect
     (Button1, "clicked",
      Widget_Cb.To_Marshaller (Change_Label'Access),
      Slot_Object => Button);
   Pack_Start (Box1, Button1, True, True, 0);
   Show (Button1);

   Gtk_New (Button1, "Change Button 2");
   Widget_Cb.Object_Connect
     (Button1, "clicked",
      Widget_Cb.To_Marshaller (Change_Label'Access),
      Slot_Object => Button2);
   Pack_Start (Box1, Button1, True, True, 0);
   Show (Button1);

   Window_Cb.Connect
     (Window, "delete_event",
      Window_Cb.To_Marshaller (On_Main_Window_Delete_Event'Access));

   Show (Window);

   Gtk.Main.Main;
end Dynamic;
