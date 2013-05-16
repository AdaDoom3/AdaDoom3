
with Glib;                use Glib;
with Gdk.Color;           use Gdk.Color;
with Gdk.GC;              use Gdk.GC;
with Gdk.Drawable;        use Gdk.Drawable;
with Gtk.Adjustment;      use Gtk.Adjustment;
with Gtk.Window;          use Gtk.Window;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.Button;          use Gtk.Button;
with Gtk.Box;             use Gtk.Box;
with Gtk.Label;           use Gtk.Label;
with Gtk.Widget;          use Gtk.Widget;
with Double_Buffer;       use Double_Buffer;
with Gtk.Toggle_Button;   use Gtk.Toggle_Button;
with Gtk.Main;            use Gtk.Main;
with Gtk.Text_Buffer;     use Gtk.Text_Buffer;
with Gtk.Text_View;       use Gtk.Text_View;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;

with Gtk.Type_Conversion;
pragma Warnings (Off, Gtk.Type_Conversion);
--  So that we can get the child of a button (a Gtk_Label) even if it
--  was created at the C level

pragma Elaborate_All (Gtk.Handlers);

with Ada.Numerics.Discrete_Random;

package body Full_Test is
   package Void_Cb is new Gtk.Handlers.User_Callback
     (Widget_Type => Gtk_Toggle_Button_Record,
      User_Type   => Gtk_Double_Buffer);
   package Button_Cb is new Gtk.Handlers.Callback
     (Widget_Type => Gtk_Double_Buffer_Record);
   package Quit_Cb is new Gtk.Handlers.Callback
     (Widget_Type => Gtk_Window_Record);

   Pixmap_Width : constant Gint := 400;
   Pixmap_Height : constant Gint := 250;

   subtype My_Gint is Gint range 0 .. Gint'Min (Pixmap_Width, Pixmap_Height);
   package Gint_Random is new Ada.Numerics.Discrete_Random (My_Gint);
   Gen : Gint_Random.Generator;

   subtype Color_Index is Natural range 1 .. 5;
   Colors : Gdk_Color_Array (Color_Index'Range);
   package Color_Random is new Ada.Numerics.Discrete_Random (Color_Index);
   Gen_Color : Color_Random.Generator;

   White_Gc : Gdk.GC.Gdk_GC;
   Black_Gc : Gdk.GC.Gdk_GC;

   -----------------------
   -- Toggle_Back_Store --
   -----------------------

   procedure Toggle_Back_Store (Button : access Gtk_Toggle_Button_Record'Class;
                                Buffer : Gtk_Double_Buffer) is
   begin
      Set_Back_Store (Buffer, Get_Active (Button));
   end Toggle_Back_Store;

   -------------------
   -- Toggle_Triple --
   -------------------

   procedure Toggle_Triple (Button : access Gtk_Toggle_Button_Record'Class;
                            Buffer : Gtk_Double_Buffer) is
   begin
      Set_Triple_Buffer (Buffer, Get_Active (Button));
   end Toggle_Triple;

   -------------------
   -- Toggle_Freeze --
   -------------------

   procedure Toggle_Freeze (Button : access Gtk_Toggle_Button_Record'Class;
                            Buffer : Gtk_Double_Buffer)
   is
      Label : Gtk_Label;
   begin
      --  The following call only works thanks to
      --  the 'with Gtk.Type_Conversion', since the label is created in C
      --  and not in Ada.

      Label := Gtk_Label (Widget_List.Get_Data (Children (Button)));

      if Get_Active (Button) then
         Freeze (Buffer);
         Set_Text (Label, "Thaw");

      else
         Thaw (Buffer);
         Set_Text (Gtk_Label (Widget_List.Get_Data (Children (Button))),
                   "Freeze");
      end if;
   end Toggle_Freeze;

   ----------
   -- Draw --
   ----------

   procedure Draw (Buffer : access Gtk_Double_Buffer_Record'Class) is
      X : Gint := Gint (Gint_Random.Random (Gen));
      Y : Gint := Gint (Gint_Random.Random (Gen));

   begin
      Gdk.GC.Set_Foreground (Black_Gc,
                             Colors (Color_Random.Random (Gen_Color)));
      Draw_Rectangle (Get_Pixmap (Buffer),
                      Black_Gc, False,
                      X, Y, 50, 30);
   end Draw;

   -----------
   -- Reset --
   -----------

   procedure Reset (Buffer : access Gtk_Double_Buffer_Record'Class) is
   begin
      Draw_Rectangle (Get_Pixmap (Buffer),
                      White_Gc, True, 0, 0,
                      Gint (Get_Allocation_Width (Buffer)),
                      Gint (Get_Allocation_Height (Buffer)));
   end Reset;

   ----------------
   -- Force_Draw --
   ----------------

   procedure Force_Draw (Buffer : access Gtk_Double_Buffer_Record'Class) is
   begin
      Double_Buffer.Draw (Buffer);
   end Force_Draw;

   ----------
   -- Quit --
   ----------

   procedure Quit (Win : access Gtk_Window_Record'Class) is
      pragma Warnings (Off, Win);
   begin
      Gtk.Main.Gtk_Exit (0);
   end Quit;

   ----------
   -- Help --
   ----------

   function Help return String is
      LF : constant Character := ASCII.LF;
   begin
      return "This demo shows how the double_buffer works:"
         & LF
         & "You can draw random rectangles on the off-screen pixmap thanks"
         & LF & " to the DRAW button."
         & LF & LF
         & "The rectangles will not appear on the screen before you explictly"
         & LF
         & " call Gtk.Widget.Draw (button GTK.WIDGET.DRAW). This copies the"
         & LF
         & " off-screen pixmap both to the screen and to the triple buffer if"
         & LF
         & " you are using it."
         & LF & LF
         & "You should also examine how the double_buffer behaves with regards"
         & LF
         & " to expose event. The simplest way to test that is to hide part or"
         & LF
         & " all of the window, and show it again."
         & LF & LF
         & "The following scenarii are worth examining:"
         & LF & LF
         & "  - If no Triple_Buffer is used and the window not frozen."
         & LF & LF
         & "    The screen is updated from the off-screen pixmap whenever you"
         & LF
         & "    click on DRAW or force an expose event. If you are in the" & LF
         & "    process of doing a long draw operation (multiple DRAW clicks)"
         & LF
         & "    the screen is updated anyway with whatever is displayed on"
         & LF & "    the off-screen buffer at the time."
         & LF & LF
         & "  - If no Triple_Buffer is used and the window is frozen."
         & LF & LF &
         "    The screen is not updated when an expose event happens, and the"
         & LF
         & "    update is delayed until the widget is thawed." & LF
         & "    Likewise, the screen is not updated even when you call" & LF
         & "    Gtk.Widget.Draw."
         & LF & LF
         & "  - If a Triple_Buffer is used and the window is not frozen."
         & LF & LF &
         "    The screen is updated from the triple buffer. Thus you can draw"
         & LF
         & "    anything on the double-buffer, it won't be visible on the"
         & LF
         & "    screen until you call Gtk.Widget.Draw, even if some expose"
         & LF
         & "    event happens during a long draw operation."
         & LF & LF
         & "  - If a Triple_Buffer is used and the window is frozen."
         & LF & LF
         & "    The screen is still updated from the triple buffer. However,"
         & LF
         & "    the double buffer is never copied to the triple buffer, even"
         & LF
         & "    when you call Gtk.Widget.Draw. When the widget is thawed, the"
         & LF
         & "    double buffer is copied to the triple buffer if required."
         & LF & LF
         & "The value of Back_Store is used when the widget is resized. If its"
         & LF
         & " value is False, then the content of the pixmap is not saved when"
         & LF
         & " widget is resized, and the new content of the pixmap is random."
         & LF &
         "If its value is True, then the content is restored, and if required"
         & LF
         & " is completed by an solid color (chosen from the Gtk_Style of the"
         & LF
         & " widget).";
   end Help;

   ----------
   -- Init --
   ----------

   procedure Init is
      Win    : Gtk_Window;
      Vbox,
      Hbox,
      Box    : Gtk_Box;
      Buffer : Gtk_Double_Buffer;
      Button : Gtk_Button;
      Toggle : Gtk_Toggle_Button;
      Text   : Gtk_Text_View;
      Text_Buffer : Gtk_Text_Buffer;
      Scrolled : Gtk_Scrolled_Window;
      Color   : Gdk.Color.Gdk_Color;
      Result  : Gint;
      Success : Boolean_Array (Color_Index'Range);

   begin
      Gtk_New (Win, Window_Toplevel);
      Set_Title (Win, "Double_Buffer tests");
      Quit_Cb.Connect (Win, "destroy", Quit_Cb.To_Marshaller (Quit'Access));

      Gtk_New_Vbox (Vbox, Homogeneous => False);
      Add (Win, Vbox);

      Gtk_New_Hbox (Hbox, Homogeneous => False, Spacing => 5);
      Pack_Start (Vbox, Hbox);

      Gtk_New_Vbox (Box, Homogeneous => False, Spacing => 5);
      Pack_Start (Hbox, Box, Expand => False, Fill => False);

      Gtk_New (Buffer);
      Set_Back_Store (Buffer, True);
      Set_Triple_Buffer (Buffer, False);
      Set_USize (Buffer, Pixmap_Width, Pixmap_Height);
      Pack_Start (Hbox, Buffer, Expand => True);

      Gtk_New (Button, "Rectangle");
      Pack_Start (Box, Button, Expand => False);
      Button_Cb.Object_Connect (Button, "clicked",
                                Button_Cb.To_Marshaller (Draw'Access),
                                Buffer);

      Gtk_New (Button, "Gtk.Widget.Draw");
      Pack_Start (Box, Button, Expand => False);
      Button_Cb.Object_Connect (Button, "clicked",
                                Button_Cb.To_Marshaller (Force_Draw'Access),
                                Buffer);

      Gtk_New (Button, "Reset");
      Pack_Start (Box, Button, Expand => False);
      Button_Cb.Object_Connect (Button, "clicked",
                                Button_Cb.To_Marshaller (Reset'Access),
                                Buffer);

      Gtk_New (Toggle, "Freeze");
      Pack_End (Box, Toggle, Expand => False);
      Void_Cb.Connect (Toggle, "clicked",
                       Void_Cb.To_Marshaller (Toggle_Freeze'Access),
                       Buffer);

      Gtk_New (Toggle, "Back Store");
      Set_Active (Toggle, True);
      Pack_End (Box, Toggle, Expand => False);
      Void_Cb.Connect (Toggle, "clicked",
                       Void_Cb.To_Marshaller (Toggle_Back_Store'Access),
                       Buffer);

      Gtk_New (Toggle, "Use Triple Buffer");
      Pack_End (Box, Toggle, Expand => False);
      Void_Cb.Connect (Toggle, "clicked",
                       Void_Cb.To_Marshaller (Toggle_Triple'Access),
                       Buffer);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Pack_Start (Vbox, Scrolled);

      Gtk_New (Text_Buffer);
      Gtk_New (Text, Text_Buffer);
      Set_Editable (Text, False);
      Set_USize (Text, Pixmap_Width, 250);
      Add (Scrolled, Text);

      Set_Text (Text_Buffer, Help);

      Show_All (Win);

      Gdk.GC.Gdk_New (White_Gc, Get_Window (Buffer));
      Color := Gdk.Color.Parse ("black");
      Gdk.Color.Alloc (Get_Default_Colormap, Color);
      Gdk.GC.Set_Foreground (White_Gc, Color);

      Reset (Buffer);
      Double_Buffer.Draw (Buffer);

      Colors (1) := Gdk.Color.Parse ("blue");
      Colors (2) := Gdk.Color.Parse ("red");
      Colors (3) := Gdk.Color.Parse ("green");
      Colors (4) := Gdk.Color.Parse ("yellow");
      Colors (5) := Gdk.Color.Parse ("orange");

      Alloc_Colors (Get_Default_Colormap,
                    Colors,
                    Writeable  => False,
                    Best_Match => True,
                    Success    => Success,
                    Result     => Result);

      Gdk.GC.Gdk_New (Black_Gc, Get_Window (Buffer));
   end Init;

end Full_Test;
