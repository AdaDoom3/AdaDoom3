with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Gdk;             use Gdk;
with Gtk;             use Gtk;
with Gdk.Color;       use Gdk.Color;
with Gdk.Drawable;    use Gdk.Drawable;
with Gdk.Event;       use Gdk.Event;
with Gdk.GC;          use Gdk.GC;
with Gtk.Widget;      use Gtk.Widget;
with Gdk.Window;      use Gdk.Window;
with Gtk.Handlers;    use Gtk.Handlers;
with Gtkada.Types;    use Gtkada.Types;

package body My_Widget is

   --  Warning: Creating a widget from scratch is a difficult matter.
   --  You should not try that unless you already know quite well how
   --  gtk works. There are a lot of issues to consider for handling
   --  signals, requesting a size, drawing the widget, ...
   --  Please have a look at the tutorial for gtk+ itself to see a
   --  brief summary of all the things to take care of.


   --  This pointer will keep a pointer to the C 'class record' for
   --  gtk. To avoid allocating memory for each widget, this may be done
   --  only once, and reused
   Class_Record : GObject_Class := Uninitialized_Class;

   --  Array of the signals created for this widget
   Signals : Chars_Ptr_Array := "bullseye" + "missed";

   package Internal_Cb is new Handlers.Callback (Target_Widget_Record);
   --  The type of callbacks for the signals above. This is used only to
   --  emit the signals.

   --  Note: To create a real-life widget, you would have to do more
   --  things than are done in this simple example:
   --    * The "expose_event" signal actually passes the Area to
   --      redraw, which can speed things up a lot if used correctly
   --    * You should probably use some double-buffers to avoid the
   --      flickering that is visible in this simple example
   --    * Connect a function to the "destroy" callback to take care of
   --      the finalization of the object.

   package Return_Boolean_Cb is new Handlers.Return_Callback
     (Target_Widget_Record, Boolean);

   --  Define our own marshaller, since this is not one of the
   --  standard one.
   package Size_Cb is new Handlers.Callback (Target_Widget_Record);
   package Requisition_Marshaller is new Size_Cb.Marshallers.Generic_Marshaller
     (Gtk_Requisition_Access, Gtk.Widget.Get_Requisition);

   package Allocation_Cb is new Handlers.Callback (Target_Widget_Record);
   package Allocation_Marshaller is new
     Allocation_Cb.Marshallers.Generic_Marshaller
       (Gtk_Allocation_Access, Gtk.Widget.Get_Allocation);

   -----------------
   -- Draw_Target --
   -----------------

   function Draw_Target
     (Widget  : access Target_Widget_Record'Class)
      return Boolean;

   function Draw_Target
     (Widget  : access Target_Widget_Record'Class)
      return Boolean
      --  This function is called when we need to redraw the widget (for
      --  instance whenever part of it has been cleared
   is
      Width, Height : Gint;
      Win  : Gdk.Window.Gdk_Window := Get_Window (Widget);
   begin
      if Widget.Gc_In = null then
         declare
            Color : Gdk_Color;
         begin
            Color := Gdk.Color.Parse ("Red");
            Gdk.Color.Alloc (Gtk.Widget.Get_Default_Colormap, Color);

            Gdk.GC.Gdk_New (Widget.Gc_In, Win);
            Gdk.GC.Set_Foreground (Widget.Gc_In, Color);

            Color := Gdk.Color.Parse ("Blue");
            Gdk.Color.Alloc (Gtk.Widget.Get_Default_Colormap, Color);

            Gdk.GC.Gdk_New (Widget.Gc_Out, Win);
            Gdk.GC.Set_Foreground (Widget.Gc_Out, Color);
         end;
      end if;

      Gdk.Drawable.Get_Size (Win, Width, Height);
      Gdk.Drawable.Draw_Arc
        (Win, Widget.Gc_Out,
         Filled => True,
         X      => (Width - Widget.Radius) / 2,
         Y      => (Height - Widget.Radius) / 2,
         Width  => Widget.Radius,
         Height => Widget.Radius,
         Angle1 => 0,
         Angle2 => 360*64);
      Gdk.Drawable.Draw_Arc
        (Win, Widget.Gc_In,
         Filled => True,
         X      => (Width - Widget.Radius / 2) / 2,
         Y      => (Height - Widget.Radius / 2) / 2,
         Width  => Widget.Radius / 2,
         Height => Widget.Radius / 2,
         Angle1 => 0,
         Angle2 => 360*64);
      return True;
   end Draw_Target;

   ------------------
   -- Size_Request --
   ------------------

   procedure Size_Request
     (Widget      : access Target_Widget_Record'Class;
      Requisition : in Gtk_Requisition_Access);

   procedure Size_Request
     (Widget      : access Target_Widget_Record'Class;
      Requisition : in Gtk_Requisition_Access)
      --  This function is called by gtk+ when the widget is realized.
      --  It should modify Requisition to ask for a appropriate size
      --  for the widget. Note that the widget will not necessary have
      --  that size, it could be bigger, depending on what it is
      --  contained into. See the signal "size_allocate" too. More
      --  information on the size requisition process can be found in
      --  the book "Gtk+/Gnome Application Development" by Havoc
      --  Pennington.
   is
   begin
      Requisition.Width := Widget.Radius;
      Requisition.Height := Widget.Radius;

      --  Stop the signal from being propagated to the parent's default
      --  size_request function
      Gtk.Handlers.Emit_Stop_By_Name (Widget, "size_request");
   end Size_Request;

   -------------------
   -- Size_Allocate --
   -------------------

   procedure Size_Allocate
     (Widget     : access Target_Widget_Record'Class;
      Allocation : in     Gtk_Allocation_Access);

   procedure Size_Allocate
     (Widget     : access Target_Widget_Record'Class;
      Allocation : in     Gtk_Allocation_Access)
      --  This function is called once gtk has decided what size and
      --  position the widget will actually have, or everytime the
      --  widget is resized. This would be a good time for instance
      --  for resizing the component sub-widgets. Note that we have to
      --  move and resize the widget ourselves, and that for such a
      --  simple case, we could simply rely on the ancestor's
      --  size_allocation function.
   is
   begin
      if Realized_Is_Set (Widget) then
         Widget.Radius :=
           Gint'Min (Allocation.Width, Allocation.Height);
         Gdk.Window.Move_Resize (Get_Window (Widget),
                                 Allocation.X, Allocation.Y,
                                 Gint (Allocation.Width),
                                 Gint (Allocation.Height));
      end if;
      Gtk.Handlers.Emit_Stop_By_Name (Widget, "size_allocate");
   end Size_Allocate;

   -------------
   -- Clicked --
   -------------

   function Clicked
     (Widget : access Target_Widget_Record'Class;
      Event  : in     Gdk_Event)
      return Boolean;
   function Clicked
     (Widget : access Target_Widget_Record'Class;
      Event  : in     Gdk_Event)
      return Boolean
      --  called when the mouse is clicked within the widget
   is
      Tmp_X, Tmp_Y : Gint;
      Width, Height : Gint;
   begin
      Gdk.Drawable.Get_Size (Get_Window (Widget), Width, Height);
      Tmp_X := Gint (Get_X (Event)) - Width / 2;
      Tmp_Y := Gint (Get_Y (Event)) - Height / 2;

      --  The signal emitted depends on where the user has clicked.
      if Tmp_X * Tmp_X + Tmp_Y * Tmp_Y
        <= Widget.Radius * Widget.Radius / 4
      then
         Internal_Cb.Emit_By_Name (Widget, "bullseye");
      else
         Internal_Cb.Emit_By_Name (Widget, "missed");
      end if;
      return True;
   end Clicked;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Target_Widget) is
      --  Used to create a new widget
   begin
      Widget := new Target_Widget_Record;
      My_Widget.Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Target_Widget_Record'Class) is
   begin
      --  We need to call the ancestor's Initialize function to create
      --  the underlying C object.
      Gtk.Drawing_Area.Initialize (Widget);

      --  The following call is required to initialize the class record,
      --  and the new signals created for this widget.
      --  Note that for now you can only create basic signals (whose
      --  callbacks do not have any parameters), and that you just have
      --  to put their name in a table.
      --  Note also that we keep Class_Record, so that the memory allocation
      --  is done only once.
      Glib.Object.Initialize_Class_Record
        (Widget, Signals, Class_Record, "TestGtkTargetWidget");

      --  Note: We can not create the GC here, since the widget is not
      --  realized yet, and thus has no window available. This could be
      --  done be using the "realize" signal, if we really want to be clean,
      --  here we just do it in the redraw function (a little bit slower).

      Widget.Radius := 60;

      --  We want to get Button_Release events
      Set_Events (Widget, Exposure_Mask or Button_Release_Mask
                  or Button_Press_Mask);

      --  Set up the appropriate callbacks to redraw, ...
      Return_Boolean_Cb.Connect
         (Widget, "expose_event",
          Return_Boolean_Cb.To_Marshaller (Draw_Target'Access), True);

      Size_Cb.Connect
        (Widget, "size_request",
         Requisition_Marshaller.To_Marshaller (Size_Request'Access));

      Return_Boolean_Cb.Connect
        (Widget, "button_release_event",
         Return_Boolean_Cb.To_Marshaller (Clicked'Access));

      Allocation_Cb.Connect
        (Widget, "size_allocate",
         Allocation_Marshaller.To_Marshaller (Size_Allocate'Access));
   end Initialize;

end My_Widget;
