with Glib.Object;
with Gdk.Drawable; use Gdk.Drawable;
with Gdk.Event;    use Gdk.Event;
with Gdk.Types;    use Gdk.Types;
with Gdk.Window;   use Gdk.Window;
with Gdk.Window_Attr; use Gdk.Window_Attr;

with Gtk.Widget;    use Gtk.Widget;
with Gtk.Style;     use Gtk.Style;
with Gtk.Handlers;  use Gtk.Handlers;
with Gtk.Event_Box; use Gtk.Event_Box;

with System;

with Ada.Numerics.Generic_Elementary_Functions; use Ada.Numerics;

package body Gtk_Dial is

   package Gdouble_Types is new
     Ada.Numerics.Generic_Elementary_Functions (Gdouble);
   use Gdouble_Types;

   Scroll_Delay_Length : constant := 300;
   Dial_Default_Size   : constant := 100;

   ------------------------
   -- Internal Callbacks --
   ------------------------

   procedure Destroy (Dial : access Gtk_Dial_Record'Class);

   procedure Realize (Dial : access Gtk_Dial_Record'Class);

   procedure Size_Request
     (Dial : access Gtk_Dial_Record'Class;
      Requisition : Gtk_Requisition_Access);

   procedure Size_Allocate
     (Dial : access Gtk_Dial_Record'Class;
      Allocation : Gtk_Allocation_Access);

   function Expose
     (Dial : access Gtk_Dial_Record'Class; Event : Gdk_Event) return Boolean;

   function Button_Press
     (Dial  : access Gtk_Dial_Record'Class;
      Event : Gdk_Event) return Boolean;

   function Button_Release
     (Dial  : access Gtk_Dial_Record'Class;
      Event : Gdk_Event) return Boolean;

   function Motion_Notify
     (Dial : access Gtk_Dial_Record'Class;
      Event : Gdk_Event) return Boolean;

   procedure Adjustment_Changed
     (Adjustment : access Gtk_Adjustment_Record'Class;
      Dial       : Gtk_Dial);

   procedure Adjustment_Value_Changed
     (Adjustment : access Gtk_Adjustment_Record'Class;
      Dial : Gtk_Dial);

   ---------------------
   -- Internal Timers --
   ---------------------

   function Timer (Dial : Gtk_Dial) return Boolean;

   package Timeout is new Gtk.Main.Timeout (Gtk_Dial);

   -------------------------
   -- Internal procedures --
   -------------------------

   procedure Update_Mouse (Dial : access Gtk_Dial_Record'Class; X, Y : Gint);

   procedure Update (Dial : access Gtk_Dial_Record'Class);

   ----------------------------
   -- Handler Instantiations --
   ----------------------------

   package Dial_Cb is new Gtk.Handlers.Callback (Gtk_Dial_Record);
   package Adj_Cb is new Gtk.Handlers.Callback (Gtk_Adjustment_Record);

   package Size_Cb is new Gtk.Handlers.Callback (Gtk_Dial_Record);
   package Requisition_Marshaller is new Size_Cb.Marshallers.Generic_Marshaller
     (Gtk_Requisition_Access, Get_Requisition);

   package Allocation_Cb is new Gtk.Handlers.Callback (Gtk_Dial_Record);
   package Allocation_Marshaller is new
     Allocation_Cb.Marshallers.Generic_Marshaller
       (Gtk_Allocation_Access, Get_Allocation);

   package Event_Cb is new Gtk.Handlers.Return_Callback
     (Gtk_Dial_Record, Boolean);

   package Adjustment_Cb is new Gtk.Handlers.User_Callback
     (Gtk_Adjustment_Record, Gtk_Dial);

   package Realize_Pkg is new Gtk.Widget.Realize_Handling
     (Gtk_Dial_Record, Realize);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Dial : out Gtk_Dial; Adjustment : Gtk_Adjustment) is
   begin
      Dial := new Gtk_Dial_Record;
      Initialize (Dial, Adjustment);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Dial : access Gtk_Dial_Record'class;
      Adjustment : Gtk_Adjustment)
   is
      Adj : Gtk_Adjustment;
   begin
      --  We first need to call the ancestor's Initialize function to create
      --  the underlying C object.
      Gtk.Event_Box.Initialize (Dial);

      if Adjustment = null then
         Gtk_New (Adj, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
         Set_Adjustment (Dial, Adj);
      else
         Set_Adjustment (Dial, Adjustment);
      end if;

      --  Set up the appropriate callbacks to redraw, ...
      Event_Cb.Connect
        (Dial, "expose_event", Event_Cb.To_Marshaller (Expose'Access), True);
      Size_Cb.Connect
        (Dial, "size_request",
         Requisition_Marshaller.To_Marshaller (Size_Request'Access));
      Allocation_Cb.Connect
        (Dial, "size_allocate",
         Allocation_Marshaller.To_Marshaller (Size_Allocate'Access));
      Event_Cb.Connect
        (Dial, "button_press_event",
         Event_Cb.To_Marshaller (Button_Press'Access), True);
      Event_Cb.Connect
        (Dial, "button_release_event",
         Event_Cb.To_Marshaller (Button_Release'Access), True);
      Dial_Cb.Connect
        (Dial, "destroy", Dial_Cb.To_Marshaller (Destroy'Access));
      Event_Cb.Connect
        (Dial, "motion_notify_event",
         Event_Cb.To_Marshaller (Motion_Notify'Access), True);

      Realize_Pkg.Set_Realize (Dial);
   end Initialize;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Dial : access Gtk_Dial_Record'Class) is
   begin
      if Dial.Adjustment /= null then
         Unref (Dial.Adjustment);
      end if;

      Gtk.Handlers.Emit_Stop_By_Name (Dial, "destroy");
   end Destroy;

   --------------------
   -- Get_Adjustment --
   --------------------

   function Get_Adjustment (Dial : access Gtk_Dial_Record)
     return Gtk_Adjustment is
   begin
      return Dial.Adjustment;
   end Get_Adjustment;

   -----------------------
   -- Set_Update_Policy --
   -----------------------

   procedure Set_Update_Policy
     (Dial : access Gtk_Dial_Record;
      Policy : Gtk_Update_Type) is
   begin
      Dial.Policy := Policy;
   end Set_Update_Policy;

   --------------------
   -- Set_Adjustment --
   --------------------

   procedure Set_Adjustment
     (Dial : access Gtk_Dial_Record; Adjustment : Gtk_Adjustment) is
   begin
      if Dial.Adjustment /= null then
         Gtk.Handlers.Disconnect (Dial.Adjustment, Dial.Changed_Id);
         Gtk.Handlers.Disconnect (Dial.Adjustment, Dial.Value_Changed_Id);
         Unref (Dial.Adjustment);
      end if;

      Dial.Adjustment := Adjustment;
      Ref (Dial.Adjustment);

      Dial.Changed_Id := Adjustment_Cb.Connect
        (Adjustment, "changed",
         Adjustment_Cb.To_Marshaller (Adjustment_Changed'Access),
         Gtk_Dial (Dial));
      Dial.Value_Changed_Id := Adjustment_Cb.Connect
        (Adjustment, "value_changed",
         Adjustment_Cb.To_Marshaller (Adjustment_Value_Changed'Access),
         Gtk_Dial (Dial));

      Dial.Old_Value := Get_Value (Adjustment);
      Dial.Old_Lower := Get_Lower (Adjustment);
      Dial.Old_Upper := Get_Upper (Adjustment);

      Update (Dial);
   end Set_Adjustment;

   -------------
   -- Realize --
   -------------

   procedure Realize (Dial : access Gtk_Dial_Record'Class) is
      Attributes : Gdk_Window_Attr;
      Attributes_Mask : Gdk_Window_Attributes_Type;
      Window : Gdk_Window;

      procedure Set_User_Data
        (Window : Gdk.Gdk_Window; Widget : System.Address);
      pragma Import (C, Set_User_Data, "gdk_window_set_user_data");

   begin
      Set_Flags (Dial, Realized);

      Gdk_New (Attributes,
        X => Get_Allocation_X (Dial),
        Y => Get_Allocation_Y (Dial),
        Width => Gint (Get_Allocation_Width (Dial)),
        Height => Gint (Get_Allocation_Height (Dial)),
        Window_Type => Window_Child,
        Event_Mask => Get_Events (Dial) or Exposure_Mask or
          Button_Press_Mask or Button_Release_Mask or Pointer_Motion_Mask or
          Pointer_Motion_Hint_Mask,
        Visual => Get_Visual (Dial),
        Colormap => Get_Colormap (Dial));
      Attributes_Mask := Wa_X or Wa_Y or Wa_Visual or Wa_Colormap;
      Gdk_New (Window, Get_Window (Get_Parent (Dial)),
        Attributes, Attributes_Mask);
      Set_Window (Dial, Window);
      Set_Style (Dial, Attach (Get_Style (Dial), Get_Window (Dial)));

      Set_User_Data (Window, Glib.Object.Get_Object (Dial));

      Set_Background (Get_Style (Dial), Get_Window (Dial), State_Active);
   end Realize;

   ------------------
   -- Size_Request --
   ------------------

   procedure Size_Request
     (Dial : access Gtk_Dial_Record'Class;
      Requisition : Gtk_Requisition_Access) is
   begin
      Requisition.Width := Dial_Default_Size;
      Requisition.Height := Dial_Default_Size;

      --  Stop the signal from being propagated to the parent's default
      --  size_request function
      Gtk.Handlers.Emit_Stop_By_Name (Dial, "size_request");
   end Size_Request;

   -------------------
   -- Size_Allocate --
   -------------------

   procedure Size_Allocate
     (Dial : access Gtk_Dial_Record'Class;
      Allocation : Gtk_Allocation_Access) is
   begin
      if Realized_Is_Set (Dial) then
         Gdk.Window.Move_Resize
           (Get_Window (Dial),
            Allocation.X, Allocation.Y,
            Gint (Allocation.Width),
            Gint (Allocation.Height));
      end if;

      Dial.Radius := Gint (
        Gdouble (Gint'Min (Allocation.Width, Allocation.Height)) * 0.45);
      Dial.Pointer_Width := Dial.Radius / 5;

      Gtk.Handlers.Emit_Stop_By_Name (Dial, "size_allocate");
   end Size_Allocate;

   ------------
   -- Expose --
   ------------

   function Expose
     (Dial : access Gtk_Dial_Record'Class; Event : Gdk_Event) return Boolean
   is
      Points       : Gdk_Points_Array (1 .. 5);
      S, C         : Gdouble;
      Theta,
      Last,
      Increment    : Gdouble;
      Blankstyle   : Gtk_Style;
      Xc, Yc       : Gdouble;
      Upper, Lower : Gint;
      Tick_Length  : Gint;
      Inc          : Gint;

   begin
      Gtk.Handlers.Emit_Stop_By_Name (Dial, "expose_event");

      if Get_Count (Event) > 0 then
         return False;
      end if;

      Gdk.Window.Clear_Area (Get_Window (Dial),
        0, 0,
        Gint (Get_Allocation_Width (Dial)),
        Gint (Get_Allocation_Height (Dial)));

      Xc := Gdouble (Get_Allocation_Width (Dial)) / 2.0;
      Yc := Gdouble (Get_Allocation_Height (Dial)) / 2.0;

      Upper := Gint (Get_Upper (Dial.Adjustment));
      Lower := Gint (Get_Lower (Dial.Adjustment));

      --  Erase old pointer

      S := Gdouble (Sin (Dial.Last_Angle));
      C := Gdouble (Cos (Dial.Last_Angle));
      Dial.Last_Angle := Dial.Angle;

      Points (1).X := Gint (Xc + S * Gdouble (Dial.Pointer_Width) / 2.0);
      Points (1).Y := Gint (Yc + C * Gdouble (Dial.Pointer_Width) / 2.0);
      Points (2).X := Gint (Xc + C * Gdouble (Dial.Radius));
      Points (2).Y := Gint (Yc - S * Gdouble (Dial.Radius));
      Points (3).X := Gint (Xc - S * Gdouble (Dial.Pointer_Width) / 2.0);
      Points (3).Y := Gint (Yc - C * Gdouble (Dial.Pointer_Width) / 2.0);
      Points (4).X := Gint (Xc - C * Gdouble (Dial.Radius) / 10.0);
      Points (4).Y := Gint (Yc + S * Gdouble (Dial.Radius) / 10.0);
      Points (5).X := Points (1).X;
      Points (5).Y := Points (1).Y;

      Gtk_New (Blankstyle);
      Set_Bg_GC (Blankstyle, State_Normal,
        Get_Bg_GC (Get_Style (Dial), State_Normal));
      Set_Dark_GC (Blankstyle, State_Normal,
        Get_Bg_GC (Get_Style (Dial), State_Normal));
      Set_Light_GC (Blankstyle, State_Normal,
        Get_Bg_GC (Get_Style (Dial), State_Normal));
      Set_Black_GC (Blankstyle,
        Get_Bg_GC (Get_Style (Dial), State_Normal));

      Draw_Polygon
        (Blankstyle, Get_Window (Dial),
         State_Normal, Shadow_Out,
         Points, False);

      Unref (Blankstyle);

      --  Draw ticks

      if Upper - Lower = 0 then
         return False;
      end if;

      Increment := (100.0 * Pi) / (Gdouble (Dial.Radius * Dial.Radius));
      Inc := Upper - Lower;

      while Inc < 100 loop
         Inc := Inc * 10;
      end loop;

      while Inc >= 1000 loop
         Inc := Inc / 10;
      end loop;

      Last := -1.0;

      for J in 0 .. Inc loop
         Theta := Gdouble (J) * Pi / (18.0 * Gdouble (Inc) / 24.0) - Pi / 6.0;

         if Theta - Last >= Increment then
            Last := Theta;

            S := Sin (Theta);
            C := Cos (Theta);

            if J rem (Inc / 10) = 0 then
               Tick_Length := Dial.Pointer_Width;
            else
               Tick_Length := Dial.Pointer_Width / 2;
            end if;

            Draw_Line
              (Get_Window (Dial),
               Get_Fg_GC (Get_Style (Dial), Get_State (Dial)),
               Gint (Xc + C * Gdouble (Dial.Radius - Tick_Length)),
               Gint (Yc - S * Gdouble (Dial.Radius - Tick_Length)),
               Gint (Xc + C * Gdouble (Dial.Radius)),
               Gint (Yc - S * Gdouble (Dial.Radius)));
         end if;
      end loop;

      --  Draw pointer

      S := Sin (Dial.Angle);
      C := Cos (Dial.Angle);
      Dial.Last_Angle := Dial.Angle;

      Points (1).X := Gint (Xc + S * Gdouble (Dial.Pointer_Width) / 2.0);
      Points (1).Y := Gint (Yc + C * Gdouble (Dial.Pointer_Width) / 2.0);
      Points (2).X := Gint (Xc + C * Gdouble (Dial.Radius));
      Points (2).Y := Gint (Yc - S * Gdouble (Dial.Radius));
      Points (3).X := Gint (Xc - S * Gdouble (Dial.Pointer_Width) / 2.0);
      Points (3).Y := Gint (Yc - C * Gdouble (Dial.Pointer_Width) / 2.0);
      Points (4).X := Gint (Xc - C * Gdouble (Dial.Radius) / 10.0);
      Points (4).Y := Gint (Yc + S * Gdouble (Dial.Radius) / 10.0);
      Points (5).X := Points (1).X;
      Points (5).Y := Points (1).Y;

      Draw_Polygon
        (Get_Style (Dial), Get_Window (Dial), State_Normal, Shadow_Out,
         Points, True);

      return False;
   end Expose;

   ------------------
   -- Button_Press --
   ------------------

   function Button_Press
     (Dial  : access Gtk_Dial_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      Dx, Dy          : Gint;
      S, C            : Gdouble;
      D_Parallel      : Gdouble;
      D_Perpendicular : Gdouble;

   begin
      --  Determine if button press was within pointer region - we
      --  do this by computing the parallel and perpendicular distance of
      --  the point where the mouse was pressed from the line passing through
      --  the pointer

      Dx := Gint (Get_X (Event) - Gdouble (Get_Allocation_Width (Dial)) / 2.0);
      Dy :=
        Gint (Gdouble (Get_Allocation_Height (Dial)) / 2.0 - Get_Y (Event));

      S := Sin (Dial.Angle);
      C := Cos (Dial.Angle);

      D_Parallel := S * Gdouble (Dy) + C * Gdouble (Dx);
      D_Perpendicular := abs (S * Gdouble (Dx) - C * Gdouble (Dy));

      if Dial.Button = 0 and then
        D_Perpendicular < Gdouble (Dial.Pointer_Width) / 2.0 and then
        D_Parallel > Gdouble (-Dial.Pointer_Width)
      then
         Grab_Add (Dial);
         Dial.Button := Get_Button (Event);
         Update_Mouse (Dial, Gint (Get_X (Event)), Gint (Get_Y (Event)));
      end if;

      Gtk.Handlers.Emit_Stop_By_Name (Dial, "button_press_event");
      return False;
   end Button_Press;

   --------------------
   -- Button_Release --
   --------------------

   function Button_Release
     (Dial  : access Gtk_Dial_Record'Class;
      Event : Gdk_Event) return Boolean is
   begin
      if Dial.Button = Get_Button (Event) then
         Grab_Remove (Dial);

         Dial.Button := 0;

         if Dial.Policy = Update_Delayed then
            Timeout_Remove (Dial.Timer);
         end if;

         if Dial.Policy /= Update_Continuous and then
           Dial.Old_Value /= Get_Value (Dial.Adjustment)
         then
            Adj_Cb.Emit_By_Name (Dial.Adjustment, "value_changed");
         end if;
      end if;

      Gtk.Handlers.Emit_Stop_By_Name (Dial, "button_release_event");
      return False;
   end Button_Release;

   -------------------
   -- Motion_Notify --
   -------------------

   function Motion_Notify
     (Dial : access Gtk_Dial_Record'Class; Event : Gdk_Event)
      return Boolean
   is
      Mods, Mask : Gdk_Modifier_Type;
      X, Y : Gint;
      Window : Gdk_Window;

      use Gdk;
   begin
      if Dial.Button /= 0 then
         X := Gint (Get_X (Event));
         Y := Gint (Get_Y (Event));

         if Get_Is_Hint (Event)
           or else Get_Window (Event) /= Get_Window (Dial)
         then
            Get_Pointer (Get_Window (Dial), X, Y, Mods, Window);
         end if;

         case Dial.Button is
            when 1 => Mask := Button1_Mask;
            when 2 => Mask := Button2_Mask;
            when 3 => Mask := Button3_Mask;
            when others => Mask := 0;
         end case;

         if (Mods and Mask) /= 0 then
            Update_Mouse (Dial, X, Y);
         end if;
      end if;

      Gtk.Handlers.Emit_Stop_By_Name (Dial, "motion_notify_event");
      return False;
   end Motion_Notify;

   -----------
   -- Timer --
   -----------

   function Timer (Dial : Gtk_Dial) return Boolean is
   begin
      if Dial.Policy = Update_Delayed then
         Adj_Cb.Emit_By_Name (Dial.Adjustment, "value_changed");
      end if;

      return False;
   end Timer;

   ------------------
   -- Update_Mouse --
   ------------------

   procedure Update_Mouse (Dial : access Gtk_Dial_Record'Class; X, Y : Gint) is
      Xc, Yc : Gint;
      Old_Value : Gdouble;

   begin
      Xc := Gint (Get_Allocation_Width (Dial) / 2);
      Yc := Gint (Get_Allocation_Height (Dial) / 2);

      Old_Value := Get_Value (Dial.Adjustment);
      Dial.Angle := Arctan (Gdouble (Yc - Y), Gdouble (X - Xc));

      if Dial.Angle < -Pi / 2.0 then
         Dial.Angle := Dial.Angle + 2.0 * Pi;
      end if;

      if Dial.Angle < -Pi / 6.0 then
         Dial.Angle := -Pi / 6.0;
      end if;

      if Dial.Angle > 7.0 * Pi/6.0 then
         Dial.Angle := 7.0 * Pi/6.0;
      end if;

      Set_Value
        (Dial.Adjustment,
         Get_Lower (Dial.Adjustment) +
           (7.0 * Pi / 6.0 - Dial.Angle) *
             (Get_Upper (Dial.Adjustment) - Get_Lower (Dial.Adjustment)) /
               (4.0 * Pi / 3.0));

      if Get_Value (Dial.Adjustment) /= Old_Value then
         if Dial.Policy = Update_Continuous then
            Adj_Cb.Emit_By_Name (Dial.Adjustment, "value_changed");
         else
            Draw (Dial);

            if Dial.Policy = Update_Delayed then
               if Dial.Timer /= 0 then
                  Timeout_Remove (Dial.Timer);
               end if;

               Dial.Timer := Timeout.Add
                 (Scroll_Delay_Length, Timer'Access, Gtk_Dial (Dial));
            end if;
         end if;
      end if;

      exception
         when Ada.Numerics.Argument_Error => null;
   end Update_Mouse;

   ------------
   -- Update --
   ------------

   procedure Update (Dial : access Gtk_Dial_Record'Class) is
      New_Value : Gdouble;
   begin
      New_Value := Get_Value (Dial.Adjustment);

      if New_Value < Get_Lower (Dial.Adjustment) then
         New_Value := Get_Lower (Dial.Adjustment);
      end if;

      if New_Value > Get_Upper (Dial.Adjustment) then
         New_Value := Get_Upper (Dial.Adjustment);
      end if;

      if New_Value /= Get_Value (Dial.Adjustment) then
         Set_Value (Dial.Adjustment, New_Value);
         Adj_Cb.Emit_By_Name (Dial.Adjustment, "value_changed");
      end if;

      Dial.Angle := 7.0 * Pi / 6.0 -
        (Gdouble (New_Value - Get_Lower (Dial.Adjustment))) * 4.0 * Pi / 3.0 /
          Gdouble (Get_Upper (Dial.Adjustment) - Get_Lower (Dial.Adjustment));
      Draw (Dial);
   end Update;

   ------------------------
   -- Adjustment_Changed --
   ------------------------

   procedure Adjustment_Changed
     (Adjustment : access Gtk_Adjustment_Record'Class;
      Dial       : Gtk_Dial) is
   begin
      if Dial.Old_Value /= Get_Value (Adjustment) or else
         Dial.Old_Lower /= Get_Lower (Adjustment) or else
         Dial.Old_Upper /= Get_Upper (Adjustment)
      then
         Update (Dial);
         Dial.Old_Value := Get_Value (Adjustment);
         Dial.Old_Lower := Get_Lower (Adjustment);
         Dial.Old_Upper := Get_Upper (Adjustment);
      end if;
   end Adjustment_Changed;

   ------------------------------
   -- Adjustment_Value_Changed --
   ------------------------------

   procedure Adjustment_Value_Changed
     (Adjustment : access Gtk_Adjustment_Record'Class;
      Dial : Gtk_Dial) is
   begin
      if Dial.Old_Value /= Get_Value (Adjustment) then
         Update (Dial);
         Dial.Old_Value := Get_Value (Adjustment);
      end if;
   end Adjustment_Value_Changed;

end Gtk_Dial;
