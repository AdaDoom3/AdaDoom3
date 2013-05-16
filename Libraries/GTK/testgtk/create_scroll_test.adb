-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                     Copyright (C) 2000-2013, AdaCore              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Glib;             use Glib;
with Gdk.Drawable;     use Gdk.Drawable;
with Gdk.Event;        use Gdk.Event;
with Gdk.GC;           use Gdk.GC;
with Gdk.Rectangle;    use Gdk.Rectangle;
with Gdk.Window;       use Gdk.Window;
with Gdk;              use Gdk;
with Gtk.Adjustment;   use Gtk.Adjustment;
with Gtk.Box;          use Gtk.Box;
with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.Scrollbar;    use Gtk.Scrollbar;
with Gtk.Handlers;
with Gtk.Style;
with Gtk.Widget;       use Gtk.Widget;
with Gtk;              use Gtk;

package body Create_Scroll_Test is

   package Adjustment_Cb is new Handlers.User_Callback
     (Widget_Type => Adjustment.Gtk_Adjustment_Record,
      User_Type => Drawing_Area.Gtk_Drawing_Area);

   package Event_Cb is new Handlers.User_Return_Callback
     (Widget_Type => Drawing_Area.Gtk_Drawing_Area_Record,
      Return_Type => Gint,
      User_Type => Adjustment.Gtk_Adjustment);

   Scroll_Test_Pos : Gint := 0;
   Scroll_Test_GC : Gdk.GC.Gdk_GC;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This demo shows how you can implement some scrolling in your"
        & " applications. Most of the time, putting a widget into a"
        & " @bGtk_Scrolling_Area@B will do the job. However, if the scrolling"
        & " takes place on a very large region, it might be more efficient to"
        & " have a widget with the minimal size visible on the screen, and"
        & " simply draw the relevant region in it."
        & ASCII.LF
        & "As you can see in this demo, this mechanism is implemented using"
        & " some @bGtk_Adjustment@B widgets, along with some"
        & " @bGtk_Scrollbar@Bs."
        & ASCII.LF
        & "Note also that you must set the event mask in your widget so that"
        & " @bexpose@B and @bconfigure@B events are correctly handled.";
   end Help;

   -------------------------
   --  Adjustment_Change  --
   -------------------------

   procedure Adjustment_Change
     (Adj    : access Adjustment.Gtk_Adjustment_Record'Class;
      Widget : Drawing_Area.Gtk_Drawing_Area)
   is
      Source_Min : Gint := Gint (Get_Value (Adj)) - Scroll_Test_Pos;
      Source_Max : Gint := Source_Min + Gint (Get_Allocation_Height (Widget));
      Dest_Min   : Gint := 0;
      Dest_Max   : Gint := Gint (Get_Allocation_Height (Widget));
      Rect       : Gdk_Rectangle;
      Event      : Gdk_Event_Expose;
      Tmp        : Boolean;
      pragma Unreferenced (Tmp, Dest_Max);

   begin
      Scroll_Test_Pos := Gint (Get_Value (Adj));

      if not Drawable_Is_Set (Widget) then
         return;
      end if;

      Rect := (X => 0, Y => 0, Width => 0, Height => 0);
      --  This is actually different from C, since we can not
      --  create a C's GdkRectangle, only a C's GdkRectangle*

      if Source_Min < 0 then
         Rect.Width := Gint (Get_Allocation_Width (Widget));
         Rect.Height :=
           Gint'Min (-Source_Min, Gint (Get_Allocation_Height (Widget)));
         Source_Min := 0;
         Dest_Min   := Rect.Height;

      else
         Rect.Y :=
                Gint'Max (0, 2 * Gint (Get_Allocation_Height (Widget))
                          - Source_Max);
         Rect.Width := Gint (Get_Allocation_Width (Widget));
         Rect.Height := Gint (Get_Allocation_Height (Widget)) - Rect.Y;
         Source_Max := Gint (Get_Allocation_Height (Widget));
         Dest_Max := Rect.Y;
      end if;

      if Source_Min /= Source_Max then
         if Scroll_Test_GC = Null_GC then
            Gdk_New (Scroll_Test_GC, Get_Window (Widget));
            Set_Exposures (Scroll_Test_GC, True);
         end if;

         Draw_Drawable
           (Get_Window (Widget), Scroll_Test_GC,
            Get_Window (Widget),
            0, Source_Min,
            0, Dest_Min,
            Gint (Get_Allocation_Width (Widget)),
            Source_Max - Source_Min);

         --  Make sure graphics expose events are processed before
         --  scrolling again

         loop
            Get_Graphics_Expose (Event, Get_Window (Widget));
            exit when not Is_Created (Event);
            Tmp := Gtk.Widget.Event (Widget, Gdk_Event (Event));
            if Get_Count (Event) = 0 then
               Free (Event);
               exit;
            end if;
            Free (Event);
         end loop;
      end if;

      if Rect.Height /= 0 then
         Draw (Widget, Rect);
      end if;

   end Adjustment_Change;

   -----------------
   --  Configure  --
   -----------------

   function Configure
     (Widget  : access Drawing_Area.Gtk_Drawing_Area_Record'Class;
      Event   : Gdk.Event.Gdk_Event;
      Adj     : Adjustment.Gtk_Adjustment) return Gint
   is
      pragma Warnings (Off, Event);
   begin
      Set_Page_Increment (Adj, 0.9 * Gdouble (Get_Allocation_Height (Widget)));
      Set_Page_Size (Adj, Gdouble (Get_Allocation_Height (Widget)));
      --  FIXME Emit_By_Name (Adj, "changed");
      return 0;
   end Configure;

   --------------
   --  Expose  --
   --------------

   function Expose
     (Widget  : access Drawing_Area.Gtk_Drawing_Area_Record'Class;
      Event   : Gdk.Event.Gdk_Event;
      Adj     : Adjustment.Gtk_Adjustment)
     return Gint
   is
      Expose_Event : constant Gdk.Event.Gdk_Event_Expose :=
        Gdk.Event.Gdk_Event_Expose (Event);
      Area : Gdk.Rectangle.Gdk_Rectangle;
      Imin, Imax, Jmin, Jmax : Gint;
      Sty : constant Gtk.Style.Gtk_Style := Get_Style (Widget);
   begin
      Area := Gdk.Event.Get_Area (Expose_Event);

      Imin := Area.X / 10;
      Imax := (Area.X + Area.Width + 9) / 10;

      Jmin := (Gint (Adjustment.Get_Value (Adj)) + Area.Y) / 10;
      Jmax := (Gint (Adjustment.Get_Value (Adj)) + Area.Y
               + Area.Height + 9) / 10;

      Gdk.Window.Clear_Area (Window => Get_Window (Widget),
                             X => Area.X, Y => Area.Y,
                             Width => Area.Width,
                             Height => Area.Height);

      for I in Imin .. Imax loop
         for J in Jmin .. Jmax loop
            if (I + J) mod 2 /= 0 then
               Gdk.Drawable.Draw_Rectangle
                 (Drawable => Get_Window (Widget),
                  GC => Gtk.Style.Get_Black_GC (Sty),
                  Filled => True,
                  X => 10 * I, Y => 10 * J - Gint (Adjustment.Get_Value (Adj)),
                  Width => 1 + I mod 10, Height => 1 + J mod 10);
            end if;
         end loop; --  J
      end loop;  --  I
      return 0;
   end Expose;

   -----------
   --  Run  --
   -----------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Hbox : Box.Gtk_Box;
      Vbox : Box.Gtk_Box;
      Drawing_Area : Gtk.Drawing_Area.Gtk_Drawing_Area;
      Adj : Gtk.Adjustment.Gtk_Adjustment;
      Scrollbar : Gtk.Scrollbar.Gtk_Scrollbar;

   begin
      Set_Label (Frame, "Scroll Test");

      Box.Gtk_New_Vbox (Vbox, Homogeneous => False, Spacing => 0);
      Add (Frame, Vbox);

      Box.Gtk_New_Hbox (Hbox, Homogeneous => False, Spacing => 0);
      Box.Pack_Start (In_Box => Vbox, Child => Hbox);

      Gtk.Drawing_Area.Gtk_New (Drawing_Area);
      Set_Size_Request (Drawing_Area, 200, 200);
      Box.Pack_Start (In_Box => Hbox, Child => Drawing_Area);

      Unrealize (Drawing_Area); --  Required for Set_Events
      Set_Events (Widget => Drawing_Area, Events => Exposure_Mask);

      Adjustment.Gtk_New (Adjustment => Adj, Value => 0.0, Lower => 0.0,
                          Upper => 1000.0, Step_Increment => 1.0,
                          Page_Increment => 180.0, Page_Size => 200.0);
      Scroll_Test_Pos := 0;

      Gtk.Scrollbar.Gtk_New_Vscrollbar (Widget => Scrollbar,
                                        Adjustment => Adj);
      Box.Pack_Start (In_Box => Hbox, Child => Scrollbar,
                      Expand => False, Fill => False);

      Event_Cb.Connect
        (Widget    => Drawing_Area,
         Name      => "expose_event",
         Marsh     => Event_Cb.To_Marshaller (Expose'Access),
         User_Data => Adj);

      Event_Cb.Connect
        (Widget    => Drawing_Area,
         Name      => "configure_event",
         Marsh     => Event_Cb.To_Marshaller (Configure'Access),
         User_Data => Adj);

      Adjustment_Cb.Connect
        (Widget    => Adj,
         Name      => "value_changed",
         Marsh  => Adjustment_Cb.To_Marshaller (Adjustment_Change'Access),
         User_Data => Drawing_Area);

      Show_All (Frame);
   end Run;

end Create_Scroll_Test;
