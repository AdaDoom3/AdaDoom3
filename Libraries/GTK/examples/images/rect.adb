-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 2007 AdaCore                    --
--                                                                   --
-- This program is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This program is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-----------------------------------------------------------------------

--  This example displays a toplevel window with a PNG image loaded from
--  the disk. The user can then use the mouse to select a rectangular
--  area, as often done in drawing tools.
--  This example shows how to load and draw an image efficiently, redrawing
--  only the minimal amount every time, and how to connect to mouse events
--  to create an interactive rectangular selection.

with Glib;             use Glib;
with Glib.Error;       use Glib.Error;
with Gdk.Color;        use Gdk.Color;
with Gdk.Drawable;     use Gdk, Gdk.Drawable;
with Gdk.Event;        use Gdk.Event;
with Gdk.GC;           use Gdk.GC;
with Gdk.Pixbuf;       use Gdk.Pixbuf;
with Gdk.Pixmap;       use Gdk.Pixmap;
with Gdk.Rectangle;    use Gdk.Rectangle;
with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Main;         use Gtk.Main;
with Gtk.Widget;       use Gtk.Widget;
with Gtk.Window;       use Gtk.Window;
with Gtkada.Handlers;  use Gtkada.Handlers;

with Cairo.Region;

procedure Rect is
   ----------------------
   -- Global variables --
   ----------------------
   --  In general, real applications should not use global variables.
   --  Among other things, this makes it much harder to have several
   --  toplevel windows displaying different things for instance.
   --  Since this example concentrates more on the handling of signals,
   --  we kept the code simpler through the use of global variables.
   --  The better implementation would be to use your own window type
   --  as in:
   --     type My_Window_Image_Record is new Gtk_Window_Record with record
   --       Pix              : Gdk_Pixbuf;
   --       Start_X, Start_Y : Gint;
   --       Prev_X,  Prev_Y  : Gint;
   --       GC               : Gdk_GC;
   --     end record;

   Win              : Gtk_Window;
   Pix              : Gdk_Pixbuf;
   Error            : GError;
   GC               : Gdk.Gdk_GC;

   Draw             : Gtk_Drawing_Area;
   --  A drawing area is a widget that users can use to draw their own
   --  contents. It doesn't memorize its contents, though, and it is the
   --  responsability of the user to redraw it every time there is a need
   --  for this (the "expose" event).
   --  See also the example doublebuffer which shows another possible
   --  implementation.

   Start_X, Start_Y : Gint;
   Prev_X, Prev_Y   : Gint;
   --  The variables memorize the initial position of the mouse when the
   --  user clicked on the button, and the last position when we drew the
   --  rectangle, so that we can delete it later on.

   --------------
   -- Draw_Pix --
   --------------
   --  This function draws whole or part of the image onto the drawing
   --  area. It includes a number of tests to make sure we only draw
   --  a valid area of the image (in case the image is shorter than the
   --  drawing area, or we are trying to draw outside of the drawing area
   --  for instance).

   procedure Draw_Pix
      (D : access Gtk_Widget_Record'Class;
       Area : Gdk_Rectangle := Full_Area)
   is
      A : Gdk_Rectangle := Area;
      use type Cairo.Region.Cairo_Rectangle_Int;
   begin
      --  If the caller wants to redraw the full image...

      if A = Full_Area then
        A := (0, 0, Get_Width (Pix), Get_Height (Pix));
      end if;

      --  Create the graphic context required to draw both the image and
      --  the rectange. This cannot be created before starting the main
      --  gtk+ loop, since we need to have access to the physical properties
      --  of the window (its color depth among others), which are only
      --  available when windows have been mapped -- we could also do that
      --  in a "mapped" signal handler

      if GC = null then
         Gdk_New (GC, Get_Window (D));
         Set_Foreground (GC, White (Get_Default_Colormap));
      end if;

      --  Make sure we draw a valid part of the image. Otherwise, we get
      --  gtk+ warnings

      if A.X < 0 then
         A.Width := Gint'Min (Get_Width (Pix), A.Width + A.X);
         A.X := 0;
      end if;

      if A.Y < 0 then
         A.Height := Gint'Min (Get_Height (Pix), A.Height + A.Y);
         A.Y := 0;
      end if;

      --  In case the image is smaller than the drawing area, make sure
      --  we pass the proper width and height

      if A.Width > Get_Width (Pix) - A.X then
         A.Width := Gint'Min (A.Width, Get_Width (Pix) - A.X);
         A.Width := Gint'Max (0, A.Width);
      end if;

      if A.Height > Get_Height (Pix) - A.Y then
         A.Height := Gint'Min (A.Height, Get_Height (Pix) - A.Y);
         A.Height := Gint'Max (0, A.Height);
      end if;

      if A.Width > 0 and then A.Height > 0 then
         Render_To_Drawable
           (Pix, Drawable => Get_Window (D),
            Gc     => GC,
            Src_X  => A.X,
            Src_Y  => A.Y,
            Dest_X => A.X,
            Dest_Y => A.Y,
            Width  => A.Width,
            Height => A.Height);
      end if;
   end Draw_Pix;

   ----------------
   -- On_Release --
   ----------------
   --  This callback is called when the user releases the mouse button.
   --  A drawing software would likely keep the rectangle visible to
   --  show the current selection. In this example, we simply hide the
   --  rectangle by redrawing the whole picture.

   function On_Release
      (D : access Gtk_Widget_Record'Class;
       Event : Gdk_Event) return Boolean is
   begin
      Draw_Pix (D);
      return True;
   end On_Release;

   --------------
   -- On_Press --
   --------------
   --  This callback reacts to the initial click by the user. It stores
   --  the current mouse location, so that we can draw the interactive
   --  selection rectangle later on.

   function On_Press
      (D : access Gtk_Widget_Record'Class;
       Event : Gdk_Event) return Boolean is
   begin
      Start_X := Gint (Get_X (Event));
      Start_Y := Gint (Get_Y (Event));
      Prev_X  := Start_X;
      Prev_Y  := Prev_Y;
      return True;
   end On_Press;

   -------------
   -- On_Move --
   -------------
   --  This callback is called when the user moves the mouse while
   --  pressing the button. It is used to update the selection
   --  rectangle. For this, we need to first delete the old one, then
   --  draw the new one. For efficiency reasons, we also need to do the
   --  minimal amount of redrawing, since this callback might be called
   --  often when the mouse is moved.

   function On_Move
      (D : access Gtk_Widget_Record'Class;
       Event : Gdk_Event) return Boolean
   is
      A : Gdk_Rectangle;
   begin
      --  Delete previous rectangle, redrawing as little as possible,
      --  ie just the borders of the rectangle. One limitation in this
      --  small example is when the image is smaller than the drawing
      --  area, the part of the rectangle that is off the image is not
      --  properly deleted. We could either scale the image to fit the
      --  window, or simply clear the area of the drawing area that is
      --  outside of the window.

      A := (Gint'Min (Prev_X, Start_X),
            Gint'Min (Prev_Y, Start_Y),
            abs (Prev_X - Start_X),
            abs (Prev_Y - Start_Y));
      if A.Width > 0 and then A.Height > 0 then
         Draw_Pix (D, (A.X, A.Y, A.Width, 1));
         Draw_Pix (D, (A.X, A.Y, 1, A.Height));
         Draw_Pix (D, (A.X, A.Y + A.Height - 1, A.Width, 1));
         Draw_Pix (D, (A.X + A.Width - 1, A.Y, 1, A.Height));
      end if;

      --  Draw the new rectangle, and memorize its position so that
      --  we can delete it later on.

      Prev_X := Gint (Get_X (Event));
      Prev_Y := Gint (Get_Y (Event));
      A := (Gint'Min (Prev_X, Start_X),
            Gint'Min (Prev_Y, Start_Y),
            abs (Prev_X - Start_X) - 1,
            abs (Prev_Y - Start_Y) - 1);
      if A.Width > 0 and then A.Height > 0 then
         Draw_Rectangle (Get_Window (D), GC, Filled => False,
                         X => A.X, Y => A.Y,
                         Width  => A.Width, Height => A.Height);
      end if;
      return True;
   end On_Move;

   ---------------
   -- On_Expose --
   ---------------
   --  This callback is called every time a part of the window has been
   --  obscured (for instance when a new window was temporarily moved on
   --  top of it), and the system things it needs redrawing.
   --  Some systems will provide caching for you, and will transparently
   --  redraw the window without calling On_Expose. Others will call it
   --  every time any part of the window needs refreshing. In all cases,
   --  your application needs to react to this signal, or the window will
   --  simply appear gray.

   function On_Expose
     (D : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean is
   begin
      --  For efficiency, only redraw relevant area that was damaged. The
      --  rest of the window is still up-to-date.

      Draw_Pix (D, Get_Area (Event));
      return True;
   end On_Expose;

begin
   Init;
   Gtk_New (Win, Window_Toplevel);
   Gtk_New (Draw);
   Add (Win, Draw);

   --  We need to indicate that the drawing area needs to react to mouse
   --  events. Without this call, these signals would not result in calls
   --  to On_Press, On_Release,...

   Add_Events (Draw, Button_Press_Mask or Button_Release_Mask
                     or Button_Motion_Mask);

   --  Load the image from the disk. A real application should test the
   --  error code of course, and provide fallbacks.
   --  Note that at this point the image has not been drawn to the screen

   Gdk_New_From_File (Pix, "../../testgtk/alps.png", Error);

   --  Connect to the relevant signals

   Return_Callback.Connect
     (Draw, "expose_event",
      Return_Callback.To_Marshaller (On_Expose'Unrestricted_Access));
   Return_Callback.Connect
     (Draw, "button_press_event",
      Return_Callback.To_Marshaller (On_Press'Unrestricted_Access));
   Return_Callback.Connect
     (Draw, "button_release_event",
      Return_Callback.To_Marshaller (On_Release'Unrestricted_Access));
   Return_Callback.Connect
     (Draw, "motion_notify_event",
      Return_Callback.To_Marshaller (On_Move'Unrestricted_Access));

   Show_All (Win);

   --  Start the main loop

   Main;
end Rect;
