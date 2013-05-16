-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 2000                            --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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

with Gdk.Drawable;        use Gdk.Drawable;
with Gdk.Event;           use Gdk.Event;
with Gdk.Rectangle;       use Gdk.Rectangle;
with Gdk.Window;          use Gdk.Window;
with Glib;                use Glib;
with Gtk.Adjustment;      use Gtk.Adjustment;
with Gtk.Button;          use Gtk.Button;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Frame;           use Gtk.Frame;
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.Label;           use Gtk.Label;
with Gtk.Layout;          use Gtk.Layout;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Style;           use Gtk.Style;

package body Create_Layout is

   package Event_Cb is new Gtk.Handlers.Return_Callback
     (Gtk_Layout_Record, Boolean);

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @bGtk_Layout@B is a mixture between a @bGtk_Viewport@B and"
        & " a @bGtk_Fixed@B. Its children can be located anywhere, the layout"
        & " does not try to align them in any way. As opposed to a"
        & " @bGtk_Fixed@B, there is no limit to the size of a @bGtk_Layout@B"
        & " and it won't use as much memory as a @bGtk_Fixed@B."
        & ASCII.LF
        & "The area that is currently visible is indicated by two"
        & " @bGtk_Alignment@B widgets. It can thus be put directly into a"
        & " @bGtk_Scrolled_Window@B widget, as is the case in this demo."
        & ASCII.LF
        & "In this demo, the background is painted by a callback on the"
        & " expose_event, and thus does not occupy any memory."
        & " The Layout has a size of 1600 by 128000.";
   end Help;

   --------------------
   -- Expose_Handler --
   --------------------

   function Expose_Handler (Layout : access Gtk_Layout_Record'Class;
                            Event  : Gdk_Event_Expose)
                           return Boolean
   is
      Area : constant Gdk_Rectangle := Get_Area (Event);
      Imin, Imax : Guint;
      Jmin, Jmax : Guint;
   begin
      Imin := Guint (Area.X) / 10;
      Imax := (Guint (Area.X) + Guint (Area.Width) + 9) / 10;
      Jmin := Guint (Area.Y) / 10;
      Jmax := (Guint (Area.Y) + Guint (Area.Height) + 9) / 10;

      Clear_Area (Get_Window (Layout), Area.X, Area.Y, Gint (Area.Width),
                  Gint (Area.Height));

      for I in Imin .. Imax - 1 loop
         for J in Jmin .. Jmax - 1 loop
            if (I + J) mod 2 /= 0 then
               Draw_Rectangle (Get_Bin_Window (Layout),
                               Get_Black_GC (Get_Style (Layout)),
                               True,
                               Gint (10 * I),
                               Gint (10 * J),
                               Gint (1 + I mod 10),
                               Gint (1 + J mod 10));
            end if;
         end loop;
      end loop;
      return True;
   end Expose_Handler;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Scrolled : Gtk_Scrolled_Window;
      Layout   : Gtk_Layout;
      Button   : Gtk_Button;
      Label    : Gtk_Label;
   begin
      Set_Label (Frame, "Layout");
      Gtk_New (Scrolled);
      Set_Shadow_Type (Scrolled, Shadow_In);
      Add (Frame, Scrolled);
      Set_Placement (Scrolled, Corner_Top_Right);

      Gtk_New (Layout);
      Set_Events (Layout, Exposure_Mask);
      Add (Scrolled, Layout);

      Set_Step_Increment (Get_Hadjustment (Layout), 10.0);
      Set_Step_Increment (Get_Vadjustment (Layout), 10.0);


      Event_Cb.Connect (Layout, "expose_event",
                        Event_Cb.To_Marshaller (Expose_Handler'Access));
      Set_Size (Layout, 1600, 128000);

      for I in 0 .. Gint'(15) loop
         for J in 0 .. Gint'(15) loop
            if (I + J) mod 2 /= 0 then
               Gtk_New (Button, "Button " & Gint'Image (I)
                        & " " & Gint'Image (J));
               Put (Layout, Button, J * 100, I * 100);
            else
               Gtk_New (Label, "Label " & Gint'Image (I)
                        & " " & Gint'Image (J));
               Put (Layout, Label, J * 100, I * 100);
            end if;
         end loop;
      end loop;

      for I in 16 .. Gint'(1279) loop
         if I mod 2 /= 0 then
            Gtk_New (Button, "Button " & Gint'Image (I) & " 0");
            Put (Layout, Button, 0, I * 100);
         else
            Gtk_New (Label, "Label " & Gint'Image (I) & " 0");
            Put (Layout, Label, 0, I * 100);
         end if;
      end loop;

      Show_All (Frame);
   end Run;

end Create_Layout;
