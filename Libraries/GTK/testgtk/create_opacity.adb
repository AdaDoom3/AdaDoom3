-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                    Copyright (C) 2010-2013, AdaCore               --
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
with Gtk.Adjustment;   use Gtk.Adjustment;
with Gtk.Box;          use Gtk.Box;
with Gtk.Label;        use Gtk.Label;
with Gtk.Scale;        use Gtk.Scale;
with Gtk.Widget;       use Gtk.Widget;
with Gtk.Window;       use Gtk.Window;
with Common;           use Common;

package body Create_Opacity is

   Main_Window : Gtk_Window;
   --  The main testgtk window.

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This demo makes the main window partially"
        & " transparent, with opacity 0 being fully transparent and 1 fully"
        & " opaque.  On X11 this has effect only on X screens with a"
        & " compositing manager running.  On Windows it should always work."
        & "  Note that setting a window's opacity after the window has been"
        & " shown causes it to flicker once on Windows.";
   end Help;

   ----------------------
   -- On_Value_Changed --
   ----------------------

   procedure On_Value_Changed
     (Adjustment : access Gtk_Adjustment_Record'Class)
   is
   begin
      Set_Opacity (Main_Window, Get_Value (Adjustment) / 100.0);
   end On_Value_Changed;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box1, Box2 : Gtk_Box;
      Label      : Gtk_Label;
      Adjustment : Gtk_Adjustment;
      Scale      : Gtk_Scale;
   begin
      --  Record our toplevel widget for use by our callbacks.
      Main_Window := Gtk_Window (Get_Toplevel (Frame));

      Set_Label (Frame, "Opacity");

      Gtk_New_Vbox (Box1, False, 0);
      Add (Frame, Box1);

      Gtk_New_Vbox (Box2, False, 10);
      Set_Border_Width (Box2, 10);
      Pack_Start (Box1, Box2, False, False, 0);

      if Is_Composited (Main_Window) then
         Gtk_New (Label, "The main window IS composited.");
      else
         Gtk_New
           (Label, "The main window IS NOT composited.  This might not work.");
      end if;
      Pack_Start (Box2, Label, False, False, 10);

      Gtk_New (Label, "Opacity setting for the main window:");
      Pack_Start (Box2, Label, False, False, 0);

      Gtk_New
        (Adjustment,
         Value          => Get_Opacity (Main_Window) * 100.0,
         Lower          =>   0.0,
         Upper          => 100.0,
         Step_Increment =>   0.1,
         Page_Increment =>   5.0);
      Gtk_New_Hscale (Scale, Adjustment);
      Pack_Start (Box2, Scale, True, True, 0);

      Adj_Handler.Connect
        (Adjustment, "value_changed",
         Adj_Handler.To_Marshaller (On_Value_Changed'Access));

      Show_All (Frame);
   end Run;

end Create_Opacity;
