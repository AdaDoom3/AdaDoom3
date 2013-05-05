-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                Copyright (C) 2000-2013, AdaCore                   --
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
with Glib.Object;      use Glib.Object;
with Gdk.Event;        use Gdk.Event;
with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Ruler;        use Gtk.Ruler;
with Gtk.Handlers;     use Gtk.Handlers;
with Gtk.Table;        use Gtk.Table;
with Gtk.Widget;       use Gtk.Widget;
with Gtk;              use Gtk;

package body Create_Rulers is

   package Motion_Cb is new Handlers.Return_Callback
     (Gtk_Widget_Record, Gint);

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "The @bGtk_Ruler@B widget is used to display the cursor"
        & " coordinates in an image. Note that to use it, you need to modify"
        & " the event mask of the widget to allow for @bPointer_Motion_Mask@B"
        & " events.";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Ruler : Gtk_Ruler;
      Table : Gtk_Table;
      Darea : Gtk_Drawing_Area;

   begin
      Set_Label (Frame, "Ruler");

      Gtk_New (Table, 2, 2, False);
      Add (Frame, Table);

      Gtk_New (Darea);
      Unrealize (Darea);
      Set_Events (Darea, Pointer_Motion_Mask + Pointer_Motion_Hint_Mask);
      Attach
        (Table, Darea, 1, 2, 1, 2,
         Expand + Enums.Fill, Expand + Enums.Fill, 0, 0);

      Gtk_New_Hruler (Ruler);
      Set_Range (Ruler, 5.0, 15.0, 0.0, 20.0);
      Motion_Cb.Object_Connect
        (GObject (Darea), "motion_notify_event",
         Motion_Cb.To_Marshaller (Default_Motion_Notify_Event'Access),
         Slot_Object => Ruler);
      Attach
        (Table, Ruler, 1, 2, 0, 1, Expand + Enums.Fill, Enums.Fill, 0, 0);

      Gtk_New_Vruler (Ruler);
      Set_Range (Ruler, 5.0, 15.0, 0.0, 20.0);
      Motion_Cb.Object_Connect
        (GObject (Darea), "motion_notify_event",
         Motion_Cb.To_Marshaller (Default_Motion_Notify_Event'Access),
         Slot_Object => Ruler);
      Attach
        (Table, Ruler, 0, 1, 1, 2, Enums.Fill, Expand + Enums.Fill, 0, 0);

      Show_All (Frame);
   end Run;

end Create_Rulers;
