-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
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

with Glib; use Glib;
with Gtk.Box; use Gtk.Box;
with Gtk.Calendar; use Gtk.Calendar;
with Gtk.Frame; use Gtk.Frame;
with Gtk; use Gtk;

package body Create_Calendar is

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @bGtk_Calendar@B is a simple way to interactively select"
        & " a date. A callback can be set for every change to the date,"
        & " for instance the date, the month,...";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box1     : Gtk_Box;
      Calendar : Gtk_Calendar;
   begin
      Set_Label (Frame, "Calendar");

      Gtk_New_Vbox (Box1, Homogeneous => False, Spacing => 0);
      Add (Frame, Box1);
      Show (Box1);

      Gtk_New (Calendar);
      Pack_Start (Box1, Calendar, False, False, 0);
      Show (Calendar);
   end Run;

end Create_Calendar;

