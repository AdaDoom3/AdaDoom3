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

with Gtk.Button; use Gtk.Button;
with Gtk.Fixed; use Gtk.Fixed;
with Gtk; use Gtk;

package body Create_Fixed is

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "@bGtk_Fixed@B is the only container that does not do any layout "
        & "for you. Instead, you can place the children at the exact "
        & "coordinates you want, as in this example."
        & ASCII.LF
        & "It is not recommended to use this container, since you have to "
        & "handle the resizing of the window yourself, and nothing is done "
        & "for you."
        & ASCII.LF
        & "If you need to use this container, it probably means that the "
        & "design of your application is not correct and should be rethought.";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Fix     : Gtk_Fixed;
      Button  : Gtk_Button;

   begin
      Gtk.Frame.Set_Label (Frame, "Fixed");

      Gtk_New (Fix);
      Add (Frame, Fix);

      Gtk_New (Button, "(10, 10)");
      Put (Fix, Button, 10, 10);

      Gtk_New (Button, "(135, 123)");
      Put (Fix, Button, 135, 123);

      Gtk_New (Button, "(265, 243)");
      Put (Fix, Button, 265, 243);

      Gtk_New (Button, "(165, 243)");
      Put (Fix, Button, 165, 243);

      Show_All (Frame);
   end Run;

end Create_Fixed;

