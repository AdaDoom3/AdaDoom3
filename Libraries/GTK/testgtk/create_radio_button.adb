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

with Gtk.Box; use Gtk.Box;
with Gtk.Radio_Button; use Gtk.Radio_Button;
with Gtk.Widget; use Gtk.Widget;
with Gtk; use Gtk;

package body Create_Radio_Button is

   function Help return String is
   begin
      return "A @bGtk_Radio_Button@B is part of a group. Only one button"
        & " can be selected in this group. If you select a new button,"
        & " the button currently selected is first deselected."
        & ASCII.LF
        & "This group is actually a simple @bWidget_SList.GSList@B. You"
        & " put the first button into the @bNull_List@B, and then add"
        & " the next buttons to the @bGroup@B created.";
   end Help;

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box1, Box2 : Gtk_Box;
      Button     : Gtk_Radio_Button;

   begin
      Set_Label (Frame, "Radio Buttons");

      Gtk_New_Vbox (Box1, False, 0);
      Add (Frame, Box1);

      Gtk_New_Vbox (Box2, False, 10);
      Set_Border_Width (Box2, 10);
      Pack_Start (Box1, Box2, False, False, 0);

      Gtk_New (Button, Widget_SList.Null_List, "button1");
      Pack_Start (Box2, Button, True, True, 0);

      Gtk_New (Button, Get_Group (Button), "button2");
      Set_Active (Button, True);
      Pack_Start (Box2, Button, True, True, 0);

      Gtk_New (Button, Get_Group (Button), "button3");
      Pack_Start (Box2, Button, True, True, 0);

      Show_All (Frame);
   end Run;

end Create_Radio_Button;

