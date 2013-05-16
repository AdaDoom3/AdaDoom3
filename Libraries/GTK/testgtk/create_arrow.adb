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
with Gtk.Arrow; use Gtk.Arrow;
with Gtk.Box; use Gtk.Box;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Label; use Gtk.Label;
with Gtk.Table; use Gtk.Table;
with Gtk; use Gtk;

package body Create_Arrow is

   function Help return String is
   begin
      return "This demo shows the four possible types of @bGtk_Shadow_Type@B"
        & " that can be used within @bGtkAda@B."
        & ASCII.LF
        & "It also demonstrates how items can be organized in a @bGtk_Table@B"
        & " to position them as you want. Each of the groups @bGtk_Arrow@B +"
        & " @bGtk_Label@B is first put in a @bGtk_Box@B, that is then put in"
        & " a table element.";
   end Help;


   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box1      : Gtk_Box;
      Box2      : Gtk_Box;
      Table     : Gtk_Table;
      Arrow     : Gtk_Arrow;
      Label     : Gtk_Label;

   begin
      Set_Label (Frame, "Arrows");

      Gtk_New_Vbox (Box1, Homogeneous => False, Spacing => 0);
      Add (Frame, Box1);

      Gtk_New (Table, Rows => 3, Columns => 3, Homogeneous => False);
      Set_Row_Spacings (Table, Spacing => 5);
      Set_Col_Spacings (Table, Spacing => 5);
      Set_Border_Width (Table, Border_Width => 10);
      Pack_Start (Box1, Table, Expand => False, Fill => False, Padding => 0);

      Gtk_New_Vbox (Box2, Homogeneous => False, Spacing => 0);
      Gtk_New (Arrow,
               Arrow_Type  => Arrow_Up,
               Shadow_Type => Shadow_In);
      Pack_Start (Box2, Arrow, Expand => True, Fill => True, Padding => 0);
      Gtk_New (Label, "Shadow_In");
      Pack_Start (Box2, Label, Expand => True, Fill => True, Padding => 0);
      Attach (Table, Box2, 1, 2, 0, 1,
              Enums.Expand or Enums.Fill, Enums.Expand or Enums.Fill, 0, 0);

      Gtk_New_Vbox (Box2, Homogeneous => False, Spacing => 0);
      Gtk_New (Arrow,
               Arrow_Type  => Arrow_Left,
               Shadow_Type => Shadow_Out);
      Pack_Start (Box2, Arrow, Expand => True, Fill => True, Padding => 0);
      Gtk_New (Label, "Shadow_Out");
      Pack_Start (Box2, Label, Expand => True, Fill => True, Padding => 0);
      Attach (Table, Box2, 0, 1, 1, 2,
              Enums.Expand or Enums.Fill, Enums.Expand or Enums.Fill, 0, 0);

      Gtk_New_Vbox (Box2, Homogeneous => False, Spacing => 0);
      Gtk_New (Arrow,
               Arrow_Type  => Arrow_Right,
               Shadow_Type => Shadow_Etched_In);
      Pack_Start (Box2, Arrow, Expand => True, Fill => True, Padding => 0);
      Gtk_New (Label, "Shadow_Etched_In");
      Pack_Start (Box2, Label, Expand => True, Fill => True, Padding => 0);
      Attach (Table, Box2, 2, 3, 1, 2,
              Enums.Expand or Enums.Fill, Enums.Expand or Enums.Fill, 0, 0);

      Gtk_New_Vbox (Box2, Homogeneous => False, Spacing => 0);
      Gtk_New (Arrow,
               Arrow_Type  => Arrow_Down,
               Shadow_Type => Shadow_Etched_Out);
      Pack_Start (Box2, Arrow, Expand => True, Fill => True, Padding => 0);
      Gtk_New (Label, "Shadow_Etched_Out");
      Pack_Start (Box2, Label, Expand => True, Fill => True, Padding => 0);
      Attach (Table, Box2, 1, 2, 2, 3,
              Enums.Expand or Enums.Fill, Enums.Expand or Enums.Fill, 0, 0);

      Show_All (Box1);
   end Run;

end Create_Arrow;

