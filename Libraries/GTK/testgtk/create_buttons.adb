-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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

with Glib;         use Glib;
with Gtk;          use Gtk;
with Gtk.Box;      use Gtk.Box;
with Gtk.Button;   use Gtk.Button;
with Gtk.Enums;    use Gtk.Enums;
with Gtk.Frame;    use Gtk.Frame;
with Gtk.Handlers; use Gtk.Handlers;
with Gtk.Table;    use Gtk.Table;
with Gtk.Widget;   use Gtk.Widget;
with Common;       use Common;

package body Create_Buttons is

   procedure Button_Window (Widget : access Gtk_Button_Record'Class);
   --  Toggles the visibility of Widget

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "a @bGtk_Button@B is the basic widget to which you can associate"
        & " a callback. Whenever the user presses the mouse on the button,"
        & " one or more functions specified by the user can be called.";
   end Help;

   -------------------
   -- Button_Window --
   -------------------

   procedure Button_Window (Widget : access Gtk_Button_Record'Class) is
   begin
      if Visible_Is_Set (Widget) then
         Hide (Widget);
      else
         Show (Widget);
      end if;
   end Button_Window;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box1    : Gtk_Box;
      Table   : Gtk_Table;
      Button  : array (0 .. 8) of Gtk_Button;
      Left_A  : constant array (0 .. 8) of Guint :=
        (0, 1, 2, 0, 2, 1, 1, 2, 0);
      Right_A : constant array (0 .. 8) of Guint :=
        (1, 2, 3, 1, 3, 2, 2, 3, 1);
      Top_A  : constant array (0 .. 8) of Guint := (0, 1, 2, 2, 0, 2, 0, 1, 1);
      Bott_A : constant array (0 .. 8) of Guint := (1, 2, 3, 3, 1, 3, 1, 2, 2);

   begin
      Gtk.Frame.Set_Label (Frame, "Buttons");

      Gtk_New_Vbox (Box1, Homogeneous => False, Spacing => 0);
      Add (Frame, Box1);

      Gtk_New (Table, Rows => 3, Columns => 3, Homogeneous => False);
      Set_Row_Spacings (Table, Spacing => 5);
      Set_Col_Spacings (Table, Spacing => 5);
      Set_Border_Width (Table, Border_Width => 10);
      Pack_Start (Box1, Table, Expand => False, Fill => False, Padding => 0);

      for J in Button'Range loop
         Gtk_New (Button (J), Label => "Button" & Integer'Image (J));
      end loop;

      for J in Button'Range loop
         Button_Handler.Object_Connect
           (Button (J), "clicked",
            Button_Handler.To_Marshaller (Button_Window'Access),
            Button ((J + 1) mod Button'Length));
         Attach (Table, Button (J),
                 Left_A (J), Right_A (J),
                 Top_A (J), Bott_A (J),
                 Expand + Fill,
                 Expand + Fill, Xpadding => 0, Ypadding => 0);
      end loop;

      Show_All (Box1);
   end Run;

end Create_Buttons;
