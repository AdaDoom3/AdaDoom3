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

with Glib; use Glib;
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Enums;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Label; use Gtk.Label;
with Gtk.Separator; use Gtk.Separator;
with Gtk; use Gtk;

package body Create_Box is

   procedure Add_Buttons (Vbox        : Gtk_Box;
                          Message     : String;
                          Homogeneous : Boolean;
                          Expand      : Boolean := False;
                          Fill        : Boolean := False);
   --  Add the buttons within Vbox

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This demo show how you can efficiently use the @bGtk_Box@B " &
        "container." &
        ASCII.LF &
        ASCII.LF &
        "The upper half shows the different " &
        "combinations of the parameters @bHomogeneous@B, @bExpand@B and " &
        "@bFill@B. Note that for homogeneous boxes, @bExpand@B is irrelevant."
        & ASCII.LF &
        " - @bHomogeneous@B: If True all the widgets in the box will have the "
        & "same size as the largest child."
        & ASCII.LF &
        " - @bExpand@B: If True, the widget size will be bigger than the "
        & "minimum requested if more space is available. Its exact size "
        & "depends on the @bFill@B parameter"
        & ASCII.LF &
        " - @bFill@B: If False, the widget will be surrounded by empty space,"
        & " but its real size will be the minimum it requested."
        & ASCII.LF
        & ASCII.LF &
        "The second part of the demo shows the difference between "
        & "@bPack_Start@B and @bPack_End@B. The two resulting groups are "
        & "separated by a space that expands when the box is resized. This "
        & "space of course exists only when the widgets do not expand. "
        & "The buttons are inserted in the order specified (first button, "
        & " second button, ...)";
   end Help;

   -----------------
   -- Add_Buttons --
   -----------------

   procedure Add_Buttons (Vbox        : Gtk_Box;
                          Message     : String;
                          Homogeneous : Boolean;
                          Expand      : Boolean := False;
                          Fill        : Boolean := False)
   is
      Button : Gtk_Button;
      Box    : Gtk_Box;
      Label  : Gtk_Label;

   begin
      Gtk_New (Label, Message);
      Pack_Start (Vbox, Label, Expand => False, Fill => False);

      Gtk_New_Hbox (Box, Homogeneous => Homogeneous);
      Pack_Start (Vbox, Box, Expand => False, Fill => False);

      --  Use a function from one of the implemented interfaces.
      --  This call is not needed, and is just here to check the binding
      --  itself.
      Set_Orientation (Box, Gtk.Enums.Orientation_Horizontal);

      Gtk_New (Button, "Small");
      Pack_Start (Box, Button, Expand => Expand, Fill => Fill);

      Gtk_New (Button, "A bit longer");
      Pack_Start (Box, Button, Expand => Expand, Fill => Fill);

      Gtk_New (Button, "The longest button");
      Pack_Start (Box, Button, Expand => Expand, Fill => Fill);

   end Add_Buttons;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Vbox    : Gtk_Box;
      Box     : Gtk_Box;
      Button  : Gtk_Button;
      Sep     : Gtk_Separator;

   begin
      Gtk.Frame.Set_Label (Frame, "Boxes");

      Gtk_New_Vbox (Vbox, Homogeneous => False, Spacing => 25);
      Add (Frame, Vbox);

      Gtk_New_Vbox (Box, Homogeneous => True);
      Pack_Start (Vbox, Box, Expand => False, Fill => False);
      Add_Buttons (Box, "Homogeneous => False, Expand => False",
                   Homogeneous => False, Expand => False, Fill => False);
      Add_Buttons (Box, "Homogeneous => False, Expand => True, Fill => False",
                   Homogeneous => False, Expand => True, Fill => False);
      Add_Buttons (Box, "Homogeneous => False, Expand => True, Fill => True",
                   Homogeneous => False, Expand => True, Fill => True);
      Add_Buttons (Box, "Homogeneous => True, Fill => False",
                   Homogeneous => True, Fill => False);
      Add_Buttons (Box, "Homogeneous => True, Fill => True",
                   Homogeneous => True, Fill => True);

      Gtk_New_Hseparator (Sep);
      Pack_Start (Vbox, Sep, Expand => False, Fill => True);

      Gtk_New_Vbox (Box, Homogeneous => False);
      Pack_Start (Vbox, Box, Expand => True, Fill => True, Padding => 10);

      Gtk_New (Button, "Pack_Start, First Button");
      Pack_Start (Box, Button, Expand => False, Fill => False);

      Gtk_New (Button, "Pack_Start, Second Button");
      Pack_Start (Box, Button, Expand => False, Fill => False);

      Gtk_New (Button, "Pack_Start, Third Button");
      Pack_Start (Box, Button, Expand => False, Fill => False);

      Gtk_New (Button, "Pack_End, First Button");
      Pack_End (Box, Button, Expand => False, Fill => False);

      Gtk_New (Button, "Pack_End, Second Button");
      Pack_End (Box, Button, Expand => False, Fill => False);

      Gtk_New (Button, "Pack_End, Third Button");
      Pack_End (Box, Button, Expand => False, Fill => False);

      Show_All (Frame);
   end Run;

end Create_Box;
