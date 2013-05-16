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
with Gtk.Button; use Gtk.Button;
with Gtk.Button_Box; use Gtk.Button_Box;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Vbutton_Box; use Gtk.Vbutton_Box;
with Gtk; use Gtk;

package body Create_Button_Box is

   function Create_Bbox (Horizontal : in Boolean;
                         Title      : in String;
                         Spacing    : in Gint;
                         Child_W    : in Gint;
                         Child_H    : in Gint;
                         Layout     : in Gtk_Button_Box_Style)
                         return Gtk_Frame;
   --  Create one of the sample button box

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This demo demonstrates the possible @bLayout@B that can be used"
        & " with a @bGtk_Button_Box@B widget." & ASCII.LF
        & "A @bGtk_Button_Box@B is a special kind of @bGtk_Box@B, homogeneous,"
        & " which can organize its children according to a special layout."
        & ASCII.LF
        & "All the boxes here are created exactly the same way, and only the"
        & " layout differs.";
   end Help;

   -----------------
   -- Create_Bbox --
   -----------------

   function Create_Bbox (Horizontal : in Boolean;
                         Title      : in String;
                         Spacing    : in Gint;
                         Child_W    : in Gint;
                         Child_H    : in Gint;
                         Layout     : in Gtk_Button_Box_Style)
                         return Gtk_Frame
   is
      Bbox   : Gtk_Button_Box;
      Button : Gtk_Button;
      Frame  : Gtk_Frame;
   begin
      Gtk_New (Frame, Label => Title);
      if Horizontal then
         declare
            B : Gtk_Hbutton_Box;
         begin
            Gtk_New (B);
            Bbox := Gtk_Button_Box (B);
         end;
      else
         declare
            B : Gtk_Vbutton_Box;
         begin
            Gtk_New (B);
            Bbox := Gtk_Button_Box (B);
         end;
      end if;

      Set_Border_Width (Bbox, Border_Width => 5);
      Add (Frame, Bbox);

      Set_Layout (Bbox, Layout);
      Set_Spacing (Bbox, Spacing);
      Set_Child_Size (Bbox, Child_W, Child_H);

      Gtk_New (Button, Label => "OK");
      Add (Bbox, Button);

      Gtk_New (Button, Label => "Cancel");
      Add (Bbox, Button);

      Gtk_New (Button, Label => "Help");
      Add (Bbox, Button);

      return Frame;
   end Create_Bbox;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Vbox   : Gtk_Box;
      Hbox   : Gtk_Box;
      Main_Vbox : Gtk_Box;
      Frame_Horz : Gtk_Frame;
      Frame_Vert : Gtk_Frame;
   begin
      Set_Label (Frame, "Button Box");

      Gtk_New_Vbox (Main_Vbox, Homogeneous => False, Spacing => 0);
      Add (Frame, Main_Vbox);

      Gtk_New (Frame_Horz, "Horizontal Button Boxes");
      Pack_Start (Main_Vbox,
                  Child   => Frame_Horz,
                  Expand  => True,
                  Fill    => True,
                  Padding => 10);

      Gtk_New_Vbox (Vbox, Homogeneous => False, Spacing => 0);
      Set_Border_Width (Vbox, Border_Width => 10);
      Add (Frame_Horz, Vbox);

      Pack_Start
        (Vbox, Create_Bbox (True, "Spread", 40, 85, 20, Buttonbox_Spread),
         True, True, 0);
      Pack_Start
        (Vbox, Create_Bbox (True, "Edge", 40, 85, 20, Buttonbox_Edge),
         True, True, 5);
      Pack_Start
        (Vbox, Create_Bbox (True, "Start", 40, 85, 20, Buttonbox_Start),
         True, True, 5);
      Pack_Start
        (Vbox, Create_Bbox (True, "End", 40, 85, 20, Buttonbox_End),
         True, True, 5);


      Gtk_New (Frame_Vert, "Vertical Button Boxes");
      Pack_Start (Main_Vbox,
                  Frame_Vert,
                  Expand  => True,
                  Fill    => True,
                  Padding => 10);
      Gtk_New_Hbox (Hbox, Homogeneous => False, Spacing => 0);
      Set_Border_Width (Hbox, Border_Width => 10);
      Add (Frame_Vert, Hbox);

      Pack_Start
        (Hbox, Create_Bbox (False, "Spread", 30, 85, 20, Buttonbox_Spread),
         True, True, 0);
      Pack_Start
        (Hbox, Create_Bbox (False, "Edge", 30, 85, 20, Buttonbox_Edge),
         True, True, 5);
      Pack_Start
        (Hbox, Create_Bbox (False, "Start", 30, 85, 20, Buttonbox_Start),
         True, True, 5);
      Pack_Start
        (Hbox, Create_Bbox (False, "End", 30, 85, 20, Buttonbox_End),
         True, True, 5);
      Show_All (Main_Vbox);
   end Run;

end Create_Button_Box;

