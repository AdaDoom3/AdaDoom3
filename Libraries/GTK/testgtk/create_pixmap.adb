-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                     Copyright 2000-2006 AdaCore                   --
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

with Gdk.Bitmap; use Gdk.Bitmap;
with Gdk.Pixmap; use Gdk.Pixmap;
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Label; use Gtk.Label;
with Gtk.Image; use Gtk.Image;
with Gtk.Style; use Gtk.Style;
with Gtk; use Gtk;

package body Create_Pixmap is

   function Help return String is
   begin
      return "This demo simply shows how you can put a pixmap in a"
        & " @bGtk_Button@B simply by putting both in a @bGtk_Box@B, and then"
        & " associating the box with the button.";
   end Help;

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box1      : Gtk_Box;
      Box2      : Gtk_Box;
      Box3      : Gtk_Box;
      Button    : Gtk_Button;
      Style     : Gtk_Style;
      Pixmap    : Gdk_Pixmap;
      Mask      : Gdk_Bitmap;
      PixmapWid : Gtk_Image;
      Label     : Gtk_Label;

   begin

      Set_Label (Frame, "Pixmap");

      Gtk_New_Vbox (Box1, False, 0);
      Add (Frame, Box1);

      Gtk_New_Vbox (Box2, False, 10);
      Set_Border_Width (Box2, 10);
      Pack_Start (Box1, Box2, True, True, 0);

      Gtk_New (Button);
      Pack_Start (Box2, Button, False, False, 0);

      Style := Get_Style (Button);
      Create_From_Xpm (Pixmap, Get_Window (Frame), Mask,
                       Get_Bg (Style, State_Normal), "test.xpm");
      Gtk_New (PixmapWid, Pixmap, Mask);

      Gtk_New (Label, "Pixmap" & ASCII.LF & "test");
      Gtk_New_Hbox (Box3, False, 0);
      Set_Border_Width (Box3, 2);
      Add (Box3, PixmapWid);
      Add (Box3, Label);
      Add (Button, Box3);

      Show_All (Frame);
   end Run;

end Create_Pixmap;

