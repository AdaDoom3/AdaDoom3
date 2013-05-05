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


with Gdk.Bitmap;   use Gdk.Bitmap;
with Gdk.Color;    use Gdk.Color;
with Gdk.Pixmap;   use Gdk.Pixmap;
with Gtk.Enums;    use Gtk.Enums;
with Gtk.GEntry;   use Gtk.GEntry;
with Gtk.Handlers; use Gtk.Handlers;
with Gtk.Image;    use Gtk.Image;
with Gtk.Separator_Tool_Item; use Gtk.Separator_Tool_Item;
with Gtk.Tool_Button;         use Gtk.Tool_Button;
with Gtk.Tool_Item; use Gtk.Tool_Item;
with Gtk.Tooltips; use Gtk.Tooltips;
with Gtk.Widget;   use Gtk.Widget;
with Gtk;          use Gtk;

package body Create_Toolbar is

   package Toolbar_Cb is new Handlers.Callback (Gtk_Toolbar_Record);

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @bGtk_Toolbar@B is a set of buttons. Each button is can be"
        & " represented both as a text or an icon, or even both. A tooltip"
        & " is automatically created for each button." & ASCII.LF
        & "The following two signals are defined:" & ASCII.LF
        & "   - ""orientation_changed"": the widget was re-oriented."
        & ASCII.LF
        & "   - ""style_changed"": a new style was selected." & ASCII.LF
        & ASCII.LF
        & "It is worth noting that the toolbar can basically contain any type"
        & " of widget, not only buttons. In this demo, we have added a "
        & " @bGtk_Entry@B widget in the middle.";
   end Help;

   ----------------
   -- New_Pixmap --
   ----------------

   function New_Pixmap (Filename   : in String;
                        Window     : in Gdk_Window;
                        Background : in Gdk_Color)
                        return Gtk_Widget
   is
      Pixmap    : Gdk_Pixmap;
      Mask      : Gdk_Bitmap;
      GtkPixmap : Gtk_Image;
   begin
      Create_From_Xpm (Pixmap, Window, Mask, Background, Filename);
      Gtk_New (GtkPixmap, Pixmap, Mask);
      return Gtk_Widget (GtkPixmap);
   end New_Pixmap;

   --------------------
   -- Set_Horizontal --
   --------------------

   procedure Set_Horizontal (Toolbar : access Gtk_Toolbar_Record'Class) is
   begin
      Set_Orientation (Toolbar, Orientation_Horizontal);
   end Set_Horizontal;

   ------------------
   -- Set_Vertical --
   ------------------

   procedure Set_Vertical (Toolbar : access Gtk_Toolbar_Record'Class) is
   begin
      Set_Orientation (Toolbar, Orientation_Vertical);
   end Set_Vertical;

   ---------------
   -- Set_Icons --
   ---------------

   procedure Set_Icons (Toolbar : access Gtk_Toolbar_Record'Class) is
   begin
      Set_Style (Toolbar, Toolbar_Icons);
   end Set_Icons;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text (Toolbar : access Gtk_Toolbar_Record'Class) is
   begin
      Set_Style (Toolbar, Toolbar_Text);
   end Set_Text;

   --------------
   -- Set_Both --
   --------------

   procedure Set_Both (Toolbar : access Gtk_Toolbar_Record'Class) is
   begin
      Set_Style (Toolbar, Toolbar_Both);
   end Set_Both;

   ---------------------
   -- Set_Small_Space --
   ---------------------

   procedure Set_Small_Space (Toolbar : access Gtk_Toolbar_Record'Class) is
      pragma Warnings (Off, Toolbar);
   begin
      --  Set_Space_Size (Toolbar, 5);
      null;
   end Set_Small_Space;

   -------------------
   -- Set_Big_Space --
   -------------------

   procedure Set_Big_Space (Toolbar : access Gtk_Toolbar_Record'Class) is
      pragma Warnings (Off, Toolbar);
   begin
      --  Set_Space_Size (Toolbar, 10);
      null;
   end Set_Big_Space;

   ----------------
   -- Set_Enable --
   ----------------

   procedure Set_Enable (Toolbar : access Gtk_Toolbar_Record'Class) is
   begin
      Set_Tooltips (Toolbar, True);
   end Set_Enable;

   -----------------
   -- Set_Disable --
   -----------------

   procedure Set_Disable (Toolbar : access Gtk_Toolbar_Record'Class) is
   begin
      Set_Tooltips (Toolbar, False);
   end Set_Disable;

   -------------
   -- Borders --
   -------------

   procedure Borders (Toolbar : access Gtk_Toolbar_Record'Class) is
      pragma Warnings (Off, Toolbar);
   begin
      --  Set_Button_Relief (Toolbar, Relief_Normal);
      null;
   end Borders;

   ----------------
   -- Borderless --
   ----------------

   procedure Borderless (Toolbar : access Gtk_Toolbar_Record'Class) is
      pragma Warnings (Off, Toolbar);
   begin
      --  Set_Button_Relief (Toolbar, Relief_None);
      null;
   end Borderless;

   -----------------------
   -- Space_Style_Empty --
   -----------------------

   procedure Space_Style_Empty (Toolbar : access Gtk_Toolbar_Record'Class) is
      pragma Warnings (Off, Toolbar);
   begin
      --  Set_Space_Style (Toolbar, Toolbar_Space_Empty);
      null;
   end Space_Style_Empty;

   ----------------------
   -- Space_Style_Line --
   ----------------------

   procedure Space_Style_Line (Toolbar : access Gtk_Toolbar_Record'Class) is
      pragma Warnings (Off, Toolbar);
   begin
      --  Set_Space_Style (Toolbar, Toolbar_Space_Line);
      null;
   end Space_Style_Line;

   ------------------
   -- Make_Toolbar --
   ------------------

   procedure Make_Toolbar (Toolbar    : out Gtk_Toolbar;
                           Toplevel   : in Gdk_Window;
                           Style      : in Gtk_Style;
                           With_Entry : in Boolean := False)
   is
      The_Entry : Gtk_Entry;
      Button    : Gtk_Tool_Button;
      Tooltips  : Gtk_Tooltips;
      Separator : Gtk_Separator_Tool_Item;
      Item      : Gtk_Tool_Item;
      Bg        : constant Gdk_Color := Get_Bg (Style, State_Normal);

   begin
      Gtk_New (Toolbar);
      Set_Orientation (Toolbar, Orientation_Horizontal);
      Set_Style (Toolbar, Toolbar_Both);
      Gtk_New (Tooltips);

      Gtk_New (Button, New_Pixmap ("test.xpm", Toplevel, Bg), "Horizontal");
      Set_Tooltip (Button, Tooltips, "Horizontal toolbar layout");
      Insert (Toolbar, Button);
      Toolbar_Cb.Object_Connect
        (Button, "clicked",
         Toolbar_Cb.To_Marshaller (Set_Horizontal'Access),
         Slot_Object => Toolbar);

      Gtk_New (Button, New_Pixmap ("test.xpm", Toplevel, Bg), "Vertical");
      Set_Tooltip (Button, Tooltips, "Vertical toolbar layout");
      Insert (Toolbar, Button);
      Toolbar_Cb.Object_Connect
        (Button, "clicked",
         Toolbar_Cb.To_Marshaller (Set_Vertical'Access),
         Slot_Object => Toolbar);

      Gtk_New (Separator);
      Insert (Toolbar, Separator);

      Gtk_New (Button, New_Pixmap ("test.xpm", Toplevel, Bg), "Icons");
      Set_Tooltip (Button, Tooltips, "Only show toolbar icons");
      Insert (Toolbar, Button);
      Toolbar_Cb.Object_Connect
        (Button, "clicked",
         Toolbar_Cb.To_Marshaller (Set_Icons'Access),
         Slot_Object => Toolbar);

      Gtk_New (Button, New_Pixmap ("test.xpm", Toplevel, Bg), "Text");
      Set_Tooltip (Button, Tooltips, "Only show toolbar text");
      Insert (Toolbar, Button);
      Toolbar_Cb.Object_Connect
        (Button, "clicked",
         Toolbar_Cb.To_Marshaller (Set_Text'Access),
         Slot_Object => Toolbar);

      Gtk_New (Button, New_Pixmap ("test.xpm", Toplevel, Bg), "Both");
      Set_Tooltip (Button, Tooltips, "Show toolbar icons and text");
      Insert (Toolbar, Button);
      Toolbar_Cb.Object_Connect
        (Button, "clicked",
         Toolbar_Cb.To_Marshaller (Set_Both'Access),
         Slot_Object => Toolbar);

      Gtk_New (Separator);
      Insert (Toolbar, Separator);

      if With_Entry then
         Gtk_New (Item);
         Insert (Toolbar, Item);

         Gtk_New (The_Entry);
         Show (The_Entry);
         Add (Item, The_Entry);

         Gtk_New (Separator);
         Insert (Toolbar, Separator);
      end if;

      Gtk_New (Button, New_Pixmap ("test.xpm", Toplevel, Bg), "Small");
      Set_Tooltip (Button, Tooltips, "Use small spaces", "Toolbar/Small");
      Insert (Toolbar, Button);
      Toolbar_Cb.Object_Connect
        (Button, "clicked",
         Toolbar_Cb.To_Marshaller (Set_Small_Space'Access),
         Slot_Object => Toolbar);

      Gtk_New (Button, New_Pixmap ("test.xpm", Toplevel, Bg), "Big");
      Set_Tooltip (Button, Tooltips, "Use big spaces", "Toolbar/Big");
      Insert (Toolbar, Button);
      Toolbar_Cb.Object_Connect
        (Button, "clicked", Toolbar_Cb.To_Marshaller (Set_Big_Space'Access),
         Slot_Object => Toolbar);

      Gtk_New (Separator);
      Insert (Toolbar, Separator);

      Gtk_New (Button, New_Pixmap ("test.xpm", Toplevel, Bg), "Enable");
      Set_Tooltip (Button, Tooltips, "Enable tooltips");
      Insert (Toolbar, Button);
      Toolbar_Cb.Object_Connect
        (Button,
         "clicked", Toolbar_Cb.To_Marshaller (Set_Enable'Access),
         Slot_Object => Toolbar);

      Gtk_New (Button, New_Pixmap ("test.xpm", Toplevel, Bg), "Disable");
      Set_Tooltip (Button, Tooltips, "Disable tooltips");
      Insert (Toolbar, Button);
      Toolbar_Cb.Object_Connect
        (Button,
         "clicked", Toolbar_Cb.To_Marshaller (Set_Disable'Access),
         Slot_Object => Toolbar);

      Gtk_New (Separator);
      Insert (Toolbar, Separator);

      Gtk_New (Button, New_Pixmap ("test.xpm", Toplevel, Bg), "Borders");
      Set_Tooltip (Button, Tooltips, "Show borders");
      Insert (Toolbar, Button);
      Toolbar_Cb.Object_Connect
        (Button,
         "clicked", Toolbar_Cb.To_Marshaller (Borders'Access),
         Slot_Object => Toolbar);

      Gtk_New (Button, New_Pixmap ("test.xpm", Toplevel, Bg), "Borderless");
      Set_Tooltip (Button, Tooltips, "Hide borders");
      Insert (Toolbar, Button);
      Toolbar_Cb.Object_Connect
        (Button,
         "clicked", Toolbar_Cb.To_Marshaller (Borderless'Access),
         Slot_Object => Toolbar);

      Gtk_New (Separator);
      Insert (Toolbar, Separator);

      Gtk_New (Button, New_Pixmap ("test.xpm", Toplevel, Bg), "Empty");
      Set_Tooltip (Button, Tooltips, "Empty spaces");
      Insert (Toolbar, Button);
      Toolbar_Cb.Object_Connect
        (Button,
         "clicked", Toolbar_Cb.To_Marshaller (Space_Style_Empty'Access),
         Slot_Object => Toolbar);

      Gtk_New (Button, New_Pixmap ("test.xpm", Toplevel, Bg), "Lines");
      Set_Tooltip (Button, Tooltips, "Lines in spaces");
      Insert (Toolbar, Button);
      Toolbar_Cb.Object_Connect
        (Button,
         "clicked", Toolbar_Cb.To_Marshaller (Space_Style_Line'Access),
         Slot_Object => Toolbar);

   end Make_Toolbar;

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Toolbar : Gtk_Toolbar;

   begin
      Set_Label (Frame, "Toolbar");
      Make_Toolbar (Toolbar, Get_Window (Frame), Get_Style (Frame), True);
      Set_Orientation (Toolbar, Orientation_Vertical);
      Add (Frame, Toolbar);

      Show_All (Frame);
   end Run;

end Create_Toolbar;

