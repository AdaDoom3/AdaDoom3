-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
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

with Glib;        use Glib;
with Glib.Object; use Glib.Object;

with Gdk.Display; use Gdk.Display;
with Gdk.Screen;  use Gdk.Screen;

with Gtk.Box;          use Gtk.Box;
with Gtk.Button;       use Gtk.Button;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Handlers;
with Gtk.Label;        use Gtk.Label;
with Gtk.Main;         use Gtk.Main;
with Gtk.Menu;         use Gtk.Menu;
with Gtk.Menu_Item;    use Gtk.Menu_Item;
with Gtk.Status_Icon;  use Gtk.Status_Icon;
with Gtk.Stock;        use Gtk.Stock;

with Common; use Common;

package body Create_Status_Icons is

   Initialized  : Boolean := False;
   --  Whether we have already initialized our library-level variables.

   Display      : Gdk_Display;
   Num_Displays : Gint;
   --  Parameters that we'll set when Run is called.  Treat these as
   --  constant, otherwise.

   type Image_Type is (Info, Warning, Error);
   Current_Image : Image_Type := Info;
   --  We use this to keep some state information about the current
   --  icon being displayed in the status area.

   type Icon_Array is array (Gint range <>) of Gtk_Status_Icon;
   type Icon_Array_Access is access Icon_Array;
   Icons : Icon_Array_Access := null;
   --  The icons that we will stick on the display's status icon area.

   package Menu_Item_Handler is
     new Gtk.Handlers.Callback (Gtk_Menu_Item_Record);
   package Status_Icon_Handler is
     new Gtk.Handlers.Callback (Gtk_Status_Icon_Record);

   procedure Blink_Icon_Cb
     (Check_Button : access Gtk_Check_Button_Record'Class);
   procedure Change_Icon_Cb (Button : access Gtk_Button_Record'Class);
   procedure Popup_Menu_Cb (Status_Icon : access Gtk_Status_Icon_Record'Class);
   procedure Show_Icon_Cb
     (Check_Button : access Gtk_Check_Button_Record'Class);
   --  Callback procedures

   -------------------
   -- Blink_Icon_Cb --
   -------------------

   procedure Blink_Icon_Cb
     (Check_Button : access Gtk_Check_Button_Record'Class)
   is
      Blink : constant Boolean := Get_Active (Check_Button);
   begin
      for I in 1 .. Num_Displays loop
         Set_Blinking (Icons (I), Blink);
      end loop;
   end Blink_Icon_Cb;

   -----------------
   -- Change_Icon --
   -----------------

   procedure Change_Icon is
   begin
      --  Advance to next image
      if Current_Image = Image_Type'Last then
         Current_Image := Image_Type'First;
      else
         Current_Image := Image_Type'Succ (Current_Image);
      end if;

      --  Change all images on all displays.
      for I in 1 .. Num_Displays loop
         case Current_Image is
            when Info =>
               Set_From_Icon_Name (Icons (I), Stock_Dialog_Info);
               Set_Tooltip_Text (Icons (I), "Some Information ...");
            when Warning =>
               Set_From_Icon_Name (Icons (I), Stock_Dialog_Warning);
               Set_Tooltip_Text (Icons (I), "Some Warning ...");
            when Error =>
               Set_From_Icon_Name (Icons (I), Stock_Dialog_Error);
               Set_Tooltip_Text (Icons (I), "Some Error ...");
         end case;
      end loop;
   end Change_Icon;

   --------------------
   -- Change_Icon_Cb --
   --------------------

   procedure Change_Icon_Cb (Button : access Gtk_Button_Record'Class) is
      pragma Unreferenced (Button);
   begin
      Change_Icon;
   end Change_Icon_Cb;

   --------------------
   -- Change_Icon_Cb --
   --------------------

   procedure Change_Icon_Cb (Menu_Item : access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Menu_Item);
   begin
      Change_Icon;
   end Change_Icon_Cb;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "a @bGtk_Status_Icon@B is used to display an icon in a"
        & " ""system tray.""  The icon can have a tooltip, and the user"
        & " can interact with it by activating it or popping up a context"
        & " menu. Critical information should not solely be displayed in a"
        & " @bGtk_Status_Icon@B, since it may not be visible (e.g. when the"
        & " user doesn't have a notification area on his panel). This can be"
        & " checked with @bIs_Embedded@B.";
   end Help;

   -------------------
   -- Popup_Menu_Cb --
   -------------------

   procedure Popup_Menu_Cb
     (Status_Icon : access Gtk_Status_Icon_Record'Class)
   is
      Menu      : Gtk_Menu;
      Menu_Item : Gtk_Menu_Item;
   begin
      Gtk_New (Menu);

      Set_Screen (Menu, Get_Screen (Status_Icon));

      Gtk_New (Menu_Item, "Change Icon");
      Menu_Item_Handler.Connect (Menu_Item, "activate", Change_Icon_Cb'Access);
      Append (Menu, Menu_Item);
      Show (Menu_Item);

      Popup
        (Menu          => Menu,
         Func          => Position_Menu'Access,
         User_Data     => Get_Object (Status_Icon),
         Activate_Time => Get_Current_Event_Time);
   end Popup_Menu_Cb;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box1          : Gtk_Box;
      Button1       : Gtk_Button;
      Check_Button1 : Gtk_Check_Button;
      Label1        : Gtk_Label;
   begin
      if not Initialized then
         --  We can't define these as constants (i.e. at elaboration time)
         --  because it's too early then.  Define them now, when the
         --  infrastructure is active and after this test is invoked.
         Display := Get_Default;
         Num_Displays := Get_N_Screens (Display);

         --  Create and dock all of our status icons.  Start them off as
         --  hidden.
         Icons := new Icon_Array (1 .. Num_Displays);
         for I in 1 .. Num_Displays loop
            Gtk_New (Icons (I));
            Set_Screen (Icons (I), Get_Screen (Display, I - 1));
            Status_Icon_Handler.Connect
              (Icons (I), "popup-menu", Popup_Menu_Cb'Access);
         end loop;

         Initialized := True;
      end if;

      --  Reset all of our icons' variable settings.
      for I in 1 .. Num_Displays loop
         Set_From_Icon_Name (Icons (I), Stock_Dialog_Info);
         Set_Tooltip_Text (Icons (I), "Some Information ...");
         Set_Visible (Icons (I), True);
         Set_Blinking (Icons (I), False);
      end loop;

      Gtk.Frame.Set_Label (Frame, "Status_Icons");

      Gtk_New_Vbox (Box1, Homogeneous => False, Spacing => 0);
      Gtk.Frame.Add (Frame, Box1);

      Gtk_New (Label1, "Look for the Status Icon in the system tray.");
      Pack_Start (Box1, Label1, False, False, 10);
      Gtk_New (Label1, "Click on Help for more information.");
      Pack_Start (Box1, Label1, False, False, 10);

      Gtk_New (Check_Button1, "Show Icon");
      Check_Handler.Connect (Check_Button1, "clicked", Show_Icon_Cb'Access);
      Set_Active (Check_Button1, True);
      Pack_Start (Box1, Check_Button1, False, False, 0);

      Gtk_New (Check_Button1, "Blink Icon");
      Check_Handler.Connect (Check_Button1, "clicked", Blink_Icon_Cb'Access);
      Set_Active (Check_Button1, False);
      Pack_Start (Box1, Check_Button1, False, False, 0);

      Gtk_New (Button1, "Change Icon (Info/Warning/Error)");
      Button_Handler.Connect (Button1, "clicked", Change_Icon_Cb'Access);
      Pack_Start (Box1, Button1, False, False, 0);

      Show_All (Box1);
   end Run;

   ------------------
   -- Show_Icon_Cb --
   ------------------

   procedure Show_Icon_Cb
     (Check_Button : access Gtk_Check_Button_Record'Class)
   is
      Show : constant Boolean := Get_Active (Check_Button);
   begin
      for I in 1 .. Num_Displays loop
         Set_Visible (Icons (I), Show);
      end loop;
   end Show_Icon_Cb;

end Create_Status_Icons;
