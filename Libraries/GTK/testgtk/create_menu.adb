-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                 Copyright (C) 2000-2013, AdaCore                  --
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

with Gdk.Color;   use Gdk.Color;

with Gtk;                   use Gtk;
with Gtkada.Handlers;       use Gtkada.Handlers;

with Gtk.Box;               use Gtk.Box;
with Gtk.Button;            use Gtk.Button;
with Gtk.Menu;              use Gtk.Menu;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Menu_Bar;          use Gtk.Menu_Bar;
with Gtk.Menu_Item;         use Gtk.Menu_Item;

--  Gtk.Option_Menu is obsolescent, but we would still like to test it.
--  Deactivate obsolencence warning.
pragma Warnings (Off);
with Gtk.Option_Menu;       use Gtk.Option_Menu;
pragma Warnings (On);

with Gtk.Spin_Button;       use Gtk.Spin_Button;
with Gtk.Radio_Menu_Item;   use Gtk.Radio_Menu_Item;
with Gtk.Tearoff_Menu_Item; use Gtk.Tearoff_Menu_Item;
with Gtk.Widget;            use Gtk.Widget;

with Common;                use Common;

package body Create_Menu is

   package My_Popup is new Gtk.Menu.User_Menu_Popup (Gint);
   use My_Popup;

   procedure Position_At_0
     (Menu : access Gtk_Menu_Record'Class;
      X    : out Gint;
      Y    : out Gint);
   --  Position function at coordinates 0,0.

   procedure Position_At_Data
     (Menu : access Gtk_Menu_Record'Class;
      X    : out Gint;
      Y    : out Gint;
      Val  : access Gint);
   --  Position function at coordinates Val,Val.

   procedure Popup_At_Position (Widget : access GObject_Record'Class);
   --  Callback for the "Popup at given coordinates" button

   procedure Popup (Widget : access Gtk_Button_Record'Class);
   --  Callback for the "Popup at 0,0 coordinates" button

   -------------------
   -- Position_At_0 --
   -------------------

   procedure Position_At_0
     (Menu : access Gtk_Menu_Record'Class;
      X    : out Gint;
      Y    : out Gint)
   is
      pragma Unreferenced (Menu);
   begin
      X := 0;
      Y := 0;
   end Position_At_0;

   ----------------------
   -- Position_At_Data --
   ----------------------

   procedure Position_At_Data
     (Menu : access Gtk_Menu_Record'Class;
      X    : out Gint;
      Y    : out Gint;
      Val  : access Gint)
   is
      pragma Unreferenced (Menu);
   begin
      X := Val.all;
      Y := Val.all;
   end Position_At_Data;

   -----------------------
   -- Popup_At_Position --
   -----------------------

   procedure Popup_At_Position (Widget : access GObject_Record'Class) is
      Spin : constant Gtk_Spin_Button := Gtk_Spin_Button (Widget);
      Menu : Gtk_Menu;
      Menu_Item : Gtk_Menu_Item;
      Val : aliased Gint := Get_Value_As_Int (Spin);
   begin
      Gtk_New (Menu);

      Gtk_New (Menu_Item, "this");
      Append (Menu, Menu_Item);
      Gtk_New (Menu_Item, "menu");
      Append (Menu, Menu_Item);
      Gtk_New (Menu_Item, "should be positioned");
      Append (Menu, Menu_Item);
      Gtk_New (Menu_Item, "at " & Val'Img & "," & Val'Img);
      Append (Menu, Menu_Item);
      Show_All (Menu);
      My_Popup.Popup
        (Menu => Menu,
         Func => Position_At_Data'Access,
         Data => Val'Access);
   end Popup_At_Position;

   -----------
   -- Popup --
   -----------

   procedure Popup (Widget : access Gtk_Button_Record'Class) is
      pragma Unreferenced (Widget);
      Menu : Gtk_Menu;
      Menu_Item : Gtk_Menu_Item;
   begin
      Gtk_New (Menu);

      Gtk_New (Menu_Item, "this");
      Append (Menu, Menu_Item);
      Gtk_New (Menu_Item, "menu");
      Append (Menu, Menu_Item);
      Gtk_New (Menu_Item, "should be positioned");
      Append (Menu, Menu_Item);
      Gtk_New (Menu_Item, "in the top-left corner");
      Append (Menu, Menu_Item);
      Show_All (Menu);
      Popup
        (Menu,
         Parent_Menu_Shell => null,
         Parent_Menu_Item  => null,
         Func              => Position_At_0'Access,
         Button            => 1,
         Activate_Time     => 0);
   end Popup;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return
        "There are several widgets involved in displaying menus. The"
        & " @bGtk_Menu_Bar@B widget is a horizontal menu bar, which normally"
        & " appears at the top of an application. The @bGtk_Menu@B widget is"
        & " the actual menu that pops up. Both @bGtk_Menu_Bar@B and"
        & " @bGtk_Menu@B are subclasses of @bGtk_Menu_Shell@B; a"
        & " @bGtk_Menu_Shell@B contains menu items (@bGtk_Menu_Item@B)."
        & " Each menu item contains text and/or images and can be selected"
        & " by the user."
        & ASCII.LF
        & "This demo shows how to create a @bGtk_Menu_Bar@B, with multiple"
        & " @bGtk_Menu@Bs. Each of this submenu is actually a @btearoff@B menu"
        & ", which means by that clicking on the dashed line, you can simply"
        & " glue the submenu to another place on your desktop, and keep it"
        & " around. To hide it, simply click on the dashed line again."
        & ASCII.LF
        & "There are several kinds of menu item, including plain"
        & " @bGtk_Menu_Item@B, @bGtk_Check_Menu_Item@B which can be"
        & " checked/unchecked, @bGtk_Radio_Menu_Item@B which is a check menu"
        & " item that's in a mutually exclusive group,"
        & " @bGtk_Separator_Menu_Item@B which is a separator bar,"
        & " @bGtk_Tearoff_Menu_Item@B which allows a @bGtk_Menu@B to be torn"
        & " off, and @bGtk_Image_Menu_Item@B which can place a @bGtk_Image@B"
        & " or other widget next to the menu text. A @bGtk_Menu_Item can have"
        & " a submenu, which is simply a @bGtk_Menu@B to pop up when the menu"
        & " item is selected. Typically, all menu items in a menu bar have"
        & " submenus."
        & ASCII.LF
        & "The @bGtk_Option_Menu@B widget is a button that pops up a"
        & " @bGtk_Menu@B when clicked. It's used inside dialogs and such."
        & " This is different from the @bGtk_Combo_Box@B that you can see"
        & " in the @bEntry@B demo, since a @bGtk_Option_Menu@B does not have"
        & " any editable entry associated with it.";
   end Help;

   -----------------
   -- Create_Menu --
   -----------------

   function Create_Menu
     (Depth : Integer; Tearoff : Boolean) return Gtk_Menu
   is
      Menu      : Gtk_Menu;
      Group     : Widget_SList.GSlist;
      Menu_Item : Gtk_Radio_Menu_Item;
      Red : constant Gdk_Color := Parse ("red");
   begin
      Gtk_New (Menu);

      if Tearoff then
         declare
            Tear_Menu : Gtk_Tearoff_Menu_Item;
         begin
            Gtk_New (Tear_Menu);
            Append (Menu, Tear_Menu);
            Show (Tear_Menu);
         end;
      end if;

      for J in 0 .. 5 loop
         Gtk_New (Menu_Item, Group, "Item" & Integer'Image (Depth)
                  & " -" & Integer'Image (J + 1));
         Group := Gtk.Radio_Menu_Item.Get_Group (Menu_Item);
         Append (Menu, Menu_Item);
         Show (Menu_Item);

         if J = 1 then
            for S in Gtk_State_Type'Range loop
               Modify_Fg (Get_Child (Menu_Item), S, Red);
            end loop;
         end if;

         if J = 3 then
            Set_Sensitive (Menu_Item, False);
         end if;

         if Depth > 1 then
            Set_Submenu (Menu_Item, Create_Menu (Depth - 1, Tearoff));
         end if;
      end loop;
      return Menu;
   end Create_Menu;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box1 : Gtk_Box;
      Box2 : Gtk_Box;
      Menu_Bar  : Gtk_Menu_Bar;
      Menu : Gtk_Menu;
      Menu_Item : Gtk_Menu_Item;
      Option_Menu : Gtk_Option_Menu;

      Button : Gtk_Button;
      Spin   : Gtk_Spin_Button;

   begin

      Set_Label (Frame, "Menus");
      Gtk_New_Vbox (Box1, False, 0);
      Add (Frame, Box1);

      Gtk_New (Menu_Bar);
      Pack_Start (Box1, Menu_Bar, False, False, 0);

      Menu := Create_Menu (2, True);

      Gtk_New (Menu_Item, "test" & ASCII.LF & "line2");
      Set_Submenu (Menu_Item, Menu);
      Append (Menu_Bar, Menu_Item);

      Gtk_New (Menu_Item, "foo");
      Set_Submenu (Menu_Item, Create_Menu (3, True));
      Append (Menu_Bar, Menu_Item);

      Gtk_New (Menu_Item, "bar");
      Set_Submenu (Menu_Item, Create_Menu (4, True));

      Set_Right_Justified (Menu_Item, True);
      Append (Menu_Bar, Menu_Item);

      Gtk_New_Vbox (Box2, False, 10);
      Set_Border_Width (Box2, 10);
      Pack_Start (Box1, Box2, False, False, 0);

      Gtk_New (Option_Menu);
      Set_Menu (Option_Menu, Create_Menu (1, False));
      Set_History (Option_Menu, 3);
      Pack_Start (Box2, Option_Menu, False, False, 0);

      Gtk_New (Button, "Popup at 0,0 coordinates");
      Pack_Start (Box1, Button, False, False, 3);

      Button_Handler.Connect
        (Button, "clicked",
         Button_Handler.To_Marshaller (Popup'Access));

      Gtk_New_Hbox (Box2, False, 10);
               Pack_Start (Box1, Box2, False, False, 0);

      Gtk_New (Spin, 0.0, 800.0, 100.0);
      Set_Value (Spin, 200.0);
      Pack_Start (Box2, Spin, False, False, 3);

      Gtk_New (Button, "Popup at given coordinates");
      Pack_Start (Box2, Button, False, False, 3);

      Object_Callback.Object_Connect
        (Button, "clicked",
         Popup_At_Position'Access,
         Spin);

      Show_All (Frame);
   end Run;

end Create_Menu;
