-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2013, AdaCore                   --
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

with System;
with Glib.Type_Conversion_Hooks;
pragma Elaborate_All (Glib.Type_Conversion_Hooks);

package body Gtk.Radio_Menu_Item is

   use Widget_SList;

   -----------------------
   -- Local Subprograms --
   -----------------------

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Radio_Menu_Item_Record);
   pragma Warnings (Off, Type_Conversion);
   --  This package is used to implement a minimal automated type conversion
   --  without having to drag the whole Gtk.Type_Conversion package for the
   --  most common widgets.

   ---------------
   -- Get_Group --
   ---------------

   function Get_Group
     (Radio_Menu_Item : access Gtk_Radio_Menu_Item_Record)
      return Widget_SList.GSlist
   is
      function Internal
        (Radio_Menu_Item : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_radio_menu_item_get_group");

      Group : Widget_SList.GSlist;

   begin
      Set_Object (Group, Internal (Get_Object (Radio_Menu_Item)));
      return Group;
   end Get_Group;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Radio_Menu_Item : out Gtk_Radio_Menu_Item;
      Group           : Widget_SList.GSlist;
      Label           : UTF8_String := "") is
   begin
      Radio_Menu_Item := new Gtk_Radio_Menu_Item_Record;
      Initialize (Radio_Menu_Item, Group, Label);
   end Gtk_New;

   ---------------------------
   -- Gtk_New_With_Mnemonic --
   ---------------------------

   procedure Gtk_New_With_Mnemonic
     (Radio_Menu_Item : out Gtk_Radio_Menu_Item;
      Group           : Widget_SList.GSlist;
      Label           : UTF8_String) is
   begin
      Radio_Menu_Item := new Gtk_Radio_Menu_Item_Record;
      Initialize_With_Mnemonic (Radio_Menu_Item, Group, Label);
   end Gtk_New_With_Mnemonic;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Radio_Menu_Item : access Gtk_Radio_Menu_Item_Record'Class;
      Group           : Widget_SList.GSlist;
      Label           : UTF8_String := "")
   is
      function Internal
        (Group : System.Address; Label : UTF8_String) return System.Address;
      pragma Import (C, Internal, "gtk_radio_menu_item_new_with_label");
      function Internal2
        (Group : System.Address) return System.Address;
      pragma Import (C, Internal2, "gtk_radio_menu_item_new");
   begin
      if Label = "" then
         Set_Object (Radio_Menu_Item, Internal2 (Get_Object (Group)));
      else
         Set_Object
           (Radio_Menu_Item, Internal (Get_Object (Group), Label & ASCII.NUL));
      end if;
   end Initialize;

   ------------------------------
   -- Initialize_With_Mnemonic --
   ------------------------------

   procedure Initialize_With_Mnemonic
     (Radio_Menu_Item : access Gtk_Radio_Menu_Item_Record'Class;
      Group           : Widget_SList.GSlist;
      Label           : UTF8_String)
   is
      function Internal
        (Group : System.Address; Label : UTF8_String) return System.Address;
      pragma Import (C, Internal, "gtk_radio_menu_item_new_with_mnemonic");
   begin
      Set_Object
        (Radio_Menu_Item, Internal (Get_Object (Group), Label & ASCII.NUL));
   end Initialize_With_Mnemonic;

   ---------------------
   -- Selected_Button --
   ---------------------

   function Selected_Button
     (In_Group : Widget_SList.GSlist) return Natural
   is
      J   : Natural := 0;
      Tmp : Widget_SList.GSlist := In_Group;
   begin
      while Tmp /= Widget_SList.Null_List loop
         exit when Get_Active (Gtk_Radio_Menu_Item (Get_Data (Tmp)));
         Tmp := Next (Tmp);
         J := J + 1;
      end loop;

      return J;
   end Selected_Button;

   ---------------
   -- Set_Group --
   ---------------

   procedure Set_Group
     (Radio_Menu_Item : access Gtk_Radio_Menu_Item_Record;
      Group           : Widget_SList.GSlist)
   is
      procedure Internal
        (Radio_Menu_Item : System.Address; Group : System.Address);
      pragma Import (C, Internal, "gtk_radio_menu_item_set_group");

   begin
      Internal (Get_Object (Radio_Menu_Item), Get_Object (Group));

      --  This is a workaround for a bug in gtk+ <= 1.2.7 (that has been
      --  reported) The same code might be included in gtk+ at some point, and
      --  can be removed from here then. ???
      Set_Active (Radio_Menu_Item, False);
   end Set_Group;

   -------------------------
   -- Gtk_New_From_Widget --
   -------------------------

   procedure Gtk_New_From_Widget
     (Radio_Menu_Item : out Gtk_Radio_Menu_Item;
      Group           : access Gtk_Radio_Menu_Item_Record'Class) is
   begin
      Radio_Menu_Item := new Gtk_Radio_Menu_Item_Record;
      Initialize_From_Widget (Radio_Menu_Item, Group);
   end Gtk_New_From_Widget;

   ----------------------------
   -- Initialize_From_Widget --
   ----------------------------

   procedure Initialize_From_Widget
     (Radio_Menu_Item : access Gtk_Radio_Menu_Item_Record'Class;
      Group           : access Gtk_Radio_Menu_Item_Record'Class)
   is
      function Internal (Group : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_radio_menu_item_new_from_widget");
   begin
      Set_Object (Radio_Menu_Item, Internal (Get_Object (Group)));
   end Initialize_From_Widget;

   ------------------------------------
   -- Gtk_New_With_Label_From_Widget --
   ------------------------------------

   procedure Gtk_New_With_Label_From_Widget
     (Radio_Menu_Item : out Gtk_Radio_Menu_Item;
      Group           : access Gtk_Radio_Menu_Item_Record'Class;
      Label           : String) is
   begin
      Radio_Menu_Item := new Gtk_Radio_Menu_Item_Record;
      Initialize_With_Label_From_Widget (Radio_Menu_Item, Group, Label);
   end Gtk_New_With_Label_From_Widget;

   ---------------------------------------
   -- Initialize_With_Label_From_Widget --
   ---------------------------------------

   procedure Initialize_With_Label_From_Widget
     (Radio_Menu_Item : access Gtk_Radio_Menu_Item_Record'Class;
      Group           : access Gtk_Radio_Menu_Item_Record'Class;
      Label           : String)
   is
      function Internal
        (Group : System.Address; Label : String) return System.Address;
      pragma Import
        (C, Internal, "gtk_radio_menu_item_new_with_label_from_widget");
   begin
      Set_Object
        (Radio_Menu_Item, Internal (Get_Object (Group), Label & ASCII.NUL));
   end Initialize_With_Label_From_Widget;

   ---------------------------------------
   -- Gtk_New_With_Mnemonic_From_Widget --
   ---------------------------------------

   procedure Gtk_New_With_Mnemonic_From_Widget
     (Radio_Menu_Item : out Gtk_Radio_Menu_Item;
      Group           : access Gtk_Radio_Menu_Item_Record'Class;
      Label           : String) is
   begin
      Radio_Menu_Item := new Gtk_Radio_Menu_Item_Record;
      Initialize_With_Mnemonic_From_Widget (Radio_Menu_Item, Group, Label);
   end Gtk_New_With_Mnemonic_From_Widget;

   ------------------------------------------
   -- Initialize_With_Mnemonic_From_Widget --
   ------------------------------------------

   procedure Initialize_With_Mnemonic_From_Widget
     (Radio_Menu_Item : access Gtk_Radio_Menu_Item_Record'Class;
      Group           : access Gtk_Radio_Menu_Item_Record'Class;
      Label           : String)
   is
      function Internal
        (Group : System.Address; Label : String)
         return System.Address;
      pragma Import
        (C, Internal, "gtk_radio_menu_item_new_with_mnemonic_from_widget");
   begin
      Set_Object
        (Radio_Menu_Item, Internal (Get_Object (Group), Label & ASCII.NUL));
   end Initialize_With_Mnemonic_From_Widget;

end Gtk.Radio_Menu_Item;
