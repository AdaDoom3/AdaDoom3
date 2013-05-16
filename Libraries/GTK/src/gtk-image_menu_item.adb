-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2001-2013, AdaCore                   --
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

package body Gtk.Image_Menu_Item is

   -----------------------
   -- Local Subprograms --
   -----------------------

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Image_Menu_Item_Record);
   pragma Warnings (Off, Type_Conversion);
   --  This package is used to implement a minimal automated type conversion
   --  without having to drag the whole Gtk.Type_Conversion package for the
   --  most common widgets.

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget : out Gtk_Image_Menu_Item;
      Label  : UTF8_String) is
   begin
      Widget := new Gtk_Image_Menu_Item_Record;
      Initialize (Widget, Label);
   end Gtk_New;

   procedure Gtk_New
     (Widget      : out Gtk_Image_Menu_Item;
      Stock_Id    : String;
      Accel_Group : Gtk.Accel_Group.Gtk_Accel_Group) is
   begin
      Widget := new Gtk_Image_Menu_Item_Record;
      Initialize (Widget, Stock_Id, Accel_Group);
   end Gtk_New;

   ------------------------
   -- Gtk_New_From_Stock --
   ------------------------

   procedure Gtk_New_From_Stock
     (Widget   : out Gtk_Image_Menu_Item;
      Stock_Id : String) is
   begin
      Widget := new Gtk_Image_Menu_Item_Record;
      Initialize_From_Stock (Widget, Stock_Id);
   end Gtk_New_From_Stock;

   ---------------------------
   -- Gtk_New_With_Mnemonic --
   ---------------------------

   procedure Gtk_New_With_Mnemonic
     (Widget : out Gtk_Image_Menu_Item;
      Label  : UTF8_String) is
   begin
      Widget := new Gtk_Image_Menu_Item_Record;
      Initialize_With_Mnemonic (Widget, Label);
   end Gtk_New_With_Mnemonic;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget : access Gtk_Image_Menu_Item_Record'Class;
      Label  : UTF8_String)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_image_menu_item_new");

      function Internal2 (S : UTF8_String) return System.Address;
      pragma Import (C, Internal2, "gtk_image_menu_item_new_with_label");

   begin
      if Label = "" then
         Set_Object (Widget, Internal);
      else
         Set_Object (Widget, Internal2 (Label & ASCII.NUL));
      end if;
   end Initialize;

   procedure Initialize
     (Widget      : access Gtk_Image_Menu_Item_Record'Class;
      Stock_Id    : String;
      Accel_Group : Gtk.Accel_Group.Gtk_Accel_Group)
   is
      function Internal
        (Stock_Id    : String;
         Accel_Group : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_image_menu_item_new_from_stock");

   begin
      Set_Object (Widget,
                  Internal (Stock_Id & ASCII.NUL, Get_Object (Accel_Group)));
   end Initialize;

   ------------------------------
   -- Initialize_With_Mnemonic --
   ------------------------------

   procedure Initialize_With_Mnemonic
     (Widget : access Gtk_Image_Menu_Item_Record'Class;
      Label  : UTF8_String)
   is
      function Internal (Label : UTF8_String) return System.Address;
      pragma Import (C, Internal, "gtk_image_menu_item_new_with_mnemonic");

   begin
      Set_Object (Widget, Internal (Label & ASCII.NUL));
   end Initialize_With_Mnemonic;

   ---------------------------
   -- Initialize_From_Stock --
   ---------------------------

   procedure Initialize_From_Stock
     (Widget   : access Gtk_Image_Menu_Item_Record'Class;
      Stock_Id : String)
   is
      function Internal
        (Stock : String;
         Accel_Group : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_image_menu_item_new_from_stock");

   begin
      Set_Object
        (Widget, Internal (Stock_Id & ASCII.NUL, System.Null_Address));
   end Initialize_From_Stock;

   ---------------------------
   -- Get_Always_Show_Image --
   ---------------------------

   function Get_Always_Show_Image
     (Image_Menu_Item : access Gtk_Image_Menu_Item_Record'Class)
      return Boolean
   is
      function Internal (Image_Menu_Item : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_image_menu_item_get_always_show_image");
   begin
      return Boolean'Val (Internal (Get_Object (Image_Menu_Item)));
   end Get_Always_Show_Image;

   -------------------
   -- Get_Use_Stock --
   -------------------

   function Get_Use_Stock
     (Image_Menu_Item : access Gtk_Image_Menu_Item_Record'Class)
      return Boolean
   is
      function Internal (Image_Menu_Item : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_image_menu_item_get_use_stock");
   begin
      return Boolean'Val (Internal (Get_Object (Image_Menu_Item)));
   end Get_Use_Stock;

   ---------------
   -- Get_Image --
   ---------------

   function Get_Image
     (Menu_Item : access Gtk_Image_Menu_Item_Record)
      return Gtk.Widget.Gtk_Widget
   is
      function Internal (Menu_Item : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_image_menu_item_get_image");

   begin
      return Gtk.Widget.Convert (Internal (Get_Object (Menu_Item)));
   end Get_Image;

   ---------------------
   -- Set_Accel_Group --
   ---------------------

   procedure Set_Accel_Group
     (Image_Menu_Item : access Gtk_Image_Menu_Item_Record'Class;
      Accel_Group     : access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class)
   is
      procedure Internal
        (Image_Menu_Item : System.Address;
         Accel_Group     : System.Address);
      pragma Import (C, Internal, "gtk_image_menu_item_set_accel_group");
   begin
      Internal (Get_Object (Image_Menu_Item), Get_Object (Accel_Group));
   end Set_Accel_Group;

   ---------------------------
   -- Set_Always_Show_Image --
   ---------------------------

   procedure Set_Always_Show_Image
     (Image_Menu_Item : access Gtk_Image_Menu_Item_Record'Class;
      Always_Show     : Boolean)
   is
      procedure Internal
        (Image_Menu_Item : System.Address;
         Always_Show     : Gboolean);
      pragma Import (C, Internal, "gtk_image_menu_item_set_always_show_image");
   begin
      Internal (Get_Object (Image_Menu_Item), Boolean'Pos (Always_Show));
   end Set_Always_Show_Image;

   ---------------
   -- Set_Image --
   ---------------

   procedure Set_Image
     (Menu_Item : access Gtk_Image_Menu_Item_Record;
      Image     : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
        (Menu_Item : System.Address;
         Image     : System.Address);
      pragma Import (C, Internal, "gtk_image_menu_item_set_image");

   begin
      Internal (Get_Object (Menu_Item), Get_Object (Image));
   end Set_Image;

   -------------------
   -- Set_Use_Stock --
   -------------------

   procedure Set_Use_Stock
     (Image_Menu_Item : access Gtk_Image_Menu_Item_Record'Class;
      Use_Stock       : Boolean)
   is
      procedure Internal
        (Image_Menu_Item : System.Address;
         Use_Stock       : Gboolean);
      pragma Import (C, Internal, "gtk_image_menu_item_set_use_stock");
   begin
      Internal (Get_Object (Image_Menu_Item), Boolean'Pos (Use_Stock));
   end Set_Use_Stock;

end Gtk.Image_Menu_Item;
