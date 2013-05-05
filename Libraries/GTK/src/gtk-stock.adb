-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                Copyright (C) 2001-2003 ACT-Europe                 --
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

with Interfaces.C.Strings;

package body Gtk.Stock is

   package ICS renames Interfaces.C.Strings;

   ---------
   -- Add --
   ---------

   procedure Add (Item : Gtk_Stock_Item) is
      procedure Internal (Item : Gtk_Stock_Item; N_Items : Guint := 1);
      pragma Import (C, Internal, "gtk_stock_add");

   begin
      Internal (Item);
   end Add;

   procedure Add (Items : Gtk_Stock_Item_Array) is
      procedure Internal (Items : Gtk_Stock_Item_Array; N_Items : Guint);
      pragma Import (C, Internal, "gtk_stock_add");

   begin
      Internal (Items, Items'Length);
   end Add;

   ----------------
   -- Add_Static --
   ----------------

   procedure Add_Static (Item : Gtk_Stock_Item) is
      procedure Internal (Item : Gtk_Stock_Item; N_Items : Guint := 1);
      pragma Import (C, Internal, "gtk_stock_add_static");

   begin
      Internal (Item);
   end Add_Static;

   procedure Add_Static (Items : Gtk_Stock_Item_Array) is
      procedure Internal (Items : Gtk_Stock_Item_Array; N_Items : Guint);
      pragma Import (C, Internal, "gtk_stock_add_static");

   begin
      Internal (Items, Items'Length);
   end Add_Static;

   ----------
   -- Free --
   ----------

   procedure Free (Item : in out Gtk_Stock_Item) is
   begin
      ICS.Free (Item.Stock_Id);
      ICS.Free (Item.Label);
      ICS.Free (Item.Translation_Domain);
   end Free;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Item               : out Gtk_Stock_Item;
      Stock_Id           : String;
      Label              : UTF8_String;
      Modifier           : Gdk.Types.Gdk_Modifier_Type;
      Keyval             : Gdk.Types.Gdk_Key_Type;
      Translation_Domain : String) is
   begin
      Item.Stock_Id           := ICS.New_String (Stock_Id);
      Item.Label              := ICS.New_String (Label);
      Item.Modifier           := Modifier;
      Item.Keyval             := Keyval;
      Item.Translation_Domain := ICS.New_String (Translation_Domain);
   end Gtk_New;

   ------------
   -- Lookup --
   ------------

   procedure Lookup
     (Stock_Id : String;
      Item     : out Gtk_Stock_Item;
      Success  : out Boolean)
   is
      function Internal
        (Stock_Id : String; Item : access Gtk_Stock_Item) return Gboolean;
      pragma Import (C, Internal, "gtk_stock_lookup");

      Tmp : aliased Gtk_Stock_Item;

   begin
      Success := Boolean'Val (Internal (Stock_Id & ASCII.NUL, Tmp'Access));

      if Success then
         Item := Tmp;
      end if;
   end Lookup;

end Gtk.Stock;
