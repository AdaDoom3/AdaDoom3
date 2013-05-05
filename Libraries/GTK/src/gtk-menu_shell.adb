-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
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

with System;

with Glib.Type_Conversion_Hooks;

package body Gtk.Menu_Shell is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Menu_Shell_Record);
   pragma Warnings (Off, Type_Conversion);

   -------------------
   -- Activate_Item --
   -------------------

   procedure Activate_Item
     (Menu_Shell       : access Gtk_Menu_Shell_Record;
      Item             : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class;
      Force_Deactivate : Boolean)
   is
      procedure Internal
        (Menu_Shell : System.Address;
         Item       : System.Address;
         Force      : Gint);
      pragma Import (C, Internal, "gtk_menu_shell_activate_item");

   begin
      Internal (Get_Object (Menu_Shell), Get_Object (Item),
                Boolean'Pos (Force_Deactivate));
   end Activate_Item;

   ------------
   -- Append --
   ------------

   procedure Append
     (Menu_Shell : access Gtk_Menu_Shell_Record;
      Child      : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
      procedure Internal (Menu_Shell : System.Address; Child : System.Address);
      pragma Import (C, Internal, "gtk_menu_shell_append");

   begin
      Internal (Get_Object (Menu_Shell), Get_Object (Child));
   end Append;

   ----------------
   -- Deactivate --
   ----------------

   procedure Deactivate (Menu_Shell : access Gtk_Menu_Shell_Record) is
      procedure Internal (Menu_Shell : System.Address);
      pragma Import (C, Internal, "gtk_menu_shell_deactivate");

   begin
      Internal (Get_Object (Menu_Shell));
   end Deactivate;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Menu_Shell : access Gtk_Menu_Shell_Record;
      Child      : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class;
      Position   : Gint)
   is
      procedure Internal
        (Menu_Shell : System.Address;
         Child      : System.Address;
         Position   : Gint);
      pragma Import (C, Internal, "gtk_menu_shell_insert");

   begin
      Internal (Get_Object (Menu_Shell), Get_Object (Child), Position);
   end Insert;

   -------------
   -- Prepend --
   -------------

   procedure Prepend
     (Menu_Shell : access Gtk_Menu_Shell_Record;
      Child      : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
      procedure Internal
        (Menu_Shell : System.Address;
         Child      : System.Address);
      pragma Import (C, Internal, "gtk_menu_shell_prepend");

   begin
      Internal (Get_Object (Menu_Shell), Get_Object (Child));
   end Prepend;

   -----------------
   -- Select_Item --
   -----------------

   procedure Select_Item
     (Menu_Shell : access Gtk_Menu_Shell_Record;
      Item       : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class)
   is
      procedure Internal
        (Menu_Shell : System.Address;
         Child      : System.Address);
      pragma Import (C, Internal, "gtk_menu_shell_select_item");

   begin
      Internal (Get_Object (Menu_Shell), Get_Object (Item));
   end Select_Item;

   --------------
   -- Deselect --
   --------------

   procedure Deselect (Menu_Shell : access Gtk_Menu_Shell_Record) is
      procedure Internal (Menu_Shell : System.Address);
      pragma Import (C, Internal, "gtk_menu_shell_deselect");

   begin
      Internal (Get_Object (Menu_Shell));
   end Deselect;

   ------------
   -- Cancel --
   ------------

   procedure Cancel (Menu_Shell : access Gtk_Menu_Shell_Record) is
      procedure Internal (Menu_Shell : System.Address);
      pragma Import (C, Internal, "gtk_menu_shell_cancel");
   begin
      Internal (Get_Object (Menu_Shell));
   end Cancel;

   --------------------
   -- Get_Take_Focus --
   --------------------

   function Get_Take_Focus
     (Menu_Shell : access Gtk_Menu_Shell_Record)
      return Boolean
   is
      function Internal (Menu_Shell : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_menu_shell_get_take_focus");
   begin
      return Boolean'Val (Internal (Get_Object (Menu_Shell)));
   end Get_Take_Focus;

   ------------------
   -- Select_First --
   ------------------

   procedure Select_First
     (Menu_Shell       : access Gtk_Menu_Shell_Record;
      Search_Sensitive : Boolean)
   is
      procedure Internal
        (Menu_Shell       : System.Address;
         Search_Sensitive : Gboolean);
      pragma Import (C, Internal, "gtk_menu_shell_select_first");
   begin
      Internal (Get_Object (Menu_Shell), Boolean'Pos (Search_Sensitive));
   end Select_First;

   --------------------
   -- Set_Take_Focus --
   --------------------

   procedure Set_Take_Focus
     (Menu_Shell : access Gtk_Menu_Shell_Record;
      Take_Focus : Boolean := True)
   is
      procedure Internal
        (Menu_Shell : System.Address;
         Take_Focus : Gboolean);
      pragma Import (C, Internal, "gtk_menu_shell_set_take_focus");
   begin
      Internal (Get_Object (Menu_Shell), Boolean'Pos (Take_Focus));
   end Set_Take_Focus;

end Gtk.Menu_Shell;
