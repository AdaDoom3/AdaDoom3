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
with Gtk.Enums;   use Gtk.Enums;
with Glib.Type_Conversion_Hooks;
pragma Elaborate_All (Glib.Type_Conversion_Hooks);

package body Gtk.Menu_Bar is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Menu_Bar_Record);
   pragma Warnings (Off, Type_Conversion);
   --  This package is used to implement a minimal automated type conversion
   --  without having to drag the whole Gtk.Type_Conversion package for the
   --  most common widgets.

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Menu_Bar : out Gtk_Menu_Bar) is
   begin
      Menu_Bar := new Gtk_Menu_Bar_Record;
      Gtk.Menu_Bar.Initialize (Menu_Bar);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Menu_Bar : access Gtk_Menu_Bar_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_menu_bar_new");

   begin
      Set_Object (Menu_Bar, Internal);
   end Initialize;

   ------------------------------
   -- Get_Child_Pack_Direction --
   ------------------------------

   function Get_Child_Pack_Direction
     (Menubar : access Gtk_Menu_Bar_Record)
      return Gtk_Pack_Direction
   is
      function Internal (Menubar : System.Address) return Gtk_Pack_Direction;
      pragma Import (C, Internal, "gtk_menu_bar_get_child_pack_direction");
   begin
      return Internal (Get_Object (Menubar));
   end Get_Child_Pack_Direction;

   ------------------------
   -- Get_Pack_Direction --
   ------------------------

   function Get_Pack_Direction
     (Menubar : access Gtk_Menu_Bar_Record)
      return Gtk_Pack_Direction
   is
      function Internal
        (Menubar : System.Address)
         return Gtk_Pack_Direction;
      pragma Import (C, Internal, "gtk_menu_bar_get_pack_direction");
   begin
      return Internal (Get_Object (Menubar));
   end Get_Pack_Direction;

   ------------------------------
   -- Set_Child_Pack_Direction --
   ------------------------------

   procedure Set_Child_Pack_Direction
     (Menubar        : access Gtk_Menu_Bar_Record;
      Child_Pack_Dir : Gtk_Pack_Direction)
   is
      procedure Internal
        (Menubar        : System.Address;
         Child_Pack_Dir : Gtk_Pack_Direction);
      pragma Import (C, Internal, "gtk_menu_bar_set_child_pack_direction");
   begin
      Internal (Get_Object (Menubar), Child_Pack_Dir);
   end Set_Child_Pack_Direction;

   ------------------------
   -- Set_Pack_Direction --
   ------------------------

   procedure Set_Pack_Direction
     (Menubar  : access Gtk_Menu_Bar_Record;
      Pack_Dir : Gtk_Pack_Direction)
   is
      procedure Internal
        (Menubar  : System.Address;
         Pack_Dir : Gtk_Pack_Direction);
      pragma Import (C, Internal, "gtk_menu_bar_set_pack_direction");
   begin
      Internal (Get_Object (Menubar), Pack_Dir);
   end Set_Pack_Direction;

end Gtk.Menu_Bar;
