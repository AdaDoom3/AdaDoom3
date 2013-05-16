-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
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
with Interfaces.C.Strings;

with Glib.Type_Conversion_Hooks;

package body Gtk.Tooltips is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Tooltips_Record);
   pragma Warnings (Off, Type_Conversion);

   ------------
   -- Enable --
   ------------

   procedure Enable (Tooltips : access Gtk_Tooltips_Record) is
      procedure Internal (Tooltips : System.Address);
      pragma Import (C, Internal, "gtk_tooltips_enable");

   begin
      Internal (Get_Object (Tooltips));
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (Tooltips : access Gtk_Tooltips_Record) is
      procedure Internal (Tooltips : System.Address);
      pragma Import (C, Internal, "gtk_tooltips_disable");

   begin
      Internal (Get_Object (Tooltips));
   end Disable;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Tooltips) is
   begin
      Widget := new Gtk_Tooltips_Record;
      Gtk.Tooltips.Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Tooltips_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_tooltips_new");

   begin
      Set_Object (Widget, Internal);
   end Initialize;

   ------------------
   -- Force_Window --
   ------------------

   procedure Force_Window (Widget : access Gtk_Tooltips_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_tooltips_force_window");

   begin
      Internal (Get_Object (Widget));
   end Force_Window;

   ---------------
   -- Set_Delay --
   ---------------

   procedure Set_Delay
     (Tooltips : access Gtk_Tooltips_Record;
      Duration : Guint := 500)
   is
      procedure Internal (Tooltips : System.Address; Duration : Guint);
      pragma Import (C, Internal, "gtk_tooltips_set_delay");

   begin
      Internal (Get_Object (Tooltips), Duration);
   end Set_Delay;

   -------------
   -- Set_Tip --
   -------------

   procedure Set_Tip
     (Tooltips    : access Gtk_Tooltips_Record;
      Widget      : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tip_Text    : UTF8_String;
      Tip_Private : UTF8_String := "")
   is
      procedure Internal
        (Tooltips    : System.Address;
         Widget      : System.Address;
         Tip_Text    : UTF8_String;
         Tip_Private : UTF8_String);
      pragma Import (C, Internal, "gtk_tooltips_set_tip");

   begin
      Internal (Get_Object (Tooltips), Get_Object (Widget),
                Tip_Text & ASCII.NUL,
                Tip_Private & ASCII.NUL);
   end Set_Tip;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) return Tooltips_Data
   is
      type C_Data is record
         Tooltips : System.Address;
         Widget   : System.Address;
         Text     : Interfaces.C.Strings.chars_ptr;
         Privat   : Interfaces.C.Strings.chars_ptr;
      end record;
      pragma Convention (C, C_Data);
      --  keep in sync with the C structure.

      type C_Data_Access is access C_Data;
      pragma Convention (C, C_Data_Access);

      function Internal (Widget : System.Address) return C_Data_Access;
      pragma Import (C, Internal, "gtk_tooltips_data_get");

      Ptr  : constant C_Data_Access := Internal (Get_Object (Widget));
      T    : constant String := Interfaces.C.Strings.Value (Ptr.Text);
      P    : constant String := Interfaces.C.Strings.Value (Ptr.Privat);
      Data : Tooltips_Data (Text_Length    => T'Length,
                            Private_Length => P'Length);
      Stub : Gtk_Tooltips_Record;

   begin
      Data.Tooltips     := Gtk_Tooltips (Get_User_Data (Ptr.Tooltips, Stub));
      Data.Widget       := Gtk.Widget.Gtk_Widget (Widget);
      Data.Text         := T;
      Data.Text_Private := P;
      return Data;
   end Get_Data;

   ----------------
   -- Set_Markup --
   ----------------

   procedure Set_Markup
     (Tooltips : access Gtk_Tooltips_Record;
      Text     : UTF8_String)
   is
      procedure Internal
        (Tooltips : System.Address;
         Text     : UTF8_String);
      procedure Internal
        (Tooltips : System.Address;
         Text     : System.Address);
      pragma Import (C, Internal, "gtk_tooltip_set_markup");

   begin
      if Text /= "" then
         Internal (Get_Object (Tooltips), Text & ASCII.NUL);

      else
         Internal (Get_Object (Tooltips), System.Null_Address);
      end if;
   end Set_Markup;

   -------------------------
   -- Set_Icon_From_Stock --
   -------------------------

   procedure Set_Icon_From_Stock
     (Tooltips : access Gtk_Tooltips_Record;
      Stock_Id : String;
      Size     : Gtk.Enums.Gtk_Icon_Size)
   is
      procedure Internal
        (Tooltips : System.Address;
         Stock_Id : UTF8_String;
         Size     : Gtk.Enums.Gtk_Icon_Size);
      procedure Internal
        (Tooltips : System.Address;
         Stock_Id : System.Address;
         Size     : Gtk.Enums.Gtk_Icon_Size);
      pragma Import (C, Internal, "gtk_tooltip_set_icon_from_stock");

   begin
      if Stock_Id /= "" then
         Internal (Get_Object (Tooltips), Stock_Id & ASCII.NUL, Size);

      else
         Internal (Get_Object (Tooltips), System.Null_Address, Size);
      end if;
   end Set_Icon_From_Stock;

end Gtk.Tooltips;
