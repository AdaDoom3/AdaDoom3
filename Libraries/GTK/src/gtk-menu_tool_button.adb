-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                 Copyright (C) 2006-2013, AdaCore                  --
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

with Gtkada.Bindings;      use Gtkada.Bindings;
with Gtk.Menu;             use Gtk.Menu;
with Gtk.Tooltips;         use Gtk.Tooltips;
with Gtk.Widget;           use Gtk.Widget;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;

with Glib.Type_Conversion_Hooks;

package body Gtk.Menu_Tool_Button is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Menu_Tool_Button_Record);
   pragma Warnings (Off, Type_Conversion);

   --------------
   -- Get_Menu --
   --------------

   function Get_Menu
     (Button : access Gtk_Menu_Tool_Button_Record)
      return Gtk_Menu
   is
      function Internal (Button : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_menu_tool_button_get_menu");
      Stub : Gtk_Menu_Record;
   begin
      return Gtk_Menu
        (Get_User_Data (Internal (Get_Object (Button)), Stub));
   end Get_Menu;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Menu        : out Gtk_Menu_Tool_Button;
      Icon_Widget : Gtk_Widget := null;
      Label       : String := "")
   is
   begin
      Menu := new Gtk_Menu_Tool_Button_Record;
      Initialize (Menu, Icon_Widget, Label);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Menu        : access Gtk_Menu_Tool_Button_Record'Class;
      Icon_Widget : Gtk_Widget := null;
      Label       : String := "")
   is
      function Internal
        (Icon_Widget : System.Address;
         Label       : chars_ptr)
         return System.Address;
      pragma Import (C, Internal, "gtk_menu_tool_button_new");
      Str : chars_ptr := String_Or_Null (Label);
      Obj : System.Address := System.Null_Address;
   begin
      if Icon_Widget /= null then
         Obj := Get_Object (Icon_Widget);
      end if;
      Set_Object (Menu, Internal (Obj, Str));
      Free (Str);
   end Initialize;

   ------------------------
   -- Gtk_New_From_Stock --
   ------------------------

   procedure Gtk_New_From_Stock
     (Menu   : out Gtk_Menu_Tool_Button;
      Stock_Id : String)
   is
   begin
      Menu := new Gtk_Menu_Tool_Button_Record;
      Initialize_From_Stock (Menu, Stock_Id);
   end Gtk_New_From_Stock;

   ---------------------------
   -- Initialize_From_Stock --
   ---------------------------

   procedure Initialize_From_Stock
     (Menu     : access Gtk_Menu_Tool_Button_Record'Class;
      Stock_Id : String)
   is
      function Internal (Stock_Id : String) return System.Address;
      pragma Import (C, Internal, "gtk_menu_tool_button_new_from_stock");
   begin
      Set_Object (Menu, Internal (Stock_Id & ASCII.NUL));
   end Initialize_From_Stock;

   -----------------------
   -- Set_Arrow_Tooltip --
   -----------------------

   procedure Set_Arrow_Tooltip
     (Button      : access Gtk_Menu_Tool_Button_Record;
      Tooltips    : access Gtk_Tooltips_Record'Class;
      Tip_Text    : String;
      Tip_Private : String := "")
   is
      procedure Internal
        (Button      : System.Address;
         Tooltips    : System.Address;
         Tip_Text    : String;
         Tip_Private : String);
      pragma Import (C, Internal, "gtk_menu_tool_button_set_arrow_tooltip");
   begin
      Internal (Get_Object (Button), Get_Object (Tooltips),
                Tip_Text & ASCII.NUL, Tip_Private & ASCII.NUL);
   end Set_Arrow_Tooltip;

   ------------------------------
   -- Set_Arrow_Tooltip_Markup --
   ------------------------------

   procedure Set_Arrow_Tooltip_Markup
     (Button : access Gtk_Menu_Tool_Button_Record;
      Markup : String)
   is
      procedure Internal (Button : System.Address; Markup : String);
      pragma Import
        (C, Internal, "gtk_menu_tool_button_set_arrow_tooltip_markup");
   begin
      Internal (Get_Object (Button), Markup & ASCII.NUL);
   end Set_Arrow_Tooltip_Markup;

   ----------------------------
   -- Set_Arrow_Tooltip_Text --
   ----------------------------

   procedure Set_Arrow_Tooltip_Text
     (Button : access Gtk_Menu_Tool_Button_Record;
      Text   : String)
   is
      procedure Internal (Button : System.Address; Text : String);
      pragma Import
        (C, Internal, "gtk_menu_tool_button_set_arrow_tooltip_text");
   begin
      Internal (Get_Object (Button), Text & ASCII.NUL);
   end Set_Arrow_Tooltip_Text;

   --------------
   -- Set_Menu --
   --------------

   procedure Set_Menu
     (Button : access Gtk_Menu_Tool_Button_Record;
      Menu   : access Gtk_Menu_Record'Class)
   is
      procedure Internal
        (Button : System.Address;
         Menu   : System.Address);
      pragma Import (C, Internal, "gtk_menu_tool_button_set_menu");
   begin
      Internal (Get_Object (Button), Get_Object (Menu));
   end Set_Menu;
end Gtk.Menu_Tool_Button;
