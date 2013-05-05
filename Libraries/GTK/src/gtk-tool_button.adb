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
with Gtk.Tool_Item;        use Gtk.Tool_Item;
with Gtk.Widget;           use Gtk.Widget;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;               use System;

with Glib.Type_Conversion_Hooks;

package body Gtk.Tool_Button is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Tool_Button_Record);
   pragma Warnings (Off, Type_Conversion);

   -------------------
   -- Get_Icon_Name --
   -------------------

   function Get_Icon_Name
     (Button : access Gtk_Tool_Button_Record)
      return String
   is
      function Internal
        (Button : System.Address)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_tool_button_get_icon_name");
   begin
      return Value (Internal (Get_Object (Button)));
   end Get_Icon_Name;

   ---------------------
   -- Get_Icon_Widget --
   ---------------------

   function Get_Icon_Widget
     (Button : access Gtk_Tool_Button_Record)
      return Gtk_Widget
   is
      function Internal
        (Button : System.Address)
         return System.Address;
      pragma Import (C, Internal, "gtk_tool_button_get_icon_widget");
      Stub : Gtk_Widget_Record;
   begin
      return Gtk_Widget
        (Get_User_Data
           (Internal (Get_Object (Button)), Stub));
   end Get_Icon_Widget;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label
     (Button : access Gtk_Tool_Button_Record) return String
   is
      function Internal
        (Button : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_tool_button_get_label");
   begin
      return Value (Internal (Get_Object (Button)));
   end Get_Label;

   ----------------------
   -- Get_Label_Widget --
   ----------------------

   function Get_Label_Widget
     (Button : access Gtk_Tool_Button_Record)
      return Gtk_Widget
   is
      function Internal
        (Button : System.Address)
         return System.Address;
      pragma Import (C, Internal, "gtk_tool_button_get_label_widget");
      Stub : Gtk_Widget_Record;
   begin
      return Gtk_Widget
        (Get_User_Data
           (Internal (Get_Object (Button)), Stub));
   end Get_Label_Widget;

   ------------------
   -- Get_Stock_Id --
   ------------------

   function Get_Stock_Id
     (Button : access Gtk_Tool_Button_Record)
      return String
   is
      function Internal
        (Button : System.Address)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_tool_button_get_stock_id");
   begin
      return Value (Internal (Get_Object (Button)));
   end Get_Stock_Id;

   -----------------------
   -- Get_Use_Underline --
   -----------------------

   function Get_Use_Underline
     (Button : access Gtk_Tool_Button_Record)
      return Boolean
   is
      function Internal
        (Button : System.Address)
         return Gboolean;
      pragma Import (C, Internal, "gtk_tool_button_get_use_underline");
   begin
      return Boolean'Val (Internal (Get_Object (Button)));
   end Get_Use_Underline;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Button      : out Gtk_Tool_Button;
      Icon_Widget : Gtk_Widget := null;
      Label       : String := "")
   is
   begin
      Button := new Gtk_Tool_Button_Record;
      Initialize
        (Button,
         Icon_Widget,
         Label);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Button      : access Gtk_Tool_Button_Record'Class;
      Icon_Widget : Gtk_Widget := null;
      Label       : String := "")
   is
      function Internal
        (Icon_Widget : System.Address;
         Label       : chars_ptr)
         return System.Address;
      pragma Import (C, Internal, "gtk_tool_button_new");
      Str : chars_ptr := String_Or_Null (Label);
      Obj : System.Address := System.Null_Address;
   begin
      if Icon_Widget /= null then
         Obj := Get_Object (Icon_Widget);
      end if;

      Set_Object (Button, Internal (Obj, Str));
      Free (Str);
   end Initialize;

   ------------------------
   -- Gtk_New_From_Stock --
   ------------------------

   procedure Gtk_New_From_Stock
     (Button   : out Gtk_Tool_Button;
      Stock_Id : String)
   is
   begin
      Button := new Gtk_Tool_Button_Record;
      Initialize_From_Stock (Button, Stock_Id);
   end Gtk_New_From_Stock;

   ---------------------------
   -- Initialize_From_Stock --
   ---------------------------

   procedure Initialize_From_Stock
     (Button   : access Gtk_Tool_Button_Record'Class;
      Stock_Id : String)
   is
      function Internal (Stock_Id : String) return System.Address;
      pragma Import (C, Internal, "gtk_tool_button_new_from_stock");
   begin
      Set_Object (Button, Internal (Stock_Id & ASCII.NUL));
   end Initialize_From_Stock;

   -------------------
   -- Set_Icon_Name --
   -------------------

   procedure Set_Icon_Name
     (Button    : access Gtk_Tool_Button_Record;
      Icon_Name : String)
   is
      procedure Internal
        (Button    : System.Address;
         Icon_Name : String);
      pragma Import (C, Internal, "gtk_tool_button_set_icon_name");
   begin
      Internal (Get_Object (Button), Icon_Name & ASCII.NUL);
   end Set_Icon_Name;

   ---------------------
   -- Set_Icon_Widget --
   ---------------------

   procedure Set_Icon_Widget
     (Button      : access Gtk_Tool_Button_Record;
      Icon_Widget : Gtk_Widget := null)
   is
      procedure Internal
        (Button      : System.Address;
         Icon_Widget : System.Address);
      pragma Import (C, Internal, "gtk_tool_button_set_icon_widget");
   begin
      Internal (Get_Object (Button), Get_Object (Icon_Widget));
   end Set_Icon_Widget;

   ---------------
   -- Set_Label --
   ---------------

   procedure Set_Label
     (Button : access Gtk_Tool_Button_Record;
      Label  : String)
   is
      procedure Internal
        (Button : System.Address;
         Label  : String);
      pragma Import (C, Internal, "gtk_tool_button_set_label");
   begin
      Internal (Get_Object (Button), Label & ASCII.NUL);
   end Set_Label;

   ----------------------
   -- Set_Label_Widget --
   ----------------------

   procedure Set_Label_Widget
     (Button       : access Gtk_Tool_Button_Record;
      Label_Widget : Gtk_Widget := null)
   is
      procedure Internal
        (Button       : System.Address;
         Label_Widget : System.Address);
      pragma Import (C, Internal, "gtk_tool_button_set_label_widget");
   begin
      Internal (Get_Object (Button), Get_Object (Label_Widget));
   end Set_Label_Widget;

   ------------------
   -- Set_Stock_Id --
   ------------------

   procedure Set_Stock_Id
     (Button   : access Gtk_Tool_Button_Record;
      Stock_Id : String)
   is
      procedure Internal
        (Button   : System.Address;
         Stock_Id : String);
      pragma Import (C, Internal, "gtk_tool_button_set_stock_id");
   begin
      Internal (Get_Object (Button), Stock_Id & ASCII.NUL);
   end Set_Stock_Id;

   -----------------------
   -- Set_Use_Underline --
   -----------------------

   procedure Set_Use_Underline
     (Button        : access Gtk_Tool_Button_Record;
      Use_Underline : Boolean := True)
   is
      procedure Internal
        (Button        : System.Address;
         Use_Underline : Gboolean);
      pragma Import (C, Internal, "gtk_tool_button_set_use_underline");
   begin
      Internal (Get_Object (Button), Boolean'Pos (Use_Underline));
   end Set_Use_Underline;

end Gtk.Tool_Button;
