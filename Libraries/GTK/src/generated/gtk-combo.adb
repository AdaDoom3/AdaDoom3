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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Interfaces.C.Strings;       use Interfaces.C.Strings;

package body Gtk.Combo is
   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Combo_Record);
   pragma Unreferenced (Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Combo_Box : out Gtk_Combo) is
   begin
      Combo_Box := new Gtk_Combo_Record;
      Gtk.Combo.Initialize (Combo_Box);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Combo_Box : access Gtk_Combo_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_combo_new");
   begin
      Set_Object (Combo_Box, Internal);
   end Initialize;

   ----------------------
   -- Disable_Activate --
   ----------------------

   procedure Disable_Activate (Combo_Box : access Gtk_Combo_Record) is
      procedure Internal (Combo_Box : System.Address);
      pragma Import (C, Internal, "gtk_combo_disable_activate");
   begin
      Internal (Get_Object (Combo_Box));
   end Disable_Activate;

   ------------------------
   -- Set_Case_Sensitive --
   ------------------------

   procedure Set_Case_Sensitive
      (Combo_Box : access Gtk_Combo_Record;
       Val       : Boolean := True)
   is
      procedure Internal (Combo_Box : System.Address; Val : Integer);
      pragma Import (C, Internal, "gtk_combo_set_case_sensitive");
   begin
      Internal (Get_Object (Combo_Box), Boolean'Pos (Val));
   end Set_Case_Sensitive;

   ---------------------
   -- Set_Item_String --
   ---------------------

   procedure Set_Item_String
      (Combo_Box  : access Gtk_Combo_Record;
       Item       : access Gtk.Item.Gtk_Item_Record'Class;
       Item_Value : UTF8_String)
   is
      procedure Internal
         (Combo_Box  : System.Address;
          Item       : System.Address;
          Item_Value : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_combo_set_item_string");
      Tmp_Item_Value : Interfaces.C.Strings.chars_ptr := New_String (Item_Value);
   begin
      Internal (Get_Object (Combo_Box), Get_Object (Item), Tmp_Item_Value);
      Free (Tmp_Item_Value);
   end Set_Item_String;

   -------------------------
   -- Set_Popdown_Strings --
   -------------------------

   procedure Set_Popdown_Strings
      (Combo_Box : access Gtk_Combo_Record;
       Strings   : Gtk.Enums.String_List.Glist)
   is
      procedure Internal
         (Combo_Box : System.Address;
          Strings   : System.Address);
      pragma Import (C, Internal, "gtk_combo_set_popdown_strings");
   begin
      Internal (Get_Object (Combo_Box), Gtk.Enums.String_List.Get_Object (Strings));
   end Set_Popdown_Strings;

   --------------------
   -- Set_Use_Arrows --
   --------------------

   procedure Set_Use_Arrows
      (Combo_Box : access Gtk_Combo_Record;
       Val       : Boolean := True)
   is
      procedure Internal (Combo_Box : System.Address; Val : Integer);
      pragma Import (C, Internal, "gtk_combo_set_use_arrows");
   begin
      Internal (Get_Object (Combo_Box), Boolean'Pos (Val));
   end Set_Use_Arrows;

   ---------------------------
   -- Set_Use_Arrows_Always --
   ---------------------------

   procedure Set_Use_Arrows_Always
      (Combo_Box : access Gtk_Combo_Record;
       Val       : Boolean := True)
   is
      procedure Internal (Combo_Box : System.Address; Val : Integer);
      pragma Import (C, Internal, "gtk_combo_set_use_arrows_always");
   begin
      Internal (Get_Object (Combo_Box), Boolean'Pos (Val));
   end Set_Use_Arrows_Always;

   -----------------------
   -- Set_Value_In_List --
   -----------------------

   procedure Set_Value_In_List
      (Combo_Box   : access Gtk_Combo_Record;
       Val         : Boolean := True;
       Ok_If_Empty : Boolean := False)
   is
      procedure Internal
         (Combo_Box   : System.Address;
          Val         : Integer;
          Ok_If_Empty : Integer);
      pragma Import (C, Internal, "gtk_combo_set_value_in_list");
   begin
      Internal (Get_Object (Combo_Box), Boolean'Pos (Val), Boolean'Pos (Ok_If_Empty));
   end Set_Value_In_List;

   ---------------
   -- Get_Entry --
   ---------------

   function Get_Entry
      (Combo_Box : access Gtk_Combo_Record) return Gtk.Widget.Gtk_Widget
   is
      function Internal (Combo_Box : System.Address) return System.Address;
      pragma Import (C, Internal, "gtkada_GtkCombo_get_entry");
      Stub : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Combo_Box)), Stub));
   end Get_Entry;

   --------------
   -- Get_List --
   --------------

   function Get_List
      (Combo_Box : access Gtk_Combo_Record) return Gtk.Widget.Gtk_Widget
   is
      function Internal (Combo_Box : System.Address) return System.Address;
      pragma Import (C, Internal, "gtkada_GtkCombo_get_list");
      Stub : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Combo_Box)), Stub));
   end Get_List;

   ---------------
   -- Set_Entry --
   ---------------

   procedure Set_Entry
      (Combo_Box : access Gtk_Combo_Record;
       Value     : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Combo_Box : System.Address;
          Value     : System.Address);
      pragma Import (C, Internal, "gtkada_GtkCombo_set_entry");
   begin
      Internal (Get_Object (Combo_Box), Get_Object (Value));
   end Set_Entry;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : access Gtk_Combo_Record) return Gtk.Enums.Gtk_Orientation
   is
      function Internal (Self : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_orientable_get_orientation");
   begin
      return Gtk.Enums.Gtk_Orientation'Val (Internal (Get_Object (Self)));
   end Get_Orientation;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
      (Self        : access Gtk_Combo_Record;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal (Self : System.Address; Orientation : Integer);
      pragma Import (C, Internal, "gtk_orientable_set_orientation");
   begin
      Internal (Get_Object (Self), Gtk.Enums.Gtk_Orientation'Pos (Orientation));
   end Set_Orientation;

end Gtk.Combo;
