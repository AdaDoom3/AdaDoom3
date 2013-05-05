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

package body Gtk.Expander is
   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Expander_Record);
   pragma Unreferenced (Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Expander : out Gtk_Expander; Label : UTF8_String) is
   begin
      Expander := new Gtk_Expander_Record;
      Gtk.Expander.Initialize (Expander, Label);
   end Gtk_New;

   ---------------------------
   -- Gtk_New_With_Mnemonic --
   ---------------------------

   procedure Gtk_New_With_Mnemonic
      (Expander : out Gtk_Expander;
       Label    : UTF8_String)
   is
   begin
      Expander := new Gtk_Expander_Record;
      Gtk.Expander.Initialize_With_Mnemonic (Expander, Label);
   end Gtk_New_With_Mnemonic;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Expander : access Gtk_Expander_Record'Class;
       Label    : UTF8_String)
   is
      function Internal
         (Label : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_expander_new");
      Tmp_Label  : Interfaces.C.Strings.chars_ptr := New_String (Label);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Label);
      Free (Tmp_Label);
      Set_Object (Expander, Tmp_Return);
   end Initialize;

   ------------------------------
   -- Initialize_With_Mnemonic --
   ------------------------------

   procedure Initialize_With_Mnemonic
      (Expander : access Gtk_Expander_Record'Class;
       Label    : UTF8_String)
   is
      function Internal
         (Label : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_expander_new_with_mnemonic");
      Tmp_Label  : Interfaces.C.Strings.chars_ptr := New_String (Label);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Label);
      Free (Tmp_Label);
      Set_Object (Expander, Tmp_Return);
   end Initialize_With_Mnemonic;

   ------------------
   -- Get_Expanded --
   ------------------

   function Get_Expanded
      (Expander : access Gtk_Expander_Record) return Boolean
   is
      function Internal (Expander : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_expander_get_expanded");
   begin
      return Boolean'Val (Internal (Get_Object (Expander)));
   end Get_Expanded;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label
      (Expander : access Gtk_Expander_Record) return UTF8_String
   is
      function Internal
         (Expander : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_expander_get_label");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Expander)));
   end Get_Label;

   --------------------
   -- Get_Label_Fill --
   --------------------

   function Get_Label_Fill
      (Expander : access Gtk_Expander_Record) return Boolean
   is
      function Internal (Expander : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_expander_get_label_fill");
   begin
      return Boolean'Val (Internal (Get_Object (Expander)));
   end Get_Label_Fill;

   ----------------------
   -- Get_Label_Widget --
   ----------------------

   function Get_Label_Widget
      (Expander : access Gtk_Expander_Record) return Gtk.Widget.Gtk_Widget
   is
      function Internal (Expander : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_expander_get_label_widget");
      Stub : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Expander)), Stub));
   end Get_Label_Widget;

   -----------------
   -- Get_Spacing --
   -----------------

   function Get_Spacing (Expander : access Gtk_Expander_Record) return Gint is
      function Internal (Expander : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_expander_get_spacing");
   begin
      return Internal (Get_Object (Expander));
   end Get_Spacing;

   --------------------
   -- Get_Use_Markup --
   --------------------

   function Get_Use_Markup
      (Expander : access Gtk_Expander_Record) return Boolean
   is
      function Internal (Expander : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_expander_get_use_markup");
   begin
      return Boolean'Val (Internal (Get_Object (Expander)));
   end Get_Use_Markup;

   -----------------------
   -- Get_Use_Underline --
   -----------------------

   function Get_Use_Underline
      (Expander : access Gtk_Expander_Record) return Boolean
   is
      function Internal (Expander : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_expander_get_use_underline");
   begin
      return Boolean'Val (Internal (Get_Object (Expander)));
   end Get_Use_Underline;

   ------------------
   -- Set_Expanded --
   ------------------

   procedure Set_Expanded
      (Expander : access Gtk_Expander_Record;
       Expanded : Boolean)
   is
      procedure Internal (Expander : System.Address; Expanded : Integer);
      pragma Import (C, Internal, "gtk_expander_set_expanded");
   begin
      Internal (Get_Object (Expander), Boolean'Pos (Expanded));
   end Set_Expanded;

   ---------------
   -- Set_Label --
   ---------------

   procedure Set_Label
      (Expander : access Gtk_Expander_Record;
       Label    : UTF8_String)
   is
      procedure Internal
         (Expander : System.Address;
          Label    : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_expander_set_label");
      Tmp_Label : Interfaces.C.Strings.chars_ptr := New_String (Label);
   begin
      Internal (Get_Object (Expander), Tmp_Label);
      Free (Tmp_Label);
   end Set_Label;

   --------------------
   -- Set_Label_Fill --
   --------------------

   procedure Set_Label_Fill
      (Expander   : access Gtk_Expander_Record;
       Label_Fill : Boolean)
   is
      procedure Internal (Expander : System.Address; Label_Fill : Integer);
      pragma Import (C, Internal, "gtk_expander_set_label_fill");
   begin
      Internal (Get_Object (Expander), Boolean'Pos (Label_Fill));
   end Set_Label_Fill;

   ----------------------
   -- Set_Label_Widget --
   ----------------------

   procedure Set_Label_Widget
      (Expander     : access Gtk_Expander_Record;
       Label_Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Expander     : System.Address;
          Label_Widget : System.Address);
      pragma Import (C, Internal, "gtk_expander_set_label_widget");
   begin
      Internal (Get_Object (Expander), Get_Object (Label_Widget));
   end Set_Label_Widget;

   -----------------
   -- Set_Spacing --
   -----------------

   procedure Set_Spacing
      (Expander : access Gtk_Expander_Record;
       Spacing  : Gint)
   is
      procedure Internal (Expander : System.Address; Spacing : Gint);
      pragma Import (C, Internal, "gtk_expander_set_spacing");
   begin
      Internal (Get_Object (Expander), Spacing);
   end Set_Spacing;

   --------------------
   -- Set_Use_Markup --
   --------------------

   procedure Set_Use_Markup
      (Expander   : access Gtk_Expander_Record;
       Use_Markup : Boolean)
   is
      procedure Internal (Expander : System.Address; Use_Markup : Integer);
      pragma Import (C, Internal, "gtk_expander_set_use_markup");
   begin
      Internal (Get_Object (Expander), Boolean'Pos (Use_Markup));
   end Set_Use_Markup;

   -----------------------
   -- Set_Use_Underline --
   -----------------------

   procedure Set_Use_Underline
      (Expander      : access Gtk_Expander_Record;
       Use_Underline : Boolean)
   is
      procedure Internal
         (Expander      : System.Address;
          Use_Underline : Integer);
      pragma Import (C, Internal, "gtk_expander_set_use_underline");
   begin
      Internal (Get_Object (Expander), Boolean'Pos (Use_Underline));
   end Set_Use_Underline;

end Gtk.Expander;
