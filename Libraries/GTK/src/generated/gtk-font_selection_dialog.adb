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

package body Gtk.Font_Selection_Dialog is
   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Font_Selection_Dialog_Record);
   pragma Unreferenced (Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Dialog : out Gtk_Font_Selection_Dialog;
       Title  : UTF8_String)
   is
   begin
      Dialog := new Gtk_Font_Selection_Dialog_Record;
      Gtk.Font_Selection_Dialog.Initialize (Dialog, Title);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Dialog : access Gtk_Font_Selection_Dialog_Record'Class;
       Title  : UTF8_String)
   is
      function Internal
         (Title : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_font_selection_dialog_new");
      Tmp_Title  : Interfaces.C.Strings.chars_ptr := New_String (Title);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Title);
      Free (Tmp_Title);
      Set_Object (Dialog, Tmp_Return);
   end Initialize;

   ----------------------
   -- Get_Apply_Button --
   ----------------------

   function Get_Apply_Button
      (Dialog : access Gtk_Font_Selection_Dialog_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Dialog : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_font_selection_dialog_get_apply_button");
      Stub : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Dialog)), Stub));
   end Get_Apply_Button;

   -----------------------
   -- Get_Cancel_Button --
   -----------------------

   function Get_Cancel_Button
      (Dialog : access Gtk_Font_Selection_Dialog_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Dialog : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_font_selection_dialog_get_cancel_button");
      Stub : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Dialog)), Stub));
   end Get_Cancel_Button;

   --------------
   -- Get_Font --
   --------------

   function Get_Font
      (Dialog : access Gtk_Font_Selection_Dialog_Record)
       return Gdk.Font.Gdk_Font
   is
      function Internal (Dialog : System.Address) return Gdk.Font.Gdk_Font;
      pragma Import (C, Internal, "gtk_font_selection_dialog_get_font");
   begin
      return Internal (Get_Object (Dialog));
   end Get_Font;

   -------------------
   -- Get_Font_Name --
   -------------------

   function Get_Font_Name
      (Dialog : access Gtk_Font_Selection_Dialog_Record) return UTF8_String
   is
      function Internal
         (Dialog : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_font_selection_dialog_get_font_name");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Dialog)));
   end Get_Font_Name;

   ------------------------
   -- Get_Font_Selection --
   ------------------------

   function Get_Font_Selection
      (Dialog : access Gtk_Font_Selection_Dialog_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Dialog : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_font_selection_dialog_get_font_selection");
      Stub : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Dialog)), Stub));
   end Get_Font_Selection;

   -------------------
   -- Get_Ok_Button --
   -------------------

   function Get_Ok_Button
      (Dialog : access Gtk_Font_Selection_Dialog_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Dialog : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_font_selection_dialog_get_ok_button");
      Stub : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Dialog)), Stub));
   end Get_Ok_Button;

   ----------------------
   -- Get_Preview_Text --
   ----------------------

   function Get_Preview_Text
      (Dialog : access Gtk_Font_Selection_Dialog_Record) return UTF8_String
   is
      function Internal
         (Dialog : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_font_selection_dialog_get_preview_text");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Dialog)));
   end Get_Preview_Text;

   -------------------
   -- Set_Font_Name --
   -------------------

   function Set_Font_Name
      (Dialog   : access Gtk_Font_Selection_Dialog_Record;
       Fontname : UTF8_String) return Boolean
   is
      function Internal
         (Dialog   : System.Address;
          Fontname : Interfaces.C.Strings.chars_ptr) return Integer;
      pragma Import (C, Internal, "gtk_font_selection_dialog_set_font_name");
      Tmp_Fontname : Interfaces.C.Strings.chars_ptr := New_String (Fontname);
      Tmp_Return   : Integer;
   begin
      Tmp_Return := Internal (Get_Object (Dialog), Tmp_Fontname);
      Free (Tmp_Fontname);
      return Boolean'Val (Tmp_Return);
   end Set_Font_Name;

   ----------------------
   -- Set_Preview_Text --
   ----------------------

   procedure Set_Preview_Text
      (Dialog : access Gtk_Font_Selection_Dialog_Record;
       Text   : UTF8_String)
   is
      procedure Internal
         (Dialog : System.Address;
          Text   : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_font_selection_dialog_set_preview_text");
      Tmp_Text : Interfaces.C.Strings.chars_ptr := New_String (Text);
   begin
      Internal (Get_Object (Dialog), Tmp_Text);
      Free (Tmp_Text);
   end Set_Preview_Text;

end Gtk.Font_Selection_Dialog;
