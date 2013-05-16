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

package body Gtk.Font_Selection is
   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Font_Selection_Record);
   pragma Unreferenced (Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Fontsel : out Gtk_Font_Selection) is
   begin
      Fontsel := new Gtk_Font_Selection_Record;
      Gtk.Font_Selection.Initialize (Fontsel);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Fontsel : access Gtk_Font_Selection_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_font_selection_new");
   begin
      Set_Object (Fontsel, Internal);
   end Initialize;

   -------------------
   -- Get_Face_List --
   -------------------

   function Get_Face_List
      (Fontsel : access Gtk_Font_Selection_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Fontsel : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_font_selection_get_face_list");
      Stub : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Fontsel)), Stub));
   end Get_Face_List;

   ---------------------
   -- Get_Family_List --
   ---------------------

   function Get_Family_List
      (Fontsel : access Gtk_Font_Selection_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Fontsel : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_font_selection_get_family_list");
      Stub : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Fontsel)), Stub));
   end Get_Family_List;

   --------------
   -- Get_Font --
   --------------

   function Get_Font
      (Fontsel : access Gtk_Font_Selection_Record) return Gdk.Font.Gdk_Font
   is
      function Internal (Fontsel : System.Address) return Gdk.Font.Gdk_Font;
      pragma Import (C, Internal, "gtk_font_selection_get_font");
   begin
      return Internal (Get_Object (Fontsel));
   end Get_Font;

   -------------------
   -- Get_Font_Name --
   -------------------

   function Get_Font_Name
      (Fontsel : access Gtk_Font_Selection_Record) return UTF8_String
   is
      function Internal
         (Fontsel : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_font_selection_get_font_name");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Fontsel)));
   end Get_Font_Name;

   -----------------------
   -- Get_Preview_Entry --
   -----------------------

   function Get_Preview_Entry
      (Fontsel : access Gtk_Font_Selection_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Fontsel : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_font_selection_get_preview_entry");
      Stub : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Fontsel)), Stub));
   end Get_Preview_Entry;

   ----------------------
   -- Get_Preview_Text --
   ----------------------

   function Get_Preview_Text
      (Fontsel : access Gtk_Font_Selection_Record) return UTF8_String
   is
      function Internal
         (Fontsel : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_font_selection_get_preview_text");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Fontsel)));
   end Get_Preview_Text;

   --------------
   -- Get_Size --
   --------------

   function Get_Size
      (Fontsel : access Gtk_Font_Selection_Record) return Gint
   is
      function Internal (Fontsel : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_font_selection_get_size");
   begin
      return Internal (Get_Object (Fontsel));
   end Get_Size;

   --------------------
   -- Get_Size_Entry --
   --------------------

   function Get_Size_Entry
      (Fontsel : access Gtk_Font_Selection_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Fontsel : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_font_selection_get_size_entry");
      Stub : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Fontsel)), Stub));
   end Get_Size_Entry;

   -------------------
   -- Get_Size_List --
   -------------------

   function Get_Size_List
      (Fontsel : access Gtk_Font_Selection_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Fontsel : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_font_selection_get_size_list");
      Stub : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Fontsel)), Stub));
   end Get_Size_List;

   -------------------
   -- Set_Font_Name --
   -------------------

   function Set_Font_Name
      (Fontsel  : access Gtk_Font_Selection_Record;
       Fontname : UTF8_String) return Boolean
   is
      function Internal
         (Fontsel  : System.Address;
          Fontname : Interfaces.C.Strings.chars_ptr) return Integer;
      pragma Import (C, Internal, "gtk_font_selection_set_font_name");
      Tmp_Fontname : Interfaces.C.Strings.chars_ptr := New_String (Fontname);
      Tmp_Return   : Integer;
   begin
      Tmp_Return := Internal (Get_Object (Fontsel), Tmp_Fontname);
      Free (Tmp_Fontname);
      return Boolean'Val (Tmp_Return);
   end Set_Font_Name;

   ----------------------
   -- Set_Preview_Text --
   ----------------------

   procedure Set_Preview_Text
      (Fontsel : access Gtk_Font_Selection_Record;
       Text    : UTF8_String)
   is
      procedure Internal
         (Fontsel : System.Address;
          Text    : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_font_selection_set_preview_text");
      Tmp_Text : Interfaces.C.Strings.chars_ptr := New_String (Text);
   begin
      Internal (Get_Object (Fontsel), Tmp_Text);
      Free (Tmp_Text);
   end Set_Preview_Text;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : access Gtk_Font_Selection_Record)
       return Gtk.Enums.Gtk_Orientation
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
      (Self        : access Gtk_Font_Selection_Record;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal (Self : System.Address; Orientation : Integer);
      pragma Import (C, Internal, "gtk_orientable_set_orientation");
   begin
      Internal (Get_Object (Self), Gtk.Enums.Gtk_Orientation'Pos (Orientation));
   end Set_Orientation;

end Gtk.Font_Selection;
