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

package body Gtk.Label is
   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Label_Record);
   pragma Unreferenced (Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Label : out Gtk_Label; Str : UTF8_String := "") is
   begin
      Label := new Gtk_Label_Record;
      Gtk.Label.Initialize (Label, Str);
   end Gtk_New;

   ---------------------------
   -- Gtk_New_With_Mnemonic --
   ---------------------------

   procedure Gtk_New_With_Mnemonic
      (Label : out Gtk_Label;
       Str   : UTF8_String)
   is
   begin
      Label := new Gtk_Label_Record;
      Gtk.Label.Initialize_With_Mnemonic (Label, Str);
   end Gtk_New_With_Mnemonic;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Label : access Gtk_Label_Record'Class;
       Str   : UTF8_String := "")
   is
      function Internal
         (Str : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_label_new");
      Tmp_Str    : Interfaces.C.Strings.chars_ptr := New_String (Str);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Str);
      Free (Tmp_Str);
      Set_Object (Label, Tmp_Return);
   end Initialize;

   ------------------------------
   -- Initialize_With_Mnemonic --
   ------------------------------

   procedure Initialize_With_Mnemonic
      (Label : access Gtk_Label_Record'Class;
       Str   : UTF8_String)
   is
      function Internal
         (Str : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_label_new_with_mnemonic");
      Tmp_Str    : Interfaces.C.Strings.chars_ptr := New_String (Str);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Str);
      Free (Tmp_Str);
      Set_Object (Label, Tmp_Return);
   end Initialize_With_Mnemonic;

   ---------------
   -- Get_Angle --
   ---------------

   function Get_Angle (Label : access Gtk_Label_Record) return Gdouble is
      function Internal (Label : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_label_get_angle");
   begin
      return Internal (Get_Object (Label));
   end Get_Angle;

   --------------------
   -- Get_Attributes --
   --------------------

   function Get_Attributes
      (Label : access Gtk_Label_Record)
       return Pango.Attributes.Pango_Attr_List
   is
      function Internal
         (Label : System.Address) return Pango.Attributes.Pango_Attr_List;
      pragma Import (C, Internal, "gtk_label_get_attributes");
   begin
      return Internal (Get_Object (Label));
   end Get_Attributes;

   ---------------------
   -- Get_Current_Uri --
   ---------------------

   function Get_Current_Uri
      (Label : access Gtk_Label_Record) return UTF8_String
   is
      function Internal
         (Label : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_label_get_current_uri");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Label)));
   end Get_Current_Uri;

   -------------------
   -- Get_Ellipsize --
   -------------------

   function Get_Ellipsize
      (Label : access Gtk_Label_Record)
       return Pango.Layout.Pango_Ellipsize_Mode
   is
      function Internal (Label : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_label_get_ellipsize");
   begin
      return Pango.Layout.Pango_Ellipsize_Mode'Val (Internal (Get_Object (Label)));
   end Get_Ellipsize;

   -----------------
   -- Get_Justify --
   -----------------

   function Get_Justify
      (Label : access Gtk_Label_Record) return Gtk.Enums.Gtk_Justification
   is
      function Internal (Label : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_label_get_justify");
   begin
      return Gtk.Enums.Gtk_Justification'Val (Internal (Get_Object (Label)));
   end Get_Justify;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label (Label : access Gtk_Label_Record) return UTF8_String is
      function Internal
         (Label : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_label_get_label");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Label)));
   end Get_Label;

   ----------------
   -- Get_Layout --
   ----------------

   function Get_Layout
      (Label : access Gtk_Label_Record) return Pango.Layout.Pango_Layout
   is
      function Internal (Label : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_label_get_layout");
      Stub : Pango.Layout.Pango_Layout_Record;
   begin
      return Pango.Layout.Pango_Layout (Get_User_Data (Internal (Get_Object (Label)), Stub));
   end Get_Layout;

   ------------------------
   -- Get_Layout_Offsets --
   ------------------------

   procedure Get_Layout_Offsets
      (Label : access Gtk_Label_Record;
       X     : out Gint;
       Y     : out Gint)
   is
      procedure Internal
         (Label : System.Address;
          X     : out Gint;
          Y     : out Gint);
      pragma Import (C, Internal, "gtk_label_get_layout_offsets");
   begin
      Internal (Get_Object (Label), X, Y);
   end Get_Layout_Offsets;

   -------------------
   -- Get_Line_Wrap --
   -------------------

   function Get_Line_Wrap (Label : access Gtk_Label_Record) return Boolean is
      function Internal (Label : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_label_get_line_wrap");
   begin
      return Boolean'Val (Internal (Get_Object (Label)));
   end Get_Line_Wrap;

   ------------------------
   -- Get_Line_Wrap_Mode --
   ------------------------

   function Get_Line_Wrap_Mode
      (Label : access Gtk_Label_Record) return Pango.Layout.Pango_Wrap_Mode
   is
      function Internal (Label : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_label_get_line_wrap_mode");
   begin
      return Pango.Layout.Pango_Wrap_Mode'Val (Internal (Get_Object (Label)));
   end Get_Line_Wrap_Mode;

   -------------------------
   -- Get_Max_Width_Chars --
   -------------------------

   function Get_Max_Width_Chars
      (Label : access Gtk_Label_Record) return Gint
   is
      function Internal (Label : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_label_get_max_width_chars");
   begin
      return Internal (Get_Object (Label));
   end Get_Max_Width_Chars;

   -------------------------
   -- Get_Mnemonic_Keyval --
   -------------------------

   function Get_Mnemonic_Keyval
      (Label : access Gtk_Label_Record) return Guint
   is
      function Internal (Label : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_label_get_mnemonic_keyval");
   begin
      return Internal (Get_Object (Label));
   end Get_Mnemonic_Keyval;

   -------------------------
   -- Get_Mnemonic_Widget --
   -------------------------

   function Get_Mnemonic_Widget
      (Label : access Gtk_Label_Record) return Gtk.Widget.Gtk_Widget
   is
      function Internal (Label : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_label_get_mnemonic_widget");
      Stub : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Label)), Stub));
   end Get_Mnemonic_Widget;

   --------------------
   -- Get_Selectable --
   --------------------

   function Get_Selectable (Label : access Gtk_Label_Record) return Boolean is
      function Internal (Label : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_label_get_selectable");
   begin
      return Boolean'Val (Internal (Get_Object (Label)));
   end Get_Selectable;

   --------------------------
   -- Get_Selection_Bounds --
   --------------------------

   procedure Get_Selection_Bounds
      (Label         : access Gtk_Label_Record;
       Start         : out Gint;
       The_End       : out Gint;
       Has_Selection : out Boolean)
   is
      function Internal
         (Label       : System.Address;
          Acc_Start   : access Gint;
          Acc_The_End : access Gint) return Integer;
      pragma Import (C, Internal, "gtk_label_get_selection_bounds");
      Acc_Start   : aliased Gint;
      Acc_The_End : aliased Gint;
      Tmp_Return  : Integer;
   begin
      Tmp_Return := Internal (Get_Object (Label), Acc_Start'Access, Acc_The_End'Access);
      Start := Acc_Start;
      The_End := Acc_The_End;
      Has_Selection := Boolean'Val (Tmp_Return);
   end Get_Selection_Bounds;

   --------------------------
   -- Get_Single_Line_Mode --
   --------------------------

   function Get_Single_Line_Mode
      (Label : access Gtk_Label_Record) return Boolean
   is
      function Internal (Label : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_label_get_single_line_mode");
   begin
      return Boolean'Val (Internal (Get_Object (Label)));
   end Get_Single_Line_Mode;

   --------------
   -- Get_Text --
   --------------

   function Get_Text (Label : access Gtk_Label_Record) return UTF8_String is
      function Internal
         (Label : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_label_get_text");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Label)));
   end Get_Text;

   -----------------------------
   -- Get_Track_Visited_Links --
   -----------------------------

   function Get_Track_Visited_Links
      (Label : access Gtk_Label_Record) return Boolean
   is
      function Internal (Label : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_label_get_track_visited_links");
   begin
      return Boolean'Val (Internal (Get_Object (Label)));
   end Get_Track_Visited_Links;

   --------------------
   -- Get_Use_Markup --
   --------------------

   function Get_Use_Markup (Label : access Gtk_Label_Record) return Boolean is
      function Internal (Label : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_label_get_use_markup");
   begin
      return Boolean'Val (Internal (Get_Object (Label)));
   end Get_Use_Markup;

   -----------------------
   -- Get_Use_Underline --
   -----------------------

   function Get_Use_Underline
      (Label : access Gtk_Label_Record) return Boolean
   is
      function Internal (Label : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_label_get_use_underline");
   begin
      return Boolean'Val (Internal (Get_Object (Label)));
   end Get_Use_Underline;

   ---------------------
   -- Get_Width_Chars --
   ---------------------

   function Get_Width_Chars (Label : access Gtk_Label_Record) return Gint is
      function Internal (Label : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_label_get_width_chars");
   begin
      return Internal (Get_Object (Label));
   end Get_Width_Chars;

   -----------------
   -- Parse_Uline --
   -----------------

   function Parse_Uline
      (Label  : access Gtk_Label_Record;
       String : UTF8_String) return Guint
   is
      function Internal
         (Label  : System.Address;
          String : Interfaces.C.Strings.chars_ptr) return Guint;
      pragma Import (C, Internal, "gtk_label_parse_uline");
      Tmp_String : Interfaces.C.Strings.chars_ptr := New_String (String);
      Tmp_Return : Guint;
   begin
      Tmp_Return := Internal (Get_Object (Label), Tmp_String);
      Free (Tmp_String);
      return Tmp_Return;
   end Parse_Uline;

   -------------------
   -- Select_Region --
   -------------------

   procedure Select_Region
      (Label        : access Gtk_Label_Record;
       Start_Offset : Gint := -1;
       End_Offset   : Gint := -1)
   is
      procedure Internal
         (Label        : System.Address;
          Start_Offset : Gint;
          End_Offset   : Gint);
      pragma Import (C, Internal, "gtk_label_select_region");
   begin
      Internal (Get_Object (Label), Start_Offset, End_Offset);
   end Select_Region;

   ---------------
   -- Set_Angle --
   ---------------

   procedure Set_Angle (Label : access Gtk_Label_Record; Angle : Gdouble) is
      procedure Internal (Label : System.Address; Angle : Gdouble);
      pragma Import (C, Internal, "gtk_label_set_angle");
   begin
      Internal (Get_Object (Label), Angle);
   end Set_Angle;

   --------------------
   -- Set_Attributes --
   --------------------

   procedure Set_Attributes
      (Label : access Gtk_Label_Record;
       Attrs : out Pango.Attributes.Pango_Attr_List)
   is
      procedure Internal
         (Label : System.Address;
          Attrs : out Pango.Attributes.Pango_Attr_List);
      pragma Import (C, Internal, "gtk_label_set_attributes");
   begin
      Internal (Get_Object (Label), Attrs);
   end Set_Attributes;

   -------------------
   -- Set_Ellipsize --
   -------------------

   procedure Set_Ellipsize
      (Label : access Gtk_Label_Record;
       Mode  : Pango.Layout.Pango_Ellipsize_Mode)
   is
      procedure Internal (Label : System.Address; Mode : Integer);
      pragma Import (C, Internal, "gtk_label_set_ellipsize");
   begin
      Internal (Get_Object (Label), Pango.Layout.Pango_Ellipsize_Mode'Pos (Mode));
   end Set_Ellipsize;

   -----------------
   -- Set_Justify --
   -----------------

   procedure Set_Justify
      (Label : access Gtk_Label_Record;
       Jtype : Gtk.Enums.Gtk_Justification)
   is
      procedure Internal (Label : System.Address; Jtype : Integer);
      pragma Import (C, Internal, "gtk_label_set_justify");
   begin
      Internal (Get_Object (Label), Gtk.Enums.Gtk_Justification'Pos (Jtype));
   end Set_Justify;

   ---------------
   -- Set_Label --
   ---------------

   procedure Set_Label (Label : access Gtk_Label_Record; Str : UTF8_String) is
      procedure Internal
         (Label : System.Address;
          Str   : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_label_set_label");
      Tmp_Str : Interfaces.C.Strings.chars_ptr := New_String (Str);
   begin
      Internal (Get_Object (Label), Tmp_Str);
      Free (Tmp_Str);
   end Set_Label;

   -------------------
   -- Set_Line_Wrap --
   -------------------

   procedure Set_Line_Wrap (Label : access Gtk_Label_Record; Wrap : Boolean) is
      procedure Internal (Label : System.Address; Wrap : Integer);
      pragma Import (C, Internal, "gtk_label_set_line_wrap");
   begin
      Internal (Get_Object (Label), Boolean'Pos (Wrap));
   end Set_Line_Wrap;

   ------------------------
   -- Set_Line_Wrap_Mode --
   ------------------------

   procedure Set_Line_Wrap_Mode
      (Label     : access Gtk_Label_Record;
       Wrap_Mode : Pango.Layout.Pango_Wrap_Mode)
   is
      procedure Internal (Label : System.Address; Wrap_Mode : Integer);
      pragma Import (C, Internal, "gtk_label_set_line_wrap_mode");
   begin
      Internal (Get_Object (Label), Pango.Layout.Pango_Wrap_Mode'Pos (Wrap_Mode));
   end Set_Line_Wrap_Mode;

   ----------------
   -- Set_Markup --
   ----------------

   procedure Set_Markup (Label : access Gtk_Label_Record; Str : UTF8_String) is
      procedure Internal
         (Label : System.Address;
          Str   : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_label_set_markup");
      Tmp_Str : Interfaces.C.Strings.chars_ptr := New_String (Str);
   begin
      Internal (Get_Object (Label), Tmp_Str);
      Free (Tmp_Str);
   end Set_Markup;

   ------------------------------
   -- Set_Markup_With_Mnemonic --
   ------------------------------

   procedure Set_Markup_With_Mnemonic
      (Label : access Gtk_Label_Record;
       Str   : UTF8_String)
   is
      procedure Internal
         (Label : System.Address;
          Str   : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_label_set_markup_with_mnemonic");
      Tmp_Str : Interfaces.C.Strings.chars_ptr := New_String (Str);
   begin
      Internal (Get_Object (Label), Tmp_Str);
      Free (Tmp_Str);
   end Set_Markup_With_Mnemonic;

   -------------------------
   -- Set_Max_Width_Chars --
   -------------------------

   procedure Set_Max_Width_Chars
      (Label   : access Gtk_Label_Record;
       N_Chars : Gint)
   is
      procedure Internal (Label : System.Address; N_Chars : Gint);
      pragma Import (C, Internal, "gtk_label_set_max_width_chars");
   begin
      Internal (Get_Object (Label), N_Chars);
   end Set_Max_Width_Chars;

   -------------------------
   -- Set_Mnemonic_Widget --
   -------------------------

   procedure Set_Mnemonic_Widget
      (Label  : access Gtk_Label_Record;
       Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Label : System.Address; Widget : System.Address);
      pragma Import (C, Internal, "gtk_label_set_mnemonic_widget");
   begin
      Internal (Get_Object (Label), Get_Object (Widget));
   end Set_Mnemonic_Widget;

   -----------------
   -- Set_Pattern --
   -----------------

   procedure Set_Pattern
      (Label   : access Gtk_Label_Record;
       Pattern : UTF8_String)
   is
      procedure Internal
         (Label   : System.Address;
          Pattern : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_label_set_pattern");
      Tmp_Pattern : Interfaces.C.Strings.chars_ptr := New_String (Pattern);
   begin
      Internal (Get_Object (Label), Tmp_Pattern);
      Free (Tmp_Pattern);
   end Set_Pattern;

   --------------------
   -- Set_Selectable --
   --------------------

   procedure Set_Selectable
      (Label   : access Gtk_Label_Record;
       Setting : Boolean)
   is
      procedure Internal (Label : System.Address; Setting : Integer);
      pragma Import (C, Internal, "gtk_label_set_selectable");
   begin
      Internal (Get_Object (Label), Boolean'Pos (Setting));
   end Set_Selectable;

   --------------------------
   -- Set_Single_Line_Mode --
   --------------------------

   procedure Set_Single_Line_Mode
      (Label            : access Gtk_Label_Record;
       Single_Line_Mode : Boolean)
   is
      procedure Internal
         (Label            : System.Address;
          Single_Line_Mode : Integer);
      pragma Import (C, Internal, "gtk_label_set_single_line_mode");
   begin
      Internal (Get_Object (Label), Boolean'Pos (Single_Line_Mode));
   end Set_Single_Line_Mode;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text (Label : access Gtk_Label_Record; Str : UTF8_String) is
      procedure Internal
         (Label : System.Address;
          Str   : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_label_set_text");
      Tmp_Str : Interfaces.C.Strings.chars_ptr := New_String (Str);
   begin
      Internal (Get_Object (Label), Tmp_Str);
      Free (Tmp_Str);
   end Set_Text;

   ----------------------------
   -- Set_Text_With_Mnemonic --
   ----------------------------

   procedure Set_Text_With_Mnemonic
      (Label : access Gtk_Label_Record;
       Str   : UTF8_String)
   is
      procedure Internal
         (Label : System.Address;
          Str   : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_label_set_text_with_mnemonic");
      Tmp_Str : Interfaces.C.Strings.chars_ptr := New_String (Str);
   begin
      Internal (Get_Object (Label), Tmp_Str);
      Free (Tmp_Str);
   end Set_Text_With_Mnemonic;

   -----------------------------
   -- Set_Track_Visited_Links --
   -----------------------------

   procedure Set_Track_Visited_Links
      (Label       : access Gtk_Label_Record;
       Track_Links : Boolean)
   is
      procedure Internal (Label : System.Address; Track_Links : Integer);
      pragma Import (C, Internal, "gtk_label_set_track_visited_links");
   begin
      Internal (Get_Object (Label), Boolean'Pos (Track_Links));
   end Set_Track_Visited_Links;

   --------------------
   -- Set_Use_Markup --
   --------------------

   procedure Set_Use_Markup
      (Label   : access Gtk_Label_Record;
       Setting : Boolean)
   is
      procedure Internal (Label : System.Address; Setting : Integer);
      pragma Import (C, Internal, "gtk_label_set_use_markup");
   begin
      Internal (Get_Object (Label), Boolean'Pos (Setting));
   end Set_Use_Markup;

   -----------------------
   -- Set_Use_Underline --
   -----------------------

   procedure Set_Use_Underline
      (Label   : access Gtk_Label_Record;
       Setting : Boolean)
   is
      procedure Internal (Label : System.Address; Setting : Integer);
      pragma Import (C, Internal, "gtk_label_set_use_underline");
   begin
      Internal (Get_Object (Label), Boolean'Pos (Setting));
   end Set_Use_Underline;

   ---------------------
   -- Set_Width_Chars --
   ---------------------

   procedure Set_Width_Chars
      (Label   : access Gtk_Label_Record;
       N_Chars : Gint)
   is
      procedure Internal (Label : System.Address; N_Chars : Gint);
      pragma Import (C, Internal, "gtk_label_set_width_chars");
   begin
      Internal (Get_Object (Label), N_Chars);
   end Set_Width_Chars;

end Gtk.Label;
