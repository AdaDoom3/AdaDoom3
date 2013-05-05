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
with System.Address_To_Access_Conversions;
with Interfaces.C.Strings;

with Glib.Type_Conversion_Hooks;
pragma Elaborate_All (Glib.Type_Conversion_Hooks);

with Pango.Layout; use Pango.Layout;
with Gtkada.Types;

package body Gtk.GEntry is

   -----------------------
   -- Local Subprograms --
   -----------------------

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Entry_Record);
   pragma Warnings (Off, Type_Conversion);
   --  This package is used to implement a minimal automated type conversion
   --  without having to drag the whole Gtk.Type_Conversion package for the
   --  most common widgets.

   package Border_Address_Access_Conversions is
     new System.Address_To_Access_Conversions (Gtk.Style.Gtk_Border_Record);
   --  This package is used to safely convert between System.Address and
   --  access to Gtk_Border_Record.

   -----------------
   -- Append_Text --
   -----------------

   procedure Append_Text
     (The_Entry : access Gtk_Entry_Record; Text : UTF8_String)
   is
      procedure Internal (The_Entry : System.Address; Text : UTF8_String);
      pragma Import (C, Internal, "gtk_entry_append_text");

   begin
      Internal (Get_Object (The_Entry), Text & ASCII.NUL);
   end Append_Text;

   ---------------------------
   -- Get_Activates_Default --
   ---------------------------

   function Get_Activates_Default
     (The_Entry : access Gtk_Entry_Record) return Boolean
   is
      function Internal (The_Entry : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_entry_get_activates_default");

   begin
      return Internal (Get_Object (The_Entry)) /= 0;
   end Get_Activates_Default;

   -------------------
   -- Get_Alignment --
   -------------------

   function Get_Alignment
     (Ent : access Gtk_Entry_Record)
      return Gfloat
   is
      function Internal (Ent : System.Address) return Gfloat;
      pragma Import (C, Internal, "gtk_entry_get_alignment");
   begin
      return Internal (Get_Object (Ent));
   end Get_Alignment;

   --------------------
   -- Get_Completion --
   --------------------

   function Get_Completion
     (Ent : access Gtk_Entry_Record) return Gtk_Entry_Completion
   is
      function Internal
        (Ent : System.Address)
         return System.Address;
      pragma Import (C, Internal, "gtk_entry_get_completion");
      Stub : Gtk_Entry_Completion_Record;
   begin
      return Gtk_Entry_Completion
        (Get_User_Data (Internal (Get_Object (Ent)), Stub));
   end Get_Completion;

   ----------------------------------
   -- Get_Current_Icon_Drag_Source --
   ----------------------------------

   function Get_Current_Icon_Drag_Source (The_Entry : access Gtk_Entry_Record)
      return Gint
   is
      function Internal (The_Entry : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_entry_get_current_icon_drag_source");
   begin
      return Internal (Get_Object (The_Entry));
   end Get_Current_Icon_Drag_Source;

   ----------------------------
   -- Get_Cursor_Hadjustment --
   ----------------------------

   function Get_Cursor_Hadjustment (The_Entry : access Gtk_Entry_Record)
      return Gtk.Adjustment.Gtk_Adjustment
   is
      use Gtk.Adjustment;
      function Internal (The_Entry : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_entry_get_cursor_hadjustment");
      Stub : Gtk_Adjustment_Record;
   begin
      return Gtk_Adjustment
        (Get_User_Data (Internal (Get_Object (The_Entry)), Stub));
   end Get_Cursor_Hadjustment;

   -------------------
   -- Get_Has_Frame --
   -------------------

   function Get_Has_Frame
     (The_Entry : access Gtk_Entry_Record) return Boolean
   is
      function Internal (The_Entry : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_entry_get_has_frame");

   begin
      return Internal (Get_Object (The_Entry)) /= 0;
   end Get_Has_Frame;

   --------------------------
   -- Get_Icon_Activatable --
   --------------------------

   function Get_Icon_Activatable
     (The_Entry : access Gtk_Entry_Record;
      Icon_Pos  : Gtk_Entry_Icon_Position)
      return Boolean
   is
      function Internal
        (The_Entry : System.Address;
         Icon_Pos  : Gtk_Entry_Icon_Position)
         return Gboolean;
      pragma Import (C, Internal, "gtk_entry_get_icon_activatable");
   begin
      return Boolean'Val (Internal (Get_Object (The_Entry), Icon_Pos));
   end Get_Icon_Activatable;

   ---------------------
   -- Get_Icon_At_Pos --
   ---------------------

   function Get_Icon_At_Pos
     (The_Entry : access Gtk_Entry_Record;
      X     : Gint;
      Y     : Gint)
      return Gint
   is
      function Internal (The_Entry : System.Address; X, Y : Gint) return Gint;
      pragma Import (C, Internal, "gtk_entry_get_icon_at_pos");
   begin
      return Internal (Get_Object (The_Entry), X, Y);
   end Get_Icon_At_Pos;

   --------------------
   -- Get_Icon_Gicon --
   --------------------

   function Get_Icon_Gicon
     (The_Entry : access Gtk_Entry_Record;
      Icon_Pos  : Gtk_Entry_Icon_Position)
      return Glib.G_Icon.G_Icon
   is
      function Internal
        (The_Entry : System.Address;
         Icon_Pos  : Gtk_Entry_Icon_Position)
         return Glib.G_Icon.G_Icon;
      pragma Import (C, Internal, "gtk_entry_get_icon_gicon");
   begin
      return Internal (Get_Object (The_Entry), Icon_Pos);
   end Get_Icon_Gicon;

   -------------------
   -- Get_Icon_Name --
   -------------------

   function Get_Icon_Name
     (The_Entry : access Gtk_Entry_Record;
      Icon_Pos  : Gtk_Entry_Icon_Position)
      return UTF8_String
   is
      use Interfaces.C.Strings;

      function Internal
        (The_Entry : System.Address;
         Icon_Pos  : Gtk_Entry_Icon_Position)
         return chars_ptr;
      pragma Import (C, Internal, "gtk_entry_get_icon_name");
      Tmp : chars_ptr;
   begin
      Tmp := Internal (Get_Object (The_Entry), Icon_Pos);
      if Tmp = Null_Ptr then
         return "";
      else
         return Value (Tmp);
      end if;
   end Get_Icon_Name;

   ---------------------
   -- Get_Icon_Pixbuf --
   ---------------------

   function Get_Icon_Pixbuf
     (The_Entry : access Gtk_Entry_Record;
      Icon_Pos  : Gtk_Entry_Icon_Position)
      return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal
        (The_Entry : System.Address;
         Icon_Pos  : Gtk_Entry_Icon_Position)
         return System.Address;
      pragma Import (C, Internal, "gtk_entry_get_icon_pixbuf");
      Stub : Gdk.Pixbuf.Gdk_Pixbuf_Record;
   begin
      return Gdk.Pixbuf.Gdk_Pixbuf
        (Get_User_Data (Internal (Get_Object (The_Entry), Icon_Pos), Stub));
   end Get_Icon_Pixbuf;

   ------------------------
   -- Get_Icon_Sensitive --
   ------------------------

   function Get_Icon_Sensitive
     (The_Entry : access Gtk_Entry_Record;
      Icon_Pos  : Gtk_Entry_Icon_Position)
      return Boolean
   is
      function Internal
        (The_Entry : System.Address;
         Icon_Pos  : Gtk_Entry_Icon_Position)
         return Gboolean;
      pragma Import (C, Internal, "gtk_entry_get_icon_sensitive");
   begin
      return Boolean'Val (Internal (Get_Object (The_Entry), Icon_Pos));
   end Get_Icon_Sensitive;

   --------------------
   -- Get_Icon_Stock --
   --------------------

   function Get_Icon_Stock
     (The_Entry : access Gtk_Entry_Record;
      Icon_Pos  : Gtk_Entry_Icon_Position)
      return UTF8_String
   is
      use Interfaces.C.Strings;

      function Internal
        (The_Entry : System.Address;
         Icon_Pos  : Gtk_Entry_Icon_Position)
         return chars_ptr;
      pragma Import (C, Internal, "gtk_entry_get_icon_stock");
      Tmp : chars_ptr;
   begin
      Tmp := Internal (Get_Object (The_Entry), Icon_Pos);
      if Tmp = Null_Ptr then
         return "";
      else
         return Value (Tmp);
      end if;
   end Get_Icon_Stock;

   ---------------------------
   -- Get_Icon_Storage_Type --
   ---------------------------

   function Get_Icon_Storage_Type
     (The_Entry : access Gtk_Entry_Record;
      Icon_Pos  : Gtk_Entry_Icon_Position)
      return Gtk.Image.Gtk_Image_Type
   is
      function Internal
        (The_Entry : System.Address;
         Icon_Pos  : Gtk_Entry_Icon_Position)
         return Gtk.Image.Gtk_Image_Type;
      pragma Import (C, Internal, "gtk_entry_get_icon_storage_type");
   begin
      return Internal (Get_Object (The_Entry), Icon_Pos);
   end Get_Icon_Storage_Type;

   -----------------------------
   -- Get_Icon_Tooltip_Markup --
   -----------------------------

   function Get_Icon_Tooltip_Markup
     (The_Entry : access Gtk_Entry_Record;
      Icon_Pos  : Gtk_Entry_Icon_Position)
      return UTF8_String
   is
      use Interfaces.C.Strings;

      function Internal
        (The_Entry : System.Address;
         Icon_Pos  : Gtk_Entry_Icon_Position)
         return chars_ptr;
      pragma Import (C, Internal, "gtk_entry_get_icon_tooltip_markup");

      Tmp : chars_ptr;
   begin
      Tmp := Internal (Get_Object (The_Entry), Icon_Pos);
      if Tmp = Null_Ptr then
         return "";
      else
         --  We need to free the string returned from the C function.
         declare
            S : constant String := Value (Tmp);
         begin
            Gtkada.Types.g_free (Tmp);
            return S;
         end;
      end if;
   end Get_Icon_Tooltip_Markup;

   ---------------------------
   -- Get_Icon_Tooltip_Text --
   ---------------------------

   function Get_Icon_Tooltip_Text
     (The_Entry : access Gtk_Entry_Record;
      Icon_Pos  : Gtk_Entry_Icon_Position)
      return UTF8_String
   is
      use Interfaces.C.Strings;

      function Internal
        (The_Entry : System.Address;
         Icon_Pos  : Gtk_Entry_Icon_Position)
         return chars_ptr;
      pragma Import (C, Internal, "gtk_entry_get_icon_tooltip_text");

      Tmp : chars_ptr;
   begin
      Tmp := Internal (Get_Object (The_Entry), Icon_Pos);
      if Tmp = Null_Ptr then
         return "";
      else
         --  We need to free the string returned from the C function.
         declare
            S : constant String := Value (Tmp);
         begin
            Gtkada.Types.g_free (Tmp);
            return S;
         end;
      end if;
   end Get_Icon_Tooltip_Text;

   ----------------------
   -- Get_Inner_Border --
   ----------------------

   function Get_Inner_Border (The_Entry : access Gtk_Entry_Record)
      return Gtk.Style.Gtk_Border
   is
      use Border_Address_Access_Conversions;
      use Gtk.Style;

      function Internal (The_Entry : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_entry_get_inner_border");
   begin
      return Gtk_Border (To_Pointer (Internal (Get_Object (The_Entry))));
   end Get_Inner_Border;

   ------------------------
   -- Get_Invisible_Char --
   ------------------------

   function Get_Invisible_Char
     (The_Entry : access Gtk_Entry_Record) return Gunichar
   is
      function Internal (The_Entry : System.Address) return Gunichar;
      pragma Import (C, Internal, "gtk_entry_get_invisible_char");

   begin
      return Internal (Get_Object (The_Entry));
   end Get_Invisible_Char;

   ----------------
   -- Get_Layout --
   ----------------

   function Get_Layout (The_Entry : access Gtk_Entry_Record)
      return Pango.Layout.Pango_Layout
   is
      function Internal (The_Entry : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_entry_get_layout");
      Stub : Pango_Layout_Record;
   begin
      return Pango_Layout
        (Get_User_Data (Internal (Get_Object (The_Entry)), Stub));
   end Get_Layout;

   ------------------------
   -- Get_Layout_Offsets --
   ------------------------

   procedure Get_Layout_Offsets
     (The_Entry : access Gtk_Entry_Record;
      X         : out Gint;
      Y         : out Gint)
   is
      procedure Internal
        (The_Entry : System.Address;
         X         : out Gint;
         Y         : out Gint);
      pragma Import (C, Internal, "gtk_entry_get_layout_offsets");

   begin
      Internal (Get_Object (The_Entry), X, Y);
   end Get_Layout_Offsets;

   --------------------
   -- Get_Max_Length --
   --------------------

   function Get_Max_Length (The_Entry : access Gtk_Entry_Record) return Gint is
      function Internal (The_Entry : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_entry_get_max_length");

   begin
      return Internal (Get_Object (The_Entry));
   end Get_Max_Length;

   ------------------------
   -- Get_Overwrite_Mode --
   ------------------------

   function Get_Overwrite_Mode (The_Entry : access Gtk_Entry_Record)
      return Boolean
   is
      function Internal (The_Entry : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_entry_get_overwrite_mode");
   begin
      return Boolean'Val (Internal (Get_Object (The_Entry)));
   end Get_Overwrite_Mode;

   ---------------------------
   -- Get_Progress_Fraction --
   ---------------------------

   function Get_Progress_Fraction (The_Entry : access Gtk_Entry_Record)
      return Gdouble
   is
      function Internal (The_Entry : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_entry_get_progress_fraction");
   begin
      return Internal (Get_Object (The_Entry));
   end Get_Progress_Fraction;

   -----------------------------
   -- Get_Progress_Pulse_Step --
   -----------------------------

   function Get_Progress_Pulse_Step (The_Entry : access Gtk_Entry_Record)
      return Gdouble
   is
      function Internal (The_Entry : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_entry_get_progress_pulse_step");
   begin
      return Internal (Get_Object (The_Entry));
   end Get_Progress_Pulse_Step;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
     (The_Entry : access Gtk_Entry_Record) return UTF8_String
   is
      function Internal
        (The_Entry : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_entry_get_text");

   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (The_Entry)));
   end Get_Text;

   ---------------------
   -- Get_Text_Length --
   ---------------------

   function Get_Text_Length (The_Entry : access Gtk_Entry_Record)
      return Guint16
   is
      function Internal (The_Entry : System.Address) return Guint16;
      pragma Import (C, Internal, "gtk_entry_get_text_length");
   begin
      return Internal (Get_Object (The_Entry));
   end Get_Text_Length;

   --------------------
   -- Get_Visibility --
   --------------------

   function Get_Visibility
     (The_Entry : access Gtk_Entry_Record) return Boolean
   is
      function Internal (The_Entry : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_entry_get_visibility");

   begin
      return Internal (Get_Object (The_Entry)) /= 0;
   end Get_Visibility;

   ---------------------
   -- Get_Width_Chars --
   ---------------------

   function Get_Width_Chars
     (The_Entry : access Gtk_Entry_Record'Class) return Gint
   is
      function Internal (The_Entry : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_entry_get_width_chars");

   begin
      return Internal (Get_Object (The_Entry));
   end Get_Width_Chars;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Entry; Max : Gint) is
   begin
      Widget := new Gtk_Entry_Record;
      pragma Warnings (Off);  --  Initialize is now obsolescent
      Initialize (Widget, Max);
      pragma Warnings (On);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Entry) is
   begin
      Widget := new Gtk_Entry_Record;
      Gtk.GEntry.Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget : access Gtk_Entry_Record'Class;
      Max    : Gint)
   is
      function Internal (Max : Gint) return System.Address;
      pragma Import (C, Internal, "gtk_entry_new_with_max_length");

   begin
      Set_Object (Widget, Internal (Max));
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Entry_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_entry_new");

   begin
      Set_Object (Widget, Internal);
   end Initialize;

   --------------------------------
   -- Layout_Index_To_Text_Index --
   --------------------------------

   function Layout_Index_To_Text_Index
     (Ent          : access Gtk_Entry_Record;
      Layout_Index : Gint)
      return Gint
   is
      function Internal
        (Ent          : System.Address;
         Layout_Index : Gint)
         return Gint;
      pragma Import (C, Internal, "gtk_entry_layout_index_to_text_index");
   begin
      return Internal (Get_Object (Ent), Layout_Index);
   end Layout_Index_To_Text_Index;

   ------------------
   -- Prepend_Text --
   ------------------

   procedure Prepend_Text
     (The_Entry : access Gtk_Entry_Record; Text : UTF8_String)
   is
      procedure Internal (The_Entry : System.Address; Text : UTF8_String);
      pragma Import (C, Internal, "gtk_entry_prepend_text");

   begin
      Internal (Get_Object (The_Entry), Text & ASCII.NUL);
   end Prepend_Text;

   --------------------
   -- Progress_Pulse --
   --------------------

   procedure Progress_Pulse (The_Entry : access Gtk_Entry_Record) is
      procedure Internal (The_Entry : System.Address);
      pragma Import (C, Internal, "gtk_entry_progress_pulse");
   begin
      Internal (Get_Object (The_Entry));
   end Progress_Pulse;

   ---------------------------
   -- Set_Activates_Default --
   ---------------------------

   procedure Set_Activates_Default
     (The_Entry : access Gtk_Entry_Record; Setting : Boolean)
   is
      procedure Internal (The_Entry : System.Address; Setting : Gboolean);
      pragma Import (C, Internal, "gtk_entry_set_activates_default");

   begin
      Internal (Get_Object (The_Entry), Boolean'Pos (Setting));
   end Set_Activates_Default;

   -------------------
   -- Set_Alignment --
   -------------------

   procedure Set_Alignment (Ent  : access Gtk_Entry_Record; Xalign : Gfloat) is
      procedure Internal (Ent  : System.Address; Xalign : Gfloat);
      pragma Import (C, Internal, "gtk_entry_set_alignment");
   begin
      Internal (Get_Object (Ent), Xalign);
   end Set_Alignment;

   --------------------
   -- Set_Completion --
   --------------------

   procedure Set_Completion
     (Ent        : access Gtk_Entry_Record;
      Completion : access Gtk_Entry_Completion_Record'Class)
   is
      procedure Internal
        (Ent        : System.Address;
         Completion : System.Address);
      pragma Import (C, Internal, "gtk_entry_set_completion");
   begin
      Internal (Get_Object (Ent), Get_Object (Completion));
   end Set_Completion;

   ----------------------------
   -- Set_Cursor_Hadjustment --
   ----------------------------

   procedure Set_Cursor_Hadjustment
     (The_Entry  : access Gtk_Entry_Record;
      Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Internal
        (The_Entry  : System.Address;
         Adjustment : System.Address);
      pragma Import (C, Internal, "gtk_entry_set_cursor_hadjustment");
   begin
      Internal (Get_Object (The_Entry), Get_Object (Adjustment));
   end Set_Cursor_Hadjustment;

   ------------------
   -- Set_Editable --
   ------------------

   procedure Set_Editable
     (The_Entry : access Gtk_Entry_Record; Editable : Boolean)
   is
      procedure Internal (The_Entry : System.Address; Editable : Gboolean);
      pragma Import (C, Internal, "gtk_entry_set_editable");

   begin
      Internal (Get_Object (The_Entry), Boolean'Pos (Editable));
   end Set_Editable;

   --------------------------
   -- Set_Icon_Drag_Source --
   --------------------------

   procedure Set_Icon_Drag_Source
     (The_Entry   : access Gtk_Entry_Record;
      Icon_Pos    : Gtk_Entry_Icon_Position;
      Target_List : Gtk.Selection.Target_List;
      Actions     : Gdk.Dnd.Drag_Action)
   is
      procedure Internal
        (The_Entry   : System.Address;
         Icon_Pos    : Gtk_Entry_Icon_Position;
         Target_List : Gtk.Selection.Target_List;
         Actions     : Gdk.Dnd.Drag_Action);
      pragma Import (C, Internal, "gtk_entry_set_icon_drag_source");
   begin
      Internal (Get_Object (The_Entry), Icon_Pos, Target_List, Actions);
   end Set_Icon_Drag_Source;

   -----------------------------
   -- Set_Icon_From_Icon_Name --
   -----------------------------

   procedure Set_Icon_From_Icon_Name
     (The_Entry : access Gtk_Entry_Record;
      Icon_Pos  : Gtk_Entry_Icon_Position;
      Icon_Name : UTF8_String)
   is
      use Interfaces.C.Strings;

      procedure Internal
        (The_Entry : System.Address;
         Icon_Pos  : Gtk_Entry_Icon_Position;
         Icon_Name : System.Address);
      pragma Import (C, Internal, "gtk_entry_set_icon_from_icon_name");
      Tmp : constant String := Icon_Name & ASCII.NUL;
   begin
      if Icon_Name = "" then
         Internal (Get_Object (The_Entry), Icon_Pos, System.Null_Address);
      else
         Internal (Get_Object (The_Entry), Icon_Pos, Tmp'Address);
      end if;
   end Set_Icon_From_Icon_Name;

   --------------------------
   -- Set_Icon_From_Pixbuf --
   --------------------------

   procedure Set_Icon_From_Pixbuf
     (The_Entry : access Gtk_Entry_Record;
      Icon_Pos  : Gtk_Entry_Icon_Position;
      Pixbuf    : Gdk.Pixbuf.Gdk_Pixbuf)
   is
      procedure Internal
        (The_Entry : System.Address;
         Icon_Pos  : Gtk_Entry_Icon_Position;
         Pixbuf    : System.Address);
      pragma Import (C, Internal, "gtk_entry_set_icon_from_pixbuf");
   begin
      Internal (Get_Object (The_Entry), Icon_Pos, Get_Object (Pixbuf));
   end Set_Icon_From_Pixbuf;

   -------------------------
   -- Set_Icon_From_Stock --
   -------------------------

   procedure Set_Icon_From_Stock
     (The_Entry : access Gtk_Entry_Record;
      Icon_Pos  : Gtk_Entry_Icon_Position;
      Stock_Id  : UTF8_String)
   is
      use Interfaces.C.Strings;

      procedure Internal
        (The_Entry : System.Address;
         Icon_Pos  : Gtk_Entry_Icon_Position;
         Stock_Id  : System.Address);
      pragma Import (C, Internal, "gtk_entry_set_icon_from_stock");

      Tmp : constant String := Stock_Id & ASCII.NUL;
   begin
      if Stock_Id = "" then
         Internal (Get_Object (The_Entry), Icon_Pos, System.Null_Address);
      else
         Internal (Get_Object (The_Entry), Icon_Pos, Tmp'Address);
      end if;
   end Set_Icon_From_Stock;

   -------------------
   -- Set_Has_Frame --
   -------------------

   procedure Set_Has_Frame
     (The_Entry : access Gtk_Entry_Record; Setting : Boolean := True)
   is
      procedure Internal (The_Entry : System.Address; Setting : Gboolean);
      pragma Import (C, Internal, "gtk_entry_set_has_frame");

   begin
      Internal (Get_Object (The_Entry), Boolean'Pos (Setting));
   end Set_Has_Frame;

   --------------------------
   -- Set_Icon_Activatable --
   --------------------------

   procedure Set_Icon_Activatable
     (The_Entry   : access Gtk_Entry_Record;
      Icon_Pos    : Gtk_Entry_Icon_Position;
      Activatable : Boolean)
   is
      procedure Internal
        (The_Entry   : System.Address;
         Icon_Pos    : Gtk_Entry_Icon_Position;
         Activatable : Gboolean);
      pragma Import (C, Internal, "gtk_entry_set_icon_activatable");
   begin
      Internal (Get_Object (The_Entry), Icon_Pos, Boolean'Pos (Activatable));
   end Set_Icon_Activatable;

   -------------------------
   -- Set_Icon_From_Gicon --
   -------------------------

   procedure Set_Icon_From_Gicon
     (The_Entry : access Gtk_Entry_Record;
      Icon_Pos  : Gtk_Entry_Icon_Position;
      Icon      : Glib.G_Icon.G_Icon)
   is
      procedure Internal
        (The_Entry : System.Address;
         Icon_Pos  : Gtk_Entry_Icon_Position;
         Icon      : Glib.G_Icon.G_Icon);
      pragma Import (C, Internal, "gtk_entry_set_icon_from_gicon");
   begin
      Internal (Get_Object (The_Entry), Icon_Pos, Icon);
   end Set_Icon_From_Gicon;

   ------------------------
   -- Set_Icon_Sensitive --
   ------------------------

   procedure Set_Icon_Sensitive
     (The_Entry : access Gtk_Entry_Record;
      Icon_Pos  : Gtk_Entry_Icon_Position;
      Sensitive : Boolean)
   is
      procedure Internal
        (The_Entry : System.Address;
         Icon_Pos  : Gtk_Entry_Icon_Position;
         Sensitive : Gboolean);
      pragma Import (C, Internal, "gtk_entry_set_icon_sensitive");
   begin
      Internal (Get_Object (The_Entry), Icon_Pos, Boolean'Pos (Sensitive));
   end Set_Icon_Sensitive;

   -----------------------------
   -- Set_Icon_Tooltip_Markup --
   -----------------------------

   procedure Set_Icon_Tooltip_Markup
     (The_Entry : access Gtk_Entry_Record;
      Icon_Pos  : Gtk_Entry_Icon_Position;
      Tooltip   : UTF8_String)
   is
      use Interfaces.C.Strings;

      procedure Internal
        (The_Entry : System.Address;
         Icon_Pos  : Gtk_Entry_Icon_Position;
         Tooltip   : System.Address);
      pragma Import (C, Internal, "gtk_entry_set_icon_tooltip_markup");

      Tmp : constant String := Tooltip & ASCII.NUL;
   begin
      if Tooltip = "" then
         Internal (Get_Object (The_Entry), Icon_Pos, System.Null_Address);
      else
         Internal (Get_Object (The_Entry), Icon_Pos, Tmp'Address);
      end if;
   end Set_Icon_Tooltip_Markup;

   ---------------------------
   -- Set_Icon_Tooltip_Text --
   ---------------------------

   procedure Set_Icon_Tooltip_Text
     (The_Entry : access Gtk_Entry_Record;
      Icon_Pos  : Gtk_Entry_Icon_Position;
      Tooltip   : UTF8_String)
   is
      use Interfaces.C.Strings;

      procedure Internal
        (The_Entry : System.Address;
         Icon_Pos  : Gtk_Entry_Icon_Position;
         Tooltip   : System.Address);
      pragma Import (C, Internal, "gtk_entry_set_icon_tooltip_text");

      Tmp : constant String := Tooltip & ASCII.NUL;
   begin
      if Tooltip = "" then
         Internal (Get_Object (The_Entry), Icon_Pos, System.Null_Address);
      else
         Internal (Get_Object (The_Entry), Icon_Pos, Tmp'Address);
      end if;
   end Set_Icon_Tooltip_Text;

   ----------------------
   -- Set_Inner_Border --
   ----------------------

   procedure Set_Inner_Border
     (The_Entry : access Gtk_Entry_Record;
      Border    : Gtk.Style.Gtk_Border)
   is
      procedure Internal
        (The_Entry : System.Address;
         Border    : System.Address);
      pragma Import (C, Internal, "gtk_entry_set_inner_border");
   begin
      Internal (Get_Object (The_Entry), Border.all'Address);
   end Set_Inner_Border;

   ------------------------
   -- Set_Invisible_Char --
   ------------------------

   procedure Set_Invisible_Char
     (The_Entry : access Gtk_Entry_Record; Char : Gunichar)
   is
      procedure Internal (The_Entry : System.Address; Char : Gunichar);
      pragma Import (C, Internal, "gtk_entry_set_invisible_char");

   begin
      Internal (Get_Object (The_Entry), Char);
   end Set_Invisible_Char;

   --------------------
   -- Set_Max_Length --
   --------------------

   procedure Set_Max_Length
     (The_Entry : access Gtk_Entry_Record;
      Max       : Gint)
   is
      procedure Internal (The_Entry : System.Address; Max : Gint);
      pragma Import (C, Internal, "gtk_entry_set_max_length");

   begin
      Internal (Get_Object (The_Entry), Max);
   end Set_Max_Length;

   ------------------------
   -- Set_Overwrite_Mode --
   ------------------------

   procedure Set_Overwrite_Mode
     (The_Entry : access Gtk_Entry_Record;
      Overwrite : Boolean)
   is
      procedure Internal
        (The_Entry : System.Address;
         Overwrite : Gboolean);
      pragma Import (C, Internal, "gtk_entry_set_overwrite_mode");
   begin
      Internal (Get_Object (The_Entry), Boolean'Pos (Overwrite));
   end Set_Overwrite_Mode;

   ---------------------------
   -- Set_Progress_Fraction --
   ---------------------------

   procedure Set_Progress_Fraction
     (The_Entry : access Gtk_Entry_Record;
      Fraction  : Gdouble)
   is
      procedure Internal
        (The_Entry : System.Address;
         Fraction  : Gdouble);
      pragma Import (C, Internal, "gtk_entry_set_progress_fraction");
   begin
      Internal (Get_Object (The_Entry), Fraction);
   end Set_Progress_Fraction;

   -----------------------------
   -- Set_Progress_Pulse_Step --
   -----------------------------

   procedure Set_Progress_Pulse_Step
     (The_Entry : access Gtk_Entry_Record;
      Fraction  : Gdouble)
   is
      procedure Internal
        (The_Entry : System.Address;
         Fraction  : Gdouble);
      pragma Import (C, Internal, "gtk_entry_set_progress_pulse_step");
   begin
      Internal (Get_Object (The_Entry), Fraction);
   end Set_Progress_Pulse_Step;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
     (The_Entry : access Gtk_Entry_Record; Text : UTF8_String)
   is
      procedure Internal (The_Entry : System.Address; Text : UTF8_String);
      pragma Import (C, Internal, "gtk_entry_set_text");

   begin
      Internal (Get_Object (The_Entry), Text & ASCII.NUL);
   end Set_Text;

   --------------------
   -- Set_Visibility --
   --------------------

   procedure Set_Visibility
     (The_Entry : access Gtk_Entry_Record; Visible : Boolean)
   is
      procedure Internal (The_Entry : System.Address; Visible : Gint);
      pragma Import (C, Internal, "gtk_entry_set_visibility");

   begin
      Internal (Get_Object (The_Entry), Boolean'Pos (Visible));
   end Set_Visibility;

   ---------------------
   -- Set_Width_Chars --
   ---------------------

   procedure Set_Width_Chars
     (The_Entry : access Gtk_Entry_Record'Class; Width : Gint)
   is
      procedure Internal (The_Entry : System.Address; Width : Gint);
      pragma Import (C, Internal, "gtk_entry_set_width_chars");
   begin
      Internal (Get_Object (The_Entry), Width);
   end Set_Width_Chars;

   --------------------------------
   -- Text_Index_To_Layout_Index --
   --------------------------------

   function Text_Index_To_Layout_Index
     (Ent        : access Gtk_Entry_Record;
      Text_Index : Gint)
      return Gint
   is
      function Internal
        (Ent        : System.Address;
         Text_Index : Gint)
         return Gint;
      pragma Import (C, Internal, "gtk_entry_text_index_to_layout_index");
   begin
      return Internal (Get_Object (Ent), Text_Index);
   end Text_Index_To_Layout_Index;

   --------------------------
   -- Unset_Invisible_Char --
   --------------------------

   procedure Unset_Invisible_Char (The_Entry : access Gtk_Entry_Record) is
      procedure Internal (The_Entry : System.Address);
      pragma Import (C, Internal, "gtk_entry_unset_invisible_char");
   begin
      Internal (Get_Object (The_Entry));
   end Unset_Invisible_Char;

end Gtk.GEntry;
