-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2001-2013, AdaCore                   --
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

with System.Address_To_Access_Conversions;
with Gtk.Text_Tag;        use Gtk.Text_Tag;
with Gtk.Text_Attributes; use Gtk.Text_Attributes;

package body Gtk.Text_Iter is

   --  Note (1): In the subprograms imported from C, the most usual declaration
   --  for a GtkTextIter parameter would be "in System.Address". However, in
   --  order to avoid unnecessary uses of the 'Address attribute, they are
   --  directly declared as "in Gtk_Text_Iter". The "Import (C," pragma then
   --  ensures that the structure is passed by reference, as expected at the
   --  gtk+ level.
   --
   --  Also note that this method also applies to cases where the iterator is
   --  an "in out" parameter at the GtkAda level. Even if declared as an "in"
   --  parameter at the imported C function level, the fact that the
   --  GtkTextIter parameter is passed by reference (see above) ensures that
   --  the "in out" semantics is respected, despite "in" mode in the profile of
   --  the imported function.
   --
   --  Note (2): On the other hand, (1) is not appropriate if the iterator
   --  at the GtkAda level is an "out" parameter: the compiler would generate
   --  an unitialized parameter warning. In these rare cases, the
   --  address of the parameter is passed by using the 'Address attribute.
   --  The portability of this construct is ensured by the fact that the
   --  Gtk_Text_Iter type is a limited record.
   --
   --  If for some reason Gtk_Text_Iter can not remain limited, taking the
   --  'Address of the parameter should not be done anymore. In that case,
   --  the proper method is to use the 'Address of a local Gtk_Text_Iter.

   package Iter_Access_Address_Conversions is
     new System.Address_To_Access_Conversions (Gtk_Text_Iter);

   procedure g_free (Mem : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, g_free, "g_free");
   --  External binding: g_free

   ----------------
   -- Can_Insert --
   ----------------

   function Can_Insert
     (Iter                : Gtk_Text_Iter;
      Default_Editability : Boolean) return Boolean
   is
      function Internal
        (Iter : Gtk_Text_Iter; Default : Gboolean) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_can_insert");

   begin
      return Internal (Iter, Boolean'Pos (Default_Editability)) /= 0;
   end Can_Insert;

   ----------
   -- Copy --
   ----------

   procedure Copy (Source : Gtk_Text_Iter; Dest : out Gtk_Text_Iter) is
      procedure Internal (Source : Gtk_Text_Iter; Dest : System.Address);
      pragma Import (C, Internal, "ada_text_iter_copy");

   begin
      Internal (Source, Dest'Address);
   end Copy;

   --------------
   -- Get_Char --
   --------------

   function Get_Char (Iter : Gtk_Text_Iter) return Gunichar is
      function Internal (Iter : Gtk_Text_Iter) return Gunichar;
      pragma Import (C, Internal, "gtk_text_iter_get_char");
      --  Note that the Get_Char function could have been directly imported
      --  from C, rather than going through this Internal function. This
      --  solution was prefered for cosmetic reasons: Having several different
      --  Get_Char functions, the "Import C" pragma would need to be located
      --  close to the function declaration, which we would like to avoid.

   begin
      return Internal (Iter);
   end Get_Char;

   --------------
   -- Get_Char --
   --------------

   function Get_Char (Iter : Gtk_Text_Iter) return Character is
      Result         : constant Gunichar := Get_Char (Iter);
      Eight_LSB_Mask : constant := 2#1111_1111#;

   begin
      --  This function relies on the Get_Char function provided by gtk+,
      --  which returns a gunichar value. Only the 8 least significant bits
      --  are then kept to deduce the associated character.

      return Character'Val (Result and Eight_LSB_Mask);
   end Get_Char;

   ----------------
   -- Get_Pixbuf --
   ----------------

   function Get_Pixbuf (Iter : Gtk_Text_Iter) return Gdk.Pixbuf.Gdk_Pixbuf is
      function Internal (Iter : Gtk_Text_Iter) return System.Address;
      pragma Import (C, Internal, "gtk_text_iter_get_pixbuf");

   begin
      return Gdk.Pixbuf.Convert (Internal (Iter));
   end Get_Pixbuf;

   ---------------
   -- Get_Slice --
   ---------------

   function Get_Slice
     (Start   : Gtk_Text_Iter;
      The_End : Gtk_Text_Iter) return Interfaces.C.Strings.chars_ptr
   is
      function Internal
        (Start   : Gtk_Text_Iter;
         The_End : Gtk_Text_Iter) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_text_iter_get_slice");

   begin
      return Internal (Start, The_End);
   end Get_Slice;

   function Get_Slice
     (Start   : Gtk_Text_Iter;
      The_End : Gtk_Text_Iter) return UTF8_String
   is
      Str : constant Interfaces.C.Strings.chars_ptr :=
        Get_Slice (Start, The_End);
      S   : constant String := Interfaces.C.Strings.Value (Str);

   begin
      g_free (Str);
      return S;
   end Get_Slice;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
     (Start   : Gtk_Text_Iter;
      The_End : Gtk_Text_Iter) return UTF8_String
   is
      function Internal
        (Start   : Gtk_Text_Iter;
         The_End : Gtk_Text_Iter)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_text_iter_get_text");

      Str : constant Interfaces.C.Strings.chars_ptr :=
        Internal (Start, The_End);

   begin
      declare
         S : constant String := Interfaces.C.Strings.Value (Str);
      begin
         g_free (Str);
         return S;
      end;
   end Get_Text;

   -----------------------
   -- Get_Visible_Slice --
   -----------------------

   function Get_Visible_Slice
     (Start   : Gtk_Text_Iter;
      The_End : Gtk_Text_Iter) return UTF8_String
   is
      function Internal
        (Start   : Gtk_Text_Iter;
         The_End : Gtk_Text_Iter) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_text_iter_get_visible_slice");

      Str : constant Interfaces.C.Strings.chars_ptr :=
        Internal (Start, The_End);

   begin
      declare
         S : constant String := Interfaces.C.Strings.Value (Str);
      begin
         g_free (Str);
         return S;
      end;
   end Get_Visible_Slice;

   ----------------------
   -- Get_Visible_Text --
   ----------------------

   function Get_Visible_Text
     (Start   : Gtk_Text_Iter;
      The_End : Gtk_Text_Iter) return UTF8_String
   is
      function Internal
        (Start   : Gtk_Text_Iter;
         The_End : Gtk_Text_Iter) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_text_iter_get_visible_text");

      Str : constant Interfaces.C.Strings.chars_ptr :=
        Internal (Start, The_End);

   begin
      declare
         S : constant String := Interfaces.C.Strings.Value (Str);
      begin
         g_free (Str);
         return S;
      end;
   end Get_Visible_Text;

   ----------------------
   -- Get_Child_Anchor --
   ----------------------

   function Get_Child_Anchor
      (Iter : Gtk_Text_Iter)
       return Gtk.Text_Child.Gtk_Text_Child_Anchor
   is
      function Internal (Iter : Gtk_Text_Iter) return System.Address;
      pragma Import (C, Internal, "gtk_text_iter_get_child_anchor");

      Stub : Gtk.Text_Child.Gtk_Text_Child_Anchor_Record;

   begin
      return Gtk.Text_Child.Gtk_Text_Child_Anchor
               (Get_User_Data_Fast (Internal (Iter), Stub));
   end Get_Child_Anchor;

   ----------------
   -- Begins_Tag --
   ----------------

   function Begins_Tag
     (Iter : Gtk_Text_Iter;
      Tag  : Gtk.Text_Tag.Gtk_Text_Tag := null) return Boolean
   is
      function Internal
        (Iter : Gtk_Text_Iter; Tag : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_begins_tag");

   begin
      if Tag = null then
         return Internal (Iter, System.Null_Address) /= 0;
      else
         return Internal (Iter, Get_Object (Tag)) /= 0;
      end if;
   end Begins_Tag;

   --------------
   -- Ends_Tag --
   --------------

   function Ends_Tag
     (Iter   : Gtk_Text_Iter;
      Tag    : Gtk.Text_Tag.Gtk_Text_Tag := null) return Boolean
   is
      function Internal
        (Iter : Gtk_Text_Iter; Tag : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_ends_tag");

   begin
      if Tag = null then
         return Internal (Iter, System.Null_Address) /= 0;
      else
         return Internal (Iter, Get_Object (Tag)) /= 0;
      end if;
   end Ends_Tag;

   -----------------
   -- Toggles_Tag --
   -----------------

   function Toggles_Tag
     (Iter : Gtk_Text_Iter;
      Tag  : Gtk.Text_Tag.Gtk_Text_Tag := null) return Boolean
   is
      function Internal
        (Iter : Gtk_Text_Iter;
         Tag  : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_toggles_tag");

   begin
      if Tag = null then
         return Internal (Iter, System.Null_Address) /= 0;
      else
         return Internal (Iter, Get_Object (Tag)) /= 0;
      end if;
   end Toggles_Tag;

   -------------
   -- Has_Tag --
   -------------

   function Has_Tag
     (Iter : Gtk_Text_Iter;
      Tag  : Gtk.Text_Tag.Gtk_Text_Tag := null) return Boolean
   is
      function Internal
        (Iter : Gtk_Text_Iter; Tag : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_has_tag");

   begin
      if Tag = null then
         return Internal (Iter, System.Null_Address) /= 0;
      else
         return Internal (Iter, Get_Object (Tag)) /= 0;
      end if;
   end Has_Tag;

   --------------
   -- Get_Tags --
   --------------

   function Get_Tags
     (Iter : Gtk_Text_Iter) return Gtk.Text_Tag.Text_Tag_List.GSlist
   is
      function Internal (Iter : Gtk_Text_Iter) return System.Address;
      pragma Import (C, Internal, "gtk_text_iter_get_tags");
      List : Gtk.Text_Tag.Text_Tag_List.GSlist;
   begin
      Gtk.Text_Tag.Text_Tag_List.Set_Object (List, Internal (Iter));
      return List;
   end Get_Tags;

   --------------
   -- Editable --
   --------------

   function Editable
     (Iter            : Gtk_Text_Iter;
      Default_Setting : Boolean := True) return Boolean
   is
      function Internal
        (Iter            : Gtk_Text_Iter;
         Default_Setting : Gboolean) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_editable");

   begin
      return Internal (Iter, Boolean'Pos (Default_Setting)) /= 0;
   end Editable;

   -----------------
   -- Starts_Word --
   -----------------

   function Starts_Word (Iter : Gtk_Text_Iter) return Boolean is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_starts_word");

   begin
      return Internal (Iter) /= 0;
   end Starts_Word;

   ---------------
   -- Ends_Word --
   ---------------

   function Ends_Word (Iter : Gtk_Text_Iter) return Boolean is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_ends_word");

   begin
      return Internal (Iter) /= 0;
   end Ends_Word;

   -----------------
   -- Inside_Word --
   -----------------

   function Inside_Word (Iter : Gtk_Text_Iter) return Boolean is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_inside_word");

   begin
      return Internal (Iter) /= 0;
   end Inside_Word;

   ---------------------
   -- Starts_Sentence --
   ---------------------

   function Starts_Sentence (Iter : Gtk_Text_Iter) return Boolean is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_starts_sentence");

   begin
      return Internal (Iter) /= 0;
   end Starts_Sentence;

   -------------------
   -- Ends_Sentence --
   -------------------

   function Ends_Sentence (Iter : Gtk_Text_Iter) return Boolean is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_ends_sentence");

   begin
      return Internal (Iter) /= 0;
   end Ends_Sentence;

   ---------------------
   -- Inside_Sentence --
   ---------------------

   function Inside_Sentence (Iter : Gtk_Text_Iter) return Boolean is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_inside_sentence");

   begin
      return Internal (Iter) /= 0;
   end Inside_Sentence;

   -----------------
   -- Starts_Line --
   -----------------

   function Starts_Line (Iter : Gtk_Text_Iter) return Boolean is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_starts_line");

   begin
      return Internal (Iter) /= 0;
   end Starts_Line;

   ---------------
   -- Ends_Line --
   ---------------

   function Ends_Line (Iter : Gtk_Text_Iter) return Boolean is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_ends_line");

   begin
      return Internal (Iter) /= 0;
   end Ends_Line;

   ------------------------
   -- Is_Cursor_Position --
   ------------------------

   function Is_Cursor_Position (Iter : Gtk_Text_Iter) return Boolean is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_is_cursor_position");

   begin
      return Internal (Iter) /= 0;
   end Is_Cursor_Position;

   ------------------
   -- Get_Language --
   ------------------

   function Get_Language (Iter : Gtk_Text_Iter) return UTF8_String is
      function Internal
        (Iter : Gtk_Text_Iter) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_text_iter_get_language");

      Str : constant Interfaces.C.Strings.chars_ptr := Internal (Iter);

   begin
      declare
         S : constant String := Interfaces.C.Strings.Value (Str);
      begin
         g_free (Str);
         return S;
      end;
   end Get_Language;

   ------------
   -- Is_End --
   ------------

   function Is_End (Iter : Gtk_Text_Iter) return Boolean is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_is_end");

   begin
      return Internal (Iter) /= 0;
   end Is_End;

   --------------
   -- Is_Start --
   --------------

   function Is_Start (Iter : Gtk_Text_Iter) return Boolean is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_is_start");

   begin
      return Internal (Iter) /= 0;
   end Is_Start;

   ------------------
   -- Forward_Char --
   ------------------

   procedure Forward_Char
     (Iter   : in out Gtk_Text_Iter;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_char");

   begin
      Result := Internal (Iter) /= 0;
   end Forward_Char;

   -------------------
   -- Backward_Char --
   -------------------

   procedure Backward_Char
     (Iter   : in out Gtk_Text_Iter;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_char");

   begin
      Result := Internal (Iter) /= 0;
   end Backward_Char;

   -------------------
   -- Forward_Chars --
   -------------------

   procedure Forward_Chars
     (Iter   : in out Gtk_Text_Iter;
      Count  : Gint := 1;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter; Count : Gint) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_chars");

   begin
      Result := Internal (Iter, Count) /= 0;
   end Forward_Chars;

   --------------------
   -- Backward_Chars --
   --------------------

   procedure Backward_Chars
     (Iter   : in out Gtk_Text_Iter;
      Count  : Gint := 1;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter; Count : Gint) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_chars");

   begin
      Result := Internal (Iter, Count) /= 0;
   end Backward_Chars;

   ------------------
   -- Forward_Line --
   ------------------

   procedure Forward_Line
     (Iter   : in out Gtk_Text_Iter;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_line");

   begin
      Result := Internal (Iter) /= 0;
   end Forward_Line;

   -------------------
   -- Backward_Line --
   -------------------

   procedure Backward_Line
     (Iter   : in out Gtk_Text_Iter;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_line");

   begin
      Result := Internal (Iter) /= 0;
   end Backward_Line;

   -------------------
   -- Forward_Lines --
   -------------------

   procedure Forward_Lines
     (Iter   : in out Gtk_Text_Iter;
      Count  : Gint := 1;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter; Count : Gint) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_lines");

   begin
      Result := Internal (Iter, Count) /= 0;
   end Forward_Lines;

   --------------------
   -- Backward_Lines --
   --------------------

   procedure Backward_Lines
     (Iter   : in out Gtk_Text_Iter;
      Count  : Gint := 1;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter; Count : Gint) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_lines");

   begin
      Result := Internal (Iter, Count) /= 0;
   end Backward_Lines;

   ----------------------
   -- Forward_Word_End --
   ----------------------

   procedure Forward_Word_End
     (Iter   : in out Gtk_Text_Iter;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_word_end");

   begin
      Result := Internal (Iter) /= 0;
   end Forward_Word_End;

   -------------------------
   -- Backward_Word_Start --
   -------------------------

   procedure Backward_Word_Start
     (Iter   : in out Gtk_Text_Iter;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_word_start");

   begin
      Result := Internal (Iter) /= 0;
   end Backward_Word_Start;

   -----------------------
   -- Forward_Word_Ends --
   -----------------------

   procedure Forward_Word_Ends
     (Iter   : in out Gtk_Text_Iter;
      Count  : Gint := 1;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter; Count : Gint) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_word_ends");

   begin
      Result := Internal (Iter, Count) /= 0;
   end Forward_Word_Ends;

   --------------------------
   -- Backward_Word_Starts --
   --------------------------

   procedure Backward_Word_Starts
     (Iter   : in out Gtk_Text_Iter;
      Count  : Gint := 1;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter; Count : Gint) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_word_starts");

   begin
      Result := Internal (Iter, Count) /= 0;
   end Backward_Word_Starts;

   --------------------------
   -- Forward_Sentence_End --
   --------------------------

   procedure Forward_Sentence_End
     (Iter   : in out Gtk_Text_Iter;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_sentence_end");

   begin
      Result := Internal (Iter) /= 0;
   end Forward_Sentence_End;

   -----------------------------
   -- Backward_Sentence_Start --
   -----------------------------

   procedure Backward_Sentence_Start
     (Iter   : in out Gtk_Text_Iter;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_sentence_start");

   begin
      Result := Internal (Iter) /= 0;
   end Backward_Sentence_Start;

   ---------------------------
   -- Forward_Sentence_Ends --
   ---------------------------

   procedure Forward_Sentence_Ends
     (Iter   : in out Gtk_Text_Iter;
      Count  : Gint := 1;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter; Count : Gint) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_sentence_ends");

   begin
      Result := Internal (Iter, Count) /= 0;
   end Forward_Sentence_Ends;

   ------------------------------
   -- Backward_Sentence_Starts --
   ------------------------------

   procedure Backward_Sentence_Starts
     (Iter   : in out Gtk_Text_Iter;
      Count  : Gint := 1;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter; Count : Gint) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_sentence_starts");

   begin
      Result := Internal (Iter, Count) /= 0;
   end Backward_Sentence_Starts;

   -----------------------------
   -- Forward_Cursor_Position --
   -----------------------------

   procedure Forward_Cursor_Position
     (Iter   : in out Gtk_Text_Iter;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_cursor_position");

   begin
      Result := Internal (Iter) /= 0;
   end Forward_Cursor_Position;

   ------------------------------
   -- Backward_Cursor_Position --
   ------------------------------

   procedure Backward_Cursor_Position
     (Iter   : in out Gtk_Text_Iter;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_cursor_position");

   begin
      Result := Internal (Iter) /= 0;
   end Backward_Cursor_Position;

   ------------------------------
   -- Forward_Cursor_Positions --
   ------------------------------

   procedure Forward_Cursor_Positions
     (Iter   : in out Gtk_Text_Iter;
      Count  : Gint := 1;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter; Count : Gint) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_cursor_positions");

   begin
      Result := Internal (Iter, Count) /= 0;
   end Forward_Cursor_Positions;

   -------------------------------
   -- Backward_Cursor_Positions --
   -------------------------------

   procedure Backward_Cursor_Positions
     (Iter   : in out Gtk_Text_Iter;
      Count  : Gint := 1;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter; Count : Gint) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_cursor_positions");

   begin
      Result := Internal (Iter, Count) /= 0;
   end Backward_Cursor_Positions;

   ----------------
   -- Set_Offset --
   ----------------

   procedure Set_Offset
     (Iter        : in out Gtk_Text_Iter;
      Char_Offset : Gint)
   is
      procedure Internal
        (Iter        : Gtk_Text_Iter;
         Char_Offset : Gint);
      pragma Import (C, Internal, "gtk_text_iter_set_offset");

   begin
      Internal (Iter, Char_Offset);
   end Set_Offset;

   --------------
   -- Set_Line --
   --------------

   procedure Set_Line
     (Iter        : in out Gtk_Text_Iter;
      Line_Number : Gint)
   is
      procedure Internal
        (Iter        : Gtk_Text_Iter;
         Line_Number : Gint);
      pragma Import (C, Internal, "gtk_text_iter_set_line");

   begin
      Internal (Iter, Line_Number);
   end Set_Line;

   ---------------------
   -- Set_Line_Offset --
   ---------------------

   procedure Set_Line_Offset
     (Iter         : in out Gtk_Text_Iter;
      Char_On_Line : Gint)
   is
      procedure Internal
        (Iter         : Gtk_Text_Iter;
         Char_On_Line : Gint);
      pragma Import (C, Internal, "gtk_text_iter_set_line_offset");

   begin
      Internal (Iter, Char_On_Line);
   end Set_Line_Offset;

   --------------------
   -- Set_Line_Index --
   --------------------

   procedure Set_Line_Index
     (Iter         : in out Gtk_Text_Iter;
      Byte_On_Line : Gint)
   is
      procedure Internal
        (Iter         : Gtk_Text_Iter;
         Byte_On_Line : Gint);
      pragma Import (C, Internal, "gtk_text_iter_set_line_index");

   begin
      Internal (Iter, Byte_On_Line);
   end Set_Line_Index;

   -------------------------
   -- Forward_To_Line_End --
   -------------------------

   procedure Forward_To_Line_End
     (Iter   : in out Gtk_Text_Iter;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_to_line_end");

   begin
      Result := Internal (Iter) /= 0;
   end Forward_To_Line_End;

   -----------------------------
   -- Set_Visible_Line_Offset --
   -----------------------------

   procedure Set_Visible_Line_Offset
     (Iter         : in out Gtk_Text_Iter;
      Char_On_Line : Gint)
   is
      procedure Internal
        (Iter         : Gtk_Text_Iter;
         Char_On_Line : Gint);
      pragma Import (C, Internal, "gtk_text_iter_set_visible_line_offset");

   begin
      Internal (Iter, Char_On_Line);
   end Set_Visible_Line_Offset;

   ----------------------------
   -- Set_Visible_Line_Index --
   ----------------------------

   procedure Set_Visible_Line_Index
     (Iter         : in out Gtk_Text_Iter;
      Byte_On_Line : Gint)
   is
      procedure Internal
        (Iter         : Gtk_Text_Iter;
         Byte_On_Line : Gint);
      pragma Import (C, Internal, "gtk_text_iter_set_visible_line_index");

   begin
      Internal (Iter, Byte_On_Line);
   end Set_Visible_Line_Index;

   ---------------------------
   -- Forward_To_Tag_Toggle --
   ---------------------------

   procedure Forward_To_Tag_Toggle
     (Iter   : in out Gtk_Text_Iter;
      Tag    : Gtk.Text_Tag.Gtk_Text_Tag := null;
      Result : out Boolean)
   is
      function Internal
        (Iter : Gtk_Text_Iter; Tag : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_to_tag_toggle");

   begin
      if Tag = null then
         Result := Internal (Iter, System.Null_Address) /= 0;
      else
         Result := Internal (Iter, Get_Object (Tag)) /= 0;
      end if;
   end Forward_To_Tag_Toggle;

   ----------------------------
   -- Backward_To_Tag_Toggle --
   ----------------------------

   procedure Backward_To_Tag_Toggle
     (Iter   : in out Gtk_Text_Iter;
      Tag    : Gtk.Text_Tag.Gtk_Text_Tag := null;
      Result : out Boolean)
   is
      function Internal
        (Iter : Gtk_Text_Iter; Tag : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_to_tag_toggle");

   begin
      if Tag = null then
         Result := Internal (Iter, System.Null_Address) /= 0;
      else
         Result := Internal (Iter, Get_Object (Tag)) /= 0;
      end if;
   end Backward_To_Tag_Toggle;

   --------------------
   -- Forward_Search --
   --------------------

   procedure Forward_Search
     (Iter         : Gtk_Text_Iter;
      Str          : UTF8_String;
      Flags        : Gtk_Text_Search_Flags;
      Match_Start  : out Gtk_Text_Iter;
      Match_End    : out Gtk_Text_Iter;
      Limit        : Gtk_Text_Iter;
      Result       : out Boolean)
   is
      function Internal
        (Iter         : Gtk_Text_Iter;
         Str          : UTF8_String;
         Flags        : Gtk_Text_Search_Flags;
         Match_Start  : System.Address;
         Match_End    : System.Address;
         Limit        : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_search");
   begin
      Result :=
        Internal
          (Iter,
           Str & ASCII.NUL,
           Flags,
           Match_Start'Address,
           Match_End'Address,
           Limit) /= 0;
   end Forward_Search;

   ---------------------
   -- Backward_Search --
   ---------------------

   procedure Backward_Search
     (Iter         : Gtk_Text_Iter;
      Str          : UTF8_String;
      Flags        : Gtk_Text_Search_Flags;
      Match_Start  : out Gtk_Text_Iter;
      Match_End    : out Gtk_Text_Iter;
      Limit        : Gtk_Text_Iter;
      Result       : out Boolean)
   is
      function Internal
        (Iter         : Gtk_Text_Iter;
         Str          : UTF8_String;
         Flags        : Gtk_Text_Search_Flags;
         Match_Start  : System.Address;
         Match_End    : System.Address;
         Limit        : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_search");

   begin
      Result :=
        Internal
          (Iter,
           Str & ASCII.NUL,
           Flags,
           Match_Start'Address,
           Match_End'Address,
           Limit) /= 0;
   end Backward_Search;

   -----------
   -- Equal --
   -----------

   function Equal (Lhs : Gtk_Text_Iter; Rhs : Gtk_Text_Iter) return Boolean is
      function Internal
        (Lhs : Gtk_Text_Iter; Rhs : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_equal");

   begin
      return Internal (Lhs, Rhs) /= 0;
   end Equal;

   --------------
   -- In_Range --
   --------------

   function In_Range
     (Iter    :  Gtk_Text_Iter;
      Start   :  Gtk_Text_Iter;
      The_End :  Gtk_Text_Iter) return Boolean
   is
      function Internal
        (Iter    : Gtk_Text_Iter;
         Start   : Gtk_Text_Iter;
         The_End : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_in_range");

   begin
      return Internal (Iter, Start, The_End) /= 0;
   end In_Range;

   -------------------
   -- Get_Text_Iter --
   -------------------

   procedure Get_Text_Iter
     (Val  : Glib.Values.GValue;
      Iter : out Gtk_Text_Iter) is
   begin
      Copy
        (Source => Iter_Access_Address_Conversions.To_Pointer
                     (Glib.Values.Get_Address (Val)).all,
         Dest   => Iter);
   end Get_Text_Iter;

   ----------------
   -- Find_Chars --
   ----------------

   package body Find_Chars is

      -----------------------
      -- Forward_Find_Char --
      -----------------------

      function Forward_Find_Char
        (Iter      : Gtk_Text_Iter;
         Pred      : Gtk_Text_Char_Predicate;
         User_Data : Data_Type;
         Limit     : Gtk_Text_Iter) return Boolean
      is
         function Proxy (Ch : Gunichar; D : System.Address) return Gboolean;
         pragma Convention (C, Proxy);
         --  Converts the C types to a proper Ada type, and call the user
         --  function

         function Proxy (Ch : Gunichar; D : System.Address) return Gboolean is
            pragma Unreferenced (D);
         begin
            return Boolean'Pos (Pred (Ch, User_Data));
         end Proxy;

         function Internal
           (Iter : Gtk_Text_Iter;
            Pred : System.Address;
            Data : System.Address;
            Limit : Gtk_Text_Iter) return Gboolean;
         pragma Import (C, Internal, "gtk_text_iter_forward_find_char");

      begin
         return Boolean'Val
           (Internal (Iter, Proxy'Address, System.Null_Address, Limit));
      end Forward_Find_Char;

      ------------------------
      -- Backward_Find_Char --
      ------------------------

      function Backward_Find_Char
        (Iter      : Gtk_Text_Iter;
         Pred      : Gtk_Text_Char_Predicate;
         User_Data : Data_Type;
         Limit     : Gtk_Text_Iter) return Boolean
      is
         function Proxy (Ch : Gunichar; D : System.Address) return Gboolean;
         pragma Convention (C, Proxy);
         --  Converts the C types to a proper Ada type, and call the user
         --  function

         function Proxy (Ch : Gunichar; D : System.Address) return Gboolean is
            pragma Unreferenced (D);
         begin
            return Boolean'Pos (Pred (Ch, User_Data));
         end Proxy;

         function Internal
           (Iter : Gtk_Text_Iter;
            Pred : System.Address;
            Data : System.Address;
            Limit : Gtk_Text_Iter) return Gboolean;
         pragma Import (C, Internal, "gtk_text_iter_backward_find_char");

      begin
         return Boolean'Val
           (Internal (Iter, Proxy'Address, System.Null_Address, Limit));
      end Backward_Find_Char;
   end Find_Chars;

   -------------------------------------
   -- Forward_Visible_Cursor_Position --
   -------------------------------------

   procedure Forward_Visible_Cursor_Position
     (Iter : in out Gtk_Text_Iter; Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import
        (C, Internal, "gtk_text_iter_forward_visible_cursor_position");
   begin
      Result := Boolean'Val (Internal (Iter));
   end Forward_Visible_Cursor_Position;

   --------------------------------------
   -- Forward_Visible_Cursor_Positions --
   --------------------------------------

   procedure Forward_Visible_Cursor_Positions
     (Iter   : in out Gtk_Text_Iter;
      Count  : Gint := 1;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter; Count : Gint) return Gboolean;
      pragma Import
        (C, Internal, "gtk_text_iter_forward_visible_cursor_positions");
   begin
      Result := Boolean'Val (Internal (Iter, Count));
   end Forward_Visible_Cursor_Positions;

   --------------------------------------
   -- Backward_Visible_Cursor_Position --
   --------------------------------------

   procedure Backward_Visible_Cursor_Position
     (Iter : in out Gtk_Text_Iter;  Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter)  return Gboolean;
      pragma Import
        (C, Internal, "gtk_text_iter_backward_visible_cursor_position");
   begin
      Result := Boolean'Val (Internal (Iter));
   end Backward_Visible_Cursor_Position;

   ---------------------------------------
   -- Backward_Visible_Cursor_Positions --
   ---------------------------------------

   procedure Backward_Visible_Cursor_Positions
     (Iter   : in out Gtk_Text_Iter;
      Count  : Gint := 1;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter; Count : Gint) return Gboolean;
      pragma Import
        (C, Internal, "gtk_text_iter_backward_visible_cursor_positions");
   begin
      Result := Boolean'Val (Internal (Iter, Count));
   end Backward_Visible_Cursor_Positions;

   --------------------------
   -- Forward_Visible_Line --
   --------------------------

   procedure Forward_Visible_Line
     (Iter : in out Gtk_Text_Iter; Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_visible_line");
   begin
      Result := Boolean'Val (Internal (Iter));
   end Forward_Visible_Line;

   ---------------------------
   -- Forward_Visible_Lines --
   ---------------------------

   procedure Forward_Visible_Lines
     (Iter   : in out Gtk_Text_Iter;
      Count  : Gint := 1;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter; Count : Gint) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_visible_lines");
   begin
      Result := Boolean'Val (Internal (Iter, Count));
   end Forward_Visible_Lines;

   ---------------------------
   -- Backward_Visible_Line --
   ---------------------------

   procedure Backward_Visible_Line
     (Iter   : in out Gtk_Text_Iter; Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_visible_line");
   begin
      Result := Boolean'Val (Internal (Iter));
   end Backward_Visible_Line;

   ----------------------------
   -- Backward_Visible_Lines --
   ----------------------------

   procedure Backward_Visible_Lines
     (Iter   : in out Gtk_Text_Iter;
      Count  : Gint := 1;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter; Count : Gint) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_visible_lines");
   begin
      Result := Boolean'Val (Internal (Iter, Count));
   end Backward_Visible_Lines;

   ------------------------------
   -- Forward_Visible_Word_End --
   ------------------------------

   procedure Forward_Visible_Word_End
     (Iter : in out Gtk_Text_Iter; Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_visible_word_end");
   begin
      Result := Boolean'Val (Internal (Iter));
   end Forward_Visible_Word_End;

   -------------------------------
   -- Forward_Visible_Word_Ends --
   -------------------------------

   procedure Forward_Visible_Word_Ends
     (Iter   : in out Gtk_Text_Iter;
      Count  : Gint := 1;
      Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter; Count : Gint) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_forward_visible_word_ends");
   begin
      Result := Boolean'Val (Internal (Iter, Count));
   end Forward_Visible_Word_Ends;

   ---------------------------------
   -- Backward_Visible_Word_Start --
   ---------------------------------

   procedure Backward_Visible_Word_Start
     (Iter : in out Gtk_Text_Iter; Result : out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_backward_visible_word_start");
   begin
      Result := Boolean'Val (Internal (Iter));
   end Backward_Visible_Word_Start;

   ----------------------------------
   -- Backward_Visible_Word_Starts --
   ----------------------------------

   procedure Backward_Visible_Word_Starts
     (Iter   : Gtk_Text_Iter;
      Count  : Gint := 1;
      Result : in out Boolean)
   is
      function Internal (Iter : Gtk_Text_Iter; Count : Gint) return Gboolean;
      pragma Import
        (C, Internal, "gtk_text_iter_backward_visible_word_starts");
   begin
      Result := Boolean'Val (Internal (Iter, Count));
   end Backward_Visible_Word_Starts;

   --------------------
   -- Get_Attributes --
   --------------------

   procedure Get_Attributes
     (Iter     : Gtk_Text_Iter;
      Values   : in out Gtk.Text_Attributes.Gtk_Text_Attributes;
      Modified : out Boolean)
   is
      pragma Unmodified (Values);
      function Internal
        (Iter   : Gtk_Text_Iter;
         Values : Gtk_Text_Attributes)
         return Gboolean;
      pragma Import (C, Internal, "gtk_text_iter_get_attributes");
   begin
      Modified := Boolean'Val (Internal (Iter, Values));
   end Get_Attributes;

   ----------------------
   -- Get_Toggled_Tags --
   ----------------------

   function Get_Toggled_Tags
     (Iter       : Gtk_Text_Iter;
      Toggled_On : Boolean)
      return Glib.Object.Object_List.GSlist
   is
      use Glib.Object.Object_List;
      function Internal
        (Iter       : Gtk_Text_Iter;
         Toggled_On : Gboolean) return System.Address;
      pragma Import (C, Internal, "gtk_text_iter_get_toggled_tags");
      L : GSlist;
   begin
      Set_Object (L, Internal (Iter, Boolean'Pos (Toggled_On)));
      return L;
   end Get_Toggled_Tags;

   ---------------
   -- Get_Marks --
   ---------------

   function Get_Marks
     (Iter : Gtk_Text_Iter) return Glib.Object.Object_List.GSlist
   is
      use Glib.Object.Object_List;
      function Internal (Iter : Gtk_Text_Iter) return System.Address;
      pragma Import (C, Internal, "gtk_text_iter_get_marks");
      L : GSlist;
   begin
      Set_Object (L, Internal (Iter));
      return L;
   end Get_Marks;

begin
   if Gtk_Text_Iter'Size /= C_Gtk_Text_Iter_Size * 8 then
      raise Program_Error;
   end if;
end Gtk.Text_Iter;
