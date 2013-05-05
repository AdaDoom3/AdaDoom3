-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                 Copyright (C) 2001-2013, AdaCore                  --
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
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;
with Gtk.Clipboard;        use Gtk.Clipboard;
with Gtk.Text_Iter;        use Gtk.Text_Iter;
with Gtk.Text_Tag;         use Gtk.Text_Tag;

with Glib.Type_Conversion_Hooks;

package body Gtk.Text_Buffer is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Text_Buffer_Record);
   pragma Warnings (Off, Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Buffer : out Gtk_Text_Buffer;
      Table  : Gtk.Text_Tag_Table.Gtk_Text_Tag_Table := null) is
   begin
      Buffer := new Gtk_Text_Buffer_Record;
      Initialize (Buffer, Table);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Buffer : access Gtk_Text_Buffer_Record'Class;
      Table  : Gtk.Text_Tag_Table.Gtk_Text_Tag_Table := null)
   is
      function Internal (Table : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_text_buffer_new");

      use type Gtk.Text_Tag_Table.Gtk_Text_Tag_Table;

   begin
      if Table = null then
         Set_Object (Buffer, Internal (System.Null_Address));
      else
         Set_Object (Buffer, Internal (Get_Object (Table)));
      end if;
   end Initialize;

   --------------------
   -- Get_Line_Count --
   --------------------

   function Get_Line_Count
     (Buffer : access Gtk_Text_Buffer_Record) return Gint
   is
      function Internal (Buffer : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_text_buffer_get_line_count");

   begin
      return Internal (Get_Object (Buffer));
   end Get_Line_Count;

   --------------------
   -- Get_Char_Count --
   --------------------

   function Get_Char_Count
     (Buffer : access Gtk_Text_Buffer_Record) return Gint
   is
      function Internal (Buffer : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_text_buffer_get_char_count");

   begin
      return Internal (Get_Object (Buffer));
   end Get_Char_Count;

   -------------------
   -- Get_Tag_Table --
   -------------------

   function Get_Tag_Table
     (Buffer : access Gtk_Text_Buffer_Record)
      return Gtk.Text_Tag_Table.Gtk_Text_Tag_Table
   is
      function Internal (Buffer : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_text_buffer_get_tag_table");
      Stub : Gtk.Text_Tag_Table.Gtk_Text_Tag_Table_Record;

   begin
      return Gtk.Text_Tag_Table.Gtk_Text_Tag_Table
               (Get_User_Data_Fast (Internal (Get_Object (Buffer)), Stub));
   end Get_Tag_Table;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
     (Buffer : access Gtk_Text_Buffer_Record;
      Text   : UTF8_String)
   is
      procedure Internal
        (Buffer : System.Address;
         Text   : UTF8_String;
         Len    : Gint);
      pragma Import (C, Internal, "gtk_text_buffer_set_text");

   begin
      Internal (Get_Object (Buffer), Text, Text'Length);
   end Set_Text;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Buffer : access Gtk_Text_Buffer_Record;
      Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Text   : UTF8_String)
   is
      procedure Internal
        (Buffer : System.Address;
         Iter   : Gtk.Text_Iter.Gtk_Text_Iter;
         Text   : UTF8_String;
         Len    : Gint);
      pragma Import (C, Internal, "gtk_text_buffer_insert");

   begin
      Internal (Get_Object (Buffer), Iter, Text, Text'Length);
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Buffer : access Gtk_Text_Buffer_Record;
      Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Text   : Gtkada.Types.Chars_Ptr)
   is
      procedure Internal
        (Buffer : System.Address;
         Iter   : Gtk.Text_Iter.Gtk_Text_Iter;
         Text   : Gtkada.Types.Chars_Ptr;
         Len    : Gint := -1);
      pragma Import (C, Internal, "gtk_text_buffer_insert");

   begin
      Internal (Get_Object (Buffer), Iter, Text);
   end Insert;

   ----------------------
   -- Insert_With_Tags --
   ----------------------

   procedure Insert_With_Tags
     (Buffer : access Gtk_Text_Buffer_Record;
      Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Text   : UTF8_String;
      Tag    : Gtk_Text_Tag)
   is
      procedure Internal
        (Buffer : System.Address;
         Iter   : Gtk.Text_Iter.Gtk_Text_Iter;
         Text   : UTF8_String;
         Len    : Gint;
         Tag    : System.Address);
      pragma Import (C, Internal, "ada_gtk_text_buffer_insert_with_tags");

   begin
      Internal
        (Get_Object (Buffer), Iter, Text, Text'Length, Get_Object (Tag));
   end Insert_With_Tags;

   ----------------------
   -- Insert_With_Tags --
   ----------------------

   procedure Insert_With_Tags
     (Buffer : access Gtk_Text_Buffer_Record;
      Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Text   : Gtkada.Types.Chars_Ptr;
      Tag    : Gtk.Text_Tag.Gtk_Text_Tag)
   is
      procedure Internal
        (Buffer : System.Address;
         Iter   : Gtk.Text_Iter.Gtk_Text_Iter;
         Text   : Gtkada.Types.Chars_Ptr;
         Len    : Gint := -1;
         Tag    : System.Address);
      pragma Import (C, Internal, "ada_gtk_text_buffer_insert_with_tags");

   begin
      Internal (Get_Object (Buffer), Iter, Text, Tag => Get_Object (Tag));
   end Insert_With_Tags;

   ------------------------------
   -- Insert_With_Tags_By_Name --
   ------------------------------

   procedure Insert_With_Tags_By_Name
     (Buffer : access Gtk_Text_Buffer_Record;
      Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Text   : UTF8_String;
      Tag_Name : String)
   is
      procedure Internal
        (Buffer : System.Address;
         Iter   : Gtk.Text_Iter.Gtk_Text_Iter;
         Text   : UTF8_String;
         Len    : Gint;
         Name   : String);
      pragma Import
        (C, Internal, "ada_gtk_text_buffer_insert_with_tags_by_name");

   begin
      Internal
        (Get_Object (Buffer), Iter, Text, Text'Length, Tag_Name & ASCII.NUL);
   end Insert_With_Tags_By_Name;

   ----------------------
   -- Insert_At_Cursor --
   ----------------------

   procedure Insert_At_Cursor
     (Buffer : access Gtk_Text_Buffer_Record;
      Text   : UTF8_String)
   is
      procedure Internal
        (Buffer : System.Address;
         Text   : UTF8_String;
         Len    : Gint);
      pragma Import (C, Internal, "gtk_text_buffer_insert_at_cursor");

   begin
      Internal (Get_Object (Buffer), Text, Text'Length);
   end Insert_At_Cursor;

   procedure Insert_At_Cursor
     (Buffer : access Gtk_Text_Buffer_Record;
      Text   : Gtkada.Types.Chars_Ptr;
      Len    : Gint := -1)
   is
      procedure Internal
        (Buffer : System.Address;
         Text   : Gtkada.Types.Chars_Ptr;
         Len    : Gint);
      pragma Import (C, Internal, "gtk_text_buffer_insert_at_cursor");

   begin
      Internal (Get_Object (Buffer), Text, Len);
   end Insert_At_Cursor;

   ------------------------
   -- Insert_Interactive --
   ------------------------

   procedure Insert_Interactive
     (Buffer           : access Gtk_Text_Buffer_Record;
      Iter             : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Text             : UTF8_String;
      Default_Editable : Boolean;
      Result           : out Boolean)
   is
      function Internal
        (Buffer           : System.Address;
         Iter             : Gtk.Text_Iter.Gtk_Text_Iter;
         Text             : UTF8_String;
         Len              : Gint;
         Default_Editable : Gboolean) return Gboolean;
      pragma Import (C, Internal, "gtk_text_buffer_insert_interactive");

   begin
      Result :=
          Internal (Get_Object (Buffer), Iter, Text, Text'Length,
           Boolean'Pos (Default_Editable)) /= 0;
   end Insert_Interactive;

   ----------------------------------
   -- Insert_Interactive_At_Cursor --
   ----------------------------------

   function Insert_Interactive_At_Cursor
     (Buffer           : access Gtk_Text_Buffer_Record;
      Text             : UTF8_String;
      Default_Editable : Boolean) return Boolean
   is
      function Internal
        (Buffer           : System.Address;
         Text             : UTF8_String;
         Len              : Gint;
         Default_Editable : Gboolean) return Gboolean;
      pragma Import
        (C, Internal, "gtk_text_buffer_insert_interactive_at_cursor");

   begin
      return
        Internal (Get_Object (Buffer), Text, Text'Length,
         Boolean'Pos (Default_Editable)) /= 0;
   end Insert_Interactive_At_Cursor;

   ------------------
   -- Insert_Range --
   ------------------

   procedure Insert_Range
     (Buffer  : access Gtk_Text_Buffer_Record;
      Iter    : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Start   : Gtk.Text_Iter.Gtk_Text_Iter;
      The_End : Gtk.Text_Iter.Gtk_Text_Iter)
   is
      procedure Internal
        (Buffer  : System.Address;
         Iter    : Gtk.Text_Iter.Gtk_Text_Iter;
         Start   : Gtk.Text_Iter.Gtk_Text_Iter;
         The_End : Gtk.Text_Iter.Gtk_Text_Iter);
      pragma Import (C, Internal, "gtk_text_buffer_insert_range");

   begin
      Internal (Get_Object (Buffer), Iter, Start, The_End);
   end Insert_Range;

   ------------------------------
   -- Insert_Range_Interactive --
   ------------------------------

   procedure Insert_Range_Interactive
     (Buffer           : access Gtk_Text_Buffer_Record;
      Iter             : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Start            : Gtk.Text_Iter.Gtk_Text_Iter;
      The_End          : Gtk.Text_Iter.Gtk_Text_Iter;
      Default_Editable : Boolean;
      Result           : out Boolean)
   is
      function Internal
        (Buffer           : System.Address;
         Iter             : Gtk.Text_Iter.Gtk_Text_Iter;
         Start            : Gtk.Text_Iter.Gtk_Text_Iter;
         The_End          : Gtk.Text_Iter.Gtk_Text_Iter;
         Default_Editable : Gboolean) return Gboolean;
      pragma Import (C, Internal, "gtk_text_buffer_insert_range_interactive");

   begin
      Result :=
        Internal (Get_Object (Buffer),
         Iter,
         Start,
         The_End,
         Boolean'Pos (Default_Editable)) /= 0;
   end Insert_Range_Interactive;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Buffer  : access Gtk_Text_Buffer_Record;
      Start   : in out Gtk.Text_Iter.Gtk_Text_Iter;
      The_End : in out Gtk.Text_Iter.Gtk_Text_Iter)
   is
      procedure Internal
        (Buffer  : System.Address;
         Start   : Gtk.Text_Iter.Gtk_Text_Iter;
         The_End : Gtk.Text_Iter.Gtk_Text_Iter);
      pragma Import (C, Internal, "gtk_text_buffer_delete");

   begin
      Internal (Get_Object (Buffer), Start, The_End);
   end Delete;

   ------------------------
   -- Delete_Interactive --
   ------------------------

   procedure Delete_Interactive
     (Buffer           : access Gtk_Text_Buffer_Record;
      Start_Iter       : in out Gtk.Text_Iter.Gtk_Text_Iter;
      End_Iter         : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Default_Editable : Boolean;
      Result           : out Boolean)
   is
      function Internal
        (Buffer           : System.Address;
         Start_Iter       : Gtk.Text_Iter.Gtk_Text_Iter;
         End_Iter         : Gtk.Text_Iter.Gtk_Text_Iter;
         Default_Editable : Gboolean) return Gboolean;
      pragma Import (C, Internal, "gtk_text_buffer_delete_interactive");

   begin
      Result :=
        Internal (Get_Object (Buffer),
         Start_Iter,
         End_Iter,
         Boolean'Pos (Default_Editable)) /= 0;
   end Delete_Interactive;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
     (Buffer               : access Gtk_Text_Buffer_Record;
      Start                : Gtk.Text_Iter.Gtk_Text_Iter;
      The_End              : Gtk.Text_Iter.Gtk_Text_Iter;
      Include_Hidden_Chars : Boolean := False) return Gtkada.Types.Chars_Ptr
   is
      function Internal
        (Buffer               : System.Address;
         Start                : Gtk.Text_Iter.Gtk_Text_Iter;
         The_End              : Gtk.Text_Iter.Gtk_Text_Iter;
         Include_Hidden_Chars : Gboolean)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_text_buffer_get_text");

   begin
      return Internal
        (Get_Object (Buffer),
         Start,
         The_End,
         Boolean'Pos (Include_Hidden_Chars));
   end Get_Text;

   function Get_Text
     (Buffer               : access Gtk_Text_Buffer_Record;
      Start                : Gtk.Text_Iter.Gtk_Text_Iter;
      The_End              : Gtk.Text_Iter.Gtk_Text_Iter;
      Include_Hidden_Chars : Boolean := False) return UTF8_String
   is
      Str : constant Interfaces.C.Strings.chars_ptr :=
        Get_Text (Buffer, Start, The_End, Include_Hidden_Chars);
      S : constant String := Interfaces.C.Strings.Value (Str);

   begin
      Gtkada.Types.g_free (Str);
      return S;
   end Get_Text;

   ---------------
   -- Get_Slice --
   ---------------

   function Get_Slice
     (Buffer               : access Gtk_Text_Buffer_Record;
      Start                : Gtk.Text_Iter.Gtk_Text_Iter;
      The_End              : Gtk.Text_Iter.Gtk_Text_Iter;
      Include_Hidden_Chars : Boolean := False) return Gtkada.Types.Chars_Ptr
   is
      function Internal
        (Buffer               : System.Address;
         Start                : Gtk.Text_Iter.Gtk_Text_Iter;
         The_End              : Gtk.Text_Iter.Gtk_Text_Iter;
         Include_Hidden_Chars : Gboolean)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_text_buffer_get_slice");

   begin
      return Internal
        (Get_Object (Buffer),
         Start,
         The_End,
         Boolean'Pos (Include_Hidden_Chars));
   end Get_Slice;

   function Get_Slice
     (Buffer               : access Gtk_Text_Buffer_Record;
      Start                : Gtk.Text_Iter.Gtk_Text_Iter;
      The_End              : Gtk.Text_Iter.Gtk_Text_Iter;
      Include_Hidden_Chars : Boolean := False) return UTF8_String
   is
      Str : constant Interfaces.C.Strings.chars_ptr :=
        Get_Slice (Buffer, Start, The_End, Include_Hidden_Chars);
      S   : constant String := Interfaces.C.Strings.Value (Str);

   begin
      Gtkada.Types.g_free (Str);
      return S;
   end Get_Slice;

   -------------------
   -- Insert_Pixbuf --
   -------------------

   procedure Insert_Pixbuf
     (Buffer : access Gtk_Text_Buffer_Record;
      Iter   : Gtk.Text_Iter.Gtk_Text_Iter;
      Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf)
   is
      procedure Internal
        (Buffer : System.Address;
         Iter   : Gtk.Text_Iter.Gtk_Text_Iter;
         Pixbuf : System.Address);
      pragma Import (C, Internal, "gtk_text_buffer_insert_pixbuf");

   begin
      Internal (Get_Object (Buffer), Iter, Get_Object (Pixbuf));
   end Insert_Pixbuf;

   -------------------------
   -- Insert_Child_Anchor --
   -------------------------

   procedure Insert_Child_Anchor
     (Buffer : access Gtk_Text_Buffer_Record;
      Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Anchor : access Gtk.Text_Child.Gtk_Text_Child_Anchor_Record'Class)
   is
      procedure Internal
        (Buffer : System.Address;
         Iter   : Gtk.Text_Iter.Gtk_Text_Iter;
         Anchor : System.Address);
      pragma Import (C, Internal, "gtk_text_buffer_insert_child_anchor");

   begin
      Internal (Get_Object (Buffer), Iter, Get_Object (Anchor));
   end Insert_Child_Anchor;

   -------------------------
   -- Create_Child_Anchor --
   -------------------------

   procedure Create_Child_Anchor
     (Buffer : access Gtk_Text_Buffer_Record;
      Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Result : out Gtk.Text_Child.Gtk_Text_Child_Anchor)
   is
      function Internal
        (Buffer : System.Address;
         Iter   : Gtk.Text_Iter.Gtk_Text_Iter) return System.Address;
      pragma Import (C, Internal, "gtk_text_buffer_create_child_anchor");

      Stub : Gtk.Text_Child.Gtk_Text_Child_Anchor_Record;
   begin
      Result := Gtk.Text_Child.Gtk_Text_Child_Anchor
        (Get_User_Data_Fast (Internal (Get_Object (Buffer), Iter), Stub));
   end Create_Child_Anchor;

   -----------------
   -- Create_Mark --
   -----------------

   function Create_Mark
     (Buffer       : access Gtk_Text_Buffer_Record;
      Mark_Name    : String := "";
      Where        : Gtk.Text_Iter.Gtk_Text_Iter;
      Left_Gravity : Boolean := True) return Gtk.Text_Mark.Gtk_Text_Mark
   is
      function Internal
        (Buffer       : System.Address;
         Mark_Name    : String;
         Where        : Gtk.Text_Iter.Gtk_Text_Iter;
         Left_Gravity : Gboolean) return System.Address;

      function Internal
        (Buffer       : System.Address;
         Mark_Name    : System.Address := System.Null_Address;
         Where        : Gtk.Text_Iter.Gtk_Text_Iter;
         Left_Gravity : Gboolean) return System.Address;
      pragma Import (C, Internal, "gtk_text_buffer_create_mark");

      Stub : Gtk.Text_Mark.Gtk_Text_Mark_Record;

   begin
      if Mark_Name /= "" then
         return
           Gtk.Text_Mark.Gtk_Text_Mark
             (Get_User_Data_Fast
               (Internal
                 (Get_Object (Buffer),
                  Mark_Name & ASCII.NUL,
                  Where,
                  Boolean'Pos (Left_Gravity)),
                  Stub));
      else
         return
           Gtk.Text_Mark.Gtk_Text_Mark
             (Get_User_Data_Fast
               (Internal
                  (Buffer => Get_Object (Buffer),
                   Where => Where,
                   Left_Gravity => Boolean'Pos (Left_Gravity)),
                   Stub));
      end if;
   end Create_Mark;

   ---------------
   -- Move_Mark --
   ---------------

   procedure Move_Mark
     (Buffer : access Gtk_Text_Buffer_Record;
      Mark   : access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class;
      Where  : Gtk.Text_Iter.Gtk_Text_Iter)
   is
      procedure Internal
        (Buffer : System.Address;
         Mark   : System.Address;
         Where  : Gtk.Text_Iter.Gtk_Text_Iter);
      pragma Import (C, Internal, "gtk_text_buffer_move_mark");

   begin
      Internal (Get_Object (Buffer), Get_Object (Mark), Where);
   end Move_Mark;

   -----------------
   -- Delete_Mark --
   -----------------

   procedure Delete_Mark
     (Buffer : access Gtk_Text_Buffer_Record;
      Mark   : access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class)
   is
      procedure Internal
        (Buffer : System.Address;
         Mark   : System.Address);
      pragma Import (C, Internal, "gtk_text_buffer_delete_mark");

   begin
      Internal (Get_Object (Buffer), Get_Object (Mark));
   end Delete_Mark;

   --------------
   -- Get_Mark --
   --------------

   function Get_Mark
     (Buffer : access Gtk_Text_Buffer_Record;
      Name   : String) return Gtk.Text_Mark.Gtk_Text_Mark
   is
      function Internal
        (Buffer : System.Address;
         Name   : String) return System.Address;
      pragma Import (C, Internal, "gtk_text_buffer_get_mark");

      Stub : Gtk.Text_Mark.Gtk_Text_Mark_Record;
   begin
      return Gtk.Text_Mark.Gtk_Text_Mark
               (Get_User_Data_Fast
                 (Internal (Get_Object (Buffer), Name & ASCII.NUL),
                  Stub));
   end Get_Mark;

   -----------------------
   -- Move_Mark_By_Name --
   -----------------------

   procedure Move_Mark_By_Name
     (Buffer : access Gtk_Text_Buffer_Record;
      Name   : String;
      Where  : Gtk.Text_Iter.Gtk_Text_Iter)
   is
      procedure Internal
        (Buffer : System.Address;
         Name   : String;
         Where  : Gtk.Text_Iter.Gtk_Text_Iter);
      pragma Import (C, Internal, "gtk_text_buffer_move_mark_by_name");

   begin
      Internal (Get_Object (Buffer), Name & ASCII.NUL, Where);
   end Move_Mark_By_Name;

   -------------------------
   -- Delete_Mark_By_Name --
   -------------------------

   procedure Delete_Mark_By_Name
     (Buffer : access Gtk_Text_Buffer_Record;
      Name   : String)
   is
      procedure Internal
        (Buffer : System.Address;
         Name   : String);
      pragma Import (C, Internal, "gtk_text_buffer_delete_mark_by_name");

   begin
      Internal (Get_Object (Buffer), Name & ASCII.NUL);
   end Delete_Mark_By_Name;

   ----------------
   -- Get_Insert --
   ----------------

   function Get_Insert
     (Buffer : access Gtk_Text_Buffer_Record)
      return Gtk.Text_Mark.Gtk_Text_Mark
   is
      function Internal (Buffer : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_text_buffer_get_insert");
      Stub : Gtk.Text_Mark.Gtk_Text_Mark_Record;

   begin
      return Gtk.Text_Mark.Gtk_Text_Mark
               (Get_User_Data_Fast (Internal (Get_Object (Buffer)), Stub));
   end Get_Insert;

   -------------------------
   -- Get_Selection_Bound --
   -------------------------

   function Get_Selection_Bound (Buffer : access Gtk_Text_Buffer_Record)
                                 return Gtk.Text_Mark.Gtk_Text_Mark
   is
      function Internal (Buffer : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_text_buffer_get_selection_bound");
      Stub : Gtk.Text_Mark.Gtk_Text_Mark_Record;
   begin
      return Gtk.Text_Mark.Gtk_Text_Mark
               (Get_User_Data_Fast (Internal (Get_Object (Buffer)), Stub));
   end Get_Selection_Bound;

   ------------------
   -- Place_Cursor --
   ------------------

   procedure Place_Cursor
     (Buffer : access Gtk_Text_Buffer_Record;
      Where  : Gtk.Text_Iter.Gtk_Text_Iter)
   is
      procedure Internal
        (Buffer : System.Address;
         Where  : Gtk.Text_Iter.Gtk_Text_Iter);
      pragma Import (C, Internal, "gtk_text_buffer_place_cursor");

   begin
      Internal (Get_Object (Buffer), Where);
   end Place_Cursor;

   ---------------
   -- Apply_Tag --
   ---------------

   procedure Apply_Tag
     (Buffer  : access Gtk_Text_Buffer_Record;
      Tag     : access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class;
      Start   : Gtk.Text_Iter.Gtk_Text_Iter;
      The_End : Gtk.Text_Iter.Gtk_Text_Iter)
   is
      procedure Internal
        (Buffer  : System.Address;
         Tag     : System.Address;
         Start   : Gtk.Text_Iter.Gtk_Text_Iter;
         The_End : Gtk.Text_Iter.Gtk_Text_Iter);
      pragma Import (C, Internal, "gtk_text_buffer_apply_tag");

   begin
      Internal (Get_Object (Buffer), Get_Object (Tag), Start, The_End);
   end Apply_Tag;

   ----------------
   -- Remove_Tag --
   ----------------

   procedure Remove_Tag
     (Buffer  : access Gtk_Text_Buffer_Record;
      Tag     : access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class;
      Start   : Gtk.Text_Iter.Gtk_Text_Iter;
      The_End : Gtk.Text_Iter.Gtk_Text_Iter)
   is
      procedure Internal
        (Buffer  : System.Address;
         Tag     : System.Address;
         Start   : Gtk.Text_Iter.Gtk_Text_Iter;
         The_End : Gtk.Text_Iter.Gtk_Text_Iter);
      pragma Import (C, Internal, "gtk_text_buffer_remove_tag");
   begin
      Internal (Get_Object (Buffer), Get_Object (Tag), Start, The_End);
   end Remove_Tag;

   ---------------------
   -- Remove_All_Tags --
   ---------------------

   procedure Remove_All_Tags
     (Buffer  : access Gtk_Text_Buffer_Record;
      Start    : Gtk.Text_Iter.Gtk_Text_Iter;
      The_End : Gtk.Text_Iter.Gtk_Text_Iter)
   is
      procedure Internal
        (Buffer  : System.Address;
         Start   : Gtk.Text_Iter.Gtk_Text_Iter;
         The_End : Gtk.Text_Iter.Gtk_Text_Iter);
      pragma Import (C, Internal, "gtk_text_buffer_remove_all_tags");
   begin
      Internal (Get_Object (Buffer), Start, The_End);
   end Remove_All_Tags;

   -----------------------
   -- Apply_Tag_By_Name --
   -----------------------

   procedure Apply_Tag_By_Name
     (Buffer  : access Gtk_Text_Buffer_Record;
      Name    : String;
      Start   : Gtk.Text_Iter.Gtk_Text_Iter;
      The_End : Gtk.Text_Iter.Gtk_Text_Iter)
   is
      procedure Internal
        (Buffer  : System.Address;
         Name    : String;
         Start   : Gtk.Text_Iter.Gtk_Text_Iter;
         The_End : Gtk.Text_Iter.Gtk_Text_Iter);
      pragma Import (C, Internal, "gtk_text_buffer_apply_tag_by_name");

   begin
      Internal (Get_Object (Buffer), Name & ASCII.NUL, Start, The_End);
   end Apply_Tag_By_Name;

   ------------------------
   -- Remove_Tag_By_Name --
   ------------------------

   procedure Remove_Tag_By_Name
     (Buffer  : access Gtk_Text_Buffer_Record;
      Name    : String;
      Start   : Gtk.Text_Iter.Gtk_Text_Iter;
      The_End : Gtk.Text_Iter.Gtk_Text_Iter)
   is
      procedure Internal
        (Buffer  : System.Address;
         Name    : String;
         Start   : Gtk.Text_Iter.Gtk_Text_Iter;
         The_End : Gtk.Text_Iter.Gtk_Text_Iter);
      pragma Import (C, Internal, "gtk_text_buffer_remove_tag_by_name");

   begin
      Internal (Get_Object (Buffer), Name & ASCII.NUL, Start, The_End);
   end Remove_Tag_By_Name;

   -----------------------------
   -- Get_Iter_At_Line_Offset --
   -----------------------------

   procedure Get_Iter_At_Line_Offset
     (Buffer      : access Gtk_Text_Buffer_Record;
      Iter        : out Gtk.Text_Iter.Gtk_Text_Iter;
      Line_Number : Gint;
      Char_Offset : Gint := 0)
   is
      procedure Internal
        (Buffer      : System.Address;
         Iter        : out Gtk.Text_Iter.Gtk_Text_Iter;
         Line_Number : Gint;
         Char_Offset : Gint);
      pragma Import (C, Internal, "gtk_text_buffer_get_iter_at_line_offset");

      Result : Boolean;
   begin
      --  Prevent critical crashes from gtk+ if Char_Offset is invalid
      Internal (Get_Object (Buffer), Iter, Line_Number, 0);

      if not Ends_Line (Iter) then
         Forward_To_Line_End (Iter, Result);

         if Char_Offset <= Get_Line_Offset (Iter) then
            Internal (Get_Object (Buffer), Iter, Line_Number, Char_Offset);
         end if;
      end if;
   end Get_Iter_At_Line_Offset;

   ----------------------------
   -- Get_Iter_At_Line_Index --
   ----------------------------

   procedure Get_Iter_At_Line_Index
     (Buffer      : access Gtk_Text_Buffer_Record;
      Iter        : out Gtk.Text_Iter.Gtk_Text_Iter;
      Line_Number : Gint;
      Byte_Index  : Gint := 0)
   is
      procedure Internal
        (Buffer      : System.Address;
         Iter        : out Gtk.Text_Iter.Gtk_Text_Iter;
         Line_Number : Gint;
         Byte_Index  : Gint);
      pragma Import (C, Internal, "gtk_text_buffer_get_iter_at_line_index");

   begin
      Internal (Get_Object (Buffer), Iter, Line_Number, Byte_Index);
   end Get_Iter_At_Line_Index;

   ------------------------
   -- Get_Iter_At_Offset --
   ------------------------

   procedure Get_Iter_At_Offset
     (Buffer      : access Gtk_Text_Buffer_Record;
      Iter        : out Gtk.Text_Iter.Gtk_Text_Iter;
      Char_Offset : Gint)
   is
      procedure Internal
        (Buffer      : System.Address;
         Iter        : out Gtk.Text_Iter.Gtk_Text_Iter;
         Char_Offset : Gint);
      pragma Import (C, Internal, "gtk_text_buffer_get_iter_at_offset");

   begin
      Internal (Get_Object (Buffer), Iter, Char_Offset);
   end Get_Iter_At_Offset;

   ----------------------
   -- Get_Iter_At_Line --
   ----------------------

   procedure Get_Iter_At_Line
     (Buffer      : access Gtk_Text_Buffer_Record;
      Iter        : out Gtk.Text_Iter.Gtk_Text_Iter;
      Line_Number : Gint)
   is
      procedure Internal
        (Buffer      : System.Address;
         Iter        : System.Address;
         Line_Number : Gint);
      pragma Import (C, Internal, "gtk_text_buffer_get_iter_at_line");

   begin
      Internal (Get_Object (Buffer), Iter'Address, Line_Number);
   end Get_Iter_At_Line;

   --------------------
   -- Get_Start_Iter --
   --------------------

   procedure Get_Start_Iter
     (Buffer : access Gtk_Text_Buffer_Record;
      Iter   : out Gtk.Text_Iter.Gtk_Text_Iter)
   is
      procedure Internal
        (Buffer : System.Address;
         Iter   : System.Address);
      pragma Import (C, Internal, "gtk_text_buffer_get_start_iter");

   begin
      Internal (Get_Object (Buffer), Iter'Address);
   end Get_Start_Iter;

   ------------------
   -- Get_End_Iter --
   ------------------

   procedure Get_End_Iter
     (Buffer : access Gtk_Text_Buffer_Record;
      Iter   : out Gtk.Text_Iter.Gtk_Text_Iter)
   is
      procedure Internal
        (Buffer : System.Address;
         Iter   : System.Address);
      pragma Import (C, Internal, "gtk_text_buffer_get_end_iter");

   begin
      Internal (Get_Object (Buffer), Iter'Address);
   end Get_End_Iter;

   ----------------
   -- Get_Bounds --
   ----------------

   procedure Get_Bounds
     (Buffer  : access Gtk_Text_Buffer_Record;
      Start   : out Gtk.Text_Iter.Gtk_Text_Iter;
      The_End : out Gtk.Text_Iter.Gtk_Text_Iter)
   is
      procedure Internal
        (Buffer  : System.Address;
         Start   : System.Address;
         The_End : System.Address);
      pragma Import (C, Internal, "gtk_text_buffer_get_bounds");

   begin
      Internal (Get_Object (Buffer), Start'Address, The_End'Address);
   end Get_Bounds;

   ----------------------
   -- Get_Iter_At_Mark --
   ----------------------

   procedure Get_Iter_At_Mark
     (Buffer : access Gtk_Text_Buffer_Record;
      Iter   : out Gtk.Text_Iter.Gtk_Text_Iter;
      Mark   : access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class)
   is
      procedure Internal
        (Buffer : System.Address;
         Iter   : System.Address;
         Mark   : System.Address);
      pragma Import (C, Internal, "gtk_text_buffer_get_iter_at_mark");

   begin
      Internal (Get_Object (Buffer), Iter'Address, Get_Object (Mark));
   end Get_Iter_At_Mark;

   ------------------------------
   -- Get_Iter_At_Child_Anchor --
   ------------------------------

   procedure Get_Iter_At_Child_Anchor
     (Buffer : access Gtk_Text_Buffer_Record;
      Iter   : out Gtk.Text_Iter.Gtk_Text_Iter;
      Anchor : access Gtk.Text_Child.Gtk_Text_Child_Anchor_Record'Class)
   is
      procedure Internal
        (Buffer : System.Address;
         Iter   : System.Address;
         Anchor : System.Address);
      pragma Import (C, Internal, "gtk_text_buffer_get_iter_at_child_anchor");

   begin
      Internal (Get_Object (Buffer), Iter'Address, Get_Object (Anchor));
   end Get_Iter_At_Child_Anchor;

   ------------------
   -- Get_Modified --
   ------------------

   function Get_Modified
     (Buffer : access Gtk_Text_Buffer_Record) return Boolean
   is
      function Internal (Buffer : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_text_buffer_get_modified");

   begin
      return Internal (Get_Object (Buffer)) /= 0;
   end Get_Modified;

   ------------------
   -- Set_Modified --
   ------------------

   procedure Set_Modified
     (Buffer  : access Gtk_Text_Buffer_Record;
      Setting : Boolean := True)
   is
      procedure Internal (Buffer : System.Address; Setting : Gboolean);
      pragma Import (C, Internal, "gtk_text_buffer_set_modified");

   begin
      Internal (Get_Object (Buffer), Boolean'Pos (Setting));
   end Set_Modified;

   -------------------
   -- Cut_Clipboard --
   -------------------

   procedure Cut_Clipboard
     (Buffer           : access Gtk_Text_Buffer_Record;
      Clipboard        : Gtk.Clipboard.Gtk_Clipboard;
      Default_Editable : Boolean := True)
   is
      procedure Internal
        (Buffer           : System.Address;
         Clipboard        : Gtk.Clipboard.Gtk_Clipboard;
         Default_Editable : Gboolean);
      pragma Import (C, Internal, "gtk_text_buffer_cut_clipboard");

   begin
      Internal
        (Get_Object (Buffer), Clipboard, Boolean'Pos (Default_Editable));
   end Cut_Clipboard;

   --------------------
   -- Copy_Clipboard --
   --------------------

   procedure Copy_Clipboard
     (Buffer     : access Gtk_Text_Buffer_Record;
      Clipboard  : Gtk.Clipboard.Gtk_Clipboard)
   is
      procedure Internal
        (Buffer : System.Address; Clipboard : Gtk.Clipboard.Gtk_Clipboard);
      pragma Import (C, Internal, "gtk_text_buffer_copy_clipboard");

   begin
      Internal (Get_Object (Buffer), Clipboard);
   end Copy_Clipboard;

   ---------------------
   -- Paste_Clipboard --
   ---------------------

   procedure Paste_Clipboard
     (Buffer            : access Gtk_Text_Buffer_Record;
      Clipboard         : Gtk.Clipboard.Gtk_Clipboard;
      Override_Location : Gtk.Text_Iter.Gtk_Text_Iter_Access := null;
      Default_Editable  : Boolean := True)
   is
      procedure Internal
        (Buffer            : System.Address;
         Clipboard         : Gtk.Clipboard.Gtk_Clipboard;
         Override_Location : System.Address;
         Default_Editable  : Gboolean);
      pragma Import (C, Internal, "gtk_text_buffer_paste_clipboard");

   begin
      if Override_Location = null then
         Internal
           (Get_Object (Buffer), Clipboard,
            System.Null_Address, Boolean'Pos (Default_Editable));
      else
         Internal
           (Get_Object (Buffer), Clipboard,
            Override_Location.all'Address, Boolean'Pos (Default_Editable));
      end if;
   end Paste_Clipboard;

   ----------------------
   -- Selection_Exists --
   ----------------------

   function Selection_Exists
     (Buffer : access Gtk_Text_Buffer_Record) return Boolean
   is
      function Internal
        (Buffer  : System.Address;
         Start   : System.Address := System.Null_Address;
         The_End : System.Address := System.Null_Address) return Gboolean;
      pragma Import (C, Internal, "gtk_text_buffer_get_selection_bounds");
   begin
      return Internal (Get_Object (Buffer)) /= 0;
   end Selection_Exists;

   --------------------------
   -- Get_Selection_Bounds --
   --------------------------

   procedure Get_Selection_Bounds
     (Buffer  : access Gtk_Text_Buffer_Record;
      Start   : out Gtk.Text_Iter.Gtk_Text_Iter;
      The_End : out Gtk.Text_Iter.Gtk_Text_Iter;
      Result  : out Boolean)
   is
      function Internal
        (Buffer  : System.Address;
         Start   : System.Address;
         The_End : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_text_buffer_get_selection_bounds");
   begin
      Result :=
        Internal (Get_Object (Buffer), Start'Address, The_End'Address) /= 0;
   end Get_Selection_Bounds;

   ----------------------
   -- Delete_Selection --
   ----------------------

   function Delete_Selection
     (Buffer           : access Gtk_Text_Buffer_Record;
      Interactive      : Boolean;
      Default_Editable : Boolean) return Boolean
   is
      function Internal
        (Buffer           : System.Address;
         Interactive      : Gboolean;
         Default_Editable : Gboolean) return Gboolean;
      pragma Import (C, Internal, "gtk_text_buffer_delete_selection");

   begin
      return
        Internal
          (Get_Object (Buffer),
           Boolean'Pos (Interactive),
           Boolean'Pos (Default_Editable)) /= 0;
   end Delete_Selection;

   -----------------------
   -- Begin_User_Action --
   -----------------------

   procedure Begin_User_Action (Buffer : access Gtk_Text_Buffer_Record) is
      procedure Internal (Buffer : System.Address);
      pragma Import (C, Internal, "gtk_text_buffer_begin_user_action");

   begin
      Internal (Get_Object (Buffer));
   end Begin_User_Action;

   ---------------------
   -- End_User_Action --
   ---------------------

   procedure End_User_Action (Buffer : access Gtk_Text_Buffer_Record) is
      procedure Internal (Buffer : System.Address);
      pragma Import (C, Internal, "gtk_text_buffer_end_user_action");

   begin
      Internal (Get_Object (Buffer));
   end End_User_Action;

   ----------------
   -- Get_Buffer --
   ----------------
   function Get_Buffer (Iter : Gtk.Text_Iter.Gtk_Text_Iter)
     return Gtk_Text_Buffer
   is
      function Internal
        (Iter : Gtk.Text_Iter.Gtk_Text_Iter) return System.Address;
      pragma Import (C, Internal, "gtk_text_iter_get_buffer");
      --  External binding: gtk_text_iter_get_buffer
      Stub : Gtk_Text_Buffer_Record;
   begin
      return Gtk_Text_Buffer (Get_User_Data_Fast (Internal (Iter), Stub));
   end Get_Buffer;

   ----------------
   -- Get_Buffer --
   ----------------

   function Get_Buffer
     (Mark : Gtk.Text_Mark.Gtk_Text_Mark) return Gtk_Text_Buffer
   is
      function Internal (Iter : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_text_mark_get_buffer");
      --  External binding: gtk_text_mark_get_buffer
      Stub : Gtk_Text_Buffer_Record;
   begin
      return Gtk_Text_Buffer
        (Get_User_Data_Fast (Internal (Get_Object (Mark)), Stub));
   end Get_Buffer;

   -----------------------------
   -- Add_Selection_Clipboard --
   -----------------------------

   procedure Add_Selection_Clipboard
     (Buffer    : access Gtk_Text_Buffer_Record;
      Clipboard : Gtk_Clipboard)
   is
      procedure Internal
        (Buffer    : System.Address;
         Clipboard : Gtk_Clipboard);
      pragma Import (C, Internal, "gtk_text_buffer_add_selection_clipboard");
   begin
      Internal (Get_Object (Buffer), Clipboard);
   end Add_Selection_Clipboard;

   --------------------------------
   -- Remove_Selection_Clipboard --
   --------------------------------

   procedure Remove_Selection_Clipboard
     (Buffer    : access Gtk_Text_Buffer_Record;
      Clipboard : Gtk_Clipboard)
   is
      procedure Internal
        (Buffer    : System.Address;
         Clipboard : Gtk_Clipboard);
      pragma Import
        (C, Internal, "gtk_text_buffer_remove_selection_clipboard");
   begin
      Internal (Get_Object (Buffer), Clipboard);
   end Remove_Selection_Clipboard;

   ----------------
   -- Create_Tag --
   ----------------

   function Create_Tag
     (Buffer              : access Gtk_Text_Buffer_Record;
      Tag_Name            : String := "")
      return Gtk_Text_Tag
   is
      function Internal
        (Buffer              : System.Address;
         Tag_Name            : Interfaces.C.Strings.chars_ptr)
         return System.Address;
      pragma Import (C, Internal, "ada_gtk_text_buffer_create_tag");
      Stub : Gtk_Text_Tag_Record;
      Str  : chars_ptr := String_Or_Null (Tag_Name);
      Tag  : Gtk_Text_Tag;
   begin
      Tag := Gtk_Text_Tag
        (Get_User_Data (Internal (Get_Object (Buffer), Str), Stub));
      Free (Str);
      return Tag;
   end Create_Tag;

   ---------------
   -- Backspace --
   ---------------

   function Backspace
     (Buffer           : access Gtk_Text_Buffer_Record;
      Iter             : Gtk.Text_Iter.Gtk_Text_Iter;
      Interactive      : Boolean;
      Default_Editable : Boolean)
      return Boolean
   is
      function Internal
        (Buffer           : System.Address;
         Iter             : Gtk_Text_Iter;
         Interactive      : Gboolean;
         Default_Editable : Gboolean)
         return Gboolean;
      pragma Import (C, Internal, "gtk_text_buffer_backspace");
   begin
      return Boolean'Val
        (Internal (Get_Object (Buffer), Iter,
         Boolean'Pos (Interactive), Boolean'Pos (Default_Editable)));
   end Backspace;

   ------------------
   -- Select_Range --
   ------------------

   procedure Select_Range
     (Buffer : access Gtk_Text_Buffer_Record;
      Ins    : Gtk_Text_Iter;
      Bound  : Gtk_Text_Iter)
   is
      procedure Internal
        (Buffer : System.Address;
         Ins    : Gtk_Text_Iter;
         Bound  : Gtk_Text_Iter);
      pragma Import (C, Internal, "gtk_text_buffer_select_range");
   begin
      Internal (Get_Object (Buffer), Ins, Bound);
   end Select_Range;

end Gtk.Text_Buffer;
