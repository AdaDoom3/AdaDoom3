-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                 Copyright (C) 2000-2013, AdaCore                  --
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
with Interfaces.C.Strings;

with Glib.Type_Conversion_Hooks;

package body Gtk.Old_Editable is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Old_Editable_Record);
   pragma Warnings (Off, Type_Conversion);

   -------------
   -- Changed --
   -------------

   procedure Changed (Editable : access Gtk_Old_Editable_Record) is
      procedure Internal (Editable : System.Address);
      pragma Import (C, Internal, "gtk_old_editable_changed");

   begin
      Internal (Get_Object (Editable));
   end Changed;

   ---------------------
   -- Claim_Selection --
   ---------------------

   procedure Claim_Selection
     (Editable : access Gtk_Old_Editable_Record;
      Claim    : Boolean := True;
      Time     : Guint32)
   is
      procedure Internal
        (Editable : System.Address;
         Claim    : Gint;
         Time     : Guint32);
      pragma Import (C, Internal, "gtk_old_editable_claim_selection");

   begin
      Internal (Get_Object (Editable), Boolean'Pos (Claim), Time);
   end Claim_Selection;

   --------------------
   -- Copy_Clipboard --
   --------------------

   procedure Copy_Clipboard
     (Editable : access Gtk_Old_Editable_Record;
      Time     : Guint32)
   is
      procedure Internal
        (Editable : System.Address;
         Time     : Guint32);
      pragma Import (C, Internal, "gtk_editable_copy_clipboard");

   begin
      Internal (Get_Object (Editable), Time);
   end Copy_Clipboard;

   -------------------
   -- Cut_Clipboard --
   -------------------

   procedure Cut_Clipboard
     (Editable : access Gtk_Old_Editable_Record;
      Time     : Guint32)
   is
      procedure Internal
        (Editable : System.Address;
         Time     : Guint32);
      pragma Import (C, Internal, "gtk_editable_cut_clipboard");

   begin
      Internal (Get_Object (Editable), Time);
   end Cut_Clipboard;

   ----------------------
   -- Delete_Selection --
   ----------------------

   procedure Delete_Selection
     (Editable : access Gtk_Old_Editable_Record)
   is
      procedure Internal (Editable : System.Address);
      pragma Import (C, Internal, "gtk_editable_delete_selection");

   begin
      Internal (Get_Object (Editable));
   end Delete_Selection;

   -----------------
   -- Delete_Text --
   -----------------

   procedure Delete_Text
     (Editable  : access Gtk_Old_Editable_Record;
      Start_Pos : Gint := 0;
      End_Pos   : Gint := -1)
   is
      procedure Internal
        (Editable  : System.Address;
         Start_Pos : Gint;
         End_Pos   : Gint);
      pragma Import (C, Internal, "gtk_editable_delete_text");

   begin
      Internal (Get_Object (Editable), Start_Pos, End_Pos);
   end Delete_Text;

   ---------------
   -- Get_Chars --
   ---------------

   function Get_Chars
     (Editable  : access Gtk_Old_Editable_Record;
      Start_Pos : Gint := 0;
      End_Pos   : Gint := -1) return UTF8_String
   is
      function Internal
        (Editable  : System.Address;
         Start_Pos : Gint;
         End_Pos   : Gint) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_editable_get_chars");

      procedure Internal_G_Free (Mem : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal_G_Free, "g_free");
      use type Interfaces.C.Strings.chars_ptr;

      Temp : constant Interfaces.C.Strings.chars_ptr :=
        Internal (Get_Object (Editable), Start_Pos, End_Pos);

   begin
      if Temp /= Interfaces.C.Strings.Null_Ptr then
         declare
            Result : constant String := Interfaces.C.Strings.Value (Temp);
         begin
            Internal_G_Free (Temp);
            return UTF8_String'(Result);
         end;
      else
         return "";
      end if;
   end Get_Chars;

   ------------------------
   -- Get_Clipboard_Text --
   ------------------------

   function Get_Clipboard_Text (Widget : access Gtk_Old_Editable_Record)
                                 return      UTF8_String
   is
      function Internal (Widget : in System.Address)
                         return      Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "ada_editable_get_clipboard_text");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Widget)));
   end Get_Clipboard_Text;

   ------------------
   -- Get_Editable --
   ------------------

   function Get_Editable
     (Widget : access Gtk_Old_Editable_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Guint;
      pragma Import (C, Internal, "ada_editable_get_editable");

   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Get_Editable;

   -----------------------
   -- Get_Has_Selection --
   -----------------------

   function Get_Has_Selection (Widget : access Gtk_Old_Editable_Record)
                               return      Boolean
   is
      function Internal (Widget : in System.Address)
                         return      Guint;
      pragma Import (C, Internal, "ada_editable_get_has_selection");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Get_Has_Selection;

   ------------------
   -- Get_Position --
   ------------------

   function Get_Position
     (Editable : access Gtk_Old_Editable_Record) return Gint
   is
      function Internal (Editable : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_editable_get_position");
   begin
      return Internal (Get_Object (Editable));
   end Get_Position;

   ---------------------------
   -- Get_Selection_End_Pos --
   ---------------------------

   function Get_Selection_End_Pos (Widget : access Gtk_Old_Editable_Record)
                                   return      Guint
   is
      function Internal (Widget : in System.Address)
                         return      Guint;
      pragma Import (C, Internal, "ada_editable_get_selection_end_pos");
   begin
      return Internal (Get_Object (Widget));
   end Get_Selection_End_Pos;

   -----------------------------
   -- Get_Selection_Start_Pos --
   -----------------------------

   function Get_Selection_Start_Pos (Widget : access Gtk_Old_Editable_Record)
                                     return      Guint
   is
      function Internal (Widget : in System.Address)
                         return      Guint;
      pragma Import (C, Internal, "ada_editable_get_selection_start_pos");
   begin
      return Internal (Get_Object (Widget));
   end Get_Selection_Start_Pos;

   -----------------
   -- Insert_Text --
   -----------------

   procedure Insert_Text
     (Editable : access Gtk_Old_Editable_Record;
      New_Text : UTF8_String;
      Position : in out Gint)
   is
      procedure Internal
        (Editable        : System.Address;
         New_Text        : UTF8_String;
         New_Text_Length : Gint;
         Position        : in out Gint);
      pragma Import (C, Internal, "gtk_editable_insert_text");

   begin
      Internal
        (Get_Object (Editable),
         New_Text,
         New_Text'Length,
         Position);
   end Insert_Text;

   ---------------------
   -- Paste_Clipboard --
   ---------------------

   procedure Paste_Clipboard
     (Editable : access Gtk_Old_Editable_Record;
      Time     : Guint32)
   is
      procedure Internal
        (Editable : System.Address;
         Time     : Guint32);
      pragma Import (C, Internal, "gtk_editable_paste_clipboard");

   begin
      Internal (Get_Object (Editable), Time);
   end Paste_Clipboard;

   -------------------
   -- Select_Region --
   -------------------

   procedure Select_Region
     (Editable : access Gtk_Old_Editable_Record;
      Start    : Gint;
      The_End  : Gint := -1)
   is
      procedure Internal
        (Editable : System.Address;
         Start    : Gint;
         The_End  : Gint);
      pragma Import (C, Internal, "gtk_editable_select_region");

   begin
      Internal (Get_Object (Editable), Start, The_End);
   end Select_Region;

   ------------------
   -- Set_Editable --
   ------------------

   procedure Set_Editable (Widget : access Gtk_Old_Editable_Record;
                           Editable : in Boolean := True)
   is
      procedure Internal (Widget : in System.Address; Editable : Guint);
      pragma Import (C, Internal, "ada_editable_set_editable");
   begin
      Internal (Get_Object (Widget), Boolean'Pos (Editable));
   end Set_Editable;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position
     (Editable : access Gtk_Old_Editable_Record;
      Position : Gint)
   is
      procedure Internal (Editable : System.Address; Position : Gint);
      pragma Import (C, Internal, "gtk_editable_set_position");

   begin
      Internal (Get_Object (Editable), Position);
   end Set_Position;

end Gtk.Old_Editable;
