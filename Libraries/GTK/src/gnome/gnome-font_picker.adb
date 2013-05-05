-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                  Copyright (C) 2001-2006                          --
--                           AdaCore                                 --
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

with Gtk; use Gtk;
with Interfaces.C.Strings;
with System;

package body Gnome.Font_Picker is

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New (Widget : out Gnome_Font_Picker) is
   begin
      Widget := new Gnome_Font_Picker_Record;
      Gnome.Font_Picker.Initialize (Widget);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gnome_Font_Picker_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gnome_font_picker_new");
   begin
      Set_Object (Widget, Internal);
   end Initialize;

   ----------------------
   -- Fi_Set_Show_Size --
   ----------------------

   procedure Fi_Set_Show_Size
     (Gfp       : access Gnome_Font_Picker_Record;
      Show_Size : Boolean)
   is
      procedure Internal
        (Gfp       : System.Address;
         Show_Size : Gint);
      pragma Import (C, Internal, "gnome_font_picker_fi_set_show_size");
   begin
      Internal (Get_Object (Gfp),
                Boolean'Pos (Show_Size));
   end Fi_Set_Show_Size;

   ------------------------------
   -- Fi_Set_Use_Font_In_Label --
   ------------------------------

   procedure Fi_Set_Use_Font_In_Label
     (Gfp               : access Gnome_Font_Picker_Record;
      Use_Font_In_Label : Boolean;
      Size              : Gint)
   is
      procedure Internal
        (Gfp               : System.Address;
         Use_Font_In_Label : Gint;
         Size              : Gint);
      pragma Import
        (C, Internal, "gnome_font_picker_fi_set_use_font_in_label");
   begin
      Internal (Get_Object (Gfp),
                Boolean'Pos (Use_Font_In_Label),
                Size);
   end Fi_Set_Use_Font_In_Label;

   -------------------
   -- Get_Font_Name --
   -------------------

   function Get_Font_Name
     (Gfp : access Gnome_Font_Picker_Record) return String
   is
      function Internal
        (Gfp : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gnome_font_picker_get_font_name");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Gfp)));
   end Get_Font_Name;

   ----------------------
   -- Get_Preview_Text --
   ----------------------

   function Get_Preview_Text (Gfp    : access Gnome_Font_Picker_Record)
                              return String
   is
      function Internal (Gfp    : System.Address)
                         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gnome_font_picker_get_preview_text");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Gfp)));
   end Get_Preview_Text;

   -------------------
   -- Set_Font_Name --
   -------------------

   function Set_Font_Name
     (Gfp      : access Gnome_Font_Picker_Record;
      Fontname : String)
      return Boolean
   is
      function Internal
        (Gfp      : System.Address;
         Fontname : String)
         return Gint;
      pragma Import (C, Internal, "gnome_font_picker_set_font_name");
   begin
      return Boolean'Val (Internal (Get_Object (Gfp),
                                    Fontname & ASCII.NUL));
   end Set_Font_Name;

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode
     (Gfp  : access Gnome_Font_Picker_Record;
      Mode : Gnome_Font_Picker_Mode)
   is
      procedure Internal
        (Gfp  : System.Address;
         Mode : Gint);
      pragma Import (C, Internal, "gnome_font_picker_set_mode");
   begin
      Internal (Get_Object (Gfp),
                Gnome_Font_Picker_Mode'Pos (Mode));
   end Set_Mode;

   ----------------------
   -- Set_Preview_Text --
   ----------------------

   procedure Set_Preview_Text
     (Gfp  : access Gnome_Font_Picker_Record;
      Text : String)
   is
      procedure Internal
        (Gfp  : System.Address;
         Text : String);
      pragma Import (C, Internal, "gnome_font_picker_set_preview_text");
   begin
      Internal (Get_Object (Gfp),
                Text & ASCII.NUL);
   end Set_Preview_Text;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title
     (Gfp   : access Gnome_Font_Picker_Record;
      Title : String)
   is
      procedure Internal
        (Gfp   : System.Address;
         Title : String);
      pragma Import (C, Internal, "gnome_font_picker_set_title");
   begin
      Internal (Get_Object (Gfp),
                Title & ASCII.NUL);
   end Set_Title;

   -------------------
   -- Uw_Set_Widget --
   -------------------

   procedure Uw_Set_Widget
     (Gfp    : access Gnome_Font_Picker_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
        (Gfp    : System.Address;
         Widget : System.Address);
      pragma Import (C, Internal, "gnome_font_picker_uw_set_widget");
   begin
      Internal (Get_Object (Gfp),
                Get_Object (Widget));
   end Uw_Set_Widget;

end Gnome.Font_Picker;
