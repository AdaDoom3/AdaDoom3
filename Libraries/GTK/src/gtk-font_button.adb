-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                 Copyright (C) 2006-2013, AdaCore                  --
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

with Interfaces.C.Strings;  use Interfaces.C.Strings;
with System;

with Glib.Type_Conversion_Hooks;

package body Gtk.Font_Button is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Font_Button_Record);
   pragma Warnings (Off, Type_Conversion);

   -------------------
   -- Get_Font_Name --
   -------------------

   function Get_Font_Name
     (Font_Button : access Gtk_Font_Button_Record) return String
   is
      function Internal (Font_Button : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_font_button_get_font_name");
   begin
      --  Do not free returned value, owned by gtk+
      return Value (Internal (Get_Object (Font_Button)));
   end Get_Font_Name;

   -------------------
   -- Get_Show_Size --
   -------------------

   function Get_Show_Size
     (Font_Button : access Gtk_Font_Button_Record) return Boolean
   is
      function Internal (Font_Button : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_font_button_get_show_size");
   begin
      return Boolean'Val (Internal (Get_Object (Font_Button)));
   end Get_Show_Size;

   --------------------
   -- Get_Show_Style --
   --------------------

   function Get_Show_Style
     (Font_Button : access Gtk_Font_Button_Record) return Boolean
   is
      function Internal (Font_Button : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_font_button_get_show_style");
   begin
      return Boolean'Val (Internal (Get_Object (Font_Button)));
   end Get_Show_Style;

   ---------------
   -- Get_Title --
   ---------------

   function Get_Title
     (Font_Button : access Gtk_Font_Button_Record) return String
   is
      function Internal (Font_Button : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_font_button_get_title");
   begin
      --  Returned value still owned by gtk+
      return Value (Internal (Get_Object (Font_Button)));
   end Get_Title;

   ------------------
   -- Get_Use_Font --
   ------------------

   function Get_Use_Font
     (Font_Button : access Gtk_Font_Button_Record) return Boolean
   is
      function Internal (Font_Button : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_font_button_get_use_font");
   begin
      return Boolean'Val (Internal (Get_Object (Font_Button)));
   end Get_Use_Font;

   ------------------
   -- Get_Use_Size --
   ------------------

   function Get_Use_Size
     (Font_Button : access Gtk_Font_Button_Record) return Boolean
   is
      function Internal (Font_Button : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_font_button_get_use_size");
   begin
      return Boolean'Val (Internal (Get_Object (Font_Button)));
   end Get_Use_Size;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Font_Button : out Gtk_Font_Button) is
   begin
      Font_Button := new Gtk_Font_Button_Record;
      Gtk.Font_Button.Initialize (Font_Button);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Font_Button : access Gtk_Font_Button_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_font_button_new");
   begin
      Set_Object (Font_Button, Internal);
   end Initialize;

   -----------------------
   -- Gtk_New_With_Font --
   -----------------------

   procedure Gtk_New_With_Font
     (Font_Button : out Gtk_Font_Button; Fontname : String)
   is
   begin
      Font_Button := new Gtk_Font_Button_Record;
      Initialize_With_Font (Font_Button, Fontname);
   end Gtk_New_With_Font;

   --------------------------
   -- Initialize_With_Font --
   --------------------------

   procedure Initialize_With_Font
     (Font_Button : access Gtk_Font_Button_Record'Class;
      Fontname   : String)
   is
      function Internal (Fontname : String) return System.Address;
      pragma Import (C, Internal, "gtk_font_button_new_with_font");
   begin
      Set_Object (Font_Button, Internal (Fontname & ASCII.NUL));
   end Initialize_With_Font;

   -------------------
   -- Set_Font_Name --
   -------------------

   function Set_Font_Name
     (Font_Button : access Gtk_Font_Button_Record;
      Fontname    : String)
      return Boolean
   is
      function Internal
        (Font_Button : System.Address;
         Fontname    : String) return Gboolean;
      pragma Import (C, Internal, "gtk_font_button_set_font_name");
   begin
      return Boolean'Val
        (Internal (Get_Object (Font_Button), Fontname & ASCII.NUL));
   end Set_Font_Name;

   -------------------
   -- Set_Show_Size --
   -------------------

   procedure Set_Show_Size
     (Font_Button : access Gtk_Font_Button_Record;
      Show_Size   : Boolean)
   is
      procedure Internal
        (Font_Button : System.Address;
         Show_Size   : Gboolean);
      pragma Import (C, Internal, "gtk_font_button_set_show_size");
   begin
      Internal (Get_Object (Font_Button), Boolean'Pos (Show_Size));
   end Set_Show_Size;

   --------------------
   -- Set_Show_Style --
   --------------------

   procedure Set_Show_Style
     (Font_Button : access Gtk_Font_Button_Record;
      Show_Style  : Boolean)
   is
      procedure Internal
        (Font_Button : System.Address;
         Show_Style  : Gboolean);
      pragma Import (C, Internal, "gtk_font_button_set_show_style");
   begin
      Internal (Get_Object (Font_Button), Boolean'Pos (Show_Style));
   end Set_Show_Style;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title
     (Font_Button : access Gtk_Font_Button_Record;
      Title       : String)
   is
      procedure Internal
        (Font_Button : System.Address;
         Title       : String);
      pragma Import (C, Internal, "gtk_font_button_set_title");
   begin
      Internal (Get_Object (Font_Button), Title & ASCII.NUL);
   end Set_Title;

   ------------------
   -- Set_Use_Font --
   ------------------

   procedure Set_Use_Font
     (Font_Button : access Gtk_Font_Button_Record;
      Use_Font    : Boolean)
   is
      procedure Internal
        (Font_Button : System.Address;
         Use_Font    : Gboolean);
      pragma Import (C, Internal, "gtk_font_button_set_use_font");
   begin
      Internal (Get_Object (Font_Button), Boolean'Pos (Use_Font));
   end Set_Use_Font;

   ------------------
   -- Set_Use_Size --
   ------------------

   procedure Set_Use_Size
     (Font_Button : access Gtk_Font_Button_Record;
      Use_Size    : Boolean)
   is
      procedure Internal
        (Font_Button : System.Address;
         Use_Size    : Gboolean);
      pragma Import (C, Internal, "gtk_font_button_set_use_size");
   begin
      Internal (Get_Object (Font_Button), Boolean'Pos (Use_Size));
   end Set_Use_Size;

end Gtk.Font_Button;
