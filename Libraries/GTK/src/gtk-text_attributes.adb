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

package body Gtk.Text_Attributes is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Text_Attr : out Gtk_Text_Attributes)
   is
      function Internal return Gtk_Text_Attributes;
      pragma Import (C, Internal, "gtk_text_attributes_new");
   begin
      Text_Attr := Internal;
   end Gtk_New;

   -----------------------
   -- Set_Strikethrough --
   -----------------------

   procedure Set_Strikethrough
     (Appearance : Gtk_Text_Appearance;
      Strikethrough : Boolean)
   is
      procedure Internal
        (Appearance    : Gtk_Text_Appearance;
         Strikethrough : Gboolean);

      pragma Import
        (C, Internal, "ada_text_appearance_set_strikethrough");
   begin
      Internal (Appearance, Boolean'Pos (Strikethrough));
   end Set_Strikethrough;

   -----------------------
   -- Get_Strikethrough --
   -----------------------

   function Get_Strikethrough
     (Appearance : Gtk_Text_Appearance) return Boolean

   is
      function Internal (Appearance : Gtk_Text_Appearance) return Gboolean;
      pragma Import
        (C, Internal, "ada_text_appearance_get_strikethrough");
   begin
      return Boolean'Val (Internal (Appearance));
   end Get_Strikethrough;

   -------------------
   -- Set_Invisible --
   -------------------

   procedure Set_Invisible
     (Text_Attr : Gtk_Text_Attributes;
      Invisible : Boolean)
   is
      procedure Internal
        (Text_Attr : Gtk_Text_Attributes;
         Invisible : Gboolean);

      pragma Import
        (C, Internal, "ada_text_attribute_set_invisible");
   begin
      Internal (Text_Attr, Boolean'Pos (Invisible));
   end Set_Invisible;

   -------------------
   -- Get_Invisible --
   -------------------

   function Get_Invisible
     (Text_Attr : Gtk_Text_Attributes) return Boolean
   is
      function Internal (Text_Attr : Gtk_Text_Attributes) return Gboolean;
      pragma Import
        (C, Internal, "ada_text_attribute_get_invisible");
   begin
      return Boolean'Val (Internal (Text_Attr));
   end Get_Invisible;

   -----------------------
   -- Set_BG_Full_Height --
   -----------------------

   procedure Set_Bg_Full_Height
     (Text_Attr   : Gtk_Text_Attributes;
      Full_Height : Boolean)
   is
      procedure Internal
        (Text_Attr   : Gtk_Text_Attributes;
         Full_Height : Gboolean);

      pragma Import
        (C, Internal, "ada_text_attribute_set_bg_full_height");
   begin
      Internal (Text_Attr, Boolean'Pos (Full_Height));
   end Set_Bg_Full_Height;

   -----------------------
   -- Get_BG_Full_Height --
   -----------------------

   function Get_Bg_Full_Height
     (Text_Attr : Gtk_Text_Attributes) return Boolean

   is
      function Internal (Text_Attr : Gtk_Text_Attributes) return Gboolean;
      pragma Import
        (C, Internal, "ada_text_attribute_get_bg_full_height");
   begin
      return Boolean'Val (Internal (Text_Attr));
   end Get_Bg_Full_Height;

   ------------------
   -- Set_Editable --
   ------------------

   procedure Set_Editable
     (Text_Attr : Gtk_Text_Attributes;
      Editable  : Boolean)
   is
      procedure Internal
        (Text_Attr    : Gtk_Text_Attributes;
         Editable     : Gboolean);

      pragma Import (C, Internal, "ada_text_attribute_set_editable");
   begin
      Internal (Text_Attr, Boolean'Pos (Editable));
   end Set_Editable;

   ------------------
   -- Get_Editable --
   ------------------

   function Get_Editable
     (Text_Attr : Gtk_Text_Attributes) return Boolean

   is
      function Internal (Text_Attr : Gtk_Text_Attributes) return Gboolean;
      pragma Import
        (C, Internal, "ada_text_attribute_get_editable");
   begin
      return Boolean'Val (Internal (Text_Attr));
   end Get_Editable;

end Gtk.Text_Attributes;
