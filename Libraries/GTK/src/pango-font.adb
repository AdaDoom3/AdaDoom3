-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                  Copyright (C) 2001-2013, AdaCore                 --
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

with Interfaces.C.Strings;
with Glib.Object; use Glib.Object;

package body Pango.Font is

   subtype String7 is String (1 .. 7);
   Style_Map : constant array (Enums.Style) of String7 :=
     (Enums.Pango_Style_Normal  => "       ",
      Enums.Pango_Style_Oblique => "Oblique",
      Enums.Pango_Style_Italic  => "Italic ");

   subtype String10 is String (1 .. 10);
   Variant_Map : constant array (Enums.Variant) of String10 :=
     (Enums.Pango_Variant_Normal     => "          ",
      Enums.Pango_Variant_Small_Caps => "Small-Caps");

   subtype String15 is String (1 .. 15);
   Stretch_Map : constant array (Enums.Stretch) of String15 :=
     (Enums.Pango_Stretch_Ultra_Condensed => "Ultra-Condensed",
      Enums.Pango_Stretch_Extra_Condensed => "Extra-Condensed",
      Enums.Pango_Stretch_Condensed       => "Condensed      ",
      Enums.Pango_Stretch_Semi_Condensed  => "Semi-Condensed ",
      Enums.Pango_Stretch_Normal          => "               ",
      Enums.Pango_Stretch_Semi_Expanded   => "Semi-Expanded  ",
      Enums.Pango_Stretch_Expanded        => "Expanded       ",
      Enums.Pango_Stretch_Extra_Expanded  => "Extra-Expanded ",
      Enums.Pango_Stretch_Ultra_Expanded  => "Ultra-Expanded ");

   --  Some of the values are not directly supported by pango.
   --  ??? See fonts.c in pango

   subtype String9 is String (1 .. 9);
   Weight_Map : constant array (Enums.Weight) of String9 :=
     (Enums.Pango_Weight_Ultralight  => "Light    ",
      Enums.Pango_Weight_Light       => "Light    ",
      Enums.Pango_Weight_Normal      => "         ",
      Enums.Pango_Weight_Medium      => "Medium   ",
      Enums.Pango_Weight_Semi_Bold   => "Semi-Bold",
      Enums.Pango_Weight_Bold        => "Bold     ",
      Enums.Pango_Weight_Ultrabold   => "Bold     ",
      Enums.Pango_Weight_Heavy       => "Heavy    ");

   procedure g_free (c_str : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, g_free, "g_free");

   -----------
   -- Equal --
   -----------

   function Equal
     (Desc1 : Pango_Font_Description;
      Desc2 : Pango_Font_Description) return Boolean
   is
      function Internal
        (Desc1 : Pango_Font_Description;
         Desc2 : Pango_Font_Description) return Gboolean;
      pragma Import (C, Internal, "pango_font_description_equal");

   begin
      return Boolean'Val (Internal (Desc1, Desc2));
   end Equal;

   ----------
   -- Free --
   ----------

   procedure Free (Desc : in out Pango_Font_Description) is
      procedure Internal (Desc : Pango_Font_Description);
      pragma Import (C, Internal, "pango_font_description_free");

   begin
      Internal (Desc);
      Desc := null;
   end Free;

   -----------------
   -- From_String --
   -----------------

   function From_String (Str : String) return Pango_Font_Description is
      function Internal (Str : String) return Pango_Font_Description;
      pragma Import (C, Internal, "pango_font_description_from_string");

   begin
      return Internal (Str & ASCII.NUL);
   end From_String;

   ------------------------
   -- To_Font_Decription --
   ------------------------

   function To_Font_Description
     (Family_Name : String := "";
      Style       : Enums.Style := Enums.Pango_Style_Normal;
      Variant     : Enums.Variant := Enums.Pango_Variant_Normal;
      Weight      : Enums.Weight := Enums.Pango_Weight_Normal;
      Stretch     : Enums.Stretch := Enums.Pango_Stretch_Normal;
      Size        : Gint := 0) return Pango_Font_Description
   is
      Result : constant Pango_Font_Description :=
        From_String (Family_Name & " " &
                     Style_Map (Style) & " " &
                     Variant_Map (Variant) &
                     Weight_Map (Weight) & " " &
                     Stretch_Map (Stretch) & Gint'Image (Size));
   begin
      return Result;
   end To_Font_Description;

   ---------------
   -- To_String --
   ---------------

   function To_String (Desc : Pango_Font_Description) return String is
      function Internal
        (Desc : Pango_Font_Description) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "pango_font_description_to_string");

      C_Result : constant Interfaces.C.Strings.chars_ptr := Internal (Desc);
      Result   : constant String := Interfaces.C.Strings.Value (C_Result);

   begin
      g_free (C_Result);
      return Result;
   end To_String;

   -----------------
   -- To_Filename --
   -----------------

   function To_Filename (Desc : Pango_Font_Description) return String is
      function Internal
        (Desc : Pango_Font_Description) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "pango_font_description_to_filename");

      C_Result : constant Interfaces.C.Strings.chars_ptr := Internal (Desc);
      Result   : constant String := Interfaces.C.Strings.Value (C_Result);

   begin
      g_free (C_Result);
      return Result;
   end To_Filename;

   ----------------
   -- Get_Family --
   ----------------

   function Get_Family (Desc : Pango_Font_Description) return String is
      function Internal
        (Desc : Pango_Font_Description) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "pango_font_description_get_family");

   begin
      return Interfaces.C.Strings.Value (Internal (Desc));
   end Get_Family;

   ----------------
   -- Set_Family --
   ----------------

   procedure Set_Family
     (Desc : Pango_Font_Description;
      Name : String)
   is
      procedure Internal
        (Desc : Pango_Font_Description;
         Name : String);
      pragma Import (C, Internal, "pango_font_description_set_family");

   begin
      Internal (Desc, Name & ASCII.NUL);
   end Set_Family;

   -----------------
   -- From_String --
   -----------------

   function From_String (Language : String) return Pango_Language is
      function Internal (Language : String) return Pango_Language;
      pragma Import (C, Internal, "pango_language_from_string");
   begin
      return Internal (Language & ASCII.NUL);
   end From_String;

   -----------------
   -- Get_Metrics --
   -----------------

   function Get_Metrics
     (Font : access Pango_Font_Record'Class;
      Language : Pango_Language := null) return Pango_Font_Metrics
   is
      function Internal (Font : System.Address; Lang : Pango_Language)
         return Pango_Font_Metrics;
      pragma Import (C, Internal, "pango_font_get_metrics");
   begin
      return Internal (Get_Object (Font), Language);
   end Get_Metrics;

   ----------------
   -- To_Address --
   ----------------

   function To_Address
     (F : Pango_Font_Description; Add : System.Address)
      return System.Address
   is
      pragma Unreferenced (Add);
   begin
      return F.all'Address;
   end To_Address;

end Pango.Font;
