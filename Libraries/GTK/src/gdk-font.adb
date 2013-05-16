-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2002 ACT-Europe                 --
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

with Pango.Font;        use Pango.Font;
with Interfaces.C;

package body Gdk.Font is

   package C renames Interfaces.C;

   -----------
   -- Equal --
   -----------

   function Equal (Fonta, Fontb : Gdk_Font) return Boolean is
      function Internal (Fonta, Fontb : Gdk_Font) return Gint;
      pragma Import (C, Internal, "gdk_font_equal");

   begin
      return Boolean'Val (Internal (Fonta, Fontb));
   end Equal;

   ------------------
   -- Char_Measure --
   ------------------

   function Char_Measure (Font : Gdk_Font; Char : Character) return Gint is
      function Internal (Font : Gdk_Font; Char : C.char) return Gint;
      pragma Import (C, Internal, "gdk_char_measure");

   begin
      return Internal (Font, C.To_C (Char));
   end Char_Measure;

   ----------------
   -- Char_Width --
   ----------------

   function Char_Width (Font : Gdk_Font; Char : Character) return Gint is
      function Internal (Font : Gdk_Font; Char : C.char) return Gint;
      pragma Import (C, Internal, "gdk_char_width");

   begin
      return Internal (Font, C.To_C (Char));
   end Char_Width;

   ----------------
   -- Char_Width --
   ----------------

   function Char_Width
     (Font : Gdk_Font; Char : Gdk.Types.Gdk_WChar) return Gint
   is
      function Internal (Font : Gdk_Font; Char : C.wchar_t) return Gint;
      pragma Import (C, Internal, "gdk_char_width_wc");

   begin
      return Internal (Font, C.To_C (Char));
   end Char_Width;

   ------------------
   -- Fontset_Load --
   ------------------

   procedure Fontset_Load (Font : out Gdk_Font; Fontset_Name : String) is
      function Internal (Fontset_Name : String) return Gdk_Font;
      pragma Import (C, Internal, "gdk_fontset_load");

   begin
      Font := Internal (Fontset_Name & ASCII.NUL);
   end Fontset_Load;

   ----------------------
   -- From_Description --
   ----------------------

   function From_Description
     (Font_Desc : Pango.Font.Pango_Font_Description) return Gdk_Font
   is
      function Internal (Desc : Pango_Font_Description) return Gdk_Font;
      pragma Import (C, Internal, "gdk_font_from_description");
   begin
      return Internal (Font_Desc);
   end From_Description;

   ----------------
   -- Get_Ascent --
   ----------------

   function Get_Ascent (Font : Gdk_Font) return Gint is
      function Internal (Font : Gdk_Font) return Gint;
      pragma Import (C, Internal, "ada_gdk_font_get_ascent");

   begin
      if Font = null then
         raise Constraint_Error;
      else
         return Internal (Font);
      end if;
   end Get_Ascent;

   -----------------
   -- Get_Descent --
   -----------------

   function Get_Descent (Font : Gdk_Font) return Gint is
      function Internal (Font : Gdk_Font) return Gint;
      pragma Import (C, Internal, "ada_gdk_font_get_descent");
   begin
      if Font = null then
         raise Constraint_Error;
      else
         return Internal (Font);
      end if;
   end Get_Descent;

   ----------
   -- Load --
   ----------

   procedure Load (Font : out Gdk_Font; Font_Name : String) is
      function Internal (Font_Name : String) return Gdk_Font;
      pragma Import (C, Internal, "gdk_font_load");

   begin
      Font := Internal (Font_Name & ASCII.NUL);
   end Load;

   --------------------
   -- String_Extents --
   --------------------

   procedure String_Extents
     (Font     : Gdk.Font.Gdk_Font;
      Str      : String;
      Lbearing : out Gint;
      Rbearing : out Gint;
      Width    : out Gint;
      Ascent   : out Gint;
      Descent  : out Gint)
   is
      procedure Internal
        (Font     : Gdk.Font.Gdk_Font;
         Str      : String;
         Length   : Gint;
         Lbearing : out Gint;
         Rbearing : out Gint;
         Width    : out Gint;
         Ascent   : out Gint;
         Descent  : out Gint);
      pragma Import (C, Internal, "gdk_text_extents");

   begin
      Internal
        (Font, Str, Str'Length, Lbearing, Rbearing, Width, Ascent, Descent);
   end String_Extents;

   procedure String_Extents
     (Font        : Gdk_Font;
      Text        : Gdk.Types.Gdk_WString;
      Lbearing    : out Gint;
      Rbearing    : out Gint;
      Width       : out Gint;
      Ascent      : out Gint;
      Descent     : out Gint)
   is
      procedure Internal
        (Font        : Gdk_Font;
         Text        : Gdk.Types.Gdk_WString;
         Text_Length : Gint;
         Lbearing    : out Gint;
         Rbearing    : out Gint;
         Width       : out Gint;
         Ascent      : out Gint;
         Descent     : out Gint);
      pragma Import (C, Internal, "gdk_text_extents_wc");

   begin
      Internal (Font, Text, Text'Length, Lbearing, Rbearing,
                Width, Ascent, Descent);
   end String_Extents;

   -------------------
   -- String_Height --
   -------------------

   function String_Height (Font : Gdk_Font; Str : String) return Gint is
      function Internal
        (Font        : Gdk_Font;
         Text        : String;
         Text_Length : Gint) return Gint;
      pragma Import (C, Internal, "gdk_text_height");

   begin
      return Internal (Font, Str, Str'Length);
   end String_Height;

   --------------------
   -- String_Measure --
   --------------------

   function String_Measure (Font : Gdk_Font; Str : String) return Gint is
      function Internal
        (Font        : Gdk_Font;
         Text        : String;
         Text_Length : Gint) return Gint;
      pragma Import (C, Internal, "gdk_text_measure");

   begin
      return Internal (Font, Str, Str'Length);
   end String_Measure;

   ------------------
   -- String_Width --
   ------------------

   function String_Width (Font : Gdk_Font; Str : String) return Gint is
      function Internal
        (Font        : Gdk_Font;
         Text        : String;
         Text_Length : Gint) return Gint;
      pragma Import (C, Internal, "gdk_text_width");

   begin
      return Internal (Font, Str, Str'Length);
   end String_Width;

   function String_Width
     (Font : Gdk_Font; Text : Gdk.Types.Gdk_WString) return Gint
   is
      function Internal
        (Font        : Gdk_Font;
         Text        : Gdk.Types.Gdk_WString;
         Text_Length : Gint) return Gint;
      pragma Import (C, Internal, "gdk_text_width_wc");

   begin
      return Internal (Font, Text, Text'Length);
   end String_Width;

end Gdk.Font;
