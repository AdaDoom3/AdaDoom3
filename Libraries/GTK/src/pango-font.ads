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

--  <description>
--
--  This package provides high-level, system-independent handling of fonts. It
--  supercedes the old Gdk.Font package, which should no longer be used.
--
--  Fonts are defined through several attributes, like their family, weight,
--  size, style, ...
--
--  The Pango_Font_Description objects created by this package can either be
--  used directly to draw text through Pango.Layout.Pango_Layout objects (and
--  the associated Gdk.Drawable.Draw_Layout procedure), or by converting them
--  to a Gdk_Font. The first method is the preferred one, and provides
--  high-level handling of multi-line texts or tabs, when you have to handle
--  this yourself in the second case.
--
--  </description>
--  <group>Pango, font handling</group>

with Glib; use Glib;
with Glib.Object;
with Glib.Generic_Properties; use Glib.Generic_Properties;
pragma Elaborate_All (Glib.Generic_Properties);
with Pango.Enums;
with System;

package Pango.Font is

   type Pango_Font_Description is new Glib.C_Proxy;

   function Get_Type return Glib.GType;
   --  Return the internal gtk+ type associated with font descriptions.

   function Copy (Desc : Pango_Font_Description) return Pango_Font_Description;
   --  Return a newly allocated font description.
   --  This Pango_Font_Description needs to be free'ed after use.

   function Equal
     (Desc1 : Pango_Font_Description;
      Desc2 : Pango_Font_Description) return Boolean;
   --  Return True if the two font descriptions are identical.
   --  Note that two font description may result in identical fonts being
   --  loaded, but still compare False.

   procedure Free (Desc : in out Pango_Font_Description);
   --  Deallocate the given font description.

   function From_String (Str : String) return Pango_Font_Description;
   --  Create a new font description from the given string representation
   --  of the given form: "[FAMILY-LIST] [STYLE-OPTIONS] [SIZE]". Any one
   --  of the options may be omitted.
   --    - FAMILY-LIST is a comma separated list (spaces are not allowed)
   --      of font families optionally terminated by a comma. If absent,
   --      the font family of the font that will be used is unspecified.
   --    - STYLE_OPTIONS is a whitespace separated list of words where each
   --      word describes either style, variant, weight, or stretch. Any
   --      unspecified style option is defaulted to "Normal", which
   --      respectively corresponds to Pango_Style_Normal, Pango_Weight_Normal,
   --      Pango_Variant_Normal, and Pango_Stretch_Normal.
   --    - SIZE is a decimal number describing the size of the font in points.
   --      If unspecified, a size of 0 will be used.

   function To_Font_Description
     (Family_Name : String := "";
      Style       : Pango.Enums.Style := Pango.Enums.Pango_Style_Normal;
      Variant     : Pango.Enums.Variant := Pango.Enums.Pango_Variant_Normal;
      Weight      : Pango.Enums.Weight := Pango.Enums.Pango_Weight_Normal;
      Stretch     : Pango.Enums.Stretch := Pango.Enums.Pango_Stretch_Normal;
      Size        : Gint := 0) return Pango_Font_Description;
   --  Create a new font decription from the given parameters.

   function To_String (Desc : Pango_Font_Description) return String;
   --  Create a string representation of a font description. The format
   --  of the string produced follows the syntax used by From_String.
   --  The family-list in the string description will have a terminating
   --  comma only if the last word of the list is a valid style option.

   function To_Filename (Desc : Pango_Font_Description) return String;
   --  Create a filename representation of a font description. The filename
   --  is identical to the result from calling To_String, but with underscores
   --  instead of characters that are untypical in filenames, and in lower
   --  case only.

   function Get_Family (Desc : Pango_Font_Description) return String;
   --  Return the Family_Name of the given Pango_Font_Description.

   procedure Set_Family (Desc : Pango_Font_Description; Name : String);
   --  Set the Family_Name of the given Pango_Font_Description.

   function Get_Style (Desc : Pango_Font_Description) return Pango.Enums.Style;
   --  Return the Style of the given Pango_Font_Description.

   procedure Set_Style
     (Desc : Pango_Font_Description; Style : Pango.Enums.Style);
   --  Set the Style of the given Pango_Font_Description.

   function Get_Variant
     (Desc : Pango_Font_Description) return Pango.Enums.Variant;
   --  Return the Variant of the given Pango_Font_Description.

   procedure Set_Variant
     (Desc : Pango_Font_Description; Variant : Pango.Enums.Variant);
   --  Set the Variant of the given Pango_Font_Description.

   function Get_Weight
     (Desc : Pango_Font_Description) return Pango.Enums.Weight;
   --  Return the Weight of the given Pango_Font_Description.

   procedure Set_Weight
     (Desc : Pango_Font_Description; Weight : Pango.Enums.Weight);
   --  Set the Weight of the given Pango_Font_Description.

   function Get_Stretch
     (Desc : Pango_Font_Description) return Pango.Enums.Stretch;
   --  Return the Stretch of the given Pango_Font_Description.

   procedure Set_Stretch
     (Desc : Pango_Font_Description; Stretch : Pango.Enums.Stretch);
   --  Set the Stretch of the given Pango_Font_Description.

   function Get_Size (Desc : Pango_Font_Description) return Gint;
   --  Return value: the size for the font description in pango units.
   --  (PANGO_SCALE pango units equals one point). Returns 0 if Desc hasn't
   --  been initialized.

   procedure Set_Size (Desc : Pango_Font_Description; Size : Gint);
   --  Set the size for the font description in pango units.  (PANGO_SCALE
   --  pango units equals one point)

   ---------------
   -- Languages --
   ---------------
   --  The following section provides types and subprograms to identify a
   --  specific script and language inside a font (Not all characters of a font
   --  are used for all languages)

   type Pango_Language is new Glib.C_Proxy;

   function Pango_Language_Get_Type return Glib.GType;
   --  Return the internal value used to identify a Pango_Language

   function From_String (Language : String) return Pango_Language;
   --  Take a RFC-3066 format language tag as a string and convert it to a
   --  Pango_Language pointer that can be efficiently copied (copy the pointer)
   --  and compared with other language tags (compare the pointer). Language is
   --  something like "fr" (french), "ar" (arabic), "en" (english), "ru"
   --  (russian), ...
   --
   --  This function first canonicalizes the string by converting it to
   --  lowercase, mapping '_' to '-', and stripping all characters other than
   --  letters and '-'.
   --
   --  The returned value need not be freed, it is stored internally by gtk+ in
   --  a hash-table.

   -------------
   -- Metrics --
   -------------
   --  The following subprograms can be used to retrieve the metrics associated
   --  with the font. Note that such metrics might depend on the specific
   --  script/language in use.

   type Pango_Font_Metrics is new Glib.C_Proxy;

   type Pango_Font_Record is new Glib.Object.GObject_Record with null record;
   type Pango_Font is access all Pango_Font_Record'Class;
   --  Created through Pango.Context.Load_Font

   function Get_Metrics
     (Font : access Pango_Font_Record'Class;
      Language : Pango_Language := null) return Pango_Font_Metrics;
   --  Gets overall metric information for a font. Since the metrics may be
   --  substantially different for different scripts, a language tag can be
   --  provided to indicate that the metrics should be retrieved that
   --  correspond to the script(s) used by that language.
   --
   --  The returned value must be Unref'ed by the caller.
   --
   --  Language determines which script to get the metrics for, or null to
   --  indicate the metrics for the entire font.

   procedure Ref (Metrics : Pango_Font_Metrics);
   procedure Unref (Metrics : Pango_Font_Metrics);
   --  Ref or unref Metrics When the reference counter reaches 0, the memory is
   --  deallocated.

   function Get_Ascent (Metrics : Pango_Font_Metrics) return Gint;
   --  Gets the ascent from a font metrics structure. The ascent is the
   --  distance from the baseline to the logical top of a line of text. (The
   --  logical top may be above or below the top of the actual drawn ink. It is
   --  necessary to lay out the text to figure where the ink will be).
   --
   --  The returned value is expressed in pango units, and must be divided by
   --  Pango_Scale to get the value in pixels.

   function Get_Descent (Metrics : Pango_Font_Metrics) return Gint;
   --  Gets the descent from a font metrics structure. The descent is the
   --  distance from the baseline to the logical bottom of a line of text. (The
   --  logical bottom may be above or below the bottom of the actual drawn
   --  ink. It is necessary to lay out the text to figure where the ink will
   --  be.)
   --
   --  The returned value is expressed in pango units, and must be divided by
   --  Pango_Scale to get the value in pixels.

   function Get_Approximate_Char_Width (Metrics : Pango_Font_Metrics)
      return Gint;
   --  Gets the approximate character width for a font metrics structure.  This
   --  is merely a representative value useful, for example, for determining
   --  the initial size for a window. Actual characters in text will be wider
   --  and narrower than this.
   --
   --  The returned value is expressed in pango units, and must be divided by
   --  Pango_Scale to get the value in pixels.

   function Get_Approximate_Digit_Width (Metrics : Pango_Font_Metrics)
      return Gint;
   --  Gets the approximate digit width for a font metrics structure.  This is
   --  merely a representative value useful, for example, for determining the
   --  initial size for a window. Actual digits in text can be wider and
   --  narrower than this, though this value is generally somewhat more
   --  accurate than the result of Get_Approximate_Char_Width.
   --
   --  The returned value is expressed in pango units, and must be divided by
   --  Pango_Scale to get the value in pixels.

   function Font_Metrics_Get_Type return Glib.GType;
   --  Return the internal value associated with a Pango_Font_Metrics

   ----------------
   -- Properties --
   ----------------
   --  See the package Glib.Properties for more information on how to
   --  use properties

   pragma Import (C, Get_Type, "pango_font_description_get_type");

   function To_Address
     (F : Pango_Font_Description; Add : System.Address) return System.Address;

   package Desc_Properties is new Generic_Internal_Boxed_Property
     (Pango_Font_Description, Get_Type, To_Address);

   type Property_Font_Description is new Desc_Properties.Property;

private
   pragma Import (C, Copy, "pango_font_description_copy");
   pragma Import (C, Get_Size, "pango_font_description_get_size");
   pragma Import (C, Set_Size, "pango_font_description_set_size");
   pragma Import (C, Pango_Language_Get_Type, "pango_language_get_type");
   pragma Import
     (C, Font_Metrics_Get_Type, "pango_font_metrics_get_type");
   pragma Import (C, Ref, "pango_font_metrics_ref");
   pragma Import (C, Unref, "pango_font_metrics_unref");
   pragma Import (C, Get_Ascent, "pango_font_metrics_get_ascent");
   pragma Import (C, Get_Descent, "pango_font_metrics_get_descent");
   pragma Import (C, Get_Approximate_Char_Width,
                  "pango_font_metrics_get_approximate_char_width");
   pragma Import (C, Get_Approximate_Digit_Width,
                  "pango_font_metrics_get_approximate_digit_width");

   pragma Import (C, Get_Style, "pango_font_description_get_style");
   pragma Import (C, Set_Style, "pango_font_description_set_style");
   pragma Import (C, Get_Variant, "pango_font_description_get_variant");
   pragma Import (C, Set_Variant, "pango_font_description_set_variant");
   pragma Import (C, Get_Weight, "pango_font_description_get_weight");
   pragma Import (C, Set_Weight, "pango_font_description_set_weight");
   pragma Import (C, Get_Stretch, "pango_font_description_get_stretch");
   pragma Import (C, Set_Stretch, "pango_font_description_set_stretch");
end Pango.Font;

--  Missing:
--  pango_language_matches
