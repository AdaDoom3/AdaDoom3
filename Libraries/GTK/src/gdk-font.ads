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

--  <description>
--
--  !!! Important note !!!: this package is now considered as deprecated in
--  GtkAda 2.x. You should use the types and subprograms in the Pango
--  hierarchy, which correctly support internationalization, right-to-left
--  writings, easy resizing of fonts, truetype fonts....
--
--  For backward compatibility, a new subprogram From_Description has been
--  added to this package, which gives access to the more advanced font
--  handling.
--
--
--  This is the base package for handling fonts.
--  GtkAda knows about bitmap and vectorial fonts, and can draw both.
--  The list of fonts available to you depends on what is installed on
--  your system.
--
--  The name of the font is indicated in the standard X11 fashion, namely:
--  (example extracted from the Xlib manual):
--
--  -adobe-courier-bold-o-normal--10-100-75-75-m-60-iso8859-1
--  where:
--    - adobe     : foundry
--    - courier   : font family
--    - bold      : weight (e.g. bold, medium)
--    - o         : slant (e.g. roman, italic, oblique)
--    - normal    : set width (e.g. normal, condensed, narrow, double)
--    - 10        : pixels
--    - 100       : points (in tenths of a point)
--    - 75        : horizontal resolution in dpi
--    - 75        : vertical resolution in dpi
--    - m         : spacing (e.g. monospace or proportional)
--    - 60        : average width (in tenths of a pixel)
--    - iso8859-1 : character set
--
--  Any of the fields can have a '*' instead, so that the system will
--  automatically find a font that matches the rest of the string, and won't
--  care about that specific field.
--
--  An easy way to select a font is by using some external programs,
--  for instance xfontsel, xlsfont, gfontsel, or even the font selection
--  dialog example in the testgtk/ directory of the GtkAda distribution.
--
--  But the easiest way to create a font is to use a Pango_Font_Description.
--  See package Pango.Font for more details about this structure.
--
--  Some of the functions below should be used only for wide-character strings.
--  This is needed for languages with more than 256 characters.
--
--  Wide character values between 0 and 127 are always identical in meaning to
--  the ASCII character codes.
--  An alternative to wide characters is multi-byte characters, which extend
--  normal char strings to cope with larger character sets. As the name
--  suggests, multi-byte characters use a different number of bytes to store
--  different character codes. For example codes 0-127 (i.e. the ASCII codes)
--  often use just one byte of memory, while other codes may use 2, 3 or even
--  4 bytes. Multi-byte characters have the advantage that they can often be
--  used in an application with little change, since strings are still
--  represented as arrays of char values. However multi-byte strings are much
--  easier to manipulate since the character are all of the same size.
--
--  On Unix systems, the external utility 'xfd' can be used to display all
--  the characters in a font.
--
--  </description>
--  <c_version>1.3.6</c_version>
--  <screenshot>font</screenshot>
--  <group>Gdk, the low-level API</group>

with Glib; use Glib;
with Gdk.Types;
with Pango.Font;

package Gdk.Font is

   subtype Gdk_Font is Gdk.Gdk_Font;
   --  A font used to draw text.
   --  This can represent a bitmap font, a scalable (vectorial) font, or
   --  a fontset. A fontset is a list of comma-separated fonts, that permits
   --  GtkAda to obtain the fonts needed for a variety of locales from a
   --  single locale-independent base font name. The single base font name
   --  should name a family of fonts whose members are encoded in the various
   --  charsets needed by the locales of interest.
   --  The algorithm used to select the font is described in the manual page
   --  for XCreateFontSet(3X).

   Null_Font : constant Gdk_Font;

   function Get_Type return Glib.GType;
   --  Return the internal value associated with Gdk_Font.

   procedure Load (Font : out Gdk_Font; Font_Name : String);
   --  Load a new font, given its name.
   --  This is the first step before using a font.
   --  The font is first looked up in the cache, and if it was already
   --  loaded, it is not reloaded again. Thus, it does not harm to call
   --  this function multiple times with the same Font_Name.
   --  Null_Font is returned if the font could not be loaded.
   --
   --  See From_Description below for another way of creating a Gdk_Font.

   procedure Fontset_Load (Font : out Gdk_Font; Fontset_Name : String);
   --  Load a new font set.
   --  Fontset_Name is a comma-separated list of fonts that will be loaded
   --  as part of the fontset.

   function From_Description
     (Font_Desc : Pango.Font.Pango_Font_Description) return Gdk.Font.Gdk_Font;
   --  Create a new Gdk_Font from the given Pango_Font_Description.
   --  This is a convenient function to create fonts from, because
   --  a Pango_Font_Description is a higher level description of a font
   --  attributes.

   procedure Ref (Font : Gdk_Font);
   --  Increment the reference counter for the font.
   --  You should not make any assumption of the initial value of the fonts
   --  returned by Load or Fontset_Load, since these can be extracted from a
   --  cache.

   procedure Unref (Font : Gdk_Font);
   --  Decrement the reference counter for the font.
   --  When this counter reaches 0, the font is deleted from memory.

   function Id (Font : Gdk_Font) return Gint;
   --  Return the X font id for the font.
   --  This Id will only be needed if you want to call directly X11 functions,
   --  you won't need it with GtkAda.

   function Equal (Fonta, Fontb : Gdk_Font) return Boolean;
   --  Compare two fonts or two fontsets for equality.
   --  Two fonts are equal if they have the same font Id.
   --  Two fontsets are equal if the name given to Fontset_Load was the same.

   function Get_Ascent (Font : Gdk_Font) return Gint;
   --  Return the maximal ascent for the font.
   --  This is the logical extent above the baseline for spacing between two
   --  lines.

   function Get_Descent (Font : Gdk_Font) return Gint;
   --  Return the maximal descent for the font.
   --  This is the logical extent below the baseline for spacing between two
   --  lines.

   function String_Width (Font : Gdk_Font; Str : String) return Gint;
   --  Return the width in pixels that Str will occupy if drawn with Font.
   --  The value returned is the distance between the origin of the text and
   --  the position at which the next string should be drawn.

   function String_Width
     (Font : Gdk_Font; Text : Gdk.Types.Gdk_WString) return Gint;
   --  Return the width in pixels that Text will occupy on the screen.
   --  This function should be used with strings that contain Unicode
   --  characters

   function Char_Width (Font : Gdk_Font; Char : Character) return Gint;
   --  Return the width in pixels occupied by a single character on the screen.
   --  The value returned is the distance between Char's origin on the screen
   --  and the origin of the next character in the string.

   function Char_Width
     (Font : Gdk_Font; Char : Gdk.Types.Gdk_WChar) return Gint;
   --  Return the width in pixels occupied by a single wide-character.

   function String_Measure (Font : Gdk_Font; Str : String) return Gint;
   --  Determine the distance from the origin to the rightmost portion of Str.
   --  This is not the correct value for determining the origin of the next
   --  portion when drawing text in multiple pieces.
   --  See String_Width instead.

   function Char_Measure (Font : Gdk_Font; Char : Character) return Gint;
   --  Return the width in pixels of Char.
   --  As opposed to Char_Width, the value returned is not the distance at
   --  which the next character should be drawn.
   --  This is also called the right bearing of the character.

   function String_Height (Font : Gdk_Font; Str : String) return Gint;
   --  Return the height in pixels of the string.
   --  This is the total height, and you can not easily tell how this height
   --  is split around the baseline.

   function Char_Height (Font : Gdk_Font; Char : Character) return Gint;
   --  Return the total height in pixels of a single character.

   procedure String_Extents
     (Font     : Gdk.Font.Gdk_Font;
      Str      : String;
      Lbearing : out Gint;
      Rbearing : out Gint;
      Width    : out Gint;
      Ascent   : out Gint;
      Descent  : out Gint);
   --  Return the metrics for a given text.
   --  See the picture for more explanations on all the fields.
   --  Lbearing : Origin to left edge of character.
   --  Rbearing : Origin to right edge of character.
   --  Width    : Advance to next character's origin.
   --  Ascent   : Baseline to top edge of character.
   --  Descent  : Baseline to bottom edge of character.

   procedure String_Extents
     (Font        : Gdk_Font;
      Text        : Gdk.Types.Gdk_WString;
      Lbearing    : out Gint;
      Rbearing    : out Gint;
      Width       : out Gint;
      Ascent      : out Gint;
      Descent     : out Gint);
   --  Return all the metrics for a given wide-character string.
   --  See the picture for more explanations on the returned values.

private
   Null_Font : constant Gdk_Font := null;
   pragma Import (C, Get_Type, "gdk_font_get_type");
   pragma Import (C, Char_Height, "gdk_char_height");
   pragma Import (C, Id, "gdk_font_id");
   pragma Import (C, Ref, "gdk_font_ref");
   pragma Import (C, Unref, "gdk_font_unref");
end Gdk.Font;
