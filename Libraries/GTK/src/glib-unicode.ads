-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                   Copyright (C) 2003 ACT-Europe                   --
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
--  This package provides functions for handling of unicode characters and
--  utf8 strings. See also Glib.Convert.
--
--  </description>
--  <c_version>2.2.1</c_version>
--  <group>Glib, the general-purpose library</group>

with Interfaces.C.Strings;

package Glib.Unicode is
   pragma Preelaborate;

   package ICS renames Interfaces.C.Strings;

   procedure UTF8_Validate
     (Str         : UTF8_String;
      Valid       : out Boolean;
      Invalid_Pos : out Natural);
   --  Validate a UTF8 string.
   --  Set Valid to True if valid, set Invalid_Pos to first invalid byte.

   -----------------------
   -- Character classes --
   -----------------------

   type G_Unicode_Type is
     (Unicode_Control,
      Unicode_Format,
      Unicode_Unassigned,
      Unicode_Private_Use,
      Unicode_Surrogate,
      Unicode_Lowercase_Letter,
      Unicode_Modifier_Letter,
      Unicode_Other_Letter,
      Unicode_Titlecase_Letter,
      Unicode_Uppercase_Letter,
      Unicode_Combining_Mark,
      Unicode_Enclosing_Mark,
      Unicode_Non_Spacing_Mark,
      Unicode_Decimal_Number,
      Unicode_Letter_Number,
      Unicode_Other_Number,
      Unicode_Connect_Punctuation,
      Unicode_Dash_Punctuation,
      Unicode_Close_Punctuation,
      Unicode_Final_Punctuation,
      Unicode_Initial_Punctuation,
      Unicode_Other_Punctuation,
      Unicode_Open_Punctuation,
      Unicode_Currency_Symbol,
      Unicode_Modifier_Symbol,
      Unicode_Math_Symbol,
      Unicode_Other_Symbol,
      Unicode_Line_Separator,
      Unicode_Paragraph_Separator,
      Unicode_Space_Separator);
   --  The possible character classifications.
   --  See http://www.unicode.org/Public/UNIDATA/UCD.html

   function Is_Space (Char : Gunichar) return Boolean;
   --  True if Char is a space character

   function Is_Alnum (Char : Gunichar) return Boolean;
   --  True if Char is an alphabetical or numerical character

   function Is_Alpha (Char : Gunichar) return Boolean;
   --  True if Char is an alphabetical character

   function Is_Digit (Char : Gunichar) return Boolean;
   --  True if Char is a digit

   function Is_Lower (Char : Gunichar) return Boolean;
   --  True if Char is a lower-case character

   function Is_Upper (Char : Gunichar) return Boolean;
   --  True if Char is an upper-case character

   function Is_Punct (Char : Gunichar) return Boolean;
   --  True if Char is a punctuation character

   function Unichar_Type (Char : Gunichar) return G_Unicode_Type;
   --  Return the unicode character type of a given character

   -------------------
   -- Case handling --
   -------------------

   function To_Lower (Char : Gunichar) return Gunichar;
   --  Convert Char to lower cases

   function To_Upper (Char : Gunichar) return Gunichar;
   --  Convert Char to upper cases

   function UTF8_Strdown
     (Str : ICS.chars_ptr; Len : Integer) return ICS.chars_ptr;
   pragma Import (C, UTF8_Strdown, "g_utf8_strdown");
   --  Convert all characters in Str to lowercase. The resulting string
   --  must be freed by the user. It can have a different length than
   --  Str.

   function UTF8_Strdown (Str : UTF8_String) return UTF8_String;
   --  Convert Str to lower cases

   function UTF8_Strup
     (Str : ICS.chars_ptr; Len : Integer) return ICS.chars_ptr;
   pragma Import (C, UTF8_Strup, "g_utf8_strup");
   --  Convert all characters in Str to uppercase. The resulting string is
   --  newly allocated, and can have a different length than Str (for
   --  instance, the german ess-zet is converted to SS).
   --  The returned string must be freed by the caller.

   function UTF8_Strup (Str : UTF8_String) return UTF8_String;
   --  Convert Str to upper cases

   ---------------------------
   --  Manipulating strings --
   ---------------------------

   function UTF8_Strlen
     (Str : ICS.chars_ptr; Max : Integer := -1) return Glong;
   pragma Import (C, UTF8_Strlen, "g_utf8_strlen");
   --  Return the length of a utf8-encoded string.
   --  Max is the maximal number of bytes to examine. If it is negative, then
   --  the string is assumed to be nul-terminated.

   function UTF8_Strlen (Str : UTF8_String) return Glong;
   --  Return the number of characters in Str

   function UTF8_Find_Next_Char
     (Str     : ICS.chars_ptr;
      Str_End : ICS.chars_ptr := ICS.Null_Ptr) return ICS.chars_ptr;
   pragma Import (C, UTF8_Find_Next_Char, "g_utf8_find_next_char");
   --  Find the start of the next UTF8 character after Str.
   --  Str_End points to the end of the string. If Null_Ptr, the string must
   --  be nul-terminated

   function UTF8_Find_Next_Char
     (Str : UTF8_String; Index : Natural) return Natural;
   pragma Inline (UTF8_Find_Next_Char);
   --  Find the start of the next UTF8 character after the Index-th byte.
   --  Index doesn't need to be on the start of a character.
   --  Index is set to a value greater than Str'Last if there is no more
   --  character.

   function UTF8_Next_Char
     (Str : UTF8_String; Index : Natural) return Natural;
   pragma Inline (UTF8_Next_Char);
   --  Find the start of the next UTF8 character after the Index-th byte.
   --  Index has to be on the start of a character.
   --  Index is set to a value greater than Str'Last if there is no more
   --  character.

   function UTF8_Find_Prev_Char
     (Str_Start : ICS.chars_ptr; Str : ICS.chars_ptr) return ICS.chars_ptr;
   pragma Import (C, UTF8_Find_Prev_Char, "g_utf8_find_prev_char");
   --  Find the start of the previous UTF8 character before Str.
   --  Str_Start is a pointer to the beginning of the string.
   --  Null_Ptr is returned if there is no previous character

   function UTF8_Find_Prev_Char
     (Str : UTF8_String; Index : Natural) return Natural;
   --  Find the start of the previous UTF8 character after the Index-th byte.
   --  Index doesn't need to be on the start of a character.
   --  Index is set to a value smaller than Str'First if there is no
   --  previous character.

   -----------------
   -- Conversions --
   -----------------

   function Unichar_To_UTF8
     (C : Gunichar; Buffer : ICS.chars_ptr := ICS.Null_Ptr) return Natural;
   pragma Import (C, Unichar_To_UTF8, "g_unichar_to_utf8");
   --  Encode C into Buffer, which must have at least 6 bytes free.
   --  Return the number of bytes written in Buffer.
   --  If Buffer is Null_Ptr, then the only effect is to compute the number of
   --  bytes to encode C.

   procedure Unichar_To_UTF8
     (C      : Gunichar;
      Buffer : out UTF8_String;
      Last   : out Natural);
   --  Encode C into Buffer. Buffer must have at least 6 bytes free.
   --  Return the index of the last byte written in Buffer.

   function UTF8_Get_Char (Str : UTF8_String) return Gunichar;
   --  Converts a sequence of bytes encoded as UTF8 to a unicode character.
   --  If Str doesn't point to a valid UTF8 encoded character, the result is
   --  undefined.

   function UTF8_Get_Char_Validated (Str : UTF8_String) return Gunichar;
   --  Same as above. However, if the sequence if an incomplete start of a
   --  possibly valid character, it returns -2. If the sequence is invalid,
   --  returns -1.

   --  ??? Gunichar is unsigned, how can we test -2 or -1 ?

private
   pragma Convention (C, G_Unicode_Type);
   pragma Import (C, To_Upper, "g_unichar_toupper");
   pragma Import (C, To_Lower, "g_unichar_tolower");
   pragma Import (C, Unichar_Type, "g_unichar_type");
end Glib.Unicode;
