-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                Copyright (C) 2001-2003 ACT-Europe                 --
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
--  This package provides definitions for string conversions and i18n.
--  See also Glib.Unicode.
--
--  </description>
--  <c_version>1.3.11</c_version>
--  <group>Glib, the general-purpose library</group>

with Glib.Error; use Glib.Error;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package Glib.Convert is
   pragma Preelaborate;

   --  Convert_Error domain for GErrors:

   No_Conversion     : constant := 0;
   Illegal_Sequence  : constant := 1;
   Failed            : constant := 2;
   Partial_Input     : constant := 3;
   Bad_URI           : constant := 4;
   Not_Absolute_Path : constant := 5;

   function Convert_Error_Domain return GQuark;
   --  Return the error domain associated with Glib.Convert.

   procedure Convert
     (Str           : String;
      To_Codeset    : String;
      From_Codeset  : String;
      Bytes_Read    : out Natural;
      Bytes_Written : out Natural;
      Error         : GError_Access := null;
      Result        : out String);
   --  Convert a string from one character set to another.
   --
   --  Str:           String to convert
   --  Result:        String converted, if no error.
   --  To_Codeset:    Name of character set into which to convert Str
   --  From_Codeset:  Character set of Str.
   --  Bytes_Read:    Number of bytes in the input string that were
   --                 successfully converted.
   --                 Even if the conversion was successful, this may be
   --                 less than Len if there were partial characters
   --                 at the end of the input. If the error
   --                 Illegal_Sequence occurs, the value
   --                 stored will the byte offset after the last valid
   --                 input sequence.
   --  Bytes_Written: Number of bytes stored in the output buffer.
   --  Error:         Location to store the error occuring, ignored if null.
   --                 Any of the errors in Convert_Error_Domain may occur.

   function Convert
     (Str           : String;
      To_Codeset    : String;
      From_Codeset  : String;
      Error         : GError_Access := null) return String;
   --  Same as above, but return a String directly.

   procedure Convert
     (Str           : chars_ptr;
      Len           : Natural;
      To_Codeset    : String;
      From_Codeset  : String;
      Bytes_Read    : out Natural;
      Bytes_Written : out Natural;
      Error         : GError_Access := null;
      Result        : out String);
   --  Same as Convert procedure, but take a C string as input.

   function Convert
     (Str           : String;
      To_Codeset    : String;
      From_Codeset  : String;
      Bytes_Read    : access Natural;
      Bytes_Written : access Natural;
      Error         : GError_Access := null) return chars_ptr;
   --  Same as Convert procedure, but return the result as a C string.

   function Convert
     (Str           : chars_ptr;
      Len           : Natural;
      To_Codeset    : String;
      From_Codeset  : String;
      Bytes_Read    : access Natural;
      Bytes_Written : access Natural;
      Error         : GError_Access := null) return chars_ptr;
   --  Same as Convert procedure, but take and return the result as a C string.

   procedure Locale_To_UTF8
     (OS_String     : String;
      Bytes_Read    : out Natural;
      Bytes_Written : out Natural;
      Error         : GError_Access := null;
      Result        : out String);
   --  Convert a string which is in the encoding used for strings by
   --  the C runtime (usually the same as that used by the operating
   --  system) in the current locale into a UTF-8 string.
   --
   --  OS_String:     A string in the encoding of the current locale
   --  Bytes_Read:    Number of bytes in the input string that were
   --                 successfully converted.
   --                 Even if the conversion was successful, this may be
   --                 less than Len if there were partial characters
   --                 at the end of the input. If the error
   --                 Illegal_Sequence occurs, the value
   --                 stored will the byte offset after the last valid
   --                 input sequence.
   --  Bytes_Written: Number of bytes stored in Result.
   --  Error:         Location to store the error occuring, ignored if null.
   --                 Any of the errors in Convert_Error_Domain may occur.

   function Locale_To_UTF8
     (OS_String     : String;
      Bytes_Read    : access Natural;
      Bytes_Written : access Natural;
      Error         : GError_Access := null) return chars_ptr;
   --  Same as procedure Locale_To_UTF8, but return the raw C string for
   --  efficiency. The caller is responsible for freeing the resulting string.

   function Locale_To_UTF8 (OS_String : String) return String;
   --  Same as procedure Locale_To_UTF8, but return only the String.

   procedure Locale_From_UTF8
     (UTF8_String   : String;
      Bytes_Read    : out Natural;
      Bytes_Written : out Natural;
      Error         : GError_Access := null;
      Result        : out String);
   --  Convert a string from UTF-8 to the encoding used for strings by
   --  the C runtime (usually the same as that used by the operating
   --  system) in the current locale.
   --
   --  UTF8_String:   A UTF-8 encoded string
   --  Bytes_Read:    Number of bytes in the input string that were
   --                 successfully converted.
   --                 Even if the conversion was successful, this may be
   --                 less than Len if there were partial characters
   --                 at the end of the input. If the error
   --                 Illegal_Sequence occurs, the value
   --                 stored will the byte offset after the last valid
   --                 input sequence.
   --  Bytes_Written: Number of bytes stored in the output buffer.
   --  Error:         Location to store the error occuring, ignored if null.
   --                 Any of the errors in Convert_Error_Domain may occur.

   function Locale_From_UTF8
     (UTF8_String   : String;
      Bytes_Read    : access Natural;
      Bytes_Written : access Natural;
      Error         : GError_Access := null) return chars_ptr;
   --  Same as procedure Locale_From_UTF8, but return the raw C string for
   --  efficiency. The caller is responsible for freeing the resulting string.
   --  Use the C "free" function to free this.

   function Locale_From_UTF8 (UTF8_String : String) return String;
   --  Same as procedure Locale_From_UTF8, but return only the String.

   function Filename_To_UTF8
     (OS_String : String;
      Error     : GError_Access := null) return String;
   --  Convert a string which is in the encoding used for filenames
   --  into a UTF-8 string.

   function Filename_From_UTF8
     (UTF8_String : String;
      Error       : GError_Access := null) return String;
   --  Convert a string from UTF-8 to the encoding used for filenames.

   function Filename_From_URI
     (URI      : String;
      Hostname : access chars_ptr;
      Error    : GError_Access := null) return String;
   --  Convert an escaped UTF-8 encoded URI to a local filename in the
   --  encoding used for filenames.
   --
   --  URI:      A uri describing a filename (escaped, encoded in UTF-8).
   --  Hostname: Location to store hostname for the URI.
   --            If there is no hostname in the URI, null will be
   --            stored in this location.
   --  Error:    Location to store the error occuring, ignored if null.
   --            Any of the errors in Convert_Error_Domain may occur.

   function Filename_To_URI
     (Filename : String;
      Hostname : String := "";
      Error    : GError_Access := null) return String;
   --  Convert an absolute filename to an escaped UTF-8 encoded URI.
   --
   --  Filename: An absolute filename specified in the encoding
   --            used for filenames by the operating system.
   --  Hostname: A UTF-8 encoded hostname, or "" for none.
   --  Error:    Location to store the error occuring, ignored if null.
   --            Any of the errors in Convert_Error may occur.

   function Escape_Text (S : String) return String;
   --  Escape the text so that it is interpreted as-is by the Pango markup
   --  language

private

   pragma Import (C, Convert_Error_Domain, "g_convert_error_quark");

end Glib.Convert;
