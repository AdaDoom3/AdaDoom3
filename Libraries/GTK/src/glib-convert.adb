-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                Copyright (C) 2001-2004 ACT-Europe                 --
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

package body Glib.Convert is

   procedure g_free (S : chars_ptr);
   pragma Import (C, g_free, "g_free");

   function g_convert
     (Str           : String;
      Len           : Gsize;
      To_Codeset    : String;
      From_Codeset  : String;
      Bytes_Read    : access Gsize;
      Bytes_Written : access Gsize;
      Error         : GError_Access) return chars_ptr;

   function g_convert
     (Str           : chars_ptr;
      Len           : Gsize;
      To_Codeset    : String;
      From_Codeset  : String;
      Bytes_Read    : access Gsize;
      Bytes_Written : access Gsize;
      Error         : GError_Access) return chars_ptr;

   pragma Import (C, g_convert, "g_convert");

   -------------
   -- Convert --
   -------------

   procedure Convert
     (Str           : String;
      To_Codeset    : String;
      From_Codeset  : String;
      Bytes_Read    : out Natural;
      Bytes_Written : out Natural;
      Error         : GError_Access := null;
      Result        : out String)
   is
      Read    : aliased Gsize;
      Written : aliased Gsize;
      S       : chars_ptr;

   begin
      S := g_convert
        (Str, Str'Length, To_Codeset & ASCII.NUL, From_Codeset & ASCII.NUL,
         Read'Access, Written'Access, Error);
      Bytes_Read := Natural (Read);
      Bytes_Written := Natural (Written);

      declare
         Res : constant String := Value (S);
      begin
         Result (Result'First .. Result'First + Bytes_Written - 1) := Res;
      end;

      g_free (S);
   end Convert;

   function Convert
     (Str           : String;
      To_Codeset    : String;
      From_Codeset  : String;
      Error         : GError_Access := null) return String
   is
      Read    : aliased Gsize;
      Written : aliased Gsize;
      S       : chars_ptr;

   begin
      S := g_convert
        (Str, Str'Length, To_Codeset & ASCII.NUL, From_Codeset & ASCII.NUL,
         Read'Access, Written'Access, Error);

      if S = Null_Ptr then
         return "";
      else
         declare
            Res : constant String := Value (S);
         begin
            g_free (S);
            return Res;
         end;
      end if;
   end Convert;

   procedure Convert
     (Str           : chars_ptr;
      Len           : Natural;
      To_Codeset    : String;
      From_Codeset  : String;
      Bytes_Read    : out Natural;
      Bytes_Written : out Natural;
      Error         : GError_Access := null;
      Result        : out String)
   is
      Read    : aliased Gsize;
      Written : aliased Gsize;
      S       : chars_ptr;

   begin
      S := g_convert
        (Str, Gsize (Len), To_Codeset & ASCII.NUL, From_Codeset & ASCII.NUL,
         Read'Access, Written'Access, Error);
      Bytes_Read := Natural (Read);
      Bytes_Written := Natural (Written);

      if S = Null_Ptr then
         Bytes_Written := 0;
      else
         declare
            Res : constant String := Value (S);
         begin
            Result (Result'First .. Result'First + Bytes_Written - 1) := Res;
         end;
         g_free (S);
      end if;
   end Convert;

   function Convert
     (Str           : String;
      To_Codeset    : String;
      From_Codeset  : String;
      Bytes_Read    : access Natural;
      Bytes_Written : access Natural;
      Error         : GError_Access := null) return chars_ptr
   is
      Read    : aliased Gsize;
      Written : aliased Gsize;
      S       : chars_ptr;

   begin
      S := g_convert
        (Str, Str'Length, To_Codeset & ASCII.NUL, From_Codeset & ASCII.NUL,
         Read'Access, Written'Access, Error);
      Bytes_Read.all := Natural (Read);
      Bytes_Written.all := Natural (Written);
      return S;
   end Convert;

   function Convert
     (Str           : chars_ptr;
      Len           : Natural;
      To_Codeset    : String;
      From_Codeset  : String;
      Bytes_Read    : access Natural;
      Bytes_Written : access Natural;
      Error         : GError_Access := null) return chars_ptr
   is
      Read    : aliased Gsize;
      Written : aliased Gsize;
      S       : chars_ptr;

   begin
      S := g_convert
        (Str, Gsize (Len), To_Codeset & ASCII.NUL, From_Codeset & ASCII.NUL,
         Read'Access, Written'Access, Error);
      Bytes_Read.all := Natural (Read);
      Bytes_Written.all := Natural (Written);
      return S;
   end Convert;

   -----------------------
   -- Filename_From_URI --
   -----------------------

   function Filename_From_URI
     (URI      : String;
      Hostname : access chars_ptr;
      Error    : GError_Access := null) return String
   is
      function Internal
        (URI      : String;
         Hostname : access chars_ptr;
         Error    : GError_Access) return chars_ptr;
      pragma Import (C, Internal, "g_filename_from_uri");

      S   : constant chars_ptr := Internal (URI & ASCII.NUL, Hostname, Error);
      Str : constant String := Value (S);

   begin
      g_free (S);
      return Str;
   end Filename_From_URI;

   ------------------------
   -- Filename_From_UTF8 --
   ------------------------

   function Filename_From_UTF8
     (UTF8_String : String;
      Error       : GError_Access := null) return String
   is
      function Internal
        (UTF8_String   : String;
         Len           : Gsize;
         Bytes_Read    : System.Address := System.Null_Address;
         Bytes_Written : System.Address := System.Null_Address;
         Error         : GError_Access) return chars_ptr;
      pragma Import (C, Internal, "g_filename_from_utf8");

      S   : constant chars_ptr := Internal
        (UTF8_String, UTF8_String'Length, Error => Error);
      Str : constant String := Value (S);

   begin
      g_free (S);
      return Str;
   end Filename_From_UTF8;

   ---------------------
   -- Filename_To_URI --
   ---------------------

   function Filename_To_URI
     (Filename : String;
      Hostname : String := "";
      Error    : GError_Access := null) return String
   is
      function Internal
        (URI      : String;
         Hostname : System.Address;
         Error    : GError_Access) return chars_ptr;
      pragma Import (C, Internal, "g_filename_to_uri");

      S    : chars_ptr;
      Host : aliased constant String := Hostname & ASCII.NUL;

   begin
      if Hostname = "" then
         S := Internal (Filename & ASCII.NUL, System.Null_Address, Error);
      else
         S := Internal (Filename & ASCII.NUL, Host'Address, Error);
      end if;

      declare
         Str : constant String := Value (S);
      begin
         g_free (S);
         return Str;
      end;
   end Filename_To_URI;

   ----------------------
   -- Filename_To_UTF8 --
   ----------------------

   function Filename_To_UTF8
     (OS_String : String;
      Error     : GError_Access := null) return String
   is
      function Internal
        (OS_String     : String;
         Len           : Gsize;
         Bytes_Read    : System.Address := System.Null_Address;
         Bytes_Written : System.Address := System.Null_Address;
         Error         : GError_Access) return chars_ptr;
      pragma Import (C, Internal, "g_filename_to_utf8");

      S   : constant chars_ptr := Internal
        (OS_String, OS_String'Length, Error => Error);
      Str : constant String := Value (S);

   begin
      g_free (S);
      return Str;
   end Filename_To_UTF8;

   ----------------------
   -- Locale_From_UTF8 --
   ----------------------

   procedure Locale_From_UTF8
     (UTF8_String   : String;
      Bytes_Read    : out Natural;
      Bytes_Written : out Natural;
      Error         : GError_Access := null;
      Result        : out String)
   is
      function Internal
        (UTF8_String   : String;
         Len           : Gsize;
         Bytes_Read    : access Gsize;
         Bytes_Written : access Gsize;
         Error         : GError_Access) return chars_ptr;
      pragma Import (C, Internal, "g_locale_from_utf8");

      Read    : aliased Gsize;
      Written : aliased Gsize;
      S       : chars_ptr;

   begin
      S := Internal
        (UTF8_String, UTF8_String'Length, Read'Access, Written'Access, Error);
      Bytes_Read := Natural (Read);
      Bytes_Written := Natural (Written);

      declare
         Res : constant String := Value (S);
      begin
         Result (Result'First .. Result'First + Bytes_Written - 1) := Res;
      end;

      g_free (S);
   end Locale_From_UTF8;

   function Locale_From_UTF8
     (UTF8_String   : String;
      Bytes_Read    : access Natural;
      Bytes_Written : access Natural;
      Error         : GError_Access := null) return chars_ptr
   is
      function Internal
        (UTF8_String   : String;
         Len           : Gsize;
         Bytes_Read    : access Gsize;
         Bytes_Written : access Gsize;
         Error         : GError_Access) return chars_ptr;
      pragma Import (C, Internal, "g_locale_from_utf8");

      Read    : aliased Gsize;
      Written : aliased Gsize;
      S       : chars_ptr;

   begin
      S := Internal
        (UTF8_String, UTF8_String'Length, Read'Access, Written'Access, Error);
      Bytes_Read.all := Natural (Read);
      Bytes_Written.all := Natural (Written);
      return S;
   end Locale_From_UTF8;

   function Locale_From_UTF8 (UTF8_String : String) return String is
      function Internal
        (UTF8_String   : String;
         Len           : Gsize;
         Bytes_Read    : System.Address := System.Null_Address;
         Bytes_Written : System.Address := System.Null_Address;
         Error         : GError_Access := null) return chars_ptr;
      pragma Import (C, Internal, "g_locale_from_utf8");

      S : constant chars_ptr := Internal (UTF8_String, UTF8_String'Length);

   begin
      if S = Null_Ptr then
         return "";
      else
         declare
            Str : constant String := Value (S);
         begin
            g_free (S);
            return Str;
         end;
      end if;
   end Locale_From_UTF8;

   --------------------
   -- Locale_To_UTF8 --
   --------------------

   procedure Locale_To_UTF8
     (OS_String     : String;
      Bytes_Read    : out Natural;
      Bytes_Written : out Natural;
      Error         : GError_Access := null;
      Result        : out String)
   is
      function Internal
        (UTF8_String   : String;
         Len           : Gsize;
         Bytes_Read    : access Gsize;
         Bytes_Written : access Gsize;
         Error         : GError_Access) return chars_ptr;
      pragma Import (C, Internal, "g_locale_to_utf8");

      Read    : aliased Gsize;
      Written : aliased Gsize;
      S       : chars_ptr;

   begin
      S := Internal
        (OS_String, OS_String'Length, Read'Access, Written'Access, Error);

      Bytes_Read := Natural (Read);
      Bytes_Written := Natural (Written);

      if S = Null_Ptr then
         return;
      end if;

      declare
         Res : constant String := Value (S);
      begin
         Result (Result'First .. Result'First + Bytes_Written - 1) := Res;
      end;

      g_free (S);
   end Locale_To_UTF8;

   function Locale_To_UTF8
     (OS_String     : String;
      Bytes_Read    : access Natural;
      Bytes_Written : access Natural;
      Error         : GError_Access := null) return chars_ptr
   is
      function Internal
        (OS_String     : String;
         Len           : Gsize;
         Bytes_Read    : access Gsize;
         Bytes_Written : access Gsize;
         Error         : GError_Access) return chars_ptr;
      pragma Import (C, Internal, "g_locale_to_utf8");

      Read    : aliased Gsize;
      Written : aliased Gsize;
      S       : chars_ptr;

   begin
      S := Internal
        (OS_String, OS_String'Length, Read'Access, Written'Access, Error);
      Bytes_Read.all := Natural (Read);
      Bytes_Written.all := Natural (Written);
      return S;
   end Locale_To_UTF8;

   function Locale_To_UTF8 (OS_String : String) return String is
      function Internal
        (OS_String     : String;
         Len           : Gsize;
         Bytes_Read    : System.Address := System.Null_Address;
         Bytes_Written : System.Address := System.Null_Address;
         Error         : GError_Access := null) return chars_ptr;
      pragma Import (C, Internal, "g_locale_to_utf8");

      S : constant chars_ptr := Internal (OS_String, OS_String'Length);

   begin
      if S = Null_Ptr then
         return "";

      else
         declare
            Str : constant String := Value (S);
         begin
            g_free (S);
            return Str;
         end;
      end if;
   end Locale_To_UTF8;

   -----------------
   -- Escape_Text --
   -----------------

   function Escape_Text (S : String) return String is
      function Internal (S : String; L : Integer) return
        Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "g_markup_escape_text");

      C_Res  : constant Interfaces.C.Strings.chars_ptr :=
        Internal (S, S'Length);
      Result : constant String := Interfaces.C.Strings.Value (C_Res);

   begin
      g_free (C_Res);
      return Result;
   end Escape_Text;

end Glib.Convert;
