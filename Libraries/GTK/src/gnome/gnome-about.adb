-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--              Copyright (C) 2000-2013, AdaCore                     --
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

with System;
with Gtk; use Gtk;
with Gdk.Pixbuf; use Gdk.Pixbuf;

package body Gnome.About is

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New
     (About              : out Gnome_About;
      Name               : String;
      Version            : String;
      Copyright          : String;
      Comments           : String;
      Authors            : Chars_Ptr_Array;
      Documenters        : Chars_Ptr_Array;
      Translator_Credits : String;
      Logo               : Gdk_Pixbuf) is
   begin
      About := new Gnome_About_Record;
      Initialize
        (About, Name, Version, Copyright,
         Comments, Authors, Documenters,
         Translator_Credits, Logo);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (About              : access Gnome_About_Record'Class;
      Name               : String;
      Version            : String;
      Copyright          : String;
      Comments           : String;
      Authors            : Chars_Ptr_Array;
      Documenters        : Chars_Ptr_Array;
      Translator_Credits : String;
      Logo               : Gdk_Pixbuf)
   is
      Authors_Padded : constant Chars_Ptr_Array := Authors + Null_Ptr;
      Documenters_Padded : constant Chars_Ptr_Array := Documenters + Null_Ptr;

      function Internal
        (Name               : String;
         Version            : String;
         Copyright          : String;
         Comments           : String;
         Authors            : Chars_Ptr_Array;
         Documenters        : Chars_Ptr_Array;
         Translator_Credits : String;
         Logo               : System.Address) return System.Address;
      pragma Import (C, Internal, "gnome_about_new");

   begin
      Set_Object
        (About,
         Internal
         (Name & ASCII.NUL,
          Version & ASCII.NUL,
          Copyright & ASCII.NUL,
          Comments & ASCII.NUL,
          Authors_Padded,
          Documenters_Padded,
          Translator_Credits & ASCII.NUL,
          Get_Object (Logo)));
   end Initialize;

end Gnome.About;
