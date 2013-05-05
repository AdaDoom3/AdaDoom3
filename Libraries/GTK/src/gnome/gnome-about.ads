-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                  Copyright (C) 2000-2002                          --
--                            ACT-Europe                             --
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

with Gdk.Pixbuf;
with Gtk;
with Gnome.Dialog;
with Gtkada.Types; use Gtkada.Types;

package Gnome.About is
   type Gnome_About_Record is new Gnome.Dialog.Gnome_Dialog_Record
     with private;
   type Gnome_About is access all Gnome_About_Record'Class;

   procedure Gnome_New
     (About              : out Gnome_About;
      Name               : String;
      Version            : String;
      Copyright          : String;
      Comments           : String;
      Authors            : Chars_Ptr_Array;
      Documenters        : Chars_Ptr_Array;
      Translator_Credits : String;
      Logo               : Gdk.Pixbuf.Gdk_Pixbuf);
   --  Create a new about box.
   --  Name is the name of the application.
   --  Version is the version string.
   --  Copyright is the copyright notice (one line.)
   --  Comments provides other comments.
   --  Authors is the list of authors.
   --  Documenters is the list of Documenters
   --  Translator_Credits is the names of Translators
   --  Logo is a logo pixbuf file.
   --
   --  Usage:
   --    Gnome_New (About, -"GnoApp", "1.2b", -"Copyright FSF (c) 2001",
   --      "author1" + "author2",
   --      "Comment line 1" & ASCII.LF & "Comment Line 2",
   --      Logo);
   --    Show (About);

   procedure Initialize
     (About              : access Gnome_About_Record'Class;
      Name               : String;
      Version            : String;
      Copyright          : String;
      Comments           : String;
      Authors            : Chars_Ptr_Array;
      Documenters        : Chars_Ptr_Array;
      Translator_Credits : String;
      Logo               : Gdk.Pixbuf.Gdk_Pixbuf);

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gnome_About.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  </signals>

private
   type Gnome_About_Record is new Gnome.Dialog.Gnome_Dialog_Record
     with null record;
   pragma Import (C, Get_Type, "gnome_about_get_type");
end Gnome.About;
