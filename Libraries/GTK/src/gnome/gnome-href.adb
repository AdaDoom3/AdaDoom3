-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001                            --
--                         ACT-Europe                                --
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

with Gtk; use Gtk;
with Interfaces.C.Strings;
with System;

package body Gnome.HRef is

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New
     (Widget : out Gnome_HRef;
      Url    : String;
      Label  : String) is
   begin
      Widget := new Gnome_HRef_Record;
      Initialize (Widget, Url, Label);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget : access Gnome_HRef_Record'Class;
      Url    : String;
      Label  : String)
   is
      function Internal
        (Url    : String;
         Label  : String) return System.Address;
      pragma Import (C, Internal, "gnome_href_new");
   begin
      Set_Object (Widget, Internal (Url & ASCII.NUL, Label & ASCII.NUL));
   end Initialize;

   --------------
   -- Get_Text --
   --------------

   function Get_Text (Href : access Gnome_HRef_Record) return String is
      function Internal
        (Href : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gnome_href_get_text");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Href)));
   end Get_Text;

   -------------
   -- Get_Url --
   -------------

   function Get_Url (Href : access Gnome_HRef_Record) return String is
      function Internal
        (Href : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gnome_href_get_url");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Href)));
   end Get_Url;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
     (Href : access Gnome_HRef_Record;
      Text : String)
   is
      procedure Internal (Href : System.Address; Text : String);
      pragma Import (C, Internal, "gnome_href_set_text");
   begin
      Internal (Get_Object (Href), Text & ASCII.NUL);
   end Set_Text;

   -------------
   -- Set_Url --
   -------------

   procedure Set_Url (Href : access Gnome_HRef_Record; Url : String) is
      procedure Internal (Href : System.Address; Url : String);
      pragma Import (C, Internal, "gnome_href_set_url");
   begin
      Internal (Get_Object (Href), Url & ASCII.NUL);
   end Set_Url;

end Gnome.HRef;
