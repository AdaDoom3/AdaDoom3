-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                  Copyright (C) 2001-2002                          --
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

with Gtk;
with Gtk.Button;

package Gnome.HRef is

   type Gnome_HRef_Record is new Gtk.Button.Gtk_Button_Record with private;
   type Gnome_HRef is access all Gnome_HRef_Record'Class;

   procedure Gnome_New
     (Widget : out Gnome_HRef;
      Url    : String;
      Label  : String);

   procedure Initialize
     (Widget : access Gnome_HRef_Record'Class;
      Url    : String;
      Label  : String);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   function Get_Text (Href : access Gnome_HRef_Record) return String;

   function Get_Url (Href : access Gnome_HRef_Record) return String;

   procedure Set_Text
     (Href : access Gnome_HRef_Record;
      Text : String);

   procedure Set_Url
     (Href : access Gnome_HRef_Record;
      Url  : String);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  </signals>

private
   type Gnome_HRef_Record is new Gtk.Button.Gtk_Button_Record with null record;

   pragma Import (C, Get_Type, "gnome_href_get_type");
end Gnome.HRef;
