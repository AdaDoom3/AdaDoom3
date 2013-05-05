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
with Gtk.Box;

package Gnome.Icon_Entry is

   type Gnome_Icon_Entry_Record is new Gtk.Box.Gtk_Vbox_Record with private;
   type Gnome_Icon_Entry is access all Gnome_Icon_Entry_Record'Class;

   procedure Gnome_New
     (Widget              : out Gnome_Icon_Entry;
      History_Id          : String;
      Browse_Dialog_Title : String);

   procedure Initialize
     (Widget              : access Gnome_Icon_Entry_Record'Class;
      History_Id          : String;
      Browse_Dialog_Title : String);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   function Get_Filename
     (Ientry : access Gnome_Icon_Entry_Record) return String;

   procedure Set_Filename
     (Ientry   : access Gnome_Icon_Entry_Record;
      Filename : String);

   procedure Set_Pixmap_Subdir
     (Ientry : access Gnome_Icon_Entry_Record;
      Subdir : String);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  </signals>

private
   type Gnome_Icon_Entry_Record is new
     Gtk.Box.Gtk_Vbox_Record with null record;

   pragma Import (C, Get_Type, "gnome_icon_entry_get_type");
end Gnome.Icon_Entry;
