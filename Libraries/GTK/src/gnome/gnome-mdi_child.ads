-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                  Copyright (C) 2001-2013, AdaCore                 --
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

with Glib.Object;
with Gnome.App_Helper;
with Gtk;
with Gtk.Widget;

package Gnome.MDI_Child is

   type Gnome_MDI_Child_Record is new
     Glib.Object.GObject_Record with private;
   type Gnome_MDI_Child is access all Gnome_MDI_Child_Record'Class;

   function Add_View
     (MDI_Child : access Gnome_MDI_Child_Record) return Gtk.Widget.Gtk_Widget;

   procedure Remove_View
     (MDI_Child : access Gnome_MDI_Child_Record;
      View      : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure Set_Menu_Template
     (MDI_Child : access Gnome_MDI_Child_Record;
      Menu_Tmpl : access Gnome.App_Helper.UI_Info_Array);

   procedure Set_Name
     (MDI_Child : access Gnome_MDI_Child_Record;
      Name      : String);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  </signals>

private
   type Gnome_MDI_Child_Record is new
     Glib.Object.GObject_Record with null record;

end Gnome.MDI_Child;
