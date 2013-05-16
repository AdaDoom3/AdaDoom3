-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                    Copyright (C) 2010-2013, AdaCore               --
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
--  Gtk_Recent_Chooser_Widget is a widget suitable for selecting recently used
--  files. It is the main building block of a Gtk_Recent_Chooser_Dialog. Most
--  applications will only need to use the latter; you can use
--  Gtk_Recent_Chooser_Widget as part of a larger window if you have special
--  needs.
--
--  Note that Gtk_Recent_Chooser_Widget does not have any methods of its own.
--  Instead, you should use the functions that work on a Gtk_Recent_Chooser.
--
--  Recently used files are supported since GTK+ 2.10.
--  </description>
--  <c_version>2.16.6</c_version>

with Gtk.Box;
with Gtk.Recent_Manager;

package Gtk.Recent_Chooser_Widget is

   type Gtk_Recent_Chooser_Widget_Record is
     new Gtk.Box.Gtk_Vbox_Record with private;
   type Gtk_Recent_Chooser_Widget is
     access all Gtk_Recent_Chooser_Widget_Record'Class;

   function Get_Type return GType;

   procedure Gtk_New (Widget : out Gtk_Recent_Chooser_Widget);
   procedure Initialize
     (Widget : access Gtk_Recent_Chooser_Widget_Record'Class);
   --  Creates a new Gtk_Recent_Chooser_Widget object.  This is an embeddable
   --  widget used to access the recently used resources list.

   procedure Gtk_New_For_Manager
     (Widget  : out Gtk_Recent_Chooser_Widget;
      Manager : access Gtk.Recent_Manager.Gtk_Recent_Manager_Record'Class);
   procedure Initialize_For_Manager
     (Widget  : access Gtk_Recent_Chooser_Widget_Record'Class;
      Manager : access Gtk.Recent_Manager.Gtk_Recent_Manager_Record'Class);
   --  Creates a new Gtk_Recent_Chooser_Widget with a specified recent manager.
   --
   --  This is useful if you have implemented your own recent manager, or if
   --  you have a customized instance of a Gtk_Recent_Manager object.

private

   type Gtk_Recent_Chooser_Widget_Record is
     new Gtk.Box.Gtk_Vbox_Record with null record;

   pragma Import (C, Get_Type, "gtk_recent_chooser_widget_get_type");

end Gtk.Recent_Chooser_Widget;
