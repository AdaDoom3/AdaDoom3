-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
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
--  This package provides the most commonly used instantiations of
--  Gtk.Handlers
--
--  Gate takes advantage of these pre-instantiated packages.
--
--  </description>
--  <group>Signal handling</group>

with Gtk.Handlers;
pragma Elaborate_All (Gtk.Handlers);

with Glib.Object;
with Gtk.Widget;

package Gtkada.Handlers is

   package Widget_Callback is new
     Gtk.Handlers.Callback (Gtk.Widget.Gtk_Widget_Record);

   package Return_Callback is new Gtk.Handlers.Return_Callback
     (Gtk.Widget.Gtk_Widget_Record, Boolean);
   --  Emit_By_Name shouldn't be used, since it should really return a Gboolean

   package Object_Callback is new
     Gtk.Handlers.Callback (Glib.Object.GObject_Record);

   package Object_Return_Callback is new Gtk.Handlers.Return_Callback
     (Glib.Object.GObject_Record, Boolean);

end Gtkada.Handlers;
