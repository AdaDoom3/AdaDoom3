-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2013, AdaCore                   --
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
--  The Gtk_Orientable interface is implemented by all widgets that can be
--  oriented horizontally or vertically. Historically, such widgets have been
--  realized as subclasses of a common base class (e.g
--  Gtk_Box/Gtk_HBox/Gtk_VBox or Gtk_Scale/Gtk_HScale/Gtk_VScale).
--  Gtk_Orientable is more flexible in that it allows the orientation to be
--  changed at runtime, allowing the widgets to 'flip'.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;       use Glib;
with Glib.Types; use Glib.Types;
with Gtk.Enums;  use Gtk.Enums;

package Gtk.Orientable is

   type Gtk_Orientable is new Glib.Types.GType_Interface;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_orientable_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Orientation
      (Self : Gtk_Orientable) return Gtk.Enums.Gtk_Orientation;
   procedure Set_Orientation
      (Self        : Gtk_Orientable;
       Orientation : Gtk.Enums.Gtk_Orientation);
   --  Sets the orientation of the Orientable.
   --  Since: gtk+ 2.16
   --  "orientation": the orientable's new orientation.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Orientation_Property
   --  Type: Gtk.Enums.Gtk_Orientation
   --  Flags: read-write
   --  The orientation of the orientable.

   Orientation_Property : constant Gtk.Enums.Property_Gtk_Orientation;

private
   Orientation_Property : constant Gtk.Enums.Property_Gtk_Orientation :=
     Gtk.Enums.Build ("orientation");
end Gtk.Orientable;
