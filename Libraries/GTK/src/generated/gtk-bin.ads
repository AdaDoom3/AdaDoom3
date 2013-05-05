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
--  Base class for containers that have only one child. This widget can not be
--  instantiated directly.
--
--  </description>
--  <group>Abstract base classes</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;          use Glib;
with Glib.Types;    use Glib.Types;
with Gtk.Buildable; use Gtk.Buildable;
with Gtk.Container; use Gtk.Container;
with Gtk.Widget;    use Gtk.Widget;

package Gtk.Bin is

   type Gtk_Bin_Record is new Gtk_Container_Record with null record;
   type Gtk_Bin is access all Gtk_Bin_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_bin_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Child
      (Bin : access Gtk_Bin_Record) return Gtk.Widget.Gtk_Widget;
   --  Gets the child of the Gtk.Bin.Gtk_Bin, or null if the bin contains no
   --  child widget. The returned widget does not have a reference added, so
   --  you do not need to unref it.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Bin_Record, Gtk_Bin);
   function "+"
     (Widget : access Gtk_Bin_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Bin
   renames Implements_Buildable.To_Object;

end Gtk.Bin;
