-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2006-2013, AdaCore                   --
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
--  This package provides a convenitn function to create a new graphic
--  context. Such contexts are needed in several places in GtkAda, in
--  particular for the drawing routines, and this function provides a
--  convenient way to create them.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Miscellaneous</group>
--  <see>Gdk.GC</see>

with Gdk.GC;

package Gtk.GC is

   function Get
     (Depth       : Gint;
      Colormap    : Gdk.Gdk_Colormap;
      Values      : Gdk.GC.Gdk_GC_Values;
      Values_Mask : Gdk.GC.Gdk_GC_Values_Mask)
      return Gdk_GC;
   --  Create a new GC with the matching attributes.
   --  If such a graphic context already exists, it is returned, which is much
   --  faster than creating a new one. Creating a new context requires a
   --  round-trip to the server (X11 for instance), and can be slow.
   --  You shouldn't modify the attributes of the returned context, since that
   --  might impact other parts of the code that have queried it.

   procedure Release (Gc : Gdk_GC);
   --  Decrease the reference counting for the GC. If it reaches 0, then
   --  calling Get will create a new one the next time it is called with the
   --  same attributes.

private
   pragma Import (C, Get, "gtk_gc_get");
   pragma Import (C, Release, "gtk_gc_release");
end Gtk.GC;
