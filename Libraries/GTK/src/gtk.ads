-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2006 AdaCore                    --
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
--  This package provides some basic Gtk+ functionalities such as getting the
--  version number.  For general GtkAda initializations,
--  see Gtk.Main.
--
--  </description>
--  <c_version>2.8.17</c_version>

with Glib;        use Glib;
with Gdk;         use Gdk;

pragma Warnings (Off);
with Glib.Object; use Glib.Object;
with System;
pragma Warnings (On);

package Gtk is

   function Major_Version return Guint;
   --  Return the major version number for Gtk+ that was linked.
   --  Note that this is not necessarily the same as for GtkAda. It could also
   --  be different when your application is running, if the dynamic linker
   --  find some other GtkAda library. Use Gtk.Main.Check_Version to ensure
   --  that the two versions are compatible
   --  If the version is 1.2.6, returns 1.

   function Minor_Version return Guint;
   --  Return the minor version number for Gtk+.
   --  Note that this is not necessarily the same as for GtkAda.
   --  If the version is 1.2.6, returns 2.

   function Micro_Version return Guint;
   --  Return the micro version number for Gtk+.
   --  Note that this is not necessarily the same as for GtkAda.
   --  If the version is 1.2.6, returns 6.

   subtype Gtk_Type is Glib.GType;
   --  Renaming used for compatiblity.
   --  Note: Gtk_Type_* constants have been replaced by GType_* constants
   --  in Glib.

   type Gtk_Notebook_Page is new Gdk.C_Proxy;
   --  A page of the notebook.
   --  It can contain a single child, and is also associated with a tab
   --  label used to select that page in the notebook.

private

   pragma Import (C, Major_Version, "ada_gtk_major_version");
   pragma Import (C, Minor_Version, "ada_gtk_minor_version");
   pragma Import (C, Micro_Version, "ada_gtk_micro_version");

end Gtk;
