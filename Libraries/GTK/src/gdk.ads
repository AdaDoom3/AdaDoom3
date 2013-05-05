-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
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
--  This is the top level package of the Gdk hierarchy.
--  It provides the type definitions used to access underlying C structures.
--
--  </description>
--  <group>Gdk, the low-level API</group>

with Glib;

package Gdk is
   pragma Preelaborate;

   subtype C_Proxy is Glib.C_Proxy;

   type Gdk_GC is new C_Proxy;

   type Gdk_Drawable is new C_Proxy;
   subtype Gdk_Window is Gdk_Drawable;
   subtype Gdk_Pixmap is Gdk_Drawable;
   subtype Gdk_Bitmap is Gdk_Drawable;

   type Gdk_Screen is new C_Proxy;

   type Gdk_Colormap is new C_Proxy;

   type Gdk_Visual is new C_Proxy;

   type Gdk_Font is new C_Proxy;

   type Gdk_Image is new C_Proxy;

   type Gdk_Region is new C_Proxy;

   type Gdk_Window_Attr is new C_Proxy;

end Gdk;
