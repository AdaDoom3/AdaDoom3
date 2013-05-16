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
--
--
--  </description>
--  <group>Obsolescent widgets</group>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Bitmap;    use Gdk.Bitmap;
with Gdk.Pixmap;    use Gdk.Pixmap;
with Glib;          use Glib;
with Glib.Types;    use Glib.Types;
with Gtk.Buildable; use Gtk.Buildable;
with Gtk.Misc;      use Gtk.Misc;
with Gtk.Widget;    use Gtk.Widget;
with Gtk.Window;    use Gtk.Window;
with Gtkada.Types;  use Gtkada.Types;

package Gtk.Pixmap is

   type Gtk_Pixmap_Record is new Gtk_Misc_Record with null record;
   type Gtk_Pixmap is access all Gtk_Pixmap_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Pixmap : out Gtk_Pixmap;
       Val    : Gdk.Pixmap.Gdk_Pixmap;
       Mask   : Gdk.Bitmap.Gdk_Bitmap);
   procedure Initialize
      (Pixmap : access Gtk_Pixmap_Record'Class;
       Val    : Gdk.Pixmap.Gdk_Pixmap;
       Mask   : Gdk.Bitmap.Gdk_Bitmap);

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_pixmap_get_type");

   -------------
   -- Methods --
   -------------

   procedure Get
      (Pixmap : access Gtk_Pixmap_Record;
       Val    : out Gdk.Pixmap.Gdk_Pixmap;
       Mask   : out Gdk.Bitmap.Gdk_Bitmap);

   procedure Set
      (Pixmap : access Gtk_Pixmap_Record;
       Val    : Gdk.Pixmap.Gdk_Pixmap;
       Mask   : Gdk.Bitmap.Gdk_Bitmap);

   procedure Set_Build_Insensitive
      (Pixmap : access Gtk_Pixmap_Record;
       Build  : Boolean);
   --  Whether the pixmap should be grayed out, as is done for insensitive
   --  widgets that do not accept user interaction

   ----------------------
   -- GtkAda additions --
   ----------------------

   function Create_Pixmap
     (Filename : String;
      Window   : access Gtk.Window.Gtk_Window_Record'Class) return Gtk_Pixmap;
   --  Create a pixmap given a window and a filename

   function Create_Pixmap
     (Data     : Gtkada.Types.Chars_Ptr_Array;
      Window   : access Gtk.Window.Gtk_Window_Record'Class) return Gtk_Pixmap;
   --  Create a pixmap given a window and a buffer.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Pixmap_Record, Gtk_Pixmap);
   function "+"
     (Widget : access Gtk_Pixmap_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Pixmap
   renames Implements_Buildable.To_Object;

end Gtk.Pixmap;
