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
--  The Gtk_Fixed widget is a container which can place child widgets at fixed
--  positions and with fixed sizes, given in pixels.
--
--  Note that it is usually bad practice to use the Gtk_Fixed container in
--  GtkAda. Instead, you should consider using one of the other many containers
--  available, that will allow you to handle resizing of your windows, as well
--  as font size changes easily.
--
--  </description>
--  <screenshot>gtk-fixed</screenshot>
--  <group>Layout containers</group>
--  <testgtk>create_fixed.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;          use Glib;
with Glib.Types;    use Glib.Types;
with Gtk.Buildable; use Gtk.Buildable;
with Gtk.Container; use Gtk.Container;
with Gtk.Widget;    use Gtk.Widget;

package Gtk.Fixed is

   type Gtk_Fixed_Record is new Gtk_Container_Record with null record;
   type Gtk_Fixed is access all Gtk_Fixed_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Fixed : out Gtk_Fixed);
   procedure Initialize (Fixed : access Gtk_Fixed_Record'Class);

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_fixed_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Has_Window (Fixed : access Gtk_Fixed_Record) return Boolean;
   pragma Obsolescent (Get_Has_Window);
   procedure Set_Has_Window
      (Fixed      : access Gtk_Fixed_Record;
       Has_Window : Boolean := False);
   pragma Obsolescent (Set_Has_Window);
   --  Sets whether a Gtk.Fixed.Gtk_Fixed widget is created with a separate
   --  Gdk.Window.Gdk_Window for Widget->window or not. (By default, it will be
   --  created with no separate Gdk.Window.Gdk_Window). This function must be
   --  called while the Gtk.Fixed.Gtk_Fixed is not realized, for instance,
   --  immediately after the window is created. This function was added to
   --  provide an easy migration path for older applications which may expect
   --  Gtk.Fixed.Gtk_Fixed to have a separate window.
   --  Deprecated since 2.20, Use Gtk.Widget.Set_Has_Window instead.
   --  "has_window": True if a separate window should be created

   procedure Move
      (Fixed  : access Gtk_Fixed_Record;
       Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
       X      : Gint;
       Y      : Gint);
   --  Move a child of a GtkFixed container to the given position. X indicates
   --  the horizontal position to place the widget at. Y is the vertical
   --  position to place the widget at.

   procedure Put
      (Fixed  : access Gtk_Fixed_Record;
       Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
       X      : Gint;
       Y      : Gint);
   --  Add Widget to a Fixed container at the given position. X indicates the
   --  horizontal position to place the widget at. Y is the vertical position
   --  to place the widget at.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Fixed_Record, Gtk_Fixed);
   function "+"
     (Widget : access Gtk_Fixed_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Fixed
   renames Implements_Buildable.To_Object;

end Gtk.Fixed;
