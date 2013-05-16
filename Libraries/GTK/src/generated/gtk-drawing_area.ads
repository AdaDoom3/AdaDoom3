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
--  This widget provides an empty canvas on which the application can draw
--  anything.
--
--  Note that this widget is simply an empty space, and that you need to
--  connect it to events to make it useful. For instance, you might want to do
--  one of the following :
--
--  * Connect it to "expose_event": The handlers are called every time the
--  widget needs to be redrawn. You can then draw anything you want on the
--  canvas, after getting its associated window with a call to
--  Gtk.Widget.Get_Window. Note that the event mask is automatically set up to
--  accept expose_events.
--
--  * Connect it to "button_press_event" and "button_release_event" events,
--  when you want it to react to user input. Note that you need to set up the
--  event mask with a call to Gtk.Widget.Set_Events.
--
--  See also the Double_Buffer widget provided in the GtkAda examples for an
--  advanced example that demonstrates how to use double buffering, to avoid
--  flickering in your drawings.
--
--  </description>
--  <group>Drawing</group>
--  <testgtk>libart_demo.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;          use Glib;
with Glib.Types;    use Glib.Types;
with Gtk.Buildable; use Gtk.Buildable;
with Gtk.Widget;    use Gtk.Widget;

package Gtk.Drawing_Area is

   type Gtk_Drawing_Area_Record is new Gtk_Widget_Record with null record;
   type Gtk_Drawing_Area is access all Gtk_Drawing_Area_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Drawing_Area : out Gtk_Drawing_Area);
   procedure Initialize
      (Drawing_Area : access Gtk_Drawing_Area_Record'Class);

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_drawing_area_get_type");

   -------------
   -- Methods --
   -------------

   procedure Size
      (Drawing_Area : access Gtk_Drawing_Area_Record;
       Width        : Gint;
       Height       : Gint);
   pragma Obsolescent (Size);
   --  Request a new size for the area. This queues a resize request for the
   --  area.
   --  Deprecated

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Drawing_Area_Record, Gtk_Drawing_Area);
   function "+"
     (Widget : access Gtk_Drawing_Area_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Drawing_Area
   renames Implements_Buildable.To_Object;

end Gtk.Drawing_Area;
