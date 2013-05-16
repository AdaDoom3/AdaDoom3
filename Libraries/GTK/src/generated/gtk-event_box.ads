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
--  This widget is a container that catches events for its child when its
--  child does not have its own window (like a Gtk_Scrolled_Window or a
--  Gtk_Label for instance). Some widgets in GtkAda do not have their own
--  window, and thus can not directly get events from the server. The
--  Gtk_Event_Box widget can be used to force its child to receive events
--  anyway.
--
--  For instance, this widget is used internally in a Gtk_Combo_Box so that
--  the application can change the cursor when the mouse is in the popup
--  window. In that case, it contains a frame, that itself contains the
--  scrolled window of the popup.
--
--  </description>
--  <group>Layout Containers</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Bin;         use Gtk.Bin;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Event_Box is

   type Gtk_Event_Box_Record is new Gtk_Bin_Record with null record;
   type Gtk_Event_Box is access all Gtk_Event_Box_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Event_Box : out Gtk_Event_Box);
   procedure Initialize (Event_Box : access Gtk_Event_Box_Record'Class);
   --  Create a new box.
   --  The box's child can then be set using the Gtk.Container.Add function.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_event_box_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Above_Child
      (Event_Box : access Gtk_Event_Box_Record) return Boolean;
   procedure Set_Above_Child
      (Event_Box   : access Gtk_Event_Box_Record;
       Above_Child : Boolean);
   --  Set whether the event box window is positioned above the windows of its
   --  child, as opposed to below it. If the window is above, all events inside
   --  the event box will go to the event box. If the window is below, events
   --  in windows of child widgets will first got to that widget, and then to
   --  its parents. The default is to keep the window below the child.
   --  Since: gtk+ 2.4
   --  "above_child": True if the event box window is above the windows of its
   --  child

   function Get_Visible_Window
      (Event_Box : access Gtk_Event_Box_Record) return Boolean;
   procedure Set_Visible_Window
      (Event_Box      : access Gtk_Event_Box_Record;
       Visible_Window : Boolean);
   --  Set whether the event box uses a visible or invisible child window. The
   --  default is to use visible windows. In an invisible window event box, the
   --  window that the event box creates is a %GDK_INPUT_ONLY window, which
   --  means that it is invisible and only serves to receive events. A visible
   --  window event box creates a visible (%GDK_INPUT_OUTPUT) window that acts
   --  as the parent window for all the widgets contained in the event box. You
   --  should generally make your event box invisible if you just want to trap
   --  events. Creating a visible window may cause artifacts that are visible
   --  to the user, especially if the user is using a theme with gradients or
   --  pixmaps. The main reason to create a non input-only event box is if you
   --  want to set the background to a different color or draw on it.
   --  Note: There is one unexpected issue for an invisible event box that has
   --  its window below the child. (See Gtk.Event_Box.Set_Above_Child.) Since
   --  the input-only window is not an ancestor window of any windows that
   --  descendent widgets of the event box create, events on these windows
   --  aren't propagated up by the windowing system, but only by GTK+. The
   --  practical effect of this is if an event isn't in the event mask for the
   --  descendant window (see Gtk.Widget.Add_Event), it won't be received by
   --  the event box.
   --  This problem doesn't occur for visible event boxes, because in that
   --  case, the event box window is actually the ancestor of the descendant
   --  windows, not just at the same place on the screen.
   --  Since: gtk+ 2.4
   --  "visible_window": boolean value

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Event_Box_Record, Gtk_Event_Box);
   function "+"
     (Widget : access Gtk_Event_Box_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Event_Box
   renames Implements_Buildable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Above_Child_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Visible_Window_Property
   --  Type: Boolean
   --  Flags: read-write

   Above_Child_Property : constant Glib.Properties.Property_Boolean;
   Visible_Window_Property : constant Glib.Properties.Property_Boolean;

private
   Above_Child_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("above-child");
   Visible_Window_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("visible-window");
end Gtk.Event_Box;
