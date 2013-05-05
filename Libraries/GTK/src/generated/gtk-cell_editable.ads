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
--  The Gtk_Cell_Editable interface must be implemented for widgets to be
--  usable when editing the contents of a Gtk_Tree_View cell
--
--  </description>
--  <group>Trees and Lists</group>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Event;       use Gdk.Event;
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;

package Gtk.Cell_Editable is

   type Gtk_Cell_Editable is new Glib.Types.GType_Interface;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_cell_editable_get_type");

   -------------
   -- Methods --
   -------------

   procedure Editing_Done (Cell_Editable : Gtk_Cell_Editable);
   pragma Import (C, Editing_Done, "gtk_cell_editable_editing_done");
   --  Emits the Gtk.Cell_Editable.Gtk_Cell_Editable::editing-done signal.

   procedure Remove_Widget (Cell_Editable : Gtk_Cell_Editable);
   pragma Import (C, Remove_Widget, "gtk_cell_editable_remove_widget");
   --  Emits the Gtk.Cell_Editable.Gtk_Cell_Editable::remove-widget signal.

   procedure Start_Editing
      (Cell_Editable : Gtk_Cell_Editable;
       Event         : Gdk.Event.Gdk_Event);
   pragma Import (C, Start_Editing, "gtk_cell_editable_start_editing");
   --  Begins editing on a Cell_Editable. Event is the GdkEvent that began the
   --  editing process. It may be null, in the instance that editing was
   --  initiated through programatic means.
   --  "event": A GdkEvent, or null

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Editing_Canceled_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Indicates whether editing on the cell has been canceled.

   Editing_Canceled_Property : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "editing-done"
   --     procedure Handler (Self : access Gtk_Cell_Editable);
   --  This signal is a sign for the cell renderer to update its value from
   --  the Cell_Editable. Implementations of
   --  Gtk.Cell_Editable.Gtk_Cell_Editable are responsible for emitting this
   --  signal when they are done editing, e.g. Gtk.GEntry.Gtk_Entry is emitting
   --  it when the user presses Enter. Gtk.Cell_Editable.Editing_Done is a
   --  convenience method for emitting GtkCellEditable::editing-done.
   --
   --  "remove-widget"
   --     procedure Handler (Self : access Gtk_Cell_Editable);
   --  This signal is meant to indicate that the cell is finished editing, and
   --  the widget may now be destroyed. Implementations of
   --  Gtk.Cell_Editable.Gtk_Cell_Editable are responsible for emitting this
   --  signal when they are done editing. It must be emitted after the
   --  Gtk.Cell_Editable.Gtk_Cell_Editable::editing-done signal, to give the
   --  cell renderer a chance to update the cell's value before the widget is
   --  removed. Gtk.Cell_Editable.Remove_Widget is a convenience method for
   --  emitting GtkCellEditable::remove-widget.

   Signal_Editing_Done : constant Glib.Signal_Name := "editing-done";
   Signal_Remove_Widget : constant Glib.Signal_Name := "remove-widget";

private
   Editing_Canceled_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("editing-canceled");
end Gtk.Cell_Editable;
