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
--  Gtk_Size_Group provides a mechanism for grouping a number of widgets
--  together so they all request the same amount of space. This is typically
--  useful when you want a column of widgets to have the same size, but you
--  can't use a Gtk_Table widget.
--
--  Note that size groups only affect the amount of space requested, not the
--  size that the widgets finally receive. If you want the widgets in a
--  Gtk_Size_Group to actually be the same size, you need to pack them in such
--  a way that they get the size they request and not more. For example, if you
--  are packing your widgets into a table, you would not include the Fill flag.
--
--  </description>
--  <testgtk>create_size_groups.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with Gtk.Buildable;           use Gtk.Buildable;
with Gtk.Widget;              use Gtk.Widget;

package Gtk.Size_Group is

   type Gtk_Size_Group_Record is new GObject_Record with null record;
   type Gtk_Size_Group is access all Gtk_Size_Group_Record'Class;

   type Size_Group_Mode is (None, Horizontal, Vertical, Both);
   pragma Convention (C, Size_Group_Mode);
   --  This type indicates how the size of all widgets in the group match:
   --  - None: The behavior is the same as if there was no size. Each widget
   --          requests its most appropriate size.
   --  - Horizontal: All the widgets in the group will have the same width.
   --  - Vertical: All the widgets in the group will have the same height
   --  - Both: All the widgets in the group will have exactly the same size.

   package Size_Group_Mode_Properties is new
   Glib.Generic_Properties.Generic_Internal_Discrete_Property
     (Size_Group_Mode);
   type Property_Size_Group_Mode is new Size_Group_Mode_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Size_Group : out Gtk_Size_Group;
       Mode       : Size_Group_Mode := Both);
   procedure Initialize
      (Size_Group : access Gtk_Size_Group_Record'Class;
       Mode       : Size_Group_Mode := Both);
   --  Create a new Gtk.Size_Group.Gtk_Size_Group.
   --  "mode": the mode for the new size group.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_size_group_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Widget
      (Size_Group : access Gtk_Size_Group_Record;
       Widget     : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Adds a widget to a Gtk.Size_Group.Gtk_Size_Group. In the future, the
   --  requisition of the widget will be determined as the maximum of its
   --  requisition and the requisition of the other widgets in the size group.
   --  Whether this applies horizontally, vertically, or in both directions
   --  depends on the mode of the size group. See Gtk.Size_Group.Set_Mode. When
   --  the widget is destroyed or no longer referenced elsewhere, it will be
   --  removed from the size group.
   --  "widget": the Gtk.Widget.Gtk_Widget to add

   function Get_Ignore_Hidden
      (Size_Group : access Gtk_Size_Group_Record) return Boolean;
   procedure Set_Ignore_Hidden
      (Size_Group    : access Gtk_Size_Group_Record;
       Ignore_Hidden : Boolean);
   --  Sets whether unmapped widgets should be ignored when calculating the
   --  size.
   --  Since: gtk+ 2.8
   --  "ignore_hidden": whether unmapped widgets should be ignored when
   --  calculating the size

   function Get_Mode
      (Size_Group : access Gtk_Size_Group_Record) return Size_Group_Mode;
   procedure Set_Mode
      (Size_Group : access Gtk_Size_Group_Record;
       Mode       : Size_Group_Mode);
   --  Sets the Size_Group_Mode of the size group. The mode of the size group
   --  determines whether the widgets in the size group should all have the
   --  same horizontal requisition (%GTK_SIZE_GROUP_MODE_HORIZONTAL) all have
   --  the same vertical requisition (%GTK_SIZE_GROUP_MODE_VERTICAL), or should
   --  all have the same requisition in both directions
   --  (%GTK_SIZE_GROUP_MODE_BOTH).
   --  "mode": the mode to set for the size group.

   function Get_Widgets
      (Size_Group : access Gtk_Size_Group_Record)
       return Gtk.Widget.Widget_SList.GSlist;
   --  Returns the list of widgets associated with Size_Group. widgets. The
   --  list is owned by GTK+ and should not be modified.
   --  Since: gtk+ 2.10

   procedure Remove_Widget
      (Size_Group : access Gtk_Size_Group_Record;
       Widget     : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Removes a widget from a Gtk.Size_Group.Gtk_Size_Group.
   --  "widget": the Gtk.Widget.Gtk_Widget to remove

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Size_Group_Record, Gtk_Size_Group);
   function "+"
     (Widget : access Gtk_Size_Group_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Size_Group
   renames Implements_Buildable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Ignore_Hidden_Property
   --  Type: Boolean
   --  Flags: read-write
   --  If True, unmapped widgets are ignored when determining the size of the
   --  group.
   --
   --  Name: Mode_Property
   --  Type: Size_Group_Mode
   --  Flags: read-write

   Ignore_Hidden_Property : constant Glib.Properties.Property_Boolean;
   Mode_Property : constant Gtk.Size_Group.Property_Size_Group_Mode;

private
   Ignore_Hidden_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("ignore-hidden");
   Mode_Property : constant Gtk.Size_Group.Property_Size_Group_Mode :=
     Gtk.Size_Group.Build ("mode");
end Gtk.Size_Group;
