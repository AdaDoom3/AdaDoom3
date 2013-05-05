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
--  A Gtk_Layout is a widget that can have an almost infinite size, without
--  occupying a lot of memory. Its children can be located anywhere within it,
--  but will only appear on the screen if the visible area of the layout
--  contains them. Just like a Gtk_Viewport, its visible area is indicated by
--  two Gtk_Adjustment widgets, and thus a Gtk_Layout can be put as is in a
--  Gtk_Scrolled_Window. As for Gtk_Fixed containers, the children can be
--  located anywhere in the layout (no automatic organization is done). But, as
--  opposed to Gtk_Fixed widgets, a Gtk_Layout does not try to resize itself to
--  show all its children.
--
--  Starting from GtkAda 2.0, you have to call Set_Size and specify the
--  maximum size of the layout, otherwise children added with Put outside the
--  size defined for the layout will never be visible. One way to do this is to
--  systematically call Set_Size before calling Put, and make sure you specify
--  a size big enough for the layout.
--
--  </description>
--  <screenshot>gtk-layout</screenshot>
--  <group>Layout containers</group>
--  <testgtk>create_layout.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Window;      use Gdk.Window;
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Adjustment;  use Gtk.Adjustment;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Container;   use Gtk.Container;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Layout is

   type Gtk_Layout_Record is new Gtk_Container_Record with null record;
   type Gtk_Layout is access all Gtk_Layout_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Layout      : out Gtk_Layout;
       Hadjustment : Gtk.Adjustment.Gtk_Adjustment := null;
       Vadjustment : Gtk.Adjustment.Gtk_Adjustment := null);
   procedure Initialize
      (Layout      : access Gtk_Layout_Record'Class;
       Hadjustment : Gtk.Adjustment.Gtk_Adjustment := null;
       Vadjustment : Gtk.Adjustment.Gtk_Adjustment := null);
   --  Creates a new Gtk.Layout.Gtk_Layout. Unless you have a specific
   --  adjustment you'd like the layout to use for scrolling, pass null for
   --  "hadjustment": horizontal scroll adjustment, or null
   --  "vadjustment": vertical scroll adjustment, or null

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_layout_get_type");

   -------------
   -- Methods --
   -------------

   procedure Freeze (Layout : access Gtk_Layout_Record);
   --  This is a deprecated function, it doesn't do anything useful.

   function Get_Bin_Window
      (Layout : access Gtk_Layout_Record) return Gdk.Window.Gdk_Window;
   --  Retrieve the bin window of the layout used for drawing operations.
   --  Since: gtk+ 2.14

   function Get_Hadjustment
      (Layout : access Gtk_Layout_Record)
       return Gtk.Adjustment.Gtk_Adjustment;
   procedure Set_Hadjustment
      (Layout     : access Gtk_Layout_Record;
       Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --  Return the adjustment that indicate the horizontal visual area of the
   --  layout. You generally do not have to modify the value of this adjustment
   --  yourself, since this is done automatically when the layout has been put
   --  in a Gtk_Scrolled_Window.
   --  "adjustment": new scroll adjustment

   procedure Get_Size
      (Layout : access Gtk_Layout_Record;
       Width  : out Guint;
       Height : out Guint);
   procedure Set_Size
      (Layout : access Gtk_Layout_Record;
       Width  : Guint;
       Height : Guint);
   --  Sets the size of the scrollable area of the layout.
   --  "width": width of entire scrollable area
   --  "height": height of entire scrollable area

   function Get_Vadjustment
      (Layout : access Gtk_Layout_Record)
       return Gtk.Adjustment.Gtk_Adjustment;
   procedure Set_Vadjustment
      (Layout     : access Gtk_Layout_Record;
       Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --  Sets the vertical scroll adjustment for the layout. See
   --  Gtk.Scrolledwindow.Gtk_Scrolledwindow, Gtk.Scrollbar.Gtk_Scrollbar,
   --  Gtk.Adjustment.Gtk_Adjustment for details.
   --  "adjustment": new scroll adjustment

   procedure Move
      (Layout       : access Gtk_Layout_Record;
       Child_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
       X            : Gint;
       Y            : Gint);
   --  Moves a current child of Layout to a new position.
   --  "child_widget": a current child of Layout
   --  "x": X position to move to
   --  "y": Y position to move to

   procedure Put
      (Layout       : access Gtk_Layout_Record;
       Child_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
       X            : Gint;
       Y            : Gint);
   --  The child will be displayed on the screen only if at least part of it
   --  intersects the visible area of the layout. The layout does not resize
   --  itself to automatically show the widget. You also need to call Set_Size,
   --  if the size you initially defined is smaller than (X, Y), or the child
   --  will never be visible even if the layout is scrolled.
   --  "child_widget": child widget
   --  "x": X position of child widget
   --  "y": Y position of child widget

   procedure Thaw (Layout : access Gtk_Layout_Record);
   --  This is a deprecated function, it doesn't do anything useful.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Layout_Record, Gtk_Layout);
   function "+"
     (Widget : access Gtk_Layout_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Layout
   renames Implements_Buildable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Hadjustment_Property
   --  Type: Gtk.Adjustment.Gtk_Adjustment
   --  Flags: read-write
   --
   --  Name: Height_Property
   --  Type: Guint
   --  Flags: read-write
   --
   --  Name: Vadjustment_Property
   --  Type: Gtk.Adjustment.Gtk_Adjustment
   --  Flags: read-write
   --
   --  Name: Width_Property
   --  Type: Guint
   --  Flags: read-write

   Hadjustment_Property : constant Glib.Properties.Property_Object;
   Height_Property : constant Glib.Properties.Property_Uint;
   Vadjustment_Property : constant Glib.Properties.Property_Object;
   Width_Property : constant Glib.Properties.Property_Uint;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "set-scroll-adjustments"
   --     procedure Handler
   --       (Self   : access Gtk_Layout_Record'Class;
   --        Object : Gtk.Adjustment.Gtk_Adjustment;
   --        P0     : Gtk.Adjustment.Gtk_Adjustment);
   --  Set the scroll adjustments for the layout. Usually scrolled containers
   --  like Gtk.Scrolledwindow.Gtk_Scrolledwindow will emit this signal to
   --  connect two instances of Gtk.Scrollbar.Gtk_Scrollbar to the scroll
   --  directions of the Gtk.Layout.Gtk_Layout.

   Signal_Set_Scroll_Adjustments : constant Glib.Signal_Name := "set-scroll-adjustments";

private
   Hadjustment_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("hadjustment");
   Height_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("height");
   Vadjustment_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("vadjustment");
   Width_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("width");
end Gtk.Layout;
