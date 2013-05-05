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
--  A Gtk_Paned is a container that organizes its two children either
--  horizontally or vertically. The initial size allocated to the children
--  depends on the size they request. However, the user has the possibility to
--  interactively move a separation bar between the two to enlarge one of the
--  children, while at the same time shrinking the second one. The bar can be
--  moved by clicking with the mouse on a small cursor displayed in the bar,
--  and then dragging the mouse.
--
--  No additional decoration is provided around the children.
--
--  Each child has two parameters, Resize and Shrink.
--
--  If Shrink is True, then the widget can be made smaller than its
--  requisition size by the user. Set this to False if you want to set a
--  minimum size.
--
--  if Resize is True, this means that the child accepts to be resized, and
--  will not require any size. Thus, the size allocated to it will be the total
--  size allocated to the container minus the size requested by the other
--  child. If Resize is False, the child should ask for a specific size, which
--  it will get. The other child will be resized accordingly. If both Child
--  have the same value for Resize (either True or False), then the size
--  allocated to each is a ratio between the size requested by both.
--
--  When you use Set_Position with a parameter other than -1, or the user
--  moves the handle to resize the widgets, the behavior of Resize is canceled.
--
--  </description>
--  <screenshot>gtk-paned</screenshot>
--  <group>Layout container</group>
--  <testgtk>create_paned.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Window;      use Gdk.Window;
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Container;   use Gtk.Container;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Orientable;  use Gtk.Orientable;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Paned is

   type Gtk_Paned_Record is new Gtk_Container_Record with null record;
   type Gtk_Paned is access all Gtk_Paned_Record'Class;

   subtype Gtk_Hpaned_Record is Gtk_Paned_Record;
   subtype Gtk_Hpaned is Gtk_Paned;

   subtype Gtk_Vpaned_Record is Gtk_Paned_Record;
   subtype Gtk_Vpaned is Gtk_Paned;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_paned_get_type");

   procedure Gtk_New_Hpaned (Paned : out Gtk_Hpaned);
   procedure Initialize_Hpaned (Paned : access Gtk_Hpaned_Record'Class);
   --  The children will be displayed next to each other

   function Get_Type_Hpaned return Glib.GType;
   pragma Import (C, Get_Type_Hpaned, "gtk_hpaned_get_type");

   procedure Gtk_New_Vpaned (Paned : out Gtk_Vpaned);
   procedure Initialize_Vpaned (Paned : access Gtk_Vpaned_Record'Class);
   --  The children will be displayed one on top of the other

   function Get_Type_Vpaned return Glib.GType;
   pragma Import (C, Get_Type_Vpaned, "gtk_vpaned_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add1
      (Paned : access Gtk_Paned_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Add the first child of the container. The child will be displayed
   --  either in the top or in the left pane, depending on the orientation of
   --  the container. This is equivalent to using the Pack1 procedure with its
   --  default parameters.

   procedure Add2
      (Paned : access Gtk_Paned_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Add the second child of the container. It will be displayed in the
   --  bottom or right pane, depending on the container's orientation. This is
   --  equivalent to using Pack2 with its default parameters.

   procedure Compute_Position
      (Paned      : access Gtk_Paned_Record;
       Allocation : Gint;
       Child1_Req : Gint;
       Child2_Req : Gint);

   function Get_Child1
      (Paned : access Gtk_Paned_Record) return Gtk.Widget.Gtk_Widget;
   --  Obtains the first child of the paned widget.
   --  Since: gtk+ 2.4

   function Get_Child2
      (Paned : access Gtk_Paned_Record) return Gtk.Widget.Gtk_Widget;
   --  Obtains the second child of the paned widget.
   --  Since: gtk+ 2.4

   function Get_Handle_Window
      (Paned : access Gtk_Paned_Record) return Gdk.Window.Gdk_Window;
   --  Returns the Gdk.Window.Gdk_Window of the handle. This function is
   --  useful when handling button or motion events because it enables the
   --  callback to distinguish between the window of the paned, a child and the
   --  handle.
   --  Since: gtk+ 2.20

   function Get_Position (Paned : access Gtk_Paned_Record) return Gint;
   procedure Set_Position (Paned : access Gtk_Paned_Record; Position : Gint);
   --  Sets the position of the divider between the two panes.
   --  "position": pixel position of divider, a negative value means that the
   --  position is unset.

   procedure Pack1
      (Paned  : access Gtk_Paned_Record;
       Child  : access Gtk.Widget.Gtk_Widget_Record'Class;
       Resize : Boolean := False;
       Shrink : Boolean := True);
   --  Add a child to the top or left pane. You can not change dynamically the
   --  attributes Resize and Shrink. Instead, you have to remove the child from
   --  the container, and put it back with the new value of the attributes. You
   --  should also first call Glib.Object.Ref on the child so as to be sure it
   --  is not destroyed when you remove it, and Glib.Object.Unref it at the
   --  end. See the example in testgtk/ in the GtkAda distribution.

   procedure Pack2
      (Paned  : access Gtk_Paned_Record;
       Child  : access Gtk.Widget.Gtk_Widget_Record'Class;
       Resize : Boolean := False;
       Shrink : Boolean := False);

   ---------------------
   -- Interfaces_Impl --
   ---------------------

   function Get_Orientation
      (Self : access Gtk_Paned_Record) return Gtk.Enums.Gtk_Orientation;
   procedure Set_Orientation
      (Self        : access Gtk_Paned_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "Orientable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Paned_Record, Gtk_Paned);
   function "+"
     (Widget : access Gtk_Paned_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Paned
   renames Implements_Buildable.To_Object;

   package Implements_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Paned_Record, Gtk_Paned);
   function "+"
     (Widget : access Gtk_Paned_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Paned
   renames Implements_Orientable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Max_Position_Property
   --  Type: Gint
   --  Flags: read-write
   --  The largest possible value for the position property. This property is
   --  derived from the size and shrinkability of the widget's children.
   --
   --  Name: Min_Position_Property
   --  Type: Gint
   --  Flags: read-write
   --  The smallest possible value for the position property. This property is
   --  derived from the size and shrinkability of the widget's children.
   --
   --  Name: Position_Property
   --  Type: Gint
   --  Flags: read-write
   --
   --  Name: Position_Set_Property
   --  Type: Boolean
   --  Flags: read-write
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Max_Position_Property : constant Glib.Properties.Property_Int;
   Min_Position_Property : constant Glib.Properties.Property_Int;
   Position_Property : constant Glib.Properties.Property_Int;
   Position_Set_Property : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "accept-position"
   --     function Handler
   --       (Self : access Gtk_Paned_Record'Class) return Boolean;
   --  The ::accept-position signal is a <link
   --  linkend="keybinding-signals">keybinding signal</link> which gets emitted
   --  to accept the current position of the handle when moving it using key
   --  bindings. The default binding for this signal is Return or Space.
   --
   --  "cancel-position"
   --     function Handler
   --       (Self : access Gtk_Paned_Record'Class) return Boolean;
   --  The ::cancel-position signal is a <link
   --  linkend="keybinding-signals">keybinding signal</link> which gets emitted
   --  to cancel moving the position of the handle using key bindings. The
   --  position of the handle will be reset to the value prior to moving it.
   --  The default binding for this signal is Escape.
   --
   --  "cycle-child-focus"
   --     function Handler
   --       (Self     : access Gtk_Paned_Record'Class;
   --        Reversed : Boolean) return Boolean;
   --    --  "reversed": whether cycling backward or forward
   --  The ::cycle-child-focus signal is a <link
   --  linkend="keybinding-signals">keybinding signal</link> which gets emitted
   --  to cycle the focus between the children of the paned. The default
   --  binding is f6.
   --
   --  "cycle-handle-focus"
   --     function Handler
   --       (Self     : access Gtk_Paned_Record'Class;
   --        Reversed : Boolean) return Boolean;
   --    --  "reversed": whether cycling backward or forward
   --  The ::cycle-handle-focus signal is a <link
   --  linkend="keybinding-signals">keybinding signal</link> which gets emitted
   --  to cycle whether the paned should grab focus to allow the user to change
   --  position of the handle by using key bindings. The default binding for
   --  this signal is f8.
   --
   --  "move-handle"
   --     function Handler
   --       (Self        : access Gtk_Paned_Record'Class;
   --        Scroll_Type : Gtk.Enums.Gtk_Scroll_Type) return Boolean;
   --    --  "scroll_type": a Gtk.Enums.Gtk_Scroll_Type
   --  The ::move-handle signal is a <link
   --  linkend="keybinding-signals">keybinding signal</link> which gets emitted
   --  to move the handle when the user is using key bindings to move it.
   --
   --  "toggle-handle-focus"
   --     function Handler
   --       (Self : access Gtk_Paned_Record'Class) return Boolean;
   --  The ::toggle-handle-focus is a <link
   --  linkend="keybinding-signals">keybinding signal</link> which gets emitted
   --  to accept the current position of the handle and then move focus to the
   --  next widget in the focus chain. The default binding is Tab.

   Signal_Accept_Position : constant Glib.Signal_Name := "accept-position";
   Signal_Cancel_Position : constant Glib.Signal_Name := "cancel-position";
   Signal_Cycle_Child_Focus : constant Glib.Signal_Name := "cycle-child-focus";
   Signal_Cycle_Handle_Focus : constant Glib.Signal_Name := "cycle-handle-focus";
   Signal_Move_Handle : constant Glib.Signal_Name := "move-handle";
   Signal_Toggle_Handle_Focus : constant Glib.Signal_Name := "toggle-handle-focus";

private
   Max_Position_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("max-position");
   Min_Position_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("min-position");
   Position_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("position");
   Position_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("position-set");
end Gtk.Paned;
