-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2007 AdaCore                    --
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
--  Base class for widgets that have children.
--
--  When writing your own container widgets, you need to fully handle the
--  size_allocate event, by also resizing all the children (based on their size
--  requisition). The size_allocate event will always be sent to the parent
--  when a child calls Gtk.Widget.Queue_Resize.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Abstract base classes</group>

with Gdk.Event;
with Glib.Properties;
with Glib.Values;
with Gtk.Adjustment;
with Gtk.Enums;
with Gtk.Widget;

package Gtk.Container is

   type Gtk_Container_Record is new Gtk.Widget.Gtk_Widget_Record with private;
   type Gtk_Container is access all Gtk_Container_Record'Class;

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Gtk_Container.

   procedure Set_Border_Width
     (Container    : access Gtk_Container_Record;
      Border_Width : Guint);
   function Get_Border_Width
     (Container    : access Gtk_Container_Record) return Guint;
   --  Modify the size of the frame that surrounds the widget.
   --  The exact visual impact depends on the specific widget class.

   procedure Add
     (Container : access Gtk_Container_Record;
      Widget    : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Add a new child to the container.
   --  Note that some containers can have only one child. Nothing is done
   --  if there is already a child.
   --  This basically sends the "add" signal (see below)

   procedure Remove
     (Container : access Gtk_Container_Record;
      Widget    : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Removes Widget from Container. Widget must be inside Container.
   --  Note that Container will own a reference to Widget, and that this
   --  may be the last reference held; so removing a widget from its
   --  container can destroy that widget. If you want to use Widget
   --  again, you need to add a reference to it while it's not inside
   --  a container, using Glib.Object.Ref. If you don't want to use Widget
   --  again it's usually more efficient to simply destroy it directly
   --  using Gtk.Widget.Destroy since this will remove it from the
   --  container and help break any circular reference count cycles.

   procedure Set_Resize_Mode
     (Container   : access Gtk_Container_Record;
      Resize_Mode : Gtk.Enums.Gtk_Resize_Mode);
   function Get_Resize_Mode
     (Container : access Gtk_Container_Record)
      return Gtk.Enums.Gtk_Resize_Mode;
   --  Change the resizing behavior for the Container.
   --  The default value is Resize_Parent.

   function Get_Children
     (Container : access Gtk_Container_Record)
      return Gtk.Widget.Widget_List.Glist;
   --  Return a list of all the children of the container.
   --  The caller must free the returned list.

   procedure Propagate_Expose
     (Container : access Gtk_Container_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event     : Gdk.Event.Gdk_Event_Expose);
   --  When a container receives an expose event, it must send synthetic
   --  expose events to all children that don't have their own Gdk_Window.
   --  This function provides a convenient way of doing this. A container,
   --  when it receives an expose event, Propagate_Expose
   --  once for each child, passing in the event the container received.
   --
   --  Propagate_Expose takes care of deciding whether
   --  an expose event needs to be sent to the child, intersecting
   --  the event's area with the child area, and sending the event.
   --
   --  In most cases, a container can simply either simply inherit the
   --  expose implementation from Gtk_Container, or, do some drawing
   --  and then chain to the expose implementation from Gtk_Container.

   -----------
   -- Focus --
   -----------

   procedure Set_Focus_Chain
     (Container         : access Gtk_Container_Record;
      Focusable_Widgets : Gtk.Widget.Widget_List.Glist);
   --  Set the chain of widgets that can take the focus for a given Container.
   --  The list should be freed by the user.
   --  This list indicates in which order the widgets will get the focus when
   --  the user presses tab or the arrow keys to move from one widget to the
   --  next.

   procedure Get_Focus_Chain
     (Container         : access Gtk_Container_Record;
      Focusable_Widgets : out Gtk.Widget.Widget_List.Glist;
      Success           : out Boolean);
   --  Retrieves the focus chain of the container, if one has been
   --  set explicitly. If no focus chain has been explicitly
   --  set, GTK+ computes the focus chain based on the positions
   --  of the children. In that case, GTK+ stores null in
   --  Focusable_Widgets and returns FALSE.
   --  The returned list must be freed by the user.

   procedure Unset_Focus_Chain (Container : access Gtk_Container_Record);
   --  Undoes the effect of Set_Focus_Chain

   procedure Set_Focus_Vadjustment
     (Container  : access Gtk_Container_Record;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment);
   function Get_Focus_Vadjustment
     (Container : access Gtk_Container_Record)
      return Gtk.Adjustment.Gtk_Adjustment;
   --  Set the focus to the vertical adjustment.
   --  Adjustment should have been created and displayed at some other
   --  place in your application.
   --  Container will make sure that Adjustment always matches the range
   --  for the focus widget's position (y .. y + height).

   procedure Set_Focus_Hadjustment
     (Container  : access Gtk_Container_Record;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment);
   function Get_Focus_Hadjustment
     (Container : access Gtk_Container_Record)
      return Gtk.Adjustment.Gtk_Adjustment;
   --  Set the focus to the horizontal adjustment.
   --  Adjustment should have been created and displayed at some other
   --  place in your application.
   --  Container will make sure that Adjustment always matches the range
   --  for the focus widget's position (x .. x + width).

   procedure Set_Focus_Child
     (Container : access Gtk_Container_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class);
   function Get_Focus_Child
     (Container : access Gtk_Container_Record) return Gtk.Widget.Gtk_Widget;
   --  Emit a "set_focus_child" signal, to set the child that currently has the
   --  keyboard focus.

   ----------------
   -- Properties --
   ----------------

   procedure Child_Set_Property
     (Container     : access Gtk_Container_Record;
      Child         : access Gtk.Widget.Gtk_Widget_Record'Class;
      Property_Name : String;
      Value         : Glib.Values.GValue);
   procedure Child_Get_Property
     (Container     : access Gtk_Container_Record;
      Child         : access Gtk.Widget.Gtk_Widget_Record'Class;
      Property_Name : String;
      Value         : out Glib.Values.GValue);
   --  Sets or Gets the value of a child property for Child and Container. This
   --  is property set at the container level, and that applies to all children
   --  of that container. These are special type of properties, different from
   --  the properties associated with each type of widget.
   --  See also Gtk.Widget.Child_Notify
   --  You should use Glib.Property_Name to get the name from the property
   --  declaration in each of the GtkAda packages

   function Class_Find_Child_Property
     (Cclass        : Glib.Object.GObject_Class;
      Property_Name : String) return Glib.Param_Spec;
   --  Finds a child property of a container class by name. The returned value
   --  describes the property (type, allowed range, description,...)
   --  You should use Glib.Property_Name to get the name from the property
   --  declaration in each of the GtkAda packages

   procedure Class_Install_Child_Property
     (Cclass      : Glib.Object.GObject_Class;
      Property_Id : Guint;
      Pspec       : Glib.Param_Spec);
   --  Installs a child property on a container class.
   --  The Property_Id is an custom id that you choose for your class. It will
   --  be used in signals that set or get the property, instead of passing
   --  around a string.

   function Class_List_Child_Properties
     (Cclass : Glib.Object.GObject_Class) return Glib.Param_Spec_Array;
   --  Returns all child properties of a container class.

   ----------------------
   -- Forall functions --
   ----------------------

   type Gtk_Callback is
     access procedure (Item : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Function that can be call for each child of a container.
   --  This is called automatically by the Forall subprogram below.

   procedure Forall
     (Container : access Gtk_Container_Record;
      Func      : Gtk_Callback);
   --  Invokes Func on each child of Container, including children that are
   --  considered "internal" (implementation details of the container).
   --  "Internal" children generally weren't added by the user of the
   --  container, but were added by the container implementation itself. See
   --  Gtk.Widget.Set_Composite_Name.
   --  Most applications should use gtk_container_foreach(), rather than
   --  gtk_container_forall().
   --  See also the generic package Forall_Pkg if you want to pass some
   --  extra data to Func.

   procedure Foreach
     (Container : access Gtk_Container_Record;
      Func      : Gtk_Callback);
   --  Invokes Func on each non-internal child of Container. See Forall for
   --  details on what constitutes an "internal" child.

   --  <doc_ignore>
   generic
      type Data_Type (<>) is private;
   package For_Pkg is
      type Gtk_Callback is
        access procedure (Item : access Gtk.Widget.Gtk_Widget_Record'Class;
                          Data : in out Data_Type);
      procedure Forall
        (Container : access Gtk_Container_Record;
         Func      : Gtk_Callback;
         Data      : Data_Type);
      --  Execute Func for each of the children of Container, including
      --  internal ones

      procedure Foreach
        (Container : access Gtk_Container_Record;
         Func      : Gtk_Callback;
         Data      : Data_Type);
      --  Execute Func for each of the children of Container, not including
      --  internal ones
   end For_Pkg;
   --  </doc_ignore>

   --------------------------
   -- Widget-level methods --
   --------------------------

   procedure Set_Reallocate_Redraws
     (Container     : access Gtk_Container_Record;
      Needs_Redraws : Boolean := False);
   --  Set the "needs_redraws" field.
   --  If Needs_Redraws is True, then a "draw" signal is emitted for the
   --  Container whenever one is emitted for a child.

   function Child_Type
     (Container : access Gtk_Container_Record) return Gtk.Gtk_Type;
   --  Return the type of the children in Container.
   --  If Container can contain any type of widget, Gtk_Type_None is
   --  returned.

   procedure Resize_Children (Container : access Gtk_Container_Record);
   --  The container hasn't changed size but one of its children
   --  queued a resize request. Which means that the allocation
   --  is not sufficient for the requisition of some child.
   --  Run through the list of widgets and reallocate their size appropriately.

   ----------------------
   -- Signals emission --
   ----------------------

   procedure Check_Resize (Container : access Gtk_Container_Record);
   --  Emit the "check_resize" signal

   -----------------
   -- Obsolescent --
   -----------------
   --  All subprograms below are now obsolescent in gtk+. They might be removed
   --  from future versions of gtk+ (and therefore GtkAda).
   --  To find out whether your code uses any of these, we recommend compiling
   --  with the -gnatwj switch
   --  <doc_ignore>

   function Children
     (Container : access Gtk_Container_Record)
      return Gtk.Widget.Widget_List.Glist
      renames Get_Children;
   --  pragma Obsolescent;

   --  </doc_ignore>

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  - Name:  Border_Width_Property
   --    Type:  Guint
   --    Flags: read-write
   --    Descr: The width of the empty border outside the containers children.
   --    See also:  Set_Border_Width
   --
   --  - Name:  Resize_Mode_Property
   --    Type:  Gtk_Resize_Mode
   --    Flags: read-write
   --    Descr: Specify how resize events are handled
   --    See also:  Set_Resize_Mode
   --
   --  - Name:  Child_Property
   --    Type:  Widget
   --    Flags: writable
   --    Descr: Can be used to add a new child to the container.
   --    See also:  Add
   --
   --  - Name:  Reallocate_Redraws_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether redraws should be reallocated
   --    See also: Set_Reallocate_Redraws
   --  </properties>

   Border_Width_Property       : constant Glib.Properties.Property_Uint;
   Resize_Mode_Property        : constant Gtk.Enums.Property_Gtk_Resize_Mode;
   Child_Property              : constant Glib.Properties.Property_Object_WO;
   Reallocate_Redraws_Property : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "add"
   --    procedure Handler (Container : access Gtk_Container_Record'Class;
   --                       Widget    : access Gtk_Widget_Record'Class);
   --
   --    A new widget is added to the container
   --
   --  - "remove"
   --    procedure Handler (Container : access Gtk_Container_Record'Class;
   --                       Widget    : access Gtk_Widget_Record'Class);
   --
   --    A widget is removed from the container
   --
   --  - "check_resize"
   --    procedure Handler (Container : access Gtk_Container_Record'Class);
   --
   --    Called every time the Container needs resizing.
   --    Upon receiving this signal, Container should check whether it needs
   --    to be resized, and if it does should queue a resize request.
   --
   --  - "focus"
   --    procedure Handler (Container : access Gtk_Container_Record'Class;
   --                       Direction : Gtk_Direction_Type);
   --
   --    Moves the current selection to a new widget.
   --
   --  - "set-focus-child"
   --    procedure Handler (Container : access Gtk_Container_Record'Class;
   --                       Widget    : access Gtk_Widget_Record'Class);
   --
   --    Emitted when a new widget gains the focus.
   --
   --  </signals>

   Signal_Add             : constant Glib.Signal_Name := "add";
   Signal_Check_Resize    : constant Glib.Signal_Name := "check_resize";
   Signal_Remove          : constant Glib.Signal_Name := "remove";
   Signal_Set_Focus_Child : constant Glib.Signal_Name := "set-focus-child";

private
   type Gtk_Container_Record is new Gtk.Widget.Gtk_Widget_Record
     with null record;

   Border_Width_Property       : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("border_width");
   Resize_Mode_Property        : constant Gtk.Enums.Property_Gtk_Resize_Mode :=
     Gtk.Enums.Build ("resize_mode");
   Child_Property              : constant Glib.Properties.Property_Object_WO :=
     Glib.Properties.Build ("child");
   Reallocate_Redraws_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("reallocate_redraws");

   pragma Import (C, Get_Type, "gtk_container_get_type");
end Gtk.Container;

--  No binding: gtk_container_child_get_valist
--  No binding: gtk_container_child_set_valist
--  No binding: gtk_container_child_set
--  No binding: gtk_container_child_get
--  No binding: gtk_container_add_with_properties

--  These functions never had a binding, but are now obsolescent
--  No binding: gtk_container_foreach_full

--  Bound using C glue function:
--  No binding: gtk_container_get_focus_child
