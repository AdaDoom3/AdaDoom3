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
--  This widget is the base of the tree for displayable objects.
--  (A displayable object is one which takes up some amount
--  of screen real estate). It provides a common base and interface
--  which actual widgets must adhere to.
--
--  This package provides some services which might have been more appropriate
--  in some other packages, but could not because of dependency circularities
--  (there are for instance some functions relating to colors and colormaps).
--  We have tried to reference these functions in the other packages as well.
--
--  </description>
--  <c_version>2.16.6</c_version>
--  <group>Abstract base classes</group>

with System;
with Glib.Object;
with Glib.Properties;
with Glib.Glist;
pragma Elaborate_All (Glib.Glist);
with Glib.GSlist;
pragma Elaborate_All (Glib.GSlist);
with Glib.Values;

with Pango.Context;
with Pango.Font;
with Pango.Layout;

with Gdk.Color;
with Gdk.Event;
with Gdk.Bitmap;
with Gdk.Rectangle;
with Gdk.Region;
with Gdk.Pixbuf;
with Gdk.Pixmap;
with Gdk.Types;
with Gdk.Visual;
with Gdk.Window;

with Gtk.Accel_Group;
with Gtk.Adjustment;
with Gtk.Enums;
with Gtk.Style;

package Gtk.Widget is
   type Gtk_Widget_Record is new Glib.Object.GObject_Record with private;
   type Gtk_Widget is access all Gtk_Widget_Record'Class;

   type Gtk_Requisition is record
      Width  : Gint;
      Height : Gint;
   end record;
   --  Gtk_Requisition is the desired amount of screen real-estate a widget
   --  requests to the server. Its real allocated size might be different.
   --  See the section in the GtkAda user guide on how to create new widgets
   --  in Ada, and the examples/base_widget directory for an example on how to
   --  use this.
   pragma Convention (C, Gtk_Requisition);

   type Gtk_Requisition_Access is access all Gtk_Requisition;
   pragma Convention (C, Gtk_Requisition_Access);
   --  This type is used to create new widgets.

   type Gtk_Allocation is record
      X      : Gint;
      Y      : Gint;
      Width  : Allocation_Int;
      Height : Allocation_Int;
   end record;
   --  Gtk_Allocation indicates a size and position a widget was allocated.
   --  See the section in the user guide on how to create new widgets for more
   --  information.
   pragma Convention (C, Gtk_Allocation);

   type Gtk_Allocation_Access is access all Gtk_Allocation;
   pragma Convention (C, Gtk_Allocation_Access);
   --  This type is used to create new widgets.

   function Get_Type return Glib.GType;
   --  Return the internal type associated with a Gtk_Widget.

   function Requisition_Get_Type return Glib.GType;
   --  Return the internal type for a Gtk_Requisition

   --------------------------------------
   -- Definitions for lists of widgets --
   --------------------------------------

   --  <doc_ignore>
   function Convert (W : Gtk_Widget) return System.Address;
   function Convert (W : System.Address) return Gtk_Widget;
   --  </doc_ignore>

   package Widget_List is new Glib.Glist.Generic_List (Gtk_Widget);
   package Widget_SList is new Glib.GSlist.Generic_SList (Gtk_Widget);

   -------------------------
   -- Widgets' life cycle --
   -------------------------

   procedure Destroy (Widget : access Gtk_Widget_Record);
   --  Destroy the widget.
   --  This emits a "destroy" signal, calls all your handlers, and then
   --  unconnects them all. The object is then unref-ed, and if its reference
   --  count goes down to 0, the memory associated with the object and its
   --  user data is freed.
   --  Note that when you destroy handlers are called, the user_data is still
   --  available.
   --
   --  When a widget is destroyed, it will break any references it holds to
   --  other objects. If the widget is inside a container, the widget will be
   --  removed from the container. If the widget is a toplevel (derived from
   --  Gtk_Window), it will be removed from the list of toplevels, and the
   --  reference GTK+ holds to it will be removed. Removing widget from its
   --  container or the list of toplevels results in the widget being
   --  finalized, unless you've added additional references to the widget with
   --  Ref.
   --
   --  In most cases, only toplevel widgets (windows) require explicit
   --  destruction, because when you destroy a toplevel its children will be
   --  destroyed as well.

   procedure Destroy_Cb (Widget : access Gtk_Widget_Record'Class);
   --  This function should be used as a callback to destroy a widget.
   --  All it does is call Destroy on its argument, but its profile is
   --  compatible with the handlers found in Gtk.Handlers.

   procedure Unparent (Widget : access Gtk_Widget_Record'Class);
   --  This function is only for use in widget implementations.
   --  Should be called by implementations of the remove method
   --  on Gtk_Container, to dissociate a child from the container.
   --  Users should call Remove instead.
   --  This function might be dangereous: it correctly updates widget to
   --  reflect that it no longer belongs to its parent, however the parent
   --  keeps an internal pointer to the widget, which will result in a
   --  storage_error if you try to further access it.

   procedure Show (Widget : access Gtk_Widget_Record);
   --  Schedule the widget to be displayed on the screen when its parent is
   --  also shown (emits the "show" signal).
   --  If its ancestors are already mapped to the screen, then the widget is
   --  immediately displayed through a call to Map below.

   procedure Show_Now (Widget : access Gtk_Widget_Record);
   --  Show the widget.
   --  If it is an unmapped toplevel widget, wait for it to be mapped. This
   --  creates a recursive main_loop.

   procedure Hide (Widget : access Gtk_Widget_Record);
   --  Hide the widget from the screen (emits the "hide" signal).
   --  If Widget was visible, it is immediately hidden.
   --  If one of its ancestor is later shown on the screen, Widget won't
   --  appear.
   --  Note that on some window managers, including CDE, hiding an iconified
   --  window will not do anything. You should in addition call
   --  Gdk.Window.Withdraw to make sure the window is properly hidden.

   procedure Show_All (Widget : access Gtk_Widget_Record);
   --  Show Widget and all its children recursively.
   --  See also Set_Child_Visible below

   procedure Hide_All (Widget : access Gtk_Widget_Record);
   --  Hide Widget and all its children.
   --  Note that if you simply want to delete Widget from the screen, you can
   --  simply call the Hide subprogram on it. This procedure Hide_All should
   --  only be used if you want to unschedule a widget to be displayed later,
   --  not to remove an actual widget from the screen.
   --  See also Set_Child_Visible below.

   procedure Set_No_Show_All
     (Widget : access Gtk_Widget_Record; No_Show_All : Boolean);
   function Get_No_Show_All
     (Widget : access Gtk_Widget_Record) return Boolean;
   --  Sets the "no_show_all" property, which determines whether calls to
   --  Show_All() and Hide_All() will affect this widget.
   --  This is mostly for use in constructing widget hierarchies with
   --  externally controlled visibility.

   procedure Map (Widget : access Gtk_Widget_Record);
   --  Map a widget to the screen.
   --  A window is created for it on the screen (through a call to Realize) and
   --  Widget is then drawn on the screen (if its ancestors are also mapped).
   --  This function is recursive and will also map all the children of Widget.
   --
   --  It is recommended to use the higher-level Show instead.

   procedure Unmap (Widget : access Gtk_Widget_Record);
   --  Unmap a widget from the screen.
   --  This results in the widget being hidden, but not destroyed. It can be
   --  shown again any time through a call to Map (provided its ancestors are
   --  also mapped).
   --
   --  It is recommended to use the higher-level Hide instead.

   procedure Realize (Widget : access Gtk_Widget_Record);
   --  Create a window for Widget and its ancestors (emit the "realize" signal)
   --  This does not mean that the widget will appear on the screen, but
   --  resources such as colormaps, etc. become available.
   --  Some routines require that the widget is realized before any call.
   --  You must set the Event_Mask before calling this routine if you want to
   --  change it from its default value.

   procedure Unrealize (Widget : access Gtk_Widget_Record);
   --  Hide the widget from the screen and deletes the associated window.
   --  This does not destroy the widget itself, only its server-side
   --  resources.

   generic
      type Widget_Type is new Gtk_Widget_Record with private;
      with procedure Realize_Proc (Widget : access Widget_Type'Class);
   package Realize_Handling is

      procedure Set_Realize (Widget : access Gtk_Widget_Record'Class);
      --  Set the realize handler at the low level.
      --  This is needed to replace the default realize in new widgets.

   private
      --  <doc_ignore>
      procedure Internal_Realize (Widget : System.Address);
      --  The wrapper passed to Gtk+.
      pragma Convention (C, Internal_Realize);
      --  </doc_ignore>
   end Realize_Handling;

   function Hide_On_Delete (Widget : access Gtk_Widget_Record'Class)
      return Boolean;
   --  Hide widget and return True.
   --  This function is intended to be used as a callback.

   procedure Set_Child_Visible
     (Widget : access Gtk_Widget_Record; Is_Visible : Boolean);
   function Get_Child_Visible
     (Widget : access Gtk_Widget_Record) return Boolean;
   --  Sets whether Widget should be mapped along with its parent when its
   --  parent is mapped and Widget has been shown with Show.
   --
   --  "mapped" indicates the moment the window is actually shown on the
   --  screen. Show and Hide indicate your intention to show Widget on the
   --  scree or not, but if the parent of Widget is itself not shown at that
   --  time, the two commands Show and Hide have no immediate effect, and just
   --  set a flag to save your intent.
   --  Set_Child_Visible indicates that the widget shouldn't be part of the
   --  recursive processing done by Show_All and Hide_All on the parent. You
   --  have decided once and for all what the behavior should be, and you don't
   --  want it to be changed by future calls to Show_All and Hide_All.
   --
   --  The child visibility can be set for widget before it is added to a
   --  container with Set_Parent, to avoid mapping children unnecessary before
   --  immediately unmapping them. However it will be reset to its default
   --  state of True when the widget is removed from a container.
   --
   --  Note that changing the child visibility of a widget does not
   --  queue a resize on the widget. Most of the time, the size of
   --  a widget is computed from all visible children, whether or
   --  not they are mapped. If this is not the case, the container
   --  can queue a resize itself.
   --
   --  This function is only useful for container implementations and
   --  should generally not be called by an application.

   function Has_Screen (Widget : access Gtk_Widget_Record) return Boolean;
   --  Checks whether there is a Gdk_Screen is associated with
   --  this widget. All toplevel widgets have an associated
   --  screen, and all widgets added into a hierarchy with a toplevel
   --  window at the top.

   ----------------------
   -- Drawing a widget --
   ----------------------

   procedure Queue_Draw (Widget : access Gtk_Widget_Record);
   --  Add a drawing request to the event queue for the whole widget.
   --  This is more efficient than calling Draw directly, since GtkAda groups
   --  drawing requests as much as possible to speed up the drawing process.
   --  The actual drawing will take place as soon as GtkAda is not busy
   --  processing other events, but before idle events.

   procedure Queue_Draw_Area
     (Widget : access Gtk_Widget_Record;
      X      : Gint;
      Y      : Gint;
      Width  : Gint;
      Height : Gint);
   --  Add a drawing request to the event queue for part of the widget.
   --  This is more efficient that calling Draw directly (see Queue_Draw).

   procedure Queue_Resize (Widget : access Gtk_Widget_Record);
   --  Queue drawing requests after a resizing of the widget.
   --  This clears the widget, and its parent if any, so that everything is
   --  correctly redrawn.
   --  You should not have to call this function directly.
   --  For a Gtk_Window, check the procedure Gtk.Window.Resize instead.

   procedure Queue_Resize_No_Redraw (Widget : access Gtk_Widget_Record);
   --  This function works like Queue_Resize(), except that the
   --  widget is not invalidated (ie will not be redrawn)

   function Create_Pango_Context
     (Widget : access Gtk_Widget_Record) return Pango.Context.Pango_Context;
   --  Create a new Pango_Context with the appropriate colormap, font
   --  description, and base direction for drawing text for this widget. See
   --  also Get_Pango_Context.
   --  The returned context must be freed by the caller.

   function Create_Pango_Layout
     (Widget : access Gtk_Widget_Record; Text : UTF8_String := "")
      return Pango.Layout.Pango_Layout;
   --  Return a new pango_layout that displays Text. This fully handles
   --  internationalization, and should be the preferred way to display text,
   --  rather than Gdk.Drawable.Draw_Text
   --  Text must be a valid Utf8 text, see Glib.Convert.

   -----------------------
   -- Size and position --
   -----------------------

   procedure Size_Request
     (Widget      : access Gtk_Widget_Record;
      Requisition : in out Gtk_Requisition);
   --  Emit a "size_request" event for the widget

   procedure Set_Size_Request
     (Widget      : access Gtk_Widget_Record;
      Width, Height : Gint := -1);
   procedure Get_Size_Request
     (Widget        : access Gtk_Widget_Record;
      Width, Height : out Gint);
   --  Sets the minimum size of a widget; that is, the widget's size request
   --  will be Width by Height. You can use this function to force a widget to
   --  be either larger or smaller than it normally would be.
   --
   --  In most cases, Set_Default_Size is a better choice for toplevel windows
   --  than this function; setting the default size will still allow users to
   --  shrink the window. Setting the size request will force them to leave the
   --  window at least as large as the size request. When dealing with window
   --  sizes, Gtk.Windo.Set_Geometry_Hints can be a useful function as well.
   --
   --  Note the inherent danger of setting any fixed size - themes,
   --  translations into other languages, different fonts, and user action can
   --  all change the appropriate size for a given widget. So, it's basically
   --  impossible to hardcode a size that will always be correct.
   --
   --  The size request of a widget is the smallest size a widget can accept
   --  while still functioning well and drawing itself correctly.  However in
   --  some strange cases a widget may be allocated less than its requested
   --  size, and in many cases a widget may be allocated more space than it
   --  requested.
   --
   --  If the size request in a given direction is -1 (unset), then
   --  the "natural" size request of the widget will be used instead.
   --
   --  Widgets can't actually be allocated a size less than 1 by 1, but
   --  you can pass 0,0 to this function to mean "as small as possible."

   procedure Size_Allocate
     (Widget     : access Gtk_Widget_Record;
      Allocation : Gtk_Allocation);
   --  Emit a "size_allocate" event for the widget.
   --  Allocation'size is first constrained to a range between 1x1 and
   --  32767x32767.
   --  A clear and draw request is also queued if required.

   function Get_Child_Requisition
     (Widget : access Gtk_Widget_Record) return Gtk_Requisition;
   --  Return the size requests by the widget.
   --  This is the ideal size for the widget, not necessarily its actual size.
   --  See the user guide's section on how to create new widgets for more
   --  information on the size requisition and allocation.

   function Get_Allocation_Width
     (Widget : access Gtk_Widget_Record) return Allocation_Int;
   --  Return the current width of the widget.

   function Get_Allocation_Height
     (Widget : access Gtk_Widget_Record) return Allocation_Int;
   --  Return the current height of the widget.

   function Get_Allocation_X (Widget : access Gtk_Widget_Record) return Gint;
   --  Return the current position of the widget, relative to its parent.

   function Get_Allocation_Y (Widget : access Gtk_Widget_Record) return Gint;
   --  Return the current position of the widget, relative to its parent.

   procedure Set_Redraw_On_Allocate
     (Widget             : access Gtk_Widget_Record;
      Redraw_On_Allocate : Boolean);
   --  Sets whether the entire widget is queued for drawing when its size
   --  allocation changes. By default, this setting is %TRUE and the entire
   --  widget is redrawn on every size change. If your widget leaves the upper
   --  left unchanged when made bigger, turning this setting on will improve
   --  performance. Note that for %NO_WINDOW widgets setting this flag to
   --  %FALSE turns off all allocation on resizing: the widget will not even
   --  redraw if its position changes; this is to allow containers that don't
   --  draw anything to avoid excess invalidations. If you set this flag on
   --  %NO_WINDOW widget that *does* draw on Get_Window (Widget), you are
   --  responsible for invalidating both the old and new allocation of the
   --  widget when the widget is moved and responsible for invalidating regions
   --  newly when the widget increases size.

   ------------------
   -- Accelerators --
   ------------------

   procedure Add_Accelerator
     (Widget       : access Gtk_Widget_Record;
      Accel_Signal : Glib.Signal_Name;
      Accel_Group  : Gtk.Accel_Group.Gtk_Accel_Group;
      Accel_Key    : Gdk.Types.Gdk_Key_Type;
      Accel_Mods   : Gdk.Types.Gdk_Modifier_Type;
      Accel_Flags  : Gtk.Accel_Group.Gtk_Accel_Flags);
   --  Add a new accelerator for the widget.
   --  The signal Accel_Signal will be sent to Widget when the matching
   --  key is pressed and the widget has the focus.
   --  Consider using Gtk.Accel_Map.Add_Entry instead, which is compatible with
   --  interactive change of accelerators by the user.

   procedure Remove_Accelerator
     (Widget       : access Gtk_Widget_Record;
      Accel_Group  : Gtk.Accel_Group.Gtk_Accel_Group;
      Accel_Key    : Gdk.Types.Gdk_Key_Type;
      Accel_Mods   : Gdk.Types.Gdk_Modifier_Type);
   --  Remove an accelerator for the widget.

   function Can_Activate_Accel
     (Widget    : access Gtk_Widget_Record;
      Signal_Id : Gulong) return Boolean;
   --  Determines whether an accelerator that activates the signal identified
   --  by Signal_Id can currently be activated. This is done by emitting the
   --  GtkWidget::can-activate-accel signal on Widget; if the signal isn't
   --  overridden by handler or in a derived widget, then the default check is
   --  that the widget must be sensitive, and the widget and all its ancestors
   --  mapped.
   --  Signal_Id comes from the value returned by Gtk.Handlers.Connect

   procedure Set_Accel_Path
     (Widget     : access Gtk_Widget_Record;
      Accel_Path : UTF8_String;
      Group      : Gtk.Accel_Group.Gtk_Accel_Group);
   --  Set the path that will be used to reference the widget in calls to the
   --  subprograms in Gtk.Accel_Map. This means, for instance, that the widget
   --  is fully setup for interactive modification of the shortcuts by the
   --  user, should he choose to activate this possibility in his themes (see
   --  gtk-accel_map.ads for more information).

   function List_Mnemonic_Labels
     (Widget : access Gtk_Widget_Record)
      return Widget_List.Glist;
   --  Returns a newly allocated list of the widgets, normally labels, for
   --  which this widget is a the target of a mnemonic (see for example,
   --  gtk.label.set_mnemonic_widget).
   --  The widgets in the list are not individually referenced. If you want to
   --  iterate through the list and perform actions involving callbacks that
   --  might destroy the widgets, you must call Ref first, and then unref all
   --  the widgets afterwards.
   --  The caller must free the returned list.

   procedure Add_Mnemonic_Label
     (Widget : access Gtk_Widget_Record;
      Label  : access Gtk_Widget_Record'Class);
   --  Adds a widget to the list of mnemonic labels for this widget. (See
   --  List_Mnemonic_Labels). Note the list of mnemonic labels for the widget
   --  is cleared when the widget is destroyed, so the caller must make sure to
   --  update its internal state at this point as well, by using a connection
   --  to the ::destroy signal or a weak notifier.

   procedure Remove_Mnemonic_Label
     (Widget : access Gtk_Widget_Record;
      Label  : access Gtk_Widget_Record'Class);
   --  Removes a widget from the list of mnemonic labels for this widget. The
   --  widget must have previously been added to the list with
   --  Add_Mnemonic_Label.

   function Mnemonic_Activate
     (Widget        : access Gtk_Widget_Record;
      Group_Cycling : Boolean) return Boolean;
   --  Emits the signal "mnemonic_activate".
   --  In general (depending on what is connected to this signal), this results
   --  in calling the "activate" signal on the widget, as if a mnemonic had
   --  been used (when Group_Cycling if False), or to grab the focus on the
   --  widget when Group_Cycling is True)

   -------------------------
   --  Events and signals --
   -------------------------

   function Event
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event) return Boolean;
   --  Emit a signal on the widget.
   --  The exact signal depends on the event type (i.e. if the type is
   --  Gdk_Button_Press, then a "button_press" signal is emitted).

   procedure Send_Expose
     (Widget : access Gtk_Widget_Record;
      Event  : Gdk.Event.Gdk_Event_Expose);
   --  Emit an expose event signals on a widget.
   --  This function is not normally used directly. The only time it is used
   --  is when propagating an expose event to a child No_Window widget, and
   --  that is normally done using Gtk.Container.Propagate_Expose.
   --
   --  If you want to force an area of a window to be redrawn,
   --  use Gdk.Window.Invalidate_Rect or Gdk.Window.Invalidate_Region.
   --  To cause the redraw to be done immediately, follow that call
   --  with a call to Gdk.Window.Process_Updates.

   procedure Activate (Widget : access Gtk_Widget_Record);
   --  Emit an activate signal on the widget.
   --  The exact signal emitted depends on the widget type (i.e. for a
   --  Gtk_Button this emits a "clicked" signal, for a Gtk_Editable this emits
   --  the "activate" signal, ...).

   procedure Grab_Focus (Widget : access Gtk_Widget_Record);
   --  Emit the "grab_focus" signal for the widget.
   --  This is sent when the widget gets the focus. Its visual aspect might
   --  change.
   --  The "Can_Focus" flag must have been set first.
   --  See also Gtk.Widget.Child_Focus, which should be used instead when
   --  writting new widgets in Ada

   function Is_Focus (Widget : access Gtk_Widget_Record) return Boolean;
   --  Determines if the widget is the focus widget within its
   --  toplevel. (This does not mean that the HAS_FOCUS flag is
   --  necessarily set; HAS_FOCUS will only be set if the
   --  toplevel widget additionally has the global input focus)

   function Child_Focus
     (Child     : access Gtk_Widget_Record'Class;
      Direction : Gtk.Enums.Gtk_Direction_Type := Gtk.Enums.Dir_Tab_Forward)
      return Boolean;
   --  Used by custom widget implementations to indicate the focus child.
   --  If you're writing an app, you'd use Grab_Focus to move the focus to a
   --  particular widget, and Gtk.Container.Set_Focus_Chain to change the focus
   --  tab order. So you may want to investigate those functions instead.
   --
   --  Child_Focus is called by containers as the user moves around
   --  the window using keyboard shortcuts. Direction indicates what kind of
   --  motion is taking place (up, down, left, right, tab forward, tab
   --  backward). Child_Focus invokes the "focus" signal on Child;
   --  widgets override the default handler for this signal in order to
   --  implement appropriate focus behavior.
   --
   --  The "focus" default handler for a widget should return True if moving in
   --  Direction left the focus on a focusable location inside that widget, and
   --  False if moving in Direction moved the focus outside the widget. If
   --  returning True, widgets normally call Grab_Focus to place the
   --  focus accordingly; if returning False, they don't modify the current
   --  focus location.
   --
   --  This function replaces Gtk.Container.Focus from GTK+ 1.2.  It was
   --  necessary to check that the child was visible, sensitive, and focusable
   --  before calling Gtk.Container.Focus. Child_Focus returns False
   --  if the widget is not currently in a focusable state, so there's no need
   --  for those checks.
   --
   --  Return value: True if focus ended up inside Child

   procedure Set_Events
     (Widget : access Gtk_Widget_Record;
      Events : Gdk.Event.Gdk_Event_Mask);
   function Get_Events
     (Widget : access Gtk_Widget_Record) return Gdk.Event.Gdk_Event_Mask;
   --  Sets or gets the event mask for the widget.
   --  Widget should not have been realized before, or nothing is done.
   --  This is the only way you can explicitly get mouse or keyboards events on
   --  widgets that do not automatically get them, as for instance in a
   --  Gtk_Drawing_Area.

   procedure Add_Events
     (Widget : access Gtk_Widget_Record;
      Events : Gdk.Event.Gdk_Event_Mask);
   --  Add some events to the current event mask of the widget.

   procedure Set_Extension_Events
     (Widget : access Gtk_Widget_Record;
      Mode   : Gdk.Types.Gdk_Extension_Mode);
   function Get_Extension_Events
     (Widget : access Gtk_Widget_Record) return Gdk.Types.Gdk_Extension_Mode;
   --  Set the extension event mask for the widget.
   --  This is used to activate some special input modes for other devices than
   --  keyboard and mouse.

   function Default_Motion_Notify_Event
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event) return Gint;
   --  Access to the standard default callback for motion events:
   --  This is mainly used for rulers in Gtk.Ruler (See the example in
   --  testgtk, with create_rulers.adb)

   function Has_Default_Motion_Notify_Handler
     (Widget : access Gtk_Widget_Record'Class) return Boolean;
   --  Return True if Widget has a default handler for motion_notify events.
   --  Note that the function Default_Motion_Notify_Event should not be called
   --  if this one returns False, since it would create a segmentation fault.

   procedure Error_Bell (Widget : access Gtk_Widget_Record);
   --  Notifies the user about an input-related error on this widget.
   --  If the GtkSettings:gtk-error-bell setting is True, it calls
   --  Gdk_Window_Beep, otherwise it does nothing.
   --
   --  Note that the effect of Gdk_Window_Beep can be configured in many
   --  ways, depending on the windowing backend and the desktop environment
   --  or window manager that is used.

   function Keynav_Failed
     (Widget    : access Gtk_Widget_Record;
      Direction : Gtk.Enums.Gtk_Direction_Type)
      return Boolean;
   --  This function should be called whenever keyboard navigation within
   --  a single widget hits a boundary. The function emits the
   --  GtkWidget::keynav-failed signal on the widget and its return
   --  value should be interpreted in a way similar to the return value of
   --  Child_Focus.
   --
   --  When True is returned, stay in the widget, the failed keyboard
   --  navigation is Ok and/or there is nowhere we can/should move the
   --  focus to.
   --
   --  When False is returned, the caller should continue with keyboard
   --  navigation outside the widget, e.g. by calling Child_Focus on the
   --  widget's toplevel.
   --
   --  The default ::keynav-failed handler returns True for
   --  Dir_Tab_Forward and Dir_Tab_Backward. For the other values of
   --  Gtk_Direction_Type, it looks at the
   --  GtkSettings:gtk-keynav-cursor-only setting and returns False
   --  if the setting is True. This way the entire user interface
   --  becomes cursor-navigatable on input devices such as mobile phones
   --  which only have cursor keys but no tab key.
   --
   --  Whenever the default handler returns True, it also calls
   --  Error_Bell to notify the user of the failed keyboard navigation.
   --
   --  A use case for providing an own implementation of ::keynav-failed
   --  (either by connecting to it or by overriding it) would be a row of
   --  Gtk_Entry widgets where the user should be able to navigate the
   --  entire row with the cursor keys, as e.g. known from user interfaces
   --  that require entering license keys.

   --------------------------
   -- Colors and colormaps --
   --------------------------

   procedure Set_Colormap
     (Widget : access Gtk_Widget_Record;
      Cmap   : Gdk.Color.Gdk_Colormap);
   function Get_Colormap
     (Widget : access Gtk_Widget_Record) return Gdk.Color.Gdk_Colormap;
   --  Modify the colormap of the widget.
   --  The widget must not have been realized when you set the colormap.
   --  The colormap is generally the same one for all widget, but might be
   --  different if for instance Gtk_Drawing_Area needs to display some
   --  different colors on a screen that only has a limited amount of colors.

   function Get_Visual
     (Widget : access Gtk_Widget_Record) return Gdk.Visual.Gdk_Visual;
   --  Get the visual used for the widget.
   --  I.e. the structure that indicates the depth of the widget (number of
   --  bits per pixel), and some information used internally by GtkAda to
   --  handle colors and colormaps.

   procedure Push_Colormap (Cmap : Gdk.Color.Gdk_Colormap);
   procedure Pop_Colormap;
   --  Modify temporarily the default colormap set for newly created widgets.
   --  You should use this in pair with Pop_Colormap (Push the new value,
   --  create the widget, and pop the value).

   procedure Set_Default_Colormap (Cmap : Gdk.Color.Gdk_Colormap);
   function Get_Default_Colormap return Gdk.Color.Gdk_Colormap;
   --  Modify permanently the default colormap used when a widget is created.
   --  If you only want to modify this colormap temporarily for a few widgets,
   --  you should consider using Push_Colormap and Pop_Colormap instead.
   --  See also Gdk.Screen.Get_Default_Colormap for a multihead-aware version

   function Get_Default_Visual return Gdk.Visual.Gdk_Visual;
   --  Return the default visual used when a new widget is created.

   function Is_Composited (Widget : access Gtk_Widget_Record) return Boolean;
   --  Returns whether Widget can rely on having its alpha channel
   --  drawn correctly. On X11 this function returns whether a
   --  compositing manager is running for Widget's screen.
   --
   --  Please note that the semantics of this call will change
   --  in the future if used on a widget that has a composited
   --  window in its hierarchy (as set by Gdk.Window.Set_Composited).

   ------------
   -- Styles --
   ------------

   procedure Set_Style
     (Widget : access Gtk_Widget_Record;
      Style  : Gtk.Style.Gtk_Style);
   function Get_Style (Widget : access Gtk_Widget_Record)
     return Gtk.Style.Gtk_Style;
   --  Set or get the style for a given widget.
   --  See also Gtk.Rc.Modify_Style

   function Get_Default_Style return Gtk.Style.Gtk_Style;
   --  Get the default global style.

   procedure Ensure_Style (Widget : access Gtk_Widget_Record);
   --  Make sure that the widget has a style associated to it.
   --  Either the default one as set by Set_Default_Style above or one set by
   --  the user with Set_Style.

   procedure Restore_Default_Style (Widget : access Gtk_Widget_Record);
   --  Restore the default style that was set for the widget.
   --  The default style is the first one that was set either by a call
   --  to Set_Style or Set_Default_Style.

   procedure Reset_Rc_Styles (Widget : access Gtk_Widget_Record);
   --  Restore the Rc style recursively for widget and its children.

   function Get_Pango_Context (Widget : access Gtk_Widget_Record)
      return Pango.Context.Pango_Context;
   --  Get a Pango_Context with the appropriate colormap, font description and
   --  base direction for this widget. Unlike the context returned by
   --  Create_Pango_Context, this context is owned by the widget (it can be
   --  used as long as widget exists), and will be updated to match any changes
   --  to the widget's attributes.
   --
   --  If you create and keep a Pango_Layout using this context, you must deal
   --  with changes to the context by calling Pango_Layout.Context_Changed on
   --  the layout in response to the ::style_set and ::direction_set signals
   --  for the widget.

   procedure Modify_Fg
     (Widget     : access Gtk_Widget_Record;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color);
   --  Sets the foreground color for a widget in a particular state.  All
   --  other style values are left untouched.

   procedure Modify_Bg
     (Widget     : access Gtk_Widget_Record;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color);
   --  Sets the background color for a widget in a particular state.  All
   --  other style values are left untouched. This procedure has no effect
   --  when Widget has no physical window associated to it (for instance
   --  a Gtk_Label). In such cases, you must put widget inside a
   --  Gtk_Event_Box, and set the background color of the box itself.

   procedure Modify_Text
     (Widget     : access Gtk_Widget_Record;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color);
   --  Sets the text color for a widget in a particular state.  All other
   --  style values are left untouched. The text color is the foreground
   --  color used along with the base color (see Modify_Base)
   --  for widgets such as Gtk_Entry and Gtk_Text_View.
   --
   --  Note that this will not work with a Gtk_Button. Modify_Fg should be
   --  called on the button's label in order to set the color of its label.
   --  For example, assuming a simple button with a label attached to it:
   --
   --     Modify_Fg (Get_Child (My_Button), My_State, My_New_Color);

   procedure Modify_Base
     (Widget     : access Gtk_Widget_Record;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color);
   --  Sets the base color for a widget in a particular state.
   --  All other style values are left untouched. The base color
   --  is the background color used along with the text color
   --  (see Modify_Text) for widgets such as Gtk_Entry and Gtk_Text_View.

   procedure Modify_Font
     (Widget : access Gtk_Widget_Record;
      Desc   : Pango.Font.Pango_Font_Description);
   --  Modify the font used for the widget.
   --  Desc must be freed by the caller to avoid memory leaks

   procedure Set_Default_Direction (Dir : Gtk.Enums.Gtk_Text_Direction);
   function Get_Default_Direction return Gtk.Enums.Gtk_Text_Direction;
   --  Obtains the current default reading direction. See
   --  Set_Default_Direction().

   procedure Set_Direction
     (Widget : access Gtk_Widget_Record; Dir : Gtk.Enums.Gtk_Text_Direction);
   function Get_Direction
     (Widget : access Gtk_Widget_Record) return Gtk.Enums.Gtk_Text_Direction;
   --  Sets the reading direction on a particular widget. This direction
   --  controls the primary direction for widgets containing text,
   --  and also the direction in which the children of a container are
   --  packed. The ability to set the direction is present in order
   --  so that correct localization into languages with right-to-left
   --  reading directions can be done. Generally, applications will
   --  let the default reading direction present, except for containers
   --  where the containers are arranged in an order that is explicitely
   --  visual rather than logical (such as buttons for text justification).
   --
   --  If the direction is set to TEXT_DIR_NONE, then the value
   --  set by Set_Default_Direction will be used.

   procedure Modify_Cursor
     (Widget    : access Gtk_Widget_Record;
      Primary   : Gdk.Color.Gdk_Color;
      Secondary : Gdk.Color.Gdk_Color);
   --  Sets the cursor color to use in a widget, overriding the
   --  GtkWidget:cursor-color and GtkWidget:secondary-cursor-color
   --  style properties. All other style values are left untouched.
   --  See also Gtk.Rc.Modify_Style.

   -------------------
   -- Widgets' tree --
   -------------------

   procedure Set_Name
     (Widget : access Gtk_Widget_Record;
      Name   : UTF8_String);
   --  Set the name for the widget.
   --  This name is used purely internally to identify the widget, and does not
   --  give any visual clue.

   function Get_Name (Widget : access Gtk_Widget_Record) return UTF8_String;
   --  Return the name of the widget if it was set by Set_Name.
   --  Return the name of its class otherwise.

   function Path          (Widget : access Gtk_Widget_Record) return String;
   function Path_Reversed (Widget : access Gtk_Widget_Record) return String;
   --  Obtains the full path to Widget. The path is simply the name of a
   --  widget and all its parents in the container hierarchy, separated by
   --  periods. The name of a widget comes from
   --  Get_Name. Paths are used to apply styles to a widget
   --  in gtkrc configuration files.  Widget names are the type of the
   --  widget by default (e.g. "GtkButton") or can be set to an
   --  application-specific value with Set_Name.  By setting
   --  the name of a widget, you allow users or theme authors to apply
   --  styles to that specific widget in their gtkrc
   --  file.
   --  Path_Reverse fills in the path in reverse order, starting with widget's
   --  name instead of starting with the name of the outermost ancestor.

   function Class_Path (Widget : access Gtk_Widget_Record) return String;
   function Class_Path_Reversed
     (Widget : access Gtk_Widget_Record) return String;
   --  Same as Path(), but always uses the name of a widget's type,
   --  never uses a custom name set with Set_Name.

   function Get_Ancestor
     (Widget        : access Gtk_Widget_Record;
      Ancestor_Type : Gtk_Type) return Gtk_Widget;
   --  Return the closest ancestor of Widget which is of type Ancestor_Type.
   --  Return null if there is none.

   procedure Set_Parent
     (Widget : access Gtk_Widget_Record;
      Parent : access Gtk_Widget_Record'Class);
   function Get_Parent (Widget : access Gtk_Widget_Record) return Gtk_Widget;
   --  Modify the parent for the widget.
   --  This is not the recommended way to do this, you should use
   --  Gtk.Container.Add or Gtk.Box.Pack_Start instead.

   procedure Set_Parent_Window
     (Widget : access Gtk_Widget_Record;
      Window : Gdk.Window.Gdk_Window);
   function Get_Parent_Window
     (Widget : access Gtk_Widget_Record) return Gdk.Window.Gdk_Window;
   --  Set the parent window for the actual Gdk_Window of the widget. This sets
   --  up required internal fields, and should be used only when you implement
   --  your own container, as opposed to using one of the standard containers.

   function Get_Toplevel (Widget : access Gtk_Widget_Record) return Gtk_Widget;
   --  This function returns the topmost widget in the container hierarchy
   --  Widget is a part of. If Widget has no parent widgets, it will be
   --  returned as the topmost widget.
   --
   --  Note the difference in behavior vs. Get_Ancestor:
   --  Get_Ancestor (Widget, GTK_TYPE_WINDOW) would return null
   --  if Widget wasn't inside a toplevel window, and if the
   --  window was inside a Gtk_Window-derived widget which was in turn
   --  inside the toplevel Gtk_Window. While the second case may
   --  seem unlikely, it actually happens when a Gtk_Plug is embedded
   --  inside a Gtk_Socket within the same application.
   --
   --  To reliably find the toplevel Gtk_Window, use
   --  Get_Toplevel and check if the "toplevel" flag
   --  is set on the result:
   --
   --  Toplevel := Get_Toplevel (Widget);
   --  if Top_Level_Is_Set (Toplevel) then
   --     [ Perform some action on Toplevel. ]
   --  end if;

   function Is_Ancestor
     (Widget   : access Gtk_Widget_Record;
      Ancestor : access Gtk_Widget_Record'Class) return Boolean;
   --  Return True if Ancestor is in the ancestor tree for Widget.
   --  I.e. if Widget is contained within Ancestor.

   procedure Reparent
     (Widget     : access Gtk_Widget_Record;
      New_Parent : access Gtk_Widget_Record'Class);
   --  Change the parent of the widget dynamically.
   --  If both the new parent and the widget are shown, then the widget is
   --  visually redrawn in its new parent.

   procedure Translate_Coordinates
     (Src_Widget  : Gtk_Widget;
      Dest_Widget : Gtk_Widget;
      Src_X       : Gint;
      Src_Y       : Gint;
      Dest_X      : out Gint;
      Dest_Y      : out Gint;
      Result      : out Boolean);
   --  Translate coordinates relative to Src_Widget's allocation to coordinates
   --  relative to Dest_Widget's allocations. In order to perform this
   --  operation, both widgets must be realized, and must share a common
   --  toplevel.
   --
   --  Result is set to False if either widget was not realized, or there
   --  was no common ancestor. In this case, nothing is stored in Dest_X and
   --  Dest_Y. Otherwise True.

   function Get_Root_Window
     (Widget : access Gtk_Widget_Record) return Gdk.Window.Gdk_Window;
   --  Get the root window where this widget is located. This function can only
   --  be called after the widget has been added to a widget hierarchy.
   --
   --  The root window is useful for such purposes as creating a popup
   --  Gdk_Window associated with the window. In general, you should only
   --  create display specific resources when a widget has been realized, and
   --  you should free those resources when the widget is unrealized.

   procedure Set_Composite_Name
     (Widget : access Gtk_Widget_Record; Name : String);
   function Get_Composite_Name
     (Widget : access Gtk_Widget_Record) return String;
   --  Sets or gets a widgets composite name. The widget must be
   --  a composite child of its parent; see Push_Composite_Child.

   procedure Push_Composite_Child;
   procedure Pop_Composite_Child;
   --  Makes all newly-created widgets as composite children until
   --  the corresponding Pop_Composite_Child call.
   --
   --  A composite child is a child that's an implementation detail of the
   --  container it's inside and should not be visible to people using the
   --  container. Composite children aren't treated differently by GTK (but
   --  see gtk.container.foreach() vs. gtk.container.forall()), but e.g. GUI
   --  builders might want to treat them in a different way.
   --
   --  Here is a simple example:
   --      Push_Composite_Child;
   --      Gtk_New (Scrolled_Window.Hscrollbar, Hadjustment);
   --      Set_Composite_Name (Scrolled_Window.Hscrollbar, "hscrollbar");
   --      Pop_Composite_Child;
   --      Set_Parent (Scrolled_Window.Hscrollbar, Scrolled_Window);
   --      Ref (Scrolled_Window.Hscrollbar);

   --------------------
   -- Misc functions --
   --------------------

   procedure Set_Scroll_Adjustments
     (Widget : access Gtk_Widget_Record;
      Hadj   : Gtk.Adjustment.Gtk_Adjustment;
      Vadj   : Gtk.Adjustment.Gtk_Adjustment);
   --  Emit the "set_scroll_adjustments" signal.
   --  The exact signal emitted depends on the widget type (see
   --  Glib.Object.Initialize_Class_Record).
   --  The handler creates the adjustments if null is passed as argument, and
   --  makes sure both adjustments are in the correct range.

   function Intersect
     (Widget       : access Gtk_Widget_Record;
      Area         : Gdk.Rectangle.Gdk_Rectangle;
      Intersection : access Gdk.Rectangle.Gdk_Rectangle) return Boolean;
   --  Return True if the widget intersects the screen area Area.
   --  The intersection area is returned in Intersection.

   function Region_Intersect
     (Widget : access Gtk_Widget_Record;
      Region : Gdk.Region.Gdk_Region)
      return Gdk.Region.Gdk_Region;
   --  Region must be in the same coordinate system as the widget's allocation,
   --  ie relative to the widget's window, or to the parent's window for
   --  No_Window widgets.
   --  Returns a newly allocated region. The coordinats are in the same system
   --  as described above.
   --  Computes the intersection of a Widget's area and Region, returning
   --  the intersection. The result may be empty, use gdk.region.empty to
   --  check.

   procedure Grab_Default (Widget : access Gtk_Widget_Record);
   --  The widget becomes the default widget for its parent window or dialog.
   --  All keyboard events will be sent to it if no other widget has the focus.
   --  Note that the "Can_Default" flag must have been set first on WIDGET.

   procedure Set_State
     (Widget : access Gtk_Widget_Record;
      State  : Enums.Gtk_State_Type);
   function Get_State
     (Widget : access Gtk_Widget_Record) return Enums.Gtk_State_Type;
   --  Modify the state of the widget.
   --  This modifies its visual aspect, and thus should be used only if you
   --  change its behavior at the same time, so as not to confuse the user.

   procedure Set_Sensitive
     (Widget    : access Gtk_Widget_Record;
      Sensitive : Boolean := True);
   --  Modify the sensitivity of the widget.
   --  An insensitive widget is generally grayed out, and can not be activated.
   --  For instance, an insensitive menu item is grayed, and can never be
   --  selected.

   procedure Set_App_Paintable
     (Widget        : access Gtk_Widget_Record;
      App_Paintable : Boolean);
   --  Modify the "App_Paintable" flag for the widget.

   procedure Set_Double_Buffered
     (Widget          : access Gtk_Widget_Record;
      Double_Buffered : Boolean := True);
   --  Modify the "Double_Buffered" flag for the widget.

   procedure Get_Pointer
     (Widget : access Gtk_Widget_Record;
      X      : out Gint;
      Y      : out Gint);
   --  Return the coordinates of the pointer (i.e. mouse) relative to Widget.

   procedure Set_Window
     (Widget : access Gtk_Widget_Record;
      Window : Gdk.Window.Gdk_Window);
   function Get_Window
     (Widget : access Gtk_Widget_Record) return Gdk.Window.Gdk_Window;
   --  Set the Gdk window associated with the widget.
   --  You can use this window if you need to draw directly on the widget using
   --  the functions found in the Gdk hierarchy.
   --  These functions are rarely used except when you implement your own own
   --  widget types. Predefined widgets takes care of that automatically.

   procedure Shape_Combine_Mask
     (Widget     : access Gtk_Widget_Record;
      Shape_Mask : Gdk.Bitmap.Gdk_Bitmap;
      Offset_X   : Gint;
      Offset_Y   : Gint);
   --  Modify the shape of the window that contains the widget.
   --  This allows for transparent windows, and requires the Xext library to be
   --  available on your system. If this library is not available, your program
   --  will still work.
   --  See the manual page for XShapeCombineMask(3x) for more information.

   procedure Input_Shape_Combine_Mask
     (Widget     : access Gtk_Widget_Record;
      Shape_Mask : Gdk.Bitmap.Gdk_Bitmap;
      Offset_X   : Gint;
      Offset_Y   : Gint);
   --  Sets an input shape for this widget's GDK window. This allows for
   --  windows which react to mouse click in a nonrectangular region, see
   --  Gdk.Window.Input_Shape_Combine_Mask for more information.

   procedure Reset_Shapes (Widget : access Gtk_Widget_Record);
   --  Recursively resets the shape on this widget and its descendants.

   function Render_Icon
     (Widget   : access Gtk_Widget_Record;
      Stock_Id : String;
      Size     : Gtk.Enums.Gtk_Icon_Size;
      Detail   : UTF8_String := "") return Gdk.Pixbuf.Gdk_Pixbuf;
   --  A convenience function that uses the theme engine for Widget, to lookup
   --  a Stock_Id (see Gtk.Stock) and render it to a pixbuf (see Gdk.Pixbuf).
   --  Detail should be a string that identifies the widget or code doing the
   --  rendering, so that the theme engine can special-case rendering for that
   --  widget or code. It can be left to the empty stirng to get the default
   --  behavior.
   --
   --  Null is returned if Stock_Id wasn't known.

   function Get_Snapshot
     (Widget    : access Gtk_Widget_Record;
      Clip_Rect : Gdk.Rectangle.Gdk_Rectangle_Access)
      return Gdk.Pixmap.Gdk_Pixmap;
   --  Create a Gdk_Pixmap of the contents of the widget and its children.
   --
   --  Works even if the widget is obscured. The depth and visual of the
   --  resulting pixmap is dependent on the widget being snapshot and likely
   --  differs from those of a target widget displaying the pixmap.
   --  The function Gdk.Pixbuf.Get_From_Drawable can be used to convert
   --  the pixmap to a visual independant representation.
   --
   --  The snapshot area used by this function is the Widget's allocation plus
   --  any extra space occupied by additional windows belonging to this widget
   --  (such as the arrows of a spin button).  Thus, the resulting snapshot
   --  pixmap is possibly larger than the allocation.
   --
   --  If Clip_Rect is non-null, the resulting pixmap is shrunken to
   --  match the specified clip_rect. The (x,y) coordinates of Clip_Rect are
   --  interpreted widget relative. If width or height of Clip_Rect are 0 or
   --  negative, the width or height of the resulting pixmap will be shrunken
   --  by the respective amount.
   --
   --  For instance using a Clip_Rect'(+5, +5, -10, -10) will chop off 5 pixels
   --  at each side of the snapshot pixmap.
   --
   --  If non-null, Clip_Rect will contain the exact widget-relative snapshot
   --  coordinates upon return. A Clip_Rect of (-1, -1, 0, 0) can be used to
   --  preserve the auto-grown snapshot area and use Clip_Rect as a pure output
   --  parameter.
   --
   --  The returned pixmap can be null, if the resulting Clip_Area was empty.

   --------------
   -- Tooltips --
   --------------

   function Get_Tooltip_Text
     (Widget : access Gtk_Widget_Record) return UTF8_String;
   procedure Set_Tooltip_Text
     (Widget : access Gtk_Widget_Record;
      Text   : UTF8_String);
   --  Gets/Sets text as the contents of the tooltip. This function will take
   --  care of setting GtkWidget::has-tooltip to TRUE and of the default
   --  handler for the GtkWidget::query-tooltip signal.
   --
   --  See also the GtkWidget:tooltip-text property and Gtk_Tooltips.Set_Text.

   function Get_Tooltip_Markup
     (Widget : access Gtk_Widget_Record) return UTF8_String;
   procedure Set_Tooltip_Markup
     (Widget : access Gtk_Widget_Record;
      Text   : UTF8_String);
   --  Gets/Sets tooltip contents, marked up with the Pango text markup
   --  language.
   --
   --  This function will take care of setting GtkWidget:has-tooltip to TRUE
   --  and of the default handler for the GtkWidget::query-tooltip signal.
   --
   --  See also the GtkWidget::tooltip-markup property and
   --  Gtk_Tooltips.Set_Markup.

   procedure Set_Tooltip_Window
     (Widget        : access Gtk_Widget_Record;
      Custom_Window : access Gtk_Widget_Record'Class);
   --   Custom_Window : access Gtk.Window.Gtk_Window_Record'Class);
   --
   --  Replaces the default, usually yellow, window used for displaying
   --  tooltips with custom_window. GTK+ will take care of showing and hiding
   --  Custom_Window at the right moment, to behave likewise as the default
   --  tooltip window. If Custom_Window is NULL, the default tooltip window
   --  will be used.

   function Get_Tooltip_Window
     (Widget : access Gtk_Widget_Record) return Gtk_Widget;
   --    return Gtk.Window.Gtk_Window;
   --
   --  Returns the GtkWindow of the current tooltip. This can be the GtkWindow
   --  created by default, or the custom tooltip window set using
   --  Gtk.Widget.Set_Tooltip_Window.

   function Get_Has_Tooltip (Widget : access Gtk_Widget_Record) return Boolean;
   procedure Set_Has_Tooltip
     (Widget      : access Gtk_Widget_Record;
      Has_Tooltip : Boolean);
   --  Gets/Sets the has-tooltip property on Widget to Has_Tooltip.  See
   --  GtkWidget:has-tooltip for more information.

   procedure Trigger_Tooltip_Query (Widget : access Gtk_Widget_Record);
   --  Triggers a tooltip query on the display where the toplevel of Widget
   --  is located. See Gtk.Tooltip.Trigger_Tooltip_Query for more
   --  information.

   --------------------------
   -- Creating new widgets --
   --------------------------
   --  Although the core subprogram for creating new widgets is
   --  Glib.Gobjects.Initialize_Class_Record, it is often useful to override
   --  some internal pointers to functions.
   --  The functions below are not needed unless you are writting your own
   --  widgets, and should be reserved for advanced customization of the
   --  standard widgets.

   procedure Set_Scroll_Adjustments_Signal
     (Widget : Glib.Object.GObject_Class; Signal : String);
   --  Modify the signal to be sent when the adjustments are modified.
   --  This is only useful when you are rewritting your own widget that can be
   --  embedded directly in a Gtk_Scrolled_Window, without any Gtk_Viewport.
   --
   --  Signal is the name of the signal that will be emitted when Widget is
   --  put inside a Gtk_Scrolled_Window.
   --
   --  Note that the handlers for this signal must take two arguments in
   --  addition to the widget (the horizontal and vertical adjustments to be
   --  used). See Gtk.Scrolled_Window and Gtk.Widget.Set_Scroll_Adjustment for
   --  more information on this signal.

   type Size_Allocate_Handler is access procedure
     (Widget : System.Address; Allocation : Gtk_Allocation);
   pragma Convention (C, Size_Allocate_Handler);
   --  Widget is the gtk+ C widget, that needs to be converted to Ada through
   --  a call to:
   --    declare
   --       Stub : Gtk_Widget_Record; --  or the exact type you expect
   --    begin
   --       My_Widget := Gtk_Widget (Glib.Object.Get_User_Data (Widget, Stub);
   --    end;

   procedure Set_Default_Size_Allocate_Handler
     (Klass   : Glib.Object.GObject_Class;
      Handler : Size_Allocate_Handler);
   --  Override the default size_allocate handler for this class. This handler
   --  is automatically called in several cases (when a widget is dynamically
   --  resized for instance), not through a signal. Thus, if you need to
   --  override the default behavior provided by one of the standard
   --  containers, you can not simply use Gtk.Handlers.Emit_Stop_By_Name, and
   --  you must override the default handler. Note also that this handler
   --  is automatically inherited by children of this class.

   procedure Set_Allocation
     (Widget : access Gtk_Widget_Record'Class; Alloc : Gtk_Allocation);
   --  Modifies directly the internal field of Widget to register the new
   --  allocation.
   --  Beware that the only use of this method is inside a callback set
   --  by Set_Default_Size_Allocate_Handler. If you simply want to resize
   --  or reposition a widget, use Size_Allocate instead.

   type Expose_Event_Handler is access function
     (Widget : System.Address; Event : Gdk.Event.Gdk_Event) return Boolean;
   pragma Convention (C, Expose_Event_Handler);
   function Default_Expose_Event_Handler (Klass : GObject_Class)
      return Expose_Event_Handler;
   --  Return the default expose event handler for the widget class Klass. The
   --  typical use for this function is when you are writting your own
   --  container class. You should then, from your own handler for
   --  expose_event, call the one of the parent class, so that all the children
   --  are automatically redrawn.

   -----------
   -- Flags --
   -----------
   --  Some flags are defined for all the visual objects (widgets).
   --  These flags are important in that they define exactly the different
   --  states a widget can be in.
   --
   --  - "Toplevel":
   --    Set if the widget is a toplevel widget, ie has no parent. This is
   --    mostly true for windows and dialogs.
   --
   --  - "No_Window":
   --    Set if the widget does not have an associated X11 window, ie can not
   --    receive events directly. For instance, a Gtk_Toolbar does not have
   --    an associated window. These objects are more lightweight, but require
   --    more work from GtkAda. This flag is only set if the widget will never
   --    have a window, even after it is realized.
   --
   --  - "Realized":
   --    Set if the widget has been realized, ie its associated X11 window has
   --    been created (providing the widget excepts a window, see the No_Window
   --    flag
   --
   --  - "Mapped":
   --    Set if the widget is visible on the screen. This is only possible if
   --    the Visible flag is also set.
   --
   --  - "Visible":
   --    Set if the widget will be displayed on the screen when mapped (see the
   --    functions Show and Hide in this package).
   --
   --  - "Sensitive":
   --    Set if the widget is listening to events. See the function
   --    Set_Sensitive in this package. An insensitive widget will generally
   --    have a different visual aspect to clue that it is unavailable (for
   --    instance an insensitive item menu will be grayed)
   --
   --  - "Parent_Sensitive":
   --    Set if the parent is sensitive. A widget is sensitive only if both
   --    the Sensitive and Parent_Sensitive are set.
   --
   --  - "Can_Focus":
   --    Set if the widget can have the focus, ie get keyboard events. Most
   --    widgets can not have the focus.
   --
   --  - "Has_Focus":
   --    Set if the widget currently has the focus. See the function Grab_Focus
   --    in this package. See also the subprogram Gtk.Widget.Is_Focus
   --
   --  - "Can_Default":
   --    Set if the widget can be the default widget in a window, ie the one
   --    that will get the keyboard events by default. For instance, the
   --    default button in a dialog is the one that gets clicked on when the
   --    user pressed Enter anywhere in the dialog.
   --
   --  - "Has_Default":
   --    Set if the widget is currently the default widget. See the function
   --    Grab_Default in this package.
   --
   --  - "Has_Grab":
   --    Set if the widget currently grabs all mouse and keyboard events in
   --    the application, even if it does not have the focus. There can be only
   --    such widget per application at any given time.
   --
   --  - "Rc_Style":
   --    Set if the widget's style is either the default style, or in a
   --    customization file. This is unset if the style has been modified by
   --    the user.
   --
   --  - "Composite_Child":
   --    This indicates whether the widget is composed of other widgets
   --
   --  - "No_Reparent":
   --    This flags is never used in gtk+.
   --
   --  - "App_Paintable":
   --    For some containers (including Gtk_Window and Gtk_Layout), this is
   --    unset when the container itself has some special drawing routines. It
   --    indicates whether the application will paint directly on the widget.
   --
   --  - "Receives_Default":
   --    Set when the widget receives the default at the time it receives the
   --    focus. This is how the default button in a dialog is automatically
   --    changed when you press another button.

   In_Destruction : constant := 2 ** 0;
   Floating       : constant := 2 ** 1;
   Reserved_1     : constant := 2 ** 2;
   Reserved_2     : constant := 2 ** 3;
   Toplevel         : constant := 2 ** 4;
   No_Window        : constant := 2 ** 5;
   Realized         : constant := 2 ** 6;
   Mapped           : constant := 2 ** 7;
   Visible          : constant := 2 ** 8;
   Sensitive        : constant := 2 ** 9;
   Parent_Sensitive : constant := 2 ** 10;
   Can_Focus        : constant := 2 ** 11;
   Has_Focus        : constant := 2 ** 12;
   Can_Default      : constant := 2 ** 13;
   Has_Default      : constant := 2 ** 14;
   Has_Grab         : constant := 2 ** 15;
   Rc_Style         : constant := 2 ** 16;
   Composite_Child  : constant := 2 ** 17;
   No_Reparent      : constant := 2 ** 18;
   App_Paintable    : constant := 2 ** 19;
   Receives_Default : constant := 2 ** 20;
   Double_Buffered  : constant := 2 ** 21;

   function Flags (Widget : access Gtk_Widget_Record) return Guint32;
   --  Return the flags that are set for the object, as a binary mask.

   procedure Set_Flags (Widget : access Gtk_Widget_Record; Flags : Guint32);
   --  Set some specific flags for the object.
   --  Flags is a mask that will be added to the current flags of the object.

   procedure Unset_Flags (Widget : access Gtk_Widget_Record; Flags : Guint32);
   --  Unset some specific flags for the object.
   --  Flags is a mask that will be deleted from the current flags of the
   --  object.

   function Flag_Is_Set
     (Widget : access Gtk_Widget_Record; Flag : Guint32) return Boolean;
   --  Return True if the specific flag Flag is set for the object.

   function In_Destruction_Is_Set
     (Widget : access Gtk_Widget_Record'Class) return Boolean;
   --  Test if the Destroyed flag is set for the object.

   --  <doc_ignore>
   function Destroyed_Is_Set (Widget : access Gtk_Widget_Record'Class)
      return Boolean renames In_Destruction_Is_Set;
   --  backward compatibility only
   --  </doc_ignore>

   function Floating_Is_Set
     (Widget : access Gtk_Widget_Record'Class) return Boolean;
   --  Test if the Floating flag is set for the object.

   function Toplevel_Is_Set
     (Widget : access Gtk_Widget_Record'Class) return Boolean;
   --  Test whether the Toplevel flag is set.

   function No_Window_Is_Set
     (Widget : access Gtk_Widget_Record'Class) return Boolean;
   --  Test whether the No_Window flag is set.

   function Realized_Is_Set
     (Widget : access Gtk_Widget_Record'Class) return Boolean;
   --  Test whether the Realized flag is set.

   function Mapped_Is_Set
     (Widget : access Gtk_Widget_Record'Class) return Boolean;
   --  Test whether the Mapped flag is set.

   function Visible_Is_Set
     (Widget : access Gtk_Widget_Record'Class) return Boolean;
   --  Test whether the Visible flag is set.

   function Drawable_Is_Set
     (Widget : access Gtk_Widget_Record'Class) return Boolean;
   --  True if the widget is both visible and mapped.
   --  In other words, if it does appear on the screen.

   function Is_Sensitive
     (Widget : access Gtk_Widget_Record'Class) return Boolean;
   --  Test whether the widget is Sensitive.

   function Can_Focus_Is_Set
     (Widget : access Gtk_Widget_Record'Class) return Boolean;
   --  Test whether the Can_Focus flag is set.

   function Has_Focus_Is_Set
     (Widget : access Gtk_Widget_Record'Class) return Boolean;
   --  Test whether the Has_Focus flag is set.

   function Has_Default_Is_Set
     (Widget : access Gtk_Widget_Record'Class) return Boolean;
   --  Test whether the Has_Default flag is set.

   function Has_Grab_Is_Set
     (Widget : access Gtk_Widget_Record'Class) return Boolean;
   --  Test whether the Has_Grab flag is set.

   function Rc_Style_Is_Set
     (Widget : access Gtk_Widget_Record'Class) return Boolean;
   --  Test whether the Rc_Style flag is set.

   function Double_Buffered_Is_Set
     (Widget : access Gtk_Widget_Record'Class) return Boolean;
   --  Test whether the Double_Buffered flag is set.

   --------------------
   -- GValue support --
   --------------------

   function Get_Requisition
     (Value : Glib.Values.GValue) return Gtk_Requisition_Access;
   --  Convert a value into a Gtk_Requisition_Access.

   function Get_Allocation
     (Value : Glib.Values.GValue) return Gtk_Allocation_Access;
   --  Convert a value into a Gtk_Allocation_Access.

   -----------------
   -- Obsolescent --
   -----------------
   --  All subprograms below are now obsolescent in gtk+. They might be removed
   --  from future versions of gtk+ (and therefore GtkAda).
   --  To find out whether your code uses any of these, we recommend compiling
   --  with the -gnatwj switch
   --  <doc_ignore>

   procedure Draw
     (Widget : access Gtk_Widget_Record;
      Area   : Gdk.Rectangle.Gdk_Rectangle := Gdk.Rectangle.Full_Area);
   pragma Obsolescent;  --  Draw
   --  Emit a "draw" signal for a specific area of the widget.
   --  The visual aspect might be different whether the widget has the focus
   --  or not.

   procedure Set_UPosition
     (Widget : access Gtk_Widget_Record;
      X, Y   : Gint);
   pragma Obsolescent;  --  Set_Uposition
   --  Modify the position of the widget.
   --  This should be used only for toplevel widgets (windows and dialogs),
   --  since other widgets' positions are handled by their parent.

   procedure Set_USize
     (Widget        : access Gtk_Widget_Record;
      Width, Height : Gint);
   pragma Obsolescent ("Use Set_Size_Request instead");  --  Set_Usize
   --  Modify the size of the widget.
   --  This sets an absolute size for the widget, no matter what its requested
   --  size would be. For Gtk_Windows, you should consider using
   --  Set_Default_Size instead, which sets a minimal size, but use the
   --  widget's requested size if it is bigger.
   --  If Width or Height is negative, they are ignored, and the widget's
   --  default width is kept.

   procedure Queue_Clear (Widget : access Gtk_Widget_Record);
   pragma Obsolescent; --  Queue_Clear
   --  Add a clear request to the event queue for the whole widget.
   --  This is added to the same list as for Queue_Draw, and thus is coalesced
   --  as much as possible with other drawing requests.

   procedure Queue_Clear_Area
     (Widget : access Gtk_Widget_Record;
      X      : Gint;
      Y      : Gint;
      Width  : Gint;
      Height : Gint);
   pragma Obsolescent; --  Queue_Clear_Area
   --  Add a clear request to the event queue for part of the widget.
   --  This is added to the same list as for Queue_Draw, and thus is coalesced
   --  as much as possible with other drawing requests.

   --  </doc_ignore>

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.

   --  <properties>
   --  Name:  Name_Property
   --  Type:  UTF8_String
   --  Flags: read-write
   --  Descr: The name of the widget
   --  See also:  Set_Name procedure
   --
   --  Name:  Parent_Property
   --  Type:  Gtk_Container'Class
   --  Flags: read-write
   --  Descr: The parent widget of this widget.
   --  See also:  Set_Parent or Add procecures
   --
   --  Name:  X_Property
   --  Type:  Gint
   --  Flags: read-write
   --  Descr: The x coordinate of the top-left corner of the widget,
   --         or -1 if not set
   --
   --  Name:  Y_Property
   --  Type:  Gint
   --  Flags: read-write
   --  Descr: The y coordinate of the top-left corner of the widget,
   --         or -1 if not set
   --
   --  Name:  Width_Property
   --  Type:  Gint
   --  Flags: read-write
   --  Descr: The width of the widget or -1 if not set
   --  See also:  Set_USize
   --
   --  Name:  Height_Property
   --  Type:  Gint
   --  Flags: read-write
   --  Descr: The height of the widget or -1 if not set
   --  See also:  Set_USize
   --
   --  Name:  Visible_Property
   --  Type:  Boolean
   --  Flags: read-write
   --  Descr: Whether the widget is visible
   --  See also:  Hide and Show procedures
   --
   --  Name:  Sensitive_Property
   --  Type:  Boolean
   --  Flags: read-write
   --  Descr: Whether the widget responds to input
   --  See also: Set_Sensitive
   --
   --  Name:  App_Paintable_Property
   --  Type:  Boolean
   --  Flags: read-write
   --  Descr: Whether the application will paint directly on the widget
   --  See also: Set_App_Paintable
   --
   --  Name:  Can_Focus_Property
   --  Type:  Boolean
   --  Flags: read-write
   --  Descr: Whether the widget can accept the input focus (keyboard)
   --  See also: Set or unset the flag Can_Focus
   --
   --  Name:  Has_Focus_Property
   --  Type:  Boolean
   --  Flags: read-write
   --  Descr: Whether the widget has the input focus
   --  See also: Grab_Focus
   --
   --  Name:  Can_Default_Property
   --  Type:  Boolean
   --  Flags: read-write
   --  Descr: Whether the widget can be the default widget
   --  See also: Set or unset the flag Can_Default
   --
   --  Name:  Has_Default_Property
   --  Type:  Boolean
   --  Flags: read-write
   --  Descr: Whether the widget is the default widget
   --  See also: Grab_Default
   --
   --  Name:  Receives_Default_Property
   --  Type:  Boolean
   --  Flags: read-write
   --  Descr: If True, the widget will receive the default action when
   --         it is focused
   --  See also: Set or unset the flag Receives_Default
   --
   --  Name:  Composite_Child_Property
   --  Type:  Boolean
   --  Flags: read-write
   --  Descr: Whether the widget is composed of other widgets
   --  See also: Set or unset the flag Composite_Child
   --
   --  Name:  Style_Property
   --  Type:  Gtk_Style
   --  Flags: read-write
   --  Descr: The style of the widget, which contains information about how
   --         it will look (colors etc).
   --  See also: Set_Style
   --
   --  Name:  Events_Property
   --  Type:  flags
   --  Flags: read-write
   --  Descr: The event mask that decides what kind of GdkEvents this widget
   --         gets.
   --  See also: Set_Events
   --
   --  Name:  Prop_Extensions_Events_Property
   --  Type:  flags
   --  Flags: read-write
   --  Descr: The mask that decides what kind of extension events this widget
   --         gets.
   --  See also: Set_Extension_Events
   --
   --  Name:  Extension_Events_Property
   --  Type:  Enum
   --  Descr: The mask that decides what kind of extension events this widget
   --         gets
   --
   --  Name:  Height_Request_Property
   --  Type:  Int
   --  Descr: Override for height request of the widget, or -1 if natural
   --         request should be used
   --
   --  Name:  Is_Focus_Property
   --  Type:  Boolean
   --  Descr: Whether the widget is the focus widget within the toplevel
   --
   --  Name:  No_Show_All_Property
   --  Type:  Boolean
   --  Descr: Whether gtk_widget_show_all() should not affect this widget
   --
   --  Name:  Width_Request_Property
   --  Type:  Int
   --  Descr: Override for width request of the widget, or -1 if natural
   --         request should be used
   --
   --  Name:  Tooltip_Markup_Property
   --  Type:  String
   --  Descr: The contents of the tooltip for this widget
   --
   --  Name:  Tooltip_Text_Property
   --  Type:  String
   --  Descr: The contents of the tooltip for this widget
   --
   --  Name:  Has_Tooltip
   --  Type:  Boolean
   --  Descr: Enables or disables the emission of "query-tooltip" on widget. A
   --         value of TRUE indicates that widget can have a tooltip, in this
   --         case the widget will be queried using "query-tooltip" to
   --         determine whether it will provide a tooltip or not.
   --
   --         Note that setting this property to TRUE for the first time will
   --         change the event masks of the GdkWindows of this widget to
   --         include leave-notify and motion-notify events. This cannot and
   --         will not be undone when the property is set to FALSE again.
   --
   --  Name:  Window_Property
   --  Type:  Object
   --  Descr: The widget's window if it is realized
   --
   --  </properties>

   procedure Child_Notify
     (Widget         : access Gtk_Widget_Record;
      Child_Property : String);
   --  Emits a "child-notify" signal for the child property on Widget.
   --  This signal indicates the the value of the child property has changed on
   --  the parent, and thus that Widget should refresh itself if needed.
   --
   --  Child_Property is the name of a child property installed on Widget's
   --  parent. You should use Glib.Propert_Name to get the name from the
   --  property declaration in each of the GtkAda packages

   procedure Freeze_Child_Notify (Widget : access Gtk_Widget_Record);
   --  Stops emission of "child-notify" signals on Widget. The signals are
   --  queued until Thaw_Child_Notify() is called on Wwidget.

   procedure Thaw_Child_Notify (Widget : access Gtk_Widget_Record);
   --  Reverts the effect of a previous call to Freeze_Child_Notify.
   --  This causes all queued "child-notify" signals on Widget to be emitted.

   procedure Class_Install_Style_Property
     (Klass : Glib.Object.GObject_Class;
      Pspec : Glib.Param_Spec);
   --  Installs a style property on a widget class. The parser for the
   --  style property is determined by the value type of Pspec.
   --  A style property configures the look-and-feel of a widget class. They
   --  are generally modified by the current gtk+ theme, although users can
   --  also modify them in their own configuration file.

   function Class_List_Style_Properties
     (Klass : Glib.Object.GObject_Class) return Glib.Param_Spec_Array;
   --  Returns all style properties of a widget class.

   function Class_Find_Style_Property
     (Klass         : Glib.Object.GObject_Class;
      Property_Name : String) return Glib.Param_Spec;
   --  Finds a style property of a widget class by name.
   --  Klass must be a descendent of Gtk_Widget.
   --  You should use Glib.Property_Name to get the name from the property
   --  declaration in each of the GtkAda packages

   procedure Style_Get_Property
     (Widget        : access Gtk_Widget_Record;
      Property_Name : String;
      Value         : out Glib.Values.GValue);
   --  Gets the value of a style property of Widget.
   --  You should use Glib.Property_Name to get the name from the property
   --  declaration in each of the GtkAda packages

   Name_Property                  : constant Glib.Properties.Property_String;
   Parent_Property                : constant Glib.Properties.Property_Object;
   X_Property                     : constant Glib.Properties.Property_Int;
   Y_Property                     : constant Glib.Properties.Property_Int;
   Width_Property                 : constant Glib.Properties.Property_Int;
   Height_Property                : constant Glib.Properties.Property_Int;
   Visible_Property               : constant Glib.Properties.Property_Boolean;
   Sensitive_Property             : constant Glib.Properties.Property_Boolean;
   App_Paintable_Property         : constant Glib.Properties.Property_Boolean;
   Can_Focus_Property             : constant Glib.Properties.Property_Boolean;
   Has_Focus_Property             : constant Glib.Properties.Property_Boolean;
   Can_Default_Property           : constant Glib.Properties.Property_Boolean;
   Has_Default_Property           : constant Glib.Properties.Property_Boolean;
   Receives_Default_Property      : constant Glib.Properties.Property_Boolean;
   Composite_Child_Property       : constant Glib.Properties.Property_Boolean;
   Style_Property                 : constant Glib.Properties.Property_Object;
   Events_Property                : constant Gdk.Event.Property_Gdk_Event_Mask;
   Prop_Extensions_Events_Property :
     constant Gdk.Types.Property_Gdk_Extension_Mode;
   Extension_Events_Property  : constant Gdk.Types.Property_Gdk_Extension_Mode;
   Height_Request_Property        : constant Glib.Properties.Property_Int;
   Is_Focus_Property              : constant Glib.Properties.Property_Boolean;
   No_Show_All_Property           : constant Glib.Properties.Property_Boolean;
   Width_Request_Property         : constant Glib.Properties.Property_Int;
   Tooltip_Markup_Property        : constant Glib.Properties.Property_String;
   Tooltip_Text_Property          : constant Glib.Properties.Property_String;
   Has_Tooltip_Property           : constant Glib.Properties.Property_Boolean;
   Window_Property                : constant Glib.Properties.Property_Object;

   ----------------------
   -- Style Properties --
   ----------------------
   --  The following properties can be changed through the gtk theme and
   --  configuration files, and retrieved through Gtk.Widget.Style_Get_Property

   --  <style_properties>
   --  Name:  Cursor_Aspect_Ratio_Property
   --  Type:  Float
   --  Descr: Aspect ratio with which to draw insertion cursor
   --
   --  Name:  Cursor_Color_Property
   --  Type:  Boxed
   --  Descr: Color with which to draw insertion cursor
   --
   --  Name:  Draw_Border_Property
   --  Type:  Boxed
   --  Descr: Size of areas outside the widget's allocation to draw
   --
   --  Name:  Focus_Line_Pattern_Property
   --  Type:  String
   --  Descr: Dash pattern used to draw the focus indicator
   --
   --  Name:  Focus_Line_Width_Property
   --  Type:  Int
   --  Descr: Width, in pixels, of the focus indicator line
   --
   --  Name:  Focus_Padding_Property
   --  Type:  Int
   --  Descr: Width, in pixels, between focus indicator and the widget 'box'
   --
   --  Name:  Interior_Focus_Property
   --  Type:  Boolean
   --  Descr: Whether to draw the focus indicator inside widgets
   --
   --  Name:  Link_Color_Property
   --  Type:  Boxed
   --  Descr: Color of unvisited links
   --
   --  Name:  Scroll_Arrow_Hlength_Property
   --  Type:  Int
   --  Descr: The length of horizontal scroll arrows
   --
   --  Name:  Scroll_Arrow_Vlength_Property
   --  Type:  Int
   --  Descr: The length of vertical scroll arrows
   --
   --  Name:  Secondary_Cursor_Color_Property
   --  Type:  Boxed
   --  Descr: Color with which to draw the secondary insertion cursor when
   --         editing mixed right-to-left and left-to-right text
   --
   --  Name:  Separator_Height_Property
   --  Type:  Int
   --  Descr: The height of separators if \
   --
   --  Name:  Separator_Width_Property
   --  Type:  Int
   --  Descr: The width of separators if wide-separators is TRUE
   --
   --  Name:  Visited_Link_Color_Property
   --  Type:  Boxed
   --  Descr: Color of visited links
   --
   --  Name:  Wide_Separators_Property
   --  Type:  Boolean
   --  Descr: Whether separators have configurable width and should be drawn
   --         using a box instead of a line
   --  </style_properties>

   Cursor_Aspect_Ratio_Property  : constant Glib.Properties.Property_Float;
   --  Cursor_Color_Property        : constant Glib.Properties.Property_Boxed;
   --  Draw_Border_Property         : constant Glib.Properties.Property_Boxed;
   Focus_Line_Pattern_Property   : constant Glib.Properties.Property_String;
   Focus_Line_Width_Property     : constant Glib.Properties.Property_Int;
   Focus_Padding_Property        : constant Glib.Properties.Property_Int;
   Interior_Focus_Property       : constant Glib.Properties.Property_Boolean;
   Link_Color_Property           : constant Glib.Properties.Property_Boxed;
   Scroll_Arrow_Hlength_Property : constant Glib.Properties.Property_Int;
   Scroll_Arrow_Vlength_Property : constant Glib.Properties.Property_Int;
   --  Secondary_Cursor_Color_Property : constant
   --    Glib.Properties.Property_Boxed;
   Separator_Height_Property     : constant Glib.Properties.Property_Int;
   Separator_Width_Property      : constant Glib.Properties.Property_Int;
   Visited_Link_Color_Property   : constant Glib.Properties.Property_Boxed;
   Wide_Separators_Property      : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "show"
   --    procedure Handler (Widget : access Gtk_Widget_Record'Class);
   --    Emitted when a widget is to be shown (see explanation for the Show
   --    subprogam). This schedules the widget to be displayed on the screen,
   --    and if this is a toplevel widget it actually appears on the screen
   --    and all its children that have been shown.
   --
   --  - "hide"
   --    procedure Handler (Widget : access Gtk_Widget_Record'Class);
   --    Emitted when a widget is to be hidden (see explanation for the Hide
   --    subprogram). Hides the widget from the screen, and if its parent is
   --    shown, the widget will not appear on the screen again.
   --
   --  - "map"
   --    procedure Handler (Widget : access Gtk_Widget_Record'Class);
   --    Emitted when a widget is mapped on the screen (the default handler
   --    simply emits the "show" signal).
   --
   --  - "unmap"
   --    procedure Handler (Widget : access Gtk_Widget_Record'Class);
   --    Emitted when a widget needs to be unmapped on the screen (the default
   --    handler simply emits the "hide" signal).
   --
   --  - "realize"
   --    procedure Handler (Widget : access Gtk_Widget_Record'Class);
   --    Emitted when a widget is realized. The default handler creates the
   --    Gdk window associated with the widget, and its ancestors.
   --
   --  - "unrealize"
   --    procedure Handler (Widget : access Gtk_Widget_Record'Class);
   --    Emitted when a widget is unrealized. The default handler destroys the
   --    Gdk windows of the widget and all its children.
   --
   --  - "draw"
   --    procedure Handler (Widget : access Gtk_Widget_Record'Class;
   --                       Area   : Gdk.Rectangle.Gdk_Rectangle);
   --    Emitted when a widget needs to be drawn. The default handler emits
   --    the "expose" event.
   --
   --  - "draw_focus"
   --    procedure Handler (Widget : access Gtk_Widget_Record'Class);
   --    Emitted when a widget needs to be drawn and it has the focus. Some
   --    widgets might want to provide visual clues that they have the focus,
   --    like a black border. This is never called if the widget can not have
   --    the focus (ie the "Can_Focus" flag is unset).
   --
   --  - "draw_default"
   --    procedure Handler (Widget : access Gtk_Widget_Record'Class);
   --    Emitted when a widget needs to be drawn and it does not have the
   --    focus. This is never called if the widget can not have the focus
   --    (ie the "Can_Focus" flag is unset).
   --
   --  - "size_request"
   --    procedure Handler (Widget      : access Gtk_Widget_Record'Class;
   --                       Requisition : access Gtk_Requisition);
   --    Should return (in Requisition) the ideal size the widget would like to
   --    have. It is not sure this is the size that will be assigned to it,
   --    since it depends on the size of its parent).
   --
   --  - "size_allocate"
   --    procedure Handler (Widget     : access Gtk_Widget_Record'Class;
   --                       Allocation : Gtk_Allocation);
   --    A size and position were assigned to the widget. This is called every
   --    time the size of the widget changes.
   --    The default handler takes care of resizing and moving the widget.
   --
   --  - "state_changed"
   --    procedure Handler (Widget         : access Gtk_Widget_Record'Class;
   --                       Previous_State : Gtk.Enums.Gtk_State_Type);
   --    The state of the widget has changed.
   --
   --  - "parent_set"
   --    procedure Handler (Widget : access Gtk_Widget_Record'Class;
   --                       Previous_Parent : access Gtk_Widget_Record'Class);
   --    A new parent has been set for the widget. The previous parent is
   --    given in arguments (if there was none,
   --    Gdk.Is_Created (Previous_Parent) returns False).
   --
   --  - "style_set"
   --    procedure Handler (Widget : access Gtk_Widget_Record'Class);
   --                       Previous_Style : Gtk.Style.Gtk_Style);
   --    The widget's style has been changed (this is not call when some
   --    settings in the style are changed, only when the style itself is
   --    completely changed with a call to Set_Style or Set_Default_Style).
   --
   --  - "add_accelerator"
   --  ???
   --
   --  - "remove_accelerator"
   --  ???
   --
   --  - "grab_focus"
   --    procedure Handler (Widget : access Gtk_Widget_Record'Class);
   --    The widget has got the focus, ie will now get the keyboard events
   --    sent to a window. This is only called if the "Can_Focus" flag is
   --    set. The "Has_Focus" flag might not be set when this signal is
   --    emitted.
   --
   --  - "event"
   --    function Handler (Widget : access Gtk_Widget_Record'Class;
   --                      Event  : Gdk.Event.Gdk_Event)
   --                     return Boolean;
   --    Some event was sent to the widget. This covers all the cases
   --    below, and acts as a general handler. This is called in addition to
   --    the relevant specific handler below.
   --    If the handler returns False, the event might be pass to the parent
   --    of widget (if no other handler of widget has returned True).
   --
   --  - "button_press_event"
   --    function Handler (Widget : access Gtk_Widget_Record'Class;
   --                      Event  : Gdk.Event.Gdk_Event_Button)
   --                     return Boolean;
   --    A button was pressed while the pointer was inside the widget.
   --    To get this signal, some widgets by have to use the Set_Events
   --    subprogram first to get this event.
   --    If the handler returns False, the event might be pass to the parent
   --    of widget (if no other handler of widget has returned True).
   --
   --  - "button_release_event"
   --    function Handler (Widget : access Gtk_Widget_Record'Class;
   --                      Event  : Gdk.Event.Gdk_Event_Button)
   --                     return Boolean;
   --    A button was released while the pointer was inside the widget.
   --    Note that in some cases (Gtk_Buttons for instance), another "clicked"
   --    signal could be emitted). This "button_release_event" should mainly
   --    be used for widgets that don't already have specific signals to cover
   --    that case (Gtk_Drawing_Area for instance).
   --
   --    To get this signal, some widgets may have to use the Set_Events
   --    subprogram first to get this event.
   --
   --    If the handler returns False, the event might be pass to the parent
   --    of widget (if no other handler of widget has returned True).
   --
   --  - "motion_notify_event"
   --    function Handler (Widget : access Gtk_Widget_Record'Class;
   --                      Event  : Gdk.Event.Gdk_Event_Motion)
   --                     return Boolean;
   --    The pointer has moved while remaining inside the widget.
   --    The Set_Events subprogram has to be called first to get this event.
   --
   --    If the handler returns False, the event might be pass to the parent
   --    of widget (if no other handler of widget has returned True).
   --
   --  - "delete_event"
   --    function Handler (Widget : access Gtk_Widget_Record'Class;
   --                      Event  : Gdk.Event.Gdk_Event)
   --                     return Boolean;
   --    The user has clicked on the "close" button in the window's frame
   --    (the button that is automatically set by the window manager). If the
   --    handler returns False, the widget will be destroyed (and the window
   --    closed), but if the handler returns True, nothing will be done.
   --    This is a good way to prevent the user from closing your application's
   --    window if there should be some clean ups first (like saving the
   --    document).
   --
   --  - "destroy"
   --    procedure Handler (Widget : access Gtk_Widget_Record'Class);
   --
   --    Raised when the widget is about to be destroyed. The "destroyed"
   --    flag has been set on the object first. Handlers should not keep
   --    a reference on the object.
   --    Note that when your destroy handlers are called, the user_data is
   --    still available.
   --    The default implementation destroys all the handlers.
   --
   --  - "destroy_event"
   --    function Handler (Widget : access Gtk_Widget_Record'Class;
   --                      Event  : Gdk.Event.Gdk_Event)
   --                     return Boolean;
   --    This signal is apparently never emitted by Gtk+. You might want to
   --    use "destroy" instead.
   --
   --  - "expose_event"
   --    function Handler (Widget : access Gtk_Widget_Record'Class;
   --                      Event  : Gdk.Event.Gdk_Event_Expose)
   --                     return Boolean;
   --    The widget needs to be partly redrawn. The exact area to redraw is
   --    found in Event. For some widgets, you should rather connect to the
   --    "draw" signal. However, for instance for Gtk_Drawing_Area widgets,
   --    you have to use this, after setting the correct event mask with
   --    Set_Events.
   --    If the handler returns False, the event might be passed to the parent
   --    of widget (if no other handler of widget has returned True).
   --
   --  - "key_press_event"
   --    function Handler (Widget : access Gtk_Widget_Record'Class;
   --                      Event  : Gdk.Event.Gdk_Event_Key)
   --                     return Boolean;
   --    A key has been pressed while Widget had the focus. Note that some
   --    widgets like Gtk_Editable provide some higher-level signals to handle
   --    this.
   --    If the handler returns False, the event might be pass to the parent
   --    of widget (if no other handler of widget has returned True).
   --
   --  - "key_release_event"
   --    function Handler (Widget : access Gtk_Widget_Record'Class;
   --                      Event  : Gdk.Event.Gdk_Event_Key)
   --                     return Boolean;
   --    A key has been released while Widget had the focus.
   --    If the handler returns False, the event might be pass to the parent
   --    of widget (if no other handler of widget has returned True).
   --
   --  - "enter_notify_event"
   --    function Handler (Widget : access Gtk_Widget_Record'Class;
   --                      Event  : Gdk.Event.Gdk_Event_Crossing)
   --                     return Boolean;
   --    The pointer has just entered the widget. If the "Can_Focus" flag is
   --    set, Widget will gain the focus, and the widget might be drawn
   --    differently.
   --    If the handler returns False, the event might be pass to the parent
   --    of widget (if no other handler of widget has returned True).
   --
   --  - "leave_notify_event"
   --    function Handler (Widget : access Gtk_Widget_Record'Class;
   --                      Event  : Gdk.Event.Gdk_Event_Crossing)
   --                     return Boolean;
   --    The pointer has just leaved the widget. If the "Can_Focus" flag is
   --    set, Widget will gain the focus, and the widget might be drawn
   --    differently.
   --    If the handler returns False, the event might be pass to the parent
   --    of widget (if no other handler of widget has returned True).
   --
   --  - "configure_event"
   --    function Handler (Widget : access Gtk_Widget_Record'Class;
   --                      Event  : Gdk.Event.Gdk_Event_Configure)
   --                     return Boolean;
   --    Some configuration of the window has changed (it has been
   --    moved or resized).
   --    If the handler returns False, the event might be pass to the parent
   --    of widget (if no other handler of widget has returned True).
   --
   --  - "focus_in_event"
   --    function Handler (Widget : access Gtk_Widget_Record'Class;
   --                      Event  : Gdk.Event.Gdk_Event_Focus)
   --                     return Boolean;
   --    The widget has just gained the focus.
   --    If the handler returns False, the event might be pass to the parent
   --    of widget (if no other handler of widget has returned True).
   --    This event is only emitted if you called Add_Events with a
   --    Enter_Notify_Mask parameter
   --
   --  - "focus_out_event"
   --    function Handler (Widget : access Gtk_Widget_Record'Class;
   --                      Event  : Gdk.Event.Gdk_Event_Focus)
   --                     return Boolean;
   --    The widget has just lost the focus.
   --    If the handler returns False, the event might be pass to the parent
   --    of widget (if no other handler of widget has returned True).
   --    This event is only emitted if you called Add_Events with a
   --    Leave_Notify_Mask parameter
   --
   --  - "map_event"
   --    function Handler (Widget : access Gtk_Widget_Record'Class;
   --                      Event  : Gdk.Event.Gdk_Event)
   --                     return Boolean;
   --    The widget has just been mapped. This is different from the "map"
   --    signal, which is called *before* the widget is actually mapped.
   --    If the handler returns False, the event might be pass to the parent
   --    of widget (if no other handler of widget has returned True).
   --
   --  - "unmap_event"
   --    function Handler (Widget : access Gtk_Widget_Record'Class;
   --                      Event  : Gdk.Event.Gdk_Event)
   --                     return Boolean;
   --    The widget has just been unmapped. This is different from the "unmap"
   --    signal, which is called *before* the widget is actually unmapped.
   --    If the handler returns False, the event might be pass to the parent
   --    of widget (if no other handler of widget has returned True).
   --
   --  - "property_notify_event"
   --    function Handler (Widget : access Gtk_Widget_Record'Class;
   --                      Event  : Gdk.Event.Gdk_Event_Property)
   --                     return Boolean;
   --    ???
   --
   --  - "selection_clear_event"
   --    function Handler (Widget : access Gtk_Widget_Record'Class;
   --                      Event  : Gdk.Event.Gdk_Event_Selection)
   --                     return Boolean;
   --    ???
   --
   --  - "selection_request_event"
   --    function Handler (Widget : access Gtk_Widget_Record'Class;
   --                      Event  : Gdk.Event.Gdk_Event_Selection)
   --                     return Boolean;
   --    ???
   --
   --  - "selection_notify_event"
   --    function Handler (Widget : access Gtk_Widget_Record'Class;
   --                      Event  : Gdk.Event.Gdk_Event_Selection)
   --                     return Boolean;
   --    ???
   --
   --  - "selection_received"
   --    Related to the selection mechanism, see Gtk.Selection
   --
   --  - "selection_get"
   --    Related to the selection mechanism, see Gtk.Selection
   --
   --  - "proximity_in_event"
   --    function Handler (Widget : access Gtk_Widget_Record'Class;
   --                      Event  : Gdk.Event.Gdk_Event_Proximity)
   --                     return Boolean;
   --    Used for special input devices. See the description of
   --    Gdk.Event.Gdk_Event_Proximity.
   --    If the handler returns False, the event might be pass to the parent
   --    of widget (if no other handler of widget has returned True).
   --
   --  - "proximity_out_event"
   --    function Handler (Widget : access Gtk_Widget_Record'Class;
   --                      Event  : Gdk.Event.Gdk_Event_Proximity)
   --                     return Boolean;
   --    Used for special input devices. See the description of
   --    Gdk.Event.Gdk_Event_Proximity.
   --    If the handler returns False, the event might be pass to the parent
   --    of widget (if no other handler of widget has returned True).
   --
   --  - "drag_leave"
   --    Event related to drag-and-drop support. See the Gtk.Dnd documentation.
   --
   --  - "drag_begin"
   --    Event related to drag-and-drop support. See the Gtk.Dnd documentation.
   --
   --  - "drag_end"
   --    Event related to drag-and-drop support. See the Gtk.Dnd documentation.
   --
   --  - "drag_data_delete"
   --    Event related to drag-and-drop support. See the Gtk.Dnd documentation.
   --
   --  - "drag_motion"
   --    Event related to drag-and-drop support. See the Gtk.Dnd documentation.
   --
   --  - "drag_drop"
   --    Event related to drag-and-drop support. See the Gtk.Dnd documentation.
   --
   --  - "drag_data_get"
   --    Event related to drag-and-drop support. See the Gtk.Dnd documentation.
   --
   --  - "drag_data_received"
   --    Event related to drag-and-drop support. See the Gtk.Dnd documentation.
   --
   --  - "visibility_notify_event"
   --    function Handler (Widget : access Gtk_Widget_Record'Class;
   --                      Event  : Gdk.Event.Gdk_Event_Visibility)
   --                     return Boolean;
   --    The visibility state of the widget has changed (partially visible,
   --    fully visible, ...). You might want to use the "expose" signal
   --    instead.
   --    If the handler returns False, the event might be pass to the parent
   --    of widget (if no other handler of widget has returned True).
   --
   --  - "client_event"
   --    ???
   --
   --  - "no_expose_event"
   --    function Handler (Widget : access Gtk_Widget_Record'Class;
   --                      Event  : Gdk.Event.Gdk_Event)
   --                     return Boolean;
   --    ???
   --
   --  - "child_notify"
   --    procedure Handler (Widget : access Gtk_Widget_Record'Class);
   --    This signal is emitted when the value of one of the child properties
   --    for the widget has been changed. If you are only interested in the
   --    changes for a specific property, you can also connect directly to
   --    "child_notify::<property>", for instance "child_notify:right_attach"
   --    for a child of Gtk.Menu.Gtk_Menu.
   --
   --
   --  - "query-tooltip"
   --    function Handler (Widget : access Gtk_Widget_Record'Class;
   --                      Params : Glib.Values.GValues)
   --                     return Boolean;
   --    Emitted when "has-tooltip" is TRUE and the "gtk-tooltip-timeout" has
   --    expired with the cursor hovering "above" widget; or emitted when
   --    widget got focus in keyboard mode.
   --
   --    Using the given coordinates, the signal handler should determine
   --    whether a tooltip should be shown for widget. If this is the case TRUE
   --    should be returned, FALSE otherwise. Note that if keyboard_mode is
   --    TRUE, the values of x and y are undefined and should not be used.
   --
   --    The signal handler is free to manipulate tooltip with the therefore
   --    destined function calls.
   --  </signals>

   Signal_Accel_Closures_Changed  : constant Glib.Signal_Name :=
                                      "accel_closures_changed";
   Signal_Button_Press_Event      : constant Glib.Signal_Name :=
                                      "button_press_event";
   Signal_Button_Release_Event    : constant Glib.Signal_Name :=
                                      "button_release_event";
   Signal_Can_Activate_Accel      : constant Glib.Signal_Name :=
                                      "can_activate_accel";
   Signal_Child_Notify            : constant Glib.Signal_Name :=
                                      "child_notify";
   Signal_Client_Event            : constant Glib.Signal_Name :=
                                      "client_event";
   Signal_Configure_Event         : constant Glib.Signal_Name :=
                                      "configure_event";
   Signal_Delete_Event            : constant Glib.Signal_Name :=
                                      "delete_event";
   Signal_Destroy                 : constant Glib.Signal_Name :=
                                      "destroy";
   Signal_Destroy_Event           : constant Glib.Signal_Name :=
                                      "destroy_event";
   Signal_Direction_Changed       : constant Glib.Signal_Name :=
                                      "direction_changed";
   Signal_Drag_Begin              : constant Glib.Signal_Name :=
                                      "drag_begin";
   Signal_Drag_Data_Delete        : constant Glib.Signal_Name :=
                                      "drag_data_delete";
   Signal_Drag_Data_Get           : constant Glib.Signal_Name :=
                                      "drag_data_get";
   Signal_Drag_Data_Received      : constant Glib.Signal_Name :=
                                      "drag_data_received";
   Signal_Drag_Drop               : constant Glib.Signal_Name :=
                                      "drag_drop";
   Signal_Drag_End                : constant Glib.Signal_Name :=
                                      "drag_end";
   Signal_Drag_Leave              : constant Glib.Signal_Name :=
                                      "drag_leave";
   Signal_Drag_Motion             : constant Glib.Signal_Name :=
                                      "drag_motion";
   Signal_Enter_Notify_Event      : constant Glib.Signal_Name :=
                                      "enter_notify_event";
   Signal_Event                   : constant Glib.Signal_Name :=
                                      "event";
   Signal_Event_After             : constant Glib.Signal_Name :=
                                      "event-after";
   Signal_Expose_Event            : constant Glib.Signal_Name :=
                                      "expose_event";
   Signal_Focus                   : constant Glib.Signal_Name :=
                                      "focus";
   Signal_Focus_In_Event          : constant Glib.Signal_Name :=
                                      "focus_in_event";
   Signal_Focus_Out_Event         : constant Glib.Signal_Name :=
                                      "focus_out_event";
   Signal_Grab_Broken_Event       : constant Glib.Signal_Name :=
                                      "grab_broken_event";
   Signal_Grab_Focus              : constant Glib.Signal_Name :=
                                      "grab_focus";
   Signal_Grab_Notify             : constant Glib.Signal_Name :=
                                      "grab_notify";
   Signal_Hide                    : constant Glib.Signal_Name :=
                                      "hide";
   Signal_Hierarchy_Changed       : constant Glib.Signal_Name :=
                                      "hierarchy_changed";
   Signal_Key_Press_Event         : constant Glib.Signal_Name :=
                                      "key_press_event";
   Signal_Key_Release_Event       : constant Glib.Signal_Name :=
                                      "key_release_event";
   Signal_Leave_Notify_Event      : constant Glib.Signal_Name :=
                                      "leave_notify_event";
   Signal_Map                     : constant Glib.Signal_Name :=
                                      "map";
   Signal_Map_Event               : constant Glib.Signal_Name :=
                                      "map_event";
   Signal_Mnemonic_Activate       : constant Glib.Signal_Name :=
                                      "mnemonic_activate";
   Signal_Motion_Notify_Event     : constant Glib.Signal_Name :=
                                      "motion_notify_event";
   Signal_No_Expose_Event         : constant Glib.Signal_Name :=
                                      "no_expose_event";
   Signal_Parent_Set              : constant Glib.Signal_Name :=
                                      "parent_set";
   Signal_Popup_Menu              : constant Glib.Signal_Name :=
                                      "popup_menu";
   Signal_Property_Notify_Event   : constant Glib.Signal_Name :=
                                      "property_notify_event";
   Signal_Proximity_In_Event      : constant Glib.Signal_Name :=
                                      "proximity_in_event";
   Signal_Proximity_Out_Event     : constant Glib.Signal_Name :=
                                      "proximity_out_event";
   Signal_Realize                 : constant Glib.Signal_Name :=
                                      "realize";
   Signal_Query_Tooltip           : constant Glib.Signal_Name :=
                                      "query-tooltip";
   Signal_Screen_Changed          : constant Glib.Signal_Name :=
                                      "screen_changed";
   Signal_Scroll_Event            : constant Glib.Signal_Name :=
                                      "scroll_event";
   Signal_Selection_Clear_Event   : constant Glib.Signal_Name :=
                                      "selection_clear_event";
   Signal_Selection_Get           : constant Glib.Signal_Name :=
                                      "selection_get";
   Signal_Selection_Notify_Event  : constant Glib.Signal_Name :=
                                      "selection_notify_event";
   Signal_Selection_Received      : constant Glib.Signal_Name :=
                                      "selection_received";
   Signal_Selection_Request_Event : constant Glib.Signal_Name :=
                                      "selection_request_event";
   Signal_Show                    : constant Glib.Signal_Name :=
                                      "show";
   Signal_Show_Help               : constant Glib.Signal_Name :=
                                      "show_help";
   Signal_Size_Allocate           : constant Glib.Signal_Name :=
                                      "size_allocate";
   Signal_Size_Request            : constant Glib.Signal_Name :=
                                      "size_request";
   Signal_State_Changed           : constant Glib.Signal_Name :=
                                      "state_changed";
   Signal_Style_Set               : constant Glib.Signal_Name :=
                                      "style_set";
   Signal_Unmap                   : constant Glib.Signal_Name :=
                                      "unmap";
   Signal_Unmap_Event             : constant Glib.Signal_Name :=
                                      "unmap_event";
   Signal_Unrealize               : constant Glib.Signal_Name :=
                                      "unrealize";
   Signal_Visibility_Notify_Event : constant Glib.Signal_Name :=
                                      "visibility_notify_event";
   Signal_Window_State_Event      : constant Glib.Signal_Name :=
                                      "window_state_event";

private

   type Gtk_Widget_Record is new Glib.Object.GObject_Record with null record;

   Name_Property                : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("name");
   Parent_Property              : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("parent");
   X_Property                   : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("x");
   Y_Property                   : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("y");
   Width_Property               : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("width");
   Height_Property              : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("height");
   Visible_Property             : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("visible");
   Sensitive_Property           : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("sensitive");
   App_Paintable_Property       : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("app_paintable");
   Can_Focus_Property           : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("can_focus");
   Has_Focus_Property           : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has_focus");
   Can_Default_Property         : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("can_default");
   Has_Default_Property         : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has_default");
   Receives_Default_Property    : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("receives_default");
   Composite_Child_Property     : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("composite_child");
   Style_Property               : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("style");
   Events_Property              : constant Gdk.Event.Property_Gdk_Event_Mask :=
     Gdk.Event.Build ("events");
   Prop_Extensions_Events_Property :
     constant Gdk.Types.Property_Gdk_Extension_Mode :=
     Gdk.Types.Build ("extension_events");
   Extension_Events_Property : constant Gdk.Types.Property_Gdk_Extension_Mode
     := Gdk.Types.Build ("extension-events");
   Height_Request_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("height-request");
   Is_Focus_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("is-focus");
   No_Show_All_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("no-show-all");
   Width_Request_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("width-request");
   Tooltip_Markup_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("tooltip-markup");
   Tooltip_Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("tooltip-text");
   Has_Tooltip_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-tooltip");
   Window_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("window");

   Cursor_Aspect_Ratio_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("cursor-aspect-ratio");
--     Cursor_Color_Property : constant Glib.Properties.Property_Boxed :=
--       Glib.Properties.Build ("cursor-color");
--     Draw_Border_Property : constant Glib.Properties.Property_Boxed :=
--       Glib.Properties.Build ("draw-border");
   Focus_Line_Pattern_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("focus-line-pattern");
   Focus_Line_Width_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("focus-line-width");
   Focus_Padding_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("focus-padding");
   Interior_Focus_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("interior-focus");
   Link_Color_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("link-color");
   Scroll_Arrow_Hlength_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("scroll-arrow-hlength");
   Scroll_Arrow_Vlength_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("scroll-arrow-vlength");
   --  Secondary_Cursor_Color_Property : constant
   --    Glib.Properties.Property_Boxed :=
   --    Glib.Properties.Build ("secondary-cursor-color");
   Separator_Height_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("separator-height");
   Separator_Width_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("separator-width");
   Visited_Link_Color_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("visited-link-color");
   Wide_Separators_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("wide-separators");

   pragma Import (C, Pop_Colormap, "gtk_widget_pop_colormap");
   pragma Import (C, Get_Type, "gtk_widget_get_type");
   pragma Import (C, Requisition_Get_Type, "gtk_requisition_get_type");
   pragma Import (C, Get_Default_Colormap, "gtk_widget_get_default_colormap");
   pragma Import (C, Get_Default_Visual, "gtk_widget_get_default_visual");
   pragma Import (C, Push_Colormap, "gtk_widget_push_colormap");
   pragma Import (C, Set_Default_Colormap, "gtk_widget_set_default_colormap");
   pragma Import (C, Set_Default_Size_Allocate_Handler,
                  "ada_gtk_widget_set_default_size_allocate_handler");
   pragma Import (C, Default_Expose_Event_Handler,
                  "ada_gtk_default_expose_event_handler");
   pragma Import (C, Push_Composite_Child, "gtk_widget_push_composite_child");
   pragma Import (C, Pop_Composite_Child, "gtk_widget_pop_composite_child");
   pragma Import
     (C, Get_Default_Direction, "gtk_widget_get_default_direction");
   pragma Import
     (C, Set_Default_Direction, "gtk_widget_set_default_direction");
   pragma Import
     (C, Class_Install_Style_Property,
      "gtk_widget_class_install_style_property");

   pragma Inline (Toplevel_Is_Set);
   pragma Inline (No_Window_Is_Set);
   pragma Inline (Realized_Is_Set);
   pragma Inline (Mapped_Is_Set);
   pragma Inline (Visible_Is_Set);
   pragma Inline (Drawable_Is_Set);
   pragma Inline (Is_Sensitive);
   pragma Inline (Can_Focus_Is_Set);
   pragma Inline (Has_Focus_Is_Set);
   pragma Inline (Has_Default_Is_Set);
   pragma Inline (Has_Grab_Is_Set);
   pragma Inline (Rc_Style_Is_Set);
   pragma Inline (Double_Buffered_Is_Set);

end Gtk.Widget;

--  Not needed, so not bound
--  No binding: gtk_requisition_copy
--  No binding: gtk_requisition_free
--  No binding: gtk_widget_ref
--  No binding: gtk_widget_unref
--  No binding: gtk_widget_new
--  No binding: gtk_widget_destroy
--  No binding: gtk_widget_destroyed
--  No binding: gtk_widget_get_accessible
--  No binding: gtk_widget_get_display
--  No binding: gtk_widget_get_screen
--  No binding: gtk_widget_style_get_valist
--  No binding: gtk_widget_set
--  No binding: gtk_widget_style_get

--  Might be useful, but very complex to explain
--  No binding: gtk_widget_list_accel_closures
--  No binding: gtk_widget_class_install_style_property_parser

--  Binding is in Gtk.RC
--  No binding: gtk_widget_modify_style
--  No binding: gtk_widget_get_modifier_style

--  Binding is in Gtk.Setting
--  No binding: gtk_widget_get_settings

--  Binding is in Gtk.Clipboard
--  No binding: gtk_widget_get_clipboard

--  Binding uses custom C glue
--  No binding: gtk_widget_get_window
