-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000, E. Briot, J. Brobecker and A. Charlet  --
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

--  A Gdk_Window is the physical window that appears on the screen.
--  This is the low-level structure known to the X server or to Win32.
--  All the widgets are internally associated with a specific Gdk_Window,
--  that holds attributes such as the window decoration, whether the window
--  can be interactively resized,...
--
--  On some systems, the graphic server knows how to display non-rectangular
--  windows (this is part of the X extensions).
--
--  If you simply want to create a simply window, you should instead look
--  at the functions provided in Gtk.Window and Gtk.Widget, which are higher
--  level than these. See also the function Gtk.Widget.Get_Window to get the
--  Gdk_Window associated with a widget. Be aware that some of them don't have
--  such windows!
--
--  Scrolling can be implemented in several ways with GtkAda (toplevel
--  scrolling should be done with the Gtk_Scrolled_Window widget, but you
--  might want to handle scrolling yourself). See the function
--  Gdk.Event.Get_Graphics_Expose for more information.
--  <c_version>1.3.6</c_version>
--  <c_version>2.12</c_version> for some of the functions
--  <group>Gdk, the low-level API</group>

with System;
with Glib; use Glib;
with Glib.Object;
with Glib.Generic_Properties; use Glib.Generic_Properties;
pragma Elaborate_All (Glib.Generic_Properties);
with Glib.Glist;
pragma Elaborate_All (Glib.Glist);
with Cairo;
with Gdk;
with Gdk.Color;
with Gdk.Cursor;
with Gdk.Event;
with Gdk.Rectangle;
with Gdk.Types;
with Unchecked_Conversion;

package Gdk.Window is

   subtype Gdk_Window is Gdk.Gdk_Window;

   Null_Window : constant Gdk_Window;

   type Gdk_Window_Class is (Input_Output, Input_Only);

   type Gdk_Window_Type is
     (Window_Root,
      --  there is only one root window and it is initialized at startup.
      --  Creating a window of type Window_Root is an error.

      Window_Toplevel,
      --  Windows which interact with the window manager.

      Window_Child,
      --  Windows which are children of some other type of window.
      --  (Any other type of window). Most windows are child windows.

      Window_Dialog,
      --  A special kind of toplevel window which interacts with the window
      --  manager slightly differently than a regular toplevel window.
      --  Dialog windows should be used for any transient window.

      Window_Temp,
      --  ???

      Window_Foreign
      --  A window that actually belongs to another application.
     );
   --  Type of windows.

   type Gdk_Window_Attributes_Type is mod 2 ** 32;
   Wa_Title    : constant Gdk_Window_Attributes_Type := 2 ** 1;
   Wa_X        : constant Gdk_Window_Attributes_Type := 2 ** 2;
   Wa_Y        : constant Gdk_Window_Attributes_Type := 2 ** 3;
   Wa_Cursor   : constant Gdk_Window_Attributes_Type := 2 ** 4;
   Wa_Colormap : constant Gdk_Window_Attributes_Type := 2 ** 5;
   Wa_Visual   : constant Gdk_Window_Attributes_Type := 2 ** 6;
   Wa_Wmclass  : constant Gdk_Window_Attributes_Type := 2 ** 7;
   Wa_Noredir  : constant Gdk_Window_Attributes_Type := 2 ** 8;

   type Gdk_Window_Hints is mod 2 ** 32;
   --  Size restriction.
   Gdk_Hint_Pos        : constant Gdk_Window_Hints := 2 ** 0;
   Gdk_Hint_Min_Size   : constant Gdk_Window_Hints := 2 ** 1;
   Gdk_Hint_Max_Size   : constant Gdk_Window_Hints := 2 ** 2;
   Gdk_Hint_Base_Size  : constant Gdk_Window_Hints := 2 ** 3;
   Gdk_Hint_Aspect     : constant Gdk_Window_Hints := 2 ** 4;
   Gdk_Hint_Resize_Inc : constant Gdk_Window_Hints := 2 ** 5;

   type Gdk_Window_Type_Hint is
     (Window_Type_Hint_Normal,
      --  Normal toplevel window

      Window_Type_Hint_Dialog,
      --  Dialog window

      Window_Type_Hint_Menu,
      --  Window used to implement a menu.

      Window_Type_Hint_Toolbar
      --  Toolbar: Window used to implement toolbars.
     );
   --  Hints for the window manager that indicate what type of function the
   --  window has. The window manager can use this when determining decoration
   --  and behaviour of the window. The hint must be set before mapping the
   --  window.

   type Gdk_Wm_Decoration is mod 2 ** 32;
   Decor_All      : constant Gdk_Wm_Decoration := 2 ** 0;
   Decor_Border   : constant Gdk_Wm_Decoration := 2 ** 1;
   Decor_Resize_H : constant Gdk_Wm_Decoration := 2 ** 2;
   Decor_Title    : constant Gdk_Wm_Decoration := 2 ** 3;
   Decor_Menu     : constant Gdk_Wm_Decoration := 2 ** 4;
   Decor_Minimize : constant Gdk_Wm_Decoration := 2 ** 5;
   Decor_Maximize : constant Gdk_Wm_Decoration := 2 ** 6;

   type Gdk_Wm_Function is mod 2 ** 32;
   Func_All      : constant Gdk_Wm_Function := 2 ** 0;
   Func_Resize   : constant Gdk_Wm_Function := 2 ** 1;
   Func_Move     : constant Gdk_Wm_Function := 2 ** 2;
   Func_Minimize : constant Gdk_Wm_Function := 2 ** 3;
   Func_Maximize : constant Gdk_Wm_Function := 2 ** 4;
   Func_Close    : constant Gdk_Wm_Function := 2 ** 5;

   type Gdk_Gravity is
     (Gravity_North_West,
      Gravity_North,
      Gravity_North_East,
      Gravity_West,
      Gravity_Center,
      Gravity_East,
      Gravity_South_West,
      Gravity_South,
      Gravity_South_East,
      Gravity_Static);

   type Gdk_Window_Edge is
     (Window_Edge_North_West,
      Window_Edge_North,
      Window_Edge_North_East,
      Window_Edge_West,
      Window_Edge_East,
      Window_Edge_South_West,
      Window_Edge_South,
      Window_Edge_South_East);

   type Gdk_Geometry is record
      Min_Width   : Gint;
      Min_Height  : Gint;
      Max_Width   : Gint;
      Max_Height  : Gint;
      Base_Width  : Gint;
      Base_Height : Gint;
      Width_Inc   : Gint;
      Height_Inc  : Gint;
      Min_Aspect  : Gdouble;
      Max_Aspect  : Gdouble;
      Win_Gravity : Gdk_Gravity;
   end record;

   procedure Gdk_New
     (Window          : out Gdk_Window;
      Parent          : Gdk_Window;
      Attributes      : Gdk_Window_Attr;
      Attributes_Mask : Gdk_Window_Attributes_Type);
   --  Creates a new gdk_window.
   --  There are few reasons for creating such windows yourself, and almost
   --  none if you are not creating a new widget.
   --  One nice thing with using such a window (rather than drawing directly on
   --  a gtk_widget is that you can get separate Events for this window
   --  (Expose, Button_Press, ...) without having do detect yourself where the
   --  event applied.
   --  Note that you should almost always call Set_User_Data on the newly
   --  created window, so that events are redirected to a specific widget.
   --
   --  You cannot pass a null value for Attributes.
   --
   --  Attributes_Mask indicates which fields are relevant in Attributes. Some
   --  of the fields are always taken into account, and thus do not have an
   --  associated mask.
   --
   --  See the package Gdk.Window_Attr for more information on window
   --  attributes.
   --
   --  Changing the background color of the window can be done through
   --  Gtk.Style.Set_Background

   procedure Set_User_Data
     (Window : Gdk.Gdk_Window;
      Widget : access Glib.Object.GObject_Record'Class);
   --  Sets a special field in the window.
   --  All the events reported by the Xserver (or the Windows server) for
   --  Window will be redirected to Widget through the standard signals
   --  "expose_event", "button_press_event", ...
   --  You almost always need to call this function after creating a new
   --  Gdk_Window yourself, or you won't be able to handle the events.

   function Get_User_Data
     (Window : Gdk.Gdk_Window) return Glib.Object.GObject;
   --  Return the widget to which events are reported when they happen on
   --  Window. This is the widget that was set through the call to
   --  Set_User_data.

   function Get_Type return Glib.GType;
   --  Return the internal lue associated with Gdk_Window.

   procedure Destroy (Window : in out Gdk_Window);
   --  Destroy a window and all its children.

   type Gdk_Filter_Return is
     (Continue,  --  Event not handled, continue processing
      Translate, --  Translated event stored
      Remove);   --  Terminate processing, removing event

   type Gdk_Filter_Func is access function
     (System_Event : C_Proxy;
      Event        : Gdk.Event.Gdk_Event;
      Data         : System.Address) return Gdk_Filter_Return;
   pragma Convention (C, Gdk_Filter_Func);
   --  A filter function, that will be called before the standard processing
   --  in gtk+. System_Event is the raw event from the system,
   --
   --  Event hasn't been set when this function is set, and the function should
   --  set it to a meaningful value if it returns Translate.
   --
   --  Data is the user_data that was passed with Add_Filter.

   procedure Add_Filter
     (Window : Gdk.Gdk_Window;
      Filter : Gdk_Filter_Func;
      Data   : System.Address);
   --  Add an event filter to Window, allowing you to intercept events
   --  before they reach GDK. This is a low-level operation and makes it
   --  easy to break GDK and/or GTK+, so you have to know what you're
   --  doing. Pass null for Window to get all events for all windows,
   --  instead of events for a specific window.
   --
   --  This can be used for a temporary keyboard grab, although you should
   --  consider using Gdk.Main.Keyboard_Grab instead.

   procedure Remove_Filter
     (Window : Gdk.Gdk_Window;
      Filter : Gdk_Filter_Func;
      Data   : System.Address);
   --  Removing the filter that was previously associated with Filter and Data

   function Get_Window_Type (Window : Gdk_Window) return Gdk_Window_Type;

   procedure Window_At_Pointer
     (Win_X  : out Gint;
      Win_Y  : out Gint;
      Window : out Gdk_Window);
   --  Return the window and the coordinates corresponding to the current
   --  position of the cursor.

   procedure Show (Window : Gdk_Window);

   procedure Show_Unraised (Window : Gdk_Window);
   --  Show Window on screen, but does not modify its stacking order. In
   --  contrast, Show will raise the window to the top of the window stack.

   procedure Hide (Window : Gdk_Window);

   procedure Withdraw (Window : Gdk_Window);

   procedure Move
     (Window : Gdk_Window;
      X      : Gint;
      Y      : Gint);

   procedure Resize
     (Window : Gdk_Window;
      Width  : Gint;
      Height : Gint);

   procedure Move_Resize
     (Window : Gdk_Window;
      X      : Gint;
      Y      : Gint;
      Width  : Gint;
      Height : Gint);

   procedure Reparent
     (Window     : Gdk_Window;
      New_Parent : Gdk_Window;
      X          : Gint;
      Y          : Gint);

   procedure Clear (Window : Gdk_Window);

   procedure Clear_Area
     (Window : Gdk_Window;
      X      : Gint;
      Y      : Gint;
      Width  : Gint;
      Height : Gint);
   --  Does not generate an expose event.

   procedure Clear_Area_E
     (Window : Gdk_Window;
      X      : Gint;
      Y      : Gint;
      Width  : Gint;
      Height : Gint);
   --  Same as Clear_Area, but generates an expose event.

   procedure Copy_Area
     (Window        : Gdk_Window;
      Gc            : Gdk.Gdk_GC;
      X             : Gint;
      Y             : Gint;
      Source_Window : Gdk_Window;
      Source_X      : Gint;
      Source_Y      : Gint;
      Width         : Gint;
      Height        : Gint);
   --  Obsolete. Use Gdk.Drawable.Draw_Drawable instead.

   function Create_Similar_Surface
     (Window  : Gdk_Window;
      Content : Cairo.Cairo_Content;
      Width   : Glib.Gint;
      Height  : Glib.Gint) return Cairo.Cairo_Surface;
   --  Same as Cairo.Surface.Create_Similar, using Windows as similar surface.

   procedure Gdk_Raise (Window : Gdk_Window);

   procedure Lower (Window : Gdk_Window);

   procedure Focus (Window : Gdk_Window; Timestamp : Guint32);

   procedure Set_Override_Redirect
     (Window            : Gdk_Window;
      Override_Redirect : Boolean := True);

   procedure Scroll (Window : Gdk_Window; Dx, Dy : Gint);

   procedure Shape_Combine_Mask
     (Window     : Gdk_Window;
      Shape_Mask : Gdk.Gdk_Bitmap;
      Offset_X   : Gint;
      Offset_Y   : Gint);
   --  Allow for making shaped (partially transparent) windows.
   --  This featureis  needed for Drag and Drop for example.
   --  Shape_Mask can be the mask from Gdk.Pixmap.Create_From_Xpm.

   procedure Shape_Combine_Region
     (Window       : Gdk_Window;
      Shape_Region : Gdk.Gdk_Region;
      Offset_X     : Gint;
      Offset_Y     : Gint);

   procedure Set_Child_Shapes (Window : Gdk_Window);
   --  Quickly take the shapes of all the child windows of a window and use
   --  their shapes as the shape mask for this window - useful for container
   --  windows that do not want to look like a big box.

   procedure Merge_Child_Shapes (Window : Gdk_Window);
   --  Merge (ie add) child shapes to your own window's shape keeping its
   --  current shape and adding the child shapes to it.

   function Is_Visible (Window : Gdk_Window) return Boolean;

   function Is_Viewable (Window : Gdk_Window) return Boolean;

   function Get_State (Window : Gdk_Window) return Gdk.Event.Gdk_Window_State;
   --  Return the current state of the Windows.
   --  See Gdk.Event.Gdk_Window_State for more details.

   function Set_Static_Gravities
     (Window     : Gdk_Window;
      Use_Static : Boolean) return Boolean;

   procedure Set_Hints
     (Window     : Gdk_Window;
      X          : Gint;
      Y          : Gint;
      Min_Width  : Gint;
      Min_Height : Gint;
      Max_Width  : Gint;
      Max_Height : Gint;
      Flags      : Gdk_Window_Hints);

   procedure Set_Type_Hint
     (Window : Gdk_Window;
      Hint   : Gdk_Window_Type_Hint);

   procedure Set_Modal_Hint
     (Window : Gdk_Window;
      Modal  : Boolean);

   procedure Set_Geometry_Hints
     (Window   : Gdk_Window;
      Geometry : in out Gdk_Geometry;
      Flags    : Gdk_Window_Hints);

   procedure Set_Title (Window : Gdk_Window; Title : UTF8_String);

   procedure Set_Role (Window : Gdk_Window; Role : String);

   procedure Set_Transient_For
     (Window : Gdk_Window; Leader : Gdk_Window);

   procedure Set_Opacity (Window : Gdk_Window; Opacity : Gdouble);
   --  Request the windowing system to make Window partially transparent, with
   --  opacity 0.0 being fully transparent and 1.0 fully opaque (Values of the
   --  opacity parameter are clamped to the [0,1] range).
   --
   --  On X11, this works only on X screens with a compositing manager running
   --  (see Gdk.Screen.Is_Composited)
   --
   --  For setting up per-pixel alpha, see Gdk.Screen.Get_Rgba_Colormap
   --  For making non-toplevel windows translucent, see Set_Composited
   --
   --  Since: gtk+ 2.12

   procedure Set_Composited (Window : Gdk_Window; Composited : Boolean);
   --  Sets Window as composited, or unsets it. Composited windows do not
   --  automatically have their contents drawn to the screen. Drawing is
   --  redirected to an offscreen buffer and an expose event is emitted on the
   --  parent of the composited window. It is the responsibility of the
   --  parent's expose handler to manually merge the off-screen content onto
   --  the screen in whatever way it sees fit.
   --
   --  It only makes sense for child windows to be composited; see Set_Opacity
   --  if you need translucent toplevel windows.
   --
   --  An additional effect of this call is that the area of this window is no
   --  longer clipped from regions marked for invalidation on its parent. Draws
   --  done on the parent window are also no longer clipped by the child.
   --
   --  This call is only supported on some systems (currently, only X11 with
   --  new enough Xcomposite and Xdamage extensions). You must call
   --  gdk_display_supports_composite() to check if setting a window as
   --  composited is supported before attempting to do so.
   --
   --  Since: 2.12

   procedure Set_Background
     (Window : Gdk_Window; Color : Gdk.Color.Gdk_Color);

   procedure Set_Back_Pixmap
     (Window          : Gdk_Window;
      Pixmap          : Gdk.Gdk_Pixmap;
      Parent_Relative : Boolean);

   procedure Set_Cursor
     (Window : Gdk_Window; Cursor : Gdk.Cursor.Gdk_Cursor);
   --  Note: the window must be realized first, ie have an associated X11/Win32
   --  window.

   procedure Get_Geometry
     (Window : Gdk_Window;
      X      : out Gint;
      Y      : out Gint;
      Width  : out Gint;
      Height : out Gint;
      Depth  : out Gint);
   --  You can get the size of the root window (ie the size of the screen)
   --  simply by giving Null_Window as the first argument to this procedure.

   procedure Get_Position
     (Window : Gdk_Window;
      X      : out Gint;
      Y      : out Gint);

   procedure Get_Origin
     (Window  : Gdk_Window;
      X       : out Gint;
      Y       : out Gint;
      Success : out Boolean);
   --  Obtains the position of a window in root window coordinates. (Compare
   --  with Get_Position and Get_Geometry which return the position of a window
   --  relative to its parent window)

   procedure Get_Desk_Relative_Origin
     (Window  : Gdk_Window;
      X       : out Gint;
      Y       : out Gint;
      Success : out Boolean);

   procedure Get_Root_Origin
     (Window : Gdk_Window;
      X      : out Gint;
      Y      : out Gint);
   --  Obtains the top-left corner of the window manager frame in root window
   --  coordinates.

   procedure Get_Frame_Extents
     (Window : Gdk_Window;
      Rect   : Gdk.Rectangle.Gdk_Rectangle);

   procedure Get_Pointer
     (Window : Gdk_Window;
      X      : out Gint;
      Y      : out Gint;
      Mask   : out Gdk.Types.Gdk_Modifier_Type;
      Result : out Gdk_Window);

   function Get_Parent (Window : Gdk_Window) return Gdk_Window;

   function Get_Toplevel (Window : Gdk_Window) return Gdk_Window;

   --  Gdk_Window_List
   --
   function Convert is new Unchecked_Conversion (Gdk_Window, System.Address);
   function Convert is new Unchecked_Conversion (System.Address, Gdk_Window);

   package Gdk_Window_List is new
     Glib.Glist.Generic_List (Gpointer => Gdk_Window);

   function Get_Children (Window : Gdk_Window) return Gdk_Window_List.Glist;

   function Peek_Children (Window : Gdk_Window) return Gdk_Window_List.Glist;

   function Get_Events (Window : Gdk_Window) return Gdk.Event.Gdk_Event_Mask;

   procedure Set_Events
     (Window : Gdk_Window; Event_Mask : Gdk.Event.Gdk_Event_Mask);

   procedure Set_Icon
     (Window      : Gdk_Window;
      Icon_Window : Gdk_Window;
      Pixmap      : Gdk_Pixmap;
      Mask        : Gdk_Bitmap);
   --  Currently not supported under Windows

   procedure Set_Icon_Name
     (Window : Gdk_Window;
      Name   : UTF8_String);

   procedure Set_Group (Window : Gdk_Window; Leader : Gdk_Window);
   --  Sets the group leader window for window. By default, GDK sets the group
   --  leader for all toplevel windows to a global window implicitly created by
   --  GDK. With this function you can override this default.
   --
   --  The group leader window allows the window manager to distinguish all
   --  windows that belong to a single application. It may for example allow
   --  users to minimize/unminimize all windows belonging to an application at
   --  once. You should only set a non-default group window if your application
   --  pretends to be multiple applications.

   procedure Set_Decorations
     (Window      : Gdk_Window;
      Decorations : Gdk_Wm_Decoration);

   procedure Get_Decorations
     (Window      : Gdk_Window;
      Decorations : out Gdk_Wm_Decoration;
      Success     : out Boolean);

   procedure Set_Functions
     (Window    : Gdk_Window;
      Functions : Gdk_Wm_Function);

   procedure Invalidate_Rect
     (Window              : Gdk_Window;
      Rectangle           : Gdk.Rectangle.Gdk_Rectangle;
      Invalidate_Children : Boolean);

   function Get_Toplevels return Gdk_Window_List.Glist;
   --  The returned list must be freed by calling Gdk_Window_List.Free.
   --  Consider using Gtk.Window.List_Toplevels instead.

   procedure Iconify (Window : Gdk_Window);

   procedure Deiconify (Window : Gdk_Window);

   procedure Stick (Window : Gdk_Window);

   procedure Unstick (Window : Gdk_Window);

   procedure Maximize (Window : Gdk_Window);

   procedure Unmaximize (Window : Gdk_Window);

   procedure Register_Dnd (Window : Gdk_Window);

   function Get_Update_Area (Window : Gdk_Window) return Gdk_Region;

   procedure Freeze_Updates (Window : Gdk_Window);

   procedure Thaw_Updates (Window : Gdk_Window);

   procedure Process_All_Updates;

   procedure Process_Updates
     (Window : Gdk_Window; Update_Children : Boolean := True);

   procedure Set_Debug_Updates (Setting : Boolean := True);

   procedure Ref (Window : Gdk_Window);
   --  Increment the reference counter associated with window.

   procedure Unref (Window : Gdk_Window);
   --  Decrement the reference counter associated with window.

   function Get_Window_Id (Window : Gdk_Window) return System.Address;
   --  Return the target specific window id.
   --  Under Windows, this returns a HWND object.
   --  Under X, this returns a Window object.

   pragma Convention (C, Gdk_Window_Type_Hint);

   pragma Convention (C, Gdk_Gravity);
   for Gdk_Gravity use
     (Gravity_North_West => 1,
      Gravity_North      => 2,
      Gravity_North_East => 3,
      Gravity_West       => 4,
      Gravity_Center     => 5,
      Gravity_East       => 6,
      Gravity_South_West => 7,
      Gravity_South      => 8,
      Gravity_South_East => 9,
      Gravity_Static     => 10);

   package Window_Type_Hint_Properties is new
     Generic_Internal_Discrete_Property (Gdk_Window_Type_Hint);
   package Gravity_Properties is new Generic_Internal_Discrete_Property
     (Gdk_Gravity);

   type Property_Window_Type_Hint  is new Window_Type_Hint_Properties.Property;
   type Property_Gravity           is new Gravity_Properties.Property;

private

   Null_Window : constant Gdk_Window := null;
   pragma Import (C, Get_Type, "gdk_window_object_get_type");
   pragma Import (C, Add_Filter, "gdk_window_add_filter");
   pragma Import (C, Clear, "gdk_window_clear");
   pragma Import (C, Clear_Area, "gdk_window_clear_area");
   pragma Import (C, Clear_Area_E, "gdk_window_clear_area_e");
   pragma Import (C, Focus, "gdk_window_focus");
   pragma Import (C, Scroll, "gdk_window_scroll");
   pragma Import (C, Shape_Combine_Mask, "gdk_window_shape_combine_mask");
   pragma Import (C, Shape_Combine_Region, "gdk_window_shape_combine_region");
   pragma Import (C, Get_State, "gdk_window_get_state");
   pragma Import (C, Set_Type_Hint, "gdk_window_set_type_hint");
   pragma Import (C, Get_Frame_Extents, "gdk_window_get_frame_extents");
   pragma Import (C, Iconify, "gdk_window_iconify");
   pragma Import (C, Deiconify, "gdk_window_deiconify");
   pragma Import (C, Stick, "gdk_window_stick");
   pragma Import (C, Unstick, "gdk_window_unstick");
   pragma Import (C, Maximize, "gdk_window_maximize");
   pragma Import (C, Unmaximize, "gdk_window_unmaximize");
   pragma Import (C, Register_Dnd, "gdk_window_register_dnd");
   pragma Import (C, Get_Update_Area, "gdk_window_get_update_area");
   pragma Import (C, Freeze_Updates, "gdk_window_freeze_updates");
   pragma Import (C, Thaw_Updates, "gdk_window_thaw_updates");
   pragma Import (C, Process_All_Updates, "gdk_window_process_all_updates");
   pragma Import (C, Get_Events, "gdk_window_get_events");
   pragma Import (C, Get_Geometry, "gdk_window_get_geometry");
   pragma Import (C, Get_Parent, "gdk_window_get_parent");
   pragma Import (C, Get_Position, "gdk_window_get_position");
   pragma Import (C, Get_Root_Origin, "gdk_window_get_root_origin");
   pragma Import (C, Get_Toplevel, "gdk_window_get_toplevel");
   pragma Import (C, Get_Window_Type, "gdk_window_get_window_type");
   pragma Import (C, Hide, "gdk_window_hide");
   pragma Import (C, Lower, "gdk_window_lower");
   pragma Import (C, Merge_Child_Shapes, "gdk_window_merge_child_shapes");
   pragma Import (C, Move, "gdk_window_move");
   pragma Import (C, Move_Resize, "gdk_window_move_resize");
   pragma Import (C, Gdk_Raise, "gdk_window_raise");
   pragma Import (C, Ref, "gdk_drawable_ref");
   pragma Import (C, Remove_Filter, "gdk_window_remove_filter");
   pragma Import (C, Reparent, "gdk_window_reparent");
   pragma Import (C, Resize, "gdk_window_resize");
   pragma Import (C, Set_Child_Shapes, "gdk_window_set_child_shapes");
   pragma Import (C, Set_Decorations, "gdk_window_set_decorations");
   pragma Import (C, Set_Events, "gdk_window_set_events");
   pragma Import (C, Set_Functions, "gdk_window_set_functions");
   pragma Import (C, Set_Geometry_Hints, "gdk_window_set_geometry_hints");
   pragma Import (C, Set_Group, "gdk_window_set_group");
   pragma Import (C, Set_Hints, "gdk_window_set_hints");
   pragma Import (C, Set_Transient_For, "gdk_window_set_transient_for");
   pragma Import (C, Show, "gdk_window_show");
   pragma Import (C, Show_Unraised, "gdk_window_show_unraised");
   pragma Import (C, Unref, "gdk_drawable_unref");
   pragma Import (C, Withdraw, "gdk_window_withdraw");
   pragma Import (C, Set_Cursor, "gdk_window_set_cursor");
   pragma Import (C, Set_Icon, "gdk_window_set_icon");
   pragma Import (C, Get_Window_Id, "ada_gdk_get_window_id");
   pragma Import (C, Set_Opacity, "gdk_window_set_opacity");
   pragma Import
     (C, Create_Similar_Surface, "gdk_window_create_similar_surface");

   pragma Convention (C, Gdk_Window_Type);

   pragma Convention (C, Gdk_Window_Class);

   pragma Convention (C, Gdk_Window_Edge);

end Gdk.Window;

--  missing:
--  gdk_set_sm_client_id
--  gdk_window_begin_paint_rect
--  gdk_window_begin_paint_region
--  gdk_window_end_paint
--  gdk_window_begin_resize_drag
--  gdk_window_begin_move_drag
--  gdk_window_invalidate_region
--  gdk_window_constrain_size
