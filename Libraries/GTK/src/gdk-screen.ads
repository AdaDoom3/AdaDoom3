-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2006-2013, AdaCore                   --
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
--  Gdk_Screen objects are the GDK representation of a physical screen. It is
--  used throughout GDK and GTK+ to specify which screen the top level windows
--  are to be displayed on. It is also used to query the screen specification
--  and default settings such as the default colormap (Get_Default_Colormap),
--  the screen width (Get_Width), etc.
--
--  Note that a screen may consist of multiple monitors which are merged to
--  form a large screen area.
--  </description>
--  <c_version>2.8.17</c_version>
--  <c_version>2.12</c_version> for some of the functions
--  <group>Gdk, the low-level API</group>
--  <see>Gdk_Display</see>

with Gdk.Display;
with Gdk.Rectangle;
with Gdk.Types;
with Glib.Object;
with Glib.Values;

package Gdk.Screen is

   type Gdk_Screen_Record is new Glib.Object.GObject_Record with null record;
   type Gdk_Screen is access all Gdk_Screen_Record'Class;

   function Get_Type return Glib.GType;
   --  Return the internal type used for screens

   -------------
   -- Display --
   -------------
   --  These subprograms should really be in gdk-display.ads to match what is
   --  done for gtk+ itself, but that would create dependency circularities.
   --  Ada 2005 has support for these, but we want GtkAda to build with Ada95
   --  compilers.

   function Get_Screen
     (Display    : access Gdk.Display.Gdk_Display_Record'Class;
      Screen_Num : Glib.Gint)
      return Gdk_Screen;
   --  Returns a screen object for one of the screens of the display.

   function Get_Default_Screen
     (Display : access Gdk.Display.Gdk_Display_Record'Class) return Gdk_Screen;
   --  Get the default Gdk_Screen for display.

   procedure Get_Pointer
     (Display : access Gdk.Display.Gdk_Display_Record'Class;
      Screen  : out Gdk_Screen;
      X       : out Glib.Gint;
      Y       : out Glib.Gint;
      Mask    : out Gdk.Types.Gdk_Modifier_Type);
   --  Gets the current location of the pointer and the current modifier
   --  mask for a given display.
   --  (X, Y) are coordinates relative to the root window on the display

   procedure Warp_Pointer
     (Display : access Gdk.Display.Gdk_Display_Record'Class;
      Screen  : access Gdk_Screen_Record;
      X       : Glib.Gint;
      Y       : Glib.Gint);
   --  Warps the pointer of display to the point x,y on the screen screen,
   --  unless the pointer is confined to a window by a grab, in which case it
   --  will be moved as far as allowed by the grab. Warping the pointer creates
   --  events as if the user had moved the mouse instantaneously to the
   --  destination.
   --
   --  Note that the pointer should normally be under the control of the user.
   --  This function was added to cover some rare use cases like keyboard
   --  navigation support for the color picker in the GtkColorSelectionDialog.

   -------------
   -- Screens --
   -------------

   function Get_Default return Gdk_Screen;
   --  Gets the default screen for the default display

   function Get_Display
     (Screen : access Gdk_Screen_Record) return Gdk.Display.Gdk_Display;
   --  Gets the display to which the screen belongs.

   procedure Set_Default_Colormap
     (Screen   : access Gdk_Screen_Record;
      Colormap : Gdk.Gdk_Colormap);
   function Get_Default_Colormap
     (Screen : access Gdk_Screen_Record) return Gdk.Gdk_Colormap;
   --  Gets the default colormap for screen.

   function Get_System_Colormap
     (Screen : access Gdk_Screen_Record) return Gdk.Gdk_Colormap;
   --  Gets the system's colormap for screen

   function Get_System_Visual
     (Screen : access Gdk_Screen_Record) return Gdk.Gdk_Visual;
   --  Get the system's default visual for screen. This is the visual for the
   --  root window of the display. The return value should not be freed.

   function Get_Rgb_Colormap
     (Screen : access Gdk_Screen_Record) return Gdk.Gdk_Colormap;
   --  Gets the preferred colormap for rendering image data on screen. Not a
   --  very useful function; historically, GDK could only render RGB image data
   --  to one colormap and visual, but in the current version it can render to
   --  any colormap and visual. So there's no need to call this function.

   function Get_Rgb_Visual
     (Screen : access Gdk_Screen_Record) return Gdk.Gdk_Visual;
   --  Gets a "preferred visual" chosen by GdkRGB for rendering image data on
   --  screen. In previous versions of GDK, this was the only visual GdkRGB
   --  could use for rendering. In current versions, it's simply the visual
   --  GdkRGB would have chosen as the optimal one in those previous versions.
   --  GdkRGB can now render to drawables with any visual.

   function Get_Rgba_Colormap
     (Screen : access Gdk_Screen_Record) return Gdk.Gdk_Colormap;
   --  Gets a colormap to use for creating windows or pixmaps with an alpha
   --  channel. The windowing system on which GTK+ is running may not support
   --  this capability, in which case NULL will be returned. Even if a non-NULL
   --  value is returned, its possible that the window's alpha channel won't be
   --  honored when displaying the window on the screen: in particular, for X
   --  an appropriate windowing manager and compositing manager must be running
   --  to provide appropriate display.

   function Get_Rgba_Visual
     (Screen : access Gdk_Screen_Record) return Gdk.Gdk_Visual;
   --  Gets a visual to use for creating windows or pixmaps with an alpha
   --  channel. See the docs for Get_Rgba_Colormap for caveats.

   function Get_Root_Window
     (Screen : access Gdk_Screen_Record) return Gdk.Gdk_Window;
   --  Gets the root window of screen.

   function Get_Number (Screen : access Gdk_Screen_Record) return Glib.Gint;
   --  Gets the index of screen among the screens in the display to which it
   --  belongs.

   function Get_Width  (Screen : access Gdk_Screen_Record) return Glib.Gint;
   function Get_Height (Screen : access Gdk_Screen_Record) return Glib.Gint;
   --  Gets the size of screen in pixels

   function Get_Width_Mm  (Screen : access Gdk_Screen_Record) return Glib.Gint;
   function Get_Height_Mm (Screen : access Gdk_Screen_Record) return Glib.Gint;
   --  Gets the width of screen in millimeters. Note that on some X servers
   --  this value will not be correct.

   function Make_Display_Name
     (Screen : access Gdk_Screen_Record) return String;
   --  Determines the name to pass to Gdk.Display.Open to get a GdkDisplay with
   --  this screen as the default screen.

   function Get_N_Monitors
     (Screen : access Gdk_Screen_Record) return Glib.Gint;
   --  Returns the number of monitors which screen consists of.

   function Is_Composited (Screen : access Gdk_Screen_Record) return Boolean;
   --  Returns whether windows with an RGBA visual can reasonably be expected
   --  to have their alpha channel drawn correctly on the screen.
   --
   --  On X11 this function returns whether a compositing manager is
   --  compositing Screen.
   --
   --  Since: 2.10

   --------------
   -- Monitors --
   --------------

   procedure Get_Monitor_Geometry
     (Screen      : access Gdk_Screen_Record;
      Monitor_Num : Glib.Gint;
      Dest        : out Gdk.Rectangle.Gdk_Rectangle);
   --  Retrieves the Gdk_Rectangle representing the size and position of the
   --  individual monitor within the entire screen area.
   --  Note that the size of the entire screen area can be retrieved via
   --  Get_Width and Get_Height.

   function Get_Monitor_At_Point
     (Screen : access Gdk_Screen_Record;
      X      : Glib.Gint;
      Y      : Glib.Gint)
      return Glib.Gint;
   --  Returns the monitor number in which the point (X,Y) is located.
   --  These are coordinates within the virtual screen. The closest monitor is
   --  returned when (X, Y) is not in any monitor

   function Get_Monitor_At_Window
     (Screen : access Gdk_Screen_Record;
      Window : Gdk.Gdk_Window) return Glib.Gint;
   --  Returns the number of the monitor in which the largest area of the
   --  bounding rectangle of Window resides.

   procedure Get_Setting
     (Screen : access Gdk_Screen_Record;
      Name   : String;
      Value  : out Glib.Values.GValue;
      Found  : out Boolean);
   --  Retrieves a desktop-wide setting such as double-click time for the
   --  Gdk_Screen screen.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --
   --  - "size_changed"
   --    procedure Handler (Screen : access Gdk_Screen_Record'Class);
   --    Emitted when the pixel width or height of a screen changes.
   --  </signals>

   Signal_Size_Changed : constant Glib.Signal_Name := "size_changed";

private
   pragma Import (C, Get_Type, "gdk_screen_get_type");
end Gdk.Screen;

--  Reserved for internal use by gtk+:
--  No binding: gdk_screen_set_font_options_libgtk_only
--  No binding: gdk_screen_set_resolution_libgtk_only
--  No binding: gdk_screen_get_resolution_libgtk_only
--  No binding: gdk_screen_get_font_options_libgtk_only

--  Low-level:
--  No binding: gdk_screen_broadcast_client_message

--  Binding would be nice, requires a list of C_Proxy:
--  No binding: gdk_screen_get_toplevel_windows
--  No binding: gdk_screen_list_visuals
