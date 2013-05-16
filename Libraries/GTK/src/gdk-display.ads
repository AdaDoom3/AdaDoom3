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
--  Gdk_Display objects purpose are two fold:
--     - To grab/ungrab keyboard focus and mouse pointer
--     - To manage and provide information about the Gdk_Screen(s) available
--       for this Gdk_Display
--  Gdk_Display objects are the GDK representation of the X Display which can
--  be described as a workstation consisting of a keyboard a pointing device
--  (such as a mouse) and one or more screens. It is used to open and keep
--  track of various Gdk_Screen objects currently instanciated by the
--  application. It is also used to grab and release the keyboard and the mouse
--  pointer.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Gdk, the low-level API</group>
--  <see>Gdk_Screen</see>

with Gdk.Event;
with Gdk.Types;
with Glib.Object;

package Gdk.Display is

   type Gdk_Display_Record is new Glib.Object.GObject_Record with null record;
   type Gdk_Display is access all Gdk_Display_Record'Class;

   function Get_Type return Glib.GType;
   --  Return the internal type associated with a Gdk_Display

   function Open (Display_Name : String) return Gdk_Display;
   --  Open a new display. Display_Name follows the unix convention, and should
   --  have the format host:screen, for instance "foo.com:0".

   function Get_Default return Gdk_Display;
   --  Gets the default Gdk_Display

   function Get_Name (Display : access Gdk_Display_Record) return String;
   --  Return the name of the screen

   function Get_N_Screens
     (Display : access Gdk_Display_Record) return Glib.Gint;
   --  Return the number of screens managed by the display.
   --  See the function Gdk.Screen.Get_Screen or Gdk.Screen.Get_Default_Screen

   procedure Pointer_Ungrab
     (Display : access Gdk_Display_Record;
      Time    : Glib.Guint32 := Gdk.Types.Current_Time);
   --  Release any pointer grab.

   procedure Keyboard_Ungrab
     (Display : access Gdk_Display_Record;
      Time    : Glib.Guint32 := Gdk.Types.Current_Time);
   --  Release any keyboard grab

   function Pointer_Is_Grabbed
     (Display : access Gdk_Display_Record) return Boolean;
   --  Test if the pointer is grabbed.

   procedure Beep (Display : access Gdk_Display_Record);
   --  Emits a short beep on display

   procedure Sync (Display : access Gdk_Display_Record);
   --  Flushes any requests queued for the windowing system and waits until all
   --  requests have been handled. This is often used for making sure that the
   --  display is synchronized with the current state of the program. Calling
   --  Sync before Gdk.Error.Trap_Pop makes sure that any errors generated from
   --  earlier requests are handled before the error trap is removed.
   --
   --  This is most useful for X11. On windowing systems where requests are
   --  handled synchronously, this function will do nothing.

   procedure Flush (Display : access Gdk_Display_Record);
   --  Flushes any requests queued for the windowing system; this happens
   --  automatically when the main loop blocks waiting for new events, but if
   --  your application is drawing without returning control to the main loop,
   --  you may need to call this function explicitely. A common case where this
   --  function needs to be called is when an application is executing drawing
   --  commands from a thread other than the thread where the main loop is
   --  running.
   --
   --  This is most useful for X11. On windowing systems where requests are
   --  handled synchronously, this function will do nothing.

   procedure Close (Display : access Gdk_Display_Record);
   --  Closes the connection to the windowing system for the given display,
   --  and cleans up associated resources.

   function Get_Event
     (Display : access Gdk_Display_Record) return Gdk.Event.Gdk_Event;
   --  Gets the next Gdk_Event to be processed for Display, fetching events
   --  from the windowing system if necessary.
   --  null is returned if no events are pending.
   --  The returned Gdk_Event must be freed with Gdk.Event.Free

   function Peek_Event
     (Display : access Gdk_Display_Record) return Gdk.Event.Gdk_Event;
   --  Gets a copy of the first Gdk_Event in the Display's event queue, without
   --  removing the event from the queue. (Note that this function will not get
   --  more events from the windowing system. It only checks the events that
   --  have already been moved to the GDK event queue.)
   --  null is returned if there are no events in the queue. The returned event
   --  should be freed with Gdk.Event.Free.

   procedure Put_Event
     (Display : access Gdk_Display_Record;
      Event   : Gdk.Event.Gdk_Event);
   --  Appends a copy of the given event onto the front of the event
   --  queue for Display.

   procedure Set_Double_Click_Time
     (Display : access Gdk_Display_Record;
      Msec    : Glib.Guint);
   --  Sets the double click time (two clicks within this time interval count
   --  as a double click and result in a GDK_2BUTTON_PRESS event). Applications
   --  should not set this, it is a global user-configured setting.

   procedure Set_Double_Click_Distance
     (Display  : access Gdk_Display_Record;
      Distance : Glib.Guint);
   --  Sets the double click distance (two clicks within this distance count as
   --  a double click and result in a GDK_2BUTTON_PRESS event). See also
   --  Set_Double_Click_Time. Applications should not set this, it is a global
   --  user-configured setting.

   procedure Get_Window_At_Pointer
     (Display : access Gdk_Display_Record;
      Win_X   : out Glib.Gint;
      Win_Y   : out Glib.Gint;
      Win     : out Gdk.Gdk_Window);
   --  Obtains the window underneath the mouse pointer, returning the location
   --  of that window in Win_X, Win_Y. Returns nullif the window
   --  under the mouse pointer is not known to GDK (for example, belongs to
   --  another application).
   --  (Win_X, Win_Y) are relative to the origin of the window under the
   --  pointer.

   function Supports_Cursor_Color
     (Display : access Gdk_Display_Record) return Boolean;
   --  Returns TRUE if multicolored cursors are supported on display.
   --  Otherwise, cursors have only a forground and a background color.

   function Supports_Cursor_Alpha
     (Display : access Gdk_Display_Record) return Boolean;
   --  Returns TRUE if cursors can use an 8bit alpha channel on display.
   --  Otherwise, cursors are restricted to bilevel alpha (i.e. a mask).

   function Get_Default_Cursor_Size
     (Display : access Gdk_Display_Record) return Glib.Guint;
   --  Returns the default size to use for cursors on display.

   procedure Get_Maximal_Cursor_Size
     (Display : access Gdk_Display_Record;
      Width   : out Glib.Guint;
      Height  : out Glib.Guint);
   --  Gets the maximal size to use for cursors on display

   function Get_Default_Group
     (Display : access Gdk_Display_Record) return Gdk.Gdk_Window;
   --  Returns the default group leader window for all toplevel windows on
   --  display. This window is implicitly created by GDK. See
   --  Gdk.Window.Set_Group.

   function Supports_Selection_Notification
     (Display : access Gdk_Display_Record) return Boolean;
   --  Returns whether Gdk.Event.Owner_Change events will be sent when the
   --  owner of a selection changes.

   function Request_Selection_Notification
     (Display   : access Gdk_Display_Record;
      Selection : Gdk.Types.Gdk_Atom) return Boolean;
   --  Request Gdk.Event.Owner_Change events for ownership changes of the
   --  selection named by the given atom.

   function Supports_Clipboard_Persistence
     (Display : access Gdk_Display_Record) return Boolean;
   --  Returns whether the specifed display supports clipboard persistance;
   --  i.e. if it's possible to store the clipboard data after an application
   --  has quit. On X11 this checks if a clipboard daemon is running.

   procedure Store_Clipboard
     (Display          : access Gdk_Display_Record;
      Clipboard_Window : Gdk.Gdk_Window;
      Time             : Glib.Guint32;
      Targets          : Gdk.Types.Gdk_Atom_Array);
   --  Issues a request to the clipboard manager to store the clipboard data.
   --  On X11, this is a special program that works according to the
   --  freedesktop clipboard specification, available at
   --  http://www.freedesktop.org/Standards/clipboard-manager-spec.
   --  See also Gtk.Clipboard.Store.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this object:
   --
   --  - "closed"
   --    procedure Handler
   --      (Display  : access Gdk_Display_Record'Class;
   --       Is_Error : Boolean);
   --    The ::closed signal is emitted when the connection to the windowing
   --    system for display is closed. Is_Error is set to true if the
   --    connection is closed due to an error.
   --  </signals>

   Signal_Closed : constant Glib.Signal_Name := "closed";

private
   pragma Import (C, Get_Type, "gdk_display_get_type");
end Gdk.Display;

--  Binding provided in gdk-screen.ads for circularity dependencies reasons:
--  No binding: gdk_display_get_default_screen
--  No binding: gdk_display_get_pointer
--  No binding: gdk_display_get_screen
--  No binding: gdk_display_warp_pointer

--  Binding might be nice later:
--  No binding: gdk_display_set_pointer_hooks

--  No binding needed (too low-level):
--  No binding: gdk_display_get_core_pointer
--  No binding: gdk_display_list_devices
--  No binding: gdk_display_add_client_message_filter

--  Function has no implementation
--  No binding: gdk_display_open_default_libgtk_only
