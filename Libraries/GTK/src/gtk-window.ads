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
--  This widget implements a top level window.
--  It is used as the base class for dialogs, ...
--
--  A window has both a default widget (to which events are sent if no other
--  widget has been selected and has the focus), and a focus widget (which
--  gets the events and overrides the default widget).
--
--  You can set many hints on the window (its minimum and maximum size, its
--  decoration, etc.) but these are only hints to the window manager, which
--  might not respect them.
--
--  A useful hint, respected by most window managers, can be used to force
--  some secondary windows to stay on top of the main window on the screen
--  (for instance, so that a smaller window can not be hidden by a bigger
--  one). See the function Set_Transient_For below.
--
--  A window can also be modal, i.e. grab all the mouse and keyboard events
--  in the application while it is displayed.
--
--  </description>
--  <c_version>2.16.6</c_version>
--  <group>Windows</group>
--  <screenshot>gtk-window</screenshot>

with Glib.Object;
with Glib.Properties;
with Gdk.Event;
with Gdk.Pixbuf;
with Gdk.Types;
with Gdk.Window;
with Gtk.Accel_Group;
with Gtk.Bin;
with Gtk.Enums;
with Gtk.Widget;

package Gtk.Window is

   type Gtk_Window_Record is new Bin.Gtk_Bin_Record with private;
   type Gtk_Window is access all Gtk_Window_Record'Class;

   type Gtk_Window_Group_Record is
     new Glib.Object.GObject_Record with null record;
   type Gtk_Window_Group is access all Gtk_Window_Group_Record'Class;

   procedure Gtk_New
     (Window   : out Gtk_Window;
      The_Type : Gtk.Enums.Gtk_Window_Type := Gtk.Enums.Window_Toplevel);
   --  Create a new window.
   --  The_Type specifies the type of the window, and can be either a
   --  top level window, a dialog or a popup window. You will most often only
   --  need to use Window_Toplevel, the other types are mostly used internally
   --  by gtk+.
   --  A Popup window is used to display a temporary information window. It has
   --  no borders nor resizing handles.

   procedure Initialize
     (Window   : access Gtk_Window_Record'Class;
      The_Type : Gtk.Enums.Gtk_Window_Type);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Gtk_Window.

   procedure Set_Title
     (Window : access Gtk_Window_Record; Title : UTF8_String);
   function Get_Title (Window : access Gtk_Window_Record) return UTF8_String;
   --  Change the title of the window, as it appears in the title bar.
   --  Note that on some systems you might not be able to change it.

   procedure Set_Wmclass
     (Window        : access Gtk_Window_Record;
      Wmclass_Name  : String;
      Wmclass_Class : String);
   --  Don't use this function. It sets the X Window System "class" and
   --  "name" hints for a window. According to the ICCCM, you should
   --  always set these to the same value for all windows in an
   --  application, and GTK sets them to that value by default, so calling
   --  this function is sort of pointless. However, you may want to call
   --  Set_Role on each window in your application, for the
   --  benefit of the session manager. Setting the role allows the window
   --  manager to restore window positions when loading a saved session.

   procedure Set_Role (Window : access Gtk_Window_Record; Role : String);
   function Get_Role  (Window : access Gtk_Window_Record) return String;
   --  In combination with the window title, the window role allows a
   --  window manager to identify "the same" window when an application is
   --  restarted. So for example you might set the "toolbox" role on your
   --  app's toolbox window, so that when the user restarts their session,
   --  the window manager can put the toolbox back in the same place.
   --  If a window already has a unique title, you don't need to set the
   --  role, since the WM can use the title to identify the window when
   --  restoring the session.
   --  Role: unique identifier for the window to be used when restoring a
   --  session

   function Activate_Focus (Window : access Gtk_Window_Record) return Boolean;
   --  Call Gtk.Widget.Activate on the widget that currently has the focus in
   --  the window, ie sends an "activate" signal to that widget. Note that this
   --  signal does not really exists and is mapped to some widget-specific
   --  signal.
   --  Return True if the widget could be activated, False otherwise.
   --  The Focus widget is set through a signal "set_focus".

   function Activate_Default
     (Window : access Gtk_Window_Record) return Boolean;
   --  Activate the default widget in the window.
   --  In other words, send an "activate" signal to that widget. Note that
   --  this signal is a virtual one and is mapped to some widget specific
   --  signal.
   --  Return False is the widget could not be activated or if there was
   --  no default widget.
   --  You can set the default widget with the following calls:
   --
   --     Gtk.Widget.Set_Flags (Widget, Can_Default);
   --     Gtk.Widget.Grab_Default (Widget);

   procedure Set_Transient_For
     (Window : access Gtk_Window_Record;
      Parent : access Gtk_Window_Record'Class);
   function Get_Transient_For
     (Window : access Gtk_Window_Record) return Gtk_Window;
   --  Specify that Window is a transient window.
   --  A transient window is a temporary window, like a popup menu or a
   --  dialog box). Parent is the toplevel window of the application to which
   --  Window belongs. A window that has set this can expect less decoration
   --  from the window manager (for instance no title bar and no borders).
   --  (see XSetTransientForHint(3) on Unix systems)
   --
   --  The main usage of this function is to force Window to be on top of
   --  Parent on the screen at all times. Most window managers respect this
   --  hint, even if this is not mandatory.

   procedure Set_Type_Hint
     (Window : access Gtk_Window_Record;
      Hint   : Gdk.Window.Gdk_Window_Type_Hint);
   function Get_Type_Hint
     (Window : access Gtk_Window_Record)
      return Gdk.Window.Gdk_Window_Type_Hint;
   --  allow the window manager to decorate and handle the window in a way
   --  which is suitable to the function of the window in your application.
   --  This function should be called before the window becomes visible.

   procedure Set_Keep_Above
     (Window  : access Gtk_Window_Record; Setting : Boolean);
   procedure Set_Keep_Below
     (Window  : access Gtk_Window_Record; Setting : Boolean);
   --  Asks to keep Window above, so that it stays on top. Note that you
   --  shouldn't assume the window is definitely above afterward, because other
   --  entities (e.g. the user or window managers) could not keep it above, and
   --  not all window managers support keeping windows above. But normally the
   --  window will end kept above. Just don't write code that crashes if not.
   --
   --  It's permitted to call this function before showing a window, in which
   --  case the window will be kept above when it appears onscreen initially.
   --
   --  You can track the above state via the "window_state_event" signal on
   --  Window.
   --
   --  Note that, according to the "Extended Window Manager Hints"
   --  specification, the above state is mainly meant for user preferences and
   --  should not be used by applications e.g. for drawing attention to their
   --  dialogs.

   procedure Set_Auto_Startup_Notification (Setting : Boolean);
   --  By default, after showing the first Window for each screen, GTK+ calls
   --  gdk_notify_startup_complete(). Call this function to disable the
   --  automatic startup notification. You might do this if your first window
   --  is a splash screen, and you want to delay notification until after your
   --  real main window has been shown, for example.
   --
   --  In that example, you would disable startup notification temporarily,
   --  show your splash screen, then re-enable it so that showing the main
   --  window would automatically result in notification.
   --
   --  Notification is used by the desktop environment to show the user that
   --  your application is still loading.

   procedure Set_Startup_Id
     (Window     : access Gtk_Window_Record;
      Startup_Id : String);
   --  Startup notification identifiers are used by desktop environment to
   --  track application startup, to provide user feedback and other
   --  features. This function changes the corresponding property on the
   --  underlying Gdk_Window. Normally, startup identifier is managed
   --  automatically and you should only use this function in special cases
   --  like transferring focus from other processes. You should use this
   --  function before calling Present or any equivalent function generating
   --  a window map event.
   --
   --  This function is only useful on X11, not with other GTK+ targets.

   function Get_Deletable (Window : access Gtk_Window_Record) return Boolean;
   procedure Set_Deletable
     (Window  : access Gtk_Window_Record;
      Setting : Boolean);
   --  By default, windows have a close button in the window frame. Some
   --  window managers allow GTK+ to disable this button. If you set the
   --  deletable property to False using this function, GTK+ will do its best
   --  to convince the window manager not to show a close button. Depending on
   --  the system, this function may not have any effect when called on a
   --  window that is already visible, so you should call it before calling
   --  Gtk.Window.Show.
   --
   --  On Windows, this function always works, since there's no window manager
   --  policy involved.

   procedure Set_Destroy_With_Parent
     (Window  : access Gtk_Window_Record;
      Setting : Boolean := True);
   function Get_Destroy_With_Parent
     (Window : access Gtk_Window_Record) return Boolean;
   --  Set whether destroying the transient parent of Window will also destroy
   --  Window itself.
   --  This is useful for dialogs that shouldn't persist beyond the lifetime
   --  of the main window they're associated with, for example.

   procedure Set_Geometry_Hints
     (Window          : access Gtk_Window_Record;
      Geometry_Widget : Gtk.Widget.Gtk_Widget;
      Geometry        : Gdk.Window.Gdk_Geometry;
      Geom_Mask       : Gdk.Window.Gdk_Window_Hints);
   --  Specify some geometry hints for the window.
   --  This includes its minimal and maximal sizes, ...
   --  These attributes are specified in Geometry.
   --  Geom_Mask indicates which of the fields in Geometry are set.
   --  Geometry_Widget can be null (and thus is not an access parameter). It
   --  adds some extra size to Geometry based on the actual size of
   --  Geometry_Widget (the extra amount is Window'Size - Geometry_Widget'Size)
   --
   --  Geometry.Base_* indicates the size that is used by the window manager
   --  to report the size: for instance, if Base_Width = 600 and actual width
   --  is 200, the window manager will indicate a width of -400.
   --
   --  If your window manager respects the hints (and its doesn't have to),
   --  then the user will never be able to resize the window to a size not
   --  in Geometry.Min_* .. Geometry.Max_*.
   --
   --  Geometry.*_Inc specifies by which amount the size will be multiplied.
   --  For instance, if Width_Inc = 50 and the size reported by the Window
   --  Manager is 2x3, then the actual width of the window is 100.
   --  Your window's size will always be a multiple of the *_Inc values.
   --
   --  Geometry.*_Aspect specifies the aspect ratio for the window. The window
   --  will always be resized so that the ratio between its width and its
   --  height remains in the range Min_Aspect .. Max_Aspect.

   procedure Set_Decorated
     (Window  : access Gtk_Window_Record;
      Setting : Boolean := True);
   function Get_Decorated (Window : access Gtk_Window_Record) return Boolean;
   --  By default, windows are decorated with a title bar, resize
   --  controls, etc. Some window managers allow GtkAda to disable these
   --  decorations, creating a borderless window. If you set the decorated
   --  property to False using this function, GtkAda will do its best to
   --  convince the window manager not to decorate the window.

   procedure Set_Modal
     (Window : access Gtk_Window_Record;  Modal  : Boolean := True);
   function Get_Modal (Window : access Gtk_Window_Record) return Boolean;
   --  Define the window as being Modal.
   --  It will grab the input from the keyboard and the mouse while it is
   --  displayed and will release it when it is hidden. The grab is only in
   --  effect for the windows that belong to the same application, and will not
   --  affect other applications running on the same screen.
   --  In cunjunction with Gtk.Main.Main, this is the easiest way to show a
   --  dialog to which the user has to answer before the application can
   --  continue.

   procedure Set_Skip_Pager_Hint
     (Window  : access Gtk_Window_Record;
      Setting : Boolean);
   function Get_Skip_Taskbar_Hint
     (Window : access Gtk_Window_Record)
      return Boolean;
   --  Windows may set a hint asking the desktop environment not to display
   --  the window in the pager. This function sets this hint.
   --  (A "pager" is any desktop navigation tool such as a workspace
   --  switcher that displays a thumbnail representation of the windows
   --  on the screen).

   procedure Set_Skip_Taskbar_Hint
     (Window  : access Gtk_Window_Record;
      Setting : Boolean);
   function Get_Skip_Pager_Hint
     (Window : access Gtk_Window_Record)
      return Boolean;
   --  Windows may set a hint asking the desktop environment not to display
   --  the window in the task bar. This function sets this hint.

   procedure Set_Urgency_Hint
     (Window  : access Gtk_Window_Record;
      Setting : Boolean);
   function Get_Urgency_Hint
     (Window : access Gtk_Window_Record)
      return Boolean;
   --  Windows may set a hint asking the desktop environment to draw
   --  the users attention to the window. This function sets this hint.

   function List_Toplevels return Gtk.Widget.Widget_List.Glist;
   --  Return a list of all existing toplevel windows.
   --  The widgets in the list are not individually referenced. If you want
   --  to iterate through the list and perform actions involving
   --  callbacks that might destroy the widgets, you must "ref"erence
   --  all the widgets in the list first and then unref all the widgets
   --  afterwards.
   --  The list itself must be freed by the caller

   procedure Present (Window : access Gtk_Window_Record);
   --  Present a window to the user.
   --  This may mean raising the window in the stacking order, deiconifying it,
   --  moving it to the current desktop, and/or giving it the keyboard focus,
   --  possibly dependent on the user's platform, window manager, and
   --  preferences.
   --
   --  If Window is hidden, this function calls Gtk.Widget.Show as well.
   --
   --  If you are calling this function in response to a user interaction, it
   --  is preferable to use Present_With_Time.

   procedure Present_With_Time
     (Window    : access Gtk_Window_Record;
      Timestamp : Guint32);
   --  Present a window to the user in response to a user interaction.
   --  Timestamp is the timestamp of the user interaction (typically a button
   --  or key press event) which triggered this call.
   --
   --  This function should be used when the user tries to open a window
   --  that's already open. Say for example the preferences dialog is
   --  currently open, and the user chooses Preferences from the menu
   --  a second time; use Present to move the already-open dialog
   --  where the user can see it.

   procedure Stick (Window : access Gtk_Window_Record);
   --  Ask to stick Window, which means that it will appear on all user
   --  desktops. Note that you shouldn't assume the window is definitely
   --  stuck afterward, because other entities (e.g. the user or window
   --  manager) could unstick it again, and some window managers do not
   --  support sticking windows. But normally the window will end up
   --  stuck.
   --
   --  It's permitted to call this function before showing a window.
   --
   --  You can track stickiness via the "window_state_event" signal
   --  on Gtk_Widget.

   procedure Unstick (Window : access Gtk_Window_Record);
   --  Ask to unstick Window, which means that it will appear on only
   --  one of the user's desktops. Note that you shouldn't assume the
   --  window is definitely unstuck afterward, because other entities
   --  (e.g. the user or window manager) could stick it again. But
   --  normally the window will end up stuck.
   --
   --  You can track stickiness via the "window_state_event" signal
   --  on Gtk_Widget.

   function Get_Opacity (Window : access Gtk_Window_Record) return Gdouble;
   procedure Set_Opacity
     (Window  : access Gtk_Window_Record;
      Opacity : Gdouble);
   --  Request the windowing system to make Window partially transparent,
   --  with opacity 0.0 being fully transparent and 1.0 fully opaque. (Values
   --  of the opacity parameter are clamped to the [0.0,1.0] range.) On X11
   --  this has any effect only on X screens with a compositing manager
   --  running. See Gtk.Widget.Is_Composited. On Windows it should always work.
   --
   --  Note that on Windows, setting a window's opacity after the window has
   --  been shown causes it to flicker once.

   --------------
   -- Position --
   --------------

   procedure Move (Window : access Gtk_Window_Record; X, Y : Gint);
   --  Asks the window manager to move Window to the given position. Window
   --  managers are free to ignore this; most window managers ignore requests
   --  for initial window positions (instead using a user-defined placement
   --  algorithm) and honor requests after the window has already been shown.
   --
   --  Note: the position is the position of the gravity-determined reference
   --  point for the window. The gravity determines two things: first, the
   --  location of the reference point in root window coordinates; and second,
   --  which point on the window is positioned at the reference point.
   --
   --  By default the gravity is GRAVITY_NORTH_WEST, so the reference point is
   --  simply the (x, y) supplied to Move. The top-left corner of the window
   --  decorations (aka window frame or border) will be placed at (x, y).
   --  Therefore, to position a window at the top left of the screen, you want
   --  to use the default gravity (which is GRAVITY_NORTH_WEST) and move the
   --  window to 0,0.
   --
   --  To position a window at the bottom right corner of the screen, you would
   --  set GRAVITY_SOUTH_EAST, which means that the reference point is at x +
   --  the window width and y + the window height, and the bottom-right corner
   --  of the window border will be placed at that reference point. So, to
   --  place a window in the bottom right corner you would first set gravity to
   --  south east, then write:
   --    Move (Window, Gdk_Screen_Width  - Window_Width,
   --                  Gdk_Screen_Height - Window_Height);

   procedure Set_Position
     (Window   : access Gtk_Window_Record;
      Position : Gtk.Enums.Gtk_Window_Position);
   --  Specify how the position of the window should be computed.
   --  If Position is Win_Pos_Center_Always or Win_Pos_Center, then the window
   --  is centered on the screen. In the first case, it is also recentered
   --  when the window is resized with Gtk.Widget.Set_Usize (ie except on
   --  user action).
   --  If Position is Win_Pos_Mouse, then the window is positioned so that it
   --  centered around the mouse.
   --  If Position is Win_Pos_None, no calculation is done. If
   --  Gtk.Widget.Set_Uposition has been called, it is respected. This is the
   --  default case.

   procedure Get_Position
     (Window         : access Gtk_Window_Record;
      Root_X, Root_Y : out Gint);
   --  This function returns the position you need to pass to gtk.window.move
   --  to keep Window in its current position. This means that the meaning of
   --  the returned value varies with window gravity. See Gtk.Window.Move for
   --  more details.
   --
   --  If you haven't changed the window gravity, its gravity will be
   --  GRAVITY_NORTH_WEST. This means that Get_Position gets the position of
   --  the top-left corner of the window manager frame for the window.
   --  gtk.window.move sets the position of this same top-left corner.
   --
   --  Get_Position is not 100% reliable because the X Window System does not
   --  specify a way to obtain the geometry of the decorations placed on a
   --  window by the window manager. Thus GTK+ is using a "best guess" that
   --  works with most window managers.
   --
   --  Moreover, nearly all window managers are historically broken with
   --  respect to their handling of window gravity. So moving a window to its
   --  current position as returned by Get_Position tends to result in moving
   --  the window slightly. Window managers are slowly getting better over
   --  time.
   --
   --  If a window has gravity GRAVITY_STATIC the window manager frame is not
   --  relevant, and thus Get_Position will always produce accurate results.
   --  However you can't use static gravity to do things like place a window in
   --  a corner of the screen, because static gravity ignores the window
   --  manager decorations.
   --
   --  If you are saving and restoring your application's window positions, you
   --  should know that it's impossible for applications to do this without
   --  getting it somewhat wrong because applications do not have sufficient
   --  knowledge of window manager state. The Correct Mechanism is to support
   --  the session management protocol (see the "GnomeClient" object in the
   --  GNOME libraries for example) and allow the window manager to save your
   --  window sizes and positions.

   procedure Begin_Move_Drag
     (Window    : access Gtk_Window_Record;
      Button    : Gint;
      Root_X    : Gint;
      Root_Y    : Gint;
      Timestamp : Guint32);
   --  Starts moving a window. This function is used if an application has
   --  window movement grips. When GDK can support it, the window movement will
   --  be done using the standard mechanism for the window manager or windowing
   --  system. Otherwise, GDK will try to emulate window movement, potentially
   --  not all that well, depending on the windowing system.
   --  (Root_X, Root_Y): Position where the user clicked to initiate the drag,
   --  in root window coordinates. Timestamp is the timestamp of the event that
   --  initiated the drag

   function Parse_Geometry
     (Window   : access Gtk_Window_Record;
      Geometry : String)
      return Boolean;
   --  Parses a standard X Window System geometry string - see the manual page
   --  for X (type 'man X') for details on this. Parse_Geometry does work on
   --  all GTK+ ports including Win32 but is primarily intended for an X
   --  environment.
   --
   --  If either a size or a position can be extracted from the geometry
   --  string, Parse_Geometry returns True and calls Set_Default_Size and/or
   --  Move to resize/move the window.
   --
   --  If Parse_Geometry returns True, it will also set the HINT_USER_POS
   --  and/or HINT_USER_SIZE hints indicating to the window manager that the
   --  size/position of the window was user-specified. This causes most window
   --  managers to honor the geometry.
   --
   --  Note that for Parse_Geometry to work as expected, it has to be called
   --  when the window has its "final" size, i.e. after calling Show_All on the
   --  contents and Set_Geometry_Hints on the window.

   -----------
   -- Sizes --
   -----------

   procedure Set_Resizable
     (Window    : access Gtk_Window_Record;
      Resizable : Boolean := True);
   function Get_Resizable (Window : access Gtk_Window_Record) return Boolean;
   --  Sets or gets whether the user can resize a window.
   --  Windows are user resizable by default.

   procedure Set_Gravity
     (Window  : access Gtk_Window_Record;
      Gravity : Gdk.Window.Gdk_Gravity);
   function Get_Gravity
     (Window : access Gtk_Window_Record) return Gdk.Window.Gdk_Gravity;
   --  Window gravity defines the "reference point" to be used when
   --  positioning or resizing a window. Calls to
   --  Gtk.Widget.Set_UPosition will position a different point on the
   --  window depending on the window gravity. When the window changes size
   --  the reference point determined by the window's gravity will stay in
   --  a fixed location.
   --
   --  See Gdk_Gravity for full details. To briefly summarize,
   --  Gravity_North_West means that the reference point is the
   --  northwest (top left) corner of the window
   --  frame. Gravity_South_East would be the bottom right corner of
   --  the frame, and so on. If you want to position the window contents,
   --  rather than the window manager's frame, Gravity_Static moves
   --  the reference point to the northwest corner of the Gtk_Window
   --  itself.
   --
   --  The default window gravity is Gravity_North_West.

   procedure Set_Has_Frame (Window : access Gtk_Window_Record);
   function Get_Has_Frame  (Window : access Gtk_Window_Record) return Boolean;
   --  If this function is called on a window before it is realized
   --  or showed it will have a "frame" window around widget-window.
   --  Using the signal frame_event you can receive all events targeted at the
   --  frame.
   --
   --  This function is used by the linux-fb port to implement managed
   --  windows, but it could concievably be used by X-programs that
   --  want to do their own window decorations.

   procedure Set_Frame_Dimensions
     (Window                   : access Gtk_Window_Record;
      Left, Top, Right, Bottom : Gint);
   procedure Get_Frame_Dimensions
     (Window                   : access Gtk_Window_Record;
      Left, Top, Right, Bottom : out Gint);
   --  Change the size of the frame border.
   --  This has only an effect for windows with frames (see Set_Has_Frame).

   procedure Fullscreen   (Window : access Gtk_Window_Record);
   procedure Unfullscreen (Window : access Gtk_Window_Record);
   --  Ask to place Window in fullscreen state.
   --  You shouldn't assume the window is definitely full screen afterward,
   --  because other entities (user or window manager) could unfullscreen it
   --  again and not all window managers honor requests to fullscreen windows.
   --  You can track the fullscreen state via the "window_state_event" signal.

   procedure Iconify (Window : access Gtk_Window_Record);
   --  Ask to iconify Window.
   --  Note that you shouldn't assume the window is definitely iconified
   --  afterward, because other entities (e.g. the user or window manager)
   --  could deiconify it again, or there may not be a window manager in which
   --  case iconification isn't possible, etc. But normally the window will end
   --  up iconified. Just don't write code that crashes if not.
   --
   --  It's permitted to call this function before showing a window,
   --  in which case the window will be iconified before it ever appears
   --  onscreen.
   --
   --  You can track iconification via the "window_state_event" signal
   --  on Gtk_Widget.

   procedure Deiconify (Window : access Gtk_Window_Record);
   --  Ask to deiconify Window.
   --  Note that you shouldn't assume the window is definitely deiconified
   --  afterward, because other entities (e.g. the user or window manager)
   --  could iconify it again before your code which assumes deiconification
   --  gets to run.
   --
   --  You can track iconification via the "window_state_event" signal
   --  on Gtk_Widget.

   procedure Maximize (Window : access Gtk_Window_Record);
   --  Ask to maximize Window, so that it becomes full-screen.
   --  Note that you shouldn't assume the window is definitely maximized
   --  afterward, because other entities (e.g. the user or window manager)
   --  could unmaximize it again, and not all window managers support
   --  maximization. But normally the window will end up maximized.
   --
   --  It's permitted to call this function before showing a window,
   --  in which case the window will be maximized when it appears onscreen
   --  initially.
   --
   --  You can track maximization via the "window_state_event" signal
   --  on Gtk_Widget.

   procedure Unmaximize (Window : access Gtk_Window_Record);
   --  Ask to unmaximize Window.
   --  Note that you shouldn't assume the window is definitely unmaximized
   --  afterward, because other entities (e.g. the user or window manager)
   --  could maximize it again, and not all window managers honor requests to
   --  unmaximize. But normally the window will end up unmaximized.
   --
   --  You can track maximization via the "window_state_event" signal
   --  on Gtk_Widget.

   procedure Set_Default_Size
     (Window : access Gtk_Window_Record; Width : Gint; Height : Gint);
   procedure Get_Default_Size
     (Window : access Gtk_Window_Record;
      Width  : out Gint;
      Height : out Gint);
   --  Sets the default size of a window. If the window's "natural" size (its
   --  size request) is larger than the default, the default will be
   --  ignored. More generally, if the default size does not obey the geometry
   --  hints for the window (Set_Geometry_Hints can be used to set these
   --  explicitly), the default size will be clamped to the nearest permitted
   --  size.
   --
   --  Unlike Gtk.Widget.Set_Size_Request, which sets a size request for a
   --  widget and thus would keep users from shrinking the window, this
   --  function only sets the initial size, just as if the user had resized the
   --  window themselves. Users can still shrink the window again as they
   --  normally would. Setting a default size of -1 means to use the "natural"
   --  default size (the size request of the window).
   --
   --  For more control over a window's initial size and how resizing works,
   --  investigate Set_Geometry_Hints.
   --
   --  For some uses, Resize is a more appropriate function.  Resize changes
   --  the current size of the window, rather than the size to be used on
   --  initial display. Resize always affects the window itself, not the
   --  geometry widget.
   --
   --  The default size of a window only affects the first time a window is
   --  shown; if a window is hidden and re-shown, it will remember the size it
   --  had prior to hiding, rather than using the default size.
   --
   --  Windows can't actually be 0x0 in size, they must be at least 1x1, but
   --  passing 0 for Width and Height is OK, resulting in a 1x1 default size.
   --
   --  This has no effect on Popup windows (set in call to Gtk_New).

   procedure Resize
     (Window : access Gtk_Window_Record;
      Width, Height : Gint);
   --  Resize the window as if the user had done so, obeying geometry
   --  constraints. The default geometry constraint is that windows may
   --  not be smaller than their size request; to override this
   --  constraint, call Gtk.Widget.Set_Size_Request to set the window's
   --  request to a smaller value.
   --
   --  If Resize is called before showing a window for the -- first time, it
   --  overrides any default size set with -- Set_Default_Size.
   --
   --  Windows may not be resized smaller than 1 by 1 pixels. However, as a
   --  special case, if both Width and Height are set to -1, the best requested
   --  size is recomputed for the window, and used.

   procedure Get_Size
     (Window        : access Gtk_Window_Record;
      Width, Height : out Gint);
   --  Obtains the current size of Window. If Window is not onscreen, it
   --  returns the size GTK+ will suggest to the window manager for the initial
   --  window size (but this is not reliably the same as the size the window
   --  manager will actually select). The size obtained by Get_Size is the last
   --  size received in Gdk_Event_Configure, that is, GTK+ uses its
   --  locally-stored size, rather than querying the X server for the size. As
   --  a result, if you call Resize then immediately call Get_Size, the size
   --  won't have taken effect yet. After the window manager processes the
   --  resize request, GTK+ receives notification that the size has changed via
   --  a configure event, and the size of the window gets updated.
   --
   --  Note 1: Nearly any use of this function creates a race condition,
   --  because the size of the window may change between the time that you get
   --  the size and the time that you perform some action assuming that size is
   --  the current size. To avoid race conditions, connect to "configure_event"
   --  on the window and adjust your size-dependent state to match the size
   --  delivered in the Gdk_Event_Configure.
   --
   --  Note 2: The returned size does *not* include the size of the window
   --  manager decorations (aka the window frame or border). Those are not
   --  drawn by GTK+ and GTK+ has no reliable method of determining their size.
   --
   --  Note 3: If you are getting a window size in order to position the window
   --  onscreen, there may be a better way. The preferred way is to simply set
   --  the window's semantic type with Set_Type_Hint, which allows the window
   --  manager to e.g. center dialogs. Also, if you set the transient parent of
   --  dialogs with Set_Transient_For window managers will often center the
   --  dialog over its parent window. It's much preferred to let the window
   --  manager handle these things rather than doing it yourself, because all
   --  apps will behave consistently and according to user prefs if the window
   --  manager handles it. Also, the window manager can take the size of the
   --  window decorations/border into account, while your application cannot.
   --
   --  In any case, if you insist on application-specified window positioning,
   --  there's *still*> a better way than doing it yourself - Set_Position will
   --  frequently handle the details for you.

   procedure Reshow_With_Initial_Size (Window : access Gtk_Window_Record);
   --  Hide Window, then reshows it, resetting the default size and position.
   --  Used by GUI builders only.

   procedure Begin_Resize_Drag
     (Window    : access Gtk_Window_Record;
      Edge      : Gdk.Window.Gdk_Window_Edge;
      Button    : Gint;
      Root_X    : Gint;
      Root_Y    : Gint;
      Timestamp : Guint32);
   --  Starts resizing a window. This function is used if an application has
   --  window resizing controls. When GDK can support it, the resize will be
   --  done using the standard mechanism for the window manager or windowing
   --  system. Otherwise, GDK will try to emulate window resizing, potentially
   --  not all that well, depending on the windowing system.

   -----------
   -- Icons --
   -----------

   procedure Set_Icon_Name (Window : access Gtk_Window_Record; Name : String);
   function Get_Icon_Name  (Window : access Gtk_Window_Record) return String;
   --  Set the icon for the window from a named themed icon. See
   --  Gtk.Icon_Them for more details. This has nothing to do with the
   --  WM_ICON_NAME property which is mentioned in the ICCCM (and related to
   --  window managers)

   procedure Set_Icon
     (Window : access Gtk_Window_Record; Icon : Gdk.Pixbuf.Gdk_Pixbuf);
   function Get_Icon
     (Window : access Gtk_Window_Record) return Gdk.Pixbuf.Gdk_Pixbuf;
   --  Sets up the icon representing Window. This icon is used when the window
   --  is minimized (also known as iconified). Some window managers or desktop
   --  environments may also place it in the window frame, or display it in
   --  other contexts.
   --
   --  The icon should be provided in whatever size it was naturally drawn;
   --  that is, don't scale the image before passing it to GTK+. Scaling is
   --  postponed until the last minute, when the desired final size is known,
   --  to allow best quality.
   --
   --  If you have your icon hand-drawn in multiple sizes, use
   --  Set_Icon_List. Then the best size will be used.
   --
   --  This function is equivalent to calling Set_Icon_List with a single
   --  element.
   --
   --  See also Set_Default_Icon_List to set the icon for all windows in your
   --  application in one go.

   procedure Set_Icon_List
     (Window : access Gtk_Window_Record;
      List   : Glib.Object.Object_Simple_List.Glist);
   function Get_Icon_List
     (Window : access Gtk_Window_Record)
      return Glib.Object.Object_Simple_List.Glist;
   --  Sets up the icon representing Window. The icon is used when the window
   --  is minimized (also known as iconified). Some window managers or desktop
   --  environments may also place it in the window frame, or display it in
   --  other contexts.
   --
   --  Set_Icon_List allows you to pass in the same icon in several hand-drawn
   --  sizes. The list should contain the natural sizes your icon is available
   --  in; that is, don't scale the image before passing it to GTK+. Scaling is
   --  postponed until the last minute, when the desired final size is known,
   --  to allow best quality.
   --
   --  By passing several sizes, you may improve the final image quality of the
   --  icon, by reducing or eliminating automatic image scaling.
   --
   --  Recommended sizes to provide: 16x16, 32x32, 48x48 at minimum, and larger
   --  images (64x64, 128x128) if you have them.
   --
   --  Note that transient windows (those who have been set transient for
   --  another window using Set_Transient_For) will inherit their icon from
   --  their transient parent. So there's no need to explicitly set the icon on
   --  transient windows.

   function Set_Icon_From_File
     (Window   : access Gtk_Window_Record;
      Filename : String) return Boolean;
   --  Equivalent to calling Set_Icon with a pixbuf loaded from Filename.
   --  return False on failure.

   procedure Set_Default_Icon_List
     (List : Glib.Object.Object_Simple_List.Glist);
   function Get_Default_Icon_List
     return Glib.Object.Object_Simple_List.Glist;
   --  Sets an icon list to be used as fallback for windows that haven't had
   --  Set_Icon_List called on them to setup a window-specific icon list.

   procedure Set_Default_Icon (Icon : Gdk.Pixbuf.Gdk_Pixbuf);
   --  Sets an icon to be used as a fallback for windows that haven't had
   --  Set_Icon called on them

   function Set_Default_Icon_From_File (Filename : String) return Boolean;
   --  Same as Set_Default_Icon, loads the pixbuf automatically.

   function Get_Default_Icon_Name return String;
   procedure Set_Default_Icon_Name (Name : String);
   --  Gets/Sets icon to be used as a fallback for windows that haven't had a
   --  themed icon set (set Set_Icon_Name).

   ------------
   -- Groups --
   ------------

   procedure Gtk_New (Group : out Gtk_Window_Group);
   --  Create a new window group.
   --  Grabs added with Gtk.Main.Grab_Add only affect windows within the same
   --  group.

   function Group_Get_Type return GType;
   --  Return the internal type used for window groups

   procedure Group_Add_Window
     (Window_Group : access Gtk_Window_Group_Record;
      Window       : access Gtk_Window_Record'Class);
   --  Add a window to Window_Group

   procedure Group_Remove_Window
     (Window_Group : access Gtk_Window_Group_Record;
      Window       : access Gtk_Window_Record'Class);
   --  Remove a specific window from the group

   function Group_List_Windows
     (Window_Group : access Gtk_Window_Group_Record)
      return Gtk.Widget.Widget_List.Glist;
   --  Returns a list of the Gtk_Windows that belong to Window_Group.

   function Get_Group
     (Window : access Gtk_Window_Record) return Gtk_Window_Group;
   --  Returns the group for Window or the default group, if
   --  Window is null or if Window does not have an explicit
   --  window group.

   -----------
   -- Focus --
   -----------

   function Get_Focus (Window : access Gtk_Window_Record)
      return Gtk.Widget.Gtk_Widget;
   --  Return the widget that would have the keyboard focus if
   --  Window itself has the focus. It currently has the focus
   --  only if Has_Focus_Is_Set returns True.
   --  To know whether the Window itself currently has the focus, check the
   --    Has_Toplevel_Focus_Property
   --  property described below

   procedure Set_Focus
     (Window : access Gtk_Window_Record;
      Focus  : Gtk.Widget.Gtk_Widget);
   --  Set the focus child for Window.
   --  If Focus is not the current focus widget, and is focusable, sets
   --  it as the focus widget for the window. If Focus is null, unsets
   --  the focus widget for this window. To set the focus to a particular
   --  widget in the toplevel, it is usually more convenient to use
   --  gtk_widget_grab_focus() instead of this function.

   procedure Set_Accept_Focus
     (Window  : access Gtk_Window_Record;  Setting : Boolean);
   function Get_Accept_Focus
     (Window : access Gtk_Window_Record) return Boolean;
   --  Windows may set a hint asking the desktop environment not to receive
   --  the input focus.

   procedure Set_Focus_On_Map
     (Window  : access Gtk_Window_Record; Setting : Boolean);
   function Get_Focus_On_Map
     (Window : access Gtk_Window_Record) return Boolean;
   --  Windows may set a hint asking the desktop environment not to receive
   --  the input focus when the window is mapped.

   function Has_Toplevel_Focus
     (Window : access Gtk_Window_Record) return Boolean;
   --  Returns whether the input focus is within this Window. For real toplevel
   --  windows, this is identical to Is_Active, but for embedded windows the
   --  results will differ

   function Is_Active (Window : access Gtk_Window_Record) return Boolean;
   --  Returns whether the window is part of the current active toplevel. (That
   --  is, the toplevel window receiving keystrokes.) The return value is True
   --  if the window is active toplevel itself, but also if it is, say, a
   --  Gtk_Plug embedded in the active toplevel. You might use this function if
   --  you wanted to draw a widget differently in an active window from a
   --  widget in an inactive window.

   ------------------------
   -- Keys and shortcuts --
   ------------------------

   function Get_Default_Widget
     (Window : access Gtk_Window_Record) return Gtk.Widget.Gtk_Widget;
   procedure Set_Default
     (Window         : access Gtk_Window_Record;
      Default_Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  The default widget is the widget that's activated when the user presses
   --  Enter in a dialog (for example). This function sets or unsets the
   --  default widget for a Window. When setting (rather than unsetting) the
   --  default widget it's generally easier to call Grab_Focus on the widget.
   --  Before making a widget the default widget, you must set the CAN_DEFAULT
   --  flag on the widget you'd like to make the default using
   --  Gtk.Widget.Set_Flags. A null value indicates no default widget.

   procedure Set_Mnemonic_Modifier
     (Window   : access Gtk_Window_Record;
      Modifier : Gdk.Types.Gdk_Modifier_Type);
   function Get_Mnemonic_Modifier
     (Window : access Gtk_Window_Record)
      return Gdk.Types.Gdk_Modifier_Type;
   --  Sets the mnemonic modifier for this window.
   --  Modifier is the mask used to active mnemonics in this window

   procedure Add_Mnemonic
     (Window : access Gtk_Window_Record;
      Keyval : Gdk.Types.Gdk_Key_Type;
      Target : access Gtk.Widget.Gtk_Widget_Record'Class);
   procedure Remove_Mnemonic
     (Window : access Gtk_Window_Record;
      Keyval : Gdk.Types.Gdk_Key_Type;
      Target : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Add a mnemonic to this window.
   --  Target will receive the "activate" signal when Keyval is pressed inside
   --  the window. In addition to keyval, the user must press the special key
   --  defined through Set_Mnemonic_Modifier

   function Mnemonic_Activate
     (Window   : access Gtk_Window_Record;
      Keyval   : Gdk.Types.Gdk_Key_Type;
      Modifier : Gdk.Types.Gdk_Modifier_Type)
     return Boolean;
   --  Activates the targets associated with the mnemonic. This sends the
   --  "activate" signal to the corresponding signal

   function Activate_Key
     (Window : access Gtk_Window_Record;
      Event  : Gdk.Event.Gdk_Event_Key) return Boolean;
   --  Activates mnemonics and accelerators for this window. This is normally
   --  called by the default key_press_event_handler for toplevel windows,
   --  however in some cases it may be useful to call this directly when
   --  overriding the standard key handling for a toplevel window.
   --  Return True if the mnemonic was found and activated.

   function Propagate_Key_Event
     (Window : access Gtk_Window_Record;
      Event  : Gdk.Event.Gdk_Event_Key) return Boolean;
   --  Propagate a key press or release event to the focus widget and up the
   --  focus container chain until a widget handles Event.
   --  This is normally called by the default key_press_event handler, but
   --  might be useful when overriding the standard key handling for a
   --  toplevel window.

   procedure Add_Accel_Group
     (Window      : access Gtk_Window_Record;
      Accel_Group : Gtk.Accel_Group.Gtk_Accel_Group);
   procedure Remove_Accel_Group
     (Window      : access Gtk_Window_Record;
      Accel_Group : Gtk.Accel_Group.Gtk_Accel_Group);
   --  Adds or Removes the specified accelerator group for the window, such
   --  that calling Gtk.Accel_Groups.Active on Window will activate
   --  accelerators in Accel_Group.

   -----------------
   -- Obsolescent --
   -----------------
   --  All subprograms below are now obsolescent in gtk+. They might be removed
   --  from future versions of gtk+ (and therefore GtkAda).
   --  To find out whether your code uses any of these, we recommend compiling
   --  with the -gnatwj switch
   --  <doc_ignore>

   procedure Set_Resizeable
     (Window    : access Gtk_Window_Record;
      Resizable : Boolean := True) renames Set_Resizable;
   --  pragma Obsolescent ("Use Gtk.Window.Set_Resizable instead");

   function Get_Resizeable (Window : access Gtk_Window_Record) return Boolean
     renames Get_Resizable;
   --  pragma Obsolescent ("Use Gtk.Window.Get_Resizable instead");

   procedure Set_Policy
     (Window       : access Gtk_Window_Record;
      Allow_Shrink : Boolean;
      Allow_Grow   : Boolean;
      Auto_Shrink  : Boolean);
   pragma Obsolescent;  --  Set_Policy
   --  Specify the behavior of the window with regards to size modifications.
   --  Default values when the window is created are:
   --    Allow_Shrink => False,
   --    Allow_Grow   => True,
   --    Auto_Shrink  => False.
   --
   --  If Allow_Shrink is False, then the minimum size of the window is
   --  calculated once depending on its children, and the window can never be
   --  smaller.
   --  If Allow_Grow is False, then the maximum size of the window is
   --  calculated once depending on its children, and the window can never be
   --  bigger.
   --  If Auto_Shrink if False, then the window is not shrinked when its
   --  content changes.

   --  </doc_ignore>

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  - Name:  Type_Property
   --    Type:  Gtk_Window_Type
   --    Flags: read-write (construct only)
   --    Descr: The type of the window
   --    See also:  Gtk_New
   --
   --  - Name:  Title_Property
   --    Type:  UTF8_String
   --    Flags: read-write
   --    Descr: The title of the window
   --    See also:  Set_Title and Get_Title
   --
   --  - Name:  Role_Property
   --    Type:  String
   --    See:   Set_Role / Get_Role
   --
   --  - Name:  Allow_Shrink_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: If TRUE, the window has no mimimum size. Don't use this
   --           feature, it makes no sense
   --    See also:  Set_Policy
   --
   --  - Name:  Allow_Grow_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: If TRUE, users can expand the window beyond its minimum size.
   --    See also:  Set_Policy
   --
   --  - Name: Resizable_Property
   --    Type: Boolean
   --    See:  Set_Resizable / Get_Resizable
   --
   --  - Name:  Modal_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: If TRUE, the window is modal (other windows are not usable
   --           while this one is up)
   --    See also:  Set_Modal
   --
   --  - Name:  Win_Pos_Property
   --    Type:  Gtk_Window_Position
   --    Flags: read-write
   --    Descr: The initial position of the window.
   --    See also:  Set_Position
   --
   --  - Name:  Default_Width_Property
   --    Type:  Gint
   --    Flags: read-write
   --    Descr: The default width of the window, or 0 to use the size request.
   --    See also:  Set_Default_Size
   --
   --  - Name:  Default_Height_Property
   --    Type:  Gint
   --    Flags: read-write
   --    Descr: The default height of the window, or 0 to use the size request.
   --    See also:  Set_Default_Size
   --
   --  - Name:  Destroy_With_Parent_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: If this window should be destroyed when the parent is destroyed
   --    See also:  Set_Destroy_With_Parent
   --
   --  - Name:  Has_Toplevel_Focus_Property
   --    Type:  Boolean
   --    Flags: read-only
   --    Descr: Whether the input focus is within this Gtk_Window
   --
   --  - Name:  Is_Active_Property
   --    Type:  Boolean
   --    Flags: read-only
   --    Descr: Whether the toplevel is the current active window
   --
   --  Name:  Deletable_Property
   --  Type:  Boolean
   --  Descr: Whether the window frame should have a close button
   --
   --  Name:  Opacity_Property
   --  Type:  Double
   --  Descr: The opacity of the window, from 0.0 to 1.0
   --
   --  Name:  Startup_Id_Property
   --  Type:  String
   --  Descr: Unique startup identifier for the window used by
   --         startup-notification
   --
   --  Name:  Transient_For_Property
   --  Type:  Object
   --  Descr: The transient parent of the dialog
   --
   --  </properties>

   Type_Property                : constant Gtk.Enums.Property_Gtk_Window_Type;
   Title_Property               : constant Glib.Properties.Property_String;
   Role_Property                : constant Glib.Properties.Property_String;
   Allow_Shrink_Property        : constant Glib.Properties.Property_Boolean;
   Allow_Grow_Property          : constant Glib.Properties.Property_Boolean;
   Modal_Property               : constant Glib.Properties.Property_Boolean;
   Resizable_Property           : constant Glib.Properties.Property_Boolean;
   Has_Toplevel_Focus_Property  : constant Glib.Properties.Property_Boolean;
   Is_Active_Property           : constant Glib.Properties.Property_Boolean;
   Window_Position_Property  : constant Gtk.Enums.Property_Gtk_Window_Position;
   Default_Width_Property       : constant Glib.Properties.Property_Int;
   Default_Height_Property      : constant Glib.Properties.Property_Int;
   Destroy_With_Parent_Property : constant Glib.Properties.Property_Boolean;
   Icon_Property                : constant Glib.Properties.Property_Object;
   Icon_Name_Property           : constant Glib.Properties.Property_String;
   Screen_Property              : constant Glib.Properties.Property_Object;
   Type_Hint_Property          : constant Gdk.Window.Property_Window_Type_Hint;
   Skip_Taskbar_Hint_Property   : constant Glib.Properties.Property_Boolean;
   Skip_Pager_Hint_Property     : constant Glib.Properties.Property_Boolean;
   Urgency_Hint_Property        : constant Glib.Properties.Property_Boolean;
   Accept_Focus_Property        : constant Glib.Properties.Property_Boolean;
   Focus_On_Map_Property        : constant Glib.Properties.Property_Boolean;
   Decorated_Property           : constant Glib.Properties.Property_Boolean;
   Gravity_Property             : constant Gdk.Window.Property_Gravity;
   Deletable_Property           : constant Glib.Properties.Property_Boolean;
   Opacity_Property             : constant Glib.Properties.Property_Double;
   Startup_Id_Property          : constant Glib.Properties.Property_String;
   Transient_For_Property       : constant Glib.Properties.Property_Object;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "set_focus"
   --    procedure Handler (Window : access Gtk_Window_Record'Class;
   --                       Widget : access Gtk_Widget_Record'Class);
   --    Called when the widget that has the focus has changed.
   --    This widget gets all keyboard events that happen in the window.
   --    You should not block the emission of this signal, since most of
   --    the work is done in the default handler.
   --
   --  - "frame_event"
   --    function Handler
   --      (Window : access Gtk_Window_Record'Class;
   --       Event  : Gdk.Event.Gdk_Event) return Boolean;
   --    If this function is called on a window before it is realized
   --    or showed it will have a "frame" window around widget-window.
   --    Called when the "frame" window set around a window receives events.
   --    This is mainly used by the linux-fb port to implement managed
   --    windows, but it could concievably be used by X-programs that
   --    want to do their own window decorations.
   --
   --  - "activate_focus"
   --    procedure Handler (Window : access Gtk_Window_Record'Class);
   --    You should emit this signal to request that the currently focused
   --    widget receives the "activate" signal. This is the same as calling
   --    Activate_Focus, but can be bound to a key binding
   --
   --  - "activate_default"
   --    procedure Handler (Window : access Gtk_Window_Record'Class);
   --    Same as Activate_Default, but can be bound to a key binding
   --
   --  - "move_focus"
   --    procedure Handler
   --       (Window    : access Gtk_Window_Record'Class;
   --        Direction : Gtk_Direction_Type);
   --    Emitted when a new child gains the focus
   --
   --  - "keys_changed"
   --    procedure Handler (Window : access Gtk_Window_Record'Class);
   --    Emitted when the key accelerators or mnemonics are changed for the
   --    window.
   --
   --  </signals>

   Signal_Set_Focus        : constant Glib.Signal_Name := "set_focus";
   Signal_Frame_Event      : constant Glib.Signal_Name := "frame_event";
   Signal_Activate_Focus   : constant Glib.Signal_Name := "activate_focus";
   Signal_Activate_Default : constant Glib.Signal_Name := "activate_default";
   Signal_Move_Focus       : constant Glib.Signal_Name := "move_focus";
   Signal_Keys_Changed     : constant Glib.Signal_Name := "keys_changed";

private
   type Gtk_Window_Record is new Bin.Gtk_Bin_Record with null record;

   Type_Property              : constant Gtk.Enums.Property_Gtk_Window_Type :=
     Gtk.Enums.Build ("type");
   Title_Property               : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("title");
   Allow_Shrink_Property        : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("allow_shrink");
   Allow_Grow_Property          : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("allow_grow");
   Modal_Property               : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("modal");
   Win_Pos_Property       : constant Gtk.Enums.Property_Gtk_Window_Position :=
     Gtk.Enums.Build ("window_position");
   Default_Width_Property       : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("default_width");
   Default_Height_Property      : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("default_height");
   Destroy_With_Parent_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("destroy_with_parent");
   Has_Toplevel_Focus_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has_toplevel_focus");
   Is_Active_Property        : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("is_active");
   Icon_Property                : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("icon");
   Icon_Name_Property           : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("icon-name");
   Screen_Property              : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("screen");
   Type_Hint_Property        : constant Gdk.Window.Property_Window_Type_Hint :=
     Gdk.Window.Build ("type-hint");
   Skip_Taskbar_Hint_Property   : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("skip-taskbar-hint");
   Skip_Pager_Hint_Property     : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("skip-pager-hint");
   Urgency_Hint_Property        : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("urgency-hint");
   Accept_Focus_Property        : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("accept-focus");
   Focus_On_Map_Property        : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("focus-on-map");
   Decorated_Property           : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("decorated");
   Gravity_Property             : constant Gdk.Window.Property_Gravity :=
     Gdk.Window.Build ("gravity");
   Window_Position_Property     :
     constant Gtk.Enums.Property_Gtk_Window_Position :=
     Gtk.Enums.Build ("window-position");
   Resizable_Property           : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("resizable");
   Role_Property                : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("role");
   Deletable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("deletable");
   Opacity_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("opacity");
   Startup_Id_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("startup-id");
   Transient_For_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("transient-for");

   pragma Import (C, Get_Type, "gtk_window_get_type");
   pragma Import (C, Group_Get_Type, "gtk_window_group_get_type");
end Gtk.Window;

--  <example>
--  <include>../examples/documentation/banner.adb</include>
--  </example>

--  No binding: gtk_window_get_screen
--  No binding: gtk_window_set_screen
--  No binding: gtk_window_add_embedded_xid
--  No binding: gtk_window_remove_embedded_xid
