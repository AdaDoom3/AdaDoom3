-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2006 AdaCore                    --
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
--  This package provides routines to handle initialization and set up of the
--  Gdk library.
--  </description>
--  <c_version>1.3.6</c_version>
--  <group>Gdk, the low-level API</group>

with Glib; use Glib;

with Gdk.Cursor;
with Gdk.Event;
with Gdk.Window;

package Gdk.Main is

   procedure Init;
   --  Initialize the library for use.
   --  The command line arguments are modified to reflect any arguments
   --  which were not handled. (Such arguments should either
   --  be handled by the application or dismissed).

   procedure Gdk_Exit (Error_Code : Gint);
   --  Restore the library to an un-itialized state and exits
   --  the program using the "exit" system call.
   --  Error_Code is the error value to pass to "exit".
   --  Allocated structures are freed and the program exits cleanly.
   --  This function is deprecated.

   function Set_Locale return String;
   --  Initialize handling of internationalization of strings.
   --  See Gtkada.Intl for more details.

   procedure Set_Locale;
   --  Drops the string returned by the Set_Locale function;

   procedure Set_Use_Xshm (Use_Xshm : Boolean := True);
   --  Set whether shared memory (when supported by the graphic server) should
   --  be used.

   function Get_Use_Xshm return Boolean;
   --  Return whether shared memory on the graphic server is used.

   function Get_Display return String;
   --  Return the name of the display.

   type Gdk_Grab_Status is
     (Grab_Success,
      Grab_Already_Grabbed,
      Gdk_Grab_Invalid_Time,
      Gdk_Grab_Not_Viewable,
      Gdk_Grab_Frozen);

   function Pointer_Grab
     (Window       : Gdk.Window.Gdk_Window;
      Owner_Events : Boolean := True;
      Event_Mask   : Gdk.Event.Gdk_Event_Mask;
      Confine_To   : Gdk.Window.Gdk_Window := Gdk.Window.Null_Window;
      Cursor       : Gdk.Cursor.Gdk_Cursor := Gdk.Cursor.Null_Cursor;
      Time         : Guint32 := 0) return Gdk_Grab_Status;
   --  Grab the pointer to a specific window.
   --    - Window is the window which will receive the grab
   --    - Owner_Events specifies whether events will be reported as is,
   --      or relative to Window
   --    - Event_Mask masks only interesting events
   --    - Confine_To limits the cursor movement to the specified window
   --    - Cursor changes the cursor for the duration of the grab
   --    - Time specifies the time
   --  Requires a corresponding call to Pointer_Ungrab

   procedure Pointer_Ungrab (Time : Guint32 := 0);
   --  Release any pointer grab.

   function Pointer_Is_Grabbed return Boolean;
   --  Tell wether there is an active pointer grab in effect.

   function Keyboard_Grab
     (Window       : Gdk.Window.Gdk_Window;
      Owner_Events : Boolean := True;
      Time         : Guint32 := 0) return Gdk_Grab_Status;
   --  Grab the keyboard to a specific window.
   --    - Window is the window which will receive the grab
   --    - Owner_Events specifies whether events will be reported as is,
   --      or relative to Window
   --    - Time specifies the time
   --  Requires a corresponding call to Keyboard_Ungrab

   procedure Keyboard_Ungrab (Time : Guint32 := 0);
   --  Release any keyboard grab.

   function Screen_Width return Gint;
   --  Return the width of the screen.

   function Screen_Height return Gint;
   --  Return the height of the screen.

   function Screen_Width_MM return Gint;
   --  Return the width of the screen in millimeters.

   function Screen_Height_MM return Gint;
   --  Return the height of the screen in millimeters.

   procedure Flush;
   --  Flush the queue of graphic events and then wait
   --  until all requests have been received and processed.

   procedure Beep;
   --  Emit a beep.

   procedure Set_Double_Click_Time (Msec : Guint);

private
   pragma Import (C, Gdk_Exit, "gdk_exit");
   pragma Import (C, Screen_Width, "gdk_screen_width");
   pragma Import (C, Screen_Height, "gdk_screen_height");
   pragma Import (C, Screen_Width_MM, "gdk_screen_width_mm");
   pragma Import (C, Screen_Height_MM, "gdk_screen_height_mm");
   pragma Import (C, Set_Double_Click_Time, "gdk_set_double_click_time");
   pragma Import (C, Flush, "gdk_flush");
   pragma Import (C, Beep, "gdk_beep");

end Gdk.Main;

--  missing:
--  gdk_wcstombs
--  gdk_mbstowcs
