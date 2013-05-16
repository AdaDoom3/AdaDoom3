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
--  This package provides functions dealing with events from the window system.
--  In GtkAda applications, the events are handled automatically in
--  Gtk.Main.Do_Event, and passed on to the appropriate widgets, so these
--  functions are rarely needed.
--
--  !! Warning !! This is one of the only package that requires manual
--  memory management in some cases. If you use the function Allocate,
--  you have to use the function Free too...
--  </description>
--  <c_version>1.3.6</c_version>
--  <group>Gdk, the low-level API</group>

with System;
with Glib; use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
pragma Elaborate_All (Glib.Generic_Properties);
with Glib.Values;
with Gdk.Rectangle;
with Gdk.Region;
with Gdk.Types;

package Gdk.Event is

   -----------------------------
   -- Definition of the types --
   -----------------------------

   type Gdk_Event_Type is
     (Nothing,
      --  No event occurred.

      Delete,
      --  A window delete event was sent by the window manager. The specified
      --  window should be deleted.

      Destroy,
      --  A window has been destroyed.

      Expose,
      --  Part of a window has been uncovered.

      Motion_Notify,
      Button_Press,
      --  A mouse button was pressed.

      Gdk_2button_Press,
      --  Double-click

      Gdk_3button_Press,
      --  Triple-click

      Button_Release,
      --  A mouse button was released.

      Key_Press,
      --  A key was pressed.

      Key_Release,
      --  A key was released.

      Enter_Notify,
      --  A window was entered.

      Leave_Notify,
      --  A window was exited.

      Focus_Change,
      --  The focus window has changed. (The focus window gets keyboard events)

      Configure,
      Map,
      --  A window has been mapped. (It is now visible on the screen).

      Unmap,
      --  A window has been unmapped. (It is no longer visible on the screen).

      Property_Notify,
      Selection_Clear,
      Selection_Request,
      Selection_Notify,
      Proximity_In,
      Proximity_Out,
      Drag_Enter,
      Drag_Leave,
      Drag_Motion,
      Drag_Status,
      Drop_Start,
      Drop_Finished,
      Client_Event,
      Visibility_Notify,
      No_Expose,
      Scroll,
      --  A mouse wheel was scrolled either up or down.

      Window_State,
      Setting,

      Owner_Change,
      --  Emitted when the owner of a selection has changed
      --  Added in gtk+ 2.6
      --  See Gdk.Display.Supports_Selection_Notification

      Grab_Broken
      --  A pointer or keyboard grab was broken
      --  Added in gtk+ 2.8
      );
   pragma Convention (C, Gdk_Event_Type);

   type Gdk_Event_Mask is mod 2 ** 32;
   --  Note that you need to change the event mask of a widget if you want
   --  to be able to get some events. To change this mask, the widget
   --  must first be Unrealized.

   Exposure_Mask            : constant Gdk_Event_Mask := 2 ** 1;

   Pointer_Motion_Mask      : constant Gdk_Event_Mask := 2 ** 2;
   --  Every time the mouse moves, GtkAda will send a Motion_Notify event.
   --  These events will be sent as fast as possible, and your application
   --  needs to be able to respond as fast as possible (generally about
   --  200 events per second).

   Pointer_Motion_Hint_Mask : constant Gdk_Event_Mask := 2 ** 3;
   --  GtkAda will only send one Motion_Notify event when the mouse moves.
   --  The handler should call Gdk.Window.Get_Pointer, to get the current
   --  position and signals GtkAda that it is ready to get another
   --  Motion_Notify signal. No new Motion_Notify will be sent until
   --  Get_Pointer has been called.

   Button_Motion_Mask       : constant Gdk_Event_Mask := 2 ** 4;
   Button1_Motion_Mask      : constant Gdk_Event_Mask := 2 ** 5;
   Button2_Motion_Mask      : constant Gdk_Event_Mask := 2 ** 6;
   Button3_Motion_Mask      : constant Gdk_Event_Mask := 2 ** 7;
   Button_Press_Mask        : constant Gdk_Event_Mask := 2 ** 8;
   Button_Release_Mask      : constant Gdk_Event_Mask := 2 ** 9;
   Key_Press_Mask           : constant Gdk_Event_Mask := 2 ** 10;
   Key_Release_Mask         : constant Gdk_Event_Mask := 2 ** 11;
   Enter_Notify_Mask        : constant Gdk_Event_Mask := 2 ** 12;
   Leave_Notify_Mask        : constant Gdk_Event_Mask := 2 ** 13;
   Focus_Change_Mask        : constant Gdk_Event_Mask := 2 ** 14;
   Structure_Mask           : constant Gdk_Event_Mask := 2 ** 15;
   Property_Change_Mask     : constant Gdk_Event_Mask := 2 ** 16;
   Visibility_Notify_Mask   : constant Gdk_Event_Mask := 2 ** 17;
   Proximity_In_Mask        : constant Gdk_Event_Mask := 2 ** 18;
   Proximity_Out_Mask       : constant Gdk_Event_Mask := 2 ** 19;
   Substructure_Mask        : constant Gdk_Event_Mask := 2 ** 20;
   Scroll_Mask              : constant Gdk_Event_Mask := 2 ** 21;
   All_Events_Mask          : constant Gdk_Event_Mask := 16#3FFFFE#;

   type Gdk_Visibility_State is
     (Visibility_Unobscured,
      Visibility_Partial,
      Visibility_Fully_Obscured);
   pragma Convention (C, Gdk_Visibility_State);

   type Gdk_Scroll_Direction is
     (Scroll_Up,
      Scroll_Down,
      Scroll_Left,
      Scroll_Right);
   pragma Convention (C, Gdk_Scroll_Direction);

   type Gdk_Notify_Type is
     (Notify_Ancestor,
      Notify_Virtual,
      Notify_Inferior,
      Notify_Non_Linear,
      Notify_Non_Linear_Virtual,
      Notify_Unknown);
   pragma Convention (C, Gdk_Notify_Type);

   type Gdk_Crossing_Mode is (Crossing_Normal, Crossing_Grab, Crossing_Ungrab);
   pragma Convention (C, Gdk_Crossing_Mode);

   type Gdk_Property_State is (Property_New_Value, Property_Delete);
   pragma Convention (C, Gdk_Property_State);

   type Gdk_Window_State is mod 2 ** 32;
   --  State of a Window. Default is 0.

   Window_State_Withdraw  : constant Gdk_Window_State := 2 ** 0;

   Window_State_Iconified : constant Gdk_Window_State := 2 ** 1;
   --  Window is iconified on the desktop

   Window_State_Maximized : constant Gdk_Window_State := 2 ** 2;
   --  Window is maximized on the desktop

   Window_State_Sticky    : constant Gdk_Window_State := 2 ** 3;

   type Gdk_Setting_Action is
     (Setting_Action_New,
      Setting_Action_Changed,
      Setting_Action_Deleted);
   pragma Convention (C, Gdk_Setting_Action);

   type Gdk_Device_Id is new Guint32;
   --  This type is specific to GtkAda. In Gdk, guint32 is used instead.

   type Gdk_Event is new Gdk.C_Proxy;

   subtype Gdk_Event_Any is Gdk_Event;
   --  Change from GtkAda1.2.3: There is no longer a tagged type
   --  hierarchy, only one type.
   --  However there are now a few runtime tests for each of the
   --  function, to check whether a given field is available or not.
   --
   --  Fields common to all events: Window, Send_Event, Event_Type

   subtype Gdk_Event_Expose is Gdk_Event;
   --  The window needs to be redrawn. For efficiency, gtk gives you the
   --  smallest area that you need to redraw.
   --  Relevant fields: Area, Region, Count
   --  Type: Expose

   subtype Gdk_Event_No_Expose is Gdk_Event;
   --  Indicate that the source region was completely available when parts of
   --  a drawable were copied.
   --  This is also emitted when a gc whose "exposures" attribute is set to
   --  False in a call to Copy_Area or Draw_Pixmap. See the documentation for
   --  Gdk.GC.Set_Exposures.
   --  No Relevent fields except the common ones
   --  Type: No_Expose

   subtype Gdk_Event_Visibility is Gdk_Event;
   --  The visibility state of the window (partially visibly, fully visible,
   --  hidden). This event almost never need to be used, since other events
   --  are generated at the same time, like expose_events
   --  Relevant fields: Visibility_State
   --  type: Visibility_Notify

   subtype Gdk_Event_Motion is Gdk_Event;
   --  The mouse has moved
   --  Relevant fields: Time, X, Y, Axes, State, Is_Hint, Device_Id,
   --  X_Root, Y_Root
   --  Type: Motion_Notify

   subtype Gdk_Event_Button is Gdk_Event;
   --  A button was pressed or released.
   --  Relevant fields: Time, X, Y, Axes, State, Button, Device_Id,
   --  X_Root, Y_Root, Window.
   --  Type: Button_Press, Gdk_2Button_Press, Gdk_3Button_Press or
   --  Button_Release.

   subtype Gdk_Event_Scroll is Gdk_Event;
   --  A button was pressed or released.
   --  Relevant fields: Time, X, Y, State, Direction, Device_Id, X_Root, Y_Root
   --  Type: Scroll

   subtype Gdk_Event_Key is Gdk_Event;
   --  A keyboard key was pressed
   --  Relevant fields: Time, State, Key_Val, String, Hardware_Keycode, Group
   --  Type: Key_Press, Key_Release

   subtype Gdk_Event_Crossing is Gdk_Event;
   --  The mouse has been moved in or out of the window
   --  Relevant fields: SubWindow, Time, X, Y, X_Root, Y_Root, Mode,
   --  Detail, Focus, State
   --  Type: Enter_Notify, Leave_Notify

   subtype Gdk_Event_Focus is Gdk_Event;
   --  The focus has changed for a window.
   --  Relevant fields: in
   --  Type: Focus_Change

   subtype Gdk_Event_Configure is Gdk_Event;
   --  The window configuration has changed: either it was remapped,
   --  resized, moved, ...
   --  Note that you usually don't have to redraw your window when you
   --  receive such an event, since it is followed by an Gdk_Event_Expose.
   --  Relevant fields: X, Y, Width, Height
   --  Type: Configure

   subtype Gdk_Event_Property is Gdk_Event;
   --  Some property of the window was modified. GtkAda provides a higher
   --  level interface, and you almost never need to use this event.
   --  Relevent fields: Atom, Time, Property_State
   --  Type: Property_Notify

   subtype Gdk_Event_Selection is Gdk_Event;
   --  This is how X11 implements a simple cut-and-paste mechanism. However,
   --  GtkAda provides a higher level interface to the selection mechanism,
   --  so this event will almost never be used.
   --  Relevant fields: Selection, Target, Property, Time, Requestor
   --  Type: Selection_Clear, Selection_Request, Selection_Notify

   subtype Gdk_Event_Proximity is Gdk_Event;
   --  This event type will be used pretty rarely. It only is
   --  important for XInput aware programs that are drawing their own
   --  cursor. This is only used with non standard input devices, like
   --  graphic tablets.
   --  Relevant fields: Time, Device_Id
   --  Type: Proximity_In, Proximity_Out

   subtype Gdk_Event_Client is Gdk_Event;
   --  This is an event used to send arbitrary data from one X application
   --  to another. This event too is almost never used, and is not documented
   --  here. Please consult an X11 documentation for more information.
   --  Relevant fields: Message_Type, Data
   --  Type: Client_Event

   subtype Gdk_Event_Setting is Gdk_Event;
   --  ???
   --  Relevant fields: Action, Name.
   --  Type: ???

   subtype Gdk_Event_Window_State is Gdk_Event;
   --  ???
   --  Relevant fields: Changed_Mask, New_Window_State.
   --  Type: Delete, Destroy, Map, Unmap ???

   subtype Gdk_Event_DND is Gdk_Event;
   --  ???
   --  Relevant fields: Context, Time, X_Root, Y_Root
   --  Type: Drag_Enter, Drag_Leave, Drag_Motion, Drag_Status, Drop_Start,
   --  Drop_Finished

   ----------------------------------------
   -- Specific definition for the fields --
   ----------------------------------------

   type Gdk_Event_Client_Data_Format is
     (Char_Array, Short_Array, Long_Array);
   for Gdk_Event_Client_Data_Format use
     (Char_Array  => 8, Short_Array => 16, Long_Array  => 32);
   --  Values extracted from the XClientMessageEvent man page.

   Number_Of_Characters : constant := 20;
   Number_Of_Shorts     : constant := 10;
   Number_Of_Longs      : constant := 5;

   type Gdk_Event_Client_Data
     (Format : Gdk_Event_Client_Data_Format) is
   record
      case Format is
         when Char_Array =>
            B : String (1 .. Number_Of_Characters);
         when Short_Array =>
            S : Gshort_Array (1 .. Number_Of_Shorts);
         when Long_Array =>
            L : Glong_Array (1 .. Number_Of_Longs);
      end case;
   end record;

   -----------------------------------
   -- Access to fields of the event --
   -----------------------------------
   --  The following functions can be used to retrieve some specific fields
   --  from an event. Some of these fields do not exist for all the types of
   --  events (see the description of each event for a list of the relevant
   --  fields).
   --  Note also that you can not pass a null event to them. The parameter must
   --  be a correct event, or the result is undefined.

   Invalid_Field : exception;
   --  If a field does not exist for the event you gave, an exception
   --  Invalid_Field is raised

   function Get_Event_Type (Event : Gdk_Event) return Gdk_Event_Type;
   --  The type of the event.

   function Get_Send_Event (Event : Gdk_Event) return Boolean;
   --  Set to true if the event was generated by the application, False
   --  if generated by the X server/Win32.

   function Get_Window    (Event : Gdk_Event) return Gdk.Gdk_Window;
   --  The window the event occured on.
   --  See the function Gdk.Window.Get_User_Data to get the actual widget.

   function Get_Time      (Event : Gdk_Event) return Guint32;
   --  Time when the event occured.

   function Get_X         (Event : Gdk_Event) return Gdouble;
   --  Horizontal coordinate of the mouse when the event occured.
   --  The coordinates are relative to the parent window.

   function Get_Y         (Event : Gdk_Event) return Gdouble;
   --  Vertical coordinate of the mouse when the event occured.
   --  The coordinates are relative to the parent window.

   function Get_X_Root    (Event : Gdk_Event) return Gdouble;
   --  Horizontal coordinate of the mouse when the event occured.
   --  Relative to the root window.

   function Get_Y_Root    (Event : Gdk_Event) return Gdouble;
   --  Vertical coordinate of the mouse when the event occured.
   --  Relative to the root window.

   function Get_Button    (Event : Gdk_Event) return Guint;
   --  Number of the button that was pressed.

   function Get_State (Event : Gdk_Event) return Gdk.Types.Gdk_Modifier_Type;
   --  State of the mouse buttons and keyboard keys just prior to the event.

   function Get_Subwindow (Event : Gdk_Event) return Gdk.Gdk_Window;
   --  Child window for the event.
   --  For an Enter_Notify_Event, this is set to the initial window for the
   --  pointer; for an Leave_Notify_Event this is set to the window occupied
   --  by the pointer in its last position.

   function Get_Mode (Event : Gdk_Event) return Gdk_Crossing_Mode;
   --  Return the mode of an Event.
   --  Set to indicate whether the events are normal events, pseudo-motion
   --  events when a grab activates or pseudo-motion events when a grab
   --  deativates.

   function Get_Detail (Event : Gdk_Event) return Gdk_Notify_Type;
   --  Set to indicate the notify details.
   --  Most applications can ignore events with a Notify Virtual or a
   --  Notify_Non_Linear_Virtual detail.

   function Get_Focus (Event : Gdk_Event) return Boolean;
   --  Set to true if the window for the event is the focus window.

   function Get_Width (Event : Gdk_Event) return Gint;
   --  Get the width in a configure event.

   function Get_Height (Event : Gdk_Event) return Gint;
   --  Get the height in a configure event.

   function Get_Direction (Event : Gdk_Event) return Gdk_Scroll_Direction;
   --  Get the direction in a scroll event.

   function Get_Device_Id (Event : Gdk_Event) return Gdk_Device_Id;
   --  Set to a constant for now in the gtk+ source... Probably useless.
   --  Since multiple input devices can be used at the same time, like a mouse
   --  and a graphic tablet, this indicates which one generated the event.

   function Get_Area (Event : Gdk_Event) return Rectangle.Gdk_Rectangle;
   --  The minimal area on which the event applies.
   --  For Expose_Events, this is the minimal area to redraw.

   function Get_Region (Event : Gdk_Event) return Gdk.Region.Gdk_Region;
   --  Return the region to which the event applies.
   --  Do not free the returned value

   function Get_Count (Event : Gdk_Event) return Gint;
   --  Number of Expose_Events that are to follow this one.
   --  Most applications can ignore the event if Count is not 0, which also
   --  allows for optimizations.

   function Get_In (Event : Gdk_Event) return Boolean;
   --  True if the window has gained the focus, False otherwise.

   function Get_Is_Hint (Event : Gdk_Event) return Boolean;
   --  ???

   function Get_Key_Val (Event : Gdk_Event) return Gdk.Types.Gdk_Key_Type;
   --  Code of the key that was pressed (and that generated the event).

   function Get_Group (Event : Gdk_Event) return Guint8;
   --  Group of the key that was pressed;

   function Get_Hardware_Keycode (Event : Gdk_Event) return Guint16;
   --  Hardware key code of the key that was pressed.

   function Get_String  (Event : Gdk_Event) return String;
   --  Symbol of the key that was pressed, as a string.

   function Get_Atom (Event : Gdk_Event) return Gdk.Types.Gdk_Atom;
   --  Indicate which property has changed.
   --  ??? Atom should not be a Guint

   function Get_Property_State (Event : Gdk_Event) return Guint;
   --  ??? The return type should be changed.

   function Get_Visibility_State
     (Event : Gdk_Event) return Gdk_Visibility_State;
   --  Return the new visibility state for the window.

   function Get_Selection (Event : Gdk_Event) return Gdk.Types.Gdk_Atom;
   --  What was selected in the window...

   function Get_Target (Event : Gdk_Event) return Gdk.Types.Gdk_Atom;
   --  ???

   function Get_Property (Event : Gdk_Event) return Gdk.Types.Gdk_Atom;
   --  ???

   function Get_Requestor (Event : Gdk_Event) return Guint32;
   --  ???

   function Get_Message_Type (Event : Gdk_Event) return Gdk.Types.Gdk_Atom;
   --  ???

   function Get_Data (Event : Gdk_Event) return Gdk_Event_Client_Data;
   --  ???

   --------------------------------------
   -- Modifying the fields of an event --
   --------------------------------------

   procedure Set_Window (Event : Gdk_Event; Win : Gdk.Gdk_Window);
   --  Set the Window field of an event.

   procedure Set_X      (Event : Gdk_Event; X : Gdouble);
   --  Set the X field of an event.

   procedure Set_Y      (Event : Gdk_Event; Y : Gdouble);
   --  Set the Y field of an event.

   procedure Set_Xroot  (Event : Gdk_Event; Xroot : Gdouble);
   --  Set the Xroot field of an event.

   procedure Set_Yroot  (Event : Gdk_Event; Yroot : Gdouble);
   --  Set the Yroot field of an event.

   procedure Set_Width  (Event : Gdk_Event; Width : Gint);
   --  Set the Width field of an event.

   procedure Set_Height (Event : Gdk_Event; Height : Gint);
   --  Set the Height field of an event.

   procedure Set_Button (Event : Gdk_Event; Button : Guint);
   --  Set the Button field of an event.

   procedure Set_Time (Event : Gdk_Event; Time : Guint32);
   --  Set the time for the event.
   --  If Time is 0, then it is set to the current time.

   procedure Set_State
     (Event : Gdk_Event; State : Gdk.Types.Gdk_Modifier_Type);
   --  Set the State field of an event.

   procedure Set_Subwindow (Event : Gdk_Event; Window : Gdk.Gdk_Window);
   --  Set the Subwindow field of an event.

   procedure Set_Mode (Event : Gdk_Event; Mode : Gdk_Crossing_Mode);
   --  Set the Mode field of an event.

   procedure Set_Detail (Event : Gdk_Event; Detail : Gdk_Notify_Type);
   --  Set the Detail field of an event.

   procedure Set_Focus (Event : Gdk_Event; Has_Focus : Boolean);
   --  Set the Focus field of an event.

   procedure Set_Area  (Event : Gdk_Event; Area : Rectangle.Gdk_Rectangle);
   --  Set the Area field of an event.

   procedure Set_In    (Event : Gdk_Event; Focus_In : Boolean);
   --  Set the In field of an event.

   procedure Set_Is_Hint (Event : Gdk_Event; Is_Hint : Boolean);
   --  Set the Is_Hint field of an event.

   procedure Set_Key_Val (Event : Gdk_Event; Key : Gdk.Types.Gdk_Key_Type);
   --  Set the Key_Val field of an event.

   procedure Set_Group (Event : Gdk_Event; Group : Guint8);
   --  Set the group field of a key event.

   procedure Set_Hardware_Keycode (Event : Gdk_Event; Keycode : Guint16);
   --  Set the hardware key code field of a key event.

   procedure Set_Direction
     (Event : Gdk_Event; Direction : Gdk_Scroll_Direction);
   --  Set the direction field of a scroll event.

   procedure Set_Atom (Event : Gdk_Event; Atom : Gdk.Types.Gdk_Atom);
   --  Set the Atom field of an event.

   procedure Set_Property_State (Event : Gdk_Event; State : Guint);
   --  Set the Property_State field of an event.

   procedure Set_Visibility_State
     (Event : Gdk_Event; State : Gdk_Visibility_State);
   --  Set the Visibility_State field of an event.

   procedure Set_Selection (Event : Gdk_Event; Selection : Gdk.Types.Gdk_Atom);
   --  Set the Selection field of an event.

   procedure Set_Target (Event : Gdk_Event; Target : Gdk.Types.Gdk_Atom);
   --  Set the Target field of an event.

   procedure Set_Property (Event : Gdk_Event; Property : Gdk.Types.Gdk_Atom);
   --  Set the Property field of an event.

   procedure Set_Requestor (Event : Gdk_Event; Requestor : Guint32);
   --  Set the Requestor field of an event.

   procedure Set_Message_Type (Event : Gdk_Event; Typ : Gdk.Types.Gdk_Atom);
   --  Set the Message_Type field of an event.

   procedure Set_String (Event : Gdk_Event; Str : String);
   --  Set the string associated with an event.

   -----------------------
   -- General functions --
   -----------------------

   function Get_Type return GType;
   --  Return the type corresponding to a Gdk_Event.

   procedure Deep_Copy (From : Gdk_Event; To : out Gdk_Event);
   --  Deep copy for an event. The C structure is itself duplicated.
   --  You need to deallocated it yourself with a call to Free below.

   procedure Get_Graphics_Expose
     (Event  : out Gdk_Event_Expose;
      Window : Gdk.Gdk_Window);
   --  Waits for a GraphicsExpose or NoExpose event
   --  If it gets a GraphicsExpose event, it returns a pointer to it,
   --  otherwise it returns an event for which Is_Created is False.
   --
   --  This function can be used to implement scrolling: you must call
   --  Gdk.GC.Set_Exposures with True on the GC you are using for the
   --  drawing, so that a events are generated for obscured areas and every
   --  time a new part of the widget is drawn. However, there is a race
   --  condition if multiple scrolls happen before you have finished
   --  processing the first one. A workaround is to call Get_Graphics_Expose
   --  after every scroll until it returns a null event.

   function Events_Pending return Boolean;
   --  Is there any event pending on the queue ?

   procedure Get (Event : out Gdk_Event);
   --  Get the next event on the queue.

   procedure Peek (Event : out Gdk_Event);
   --  Look at the next event on the queue, but leave if there.

   procedure Put (Event : Gdk_Event);
   --  Add an event on the queue - Better to use Gtk.Signal.Emit_By_Name

   procedure Set_Show_Events (Show_Events : Boolean := True);
   --  For debug purposes, you can choose whether you want to see the events
   --  GtkAda receives.

   function Get_Show_Events return Boolean;
   --  Return the current state of Show_Events.

   procedure Send_Client_Message_To_All (Event : Gdk_Event);
   --  Low level routine to send an Event to every window.

   function Send_Client_Message
     (Event : Gdk_Event;
      Xid   : Guint32) return Boolean;
   --  Low level routine to send an Event to a specified X window.

   procedure Allocate
     (Event      : out Gdk_Event;
      Event_Type : Gdk_Event_Type;
      Window     : Gdk.Gdk_Window);
   --  Create an event, whose fields are uninitialized.
   --  You need to use the function Set_* above to modify them, before you can
   --  send the event with Emit_By_Name.
   --  !!Note!!: The event has to be freed if you have called this function.
   --  Use the function Free below.

   procedure Free (Event : in out Gdk_Event);
   --  Free the memory (and C structure) associated with an event.
   --  You need to call this function only if the event was created through
   --  Allocate, not if it was created by GtkAda itself (or you would get
   --  a segmentation fault).

   type Event_Handler_Func is access procedure
     (Event : Gdk_Event; Data : System.Address);
   pragma Convention (C, Event_Handler_Func);
   --  Function that can be used as a new event handler.
   --  This function should dispatch all the events properly, since it replaces
   --  completly the default event handler. However, it can call
   --  Gtk.Main.Do_Event to take care of the events it does not know how to
   --  handle.
   --  See also Gtk.Main.Get_Event_Widget to get the widget from the event.

   procedure Event_Handler_Set
     (Func : Event_Handler_Func; Data : System.Address);
   --  Set up a new event handler.
   --  This handler replaces the default GtkAda event handler, and thus should
   --  make sure that all events are correctly handled.
   --
   --  Note that managing the memory for Data is your responsability, and
   --  Data is passed as is to Func.

   function From_Address (C : System.Address) return Gdk_Event;
   --  Convert a C handler to the matching Event structure.

   function To_Address (C : Gdk_Event) return System.Address;
   --  Convert an event to the underlying C handler.

   function Is_Created (E : Gdk_Event) return Boolean;
   --  Return True if the underlying C event has been created.

   --------------------
   -- GValue support --
   --------------------

   function Get_Event (Value : Glib.Values.GValue) return Gdk_Event;
   --  Convert a value into a Gdk_Event.

   ----------------
   -- Properties --
   ----------------
   --  The following packages and types are used to represent properties of
   --  the given type. They are used in the packages that use these properties

   package Event_Mask_Properties is new Generic_Internal_Discrete_Property
     (Gdk_Event_Mask);

   type Property_Gdk_Event_Mask is new Event_Mask_Properties.Property;

   -------------------
   -- Design issues --
   -------------------
   --  See gdk-event.adb for some of the design issues behing that package.

   ---------------------
   -- Event Recording --
   ---------------------

   procedure Set_Follow_Events (Follow_Events : Boolean := True);
   --  Set whether windows should follow events that they normally don't
   --  (such as motion events) for event recording purposes.
   --  This function is used in cunjonction with GtkAda.Macro

   function Get_Follow_Events return Boolean;
   --  Return follow_events value.
   --  See Set_Follow_Events for more details.

private

   for Gdk_Event_Type use
     (Nothing => -1,
      Delete => 0,
      Destroy => 1,
      Expose => 2,
      Motion_Notify => 3,
      Button_Press => 4,
      Gdk_2button_Press => 5,
      Gdk_3button_Press => 6,
      Button_Release => 7,
      Key_Press => 8,
      Key_Release => 9,
      Enter_Notify => 10,
      Leave_Notify => 11,
      Focus_Change => 12,
      Configure => 13,
      Map => 14,
      Unmap => 15,
      Property_Notify => 16,
      Selection_Clear => 17,
      Selection_Request => 18,
      Selection_Notify => 19,
      Proximity_In => 20,
      Proximity_Out => 21,
      Drag_Enter => 22,
      Drag_Leave => 23,
      Drag_Motion => 24,
      Drag_Status => 25,
      Drop_Start => 26,
      Drop_Finished => 27,
      Client_Event => 28,
      Visibility_Notify => 29,
      No_Expose => 30,
      Scroll => 31,
      Window_State => 32,
      Setting => 33,
      Owner_Change => 34,
      Grab_Broken => 35);

   pragma Import (C, Get_Type, "gdk_event_get_type");
   pragma Import (C, Get_Event_Type, "ada_gdk_event_get_type");
   pragma Import (C, Get_Window, "ada_gdk_event_get_window");
   pragma Import (C, Get_Time, "gdk_event_get_time");
   pragma Import (C, Put, "gdk_event_put");
   pragma Import (C, Get_Region, "ada_gdk_event_get_region");
   pragma Import
     (C, Send_Client_Message_To_All, "gdk_event_send_clientmessage_toall");
   pragma Import (C, Set_Window, "ada_gdk_event_set_window");

end Gdk.Event;
