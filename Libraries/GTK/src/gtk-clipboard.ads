-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                 Copyright (C) 2002-2013, AdaCore                  --
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
--  The Gtk_Clipboard object represents a clipboard of data shared between
--  different processes or between different widgets in the same process. Each
--  clipboard is identified by a name encoded as a Gdk_Atom. (Conversion to and
--  from strings can be done with gdk.properties.atom_intern and
--  gdk.properties.atom_name().) The default clipboard corresponds to the
--  "CLIPBOARD" atom; another commonly used clipboard is the "PRIMARY"
--  clipboard, which, in X, traditionally contains the currently selected text.
--
--  To support having a number of different formats on the clipboard at the
--  same time, the clipboard mechanism allows providing callbacks instead of
--  the actual data. When you set the contents of the clipboard, you can either
--  supply the data directly (via functions like Set_Text), or you can supply a
--  callback to be called at a later time when the data is needed (via
--  Set_With_Data or Set_With_Owner.) Providing a callback also avoids having
--  to make copies of the data when it is not needed.
--
--  Set_With_Data and Set_With_Owner are quite similar; the choice between the
--  two depends mostly on which is more convenient in a particular situation.
--  The former is most useful when you want to have a blob of data with
--  callbacks to convert it into the various data types that you advertise.
--  When the clear_func you provided is called, you simply free the data blob.
--  The latter is more useful when the contents of clipboard reflect the
--  internal state of a GObject (As an example, for the PRIMARY clipboard, when
--  an entry widget provides the clipboard's contents the contents are simply
--  the text within the selected region.) If the contents change, the entry
--  widget can call Set_With_Owner() to update the timestamp for clipboard
--  ownership, without having to worry about clear_func being called.
--
--  Requesting the data from the clipboard is essentially asynchronous. If the
--  contents of the clipboard are provided within the same process, then direct
--  function call will be made to retrieve the data, but if they are provided
--  by another process, then the data needs to be retrieved from the other
--  process, which may take some time. To avoid blocking the user interface,
--  the call to request the selection, Request_Contents takes a callback that
--  will be called when the contents are received (or when the request fails.)
--  If you don't want to deal with providing a separate callback, you can also
--  use Wait_For_Contents. What this does is run the GLib main loop recursively
--  waiting for the contents. This can simplify the code flow, but you still
--  have to be aware that other callbacks in your program can be called while
--  this recursive mainloop is running.
--
--  Along with the functions to get the clipboard contents as an arbitrary data
--  chunk, there are also functions to retrieve it as text, Request_Text and
--  Wait_For_Text. These functions take care of determining which formats are
--  advertised by the clipboard provider, asking for the clipboard in the best
--  available format and converting the results into the UTF-8 encoding. (The
--  standard form for representing strings in GTK+.)
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Inter-Process communication</group>
--  <testgtk>create_clipboard.adb</testgtk>

with Gdk.Pixbuf;
with Gdk.Types;
with Gtk.Selection;
with Gtk.Widget;
with Interfaces.C.Strings;
with System;

package Gtk.Clipboard is

   type Gtk_Clipboard is new Glib.C_Proxy;

   function Get_Type return Glib.GType;
   --  Return the internal type used for clipboards

   function Get_Clipboard
     (Widget    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Selection : Gdk.Types.Gdk_Atom)
      return Gtk.Clipboard.Gtk_Clipboard;
   --  Returns the clipboard object for the given selection to be used with
   --  Widget. Widget must have a Gdk_Display associated with it, so must be
   --  attached to a toplevel window.
   --
   --  Return value: the appropriate clipboard object. If no clipboard already
   --  exists, a new one will be created. Once a clipboard object has been
   --  created, it is persistent for all time.

   function Get
     (Selection : Gdk.Types.Gdk_Atom := Gdk.Types.Gdk_None)
      return Gtk_Clipboard;
   --  Return the clipboard object for the given selection. Cut/copy/paste menu
   --  items and keyboard shortcuts should use the default clipboard, returned
   --  by passing Gdk_None for Selection. The currently-selected object or text
   --  should be provided on the clipboard identified by Selection_Primary.
   --  Cut/copy/paste menu items conceptually copy the contents of the
   --  Selection_Primary clipboard to the default clipboard, i.e. they copy the
   --  selection to what the user sees as the clipboard.
   --
   --  (Passing Gdk_None is the same as using Atom_Intern ("CLIPBOARD", False).
   --  See http://standards.freedesktop.org/clipboards-spec/ for a
   --  detailed discussion of the "CLIPBOARD" vs. "PRIMARY" selections under
   --  the X window system. On Win32 the Selection_Primary clipboard is
   --  essentially ignored.)
   --
   --  It's possible to have arbitrary named clipboards; if you do invent new
   --  clipboards, you should prefix the selection name with an underscore
   --  (because the ICCCM requires that nonstandard atoms are
   --  underscore-prefixed), and namespace it as well. For example, if your
   --  application called "Foo" has a special-purpose clipboard, you might call
   --  it "_FOO_SPECIAL_CLIPBOARD".
   --
   --  Selection is a Gdk_Atom which identifies the clipboard to use.
   --
   --  If no clipboard already exists, a new one will be created. Once
   --  clipboard object has been created, it is persistent for all time and
   --  cannot be freed.

   procedure Set_Can_Store
     (Clipboard : Gtk_Clipboard;
      Targets   : Gtk.Selection.Target_Entry_Array);
   --  Hints that the clipboard data should be stored somewhere when the
   --  application exits or when Store is called.
   --  This value is reset when the clipboard owner changes. Where the
   --  clipboard data is stored is platform dependent.
   --  Targets is an array containing information about which forms should be
   --  stored, or an empty array to indicate that all forms should be stored.

   procedure Store (Clipboard : Gtk_Clipboard);
   --  Stores the current clipboard data somewhere so that it will stay
   --  around after the application has quit.
   --  See also Gdk.Display.Supports_Clipboard_Persistence and
   --  Gdk.Display.Store_Clipboard.

   function Get_Owner (Clipboard : Gtk_Clipboard) return Glib.Object.GObject;
   --  If the clipboard contents callbacks were set with Set_With_Owner, and
   --  the Set_With_Data or Clear has not subsequently called, returns the
   --  owner set by Set_With_Owner.

   procedure Clear (Clipboard : Gtk_Clipboard);
   --  Clear the contents of the clipboard.
   --  Generally this should only be called between the time you call
   --  Set_With_Owner or Set_With_Data, and when the Clear_Func you supplied
   --  is called. Otherwise, the clipboard may be owned by someone else.

   ----------
   -- Text --
   ----------

   type Gtk_Clipboard_Text_Received_Func is access
     procedure (Clipboard : Gtk_Clipboard;
                Text      : Interfaces.C.Strings.chars_ptr;
                Data      : System.Address);
   pragma Convention (C, Gtk_Clipboard_Text_Received_Func);
   --  Called when some text is received from the keyboard, or the retrieval
   --  fails.
   --  The Text parameter will contain the resulting text if the request
   --  succeeded, or Null_Ptr if it failed. This could happen for various
   --  reasons, in particular if the clipboard was empty or if the contents of
   --  the clipboard could not be converted into text form.

   procedure Set_Text
     (Clipboard : Gtk_Clipboard;
      Text      : UTF8_String);
   --  Set the contents of the clipboard.

   function Wait_For_Text (Clipboard : Gtk_Clipboard) return UTF8_String;
   --  Requests the contents of the clipboard as text and converts the result
   --  to UTF-8 if necessary. This function waits for the data to be received
   --  using the main loop, so events, timeouts, etc, may be dispatched during
   --  the wait.
   --
   --  Return "" if retrieving the selection data failed. (This could happen
   --  for various reasons, in particular if the clipboard was empty or if the
   --  contents of the clipboard could not be converted into text form)

   function Wait_Is_Text_Available
     (Clipboard : Gtk_Clipboard) return Boolean;
   --  Test to see if there is text available to be pasted. This function waits
   --  for the data to be received using the main loop, so events, timeouts,
   --  etc, may be dispatched during the wait.

   procedure Request_Text
     (Clipboard : Gtk_Clipboard;
      Callback  : Gtk_Clipboard_Text_Received_Func;
      User_Data : System.Address);
   --  Requests the contents of the clipboard as text. When the text is later
   --  received, it will be converted to UTF-8 if necessary, and Callback will
   --  be called.

   ------------
   -- Images --
   ------------

   type Gtk_Clipboard_Image_Received_Func is access
     procedure (Clipboard : Gtk_Clipboard;
                Pixbuf    : System.Address;
                Data      : System.Address);
   pragma Convention (C, Gtk_Clipboard_Image_Received_Func);
   --  Pixbuf will contain null if the request failed.
   --  Pixbuf must not be Unref. Pixbuf must be converted to GtkAda object
   --  using Gdk.Pixbuf.Convert.

   procedure Set_Image
     (Clipboard : Gtk_Clipboard;
      Pixbuf    : Gdk.Pixbuf.Gdk_Pixbuf);
   --  Sets the contents of the clipboard to the given pixbuf. GTK+ will take
   --  responsibility for responding for requests for the image, and for
   --  converting the image into the requested format.

   function Wait_For_Image
     (Clipboard : Gtk_Clipboard)
      return Gdk.Pixbuf.Gdk_Pixbuf;
   --  Requests the contents of the clipboard as image and converts the result
   --  to a pixbuf. This function waits for the data to be received using the
   --  main loop, so events, timeouts, etc, may be dispatched during the wait.
   --  The returned value must be freed with a call to Unref.

   function Wait_Is_Image_Available (Clipboard : Gtk_Clipboard) return Boolean;
   --  Test to see if there is an image available to be pasted. This is done by
   --  requesting the TARGETS atom and checking if it contains any of the
   --  supported image targets. This function waits for the data to be received
   --  using the main loop, so events, timeouts, etc, may be dispatched during
   --  the wait.
   --  This function is a little faster than calling Wait_For_Image since it
   --  doesn't need to retrieve the actual image data.

   procedure Request_Image
     (Clipboard : Gtk_Clipboard;
      Callback  : Gtk_Clipboard_Image_Received_Func;
      User_Data : System.Address);
   --  Requests the contents of the clipboard as image. When the image is
   --  later received, it will be converted to a pixbuf, and Callback
   --  will be called.

   --------------------
   -- Other contents --
   --------------------

   type Gtk_Clipboard_Get_Func is access procedure
     (Clipboard          : Gtk_Clipboard;
      Selection_Data     : Gtk.Selection.Selection_Data;
      Info               : Guint;
      User_Data_Or_Owner : System.Address);
   pragma Convention (C, Gtk_Clipboard_Get_Func);
   --  Called when the actual clipboard data is requested. Selection_Data
   --  should be modified to return the data.
   --  Info describes the expected format (see Gtk.Selection.Target_Entry).
   --  If User_Data is the owner (ie when you used Set_With_Owner), you must
   --  convert it to a proper Gtk_Widget by using Gtk.Widget.Convert.

   type Gtk_Clipboard_Clear_Func is access procedure
     (Clipboard          : Gtk_Clipboard;
      User_Data_Or_Owner : System.Address);
   pragma Convention (C, Gtk_Clipboard_Clear_Func);
   --  Called when the contents of the clipboard is overriden. Get_Func will
   --  not be called subsequently.
   --  If User_Data is the owner (ie when you used Set_With_Owner), you must
   --  convert it to a proper Gtk_Widget by using Gtk.Widget.Convert.

   type Gtk_Clipboard_Received_Func is access procedure
     (Clipboard      : Gtk_Clipboard;
      Selection_Data : Gtk.Selection.Selection_Data;
      User_Data      : System.Address);
   pragma Convention (C, Gtk_Clipboard_Received_Func);
   --  Called when data from the clipboard is made available to the application

   type Gtk_Clipboard_Targets_Received_Func is access procedure
     (Clipboard      : Gtk_Clipboard;
      Atoms          : Gdk.Types.Gdk_Atom_Array;
      N_Atoms        : Gint;
      User_Data      : System.Address);
   pragma Convention (C, Gtk_Clipboard_Targets_Received_Func);
   --  Called when the application has requested the list of supported targets
   --  for the current clipboard

   function Set_With_Data
     (Clipboard  : Gtk_Clipboard;
      Targets    : Gtk.Selection.Target_Entry_Array;
      Get_Func   : Gtk_Clipboard_Get_Func;
      Clear_Func : Gtk_Clipboard_Clear_Func;
      User_Data  : System.Address)
      return Boolean;
   --  Virtually sets the contents of the specified clipboard by providing a
   --  list of supported formats for the clipboard data and a function to call
   --  to get the actual data when it is requested. No actual copy of the data
   --  is made until someones actually requests it.
   --  Targets contains information about the available forms for the clipboard
   --  data.
   --  This function returns True if setting the clipboard data succeeded.

   function Set_With_Owner
     (Clipboard  : Gtk_Clipboard;
      Targets    : Gtk.Selection.Target_Entry_Array;
      Get_Func   : Gtk_Clipboard_Get_Func;
      Clear_Func : Gtk_Clipboard_Clear_Func;
      Owner      : access Glib.Object.GObject_Record'Class)
      return Boolean;
   --  Same as Set_With_Data, but an actual object is passed instead of a
   --  generic user_data. This takes care of referencing the object as
   --  appropriate.

   function Wait_For_Targets
     (Clipboard : Gtk_Clipboard) return Gdk.Types.Gdk_Atom_Array;
   --  Returns a list of targets that are present on the clipboard, or an empty
   --  array if there aren't any targets available.
   --  This function waits for the data to be received using the main
   --  loop, so events, timeouts, etc, may be dispatched during the wait.

   function Wait_For_Contents
     (Clipboard : Gtk_Clipboard;
      Target    : Gdk.Types.Gdk_Atom)
      return Gtk.Selection.Selection_Data;
   --  Requests the contents of the clipboard using the given target. This
   --  function waits for the data to be received using the main loop, so
   --  events, timeouts, etc, may be dispatched during the wait.
   --  The result must be freed.

   function Wait_Is_Target_Available
     (Clipboard : Gtk_Clipboard; Target : Gdk.Types.Gdk_Atom) return Boolean;
   --  Checks if a clipboard supports pasting data of a given type. This
   --  function can be used to determine if a "Paste" menu item should be
   --  insensitive or not.
   --  If you want to see if there's text available on the clipboard, use
   --  Wait_Is_Text_Available instead.
   --  The value for Target is similar to the one in Gtk.Selection.Target_Entry

   procedure Request_Contents
     (Clipboard : Gtk_Clipboard;
      Target    : Gdk.Types.Gdk_Atom;
      Callback  : Gtk_Clipboard_Received_Func;
      User_Data : System.Address);
   --  Requests the contents of clipboard as the given target.
   --  When the results of the result are later received the supplied callback
   --  will be called.

   procedure Request_Targets
     (Clipboard : Gtk_Clipboard;
      Callback  : Gtk_Clipboard_Targets_Received_Func;
      User_Data : System.Address);
   --  Requests the contents of the clipboard as list of supported targets.
   --  When the list is later received, Callback will be called.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "owner_change"
   --    Emitted when the owner of the clipboard has changed
   --
   --  </signals>

   Signal_Owner_Change : constant Glib.Signal_Name := "owner_change";

private
   pragma Import (C, Get_Type,          "gtk_clipboard_get_type");
   pragma Import (C, Get,               "gtk_clipboard_get");
   pragma Import (C, Clear,             "gtk_clipboard_clear");
   pragma Import (C, Store,             "gtk_clipboard_store");
   pragma Import (C, Request_Text,      "gtk_clipboard_request_text");
   pragma Import (C, Request_Image,     "gtk_clipboard_request_image");
   pragma Import (C, Wait_For_Contents, "gtk_clipboard_wait_for_contents");
end Gtk.Clipboard;

--  No binding: gtk_clipboard_get_display
--  No binding: gtk_clipboard_get_for_display
