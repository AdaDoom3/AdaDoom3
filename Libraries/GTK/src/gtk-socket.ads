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
--  Note that this package is currently not supported under Win32 systems.
--
--  Together with Gtk_Plug, Gtk_Socket provides the ability to embed widgets
--  from one process into another process in a fashion that is transparent to
--  the user. One process creates a Gtk_Socket widget and, passes the XID of
--  that widget's window to the other process, which then creates a Gtk_Plug
--  window with that XID.
--  Any widgets contained in the Gtk_Plug then will appear inside the first
--  applications window.
--
--  The XID of the socket's window is obtained by using the
--  XWindow function provided in this package. Before using
--  this macro, the socket must have been realized, and for hence, have been
--  added to its parent.
--
--  Note that if you pass the XID of the socket to another process that will
--  create a plug in the socket, you must make sure that the socket widget is
--  not destroyed until that plug is created. Violating this rule will cause
--  unpredictable consequences, the most likely consequence being that the plug
--  will appear as a separate toplevel window. You can check if the plug has
--  been created by examining the plug_window field of the Gtk_Socket
--  structure. If this field is non-NULL, then the plug has been succesfully
--  created inside of the socket.
--
--  When GtkAda is notified that the embedded window has been destroyed, then
--  it will destroy the socket as well. You should always, therefore, be
--  prepared for your sockets to be destroyed at any time when the main event
--  loop is running.
--
--  A socket can also be used to swallow arbitrary pre-existing top-level
--  windows using Steal, though the integration when this is done will not be
--  as close as between a Gtk_Plug and a Gtk_Socket. All you need in that case
--  is the X11 window identifier for the external process.
--
--  Note that it is recommended that the external window be first hidden before
--  being swallowed, so that Gtk.Socket works with most window managers. If
--  you start with visible windows, some window managers will not be able to
--  correctly merge the two windows (Enlightenment for instance).
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Inter-Process communication</group>

with Gtk.Container;
with Gdk.Window;

package Gtk.Socket is

   type Gtk_Socket_Record is new
     Gtk.Container.Gtk_Container_Record with private;
   type Gtk_Socket is access all Gtk_Socket_Record'Class;

   procedure Gtk_New (Widget : out Gtk_Socket);
   --  Create a new empty GtkSocket.

   procedure Initialize (Widget : access Gtk_Socket_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Gtk_Socket.

   procedure Add_Id (Socket : access Gtk_Socket_Record; Id : Guint32);
   --  Add an XEMBED client, such as a Gtk_Plug, to the Gtk_Socket.
   --  The client may be in the same process or in a different process.
   --
   --  To embed a Gtk_Plug in a Gtk_Socket, you can either create the
   --  Gtk_Plug with Gtk_New (0), call Gtk.Plug.Get_Id to get the
   --  window ID of the plug, and then pass that to the Gtk.Socket.Add_Id, or
   --  you can call Gtk.Socket.Get_Id to get the window ID for the socket, and
   --  call Gtk.Plug.Gtk_New passing in that ID.
   --
   --  Id: the XID of a client participating in the XEMBED protocol.
   --
   --  The Gtk_Socket must have already be added into a toplevel window
   --  before you can make this call.

   function Get_Id (Socket : access Gtk_Socket_Record) return Guint32;
   --  Get the window ID of a Gtk_Socket widget, which can then be used to
   --  create a client embedded inside the socket, for instance with
   --  Gtk.Socket.Gtk_New (Id). The Gtk_Socket must have already been added
   --  into a toplevel window before you can make this call.

   function Get_Plug_Window
     (Socket : access Gtk_Socket_Record) return Gdk.Window.Gdk_Window;
   --  Return the id of the embedded window.

   -----------------
   -- Obsolescent --
   -----------------
   --  All subprograms below are now obsolescent in gtk+. They might be removed
   --  from future versions of gtk+ (and therefore GtkAda).
   --  To find out whether your code uses any of these, we recommend compiling
   --  with the -gnatwj switch
   --  <doc_ignore>

   procedure Steal (Socket : access Gtk_Socket_Record; Wid : Guint32);
   pragma Obsolescent;  --  Steal
   --  Reparent a pre-existing toplevel window into a Gtk_Socket.
   --  This is meant to embed clients that do not know about embedding into a
   --  Gtk_Socket, however doing so is inherently unreliable, and using
   --  this function is not recommended.
   --  The Gtk_Socket must have already be added into a toplevel window
   --  before you can make this call.
   --  Wid is the XID of an existing toplevel window.

   --  </doc_ignore>

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  </properties>

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "plug_added"
   --    procedure Handler (Socket : access Gtk_Socket_Record'Class);
   --    Emitted when a client is successfully added to the socket
   --
   --  - "plug_removed"
   --    function Handler
   --       (Socket : access Gtk_Socket_Record'Class) return Boolean;
   --    Emitted when a client is successfully removed from the socket. The
   --    default action is to destroy the socket, so you want to reuse it you
   --    must return True.
   --
   --  </signals>

   Signal_Plug_Added   : constant Glib.Signal_Name := "plug_added";
   Signal_Plug_Removed : constant Glib.Signal_Name := "plug_removed";

private

   type Gtk_Socket_Record is new Gtk.Container.Gtk_Container_Record
     with null record;

   pragma Import (C, Get_Type, "gtk_socket_get_type");
end Gtk.Socket;

--  <example>
--  Obtaining the XID of a socket
--
--  with Gtk.Socket;
--  use Gtk.Socket;
--
--  Socket : Gtk_Socket;
--
--  Gtk_New (Socket);
--  Show (Socket);
--  Add (Parent, Socket);
--
--  --  The following call is only necessary if one of
--  --  the ancestors of the socket is not yet visible.
--
--  Realize (Socket);
--  Put_Line ("The XID of the sockets window is" &
--            Guint32'Image (Get_Id (Socket)));
--  </example>
