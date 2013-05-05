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
--  Together with Gtk_Socket, Gtk_Plug provides the ability to embed widgets
--  from one process into another process in a fashion that is transparent to
--  the user. One process creates a Gtk_Socket widget and, passes the XID of
--  that widgets window to the other process, which then creates a Gtk_Plug
--  window with that XID.
--  Any widgets contained in the Gtk_Plug then will appear inside the first
--  applications window.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Inter-Process communication</group>

with Gtk.Window;

package Gtk.Plug is

   type Gtk_Plug_Record is new Gtk.Window.Gtk_Window_Record with private;
   type Gtk_Plug is access all Gtk_Plug_Record'Class;

   procedure Gtk_New (Plug : out Gtk_Plug; Socket_Id : Guint32);
   --  Create a new plug widget inside the Gtk_Socket identified by socket_id.
   --  Socket_Id is the XID of the socket's window.

   procedure Initialize
     (Plug : access Gtk_Plug_Record'Class; Socket_Id : Guint32);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Id (Plug : access Gtk_Plug_Record) return Guint32;
   --  Return the low level window id associated with Plug.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Plug.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "embedded"
   --    procedure Handler (Plug : access Gtk_Plug_Record'Class);
   --    Emitted when the plug has been successfully added to a socket.
   --
   --  </signals>

   Signal_Embedded : constant Glib.Signal_Name := "embedded";

private
   type Gtk_Plug_Record is new Gtk.Window.Gtk_Window_Record with null record;
   pragma Import (C, Get_Type, "gtk_plug_get_type");
end Gtk.Plug;

--  No binding: gtk_plug_construct
--  No binding: gtk_plug_construct_for_display
--  No binding: gtk_plug_new_for_display
