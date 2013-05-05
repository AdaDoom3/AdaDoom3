-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2003 ACT-Europe                 --
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
--  This package provides simple primitives to write multi-threaded
--  applications with GtkAda. See the GtkAda User's Guide for more details
--  (section Tasking with GtkAda).
--
--  </description>
--  <c_version>1.3.6</c_version>
--  <group>Gdk, the low-level API</group>

with System;

package Gdk.Threads is

   procedure G_Init (Vtable : System.Address := System.Null_Address);
   --  Initialize the Glib internal threading support.
   --  This procedure must be called before any call to Enter or Leave.
   --  The parameter Vtable should never be used for now.

   procedure Init;
   --  Initialize the Gdk internal threading support.
   --  This function must be called after G_Init and before any call to
   --  Enter or Leave.

   procedure Enter;
   --  Take the GtkAda global lock.
   --  See the GtkAda User's Guide for more details (section Tasking with
   --  GtkAda).

   procedure Leave;
   --  Release the GtkAda global lock.
   --  See the GtkAda User's Guide for more details (section Tasking with
   --  GtkAda).

private
   pragma Linker_Options ("-lgthread-2.0");
   --  This is needed to resolve g_thread_init

   pragma Import (C, G_Init, "g_thread_init");
   pragma Import (C, Init, "gdk_threads_init");
   pragma Import (C, Enter, "gdk_threads_enter");
   pragma Import (C, Leave, "gdk_threads_leave");
end Gdk.Threads;
