-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
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

--  This package provides low level handling of file descriptors in the
--  GtkAda event loop. Note that this package is currently not supported
--  under Windows.
--  <group>Gdk, the low-level API</group>

with Gdk.Event;
with Gdk.Types;
with Gdk.Window;
with Glib;

package Gdk.Input is

   procedure Init;

   procedure Gdk_Exit;

   procedure Set_Extension_Events
     (Window : in Gdk.Window.Gdk_Window;
      Mask   : in Gdk.Event.Gdk_Event_Mask;
      Mode   : in Gdk.Types.Gdk_Extension_Mode);
   --  ??? Check that Mask is indeed of type GdkEventMask.
   --  The C code defines it as Gint...

   generic
      type Data_Type (<>) is private;
   package Input_Add is

      type Data_Access is access all Data_Type;
      pragma Convention (C, Data_Access);

      type Gdk_Input_Function is access procedure
        (Data      : Data_Access;
         Source    : Glib.Gint;
         Condition : Gdk.Types.Gdk_Input_Condition);

      function Add
        (Source    : Glib.Gint;
         Condition : Gdk.Types.Gdk_Input_Condition;
         Func      : Gdk_Input_Function;
         Data      : Data_Access) return Glib.Gint;

   private
      pragma Import (C, Add, "gdk_input_add");
   end Input_Add;

   procedure Remove (Tag : Glib.Gint);

   --  to bind: gdk_input_motion_events
   --    This returns the list of past motion events in the window, between two
   --    times. This might give a finer granularity than getting the
   --    Motion_Events themselves, and might be more efficient.
   --    Not supported by all XServers though.

private
   pragma Import (C, Init, "gdk_input_init");
   pragma Import (C, Gdk_Exit, "gdk_input_exit");
   pragma Import (C, Set_Extension_Events, "gdk_input_set_extension_events");
   pragma Import (C, Remove, "gdk_input_remove");
end Gdk.Input;
