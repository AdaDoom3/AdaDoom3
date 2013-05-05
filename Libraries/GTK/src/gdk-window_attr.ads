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

--  <group>Gdk, the low-level API</group>

with Gdk.Color;
with Gdk.Cursor;
with Gdk.Event;
with Gdk.Visual;
with Gdk.Window;
with Glib;

package Gdk.Window_Attr is

   subtype Gdk_Window_Attr is Gdk.Gdk_Window_Attr;
   Null_Window_Attr : constant Gdk_Window_Attr;
   --  This record describes the initial attributes for a window. Most of
   --  them can be changed later on, but it is more efficient to set them
   --  right from the start.
   --  You usually need to do the following when initializing such a
   --  structure:
   --      Window_Attr.Visual := Get_Visual (Window);
   --      Window_Attr.Colormap := Get_Colormap (Window);
   --      Window_Attr.Event_Mask := Get_Events (Window) or Exposure_Mask;

   procedure Gdk_New
     (Window_Attr       : out Gdk_Window_Attr;
      Title             : Glib.UTF8_String := "";
      Event_Mask        : Gdk.Event.Gdk_Event_Mask := 0;
      X, Y              : Glib.Gint := 0;
      Width             : Glib.Gint := 0;
      Height            : Glib.Gint := 0;
      Wclass            : Gdk.Window.Gdk_Window_Class :=
        Gdk.Window.Input_Output;
      Visual            : Gdk.Visual.Gdk_Visual := null;
      Colormap          : Gdk.Color.Gdk_Colormap := null;
      Window_Type       : Gdk.Window.Gdk_Window_Type :=
        Gdk.Window.Window_Root;
      Cursor            : Gdk.Cursor.Gdk_Cursor := null;
      Wmclass_Name      : String := "";
      Wmclass_Class     : String := "";
      Override_Redirect : Boolean := True);
   --  Creates a new Gdk_Window_Attr structure.
   --  It is your responsability to free the visual, colormap, cursor,... field
   --  when they are no longer needed.

   procedure Destroy (Window_Attr : in out Gdk_Window_Attr);

   procedure Set_Title
     (Window_Attr : Gdk_Window_Attr;
      Title       : Glib.UTF8_String);

   function Get_Title (Window_Attr : Gdk_Window_Attr) return Glib.UTF8_String;

   procedure Set_Event_Mask
     (Window_Attr : Gdk_Window_Attr;
      Event_Mask  : Gdk.Event.Gdk_Event_Mask);

   function Get_Event_Mask
     (Window_Attr : Gdk_Window_Attr) return Gdk.Event.Gdk_Event_Mask;

   procedure Set_X
     (Window_Attr : Gdk_Window_Attr;
      X           : Glib.Gint);

   function Get_X (Window_Attr : Gdk_Window_Attr) return Glib.Gint;

   procedure Set_Y
     (Window_Attr : Gdk_Window_Attr;
      Y           : Glib.Gint);

   function Get_Y (Window_Attr : Gdk_Window_Attr) return Glib.Gint;

   procedure Set_Width
     (Window_Attr : Gdk_Window_Attr;
      Width       : Glib.Gint);

   function Get_Width (Window_Attr : Gdk_Window_Attr) return Glib.Gint;

   procedure Set_Height
     (Window_Attr : Gdk_Window_Attr;
      Height      : Glib.Gint);

   function Get_Height (Window_Attr : Gdk_Window_Attr) return Glib.Gint;

   procedure Set_Window_Class
     (Window_Attr : Gdk_Window_Attr;
      Wclass      : Gdk.Window.Gdk_Window_Class);

   function Get_Window_Class
     (Window_Attr : Gdk_Window_Attr) return Gdk.Window.Gdk_Window_Class;

   procedure Set_Visual
     (Window_Attr : Gdk_Window_Attr;
      Visual      : Gdk.Visual.Gdk_Visual);

   function Get_Visual
     (Window_Attr : Gdk_Window_Attr) return Gdk.Visual.Gdk_Visual;

   procedure Set_Colormap
     (Window_Attr : Gdk_Window_Attr;
      Colormap    : Gdk.Color.Gdk_Colormap);

   function Get_Colormap
     (Window_Attr : Gdk_Window_Attr) return Gdk.Color.Gdk_Colormap;

   procedure Set_Window_Type
     (Window_Attr : Gdk_Window_Attr;
      Window_Type : Gdk.Window.Gdk_Window_Type);

   function Get_Window_Type
     (Window_Attr : Gdk_Window_Attr) return Gdk.Window.Gdk_Window_Type;

   procedure Set_Cursor
     (Window_Attr : Gdk_Window_Attr;
      Cursor      : Gdk.Cursor.Gdk_Cursor);

   function Get_Cursor
     (Window_Attr : Gdk_Window_Attr)
      return Gdk.Cursor.Gdk_Cursor;

   procedure Set_Wmclass_Name
     (Window_Attr  : Gdk_Window_Attr;
      Wmclass_Name : String);

   function Get_Wmclass_Name (Window_Attr : Gdk_Window_Attr) return String;

   procedure Set_Wmclass_Class
     (Window_Attr   : Gdk_Window_Attr;
      Wmclass_Class : String);

   function Get_Wmclass_Class (Window_Attr : Gdk_Window_Attr) return String;

   procedure Set_Override_Redirect
     (Window_Attr       : Gdk_Window_Attr;
      Override_Redirect : Boolean);

   function Get_Override_Redirect
     (Window_Attr : Gdk_Window_Attr) return Boolean;

private
   Null_Window_Attr : constant Gdk_Window_Attr := null;
   pragma Import (C, Get_Colormap, "ada_gdk_window_attr_get_colormap");
   pragma Import (C, Get_Cursor, "ada_gdk_window_attr_get_cursor");
   pragma Import (C, Get_Event_Mask, "ada_gdk_window_attr_get_event_mask");
   pragma Import (C, Get_Height, "ada_gdk_window_attr_get_height");
   pragma Import (C, Get_Visual, "ada_gdk_window_attr_get_visual");
   pragma Import (C, Get_Width, "ada_gdk_window_attr_get_width");
   pragma Import (C, Get_Window_Class, "ada_gdk_window_attr_get_wclass");
   pragma Import (C, Get_Window_Type, "ada_gdk_window_attr_get_window_type");
   pragma Import (C, Get_X, "ada_gdk_window_attr_get_x");
   pragma Import (C, Get_Y, "ada_gdk_window_attr_get_y");
   pragma Import (C, Set_Colormap, "ada_gdk_window_attr_set_colormap");
   pragma Import (C, Set_Cursor, "ada_gdk_window_attr_set_cursor");
   pragma Import (C, Set_Event_Mask, "ada_gdk_window_attr_set_event_mask");
   pragma Import (C, Set_Height, "ada_gdk_window_attr_set_height");
   pragma Import (C, Set_Visual, "ada_gdk_window_attr_set_visual");
   pragma Import (C, Set_Width, "ada_gdk_window_attr_set_width");
   pragma Import (C, Set_Window_Class, "ada_gdk_window_attr_set_wclass");
   pragma Import (C, Set_Window_Type, "ada_gdk_window_attr_set_window_type");
   pragma Import (C, Set_X, "ada_gdk_window_attr_set_x");
   pragma Import (C, Set_Y, "ada_gdk_window_attr_set_y");
end Gdk.Window_Attr;
