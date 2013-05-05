-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                 Copyright (C) 2001-2013, AdaCore                  --
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

with System;     use System;

with Gtk;        use Gtk;
with Gtk.Cell_Editable; use Gtk.Cell_Editable;
with Gtk.Widget; use Gtk.Widget;

with Glib.Type_Conversion_Hooks;

package body Gtk.Cell_Renderer is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Cell_Renderer_Record);
   pragma Warnings (Off, Type_Conversion);

   --------------
   -- Get_Size --
   --------------

   procedure Get_Size
     (Cell      : access Gtk_Cell_Renderer_Record;
      Widget    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Cell_Area : out Gdk.Rectangle.Gdk_Rectangle;
      X_Offset  : out Gint;
      Y_Offset  : out Gint;
      Width     : out Gint;
      Height    : out Gint)
   is
      procedure Internal
        (Cell      : System.Address;
         Widget    : System.Address;
         Cell_Area : out Gdk.Rectangle.Gdk_Rectangle;
         X_Offset  : out Gint;
         Y_Offset  : out Gint;
         Width     : out Gint;
         Height    : out Gint);
      pragma Import (C, Internal, "gtk_cell_renderer_get_size");

   begin
      Internal
        (Get_Object (Cell), Get_Object (Widget),
         Cell_Area, X_Offset, Y_Offset, Width, Height);
   end Get_Size;

   ------------
   -- Render --
   ------------

   procedure Render
     (Cell            : access Gtk_Cell_Renderer_Record;
      Window          : Gdk.Window.Gdk_Window;
      Widget          : access Gtk.Widget.Gtk_Widget_Record'Class;
      Background_Area : Gdk.Rectangle.Gdk_Rectangle;
      Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
      Expose_Area     : Gdk.Rectangle.Gdk_Rectangle;
      Flags           : Gtk_Cell_Renderer_State)
   is
      procedure Internal
        (Cell            : System.Address;
         Window          : Gdk.Window.Gdk_Window;
         Widget          : System.Address;
         Background_Area : Gdk.Rectangle.Gdk_Rectangle;
         Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
         Expose_Area     : Gdk.Rectangle.Gdk_Rectangle;
         Flags           : Gint);
      pragma Import (C, Internal, "gtk_cell_renderer_render");

   begin
      Internal
        (Get_Object (Cell),
         Window,
         Get_Object (Widget),
         Background_Area,
         Cell_Area,
         Expose_Area,
         Gtk_Cell_Renderer_State'Pos (Flags));
   end Render;

   --------------
   -- Activate --
   --------------

   function Activate
     (Cell            : access Gtk_Cell_Renderer_Record;
      Event           : Gdk.Event.Gdk_Event;
      Widget          : access Gtk.Widget.Gtk_Widget_Record'Class;
      Path            : UTF8_String;
      Background_Area : Gdk.Rectangle.Gdk_Rectangle;
      Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
      Flags           : Gtk_Cell_Renderer_State) return Boolean
   is
      function Internal
        (Cell            : System.Address;
         Event           : Gdk.Event.Gdk_Event;
         Widget          : System.Address;
         Path            : UTF8_String;
         Background_Area : Gdk.Rectangle.Gdk_Rectangle;
         Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
         Flags           : Gint) return Gboolean;
      pragma Import (C, Internal, "gtk_cell_renderer_activate");

   begin
      return Internal
        (Get_Object (Cell),
         Event,
         Get_Object (Widget),
         Path & ASCII.NUL,
         Background_Area,
         Cell_Area,
         Gtk_Cell_Renderer_State'Pos (Flags)) /= 0;
   end Activate;

   -------------------
   -- Start_Editing --
   -------------------

   function Start_Editing
     (Cell            : access Gtk_Cell_Renderer_Record;
      Event           : Gdk.Event.Gdk_Event;
      Widget          : access Gtk.Widget.Gtk_Widget_Record'Class;
      Path            : UTF8_String;
      Background_Area : Gdk.Rectangle.Gdk_Rectangle;
      Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
      Flags           : Gtk_Cell_Renderer_State)
      return Gtk_Cell_Editable
   is
      function Internal
        (Cell            : System.Address;
         Event           : Gdk.Event.Gdk_Event;
         Widget          : System.Address;
         Path            : UTF8_String;
         Background_Area : Gdk.Rectangle.Gdk_Rectangle;
         Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
         Flags           : Gint) return Gtk_Cell_Editable;
      pragma Import (C, Internal, "gtk_cell_renderer_start_editing");

   begin
      return Internal
        (Get_Object (Cell),
         Event,
         Get_Object (Widget),
         Path & ASCII.NUL,
         Background_Area,
         Cell_Area,
         Gtk_Cell_Renderer_State'Pos (Flags));
   end Start_Editing;

   --------------------
   -- Set_Fixed_Size --
   --------------------

   procedure Set_Fixed_Size
     (Cell   : access Gtk_Cell_Renderer_Record;
      Width  : Gint;
      Height : Gint)
   is
      procedure Internal
        (Cell   : System.Address;
         Width  : Gint;
         Height : Gint);
      pragma Import (C, Internal, "gtk_cell_renderer_set_fixed_size");
   begin
      Internal (Get_Object (Cell),
                Width,
                Height);
   end Set_Fixed_Size;

   --------------------
   -- Get_Fixed_Size --
   --------------------

   procedure Get_Fixed_Size
     (Cell   : access Gtk_Cell_Renderer_Record;
      Width  : out Gint;
      Height : out Gint)
   is
      procedure Internal
        (Cell   : System.Address;
         Width  : out Gint;
         Height : out Gint);
      pragma Import (C, Internal, "gtk_cell_renderer_get_fixed_size");
   begin
      Internal (Get_Object (Cell),
                Width,
                Height);
   end Get_Fixed_Size;

   -------------
   -- Convert --
   -------------

   function Convert (R : Gtk_Cell_Renderer) return System.Address is
   begin
      return Get_Object (R);
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert (R : System.Address) return Gtk_Cell_Renderer is
      Stub        : Gtk_Cell_Renderer_Record;
   begin
      return Gtk_Cell_Renderer
        (Glib.Object.Get_User_Data (R, Stub));
   end Convert;

end Gtk.Cell_Renderer;
