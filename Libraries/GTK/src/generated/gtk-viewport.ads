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
--  This widget is an adapter: it can contain any child, and will make it
--  scrollable. Its use is not necessary inside a Gtk_Scrolled_Window, which
--  automatically uses a Gtk_Viewport when necessary.
--
--  </description>
--  <group>Scrolling</group>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Window;      use Gdk.Window;
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Adjustment;  use Gtk.Adjustment;
with Gtk.Bin;         use Gtk.Bin;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Viewport is

   type Gtk_Viewport_Record is new Gtk_Bin_Record with null record;
   type Gtk_Viewport is access all Gtk_Viewport_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Viewport    : out Gtk_Viewport;
       Hadjustment : Gtk.Adjustment.Gtk_Adjustment := null;
       Vadjustment : Gtk.Adjustment.Gtk_Adjustment := null);
   procedure Initialize
      (Viewport    : access Gtk_Viewport_Record'Class;
       Hadjustment : Gtk.Adjustment.Gtk_Adjustment := null;
       Vadjustment : Gtk.Adjustment.Gtk_Adjustment := null);
   --  Creates a new Gtk.Viewport.Gtk_Viewport with the given adjustments.
   --  "hadjustment": horizontal adjustment.
   --  "vadjustment": vertical adjustment.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_viewport_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Bin_Window
      (Viewport : access Gtk_Viewport_Record) return Gdk.Window.Gdk_Window;
   --  Gets the bin window of the Gtk.Viewport.Gtk_Viewport.
   --  Since: gtk+ 2.20

   function Get_Hadjustment
      (Viewport : access Gtk_Viewport_Record)
       return Gtk.Adjustment.Gtk_Adjustment;
   procedure Set_Hadjustment
      (Viewport   : access Gtk_Viewport_Record;
       Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --  Sets the horizontal adjustment of the viewport.
   --  "adjustment": a Gtk.Adjustment.Gtk_Adjustment.

   function Get_Shadow_Type
      (Viewport : access Gtk_Viewport_Record)
       return Gtk.Enums.Gtk_Shadow_Type;
   procedure Set_Shadow_Type
      (Viewport : access Gtk_Viewport_Record;
       The_Type : Gtk.Enums.Gtk_Shadow_Type);
   --  Sets the shadow type of the viewport.
   --  "type": the new shadow type.

   function Get_Vadjustment
      (Viewport : access Gtk_Viewport_Record)
       return Gtk.Adjustment.Gtk_Adjustment;
   procedure Set_Vadjustment
      (Viewport   : access Gtk_Viewport_Record;
       Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --  Sets the vertical adjustment of the viewport.
   --  "adjustment": a Gtk.Adjustment.Gtk_Adjustment.

   function Get_View_Window
      (Viewport : access Gtk_Viewport_Record) return Gdk.Window.Gdk_Window;
   --  Gets the view window of the Gtk.Viewport.Gtk_Viewport.
   --  Since: gtk+ 2.22

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Viewport_Record, Gtk_Viewport);
   function "+"
     (Widget : access Gtk_Viewport_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Viewport
   renames Implements_Buildable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Hadjustment_Property
   --  Type: Gtk.Adjustment.Gtk_Adjustment
   --  Flags: read-write
   --
   --  Name: Shadow_Type_Property
   --  Type: Gtk.Enums.Gtk_Shadow_Type
   --  Flags: read-write
   --
   --  Name: Vadjustment_Property
   --  Type: Gtk.Adjustment.Gtk_Adjustment
   --  Flags: read-write

   Hadjustment_Property : constant Glib.Properties.Property_Object;
   Shadow_Type_Property : constant Gtk.Enums.Property_Gtk_Shadow_Type;
   Vadjustment_Property : constant Glib.Properties.Property_Object;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "set-scroll-adjustments"
   --     procedure Handler
   --       (Self   : access Gtk_Viewport_Record'Class;
   --        Object : Gtk.Adjustment.Gtk_Adjustment;
   --        P0     : Gtk.Adjustment.Gtk_Adjustment);
   --  Set the scroll adjustments for the viewport. Usually scrolled
   --  containers like Gtk.Scrolledwindow.Gtk_Scrolledwindow will emit this
   --  signal to connect two instances of Gtk.Scrollbar.Gtk_Scrollbar to the
   --  scroll directions of the Gtk.Viewport.Gtk_Viewport.

   Signal_Set_Scroll_Adjustments : constant Glib.Signal_Name := "set-scroll-adjustments";

private
   Hadjustment_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("hadjustment");
   Shadow_Type_Property : constant Gtk.Enums.Property_Gtk_Shadow_Type :=
     Gtk.Enums.Build ("shadow-type");
   Vadjustment_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("vadjustment");
end Gtk.Viewport;
