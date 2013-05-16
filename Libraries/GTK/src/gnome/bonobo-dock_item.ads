-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                  Copyright (C) 2001-2002                          --
--                         ACT-Europe                                --
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

with Glib; use Glib;
with Gtk;
with Gtk.Bin;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Widget; use Gtk.Widget;

package Bonobo.Dock_Item is

   type Bonobo_Dock_Item_Record is new Gtk.Bin.Gtk_Bin_Record with private;
   type Bonobo_Dock_Item is access all Bonobo_Dock_Item_Record'Class;

   type Bonobo_Dock_Item_Behavior is mod 2 ** 32;
   Beh_Normal : constant Bonobo_Dock_Item_Behavior := 0;
   Beh_Exclusive : constant Bonobo_Dock_Item_Behavior := 2 ** 0;
   Beh_Never_Floating : constant Bonobo_Dock_Item_Behavior := 2 ** 1;
   Beh_Never_Vertical : constant Bonobo_Dock_Item_Behavior := 2 ** 2;
   Beh_Never_Horizontal : constant Bonobo_Dock_Item_Behavior := 2 ** 3;
   Beh_Locked : constant Bonobo_Dock_Item_Behavior := 2 ** 4;

   procedure Bonobo_New
     (Widget   : out Bonobo_Dock_Item;
      Name     : String;
      Behavior : Bonobo_Dock_Item_Behavior);

   procedure Initialize
     (Widget   : access Bonobo_Dock_Item_Record'Class;
      Name     : String;
      Behavior : Bonobo_Dock_Item_Behavior);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   procedure Attach
     (Item   : access Bonobo_Dock_Item_Record;
      Parent : access Gtk.Widget.Gtk_Widget_Record'Class;
      X      : Gint;
      Y      : Gint);

   function Detach
     (Item   : access Bonobo_Dock_Item_Record;
      X      : Gint;
      Y      : Gint)
      return Boolean;

   procedure Drag_Floating
     (Item : access Bonobo_Dock_Item_Record;
      X    : Gint;
      Y    : Gint);

   function Get_Child
     (Dock_Item : access Bonobo_Dock_Item_Record) return Gtk.Widget.Gtk_Widget;

   procedure Get_Floating_Position
     (Item : access Bonobo_Dock_Item_Record;
      X    : out Gint;
      Y    : out Gint);

   function Get_Name
     (Dock_Item : access Bonobo_Dock_Item_Record) return String;

   function Get_Orientation
     (Dock_Item : access Bonobo_Dock_Item_Record) return Gtk_Orientation;

   function Get_Shadow_Type
     (Dock_Item : access Bonobo_Dock_Item_Record) return Gtk_Shadow_Type;

   procedure Grab_Pointer (Item : access Bonobo_Dock_Item_Record);

   procedure Handle_Size_Request
     (Item        : access Bonobo_Dock_Item_Record;
      Requisition : Gtk_Requisition);

   function Set_Orientation
     (Dock_Item   : access Bonobo_Dock_Item_Record;
      Orientation : Gtk_Orientation) return Boolean;

   procedure Set_Shadow_Type
     (Dock_Item : access Bonobo_Dock_Item_Record;
      The_Type  : Gtk_Shadow_Type);

   function Get_Behavior
     (Dock_Item : access Bonobo_Dock_Item_Record)
      return Bonobo_Dock_Item_Behavior;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "dock_drag_begin"
   --    procedure Handler (Widget : access Bonobo_Dock_Item_Record'Class);
   --
   --  - "dock_drag_motion"
   --    procedure Handler (Widget : access Bonobo_Dock_Item_Record'Class;
   --       X : Gint;
   --       Y : Gint);
   --
   --  - "dock_drag_end"
   --    procedure Handler (Widget : access Bonobo_Dock_Item_Record'Class);
   --
   --  - "dock_detach"
   --    procedure Handler (Widget : access Bonobo_Dock_Item_Record'Class);
   --
   --  </signals>

private
   type Bonobo_Dock_Item_Record is new
     Gtk.Bin.Gtk_Bin_Record with null record;

   pragma Import (C, Get_Type, "bonobo_dock_item_get_type");
end Bonobo.Dock_Item;
