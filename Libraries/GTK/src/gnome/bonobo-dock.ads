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
with Gtk.Container;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Widget;
with Bonobo.Dock_Item; use Bonobo.Dock_Item;

package Bonobo.Dock is

   type Bonobo_Dock_Record is new
     Gtk.Container.Gtk_Container_Record with private;
   type Bonobo_Dock is access all Bonobo_Dock_Record'Class;

   type Bonobo_Dock_Placement is (
      Top,
      Right,
      Bottom,
      Left,
      Floating);

   procedure Bonobo_New (Widget : out Bonobo_Dock);

   procedure Initialize (Widget : access Bonobo_Dock_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   procedure Add_Floating_Item
     (Dock        : access Bonobo_Dock_Record;
      Widget      : access Bonobo_Dock_Item_Record;
      X           : Gint;
      Y           : Gint;
      Orientation : Gtk_Orientation);

   procedure Add_Item
     (Dock        : access Bonobo_Dock_Record;
      Item        : access Bonobo_Dock_Item_Record;
      Placement   : Bonobo_Dock_Placement;
      Band_Num    : Guint;
      Position    : Gint;
      Offset      : Guint;
      In_New_Band : Boolean);

   procedure Allow_Floating_Items
     (Dock   : access Bonobo_Dock_Record;
      Enable : Boolean);

   function Get_Client_Area (Dock   : access Bonobo_Dock_Record)
                             return Gtk.Widget.Gtk_Widget;

   procedure Get_Item_By_Name
     (Dock                 : access Bonobo_Dock_Record;
      Name                 : String;
      Placement            : out Bonobo_Dock_Placement;
      Num_Band             : out Guint;
      Band_Position        : out Guint;
      Offset               : out Guint;
      Dock_Item            : out Bonobo_Dock_Item);

   procedure Set_Client_Area
     (Dock   : access Bonobo_Dock_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "layout_changed"
   --    procedure Handler (Widget : access Bonobo_Dock_Record'Class);
   --
   --  </signals>

private
   type Bonobo_Dock_Record is new
     Gtk.Container.Gtk_Container_Record with null record;

   pragma Import (C, Get_Type, "bonobo_dock_get_type");
end Bonobo.Dock;
