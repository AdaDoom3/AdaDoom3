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
with Bonobo.Dock;
with Bonobo.Dock_Item;
with Bonobo.Dock_Layout;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Container;
with Gtk.Widget;

package Bonobo.Dock_Band is

   type Bonobo_Dock_Band_Record is new
     Gtk.Container.Gtk_Container_Record with private;
   type Bonobo_Dock_Band is access all Bonobo_Dock_Band_Record'Class;

   procedure Gnome_New (Widget : out Bonobo_Dock_Band);

   procedure Initialize (Widget : access Bonobo_Dock_Band_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   function Append
     (Band   : access Bonobo_Dock_Band_Record;
      Child  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Offset : Guint)
      return Boolean;

   procedure Drag_Begin
     (Band : access Bonobo_Dock_Band_Record;
      Item : access Bonobo.Dock_Item.Bonobo_Dock_Item_Record'Class);

   procedure Drag_End
     (Band : access Bonobo_Dock_Band_Record;
      Item : access Bonobo.Dock_Item.Bonobo_Dock_Item_Record'Class);

   function Drag_To
     (Band   : access Bonobo_Dock_Band_Record;
      Item   : access Bonobo.Dock_Item.Bonobo_Dock_Item_Record'Class;
      X      : Gint;
      Y      : Gint)
      return Boolean;

   function Get_Child_Offset
     (Band   : access Bonobo_Dock_Band_Record;
      Child  : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Guint;

   procedure Get_Item_By_Name
     (Band     : access Bonobo_Dock_Band_Record;
      Name     : String;
      Position : out Guint;
      Offset   : out Guint;
      Item     : out Bonobo.Dock_Item.Bonobo_Dock_Item);

   function Get_Num_Children (Band   : access Bonobo_Dock_Band_Record)
                              return Guint;

   function Get_Orientation (Band   : access Bonobo_Dock_Band_Record)
                             return Gtk_Orientation;

   function Insert
     (Band     : access Bonobo_Dock_Band_Record;
      Child    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Offset   : Guint;
      Position : Gint)
      return Boolean;

   procedure Layout_Add
     (Band      : access Bonobo_Dock_Band_Record;
      Layout    : access Bonobo.Dock_Layout.Bonobo_Dock_Layout_Record'Class;
      Placement : Bonobo.Dock.Bonobo_Dock_Placement;
      Band_Num  : Guint);

   --  procedure Move_Child
   --    (Band      : access Bonobo_Dock_Band_Record;
   --     Old_Child : out GList;
   --     New_Num   : Guint);

   function Prepend
     (Band   : access Bonobo_Dock_Band_Record;
      Child  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Offset : Guint)
      return Boolean;

   procedure Set_Child_Offset
     (Band   : access Bonobo_Dock_Band_Record;
      Child  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Offset : Guint);

   procedure Set_Orientation
     (Band        : access Bonobo_Dock_Band_Record;
      Orientation : Gtk_Orientation);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  </signals>

private
   type Bonobo_Dock_Band_Record is new
     Gtk.Container.Gtk_Container_Record with null record;

   pragma Import (C, Get_Type, "gnome_dock_band_get_type");
end Bonobo.Dock_Band;
