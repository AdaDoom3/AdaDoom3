-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                  Copyright (C) 2001-2006                          --
--                            AdaCore                                --
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

with Gtk.Widget; use Gtk.Widget;
with Gtk; use Gtk;
with System;

package body Bonobo.Dock_Band is

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New (Widget : out Bonobo_Dock_Band) is
   begin
      Widget := new Bonobo_Dock_Band_Record;
      Bonobo.Dock_Band.Initialize (Widget);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Bonobo_Dock_Band_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "bonobo_dock_band_new");
   begin
      Set_Object (Widget, Internal);
   end Initialize;

   ------------
   -- Append --
   ------------

   function Append
     (Band   : access Bonobo_Dock_Band_Record;
      Child  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Offset : Guint)
      return Boolean
   is
      function Internal
        (Band   : System.Address;
         Child  : System.Address;
         Offset : Guint)
         return Gint;
      pragma Import (C, Internal, "bonobo_dock_band_append");
   begin
      return Boolean'Val (Internal (Get_Object (Band),
                                    Get_Object (Child),
                                    Offset));
   end Append;

   ----------------
   -- Drag_Begin --
   ----------------

   procedure Drag_Begin
     (Band : access Bonobo_Dock_Band_Record;
      Item : access Bonobo.Dock_Item.Bonobo_Dock_Item_Record'Class)
   is
      procedure Internal
        (Band : System.Address;
         Item : System.Address);
      pragma Import (C, Internal, "bonobo_dock_band_drag_begin");
   begin
      Internal (Get_Object (Band),
                Get_Object (Item));
   end Drag_Begin;

   --------------
   -- Drag_End --
   --------------

   procedure Drag_End
     (Band : access Bonobo_Dock_Band_Record;
      Item : access Bonobo.Dock_Item.Bonobo_Dock_Item_Record'Class)
   is
      procedure Internal
        (Band : System.Address;
         Item : System.Address);
      pragma Import (C, Internal, "bonobo_dock_band_drag_end");
   begin
      Internal (Get_Object (Band),
                Get_Object (Item));
   end Drag_End;

   -------------
   -- Drag_To --
   -------------

   function Drag_To
     (Band   : access Bonobo_Dock_Band_Record;
      Item   : access Bonobo.Dock_Item.Bonobo_Dock_Item_Record'Class;
      X      : Gint;
      Y      : Gint)
      return Boolean
   is
      function Internal
        (Band   : System.Address;
         Item   : System.Address;
         X      : Gint;
         Y      : Gint)
         return Gint;
      pragma Import (C, Internal, "bonobo_dock_band_drag_to");
   begin
      return Boolean'Val (Internal (Get_Object (Band),
                                    Get_Object (Item),
                                    X,
                                    Y));
   end Drag_To;

   ----------------------
   -- Get_Child_Offset --
   ----------------------

   function Get_Child_Offset
     (Band   : access Bonobo_Dock_Band_Record;
      Child  : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Guint
   is
      function Internal
        (Band   : System.Address;
         Child  : System.Address)
         return Guint;
      pragma Import (C, Internal, "bonobo_dock_band_get_child_offset");
   begin
      return Internal (Get_Object (Band),
                       Get_Object (Child));
   end Get_Child_Offset;

   ----------------------
   -- Get_Item_By_Name --
   ----------------------

   procedure Get_Item_By_Name
     (Band     : access Bonobo_Dock_Band_Record;
      Name     : String;
      Position : out Guint;
      Offset   : out Guint;
      Item     : out Bonobo.Dock_Item.Bonobo_Dock_Item)
   is
      function Internal
        (Band     : System.Address;
         Name     : String;
         Position : access Guint;
         Offset   : access Guint) return System.Address;
      pragma Import (C, Internal, "bonobo_dock_band_get_item_by_name");

      Pos, Off : aliased Guint;
      Stub     : Bonobo.Dock_Item.Bonobo_Dock_Item_Record;

   begin
      Item := Bonobo.Dock_Item.Bonobo_Dock_Item (Get_User_Data (Internal
        (Get_Object (Band), Name & ASCII.NUL,
         Pos'Unchecked_Access, Off'Unchecked_Access), Stub));
      Position := Pos;
      Offset   := Off;
   end Get_Item_By_Name;

   ----------------------
   -- Get_Num_Children --
   ----------------------

   function Get_Num_Children (Band   : access Bonobo_Dock_Band_Record)
                              return Guint
   is
      function Internal (Band   : System.Address)
                         return Guint;
      pragma Import (C, Internal, "bonobo_dock_band_get_num_children");
   begin
      return Internal (Get_Object (Band));
   end Get_Num_Children;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation (Band   : access Bonobo_Dock_Band_Record)
                             return Gtk_Orientation
   is
      function Internal (Band   : System.Address)
                         return Gint;
      pragma Import (C, Internal, "bonobo_dock_band_get_orientation");
   begin
      return Gtk_Orientation'Val (Internal (Get_Object (Band)));
   end Get_Orientation;

   ------------
   -- Insert --
   ------------

   function Insert
     (Band     : access Bonobo_Dock_Band_Record;
      Child    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Offset   : Guint;
      Position : Gint)
      return Boolean
   is
      function Internal
        (Band     : System.Address;
         Child    : System.Address;
         Offset   : Guint;
         Position : Gint)
         return Gint;
      pragma Import (C, Internal, "bonobo_dock_band_insert");
   begin
      return Boolean'Val (Internal (Get_Object (Band),
                                    Get_Object (Child),
                                    Offset,
                                    Position));
   end Insert;

   ----------------
   -- Layout_Add --
   ----------------

   procedure Layout_Add
     (Band      : access Bonobo_Dock_Band_Record;
      Layout    : access Bonobo.Dock_Layout.Bonobo_Dock_Layout_Record'Class;
      Placement : Bonobo.Dock.Bonobo_Dock_Placement;
      Band_Num  : Guint)
   is
      procedure Internal
        (Band      : System.Address;
         Layout    : System.Address;
         Placement : Gint;
         Band_Num  : Guint);
      pragma Import (C, Internal, "bonobo_dock_band_layout_add");
   begin
      Internal (Get_Object (Band),
                Get_Object (Layout),
                Bonobo.Dock.Bonobo_Dock_Placement'Pos (Placement),
                Band_Num);
   end Layout_Add;

   ----------------
   -- Move_Child --
   ----------------

   --  procedure Move_Child
   --    (Band      : access Bonobo_Dock_Band_Record;
   --     Old_Child : out GList;
   --     New_Num   : Guint)
   --  is
   --     procedure Internal
   --       (Band      : System.Address;
   --        Old_Child : System.Address;
   --        New_Num   : Guint);
   --     pragma Import (C, Internal, "bonobo_dock_band_move_child");
   --  begin
   --     Internal (Get_Object (Band),
   --               Get_Object (Old_Child),
   --               New_Num);
   --  end Move_Child;

   -------------
   -- Prepend --
   -------------

   function Prepend
     (Band   : access Bonobo_Dock_Band_Record;
      Child  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Offset : Guint)
      return Boolean
   is
      function Internal
        (Band   : System.Address;
         Child  : System.Address;
         Offset : Guint)
         return Gint;
      pragma Import (C, Internal, "bonobo_dock_band_prepend");
   begin
      return Boolean'Val (Internal (Get_Object (Band),
                                    Get_Object (Child),
                                    Offset));
   end Prepend;

   ----------------------
   -- Set_Child_Offset --
   ----------------------

   procedure Set_Child_Offset
     (Band   : access Bonobo_Dock_Band_Record;
      Child  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Offset : Guint)
   is
      procedure Internal
        (Band   : System.Address;
         Child  : System.Address;
         Offset : Guint);
      pragma Import (C, Internal, "bonobo_dock_band_set_child_offset");
   begin
      Internal (Get_Object (Band),
                Get_Object (Child),
                Offset);
   end Set_Child_Offset;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
     (Band        : access Bonobo_Dock_Band_Record;
      Orientation : Gtk_Orientation)
   is
      procedure Internal
        (Band        : System.Address;
         Orientation : Gint);
      pragma Import (C, Internal, "bonobo_dock_band_set_orientation");
   begin
      Internal (Get_Object (Band),
                Gtk_Orientation'Pos (Orientation));
   end Set_Orientation;

end Bonobo.Dock_Band;
