-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                  Copyright (C) 2001-2006                          --
--                         AdaCore                                   --
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

with System;

with Gtk.Widget; use Gtk.Widget;

package body Bonobo.Dock is

   ---------------
   -- Bonobo_New --
   ---------------

   procedure Bonobo_New (Widget : out Bonobo_Dock) is
   begin
      Widget := new Bonobo_Dock_Record;
      Bonobo.Dock.Initialize (Widget);
   end Bonobo_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Bonobo_Dock_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "bonobo_dock_new");
   begin
      Set_Object (Widget, Internal);
   end Initialize;

   -----------------------
   -- Add_Floating_Item --
   -----------------------

   procedure Add_Floating_Item
     (Dock        : access Bonobo_Dock_Record;
      Widget      : access Bonobo_Dock_Item_Record;
      X           : Gint;
      Y           : Gint;
      Orientation : Gtk_Orientation)
   is
      procedure Internal
        (Dock        : System.Address;
         Widget      : System.Address;
         X           : Gint;
         Y           : Gint;
         Orientation : Gint);
      pragma Import (C, Internal, "bonobo_dock_add_floating_item");
   begin
      Internal (Get_Object (Dock),
                Get_Object (Widget),
                X,
                Y,
                Gtk_Orientation'Pos (Orientation));
   end Add_Floating_Item;

   --------------
   -- Add_Item --
   --------------

   procedure Add_Item
     (Dock        : access Bonobo_Dock_Record;
      Item        : access Bonobo_Dock_Item_Record;
      Placement   : Bonobo_Dock_Placement;
      Band_Num    : Guint;
      Position    : Gint;
      Offset      : Guint;
      In_New_Band : Boolean)
   is
      procedure Internal
        (Dock        : System.Address;
         Item        : System.Address;
         Placement   : Gint;
         Band_Num    : Guint;
         Position    : Gint;
         Offset      : Guint;
         In_New_Band : Gint);
      pragma Import (C, Internal, "bonobo_dock_add_item");
   begin
      Internal (Get_Object (Dock),
                Get_Object (Item),
                Bonobo_Dock_Placement'Pos (Placement),
                Band_Num,
                Position,
                Offset,
                Boolean'Pos (In_New_Band));
   end Add_Item;

   --------------------------
   -- Allow_Floating_Items --
   --------------------------

   procedure Allow_Floating_Items
     (Dock   : access Bonobo_Dock_Record;
      Enable : Boolean)
   is
      procedure Internal
        (Dock   : System.Address;
         Enable : Gint);
      pragma Import (C, Internal, "bonobo_dock_allow_floating_items");
   begin
      Internal (Get_Object (Dock),
                Boolean'Pos (Enable));
   end Allow_Floating_Items;

   ---------------------
   -- Get_Client_Area --
   ---------------------

   function Get_Client_Area (Dock   : access Bonobo_Dock_Record)
                             return Gtk.Widget.Gtk_Widget
   is
      function Internal (Dock   : System.Address)
                         return System.Address;
      pragma Import (C, Internal, "bonobo_dock_get_client_area");
   begin
      return Convert (Internal (Get_Object (Dock)));
   end Get_Client_Area;

   ----------------------
   -- Get_Item_By_Name --
   ----------------------

   procedure Get_Item_By_Name
     (Dock                 : access Bonobo_Dock_Record;
      Name                 : String;
      Placement            : out Bonobo_Dock_Placement;
      Num_Band             : out Guint;
      Band_Position        : out Guint;
      Offset               : out Guint;
      Dock_Item            : out Bonobo_Dock_Item)
   is
      function Internal
        (Dock                 : System.Address;
         Name                 : String;
         Placement_Return     : access Bonobo_Dock_Placement;
         Num_Band_Return      : access Guint;
         Band_Position_Return : access Guint;
         Offset_Return        : access Guint)
         return System.Address;
      pragma Import (C, Internal, "bonobo_dock_get_item_by_name");

      Local_Placement     : aliased Bonobo_Dock_Placement;
      Local_Num_Band      : aliased Guint;
      Local_Band_Position : aliased Guint;
      Local_Offset        : aliased Guint;
      Stub                : Bonobo_Dock_Item_Record;

   begin
      Dock_Item := Bonobo_Dock_Item (Get_User_Data (Internal
        (Get_Object (Dock),
         Name & ASCII.NUL,
         Local_Placement'Unchecked_Access,
         Local_Num_Band'Unchecked_Access,
         Local_Band_Position'Unchecked_Access,
         Local_Offset'Unchecked_Access), Stub));
      Placement := Local_Placement;
      Num_Band  := Local_Num_Band;
      Band_Position := Local_Band_Position;
      Offset    := Local_Offset;
   end Get_Item_By_Name;

   ---------------------
   -- Set_Client_Area --
   ---------------------

   procedure Set_Client_Area
     (Dock   : access Bonobo_Dock_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
        (Dock   : System.Address;
         Widget : System.Address);
      pragma Import (C, Internal, "bonobo_dock_set_client_area");
   begin
      Internal (Get_Object (Dock),
                Get_Object (Widget));
   end Set_Client_Area;

end Bonobo.Dock;
