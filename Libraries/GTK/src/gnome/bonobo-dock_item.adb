-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                  Copyright (C) 2001-2006                          --
--                           AdaCore                                 --
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

with Interfaces.C.Strings;
with System;

with Gtk;       use Gtk;

package body Bonobo.Dock_Item is

   ----------------
   -- Bonobo_New --
   ----------------

   procedure Bonobo_New
     (Widget   : out Bonobo_Dock_Item;
      Name     : String;
      Behavior : Bonobo_Dock_Item_Behavior)
   is
   begin
      Widget := new Bonobo_Dock_Item_Record;
      Initialize (Widget, Name, Behavior);
   end Bonobo_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget   : access Bonobo_Dock_Item_Record'Class;
      Name     : String;
      Behavior : Bonobo_Dock_Item_Behavior)
   is
      function Internal
        (Name     : String;
         Behavior : Gint)
         return System.Address;
      pragma Import (C, Internal, "bonobo_dock_item_new");
   begin
      Set_Object (Widget, Internal (Name & ASCII.NUL,
                                    Bonobo_Dock_Item_Behavior'Pos (Behavior)));
   end Initialize;

   ------------
   -- Attach --
   ------------

   procedure Attach
     (Item   : access Bonobo_Dock_Item_Record;
      Parent : access Gtk.Widget.Gtk_Widget_Record'Class;
      X      : Gint;
      Y      : Gint)
   is
      procedure Internal
        (Item   : System.Address;
         Parent : System.Address;
         X      : Gint;
         Y      : Gint);
      pragma Import (C, Internal, "bonobo_dock_item_attach");
   begin
      Internal (Get_Object (Item),
                Get_Object (Parent),
                X,
                Y);
   end Attach;

   ------------
   -- Detach --
   ------------

   function Detach
     (Item   : access Bonobo_Dock_Item_Record;
      X      : Gint;
      Y      : Gint)
      return Boolean
   is
      function Internal
        (Item   : System.Address;
         X      : Gint;
         Y      : Gint)
         return Gint;
      pragma Import (C, Internal, "bonobo_dock_item_detach");
   begin
      return Boolean'Val (Internal (Get_Object (Item), X, Y));
   end Detach;

   -------------------
   -- Drag_Floating --
   -------------------

   procedure Drag_Floating
     (Item : access Bonobo_Dock_Item_Record;
      X    : Gint;
      Y    : Gint)
   is
      procedure Internal
        (Item : System.Address;
         X    : Gint;
         Y    : Gint);
      pragma Import (C, Internal, "bonobo_dock_item_drag_floating");
   begin
      Internal (Get_Object (Item),
                X,
                Y);
   end Drag_Floating;

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child (Dock_Item : access Bonobo_Dock_Item_Record)
                       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Dock_Item : System.Address)
                         return System.Address;
      pragma Import (C, Internal, "bonobo_dock_item_get_child");
   begin
      return Widget.Convert (Internal (Get_Object (Dock_Item)));
   end Get_Child;

   ---------------------------
   -- Get_Floating_Position --
   ---------------------------

   procedure Get_Floating_Position
     (Item : access Bonobo_Dock_Item_Record;
      X    : out Gint;
      Y    : out Gint)
   is
      procedure Internal
        (Item : System.Address;
         X    : out Gint;
         Y    : out Gint);
      pragma Import (C, Internal, "bonobo_dock_item_get_floating_position");
   begin
      Internal (Get_Object (Item), X, Y);
   end Get_Floating_Position;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Dock_Item : access Bonobo_Dock_Item_Record)
                      return String
   is
      function Internal (Dock_Item : System.Address)
                         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "bonobo_dock_item_get_name");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Dock_Item)));
   end Get_Name;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation (Dock_Item : access Bonobo_Dock_Item_Record)
                             return Gtk_Orientation
   is
      function Internal (Dock_Item : System.Address)
                         return Gint;
      pragma Import (C, Internal, "bonobo_dock_item_get_orientation");
   begin
      return Gtk_Orientation'Val (Internal (Get_Object (Dock_Item)));
   end Get_Orientation;

   ---------------------
   -- Get_Shadow_Type --
   ---------------------

   function Get_Shadow_Type (Dock_Item : access Bonobo_Dock_Item_Record)
                             return Gtk_Shadow_Type
   is
      function Internal (Dock_Item : System.Address)
                         return Gint;
      pragma Import (C, Internal, "bonobo_dock_item_get_shadow_type");
   begin
      return Gtk_Shadow_Type'Val (Internal (Get_Object (Dock_Item)));
   end Get_Shadow_Type;

   ------------------
   -- Grab_Pointer --
   ------------------

   procedure Grab_Pointer (Item : access Bonobo_Dock_Item_Record)
   is
      procedure Internal (Item : System.Address);
      pragma Import (C, Internal, "bonobo_dock_item_grab_pointer");
   begin
      Internal (Get_Object (Item));
   end Grab_Pointer;

   -------------------------
   -- Handle_Size_Request --
   -------------------------

   procedure Handle_Size_Request
     (Item        : access Bonobo_Dock_Item_Record;
      Requisition : Gtk_Requisition)
   is
      procedure Internal
        (Item        : System.Address;
         Requisition : Gtk_Requisition);
      pragma Import (C, Internal, "bonobo_dock_item_handle_size_request");
   begin
      Internal (Get_Object (Item), Requisition);
   end Handle_Size_Request;

   ---------------------
   -- Set_Orientation --
   ---------------------

   function Set_Orientation
     (Dock_Item   : access Bonobo_Dock_Item_Record;
      Orientation : Gtk_Orientation)
      return Boolean
   is
      function Internal
        (Dock_Item   : System.Address;
         Orientation : Gint) return Gint;
      pragma Import (C, Internal, "bonobo_dock_item_set_orientation");
   begin
      return Boolean'Val
        (Internal (Get_Object (Dock_Item), Gtk_Orientation'Pos (Orientation)));
   end Set_Orientation;

   ---------------------
   -- Set_Shadow_Type --
   ---------------------

   procedure Set_Shadow_Type
     (Dock_Item : access Bonobo_Dock_Item_Record;
      The_Type  : Gtk_Shadow_Type)
   is
      procedure Internal
        (Dock_Item : System.Address;
         The_Type  : Gint);
      pragma Import (C, Internal, "bonobo_dock_item_set_shadow_type");
   begin
      Internal
        (Get_Object (Dock_Item), Gtk_Shadow_Type'Pos (The_Type));
   end Set_Shadow_Type;

   ------------------
   -- Get_Behavior --
   ------------------

   function Get_Behavior
     (Dock_Item : access Bonobo_Dock_Item_Record)
      return Bonobo_Dock_Item_Behavior
   is
      function Internal
        (Dock_Item : System.Address) return Bonobo_Dock_Item_Behavior;
      pragma Import (C, Internal, "bonobo_dock_item_get_behavior");

   begin
      return Internal (Get_Object (Dock_Item));
   end Get_Behavior;

end Bonobo.Dock_Item;
