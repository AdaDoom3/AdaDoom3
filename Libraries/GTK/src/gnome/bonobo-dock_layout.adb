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

with Bonobo.Dock;      use Bonobo.Dock;

with Gtk;              use Gtk;

package body Bonobo.Dock_Layout is

   ---------------
   -- Gnome_New --
   ---------------

   procedure Bonobo_New (Widget : out Bonobo_Dock_Layout) is
   begin
      Widget := new Bonobo_Dock_Layout_Record;
      Bonobo.Dock_Layout.Initialize (Widget);
   end Bonobo_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Bonobo_Dock_Layout_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "bonobo_dock_layout_new");
   begin
      Set_Object (Widget, Internal);
   end Initialize;

   -----------------------
   -- Add_Floating_Item --
   -----------------------

   function Add_Floating_Item
     (Layout      : access Bonobo_Dock_Layout_Record;
      Item        : access Bonobo.Dock_Item.Bonobo_Dock_Item_Record'Class;
      X           : Gint;
      Y           : Gint;
      Orientation : Gtk_Orientation)
      return Boolean
   is
      function Internal
        (Layout      : System.Address;
         Item        : System.Address;
         X           : Gint;
         Y           : Gint;
         Orientation : Gint) return Gint;
      pragma Import (C, Internal, "bonobo_dock_layout_add_floating_item");
   begin
      return Boolean'Val
        (Internal (Get_Object (Layout), Get_Object (Item),
          X, Y, Gtk_Orientation'Pos (Orientation)));
   end Add_Floating_Item;

   ---------------------
   -- Add_From_Layout --
   ---------------------

   function Add_From_Layout
     (Dock   : access Bonobo_Dock_Record'Class;
      Layout : access Bonobo_Dock_Layout_Record) return Boolean
   is
      function Internal
        (Dock   : System.Address;
         Layout : System.Address) return Gint;
      pragma Import (C, Internal, "bonobo_dock_add_from_layout");
   begin
      return Boolean'Val (Internal (Get_Object (Dock), Get_Object (Layout)));
   end Add_From_Layout;

   --------------
   -- Add_Item --
   --------------

   function Add_Item
     (Layout        : access Bonobo_Dock_Layout_Record;
      Item          : access Bonobo.Dock_Item.Bonobo_Dock_Item_Record'Class;
      Placement     : Bonobo_Dock_Placement;
      Band_Num      : Gint;
      Band_Position : Gint;
      Offset        : Gint)
      return Boolean
   is
      function Internal
        (Layout        : System.Address;
         Item          : System.Address;
         Placement     : Gint;
         Band_Num      : Gint;
         Band_Position : Gint;
         Offset        : Gint)
         return Gint;
      pragma Import (C, Internal, "bonobo_dock_layout_add_item");
   begin
      return Boolean'Val (Internal
        (Get_Object (Layout),
         Get_Object (Item),
         Bonobo_Dock_Placement'Pos (Placement),
         Band_Num,
         Band_Position,
         Offset));
   end Add_Item;

   -----------------
   -- Add_To_Dock --
   -----------------

   function Add_To_Dock
     (Layout : access Bonobo_Dock_Layout_Record;
      Dock   : access Bonobo.Dock.Bonobo_Dock_Record'Class) return Boolean
   is
      function Internal
        (Layout : System.Address;
         Dock   : System.Address)
         return Gint;
      pragma Import (C, Internal, "bonobo_dock_layout_add_to_dock");
   begin
      return Boolean'Val (Internal (Get_Object (Layout), Get_Object (Dock)));
   end Add_To_Dock;

   -------------------
   -- Create_String --
   -------------------

   function Create_String (Layout : access Bonobo_Dock_Layout_Record)
                           return String
   is
      function Internal (Layout : System.Address)
                         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "bonobo_dock_layout_create_string");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Layout)));
   end Create_String;

   --------------
   -- Get_Item --
   --------------

   --  function Get_Item
   --    (Layout : access Bonobo_Dock_Layout_Record;
   --     Item   : access Bonobo.Dock_Item.Bonobo_Dock_Item_Record'Class)
   --     return Bonobo_Dock_Layout_Item
   --  is
   --     function Internal
   --       (Layout : System.Address;
   --        Item   : System.Address)
   --        return System.Address;
   --     pragma Import (C, Internal, "bonobo_dock_layout_get_item");
   --  begin
   --     return Bonobo_Dock_Layout_Item (Convert (Internal
   --       (Get_Object (Layout), Get_Object (Item))));
   --  end Get_Item;

   ----------------------
   -- Get_Item_By_Name --
   ----------------------

   --  function Get_Item_By_Name
   --    (Layout : access Bonobo_Dock_Layout_Record;
   --     Name   : String)
   --     return Bonobo_Dock_Layout_Item
   --  is
   --     function Internal
   --       (Layout : System.Address;
   --        Name   : String)
   --        return System.Address;
   --     pragma Import (C, Internal, "bonobo_dock_layout_get_item_by_name");
   --  begin
   --     return Bonobo_Dock_Layout_Item (Convert (Internal
   --       (Get_Object (Layout), Name & ASCII.NUL)));
   --  end Get_Item_By_Name;

   ----------------
   -- Get_Layout --
   ----------------

   function Get_Layout
     (Dock : access Bonobo_Dock_Record'Class) return Bonobo_Dock_Layout
   is
      function Internal (Dock : System.Address) return System.Address;
      pragma Import (C, Internal, "bonobo_dock_get_layout");
      Stub : Bonobo_Dock_Layout_Record;
   begin
      return Bonobo_Dock_Layout
        (Get_User_Data (Internal (Get_Object (Dock)), Stub));
   end Get_Layout;

   ------------------
   -- Parse_String --
   ------------------

   function Parse_String
     (Layout : access Bonobo_Dock_Layout_Record;
      Str    : String) return Boolean
   is
      function Internal
        (Layout : System.Address;
         Str    : String) return Gint;
      pragma Import (C, Internal, "bonobo_dock_layout_parse_string");
   begin
      return Boolean'Val (Internal (Get_Object (Layout), Str & ASCII.NUL));
   end Parse_String;

   -----------------
   -- Remove_Item --
   -----------------

   function Remove_Item
     (Layout : access Bonobo_Dock_Layout_Record;
      Item   : access Bonobo.Dock_Item.Bonobo_Dock_Item_Record'Class)
      return Boolean
   is
      function Internal
        (Layout : System.Address;
         Item   : System.Address) return Gint;
      pragma Import (C, Internal, "bonobo_dock_layout_remove_item");
   begin
      return Boolean'Val (Internal (Get_Object (Layout), Get_Object (Item)));
   end Remove_Item;

   -------------------------
   -- Remove_Item_By_Name --
   -------------------------

   function Remove_Item_By_Name
     (Layout : access Bonobo_Dock_Layout_Record;
      Name   : String)
      return Boolean
   is
      function Internal
        (Layout : System.Address;
         Name   : String) return Gint;
      pragma Import (C, Internal, "bonobo_dock_layout_remove_item_by_name");
   begin
      return Boolean'Val (Internal (Get_Object (Layout), Name & ASCII.NUL));
   end Remove_Item_By_Name;

end Bonobo.Dock_Layout;
