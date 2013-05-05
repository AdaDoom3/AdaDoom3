-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                 Copyright (C) 2006-2013, AdaCore                  --
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

with Gtk.Enums;        use Gtk.Enums;
with Gtk.Menu_Item;    use Gtk.Menu_Item;
with Gtk.Tooltips;     use Gtk.Tooltips;

with Glib.Type_Conversion_Hooks;

package body Gtk.Tool_Item is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Tool_Item_Record);
   pragma Warnings (Off, Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Item : out Gtk_Tool_Item) is
   begin
      Item := new Gtk_Tool_Item_Record;
      Gtk.Tool_Item.Initialize (Item);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Item : access Gtk_Tool_Item_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_tool_item_new");
   begin
      Set_Object (Item, Internal);
   end Initialize;

   ----------------
   -- Get_Expand --
   ----------------

   function Get_Expand
     (Tool_Item : access Gtk_Tool_Item_Record)
      return Boolean
   is
      function Internal
        (Tool_Item : System.Address)
         return Gboolean;
      pragma Import (C, Internal, "gtk_tool_item_get_expand");
   begin
      return Boolean'Val (Internal (Get_Object (Tool_Item)));
   end Get_Expand;

   ---------------------
   -- Get_Homogeneous --
   ---------------------

   function Get_Homogeneous
     (Tool_Item : access Gtk_Tool_Item_Record)
      return Boolean
   is
      function Internal
        (Tool_Item : System.Address)
         return Gboolean;
      pragma Import (C, Internal, "gtk_tool_item_get_homogeneous");
   begin
      return Boolean'Val (Internal (Get_Object (Tool_Item)));
   end Get_Homogeneous;

   -------------------
   -- Get_Icon_Size --
   -------------------

   function Get_Icon_Size
     (Tool_Item : access Gtk_Tool_Item_Record)
      return Gtk_Icon_Size
   is
      function Internal
        (Tool_Item : System.Address)
         return Gtk_Icon_Size;
      pragma Import (C, Internal, "gtk_tool_item_get_icon_size");
   begin
      return Internal (Get_Object (Tool_Item));
   end Get_Icon_Size;

   ----------------------
   -- Get_Is_Important --
   ----------------------

   function Get_Is_Important
     (Tool_Item : access Gtk_Tool_Item_Record)
      return Boolean
   is
      function Internal
        (Tool_Item : System.Address)
         return Gboolean;
      pragma Import (C, Internal, "gtk_tool_item_get_is_important");
   begin
      return Boolean'Val (Internal (Get_Object (Tool_Item)));
   end Get_Is_Important;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
     (Tool_Item : access Gtk_Tool_Item_Record)
      return Gtk_Orientation
   is
      function Internal
        (Tool_Item : System.Address)
         return Gtk_Orientation;
      pragma Import (C, Internal, "gtk_tool_item_get_orientation");
   begin
      return Internal (Get_Object (Tool_Item));
   end Get_Orientation;

   -------------------------
   -- Get_Proxy_Menu_Item --
   -------------------------

   function Get_Proxy_Menu_Item
     (Tool_Item    : access Gtk_Tool_Item_Record;
      Menu_Item_Id : String)
      return Gtk_Menu_Item
   is
      function Internal
        (Tool_Item    : System.Address;
         Menu_Item_Id : String)
         return System.Address;
      pragma Import (C, Internal, "gtk_tool_item_get_proxy_menu_item");
      Stub : Gtk_Menu_Item_Record;
   begin
      return Gtk_Menu_Item
        (Get_User_Data
          (Internal (Get_Object (Tool_Item), Menu_Item_Id & ASCII.NUL), Stub));
   end Get_Proxy_Menu_Item;

   ----------------------
   -- Get_Relief_Style --
   ----------------------

   function Get_Relief_Style
     (Tool_Item : access Gtk_Tool_Item_Record)
      return Gtk_Relief_Style
   is
      function Internal
        (Tool_Item : System.Address)
         return Gtk_Relief_Style;
      pragma Import (C, Internal, "gtk_tool_item_get_relief_style");
   begin
      return Internal (Get_Object (Tool_Item));
   end Get_Relief_Style;

   -----------------------
   -- Get_Toolbar_Style --
   -----------------------

   function Get_Toolbar_Style
     (Tool_Item : access Gtk_Tool_Item_Record)
      return Gtk_Toolbar_Style
   is
      function Internal
        (Tool_Item : System.Address)
         return Gtk_Toolbar_Style;
      pragma Import (C, Internal, "gtk_tool_item_get_toolbar_style");
   begin
      return Internal (Get_Object (Tool_Item));
   end Get_Toolbar_Style;

   -------------------------
   -- Get_Use_Drag_Window --
   -------------------------

   function Get_Use_Drag_Window
     (Toolitem : access Gtk_Tool_Item_Record)
      return Boolean
   is
      function Internal
        (Toolitem : System.Address)
         return Gboolean;
      pragma Import (C, Internal, "gtk_tool_item_get_use_drag_window");
   begin
      return Boolean'Val (Internal (Get_Object (Toolitem)));
   end Get_Use_Drag_Window;

   ----------------------------
   -- Get_Visible_Horizontal --
   ----------------------------

   function Get_Visible_Horizontal
     (Toolitem : access Gtk_Tool_Item_Record)
      return Boolean
   is
      function Internal
        (Toolitem : System.Address)
         return Gboolean;
      pragma Import (C, Internal, "gtk_tool_item_get_visible_horizontal");
   begin
      return Boolean'Val (Internal (Get_Object (Toolitem)));
   end Get_Visible_Horizontal;

   --------------------------
   -- Get_Visible_Vertical --
   --------------------------

   function Get_Visible_Vertical
     (Toolitem : access Gtk_Tool_Item_Record)
      return Boolean
   is
      function Internal
        (Toolitem : System.Address)
         return Gboolean;
      pragma Import (C, Internal, "gtk_tool_item_get_visible_vertical");
   begin
      return Boolean'Val (Internal (Get_Object (Toolitem)));
   end Get_Visible_Vertical;

   ------------------
   -- Rebuild_Menu --
   ------------------

   procedure Rebuild_Menu
     (Tool_Item : access Gtk_Tool_Item_Record)
   is
      procedure Internal
        (Tool_Item : System.Address);
      pragma Import (C, Internal, "gtk_tool_item_rebuild_menu");
   begin
      Internal (Get_Object (Tool_Item));
   end Rebuild_Menu;

   ------------------------------
   -- Retrieve_Proxy_Menu_Item --
   ------------------------------

   function Retrieve_Proxy_Menu_Item
     (Tool_Item : access Gtk_Tool_Item_Record)
      return Gtk_Menu_Item
   is
      function Internal
        (Tool_Item : System.Address)
         return System.Address;
      pragma Import (C, Internal, "gtk_tool_item_retrieve_proxy_menu_item");
      Stub : Gtk_Menu_Item_Record;
   begin
      return Gtk_Menu_Item
        (Get_User_Data
          (Internal (Get_Object (Tool_Item)), Stub));
   end Retrieve_Proxy_Menu_Item;

   ----------------
   -- Set_Expand --
   ----------------

   procedure Set_Expand
     (Tool_Item : access Gtk_Tool_Item_Record;
      Expand    : Boolean)
   is
      procedure Internal
        (Tool_Item : System.Address;
         Expand    : Gboolean);
      pragma Import (C, Internal, "gtk_tool_item_set_expand");
   begin
      Internal (Get_Object (Tool_Item), Boolean'Pos (Expand));
   end Set_Expand;

   ---------------------
   -- Set_Homogeneous --
   ---------------------

   procedure Set_Homogeneous
     (Tool_Item   : access Gtk_Tool_Item_Record;
      Homogeneous : Boolean)
   is
      procedure Internal
        (Tool_Item   : System.Address;
         Homogeneous : Gboolean);
      pragma Import (C, Internal, "gtk_tool_item_set_homogeneous");
   begin
      Internal (Get_Object (Tool_Item), Boolean'Pos (Homogeneous));
   end Set_Homogeneous;

   ----------------------
   -- Set_Is_Important --
   ----------------------

   procedure Set_Is_Important
     (Tool_Item    : access Gtk_Tool_Item_Record;
      Is_Important : Boolean)
   is
      procedure Internal
        (Tool_Item    : System.Address;
         Is_Important : Gboolean);
      pragma Import (C, Internal, "gtk_tool_item_set_is_important");
   begin
      Internal (Get_Object (Tool_Item), Boolean'Pos (Is_Important));
   end Set_Is_Important;

   -------------------------
   -- Set_Proxy_Menu_Item --
   -------------------------

   procedure Set_Proxy_Menu_Item
     (Tool_Item    : access Gtk_Tool_Item_Record;
      Menu_Item_Id : String;
      Menu_Item    : Gtk_Menu_Item)
   is
      procedure Internal
        (Tool_Item    : System.Address;
         Menu_Item_Id : String;
         Menu_Item    : System.Address);
      pragma Import (C, Internal, "gtk_tool_item_set_proxy_menu_item");
   begin
      Internal (Get_Object (Tool_Item), Menu_Item_Id & ASCII.NUL,
                Get_Object (Menu_Item));
   end Set_Proxy_Menu_Item;

   -----------------
   -- Set_Tooltip --
   -----------------

   procedure Set_Tooltip
     (Tool_Item   : access Gtk_Tool_Item_Record;
      Tooltips    : access Gtk_Tooltips_Record'Class;
      Tip_Text    : String;
      Tip_Private : String := "")
   is
      procedure Internal
        (Tool_Item   : System.Address;
         Tooltips    : System.Address;
         Tip_Text    : String;
         Tip_Private : String);
      pragma Import (C, Internal, "gtk_tool_item_set_tooltip");
   begin
      Internal (Get_Object (Tool_Item), Get_Object (Tooltips),
                Tip_Text & ASCII.NUL, Tip_Private & ASCII.NUL);
   end Set_Tooltip;

   ------------------------
   -- Set_Tooltip_Markup --
   ------------------------

   procedure Set_Tooltip_Markup
     (Tool_Item : access Gtk_Tool_Item_Record;
      Markup    : UTF8_String)
   is
      procedure Internal (Tool_Item, Markup : System.Address);
      pragma Import (C, Internal, "gtk_tool_item_set_tooltip_markup");

      Tmp : constant UTF8_String := Markup & ASCII.NUL;
   begin
      if Markup = "" then
         Internal (Get_Object (Tool_Item), System.Null_Address);
      else
         Internal (Get_Object (Tool_Item), Tmp'Address);
      end if;
   end Set_Tooltip_Markup;

   ----------------------
   -- Set_Tooltip_Text --
   ----------------------

   procedure Set_Tooltip_Text
     (Tool_Item : access Gtk_Tool_Item_Record;
      Text      : UTF8_String)
   is
      procedure Internal (Tool_Item, Text : System.Address);
      pragma Import (C, Internal, "gtk_tool_item_set_tooltip_text");

      Tmp : constant UTF8_String := Text & ASCII.NUL;
   begin
      if Text = "" then
         Internal (Get_Object (Tool_Item), System.Null_Address);
      else
         Internal (Get_Object (Tool_Item), Tmp'Address);
      end if;
   end Set_Tooltip_Text;

   -------------------------
   -- Set_Use_Drag_Window --
   -------------------------

   procedure Set_Use_Drag_Window
     (Toolitem        : access Gtk_Tool_Item_Record;
      Use_Drag_Window : Boolean)
   is
      procedure Internal
        (Toolitem        : System.Address;
         Use_Drag_Window : Gboolean);
      pragma Import (C, Internal, "gtk_tool_item_set_use_drag_window");
   begin
      Internal (Get_Object (Toolitem), Boolean'Pos (Use_Drag_Window));
   end Set_Use_Drag_Window;

   ----------------------------
   -- Set_Visible_Horizontal --
   ----------------------------

   procedure Set_Visible_Horizontal
     (Toolitem           : access Gtk_Tool_Item_Record;
      Visible_Horizontal : Boolean)
   is
      procedure Internal
        (Toolitem           : System.Address;
         Visible_Horizontal : Gboolean);
      pragma Import (C, Internal, "gtk_tool_item_set_visible_horizontal");
   begin
      Internal (Get_Object (Toolitem), Boolean'Pos (Visible_Horizontal));
   end Set_Visible_Horizontal;

   --------------------------
   -- Set_Visible_Vertical --
   --------------------------

   procedure Set_Visible_Vertical
     (Toolitem         : access Gtk_Tool_Item_Record;
      Visible_Vertical : Boolean)
   is
      procedure Internal
        (Toolitem         : System.Address;
         Visible_Vertical : Gboolean);
      pragma Import (C, Internal, "gtk_tool_item_set_visible_vertical");
   begin
      Internal (Get_Object (Toolitem), Boolean'Pos (Visible_Vertical));
   end Set_Visible_Vertical;

   --------------------------
   -- Toolbar_Reconfigured --
   --------------------------

   procedure Toolbar_Reconfigured (Tool_Item : access Gtk_Tool_Item_Record) is
      procedure Internal (Tool_Item : System.Address);
      pragma Import (C, Internal, "gtk_tool_item_toolbar_reconfigured");
   begin
      Internal (Get_Object (Tool_Item));
   end Toolbar_Reconfigured;

end Gtk.Tool_Item;
