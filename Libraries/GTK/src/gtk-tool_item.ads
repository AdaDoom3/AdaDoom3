-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                Copyright (C) 2006-2010 AdaCore                    --
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
--  This package defines the base class for all items that can be added into
--  a toolbar (see gtk-toolbar.ads).
--  See also Gtk.Tool_Button (gtk-tool_button.ads).
--  See also Gtk.Separator_Tool_Item (gtk-separator_tool_item).
--  </description>
--  <c_version>2.16.6</c_version>
--  <group>Menus and Toolbars</group>

with Glib.Properties;
with Gtk.Bin;
with Gtk.Enums;
with Gtk.Menu_Item;
with Gtk.Tooltips;

package Gtk.Tool_Item is

   type Gtk_Tool_Item_Record is new Gtk.Bin.Gtk_Bin_Record with null record;
   type Gtk_Tool_Item is access all Gtk_Tool_Item_Record'Class;

   --------------------
   -- Creating items --
   --------------------

   procedure Gtk_New    (Item : out Gtk_Tool_Item);
   procedure Initialize (Item : access Gtk_Tool_Item_Record'Class);
   --  Create a new tool item, which contains a single child.

   function Get_Type return GType;
   --  Return the internal value associated with a Gtk_Button.

   procedure Set_Expand
     (Tool_Item : access Gtk_Tool_Item_Record;
      Expand    : Boolean);
   function Get_Expand
     (Tool_Item : access Gtk_Tool_Item_Record)
      return Boolean;
   --   Sets whether Tool_Item is allocated extra space when there is more room
   --   on the toolbar than needed for the items. The effect is that the item
   --   gets bigger when the toolbar gets bigger.

   procedure Set_Homogeneous
     (Tool_Item   : access Gtk_Tool_Item_Record;
      Homogeneous : Boolean);
   function Get_Homogeneous
     (Tool_Item : access Gtk_Tool_Item_Record)
      return Boolean;
   --  Sets whether Tool_Item is to be allocated the same size as other
   --  homogeneous items. The effect is that all homogeneous items will have
   --  the same width as the widest of the items.

   function Get_Icon_Size
     (Tool_Item : access Gtk_Tool_Item_Record)
      return Gtk.Enums.Gtk_Icon_Size;
   --  Returns the icon size used for Tool_Item. Custom subclasses of
   --  Gtk_Tool_Item_Record should call this function to find out what size
   --  icons they should use. This settings depends on the toolbar that
   --  contains the item

   procedure Set_Is_Important
     (Tool_Item    : access Gtk_Tool_Item_Record;
      Is_Important : Boolean);
   function Get_Is_Important
     (Tool_Item : access Gtk_Tool_Item_Record)
      return Boolean;
   --  Sets whether Tool_Item should be considered important. The
   --  Gtk_Tool_Button class uses this property to determine whether to show or
   --  hide its label when the toolbar style is Toolbar_Both_Horiz. The result
   --  is that only tool buttons with the "is_important" property set have
   --  labels, an effect known as "priority text".

   function Get_Orientation
     (Tool_Item : access Gtk_Tool_Item_Record)
      return Gtk.Enums.Gtk_Orientation;
   --  Returns the orientation used for Tool_Item.

   procedure Set_Proxy_Menu_Item
     (Tool_Item    : access Gtk_Tool_Item_Record;
      Menu_Item_Id : String;
      Menu_Item    : Gtk.Menu_Item.Gtk_Menu_Item);
   function Get_Proxy_Menu_Item
     (Tool_Item    : access Gtk_Tool_Item_Record;
      Menu_Item_Id : String)
      return Gtk.Menu_Item.Gtk_Menu_Item;
   --  Sets the menu item used in the toolbar overflow menu. Menu_Item_Id is
   --  used to identify the caller of this function and should also be used
   --  with Get_Proxy_Menu_Item.
   --  Custom subclasses of Gtk_Tool_Item_Record should use this function to
   --  update their menu item when the tool item changes.
   --  See also Gtk.Toolbar.Set_Show_Arrow.

   function Retrieve_Proxy_Menu_Item
     (Tool_Item : access Gtk_Tool_Item_Record)
      return Gtk.Menu_Item.Gtk_Menu_Item;
   --  Returns the menu item that was last set by Set_Proxy_Menu_Item, ie the
   --  menu item that will appear in the overflow menu. This might be
   --  different from the one set through Set_Proxy_Menu_Item, if someone else
   --  has overriden the menu afterward.

   procedure Rebuild_Menu (Tool_Item : access Gtk_Tool_Item_Record);
   --  Calling this function signals to the toolbar that the
   --  overflow menu item for Tool_Item has changed. If the
   --  overflow menu is visible when this function it called,
   --  the menu will be rebuilt.

   procedure Set_Tooltip
     (Tool_Item   : access Gtk_Tool_Item_Record;
      Tooltips    : access Gtk.Tooltips.Gtk_Tooltips_Record'Class;
      Tip_Text    : String;
      Tip_Private : String := "");
   --  Sets the tooltips object to be used for Tool item, the text to be
   --  displayed as tooltip on the item and the private text to be used

   procedure Set_Tooltip_Markup
     (Tool_Item : access Gtk_Tool_Item_Record;
      Markup    : UTF8_String);
   --  Sets the markup text to be displayed as tooltip on the item.
   --  See Gtk.Widget.Set_Tooltip_Markup.

   procedure Set_Tooltip_Text
     (Tool_Item : access Gtk_Tool_Item_Record;
      Text      : UTF8_String);
   --  Sets the text to be displayed as tooltip on the item.
   --  See Gtk.Widget.Set_Tooltip_Text.

   procedure Set_Visible_Vertical
     (Toolitem         : access Gtk_Tool_Item_Record;
      Visible_Vertical : Boolean);
   function Get_Visible_Vertical
     (Toolitem : access Gtk_Tool_Item_Record)
      return Boolean;
   --  Sets whether Toolitem is visible when the toolbar is docked
   --  vertically. Some tool items, such as text entries, are too wide to be
   --  useful on a vertically docked toolbar. If visible_vertical is False
   --  Toolitem will not appear on toolbars that are docked vertically.

   procedure Set_Visible_Horizontal
     (Toolitem           : access Gtk_Tool_Item_Record;
      Visible_Horizontal : Boolean);
   function Get_Visible_Horizontal
     (Toolitem : access Gtk_Tool_Item_Record)
      return Boolean;
   --  Same as Set_Visible_Vertical, but for a horizontal orientation

   procedure Set_Use_Drag_Window
     (Toolitem        : access Gtk_Tool_Item_Record;
      Use_Drag_Window : Boolean);
   function Get_Use_Drag_Window
     (Toolitem : access Gtk_Tool_Item_Record)
      return Boolean;
   --  Sets whether Toolitem has a drag window. When True the
   --  toolitem can be used as a drag source through gtk_drag_source_set().
   --  When Toolitem has a drag window it will intercept all events,
   --  even those that would otherwise be sent to a child of Toolitem.

   function Get_Relief_Style
     (Tool_Item : access Gtk_Tool_Item_Record)
      return Gtk.Enums.Gtk_Relief_Style;
   --  Get the relief style of the item

   function Get_Toolbar_Style
     (Tool_Item : access Gtk_Tool_Item_Record)
      return Gtk.Enums.Gtk_Toolbar_Style;
   --  Get the style of the toolbar that contains the item

   procedure Toolbar_Reconfigured (Tool_Item : access Gtk_Tool_Item_Record);
   --  Emits the signal #GtkToolItem::toolbar_reconfigured on Tool_Item.
   --  Gtk_Toolbar and other Gtk_Tool_Shell implementations use this function
   --  to notify children when some aspect of their configuration changes.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  - Name: Visible_Horizontal_Property
   --    Type: Boolean
   --    See : Set_Visible_Horizontal
   --
   --  - Name: Visible_Vertical_Property
   --    Type: Boolean
   --    See : Set_Visible_Vertical
   --
   --  - Name: Is_Important_Property
   --    Type: Boolean
   --    See : Set_Is_Important
   --
   --  </properties>

   Visible_Horizontal_Property : constant Glib.Properties.Property_Boolean;
   Visible_Vertical_Property   : constant Glib.Properties.Property_Boolean;
   Is_Important_Property       : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "create_menu_proxy"
   --    function Handler
   --       (Item : access Gtk_Tool_Item_Record'Class) return Boolean;
   --    Emitted when the toolbar needs information from the item about whether
   --    the item should appear in the toolbar overflow menu. In response, the
   --    item should either:
   --       - call Set_Proxy_Menu_Item with a null parameter, and return True,
   --         to indicate that the item should not appear
   --       - call Set_Proxy_Menu_Item with a new menu item, and return True
   --       - return False to indicate that the signal wasn't handled. The item
   --         will not appear in the overflow menu unless a later handler
   --         installs a menu item
   --    The toolbar may cache the result of this signal. See Rebuild_Menu to
   --    invalidate the cache.
   --
   --  - "toolbar_reconfigured"
   --    procedure Handler (Item : access Gtk_Tool_Item_Record'Class);
   --    Emitted when some property of the toolbar that Item belongs to has
   --    changed.
   --
   --  - "set_tooltip"
   --    function Handler
   --       (Item        : access Gtk_Tool_Item_Record'Class;
   --        Tooltips    : access Gtk_Tooltips_Record'Class;
   --        Tip         : String;
   --        Tip_Private : String) return Boolean;
   --    Emitted when the item's tooltip has changed through Set_Tooltip.
   --    Should return True if the signal was handled.
   --
   --  </signals>

   Signal_Create_Menu_Proxy    : constant Glib.Signal_Name :=
                                   "create_menu_proxy";
   Signal_Toolbar_Reconfigured : constant Glib.Signal_Name :=
                                   "toolbar_reconfigured";
   Signal_Set_Tooltip          : constant Glib.Signal_Name :=
                                   "set_tooltip";

private

   pragma Import (C, Get_Type, "gtk_tool_item_get_type");

   Visible_Horizontal_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("visible-horizontal");
   Visible_Vertical_Property   : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("visible-vertical");
   Is_Important_Property       : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("is-important");
end Gtk.Tool_Item;
