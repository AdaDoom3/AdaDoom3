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
--  This widget represents one of the lines in a menu, on which the user can
--  click to execute an action. The menu items can be bound to a submenu, so
--  that clicking on them will in fact display the submenu on the screen.
--
--  They can also be associated with key shortcuts (called accelerators). See
--  the subprogram Set_Accel_Path, and the subprograms in the package
--  Gtk.Accel_Map.
--
--  Activating the proper options in the theme files will allow the user to
--  interactively modify the shortcuts.
--  </description>
--  <c_version>2.16.6</c_version>
--  <group>Menus and Toolbars</group>

with Glib.Properties;
with Gtk.Item;
with Gtk.Widget;

package Gtk.Menu_Item is

   type Gtk_Menu_Item_Record is new Item.Gtk_Item_Record with private;
   type Gtk_Menu_Item is access all Gtk_Menu_Item_Record'Class;

   Null_Submenu : constant Widget.Gtk_Widget;

   procedure Gtk_New
     (Menu_Item : out Gtk_Menu_Item; Label : UTF8_String := "");
   procedure Initialize
     (Menu_Item : access Gtk_Menu_Item_Record'Class; Label : UTF8_String);
   --  Creates or initializes a new menu item containing a simple label.

   procedure Gtk_New_With_Mnemonic
     (Menu_Item : out Gtk_Menu_Item;
      Label     : UTF8_String);
   procedure Initialize_With_Mnemonic
     (Menu_Item : access Gtk_Menu_Item_Record'Class;
      Label     : UTF8_String);
   --  Creates or initializes a new Gtk_Menu_Item containing a label.
   --  The label is created using Gtk.Label.Gtk_New_With_Mnemonic, so
   --  underscores in Label indicate the mnemonic for the menu item.
   --
   --  Warning: with some versions of gtk+, the menu_item will not be properly
   --  destroyed when you remove it from its parent menu, if you created it
   --  with a non-empty Label. In this case, you first need to destroy the
   --  child of the Menu_Item, and then remove it from its parent menu.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Menu_Item.

   procedure Set_Submenu
     (Menu_Item : access Gtk_Menu_Item_Record;
      Submenu   : access Widget.Gtk_Widget_Record'Class);
   function Get_Submenu
     (Menu_Item : access Gtk_Menu_Item_Record) return Gtk.Widget.Gtk_Widget;
   --  Set or Get the submenu underneath Menu_Item.

   procedure Set_Label
     (Menu_Item : access Gtk_Menu_Item_Record;
      Label     : String);
   function Get_Label
     (Menu_Item : access Gtk_Menu_Item_Record)
      return String;
   --  Controls the text in Menu_Item's label.

   procedure Set_Right_Justified
     (Menu_Item : access Gtk_Menu_Item_Record;
      Justify   : Boolean := True);
   function Get_Right_Justified
     (Menu_Item : access Gtk_Menu_Item_Record) return Boolean;
   --  Sets whether the menu item appears justified at the right side of a menu
   --  bar. This was traditionally done for "Help" menu items, but is now
   --  considered a bad idea. (If the widget layout is reversed for a
   --  right-to-left language like Hebrew or Arabic, right-justified-menu-items
   --  appear at the left.)

   procedure Set_Use_Underline
     (Menu_Item : access Gtk_Menu_Item_Record;
      Setting   : Boolean);
   function Get_Use_Underline
     (Menu_Item : access Gtk_Menu_Item_Record)
      return Boolean;
   --  If true, an underline in the text indicates the next character should be
   --  used for the mnemonic accelerator key.

   procedure Set_Accel_Path
     (Menu_Item  : access Gtk_Menu_Item_Record;
      Accel_Path : UTF8_String);
   function Get_Accel_Path
     (Menu_Item : access Gtk_Menu_Item_Record)
      return String;
   --  Set the path that will be used to reference the widget in calls to the
   --  subprograms in Gtk.Accel_Map. This means, for instance, that the widget
   --  is fully setup for interactive modification of the shortcuts by the
   --  user, should he choose to activate this possibility in his themes (see
   --  gtk-accel_map.ads for more information).

   -----------------
   -- Obsolescent --
   -----------------
   --  All subprograms below are now obsolescent in gtk+. They might be removed
   --  from future versions of gtk+ (and therefore GtkAda).
   --  To find out whether your code uses any of these, we recommend compiling
   --  with the -gnatwj switch
   --  <doc_ignore>

   procedure Remove_Submenu (Menu_Item : access Gtk_Menu_Item_Record);
   pragma Obsolescent; --  Remove_Submenu
   --  Remove the menu_item's submenu
   --
   --  Deprecated: 2.12: Remove_Submenu deprecated and should not be used
   --  in newly written code. Use Set_Submenu instead.

   procedure Set_Right_Justify
     (Menu_Item : access Gtk_Menu_Item_Record;
      Justify   : Boolean) renames Set_Right_Justified;
   --  pragma Obsolescent;
   --  This procedure is needed by Gate to automate the code generation.

   procedure Right_Justify (Menu_Item : access Gtk_Menu_Item_Record);
   pragma Obsolescent;
   --  Use Set_Right_Justified with Justify = True instead.

   --  </doc_ignore>

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "activate"
   --    procedure Handler
   --      (Menu_Item : access Gtk_Menu_Item_Record'Class);
   --    Emitted when the menu item has been activated, ie the user has clicked
   --    on it (or use a key shortcut for this)
   --
   --  - "activate_item"
   --    procedure Handler
   --      (Menu_Item : access Gtk_Menu_Item_Record'Class);
   --    ???
   --
   --  - "toggle_size_request"
   --    procedure Handler
   --      (Menu_Item : access Gtk_Menu_Item_Record'Class;
   --       Request   : Gtk_Requisition_Access);
   --    Query the menu item to ask for its preferred size (this might not be
   --    the one actually allocated for it, depending on screen space)
   --
   --  - "toggle_size_allocate"
   --    procedure Handler
   --      (Menu_Item  : access Gtk_Menu_Item_Record'Class;
   --       Allocation : Gtk_Allocation_Request);
   --    You should emit this signal to allocate a specific size for the item.
   --    In practice, you will not need to do this yourself, since gtk+ takes
   --    care of it correctly most of the time.
   --
   --  </signals>
   --
   --  If you want to get a signal every time the menu item is made visible
   --  on screen, for instance because you want to dynamically set its
   --  sensitive state, you should connect to the "map" signal of the
   --  toplevel menu, as in:
   --     Gtkada.Handlers.Widget_Callback.Object_Connect
   --        (Get_Toplevel (Item), "map",
   --         Slot_Object => Item);

   Signal_Activate             : constant Glib.Signal_Name :=
                                   "activate";
   Signal_Activate_Item        : constant Glib.Signal_Name :=
                                   "activate_item";
   Signal_Toggle_Size_Allocate : constant Glib.Signal_Name :=
                                   "toggle_size_allocate";
   Signal_Toggle_Size_Request  : constant Glib.Signal_Name :=
                                   "toggle_size_request";

   procedure Gtk_Select (Menu_Item : access Gtk_Menu_Item_Record);
   --  Emits the "select" signal on Menu_Item

   procedure Deselect (Menu_Item : access Gtk_Menu_Item_Record);
   --  Emits the "deselect" signal on Menu_Item

   procedure Activate (Menu_Item : access Gtk_Menu_Item_Record);
   --  Emits the "activate" signal on Menu_Item

   procedure Toggle_Size_Allocate
     (Menu_Item  : access Gtk_Menu_Item_Record;
      Allocation : Gtk.Widget.Gtk_Allocation);
   --  Emits the "toggle_size_allocate" signal on Menu_Item

   procedure Toggle_Size_Request
     (Menu_Item   : access Gtk_Menu_Item_Record;
      Requisition : out Gtk.Widget.Gtk_Requisition);
   --  Emits the "toggle_size_request" signal on Menu_Item

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Accel_Path_Property
   --  Type:  String
   --  Descr: Sets the accelerator path of the menu item
   --
   --  Name:  Label_Property
   --  Type:  String
   --  Descr: The text for the child label
   --
   --  Name:  Right_Justified_Property
   --  Type:  Boolean
   --  Descr: Sets whether the menu item appears justified at the right
   --         side of a menu bar
   --
   --  Name:  Submenu_Property
   --  Type:  Object
   --  Descr: The submenu attached to the menu item, or null if it has none
   --
   --  Name:  Use_Underline_Property
   --  Type:  Boolean
   --  Descr: If set, an underline in the text indicates the next character
   --         should be used for the mnemonic accelerator key
   --
   --  </properties>

   Accel_Path_Property      : constant Glib.Properties.Property_String;
   Label_Property           : constant Glib.Properties.Property_String;
   Right_Justified_Property : constant Glib.Properties.Property_Boolean;
   Submenu_Property         : constant Glib.Properties.Property_Object;
   Use_Underline_Property   : constant Glib.Properties.Property_Boolean;

   ----------------------
   -- Style Properties --
   ----------------------
   --  The following properties can be changed through the gtk theme and
   --  configuration files, and retrieved through Gtk.Widget.Style_Get_Property

   --  <style_properties>
   --  Name:  Arrow_Scaling_Property
   --  Type:  Float
   --  Descr: Amount of space used up by arrow, relative to the menu item's
   --         font size
   --
   --  Name:  Width_Chars_Property
   --  Type:  Int
   --  Descr: The minimum desired width of the menu item in characters
   --
   --  </style_properties>

   Arrow_Scaling_Property : constant Glib.Properties.Property_Float;
   Width_Chars_Property   : constant Glib.Properties.Property_Int;

private
   type Gtk_Menu_Item_Record is new Item.Gtk_Item_Record with null record;

   Accel_Path_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("accel-path");
   Label_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("label");
   Right_Justified_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("right-justified");
   Submenu_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("submenu");
   Use_Underline_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-underline");

   Arrow_Scaling_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("arrow-scaling");
   Width_Chars_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("width-chars");

   Null_Submenu : constant Widget.Gtk_Widget := null;

   pragma Import (C, Get_Type, "gtk_menu_item_get_type");
end Gtk.Menu_Item;

--  missing:
--  procedure Toggle_Size_Request
--    (Menu_Item   : access Gtk_Menu_Item_Record;
--     Requisition : int*);
--  Emit the signal "toggle_size_request"

--  procedure Toggle_Size_Allocate
--    (Menu_Item  : access Gtk_Menu_Item_Record;
--     Allocation : Gint);
--  Emit the signal "toggle_size_allocate"
