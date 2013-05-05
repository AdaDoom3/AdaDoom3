-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2007 AdaCore                    --
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
--  This widget provides a special kind of menu item that represents a
--  radio button. Such a button can be checked or unchecked by the user, but
--  only one button in a group can be selected at any given time, as opposed to
--  a toggle menu item.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Menus and Toolbars</group>

with Glib.Properties;
with Gtk.Check_Menu_Item;
with Gtk.Widget; use Gtk.Widget;

package Gtk.Radio_Menu_Item is

   type Gtk_Radio_Menu_Item_Record is new
     Gtk.Check_Menu_Item.Gtk_Check_Menu_Item_Record with private;
   type Gtk_Radio_Menu_Item is access all Gtk_Radio_Menu_Item_Record'Class;

   procedure Gtk_New
     (Radio_Menu_Item : out Gtk_Radio_Menu_Item;
      Group           : Widget_SList.GSlist;
      Label           : UTF8_String := "");
   procedure Initialize
     (Radio_Menu_Item : access Gtk_Radio_Menu_Item_Record'Class;
      Group           : Widget_SList.GSlist;
      Label           : UTF8_String := "");
   --  Creates or initializes a menu item

   procedure Gtk_New_With_Mnemonic
     (Radio_Menu_Item : out Gtk_Radio_Menu_Item;
      Group           : Widget_SList.GSlist;
      Label           : UTF8_String);
   procedure Initialize_With_Mnemonic
     (Radio_Menu_Item : access Gtk_Radio_Menu_Item_Record'Class;
      Group           : Widget_SList.GSlist;
      Label           : UTF8_String);
   --  Creates or initializes a menu item.
   --  The first underscore sign in Label will be used as a key shortcut to
   --  this item when the menu is displayed. For instance, if you use "_Open",
   --  then pressing alt-o when the menu is open will select the menu item

   procedure Gtk_New_From_Widget
     (Radio_Menu_Item : out Gtk_Radio_Menu_Item;
      Group           : access Gtk_Radio_Menu_Item_Record'Class);
   procedure Initialize_From_Widget
     (Radio_Menu_Item : access Gtk_Radio_Menu_Item_Record'Class;
      Group           : access Gtk_Radio_Menu_Item_Record'Class);
   --  Creates or initializes a menu item. It initially belongs to the same
   --  group as Group

   procedure Gtk_New_With_Label_From_Widget
     (Radio_Menu_Item : out Gtk_Radio_Menu_Item;
      Group           : access Gtk_Radio_Menu_Item_Record'Class;
      Label           : String);
   procedure Initialize_With_Label_From_Widget
     (Radio_Menu_Item : access Gtk_Radio_Menu_Item_Record'Class;
      Group           : access Gtk_Radio_Menu_Item_Record'Class;
      Label           : String);
   --  Creates or initializes a menu item. It initially belongs to the same
   --  group as Group.

   procedure Gtk_New_With_Mnemonic_From_Widget
     (Radio_Menu_Item : out Gtk_Radio_Menu_Item;
      Group           : access Gtk_Radio_Menu_Item_Record'Class;
      Label           : String);
   procedure Initialize_With_Mnemonic_From_Widget
     (Radio_Menu_Item : access Gtk_Radio_Menu_Item_Record'Class;
      Group           : access Gtk_Radio_Menu_Item_Record'Class;
      Label           : String);
   --  Same as Gtk_New_With_Mnemonic, except the group is given by a widget

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Radio_Menu_Item.

   procedure Set_Group
     (Radio_Menu_Item : access Gtk_Radio_Menu_Item_Record;
      Group           : Widget_SList.GSlist);
   function Get_Group
     (Radio_Menu_Item : access Gtk_Radio_Menu_Item_Record)
      return Widget_SList.GSlist;
   --  Set or Get to which the menu item belongs. Only one menu item in this
   --  group can be selected at any given time

   function Selected_Button (In_Group : Widget_SList.GSlist) return Natural;
   --  Return the button number of the selected button in the group.
   --  Note: This function is not part of Gtk+ itself, but is provided as a
   --  convenient function

   -----------------
   -- Obsolescent --
   -----------------
   --  All subprograms below are now obsolescent in gtk+. They might be removed
   --  from future versions of gtk+ (and therefore GtkAda).
   --  To find out whether your code uses any of these, we recommend compiling
   --  with the -gnatwj switch
   --  <doc_ignore>

   function Group
     (Radio_Menu_Item : access Gtk_Radio_Menu_Item_Record)
      return Widget_SList.GSlist renames Get_Group;
   --  pragma Obsolescent;
   --  This function is deprecated. Get_Group should be used instead.

   --  </doc_ignore>

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "group-changed"
   --    procedure Handler (Item : access Gtk_Radio_Menu_Item_Record'Class);
   --    Emitted when the group of Item has been changed.
   --
   --  </signals>

   Signal_Group_Changed : constant Glib.Signal_Name := "group-changed";

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Group_Property
   --  Type:  Object
   --  Descr: The radio menu item whose group this widget belongs to.
   --
   --  </properties>

   Group_Property : constant Glib.Properties.Property_Object;

private
   type Gtk_Radio_Menu_Item_Record is new
     Gtk.Check_Menu_Item.Gtk_Check_Menu_Item_Record with null record;

   Group_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("group");

   pragma Import (C, Get_Type, "gtk_radio_menu_item_get_type");
end Gtk.Radio_Menu_Item;
