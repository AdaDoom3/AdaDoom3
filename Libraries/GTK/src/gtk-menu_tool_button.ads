-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                  Copyright (C) 2006-2013, AdaCore                 --
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
--  This package defines a special kind of menu, that can be inserted in a
--  toolbar. This is not something used very often, as in general a toolbar
--  provides a quick access to features that are already accessible in the
--  menu bar itself.
--  In practice, it is used internally by gtk+ itself to implement the
--  overflow menu in the toolbar.
--  </description>
--  <c_version>2.16.6</c_version>
--  <group>Menus and Toolbars</group>

with Glib.Properties;
with Gtk.Menu;
with Gtk.Tool_Button;
with Gtk.Tooltips;
with Gtk.Widget;

package Gtk.Menu_Tool_Button is

   type Gtk_Menu_Tool_Button_Record is
     new Gtk.Tool_Button.Gtk_Tool_Button_Record with null record;
   type Gtk_Menu_Tool_Button is access all Gtk_Menu_Tool_Button_Record'Class;

   procedure Gtk_New
     (Menu        : out Gtk_Menu_Tool_Button;
      Icon_Widget : Gtk.Widget.Gtk_Widget := null;
      Label       : String := "");
   procedure Initialize
     (Menu        : access Gtk_Menu_Tool_Button_Record'Class;
      Icon_Widget : Gtk.Widget.Gtk_Widget := null;
      Label       : String := "");
   --  Create a new menu by specifying explicitly the text that should appear
   --  on its button. When the button is clicked on, the menu is displayed.

   procedure Gtk_New_From_Stock
     (Menu     : out Gtk_Menu_Tool_Button;
      Stock_Id : String);
   procedure Initialize_From_Stock
     (Menu     : access Gtk_Menu_Tool_Button_Record'Class;
      Stock_Id : String);
   --  Create a new menu. The label and icon of its button are read from
   --  stock items (see gtk-stock.ads)

   function Get_Type return GType;
   --  Return the internal type used for this class of widgets

   procedure Set_Menu
     (Button : access Gtk_Menu_Tool_Button_Record;
      Menu   : access Gtk.Menu.Gtk_Menu_Record'Class);
   function Get_Menu
     (Button : access Gtk_Menu_Tool_Button_Record)
      return Gtk.Menu.Gtk_Menu;
   --  Set or Get the menu that it displayed when the button is clicked on

   procedure Set_Arrow_Tooltip
     (Button      : access Gtk_Menu_Tool_Button_Record;
      Tooltips    : access Gtk.Tooltips.Gtk_Tooltips_Record'Class;
      Tip_Text    : String;
      Tip_Private : String := "");
   pragma Obsolescent; --  Set_Arrow_Tooltip
   --  Set the tooltip set on the arrow button that will display the menu when
   --  clicked on.
   --
   --  Set_Arrow_Tooltip has been deprecated since version 2.12 and should
   --  not be used in newly-written code. Use Set_Arrow_Tooltip_Text instead.

   procedure Set_Arrow_Tooltip_Markup
     (Button : access Gtk_Menu_Tool_Button_Record;
      Markup : String);
   --  Sets the tooltip markup text to be used as tooltip for the arrow button
   --  which pops up the menu.  See Gtk.Tool_Item.Set_Tooltip for setting a
   --  tooltip on the whole Gtk_Menu_Tool_Button.

   procedure Set_Arrow_Tooltip_Text
     (Button : access Gtk_Menu_Tool_Button_Record;
      Text   : String);
   --  Sets the tooltip text to be used as tooltip for the arrow button which
   --  pops up the menu.  See Gtk.Tool_Item.Set_Tooltip for setting a tooltip
   --  on the whole Gtk_Menu_Tool_Button.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "show-menu"
   --    procedure Handler (Menu : access Gtk_Menu_Tool_Button_Record'Class);
   --    Emitted when the menu is being displayed
   --
   --  </signals>

   Signal_Show_Menu : constant Glib.Signal_Name := "show-menu";

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name: Menu_Property
   --  Type: Object
   --  See : Set_Menu / Get_Menu
   --
   --  </properties>

   Menu_Property : constant Glib.Properties.Property_Object;

private
   Menu_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("menu");
   pragma Import (C, Get_Type, "gtk_menu_tool_button_get_type");

end Gtk.Menu_Tool_Button;
