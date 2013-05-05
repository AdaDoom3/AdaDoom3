-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                Copyright (C) 2006 AdaCore                         --
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
--  This package defines a special kind of toggle button that can be inserted
--  in a toolbar. Such buttons are usually created in groups, and only one of
--  them can be active at any time
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Menus and Toolbars</group>

with Glib.Properties;
with Gtk.Toggle_Tool_Button;
with Gtk.Widget;             use Gtk.Widget;

package Gtk.Radio_Tool_Button is

   type Gtk_Radio_Tool_Button_Record is
     new Gtk.Toggle_Tool_Button.Gtk_Toggle_Tool_Button_Record with null record;
   type Gtk_Radio_Tool_Button is access all Gtk_Radio_Tool_Button_Record'Class;

   procedure Gtk_New
     (Radio  : out Gtk_Radio_Tool_Button;
      Group  : Widget_SList.GSlist := Widget_SList.Null_List);
   procedure Initialize
     (Radio  : access Gtk_Radio_Tool_Button_Record'Class;
      Group  : Widget_SList.GSlist := Widget_SList.Null_List);
   --  Create or initialize a new radio button, belonging to Group.
   --  A new group is created if Group is unspecified

   procedure Gtk_New_From_Stock
     (Radio    : out Gtk_Radio_Tool_Button;
      Group    : Widget_SList.GSlist := Widget_SList.Null_List;
      Stock_Id : String);
   procedure Initialize_From_Stock
     (Radio    : access Gtk_Radio_Tool_Button_Record'Class;
      Group    : Widget_SList.GSlist := Widget_SList.Null_List;
      Stock_Id : String);
   --  Create or initialize a new radio button, that will contain a text and
   --  icon specified by Stock_Id

   procedure Gtk_New_From_Widget
     (Radio  : out Gtk_Radio_Tool_Button;
      Group  : access Gtk_Radio_Tool_Button_Record'Class);
   procedure Initialize_From_Widget
     (Radio  : access Gtk_Radio_Tool_Button_Record'Class;
      Group  : access Gtk_Radio_Tool_Button_Record'Class);
   --  Create or initialize a new button belonging to the same group as Group.

   procedure Gtk_New_With_Stock_From_Widget
     (Radio    : out Gtk_Radio_Tool_Button;
      Group    : access Gtk_Radio_Tool_Button_Record'Class;
      Stock_Id : String);
   procedure Initialize_With_Stock_From_Widget
     (Radio    : access Gtk_Radio_Tool_Button_Record'Class;
      Group    : access Gtk_Radio_Tool_Button_Record'Class;
      Stock_Id : String);
   --  Create or initialize a new radio button, that will contain a text and
   --  icon specified by Stock_Id.
   --  The button belongs to the same group as Group.

   procedure Set_Group
     (Button : access Gtk_Radio_Tool_Button_Record;
      Group  : Widget_SList.GSlist);
   --  Change the group to which the button belongs

   function Get_Group
     (Button : access Gtk_Radio_Tool_Button_Record) return Widget_SList.GSlist;
   --  Return the group to which the button belongs

   function Get_Type return GType;
   --  Return the internal type used for this class of widgets

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name: Group_Property
   --  Type: Object
   --  See : Set_Group / Get_Group
   --
   --  </properties>

   Group_Property : constant Glib.Properties.Property_Object;

private
   Group_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("group");
   pragma Import (C, Get_Type, "gtk_radio_tool_button_get_type");

end Gtk.Radio_Tool_Button;
