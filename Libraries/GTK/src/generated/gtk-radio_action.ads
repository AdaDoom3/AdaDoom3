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
--  A Gtk_Radio_Action is similar to Gtk_Radio_Menu_Item. A number of radio
--  actions can be linked together so that only one may be active at any one
--  time.
--
--  </description>
--  <group>Action-based menus</group>
--  <see>Gtk_Action</see>

pragma Warnings (Off, "*is already use-visible*");
with Glib;              use Glib;
with Glib.Properties;   use Glib.Properties;
with Glib.Types;        use Glib.Types;
with Gtk.Buildable;     use Gtk.Buildable;
with Gtk.Toggle_Action; use Gtk.Toggle_Action;
with Gtk.Widget;        use Gtk.Widget;

package Gtk.Radio_Action is

   type Gtk_Radio_Action_Record is new Gtk_Toggle_Action_Record with null record;
   type Gtk_Radio_Action is access all Gtk_Radio_Action_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Action   : out Gtk_Radio_Action;
       Name     : UTF8_String;
       Label    : UTF8_String := "";
       Tooltip  : UTF8_String := "";
       Stock_Id : UTF8_String := "";
       Value    : Gint);
   procedure Initialize
      (Action   : access Gtk_Radio_Action_Record'Class;
       Name     : UTF8_String;
       Label    : UTF8_String := "";
       Tooltip  : UTF8_String := "";
       Stock_Id : UTF8_String := "";
       Value    : Gint);
   --  Creates a new Gtk.Radio_Action.Gtk_Radio_Action object. To add the
   --  action to a Gtk.Actiongroup.Gtk_Actiongroup and set the accelerator for
   --  the action, call Gtk.Action_Group.Add_Action_With_Accel.
   --  Since: gtk+ 2.4
   --  "name": A unique name for the action
   --  "label": The label displayed in menu items and on buttons, or null
   --  "tooltip": A tooltip for this action, or null
   --  "stock_id": The stock icon to display in widgets representing this
   --  action, or null
   --  "value": The value which Gtk.Radio_Action.Get_Current_Value should
   --  return if this action is selected.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_radio_action_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Current_Value
      (Action : access Gtk_Radio_Action_Record) return Gint;
   procedure Set_Current_Value
      (Action        : access Gtk_Radio_Action_Record;
       Current_Value : Gint);
   --  Sets the currently active group member to the member with value
   --  property Current_Value.
   --  Since: gtk+ 2.10
   --  "current_value": the new value

   function Get_Group
      (Action : access Gtk_Radio_Action_Record)
       return Gtk.Widget.Widget_SList.GSlist;
   procedure Set_Group
      (Action : access Gtk_Radio_Action_Record;
       Group  : Gtk.Widget.Widget_SList.GSlist);
   --  Sets the radio group for the radio action object.
   --  A common way to set up a group of radio group is the following:
   --       Group  : GSlist := null;
   --       Action : Gtk_Radio_Action;
   --       while ... loop
   --          Gtk_New (Action, ...);
   --          Set_Group (Action, Group);
   --          Group := Get_Group (Action);
   --       end loop;
   --  Since: gtk+ 2.4
   --  "group": a list representing a radio group

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Radio_Action_Record, Gtk_Radio_Action);
   function "+"
     (Widget : access Gtk_Radio_Action_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Radio_Action
   renames Implements_Buildable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Current_Value_Property
   --  Type: Gint
   --  Flags: read-write
   --  The value property of the currently active member of the group to which
   --  this action belongs.
   --
   --  Name: Group_Property
   --  Type: Gtk_Radio_Action
   --  Flags: write
   --  Sets a new group for a radio action.
   --
   --  Name: Value_Property
   --  Type: Gint
   --  Flags: read-write
   --  The value is an arbitrary integer which can be used as a convenient way
   --  to determine which action in the group is currently active in an
   --  ::activate or ::changed signal handler. See
   --  Gtk.Radio_Action.Get_Current_Value and GtkRadioActionEntry for
   --  convenient ways to get and set this property.

   Current_Value_Property : constant Glib.Properties.Property_Int;
   Group_Property : constant Glib.Properties.Property_Object;
   Value_Property : constant Glib.Properties.Property_Int;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "changed"
   --     procedure Handler
   --       (Self    : access Gtk_Radio_Action_Record'Class;
   --        Current : Gtk_Radio_Action);
   --    --  "current": the member of Action<!-- -->s group which has just been
   --    --  activated
   --  The ::changed signal is emitted on every member of a radio group when
   --  the active member is changed. The signal gets emitted after the
   --  ::activate signals for the previous and current active members.

   Signal_Changed : constant Glib.Signal_Name := "changed";

private
   Current_Value_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("current-value");
   Group_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("group");
   Value_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("value");
end Gtk.Radio_Action;
