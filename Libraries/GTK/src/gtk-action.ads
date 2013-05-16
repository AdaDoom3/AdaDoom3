-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2006-2013, AdaCore                   --
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
--  Actions represent operations that the user can perform, along with some
--  information on how it should be presented in the interface. Each action
--  provides methods to create icons, menu items and toolbar items representing
--  itself.
--
--  As well as the callback that is called when the action gets activated, the
--  following also gets associated with the action:
--    - a name (not translated, for path lookup)
--    - a label (translated, for display)
--    - an accelerator
--    - whether label indicates a stock id
--    - a tooltip (optional, translated)
--    - a toolbar label (optional, shorter than label)
--
--  The action will also have some state information:
--    - visible (shown/hidden)
--    - sensitive (enabled/disabled)
--
--  Apart from regular actions, there are toggle actions, which can be toggled
--  between two states and radio actions, of which only one in a group can be
--  in the "active" state. Other actions can be implemented as Gtk_Action
--  subclasses.
--
--  Each action can have one or more proxy menu item, toolbar button or other
--  proxy widgets. Proxies mirror the state of the action (text label, tooltip,
--  icon, visible, sensitive, etc), and should change when the action's state
--  changes. When the proxy is activated, it should activate its action.
--  </description>
--  <c_version>2.16.6</c_version>
--  <group>Action-based menus</group>

with Glib.G_Icon;
with Glib.Object;
with Glib.Properties;
with Gtk.Accel_Group;
with Gtk.Enums;
with Gtk.Widget;
with System;

package Gtk.Action is

   type Gtk_Action_Record is new Glib.Object.GObject_Record with private;
   type Gtk_Action is access all Gtk_Action_Record'Class;

   procedure Gtk_New
     (Action   : out Gtk_Action;
      Name     : String;
      Label    : String;
      Tooltip  : String := "";
      Stock_Id : String := "");
   procedure Initialize
     (Action   : access Gtk_Action_Record'Class;
      Name     : String;
      Label    : String;
      Tooltip  : String := "";
      Stock_Id : String := "");
   --  Creates a new Gtk_Action object. To add the action to a Gtk_Action_Group
   --  and set the accelerator for the action, call
   --  Gtk.Action_Group.Add_Action_With_Accel.
   --  Name must be a unique name for the action. Label is the label displayed
   --  in menu items and on buttons.

   function Convert (C_Object : System.Address) return Gtk_Action;
   --  Convert a C object to a Gtk_Action. The type of the C object must match,
   --  of course.

   function Get_Type return GType;
   --  Return the internal value associated with a Gtk_Action

   procedure Activate (Action : access Gtk_Action_Record);
   --  Emits the "activate" signal on the specified action, if it isn't
   --  insensitive. This gets called by the proxy widgets when they get
   --  activated.
   --  It can also be used to manually activate an action.

   procedure Connect_Accelerator    (Action : access Gtk_Action_Record);
   procedure Disconnect_Accelerator (Action : access Gtk_Action_Record);
   --  Installs the accelerator for Action if Action has an accel path and
   --  group. See Set_Accel_Path and Set_Accel_Group.
   --  Since multiple proxies may independently trigger the installation
   --  of the accelerator, the Action counts the number of times this
   --  function has been called and doesn't remove the accelerator until
   --  Disconnect_Accelerator has been called as many times.

   function Create_Icon
     (Action    : access Gtk_Action_Record;
      Icon_Size : Gtk.Enums.Gtk_Icon_Size) return Gtk.Widget.Gtk_Widget;
   --  This function is intended for use by action implementations to
   --  create icons displayed in the proxy widgets.
   --  Returns a widget that displays the icon for this action.

   function Get_GIcon (Action : access Gtk_Action_Record)
      return Glib.G_Icon.G_Icon;
   procedure Set_GIcon
     (Action : access Gtk_Action_Record;
      Icon   : Glib.G_Icon.G_Icon);
   --  Gets/Sets the Action's G_Icon.

   function Get_Icon_Name (Action : access Gtk_Action_Record) return String;
   procedure Set_Icon_Name
     (Action    : access Gtk_Action_Record;
      Icon_Name : String);
   --  Gets/Sets the Action's icon name.

   function Get_Is_Important (Action : access Gtk_Action_Record)
      return Boolean;
   procedure Set_Is_Important
     (Action       : access Gtk_Action_Record;
      Is_Important : Boolean);
   --  Gets/Sets whether or not Action is important.

   function Get_Label (Action : access Gtk_Action_Record) return String;
   procedure Set_Label
     (Action : access Gtk_Action_Record;
      Label  : String);
   --  Gets/Sets the label text associated with Action.

   function Get_Short_Label (Action : access Gtk_Action_Record) return String;
   procedure Set_Short_Label
     (Action      : access Gtk_Action_Record;
      Short_Label : String);
   --  Gets/Sets the short label text of Action.

   function Get_Stock_Id (Action : access Gtk_Action_Record) return String;
   procedure Set_Stock_Id
     (Action   : access Gtk_Action_Record;
      Stock_Id : String);
   --  Gets/Sets the stock id of Action.

   function Get_Tooltip (Action : access Gtk_Action_Record) return String;
   procedure Set_Tooltip
     (Action  : access Gtk_Action_Record;
      Tooltip : String);
   --  Gets/Sets the tooltip text associated with Action.

   function Get_Visible_Horizontal (Action : access Gtk_Action_Record)
      return Boolean;
   procedure Set_Visible_Horizontal
     (Action             : access Gtk_Action_Record;
      Visible_Horizontal : Boolean);
   --  Gets/Sets whether Action is visible when horizontal.

   function Get_Visible_Vertical (Action : access Gtk_Action_Record)
      return Boolean;
   procedure Set_Visible_Vertical
     (Action           : access Gtk_Action_Record;
      Visible_Vertical : Boolean);
   --  Gets/Sets whether Action is visible when vertical.

   function Create_Menu
     (Action : access Gtk_Action_Record)
      return Gtk.Widget.Gtk_Widget;
   --  If Action provides a Gtk_Menu widget as a submenu for the menu
   --  item or the toolbar item it creates, this function returns an
   --  instance of that menu.
   --  Since: 2.12

   function Create_Menu_Item
     (Action : access Gtk_Action_Record) return Gtk.Widget.Gtk_Widget;
   --  Creates a menu item widget that proxies for the given action.

   function Create_Tool_Item
     (Action : access Gtk_Action_Record) return Gtk.Widget.Gtk_Widget;
   --  Creates a toolbar item widget that proxies for the given action.

   procedure Set_Accel_Group
     (Action      : access Gtk_Action_Record;
      Accel_Group : Gtk.Accel_Group.Gtk_Accel_Group := null);
   --  Sets the Gtk_Accel_Group in which the accelerator for this action
   --  will be installed.

   procedure Set_Accel_Path
     (Action : access Gtk_Action_Record; Accel_Path : String);
   function Get_Accel_Path (Action : access Gtk_Action_Record) return String;
   --  Sets the accel path for this action.  All proxy widgets associated
   --  with the action will have this accel path, so that their
   --  accelerators are consistent.

   function Get_Name (Action : access Gtk_Action_Record) return String;
   --  Returns the name of the action.

   procedure Set_Sensitive
     (Action    : access Gtk_Action_Record; Sensitive : Boolean);
   function Get_Sensitive (Action : access Gtk_Action_Record) return Boolean;
   --  Returns whether the action itself is sensitive. Note that this doesn't
   --  necessarily mean effective sensitivity. See Is_Sensitive for that.

   function Is_Sensitive (Action : access Gtk_Action_Record) return Boolean;
   --  Returns whether the action is effectively sensitive.
   --  Returns True if teh action and its associated action group are both
   --  sensitive.

   procedure Set_Visible
     (Action : access Gtk_Action_Record; Visible : Boolean);
   function Get_Visible (Action : access Gtk_Action_Record) return Boolean;
   --  Returns whether the action itself is visible. Note that this doesn't
   --  necessarily mean effective visibility. See Is_Visible for that.

   function Is_Visible (Action : access Gtk_Action_Record) return Boolean;
   --  Returns whether the action is effectively visible.
   --  Returns True if the action and its associated action group are both
   --  visible.

   -------------
   -- Proxies --
   -------------

   function Get_Proxies
     (Action : access Gtk_Action_Record) return Gtk.Widget.Widget_SList.GSlist;
   --  Returns the proxy widgets for an action. The returned list must not be
   --  modified

   function Gtk_Widget_Get_Action
     (Widget : access Gtk.Widget.Gtk_Widget_Record) return Gtk_Action;
   pragma Obsolescent (Gtk_Widget_Get_Action);
   --  Returns the action that Widget is a proxy for.
   --  See also Get_Proxies.
   --  Since: 2.10

   procedure Connect_Proxy
     (Action : access Gtk_Action_Record;
      Proxy  : access Gtk.Widget.Gtk_Widget_Record'Class);
   pragma Obsolescent (Connect_Proxy);
   procedure Disconnect_Proxy
     (Action : access Gtk_Action_Record;
      Proxy  : access Gtk.Widget.Gtk_Widget_Record'Class);
   pragma Obsolescent (Disconnect_Proxy);
   --  Connects a widget to an action object as a proxy. Synchronises various
   --  properties of the action with the widget (such as label text, icon,
   --  tooltip, etc), and attaches a callback so that the action gets activated
   --  when the proxy widget does.
   --  If the widget is already connected to an action, it is disconnected
   --  first.
   --  Disconnect_Proxy does not destroy the widget.

   procedure Block_Activate   (Action : access Gtk_Action_Record);
   procedure Unblock_Activate (Action : access Gtk_Action_Record);
   --  Disable or reenable activation signals from the action.  This is
   --  needed when updating the state of your proxy widget could result
   --  in calling Activate.  This is a convenience function to avoid
   --  recursing in those cases (updating toggle state for instance).

   procedure Block_Activate_From
     (Action : access Gtk_Action_Record;
      Proxy  : access Gtk.Widget.Gtk_Widget_Record'Class);
   pragma Obsolescent (Block_Activate_From);
   procedure Unblock_Activate_From
     (Action : access Gtk_Action_Record;
      Proxy  : access Gtk.Widget.Gtk_Widget_Record'Class);
   pragma Obsolescent (Unblock_Activate_From);
   --  Disables calls to the Activate function by signals on the given proxy
   --  widget. This is used to break notification loops for things like check
   --  or radio actions.
   --  This function is intended for use by action implementations.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Action_Group_Property
   --  Type:  Object
   --  Descr: The Gtk_Action_Group this Gtk_Action is associated with, or NULL
   --        (for internal use).
   --
   --  Name:  GIcon_Property
   --  Type:  Object
   --  Descr: The GIcon being displayed
   --
   --  Name:  Hide_If_Empty_Property
   --  Type:  Boolean
   --  Descr: When TRUE, empty menu proxies for this action are hidden.
   --
   --  Name:  Icon_Name_Property
   --  Type:  String
   --  Descr: The name of the icon from the icon theme
   --
   --  Name:  Is_Important_Property
   --  Type:  Boolean
   --  Descr: Whether the action is considered important.
   --
   --  Name:  Label_Property
   --  Type:  String
   --  Descr: The label used for menu items and buttons
   --
   --  Name:  Name_Property
   --  Type:  String
   --  Descr: A unique name for the action.
   --
   --  Name:  Sensitive_Property
   --  Type:  Boolean
   --  Descr: Whether the action is enabled.
   --
   --  Name:  Short_Label_Property
   --  Type:  String
   --  Descr: A shorter label that may be used on toolbar buttons.
   --
   --  Name:  Stock_Id_Property
   --  Type:  String
   --  Descr: The stock icon displayed in widgets representing
   --
   --  Name:  Tooltip_Property
   --  Type:  String
   --  Descr: A tooltip for this action.
   --
   --  Name:  Visible_Property
   --  Type:  Boolean
   --  Descr: Whether the action is visible.
   --
   --  Name:  Visible_Horizontal_Property
   --  Type:  Boolean
   --  Descr: Whether the toolbar item is visible when the toolbar
   --
   --  Name:  Visible_Overflown_Property
   --  Type:  Boolean
   --  Descr: When TRUE, toolitem proxies for this action
   --
   --  Name:  Visible_Vertical_Property
   --  Type:  Boolean
   --  Descr: Whether the toolbar item is visible when the toolbar
   --
   --  </properties>

   Action_Group_Property       : constant Glib.Properties.Property_Object;
   GIcon_Property              : constant Glib.Properties.Property_Object;
   Hide_If_Empty_Property      : constant Glib.Properties.Property_Boolean;
   Icon_Name_Property          : constant Glib.Properties.Property_String;
   Is_Important_Property       : constant Glib.Properties.Property_Boolean;
   Label_Property              : constant Glib.Properties.Property_String;
   Name_Property               : constant Glib.Properties.Property_String;
   Sensitive_Property          : constant Glib.Properties.Property_Boolean;
   Short_Label_Property        : constant Glib.Properties.Property_String;
   Stock_Id_Property           : constant Glib.Properties.Property_String;
   Tooltip_Property            : constant Glib.Properties.Property_String;
   Visible_Property            : constant Glib.Properties.Property_Boolean;
   Visible_Horizontal_Property : constant Glib.Properties.Property_Boolean;
   Visible_Overflown_Property  : constant Glib.Properties.Property_Boolean;
   Visible_Vertical_Property   : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "activate"
   --    procedure Handler (Action : access Gtk_Action_Record'Class);
   --    The "activate" signal is emitted when the action is activated.
   --  </signals>

   Signal_Activate : constant Glib.Signal_Name := "activate";

private
   type Gtk_Action_Record is new Glib.Object.GObject_Record with null record;

   Action_Group_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("action-group");
   GIcon_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("gicon");
   Hide_If_Empty_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("hide-if-empty");
   Icon_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("icon-name");
   Is_Important_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("is-important");
   Label_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("label");
   Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("name");
   Sensitive_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("sensitive");
   Short_Label_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("short-label");
   Stock_Id_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("stock-id");
   Tooltip_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("tooltip");
   Visible_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("visible");
   Visible_Horizontal_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("visible-horizontal");
   Visible_Overflown_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("visible-overflown");
   Visible_Vertical_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("visible-vertical");

   pragma Import (C, Get_Type, "gtk_action_get_type");
end Gtk.Action;

--  No binding: gtk_action_get_accel_closure
