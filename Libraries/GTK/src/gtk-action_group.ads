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
--  Actions are organised into groups. An action group is essentially a map
--  from names to Gtk_Action objects.
--
--  All actions that would make sense to use in a particular context should be
--  in a single group. Multiple action groups may be used for a particular user
--  interface. In fact, it is expected that most nontrivial applications will
--  make use of multiple groups. For example, in an application that can edit
--  multiple documents, one group holding global actions (e.g. quit, about,
--  new), and one group per document holding actions that act on that document
--  (eg. save, cut/copy/paste, etc). Each window's menus would be constructed
--  from a combination of two action groups.
--
--  Accelerators are handled by the GTK+ accelerator map. All actions are
--  assigned an accelerator path (which normally has the form
--  "<Actions>/group-name/action-name") and a shortcut is associated with this
--  accelerator path. All menu items and tool items take on this accelerator
--  path. The GTK+ accelerator map code makes sure that the correct shortcut is
--  displayed next to the menu item.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Action-based menus</group>

with Glib.Object;
with Glib.Properties;
with Gtk.Action;
with Interfaces.C.Strings;
with System;

package Gtk.Action_Group is

   type Gtk_Action_Group_Record is new Glib.Object.GObject_Record with
     null record;
   type Gtk_Action_Group is access all Gtk_Action_Group_Record'Class;

   procedure Gtk_New (Group : out Gtk_Action_Group; Name : String);
   procedure Initialize
     (Group : access Gtk_Action_Group_Record'Class; Name : String);
   --  Creates a new Gtk_Action_Group object. The name of the action group
   --  is used when associating keybindings with the actions.

   function Get_Type return GType;
   --  Return the internal value associated with a Gtk_Action_Group.

   function Get_Action
     (Action_Group : access Gtk_Action_Group_Record;
      Action_Name  : String) return Gtk.Action.Gtk_Action;
   --  Looks up an action in the action group by name, or null if no such
   --  action exists.

   function Get_Name
     (Action_Group : access Gtk_Action_Group_Record) return String;
   --  Gets the name of the action group.

   procedure Set_Sensitive
     (Action_Group : access Gtk_Action_Group_Record; Sensitive : Boolean);
   function Get_Sensitive
     (Action_Group : access Gtk_Action_Group_Record) return Boolean;
   --  Returns True if the group is sensitive.  The constituent actions
   --  can only be logically sensitive (see Gtk.Action.Is_Sensitive) if
   --  they are sensitive (see Gtk.Action.Get_Sensitive) and their group
   --  is sensitive.

   procedure Set_Visible
     (Action_Group : access Gtk_Action_Group_Record; Visible : Boolean);
   function Get_Visible
     (Action_Group : access Gtk_Action_Group_Record) return Boolean;
   --  Returns True if the group is visible.  The constituent actions
   --  can only be logically visible (see Gtk.Action.Is_Visible) if
   --  they are visible (see Gtk.Action.Get_Visible) and their group
   --  is visible.

   function List_Actions
     (Action_Group : access Gtk_Action_Group_Record)
     return Glib.Object.Object_Simple_List.Glist;
   --  Lists the actions in the action group. The returned list must be freed
   --  by the user.

   procedure Set_Translation_Domain
     (Action_Group : access Gtk_Action_Group_Record;
      Domain       : String);
   --  Sets the translation domain and uses dgettext() for translating the
   --  Label and Tooltip of Gtk_Action_Entry's added by Add_Actions.

   ---------------------------------
   -- Adding and removing actions --
   ---------------------------------

   type Action_Entry        is private;
   type Radio_Action_Entry  is private;
   type Toggle_Action_Entry is private;
   --  An opaque structure describing an action entry

   type Action_Entry_Array is array (Natural range <>) of Action_Entry;
   type Radio_Action_Entry_Array
     is array (Natural range <>) of Radio_Action_Entry;
   type Toggle_Action_Entry_Array
     is array (Natural range <>) of Toggle_Action_Entry;

   type Action_Callback is access procedure
     (Action : System.Address; User_Data : System.Address);
   pragma Convention (C, Action_Callback);
   --  Profile of callbacks when an action is activated. You must convert
   --  Action to a Gtk_Action through:
   --      Act : constant Gtk_Action := Convert (Action);

   type Radio_Action_Callback is access procedure
     (Group     : access Gtk.Action.Gtk_Action_Record'Class;
      Current   : access Gtk.Action.Gtk_Action_Record'Class;
      User_Data : System.Address);
   --   Called when an element of the Gtk_Radio_Action group is selected

   function Create
     (Name        : String;
      Label       : String := "";
      Stock_Id    : String := "";
      Accelerator : String := "";
      Tooltip     : String := "";
      Callback    : Action_Callback := null) return Action_Entry;
   --  Create a new Action_Entry. The returned value must be freed by the
   --  caller.

   function Create
     (Name        : String;
      Label       : String := "";
      Stock_Id    : String := "";
      Accelerator : String := "";
      Tooltip     : String := "";
      Callback    : Action_Callback := null;
      Is_Active   : Boolean := True) return Toggle_Action_Entry;
   --  Create a new Action_Entry. The returned value must be freed by the
   --  caller. Is_Active is the initial state of the button.

   function Create
     (Name        : String;
      Label       : String;
      Stock_Id    : String := "";
      Accelerator : String := "";
      Tooltip     : String := "";
      Value       : Glib.Gint) return Radio_Action_Entry;
   --  Create a new Radio_Action_Entry. Value is the value set on the radio
   --  action (see Gtk.Radio_Action.Get_Current_Value)

   procedure Free (Action  : in out Action_Entry);
   procedure Free (Actions : in out Action_Entry_Array);
   procedure Free (Action  : in out Radio_Action_Entry);
   procedure Free (Actions : in out Radio_Action_Entry_Array);
   procedure Free (Action  : in out Toggle_Action_Entry);
   procedure Free (Actions : in out Toggle_Action_Entry_Array);
   --  Free Action and Actions

   procedure Add_Action
     (Action_Group : access Gtk_Action_Group_Record;
      Action       : access Gtk.Action.Gtk_Action_Record'Class);
   --  Adds an action object to the action group. Note that this function
   --  does not set up the accel path of the action, which can lead to problems
   --  if a user tries to modify the accelerator of a menuitem associated with
   --  the action. Therefore you must either set the accel path yourself with
   --  Gtk.Action.Set_Accel_Path, or use Add_Action_With_Accel.

   procedure Remove_Action
     (Action_Group : access Gtk_Action_Group_Record;
      Action       : access Gtk.Action.Gtk_Action_Record'Class);
   --  Removes an action object from the action group.

   procedure Add_Action_With_Accel
     (Action_Group : access Gtk_Action_Group_Record;
      Action       : access Gtk.Action.Gtk_Action_Record'Class;
      Accelerator  : String := "");
   --  Adds an action object to the action group and sets up the accelerator.
   --  If Accelerator is unspecified, attempts to use the accelerator
   --  associated with the stock_id of the action.
   --  Accel paths are set to <Actions>/group-name/action-name.

   procedure Add_Actions
     (Action_Group : access Gtk_Action_Group_Record;
      Entries      : Action_Entry_Array;
      User_Data    : System.Address := System.Null_Address;
      Destroy      : Glib.G_Destroy_Notify_Address := null);
   --  This is a convenience function to create a number of actions and add
   --  them to the action group.
   --  Destroy is called when User_Data is no longer needed.
   --
   --  The "activate" signals of the actions are connected to the callbacks in
   --  Entries, and their accel paths are set to
   --  <Actions>/group-name/action-name.

   procedure Add_Radio_Actions
     (Action_Group : access Gtk_Action_Group_Record;
      Entries      : Radio_Action_Entry_Array;
      Value        : Glib.Gint;
      On_Change    : Radio_Action_Callback;
      User_Data    : System.Address := System.Null_Address;
      Destroy      : Glib.G_Destroy_Notify_Address := null);
   --  This is a convenience routine to create a group of radio actions and
   --  add them to the action group.
   --
   --  The "changed" signal of the first radio action is connected to the
   --  On_Change callback and the accel paths of the actions are set to
   --    <Actions>/group-name/action-name
   --
   --  Value is the value of the action to activate initially, or -1 if no
   --  action should be activated.
   --  Destroy is called when User_Data is no longer necessary.

   procedure Add_Toggle_Actions
     (Action_Group : access Gtk_Action_Group_Record;
      Entries      : Toggle_Action_Entry_Array;
      User_Data    : System.Address := System.Null_Address;
      Destroy      : Glib.G_Destroy_Notify_Address := null);
   --  This is a convenience function to create a number of toggle actions and
   --  add them to the action group.
   --  The "activate" signals of the actions are connected to the callbacks and
   --  their accel paths are set to <Actions>/group-name/action-name.
   --  Destroy is called when User_Data is no longer necessary.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Name_Property
   --  Type:  String
   --  Descr: A name for the action group.
   --
   --  Name:  Sensitive_Property
   --  Type:  Boolean
   --  Descr: Whether the action group is enabled.
   --
   --  Name:  Visible_Property
   --  Type:  Boolean
   --  Descr: Whether the action group is visible.
   --
   --  </properties>

   Name_Property      : constant Glib.Properties.Property_String;
   Sensitive_Property : constant Glib.Properties.Property_Boolean;
   Visible_Property   : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "connect_proxy"
   --    procedure Handler
   --       (Group  : access Gtk_Action_Group_Record'Class;
   --        Action : access Gtk_Action_Record'Class;
   --        Proxy  : access Gtk_Widget_Record'Class);
   --    The connect_proxy signal is emitted after connecting a proxy to an
   --    action in the group. Note that the proxy may have been connected to a
   --    different action before.
   --    This is intended for simple customizations for which a custom action
   --    class would be too clumsy, e.g. showing tooltips for menuitems in the
   --    statusbar.
   --    Gtk_UI_Manager proxies the signal and provides global notification
   --    just before any action is connected to a proxy, which is probably more
   --    convenient to use.
   --
   --  - "disconnect_proxy"
   --    procedure Handler
   --       (Group  : access Gtk_Action_Group_Record'Class;
   --        Action : access Gtk_Action_Record'Class;
   --        Proxy  : access Gtk_Widget_Record'Class);
   --    The disconnect_proxy signal is emitted after disconnecting a proxy
   --    from an action in the group.
   --    Gtk_UI_Manager proxies the signal and provides global notification
   --    just before any action is connected to a proxy, which is probably more
   --    convenient to use.
   --
   --  - "post_activate"
   --    procedure Handler
   --       (Group  : access Gtk_Action_Group_Record'Class;
   --        Action : access Gtk_Action_Record'Class);
   --    The post_activate signal is emitted just after the action in the
   --    action_group is activated
   --    This is intended for Gtk_UI_Manager to proxy the signal and provide
   --    global notification just after any action is activated.
   --
   --  - "pre_activate"
   --    procedure Handler
   --       (Group  : access Gtk_Action_Group_Record'Class;
   --        Action : access Gtk_Action_Record'Class);
   --    The post_activate signal is emitted just before the action in the
   --    action_group is activated
   --    This is intended for Gtk_UI_Manager to proxy the signal and provide
   --    global notification just after any action is activated.
   --  </signals>

   Signal_Connect_Proxy    : constant Glib.Signal_Name := "connect_proxy";
   Signal_Disconnect_Proxy : constant Glib.Signal_Name := "disconnect_proxy";
   Signal_Post_Activate    : constant Glib.Signal_Name := "post_activate";
   Signal_Pre_Activate     : constant Glib.Signal_Name := "pre_activate";

private
   Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("name");
   Sensitive_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("sensitive");
   Visible_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("visible");

   type Action_Entry is record
      Name         : Interfaces.C.Strings.chars_ptr;
      Stock_Id     : Interfaces.C.Strings.chars_ptr;
      Label        : Interfaces.C.Strings.chars_ptr;
      Accelerator  : Interfaces.C.Strings.chars_ptr;
      Tooltip      : Interfaces.C.Strings.chars_ptr;
      Callback     : Action_Callback;
   end record;
   pragma Convention (C, Action_Entry);

   type Radio_Action_Entry is record
      Name         : Interfaces.C.Strings.chars_ptr;
      Stock_Id     : Interfaces.C.Strings.chars_ptr;
      Label        : Interfaces.C.Strings.chars_ptr;
      Accelerator  : Interfaces.C.Strings.chars_ptr;
      Tooltip      : Interfaces.C.Strings.chars_ptr;
      Value        : Glib.Gint;
   end record;
   pragma Convention (C, Radio_Action_Entry);

   type Toggle_Action_Entry is record
      Name         : Interfaces.C.Strings.chars_ptr;
      Stock_Id     : Interfaces.C.Strings.chars_ptr;
      Label        : Interfaces.C.Strings.chars_ptr;
      Accelerator  : Interfaces.C.Strings.chars_ptr;
      Tooltip      : Interfaces.C.Strings.chars_ptr;
      Callback     : Action_Callback;
      Is_Active    : Glib.Gboolean;
   end record;
   pragma Convention (C, Toggle_Action_Entry);

   pragma Import (C, Get_Type, "gtk_action_group_get_type");
end Gtk.Action_Group;

--  Binding is through *_full
--  No binding: gtk_action_group_add_actions
--  No binding: gtk_action_group_add_radio_actions
--  No binding: gtk_action_group_add_toggle_actions

--  Binding might be needed later
--  No binding: gtk_action_group_set_translate_func
--  No binding: gtk_action_group_translate_string
