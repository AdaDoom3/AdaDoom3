-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2006-2007 AdaCore                    --
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
--  A Gtk_UI_Manager constructs a user interface (menus and toolbars) from one
--  or more UI definitions, which reference actions from one or more action
--  groups.
--
---------------------
--  UI Definitions --
---------------------
--
--  The UI definitions are specified in an XML format which can be roughly
--  described by the following DTD.
--    - <!ELEMENT ui          (menubar|toolbar|popup|accelerator)* >
--    - <!ELEMENT menubar     (menuitem|separator|placeholder|menu)* >
--    - <!ELEMENT menu        (menuitem|separator|placeholder|menu)* >
--    - <!ELEMENT popup       (menuitem|separator|placeholder|menu)* >
--    - <!ELEMENT toolbar     (toolitem|separator|placeholder)* >
--    - <!ELEMENT placeholder (menuitem|toolitem|separator|placeholder|menu)* >
--    - <!ELEMENT menuitem     EMPTY >
--    - <!ELEMENT toolitem     (menu?) >
--    - <!ELEMENT separator    EMPTY >
--    - <!ELEMENT accelerator  EMPTY >
--    - <!ATTLIST menubar      name                  #IMPLIED
--    -                        action                #IMPLIED >
--    - <!ATTLIST toolbar      name                  #IMPLIED
--    -                        action                #IMPLIED >
--    - <!ATTLIST popup        name                  #IMPLIED
--    -                        action                #IMPLIED >
--    - <!ATTLIST placeholder  name                  #IMPLIED
--    -                        action                #IMPLIED >
--    - <!ATTLIST separator    name                  #IMPLIED
--    -                        action                #IMPLIED
--    -                        expand   (true|false) #IMPLIED >
--    - <!ATTLIST menu         name                  #IMPLIED
--    -                        action                #REQUIRED
--    -                        position (top|bot)    #IMPLIED >
--    - <!ATTLIST menuitem     name                  #IMPLIED
--    -                        action                #REQUIRED
--    -                        position (top|bot)    #IMPLIED >
--    - <!ATTLIST toolitem     name                  #IMPLIED
--    -                        action                #REQUIRED
--    -                        position (top|bot)    #IMPLIED >
--    - <!ATTLIST accelerator  name                  #IMPLIED
--    -                        action                #REQUIRED >
--
--  There are some additional restrictions beyond those specified in the DTD,
--  e.g. every toolitem must have a toolbar in its ancestry and every menuitem
--  must have a menubar or popup in its ancestry. Since a GMarkup parser is
--  used to parse the UI description, it must not only be valid XML, but valid
--  GMarkup.
--
--  If a name is not specified, it defaults to the action. If an action is not
--  specified either, the element name is used. The name and action attributes
--  must not contain '/' characters after parsing (since that would mess up
--  path lookup) and must be usable as XML attributes when enclosed in
--  doublequotes, thus they must not '"' characters or references to the &quot;
--  entity.
--
--  Here is an example:
--  <example>
--  <ui>
--     <menubar>
--       <menu name="FileMenu" action="FileMenuAction">
--         <menuitem name="New" action="New2Action" />
--         <placeholder name="FileMenuAdditions" />
--       </menu>
--       <menu name="JustifyMenu" action="JustifyMenuAction">
--         <menuitem name="Left" action="justify-left"/>
--         <menuitem name="Centre" action="justify-center"/>
--         <menuitem name="Right" action="justify-right"/>
--         <menuitem name="Fill" action="justify-fill"/>
--       </menu>
--     </menubar>
--     <toolbar action="toolbar1">
--       <placeholder name="JustifyToolItems">
--       <separator/>
--       <toolitem name="Left" action="justify-left"/>
--       <toolitem name="Centre" action="justify-center"/>
--       <toolitem name="Right" action="justify-right"/>
--       <toolitem name="Fill" action="justify-fill"/>
--       <separator/>
--     </placeholder>
--    </toolbar>
--  </ui>
--  </example>
--
--  The constructed widget hierarchy is very similar to the element tree of the
--  XML, with the exception that placeholders are merged into their parents.
--  The correspondence of XML elements to widgets should be almost obvious:
--
--  - menubar  : a Gtk_Menu_Bar
--  - toolbar  : a Gtk_Toolbar
--  - popup    : a toplevel Gtk_Menu
--  - menu     : a Gtk_Menu attached to a menuitem
--  - menuitem : a Gtk_Menu_Item subclass, the exact type depends on the action
--  - toolitem : a Gtk_Tool_Item subclass, exact type depends on the action.
--               This may contain a menu element only if the associated action
--               specifies a Gtk_Menu_Tool_Button as proxy
--  - separator   : a Gtk_Separator_Menu_Item or Gtk_Separator_Tool_Item
--  - accelerator : a keyboard accelerator
--
--  The "position" attribute determines where a constructed widget is
--  positioned wrt. to its siblings in the partially constructed tree. If it is
--  "top", the widget is prepended, otherwise it is appended.
--
-----------------
--  UI Merging --
-----------------
--
--  The most remarkable feature of Gtk_UI_Manager is that it can overlay a set
--  of menuitems and toolitems over another one, and demerge them later.
--
--  Merging is done based on the names of the XML elements. Each element is
--  identified by a path which consists of the names of its anchestors,
--  separated by slashes. For example, the menuitem named "Left" in the example
--  above has the path /ui/menubar/JustifyMenu/Left and the toolitem with the
--  same name has path /ui/toolbar1/JustifyToolItems/Left.
--
-------------------
--  Accelerators --
-------------------
--
--  Every action has an accelerator path. Accelerators are installed together
--  with menuitem proxies, but they can also be explicitly added with
--  <accelerator> elements in the UI definition. This makes it possible to have
--  accelerators for actions even if they have no visible proxies.
--
-----------------------
--  Smart Separators --
-----------------------
--
--  The separators created by Gtk_UI_Manager are "smart", i.e. they do not show
--  up in the UI unless they end up between two visible menu or tool items.
--  Separators which are located at the very beginning or end of the menu or
--  toolbar containing them, or multiple separators next to each other, are
--  hidden. This is a useful feature, since the merging of UI elements from
--  multiple sources can make it hard or impossible to determine in advance
--  whether a separator will end up in such an unfortunate position.
--
--  For separators in toolbars, you can set expand="true" to turn them from a
--  small, visible separator to an expanding, invisible one. Toolitems
--  following an expanding separator are effectively right-aligned.
--
------------------
--  Empty Menus --
------------------
--
--  Submenus pose similar problems to separators inconnection with merging. It
--  is impossible to know in advance whether they will end up empty after
--  merging. Gtk_UI_Manager offers two ways to treat empty submenus:
--
--    - make them disappear by hiding the menu item they're attached to
--    - add an insensitive "Empty" item
--
--  The behaviour is chosen based on the "hide_if_empty" property of the action
--  to which the submenu is associated.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Action-based menus</group>

with Glib.Error;
with Glib.Object;
with Glib.Properties;
with Gtk.Accel_Group;
with Gtk.Action;
with Gtk.Action_Group;
with Gtk.Widget;

package Gtk.UI_Manager is

   type Gtk_UI_Manager_Record is new Glib.Object.GObject_Record with
     null record;
   type Gtk_UI_Manager is access all Gtk_UI_Manager_Record'Class;

   type Manager_Item_Type is mod 2 ** 16;
   Manager_Auto        : constant Manager_Item_Type := 2 ** 0;
   Manager_Menubar     : constant Manager_Item_Type := 2 ** 1;
   Manager_Menu        : constant Manager_Item_Type := 2 ** 2;
   Manager_Toolbar     : constant Manager_Item_Type := 2 ** 3;
   Manager_Placeholder : constant Manager_Item_Type := 2 ** 4;
   Manager_Popup       : constant Manager_Item_Type := 2 ** 5;
   Manager_Menuitem    : constant Manager_Item_Type := 2 ** 6;
   Manager_Toolitem    : constant Manager_Item_Type := 2 ** 7;
   Manager_Separator   : constant Manager_Item_Type := 2 ** 8;
   Manager_Accelerator : constant Manager_Item_Type := 2 ** 9;
   --  These enumeration values are used by Add_UI to determine what UI element
   --  to create.

   --------------
   -- Creation --
   --------------

   procedure Gtk_New    (UI : out Gtk_UI_Manager);
   procedure Initialize (UI : access Gtk_UI_Manager_Record'Class);
   --  Creates a new ui manager object.

   function Get_Type return GType;
   --  Return the internal value associated with a Gtk_UI_Manager.

   function New_Merge_Id
     (Self : access Gtk_UI_Manager_Record) return Guint;
   --  Returns an unused merge id, suitable for use with Add_UI.

   procedure Ensure_Update (Self : access Gtk_UI_Manager_Record);
   --  Makes sure that all pending updates to the UI have been completed.
   --  This may occasionally be necessary, since Gtk_UI_Manager updates the
   --  UI in an idle function. A typical example where this function is
   --  useful is to enforce that the menubar and toolbar have been added to
   --  the main window before showing it:
   --      Add (Window, Vbox);
   --      Connect (Merge, "add_widget", Add_Widget'Access, Vbox);
   --      Add_UI_From_File (Merge, "my-menus");
   --      Add_UI_From_File (Merge, "my-toolbars");
   --      Ensure_Update (Merge);
   --      Show (Window);

   ----------------------
   -- Merging contents --
   ----------------------

   procedure Add_UI
     (Self     : access Gtk_UI_Manager_Record;
      Merge_Id : Guint;
      Path     : String;
      Name     : String;
      Action   : String := "";
      Typ      : Manager_Item_Type := Manager_Auto;
      Top      : Boolean := False);
   procedure Remove_UI
     (Self     : access Gtk_UI_Manager_Record;
      Merge_Id : Guint);
   --  Adds a UI element to the current contents of Self.
   --
   --  If Typ is Manager_Auto, GTK+ inserts a menuitem, toolitem or separator
   --  if such an element can be inserted at the place determined by Path.
   --  Otherwise Typ must indicate an element that can be inserted at the place
   --  determined by Path.
   --
   --  If Path points to a menuitem or toolitem, the new element will be
   --  inserted before or after this item, depending on Top.
   --
   --  Merge_Id: see New_Merge_Id.
   --  Action should be the empty string for a separator.

   function Add_UI_From_File
     (Self     : access Gtk_UI_Manager_Record;
      Filename : String;
      Error    : Glib.Error.GError_Access := null) return Guint;
   --  Parses a file containing a UI definition and merges it with the current
   --  contents of Self.
   --  Return value: The merge id for the merged UI. The merge id can be used
   --  to unmerge the UI with Remove_UI. If an error occurred, the return value
   --  is 0, and if Error was specified it is set to the error message.

   function Add_UI_From_String
     (Self   : access Gtk_UI_Manager_Record;
      Buffer : String;
      Error  : Glib.Error.GError_Access := null) return Guint;
   --  Parses a string containing a UI definition and merges it with the
   --  current contents of Self. An enclosing <ui> element is added if it is
   --  missing.
   --  Return value: The merge id for the merged UI. The merge id can be used
   --  to unmerge the UI with Remove_UI. If an error occurred, the return value
   --  is 0, and if Error was specified it is set to the error message.

   procedure Insert_Action_Group
     (Self         : access Gtk_UI_Manager_Record;
      Action_Group : access Gtk.Action_Group.Gtk_Action_Group_Record'Class;
      Pos          : Gint);
   procedure Remove_Action_Group
     (Self         : access Gtk_UI_Manager_Record;
      Action_Group : access Gtk.Action_Group.Gtk_Action_Group_Record'Class);
   --  Inserts an action group into the list of action groups associated
   --  with Self. Actions in earlier groups hide actions with the same
   --  name in later groups.
   --  with Self.

   -----------------------
   -- Querying contents --
   -----------------------

   function Get_Accel_Group
     (Self : access Gtk_UI_Manager_Record)
      return Gtk.Accel_Group.Gtk_Accel_Group;
   --  Returns the Gtk_Accel_Group associated with Self.

   function Get_Action
     (Self : access Gtk_UI_Manager_Record; Path : String)
      return Gtk.Action.Gtk_Action;
   --  Looks up an action by following a path. See Get_Widget for more
   --  information about paths.
   --  Returns the action whose proxy widget is found by following the path,
   --  or null if no widget was found.

   function Get_Action_Groups
     (Self : access Gtk_UI_Manager_Record)
     return Glib.Object.Object_Simple_List.Glist;
   --  Returns the list of action groups associated with Self. The returned
   --  list should not be modified.

   procedure Set_Add_Tearoffs
     (Self         : access Gtk_UI_Manager_Record;
      Add_Tearoffs : Boolean);
   function Get_Add_Tearoffs
     (Self : access Gtk_UI_Manager_Record) return Boolean;
   --  Sets the "add_tearoffs" property, which controls whether menus
   --  generated by this Gtk_UI_Manager will have tearoff menu items.
   --  Note that this only affects regular menus. Generated popup
   --  menus never have tearoff menu items.

   function Get_Toplevels
     (Self  : access Gtk_UI_Manager_Record;
      Types : Manager_Item_Type)
      return Gtk.Widget.Widget_SList.GSlist;
   --  Obtains a list of all toplevel widgets of the requested types.
   --  Types may contain Manager_Menubar, Manager_Toolbar or Manager_Popup.
   --  The returned list must be freed by the caller.

   function Get_UI (Self : access Gtk_UI_Manager_Record) return String;
   --  Create a UI definition of the merged UI

   function Get_Widget
     (Self : access Gtk_UI_Manager_Record;
      Path : String)
      return Gtk.Widget.Gtk_Widget;
   --  Looks up a widget by following a path.
   --  The path consists of the names specified in the XML description of the
   --  UI. separated by '/'. Elements which don't have a name or action
   --  attribute in the XML (e.g. &lt;popup&gt;) can be addressed by their XML
   --  element name (e.g. "popup"). The root element ("/ui") can be omitted in
   --  the path.
   --
   --  Note that the widget found by following a path that ends in a
   --  <menu>; element is the menuitem to which the menu is attached, not
   --  the menu itself.
   --
   --  Also note that the widgets constructed by a ui manager are not tied to
   --  the lifecycle of the ui manager. If you add the widgets returned by this
   --  function to some container or explicitly ref them, they will survive the
   --  destruction of the ui manager.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Add_Tearoffs_Property
   --  Type:  Boolean
   --  Descr: Whether tearoff menu items should be added to menus
   --
   --  Name:  Ui_Property
   --  Type:  String
   --  Descr: An XML string describing the merged UI
   --
   --  </properties>

   Add_Tearoffs_Property : constant Glib.Properties.Property_Boolean;
   UI_Property           : constant Glib.Properties.Property_String;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "actions_changed"
   --    procedure Handler (UI : access Gtk_UI_Manager_Record'Class);
   --    This signal is emitted whenever the set of actions changes.
   --
   --  - "add_widget"
   --    procedure Handler
   --       (UI     : access Gtk_UI_Manager_Record'Class;
   --        Widget : access Gtk_Widget_Record'Class);
   --    The add_widget signal is emitted for each generated menubar and
   --    toolbar. It is not emitted for generated popup menus, which can be
   --    obtained by Get_Widget.
   --
   --  - "connect_proxy"
   --    procedure Handler
   --       (UI     : access Gtk_UI_Manager_Record'Class;
   --        Action : access Gtk_Action_Record'Class;
   --        Proxy  : access Gtk_Widget_Record'Class);
   --    This signal is emitted after connecting a proxy to an action in the
   --    group.
   --    This is intended for simple customizations for which a custom action
   --    class would be too clumsy, e.g. showing tooltips for menuitems in the
   --    statusbar.
   --
   --  - "disconnect_proxy"
   --    procedure Handler
   --       (UI     : access Gtk_UI_Manager_Record'Class;
   --        Action : access Gtk_Action_Record'Class;
   --        Proxy  : access Gtk_Widget_Record'Class);
   --    The disconnect_proxy signal is emitted after disconnecting a proxy
   --    from an action in the group.
   --
   --  - "post_activate"
   --    procedure Handler
   --       (UI     : access Gtk_UI_Manager_Record'Class;
   --        Action : access Gtk_Action_Record'Class);
   --    The post_activate signal is emitted just after the action is
   --    activated. This is intended for applications to get notification just
   --    after any action is activated.
   --
   --  - "pre_activate"
   --    procedure Handler
   --       (UI     : access Gtk_UI_Manager_Record'Class;
   --        Action : access Gtk_Action_Record'Class);
   --    The pre_activate signal is emitted just before the action is
   --    activated. This is intended for applications to get notification just
   --    before any action is activated.
   --  </signals>

   Signal_Actions_Changed  : constant Glib.Signal_Name := "actions_changed";
   Signal_Add_Widget       : constant Glib.Signal_Name := "add_widget";
   Signal_Connect_Proxy    : constant Glib.Signal_Name := "connect_proxy";
   Signal_Disconnect_Proxy : constant Glib.Signal_Name := "disconnect_proxy";
   Signal_Post_Activate    : constant Glib.Signal_Name := "post_activate";
   Signal_Pre_Activate     : constant Glib.Signal_Name := "pre_activate";

private
   Add_Tearoffs_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("add-tearoffs");
   Ui_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("ui");

   pragma Import (C, Get_Type, "gtk_ui_manager_get_type");

end Gtk.UI_Manager;
