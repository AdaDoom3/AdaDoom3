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
--  This widget implements a drop-down menu.
--  This is basically a simple box that contains a series of Gtk_Menu_Item
--  widgets, on which the user can click to perform actions.
--
--  Such a menu is usually part of a Gtk_Menu_Bar (at the top of the window),
--  or activated by clicking on an item in another Gtk_Menu.
--  See Gtk.Option_Menu for another way of displaying menus.
--
--  All the menus in GtkAda can be "Tear off" menus, i.e you can detach
--  them from their parent (either a menu bar or another menu) to keep them
--  visible on the screen at all times).
--
--  It is worth noting that by default, the user of your application will be
--  able to dynamically modify the shortcuts associated with each menu item.
--  For instance, selecting a menu item and pressing a key will assign this
--  new shortcut to the item, possibly removing the shortcut from any other
--  item it was associated with.
--
--  Note that pressing <backspace> will simply remove the shortcut.
--
--  This default behavior, somewhat unexpected, can be canceled.
--  There are two ways to control this behavior: you can lock a specific menu
--  item by calling Gtk.Widget.Lock_Accelerators on it. But you can also
--  lock all the menu items at once by calling Gtk.Accel_Group.Lock for all
--  the accelerator groups that were used (the GUI builder gate generally
--  creates a single one), as well as on the group returned by
--  Gtk.Accel_Group.Get_Default, which is the one used for items that don't
--  initially have a shortcut.
--  </description>
--  <c_version>2.16.6</c_version>
--  <group>Menus and Toolbars</group>
--  <testgtk>create_menu.adb</testgtk>

with Gdk.Screen;
with Glib.Properties;
with Gtk.Accel_Group;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Menu_Shell;
with Gtk.Widget;

package Gtk.Menu is

   type Gtk_Menu_Record is new
     Gtk.Menu_Shell.Gtk_Menu_Shell_Record with private;
   type Gtk_Menu is access all Gtk_Menu_Record'Class;

   type Gtk_Menu_Detach_Func is access procedure
     (Attach_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Menu          : access Gtk_Menu_Record'Class);
   pragma Convention (C, Gtk_Menu_Detach_Func);
   --  Function called when a menu previously attached to a widget is detached.
   --  An access to this function is given in Attach_To_Widget.

   ---------------------
   -- Creating a menu --
   ---------------------

   procedure Gtk_New (Widget : out Gtk_Menu);
   --  Create a new empty menu.

   procedure Initialize (Widget : access Gtk_Menu_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Menu.

   procedure Set_Active (Menu : access Gtk_Menu_Record; Index : Guint);
   function Get_Active
     (Menu : access Gtk_Menu_Record) return Gtk.Menu_Item.Gtk_Menu_Item;
   --  Select a specified item in the menu.
   --  You will almost never need this function, it is used internally by
   --  Gtk_Option_Menu, for which it is the item that is currently selected.
   --  Note that the item is not considered as being pressed by the user when
   --  you call Set_Active, and thus no callback is called as a result.

   procedure Set_Tearoff_State
     (Menu : access Gtk_Menu_Record; Torn_Off : Boolean);
   function Get_Tearoff_State (Menu : access Gtk_Menu_Record) return Boolean;
   --  Modify the tearoff status of the menu.
   --  If Torn_Off is False, the menu is displayed as a drop down menu which
   --  disappears when the menu is not active. If Torn_Off is True, the menu
   --  persists until it is closed or reattached.
   --  Note that you can give the user access to this functionality by
   --  inserting a Gtk_Tearoff_Menu_Item in the menu.

   procedure Set_Title (Menu : access Gtk_Menu_Record; Title : UTF8_String);
   function Get_Title  (Menu : access Gtk_Menu_Record) return UTF8_String;
   --  Set the title of the menu.
   --  Title is displayed when the menu is displayed as a tearoff menu in an
   --  independent window.

   procedure Reorder_Child
     (Menu     : access Gtk_Menu_Record;
      Child    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Position : Gint);
   --  Move an existing menu_item within the menu.
   --  Its new position is given by Position, 0 being the first item in the
   --  menu.
   --  If Child does not exist in the menu, nothing is done.

   procedure Attach
     (Menu          : access Gtk_Menu_Record;
      Child         : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class;
      Left_Attach   : Guint;
      Right_Attach  : Guint;
      Top_Attach    : Guint;
      Bottom_Attach : Guint);
   --  Adds a new #GtkMenuItem to a (table) menu. The number of 'cells' that
   --  an item will occupy is specified by left_attach, right_attach,
   --  top_attach and bottom_attach. These each represent the leftmost,
   --  rightmost, uppermost and lower column and row numbers of the table.
   --  (Columns and rows are indexed from zero).
   --
   --  Note that this function is not related to Detach().
   --
   --  Adding items to a standard menu is simply done by calling Add().

   -----------------------
   -- Displaying a menu --
   -----------------------

   type Gtk_Menu_Position_Func is access procedure
     (Menu : access Gtk_Menu_Record'Class;
      X    : out Gint;
      Y    : out Gint);
   --  This function is called when displaying a popup menu on the screen.
   --  It should return the (X, Y) coordinates of the menu.
   --  Note that you might want to attach the menu to a widget first if you
   --  want to display the menu relative to its attached widget.
   --
   --  Note that there is a second version of this function (with added
   --  user data in the package User_Menu_Popup below

   procedure Popup
     (Menu              : access Gtk_Menu_Record;
      Parent_Menu_Shell : Gtk.Menu_Shell.Gtk_Menu_Shell := null;
      Parent_Menu_Item  : Gtk.Menu_Item.Gtk_Menu_Item := null;
      Func              : Gtk_Menu_Position_Func := null;
      Button            : Guint := 1;
      Activate_Time     : Guint32 := 0);
   --  Display a menu on the screen.
   --  This is the function to use to create contextual menus.
   --  Most of the time, the parameters can have a null value.
   --  Parent_Menu_Shell is the Gtk_Menu_Shell that contains Parent_Menu_Item,
   --  i.e. the widget that triggered the display of the menu.
   --  Func is a function that returns the coordinates for the menu. If it is
   --  null, then a default function that positions the menu at the pointer
   --  location is used.
   --  Button is the mouse button that was pressed to initiate the event.
   --  Activate_Time is the time at which the event occurred (you can get it
   --  directly from the Gdk_Event structure).
   --
   --  Note that a variant of this function is given in the generic package
   --  User_Menu_Popup.

   --  Note: in the Popup function, the Parent_* parameters are not access
   --  parameters because they might be null.

   type C_Gtk_Menu_Position_Func is access procedure
     (Menu      : System.Address;
      X         : out Gint;
      Y         : out Gint;
      Push_In   : out Gboolean;
      User_Data : System.Address);
   pragma Convention (C, C_Gtk_Menu_Position_Func);

   procedure Popup
     (Menu              : access Gtk_Menu_Record;
      Parent_Menu_Shell : Gtk.Menu_Shell.Gtk_Menu_Shell := null;
      Parent_Menu_Item  : Gtk.Menu_Item.Gtk_Menu_Item := null;
      Func              : C_Gtk_Menu_Position_Func := null;
      User_Data         : System.Address;
      Button            : Guint := 1;
      Activate_Time     : Guint32 := 0);
   --  Similar to the Popup function above, but exposes a lower level
   --  interface to a C positioning function (C_Gtk_Menu_Position_Func).

   generic
      --  <doc_ignore>
      type Data_Type is private;
      --  </doc_ignore>

   package User_Menu_Popup is
      --  <doc_ignore>
      type Gtk_Menu_Position_Func is access procedure
        (Menu      : access Gtk_Menu_Record'Class;
         X         : out Gint;
         Y         : out Gint;
         User_Data : access Data_Type);
      --  </doc_ignore>

      procedure Popup
        (Menu              : access Gtk_Menu_Record'Class;
         Data              : access Data_Type;
         Parent_Menu_Shell : Gtk.Menu_Shell.Gtk_Menu_Shell := null;
         Parent_Menu_Item  : Gtk.Menu_Item.Gtk_Menu_Item := null;
         Func              : Gtk_Menu_Position_Func := null;
         Button            : Guint := 1;
         Activate_Time     : Guint32 := 0);
      --  Same as the Popup function above.
      --  Note that Data is not duplicated, thus you should take care of the
      --  memory allocation/deallocation yourself.

      --  Note also that the order of parameters is slightly different from the
      --  C version.
   private
      procedure Internal_Menu_Position_Func_With_Data
        (Menu      : System.Address;
         X         : out Gint;
         Y         : out Gint;
         Push_In   : out Gboolean;
         User_Data : System.Address);
      pragma Convention (C, Internal_Menu_Position_Func_With_Data);
      --  Wrapper function passed to C.  This spec has been put in the
      --  generic's private part because we can not use 'Access in the
      --  generic body to assign to a C_Gtk_Menu_Position_Func type, because
      --  the type is declared outside the generic unit.  (RM 3.10.2(32))

      Internal_Menu_Position_Func_With_Data_Access :
        constant C_Gtk_Menu_Position_Func :=
        Internal_Menu_Position_Func_With_Data'Access;
   end User_Menu_Popup;

   procedure Popdown (Menu : access Gtk_Menu_Record);
   --  Remove the menu from the screen

   procedure Reposition (Menu : access Gtk_Menu_Record);
   --  Reposition a menu according to its position function.
   --  This function is set when Popup is called.

   procedure Set_Monitor
     (Menu        : access Gtk_Menu_Record;
      Monitor_Num : Gint);
   --  Informs GTK+ on which monitor a menu should be popped up.
   --  See Gdk.Screen.Get_Monitor_Geometry.
   --
   --  This function should be called from a Gtk_Menu_Position_Func if the
   --  menu should not appear on the same monitor as the pointer. This
   --  information can't be reliably inferred from the coordinates returned
   --  by a Gtk_Menu_Position_Func, since, for very long menus, these
   --  coordinates may extend beyond the monitor boundaries or even the screen
   --  boundaries.

   function Get_Monitor (Menu : access Gtk_Menu_Record) return Gint;
   --  Retrieves the number of the monitor on which to show the menu, or
   --  -1 if no monitor has been set.

   procedure Set_Screen
     (Menu   : access Gtk_Menu_Record;
      Screen : access Gdk.Screen.Gdk_Screen_Record'Class);
   --  Sets the Gdk_Screen on which the menu will be displayed.

   --------------------------------
   -- Modifying the accelerators --
   --------------------------------

   procedure Set_Accel_Group
     (Menu  : access Gtk_Menu_Record;
      Accel : Accel_Group.Gtk_Accel_Group);
   function Get_Accel_Group
     (Menu : access Gtk_Menu_Record) return Accel_Group.Gtk_Accel_Group;
   --  Set the Accel_Group that holds the global accelerators and key bindings
   --  for the menu.

   procedure Set_Accel_Path
     (Menu       : access Gtk_Menu_Record;
      Accel_Path : UTF8_String);
   function Get_Accel_Path (Menu : access Gtk_Menu_Record) return String;
   --  Set an accelerator path for this menu from which accelerator paths
   --  for its immediate children, its menu items, can be constructed.
   --  The main purpose of this function is to spare the programmer the
   --  inconvenience of having to call Gtk.Menu_Item.Set_Accel_Path on
   --  each menu item that should support runtime user changable accelerators.
   --  Instead, by just calling Gtk.Menu.Set_Accel_Path on their parent,
   --  each menu item of this menu, that contains a label describing its
   --  purpose, automatically gets an accel path assigned. For example, a menu
   --  containing menu items "New" and "Exit", will, after
   --  Set_Accel_Path (menu, "<Gnumeric-Sheet>/File"); has been called, assign
   --  its items the accel paths:
   --  "<Gnumeric-Sheet>/File/New" and "<Gnumeric-Sheet>/File/Exit".
   --  Assigning accel paths to menu items then enables the user to change
   --  their accelerators at runtime.

   ----------------------------------
   -- Attaching a menu to a widget --
   ----------------------------------

   procedure Attach_To_Widget
     (Menu          : access Gtk_Menu_Record;
      Attach_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Detacher      : Gtk_Menu_Detach_Func);
   --  Attach a menu to the widget.
   --  When the menu is detached from the widget (for instance when it is
   --  destroyed), the procedure Detacher will be called.
   --  You will almost never need to use this function, unless you specifically
   --  want a call back when a widget becomes unavailable.
   --  If Attach_Widget is a menu_item with a single label in it, the name of
   --  the window created when Menu is teared-off will be the label in the
   --  menu_item.

   procedure Detach (Menu : access Gtk_Menu_Record);
   --  Detach the menu from its widget, and call the Detacher set in
   --  Attach_To_Widget.

   function Get_Attach_Widget
     (Menu : access Gtk_Menu_Record) return Gtk.Widget.Gtk_Widget;
   --  Return the widget to which the menu was attached.
   --  If the menu was not attached, this function returns null.

   function Get_For_Attach_Widget
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Gtk.Widget.Widget_List.Glist;
   --  Returns a list of the menus which are attached to this widget.
   --  This list is owned by GTK+ and must not be modified.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.

   --  <properties>
   --  Name:  Accel_Group_Property
   --  Type:  Object
   --  Descr: The accel group holding accelerators for the menu
   --
   --  Name:  Accel_Path_Property
   --  Type:  String
   --  Descr: An accel path used to conveniently construct accel paths of
   --         child items
   --
   --  Name:  Active_Property
   --  Type:  Int
   --  Descr: The currently selected menu item
   --
   --  Name:  Attach_Widget_Property
   --  Type:  Object
   --  Descr: The widget the menu is attached to
   --
   --  Name:  Monitor_Property
   --  Type:  Int
   --  Descr: The monitor the menu will be popped up on
   --
   --  Name:  Tearoff_State_Property
   --  Type:  Boolean
   --  Descr: A boolean that indicates whether the menu is torn-off
   --
   --  Name:  Tearoff_Title_Property
   --  Type:  String
   --  Descr: A title that may be displayed by the window manager when this
   --         menu is torn-off
   --  </properties>

   Accel_Group_Property   : constant Glib.Properties.Property_Object;
   Accel_Path_Property    : constant Glib.Properties.Property_String;
   Active_Property        : constant Glib.Properties.Property_Int;
   Attach_Widget_Property : constant Glib.Properties.Property_Object;
   Monitor_Property       : constant Glib.Properties.Property_Int;
   Tearoff_State_Property : constant Glib.Properties.Property_Boolean;
   Tearoff_Title_Property : constant Glib.Properties.Property_String;

   ----------------------
   -- Child Properties --
   ----------------------
   --  The following properties can be set on children of this widget. See
   --  in particular Gtk.Containers.Child_Set_Property.

   --  <child_properties>
   --  Name:  Bottom_Attach_Property
   --  Type:  Int
   --  Descr: The row number to attach the bottom of the child to
   --
   --  Name:  Left_Attach_Property
   --  Type:  Int
   --  Descr: The column number to attach the left side of the child to
   --
   --  Name:  Right_Attach_Property
   --  Type:  Int
   --  Descr: The column number to attach the right side of the child to
   --
   --  Name:  Top_Attach_Property
   --  Type:  Int
   --  Descr: The row number to attach the top of the child to
   --  </child_properties>

   Bottom_Attach_Property : constant Glib.Properties.Property_Int;
   Left_Attach_Property   : constant Glib.Properties.Property_Int;
   Right_Attach_Property  : constant Glib.Properties.Property_Int;
   Top_Attach_Property    : constant Glib.Properties.Property_Int;

   ----------------------
   -- Style Properties --
   ----------------------
   --  The following properties can be changed through the gtk theme and
   --  configuration files, and retrieved through Gtk.Widget.Style_Get_Property

   --  <style_properties>
   --  Name:  Arrow_Placement_Property
   --  Type:  Enum
   --  Descr: Indicates where scroll arrows should be placed
   --
   --  Name:  Arrow_Scaling_Property
   --  Type:  Float
   --  Descr: Arbitrary constant to scale down the size of the scroll arrow
   --
   --  Name:  Double_Arrows_Property
   --  Type:  Boolean
   --  Descr: When scrolling, always show both arrows.
   --
   --  Name:  Horizontal_Offset_Property
   --  Type:  Int
   --  Descr: When the menu is a submenu, position it this number of pixels
   --         offset horizontally
   --
   --  Name:  Horizontal_Padding_Property
   --  Type:  Int
   --  Descr: Extra space at the left and right edges of the menu
   --
   --  Name:  Vertical_Offset_Property
   --  Type:  Int
   --  Descr: When the menu is a submenu, position it this number of pixels
   --         offset vertically
   --
   --  Name:  Vertical_Padding_Property
   --  Type:  Int
   --  Descr: Extra space at the top and bottom of the menu
   --  </style_properties>

   Arrow_Placement_Property    : constant Glib.Properties.Property_Enum;
   Arrow_Scaling_Property      : constant Glib.Properties.Property_Float;
   Double_Arrows_Property      : constant Glib.Properties.Property_Boolean;
   Horizontal_Offset_Property  : constant Glib.Properties.Property_Int;
   Horizontal_Padding_Property : constant Glib.Properties.Property_Int;
   Vertical_Offset_Property    : constant Glib.Properties.Property_Int;
   Vertical_Padding_Property   : constant Glib.Properties.Property_Int;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "move_scroll"
   --    procedure Handler
   --       (Menu : access Gtk_Menu_Record'Class;
   --        Typ  : Gtk_Scroll_Type);
   --    Requests that another part of the menu be made visible. Menus that
   --    display lots of items might not fit on the screen. When this is the
   --    case, gtk+ will insert some scrolling arrows on both ends of the menus
   --    and emitting this signal will behave as if the user had clicked on one
   --    of these arrows.
   --    This signal is mostly useful as a keybinding
   --
   --  </signals>

   Signal_Move_Scroll : constant Glib.Signal_Name := "move_scroll";

private
   type Gtk_Menu_Record is new Gtk.Menu_Shell.Gtk_Menu_Shell_Record
     with null record;

   Accel_Group_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("accel-group");
   Accel_Path_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("accel-path");
   Active_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("active");
   Attach_Widget_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("attach-widget");
   Monitor_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("monitor");
   Tearoff_State_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("tearoff-state");
   Tearoff_Title_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("tearoff-title");

   Bottom_Attach_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("bottom-attach");
   Left_Attach_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("left-attach");
   Right_Attach_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("right-attach");
   Top_Attach_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("top-attach");

   Arrow_Placement_Property : constant Glib.Properties.Property_Enum :=
     Glib.Properties.Build ("arrow-placement");
   Arrow_Scaling_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("arrow-scaling");
   Double_Arrows_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("double-arrows");
   Horizontal_Offset_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("horizontal-offset");
   Horizontal_Padding_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("horizontal-padding");
   Vertical_Offset_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("vertical-offset");
   Vertical_Padding_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("vertical-padding");

   pragma Import (C, Get_Type, "gtk_menu_get_type");
end Gtk.Menu;

--  <example>
--  <include>../examples/documentation/contextual.adb</include>
--  </example>
