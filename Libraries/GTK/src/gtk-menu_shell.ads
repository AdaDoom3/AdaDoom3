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
--
--  This widget is a base class for all menu widgets. It contains a list of
--  items that can be navigated, selected and activated by the user.
--  It can not be instantiated directly.
--
--  A menu is considered "active" when it is displayed on the screen, or, in
--  the case of a menu_bar when one of its menus is active.
--
--  An item is "selected" if it is displayed in a prelight state and its
--  submenu (if any) displayed.
--
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Menus and Toolbars</group>

with Glib.Properties;
with Gtk.Container;
with Gtk.Menu_Item; use Gtk.Menu_Item;

package Gtk.Menu_Shell is

   type Gtk_Menu_Shell_Record is new
     Gtk.Container.Gtk_Container_Record with private;
   type Gtk_Menu_Shell is access all Gtk_Menu_Shell_Record'Class;

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Menu_Shell.

   procedure Append
     (Menu_Shell : access Gtk_Menu_Shell_Record;
      Child      : access Gtk_Menu_Item_Record'Class);
   --  Add a new item at the end of the menu.

   procedure Prepend
     (Menu_Shell : access Gtk_Menu_Shell_Record;
      Child      : access Gtk_Menu_Item_Record'Class);
   --  Add a new item at the beginning of the menu

   procedure Insert
     (Menu_Shell : access Gtk_Menu_Shell_Record;
      Child      : access Gtk_Menu_Item_Record'Class;
      Position   : Gint);
   --  Add a new item at a specific position in the menu.
   --  The first item is at position 0. To insert as the last item in the menu,
   --  set Position to -1.

   procedure Set_Take_Focus
     (Menu_Shell : access Gtk_Menu_Shell_Record;
      Take_Focus : Boolean := True);
   function Get_Take_Focus
     (Menu_Shell : access Gtk_Menu_Shell_Record) return Boolean;
   --  If Take_Focus is TRUE the menu shell will take the keyboard focus so
   --  that it will receive all keyboard events which is needed to enable
   --  keyboard navigation in menus.
   --
   --  Setting Take_Focus to FALSE is useful only for special applications like
   --  virtual keyboard implementations which should not take keyboard focus.
   --
   --  The Take_Focus state of a menu or menu bar is automatically propagated
   --  to submenus whenever a submenu is popped up, so you don't have to worry
   --  about recursively setting it for your entire menu hierarchy. Only when
   --  programmatically picking a submenu and popping it up manually, the
   --  Take_Focus property of the submenu needs to be set explicitely.
   --
   --  Note that setting it to %ALSE has side-effects:
   --
   --  If the focus is in some other app, it keeps the focus and keynav in
   --  the menu doesn't work. Consequently, keynav on the menu will only
   --  work if the focus is on some toplevel owned by the onscreen keyboard.
   --
   --  To avoid confusing the user, menus with Take_Focus set to FALSE
   --  should not display mnemonics or accelerators, since it cannot be
   --  guaranteed that they will work.

   procedure Select_First
     (Menu_Shell       : access Gtk_Menu_Shell_Record;
      Search_Sensitive : Boolean);
   --  Select the first visible or selectable child of the menu shell;
   --  don't select tearoff items unless the only item is a tearoff
   --  item.
   --  If Search_Sensitive is True, search for the first selectable menu item,
   --  otherwise select nothing if the first item isn't sensitive. This should
   --  be False if the menu is being popped up initially.

   ----------------------
   -- Signals emission --
   ----------------------

   procedure Deactivate (Menu_Shell : access Gtk_Menu_Shell_Record);
   --  Emit the "deactivate" signal.
   --  This deselects the selected item, ungrabs the mouse and keyboard, and
   --  erase the Menu_Shell from the screen.

   procedure Select_Item
     (Menu_Shell : access Gtk_Menu_Shell_Record;
      Item       : access Gtk_Menu_Item_Record'Class);
   --  Select a new item in the menu, after deselecting the current item.

   procedure Deselect (Menu_Shell : access Gtk_Menu_Shell_Record);
   --  Deselect the currently selected item.

   procedure Activate_Item
     (Menu_Shell       : access Gtk_Menu_Shell_Record;
      Item             : access Gtk_Menu_Item_Record'Class;
      Force_Deactivate : Boolean);
   --  Activate the item.
   --  If Force_Deactivate is True or the menu_shell sets this property,
   --  Menu_Shell and all its parent menus are deactivated and erased from
   --  the screen.

   procedure Cancel (Menu_Shell : access Gtk_Menu_Shell_Record);
   --  Cancels the selection within the menu shell.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Take_Focus_Property
   --  Type:  Boolean
   --  Descr: A boolean that determines whether the menu grabs the keyboard
   --         focus
   --
   --  </properties>

   Take_Focus_Property : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "deactivate"
   --    procedure Handler (Menu_Shell : access Gtk_Menu_Shell_Record'Class);
   --    Emitted when the menu is deactivated, ie is erased from the screen.
   --
   --  - "selection-done"
   --    procedure Handler (Menu_Shell : access Gtk_Menu_Shell_Record'Class);
   --    Emitted when an item has been selected. The menu shell might not be
   --    activated when the signal is emitted.
   --
   --  - "move_current"
   --    procedure Handler (Menu_Shell : access Gtk_Menu_Shell_Record'Class;
   --                       Direction  : Gtk_Menu_Direction_Type);
   --    You should emit this signal to request that another menu item be
   --    selected. It is mostly useful when bound to a keybinding.
   --    In a menu, this is bound by default to the arrow keys to move the
   --    the selection.
   --
   --  - "cycle_focus"
   --    procedure Handler (Menu_Shell : access Gtk_Menu_Shell_Record'Class;
   --                       Direction  : Gtk_Menu_Direction_Type);
   --    You should emit this signal to request that another child of
   --    Menu_Shell gets the focus. The child is not activated.
   --
   --  - "activate_current"
   --    procedure Handler (Menu_Shell : access Gtk_Menu_Shell_Record'Class;
   --                       Force_Hide : Gboolean);
   --    Activates the current menu item within the Menu_Shell.
   --    if Force_Hide is True, hide the menu afterwards.
   --
   --  - "cancel"
   --    procedure Handler (Menu_Shell : access Gtk_Menu_Shell_Record'Class);
   --    Cancels the selection within the menu_shell. Causes a "selection-done"
   --    signal to be emitted.
   --
   --  </signals>

   Signal_Activate_Current : constant Glib.Signal_Name := "activate_current";
   Signal_Cancel           : constant Glib.Signal_Name := "cancel";
   Signal_Cycle_Focus      : constant Glib.Signal_Name := "cycle_focus";
   Signal_Deactivate       : constant Glib.Signal_Name := "deactivate";
   Signal_Move_Current     : constant Glib.Signal_Name := "move_current";
   Signal_Selection_Done   : constant Glib.Signal_Name := "selection-done";

private
   type Gtk_Menu_Shell_Record is new
     Gtk.Container.Gtk_Container_Record with null record;

   Take_Focus_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("take-focus");

   pragma Import (C, Get_Type, "gtk_menu_shell_get_type");
end Gtk.Menu_Shell;
