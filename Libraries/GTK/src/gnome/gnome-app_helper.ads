-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                   Copyright (C) 2000-2002                         --
--                         ACT-Europe                                --
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
--  This module lets you easily create menus and toolbars for your
--  applications. You basically define a hierarchy of arrays of UI_Info
--  structures, and you later call the provided functions to create menu bars
--  or tool bars.
--  </description>

with Glib; use Glib;
with Gdk.Types; use Gdk.Types;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Menu_Shell; use Gtk.Menu_Shell;
with Gtk.Status_Bar; use Gtk.Status_Bar;
with Gtk.Toolbar; use Gtk.Toolbar;
with Gtk.Widget; use Gtk.Widget;
with Gtkada.Types; use Gtkada.Types;
with Gtkada.Intl; use Gtkada.Intl;
with System; use System;
with Gnome.App; use Gnome.App;

package Gnome.App_Helper is

   type UI_Info_Configurable_Types is
     (Configurable_Item_New,
      Configurable_Item_Open,
      Configurable_Item_Save,
      Configurable_Item_Save_As,
      Configurable_Item_Revert,
      Configurable_Item_Print,
      Configurable_Item_Print_Setup,
      Configurable_Item_Close,
      Configurable_Item_Exit,
      Configurable_Item_Cut,
      Configurable_Item_Copy,
      Configurable_Item_Paste,
      Configurable_Item_Clear,
      Configurable_Item_Undo,
      Configurable_Item_Redo,
      Configurable_Item_Find,
      Configurable_Item_Find_Again,
      Configurable_Item_Replace,
      Configurable_Item_Properties,
      Configurable_Item_Preferences,
      Configurable_Item_About,
      Configurable_Item_Select_All,
      Configurable_Item_New_Window,
      Configurable_Item_Close_Window,
      Configurable_Item_New_Game,
      Configurable_Item_Pause_Game,
      Configurable_Item_Restart_Game,
      Configurable_Item_Undo_Move,
      Configurable_Item_Redo_Move,
      Configurable_Item_Hint,
      Configurable_Item_Scores,
      Configurable_Item_End_Game);
   for UI_Info_Configurable_Types'Size use Gint'Size;

   type UI_Pixmap_Type is
     (Pixmap_None,
      --  No pixmap specified

      Pixmap_Stock,
      --  Use a stock pixmap (Gnome.Stock)

      Pixmap_Data,
      --  Use a pixmap from inline xpm data

      Pixmap_Filename
      --  Use a pixmap from the specified filename
     );
   for UI_Pixmap_Type'Size use Gint'Size;
   --  These values identify the type of pixmap used in an item

   type UI_Info is private;
   type UI_Info_Array is array (Natural range <>) of UI_Info;
   --  This is the structure that defines an item in a menu bar or toolbar. The
   --  idea is to create an array of such structures with the information
   --  needed to create menus or toolbars. The most convenient way to create
   --  such a structure is to use the UI_Info_* functions provided below.

   type UI_Info_Array_Access is access UI_Info_Array;

   type Generic_Callback is access
     procedure (Widget : access Gtk_Widget_Record'Class);

   function UI_New_Item
     (Label           : String;
      Hint            : String := "";
      Callback        : Generic_Callback := null;
      Pixmap_Type     : UI_Pixmap_Type := Pixmap_None;
      Pixmap_Info     : String := "";
      Accelerator_Key : Gdk_Key_Type := 0;
      Ac_Mods         : Gdk_Modifier_Type := 0) return UI_Info;
   --  Return a normal item, or radio item if it is inside a radioitems group.
   --  Label: String to use in the label
   --  Hint: The status bar message
   --  Callback : Function to call when the item is activated
   --  Pixmap_Type: Type of pixmap for the item
   --  Pixmap_Info:
   --    - For Pixmap_Stock, the stock icon name.
   --    - For Pixmap_Data, the inline xpm data ???
   --    - For Pixmap_Filename, the filename string.
   --  Accelerator_Key: Accelerator key
   --  Ac_Mods: Mask of modifier keys for the accelerator

   function UI_New_Subtree
     (Label           : String;
      Info            : UI_Info_Array_Access;
      Pixmap_Type     : UI_Pixmap_Type := Pixmap_None;
      Pixmap_Info     : String := "";
      Accelerator_Key : Gdk_Key_Type := 0;
      Ac_Mods         : Gdk_Modifier_Type := 0) return UI_Info;
   --  Item that defines a subtree/submenu

   --  function UI_New_Toggle_Item
   --  Toggle (check box) item

   --  function UI_New_Radio_Items
   --  Radio item group

   --  function UI_New_Help
   --  Create a list of help topics, used in the Help menu

   --  function UI_New_Item_Configurable
   --  A configurable menu item.

   --  function UI_New_Subtree_Stock
   --  Item that defines a subtree/submenu, same as UI_Subtree,
   --  but the texts should be looked up in the gnome-libs catalog

   --  Handy UI_Info shortcuts

   UI_Info_End : constant UI_Info;
   --  Used to terminate an array of Gnome_UI_Info structures

   UI_Info_Separator : constant UI_Info;
   --  Insert a separator line (on a menu) or a blank space (on a toolbar)

   function UI_Info_Item
     (Label    : String;
      Tooltip  : String;
      Callback : Generic_Callback;
      Xpm_Data : Chars_Ptr_Array) return UI_Info;
   --  Insert an item with an inline xpm icon

   function UI_Info_Item_Stock
     (Label    : String;
      Tooltip  : String;
      Callback : Generic_Callback;
      Stock_Id : String) return UI_Info;
   --  Insert an item with a stock icon

   function UI_Info_Item_None
     (Label    : String;
      Tooltip  : String;
      Callback : Generic_Callback) return UI_Info;
   --  Insert an item with no icon

   function UI_Info_Toggleitem
     (Label     : String;
      Tooltip   : String;
      Callback  : Generic_Callback;
      Xpm_Data  : Chars_Ptr_Array) return UI_Info;
   --  Insert a toggle item (check box) with an inline xpm icon

   function UI_Info_Help (App_Name : String) return UI_Info;
   --  Insert all the help topics based on the application's id

   function UI_Info_Subtree
     (Label : String;
      Tree  : UI_Info_Array_Access) return UI_Info;
   --  Insert a subtree (submenu)

   function UI_Info_Subtree_Hint
     (Label : String;
      Hint  : String;
      Tree  : UI_Info_Array_Access) return UI_Info;
   --  Insert a subtree with a hint

   function UI_Info_Subtree_Stock
     (Label    : String;
      Tree     : UI_Info_Array_Access;
      Stock_Id : String) return UI_Info;
   --  Insert a subtree (submenu) with a stock icon

   --  function UI_Info_Radiolist (List : Gtk_Radio_Item_Array) return UI_Info;
   --  (UI_Radioitems, null, null, List, null, null, 0, null, 0, 0, Null);
   --  Insert a list of radio items
   --  ??? Not bound for now

   function UI_Info_Radioitem
     (Label    : String;
      Tooltip  : String;
      Callback : Generic_Callback;
      Xpm_Data : Chars_Ptr_Array) return UI_Info;
   --  Insert a radio item with an inline xpm icon

   --  Stock menu item macros for some common menu items. Please see
   --  gnome-libs/devel-docs/suggestions.txt about GNOME menu standards.

   --  The 'File' menu

   --  Note: New item requires you to specify what is new, so you need
   --  to specify the document type, so you need to supply the label
   --  as well (it should start with "_New ")

   function UI_Info_Menu_New_Item
     (Label     : String;
      Tooltip   : String;
      Callback  : Generic_Callback) return UI_Info;

   function UI_Info_Menu_New_Subtree
     (Tree : UI_Info_Array_Access) return UI_Info;
   --  If you have more than one new type, use this tree

   function UI_Info_Menu_Open_Item
     (Callback : Generic_Callback) return UI_Info;

   function UI_Info_Menu_Save_Item
     (Callback : Generic_Callback) return UI_Info;

   function UI_Info_Menu_Save_As_Item
     (Callback : Generic_Callback) return UI_Info;

   function UI_Info_Menu_Revert_Item
     (Callback : Generic_Callback) return UI_Info;

   function UI_Info_Menu_Print_Item
     (Callback : Generic_Callback) return UI_Info;

   function UI_Info_Menu_Print_Setup_Item
     (Callback : Generic_Callback) return UI_Info;

   function UI_Info_Menu_Close_Item
     (Callback : Generic_Callback) return UI_Info;

   function UI_Info_Menu_Exit_Item
     (Callback : Generic_Callback) return UI_Info;

   --  The "Edit" menu

   function UI_Info_Menu_Cut_Item
     (Callback : Generic_Callback) return UI_Info;

   function UI_Info_Menu_Copy_Item
     (Callback : Generic_Callback) return UI_Info;

   function UI_Info_Menu_Paste_Item
     (Callback : Generic_Callback) return UI_Info;

   function UI_Info_Menu_Select_All_Item
     (Callback : Generic_Callback) return UI_Info;

   function UI_Info_Menu_Clear_Item
     (Callback : Generic_Callback) return UI_Info;

   function UI_Info_Menu_Undo_Item
     (Callback : Generic_Callback) return UI_Info;

   function UI_Info_Menu_Redo_Item
     (Callback : Generic_Callback) return UI_Info;

   function UI_Info_Menu_Find_Item
     (Callback : Generic_Callback) return UI_Info;

   function UI_Info_Menu_Find_Again_Item
     (Callback : Generic_Callback) return UI_Info;

   function UI_Info_Menu_Replace_Item
     (Callback : Generic_Callback) return UI_Info;

   function UI_Info_Menu_Properties_Item
     (Callback : Generic_Callback) return UI_Info;

   --  The Settings menu

   function UI_Info_Menu_Preferences_Item
     (Callback : Generic_Callback) return UI_Info;

   --  The Windows menu

   function UI_Info_Menu_New_Window_Item
     (Callback : Generic_Callback) return UI_Info;

   function UI_Info_Menu_Close_Window_Item
     (Callback : Generic_Callback) return UI_Info;

   --  And the "Help" menu

   function UI_Info_Menu_About_Item
     (Callback : Generic_Callback) return UI_Info;

   --  The "Game" menu

   function UI_Info_Menu_New_Game_Item
     (Callback : Generic_Callback) return UI_Info;

   function UI_Info_Menu_Pause_Game_Item
     (Callback : Generic_Callback) return UI_Info;

   function UI_Info_Menu_Restart_Game_Item
     (Callback : Generic_Callback) return UI_Info;

   function UI_Info_Menu_Undo_Move_Item
     (Callback : Generic_Callback) return UI_Info;

   function UI_Info_Menu_Redo_Move_Item
     (Callback : Generic_Callback) return UI_Info;

   function UI_Info_Menu_Hint_Item
     (Callback : Generic_Callback) return UI_Info;

   function UI_Info_Menu_Scores_Item
     (Callback : Generic_Callback) return UI_Info;

   function UI_Info_Menu_End_Game_Item
     (Callback : Generic_Callback) return UI_Info;

   function Helper_Gettext (Str : String) return String;

   --  function D_(x) dgettext (PACKAGE, x)
   --  function L_(x) helper_gettext(x)

   --  Some standard menus

   function UI_Info_Menu_File_Tree
     (Tree : UI_Info_Array_Access) return UI_Info;

   function UI_Info_Menu_Edit_Tree
     (Tree : UI_Info_Array_Access) return UI_Info;

   function UI_Info_Menu_View_Tree
     (Tree : UI_Info_Array_Access) return UI_Info;

   function UI_Info_Menu_Settings_Tree
     (Tree : UI_Info_Array_Access) return UI_Info;

   function UI_Info_Menu_Files_Tree
     (Tree : UI_Info_Array_Access) return UI_Info;

   function UI_Info_Menu_Windows_Tree
     (Tree : UI_Info_Array_Access) return UI_Info;

   function UI_Info_Menu_Help_Tree
     (Tree : UI_Info_Array_Access) return UI_Info;

   function UI_Info_Menu_Game_Tree
     (Tree : UI_Info_Array_Access) return UI_Info;

   --  These are strings to be used for paths when working with the menus
   --  stuff

   Menu_File_String     : constant String := -"_File" & ASCII.NUL;
   Menu_File_Path       : constant String := -"_File/";
   Menu_Edit_String     : constant String := -"_Edit" & ASCII.NUL;
   Menu_Edit_Path       : constant String := -"_Edit/";
   Menu_View_String     : constant String := -"_View" & ASCII.NUL;
   Menu_View_Path       : constant String := -"_View/";
   Menu_Settings_String : constant String := -"_Settings" & ASCII.NUL;
   Menu_Settings_Path   : constant String := -"_Settings/";
   Menu_New_String      : constant String := -"_New" & ASCII.NUL;
   Menu_New_Path        : constant String := -"_New/";
   Menu_Files_String    : constant String := -"Fi_les" & ASCII.NUL;
   Menu_Files_Path      : constant String := -"Fi_les/";
   Menu_Windows_String  : constant String := -"_Windows" & ASCII.NUL;
   Menu_Windows_Path    : constant String := -"_Windows/";

   procedure Gnome_Accelerators_Sync;
   --  Flush the accelerator definitions into the application specific
   --  configuration file ~/.gnome/accels/<app-id>.

   procedure Fill_Menu
     (Menu_Shell   : access Gtk_Menu_Shell_Record'Class;
      Info         : UI_Info_Array_Access;
      Accel_Group  : Gtk_Accel_Group := null;
      Uline_Accels : Boolean := False;
      Pos          : Gint := 0;
      Object       : Gtk_Widget := null);
   --  Fill the specified menu shell with items created from the specified
   --  info, inserting them from the item no. pos on.
   --  The accel group will be used as the accel group for all newly created
   --  sub menus and serves as the global accel group for all menu item
   --  hotkeys. If it is passed as null, global hotkeys will be disabled.
   --  The Uline_Accels argument determines whether underline accelerators
   --  will be featured from the menu item labels.
   --  Object, if not null, will be passed to the callbacks as the emitter
   --  (similarly to what is done in Object_Connect).

   procedure Create_Menus
     (App : access Gnome_App_Record'Class; Info : UI_Info_Array_Access);
   --  Construct a menu bar and attach it to the specified application window

   procedure Fill_Toolbar
     (Toolbar     : access Gtk_Toolbar_Record'Class;
      Info        : UI_Info_Array_Access;
      Accel_Group : Gtk_Accel_Group := null);
   --  Fill the specified toolbar with buttons created from the specified info.
   --  If Accel_Group is not null, then the items' accelerator keys are put
   --  into it.

   procedure Create_Toolbar
     (App : access Gnome_App_Record'Class; Info : UI_Info_Array_Access);
   --  Construct a toolbar and attach it to the specified application window

   function Find_Menu_Pos
     (Parent : access Gtk_Widget_Record'Class;
      Path   : String;
      Pos    : Gint) return Gtk_Widget;
   --  Find menu item described by path (see below for details) starting in the
   --  Gtk_Menu_Shell top and return its parent Gtk_Menu_Shell and the position
   --  after this item in pos:
   --  Gtk.Menu_Shell.Insert (P, W, Pos) would then insert widget w in
   --  Gtk_Menu_Shell P right after the menu item described by path.
   --  The path argument should be in the form "File/.../.../Something".
   --  "" will insert the item as the first one in the menubar
   --  "File/" will insert it as the first one in the File menu
   --  "File/Settings" will insert it after the Setting item in the File menu
   --  use of  "File/<Separator>" should be obvious. However this stops after
   --  the first separator.

   procedure Remove_Menus
     (App   : access Gnome_App_Record'Class;
      Path  : String;
      Items : Gint);
   --  Remove num items from the existing app's menu structure begining with
   --  item described by path

   procedure Remove_Menu_Range
     (App   : access Gnome_App_Record'Class;
      Path  : String;
      Start : Gint;
      Items : Gint);
   --  Same as the above, except it removes the specified number of items
   --  from the existing app's menu structure begining with item described by
   --  path, plus the number specified by start - very useful for adding and
   --  removing Recent document items in the File menu.

   procedure Insert_Menus
     (App       : access Gnome_App_Record'Class;
      Path      : String;
      Menu_Info : UI_Info_Array_Access);
   --  what does it do ???

   --  procedure Install_Appbar_Menu_Hints
   --    (Appbar : Gnome_App_Bar;
   --     Info   : UI_Info_Array_Access);
   --  Activate the menu item hints, displaying in the given appbar.
   --  This can't be automatic since we can't reliably find the
   --  appbar.
   --  Really? Why can't it be automatic?

   procedure Install_Statusbar_Menu_Hints
     (Bar : Gtk_Status_Bar; Info : UI_Info_Array_Access);

   procedure Install_Menu_Hints (App : Gnome_App; Info : UI_Info_Array_Access);

private
   type UI_Info_Type is
     (UI_Endofinfo,
      --  No more items, use it at the end of an array

      UI_Item,
      --  Normal item, or radio item if it is inside a radioitems group

      UI_Toggleitem,
      --  Toggle (check box) item

      UI_Radioitems,
      --  Radio item group

      UI_Subtree,
      --  Item that defines a subtree/submenu

      UI_Separator,
      --  Separator line (menus) or blank space (toolbars)

      UI_Help,
      --  Create a list of help topics, used in the Help menu

      UI_Builder_Data,
      --  Specifies the builder data for the following entries
      --  One should be careful when using
      --  Create_*_[Custom|Interp|With_Data] functions with
      --  UI_Info arrays containing UI_Builder_Data items since
      --  their UI_Builder_Data structures completely override the ones
      --  generated or supplied by the above functions.

      UI_Item_Configurable,
      --  A configurable menu item.

      UI_Subtree_Stock
      --  Item that defines a subtree/submenu, same as UI_Subtree,
      --  but the texts should be looked up in the gnome-libs catalog
     );
   for UI_Info_Type'Size use Gint'Size;
   --  These values identify the item type that a particular UI_Info
   --  structure specifies

   type Data_Type_Record is record
      Func   : Generic_Callback;
      --  User's callback

      Object : Gtk_Widget := null;
      --  Slot Object for Object_Connect
   end record;

   type Data_Type_Access is access all Data_Type_Record;
   pragma Convention (C, Data_Type_Access);
   --  Data passed to the C handler

   type UI_Info is record
      Item_Type : UI_Info_Type;
      --  Type of item

      Label     : Chars_Ptr := Null_Ptr;
      --  String to use in the label

      Hint      : Chars_Ptr := Null_Ptr;
      --  For toolbar items, the tooltip. For menu items, the status bar
      --  message

      Moreinfo  : System.Address := System.Null_Address;
      --  For an item, toggleitem, or radioitem, this is a pointer to the
      --  function to call when the item is activated.
      --  For a subtree, a pointer to another array of UI_Info
      --  structures.
      --  For a radioitem lead entry, a pointer to an array of
      --  UI_Info structures for the radio item group.
      --  For a help item, specifies the help node to load
      --  (i.e. the application's identifier) or null for the main program's
      --  name.
      --  For builder data, points to the UI_Builder_Data structure for
      --  the following items

      User_Data   : Data_Type_Access := null;
      --  Data pointer to pass to callbacks.

      Unused_Data : System.Address := System.Null_Address;
      --  Reserved for future expansion, should be Null

      Pixmap_Type : UI_Pixmap_Type := Pixmap_None;
      --  Type of pixmap for the item

      Pixmap_Info : System.Address := System.Null_Address;
      --  Pointer to the pixmap information:
      --  For Pixmap_Stock, a pointer to the stock icon name.
      --  For Pixmap_Data, a pointer to the inline xpm data.
      --  For Pixmap_Filename, a pointer to the filename string.

      Accelerator_Key : Gint := 0;
      --  Accelerator key/UI_Info_Configurable_Types, or 0 for none

      Ac_Mods         : Gdk_Modifier_Type := 0;
      --  Mask of modifier keys for the accelerator

      Widget          : System.Address := System.Null_Address;
      --  Filled in by Gnome.App.Create*, you can use this to tweak the widgets
      --  once they have been created
   end record;
   pragma Convention (C, UI_Info);

   UI_Info_End : constant UI_Info :=
     (UI_Endofinfo, Null_Ptr, Null_Ptr, Null_Address, null,
      Null_Address, Pixmap_None, Null_Address, 0, 0, Null_Address);

   UI_Info_Separator : constant UI_Info :=
     (UI_Separator, Null_Ptr, Null_Ptr, Null_Address, null,
      Null_Address, Pixmap_None, Null_Address, 0, 0, Null_Address);

   pragma Import (C, Gnome_Accelerators_Sync, "gnome_accelerators_sync");

end Gnome.App_Helper;
