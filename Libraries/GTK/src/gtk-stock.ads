-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                Copyright (C) 2001-2013, AdaCore                   --
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
--  These functions provide an applications programmer with default
--  images and buttons for toolbars, menu pixmaps, etc.
--
--  See the function Gtk.Widget.Render_Icon for a convenience function that
--  converts a stock icon to an actual pixmap/pixbuf.
--  </description>
--  <c_version>2.16.6</c_version>

with Gdk.Types;
with Gtkada.Types;

package Gtk.Stock is

   type Gtk_Stock_Item is record
      Stock_Id           : Gtkada.Types.Chars_Ptr;
      Label              : Gtkada.Types.Chars_Ptr;
      Modifier           : Gdk.Types.Gdk_Modifier_Type;
      Keyval             : Gdk.Types.Gdk_Key_Type;
      Translation_Domain : Gtkada.Types.Chars_Ptr;
   end record;
   pragma Convention (C, Gtk_Stock_Item);

   type Gtk_Stock_Item_Access is access all Gtk_Stock_Item;
   pragma Convention (C, Gtk_Stock_Item_Access);

   type Gtk_Stock_Item_Array is array (Natural range <>) of Gtk_Stock_Item;

   procedure Gtk_New
     (Item               : out Gtk_Stock_Item;
      Stock_Id           : String;
      Label              : UTF8_String;
      Modifier           : Gdk.Types.Gdk_Modifier_Type;
      Keyval             : Gdk.Types.Gdk_Key_Type;
      Translation_Domain : String);
   --  Create a new stock item.

   procedure Add (Item : Gtk_Stock_Item);
   --  Register Item.
   --  If an item already exists with the same stock ID as one of the items,
   --  the old item gets replaced. The stock item is copied, so GtkAda does
   --  not hold any pointer into item and item can be freed. Use
   --  Add_Static if item is persistent and GtkAda need not copy the array.

   procedure Add (Items : Gtk_Stock_Item_Array);
   --  Register each of the stock items in Items.

   procedure Add_Static (Item : Gtk_Stock_Item);
   --  Same as Add, but do not copy Item, so Item must persist until
   --  application exit.

   procedure Add_Static (Items : Gtk_Stock_Item_Array);
   --  Same as Add, but do not copy Items, so Items must persist until
   --  application exit.

   procedure Lookup
     (Stock_Id : String;
      Item     : out Gtk_Stock_Item;
      Success  : out Boolean);
   --  Fill Item with the registered values for Stock_Id.
   --  Success if set to True of Stock_Id was known.

   procedure Free (Item : in out Gtk_Stock_Item);
   --  Free memory allocated in Item.

   --  Stock IDs (not all are stock items; some are images only)

   Stock_Dialog_Authentication : aliased constant String :=
     "gtk-dialog-authentication";
   Stock_Dialog_Info      : aliased constant String := "gtk-dialog-info";
   Stock_Dialog_Warning   : aliased constant String := "gtk-dialog-warning";
   Stock_Dialog_Error     : aliased constant String := "gtk-dialog-error";
   Stock_Dialog_Question  : aliased constant String := "gtk-dialog-question";

   Stock_Dnd              : aliased constant String := "gtk-dnd";
   Stock_Dnd_Multiple     : aliased constant String := "gtk-dnd-multiple";

   Stock_About            : aliased constant String := "gtk-about";
   Stock_Add              : aliased constant String := "gtk-add";
   Stock_Apply            : aliased constant String := "gtk-apply";
   Stock_Bold             : aliased constant String := "gtk-bold";
   Stock_Cancel           : aliased constant String := "gtk-cancel";
   Stock_Caps_Lock_Warning : aliased constant String :=
     "gtk-caps-lock-warning";
   Stock_Cdrom            : aliased constant String := "gtk-cdrom";
   Stock_Clear            : aliased constant String := "gtk-clear";
   Stock_Close            : aliased constant String := "gtk-close";
   Stock_Color_Picker     : aliased constant String := "gtk-color-picker";
   Stock_Convert          : aliased constant String := "gtk-convert";
   Stock_Connect          : aliased constant String := "gtk-connect";
   Stock_Copy             : aliased constant String := "gtk-copy";
   Stock_Cut              : aliased constant String := "gtk-cut";
   Stock_Delete           : aliased constant String := "gtk-delete";
   Stock_Directory        : aliased constant String := "gtk-directory";
   Stock_Discard          : aliased constant String := "gtk-discard";
   Stock_Disconnect       : aliased constant String := "gtk-disconnect";
   Stock_Edit             : aliased constant String := "gtk-edit";
   Stock_Execute          : aliased constant String := "gtk-execute";
   Stock_File             : aliased constant String := "gtk-file";
   Stock_Find             : aliased constant String := "gtk-find";
   Stock_Find_And_Replace : aliased constant String := "gtk-find-and-replace";
   Stock_Floppy           : aliased constant String := "gtk-floppy";
   Stock_Fullscreen       : aliased constant String := "gtk-fullscreen";
   Stock_Goto_Bottom      : aliased constant String := "gtk-goto-bottom";
   Stock_Goto_First       : aliased constant String := "gtk-goto-first";
   Stock_Goto_Last        : aliased constant String := "gtk-goto-last";
   Stock_Goto_Top         : aliased constant String := "gtk-goto-top";
   Stock_Go_Back          : aliased constant String := "gtk-go-back";
   Stock_Go_Down          : aliased constant String := "gtk-go-down";
   Stock_Go_Forward       : aliased constant String := "gtk-go-forward";
   Stock_Go_Up            : aliased constant String := "gtk-go-up";
   Stock_Harddisk         : aliased constant String := "gtk-harddisk";
   Stock_Help             : aliased constant String := "gtk-help";
   Stock_Home             : aliased constant String := "gtk-home";
   Stock_Index            : aliased constant String := "gtk-index";
   Stock_Indent           : aliased constant String := "gtk-indent";
   Stock_Info             : aliased constant String := "gtk-info";
   Stock_Unindent         : aliased constant String := "gtk-unindent";
   Stock_Italic           : aliased constant String := "gtk-italic";
   Stock_Jump_To          : aliased constant String := "gtk-jump-to";
   Stock_Justify_Center   : aliased constant String := "gtk-justify-center";
   Stock_Justify_Fill     : aliased constant String := "gtk-justify-fill";
   Stock_Justify_Left     : aliased constant String := "gtk-justify-left";
   Stock_Justify_Right    : aliased constant String := "gtk-justify-right";
   Stock_Leave_Fullscreen : aliased constant String := "gtk-leave-fullscreen";
   Stock_Missing_Image    : aliased constant String := "gtk-missing-image";
   Stock_Media_Forward    : aliased constant String := "gtk-media-forward";
   Stock_Media_Next       : aliased constant String := "gtk-media-next";
   Stock_Media_Pause      : aliased constant String := "gtk-media-pause";
   Stock_Media_Play       : aliased constant String := "gtk-media-play";
   Stock_Media_Previous   : aliased constant String := "gtk-media-previous";
   Stock_Media_Record     : aliased constant String := "gtk-media-record";
   Stock_Media_Rewind     : aliased constant String := "gtk-media-rewind";
   Stock_Media_Stop       : aliased constant String := "gtk-media-stop";
   Stock_Network          : aliased constant String := "gtk-network";
   Stock_New              : aliased constant String := "gtk-new";
   Stock_No               : aliased constant String := "gtk-no";
   Stock_Ok               : aliased constant String := "gtk-ok";
   Stock_Open             : aliased constant String := "gtk-open";
   Stock_Orientation_Portrait          : aliased constant String :=
     "gtk-orientation-portrait";
   Stock_Orientation_Landscape         : aliased constant String :=
     "gtk-orientation-landscape";
   Stock_Orientation_Reverse_Landscape : aliased constant String :=
     "gtk-orientation-reverse-landscape";
   Stock_Orientation_Reverse_Portrait  : aliased constant String :=
     "gtk-orientation-reverse-portrait";
   Stock_Page_Setup       : aliased constant String := "gtk-page-setup";
   Stock_Paste            : aliased constant String := "gtk-paste";
   Stock_Preferences      : aliased constant String := "gtk-preferences";
   Stock_Print            : aliased constant String := "gtk-print";
   Stock_Print_Error      : aliased constant String := "gtk-print-error";
   Stock_Print_Paused     : aliased constant String := "gtk-print-paused";
   Stock_Print_Preview    : aliased constant String := "gtk-print-preview";
   Stock_Print_Report     : aliased constant String := "gtk-print-report";
   Stock_Print_Warning    : aliased constant String := "gtk-print-warning";
   Stock_Properties       : aliased constant String := "gtk-properties";
   Stock_Quit             : aliased constant String := "gtk-quit";
   Stock_Redo             : aliased constant String := "gtk-redo";
   Stock_Refresh          : aliased constant String := "gtk-refresh";
   Stock_Remove           : aliased constant String := "gtk-remove";
   Stock_Revert_To_Saved  : aliased constant String := "gtk-revert-to-saved";
   Stock_Save             : aliased constant String := "gtk-save";
   Stock_Save_As          : aliased constant String := "gtk-save-as";
   Stock_Select_All       : aliased constant String := "gtk-select-all";
   Stock_Select_Color     : aliased constant String := "gtk-select-color";
   Stock_Select_Font      : aliased constant String := "gtk-select-font";
   Stock_Sort_Ascending   : aliased constant String := "gtk-sort-ascending";
   Stock_Sort_Descending  : aliased constant String := "gtk-sort-descending";
   Stock_Spell_Check      : aliased constant String := "gtk-spell-check";
   Stock_Stop             : aliased constant String := "gtk-stop";
   Stock_Strikethrough    : aliased constant String := "gtk-strikethrough";
   Stock_Undelete         : aliased constant String := "gtk-undelete";
   Stock_Underline        : aliased constant String := "gtk-underline";
   Stock_Undo             : aliased constant String := "gtk-undo";
   Stock_Yes              : aliased constant String := "gtk-yes";
   Stock_Zoom_100         : aliased constant String := "gtk-zoom-100";
   Stock_Zoom_Fit         : aliased constant String := "gtk-zoom-fit";
   Stock_Zoom_In          : aliased constant String := "gtk-zoom-in";
   Stock_Zoom_Out         : aliased constant String := "gtk-zoom-out";

   --  No binding: gtk_stock_list_ids
   --  No binding: gtk_stock_item_free
   --  No binding: gtk_stock_item_copy
   --  No binding: gtk_stock_set_translate_func
end Gtk.Stock;
