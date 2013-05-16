-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
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

with Gtk; use Gtk;
with Gnome.UI_Defs; use Gnome.UI_Defs;
with Interfaces.C.Strings;
with Unchecked_Conversion;

package body Gnome.App_Helper is

   function New_String (Str : String) return Chars_Ptr
     renames Interfaces.C.Strings.New_String;

   function New_String (Str : String) return System.Address;

   function To_Gint is new
     Unchecked_Conversion (UI_Info_Configurable_Types, Gint);

   procedure First_Marshaller
     (Object    : System.Address;
      User_Data : Data_Type_Access);
   pragma Convention (C, First_Marshaller);
   --  First level marshaller. This is the function that is actually
   --  called by Gnome. It then calls the Ada functions as required.

   ----------------------
   -- First_Marshaller --
   ----------------------

   procedure First_Marshaller
     (Object    : System.Address;
      User_Data : Data_Type_Access) is
   begin
      if User_Data.Func = null then
         return;
      end if;

      if User_Data.Object = null then
         User_Data.Func (Convert (Object));
      else
         User_Data.Func (User_Data.Object);
      end if;
   end First_Marshaller;

   ----------------
   -- New_String --
   ----------------

   function New_String (Str : String) return System.Address is
      function To_Address is new
        Unchecked_Conversion (Chars_Ptr, System.Address);
   begin
      return To_Address (New_String (Str));
   end New_String;

   ------------------
   -- Create_Menus --
   ------------------

   procedure Create_Menus
     (App  : access Gnome_App_Record'Class;
      Info : UI_Info_Array_Access)
   is

      procedure Internal
        (App          : System.Address;
         Info         : System.Address);
      pragma Import (C, Internal, "gnome_app_create_menus");

   begin
      Internal (Get_Object (App), Info.all'Address);
   end Create_Menus;

   --------------------
   -- Create_Toolbar --
   --------------------

   procedure Create_Toolbar
     (App  : access Gnome_App_Record'Class;
      Info : UI_Info_Array_Access)
   is
      procedure Internal
        (App          : System.Address;
         Info         : System.Address);
      pragma Import (C, Internal, "gnome_app_create_toolbar");

   begin
      Internal (Get_Object (App), Info.all'Address);
   end Create_Toolbar;

   ---------------
   -- Fill_Menu --
   ---------------

   procedure Fill_Menu
     (Menu_Shell   : access Gtk_Menu_Shell_Record'Class;
      Info         : UI_Info_Array_Access;
      Accel_Group  : Gtk_Accel_Group := null;
      Uline_Accels : Boolean := False;
      Pos          : Gint := 0;
      Object       : Gtk_Widget := null)
   is
      procedure Internal
        (Menu_Shell   : System.Address;
         Info         : System.Address;
         Accel_Group  : System.Address;
         Uline_Accels : Gint;
         Pos          : Gint);
      pragma Import (C, Internal, "gnome_app_fill_menu");

      type Flat_UI_Info_Array is new UI_Info_Array (Natural);
      type Flat_UI_Info_Array_Access is access all Flat_UI_Info_Array;

      function To_Flat is new Unchecked_Conversion
        (System.Address, Flat_UI_Info_Array_Access);

      procedure Fill_Object
        (Info   : Flat_UI_Info_Array_Access;
         Object : Gtk_Widget);
      --  Fill the User_Data.Object field for each Info element, including
      --  elements inside other info arrays that are contained in Info.

      procedure Fill_Object
        (Info   : Flat_UI_Info_Array_Access;
         Object : Gtk_Widget) is
      begin
         for J in Info'Range loop
            case Info (J).Item_Type is
               when UI_Endofinfo =>
                  exit;

               when UI_Item | UI_Toggleitem | UI_Item_Configurable =>
                  Info (J).User_Data.Object := Object;

               when UI_Subtree | UI_Subtree_Stock | UI_Radioitems =>
                  Fill_Object
                    (To_Flat (Info (J).Moreinfo),
                     Object);

               when UI_Separator | UI_Help | UI_Builder_Data =>
                  null;
            end case;
         end loop;
      end Fill_Object;

   begin
      if Object /= null then
         Fill_Object (To_Flat (Info.all'Address), Object);
      end if;

      Internal
        (Get_Object (Menu_Shell),
         Info.all'Address,
         Get_Object (Accel_Group),
         Boolean'Pos (Uline_Accels),
         Pos);
   end Fill_Menu;

   ------------------
   -- Fill_Toolbar --
   ------------------

   procedure Fill_Toolbar
     (Toolbar     : access Gtk_Toolbar_Record'Class;
      Info        : UI_Info_Array_Access;
      Accel_Group : Gtk_Accel_Group := null)
   is
      procedure Internal
        (Toolbar     : System.Address;
         Info        : System.Address;
         Accel_Group : System.Address);
      pragma Import (C, Internal, "gnome_app_fill_toolbar");

   begin
      Internal
        (Get_Object (Toolbar), Info.all'Address, Get_Object (Accel_Group));
   end Fill_Toolbar;

   -------------------
   -- Find_Menu_Pos --
   -------------------

   function Find_Menu_Pos
     (Parent : access Gtk_Widget_Record'Class;
      Path   : String;
      Pos    : Gint) return Gtk_Widget
   is
      function Internal
        (Parent : System.Address;
         Path   : String;
         Pos    : Gint) return System.Address;
      pragma Import (C, Internal, "gnome_app_find_menu_pos");

   begin
      return Convert (Internal (Get_Object (Parent), Path & ASCII.NUL, Pos));
   end Find_Menu_Pos;

   --------------------
   -- Helper_Gettext --
   --------------------

   function Helper_Gettext (Str : String) return String is
      function Internal (Str : String) return Chars_Ptr;
      pragma Import (C, Internal, "gnome_app_helper_gettext");

   begin
      return Interfaces.C.Strings.Value (Internal (Str & ASCII.NUL));
   end Helper_Gettext;

   ------------------
   -- Insert_Menus --
   ------------------

   procedure Insert_Menus
     (App       : access Gnome_App_Record'Class;
      Path      : String;
      Menu_Info : UI_Info_Array_Access)
   is
      procedure Internal
        (App       : System.Address;
         Path      : String;
         Menu_Info : System.Address);
      pragma Import (C, Internal, "gnome_app_fill_toolbar");

   begin
      Internal (Get_Object (App), Path & ASCII.NUL, Menu_Info.all'Address);
   end Insert_Menus;

   ------------------------
   -- Install_Menu_Hints --
   ------------------------

   procedure Install_Menu_Hints
     (App : Gnome_App; Info : UI_Info_Array_Access)
   is
      procedure Internal
        (App  : System.Address;
         Info : System.Address);
      pragma Import (C, Internal, "gnome_app_install_menu_hints");

   begin
      Internal (Get_Object (App), Info.all'Address);
   end Install_Menu_Hints;

   ----------------------------------
   -- Install_Statusbar_Menu_Hints --
   ----------------------------------

   procedure Install_Statusbar_Menu_Hints
     (Bar  : Gtk_Status_Bar;
      Info : UI_Info_Array_Access)
   is
      procedure Internal
        (Statusbar : System.Address;
         Info      : System.Address);
      pragma Import (C, Internal, "gnome_app_install_statusbar_menu_hints");

   begin
      Internal (Get_Object (Bar), Info.all'Address);
   end Install_Statusbar_Menu_Hints;

   -----------------------
   -- Remove_Menu_Range --
   -----------------------

   procedure Remove_Menu_Range
     (App   : access Gnome_App_Record'Class;
      Path  : String;
      Start : Gint;
      Items : Gint)
   is
      procedure Internal
        (App   : System.Address;
         Path  : String;
         Start : Gint;
         Items : Gint);
      pragma Import (C, Internal, "gnome_app_remove_menu_range");

   begin
      Internal (Get_Object (App), Path & ASCII.NUL, Start, Items);
   end Remove_Menu_Range;

   ------------------
   -- Remove_Menus --
   ------------------

   procedure Remove_Menus
     (App   : access Gnome_App_Record'Class;
      Path  : String;
      Items : Gint)
   is
      procedure Internal
        (App   : System.Address;
         Path  : String;
         Items : Gint);
      pragma Import (C, Internal, "gnome_app_remove_menus");

   begin
      Internal (Get_Object (App), Path & ASCII.NUL, Items);
   end Remove_Menus;

   ------------------
   -- UI_Info_Help --
   ------------------

   function UI_Info_Help (App_Name : String) return UI_Info is
   begin
      return
        (UI_Help, Null_Ptr, Null_Ptr, New_String (App_Name),
         null, Null_Address, Pixmap_None, Null_Address, 0, 0, Null_Address);
   end UI_Info_Help;

   ------------------
   -- UI_Info_Item --
   ------------------

   function UI_Info_Item
     (Label    : String;
      Tooltip  : String;
      Callback : Generic_Callback;
      Xpm_Data : Chars_Ptr_Array) return UI_Info is
   begin
      return
        (UI_Item, New_String (Label), New_String (Tooltip),
         First_Marshaller'Address, new Data_Type_Record'(Callback, null),
         Null_Address, Pixmap_Data, Xpm_Data'Address, 0, 0, Null_Address);
   end UI_Info_Item;

   -----------------------
   -- UI_Info_Item_None --
   -----------------------

   function UI_Info_Item_None
     (Label    : String;
      Tooltip  : String;
      Callback : Generic_Callback) return UI_Info is
   begin
      return
        (UI_Item, New_String (Label), New_String (Tooltip),
         First_Marshaller'Address, new Data_Type_Record'(Callback, null),
         Null_Address,
         Pixmap_None, Null_Address, 0, 0, Null_Address);
   end UI_Info_Item_None;

   ------------------------
   -- UI_Info_Item_Stock --
   ------------------------

   function UI_Info_Item_Stock
     (Label    : String;
      Tooltip  : String;
      Callback : Generic_Callback;
      Stock_Id : String) return UI_Info is
   begin
      return
        (UI_Item, New_String (Label), New_String (Tooltip),
         First_Marshaller'Address, new Data_Type_Record'(Callback, null),
         Null_Address,
         Pixmap_Stock, Stock_Id'Address, 0, 0, Null_Address);
   end UI_Info_Item_Stock;

   -----------------------------
   -- UI_Info_Menu_About_Item --
   -----------------------------

   function UI_Info_Menu_About_Item
     (Callback : Generic_Callback) return UI_Info is
   begin
      return
        (UI_Item_Configurable, Null_Ptr, Null_Ptr,
         First_Marshaller'Address, new Data_Type_Record'(Callback, null),
         Null_Address,
         Pixmap_None, Null_Address, To_Gint (Configurable_Item_About),
         0, Null_Address);
   end UI_Info_Menu_About_Item;

   -----------------------------
   -- UI_Info_Menu_Clear_Item --
   -----------------------------

   function UI_Info_Menu_Clear_Item
     (Callback : Generic_Callback) return UI_Info is
   begin
      return
        (UI_Item_Configurable, Null_Ptr, Null_Ptr,
         First_Marshaller'Address, new Data_Type_Record'(Callback, null),
         Null_Address,
         Pixmap_None, Null_Address, To_Gint (Configurable_Item_Clear),
         0, Null_Address);
   end UI_Info_Menu_Clear_Item;

   -----------------------------
   -- UI_Info_Menu_Close_Item --
   -----------------------------

   function UI_Info_Menu_Close_Item
     (Callback : Generic_Callback) return UI_Info is
   begin
      return
        (UI_Item_Configurable, Null_Ptr, Null_Ptr,
         First_Marshaller'Address, new Data_Type_Record'(Callback, null),
         Null_Address,
         Pixmap_None, Null_Address, To_Gint (Configurable_Item_Close),
         0, Null_Address);
   end UI_Info_Menu_Close_Item;

   ------------------------------------
   -- UI_Info_Menu_Close_Window_Item --
   ------------------------------------

   function UI_Info_Menu_Close_Window_Item
     (Callback : Generic_Callback) return UI_Info is
   begin
      return
        (UI_Item_Configurable, Null_Ptr, Null_Ptr,
         First_Marshaller'Address, new Data_Type_Record'(Callback, null),
         Null_Address,
         Pixmap_None, Null_Address, To_Gint (Configurable_Item_Close_Window),
         0, Null_Address);
   end UI_Info_Menu_Close_Window_Item;

   ----------------------------
   -- UI_Info_Menu_Copy_Item --
   ----------------------------

   function UI_Info_Menu_Copy_Item
     (Callback : Generic_Callback) return UI_Info is
   begin
      return
        (UI_Item_Configurable, Null_Ptr, Null_Ptr,
         First_Marshaller'Address, new Data_Type_Record'(Callback, null),
         Null_Address,
         Pixmap_None, Null_Address, To_Gint (Configurable_Item_Copy),
         0, Null_Address);
   end UI_Info_Menu_Copy_Item;

   ---------------------------
   -- UI_Info_Menu_Cut_Item --
   ---------------------------

   function UI_Info_Menu_Cut_Item
     (Callback : Generic_Callback) return UI_Info is
   begin
      return
        (UI_Item_Configurable, Null_Ptr, Null_Ptr,
         First_Marshaller'Address, new Data_Type_Record'(Callback, null),
         Null_Address,
         Pixmap_None, Null_Address, To_Gint (Configurable_Item_Cut),
         0, Null_Address);
   end UI_Info_Menu_Cut_Item;

   ----------------------------
   -- UI_Info_Menu_Edit_Tree --
   ----------------------------

   function UI_Info_Menu_Edit_Tree
     (Tree : UI_Info_Array_Access) return UI_Info is
   begin
      return
        (UI_Subtree_Stock, New_String (-"_Edit"), Null_Ptr,
         Tree.all'Address, null, Null_Address,
         Pixmap_None, Null_Address, 0, 0, Null_Address);
   end UI_Info_Menu_Edit_Tree;

   --------------------------------
   -- UI_Info_Menu_End_Game_Item --
   --------------------------------

   function UI_Info_Menu_End_Game_Item
     (Callback : Generic_Callback) return UI_Info is
   begin
      return
        (UI_Item_Configurable, Null_Ptr, Null_Ptr,
         First_Marshaller'Address, new Data_Type_Record'(Callback, null),
         Null_Address,
         Pixmap_None, Null_Address, To_Gint (Configurable_Item_End_Game),
         0, Null_Address);
   end UI_Info_Menu_End_Game_Item;

   ----------------------------
   -- UI_Info_Menu_Exit_Item --
   ----------------------------

   function UI_Info_Menu_Exit_Item
     (Callback : Generic_Callback) return UI_Info is
   begin
      return
        (UI_Item_Configurable, Null_Ptr, Null_Ptr,
         First_Marshaller'Address, new Data_Type_Record'(Callback, null),
         Null_Address,
         Pixmap_None, Null_Address, To_Gint (Configurable_Item_Exit),
         0, Null_Address);
   end UI_Info_Menu_Exit_Item;

   ----------------------------
   -- UI_Info_Menu_File_Tree --
   ----------------------------

   function UI_Info_Menu_File_Tree
     (Tree : UI_Info_Array_Access) return UI_Info is
   begin
      return
        (UI_Subtree_Stock, New_String (-"_File"), Null_Ptr,
         Tree.all'Address, null, Null_Address,
         Pixmap_None, Null_Address, 0, 0, Null_Address);
   end UI_Info_Menu_File_Tree;

   -----------------------------
   -- UI_Info_Menu_Files_Tree --
   -----------------------------

   function UI_Info_Menu_Files_Tree
     (Tree : UI_Info_Array_Access) return UI_Info is
   begin
      return
        (UI_Subtree_Stock, New_String (-"Fi_les"), Null_Ptr,
         Tree.all'Address, null, Null_Address,
         Pixmap_None, Null_Address, 0, 0, Null_Address);
   end UI_Info_Menu_Files_Tree;

   ----------------------------------
   -- UI_Info_Menu_Find_Again_Item --
   ----------------------------------

   function UI_Info_Menu_Find_Again_Item
     (Callback : Generic_Callback) return UI_Info is
   begin
      return
        (UI_Item_Configurable, Null_Ptr, Null_Ptr,
         First_Marshaller'Address, new Data_Type_Record'(Callback, null),
         Null_Address,
         Pixmap_None, Null_Address, To_Gint (Configurable_Item_Find_Again),
         0, Null_Address);
   end UI_Info_Menu_Find_Again_Item;

   ----------------------------
   -- UI_Info_Menu_Find_Item --
   ----------------------------

   function UI_Info_Menu_Find_Item
     (Callback : Generic_Callback) return UI_Info is
   begin
      return
        (UI_Item_Configurable, Null_Ptr, Null_Ptr,
         First_Marshaller'Address, new Data_Type_Record'(Callback, null),
         Null_Address,
         Pixmap_None, Null_Address, To_Gint (Configurable_Item_Find),
         0, Null_Address);
   end UI_Info_Menu_Find_Item;

   ----------------------------
   -- UI_Info_Menu_Game_Tree --
   ----------------------------

   function UI_Info_Menu_Game_Tree
     (Tree : UI_Info_Array_Access) return UI_Info is
   begin
      return
        (UI_Subtree_Stock, New_String (-"_Game"), Null_Ptr,
         Tree.all'Address, null, Null_Address,
         Pixmap_None, Null_Address, 0, 0, Null_Address);
   end UI_Info_Menu_Game_Tree;

   ----------------------------
   -- UI_Info_Menu_Help_Tree --
   ----------------------------

   function UI_Info_Menu_Help_Tree
     (Tree : UI_Info_Array_Access) return UI_Info is
   begin
      return
        (UI_Subtree_Stock, New_String (-"_Help"), Null_Ptr,
         Tree.all'Address, null, Null_Address,
         Pixmap_None, Null_Address, 0, 0, Null_Address);
   end UI_Info_Menu_Help_Tree;

   ----------------------------
   -- UI_Info_Menu_Hint_Item --
   ----------------------------

   function UI_Info_Menu_Hint_Item
     (Callback : Generic_Callback) return UI_Info is
   begin
      return
        (UI_Item_Configurable, Null_Ptr, Null_Ptr,
         First_Marshaller'Address, new Data_Type_Record'(Callback, null),
         Null_Address,
         Pixmap_None, Null_Address, To_Gint (Configurable_Item_Hint),
         0, Null_Address);
   end UI_Info_Menu_Hint_Item;

   --------------------------------
   -- UI_Info_Menu_New_Game_Item --
   --------------------------------

   function UI_Info_Menu_New_Game_Item
     (Callback : Generic_Callback) return UI_Info is
   begin
      return
        (UI_Item_Configurable, Null_Ptr, Null_Ptr,
         First_Marshaller'Address, new Data_Type_Record'(Callback, null),
         Null_Address,
         Pixmap_None, Null_Address, To_Gint (Configurable_Item_New_Game),
         0, Null_Address);
   end UI_Info_Menu_New_Game_Item;

   ---------------------------
   -- UI_Info_Menu_New_Item --
   ---------------------------

   function UI_Info_Menu_New_Item
     (Label    : String;
      Tooltip  : String;
      Callback : Generic_Callback) return UI_Info is
   begin
      return
        (UI_Item_Configurable, New_String (Label), New_String (Tooltip),
         First_Marshaller'Address, new Data_Type_Record'(Callback, null),
         Null_Address,
         Pixmap_None, Null_Address, To_Gint (Configurable_Item_New),
         0, Null_Address);
   end UI_Info_Menu_New_Item;

   ------------------------------
   -- UI_Info_Menu_New_Subtree --
   ------------------------------

   function UI_Info_Menu_New_Subtree
     (Tree : UI_Info_Array_Access) return UI_Info is
   begin
      return
        (UI_Subtree_Stock, New_String (-"_New"), Null_Ptr,
         Tree.all'Address, null, Null_Address,
         Pixmap_Stock, Menu_New_String'Address, Gint (Key_Name_New),
         Key_Mod_New, Null_Address);
   end UI_Info_Menu_New_Subtree;

   ----------------------------------
   -- UI_Info_Menu_New_Window_Item --
   ----------------------------------

   function UI_Info_Menu_New_Window_Item
     (Callback : Generic_Callback) return UI_Info is
   begin
      return
        (UI_Item_Configurable, Null_Ptr, Null_Ptr,
         First_Marshaller'Address, new Data_Type_Record'(Callback, null),
         Null_Address,
         Pixmap_None, Null_Address, To_Gint (Configurable_Item_New_Window),
         0, Null_Address);
   end UI_Info_Menu_New_Window_Item;

   ----------------------------
   -- UI_Info_Menu_Open_Item --
   ----------------------------

   function UI_Info_Menu_Open_Item
     (Callback : Generic_Callback) return UI_Info is
   begin
      return
        (UI_Item_Configurable, Null_Ptr, Null_Ptr,
         First_Marshaller'Address, new Data_Type_Record'(Callback, null),
         Null_Address,
         Pixmap_None, Null_Address, To_Gint (Configurable_Item_Open),
         0, Null_Address);
   end UI_Info_Menu_Open_Item;

   -----------------------------
   -- UI_Info_Menu_Paste_Item --
   -----------------------------

   function UI_Info_Menu_Paste_Item
     (Callback : Generic_Callback) return UI_Info is
   begin
      return
        (UI_Item_Configurable, Null_Ptr, Null_Ptr,
         First_Marshaller'Address, new Data_Type_Record'(Callback, null),
         Null_Address,
         Pixmap_None, Null_Address, To_Gint (Configurable_Item_Paste),
         0, Null_Address);
   end UI_Info_Menu_Paste_Item;

   ----------------------------------
   -- UI_Info_Menu_Pause_Game_Item --
   ----------------------------------

   function UI_Info_Menu_Pause_Game_Item
     (Callback : Generic_Callback) return UI_Info is
   begin
      return
        (UI_Item_Configurable, Null_Ptr, Null_Ptr,
         First_Marshaller'Address, new Data_Type_Record'(Callback, null),
         Null_Address,
         Pixmap_None, Null_Address, To_Gint (Configurable_Item_Pause_Game),
         0, Null_Address);
   end UI_Info_Menu_Pause_Game_Item;

   -----------------------------------
   -- UI_Info_Menu_Preferences_Item --
   -----------------------------------

   function UI_Info_Menu_Preferences_Item
     (Callback : Generic_Callback) return UI_Info is
   begin
      return
        (UI_Item_Configurable, Null_Ptr, Null_Ptr,
         First_Marshaller'Address, new Data_Type_Record'(Callback, null),
         Null_Address,
         Pixmap_None, Null_Address, To_Gint (Configurable_Item_Preferences),
         0, Null_Address);
   end UI_Info_Menu_Preferences_Item;

   -----------------------------
   -- UI_Info_Menu_Print_Item --
   -----------------------------

   function UI_Info_Menu_Print_Item
     (Callback : Generic_Callback) return UI_Info is
   begin
      return
        (UI_Item_Configurable, Null_Ptr, Null_Ptr,
         First_Marshaller'Address, new Data_Type_Record'(Callback, null),
         Null_Address,
         Pixmap_None, Null_Address, To_Gint (Configurable_Item_Print),
         0, Null_Address);
   end UI_Info_Menu_Print_Item;

   -----------------------------------
   -- UI_Info_Menu_Print_Setup_Item --
   -----------------------------------

   function UI_Info_Menu_Print_Setup_Item
     (Callback : Generic_Callback) return UI_Info is
   begin
      return
        (UI_Item_Configurable, Null_Ptr, Null_Ptr,
         First_Marshaller'Address, new Data_Type_Record'(Callback, null),
         Null_Address,
         Pixmap_None, Null_Address, To_Gint (Configurable_Item_Print_Setup),
         0, Null_Address);
   end UI_Info_Menu_Print_Setup_Item;

   ----------------------------------
   -- UI_Info_Menu_Properties_Item --
   ----------------------------------

   function UI_Info_Menu_Properties_Item
     (Callback : Generic_Callback) return UI_Info is
   begin
      return
        (UI_Item_Configurable, Null_Ptr, Null_Ptr,
         First_Marshaller'Address, new Data_Type_Record'(Callback, null),
         Null_Address,
         Pixmap_None, Null_Address, To_Gint (Configurable_Item_Properties),
         0, Null_Address);
   end UI_Info_Menu_Properties_Item;

   ----------------------------
   -- UI_Info_Menu_Redo_Item --
   ----------------------------

   function UI_Info_Menu_Redo_Item
     (Callback : Generic_Callback) return UI_Info is
   begin
      return
        (UI_Item_Configurable, Null_Ptr, Null_Ptr,
         First_Marshaller'Address, new Data_Type_Record'(Callback, null),
         Null_Address,
         Pixmap_None, Null_Address, To_Gint (Configurable_Item_Redo),
         0, Null_Address);
   end UI_Info_Menu_Redo_Item;

   ---------------------------------
   -- UI_Info_Menu_Redo_Move_Item --
   ---------------------------------

   function UI_Info_Menu_Redo_Move_Item
     (Callback : Generic_Callback) return UI_Info is
   begin
      return
        (UI_Item_Configurable, Null_Ptr, Null_Ptr,
         First_Marshaller'Address, new Data_Type_Record'(Callback, null),
         Null_Address,
         Pixmap_None, Null_Address, To_Gint (Configurable_Item_Redo_Move),
         0, Null_Address);
   end UI_Info_Menu_Redo_Move_Item;

   -------------------------------
   -- UI_Info_Menu_Replace_Item --
   -------------------------------

   function UI_Info_Menu_Replace_Item
     (Callback : Generic_Callback) return UI_Info is
   begin
      return
        (UI_Item_Configurable, Null_Ptr, Null_Ptr,
         First_Marshaller'Address, new Data_Type_Record'(Callback, null),
         Null_Address,
         Pixmap_None, Null_Address, To_Gint (Configurable_Item_Replace),
         0, Null_Address);
   end UI_Info_Menu_Replace_Item;

   ------------------------------------
   -- UI_Info_Menu_Restart_Game_Item --
   ------------------------------------

   function UI_Info_Menu_Restart_Game_Item
     (Callback : Generic_Callback) return UI_Info is
   begin
      return
        (UI_Item_Configurable, Null_Ptr, Null_Ptr,
         First_Marshaller'Address, new Data_Type_Record'(Callback, null),
         Null_Address,
         Pixmap_None, Null_Address, To_Gint (Configurable_Item_Restart_Game),
         0, Null_Address);
   end UI_Info_Menu_Restart_Game_Item;

   ------------------------------
   -- UI_Info_Menu_Revert_Item --
   ------------------------------

   function UI_Info_Menu_Revert_Item
     (Callback : Generic_Callback) return UI_Info is
   begin
      return
        (UI_Item_Configurable, Null_Ptr, Null_Ptr,
         First_Marshaller'Address, new Data_Type_Record'(Callback, null),
         Null_Address,
         Pixmap_None, Null_Address, To_Gint (Configurable_Item_Revert),
         0, Null_Address);
   end UI_Info_Menu_Revert_Item;

   -------------------------------
   -- UI_Info_Menu_Save_As_Item --
   -------------------------------

   function UI_Info_Menu_Save_As_Item
     (Callback : Generic_Callback) return UI_Info is
   begin
      return
        (UI_Item_Configurable, Null_Ptr, Null_Ptr,
         First_Marshaller'Address, new Data_Type_Record'(Callback, null),
         Null_Address,
         Pixmap_None, Null_Address, To_Gint (Configurable_Item_Save_As),
         0, Null_Address);
   end UI_Info_Menu_Save_As_Item;

   ----------------------------
   -- UI_Info_Menu_Save_Item --
   ----------------------------

   function UI_Info_Menu_Save_Item
     (Callback : Generic_Callback) return UI_Info is
   begin
      return
        (UI_Item_Configurable, Null_Ptr, Null_Ptr,
         First_Marshaller'Address, new Data_Type_Record'(Callback, null),
         Null_Address,
         Pixmap_None, Null_Address, To_Gint (Configurable_Item_Save),
         0, Null_Address);
   end UI_Info_Menu_Save_Item;

   ------------------------------
   -- UI_Info_Menu_Scores_Item --
   ------------------------------

   function UI_Info_Menu_Scores_Item
     (Callback : Generic_Callback) return UI_Info is
   begin
      return
        (UI_Item_Configurable, Null_Ptr, Null_Ptr,
         First_Marshaller'Address, new Data_Type_Record'(Callback, null),
         Null_Address,
         Pixmap_None, Null_Address, To_Gint (Configurable_Item_Scores),
         0, Null_Address);
   end UI_Info_Menu_Scores_Item;

   ----------------------------------
   -- UI_Info_Menu_Select_All_Item --
   ----------------------------------

   function UI_Info_Menu_Select_All_Item
     (Callback : Generic_Callback) return UI_Info is
   begin
      return
        (UI_Item_Configurable, Null_Ptr, Null_Ptr,
         First_Marshaller'Address, new Data_Type_Record'(Callback, null),
         Null_Address,
         Pixmap_None, Null_Address, To_Gint (Configurable_Item_Select_All),
         0, Null_Address);
   end UI_Info_Menu_Select_All_Item;

   --------------------------------
   -- UI_Info_Menu_Settings_Tree --
   --------------------------------

   function UI_Info_Menu_Settings_Tree
     (Tree : UI_Info_Array_Access) return UI_Info is
   begin
      return
        (UI_Subtree_Stock, New_String (-"_Settings"), Null_Ptr,
         Tree.all'Address, null, Null_Address,
         Pixmap_None, Null_Address, 0, 0, Null_Address);
   end UI_Info_Menu_Settings_Tree;

   ----------------------------
   -- UI_Info_Menu_Undo_Item --
   ----------------------------

   function UI_Info_Menu_Undo_Item
     (Callback : Generic_Callback) return UI_Info is
   begin
      return
        (UI_Item_Configurable, Null_Ptr, Null_Ptr,
         First_Marshaller'Address, new Data_Type_Record'(Callback, null),
         Null_Address,
         Pixmap_None, Null_Address, To_Gint (Configurable_Item_Undo),
         0, Null_Address);
   end UI_Info_Menu_Undo_Item;

   ---------------------------------
   -- UI_Info_Menu_Undo_Move_Item --
   ---------------------------------

   function UI_Info_Menu_Undo_Move_Item
     (Callback : Generic_Callback) return UI_Info is
   begin
      return
        (UI_Item_Configurable, Null_Ptr, Null_Ptr,
         First_Marshaller'Address, new Data_Type_Record'(Callback, null),
         Null_Address, Pixmap_None, Null_Address,
         To_Gint (Configurable_Item_Undo_Move), 0, Null_Address);
   end UI_Info_Menu_Undo_Move_Item;

   ----------------------------
   -- UI_Info_Menu_View_Tree --
   ----------------------------

   function UI_Info_Menu_View_Tree
     (Tree : UI_Info_Array_Access) return UI_Info is
   begin
      return
        (UI_Subtree_Stock, New_String (-"_View"), Null_Ptr,
         Tree.all'Address, null, Null_Address,
         Pixmap_None, Null_Address, 0, 0, Null_Address);
   end UI_Info_Menu_View_Tree;

   -------------------------------
   -- UI_Info_Menu_Windows_Tree --
   -------------------------------

   function UI_Info_Menu_Windows_Tree
     (Tree : UI_Info_Array_Access) return UI_Info is
   begin
      return
        (UI_Subtree_Stock, New_String (-"_Windows"), Null_Ptr,
         Tree.all'Address, null, Null_Address,
         Pixmap_None, Null_Address, 0, 0, Null_Address);
   end UI_Info_Menu_Windows_Tree;

   -----------------------
   -- UI_Info_Radioitem --
   -----------------------

   function UI_Info_Radioitem
     (Label    : String;
      Tooltip  : String;
      Callback : Generic_Callback;
      Xpm_Data : Chars_Ptr_Array) return UI_Info is
   begin
      return
        (UI_Item, New_String (Label), New_String (Tooltip),
         First_Marshaller'Address, new Data_Type_Record'(Callback, null),
         Null_Address, Pixmap_Data, Xpm_Data'Address, 0, 0, Null_Address);
   end UI_Info_Radioitem;

   ---------------------
   -- UI_Info_Subtree --
   ---------------------

   function UI_Info_Subtree
     (Label : String;
      Tree  : UI_Info_Array_Access) return UI_Info is
   begin
      return
        (UI_Subtree, New_String (Label), Null_Ptr,
         Tree.all'Address, null, Null_Address,
         Pixmap_None, Null_Address, 0, 0, Null_Address);
   end UI_Info_Subtree;

   --------------------------
   -- UI_Info_Subtree_Hint --
   --------------------------

   function UI_Info_Subtree_Hint
     (Label : String;
      Hint  : String;
      Tree  : UI_Info_Array_Access) return UI_Info is
   begin
      return
        (UI_Subtree, New_String (Label), New_String (Hint),
         Tree.all'Address, null, Null_Address,
         Pixmap_None, Null_Address, 0, 0, Null_Address);
   end UI_Info_Subtree_Hint;

   ---------------------------
   -- UI_Info_Subtree_Stock --
   ---------------------------

   function UI_Info_Subtree_Stock
     (Label    : String;
      Tree     : UI_Info_Array_Access;
      Stock_Id : String) return UI_Info is
   begin
      return
        (UI_Subtree, New_String (Label), Null_Ptr,
         Tree.all'Address, null, Null_Address,
         Pixmap_Stock, Stock_Id'Address, 0, 0, Null_Address);
   end UI_Info_Subtree_Stock;

   ------------------------
   -- UI_Info_Toggleitem --
   ------------------------

   function UI_Info_Toggleitem
     (Label    : String;
      Tooltip  : String;
      Callback : Generic_Callback;
      Xpm_Data : Chars_Ptr_Array) return UI_Info is
   begin
      return
        (UI_Toggleitem, New_String (Label), New_String (Tooltip),
         First_Marshaller'Address, new Data_Type_Record'(Callback, null),
         Null_Address, Pixmap_Data, Xpm_Data'Address, 0, 0, Null_Address);
   end UI_Info_Toggleitem;

   -----------------
   -- UI_New_Item --
   -----------------

   function UI_New_Item
     (Label           : String;
      Hint            : String := "";
      Callback        : Generic_Callback := null;
      Pixmap_Type     : UI_Pixmap_Type := Pixmap_None;
      Pixmap_Info     : String := "";
      Accelerator_Key : Gdk_Key_Type := 0;
      Ac_Mods         : Gdk_Modifier_Type := 0) return UI_Info
   is
      The_Hint : Chars_Ptr;
      Info     : aliased constant String := Pixmap_Info & ASCII.NUL;
      Pixmap   : System.Address;

   begin
      if Hint'Length = 0 then
         The_Hint := Null_Ptr;
      else
         The_Hint := New_String (Hint);
      end if;

      if Pixmap_Info'Length = 0 then
         Pixmap := Null_Address;
      else
         Pixmap := Info'Address;
      end if;

      return
        (UI_Item, New_String (Label), The_Hint,
         First_Marshaller'Address, new Data_Type_Record'(Callback, null),
         Null_Address, Pixmap_Type, Pixmap,
         Gint (Accelerator_Key), Ac_Mods, Null_Address);
   end UI_New_Item;

   --------------------
   -- UI_New_Subtree --
   --------------------

   function UI_New_Subtree
     (Label           : String;
      Info            : UI_Info_Array_Access;
      Pixmap_Type     : UI_Pixmap_Type := Pixmap_None;
      Pixmap_Info     : String := "";
      Accelerator_Key : Gdk_Key_Type := 0;
      Ac_Mods         : Gdk_Modifier_Type := 0) return UI_Info
   is
      Str    : aliased constant String := Pixmap_Info & ASCII.NUL;
      Pixmap : System.Address;

   begin
      if Pixmap_Info'Length = 0 then
         Pixmap := Null_Address;
      else
         Pixmap := Str'Address;
      end if;

      return
        (UI_Subtree, New_String (Label), Null_Ptr, Info.all'Address,
         null, Null_Address, Pixmap_Type, Pixmap,
         Gint (Accelerator_Key), Ac_Mods, Null_Address);
   end UI_New_Subtree;

end Gnome.App_Helper;
