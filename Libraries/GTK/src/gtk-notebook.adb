-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                 Copyright (C) 2000-2013, AdaCore                  --
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

with Gtk.Container;        use Gtk.Container;
with Gtk.Widget;           use Gtk.Widget;
with Gtk.Enums;            use Gtk.Enums;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;

with Glib.Type_Conversion_Hooks;

package body Gtk.Notebook is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Notebook_Record);
   pragma Warnings (Off, Type_Conversion);

   -----------------
   -- Append_Page --
   -----------------

   procedure Append_Page
     (Notebook  : access Gtk_Notebook_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tab_Label : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
        (Notebook  : System.Address;
         Child     : System.Address;
         Tab_Label : System.Address);
      pragma Import (C, Internal, "gtk_notebook_append_page");

   begin
      Internal (Get_Object (Notebook),
                Get_Object (Child),
                Get_Object (Tab_Label));
   end Append_Page;

   -----------------
   -- Append_Page --
   -----------------

   procedure Append_Page
     (Notebook  : access Gtk_Notebook_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
        (Notebook  : System.Address;
         Child     : System.Address;
         Tab_Label : System.Address := System.Null_Address);
      pragma Import (C, Internal, "gtk_notebook_append_page");

   begin
      Internal (Get_Object (Notebook), Get_Object (Child));
   end Append_Page;

   ----------------------
   -- Append_Page_Menu --
   ----------------------

   procedure Append_Page_Menu
     (Notebook   : access Gtk_Notebook_Record;
      Child      : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tab_Label  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Menu_Label : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
        (Notebook   : System.Address;
         Child      : System.Address;
         Tab_Label  : System.Address;
         Menu_Label : System.Address);
      pragma Import (C, Internal, "gtk_notebook_append_page_menu");

   begin
      Internal (Get_Object (Notebook),
                Get_Object (Child),
                Get_Object (Tab_Label),
                Get_Object (Menu_Label));
   end Append_Page_Menu;

   ----------------------
   -- Get_Current_Page --
   ----------------------

   function Get_Current_Page
     (Notebook : access Gtk_Notebook_Record) return Gint
   is
      function Internal (Notebook : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_notebook_get_current_page");

   begin
      return Internal (Get_Object (Notebook));
   end Get_Current_Page;

   ------------------
   -- Get_Children --
   ------------------

   function Get_Children
     (Widget : access Gtk_Notebook_Record) return Page_List.Glist
   is
      function Internal (Widget : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_notebook_get_children");

      use Page_List;
      List : Page_List.Glist;

   begin
      Set_Object (List, Internal (Get_Object (Widget)));
      return List;
   end Get_Children;

   ---------------
   -- Get_Group --
   ---------------

   function Get_Group (Notebook : access Gtk_Notebook_Record)
      return Gtk_Notebook_Group
   is
      function Internal (Notebook : System.Address) return Gtk_Notebook_Group;
      pragma Import (C, Internal, "gtk_notebook_get_group");
   begin
      return Internal (Get_Object (Notebook));
   end Get_Group;

   --------------------
   -- Get_Menu_Label --
   --------------------

   function Get_Menu_Label
     (Notebook : access Gtk_Notebook_Record;
      Child    : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Gtk.Widget.Gtk_Widget
   is
      function Internal
        (Notebook : System.Address;
         Child    : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_notebook_get_menu_label");

   begin
      return Convert (Internal (Get_Object (Notebook), Get_Object (Child)));
   end Get_Menu_Label;

   -------------------------
   -- Get_Menu_Label_Text --
   -------------------------

   function Get_Menu_Label_Text
     (Notebook : access Gtk_Notebook_Record;
      Child    : access Gtk.Widget.Gtk_Widget_Record'Class) return UTF8_String
   is
      function Internal
        (Widget : System.Address;
         Child  : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_notebook_get_menu_label_text");

   begin
      return Value (Internal (Get_Object (Notebook), Get_Object (Child)));
   end Get_Menu_Label_Text;

   ---------------------
   -- Get_Show_Border --
   ---------------------

   function Get_Show_Border
     (Notebook : access Gtk_Notebook_Record) return Boolean
   is
      function Internal
        (Notebook : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_notebook_get_show_border");

   begin
      return Internal (Get_Object (Notebook)) /= 0;
   end Get_Show_Border;

   -------------------
   -- Get_Show_Tabs --
   -------------------

   function Get_Show_Tabs
     (Notebook : access Gtk_Notebook_Record) return Boolean
   is
      function Internal
        (Notebook : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_notebook_get_show_tabs");

   begin
      return Internal (Get_Object (Notebook)) /= 0;
   end Get_Show_Tabs;

   --------------------
   -- Get_Scrollable --
   --------------------

   function Get_Scrollable
     (Notebook : access Gtk_Notebook_Record) return Boolean
   is
      function Internal
        (Notebook : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_notebook_get_scrollable");

   begin
      return Internal (Get_Object (Notebook)) /= 0;
   end Get_Scrollable;

   -------------------
   -- Get_Tab_Label --
   -------------------

   function Get_Tab_Label
     (Notebook  : access Gtk_Notebook_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Gtk.Widget.Gtk_Widget
   is
      function Internal
        (Widget : System.Address;
         Child  : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_notebook_get_tab_label");

   begin
      return Convert (Internal (Get_Object (Notebook), Get_Object (Child)));
   end Get_Tab_Label;

   ------------------------
   -- Get_Tab_Label_Text --
   ------------------------

   function Get_Tab_Label_Text
     (Notebook : access Gtk_Notebook_Record;
      Child    : access Gtk.Widget.Gtk_Widget_Record'Class) return UTF8_String
   is
      function Internal
        (Widget : System.Address;
         Child  : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_notebook_get_tab_label_text");

   begin
      return Value (Internal (Get_Object (Notebook), Get_Object (Child)));
   end Get_Tab_Label_Text;

   -----------------
   -- Get_Tab_Pos --
   -----------------

   function Get_Tab_Pos
     (Widget : access Gtk_Notebook_Record) return Gtk_Position_Type
   is
      function Internal (Widget : System.Address) return Gtk_Position_Type;
      pragma Import (C, Internal, "gtk_notebook_get_tab_pos");

   begin
      return Internal (Get_Object (Widget));
   end Get_Tab_Pos;

   ------------------
   -- Get_Nth_Page --
   ------------------

   function Get_Nth_Page
     (Widget   : access Gtk_Notebook_Record'Class;
      Page_Num : Gint) return Gtk.Widget.Gtk_Widget
   is
      function Internal
        (Widget : System.Address; Page_Num : Gint) return System.Address;
      pragma Import (C, Internal, "gtk_notebook_get_nth_page");

   begin
      return Convert (Internal (Get_Object (Widget), Page_Num));
   end Get_Nth_Page;

   --------------
   -- Page_Num --
   --------------

   function Page_Num
     (Widget : access Gtk_Notebook_Record'Class;
      Child  : access Gtk_Widget_Record'Class) return Gint
   is
      function Internal
        (Widget : System.Address; Child : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_notebook_page_num");

   begin
      return Internal (Get_Object (Widget), Get_Object (Child));
   end Page_Num;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Notebook) is
   begin
      Widget := new Gtk_Notebook_Record;
      Gtk.Notebook.Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Notebook_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_notebook_new");

   begin
      Set_Object (Widget, Internal);
   end Initialize;

   -----------------
   -- Insert_Page --
   -----------------

   procedure Insert_Page
     (Notebook  : access Gtk_Notebook_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tab_Label : access Gtk.Widget.Gtk_Widget_Record'Class;
      Position  : Gint)
   is
      procedure Internal
        (Notebook  : System.Address;
         Child     : System.Address;
         Tab_Label : System.Address;
         Position  : Gint);
      pragma Import (C, Internal, "gtk_notebook_insert_page");

   begin
      Internal (Get_Object (Notebook),
                Get_Object (Child),
                Get_Object (Tab_Label),
                Position);
   end Insert_Page;

   ----------------------
   -- Insert_Page_Menu --
   ----------------------

   procedure Insert_Page_Menu
     (Notebook   : access Gtk_Notebook_Record;
      Child      : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tab_Label  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Menu_Label : access Gtk.Widget.Gtk_Widget_Record'Class;
      Position   : Gint)
   is
      procedure Internal
        (Notebook   : System.Address;
         Child      : System.Address;
         Tab_Label  : System.Address;
         Menu_Label : System.Address;
         Position   : Gint);
      pragma Import (C, Internal, "gtk_notebook_insert_page_menu");

   begin
      Internal (Get_Object (Notebook),
                Get_Object (Child),
                Get_Object (Tab_Label),
                Get_Object (Menu_Label),
                Position);
   end Insert_Page_Menu;

   ---------------
   -- Next_Page --
   ---------------

   procedure Next_Page (Notebook : access Gtk_Notebook_Record) is
      procedure Internal (Notebook : System.Address);
      pragma Import (C, Internal, "gtk_notebook_next_page");

   begin
      Internal (Get_Object (Notebook));
   end Next_Page;

   -------------------
   -- Popup_Disable --
   -------------------

   procedure Popup_Disable (Notebook : access Gtk_Notebook_Record) is
      procedure Internal (Notebook : System.Address);
      pragma Import (C, Internal, "gtk_notebook_popup_disable");

   begin
      Internal (Get_Object (Notebook));
   end Popup_Disable;

   ------------------
   -- Popup_Enable --
   ------------------

   procedure Popup_Enable (Notebook : access Gtk_Notebook_Record) is
      procedure Internal (Notebook : System.Address);
      pragma Import (C, Internal, "gtk_notebook_popup_enable");

   begin
      Internal (Get_Object (Notebook));
   end Popup_Enable;

   ------------------
   -- Prepend_Page --
   ------------------

   procedure Prepend_Page
     (Notebook  : access Gtk_Notebook_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tab_Label : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
        (Notebook  : System.Address;
         Child     : System.Address;
         Tab_Label : System.Address);
      pragma Import (C, Internal, "gtk_notebook_prepend_page");

   begin
      Internal (Get_Object (Notebook),
                Get_Object (Child),
                Get_Object (Tab_Label));
   end Prepend_Page;

   -----------------------
   -- Prepend_Page_Menu --
   -----------------------

   procedure Prepend_Page_Menu
     (Notebook   : access Gtk_Notebook_Record;
      Child      : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tab_Label  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Menu_Label : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
        (Notebook   : System.Address;
         Child      : System.Address;
         Tab_Label  : System.Address;
         Menu_Label : System.Address);
      pragma Import (C, Internal, "gtk_notebook_prepend_page_menu");

   begin
      Internal (Get_Object (Notebook),
                Get_Object (Child),
                Get_Object (Tab_Label),
                Get_Object (Menu_Label));
   end Prepend_Page_Menu;

   ---------------
   -- Prev_Page --
   ---------------

   procedure Prev_Page (Notebook : access Gtk_Notebook_Record) is
      procedure Internal (Notebook : System.Address);
      pragma Import (C, Internal, "gtk_notebook_prev_page");

   begin
      Internal (Get_Object (Notebook));
   end Prev_Page;

   -----------------------------
   -- Query_Tab_Label_Packing --
   -----------------------------

   procedure Query_Tab_Label_Packing
     (Notebook   : access Gtk_Notebook_Record;
      Child      : access Gtk.Widget.Gtk_Widget_Record'Class;
      Expand     : out Boolean;
      Fill       : out Boolean;
      Pack_Type  : out Gtk_Pack_Type)
   is
      procedure Internal
        (Notebook  : System.Address;
         Child     : System.Address;
         Expand    : out Gint;
         Fill      : out Gint;
         Pack_Type : out Gint);
      pragma Import (C, Internal, "gtk_notebook_query_tab_label_packing");

      Expand_Ptr : Gint;
      Fill_Ptr   : Gint;
      Pack_Ptr   : Gint;

   begin
      Internal (Get_Object (Notebook), Get_Object (Child),
                Expand_Ptr, Fill_Ptr, Pack_Ptr);
      Expand := Expand_Ptr /= 0;
      Fill   := Fill_Ptr /= 0;
      Pack_Type := Gtk_Pack_Type'Val (Pack_Ptr);
   end Query_Tab_Label_Packing;

   -----------------
   -- Remove_Page --
   -----------------

   procedure Remove_Page
     (Notebook : access Gtk_Notebook_Record; Page_Num : Gint)
   is
      procedure Internal (Notebook : System.Address; Page_Num : Gint);
      pragma Import (C, Internal, "gtk_notebook_remove_page");

   begin
      Internal (Get_Object (Notebook), Page_Num);
   end Remove_Page;

   --------------------------
   -- Set_Homogeneous_Tabs --
   --------------------------

   procedure Set_Homogeneous_Tabs
     (Notebook    : access Gtk_Notebook_Record;
      Homogeneous : Boolean := True)
   is
      procedure Internal (Notebook : System.Address; Homogeneous : Gint);
      pragma Import (C, Internal, "gtk_notebook_set_homogeneous_tabs");

   begin
      Internal (Get_Object (Notebook), Boolean'Pos (Homogeneous));
   end Set_Homogeneous_Tabs;

   ----------------------
   -- Set_Current_Page --
   ----------------------

   procedure Set_Current_Page
     (Notebook : access Gtk_Notebook_Record; Page_Num : Gint := -1)
   is
      procedure Internal (Notebook : System.Address; Page_Num : Gint);
      pragma Import (C, Internal, "gtk_notebook_set_current_page");

   begin
      Internal (Get_Object (Notebook), Page_Num);
   end Set_Current_Page;

   ---------------
   -- Set_Group --
   ---------------

   procedure Set_Group
     (Notebook : access Gtk_Notebook_Record;
      Group    : Gtk_Notebook_Group)
   is
      procedure Internal
        (Notebook : System.Address;
         Group    : Gtk_Notebook_Group);
      pragma Import (C, Internal, "gtk_notebook_set_group");
   begin
      Internal (Get_Object (Notebook), Group);
   end Set_Group;

   --------------------
   -- Set_Scrollable --
   --------------------

   procedure Set_Scrollable
     (Notebook   : access Gtk_Notebook_Record;
      Scrollable : Boolean := True)
   is
      procedure Internal (Notebook : System.Address; Scrollable : Gint);
      pragma Import (C, Internal, "gtk_notebook_set_scrollable");

   begin
      Internal (Get_Object (Notebook), Boolean'Pos (Scrollable));
   end Set_Scrollable;

   ---------------------
   -- Set_Show_Border --
   ---------------------

   procedure Set_Show_Border
     (Notebook    : access Gtk_Notebook_Record;
      Show_Border : Boolean := True)
   is
      procedure Internal (Notebook : System.Address; Show_Border : Gint);
      pragma Import (C, Internal, "gtk_notebook_set_show_border");

   begin
      Internal (Get_Object (Notebook), Boolean'Pos (Show_Border));
   end Set_Show_Border;

   ---------------------------
   -- Set_Tab_Label_Packing --
   ---------------------------

   procedure Set_Tab_Label_Packing
     (Notebook  : access Gtk_Notebook_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Expand    : Boolean;
      Fill      : Boolean;
      Pack_Type : Gtk_Pack_Type)
   is
      procedure Internal
        (Notebook  : System.Address;
         Child     : System.Address;
         Expand    : Gint;
         Fill      : Gint;
         Pack_Type : Gtk_Pack_Type);
      pragma Import (C, Internal, "gtk_notebook_set_tab_label_packing");

   begin
      Internal
        (Get_Object (Notebook),
         Get_Object (Child),
         Boolean'Pos (Expand),
         Boolean'Pos (Fill),
         Pack_Type);
   end Set_Tab_Label_Packing;

   -------------------
   -- Set_Show_Tabs --
   -------------------

   procedure Set_Show_Tabs
     (Notebook  : access Gtk_Notebook_Record;
      Show_Tabs : Boolean := True)
   is
      procedure Internal (Notebook : System.Address; Show_Tabs : Gint);
      pragma Import (C, Internal, "gtk_notebook_set_show_tabs");

   begin
      Internal (Get_Object (Notebook), Boolean'Pos (Show_Tabs));
   end Set_Show_Tabs;

   -------------
   -- Set_Tab --
   -------------

   procedure Set_Tab
     (Notebook  : access Gtk_Notebook_Record;
      Page_Num  : Gint;
      Tab_Label : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Page : Gtk_Widget;
   begin
      Page := Get_Nth_Page (Notebook, Page_Num);

      if Page /= null then
         Set_Tab_Label (Notebook, Page, Tab_Label);
      end if;
   end Set_Tab;

   --------------------
   -- Set_Tab_Border --
   --------------------

   procedure Set_Tab_Border
     (Notebook : access Gtk_Notebook_Record; Border_Width : Gint)
   is
      procedure Internal (Notebook : System.Address; Border_Width : Gint);
      pragma Import (C, Internal, "gtk_notebook_set_tab_border");

   begin
      Internal (Get_Object (Notebook), Border_Width);
   end Set_Tab_Border;

   ---------------------
   -- Set_Tab_Hborder --
   ---------------------

   procedure Set_Tab_Hborder
     (Notebook : access Gtk_Notebook_Record; Border_Width : Gint)
   is
      procedure Internal (Notebook : System.Address; Border_Width : Gint);
      pragma Import (C, Internal, "gtk_notebook_set_tab_hborder");

   begin
      Internal (Get_Object (Notebook), Border_Width);
   end Set_Tab_Hborder;

   --------------------
   -- Set_Tab_Border --
   --------------------

   procedure Set_Tab_Vborder
     (Notebook : access Gtk_Notebook_Record; Border_Width : Gint)
   is
      procedure Internal (Notebook : System.Address; Border_Width : Gint);
      pragma Import (C, Internal, "gtk_notebook_set_tab_vborder");

   begin
      Internal (Get_Object (Notebook), Border_Width);
   end Set_Tab_Vborder;

   -----------------
   -- Set_Tab_Pos --
   -----------------

   procedure Set_Tab_Pos
     (Notebook : access Gtk_Notebook_Record; Pos : Gtk_Position_Type)
   is
      procedure Internal (Notebook : System.Address; Pos : Gtk_Position_Type);
      pragma Import (C, Internal, "gtk_notebook_set_tab_pos");

   begin
      Internal (Get_Object (Notebook), Pos);
   end Set_Tab_Pos;

   -------------------
   -- Set_Tab_Label --
   -------------------

   procedure Set_Tab_Label
     (Notebook  : access Gtk_Notebook_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tab_Label : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
        (Notebook  : System.Address;
         Child     : System.Address;
         Tab_Label : System.Address);
      pragma Import (C, Internal, "gtk_notebook_set_tab_label");

   begin
      Internal (Get_Object (Notebook), Get_Object (Child),
                Get_Object (Tab_Label));
   end Set_Tab_Label;

   ------------------------
   -- Set_Tab_Label_Text --
   ------------------------

   procedure Set_Tab_Label_Text
     (Notebook : access Gtk_Notebook_Record;
      Child    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tab_Text : UTF8_String)
   is
      procedure Internal
        (Notebook : System.Address;
         Child    : System.Address;
         Tab_Text : UTF8_String);
      pragma Import (C, Internal, "gtk_notebook_set_tab_label_text");

   begin
      Internal (Get_Object (Notebook), Get_Object (Child),
                Tab_Text & ASCII.NUL);
   end Set_Tab_Label_Text;

   --------------------
   -- Set_Menu_Label --
   --------------------

   procedure Set_Menu_Label
     (Notebook   : access Gtk_Notebook_Record;
      Child      : access Gtk.Widget.Gtk_Widget_Record'Class;
      Menu_Label : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
        (Notebook   : System.Address;
         Child      : System.Address;
         Menu_Label : System.Address);
      pragma Import (C, Internal, "gtk_notebook_set_menu_label");

   begin
      Internal (Get_Object (Notebook), Get_Object (Child),
                Get_Object (Menu_Label));
   end Set_Menu_Label;

   -------------------------
   -- Set_Menu_Label_Text --
   -------------------------

   procedure Set_Menu_Label_Text
     (Notebook  : access Gtk_Notebook_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Menu_Text : UTF8_String)
   is
      procedure Internal
        (Notebook  : System.Address;
         Child     : System.Address;
         Menu_Text : UTF8_String);
      pragma Import (C, Internal, "gtk_notebook_set_menu_label_text");

   begin
      Internal (Get_Object (Notebook), Get_Object (Child),
                Menu_Text & ASCII.NUL);
   end Set_Menu_Label_Text;

   -------------------
   -- Reorder_Child --
   -------------------

   procedure Reorder_Child
     (Notebook : access Gtk_Notebook_Record;
      Child    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Position : Gint)
   is
      procedure Internal
        (Notebook : System.Address;
         Child    : System.Address;
         Position : Gint);
      pragma Import (C, Internal, "gtk_notebook_reorder_child");

   begin
      Internal (Get_Object (Notebook), Get_Object (Child), Position);
   end Reorder_Child;

   -------------------------
   -- Get_Tab_Reorderable --
   -------------------------

   function Get_Tab_Reorderable
     (Notebook : access Gtk_Notebook_Record;
      Child    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Position : Gint)
      return Boolean
   is
      function Internal
        (Notebook : System.Address;
         Child    : System.Address;
         Position : Gint) return Gboolean;
      pragma Import (C, Internal, "gtk_notebook_get_tab_reorderable");

   begin
      return Internal
        (Get_Object (Notebook), Get_Object (Child), Position) /= 0;
   end Get_Tab_Reorderable;

   -------------------------
   -- Set_Tab_Reorderable --
   -------------------------

   procedure Set_Tab_Reorderable
     (Notebook    : access Gtk_Notebook_Record;
      Child       : access Gtk.Widget.Gtk_Widget_Record'Class;
      Reorderable : Boolean := True)
   is
      procedure Internal
        (Notebook    : System.Address;
         Child       : System.Address;
         Reorderable : Gint);
      pragma Import (C, Internal, "gtk_notebook_set_tab_reorderable");
   begin
      Internal
        (Get_Object (Notebook), Get_Object (Child), Boolean'Pos (Reorderable));
   end Set_Tab_Reorderable;

   ------------------------
   -- Get_Tab_Detachable --
   ------------------------

   function Get_Tab_Detachable
     (Notebook : access Gtk_Notebook_Record;
      Child    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Position : Gint)
      return Boolean
   is
      function Internal
        (Notebook : System.Address;
         Child    : System.Address;
         Position : Gint)
         return Gboolean;
      pragma Import (C, Internal, "gtk_notebook_get_tab_detachable");
   begin
      return Internal
        (Get_Object (Notebook), Get_Object (Child), Position) /= 0;
   end Get_Tab_Detachable;

   ------------------------
   -- Set_Tab_Detachable --
   ------------------------

   procedure Set_Tab_Detachable
     (Notebook   : access Gtk_Notebook_Record;
      Child      : access Gtk.Widget.Gtk_Widget_Record'Class;
      Detachable : Boolean := True)
   is
      procedure Internal
        (Notebook   : System.Address;
         Child      : System.Address;
         Detachable : Gint);
      pragma Import (C, Internal, "gtk_notebook_set_tab_detachable");
   begin
      Internal
        (Get_Object (Notebook), Get_Object (Child), Boolean'Pos (Detachable));
   end Set_Tab_Detachable;

   -----------------------
   -- Get_Notebook_Page --
   -----------------------

   function Get_Notebook_Page
     (Value : Glib.Values.GValue) return Gtk_Notebook_Page is
   begin
      return Gtk_Notebook_Page (Glib.Values.Get_Proxy (Value));
   end Get_Notebook_Page;

   -----------------
   -- Get_N_Pages --
   -----------------

   function Get_N_Pages
     (Notebook : access Gtk_Notebook_Record)
      return Gint
   is
      function Internal  (Notebook : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_notebook_get_n_pages");
   begin
      return Internal (Get_Object (Notebook));
   end Get_N_Pages;

   ------------------------------
   -- Set_Window_Creation_Hook --
   ------------------------------

   procedure Set_Window_Creation_Hook
     (Func    : Gtk_Notebook_Window_Creation_Func;
      Data    : System.Address;
      Destroy : Glib.G_Destroy_Notify_Address)
   is
      procedure Internal
        (Func : Gtk_Notebook_Window_Creation_Func;
         Data : System.Address;
         Destroy : G_Destroy_Notify_Address);
      pragma Import (C, Internal, "gtk_notebook_set_window_creation_hook");
   begin
      Internal (Func, Data, Destroy);
   end Set_Window_Creation_Hook;

   ------------------
   -- Set_Group_Id --
   ------------------

   procedure Set_Group_Id
     (Notebook : access Gtk_Notebook_Record; Group_Id : Gint)
   is
      procedure Internal
        (Notebook : System.Address;
         Group_Id : Gint);
      pragma Import (C, Internal, "gtk_notebook_set_group_id");
   begin
      Internal (Get_Object (Notebook), Group_Id);
   end Set_Group_Id;

   ------------------
   -- Get_Group_Id --
   ------------------

   function Get_Group_Id (Notebook : access Gtk_Notebook_Record) return Gint is
      function Internal (Notebook : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_notebook_get_group_id");
   begin
      return Internal (Get_Object (Notebook));
   end Get_Group_Id;

end Gtk.Notebook;
