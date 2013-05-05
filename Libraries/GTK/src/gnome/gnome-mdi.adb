-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                  Copyright (C) 2001-2013, AdaCore                 --
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

with Gtk.Widget; use Gtk.Widget;
with Gtk; use Gtk;
with System;

package body Gnome.MDI is

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New
     (Widget  : out Gnome_MDI;
      Appname : String;
      Title   : String) is
   begin
      Widget := new Gnome_MDI_Record;
      Initialize (Widget, Appname, Title);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget  : access Gnome_MDI_Record'Class;
      Appname : String;
      Title   : String)
   is
      function Internal
        (Appname : String;
         Title   : String)
         return System.Address;
      pragma Import (C, Internal, "gnome_mdi_new");
   begin
      Set_Object (Widget, Internal (Appname & ASCII.NUL, Title & ASCII.NUL));
   end Initialize;

   ---------------
   -- Add_Child --
   ---------------

   function Add_Child
     (MDI    : access Gnome_MDI_Record;
      Child  : access Gnome_MDI_Child_Record'Class) return Gint
   is
      function Internal
        (MDI    : System.Address;
         Child  : System.Address)
         return Gint;
      pragma Import (C, Internal, "gnome_mdi_add_child");
   begin
      return Internal (Get_Object (MDI), Get_Object (Child));
   end Add_Child;

   -----------------------
   -- Add_Toplevel_View --
   -----------------------

   function Add_Toplevel_View
     (MDI    : access Gnome_MDI_Record;
      Child  : access Gnome_MDI_Child_Record'Class) return Gint
   is
      function Internal
        (MDI    : System.Address;
         Child  : System.Address)
         return Gint;
      pragma Import (C, Internal, "gnome_mdi_add_toplevel_view");
   begin
      return Internal (Get_Object (MDI), Get_Object (Child));
   end Add_Toplevel_View;

   --------------
   -- Add_View --
   --------------

   function Add_View
     (MDI    : access Gnome_MDI_Record;
      Child  : access Gnome_MDI_Child_Record'Class)
      return Gint
   is
      function Internal
        (MDI    : System.Address;
         Child  : System.Address)
         return Gint;
      pragma Import (C, Internal, "gnome_mdi_add_view");
   begin
      return Internal (Get_Object (MDI), Get_Object (Child));
   end Add_View;

   ----------------
   -- Find_Child --
   ----------------

   function Find_Child
     (MDI    : access Gnome_MDI_Record;
      Name   : String)
      return Gnome_MDI_Child
   is
      function Internal
        (MDI  : System.Address;
         Name : String)
         return System.Address;
      pragma Import (C, Internal, "gnome_mdi_find_child");

      Stub : Gnome_MDI_Child_Record;
   begin
      return Gnome_MDI_Child (Get_User_Data
        (Internal (Get_Object (MDI), Name & ASCII.NUL), Stub));
   end Find_Child;

   ----------------------
   -- Get_Active_Child --
   ----------------------

   function Get_Active_Child
     (MDI : access Gnome_MDI_Record) return Gnome_MDI_Child
   is
      function Internal (MDI : System.Address) return System.Address;
      pragma Import (C, Internal, "gnome_mdi_get_active_child");

      Stub : Gnome_MDI_Child_Record;
   begin
      return Gnome_MDI_Child (Get_User_Data
        (Internal (Get_Object (MDI)), Stub));
   end Get_Active_Child;

   ---------------------
   -- Get_Active_View --
   ---------------------

   function Get_Active_View
     (MDI : access Gnome_MDI_Record) return Gtk.Widget.Gtk_Widget
   is
      function Internal (MDI : System.Address) return System.Address;
      pragma Import (C, Internal, "gnome_mdi_get_active_view");
   begin
      return Widget.Convert (Internal (Get_Object (MDI)));
   end Get_Active_View;

   -----------------------
   -- Get_Active_Window --
   -----------------------

   function Get_Active_Window
     (MDI : access Gnome_MDI_Record) return Gnome.App.Gnome_App
   is
      function Internal (MDI : System.Address) return System.Address;
      pragma Import (C, Internal, "gnome_mdi_get_active_window");

      Stub : Gnome.App.Gnome_App_Record;
   begin
      return Gnome.App.Gnome_App (Get_User_Data
        (Internal (Get_Object (MDI)), Stub));
   end Get_Active_Window;

   -----------------------
   -- Get_App_From_View --
   -----------------------

   function Get_App_From_View
     (View : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Gnome.App.Gnome_App
   is
      function Internal (View : System.Address) return System.Address;
      pragma Import (C, Internal, "gnome_mdi_get_app_from_view");

      Stub : Gnome.App.Gnome_App_Record;
   begin
      return Gnome.App.Gnome_App (Get_User_Data
        (Internal (Get_Object (View)), Stub));
   end Get_App_From_View;

   -------------------------
   -- Get_Child_From_View --
   -------------------------

   function Get_Child_From_View
     (View : access Gtk.Widget.Gtk_Widget_Record'Class) return Gnome_MDI_Child
   is
      function Internal (View : System.Address) return System.Address;
      pragma Import (C, Internal, "gnome_mdi_get_child_from_view");

      Stub : Gnome_MDI_Child_Record;
   begin
      return Gnome_MDI_Child (Get_User_Data
        (Internal (Get_Object (View)), Stub));
   end Get_Child_From_View;

   -------------------------
   -- Get_Child_Menu_Info --
   -------------------------

   function Get_Child_Menu_Info
     (App : access Gnome.App.Gnome_App_Record'Class)
      return Flat_UI_Info_Array_Access
   is
      function Internal
        (App : System.Address) return Flat_UI_Info_Array_Access;
      pragma Import (C, Internal, "gnome_mdi_get_child_menu_info");
   begin
      return Internal (Get_Object (App));
   end Get_Child_Menu_Info;

   ----------------------
   -- Get_Menubar_Info --
   ----------------------

   function Get_Menubar_Info
     (App : access Gnome.App.Gnome_App_Record'Class)
      return Flat_UI_Info_Array_Access
   is
      function Internal
        (App : System.Address) return Flat_UI_Info_Array_Access;
      pragma Import (C, Internal, "gnome_mdi_get_menubar_info");
   begin
      return Internal (Get_Object (App));
   end Get_Menubar_Info;

   ----------------------
   -- Get_Toolbar_Info --
   ----------------------

   function Get_Toolbar_Info
     (App : access Gnome.App.Gnome_App_Record'Class)
      return Flat_UI_Info_Array_Access
   is
      function Internal
        (App : System.Address) return Flat_UI_Info_Array_Access;
      pragma Import (C, Internal, "gnome_mdi_get_toolbar_info");
   begin
      return Internal (Get_Object (App));
   end Get_Toolbar_Info;

   --------------------------
   -- Get_View_From_Window --
   --------------------------

   function Get_View_From_Window
     (MDI    : access Gnome_MDI_Record;
      App    : access Gnome.App.Gnome_App_Record'Class)
      return Gtk.Widget.Gtk_Widget
   is
      function Internal
        (MDI    : System.Address;
         App    : System.Address)
         return System.Address;
      pragma Import (C, Internal, "gnome_mdi_get_view_from_window");
   begin
      return Widget.Convert (Internal (Get_Object (MDI),
                                       Get_Object (App)));
   end Get_View_From_Window;

   -------------------
   -- Open_Toplevel --
   -------------------

   procedure Open_Toplevel (MDI : access Gnome_MDI_Record)
   is
      procedure Internal (MDI : System.Address);
      pragma Import (C, Internal, "gnome_mdi_open_toplevel");
   begin
      Internal (Get_Object (MDI));
   end Open_Toplevel;

   --------------
   -- Register --
   --------------

   procedure Register
     (MDI    : access Gnome_MDI_Record;
      Object : access Glib.Object.GObject_Record'Class)
   is
      procedure Internal
        (MDI    : System.Address;
         Object : System.Address);
      pragma Import (C, Internal, "gnome_mdi_register");
   begin
      Internal (Get_Object (MDI), Get_Object (Object));
   end Register;

   ----------------
   -- Remove_All --
   ----------------

   function Remove_All
     (MDI    : access Gnome_MDI_Record;
      Force  : Gint)
      return Gint
   is
      function Internal
        (MDI    : System.Address;
         Force  : Gint)
         return Gint;
      pragma Import (C, Internal, "gnome_mdi_remove_all");
   begin
      return Internal (Get_Object (MDI),
                       Force);
   end Remove_All;

   ------------------
   -- Remove_Child --
   ------------------

   function Remove_Child
     (MDI    : access Gnome_MDI_Record;
      Child  : access Gnome_MDI_Child_Record'Class;
      Force  : Gint)
      return Gint
   is
      function Internal
        (MDI    : System.Address;
         Child  : System.Address;
         Force  : Gint)
         return Gint;
      pragma Import (C, Internal, "gnome_mdi_remove_child");
   begin
      return Internal (Get_Object (MDI),
                       Get_Object (Child),
                       Force);
   end Remove_Child;

   -----------------
   -- Remove_View --
   -----------------

   function Remove_View
     (MDI    : access Gnome_MDI_Record;
      View   : access Gtk.Widget.Gtk_Widget_Record'Class;
      Force  : Gint)
      return Gint
   is
      function Internal
        (MDI    : System.Address;
         View   : System.Address;
         Force  : Gint)
         return Gint;
      pragma Import (C, Internal, "gnome_mdi_remove_view");
   begin
      return Internal (Get_Object (MDI),
                       Get_Object (View),
                       Force);
   end Remove_View;

   ---------------------
   -- Set_Active_View --
   ---------------------

   procedure Set_Active_View
     (MDI  : access Gnome_MDI_Record;
      View : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
        (MDI  : System.Address;
         View : System.Address);
      pragma Import (C, Internal, "gnome_mdi_set_active_view");
   begin
      Internal (Get_Object (MDI),
                Get_Object (View));
   end Set_Active_View;

   -------------------------
   -- Set_Child_List_Path --
   -------------------------

   procedure Set_Child_List_Path
     (MDI  : access Gnome_MDI_Record;
      Path : String)
   is
      procedure Internal
        (MDI  : System.Address;
         Path : String);
      pragma Import (C, Internal, "gnome_mdi_set_child_list_path");
   begin
      Internal (Get_Object (MDI),
                Path & ASCII.NUL);
   end Set_Child_List_Path;

   -------------------------
   -- Set_Child_Menu_Path --
   -------------------------

   procedure Set_Child_Menu_Path
     (MDI  : access Gnome_MDI_Record;
      Path : String)
   is
      procedure Internal
        (MDI  : System.Address;
         Path : String);
      pragma Import (C, Internal, "gnome_mdi_set_child_menu_path");
   begin
      Internal (Get_Object (MDI),
                Path & ASCII.NUL);
   end Set_Child_Menu_Path;

   --------------------------
   -- Set_Menubar_Template --
   --------------------------

   procedure Set_Menubar_Template
     (MDI       : access Gnome_MDI_Record;
      Menu_Tmpl : access Gnome.App_Helper.UI_Info_Array)
   is
      procedure Internal
        (MDI       : System.Address;
         Menu_Tmpl : System.Address);
      pragma Import (C, Internal, "gnome_mdi_set_menubar_template");
   begin
      Internal (Get_Object (MDI), Menu_Tmpl.all'Address);
   end Set_Menubar_Template;

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode
     (MDI  : access Gnome_MDI_Record;
      Mode : Gnome_MDI_Mode)
   is
      procedure Internal
        (MDI  : System.Address;
         Mode : Gnome_MDI_Mode);
      pragma Import (C, Internal, "gnome_mdi_set_mode");
   begin
      Internal (Get_Object (MDI), Mode);
   end Set_Mode;

   --------------------------
   -- Set_Toolbar_Template --
   --------------------------

   procedure Set_Toolbar_Template
     (MDI       : access Gnome_MDI_Record;
      Tbar_Tmpl : access Gnome.App_Helper.UI_Info_Array)
   is
      procedure Internal
        (MDI       : System.Address;
         Tbar_Tmpl : System.Address);
      pragma Import (C, Internal, "gnome_mdi_set_toolbar_template");
   begin
      Internal (Get_Object (MDI), Tbar_Tmpl.all'Address);
   end Set_Toolbar_Template;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister
     (MDI    : access Gnome_MDI_Record;
      Object : access Glib.Object.GObject_Record'Class)
   is
      procedure Internal
        (MDI    : System.Address;
         Object : System.Address);
      pragma Import (C, Internal, "gnome_mdi_unregister");
   begin
      Internal (Get_Object (MDI), Get_Object (Object));
   end Unregister;

   ------------------
   -- Update_Child --
   ------------------

   procedure Update_Child
     (MDI   : access Gnome_MDI_Record;
      Child : access Gnome_MDI_Child_Record'Class)
   is
      procedure Internal
        (MDI   : System.Address;
         Child : System.Address);
      pragma Import (C, Internal, "gnome_mdi_update_child");
   begin
      Internal (Get_Object (MDI), Get_Object (Child));
   end Update_Child;

end Gnome.MDI;
