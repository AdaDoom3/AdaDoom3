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

with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Glib.Type_Conversion_Hooks;
pragma Elaborate_All (Glib.Type_Conversion_Hooks);
with Gtk.Menu_Shell; use Gtk.Menu_Shell;
with Gtk.Widget;     use Gtk.Widget;

package body Gtk.Menu is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Menu_Record);
   pragma Warnings (Off, Type_Conversion);
   --  This package is used to implement a minimal automated type conversion
   --  without having to drag the whole Gtk.Type_Conversion package for the
   --  most common widgets.

   procedure Internal_Menu_Position_Func
     (Menu    : System.Address;
      X       : out Gint;
      Y       : out Gint;
      Push_In : out Gboolean;
      Data    : System.Address);
   pragma Convention (C, Internal_Menu_Position_Func);
   --  Wrapper function passed to C.

   ----------------------
   -- Attach_To_Widget --
   ----------------------

   procedure Attach_To_Widget
     (Menu          : access Gtk_Menu_Record;
      Attach_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Detacher      : Gtk_Menu_Detach_Func)
   is
      procedure Internal
        (Menu          : System.Address;
         Attach_Widget : System.Address;
         Detacher      : Gtk_Menu_Detach_Func);
      pragma Import (C, Internal, "gtk_menu_attach_to_widget");

   begin
      Internal (Get_Object (Menu), Get_Object (Attach_Widget), Detacher);
   end Attach_To_Widget;

   ------------
   -- Detach --
   ------------

   procedure Detach (Menu : access Gtk_Menu_Record) is
      procedure Internal (Menu : System.Address);
      pragma Import (C, Internal, "gtk_menu_detach");

   begin
      Internal (Get_Object (Menu));
   end Detach;

   ---------------------
   -- Get_Accel_Group --
   ---------------------

   function Get_Accel_Group
     (Menu : access Gtk_Menu_Record) return Accel_Group.Gtk_Accel_Group
   is
      function Internal
        (Menu : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_menu_get_accel_group");

      Stub : Accel_Group.Gtk_Accel_Group_Record;

   begin
      return Accel_Group.Gtk_Accel_Group
        (Get_User_Data_Fast (Internal (Get_Object (Menu)), Stub));
   end Get_Accel_Group;

   --------------------
   -- Get_Accel_Path --
   --------------------

   function Get_Accel_Path (Menu : access Gtk_Menu_Record) return String is
      function Internal (Menu : System.Address)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_menu_get_accel_path");
   begin
      return Value (Internal (Get_Object (Menu)));
   end Get_Accel_Path;

   ----------------
   -- Get_Active --
   ----------------

   function Get_Active
     (Menu : access Gtk_Menu_Record) return Gtk.Menu_Item.Gtk_Menu_Item
   is
      function Internal (Menu : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_menu_get_active");

      Stub : Gtk.Menu_Item.Gtk_Menu_Item_Record;

   begin
      return Gtk.Menu_Item.Gtk_Menu_Item
        (Get_User_Data (Internal (Get_Object (Menu)), Stub));
   end Get_Active;

   -----------------------
   -- Get_Attach_Widget --
   -----------------------

   function Get_Attach_Widget
     (Menu : access Gtk_Menu_Record) return Gtk.Widget.Gtk_Widget
   is
      function Internal (Menu : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_menu_get_attach_widget");

   begin
      return Gtk.Widget.Convert (Internal (Get_Object (Menu)));
   end Get_Attach_Widget;

   -----------------
   -- Get_Monitor --
   -----------------

   function Get_Monitor (Menu : access Gtk_Menu_Record) return Gint is
      function Internal (Menu : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_menu_get_monitor");
   begin
      return Internal (Get_Object (Menu));
   end Get_Monitor;

   -----------------------
   -- Get_Tearoff_State --
   -----------------------

   function Get_Tearoff_State (Menu : access Gtk_Menu_Record) return Boolean is
      function Internal (Menu : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_menu_get_tearoff_state");

   begin
      return Internal (Get_Object (Menu)) /= 0;
   end Get_Tearoff_State;

   ---------------
   -- Get_Title --
   ---------------

   function Get_Title (Menu : access Gtk_Menu_Record) return UTF8_String is
      function Internal (Menu : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_menu_get_title");

   begin
      return Value (Internal (Get_Object (Menu)));
   end Get_Title;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Menu) is
   begin
      Widget := new Gtk_Menu_Record;
      Gtk.Menu.Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Menu_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_menu_new");

   begin
      Set_Object (Widget, Internal);
   end Initialize;

   -------------
   -- Popdown --
   -------------

   procedure Popdown (Menu : access Gtk_Menu_Record) is
      procedure Internal (Menu : System.Address);
      pragma Import (C, Internal, "gtk_menu_popdown");

   begin
      Internal (Get_Object (Menu));
   end Popdown;

   -------------------
   -- Reorder_Child --
   -------------------

   procedure Reorder_Child
     (Menu     : access Gtk_Menu_Record;
      Child    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Position : Gint)
   is
      procedure Internal
        (Menu     : System.Address;
         Child    : System.Address;
         Position : Gint);
      pragma Import (C, Internal, "gtk_menu_reorder_child");

   begin
      Internal (Get_Object (Menu), Get_Object (Child), Position);
   end Reorder_Child;

   ----------------
   -- Reposition --
   ----------------

   procedure Reposition (Menu : access Gtk_Menu_Record) is
      procedure Internal (Menu : System.Address);
      pragma Import (C, Internal, "gtk_menu_reposition");

   begin
      Internal (Get_Object (Menu));
   end Reposition;

   ---------------------
   -- Set_Accel_Group --
   ---------------------

   procedure Set_Accel_Group
      (Menu    : access Gtk_Menu_Record;
       Accel   : Gtk.Accel_Group.Gtk_Accel_Group)
   is
      procedure Internal
        (Menu        : System.Address;
         Accel_Group : System.Address);
      pragma Import (C, Internal, "gtk_menu_set_accel_group");

   begin
      Internal (Get_Object (Menu), Get_Object (Accel));
   end Set_Accel_Group;

   --------------------
   -- Set_Accel_Path --
   --------------------

   procedure Set_Accel_Path
     (Menu       : access Gtk_Menu_Record;
      Accel_Path : UTF8_String)
   is
      procedure Internal (Menu : System.Address; Accel_Path : UTF8_String);
      pragma Import (C, Internal, "gtk_menu_set_accel_path");

   begin
      Internal (Get_Object (Menu), Accel_Path & ASCII.NUL);
   end Set_Accel_Path;

   ----------------
   -- Set_Active --
   ----------------

   procedure Set_Active (Menu : access Gtk_Menu_Record; Index : Guint) is
      procedure Internal (Menu : System.Address; Index : Guint);
      pragma Import (C, Internal, "gtk_menu_set_active");

   begin
      Internal (Get_Object (Menu), Index);
   end Set_Active;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title (Menu : access Gtk_Menu_Record; Title : UTF8_String) is
      procedure Internal (Menu : System.Address; Title : UTF8_String);
      pragma Import (C, Internal, "gtk_menu_set_title");

   begin
      Internal (Get_Object (Menu), Title & ASCII.NUL);
   end Set_Title;

   -----------------------
   -- Set_Tearoff_State --
   -----------------------

   procedure Set_Tearoff_State
     (Menu     : access Gtk_Menu_Record;
      Torn_Off : Boolean)
   is
      procedure Internal (Menu : System.Address; Torn_Off : Gboolean);
      pragma Import (C, Internal, "gtk_menu_set_tearoff_state");

   begin
      Internal (Get_Object (Menu), Boolean'Pos (Torn_Off));
   end Set_Tearoff_State;

   -----------
   -- Popup --
   -----------

   procedure Popup
     (Menu              : access Gtk_Menu_Record;
      Parent_Menu_Shell : Gtk.Menu_Shell.Gtk_Menu_Shell := null;
      Parent_Menu_Item  : Gtk.Menu_Item.Gtk_Menu_Item := null;
      Func              : C_Gtk_Menu_Position_Func := null;
      User_Data         : System.Address;
      Button            : Guint := 1;
      Activate_Time     : Guint32 := 0)
   is
      procedure Internal
        (Menu          : System.Address;
         Parent_M      : System.Address;
         Parent_I      : System.Address;
         Func          : System.Address;
         Data          : System.Address;
         Button        : Guint;
         Activate_Time : Guint32);
      pragma Import (C, Internal, "gtk_menu_popup");

      Parent_Shell : System.Address := System.Null_Address;
      Parent_Item  : System.Address := System.Null_Address;

   begin
      if Parent_Menu_Shell /= null then
         Parent_Shell := Get_Object (Parent_Menu_Shell);
      end if;

      if Parent_Menu_Item /= null then
         Parent_Item := Get_Object (Parent_Menu_Item);
      end if;

      if Func = null then
         Internal
           (Get_Object (Menu), Parent_Shell, Parent_Item,
            System.Null_Address,
            System.Null_Address, Button, Activate_Time);
      else
         Internal
           (Get_Object (Menu), Parent_Shell, Parent_Item,
            Func.all'Address, User_Data, Button, Activate_Time);
      end if;
   end Popup;

   ---------------------------------
   -- Internal_Menu_Position_Func --
   ---------------------------------

   procedure Internal_Menu_Position_Func
     (Menu    : System.Address;
      X       : out Gint;
      Y       : out Gint;
      Push_In : out Gboolean;
      Data    : System.Address)
   is
      function Convert is
        new Ada.Unchecked_Conversion (System.Address, Gtk_Menu_Position_Func);
      Func : constant Gtk_Menu_Position_Func := Convert (Data);

      M    : Gtk_Menu;
      Stub : Gtk_Menu_Record;
   begin
      M := Gtk_Menu (Get_User_Data (Menu, Stub));
      Func.all (M, X, Y);

      --  Always place popup at our specified coordinates regardless of
      --  whether it may be outside the visible area.
      Push_In := Boolean'Pos (False);
   end Internal_Menu_Position_Func;

   -----------
   -- Popup --
   -----------

   procedure Popup
     (Menu              : access Gtk_Menu_Record;
      Parent_Menu_Shell : Gtk.Menu_Shell.Gtk_Menu_Shell := null;
      Parent_Menu_Item  : Gtk.Menu_Item.Gtk_Menu_Item := null;
      Func              : Gtk_Menu_Position_Func := null;
      Button            : Guint := 1;
      Activate_Time     : Guint32 := 0)
   is
      F : C_Gtk_Menu_Position_Func := null;
      D : System.Address := System.Null_Address;
   begin
      if Func /= null then
         F := Internal_Menu_Position_Func'Access;
         D := Func.all'Address;
      end if;

      Popup
        (Menu,
         Parent_Menu_Shell,
         Parent_Menu_Item,
         F,
         D,
         Button,
         Activate_Time);
   end Popup;

   ---------------------------------------
   -- User_Menu_Popup (generic package) --
   ---------------------------------------

   package body User_Menu_Popup is

      type Data_Access is access Data_Type;

      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Data_Type, Data_Access);

      type Data_And_Cb is record
         Data : Data_Access;
         Cb   : Gtk_Menu_Position_Func;
      end record;

      type Data_And_Cb_Access is access Data_And_Cb;

      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Data_And_Cb, Data_And_Cb_Access);
      function Convert is
        new Ada.Unchecked_Conversion (System.Address, Data_And_Cb_Access);
      function Convert is
        new Ada.Unchecked_Conversion (Data_And_Cb_Access, System.Address);

      -------------------------------------------
      -- Internal_Menu_Position_Func_With_Data --
      -------------------------------------------

      procedure Internal_Menu_Position_Func_With_Data
        (Menu      : System.Address;
         X         : out Gint;
         Y         : out Gint;
         Push_In   : out Gboolean;
         User_Data : System.Address)
      is
         Data : Data_And_Cb_Access := Convert (User_Data);
         M    : Gtk_Menu;
         Stub : Gtk_Menu_Record;
      begin
         M := Gtk_Menu (Get_User_Data (Menu, Stub));
         Data.Cb (M, X, Y, Data.Data);

         --  Always place popup at our specified coordinates regardless of
         --  whether it may be outside the visible area.
         Push_In := Boolean'Pos (False);

         Unchecked_Free (Data.Data);
         Unchecked_Free (Data);
      end Internal_Menu_Position_Func_With_Data;

      -----------
      -- Popup --
      -----------

      procedure Popup
        (Menu              : access Gtk_Menu_Record'Class;
         Data              : access Data_Type;
         Parent_Menu_Shell : Gtk.Menu_Shell.Gtk_Menu_Shell := null;
         Parent_Menu_Item  : Gtk.Menu_Item.Gtk_Menu_Item := null;
         Func              : Gtk_Menu_Position_Func := null;
         Button            : Guint := 1;
         Activate_Time     : Guint32 := 0)
      is
         The_Data : Data_And_Cb_Access;
         F : C_Gtk_Menu_Position_Func := null;
         D : System.Address := System.Null_Address;
      begin
         if Func /= null then
            The_Data := new Data_And_Cb'(new Data_Type'(Data.all), Func);
            F := Internal_Menu_Position_Func_With_Data_Access;
            D := Convert (The_Data);
         end if;

         Popup
           (Menu,
            Parent_Menu_Shell,
            Parent_Menu_Item,
            F,
            D,
            Button,
            Activate_Time);
      end Popup;
   end User_Menu_Popup;

   ------------
   -- Attach --
   ------------

   procedure Attach
     (Menu          : access Gtk_Menu_Record;
      Child         : access Gtk_Menu_Item_Record'Class;
      Left_Attach   : Guint;
      Right_Attach  : Guint;
      Top_Attach    : Guint;
      Bottom_Attach : Guint)
   is
      procedure Internal
        (Menu          : System.Address;
         Child         : System.Address;
         Left_Attach   : Guint;
         Right_Attach  : Guint;
         Top_Attach    : Guint;
         Bottom_Attach : Guint);
      pragma Import (C, Internal, "gtk_menu_attach");
   begin
      Internal (Get_Object (Menu), Get_Object (Child), Left_Attach,
                Right_Attach, Top_Attach, Bottom_Attach);
   end Attach;

   ---------------------------
   -- Get_For_Attach_Widget --
   ---------------------------

   function Get_For_Attach_Widget
     (Widget : access Gtk_Widget_Record'Class)
      return Widget_List.Glist
   is
      function Internal (Widget : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_menu_get_for_attach_widget");
      List : Widget_List.Glist;
   begin
      Widget_List.Set_Object (List, Internal (Get_Object (Widget)));
      return List;
   end Get_For_Attach_Widget;

   -----------------
   -- Set_Monitor --
   -----------------

   procedure Set_Monitor
     (Menu        : access Gtk_Menu_Record;
      Monitor_Num : Gint)
   is
      procedure Internal (Menu : System.Address;  Monitor_Num : Gint);
      pragma Import (C, Internal, "gtk_menu_set_monitor");
   begin
      Internal (Get_Object (Menu), Monitor_Num);
   end Set_Monitor;

   ----------------
   -- Set_Screen --
   ----------------

   procedure Set_Screen
     (Menu   : access Gtk_Menu_Record;
      Screen : access Gdk.Screen.Gdk_Screen_Record'Class)
   is
      procedure Internal
        (Menu   : System.Address;
         Screen : System.Address);
      pragma Import (C, Internal, "gtk_menu_set_screen");
   begin
      Internal (Get_Object (Menu), Get_Object (Screen));
   end Set_Screen;

end Gtk.Menu;
