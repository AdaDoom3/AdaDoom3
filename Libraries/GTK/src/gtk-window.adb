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

with System;
with Gdk.Event;        use Gdk.Event;
with Gdk.Pixbuf;       use Gdk.Pixbuf;
with Gdk.Types;        use Gdk.Types;
with Gdk.Window;       use Gdk.Window;
with Gtk.Accel_Group;  use Gtk.Accel_Group;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Widget;       use Gtk.Widget;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Glib.Type_Conversion_Hooks;
pragma Elaborate_All (Glib.Type_Conversion_Hooks);

package body Gtk.Window is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Window_Record);
   pragma Warnings (Off, Type_Conversion);
   --  This package is used to implement a minimal automated type conversion
   --  without having to drag the whole Gtk.Type_Conversion package for the
   --  most common widgets.

   ----------------------
   -- Activate_Default --
   ----------------------

   function Activate_Default
     (Window : access Gtk_Window_Record) return Boolean
   is
      function Internal (Window : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_window_activate_default");

   begin
      return Internal (Get_Object (Window)) /= 0;
   end Activate_Default;

   --------------------
   -- Activate_Focus --
   --------------------

   function Activate_Focus
     (Window : access Gtk_Window_Record) return Boolean
   is
      function Internal (Window : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_window_activate_focus");

   begin
      return Internal (Get_Object (Window)) /= 0;
   end Activate_Focus;

   ---------------------
   -- Add_Accel_Group --
   ---------------------

   procedure Add_Accel_Group
     (Window      : access Gtk_Window_Record;
      Accel_Group : Gtk_Accel_Group)
   is
      procedure Internal
        (Window      : System.Address;
         Accel_Group : System.Address);
      pragma Import (C, Internal, "gtk_window_add_accel_group");

   begin
      Internal (Get_Object (Window), Get_Object (Accel_Group));
   end Add_Accel_Group;

   ------------------
   -- Add_Mnemonic --
   ------------------

   procedure Add_Mnemonic
     (Window : access Gtk_Window_Record;
      Keyval : Gdk.Types.Gdk_Key_Type;
      Target : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Window : System.Address;
                          Keyval : Gdk_Key_Type;
                          Target : System.Address);
      pragma Import (C, Internal, "gtk_window_add_mnemonic");
   begin
      Internal (Get_Object (Window), Keyval, Get_Object (Target));
   end Add_Mnemonic;

   ---------------
   -- Deiconify --
   ---------------

   procedure Deiconify (Window : access Gtk_Window_Record) is
      procedure Internal (Window : System.Address);
      pragma Import (C, Internal, "gtk_window_deiconify");

   begin
      Internal (Get_Object (Window));
   end Deiconify;

   -------------------
   -- Get_Deletable --
   -------------------

   function Get_Deletable (Window : access Gtk_Window_Record) return Boolean is
      function Internal (Window : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_window_get_deletable");
   begin
      return Boolean'Val (Internal (Get_Object (Window)));
   end Get_Deletable;

   ---------------------------
   -- Get_Default_Icon_Name --
   ---------------------------

   function Get_Default_Icon_Name return String is
      function Internal return chars_ptr;
      pragma Import (C, Internal, "gtk_window_get_default_icon_name");
   begin
      return Value (Internal);
   end Get_Default_Icon_Name;

   ------------------------
   -- Get_Default_Widget --
   ------------------------

   function Get_Default_Widget
     (Window : access Gtk_Window_Record) return Gtk.Widget.Gtk_Widget
   is
      function Internal (Window : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_window_get_default_widget");
      Stub : Gtk_Widget_Record;
   begin
      return Gtk_Widget (Get_User_Data (Internal (Get_Object (Window)), Stub));
   end Get_Default_Widget;

   ---------------
   -- Get_Focus --
   ---------------

   function Get_Focus (Window : access Gtk_Window_Record)
      return Gtk.Widget.Gtk_Widget
   is
      function Internal (W : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_window_get_focus");
      Stub : Gtk_Widget_Record;
   begin
      return Gtk_Widget
        (Get_User_Data (Internal (Get_Object (Window)), Stub));
   end Get_Focus;

   -----------------
   -- Get_Gravity --
   -----------------

   function Get_Gravity
     (Window : access Gtk_Window_Record) return Gdk.Window.Gdk_Gravity
   is
      function Internal
        (Window : System.Address) return Gdk.Window.Gdk_Gravity;
      pragma Import (C, Internal, "gtk_window_get_gravity");

   begin
      return Internal (Get_Object (Window));
   end Get_Gravity;

   ---------------
   -- Get_Group --
   ---------------

   function Get_Group
     (Window : access Gtk_Window_Record) return Gtk_Window_Group
   is
      function Internal (Window : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_window_get_group");
      Stub : Gtk_Window_Group_Record;
   begin
      return Gtk_Window_Group
        (Get_User_Data (Internal (Get_Object (Window)), Stub));
   end Get_Group;

   -----------------
   -- Get_Opacity --
   -----------------

   function Get_Opacity (Window : access Gtk_Window_Record) return Gdouble is
      function Internal (Window : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_window_get_opacity");
   begin
      return Internal (Get_Object (Window));
   end Get_Opacity;

   ---------------
   -- Get_Title --
   ---------------

   function Get_Title (Window : access Gtk_Window_Record) return UTF8_String is
      function Internal
        (Window : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_window_get_title");

      S : constant chars_ptr := Internal (Get_Object (Window));

   begin
      if S /= Null_Ptr then
         return Value (S);
      else
         return "";
      end if;
   end Get_Title;

   -------------------
   -- Get_Resizable --
   -------------------

   function Get_Resizable
     (Window : access Gtk_Window_Record) return Boolean
   is
      function Internal (Window : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_window_get_resizable");

   begin
      return Internal (Get_Object (Window)) /= 0;
   end Get_Resizable;

   -----------------------
   -- Get_Transient_For --
   -----------------------

   function Get_Transient_For
     (Window : access Gtk_Window_Record) return Gtk_Window
   is
      function Internal (Window : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_window_get_transient_for");

      Stub : Gtk_Window_Record;

   begin
      return Gtk_Window
        (Get_User_Data_Fast (Internal (Get_Object (Window)), Stub));
   end Get_Transient_For;

   ------------------------
   -- Group_List_Windows --
   ------------------------

   function Group_List_Windows
     (Window_Group : access Gtk_Window_Group_Record)
      return Gtk.Widget.Widget_List.Glist
   is
      function Internal (Window_Group : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_window_group_list_windows");

      List : Gtk.Widget.Widget_List.Glist;
   begin
      Gtk.Widget.Widget_List.Set_Object
        (List, Internal (Get_Object (Window_Group)));
      return List;
   end Group_List_Windows;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Window   : out Gtk_Window;
      The_Type : Gtk_Window_Type := Window_Toplevel) is
   begin
      Window := new Gtk_Window_Record;
      Initialize (Window, The_Type);
   end Gtk_New;

   -------------
   -- Iconify --
   -------------

   procedure Iconify (Window : access Gtk_Window_Record) is
      procedure Internal (Window : System.Address);
      pragma Import (C, Internal, "gtk_window_iconify");

   begin
      Internal (Get_Object (Window));
   end Iconify;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Window   : access Gtk_Window_Record'Class;
      The_Type : Gtk_Window_Type)
   is
      function Internal (Typ : Gtk_Window_Type) return System.Address;
      pragma Import (C, Internal, "gtk_window_new");

   begin
      Set_Object (Window, Internal (The_Type));

      if Get_Follow_Events then
         Add_Events (Window, Pointer_Motion_Mask);
      end if;
   end Initialize;

   --------------------
   -- List_Toplevels --
   --------------------

   function List_Toplevels return Gtk.Widget.Widget_List.Glist is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_window_list_toplevels");

      List : Gtk.Widget.Widget_List.Glist;

   begin
      Gtk.Widget.Widget_List.Set_Object (List, Internal);
      return List;
   end List_Toplevels;

   --------------
   -- Maximize --
   --------------

   procedure Maximize (Window : access Gtk_Window_Record) is
      procedure Internal (Window : System.Address);
      pragma Import (C, Internal, "gtk_window_maximize");

   begin
      Internal (Get_Object (Window));
   end Maximize;

   -------------
   -- Present --
   -------------

   procedure Present (Window : access Gtk_Window_Record) is
      procedure Internal (Window : System.Address);
      pragma Import (C, Internal, "gtk_window_present");

   begin
      Internal (Get_Object (Window));
   end Present;

   ------------------------
   -- Remove_Accel_Group --
   ------------------------

   procedure Remove_Accel_Group
     (Window      : access Gtk_Window_Record;
      Accel_Group : Gtk_Accel_Group)
   is
      procedure Internal
        (Window      : System.Address;
         Accel_Group : System.Address);
      pragma Import (C, Internal, "gtk_window_remove_accel_group");

   begin
      Internal (Get_Object (Window), Get_Object (Accel_Group));
   end Remove_Accel_Group;

   ------------
   -- Resize --
   ------------

   procedure Resize
     (Window : access Gtk_Window_Record;
      Width, Height : Gint)
   is
      procedure Internal (Window : System.Address; Width, Height : Gint);
      pragma Import (C, Internal, "gtk_window_resize");
      Req : Gtk_Requisition;
   begin
      if Width = -1
        and then Height = -1
        and then Get_Child (Window) /= null
      then
         --  Size_Request directly on the window would return 0x0 if the
         --  window is not currently visible, unfortunately. So we cheat a
         --  little and directly ask the window's child for its prefered
         --  size.

         Size_Request (Get_Child (Window), Req);
         Req.Width  := Req.Width + Gint (Get_Border_Width (Window) * 2);
         Req.Height := Req.Height + Gint (Get_Border_Width (Window) * 2);
         Internal (Get_Object (Window), Req.Width, Req.Height);
      else
         Internal (Get_Object (Window), Width, Height);
      end if;
   end Resize;

   -------------------
   -- Set_Decorated --
   -------------------

   procedure Set_Decorated
     (Window  : access Gtk_Window_Record;
      Setting : Boolean := True)
   is
      procedure Internal
        (Window  : System.Address;
         Setting : Gboolean);
      pragma Import (C, Internal, "gtk_window_set_decorated");

   begin
      Internal (Get_Object (Window), Boolean'Pos (Setting));
   end Set_Decorated;

   ----------------------
   -- Set_Default_Size --
   ----------------------

   procedure Set_Default_Size
     (Window : access Gtk_Window_Record;
      Width  : Gint;
      Height : Gint)
   is
      procedure Internal
        (Window : System.Address;
         Width  : Gint;
         Height : Gint);
      pragma Import (C, Internal, "gtk_window_set_default_size");

   begin
      Internal (Get_Object (Window), Width, Height);
   end Set_Default_Size;

   -------------------
   -- Set_Deletable --
   -------------------

   procedure Set_Deletable
     (Window  : access Gtk_Window_Record;
      Setting : Boolean)
   is
      procedure Internal
        (Window  : System.Address;
         Setting : Gboolean);
      pragma Import (C, Internal, "gtk_window_set_deletable");
   begin
      Internal (Get_Object (Window), Boolean'Pos (Setting));
   end Set_Deletable;

   -----------------------------
   -- Set_Destroy_With_Parent --
   -----------------------------

   procedure Set_Destroy_With_Parent
     (Window  : access Gtk_Window_Record;
      Setting : Boolean := True)
   is
      procedure Internal
        (Window  : System.Address;
         Setting : Gboolean);
      pragma Import (C, Internal, "gtk_window_set_destroy_with_parent");

   begin
      Internal (Get_Object (Window), Boolean'Pos (Setting));
   end Set_Destroy_With_Parent;

   ---------------
   -- Set_Focus --
   ---------------

   procedure Set_Focus
     (Window : access Gtk_Window_Record;
      Focus  : Gtk.Widget.Gtk_Widget)
   is
      procedure Internal (Window, Focus : System.Address);
      pragma Import (C, Internal, "gtk_window_set_focus");
   begin
      if Focus = null then
         Internal (Get_Object (Window), System.Null_Address);
      else
         Internal (Get_Object (Window), Get_Object (Focus));
      end if;
   end Set_Focus;

   --------------------------
   -- Set_Frame_Dimensions --
   --------------------------

   procedure Set_Frame_Dimensions
     (Window : access Gtk_Window_Record;
      Left   : Gint;
      Top    : Gint;
      Right  : Gint;
      Bottom : Gint)
   is
      procedure Internal
        (Window  : System.Address;
         Left   : Gint;
         Top    : Gint;
         Right  : Gint;
         Bottom : Gint);
      pragma Import (C, Internal, "gtk_window_set_frame_dimensions");

   begin
      Internal (Get_Object (Window), Left, Top, Right, Bottom);
   end Set_Frame_Dimensions;

   ------------------------
   -- Set_Geometry_Hints --
   ------------------------

   procedure Set_Geometry_Hints
     (Window          : access Gtk_Window_Record;
      Geometry_Widget : Gtk_Widget;
      Geometry        : Gdk_Geometry;
      Geom_Mask       : Gdk_Window_Hints)
   is
      procedure Internal
        (Window   : System.Address;
         Wid      : System.Address;
         Geometry : in out Gdk_Geometry;
         Mask     : Gdk_Window_Hints);
      pragma Import (C, Internal, "gtk_window_set_geometry_hints");

      Geom : Gdk_Geometry := Geometry;
      Wid  : System.Address := System.Null_Address;

   begin
      if Geometry_Widget /= null then
         Wid := Get_Object (Geometry_Widget);
      end if;

      Internal (Get_Object (Window), Wid, Geom, Geom_Mask);
   end Set_Geometry_Hints;

   -----------------
   -- Set_Gravity --
   -----------------

   procedure Set_Gravity
     (Window  : access Gtk_Window_Record;
      Gravity : Gdk.Window.Gdk_Gravity)
   is
      procedure Internal
        (Window : System.Address; Gravity : Gdk.Window.Gdk_Gravity);
      pragma Import (C, Internal, "gtk_window_set_gravity");

   begin
      Internal (Get_Object (Window), Gravity);
   end Set_Gravity;

   -------------------
   -- Set_Has_Frame --
   -------------------

   procedure Set_Has_Frame (Window : access Gtk_Window_Record) is
      procedure Internal (Window : System.Address);
      pragma Import (C, Internal, "gtk_window_set_has_frame");

   begin
      Internal (Get_Object (Window));
   end Set_Has_Frame;

   ---------------
   -- Set_Modal --
   ---------------

   procedure Set_Modal
     (Window : access Gtk_Window_Record;
      Modal  : Boolean := True)
   is
      procedure Internal (Window : System.Address; Modal : Integer);
      pragma Import (C, Internal, "gtk_window_set_modal");

   begin
      Internal (Get_Object (Window), Boolean'Pos (Modal));
   end Set_Modal;

   -----------------
   -- Set_Opacity --
   -----------------

   procedure Set_Opacity
     (Window  : access Gtk_Window_Record;
      Opacity : Gdouble)
   is
      procedure Internal
        (Window  : System.Address;
         Opacity : Gdouble);
      pragma Import (C, Internal, "gtk_window_set_opacity");
   begin
      Internal (Get_Object (Window), Opacity);
   end Set_Opacity;

   ----------------
   -- Set_Policy --
   ----------------

   procedure Set_Policy
     (Window       : access Gtk_Window_Record;
      Allow_Shrink : Boolean;
      Allow_Grow   : Boolean;
      Auto_Shrink  : Boolean)
   is
      procedure Internal
        (Window       : System.Address;
         Allow_Shrink : Gint;
         Allow_Grow   : Gint;
         Auto_Shrink  : Gint);
      pragma Import (C, Internal, "gtk_window_set_policy");

   begin
      Internal (Get_Object (Window), To_Gint (Allow_Shrink),
                To_Gint (Allow_Grow), To_Gint (Auto_Shrink));
   end Set_Policy;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position
     (Window   : access Gtk_Window_Record;
      Position : Gtk_Window_Position)
   is
      procedure Internal
        (Window   : System.Address;
         Position : Gtk_Window_Position);
      pragma Import (C, Internal, "gtk_window_set_position");

   begin
      Internal (Get_Object (Window), Position);
   end Set_Position;

   --------------------
   -- Set_Startup_Id --
   --------------------

   procedure Set_Startup_Id
     (Window     : access Gtk_Window_Record;
      Startup_Id : String)
   is
      procedure Internal
        (Window     : System.Address;
         Startup_Id : String);
      pragma Import (C, Internal, "gtk_window_set_startup_id");
   begin
      Internal (Get_Object (Window), Startup_Id & ASCII.NUL);
   end Set_Startup_Id;

   -----------------------
   -- Set_Transient_For --
   -----------------------

   procedure Set_Transient_For
     (Window : access Gtk_Window_Record;
      Parent : access Gtk_Window_Record'Class)
   is
      procedure Internal
        (Window : System.Address; Parent : System.Address);
      pragma Import (C, Internal, "gtk_window_set_transient_for");

   begin
      Internal (Get_Object (Window), Get_Object (Parent));
   end Set_Transient_For;

   -------------------
   -- Set_Resizable --
   -------------------

   procedure Set_Resizable
     (Window    : access Gtk_Window_Record;
      Resizable : Boolean := True)
   is
      procedure Internal
        (Window : System.Address; Resizeable : Gboolean);
      pragma Import (C, Internal, "gtk_window_set_resizable");

   begin
      Internal (Get_Object (Window), Boolean'Pos (Resizable));
   end Set_Resizable;

   --------------
   -- Set_Role --
   --------------

   procedure Set_Role (Window : access Gtk_Window_Record; Role : String) is
      procedure Internal (Window : System.Address; Role : String);
      pragma Import (C, Internal, "gtk_window_set_role");

   begin
      Internal (Get_Object (Window), Role & ASCII.NUL);
   end Set_Role;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title
     (Window : access Gtk_Window_Record;
      Title  : UTF8_String)
   is
      procedure Internal (W : System.Address; T : UTF8_String);
      pragma Import (C, Internal, "gtk_window_set_title");

   begin
      Internal (Get_Object (Window), Title & ASCII.NUL);
   end Set_Title;

   --------------------
   -- Set_Type_Hints --
   --------------------

   procedure Set_Type_Hint
     (Window : access Gtk_Window_Record;
      Hint   : Gdk.Window.Gdk_Window_Type_Hint)
   is
      procedure Internal
        (Widget : System.Address; Hint : Gdk.Window.Gdk_Window_Type_Hint);
      pragma Import (C, Internal, "gtk_window_set_type_hint");

   begin
      Internal (Get_Object (Window), Hint);
   end Set_Type_Hint;

   -----------------
   -- Set_Wmclass --
   -----------------

   procedure Set_Wmclass
     (Window        : access Gtk_Window_Record;
      Wmclass_Name  : String;
      Wmclass_Class : String)
   is
      procedure Internal (W : System.Address; N : String; C : String);
      pragma Import (C, Internal, "gtk_window_set_wmclass");

   begin
      Internal
        (Get_Object (Window),
         Wmclass_Name & ASCII.NUL,
         Wmclass_Class & ASCII.NUL);
   end Set_Wmclass;

   -----------
   -- Stick --
   -----------

   procedure Stick (Window : access Gtk_Window_Record) is
      procedure Internal (Window : System.Address);
      pragma Import (C, Internal, "gtk_window_stick");

   begin
      Internal (Get_Object (Window));
   end Stick;

   ----------------
   -- Unmaximize --
   ----------------

   procedure Unmaximize (Window : access Gtk_Window_Record) is
      procedure Internal (Window : System.Address);
      pragma Import (C, Internal, "gtk_window_unmaximize");

   begin
      Internal (Get_Object (Window));
   end Unmaximize;

   -------------
   -- Unstick --
   -------------

   procedure Unstick (Window : access Gtk_Window_Record) is
      procedure Internal (Window : System.Address);
      pragma Import (C, Internal, "gtk_window_unstick");
   begin
      Internal (Get_Object (Window));
   end Unstick;

   -------------------------
   -- Set_Skip_Pager_Hint --
   -------------------------

   procedure Set_Skip_Pager_Hint
     (Window  : access Gtk_Window_Record;
      Setting : Boolean)
   is
      procedure Internal (Window  : System.Address; Setting : Gboolean);
      pragma Import (C, Internal, "gtk_window_set_skip_pager_hint");
   begin
      Internal (Get_Object (Window), Boolean'Pos (Setting));
   end Set_Skip_Pager_Hint;

   ---------------------------
   -- Set_Skip_Taskbar_Hint --
   ---------------------------

   procedure Set_Skip_Taskbar_Hint
     (Window  : access Gtk_Window_Record;
      Setting : Boolean)
   is
      procedure Internal (Window  : System.Address; Setting : Gboolean);
      pragma Import (C, Internal, "gtk_window_set_skip_taskbar_hint");
   begin
      Internal (Get_Object (Window), Boolean'Pos (Setting));
   end Set_Skip_Taskbar_Hint;

   ----------------------
   -- Set_Urgency_Hint --
   ----------------------

   procedure Set_Urgency_Hint
     (Window  : access Gtk_Window_Record;
      Setting : Boolean)
   is
      procedure Internal (Window  : System.Address; Setting : Gboolean);
      pragma Import (C, Internal, "gtk_window_set_urgency_hint");
   begin
      Internal (Get_Object (Window), Boolean'Pos (Setting));
   end Set_Urgency_Hint;

   ------------------
   -- Unfullscreen --
   ------------------

   procedure Unfullscreen (Window : access Gtk_Window_Record) is
      procedure Internal (Window : System.Address);
      pragma Import (C, Internal, "gtk_window_unfullscreen");
   begin
      Internal (Get_Object (Window));
   end Unfullscreen;

   ------------------------
   -- Set_Icon_From_File --
   ------------------------

   function Set_Icon_From_File
     (Window   : access Gtk_Window_Record;
      Filename : String)
      return Boolean
   is
      function Internal
        (Window   : System.Address;
         Filename : String)
         return Gboolean;
      pragma Import (C, Internal, "gtk_window_set_icon_from_file");
   begin
      return Boolean'Val
        (Internal (Get_Object (Window), Filename & ASCII.NUL));
   end Set_Icon_From_File;

   -------------------
   -- Set_Icon_List --
   -------------------

   procedure Set_Icon_List
     (Window : access Gtk_Window_Record;
      List   : Glib.Object.Object_Simple_List.Glist)
   is
      use Glib.Object.Object_Simple_List;
      procedure Internal (Window, List : System.Address);
      pragma Import (C, Internal, "gtk_window_set_icon_list");
   begin
      Internal (Get_Object (Window), Get_Object (List));
   end Set_Icon_List;

   -------------------
   -- Set_Icon_Name --
   -------------------

   procedure Set_Icon_Name
     (Window : access Gtk_Window_Record;
      Name   : String)
   is
      procedure Internal
        (Window : System.Address;
         Name   : String);
      pragma Import (C, Internal, "gtk_window_set_icon_name");
   begin
      Internal (Get_Object (Window), Name & ASCII.NUL);
   end Set_Icon_Name;

   --------------------
   -- Set_Keep_Above --
   --------------------

   procedure Set_Keep_Above
     (Window  : access Gtk_Window_Record;
      Setting : Boolean)
   is
      procedure Internal
        (Window  : System.Address;
         Setting : Gboolean);
      pragma Import (C, Internal, "gtk_window_set_keep_above");
   begin
      Internal (Get_Object (Window), Boolean'Pos (Setting));
   end Set_Keep_Above;

   --------------------
   -- Set_Keep_Below --
   --------------------

   procedure Set_Keep_Below
     (Window  : access Gtk_Window_Record;
      Setting : Boolean)
   is
      procedure Internal
        (Window  : System.Address;
         Setting : Gboolean);
      pragma Import (C, Internal, "gtk_window_set_keep_below");
   begin
      Internal (Get_Object (Window), Boolean'Pos (Setting));
   end Set_Keep_Below;

   ---------------------------
   -- Set_Mnemonic_Modifier --
   ---------------------------

   procedure Set_Mnemonic_Modifier
     (Window   : access Gtk_Window_Record;
      Modifier : Gdk_Modifier_Type)
   is
      procedure Internal
        (Window   : System.Address;
         Modifier : Gdk_Modifier_Type);
      pragma Import (C, Internal, "gtk_window_set_mnemonic_modifier");
   begin
      Internal (Get_Object (Window), Modifier);
   end Set_Mnemonic_Modifier;

   ------------------------------
   -- Reshow_With_Initial_Size --
   ------------------------------

   procedure Reshow_With_Initial_Size
     (Window : access Gtk_Window_Record)
   is
      procedure Internal (Window : System.Address);
      pragma Import (C, Internal, "gtk_window_reshow_with_initial_size");
   begin
      Internal (Get_Object (Window));
   end Reshow_With_Initial_Size;

   ----------------------
   -- Set_Accept_Focus --
   ----------------------

   procedure Set_Accept_Focus
     (Window  : access Gtk_Window_Record;
      Setting : Boolean)
   is
      procedure Internal
        (Window  : System.Address;
         Setting : Gboolean);
      pragma Import (C, Internal, "gtk_window_set_accept_focus");
   begin
      Internal (Get_Object (Window), Boolean'Pos (Setting));
   end Set_Accept_Focus;

   -----------------------------------
   -- Set_Auto_Startup_Notification --
   -----------------------------------

   procedure Set_Auto_Startup_Notification (Setting : Boolean) is
      procedure Internal (Setting : Gboolean);
      pragma Import (C, Internal, "gtk_window_set_auto_startup_notification");
   begin
      Internal (Boolean'Pos (Setting));
   end Set_Auto_Startup_Notification;

   -----------------
   -- Set_Default --
   -----------------

   procedure Set_Default
     (Window         : access Gtk_Window_Record;
      Default_Widget : access Gtk_Widget_Record'Class)
   is
      procedure Internal
        (Window         : System.Address;
         Default_Widget : System.Address);
      pragma Import (C, Internal, "gtk_window_set_default");
   begin
      Internal (Get_Object (Window), Get_Object (Default_Widget));
   end Set_Default;

   ----------------------
   -- Set_Default_Icon --
   ----------------------

   procedure Set_Default_Icon (Icon : Gdk_Pixbuf) is
      procedure Internal (Icon : System.Address);
      pragma Import (C, Internal, "gtk_window_set_default_icon");
   begin
      Internal (Get_Object (Icon));
   end Set_Default_Icon;

   --------------------------------
   -- Set_Default_Icon_From_File --
   --------------------------------

   function Set_Default_Icon_From_File (Filename : String) return Boolean is
      function Internal (Filename : String) return Gboolean;
      pragma Import (C, Internal, "gtk_window_set_default_icon_from_file");
   begin
      return Boolean'Val (Internal (Filename & ASCII.NUL));
   end Set_Default_Icon_From_File;

   ---------------------------
   -- Set_Default_Icon_List --
   ---------------------------

   procedure Set_Default_Icon_List
     (List : Glib.Object.Object_Simple_List.Glist)
   is
      use Glib.Object.Object_Simple_List;
      procedure Internal (List : System.Address);
      pragma Import (C, Internal, "gtk_window_set_default_icon_list");
   begin
      Internal (Get_Object (List));
   end Set_Default_Icon_List;

   ---------------------------
   -- Set_Default_Icon_Name --
   ---------------------------

   procedure Set_Default_Icon_Name (Name : String) is
      procedure Internal (Name : String);
      pragma Import (C, Internal, "gtk_window_set_default_icon_name");
   begin
      Internal (Name & ASCII.NUL);
   end Set_Default_Icon_Name;

   ----------------------
   -- Set_Focus_On_Map --
   ----------------------

   procedure Set_Focus_On_Map
     (Window  : access Gtk_Window_Record;
      Setting : Boolean)
   is
      procedure Internal
        (Window  : System.Address;
         Setting : Gboolean);
      pragma Import (C, Internal, "gtk_window_set_focus_on_map");
   begin
      Internal (Get_Object (Window), Boolean'Pos (Setting));
   end Set_Focus_On_Map;

   --------------
   -- Set_Icon --
   --------------

   procedure Set_Icon
     (Window : access Gtk_Window_Record;
      Icon   : Gdk_Pixbuf)
   is
      procedure Internal
        (Window : System.Address;
         Icon   : System.Address);
      pragma Import (C, Internal, "gtk_window_set_icon");
   begin
      Internal (Get_Object (Window), Get_Object (Icon));
   end Set_Icon;

   ----------
   -- Move --
   ----------

   procedure Move
     (Window : access Gtk_Window_Record;
      X, Y   : Gint)
   is
      procedure Internal (Window : System.Address; X, Y : Gint);
      pragma Import (C, Internal, "gtk_window_move");
   begin
      Internal (Get_Object (Window), X, Y);
   end Move;

   --------------------
   -- Parse_Geometry --
   --------------------

   function Parse_Geometry
     (Window   : access Gtk_Window_Record;
      Geometry : String)
      return Boolean
   is
      function Internal
        (Window   : System.Address;
         Geometry : String)
         return Gboolean;
      pragma Import (C, Internal, "gtk_window_parse_geometry");
   begin
      return Boolean'Val
        (Internal (Get_Object (Window), Geometry & ASCII.NUL));
   end Parse_Geometry;

   -----------------------
   -- Present_With_Time --
   -----------------------

   procedure Present_With_Time
     (Window    : access Gtk_Window_Record;
      Timestamp : Guint32)
   is
      procedure Internal
        (Window    : System.Address;
         Timestamp : Guint32);
      pragma Import (C, Internal, "gtk_window_present_with_time");
   begin
      Internal (Get_Object (Window), Timestamp);
   end Present_With_Time;

   -------------------------
   -- Propagate_Key_Event --
   -------------------------

   function Propagate_Key_Event
     (Window : access Gtk_Window_Record;
      Event  : Gdk.Event.Gdk_Event_Key)
      return Boolean
   is
      function Internal
        (Window : System.Address;
         Event  : Gdk.Event.Gdk_Event_Key)
         return Gboolean;
      pragma Import (C, Internal, "gtk_window_propagate_key_event");
   begin
      return Boolean'Val (Internal (Get_Object (Window), Event));
   end Propagate_Key_Event;

   ---------------------
   -- Remove_Mnemonic --
   ---------------------

   procedure Remove_Mnemonic
     (Window : access Gtk_Window_Record;
      Keyval : Gdk_Key_Type;
      Target : access Gtk_Widget_Record'Class)
   is
      procedure Internal
        (Window : System.Address;
         Keyval : Gdk_Key_Type;
         Target : System.Address);
      pragma Import (C, Internal, "gtk_window_remove_mnemonic");
   begin
      Internal (Get_Object (Window), Keyval, Get_Object (Target));
   end Remove_Mnemonic;

   -------------------------
   -- Group_Remove_Window --
   -------------------------

   procedure Group_Remove_Window
     (Window_Group : access Gtk_Window_Group_Record;
      Window       : access Gtk_Window_Record'Class)
   is
      procedure Internal
        (Window_Group : System.Address;
         Window       : System.Address);
      pragma Import (C, Internal, "gtk_window_group_remove_window");
   begin
      Internal (Get_Object (Window_Group), Get_Object (Window));
   end Group_Remove_Window;

   ------------------------
   -- Has_Toplevel_Focus --
   ------------------------

   function Has_Toplevel_Focus
     (Window : access Gtk_Window_Record) return Boolean
   is
      function Internal (Window : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_window_has_toplevel_focus");
   begin
      return Boolean'Val (Internal (Get_Object (Window)));
   end Has_Toplevel_Focus;

   ---------------
   -- Is_Active --
   ---------------

   function Is_Active
     (Window : access Gtk_Window_Record) return Boolean
   is
      function Internal (Window : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_window_is_active");
   begin
      return Boolean'Val (Internal (Get_Object (Window)));
   end Is_Active;

   -----------------------
   -- Mnemonic_Activate --
   -----------------------

   function Mnemonic_Activate
     (Window   : access Gtk_Window_Record;
      Keyval   : Gdk_Key_Type;
      Modifier : Gdk_Modifier_Type)
      return Boolean
   is
      function Internal
        (Window   : System.Address;
         Keyval   : Gdk_Key_Type;
         Modifier : Gdk_Modifier_Type)
         return Gboolean;
      pragma Import (C, Internal, "gtk_window_mnemonic_activate");
   begin
      return Boolean'Val (Internal (Get_Object (Window), Keyval, Modifier));
   end Mnemonic_Activate;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Group : out Gtk_Window_Group) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_window_group_new");
   begin
      Group := new Gtk_Window_Group_Record;
      Set_Object (Group, Internal);
   end Gtk_New;

   --------------
   -- Get_Role --
   --------------

   function Get_Role
     (Window : access Gtk_Window_Record)
      return String
   is
      function Internal
        (Window : System.Address)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_window_get_role");
   begin
      return Value (Internal (Get_Object (Window)));
   end Get_Role;

   --------------
   -- Get_Size --
   --------------

   procedure Get_Size
     (Window : access Gtk_Window_Record;
      Width  : out Gint;
      Height : out Gint)
   is
      procedure Internal
        (Window : System.Address;
         Width  : out Gint;
         Height : out Gint);
      pragma Import (C, Internal, "gtk_window_get_size");
   begin
      Internal (Get_Object (Window), Width, Height);
   end Get_Size;

   -------------------------
   -- Get_Skip_Pager_Hint --
   -------------------------

   function Get_Skip_Pager_Hint
     (Window : access Gtk_Window_Record)
      return Boolean
   is
      function Internal
        (Window : System.Address)
         return Gboolean;
      pragma Import (C, Internal, "gtk_window_get_skip_pager_hint");
   begin
      return Boolean'Val (Internal (Get_Object (Window)));
   end Get_Skip_Pager_Hint;

   ---------------------------
   -- Get_Skip_Taskbar_Hint --
   ---------------------------

   function Get_Skip_Taskbar_Hint
     (Window : access Gtk_Window_Record)
      return Boolean
   is
      function Internal (Window : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_window_get_skip_taskbar_hint");
   begin
      return Boolean'Val (Internal (Get_Object (Window)));
   end Get_Skip_Taskbar_Hint;

   -------------------
   -- Get_Type_Hint --
   -------------------

   function Get_Type_Hint
     (Window : access Gtk_Window_Record)
      return Gdk_Window_Type_Hint
   is
      function Internal (Window : System.Address) return Gdk_Window_Type_Hint;
      pragma Import (C, Internal, "gtk_window_get_type_hint");
   begin
      return Internal (Get_Object (Window));
   end Get_Type_Hint;

   ----------------------
   -- Get_Urgency_Hint --
   ----------------------

   function Get_Urgency_Hint
     (Window : access Gtk_Window_Record)
      return Boolean
   is
      function Internal
        (Window : System.Address)
         return Gboolean;
      pragma Import (C, Internal, "gtk_window_get_urgency_hint");
   begin
      return Boolean'Val (Internal (Get_Object (Window)));
   end Get_Urgency_Hint;

   ----------------------
   -- Group_Add_Window --
   ----------------------

   procedure Group_Add_Window
     (Window_Group : access Gtk_Window_Group_Record;
      Window       : access Gtk_Window_Record'Class)
   is
      procedure Internal
        (Window_Group : System.Address;
         Window       : System.Address);
      pragma Import (C, Internal, "gtk_window_group_add_window");
   begin
      Internal (Get_Object (Window_Group), Get_Object (Window));
   end Group_Add_Window;

   ----------------------
   -- Get_Default_Size --
   ----------------------

   procedure Get_Default_Size
     (Window : access Gtk_Window_Record;
      Width  : out Gint;
      Height : out Gint)
   is
      procedure Internal
        (Window : System.Address;
         Width  : out Gint;
         Height : out Gint);
      pragma Import (C, Internal, "gtk_window_get_default_size");
   begin
      Internal (Get_Object (Window), Width, Height);
   end Get_Default_Size;

   -----------------------------
   -- Get_Destroy_With_Parent --
   -----------------------------

   function Get_Destroy_With_Parent
     (Window : access Gtk_Window_Record)
      return Boolean
   is
      function Internal
        (Window : System.Address)
         return Gboolean;
      pragma Import (C, Internal, "gtk_window_get_destroy_with_parent");
   begin
      return Boolean'Val (Internal (Get_Object (Window)));
   end Get_Destroy_With_Parent;

   ----------------------
   -- Get_Focus_On_Map --
   ----------------------

   function Get_Focus_On_Map
     (Window : access Gtk_Window_Record)
      return Boolean
   is
      function Internal
        (Window : System.Address)
         return Gboolean;
      pragma Import (C, Internal, "gtk_window_get_focus_on_map");
   begin
      return Boolean'Val (Internal (Get_Object (Window)));
   end Get_Focus_On_Map;

   --------------------------
   -- Get_Frame_Dimensions --
   --------------------------

   procedure Get_Frame_Dimensions
     (Window : access Gtk_Window_Record;
      Left   : out Gint;
      Top    : out Gint;
      Right  : out Gint;
      Bottom : out Gint)
   is
      procedure Internal
        (Window : System.Address;
         Left   : out Gint;
         Top    : out Gint;
         Right  : out Gint;
         Bottom : out Gint);
      pragma Import (C, Internal, "gtk_window_get_frame_dimensions");
   begin
      Internal (Get_Object (Window), Left, Top, Right, Bottom);
   end Get_Frame_Dimensions;

   -------------------
   -- Get_Has_Frame --
   -------------------

   function Get_Has_Frame
     (Window : access Gtk_Window_Record)
      return Boolean
   is
      function Internal
        (Window : System.Address)
         return Gboolean;
      pragma Import (C, Internal, "gtk_window_get_has_frame");
   begin
      return Boolean'Val (Internal (Get_Object (Window)));
   end Get_Has_Frame;

   --------------
   -- Get_Icon --
   --------------

   function Get_Icon
     (Window : access Gtk_Window_Record)
      return Gdk_Pixbuf
   is
      function Internal
        (Window : System.Address)
         return System.Address;
      pragma Import (C, Internal, "gtk_window_get_icon");

   begin
      return Convert (Internal (Get_Object (Window)));
   end Get_Icon;

   -------------------
   -- Get_Icon_List --
   -------------------

   function Get_Icon_List
     (Window : access Gtk_Window_Record)
      return Object_Simple_List.Glist
   is
      use Glib.Object.Object_Simple_List;
      function Internal (Window : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_window_get_icon_list");
      List : Glist;
   begin
      Set_Object (List, Internal (Get_Object (Window)));
      return List;
   end Get_Icon_List;

   -------------------
   -- Get_Icon_Name --
   -------------------

   function Get_Icon_Name
     (Window : access Gtk_Window_Record)
      return String
   is
      function Internal
        (Window : System.Address)
         return chars_ptr;
      pragma Import (C, Internal, "gtk_window_get_icon_name");
   begin
      return Value (Internal (Get_Object (Window)));
   end Get_Icon_Name;

   ---------------------------
   -- Get_Mnemonic_Modifier --
   ---------------------------

   function Get_Mnemonic_Modifier
     (Window : access Gtk_Window_Record)
      return Gdk_Modifier_Type
   is
      function Internal (Window : System.Address) return Gdk_Modifier_Type;
      pragma Import (C, Internal, "gtk_window_get_mnemonic_modifier");
   begin
      return Internal (Get_Object (Window));
   end Get_Mnemonic_Modifier;

   ---------------
   -- Get_Modal --
   ---------------

   function Get_Modal
     (Window : access Gtk_Window_Record)
      return Boolean
   is
      function Internal (Window : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_window_get_modal");
   begin
      return Boolean'Val (Internal (Get_Object (Window)));
   end Get_Modal;

   ------------------
   -- Get_Position --
   ------------------

   procedure Get_Position
     (Window : access Gtk_Window_Record;
      Root_X : out Gint;
      Root_Y : out Gint)
   is
      procedure Internal
        (Window : System.Address;
         Root_X : out Gint;
         Root_Y : out Gint);
      pragma Import (C, Internal, "gtk_window_get_position");
   begin
      Internal (Get_Object (Window), Root_X, Root_Y);
   end Get_Position;

   ------------------
   -- Activate_Key --
   ------------------

   function Activate_Key
     (Window : access Gtk_Window_Record;
      Event  : Gdk.Event.Gdk_Event_Key)
      return Boolean
   is
      function Internal
        (Window : System.Address;
         Event  : Gdk.Event.Gdk_Event_Key)
         return Gboolean;
      pragma Import (C, Internal, "gtk_window_activate_key");
   begin
      return Boolean'Val (Internal (Get_Object (Window), Event));
   end Activate_Key;

   ---------------------
   -- Begin_Move_Drag --
   ---------------------

   procedure Begin_Move_Drag
     (Window    : access Gtk_Window_Record;
      Button    : Gint;
      Root_X    : Gint;
      Root_Y    : Gint;
      Timestamp : Guint32)
   is
      procedure Internal
        (Window    : System.Address;
         Button    : Gint;
         Root_X    : Gint;
         Root_Y    : Gint;
         Timestamp : Guint32);
      pragma Import (C, Internal, "gtk_window_begin_move_drag");
   begin
      Internal (Get_Object (Window), Button, Root_X, Root_Y, Timestamp);
   end Begin_Move_Drag;

   -----------------------
   -- Begin_Resize_Drag --
   -----------------------

   procedure Begin_Resize_Drag
     (Window    : access Gtk_Window_Record;
      Edge      : Gdk_Window_Edge;
      Button    : Gint;
      Root_X    : Gint;
      Root_Y    : Gint;
      Timestamp : Guint32)
   is
      procedure Internal
        (Window    : System.Address;
         Edge      : Gdk_Window_Edge;
         Button    : Gint;
         Root_X    : Gint;
         Root_Y    : Gint;
         Timestamp : Guint32);
      pragma Import (C, Internal, "gtk_window_begin_resize_drag");
   begin
      Internal (Get_Object (Window), Edge, Button, Root_X, Root_Y, Timestamp);
   end Begin_Resize_Drag;

   ----------------
   -- Fullscreen --
   ----------------

   procedure Fullscreen
     (Window : access Gtk_Window_Record)
   is
      procedure Internal (Window : System.Address);
      pragma Import (C, Internal, "gtk_window_fullscreen");
   begin
      Internal (Get_Object (Window));
   end Fullscreen;

   ----------------------
   -- Get_Accept_Focus --
   ----------------------

   function Get_Accept_Focus
     (Window : access Gtk_Window_Record)
      return Boolean
   is
      function Internal (Window : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_window_get_accept_focus");
   begin
      return Boolean'Val (Internal (Get_Object (Window)));
   end Get_Accept_Focus;

   -------------------
   -- Get_Decorated --
   -------------------

   function Get_Decorated
     (Window : access Gtk_Window_Record) return Boolean
   is
      function Internal (Window : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_window_get_decorated");
   begin
      return Boolean'Val (Internal (Get_Object (Window)));
   end Get_Decorated;

   ---------------------------
   -- Get_Default_Icon_List --
   ---------------------------

   function Get_Default_Icon_List
      return Glib.Object.Object_Simple_List.Glist
   is
      use Glib.Object.Object_Simple_List;
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_window_get_default_icon_list");
      List : Glist;
   begin
      Set_Object (List, Internal);
      return List;
   end Get_Default_Icon_List;

end Gtk.Window;
