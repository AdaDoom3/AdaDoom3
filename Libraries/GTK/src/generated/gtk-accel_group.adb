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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Interfaces.C.Strings;       use Interfaces.C.Strings;

package body Gtk.Accel_Group is
   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Accel_Group_Record);
   pragma Unreferenced (Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Accel_Group : out Gtk_Accel_Group) is
   begin
      Accel_Group := new Gtk_Accel_Group_Record;
      Gtk.Accel_Group.Initialize (Accel_Group);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Accel_Group : access Gtk_Accel_Group_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_accel_group_new");
   begin
      Set_Object (Accel_Group, Internal);
   end Initialize;

   --------------
   -- Activate --
   --------------

   function Activate
      (Accel_Group   : access Gtk_Accel_Group_Record;
       Accel_Quark   : GQuark;
       Acceleratable : access Glib.Object.GObject_Record'Class;
       Accel_Key     : Guint;
       Accel_Mods    : Gdk.Types.Gdk_Modifier_Type) return Boolean
   is
      function Internal
         (Accel_Group   : System.Address;
          Accel_Quark   : GQuark;
          Acceleratable : System.Address;
          Accel_Key     : Guint;
          Accel_Mods    : Gdk.Types.Gdk_Modifier_Type) return Integer;
      pragma Import (C, Internal, "gtk_accel_group_activate");
   begin
      return Boolean'Val (Internal (Get_Object (Accel_Group), Accel_Quark, Get_Object (Acceleratable), Accel_Key, Accel_Mods));
   end Activate;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Accel_Group : access Gtk_Accel_Group_Record;
       Accel_Key   : Guint;
       Accel_Mods  : Gdk.Types.Gdk_Modifier_Type;
       Accel_Flags : Gtk_Accel_Flags;
       Closure     : C_Gtk_Accel_Group_Activate)
   is
      procedure Internal
         (Accel_Group : System.Address;
          Accel_Key   : Guint;
          Accel_Mods  : Gdk.Types.Gdk_Modifier_Type;
          Accel_Flags : Gtk_Accel_Flags;
          Closure     : C_Gtk_Accel_Group_Activate);
      pragma Import (C, Internal, "gtk_accel_group_connect");
   begin
      Internal (Get_Object (Accel_Group), Accel_Key, Accel_Mods, Accel_Flags, Closure);
   end Connect;

   ---------------------
   -- Connect_By_Path --
   ---------------------

   procedure Connect_By_Path
      (Accel_Group : access Gtk_Accel_Group_Record;
       Accel_Path  : UTF8_String;
       Closure     : C_Gtk_Accel_Group_Activate)
   is
      procedure Internal
         (Accel_Group : System.Address;
          Accel_Path  : Interfaces.C.Strings.chars_ptr;
          Closure     : C_Gtk_Accel_Group_Activate);
      pragma Import (C, Internal, "gtk_accel_group_connect_by_path");
      Tmp_Accel_Path : Interfaces.C.Strings.chars_ptr := New_String (Accel_Path);
   begin
      Internal (Get_Object (Accel_Group), Tmp_Accel_Path, Closure);
      Free (Tmp_Accel_Path);
   end Connect_By_Path;

   ----------------
   -- Disconnect --
   ----------------

   function Disconnect
      (Accel_Group : access Gtk_Accel_Group_Record;
       Closure     : C_Gtk_Accel_Group_Activate) return Boolean
   is
      function Internal
         (Accel_Group : System.Address;
          Closure     : C_Gtk_Accel_Group_Activate) return Integer;
      pragma Import (C, Internal, "gtk_accel_group_disconnect");
   begin
      return Boolean'Val (Internal (Get_Object (Accel_Group), Closure));
   end Disconnect;

   --------------------
   -- Disconnect_Key --
   --------------------

   function Disconnect_Key
      (Accel_Group : access Gtk_Accel_Group_Record;
       Accel_Key   : Guint;
       Accel_Mods  : Gdk.Types.Gdk_Modifier_Type) return Boolean
   is
      function Internal
         (Accel_Group : System.Address;
          Accel_Key   : Guint;
          Accel_Mods  : Gdk.Types.Gdk_Modifier_Type) return Integer;
      pragma Import (C, Internal, "gtk_accel_group_disconnect_key");
   begin
      return Boolean'Val (Internal (Get_Object (Accel_Group), Accel_Key, Accel_Mods));
   end Disconnect_Key;

   ----------
   -- Find --
   ----------

   function Find
      (Accel_Group : access Gtk_Accel_Group_Record;
       Find_Func   : C_Gtk_Accel_Group_Find_Func;
       Data        : System.Address) return Gtk_Accel_Key
   is
      function Internal
         (Accel_Group : System.Address;
          Find_Func   : C_Gtk_Accel_Group_Find_Func;
          Data        : System.Address) return Gtk_Accel_Key;
      pragma Import (C, Internal, "gtk_accel_group_find");
   begin
      return Internal (Get_Object (Accel_Group), Find_Func, Data);
   end Find;

   -------------------
   -- Get_Is_Locked --
   -------------------

   function Get_Is_Locked
      (Accel_Group : access Gtk_Accel_Group_Record) return Boolean
   is
      function Internal (Accel_Group : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_accel_group_get_is_locked");
   begin
      return Boolean'Val (Internal (Get_Object (Accel_Group)));
   end Get_Is_Locked;

   -----------------------
   -- Get_Modifier_Mask --
   -----------------------

   function Get_Modifier_Mask
      (Accel_Group : access Gtk_Accel_Group_Record)
       return Gdk.Types.Gdk_Modifier_Type
   is
      function Internal
         (Accel_Group : System.Address) return Gdk.Types.Gdk_Modifier_Type;
      pragma Import (C, Internal, "gtk_accel_group_get_modifier_mask");
   begin
      return Internal (Get_Object (Accel_Group));
   end Get_Modifier_Mask;

   ----------
   -- Lock --
   ----------

   procedure Lock (Accel_Group : access Gtk_Accel_Group_Record) is
      procedure Internal (Accel_Group : System.Address);
      pragma Import (C, Internal, "gtk_accel_group_lock");
   begin
      Internal (Get_Object (Accel_Group));
   end Lock;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (Accel_Group : access Gtk_Accel_Group_Record) is
      procedure Internal (Accel_Group : System.Address);
      pragma Import (C, Internal, "gtk_accel_group_unlock");
   begin
      Internal (Get_Object (Accel_Group));
   end Unlock;

   ---------------------------
   -- Accel_Groups_Activate --
   ---------------------------

   function Accel_Groups_Activate
      (Object     : access Glib.Object.GObject_Record'Class;
       Accel_Key  : Gdk.Types.Gdk_Key_Type;
       Accel_Mods : Gdk.Types.Gdk_Modifier_Type) return Boolean
   is
      function Internal
         (Object     : System.Address;
          Accel_Key  : Gdk.Types.Gdk_Key_Type;
          Accel_Mods : Gdk.Types.Gdk_Modifier_Type) return Integer;
      pragma Import (C, Internal, "gtk_accel_groups_activate");
   begin
      return Boolean'Val (Internal (Get_Object (Object), Accel_Key, Accel_Mods));
   end Accel_Groups_Activate;

   ---------------------------
   -- Accelerator_Get_Label --
   ---------------------------

   function Accelerator_Get_Label
      (Accelerator_Key  : Gdk.Types.Gdk_Key_Type;
       Accelerator_Mods : Gdk.Types.Gdk_Modifier_Type) return UTF8_String
   is
      function Internal
         (Accelerator_Key  : Gdk.Types.Gdk_Key_Type;
          Accelerator_Mods : Gdk.Types.Gdk_Modifier_Type)
          return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_accelerator_get_label");
   begin
      return Interfaces.C.Strings.Value (Internal (Accelerator_Key, Accelerator_Mods));
   end Accelerator_Get_Label;

   ----------------------
   -- Accelerator_Name --
   ----------------------

   function Accelerator_Name
      (Accelerator_Key  : Gdk.Types.Gdk_Key_Type;
       Accelerator_Mods : Gdk.Types.Gdk_Modifier_Type) return UTF8_String
   is
      function Internal
         (Accelerator_Key  : Gdk.Types.Gdk_Key_Type;
          Accelerator_Mods : Gdk.Types.Gdk_Modifier_Type)
          return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_accelerator_name");
   begin
      return Interfaces.C.Strings.Value (Internal (Accelerator_Key, Accelerator_Mods));
   end Accelerator_Name;

   -----------------------
   -- Accelerator_Parse --
   -----------------------

   procedure Accelerator_Parse
      (Accelerator      : UTF8_String;
       Accelerator_Key  : out Gdk.Types.Gdk_Key_Type;
       Accelerator_Mods : out Gdk.Types.Gdk_Modifier_Type)
   is
      procedure Internal
         (Accelerator      : Interfaces.C.Strings.chars_ptr;
          Accelerator_Key  : out Gdk.Types.Gdk_Key_Type;
          Accelerator_Mods : out Gdk.Types.Gdk_Modifier_Type);
      pragma Import (C, Internal, "gtk_accelerator_parse");
      Tmp_Accelerator : Interfaces.C.Strings.chars_ptr := New_String (Accelerator);
   begin
      Internal (Tmp_Accelerator, Accelerator_Key, Accelerator_Mods);
      Free (Tmp_Accelerator);
   end Accelerator_Parse;

   -----------------------
   -- Accelerator_Valid --
   -----------------------

   function Accelerator_Valid
      (Keyval    : Gdk.Types.Gdk_Key_Type;
       Modifiers : Gdk.Types.Gdk_Modifier_Type) return Boolean
   is
      function Internal
         (Keyval    : Gdk.Types.Gdk_Key_Type;
          Modifiers : Gdk.Types.Gdk_Modifier_Type) return Integer;
      pragma Import (C, Internal, "gtk_accelerator_valid");
   begin
      return Boolean'Val (Internal (Keyval, Modifiers));
   end Accelerator_Valid;

   ------------------------
   -- From_Accel_Closure --
   ------------------------

   function From_Accel_Closure
      (Closure : C_Gtk_Accel_Group_Activate)
       return Gtk.Accel_Group.Gtk_Accel_Group
   is
      function Internal
         (Closure : C_Gtk_Accel_Group_Activate) return System.Address;
      pragma Import (C, Internal, "gtk_accel_group_from_accel_closure");
      Stub : Gtk_Accel_Group_Record;
   begin
      return Gtk.Accel_Group.Gtk_Accel_Group (Get_User_Data (Internal (Closure), Stub));
   end From_Accel_Closure;

   -----------------
   -- From_Object --
   -----------------

   function From_Object
      (Object : access Glib.Object.GObject_Record'Class)
       return Glib.Object.Object_List.GSList
   is
      function Internal (Object : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_accel_groups_from_object");
      Tmp_Return : Glib.Object.Object_List.GSList;
   begin
      Glib.Object.Object_List.Set_Object (Tmp_Return, Internal (Get_Object (Object)));
      return Tmp_Return;
   end From_Object;

   --------------------------
   -- Get_Default_Mod_Mask --
   --------------------------

   function Get_Default_Mod_Mask return Gdk.Types.Gdk_Modifier_Type is
      function Internal return Gdk.Types.Gdk_Modifier_Type;
      pragma Import (C, Internal, "gtk_accelerator_get_default_mod_mask");
   begin
      return Internal;
   end Get_Default_Mod_Mask;

   --------------------------
   -- Set_Default_Mod_Mask --
   --------------------------

   procedure Set_Default_Mod_Mask
      (Default_Mod_Mask : Gdk.Types.Gdk_Modifier_Type)
   is
      procedure Internal (Default_Mod_Mask : Gdk.Types.Gdk_Modifier_Type);
      pragma Import (C, Internal, "gtk_accelerator_set_default_mod_mask");
   begin
      Internal (Default_Mod_Mask);
   end Set_Default_Mod_Mask;

end Gtk.Accel_Group;
