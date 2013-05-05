-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                 Copyright (C) 2002-2013, AdaCore                  --
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

with Gdk.Types;            use Gdk.Types;
with Gtk.Accel_Group;      use Gtk.Accel_Group;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;               use System;

with Glib.Type_Conversion_Hooks;

package body Gtk.Accel_Map is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Accel_Map_Record);
   pragma Warnings (Off, Type_Conversion);

   ---------
   -- Get --
   ---------

   function Get return Gtk_Accel_Map is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_accel_map_get");
      Stub : Gtk_Accel_Map_Record;
   begin
      return Gtk_Accel_Map
        (Get_User_Data (Internal, Stub));
   end Get;

   ----------
   -- Save --
   ----------

   procedure Save (File_Name : String) is
      procedure Internal (File_Name : String);
      pragma Import (C, Internal, "gtk_accel_map_save");
   begin
      Internal (File_Name & ASCII.NUL);
   end Save;

   ----------
   -- Load --
   ----------

   procedure Load (File_Name : String) is
      procedure Internal (File_Name : String);
      pragma Import (C, Internal, "gtk_accel_map_load");
   begin
      Internal (File_Name & ASCII.NUL);
   end Load;

   ---------------
   -- Add_Entry --
   ---------------

   procedure Add_Entry
     (Accel_Path : UTF8_String;
      Accel_Key  : Gdk.Types.Gdk_Key_Type;
      Accel_Mods : Gdk.Types.Gdk_Modifier_Type)
   is
      procedure Internal
        (P : UTF8_String; K : Gdk_Key_Type; M : Gdk_Modifier_Type);
      pragma Import (C, Internal, "gtk_accel_map_add_entry");
   begin
      Internal (Accel_Path & ASCII.NUL, Accel_Key, Accel_Mods);
   end Add_Entry;

   ------------------
   -- Lookup_Entry --
   ------------------

   procedure Lookup_Entry
     (Accel_Path : UTF8_String;
      Key        : out Gtk.Accel_Group.Gtk_Accel_Key;
      Found      : out Boolean)
   is
      type Gtk_Accel_Key_Access is access all Gtk_Accel_Key;
      function Internal
        (Path : UTF8_String; Key : Gtk_Accel_Key_Access) return Integer;
      pragma Import (C, Internal, "gtk_accel_map_lookup_entry");

      K : aliased Gtk_Accel_Key;

   begin
      Found := Boolean'Val (Internal (Accel_Path & ASCII.NUL, K'Access));
      Key := K;
   end Lookup_Entry;

   ------------------
   -- Change_Entry --
   ------------------

   function Change_Entry
     (Accel_Path : UTF8_String;
      Accel_Key  : Gdk.Types.Gdk_Key_Type;
      Accel_Mods : Gdk.Types.Gdk_Modifier_Type;
      Replace    : Boolean) return Boolean
   is
      function Internal
        (P : UTF8_String;
         K : Gdk_Key_Type; M : Gdk_Modifier_Type; R : Integer)
         return Gboolean;
      pragma Import (C, Internal, "gtk_accel_map_change_entry");
   begin
      return Boolean'Val
        (Internal (Accel_Path & ASCII.NUL, Accel_Key, Accel_Mods,
         Boolean'Pos (Replace)));
   end Change_Entry;

   -------------
   -- Foreach --
   -------------

   type Data_Wrapper is record
      Data : System.Address;
      Func : Gtk_Accel_Map_Foreach;
   end record;

   procedure Foreach_Wrapper
     (Data       : access Data_Wrapper;
      Accel_Path : Interfaces.C.Strings.chars_ptr;
      Accel_Key  : Gdk.Types.Gdk_Key_Type;
      Accel_Mods : Gdk.Types.Gdk_Modifier_Type;
      Changed    : Gboolean);
   pragma Convention (C, Foreach_Wrapper);
   --  Wrapper called by gtk_accel_map_foreach

   procedure Foreach_Wrapper
     (Data       : access Data_Wrapper;
      Accel_Path : Interfaces.C.Strings.chars_ptr;
      Accel_Key  : Gdk.Types.Gdk_Key_Type;
      Accel_Mods : Gdk.Types.Gdk_Modifier_Type;
      Changed    : Gboolean) is
   begin
      Data.Func
        (Data.Data, Value (Accel_Path), Accel_Key,
         Accel_Mods, Boolean'Val (Changed));
   end Foreach_Wrapper;

   -------------
   -- Foreach --
   -------------

   procedure Foreach
     (Data : System.Address; Func : Gtk_Accel_Map_Foreach)
   is
      procedure Internal (Data : access Data_Wrapper; Func : System.Address);
      pragma Import (C, Internal, "gtk_accel_map_foreach");

      D : aliased Data_Wrapper := (Data, Func);
   begin
      Internal (D'Access, Foreach_Wrapper'Address);
   end Foreach;

   ------------------------
   -- Foreach_Unfiltered --
   ------------------------

   procedure Foreach_Unfiltered
     (Data : System.Address; Func : Gtk_Accel_Map_Foreach)
   is
      procedure Internal (Data : access Data_Wrapper; Func : System.Address);
      pragma Import (C, Internal, "gtk_accel_map_foreach_unfiltered");

      D : aliased Data_Wrapper := (Data, Func);
   begin
      Internal (D'Access, Foreach_Wrapper'Address);
   end Foreach_Unfiltered;

   ----------------
   -- Add_Filter --
   ----------------

   procedure Add_Filter (Filter_Pattern : String) is
      procedure Internal (Filter_Pattern : String);
      pragma Import (C, Internal, "gtk_accel_map_add_filter");
   begin
      Internal (Filter_Pattern & ASCII.NUL);
   end Add_Filter;

   ---------------
   -- Lock_Path --
   ---------------

   procedure Lock_Path (Accel_Path : String) is
      procedure Internal (Accel_Path : String);
      pragma Import (C, Internal, "gtk_accel_map_lock_path");
   begin
      Internal (Accel_Path & ASCII.NUL);
   end Lock_Path;

   -----------------
   -- Unlock_Path --
   -----------------

   procedure Unlock_Path (Accel_Path : String) is
      procedure Internal (Accel_Path : String);
      pragma Import (C, Internal, "gtk_accel_map_unlock_path");
   begin
      Internal (Accel_Path & ASCII.NUL);
   end Unlock_Path;

end Gtk.Accel_Map;
