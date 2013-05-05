-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                   Copyright (C) 2002-2007 AdaCore                 --
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
--  An accel_map provides support for loading and saving accelerators (see
--  also Gtk.Accel_Group).
--  </description>
--  <c_version>2.8.17</c_version>

with Glib;
with Gdk.Types;
with Gtk.Accel_Group;

package Gtk.Accel_Map is

   type Gtk_Accel_Map_Record is
     new Glib.Object.GObject_Record with null record;
   type Gtk_Accel_Map is access all Gtk_Accel_Map_Record'Class;

   function Get return Gtk_Accel_Map;
   --  Gets the singleton global Gtk_Accel_Map object. This object
   --  is useful only for notification of changes to the accelerator
   --  map via the ::changed signal; it isn't a parameter to the
   --  other accelerator map functions.

   function Get_Type return Glib.GType;
   --  Return the internal type used for a Gtk_Accel_Map

   procedure Save (File_Name : String);
   --  Save the key shortcuts to a file. These are the shortcuts that might
   --  have been changed dynamically by the user, if the RC file (see Gtk.RC)
   --  contained the line "gtk-can-change-accels=1"

   procedure Load (File_Name : String);
   --  Load the key shortcuts from a file

   procedure Add_Entry
     (Accel_Path : String;
      Accel_Key  : Gdk.Types.Gdk_Key_Type;
      Accel_Mods : Gdk.Types.Gdk_Modifier_Type);
   --  Register a new accelerator for a given menu item, within the global
   --  accelerator map.
   --  This function should only be called once per Accel_Path. To change it
   --  programmatically during runtime, use Change_Entry.
   --  Accel_Path is of the form:
   --     <app>/Category1/Category2/.../Action",
   --  where "app" is a unique, application-specific identifier (for examples
   --  of valid Accel_Path, check the file created by Save above).
   --
   --  For instance, the path in the testgtk application for the menu
   --  File->Open would be
   --     <testgtk>/file/open
   --
   --  Generally, the path need to be set explicitely for an item, through a
   --  call to Gtk.Menu_Item.Set_Accel_Path or
   --  Gtk.Widget.Set_Accel_Path. However, if the widget is created
   --  automatically through a Gtk.Item_Factory, this is done automatically.
   --
   --  It is better to use this function instead of Add_Accelerator, since when
   --  the accelerators are changed interactively by the user, the new value
   --  will be shown properly in the menu, which wouldn't happen if they had
   --  been forced by Add_Accelerator.

   procedure Lookup_Entry
     (Accel_Path : String;
      Key        : out Gtk.Accel_Group.Gtk_Accel_Key;
      Found      : out Boolean);
   --  Look up the accelerator for Accel_Path, and set Key appropriately. If no
   --  accelerator was set, Found is set to False, and the value of Key is
   --  meaningless.

   function Change_Entry
     (Accel_Path : String;
      Accel_Key  : Gdk.Types.Gdk_Key_Type;
      Accel_Mods : Gdk.Types.Gdk_Modifier_Type;
      Replace    : Boolean) return Boolean;
   --  Change the accelerator currently associated wtih Accel_Path.
   --  A change may not always be possible due to conflicts with other
   --  accelerators. Replace should be set to True if other accelerators may be
   --  deleted to resolve such conflicts.
   --  Returns True if the entry could be changed

   procedure Lock_Path   (Accel_Path : String);
   procedure Unlock_Path (Accel_Path : String);
   --  Locks the given accelerator path. If the accelerator map doesn't yet
   --  contain an entry for Accel_Path, a new one is created.
   --
   --  Locking an accelerator path prevents its accelerator from being changed
   --  during runtime. A locked accelerator path can be unlocked by
   --  Unlock_Path. Refer to Change_Entry for information about runtime
   --  accelerator changes.
   --
   --  If called more than once, Accel_Path remains locked until Unlock_Path
   --  has been called an equivalent number of times.
   --
   --  Note that locking of individual accelerator paths is independent from
   --  locking the Gtk_Accel_Group containing them. For runtime accelerator
   --  changes to be possible both the accelerator path and its accel group
   --  have to be unlocked.

   -------------
   -- Foreach --
   -------------

   procedure Add_Filter (Filter_Pattern : String);
   --  Adds a filter to the global list of accel path filters.
   --  Accel map entries whose accel path matches one of the filters
   --  are skipped by Foreach.
   --  This function is intended for GTK+ modules that create their own
   --  menus, but don't want them to be saved into the applications accelerator
   --  map dump.

   type Gtk_Accel_Map_Foreach is access procedure
     (Data       : System.Address;
      Accel_Path : String;
      Accel_Key  : Gdk.Types.Gdk_Key_Type;
      Accel_Mods : Gdk.Types.Gdk_Modifier_Type;
      Changed    : Boolean);
   --  Changed is set to true if the keybinding was changed manually by the
   --  user, and thus would need saving during an accelerator map dump.

   procedure Foreach
     (Data : System.Address; Func : Gtk_Accel_Map_Foreach);
   --  Calls Func for each of the currently defined key shortcuts.
   --  Data is passed as is to Func

   procedure Foreach_Unfiltered
     (Data : System.Address; Func : Gtk_Accel_Map_Foreach);
   --  Loops over all entries in the accelerator map, and execute
   --  Func on each.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "changed"
   --    procedure Handler
   --      (Map : Gtk_Accel_Map;
   --       Accel_Path : String;
   --       Accel_Key  : Gdk_Key_Type;
   --       Accel_Mods : Gdk_Modifier_Type);
   --    Notifies of a change in the global accelerator map. The path is also
   --    used as the detail for the signal, so it is possible to connect to
   --    changed::accel_path.
   --
   --  </signals>

   Signal_Changed : constant Glib.Signal_Name := "changed";

private
   pragma Import (C, Get_Type, "gtk_accel_map_get_type");
end Gtk.Accel_Map;

--  No binding: gtk_accel_map_load_fd
--  No binding: gtk_accel_map_save_fd
--  No binding: gtk_accel_map_load_scanner


