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

--  <description>
--  An accel group represents a group of keyboard accelerators, generally
--  attached to a toplevel window. Accelerators are different from mnemonics.
--  Accelerators are shortcuts for activating a menu item. They appear
--  alongside the menu item they are a shortcut for. Mnemonics are shortcuts
--  for GUI elements, such as buttons. They appear as underline characters.
--  Menu items can have both.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk;             use Gdk;
with Gdk.Types;       use Gdk.Types;
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;

package Gtk.Accel_Group is

   type Gtk_Accel_Group_Record is new GObject_Record with null record;
   type Gtk_Accel_Group is access all Gtk_Accel_Group_Record'Class;

   type Gtk_Accel_Flags is new Guint;
   Accel_Visible : constant Gtk_Accel_Flags := 2 ** 0;
   Accel_Locked  : constant Gtk_Accel_Flags := 2 ** 1;
   Accel_Mask    : constant Gtk_Accel_Flags := 16#07#;

   type Gtk_Accel_Key is record
      Accel_Key  : Gdk.Types.Gdk_Key_Type;
      Accel_Mods : Gdk.Types.Gdk_Modifier_Type;
      Flags      : Gtk_Accel_Flags;
   end record;
   pragma Convention (C, Gtk_Accel_Key);

   type Gtk_Accel_Group_Activate is access function
     (Accel_Group   : access Gtk_Accel_Group_Record'Class;
      Acceleratable : Glib.Object.GObject;
      Keyval        : Gdk.Types.Gdk_Key_Type;
      Modifier      : Gdk.Types.Gdk_Modifier_Type) return Boolean;

   type C_Gtk_Accel_Group_Activate is access function
     (Accel_Group   : System.Address;
      Acceleratable : System.Address;
      Keyval        : Gdk.Types.Gdk_Key_Type;
      Modifier      : Gdk.Types.Gdk_Modifier_Type) return Boolean;
   pragma Convention (C, C_Gtk_Accel_Group_Activate);
   --  Same as Gtk_Accel_Group_Activate, but passing directly the C values.
   --  You must use Get_User_Data to convert to the Ada types.

   type C_Gtk_Accel_Group_Find_Func is access function
     (Key           : access Gtk_Accel_Key;
      Closure       : C_Gtk_Accel_Group_Activate;
      Data          : System.Address) return Boolean;
   pragma Convention (C, C_Gtk_Accel_Group_Find_Func);
   --  When a match is found, must return True.
   --  Must not modify Key

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Accel_Group : out Gtk_Accel_Group);
   procedure Initialize (Accel_Group : access Gtk_Accel_Group_Record'Class);
   --  Creates a new Gtk.Accel_Group.Gtk_Accel_Group. Remember to call
   --  Gtk.Window.Add_Accel_Group to active the group.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_accel_group_get_type");

   -------------
   -- Methods --
   -------------

   function Activate
      (Accel_Group   : access Gtk_Accel_Group_Record;
       Accel_Quark   : GQuark;
       Acceleratable : access Glib.Object.GObject_Record'Class;
       Accel_Key     : Guint;
       Accel_Mods    : Gdk.Types.Gdk_Modifier_Type) return Boolean;
   --  Finds the first accelerator in Accel_Group that matches Accel_Key and
   --  Accel_Mods, and activates it.
   --  "accel_quark": the quark for the accelerator name
   --  "acceleratable": the GObject, usually a Gtk.Window.Gtk_Window, on which
   --  to activate the accelerator.
   --  "accel_key": accelerator keyval from a key event
   --  "accel_mods": keyboard state mask from a key event

   procedure Connect
      (Accel_Group : access Gtk_Accel_Group_Record;
       Accel_Key   : Guint;
       Accel_Mods  : Gdk.Types.Gdk_Modifier_Type;
       Accel_Flags : Gtk_Accel_Flags;
       Closure     : C_Gtk_Accel_Group_Activate);
   --  Installs an accelerator in this group. When Accel_Group is being
   --  activated in response to a call to
   --  Gtk.Accel_Group.Accel_Groups_Activate, Closure will be invoked if the
   --  Accel_Key and Accel_Mods from Gtk.Accel_Group.Accel_Groups_Activate
   --  match those of this connection. The signature used for the Closure is
   --  that of GtkAccelGroupActivate. Note that, due to implementation details,
   --  a single closure can only be connected to one accelerator group.
   --  "accel_key": key value of the accelerator
   --  "accel_mods": modifier combination of the accelerator
   --  "accel_flags": a flag mask to configure this accelerator
   --  "closure": closure to be executed upon accelerator activation

   procedure Connect_By_Path
      (Accel_Group : access Gtk_Accel_Group_Record;
       Accel_Path  : UTF8_String;
       Closure     : C_Gtk_Accel_Group_Activate);
   --  Installs an accelerator in this group, using an accelerator path to
   --  look up the appropriate key and modifiers (see Gtk.Accel_Map.Add_Entry).
   --  When Accel_Group is being activated in response to a call to
   --  Gtk.Accel_Group.Accel_Groups_Activate, Closure will be invoked if the
   --  Accel_Key and for the path. The signature used for the Closure is that
   --  of GtkAccelGroupActivate. Note that Accel_Path string will be stored in
   --  a GQuark. Therefore, if you pass a static string, you can save some
   --  memory by interning it first with g_intern_static_string.
   --  "accel_path": path used for determining key and modifiers.
   --  "closure": closure to be executed upon accelerator activation

   function Disconnect
      (Accel_Group : access Gtk_Accel_Group_Record;
       Closure     : C_Gtk_Accel_Group_Activate) return Boolean;
   --  Removes an accelerator previously installed through
   --  Gtk.Accel_Group.Connect. Since 2.20 Closure can be null.
   --  "closure": the closure to remove from this accelerator group, or null
   --  to remove all closures

   function Disconnect_Key
      (Accel_Group : access Gtk_Accel_Group_Record;
       Accel_Key   : Guint;
       Accel_Mods  : Gdk.Types.Gdk_Modifier_Type) return Boolean;
   --  Removes an accelerator previously installed through
   --  Gtk.Accel_Group.Connect.
   --  "accel_key": key value of the accelerator
   --  "accel_mods": modifier combination of the accelerator

   function Find
      (Accel_Group : access Gtk_Accel_Group_Record;
       Find_Func   : C_Gtk_Accel_Group_Find_Func;
       Data        : System.Address) return Gtk_Accel_Key;
   --  Finds the first entry in an accelerator group for which
   --  "find_func": a function to filter the entries of Accel_Group with
   --  "data": data to pass to Find_Func

   function Get_Is_Locked
      (Accel_Group : access Gtk_Accel_Group_Record) return Boolean;
   --  Locks are added and removed using Gtk.Accel_Group.Lock and
   --  Gtk.Accel_Group.Unlock. False otherwise.
   --  Since: gtk+ 2.14

   function Get_Modifier_Mask
      (Accel_Group : access Gtk_Accel_Group_Record)
       return Gdk.Types.Gdk_Modifier_Type;
   --  Gets a Gdk.Types.Gdk_Modifier_Type representing the mask for this
   --  Since: gtk+ 2.14

   procedure Lock (Accel_Group : access Gtk_Accel_Group_Record);
   --  Locks the given accelerator group. Locking an acelerator group prevents
   --  the accelerators contained within it to be changed during runtime. Refer
   --  to Gtk.Accel_Map.Change_Entry about runtime accelerator changes. If
   --  called more than once, Accel_Group remains locked until
   --  Gtk.Accel_Group.Unlock has been called an equivalent number of times.

   procedure Unlock (Accel_Group : access Gtk_Accel_Group_Record);
   --  Undoes the last call to Gtk.Accel_Group.Lock on this Accel_Group.

   ---------------
   -- Functions --
   ---------------

   function From_Accel_Closure
      (Closure : C_Gtk_Accel_Group_Activate)
       return Gtk.Accel_Group.Gtk_Accel_Group;
   --  Finds the Gtk.Accel_Group.Gtk_Accel_Group to which Closure is
   --  connected; see Gtk.Accel_Group.Connect.
   --  "closure": a GClosure

   function Accel_Groups_Activate
      (Object     : access Glib.Object.GObject_Record'Class;
       Accel_Key  : Gdk.Types.Gdk_Key_Type;
       Accel_Mods : Gdk.Types.Gdk_Modifier_Type) return Boolean;
   --  Finds the first accelerator in any Gtk.Accel_Group.Gtk_Accel_Group
   --  attached to Object that matches Accel_Key and Accel_Mods, and activates
   --  that accelerator.
   --  "object": the GObject, usually a Gtk.Window.Gtk_Window, on which to
   --  activate the accelerator.
   --  "accel_key": accelerator keyval from a key event
   --  "accel_mods": keyboard state mask from a key event

   function From_Object
      (Object : access Glib.Object.GObject_Record'Class)
       return Glib.Object.Object_List.GSlist;
   --  Gets a list of all accel groups which are attached to Object.
   --  "object": a GObject, usually a Gtk.Window.Gtk_Window

   function Accelerator_Valid
      (Keyval    : Gdk.Types.Gdk_Key_Type;
       Modifiers : Gdk.Types.Gdk_Modifier_Type) return Boolean;
   --  Determines whether a given keyval and modifier mask constitute a valid
   --  keyboard accelerator. For example, the GDK_a keyval plus
   --  GDK_CONTROL_MASK is valid - this is a "Ctrl+a" accelerator. But, you
   --  can't, for instance, use the GDK_Control_L keyval as an accelerator.
   --  "keyval": a GDK keyval
   --  "modifiers": modifier mask

   procedure Accelerator_Parse
      (Accelerator      : UTF8_String;
       Accelerator_Key  : out Gdk.Types.Gdk_Key_Type;
       Accelerator_Mods : out Gdk.Types.Gdk_Modifier_Type);
   --  Parses a string representing an accelerator. The format looks like
   --  "&lt;Control&gt;a" or "&lt;Shift&gt;&lt;Alt&gt;F1" or "&lt;Release&gt;z"
   --  (the last one is for key release). The parser is fairly liberal and
   --  allows lower or upper case, and also abbreviations such as "&lt;Ctl&gt;"
   --  and "&lt;Ctrl&gt;". Key names are parsed using gdk_keyval_from_name. For
   --  character keys the name is not the symbol, but the lowercase name, e.g.
   --  one would use "&lt;Ctrl&gt;minus" instead of "&lt;Ctrl&gt;-". If the
   --  parse fails, Accelerator_Key and Accelerator_Mods will be set to 0
   --  (zero).
   --  "accelerator": string representing an accelerator
   --  "accelerator_key": return location for accelerator keyval
   --  "accelerator_mods": return location for accelerator modifier mask

   function Accelerator_Name
      (Accelerator_Key  : Gdk.Types.Gdk_Key_Type;
       Accelerator_Mods : Gdk.Types.Gdk_Modifier_Type) return UTF8_String;
   --  Converts an accelerator keyval and modifier mask into a string
   --  parseable by Gtk.Accel_Group.Accelerator_Parse. For example, if you pass
   --  in GDK_q and GDK_CONTROL_MASK, this function returns "&lt;Control&gt;q".
   --  If you need to display accelerators in the user interface, see
   --  Gtk.Accel_Group.Accelerator_Get_Label.
   --  "accelerator_key": accelerator keyval
   --  "accelerator_mods": accelerator modifier mask

   function Accelerator_Get_Label
      (Accelerator_Key  : Gdk.Types.Gdk_Key_Type;
       Accelerator_Mods : Gdk.Types.Gdk_Modifier_Type) return UTF8_String;
   --  Converts an accelerator keyval and modifier mask into a string which
   --  can be used to represent the accelerator to the user.
   --  Since: gtk+ 2.6
   --  "accelerator_key": accelerator keyval
   --  "accelerator_mods": accelerator modifier mask

   procedure Set_Default_Mod_Mask
      (Default_Mod_Mask : Gdk.Types.Gdk_Modifier_Type);
   function Get_Default_Mod_Mask return Gdk.Types.Gdk_Modifier_Type;
   --  Gets the value set by Gtk.Accel_Group.Set_Default_Mod_Mask.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Is_Locked_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Modifier_Mask_Property
   --  Type: Gdk.ModifierType
   --  Flags: read-write

   Is_Locked_Property : constant Glib.Properties.Property_Boolean;
   Modifier_Mask_Property : constant Glib.Properties.Property_Boxed;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "accel-activate"
   --     function Handler
   --       (Self          : access Gtk_Accel_Group_Record'Class;
   --        Acceleratable : Glib.Object.GObject;
   --        Keyval        : Guint;
   --        Modifier      : Gdk.ModifierType) return Boolean;
   --    --  "acceleratable": the object on which the accelerator was activated
   --    --  "keyval": the accelerator keyval
   --    --  "modifier": the modifier combination of the accelerator
   --  The accel-activate signal is an implementation detail of
   --  Gtk.Accel_Group.Gtk_Accel_Group and not meant to be used by
   --  applications.
   --  Returns True if the accelerator was activated
   --
   --  "accel-changed"
   --     procedure Handler
   --       (Self          : access Gtk_Accel_Group_Record'Class;
   --        Keyval        : Guint;
   --        Modifier      : Gdk.ModifierType;
   --        Accel_Closure : System.Address);
   --    --  "keyval": the accelerator keyval
   --    --  "modifier": the modifier combination of the accelerator
   --    --  "accel_closure": the GClosure of the accelerator
   --  The accel-changed signal is emitted when a GtkAccelGroupEntry is added
   --  to or removed from the accel group. Widgets like
   --  Gtk.Accellabel.Gtk_Accellabel which display an associated accelerator
   --  should connect to this signal, and rebuild their visual representation
   --  if the Accel_Closure is theirs.

   Signal_Accel_Activate : constant Glib.Signal_Name := "accel-activate";
   Signal_Accel_Changed : constant Glib.Signal_Name := "accel-changed";

private
   Is_Locked_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("is-locked");
   Modifier_Mask_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("modifier-mask");
end Gtk.Accel_Group;
