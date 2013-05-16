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
--
--  This package is deprecated and is here for purposes of backwards
--  compatibility.
--
--  The handling of flags has been moved to Gtk.Widget, and the rest
--  of the facilities are available through Glib.Object.
--
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Abstract base classes</group>

with Glib.Object;
with Glib.Properties;
with Glib.GSlist;
with Gtkada.Types;

package Gtk.Object is

   type Gtk_Object_Record is new Glib.Object.GObject_Record with private;
   type Gtk_Object is access all Gtk_Object_Record'Class;

   procedure Destroy (Object : access Gtk_Object_Record);
   --  Destroy the object.
   --  This emits a "destroy" signal, calls all your handlers, and then
   --  unconnects them all. The object is then unref-ed, and if its reference
   --  count goes down to 0, the memory associated with the object and its
   --  user data is freed.
   --  Note that when you destroy handlers are called, the user_data is still
   --  available.
   --
   --  When a widget is destroyed, it will break any references it holds to
   --  other objects. If the widget is inside a container, the widget will be
   --  removed from the container. If the widget is a toplevel (derived from
   --  Gtk_Window), it will be removed from the list of toplevels, and the
   --  reference GTK+ holds to it will be removed. Removing widget from its
   --  container or the list of toplevels results in the widget being
   --  finalized, unless you've added additional references to the widget with
   --  Ref.
   --
   --  In most cases, only toplevel widgets (windows) require explicit
   --  destruction, because when you destroy a toplevel its children will be
   --  destroyed as well.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Object internally.
   pragma Import (C, Get_Type, "gtk_object_get_type");

   function Get_Type (Object : access Gtk_Object_Record) return Gtk_Type;
   --  This function is now obsolete, and is temporarily kept for backward
   --  compatibility only. Use Glib.Object.Get_Type instead.
   --  ???

   -----------
   -- Lists --
   -----------

   function Convert (W : Gtk_Object) return System.Address;
   function Convert (W : System.Address) return Gtk_Object;

   package Object_SList is new Glib.GSlist.Generic_SList (Gtk_Object);

   -----------
   -- Flags --
   -----------
   --  Each object is associated with a set of flags, that reports the state
   --  of the object.
   --  The following flags are known by all objects:
   --
   --  - "Destroyed":
   --     Set if the object is marked as destroyed (if its reference count is
   --     not yet 0, the memory has not been freed, but you should not use it
   --     anyway).
   --
   --  - "Floating":
   --     The object has no parent yet, since it was just created. Its
   --     reference count is still 1 (as it was initially). This flag is
   --     cleared as soon as Set_Parent is called on the widget or the widget
   --     is qualified as a toplevel widget (see
   --     Gtk.Container.Register_Toplevel).

   In_Destruction : constant := 2 ** 0;
   Floating       : constant := 2 ** 1;
   Reserved_1     : constant := 2 ** 2;
   Reserved_2     : constant := 2 ** 3;

   function Flags (Object : access Gtk_Object_Record) return Guint32;
   --  Return the flags that are set for the object, as a binary mask.

   procedure Set_Flags (Object : access Gtk_Object_Record; Flags : Guint32);
   --  Set some specific flags for the object.
   --  Flags is a mask that will be added to the current flags of the object.

   procedure Unset_Flags (Object : access Gtk_Object_Record; Flags : Guint32);
   --  Unset some specific flags for the object.
   --  Flags is a mask that will be deleted from the current flags of the
   --  object.

   function Flag_Is_Set
     (Object : access Gtk_Object_Record; Flag : Guint32) return Boolean;
   --  Return True if the specific flag Flag is set for the object.

   function In_Destruction_Is_Set
     (Object : access Gtk_Object_Record'Class) return Boolean;
   --  Test if the Destroyed flag is set for the object.

   --  <doc_ignore>
   function Destroyed_Is_Set (Object : access Gtk_Object_Record'Class)
      return Boolean renames In_Destruction_Is_Set;
   --  backward compatibility only
   --  </doc_ignore>

   function Floating_Is_Set
     (Object : access Gtk_Object_Record'Class) return Boolean;
   --  Test if the Floating flag is set for the object.

   --------------------------
   -- Creating new widgets --
   --------------------------

   --  <doc_ignore>
   --  The following definitions are only provided for better backward
   --  compatibility. You should use Glib.Object directly.

   subtype GObject_Class is Glib.Object.GObject_Class;
   Uninitialized_Class : GObject_Class renames
     Glib.Object.Uninitialized_Class;

   subtype Signal_Parameter_Types is Glib.Object.Signal_Parameter_Types;

   Null_Parameter_Types : Signal_Parameter_Types renames
     Glib.Object.Null_Parameter_Types;

   procedure Initialize_Class_Record
     (Object       : access GObject_Record'Class;
      Signals      : Gtkada.Types.Chars_Ptr_Array;
      Class_Record : in out GObject_Class;
      Type_Name    : String;
      Parameters   : Signal_Parameter_Types := Null_Parameter_Types)
      renames Glib.Object.Initialize_Class_Record;

   --  </doc_ignore>

   ---------------
   -- User Data --
   ---------------
   --  It is possible to associate your own specific data with an existing
   --  object. See the documentation in Glib.Object.
   --  The declaration below has been kept for compatibility reasons.

   generic
   package User_Data renames Glib.Object.User_Data;

   -----------------
   -- Obsolescent --
   -----------------
   --  All subprograms below are now obsolescent in gtk+. They might be removed
   --  from future versions of gtk+ (and therefore GtkAda).
   --  To find out whether your code uses any of these, we recommend compiling
   --  with the -gnatwj switch
   --  <doc_ignore>

   procedure Sink (Object : access Gtk_Object_Record);
   pragma Obsolescent (Sink);
   --  Sink the object.
   --  If the object is floating (does not have a parent yet), it is unref-ed
   --  once and the floating flag is cleared.

   --  </doc_ignore>

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  - Name:  User_Data_Property
   --    Type:  Pointer
   --    Flags: read-write
   --    Descr: Anonymous User Data Pointer
   --    See also: User_Data.Set, using the default Id "user_data"
   --
   --  </properties>

   User_Data_Property : constant Glib.Properties.Property_Address;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "destroy"
   --    procedure Handler (Object : access Gtk_Object_Record'Class);
   --
   --    Raised when the object is about to be destroyed. The "destroyed"
   --    flag has been set on the object first. Handlers should not keep
   --    a reference on the object.
   --    Note that when your destroy handlers are called, the user_data is
   --    still available.
   --    The default implementation destroys all the handlers.
   --  </signals>

   Signal_Destroy : constant Glib.Signal_Name := "destroy";

private
   type Gtk_Object_Record is new Glib.Object.GObject_Record with null record;

   User_Data_Property : constant Glib.Properties.Property_Address :=
     Glib.Properties.Build ("user_data");

   pragma Inline (Floating_Is_Set);
   pragma Inline (In_Destruction_Is_Set);

end Gtk.Object;

--  The following subprograms never had a binding, but are now obsolescent
--  No binding: gtk_object_add_arg_type
--  No binding: gtk_object_get
--  No binding: gtk_object_get_data
--  No binding: gtk_object_get_data_by_id
--  No binding: gtk_object_get_user_data
--  No binding: gtk_object_new
--  No binding: gtk_object_ref
--  No binding: gtk_object_unref
--  No binding: gtk_object_remove_data
--  No binding: gtk_object_remove_data_by_id
--  No binding: gtk_object_remove_no_notify
--  No binding: gtk_object_remove_no_notify_by_id
--  No binding: gtk_object_set
--  No binding: gtk_object_set_data
--  No binding: gtk_object_set_data_by_id
--  No binding: gtk_object_set_data_by_id_full
--  No binding: gtk_object_set_data_full
--  No binding: gtk_object_set_user_data
--  No binding: gtk_object_weakref
--  No binding: gtk_object_weakunref
