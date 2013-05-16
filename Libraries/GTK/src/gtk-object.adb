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

with System;

package body Gtk.Object is

   -------------
   -- Convert --
   -------------

   function Convert (W : Gtk_Object) return System.Address is
   begin
      if W = null then
         return System.Null_Address;
      else
         return Get_Object (W);
      end if;
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert (W : System.Address) return Gtk_Object is
      Stub : Gtk_Object_Record;
   begin
      return Gtk_Object (Get_User_Data (W, Stub));
   end Convert;

   ---------------------------
   -- In_Destruction_Is_Set --
   ---------------------------

   function In_Destruction_Is_Set
     (Object : access Gtk_Object_Record'Class) return Boolean
   is
      use type System.Address;
   begin
      return Get_Object (Object) = System.Null_Address
        or else Flag_Is_Set (Object, In_Destruction);
   end In_Destruction_Is_Set;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Object : access Gtk_Object_Record) is
      procedure Internal (Object : System.Address);
      pragma Import (C, Internal, "gtk_object_destroy");

      procedure Unref_Internal (Object : System.Address);
      pragma Import (C, Unref_Internal, "g_object_unref");
      --  External binding: g_object_unref

      Ptr : constant System.Address := Get_Object (Object);

      use type System.Address;
   begin
      --  Keep a reference on the object, so that the Ada structure is
      --  never automatically deleted when the C object is.
      --  We can't reset the content of Object to System.Null_Address before
      --  calling the C function, because we want the user's destroy
      --  callbacks to be called with the appropriate object.
      Ref (Object);
      Internal (Ptr);

      --  We then can make sure that the object won't be referenced any
      --  more, (The Ada structure won't be free before the ref count goes
      --  down to 0, and we don't want the user to use a deleted object...).
      Set_Object (Object, System.Null_Address);

      --  Free the reference we had. In most cases, this results in the
      --  object being freed. We can't use directly Unref, since the Ptr
      --  field for Object is Null_Address.
      Unref_Internal (Ptr);
   end Destroy;

   -----------
   -- Flags --
   -----------

   function Flags (Object : access Gtk_Object_Record) return Guint32 is
      function Internal (Object : System.Address) return Guint32;
      pragma Import (C, Internal, "ada_object_flags");

   begin
      return Internal (Get_Object (Object));
   end Flags;

   --------------
   -- Floating --
   --------------

   function Floating_Is_Set
     (Object : access Gtk_Object_Record'Class) return Boolean is
   begin
      return Flag_Is_Set (Object, Floating);
   end Floating_Is_Set;

   --------------
   -- Get_Type --
   --------------

   function Get_Type (Object : access Gtk_Object_Record) return Gtk_Type is
      function Internal (Object : System.Address) return Gtk_Type;
      pragma Import (C, Internal, "ada_gobject_get_type");

   begin
      return Internal (Get_Object (Object));
   end Get_Type;

   ---------------
   -- Set_Flags --
   ---------------

   procedure Set_Flags (Object : access Gtk_Object_Record; Flags : Guint32) is
      procedure Internal (Object : System.Address; Flags : Guint32);
      pragma Import (C, Internal, "ada_object_set_flags");

   begin
      Internal (Get_Object (Object), Flags);
   end Set_Flags;

   ----------
   -- Sink --
   ----------

   procedure Sink (Object : access Gtk_Object_Record) is
      procedure Internal (Object : System.Address);
      pragma Import (C, Internal, "gtk_object_sink");

   begin
      Internal (Get_Object (Object));
   end Sink;

   -----------------
   -- Unset_Flags --
   -----------------

   procedure Unset_Flags
     (Object : access Gtk_Object_Record; Flags : Guint32)
   is
      procedure Internal (Object : System.Address; Flags : Guint32);
      pragma Import (C, Internal, "ada_object_unset_flags");

   begin
      Internal (Get_Object (Object), Flags);
   end Unset_Flags;

   -----------------
   -- Flag_Is_Set --
   -----------------

   function Flag_Is_Set
     (Object : access Gtk_Object_Record; Flag : Guint32) return Boolean
   is
      function Internal (Object : System.Address; Flag : Guint32) return Gint;
      pragma Import (C, Internal, "ada_object_flag_is_set");

   begin
      return Boolean'Val (Internal (Get_Object (Object), Flag));
   end Flag_Is_Set;

end Gtk.Object;
