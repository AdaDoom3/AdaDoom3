-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                Copyright (C) 2001-2003, ACT-Europe                --
--                    Copyright (C) 2010-2013, AdaCore               --
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
--  Properties are a fully general way to modify the appareance or behavior
--  of widgets. Most of the time, there exists a faster way to modify the
--  widget in the same fashion (for instance a direct call to a primitive
--  subprogram). However, the properties provide a general scheme to modify
--  these attributes.
--  For instance, they can be used to provide introspection on the widget
--  (to automatically retrieve the attributes that can be modified), or if
--  you need to implement a tool like a GUI-Builder that is able to
--  manipulate any widget, even those that didn't exist when the tool was
--  written.
--
--  Two functions are provided for each type of property: Set_Property and
--  Get_Property, which allow easy modification of specific widget
--  properties. For instance, you could do the following:
--      declare
--          Button : Gtk_Button;
--      begin
--          Gtk_New (Button, "old label");
--          Set_Property (Button, Label_Property, "new label");
--      end;
--  to modify the label of a button.
--
--  Likewise, you can retrieve the current label with:
--      Current : String := Get_Property (Button, Label_Property);
--
--  Dispatching is used ensure type-safety while using properties. The
--  appropriate Set_Property/Get_Property functions are called depending
--  on the type of the property you are trying to use. This is checked
--  statically by the compiler, which provides additional type-safety
--  compared to the C library.
--
--  Note that some properties are read-only, and thus do not have the
--  Set_Property subprogram defined.
--
--  When a property is modified, the signal "notify::<property>" is emitted,
--  for instance, "notify::label" for a gtk_button. This is a standard gtk+
--  signal to which you can connect with the subprograms in gtk-handlers.ads

--  </description>
--  <c_version>1.3.4</c_version>
--  <group>Glib, the general-purpose library</group>

with Glib.Object;
with Glib.Generic_Properties; use Glib.Generic_Properties;
pragma Elaborate_All (Glib.Generic_Properties);
with System;
with Glib.Values;

package Glib.Properties is

   --  <doc_ignore>

   --  Definition of the types and subprograms.
   --  You can ignore this section.

   package Char_Properties is new
     Generic_Internal_Discrete_Property (Glib.Gchar);
   package Uchar_Properties is new
     Generic_Internal_Discrete_Property (Glib.Guchar);
   package Int_Properties is new
     Generic_Internal_Discrete_Property (Glib.Gint);
   package Uint_Properties is new
     Generic_Internal_Discrete_Property (Glib.Guint);
   package Long_Properties is new
     Generic_Internal_Discrete_Property (Glib.Glong);
   package Ulong_Properties is new
     Generic_Internal_Discrete_Property (Glib.Gulong);
   package Unichar_Properties is new
     Generic_Internal_Discrete_Property (Glib.Gunichar);

   --  </doc_ignore>

   --  Predefined types of properties. Additional types are available
   --  for most of the standard enumeration types, and you can create
   --  your own types (see Glib.Properties).

   type Property_Char      is new Char_Properties.Property;
   type Property_Char_RO   is new Char_Properties.Property_RO;
   type Property_Uchar     is new Uchar_Properties.Property;
   type Property_Uchar_RO  is new Uchar_Properties.Property_RO;
   type Property_Int       is new Int_Properties.Property;
   type Property_Uint_RO   is new Uint_Properties.Property_RO;
   type Property_Uint      is new Uint_Properties.Property;
   type Property_Long_RO   is new Long_Properties.Property_RO;
   type Property_Long      is new Long_Properties.Property;
   type Property_Ulong_RO  is new Ulong_Properties.Property_RO;
   type Property_Ulong     is new Ulong_Properties.Property;
   type Property_Unichar   is new Unichar_Properties.Property;
   type Property_C_Proxy   is new Glib.Property;
   type Property_String_RO is new Glib.Property;
   type Property_String_WO is new Glib.Property;
   type Property_String    is new Glib.Property;
   type Property_Boolean   is new Glib.Property;
   type Property_Object    is new Glib.Property;
   type Property_Object_WO is new Glib.Property;
   type Property_Address   is new Glib.Property;
   type Property_Float     is new Glib.Property;
   type Property_Double    is new Glib.Property;
   type Property_Enum      is new Glib.Property;
   type Property_Boxed     is new Glib.Property;

   --  General properties getter

   procedure Set_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : String;
      Value  : in out Glib.Values.GValue);
   procedure Get_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : String;
      Value  : in out Glib.Values.GValue);
   --  Get the property. Value must have been initialized first with the
   --  expected type for the property, as in:
   --      Value : GValue;
   --      Init (Value, Value_Type (Pspec));
   --      Get_Property (Object, Pspec_Name (Pspec), Value);
   --  If you do not have a Param_Spec, this can be replaced with:
   --      Init (Value, GType_Int);
   --      Get_Property (Object, Property_Name (Property), Value);
   --  Value must be Unset by the caller to free memory

   --  Special handling of string properties

   procedure Set_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name : Property_String;
      Value : String);

   procedure Set_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name : Property_String_WO;
      Value : String);

   function Get_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name : Property_String) return String;

   function Get_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name : Property_String_RO) return String;

   --  Special handling of boolean properties

   procedure Set_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : Property_Boolean;
      Value  : Boolean);

   function Get_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name : Property_Boolean) return Boolean;

   --  Special handling of object properties

   procedure Set_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : Property_Object;
      Value  : access Glib.Object.GObject_Record'Class);

   function Get_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name : Property_Object) return Glib.Object.GObject;

   procedure Set_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : Property_Object_WO;
      Value  : access Glib.Object.GObject_Record'Class);

   --  Special handling of address properties

   procedure Set_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : Property_Address;
      Value  : System.Address);

   function Get_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name : Property_Address) return System.Address;

   --  Special handling of float properties

   procedure Set_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : Property_Float;
      Value  : Gfloat);

   function Get_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name : Property_Float) return Gfloat;

   --  Special handling of double properties

   procedure Set_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : Property_Double;
      Value  : Gdouble);

   function Get_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name : Property_Double) return Gdouble;

   --  Special handling of c_proxy properties

   procedure Set_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : Property_C_Proxy;
      Value  : C_Proxy);

   function Get_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name : Property_C_Proxy) return C_Proxy;

end Glib.Properties;
