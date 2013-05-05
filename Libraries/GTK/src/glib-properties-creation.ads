-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                   Copyright (C) 2001 ACT-Europe                   --
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
--  This package provides all the required subprograms to create and
--  manipulate new properties associated with new widget types.
--
--  You do not have to be familiar with this package in order to use
--  properties. See Glib.Object instead, that provides the minimal
--  subprograms to get and set properties.
--
--  This package is only intended for writers of new widgets. You will need
--  this function to create new properties.
--
--  Each object in gtk+ has a set of so-called properties. These are
--  attributes that can be accessed, and possibly modified, by names.
--  They provide introspection, that is an object can specify which
--  properties it knows about, which can be modified,..., and thus provide a
--  huge support for special applications like GUI-Builders that need to act
--  on any kind of widgets, even those it doesn't know about yet.
--
--  However, for efficiency reasons, the properties are only names, and are
--  not the only way to modify attributes of objects. It is often more
--  efficient to use the alternate method, as documented in the GtkAda
--  documentation for each property.
--
--  Another interesting feature of properties is that every time a property
--  is modified, a signal "property_changed" or "notify" is emitted, and
--  it is thus easy to keep track of attributes in objects.
--
--  </description>
--  <group>Glib, the general-purpose library</group>

with Glib.Object;
with Glib.Values;
with Interfaces.C.Strings;

package Glib.Properties.Creation is

   --  subtype Param_Spec is Glib.Param_Spec;

   --  This type is the internal representation for properties. It contains all
   --  the required information about it, including some help string or the
   --  range of values it recognizes.
   --
   --  All the values contained in a param_spec are typed. However, since these
   --  are not implemented as tagged types, it is your responsability to
   --  convert the param_specs to the appropriate type, based on the value
   --  returned by Value_Type. Most of the time, you won't need to use these
   --  functions at all, unless you are programming some self-inspection
   --  application, like a GUI-Builder for instance, or creating new properties
   --  for a new widget.

   procedure Unref (Param : Param_Spec);
   --  Decrement the reference counter. If it reaches 0, the memory is freed.

   ------------------
   -- Enum classes --
   ------------------
   --  gtk+, a C library, has a whole system to describe its enumeration types,
   --  similar to what is available from the start in Ada ('Image and 'Value
   --  for instance). All enumerations are represented internally as
   --  Enum_Classes. However, there is no easy conversion between such an
   --  enum class and a GtkAda enumeration type.
   --  Most of the time, this has no impact on your work, since you know
   --  what type you need to use when calling an Ada function. However, you
   --  will need to manipulate these enumeration classes when interfacing
   --  with ParamSpecs and dealing with properties.

   type Enum_Class is new Glib.C_Proxy;
   type Enum_Value is new Glib.C_Proxy;

   function Get_Value (Klass : Enum_Class; Value : Glib.Gint)
      return Enum_Value;
   --  Return the value in Klass that is Value (equivalent of 'Val in Ada)

   function Nth_Value (Klass : Enum_Class; Nth : Glib.Guint) return Enum_Value;
   --  Return the Nth-th value in Klass, or null if there is no such value.

   function Value (Val : Enum_Value) return Glib.Gint;
   --  Return the numeric value for a specific enumeration. Use the matching
   --  Ada type and 'Val to convert it to a valid Ada enumeration

   function Name (Val : Enum_Value) return String;
   --  Return the name of Val. This is the equivalent of 'Image in Ada.

   function Nick (Val : Enum_Value) return String;
   --  Return a displayable string for Val.

   function Register_Static_Enum
     (Name   : String;
      Values : Interfaces.C.Strings.chars_ptr_array) return Glib.GType;
   --  Create a new enumeration class from a list of valid values.
   --  Values must be freed by the caller.

   function Enum_Class_From_Type (Typ : Glib.GType) return Enum_Class;
   --  Return the enumeration class corresponding to a type

   -------------------
   -- Flags classes --
   -------------------
   --  These are very similar to Enum Classes. However, the actual value
   --  of an instance of this type is a combination of a set of flags, rather
   --  than one single enumeration value.
   --  For instance, a Gdk_Event_Mask is a Flags_Class

   type Flags_Class is new Glib.C_Proxy;
   type Flags_Value is new Glib.C_Proxy;
   type Flags_Int_Value is mod Glib.Gint'Last;

   function Nth_Value (Klass : Flags_Class; Nth : Glib.Guint)
      return Flags_Value;
   --  Return the Nth-th value in Klass, or null if there is no such value.

   function Value (Val : Flags_Value) return Flags_Int_Value;
   --  Return the numeric value for a specific enumeration. Use the matching
   --  Ada type and 'Val to convert it to a valid Ada enumeration

   function Name (Val : Flags_Value) return String;
   --  Return the name of Val. This is the equivalent of 'Image in Ada.

   function Nick (Val : Flags_Value) return String;
   --  Return a displayable string for Val.

   ---------------
   -- ParamSpec --
   ---------------

   function Pspec_Name (Param : Param_Spec) return String;
   --  Return the name of the property.
   --  This is the internal string representing the property. It
   --  Should probably not be displayed on

   function Nick_Name (Param : Param_Spec) return String;
   --  Return the nickname of the property. This is a string
   --  that can be displayed to represent the property, and is
   --  more user-friendly than the result of Name.

   function Flags (Param : Param_Spec) return Param_Flags;
   --  Return the flags for the property

   function Owner_Type (Param : Param_Spec) return Glib.GType;
   --  The type that defined Param. If you look for instance at all properties
   --  provides by a type, they will also include properties provided by the
   --  parents of the type. This function can be used to find those declared
   --  with that type only

   function Description (Param : Param_Spec) return String;
   --  Return the description (ie the help string) for Param

   function Value_Type (Param : Param_Spec) return Glib.GType;
   --  Return the type of param

   procedure Set_Value_Type (Param : Param_Spec; Typ : Glib.GType);
   --  Override the type of param. You should only use this function when
   --  creating new Param_Spec types based on existing types. You should not
   --  change the type if you haven't created param yourself.

   function Get_Qdata (Param : Param_Spec; Quark : GQuark) return Glib.C_Proxy;
   --  Return the user data set for Param

   procedure Set_Qdata
     (Param   : Param_Spec;
      Quark   : GQuark;
      Data    : Glib.C_Proxy;
      Destroy : G_Destroy_Notify := null);
   --  Associate some named data with Param. Destroy is called when Param is
   --  destroyed.

   --  Value_Type returns GType_Char
   type Param_Spec_Char is new Param_Spec;
   function Minimum (Param : Param_Spec_Char) return Glib.Gint8;
   function Maximum (Param : Param_Spec_Char) return Glib.Gint8;
   function Default (Param : Param_Spec_Char) return Glib.Gint8;
   function Gnew_Char
     (Name, Nick, Blurb         : String;
      Minimum, Maximum, Default : Glib.Gint8;
      Flags : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec;

   --  Value_Type returns GType_UChar
   type Param_Spec_Uchar is new Param_Spec;
   function Minimum (Param : Param_Spec_Uchar) return Glib.Guint8;
   function Maximum (Param : Param_Spec_Uchar) return Glib.Guint8;
   function Default (Param : Param_Spec_Uchar) return Glib.Guint8;
   function Gnew_Uchar
     (Name, Nick, Blurb         : String;
      Minimum, Maximum, Default : Glib.Guint8;
      Flags : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec;

   --  Value_Type returns GType_Bool
   type Param_Spec_Boolean is new Param_Spec;
   function Default (Param : Param_Spec_Boolean) return Boolean;
   function Gnew_Boolean
     (Name, Nick, Blurb : String;
      Default           : Boolean;
      Flags : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec;

   --  Value_Type returns GType_Int
   type Param_Spec_Int is new Param_Spec;
   function Minimum (Param : Param_Spec_Int) return Glib.Gint;
   function Maximum (Param : Param_Spec_Int) return Glib.Gint;
   function Default (Param : Param_Spec_Int) return Glib.Gint;
   function Gnew_Int
     (Name, Nick, Blurb         : String;
      Minimum, Maximum, Default : Glib.Gint;
      Flags : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec;

   --  Value_Type returns GType_Uint
   type Param_Spec_Uint is new Param_Spec;
   function Minimum (Param : Param_Spec_Uint) return Glib.Guint;
   function Maximum (Param : Param_Spec_Uint) return Glib.Guint;
   function Default (Param : Param_Spec_Uint) return Glib.Guint;
   function Gnew_Uint
     (Name, Nick, Blurb         : String;
      Minimum, Maximum, Default : Glib.Guint;
      Flags : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec;

   --  Value_Type returns GType_Long
   type Param_Spec_Long is new Param_Spec;
   function Minimum (Param : Param_Spec_Long) return Glib.Glong;
   function Maximum (Param : Param_Spec_Long) return Glib.Glong;
   function Default (Param : Param_Spec_Long) return Glib.Glong;
   function Gnew_Long
     (Name, Nick, Blurb         : String;
      Minimum, Maximum, Default : Glib.Glong;
      Flags : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec;

   --  Value_Type returns GType_ULong
   type Param_Spec_Ulong is new Param_Spec;
   function Minimum (Param : Param_Spec_Ulong) return Glib.Gulong;
   function Maximum (Param : Param_Spec_Ulong) return Glib.Gulong;
   function Default (Param : Param_Spec_Ulong) return Glib.Gulong;
   function Gnew_Ulong
     (Name, Nick, Blurb         : String;
      Minimum, Maximum, Default : Glib.Gulong;
      Flags : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec;

   --  Value_Type returns ???
   type Param_Spec_Unichar is new Param_Spec;
   function Default (Param : Param_Spec_Unichar) return Gunichar;
   function Gnew_Unichar
     (Name, Nick, Blurb : String;
      Default           : Gunichar;
      Flags : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec;

   --  Value_Type returns GType_Enum
   --  See also the package Generic_Enumeration_Property on how to create
   --  properties based on an Ada enumeration type.
   type Param_Spec_Enum is new Param_Spec;
   function Enumeration (Param : Param_Spec_Enum) return Enum_Class;
   function Default (Param : Param_Spec_Enum) return Glib.Gint;
   function Gnew_Enum
     (Name, Nick, Blurb : String;
      Enum_Type         : GType;
      Default           : Gint := 0;
      Flags : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec;
   --  See Glib.Properties.Creation.Register_Static_Enum on how to create
   --  Enum_Type

   --  Value_Type returns GType_Flags
   type Param_Spec_Flags is new Param_Spec;
   function Flags_Enumeration (Param : Param_Spec_Flags) return Flags_Class;
   function Default (Param : Param_Spec_Flags) return Glong;
   function Gnew_Flags
     (Name, Nick, Blurb : String;
      Flags_Type        : Glib.GType;
      Default           : Guint;
      Flags : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec;

   --  Value_Type returns GType_Float
   type Param_Spec_Float is new Param_Spec;
   function Minimum (Param : Param_Spec_Float) return Gfloat;
   function Maximum (Param : Param_Spec_Float) return Gfloat;
   function Default (Param : Param_Spec_Float) return Gfloat;
   function Epsilon (Param : Param_Spec_Float) return Gfloat;
   function Gnew_Float
     (Name, Nick, Blurb         : String;
      Minimum, Maximum, Default : Glib.Gfloat;
      Flags : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec;

   --  Value_Type returns GType_Double
   type Param_Spec_Double is new Param_Spec;
   function Minimum (Param : Param_Spec_Double) return Gdouble;
   function Maximum (Param : Param_Spec_Double) return Gdouble;
   function Default (Param : Param_Spec_Double) return Gdouble;
   function Epsilon (Param : Param_Spec_Double) return Gdouble;
   function Gnew_Double
     (Name, Nick, Blurb         : String;
      Minimum, Maximum, Default : Glib.Gdouble;
      Flags : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec;

   --  Value_Type returns GType_String
   type Param_Spec_String is new Param_Spec;
   function Default (Param : Param_Spec_String) return String;
   function Cset_First (Param : Param_Spec_String) return String;
   function Cset_Nth (Param : Param_Spec_String) return String;
   function Substitutor (Param : Param_Spec_String) return Character;
   function Ensure_Non_Null (Param : Param_Spec_String) return Boolean;
   function Gnew_String
     (Name, Nick, Blurb : String;
      Default           : String;
      Flags : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec;

   --  Value_Type returns GType_Param
   type Param_Spec_Param is new Param_Spec;
   function Gnew_Param
     (Name, Nick, Blurb : String;
      Param_Type        : Glib.GType;
      Flags : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec;

   --  Value_Type returns GType_Boxed
   type Param_Spec_Boxed is new Param_Spec;
   function Gnew_Boxed
     (Name, Nick, Blurb : String;
      Boxed_Type        : Glib.GType;
      Flags : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec;

   --  Value_Type returns GType_Pointer
   type Param_Spec_Pointer is new Param_Spec;
   function Gnew_Pointer
     (Name, Nick, Blurb : String;
      Flags : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec;

   --  Value_Type returns GType_Param_Value_Array
   --  type Param_Spec_Value_Array is new Param_Spec;

   --  Value_Type returns GType_Object
   type Param_Spec_Object is new Param_Spec;
   function Gnew_Object
     (Name, Nick, Blurb : String;
      Object_Type       : Glib.GType;
      Flags : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec;

   -----------------------------
   -- Creating new properties --
   -----------------------------
   --  There are several things that need to be done when creating a property.
   --  For one thing, you need to create the string that represents the
   --  property. This is the only item that needs to go in the specifications
   --  of your page.
   --  You then need to describe the type of the property, and the values it
   --  allows. This is very simple for simple types, and a generic packages is
   --  provided to handle the more complex enumeration-based properties.
   --
   --  Your widget needs to define two handlers, Set_Property_Handler and
   --  Get_Property_Handler, that are called every time the user accesses the
   --  value of a property through a call to Glib.Object.Set_Property or
   --  Glib.Object.Get_Property.
   --
   --  For efficiency reasons, a property is also associated with an integer
   --  value, that you must provide when creating the property. This value is
   --  completely free, and is passed to the two handlers described above.
   --
   --  The two handlers manipulate Glib.Values.GValue values, so that they
   --  can get and return various types.

   type Property_Id is new Guint;

   type Set_Property_Handler is access procedure
     (Object        : access Glib.Object.GObject_Record'Class;
      Prop_Id       : Property_Id;
      Value         : Glib.Values.GValue;
      Property_Spec : Param_Spec);
   --  This handler is called every time the user has asked for a new
   --  property to be changed.
   --  Prop_Id is the id you used when creating the property.
   --  Value is the new value that should be set. It is your responsability
   --  to extract the actual value from it. However, consistency has already
   --  been checked by gtk+, so we know the type is correct.
   --  Property_Spec is the definition of the property.
   --
   --  Note: It is your responsability to emit the "notify" signal after a
   --  property has been modified. The recommended way is to emit this signal
   --  from the internal function that actually modified the property, not
   --  directly from the Set_Propert_Handler itself. This will ensure that
   --  even if the user doesn't modify the attribute through a property but
   --  directly by calling the lower-level subprogram, the signal will still
   --  be emitted.

   type Get_Property_Handler is access procedure
     (Object        : access Glib.Object.GObject_Record'Class;
      Prop_Id       : Property_Id;
      Value         : out Glib.Values.GValue;
      Property_Spec : Param_Spec);
   --  This handler is called when the application needs to retrive the value
   --  of a property. You should set the value in Value

   procedure Set_Properties_Handlers
     (Class_Record : Glib.Object.GObject_Class;
      Set_Property : Set_Property_Handler;
      Get_Property : Get_Property_Handler);
   --  Set the two functions used to set and retrieve properties. You
   --  should never call this function on the class record of the standard
   --  gtk+ widgets, since this will break their behavior. You should first
   --  create a new class record through Initialize_Class_Record, and then
   --  use the returned Class_Record as a parameter to this subprogram.
   --
   --  You cannot pass null to either of the two parameters, or you won't
   --  be able to install new properties afterwards

   procedure Install_Property
     (Class_Record  : Glib.Object.GObject_Class;
      Prop_Id       : Property_Id;
      Property_Spec : Param_Spec);
   --  Adds a new property to Class_Record. You should use this function only
   --  on class records you have created yourself, not on one of the standard
   --  widgets.
   --  Prop_Id is the internal representation for properties, that will be
   --  passed to the Set_Property and Get_Property_Handlers (see above) to set
   --  and retrieve the value of a property.
   --  Property_Spec should be the result of one of the GNew_* subprograms for
   --  Param_Spec, and this defines the type of the property.

private
   pragma Import (C, Flags, "ada_gparam_get_flags");
   pragma Import (C, Owner_Type, "ada_gparam_get_owner_type");
   pragma Import (C, Value_Type, "ada_gparam_get_value_type");
   pragma Import (C, Set_Value_Type, "ada_gparam_set_value_type");
   pragma Import (C, Get_Value, "g_enum_get_value");
   pragma Import (C, Flags_Enumeration, "ada_gparam_get_flags_flags");
   pragma Import (C, Enumeration, "ada_gparam_get_enum_class_enum");
   pragma Import (C, Install_Property, "g_object_class_install_property");
   pragma Import (C, Unref, "g_param_spec_unref");
   pragma Import (C, Get_Qdata, "g_param_spec_get_qdata");
   pragma Import (C, Set_Qdata, "g_param_spec_set_qdata_full");
   pragma Import (C, Enum_Class_From_Type, "g_type_class_ref");
   pragma Inline (Description);
   pragma Inline (Name);

   pragma Inline (Minimum);
   pragma Inline (Maximum);
   pragma Inline (Default);
   pragma Inline (Epsilon);
   pragma Inline (Substitutor);
   pragma Inline (Cset_Nth);
   pragma Inline (Cset_First);
   pragma Inline (Ensure_Non_Null);
end Glib.Properties.Creation;
