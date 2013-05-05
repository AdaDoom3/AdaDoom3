-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                 Copyright (C) 2001-2013, AdaCore                  --
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
--  Note: this package need only be used and understood by people who
--  want to create their own new widgets and their associated properties.
--  Normal usage of properties doesn't require any deep understanding of
--  this package.
--
--  This package provides two generic subpackages that make it easy to
--  declare properties.
--  Each of these packages define two types:
--
--  - Property_RO : this type should be used for a read-only property
--                  of the given type.
--  - Property    : This is for read-write properties
--
--  Each of these two types is associated with one or two primitive
--  operations Get_Property and Set_Property, that allows the modification
--  of properties of this type.
--
--  As a user and creator of new widgets, you should always use the
--  Generic_Enumeration_Property package, since it also registers the
--  enumeration type with gtk+ for a full compatibility with C.
--  </description>
--  <group>Glib, the general-purpose library</group>

with Glib.Object;
with Glib.Values;

package Glib.Generic_Properties is

   --------------------------------------------------
   -- Generic package for discrete type properties --
   --------------------------------------------------
   --  This package should be used to implement the
   --  Get_Property and Set_Property subprograms for all
   --  properties related to enumeration types and simple
   --  types.
   --  This should be used only for types defined in GtkAda
   --  or gtk+ themselves, not for types that you define
   --  yourself. Use Generic_Discrete_Type instead.

   generic
      type Discrete_Type is (<>);
   package Generic_Internal_Discrete_Property is
      type Property_RO is new Glib.Property;
      type Property is new Glib.Property;

      procedure Set_Property
        (Object : access Glib.Object.GObject_Record'Class;
         Name   : Property;
         Value  : Discrete_Type);
      --  Set a property of Object based on Enumeration_Type.

      function Get_Property
        (Object : access Glib.Object.GObject_Record'Class;
         Name   : Property) return Discrete_Type;
      pragma Inline (Get_Property);

      function Get_Property
        (Object : access Glib.Object.GObject_Record'Class;
         Name   : Property_RO) return Discrete_Type;
      --  Get a property from Object

   end Generic_Internal_Discrete_Property;

   -------------------------------------------------
   -- Generic package for enumerations properties --
   -------------------------------------------------
   --  This package should be used to implement the
   --  Get_Property and Set_Property subprograms for all
   --  properties related to enumeration types and simple
   --  types in users' applications.
   --  Name is the name registered in gtk+ for the type. This is also
   --  the name that appears in all the tools that use introspection
   --  to get information about the widgets and their properties, like
   --  GUI builders for instance
   --
   --  !!IMPORTANT!!: For proper usage of properties based on enumeration
   --  types, you must specify the Convention C on the type:
   --      pragma Convention (C, Enumeration_Type);

   generic
      Name : String;
      type Enumeration is (<>);
   package Generic_Enumeration_Property is

      -----------------
      --  Properties --
      -----------------

      package Properties is new Generic_Internal_Discrete_Property
        (Enumeration);
      type Property_RO is new Properties.Property_RO;
      type Property    is new Properties.Property;

      -----------
      -- Types --
      -----------

      function Get_Type return Glib.GType;
      --  Return the internal gtk+ type associated with the Ada enumeration
      --  Enumeration. You don't need to use such a function for the types
      --  defined in standard in GtkAda. Use Glib.Type_From_Name
      --  instead.

      function Gnew_Enum
        (Name, Nick, Blurb   : String;
         Default             : Enumeration := Enumeration'First;
         Flags : Param_Flags := Param_Readable or Param_Writable)
         return Param_Spec;
      --  Create a new param_spec (to describe properties), based on the
      --  Ada enumeration type Enumeration. This function is used when
      --  creating the property with Install_Property on an object.
      --  Name, Nick and Blurb should describe the property, not its type.

      ------------
      -- Values --
      ------------

      function Get_Enum (Value : Glib.Values.GValue) return Enumeration;
      --  Return the enumeration contained in Value, assuming it is of type
      --  Enumeration

      procedure Set_Enum
        (Value : in out Glib.Values.GValue; Enum : Enumeration);
      --  Set the enumeration value for Value. This properly initializes the
      --  type of Value, so you don't need to call Init yourself.

   private
      The_Type : Glib.GType := Glib.GType_Invalid;
      pragma Import (C, Get_Enum, "g_value_get_enum");
   end Generic_Enumeration_Property;

   -------------------------------------------------
   -- Generic package for record types properties --
   -------------------------------------------------
   --  This package should be used to implement the
   --  Get_Property and Set_Property subprograms for all
   --  properties related to record type, like Gdk_Color and
   --  Gdk_Rectangle.
   --  This should be used only for types defined in GtkAda
   --  or gtk+ themselves, not for types that you define
   --  yourself.

   generic
      type Boxed_Type is private;
      with function Get_Type return Glib.GType;

      with function To_Address
        (B : Boxed_Type; Default : System.Address) return System.Address;
      --  Convert B into an address that can be passed to gtk+.
      --  Default is the address of the parameters passed by the user (since
      --  this function cannot return B'Address, where B might be passed by
      --  copy).

   package Generic_Internal_Boxed_Property is
      type Property_RO is new Glib.Property;
      type Property    is new Glib.Property;

      procedure Set_Property
        (Object : access Glib.Object.GObject_Record'Class;
         Name   : Property;
         Value  : Boxed_Type);
      --  Set a property of Object based on Enumeration_Type.

      function Get_Property
        (Object : access Glib.Object.GObject_Record'Class;
         Name   : Property) return Boxed_Type;
      pragma Inline (Get_Property);

      function Get_Property
        (Object : access Glib.Object.GObject_Record'Class;
         Name   : Property_RO) return Boxed_Type;
      --  Get a property from Object.
      --  Unset_Value is raised if the property is not set

      procedure Set_Value
        (Value  : out Glib.Values.GValue;
         Val    : Boxed_Type);
      --  Store Val in Value. The latter is properly initialized, and reference
      --  counting is handled automatically. You must Unset Value when you are
      --  done using it.

      function Get_Value (Value : Glib.Values.GValue) return Boxed_Type;
      --  Get the value stored in Value. Reference counting is automatically
      --  handled, and the returned value has been properly referenced.
      --  Unset_Value is raised if Value contains no data

   end Generic_Internal_Boxed_Property;

   Unset_Value : exception;

end Glib.Generic_Properties;
