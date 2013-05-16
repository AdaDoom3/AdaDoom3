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
--  This package provides definitions for the basic types used in Glib, Cairo,
--  Gdk and Gtk.
--
--  </description>
--  <group>Glib, the general-purpose library</group>

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with System;
with Interfaces.C;

package Glib is
   pragma Preelaborate;

   package C renames Interfaces.C;
   use type C.int;
   use type C.unsigned;

   -------------------------------------
   -- The basic types defined by glib --
   -------------------------------------

   type Gshort is new C.short;
   type Glong  is new C.long;
   type Gint   is new C.int;
   type Gchar  is new C.char;
   type Gboolean is new Gint;

   type Gushort is new C.unsigned_short;
   type Gulong  is new C.unsigned_long;
   type Guint   is new C.unsigned;
   type Guchar  is new C.unsigned_char;

   type Gfloat  is new C.C_float;
   type Gdouble is new C.double;

   type Gint8  is range -(2 ** 7) .. (2 ** 7 - 1);
   type Gint16 is range -(2 ** 15) .. (2 ** 15 - 1);
   type Gint32 is range -(2 ** 31) .. (2 ** 31 - 1);
   type Gint64 is range -(2 ** 63) .. (2 ** 63 - 1);

   type Guint8  is mod 2 ** 8;
   type Guint16 is mod 2 ** 16;
   type Guint32 is mod 2 ** 32;
   type Guint64 is mod 2 ** 64;

   type Gsize is new C.size_t;

   type Gunichar is new Guint32;

   subtype UTF8_String is String;
   --  A string that accepts only valid UTF8 sequences.
   --  Most Gtk+ function expect valid UTF8 strings instead of regular strings.

   type GTime_Val is record
      TV_Sec  : Glong;
      TV_Usec : Glong;
   end record;
   pragma Convention (C, GTime_Val);

   type GTime_Val_Access is access all GTime_Val;
   pragma Convention (C, GTime_Val_Access);

   subtype Grange_Float is Gdouble;
   --  Needed for better compatibility between GtkAda 1.2 and 2.0

   subtype Gcolor_Int is Guint16;
   --  Provided for better compatibility between GtkAda 1.2 and 2.0

   subtype Allocation_Int is Gint;
   --  Provided for better compatibility between GtkAda 1.2 and 2.0

   ----------------------
   -- Some Array types --
   ----------------------

   type Gboolean_Array is array (Natural range <>) of Gboolean;
   type Gshort_Array   is array (Natural range <>) of Gshort;
   type Glong_Array    is array (Natural range <>) of Glong;
   type Gint_Array     is array (Natural range <>) of Gint;
   type Guint_Array    is array (Natural range <>) of Guint;
   type Guint32_Array  is array (Natural range <>) of Guint32;
   type Gushort_Array  is array (Natural range <>) of Gushort;
   type Gulong_Array   is array (Natural range <>) of Gulong;
   type Gfloat_Array   is array (Natural range <>) of Gfloat;
   type Guchar_Array   is array (Natural range <>) of Guchar;
   type Gdouble_Array  is array (Natural range <>) of Gdouble;

   type Boolean_Array  is array (Natural range <>) of Boolean;

   type Short_Array    is array (Natural range <>) of C.short;
   type Long_Array     is array (Natural range <>) of C.long;

   -------------------------
   -- Conversion services --
   -------------------------

   function To_Boolean_Array (A : Gboolean_Array) return Boolean_Array;
   --  Convert a C-style boolean array into an Ada-style array.

   function To_Gint (Bool : Boolean) return Gint;
   --  Convert an Ada boolean into a C int.

   -----------------------
   -- Some Access types --
   -----------------------

   type Guchar_Array_Access is access Guchar_Array;

   type String_Ptr is access all String;

   --  <doc_ignore>
   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Guchar_Array, Name => Guchar_Array_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => String, Name => String_Ptr);
   --  </doc_ignore>

   --  <doc_ignore>
   type C_Dummy is limited private;
   --  </doc_ignore>

   type C_Proxy is access all C_Dummy;
   --  General proxy for C structures.
   --  This type is used instead of System.Address so that the variables are
   --  automatically initialized to 'null'.
   --  The value pointed to is irrelevant, and in fact should not be accessed.
   --  It has thus been made limited private with no subprogram to access it.
   --  C_Proxy is a public type so that one can compare directly the value
   --  of the variables with 'null'.

   --  <doc_ignore>
   pragma Convention (C, C_Proxy);

   function To_Proxy is new Ada.Unchecked_Conversion (System.Address, C_Proxy);
   function To_Address is new
     Ada.Unchecked_Conversion (C_Proxy, System.Address);
   --  Converts from a System.Address returned by a C function to an
   --  internal C_Proxy.

   --  </doc_ignore>

   type G_Destroy_Notify_Address is
     access procedure (Data : System.Address);
   pragma Convention (C, G_Destroy_Notify_Address);

   type G_Destroy_Notify is access procedure (Data : Glib.C_Proxy);
   pragma Convention (C, G_Destroy_Notify);
   --  Callback used when some named data associated with an object is
   --  destroyed.

   ------------
   -- Quarks --
   ------------

   type GQuark is new Guint32;
   --  Represents a string internally in GtkAda. Once you know the
   --  equivalent for a string, you can always use it instead of the string,
   --  which provides a faster access for all the functions that use htables
   --  in GtkAda.
   --  There is a global htable that contains all the quarks defined in
   --  your application and GtkAda itself.

   Unknown_Quark : constant GQuark := 0;

   function Quark_From_String (Id : String) return GQuark;
   --  Return, or create the quark associated with the string.
   --  Note that if the quark does not already exist, an entry is created for
   --  it in the global htable for quarks.

   function Quark_Try_String (Id : String) return GQuark;
   --  Return the quark associated with the string, if it exists.
   --  If it does not exist, return Unknown_Quark.

   -------------
   -- Signals --
   -------------

   type Signal_Id is private;
   --  This uniquely identifies a connection widget<->signal.

   type Signal_Name is new String;
   --  A signal name as used in connect, shared type between the Gtk
   --  and Glib layer.

   Invalid_Signal_Id : constant Signal_Id;
   Null_Signal_Id : constant Signal_Id;

   type G_Connect_Flags is mod 2 ** C.int'Size;
   G_Connect_After   : constant G_Connect_Flags := 2 ** 0;
   G_Connect_Swapped : constant G_Connect_Flags := 2 ** 1;
   --  Used to specify the behavior of a signal's connection.

   ----------------
   -- Properties --
   ----------------
   --  This is only the definition of the property types. See Glib.Properties
   --  on how to get and set the value of properties for specific objects, or
   --  the package Glib.Properties.Creation for information on how to create
   --  new properties in your own widgets.
   --  Introspection is available, ie from an existing object you can find out
   --  the list of properties it supports. See the functions
   --  Glib.Object.Interface_List_Properties and
   --  Glib.Object.Class_List_Properties

   type Property (<>) is private;

   function Build (Name : String) return Property;
   --  You should use this function only if you are creating new widgets, and
   --  their properties. Normal usage of properties doesn't require the use
   --  of this function.
   --  An ASCII.NUL character is automatically appended if necessary

   function Property_Name (Prop : Property) return String;
   --  Return the name of the property.
   --  This name includes the trailing ASCII.Nul, and thus can be passed as is
   --  to C.

   type Param_Spec is new Glib.C_Proxy;
   type Param_Spec_Array is array (Natural range <>) of Param_Spec;
   --  See Glib.Properties.Creation for more information on this type

   type Param_Flags is mod 2 ** 6;
   Param_Readable       : constant Param_Flags := 2 ** 0;
   Param_Writable       : constant Param_Flags := 2 ** 1;
   Param_Construct      : constant Param_Flags := 2 ** 2;
   Param_Construct_Only : constant Param_Flags := 2 ** 3;
   Param_Lax_Validation : constant Param_Flags := 2 ** 4;
   Param_Private        : constant Param_Flags := 2 ** 5;
   --  These are the various flags that help define if, and when, a property
   --  can be read and modified.

   -----------
   -- GType --
   -----------

   type GType is new Gsize;
   --  This type describes an internal type in Glib.
   --  You shouldn't have to use it in your own applications, however it might
   --  be useful sometimes.
   --  Every object type is associated with a specific value, created
   --  dynamically at run time the first time you instantiate an object of that
   --  type (thus if you have never used e.g a Gtk_File_Selection, it won't
   --  have any GType associated with it).
   --  You can get the exact type value for each type by using the functions
   --  Get_Type provided in all the packages in GtkAda.
   --  You can get the specific value for an existing widget by using the
   --  function Glib.Object.Get_Type.

   type GType_Class is private;
   --  An opaque structure used as the base for all classes in glib and gtk+.
   --  See also Glib.Object.GObject_Class for a more useful child of this
   --  type.

   type GType_Array is array (Guint range <>) of Glib.GType;

   function Parent (Typ : GType) return GType;
   --  Return the parent type of Typ (eg if Typ is associated with a Gtk
   --  widget, it returns the typ of its parent).

   function Fundamental (Typ : GType) return GType;
   --  Return the fundamental type for Type.  In gtk+, the types are organized
   --  into several hierarchies, similar to what is done for widgets.
   --  All of these hierarchies are based on one of the fundamental types
   --  defined below.
   --  This function returns that fundamental type.
   --
   --  For instance, each enumeration type in gtk+ has its own GType.
   --  However, Fundamental will return GType_Enum in all of these cases.

   function Type_Name (Type_Num : GType) return String;
   --  Return the name of the type (enumeration,...) associated with Typ.
   --  If Fundamental (Typ) return GType_Enum, this returns the name of
   --  the enumeration type that Typ represents.
   --  This might be useful in debug messages.

   function Type_From_Name (Name : String) return GType;
   --  Convert a string to the matching type.
   --  Name should be the C GObject name rather than the Ada name: thus,
   --  use names such as GtkScrollbar or GtkButton for widgets.

   function Get_Qdata (Typ : GType; Quark : GQuark) return Glib.C_Proxy;
   --  Return the user data set for Typ

   procedure Set_Qdata
     (Typ     : GType;
      Quark   : GQuark;
      Data    : Glib.C_Proxy);
   --  Associate some named data with Typ.

   --  The list of fundamental types defined in Glib. As opposed to most other
   --  types (for instance the ones used for widgets), the types have static
   --  values, always the same.

   GType_Fundamental_Shift : constant Integer := 2;

   GType_Invalid   : constant GType := 0 * (2 ** GType_Fundamental_Shift);
   GType_None      : constant GType := 1 * (2 ** GType_Fundamental_Shift);
   GType_Interface : constant GType := 2 * (2 ** GType_Fundamental_Shift);

   GType_Char      : constant GType := 3 * (2 ** GType_Fundamental_Shift);
   GType_Uchar     : constant GType := 4 * (2 ** GType_Fundamental_Shift);
   GType_Boolean   : constant GType := 5 * (2 ** GType_Fundamental_Shift);
   GType_Int       : constant GType := 6 * (2 ** GType_Fundamental_Shift);
   GType_Uint      : constant GType := 7 * (2 ** GType_Fundamental_Shift);
   GType_Long      : constant GType := 8 * (2 ** GType_Fundamental_Shift);
   GType_Ulong     : constant GType := 9 * (2 ** GType_Fundamental_Shift);
   GType_Int64     : constant GType := 10 * (2 ** GType_Fundamental_Shift);
   GType_UInt64    : constant GType := 11 * (2 ** GType_Fundamental_Shift);
   GType_Enum      : constant GType := 12 * (2 ** GType_Fundamental_Shift);
   GType_Flags     : constant GType := 13 * (2 ** GType_Fundamental_Shift);
   GType_Float     : constant GType := 14 * (2 ** GType_Fundamental_Shift);
   GType_Double    : constant GType := 15 * (2 ** GType_Fundamental_Shift);
   GType_String    : constant GType := 16 * (2 ** GType_Fundamental_Shift);
   --  Null terminated string.

   GType_Pointer   : constant GType := 17 * (2 ** GType_Fundamental_Shift);
   --  A general pointer type.

   GType_Param     : constant GType := 19 * (2 ** GType_Fundamental_Shift);
   GType_Object    : constant GType := 20 * (2 ** GType_Fundamental_Shift);
   --  One of the widgets/objects

   -----------------
   -- Boxed types --
   -----------------
   --  Boxed types are a convenient way to encapsulate Ada types through a C
   --  layer. An initialization and a finalization function can be provided.
   --  The most frequent usage of such types is in argument to signals and
   --  handlers (See the functions in Glib.Values), or to store such types
   --  in a Gtk_Tree_Model. This allows you for instance to store reference
   --  counted types where you want to be able to control what should happen
   --  when the cell is removed from the tree.
   --
   --  See an example with the subprogram Glib.Values.Set_Boxed

   GType_Boxed     : constant GType := 18 * (2 ** GType_Fundamental_Shift);
   --  The base type for all boxed types. In tree models, you should use the
   --  actual type returned by Boxed_Type_Register_Static.

   type Boxed_Copy is access
      function (Boxed : System.Address) return System.Address;
   pragma Convention (C, Boxed_Copy);
   type Boxed_Free is access procedure (Boxed : System.Address);
   pragma Convention (C, Boxed_Free);

   function Boxed_Type_Register_Static
     (Name : String;
      Copy : Boxed_Copy;
      Free : Boxed_Free) return GType;
   --  Create a new boxed type

private
   type C_Dummy is record
      Dummy1 : System.Address;
   end record;
   pragma Convention (C, C_Dummy);
   --  This array can contain anything, since it is never used on the Ada side
   --  anyway. Pretend it contains an address so that the compiler a
   --  reasonable default alignment, compatible with most types.

   type GType_Class is new System.Address;

   type Property is new String;

   type Signal_Id is new Guint;
   Invalid_Signal_Id : constant Signal_Id := -1;
   Null_Signal_Id : constant Signal_Id := 0;

   pragma Import (C, Fundamental, "ada_gtype_fundamental");
   pragma Import (C, Parent, "g_type_parent");
   pragma Import (C, Get_Qdata, "g_type_get_qdata");
   pragma Import (C, Set_Qdata, "g_type_set_qdata");
   pragma Inline (Build);

end Glib;
