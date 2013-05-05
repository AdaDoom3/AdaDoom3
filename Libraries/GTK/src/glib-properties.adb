-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                Copyright (C) 2001-2006 AdaCore                    --
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

with Glib.Object; use Glib.Object;
with Glib.Values; use Glib.Values;

package body Glib.Properties is

   procedure Get
     (Object : System.Address; Name : Glib.Property; Value : in out GValue);
   procedure Get
     (Object : System.Address; Name : String; Value : in out GValue);
   pragma Import (C, Get, "g_object_get_property");
   --  Internal function to get the properties

   procedure Set
     (Object : System.Address; Name : String; Value : in out GValue);
   pragma Import (C, Set, "g_object_set_property");
   --  Internal function to set the properties

   ------------------
   -- Get_Property --
   ------------------

   procedure Get_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : String;
      Value  : in out Glib.Values.GValue)
   is
   begin
      Get (Get_Object (Object), Name & ASCII.NUL, Value);
   end Get_Property;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : String;
      Value  : in out Glib.Values.GValue)
   is
   begin
      Set (Get_Object (Object), Name & ASCII.NUL, Value);
   end Set_Property;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Object : access GObject_Record'Class;
      Name   : Property_String_WO;
      Value  : String)
   is
      procedure Internal
        (Object : System.Address;
         Name   : Property;
         Value  : String);
      pragma Import (C, Internal, "ada_g_object_set_string");
   begin
      Internal (Get_Object (Object), Property (Name), Value & ASCII.NUL);
   end Set_Property;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Object : access GObject_Record'Class;
      Name   : Property_String;
      Value  : String) is
   begin
      Set_Property (Object, Property_String_WO (Name), Value);
   end Set_Property;

   ------------------
   -- Get_Property --
   ------------------

   function Get_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : Property_String_RO) return String
   is
      Value : GValue;
   begin
      Init (Value, GType_String);
      Get (Get_Object (Object), Property (Name), Value);
      declare
         S : constant String := Get_String (Value);
      begin
         Unset (Value);
         return S;
      end;
   end Get_Property;

   ------------------
   -- Get_Property --
   ------------------

   function Get_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : Property_String) return String is
   begin
      return Get_Property (Object, Property_String_RO (Name));
   end Get_Property;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : Property_Boolean;
      Value  : Boolean)
   is
      procedure Internal
        (Object : System.Address;
         Name   : Property;
         Value  : Gint);
      pragma Import (C, Internal, "ada_g_object_set_int");

   begin
      Internal (Get_Object (Object), Property (Name), Boolean'Pos (Value));
   end Set_Property;

   ------------------
   -- Get_Property --
   ------------------

   function Get_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name : Property_Boolean) return Boolean
   is
      Value : GValue;
      B : Boolean;
   begin
      Init (Value, GType_Boolean);
      Get (Get_Object (Object), Property (Name), Value);
      B := Get_Boolean (Value);
      Unset (Value);
      return B;
   end Get_Property;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : Property_Object;
      Value  : access Glib.Object.GObject_Record'Class) is
   begin
      Set_Property (Object, Property_Address (Name), Get_Object (Value));
   end Set_Property;

   ------------------
   -- Get_Property --
   ------------------

   function Get_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name : Property_Object) return Glib.Object.GObject
   is
      Value : GValue;
      Addr  : System.Address;
      Stub  : GObject_Record;
   begin
      Init (Value, GType_Object);
      Get (Get_Object (Object), Property (Name), Value);
      Addr := Get_Address (Value);
      Unset (Value);
      return Get_User_Data (Addr, Stub);
   end Get_Property;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : Property_Object_WO;
      Value  : access Glib.Object.GObject_Record'Class) is
   begin
      Set_Property (Object, Property_Address (Name), Get_Object (Value));
   end Set_Property;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : Property_Address;
      Value  : System.Address)
   is
      procedure Internal
        (Object : System.Address;
         Name   : Property;
         Value  : System.Address);
      pragma Import (C, Internal, "ada_g_object_set_ptr");

   begin
      Internal (Get_Object (Object), Property (Name), Value);
   end Set_Property;

   ------------------
   -- Get_Property --
   ------------------

   function Get_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name : Property_Address) return System.Address
   is
      Value : GValue;
      A     : System.Address;
   begin
      Init (Value, GType_Pointer);
      Get (Get_Object (Object), Property (Name), Value);
      A := Get_Address (Value);
      Unset (Value);
      return A;
   end Get_Property;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : Property_Float;
      Value  : Gfloat)
   is
      procedure Internal
        (Object : System.Address;
         Name   : Property;
         Value  : Gfloat);
      pragma Import (C, Internal, "ada_g_object_set_float");

   begin
      Internal (Get_Object (Object), Property (Name), Value);
   end Set_Property;

   ------------------
   -- Get_Property --
   ------------------

   function Get_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : Property_Float) return Gfloat
   is
      Value : GValue;
      A     : Gfloat;
   begin
      Init (Value, GType_Float);
      Get (Get_Object (Object), Property (Name), Value);
      A := Get_Float (Value);
      Unset (Value);
      return A;
   end Get_Property;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : Property_Double;
      Value  : Gdouble)
   is
      procedure Internal
        (Object : System.Address;
         Name   : Property;
         Value  : Gdouble);
      pragma Import (C, Internal, "ada_g_object_set_double");
   begin
      Internal (Get_Object (Object), Property (Name), Value);
   end Set_Property;

   ------------------
   -- Get_Property --
   ------------------

   function Get_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name : Property_Double) return Gdouble
   is
      Value : GValue;
      A     : Gdouble;
   begin
      Init (Value, GType_Double);
      Get (Get_Object (Object), Property (Name), Value);
      A := Get_Double (Value);
      Unset (Value);
      return A;
   end Get_Property;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : Property_C_Proxy;
      Value  : C_Proxy)
   is
      procedure Internal
        (Object : System.Address;
         Name   : Property;
         Value  : C_Proxy);
      pragma Import (C, Internal, "ada_g_object_set_ptr");
   begin
      Internal (Get_Object (Object), Property (Name), Value);
   end Set_Property;

   ------------------
   -- Get_Property --
   ------------------

   function Get_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name : Property_C_Proxy) return C_Proxy
   is
      Value : GValue;
      A     : C_Proxy;
   begin
      Init (Value, GType_Object);
      Get (Get_Object (Object), Property (Name), Value);
      A := Get_Proxy (Value);
      Unset (Value);
      return A;
   end Get_Property;

end Glib.Properties;
