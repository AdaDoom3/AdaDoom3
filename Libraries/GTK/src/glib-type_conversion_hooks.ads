-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2001-2013, AdaCore                   --
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
--  This package provides an implementation for hooks used in
--  Gtk.Type_Conversion. These hooks should be used when you import a new
--  C GObject, so that GtkAda can recreate the Ada structure from the
--  underlying C structure.
--  Note that when you create a GObject directly in Ada, you do not need to
--  provide any hook.
--
--  Implementation note: This is a separate package from Gtk.Type_Conversion
--  so that adding a hook does not necessarily mean the user has to 'with'
--  Gtk.Type_Conversion, and thus all the packages from GtkAda.
--
--  Note that this package is not thread safe. You should call the
--  function Add_Hook from the elaboration part of your packages.
--
--  </description>
--  <group>Glib, the general-purpose library</group>

with Glib.Object; use Glib.Object;

package Glib.Type_Conversion_Hooks is

   --  <doc_ignore>

   function Conversion_Function
     (Obj : System.Address; Stub : GObject_Record'Class) return GObject;
   --  This function has to convert a C object to an Ada object.
   --  It will first try all the registered functions (in
   --  Glib.Type_Conversion_Hooks). If no match is found, then it will try
   --  recursively all parents of the C object. If no match is found at all,
   --  it will create an object of type Expected_Type, no matter what the real
   --  C type is.

   type Get_GType_Func is access function return Glib.GType;
   pragma Convention (C, Get_GType_Func);
   --  Type used during the type conversion process

   type Conversion_Creator_Hook_Type is
     access function (Expected_Object : GObject_Record'Class) return GObject;

   --  </doc_ignore>

   --  This package is used to allow automatic conversion from a C gtk object
   --  to Ada.
   --  To allow GtkAda to automatically bind an incoming externally created
   --  widget to the correct Ada type, you just need to instantiate this
   --  package, that will then automatically register the appropriate
   --  conversion methods.
   generic
      Get_GType : Get_GType_Func;
      --  This function returns the GType assiciated with the type we want to
      --  convert to. Usually, all widgets have a class-wide Get_Type that can
      --  directly be used here.

      type Handled_Type is new GObject_Record with private;
      --  The type we want to convert to.

   package Hook_Registrator is

      function Creator (Expected_Object : GObject_Record'Class) return GObject;
      --  This function will create an Ada type corresponding to Handled_Type.
      --  In case Expected_Object is a child type of Handled_Type, an Ada
      --  object of type Expected_Object is returned instead.
      --
      --  This allows convertion of types we know are expected, but don't have
      --  registered conversion hook functions.

   private
      Creator_Access : constant Conversion_Creator_Hook_Type := Creator'Access;
      --  We need to create this access type here because of RM 3.10.2(28)
      --  It should go to the body, but conversion of Creator'Access to
      --  Conversion_Creator_Hook_Type is only allowed in the private section.
   end Hook_Registrator;

end Glib.Type_Conversion_Hooks;
