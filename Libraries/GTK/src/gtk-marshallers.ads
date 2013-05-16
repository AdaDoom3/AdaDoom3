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
--  This package provides a set of generic packages to easily create
--  some Marshallers. Although this package has been designed to be
--  easily reusable, its primary aim is to simplify the use of callbacks.
--
--  Note that most users don't need to understand or even look at this
--  package, since the main functions are also renamed in the Gtk.Handlers
--  package (They are called To_Marshaller). This package is rather
--  complex (generic packages inside generic packages), and thus you should
--  understand correctly how Gtk.Handlers work before looking at this one.
--
--  To understand the paradigm used in this package, some definitions
--  are necessary:
--
--     A Handler, or Callback, is a subprogram provided by the user.
--     This handler, when attached to a particular object, will be
--     called when certain events happen during the life of this
--     object. All handlers take as a first argument an access to
--     the object they were attached to. Depending on the signal, this
--     handler can also have some extra parameters; most of the time,
--     only one extra parameter will be used. For more information about
--     Handlers, refer to the package Gtk.Handlers, where this notion is
--     explained in more details.
--
--     A General_Handler is an access to any Handler. Note that this is
--     a type used internally, most users should *not* be using it. It is
--     publicly declared so that users can create new marshallers that
--     would not be already provided here.
--
--     A Handler_Proxy is a subprogram that calls its associated
--     handler with the appropriate arguments (from an array of arguments
--     stored in Glib.Values.GValues)
--
--     A Marshaller is the association of a General_Handler and a
--     Handler_Proxy.
--
--  This package is divided in four generic packages. Each package has
--  been designed to cover a certain kind of callback by providing the
--  associated marshallers. There are two primary factors that describe
--  a callback, and that decide which marshaller to use: Does the
--  callback have access to some user data?  Does the callback return
--  some value?
--
--  Depending on that, the appropriate generic package should be chosen.
--  For example, if the callback returns a value, but does not expect
--  user data, then the "Return_Marshallers" package should be used.
--  More details about the usage of each package is provided individually
--  below.
--
--  Each of these packages is in turn divided into three generic
--  sub-packages.  The organization of these subpackages is always the
--  same :
--     o The type "Handler" is defined. It describes the profile of the
--       Handler covered in this generic package.
--     o a "To_Marshaller" function is provided to build a Marshaller
--       from any Handler.
--     o A "Emit_By_Name" procedure is also provided to allow the user
--       to "emit" a signal. This service is explained in more details in
--       Gtk.Handlers.
--     o A private function "Call" is also defined. This is the actual
--       Handler_Proxy that will be used when creating Marshallers with
--       the "To_Marshaller" service.
--
--  Once again, selecting the right generic sub-package depends on the
--  callback. For instance, the first sub-package, always called
--  "Generic_Marshaller", is to be used when the handler has one extra
--  argument which is a simple non-tagged type. More details about the
--  usage of each sub-package is also provided individually.
--
--  Although most of the cases are covered by the packages below, some
--  unusual cases may appear. This is the case for example when the
--  callback accepts several extra parameters. In such cases, two options
--  are available: The first option is to use the "standard" callback
--  mechanism with one parameter, this parameter being an array of
--  arguments that you will parse yourself. The second option is to
--  create a new Marshaller package. This is more interesting if more
--  than one callback will follow the same pattern. The body of this
--  package can be used as a good model to build such new marshallers.
--  See also the example in the GtkAda distribution for how to create your
--  own marshallers.
--
--  </description>
--  <group>Signal handling</group>
--  <c_version>2.8.17</c_version>

with Glib.Object;
with Gtk.Widget;
with Glib.Values;

package Gtk.Marshallers is

   --  <doc_ignore>Do not create automatic documentation for this package

   type General_Handler is access procedure;

   --------------------------------------------------------------
   --  Return Marshallers: Return a value, don't have user data
   --------------------------------------------------------------

   generic
      type Widget_Type is new Glib.Object.GObject_Record with private;
      type Return_Type is (<>);
   package Return_Marshallers is

      type Handler_Proxy is access function
        (Widget  : access Widget_Type'Class;
         Params  : Glib.Values.GValues;
         Cb      : General_Handler) return Return_Type;

      type Marshaller is record
         Func  : General_Handler;   --  User callback
         Proxy : Handler_Proxy;     --  Handler_Proxy for this callback
      end record;

      --  Basic Marshaller
      generic
         type Base_Type is private;
         with function Conversion
           (Value : Glib.Values.GValue) return Base_Type;

      package Generic_Marshaller is
         type Handler is access function
           (Widget : access Widget_Type'Class;
            Param  : Base_Type) return Return_Type;

         function To_Marshaller (Cb : Handler) return Marshaller;

         function Emit_By_Name
           (Object : access Widget_Type'Class;
            Name   : Glib.Signal_Name;
            Param  : Base_Type) return Return_Type;
         --  The function above should be used when Base_Type can be passed
         --  as is to C.

         generic
            with function Conversion (Param : Base_Type) return System.Address;
         function Emit_By_Name_Generic
           (Object : access Widget_Type'Class;
            Name   : Glib.Signal_Name;
            Param  : Base_Type) return Return_Type;
         --  Provide an explicit conversion function for PARAM.

      private
         function Call
           (Widget : access Widget_Type'Class;
            Params : Glib.Values.GValues;
            Cb     : General_Handler) return Return_Type;

         Call_Access : constant Handler_Proxy := Call'Access;
      end Generic_Marshaller;

      --  Widget Marshaller
      generic
         type Base_Type is new Gtk.Widget.Gtk_Widget_Record with private;
         type Access_Type is access all Base_Type'Class;
      package Generic_Widget_Marshaller is
         type Handler is access function
           (Widget : access Widget_Type'Class;
            Param  : access Base_Type'Class) return Return_Type;

         function To_Marshaller (Cb : Handler) return Marshaller;

         function Emit_By_Name
           (Object : access Widget_Type'Class;
            Name   : Glib.Signal_Name;
            Param  : access Base_Type'Class) return Return_Type;

      private
         function Call
           (Widget : access Widget_Type'Class;
            Params : Glib.Values.GValues;
            Cb     : General_Handler) return Return_Type;

         Call_Access : constant Handler_Proxy := Call'Access;
      end Generic_Widget_Marshaller;

      --  Void Marshaller
      package Void_Marshaller is
         type Handler is access function
           (Widget : access Widget_Type'Class) return Return_Type;

         function To_Marshaller (Cb : Handler) return Marshaller;

         function Emit_By_Name
           (Object : access Widget_Type'Class;
            Name   : Glib.Signal_Name) return Return_Type;

      private
         function Call
           (Widget : access Widget_Type'Class;
            Params : Glib.Values.GValues;
            Cb     : General_Handler) return Return_Type;

         Call_Access : constant Handler_Proxy := Call'Access;
      end Void_Marshaller;
   end Return_Marshallers;

   --------------------------------------------------------------
   --  User_Return_Marshallers: Return a value, have a user data
   --------------------------------------------------------------

   generic
      type Widget_Type is new Glib.Object.GObject_Record with private;
      type Return_Type is (<>);
      type User_Type (<>) is private;
   package User_Return_Marshallers is

      type Handler_Proxy is access function
        (Widget    : access Widget_Type'Class;
         Params    : Glib.Values.GValues;
         Cb        : General_Handler;
         User_Data : User_Type) return Return_Type;

      type Marshaller is record
         Func  : General_Handler;
         Proxy : Handler_Proxy;
      end record;

      --  Basic Marshaller
      generic
         type Base_Type is private;
         with function Conversion
           (Value : Glib.Values.GValue) return Base_Type;

      package Generic_Marshaller is
         type Handler is access function
           (Widget    : access Widget_Type'Class;
            Param     : Base_Type;
            User_Data : User_Type) return Return_Type;
         function To_Marshaller (Cb : Handler) return Marshaller;

         function Emit_By_Name
           (Object : access Widget_Type'Class;
            Name   : Glib.Signal_Name;
            Param  : Base_Type) return Return_Type;
         --  The function above should be used when BASE_TYPE can be passed
         --  as is to C.

         generic
            with function Conversion (Param : Base_Type) return System.Address;
         function Emit_By_Name_Generic
           (Object : access Widget_Type'Class;
            Name   : Glib.Signal_Name;
            Param  : Base_Type) return Return_Type;
         --  Provide an explicit conversion function for PARAM.
      private
         function Call
           (Widget    : access Widget_Type'Class;
            Params    : Glib.Values.GValues;
            Cb        : General_Handler;
            User_Data : User_Type) return Return_Type;

         Call_Access : constant Handler_Proxy := Call'Access;
      end Generic_Marshaller;

      --  Widget Marshaller
      generic
         type Base_Type is new Gtk.Widget.Gtk_Widget_Record with private;
         type Access_Type is access all Base_Type'Class;
      package Generic_Widget_Marshaller is
         type Handler is access function
           (Widget    : access Widget_Type'Class;
            Param     : access Base_Type'Class;
            User_Data : User_Type) return Return_Type;

         function To_Marshaller (Cb : Handler) return Marshaller;

         function Emit_By_Name
           (Object : access Widget_Type'Class;
            Name   : Glib.Signal_Name;
            Param  : access Base_Type'Class) return Return_Type;

      private
         function Call
           (Widget    : access Widget_Type'Class;
            Params    : Glib.Values.GValues;
            Cb        : General_Handler;
            User_Data : User_Type) return Return_Type;

         Call_Access : constant Handler_Proxy := Call'Access;
      end Generic_Widget_Marshaller;

      --  Void Marshaller
      package Void_Marshaller is
         type Handler is access function
           (Widget    : access Widget_Type'Class;
            User_Data : User_Type) return Return_Type;

         function To_Marshaller (Cb : Handler) return Marshaller;

         function Emit_By_Name
           (Object : access Widget_Type'Class;
            Name   : Glib.Signal_Name) return Return_Type;

      private
         function Call
           (Widget    : access Widget_Type'Class;
            Params    : Glib.Values.GValues;
            Cb        : General_Handler;
            User_Data : User_Type) return Return_Type;

         Call_Access : constant Handler_Proxy := Call'Access;
      end Void_Marshaller;

   end User_Return_Marshallers;

   -----------------
   --  Callback_Marshallers: Do not return a value, no user data
   -----------------

   generic
      type Widget_Type is new Glib.Object.GObject_Record with private;
   package Void_Marshallers is

      type Handler_Proxy is access procedure
        (Widget : access Widget_Type'Class;
         Params : Glib.Values.GValues;
         Cb     : General_Handler);

      type Marshaller is record
         Func  : General_Handler;
         Proxy : Handler_Proxy;
      end record;

      --  Basic Marshaller
      generic
         type Base_Type is private;
         with function Conversion
           (Value : Glib.Values.GValue) return Base_Type;

      package Generic_Marshaller is
         type Handler is access procedure
           (Widget : access Widget_Type'Class;
            Param  : Base_Type);

         function To_Marshaller (Cb : Handler) return Marshaller;

         procedure Emit_By_Name
           (Object : access Widget_Type'Class;
            Name   : Glib.Signal_Name;
            Param  : Base_Type);
         --  The function above should be used when BASE_TYPE can be passed
         --  as is to C.

         generic
            with function Conversion (Param : Base_Type) return System.Address;
         procedure Emit_By_Name_Generic
           (Object : access Widget_Type'Class;
            Name   : Glib.Signal_Name;
            Param  : Base_Type);
         --  Provide an explicit conversion function for PARAM.

      private
         procedure Call
           (Widget : access Widget_Type'Class;
            Params : Glib.Values.GValues;
            Cb     : General_Handler);

         Call_Access : constant Handler_Proxy := Call'Access;
      end Generic_Marshaller;

      generic
         type Base_Type_1 is private;
         with function Conversion
           (Value : Glib.Values.GValue) return Base_Type_1;
         type Base_Type_2 is private;
         with function Conversion
           (Value : Glib.Values.GValue) return Base_Type_2;

      package Generic_Marshaller_2 is
         type Handler is access procedure
           (Widget  : access Widget_Type'Class;
            Param_1 : Base_Type_1;
            Param_2 : Base_Type_2);

         function To_Marshaller (Cb : Handler) return Marshaller;

         procedure Emit_By_Name
           (Object  : access Widget_Type'Class;
            Name    : Glib.Signal_Name;
            Param_1 : Base_Type_1;
            Param_2 : Base_Type_2);
         --  The function above should be used when BASE_TYPE can be passed
         --  as is to C.

         generic
            with function Conversion
                            (Param : Base_Type_1) return System.Address;
            with function Conversion
                            (Param : Base_Type_2) return System.Address;
         procedure Emit_By_Name_Generic
           (Object  : access Widget_Type'Class;
            Name    : Glib.Signal_Name;
            Param_1 : Base_Type_1;
            Param_2 : Base_Type_2);
         --  Provide an explicit conversion function for PARAM.

      private
         procedure Call
           (Widget : access Widget_Type'Class;
            Params : Glib.Values.GValues;
            Cb     : General_Handler);

         Call_Access : constant Handler_Proxy := Call'Access;
      end Generic_Marshaller_2;

      --  Widget Marshaller
      generic
         type Base_Type is new Gtk.Widget.Gtk_Widget_Record with private;
         type Access_Type is access all Base_Type'Class;
      package Generic_Widget_Marshaller is
         type Handler is access procedure
           (Widget : access Widget_Type'Class;
            Param  : access Base_Type'Class);

         function To_Marshaller (Cb : Handler) return Marshaller;

         procedure Emit_By_Name
           (Object : access Widget_Type'Class;
            Name   : Glib.Signal_Name;
            Param  : access Base_Type'Class);

      private
         procedure Call
           (Widget : access Widget_Type'Class;
            Params : Glib.Values.GValues;
            Cb     : General_Handler);

         Call_Access : constant Handler_Proxy := Call'Access;
      end Generic_Widget_Marshaller;

      --  Void Marshaller
      package Void_Marshaller is
         type Handler is access procedure (Widget : access Widget_Type'Class);

         function To_Marshaller (Cb : Handler) return Marshaller;

         procedure Emit_By_Name
           (Object : access Widget_Type'Class;
            Name   : Glib.Signal_Name);

      private
         procedure Call
           (Widget : access Widget_Type'Class;
            Params : Glib.Values.GValues;
            Cb     : General_Handler);

         Call_Access : constant Handler_Proxy := Call'Access;
      end Void_Marshaller;

   end Void_Marshallers;

   ----------------------------------------------------------------------
   --  User_Callback_Marshallers: Do not return a value, have user data
   ----------------------------------------------------------------------

   generic
      type Widget_Type is new Glib.Object.GObject_Record with private;
      type User_Type (<>) is private;
   package User_Void_Marshallers is
      type Handler_Proxy is access procedure
        (Widget    : access Widget_Type'Class;
         Params    : Glib.Values.GValues;
         Cb        : General_Handler;
         User_Data : User_Type);

      type Marshaller is record
         Func  : General_Handler;
         Proxy : Handler_Proxy;
      end record;

      --  Basic Marshaller
      generic
         type Base_Type is private;
         with function Conversion
           (Value : Glib.Values.GValue) return Base_Type;

      package Generic_Marshaller is
         type Handler is access procedure
           (Widget    : access Widget_Type'Class;
            Param     : Base_Type;
            User_Data : User_Type);

         function To_Marshaller (Cb : Handler) return Marshaller;

         procedure Emit_By_Name
           (Object : access Widget_Type'Class;
            Name   : Glib.Signal_Name;
            Param  : Base_Type);
         --  The function above should be used when BASE_TYPE can be passed
         --  as is to C.

         generic
            with function Conversion (Param : Base_Type) return System.Address;
         procedure Emit_By_Name_Generic
           (Object : access Widget_Type'Class;
            Name   : Glib.Signal_Name;
            Param  : Base_Type);
         --  Provide an explicit conversion function for PARAM.

      private
         procedure Call
           (Widget    : access Widget_Type'Class;
            Params    : Glib.Values.GValues;
            Cb        : General_Handler;
            User_Data : User_Type);

         Call_Access : constant Handler_Proxy := Call'Access;
      end Generic_Marshaller;

      generic
         type Base_Type_1 is private;
         with function Conversion
           (Value : Glib.Values.GValue) return Base_Type_1;
         type Base_Type_2 is private;
         with function Conversion
           (Value : Glib.Values.GValue) return Base_Type_2;

      package Generic_Marshaller_2 is

         type Handler is access procedure
           (Widget    : access Widget_Type'Class;
            Param_1   : Base_Type_1;
            Param_2   : Base_Type_2;
            User_Data : User_Type);

         function To_Marshaller (Cb : Handler) return Marshaller;

         procedure Emit_By_Name
           (Object  : access Widget_Type'Class;
            Name    : Glib.Signal_Name;
            Param_1 : Base_Type_1;
            Param_2 : Base_Type_2);
         --  The function above should be used when BASE_TYPE can be passed
         --  as is to C.

         generic
            with function Conversion
                            (Param : Base_Type_1) return System.Address;
            with function Conversion
                            (Param : Base_Type_2) return System.Address;
         procedure Emit_By_Name_Generic
           (Object  : access Widget_Type'Class;
            Name    : Glib.Signal_Name;
            Param_1 : Base_Type_1;
            Param_2 : Base_Type_2);
         --  Provide an explicit conversion function for PARAM.

      private
         procedure Call
           (Widget    : access Widget_Type'Class;
            Params    : Glib.Values.GValues;
            Cb        : General_Handler;
            User_Data : User_Type);

         Call_Access : constant Handler_Proxy := Call'Access;
      end Generic_Marshaller_2;

      --  Widget Marshaller
      generic
         type Base_Type is new Gtk.Widget.Gtk_Widget_Record with private;
         type Access_Type is access all Base_Type'Class;
      package Generic_Widget_Marshaller is
         type Handler is access procedure
           (Widget    : access Widget_Type'Class;
            Param     : access Base_Type'Class;
            User_Data : User_Type);

         function To_Marshaller (Cb : Handler) return Marshaller;

         procedure Emit_By_Name
           (Object : access Widget_Type'Class;
            Name   : Glib.Signal_Name;
            Param  : access Base_Type'Class);

      private
         procedure Call
           (Widget    : access Widget_Type'Class;
            Params    : Glib.Values.GValues;
            Cb        : General_Handler;
            User_Data : User_Type);

         Call_Access : constant Handler_Proxy := Call'Access;
      end Generic_Widget_Marshaller;

      --  Void Marshaller
      package Void_Marshaller is
         type Handler is access procedure
           (Widget    : access Widget_Type'Class;
            User_Data : User_Type);

         function To_Marshaller (Cb : Handler) return Marshaller;

         procedure Emit_By_Name
           (Object : access Widget_Type'Class;
            Name   : Glib.Signal_Name);

      private
         procedure Call
           (Widget    : access Widget_Type'Class;
            Params    : Glib.Values.GValues;
            Cb        : General_Handler;
            User_Data : User_Type);

         Call_Access : constant Handler_Proxy := Call'Access;
      end Void_Marshaller;

   end User_Void_Marshallers;

   --  </doc_ignore>
end Gtk.Marshallers;
