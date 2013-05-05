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
--  The aim of this package is to provide some services to connect a
--  handler to a signal emitted by a Gtk Object. To understand the
--  services provided by this package, some definitions are necessary:
--
--    Signal: A signal is a kind of message that an object wants to
--    broadcast. All GObjects can emit signals. These messages are
--    associated to certain events happening during the life of an
--    object. For instance, when a user clicks on a button, the
--    "clicked" signal is emitted by the button.
--
--    Handler (or callback): A handler is a function or procedure that
--    the user "connects" to a signal for a particular object.
--    Connecting a handler to a signal means associating this handler to
--    the signal.  When the signal is emitted, all connected handlers
--    are called back. Usually, the role of those callbacks is to do
--    some processing triggered by a user action. For instance, when
--    "clicked" signal is emitted by the "OK" button of a dialog, the
--    connected handler can be used to close the dialog or recompute
--    some value.
--
--    In GtkAda, the handlers are defined in a form as general as
--    possible. The first argument is always an access to the object it
--    has been connected to. The second object is a table of values
--    (See Glib.Values for more details about this table). It is the
--    responsibility of this handler to extract the values from it, and
--    to convert them to the correct Ada type.
--
--    Because such handlers are not very convenient to use, this package
--    also provides some services to connect a marshaller instead. It
--    will then do the extraction work before calling the more
--    programmer-friendly handler, as defined in Gtk.Marshallers (see
--    Gtk.Marshallers for more details).
--
--  The subdivision of this package is identical to Gtk.Marshallers; it
--  is made of four generic sub-packages, each representing one of the
--  four possible kinds of handlers: they can return a value or not, and
--  they can have some user specific data associated to them or not.
--  Selecting the right package depends on the profile of the handler.
--  For example, the handler for the "delete_event" signal of a
--  Gtk_Window has a return value, and has an extra parameter (a Gint).
--  All handlers also have a user_data field by default, but its usage
--  is optional. To connect a handler to this signal, if the user_data
--  field is not used, the Return_Callback generic should be
--  instantiated. On the other hand, if the user_data field is
--  necessary, then the User_Return_Callback generic should be used.
--
--  Note also that the real handler in Gtk+ should expect at least as
--  many arguments as in the marshaller you are using. If your
--  marshaller has one argument, the C handler must have at least one
--  argument too.
--
--  The common generic parameter to all sub-packages is the widget type,
--  which is the basic widget manipulated. This can be
--  Glib.Object.GObject_Record type if you want to reduce the number of
--  instantiations, but the conversion to the original type will have to be
--  done inside the handler.
--
--  All sub-packages are organized in the same way.
--
--    First, the type "Handler" is defined. It represents the general
--    form of the callbacks supported by the sub-package.
--
--    The corresponding sub-package of Gtk.Marshallers is instantiated.
--
--    A series of "Connect" procedures and functions is given. All cases
--    are covered: the functions return the Handler_Id of the newly
--    created association, while the procedures just connect the
--    handler, dropping the Handler_Id; some services allow the user to
--    connect a Handler while some others allow the usage of
--    Marshallers, which are more convenient. Note that more than one
--    handler may be connected to a signal; the handlers will then be
--    invoked in the order of connection.
--
--    Some "Connect_Object" services are also provided. Those services
--    never have a user_data. They accept an additional parameter called
--    Slot_Object. When the callback in invoked, the Gtk Object emitting
--    the signal is substituted by this Slot_Object.
--    These callbacks are always automatically disconnected as soon as one
--    of the two widgets involved is destroyed.
--
--    There are several methods to connect a handler. For each method,
--    although the option of connecting a Handler is provided, the
--    recommended way is to use Marshallers. Each connect service is
--    documented below, in the first sub-package.
--
--    A series of "To_Marshaller" functions are provided. They return
--    some marshallers for the most commonly used types in order to ease
--    the usage of this package. Most of the time, it will not be
--    necessary to use some other marshallers.
--    For instance, if a signal is documented as receiving a single argument,
--    the widget (for instance the "clicked" signal for a Gtk_Button), you
--    will connect to it with:
--        with Gtkada.Handlers;
--        procedure On_Clicked (Button : access Gtk_Widget_Record'Class);
--        ...
--           Widget_Callback.Connect (Button, "clicked", On_Clicked'Access);
--
--    The simple form above also applies for most handlers that take one
--    additional argument, for instance the "button_press_event" in
--    gtk-widget.ads. Just declare your subprogram with the appropriate profile
--    and connect it, as in:
--        with Gtkada.Handlers;
--        procedure On_Button (Widget : access Gtk_Widget_Record'Class;
--                             Event  : Gdk_Event);
--        ...
--           Widget_Callback.Connect (Widget, "button_press_event",
--                                    On_Button'Access);
--
--    More complex forms of handlers exists however in GtkAda, for which no
--    predefined marshaller exists. In this case, you have to use the general
--    form of callbacks. For instance, the "select_row" signal of Gtk.Clist.
--        with Gtkada.Handlers;
--        with Gtk.Arguments;
--        procedure On_Select (Clist : access Gtk_Widget_Record'Class;
--                             Args  : Glib.Values.GValues)
--        is
--           Row : constant Gint := To_Gint (Args, 1);
--           Column : constant Gint := To_Gint (Args, 2);
--           Event  : constant Gdk_Event := To_Event (Args, 3);
--        begin
--           ...
--        end On_Select;
--        ...
--            Widget_Callback.Connect (Clist, "select_row", On_Select'Access);
--
--    As for the "To_Marshaller" functions, a series of "Emit_By_Name"
--    procedures are also provided for the same most common types, to
--    allow the user to easily emit signals. These procedures are mainly
--    intended for people building new GObjects.
--
--  At the end of this package, some general services related to the
--  management of signals and handlers are also provided. Each one of
--  them is documented individually below.
--
--  IMPORTANT NOTE: These packages must be instantiated at library-level
--
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Signal handling</group>

with Glib.Values;
with Gdk.Event;
with Glib.Object;
with Gtk.Marshallers;
pragma Elaborate_All (Gtk.Marshallers);

with Gtk.Notebook;
with Gtk.Tree_Model;
with Gtk.Widget;

with Unchecked_Conversion;

package Gtk.Handlers is

   --  <doc_ignore>

   pragma Elaborate_Body;

   type GClosure is new Glib.C_Proxy;

   Null_Handler_Id : constant Gulong := 0;

   type Handler_Id is record
      Id      : Gulong := Null_Handler_Id;
      Closure : GClosure;
   end record;
   --  This uniquely identifies a connection widget<->signal.
   --  Closure is an internal data, that you should not use.

   ---------------------------------------------------------
   --  These handlers should return a value
   --  They do not have a User_Data
   ---------------------------------------------------------

   generic
      type Widget_Type is new Glib.Object.GObject_Record with private;
      type Return_Type is (<>);
   package Return_Callback is

      type Handler is access function
        (Widget : access Widget_Type'Class;
         Params : Glib.Values.GValues) return Return_Type;

      type Simple_Handler is access function
        (Widget : access Widget_Type'Class) return Return_Type;

      package Marshallers is new Gtk.Marshallers.Return_Marshallers
        (Widget_Type, Return_Type);

      --  Connecting a handler to an object

      --  In all the Connect services below, the following arguments
      --  will be used:
      --    o Widget, Name: This represents the association (Gtk Object,
      --      Glib.Signal_Name) to which the handler is to be connected.
      --    o After: If this boolean is set to True, then the handler
      --      will be connected after all the default handlers. By
      --      default, it is set to False.

      procedure Connect
        (Widget : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Marsh  : Marshallers.Marshaller;
         After  : Boolean := False);
      --  Connects a Marshaller. The Handler_Id is dropped.

      procedure Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Marsh       : Marshallers.Marshaller;
         Slot_Object : access Widget_Type'Class;
         After       : Boolean := False);
      --  Connect a Marshaller. The Handler_Id is dropped.
      --  This is automatically disconnected as soon as either Widget or
      --  Slot_Object is destroyed.
      --  Slot_Object *must* be of type Gtk_Object or one of its children.

      procedure Connect
        (Widget : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Cb     : Simple_Handler;
         After  : Boolean := False);
      procedure Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Cb          : Simple_Handler;
         Slot_Object : access Widget_Type'Class;
         After       : Boolean := False);
      --  Same as above, except with a simple handle with no parameter. This
      --  is the same as using a To_Marshaller call to the above two
      --  procedures, except it is shorter to write.

      procedure Connect
        (Widget : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Cb     : Handler;
         After  : Boolean := False);
      procedure Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Cb          : Handler;
         Slot_Object : access Widget_Type'Class;
         After       : Boolean := False);
      --  Connect a Handler. The Handler_Id is dropped.
      --  This is automatically disconnected as soon as either Widget or
      --  Slot_Object is destroyed.
      --  Slot_Object *must* be of type Gtk_Object or one of its children.

      pragma Inline (Connect);
      pragma Inline (Object_Connect);

      function Connect
        (Widget : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Marsh  : Marshallers.Marshaller;
         After  : Boolean := False) return Handler_Id;
      --  Connects a Marshaller. Returns the Handler_Id.

      function Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Marsh       : Marshallers.Marshaller;
         Slot_Object : access Widget_Type'Class;
         After       : Boolean := False) return Handler_Id;
      --  Connect a Marshaller. Return the Handler_Id.
      --  This is automatically disconnected as soon as either Widget or
      --  Slot_Object is destroyed.
      --  Slot_Object *must* be of type Gtk_Object or one of its children.

      function Connect
        (Widget : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Cb     : Handler;
         After  : Boolean := False) return Handler_Id;
      --  Connects a Handler. Returns the Handler_Id.

      function Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Cb          : Handler;
         Slot_Object : access Widget_Type'Class;
         After       : Boolean := False) return Handler_Id;
      --  Connect a Handler. Returns the Handler_Id.
      --  This is automatically disconnected as soon as either Widget or
      --  Slot_Object is destroyed.
      --  Slot_Object *must* be of type Gtk_Object or one of its children.

      --  Some convenient functions to create marshallers

      package Gint_Marshaller is new Marshallers.Generic_Marshaller
        (Gint, Glib.Values.Get_Int);
      package Guint_Marshaller is new Marshallers.Generic_Marshaller
        (Guint, Glib.Values.Get_Uint);
      package Event_Marshaller is new Marshallers.Generic_Marshaller
        (Gdk.Event.Gdk_Event, Gdk.Event.Get_Event);
      package Widget_Marshaller is new Marshallers.Generic_Widget_Marshaller
        (Gtk.Widget.Gtk_Widget_Record, Gtk.Widget.Gtk_Widget);
      package Notebook_Page_Marshaller is new Marshallers.Generic_Marshaller
        (Gtk.Notebook.Gtk_Notebook_Page, Gtk.Notebook.Get_Notebook_Page);

      function To_Marshaller
        (Cb : Gint_Marshaller.Handler)
         return Marshallers.Marshaller renames Gint_Marshaller.To_Marshaller;

      function To_Marshaller
        (Cb : Guint_Marshaller.Handler)
         return Marshallers.Marshaller renames Guint_Marshaller.To_Marshaller;

      function To_Marshaller
        (Cb : Event_Marshaller.Handler)
         return Marshallers.Marshaller renames Event_Marshaller.To_Marshaller;

      function To_Marshaller
        (Cb : Widget_Marshaller.Handler)
         return Marshallers.Marshaller renames Widget_Marshaller.To_Marshaller;

      function To_Marshaller
        (Cb : Marshallers.Void_Marshaller.Handler)
         return Marshallers.Marshaller
         renames Marshallers.Void_Marshaller.To_Marshaller;

      function To_Marshaller
        (Cb : Notebook_Page_Marshaller.Handler)
         return Marshallers.Marshaller
         renames Notebook_Page_Marshaller.To_Marshaller;

      --  Emitting a signal

      function Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Param  : Gint)
         return Return_Type renames Gint_Marshaller.Emit_By_Name;

      function Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Param  : Guint)
         return Return_Type renames Guint_Marshaller.Emit_By_Name;

      function Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Param  : Gdk.Event.Gdk_Event) return Return_Type;

      function Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Param  : access Gtk.Widget.Gtk_Widget_Record'Class)
         return Return_Type renames Widget_Marshaller.Emit_By_Name;

      function Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name)
         return Return_Type renames Marshallers.Void_Marshaller.Emit_By_Name;

      function Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Param  : Gtk.Notebook.Gtk_Notebook_Page)
         return Return_Type renames Notebook_Page_Marshaller.Emit_By_Name;

   private
      --  <doc_ignore>
      type Acc is access all Widget_Type'Class;
      --  This type has to be declared at library level, otherwise
      --  Program_Error might be raised when trying to cast from the
      --  parameter of Marshaller to another type.

      type Data_Type_Record is record
         Func   : Handler;
         --  User's callback

         Proxy  : Marshallers.Handler_Proxy := null;
         --  Handler_Proxy to use

         Object : Acc := null;
         --  Slot Object for Object_Connect
      end record;
      type Data_Type_Access is access all Data_Type_Record;
      pragma Convention (C, Data_Type_Access);
      --  Data passed to the C handler

      function Convert is new Unchecked_Conversion
        (Data_Type_Access, System.Address);
      function Convert is new Unchecked_Conversion
        (System.Address, Data_Type_Access);

      procedure Free_Data (Data : Data_Type_Access);
      pragma Convention (C, Free_Data);
      --  Free the memory associated with the callback's data

      procedure First_Marshaller
        (Closure         : GClosure;
         Return_Value    : Glib.Values.GValue;
         N_Params        : Guint;
         Params          : System.Address;
         Invocation_Hint : System.Address;
         User_Data       : System.Address);
      pragma Convention (C, First_Marshaller);
      --  First level marshaller. This is the function that is actually
      --  called by gtk+. It then calls the Ada functions as required.
      --  </doc_ignore>

   end Return_Callback;

   ---------------------------------------------------------
   --  These handlers should return a value
   --  They require a User_Data
   --  See also the package User_Callback_With_Setup
   ---------------------------------------------------------

   generic
      type Widget_Type is new Glib.Object.GObject_Record with private;
      type Return_Type is (<>);
      type User_Type (<>) is private;
   package User_Return_Callback is

      type Handler is access function
        (Widget    : access Widget_Type'Class;
         Params    : Glib.Values.GValues;
         User_Data : User_Type) return Return_Type;
      type Simple_Handler is access function
        (Widget    : access Widget_Type'Class;
         User_Data : User_Type) return Return_Type;

      package Marshallers is new Gtk.Marshallers.User_Return_Marshallers
        (Widget_Type, Return_Type, User_Type);

      --  Connecting a handler to an object

      procedure Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Marsh     : Marshallers.Marshaller;
         User_Data : User_Type;
         After     : Boolean := False);
      procedure Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Marsh       : Marshallers.Marshaller;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False);

      procedure Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Cb        : Simple_Handler;
         User_Data : User_Type;
         After     : Boolean := False);
      procedure Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Cb          : Simple_Handler;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False);

      procedure Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Cb        : Handler;
         User_Data : User_Type;
         After     : Boolean := False);
      procedure Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Cb          : Handler;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False);

      pragma Inline (Connect);

      function Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Marsh     : Marshallers.Marshaller;
         User_Data : User_Type;
         After     : Boolean := False) return Handler_Id;

      function Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Marsh       : Marshallers.Marshaller;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False) return Handler_Id;

      function Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Cb        : Handler;
         User_Data : User_Type;
         After     : Boolean := False) return Handler_Id;

      function Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Cb          : Handler;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False) return Handler_Id;

      --  Some convenient functions to create marshallers

      package Gint_Marshaller is new Marshallers.Generic_Marshaller
        (Gint, Glib.Values.Get_Int);
      package Guint_Marshaller is new Marshallers.Generic_Marshaller
        (Guint, Glib.Values.Get_Uint);
      package Event_Marshaller is new Marshallers.Generic_Marshaller
        (Gdk.Event.Gdk_Event, Gdk.Event.Get_Event);
      package Widget_Marshaller is new Marshallers.Generic_Widget_Marshaller
        (Gtk.Widget.Gtk_Widget_Record, Gtk.Widget.Gtk_Widget);
      package Notebook_Page_Marshaller is new Marshallers.Generic_Marshaller
        (Gtk.Notebook.Gtk_Notebook_Page, Gtk.Notebook.Get_Notebook_Page);

      function To_Marshaller
        (Cb : Gint_Marshaller.Handler)
         return Marshallers.Marshaller renames Gint_Marshaller.To_Marshaller;

      function To_Marshaller
        (Cb : Guint_Marshaller.Handler)
         return Marshallers.Marshaller renames Guint_Marshaller.To_Marshaller;

      function To_Marshaller
        (Cb : Event_Marshaller.Handler)
         return Marshallers.Marshaller renames Event_Marshaller.To_Marshaller;

      function To_Marshaller
        (Cb : Widget_Marshaller.Handler)
         return Marshallers.Marshaller renames Widget_Marshaller.To_Marshaller;

      function To_Marshaller
        (Cb : Marshallers.Void_Marshaller.Handler)
         return Marshallers.Marshaller
         renames Marshallers.Void_Marshaller.To_Marshaller;

      function To_Marshaller
        (Cb : Notebook_Page_Marshaller.Handler)
         return Marshallers.Marshaller
         renames Notebook_Page_Marshaller.To_Marshaller;

      --  Emitting a signal

      function Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Param  : Gint)
         return Return_Type renames Gint_Marshaller.Emit_By_Name;

      function Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Param  : Guint)
         return Return_Type renames Guint_Marshaller.Emit_By_Name;

      function Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Param  : Gdk.Event.Gdk_Event) return Return_Type;

      function Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Param  : access Gtk.Widget.Gtk_Widget_Record'Class)
         return Return_Type renames Widget_Marshaller.Emit_By_Name;

      function Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name)
         return Return_Type renames Marshallers.Void_Marshaller.Emit_By_Name;

      function Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Param  : Gtk.Notebook.Gtk_Notebook_Page)
         return Return_Type renames Notebook_Page_Marshaller.Emit_By_Name;

   private
      --  <doc_ignore>
      type Acc is access all Widget_Type'Class;
      --  This type has to be declared at library level, otherwise
      --  Program_Error might be raised when trying to cast from the
      --  parameter of Marshaller to another type.

      type User_Access is access User_Type;
      type Data_Type_Record is record
         Func   : Handler;
         --  User's callback

         Proxy  : Marshallers.Handler_Proxy := null;
         --  Handler_Proxy to use

         User   : User_Access := null;
         Object : Acc := null;
         --  Slot Object for Object_Connect
      end record;
      type Data_Type_Access is access all Data_Type_Record;
      pragma Convention (C, Data_Type_Access);
      --  Data passed to the C handler

      function Convert is new Unchecked_Conversion
        (Data_Type_Access, System.Address);
      function Convert is new Unchecked_Conversion
        (System.Address, Data_Type_Access);

      procedure Free_Data (Data : Data_Type_Access);
      pragma Convention (C, Free_Data);
      --  Free the memory associated with the callback's data

      procedure First_Marshaller
        (Closure         : GClosure;
         Return_Value    : Glib.Values.GValue;
         N_Params        : Guint;
         Params          : System.Address;
         Invocation_Hint : System.Address;
         User_Data       : System.Address);
      pragma Convention (C, First_Marshaller);
      --  First level marshaller. This is the function that is actually
      --  called by gtk+. It then calls the Ada functions as required.
      --  </doc_ignore>

   end User_Return_Callback;

   -------------------------------------
   -- User_Return_Callback_With_Setup --
   -------------------------------------
   --  This package is basically the same as User_Return_Callback, except that
   --  an extra function (Setup) is called after a handler has been
   --  connected. Typical usage is to automatically call Add_Watch (see below)
   --  in case the User_Type is (or contains) widgets.

   generic
      type Widget_Type is new Glib.Object.GObject_Record with private;
      type Return_Type is (<>);
      type User_Type (<>) is private;
      with procedure Setup (User_Data : User_Type; Id : Handler_Id);
   package User_Return_Callback_With_Setup is

      package Internal_Cb is new User_Return_Callback
        (Widget_Type, Return_Type, User_Type);

      subtype Handler is Internal_Cb.Handler;
      subtype Simple_Handler is Internal_Cb.Simple_Handler;
      package Marshallers renames Internal_Cb.Marshallers;

      --  Connecting a handler to an object

      procedure Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Marsh     : Marshallers.Marshaller;
         User_Data : User_Type;
         After     : Boolean := False);
      procedure Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Marsh       : Marshallers.Marshaller;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False);

      procedure Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Cb        : Handler;
         User_Data : User_Type;
         After     : Boolean := False);
      procedure Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Cb          : Handler;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False);

      procedure Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Cb        : Simple_Handler;
         User_Data : User_Type;
         After     : Boolean := False);
      procedure Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Cb          : Simple_Handler;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False);

      pragma Inline (Connect);

      function Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Marsh     : Marshallers.Marshaller;
         User_Data : User_Type;
         After     : Boolean := False) return Handler_Id;

      function Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Marsh       : Marshallers.Marshaller;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False) return Handler_Id;

      function Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Cb        : Handler;
         User_Data : User_Type;
         After     : Boolean := False) return Handler_Id;

      function Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Cb          : Handler;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False) return Handler_Id;

      --  Some convenient functions to create marshallers

      package Gint_Marshaller renames Internal_Cb.Gint_Marshaller;
      package Guint_Marshaller renames Internal_Cb.Guint_Marshaller;
      package Event_Marshaller renames Internal_Cb.Event_Marshaller;
      package Widget_Marshaller renames Internal_Cb.Widget_Marshaller;
      package Notebook_Page_Marshaller
        renames Internal_Cb.Notebook_Page_Marshaller;

      function To_Marshaller
        (Cb : Gint_Marshaller.Handler)
         return Internal_Cb.Marshallers.Marshaller
         renames Internal_Cb.To_Marshaller;
      function To_Marshaller
        (Cb : Guint_Marshaller.Handler)
         return Internal_Cb.Marshallers.Marshaller
         renames Internal_Cb.To_Marshaller;
      function To_Marshaller
        (Cb : Event_Marshaller.Handler)
         return Internal_Cb.Marshallers.Marshaller
         renames Internal_Cb.To_Marshaller;
      function To_Marshaller
        (Cb : Widget_Marshaller.Handler)
         return Internal_Cb.Marshallers.Marshaller
         renames Internal_Cb.To_Marshaller;
      function To_Marshaller
        (Cb : Internal_Cb.Marshallers.Void_Marshaller.Handler)
         return Internal_Cb.Marshallers.Marshaller
         renames Internal_Cb.To_Marshaller;
      function To_Marshaller
        (Cb : Notebook_Page_Marshaller.Handler)
         return Internal_Cb.Marshallers.Marshaller
         renames Internal_Cb.To_Marshaller;

      --  Emitting a signal

      function Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Param  : Gint) return Return_Type renames Internal_Cb.Emit_By_Name;

      function Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Param  : Guint) return Return_Type renames Internal_Cb.Emit_By_Name;

      function Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Param  : Gdk.Event.Gdk_Event) return Return_Type
         renames Internal_Cb.Emit_By_Name;

      function Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Param  : access Gtk.Widget.Gtk_Widget_Record'Class)
         return Return_Type renames Internal_Cb.Emit_By_Name;

      function Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name)
         return Return_Type renames Internal_Cb.Emit_By_Name;

      function Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Param  : Gtk.Notebook.Gtk_Notebook_Page)
         return Return_Type renames Internal_Cb.Emit_By_Name;

   end User_Return_Callback_With_Setup;

   ---------------------------------------------------------
   --  These handlers do not return a value
   --  They do not have a User_Data
   ---------------------------------------------------------

   generic
      type Widget_Type is new Glib.Object.GObject_Record with private;
   package Callback is

      type Handler is access procedure
        (Widget : access Widget_Type'Class;
         Params : Glib.Values.GValues);
      type Simple_Handler is access procedure
        (Widget : access Widget_Type'Class);

      package Marshallers is new
        Gtk.Marshallers.Void_Marshallers (Widget_Type);

      --  Connecting a handler to an object

      procedure Connect
        (Widget : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Marsh  : Marshallers.Marshaller;
         After  : Boolean := False);
      procedure Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Marsh       : Marshallers.Marshaller;
         Slot_Object : access Widget_Type'Class;
         After       : Boolean := False);

      procedure Connect
        (Widget : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Cb     : Handler;
         After  : Boolean := False);
      procedure Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Cb          : Handler;
         Slot_Object : access Widget_Type'Class;
         After       : Boolean := False);

      procedure Connect
        (Widget : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Cb     : Simple_Handler;
         After  : Boolean := False);
      procedure Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Cb          : Simple_Handler;
         Slot_Object : access Widget_Type'Class;
         After       : Boolean := False);

      pragma Inline (Connect);
      pragma Inline (Object_Connect);

      function Connect
        (Widget : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Marsh  : Marshallers.Marshaller;
         After  : Boolean := False) return Handler_Id;

      function Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Marsh       : Marshallers.Marshaller;
         Slot_Object : access Widget_Type'Class;
         After       : Boolean := False) return Handler_Id;

      function Connect
        (Widget : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Cb     : Handler;
         After  : Boolean := False) return Handler_Id;

      function Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Cb          : Handler;
         Slot_Object : access Widget_Type'Class;
         After       : Boolean := False) return Handler_Id;

      --  Some convenient functions to create marshallers

      package Gint_Marshaller is new Marshallers.Generic_Marshaller
        (Gint, Glib.Values.Get_Int);
      package Guint_Marshaller is new Marshallers.Generic_Marshaller
        (Guint, Glib.Values.Get_Uint);
      package Event_Marshaller is new Marshallers.Generic_Marshaller
        (Gdk.Event.Gdk_Event, Gdk.Event.Get_Event);
      package Widget_Marshaller is new Marshallers.Generic_Widget_Marshaller
        (Gtk.Widget.Gtk_Widget_Record, Gtk.Widget.Gtk_Widget);
      package Notebook_Page_Marshaller is new Marshallers.Generic_Marshaller
        (Gtk.Notebook.Gtk_Notebook_Page, Gtk.Notebook.Get_Notebook_Page);
      package Tree_Path_Marshaller is new Marshallers.Generic_Marshaller
        (Gtk.Tree_Model.Gtk_Tree_Path, Gtk.Tree_Model.Get_Tree_Path);
      package Tree_Iter_Tree_Path_Marshaller is
         new Marshallers.Generic_Marshaller_2
               (Gtk.Tree_Model.Gtk_Tree_Iter, Gtk.Tree_Model.Get_Tree_Iter,
                Gtk.Tree_Model.Gtk_Tree_Path, Gtk.Tree_Model.Get_Tree_Path);
      package Tree_Path_Tree_Iter_Marshaller is
         new Marshallers.Generic_Marshaller_2
               (Gtk.Tree_Model.Gtk_Tree_Path, Gtk.Tree_Model.Get_Tree_Path,
                Gtk.Tree_Model.Gtk_Tree_Iter, Gtk.Tree_Model.Get_Tree_Iter);

      function To_Marshaller
        (Cb : Gint_Marshaller.Handler)
         return Marshallers.Marshaller renames Gint_Marshaller.To_Marshaller;

      function To_Marshaller
        (Cb : Guint_Marshaller.Handler)
         return Marshallers.Marshaller renames Guint_Marshaller.To_Marshaller;

      function To_Marshaller
        (Cb : Event_Marshaller.Handler)
         return Marshallers.Marshaller renames Event_Marshaller.To_Marshaller;

      function To_Marshaller
        (Cb : Widget_Marshaller.Handler)
         return Marshallers.Marshaller renames Widget_Marshaller.To_Marshaller;

      function To_Marshaller
        (Cb : Marshallers.Void_Marshaller.Handler)
         return Marshallers.Marshaller
         renames Marshallers.Void_Marshaller.To_Marshaller;

      function To_Marshaller
        (Cb : Notebook_Page_Marshaller.Handler)
         return Marshallers.Marshaller
         renames Notebook_Page_Marshaller.To_Marshaller;

      function To_Marshaller
        (Cb : Tree_Path_Marshaller.Handler)
         return Marshallers.Marshaller
         renames Tree_Path_Marshaller.To_Marshaller;

      function To_Marshaller
        (Cb : Tree_Iter_Tree_Path_Marshaller.Handler)
         return Marshallers.Marshaller
         renames Tree_Iter_Tree_Path_Marshaller.To_Marshaller;

      function To_Marshaller
        (Cb : Tree_Path_Tree_Iter_Marshaller.Handler)
         return Marshallers.Marshaller
         renames Tree_Path_Tree_Iter_Marshaller.To_Marshaller;

      --  Emitting a signal

      procedure Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Param  : Gint) renames Gint_Marshaller.Emit_By_Name;

      procedure Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Param  : Guint) renames Guint_Marshaller.Emit_By_Name;

      procedure Emit_By_Name
         (Object : access Widget_Type'Class;
          Name   : Glib.Signal_Name;
          Param  : Gdk.Event.Gdk_Event);

      procedure Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Param  : access Gtk.Widget.Gtk_Widget_Record'Class)
         renames Widget_Marshaller.Emit_By_Name;

      procedure Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name)
         renames Marshallers.Void_Marshaller.Emit_By_Name;

      procedure Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Param  : Gtk.Notebook.Gtk_Notebook_Page)
         renames Notebook_Page_Marshaller.Emit_By_Name;

      procedure Emit_By_Name is
        new Tree_Path_Marshaller.Emit_By_Name_Generic
              (Gtk.Tree_Model.To_Address);

      procedure Emit_By_Name is
        new Tree_Iter_Tree_Path_Marshaller.Emit_By_Name_Generic
              (Gtk.Tree_Model.To_Address,
               Gtk.Tree_Model.To_Address);

      procedure Emit_By_Name is
        new Tree_Path_Tree_Iter_Marshaller.Emit_By_Name_Generic
              (Gtk.Tree_Model.To_Address,
               Gtk.Tree_Model.To_Address);

   private
      --  <doc_ignore>
      type Acc is access all Widget_Type'Class;
      --  This type has to be declared at library level, otherwise
      --  Program_Error might be raised when trying to cast from the
      --  parameter of Marshaller to another type.

      type Data_Type_Record is record
         Func   : Handler;             --  User's callback
         Proxy  : Marshallers.Handler_Proxy := null;  --  Handler_Proxy to use
         Object : Acc := null;         --  Slot Object for Object_Connect
      end record;
      type Data_Type_Access is access all Data_Type_Record;
      pragma Convention (C, Data_Type_Access);
      --  Data passed to the C handler

      function Convert is new Unchecked_Conversion
        (Data_Type_Access, System.Address);
      function Convert is new Unchecked_Conversion
        (System.Address, Data_Type_Access);

      procedure Free_Data (Data : Data_Type_Access);
      pragma Convention (C, Free_Data);
      --  Free the memory associated with the callback's data

      procedure First_Marshaller
        (Closure         : GClosure;
         Return_Value    : Glib.Values.GValue;
         N_Params        : Guint;
         Params          : System.Address;
         Invocation_Hint : System.Address;
         User_Data       : System.Address);
      pragma Convention (C, First_Marshaller);
      --  First level marshaller. This is the function that is actually
      --  called by gtk+. It then calls the Ada functions as required.
      --  </doc_ignore>

   end Callback;

   ---------------------------------------------------------
   --  These handlers do not return a value
   --  They require a User_Data
   --  See also the package User_Callback_With_Setup
   ---------------------------------------------------------

   generic
      type Widget_Type is new Glib.Object.GObject_Record with private;
      type User_Type (<>) is private;
   package User_Callback is

      type Handler is access procedure
        (Widget    : access Widget_Type'Class;
         Params    : Glib.Values.GValues;
         User_Data : User_Type);
      type Simple_Handler is access procedure
        (Widget    : access Widget_Type'Class;
         User_Data : User_Type);

      package Marshallers is new
        Gtk.Marshallers.User_Void_Marshallers (Widget_Type, User_Type);

      --  Connecting a handler to an object

      procedure Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Marsh     : Marshallers.Marshaller;
         User_Data : User_Type;
         After     : Boolean := False);
      procedure Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Marsh       : Marshallers.Marshaller;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False);

      procedure Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Cb        : Handler;
         User_Data : User_Type;
         After     : Boolean := False);
      procedure Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Cb          : Handler;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False);

      procedure Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Cb        : Simple_Handler;
         User_Data : User_Type;
         After     : Boolean := False);
      procedure Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Cb          : Simple_Handler;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False);

      pragma Inline (Connect);

      function Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Marsh     : Marshallers.Marshaller;
         User_Data : User_Type;
         After     : Boolean := False) return Handler_Id;

      function Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Marsh       : Marshallers.Marshaller;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False) return Handler_Id;

      function Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Cb        : Handler;
         User_Data : User_Type;
         After     : Boolean := False) return Handler_Id;

      function Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Cb          : Handler;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False) return Handler_Id;

      --  Some convenient functions to create marshallers

      package Gint_Marshaller is new Marshallers.Generic_Marshaller
        (Gint, Glib.Values.Get_Int);
      package Guint_Marshaller is new Marshallers.Generic_Marshaller
        (Guint, Glib.Values.Get_Uint);
      package Event_Marshaller is new Marshallers.Generic_Marshaller
        (Gdk.Event.Gdk_Event, Gdk.Event.Get_Event);
      package Widget_Marshaller is new Marshallers.Generic_Widget_Marshaller
        (Gtk.Widget.Gtk_Widget_Record, Gtk.Widget.Gtk_Widget);
      package Notebook_Page_Marshaller is new Marshallers.Generic_Marshaller
        (Gtk.Notebook.Gtk_Notebook_Page, Gtk.Notebook.Get_Notebook_Page);
      package Tree_Path_Marshaller is new Marshallers.Generic_Marshaller
        (Gtk.Tree_Model.Gtk_Tree_Path, Gtk.Tree_Model.Get_Tree_Path);
      package Tree_Iter_Tree_Path_Marshaller is
         new Marshallers.Generic_Marshaller_2
               (Gtk.Tree_Model.Gtk_Tree_Iter, Gtk.Tree_Model.Get_Tree_Iter,
                Gtk.Tree_Model.Gtk_Tree_Path, Gtk.Tree_Model.Get_Tree_Path);
      package Tree_Path_Tree_Iter_Marshaller is
         new Marshallers.Generic_Marshaller_2
               (Gtk.Tree_Model.Gtk_Tree_Path, Gtk.Tree_Model.Get_Tree_Path,
                Gtk.Tree_Model.Gtk_Tree_Iter, Gtk.Tree_Model.Get_Tree_Iter);

      function To_Marshaller
        (Cb : Gint_Marshaller.Handler)
         return Marshallers.Marshaller renames Gint_Marshaller.To_Marshaller;

      function To_Marshaller
        (Cb : Guint_Marshaller.Handler)
         return Marshallers.Marshaller renames Guint_Marshaller.To_Marshaller;

      function To_Marshaller
        (Cb : Event_Marshaller.Handler)
         return Marshallers.Marshaller renames Event_Marshaller.To_Marshaller;

      function To_Marshaller
        (Cb : Widget_Marshaller.Handler)
         return Marshallers.Marshaller renames Widget_Marshaller.To_Marshaller;

      function To_Marshaller
        (Cb : Marshallers.Void_Marshaller.Handler)
         return Marshallers.Marshaller
         renames Marshallers.Void_Marshaller.To_Marshaller;

      function To_Marshaller
        (Cb : Notebook_Page_Marshaller.Handler)
         return Marshallers.Marshaller
         renames Notebook_Page_Marshaller.To_Marshaller;

      function To_Marshaller
        (Cb : Tree_Path_Marshaller.Handler)
         return Marshallers.Marshaller
         renames Tree_Path_Marshaller.To_Marshaller;

      function To_Marshaller
        (Cb : Tree_Iter_Tree_Path_Marshaller.Handler)
         return Marshallers.Marshaller
         renames Tree_Iter_Tree_Path_Marshaller.To_Marshaller;

      function To_Marshaller
        (Cb : Tree_Path_Tree_Iter_Marshaller.Handler)
         return Marshallers.Marshaller
         renames Tree_Path_Tree_Iter_Marshaller.To_Marshaller;

      --  Emitting a signal

      procedure Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Param  : Gint) renames Gint_Marshaller.Emit_By_Name;

      procedure Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Param  : Guint) renames Guint_Marshaller.Emit_By_Name;

      procedure Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Param  : Gdk.Event.Gdk_Event);

      procedure Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Param  : access Gtk.Widget.Gtk_Widget_Record'Class)
         renames Widget_Marshaller.Emit_By_Name;

      procedure Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name)
         renames Marshallers.Void_Marshaller.Emit_By_Name;

      procedure Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Param  : Gtk.Notebook.Gtk_Notebook_Page)
         renames Notebook_Page_Marshaller.Emit_By_Name;

      procedure Emit_By_Name is
        new Tree_Path_Marshaller.Emit_By_Name_Generic
              (Gtk.Tree_Model.To_Address);

      procedure Emit_By_Name is
        new Tree_Iter_Tree_Path_Marshaller.Emit_By_Name_Generic
              (Gtk.Tree_Model.To_Address,
               Gtk.Tree_Model.To_Address);

      procedure Emit_By_Name is
        new Tree_Path_Tree_Iter_Marshaller.Emit_By_Name_Generic
              (Gtk.Tree_Model.To_Address,
               Gtk.Tree_Model.To_Address);

   private
      --  <doc_ignore>
      type Acc is access all Widget_Type'Class;
      --  This type has to be declared at library level, otherwise
      --  Program_Error might be raised when trying to cast from the
      --  parameter of Marshaller to another type.

      type User_Access is access User_Type;
      type Data_Type_Record is record
         Func   : Handler;
         --  User's callback

         Proxy  : Marshallers.Handler_Proxy := null;
         --  Handler_Proxy to use

         User   : User_Access := null;
         Object : Acc := null;
         --  Slot_Object for Object_Connect
      end record;
      type Data_Type_Access is access all Data_Type_Record;
      pragma Convention (C, Data_Type_Access);
      --  Data passed to the C handler

      function Convert is new Unchecked_Conversion
        (Data_Type_Access, System.Address);
      function Convert is new Unchecked_Conversion
        (System.Address, Data_Type_Access);

      procedure Free_Data (Data : Data_Type_Access);
      pragma Convention (C, Free_Data);
      --  Free the memory associated with the callback's data

      procedure First_Marshaller
        (Closure         : GClosure;
         Return_Value    : Glib.Values.GValue;
         N_Params        : Guint;
         Params          : System.Address;
         Invocation_Hint : System.Address;
         User_Data       : System.Address);
      pragma Convention (C, First_Marshaller);
      --  First level marshaller. This is the function that is actually
      --  called by gtk+. It then calls the Ada functions as required.
      --  </doc_ignore>

   end User_Callback;

   ------------------------------
   -- User_Callback_With_Setup --
   ------------------------------
   --  This package is basically the same as User_Callback, except that an
   --  extra function (Setup) is called after a handler has been
   --  connected. Typical usage is to automatically call Add_Watch (see below)
   --  in case the User_Type is (or contains) widgets.

   generic
      type Widget_Type is new Glib.Object.GObject_Record with private;
      type User_Type (<>) is private;
      with procedure Setup (User_Data : User_Type; Id : Handler_Id);
   package User_Callback_With_Setup is

      package Internal_Cb is new User_Callback (Widget_Type, User_Type);
      package Marshallers renames Internal_Cb.Marshallers;

      subtype Handler is Internal_Cb.Handler;
      subtype Simple_Handler is Internal_Cb.Simple_Handler;

      --  Connecting a handler to an object

      procedure Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Marsh     : Marshallers.Marshaller;
         User_Data : User_Type;
         After     : Boolean := False);
      procedure Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Marsh       : Marshallers.Marshaller;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False);

      procedure Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Cb        : Handler;
         User_Data : User_Type;
         After     : Boolean := False);
      procedure Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Cb          : Handler;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False);

      procedure Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Cb        : Simple_Handler;
         User_Data : User_Type;
         After     : Boolean := False);
      procedure Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Cb          : Simple_Handler;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False);

      pragma Inline (Connect);

      function Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Marsh     : Marshallers.Marshaller;
         User_Data : User_Type;
         After     : Boolean := False) return Handler_Id;

      function Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Marsh       : Marshallers.Marshaller;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False) return Handler_Id;

      function Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Cb        : Handler;
         User_Data : User_Type;
         After     : Boolean := False) return Handler_Id;

      function Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Cb          : Handler;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False) return Handler_Id;

      --  Some convenient functions to create marshallers

      package Gint_Marshaller renames Internal_Cb.Gint_Marshaller;
      package Guint_Marshaller renames Internal_Cb.Guint_Marshaller;
      package Event_Marshaller renames Internal_Cb.Event_Marshaller;
      package Widget_Marshaller renames Internal_Cb.Widget_Marshaller;
      package Notebook_Page_Marshaller
        renames Internal_Cb.Notebook_Page_Marshaller;

      function To_Marshaller
        (Cb : Gint_Marshaller.Handler)
         return Internal_Cb.Marshallers.Marshaller
         renames Internal_Cb.To_Marshaller;
      function To_Marshaller
        (Cb : Guint_Marshaller.Handler)
         return Internal_Cb.Marshallers.Marshaller
         renames Internal_Cb.To_Marshaller;
      function To_Marshaller
        (Cb : Event_Marshaller.Handler)
         return Internal_Cb.Marshallers.Marshaller
         renames Internal_Cb.To_Marshaller;
      function To_Marshaller
        (Cb : Widget_Marshaller.Handler)
         return Internal_Cb.Marshallers.Marshaller
         renames Internal_Cb.To_Marshaller;
      function To_Marshaller
        (Cb : Internal_Cb.Marshallers.Void_Marshaller.Handler)
         return Internal_Cb.Marshallers.Marshaller
         renames Internal_Cb.To_Marshaller;
      function To_Marshaller
        (Cb : Notebook_Page_Marshaller.Handler)
         return Internal_Cb.Marshallers.Marshaller
         renames Internal_Cb.To_Marshaller;

      --  Emitting a signal

      procedure Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Param  : Gint) renames Internal_Cb.Emit_By_Name;

      procedure Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Param  : Guint) renames Internal_Cb.Emit_By_Name;

      procedure Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Param  : Gdk.Event.Gdk_Event) renames Internal_Cb.Emit_By_Name;

      procedure Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Param  : access Gtk.Widget.Gtk_Widget_Record'Class)
         renames Internal_Cb.Emit_By_Name;

      procedure Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name) renames Internal_Cb.Emit_By_Name;

      procedure Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Param  : Gtk.Notebook.Gtk_Notebook_Page)
         renames Internal_Cb.Emit_By_Name;

   end User_Callback_With_Setup;

   ------------------------------------------------------------------
   --  General functions
   ------------------------------------------------------------------

   procedure Add_Watch
     (Id : Handler_Id; Object : access Glib.Object.GObject_Record'Class);
   --  Make sure that when Object is destroyed, the handler Id is also
   --  destroyed. This function should mostly be used in cases where you use a
   --  User_Data that is Object. If you don't destroy the callback at the same
   --  time, then the next time the callback is called it will try to access
   --  some invalid memory (Object being destroyed), and you will likely get a
   --  Storage_Error.

   procedure Disconnect
     (Object : access Glib.Object.GObject_Record'Class;
      Id     : in out Handler_Id);
   --  Disconnect the handler identified by the given Handler_Id.

   procedure Emit_Stop_By_Name
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : Glib.Signal_Name);
   --  During a signal emission, invoking this procedure will halt the
   --  emission.

   procedure Handler_Block
     (Obj : access Glib.Object.GObject_Record'Class;
      Id  : Handler_Id);
   --  Blocks temporily the signal. For each call to this procedure,
   --  a call to Handler_Unblock must be performed in order to really
   --  unblock the signal.

   procedure Handlers_Destroy
     (Obj : access Glib.Object.GObject_Record'Class);
   --  Destroys all the handlers associated to the given object.

   procedure Handler_Unblock
     (Obj : access Glib.Object.GObject_Record'Class;
      Id  : Handler_Id);
   --  See Handler_Block.

   --  </doc_ignore>

end Gtk.Handlers;

--  <example>
--  --  This example connects the "delete_event" signal to a widget.
--  --  The handlers for this signal get an extra argument which is
--  --  the Gdk_Event that generated the signal.
--
--  with Gtk.Handlers;    use Gtk.Handlers;
--  with Gtk.Marshallers; use Gtk.Marshallers;
--
--  function My_Cb (Widget : access Gtk_Widget_Record'Class;
--                  Event  : Gdk.Event.Gdk_Event)
--                  return Gint;
--  --  your own function
--
--  package Return_Widget_Cb is new Gtk.Handlers.Return_Callback
--     (Gtk.Widget.Gtk_Widget_Record, Gint);
--
--  Return_Widget_Cb.Connect (W, "delete_event",
--     Return_Widget_Cb.To_Marshaller (My_Cb'Access));
--
--  </example>
