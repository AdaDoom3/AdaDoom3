-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                 Copyright (C) 2000-2013, AdaCore                   --
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
with System.Assertions;       use System.Assertions;
with Unchecked_Deallocation;

with Glib.Values;             use Glib.Values;

package body Gtk.Handlers is

   function Count_Arguments
     (Object : access GObject_Record'Class; Signal : Glib.Signal_Name)
      return Guint;
   --  Convenience function that returns the number of arguments for this
   --  signal

   function Do_Signal_Connect
     (Object              : Glib.Object.GObject;
      Name                : Glib.Signal_Name;
      Marshaller          : System.Address;
      Handler             : System.Address;
      Func_Data           : System.Address;
      Destroy             : System.Address;
      After               : Boolean;
      Slot_Object         : System.Address := System.Null_Address;
      Expect_Return_Value : Boolean) return Handler_Id;
   --  Internal function used to connect the signal.
   --  Expect_Return_Value should be true if the user is connecting a function
   --  to the signal, False if he is connecting a procedure

   function G_Signal_Parse_Name
     (Detailed_Signal    : Glib.Signal_Name;
      Itype              : GType;
      Signal_Id_P        : access Signal_Id;
      Detail_P           : access GQuark;
      Force_Detail_Quark : Gboolean) return Gboolean;
   pragma Import (C, G_Signal_Parse_Name, "g_signal_parse_name");
   --  Internal function to parse a signal name into its signal_id and
   --  detail quark.
   --  Detailed_Signal is a string of the form "signal-name::detail".
   --  Itype is the interface or instance that introduce "signal-name".
   --  Force_Detail_Quark, if True, forces the creation of a GQuark for the
   --     detail.
   --  This function returns true if the signal name could successfully be
   --  parsed, and Signal_Id_P and Detail_P contain valid return values.

   procedure Disconnect_Internal (Obj : System.Address; Id : Gulong);
   pragma Import (C, Disconnect_Internal, "g_signal_handler_disconnect");
   --  Internal version of Disconnect

   function Signal_Lookup
     (Name : Glib.Signal_Name; IType : GType) return Signal_Id;
   pragma Import (C, Signal_Lookup, "g_signal_lookup");

   procedure Set_Value (Value : GValue; Val : System.Address);
   pragma Import (C, Set_Value, "ada_gvalue_set");
   --  Function used internally to specify the value returned by a callback.

   --------------------------------
   -- Glib.Closure small binding --
   --------------------------------

   function CClosure_New
     (Callback  : System.Address;
      User_Data : System.Address;
      Destroy   : System.Address) return GClosure;
   pragma Import (C, CClosure_New, "g_cclosure_new");

   procedure Set_Marshal (Closure : GClosure; Marshaller : System.Address);
   pragma Import (C, Set_Marshal, "g_closure_set_marshal");

   function Get_Data (Closure : GClosure) return System.Address;
   pragma Import (C, Get_Data, "ada_gclosure_get_data");

   procedure Watch_Closure (Object : System.Address; Closure : GClosure);
   pragma Import (C, Watch_Closure, "g_object_watch_closure");
   --  The closure will be destroyed when Object is destroyed.

   ---------------------
   -- Count_Arguments --
   ---------------------

   function Count_Arguments
     (Object : access GObject_Record'Class; Signal : Glib.Signal_Name)
      return Guint
   is
      Q  : Signal_Query;
      Id : constant Signal_Id :=
        Lookup (Get_Type (Object), String (Signal) & ASCII.NUL);

   begin
      if Id = Invalid_Signal_Id then
         return 0;
      else
         Query (Id, Q);
         return Params (Q)'Length;
      end if;
   end Count_Arguments;

   -----------------------
   -- Do_Signal_Connect --
   -----------------------

   function Do_Signal_Connect
     (Object              : Glib.Object.GObject;
      Name                : Glib.Signal_Name;
      Marshaller          : System.Address;
      Handler             : System.Address;
      Func_Data           : System.Address;
      Destroy             : System.Address;
      After               : Boolean;
      Slot_Object         : System.Address := System.Null_Address;
      Expect_Return_Value : Boolean) return Handler_Id
   is
      function Internal
        (Instance : System.Address;
         Id       : Signal_Id;
         Detail   : GQuark;
         Closure  : GClosure;
         After    : Gint := 0) return Gulong;
      pragma Import (C, Internal, "g_signal_connect_closure_by_id");

      use type System.Address;
      Id      : Handler_Id;
      Signal  : aliased Signal_Id;
      Detail  : aliased GQuark;
      Success : Gboolean;
      Q       : Signal_Query;

   begin
      --  When the handler is destroyed, for instance because Object is
      --  destroyed, then the closure is destroyed as well, and Destroy gets
      --  called.
      --  The closure is invoked when the signal is emitted. As a result,
      --  Handler is called, with Func_Data as a parameter.

      Success := G_Signal_Parse_Name
        (Detailed_Signal    => Name & ASCII.NUL,
         Itype              => Get_Type (Object),
         Signal_Id_P        => Signal'Access,
         Detail_P           => Detail'Access,
         Force_Detail_Quark => 0);

      if Success = 0 or else Signal = Invalid_Signal_Id then
         Raise_Assert_Failure
           ("Trying to connect to unknown signal (""" & String (Name)
            & """) on type " & Type_Name (Get_Type (Object)));
      end if;

      Query (Signal, Q);
      if Expect_Return_Value then
         if Return_Type (Q) = GType_None then
            Raise_Assert_Failure
              ("Handlers for """ & String (Name) & """ on a "
               & Type_Name (Get_Type (Object))
               & " should be procedures");
         end if;

      else
         if Return_Type (Q) /= GType_None then
            Raise_Assert_Failure
              ("Handlers for """ & String (Name) & """ on a "
               & Type_Name (Get_Type (Object))
               & " should be functions");
         end if;
      end if;

      Id.Closure := CClosure_New (Handler, Func_Data, Destroy);
      Set_Marshal (Id.Closure, Marshaller);

      Id.Id := Internal
        (Get_Object (Object),
         Id      => Signal,
         Detail  => Detail,
         Closure => Id.Closure,
         After   => Boolean'Pos (After));

      --  If we have connected with Object_Connect, we want to automatically
      --  disconnect the signal as well.

      if Slot_Object /= System.Null_Address then
         Watch_Closure (Slot_Object, Id.Closure);
      end if;

      return Id;
   end Do_Signal_Connect;

   ---------------
   -- Add_Watch --
   ---------------

   procedure Add_Watch
     (Id : Handler_Id; Object : access Glib.Object.GObject_Record'Class) is
   begin
      pragma Assert (Id.Closure /= null);
      Watch_Closure (Get_Object (Object), Id.Closure);
   end Add_Watch;

   ---------------------
   -- Return_Callback --
   ---------------------

   package body Return_Callback is

      function To_Handler is new Unchecked_Conversion
        (Gtk.Marshallers.General_Handler, Handler);
      function To_General_Handler is new Unchecked_Conversion
        (Handler, Gtk.Marshallers.General_Handler);
      function To_Address is new Unchecked_Conversion
        (Handler, System.Address);
      function To_Address is new Unchecked_Conversion
        (Marshallers.Handler_Proxy, System.Address);

      -------------
      -- Connect --
      -------------

      procedure Connect
        (Widget : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Marsh  : Marshallers.Marshaller;
         After  : Boolean := False)
      is
         Id : Handler_Id;
         pragma Warnings (Off, Id);
      begin
         Id := Connect (Widget, Name, Marsh, After);
      end Connect;

      --------------------
      -- Object_Connect --
      --------------------

      procedure Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Marsh       : Marshallers.Marshaller;
         Slot_Object : access Widget_Type'Class;
         After       : Boolean := False)
      is
         Id : Handler_Id;
         pragma Warnings (Off, Id);
      begin
         Id := Object_Connect (Widget, Name, Marsh, Slot_Object, After);
      end Object_Connect;

      -------------
      -- Connect --
      -------------

      procedure Connect
        (Widget : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Cb     : Handler;
         After  : Boolean := False)
      is
         Id : Handler_Id;
         pragma Warnings (Off, Id);
      begin
         Id := Connect (Widget, Name, Cb, After);
      end Connect;

      --------------------
      -- Object_Connect --
      --------------------

      procedure Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Cb          : Handler;
         Slot_Object : access Widget_Type'Class;
         After       : Boolean := False)
      is
         Id : Handler_Id;
         pragma Warnings (Off, Id);
      begin
         Id := Object_Connect (Widget, Name, Cb, Slot_Object, After);
      end Object_Connect;

      -------------
      -- Connect --
      -------------

      procedure Connect
        (Widget : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Cb     : Simple_Handler;
         After  : Boolean := False)
      is
         Id : Handler_Id;
         pragma Warnings (Off, Id);
      begin
         Id := Connect
           (Widget, Name,
            To_Marshaller (Marshallers.Void_Marshaller.Handler (Cb)),
            After);
      end Connect;

      --------------------
      -- Object_Connect --
      --------------------

      procedure Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Cb          : Simple_Handler;
         Slot_Object : access Widget_Type'Class;
         After       : Boolean := False)
      is
         Id : Handler_Id;
         pragma Warnings (Off, Id);
      begin
         Id := Object_Connect
           (Widget, Name,
            To_Marshaller (Marshallers.Void_Marshaller.Handler (Cb)),
            Slot_Object, After);
      end Object_Connect;

      ---------------
      -- Free_Data --
      ---------------

      procedure Free_Data (Data : Data_Type_Access) is
         procedure Internal is new Unchecked_Deallocation
           (Data_Type_Record, Data_Type_Access);
         D : Data_Type_Access := Data;

      begin
         Internal (D);
      end Free_Data;

      ----------------------
      -- First_Marshaller --
      ----------------------

      procedure First_Marshaller
        (Closure         : GClosure;
         Return_Value    : GValue;
         N_Params        : Guint;
         Params          : System.Address;
         Invocation_Hint : System.Address;
         User_Data       : System.Address)
      is
         pragma Unreferenced (Invocation_Hint, User_Data);

         use type Marshallers.Handler_Proxy;

         Data   : constant Data_Type_Access := Convert (Get_Data (Closure));
         Stub   : Widget_Type;
         pragma Warnings (Off, Stub);

         Value  : aliased Return_Type;
         Values : GValues;

      begin
         if Data.Func = null then
            return;
         end if;

         Values := Make_Values (N_Params, Params);

         if Data.Object = null then
            if Data.Proxy /= null then
               Value :=
                 Data.Proxy
                   (Acc (Get_User_Data (Get_Address (Nth (Values, 0)), Stub)),
                    Values, To_General_Handler (Data.Func));
            else
               Value :=
                 Data.Func
                   (Acc (Get_User_Data (Get_Address (Nth (Values, 0)), Stub)),
                    Values);
            end if;
         else
            if Data.Proxy /= null then
               Value :=
                 Data.Proxy
                   (Data.Object, Values, To_General_Handler (Data.Func));
            else
               Value := Data.Func (Data.Object, Values);
            end if;
         end if;

         Set_Value (Return_Value, Value'Address);
      end First_Marshaller;

      -------------
      -- Connect --
      -------------

      function Connect
        (Widget  : access Widget_Type'Class;
         Name    : Glib.Signal_Name;
         Marsh   : Marshallers.Marshaller;
         After   : Boolean := False) return Handler_Id
      is
         D : constant Data_Type_Access :=
           new Data_Type_Record'
             (Func     => To_Handler (Marsh.Func),
              Proxy    => Marsh.Proxy,
              Object   => null);
      begin
         return Do_Signal_Connect
           (Glib.Object.GObject (Widget),
            Name,
            First_Marshaller'Address,
            To_Address (Marsh.Proxy),
            Convert (D),
            Free_Data'Address,
            After,
            Expect_Return_Value => True);
      end Connect;

      --------------------
      -- Object_Connect --
      --------------------

      function Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Marsh       : Marshallers.Marshaller;
         Slot_Object : access Widget_Type'Class;
         After       : Boolean := False) return Handler_Id
      is
         D : constant Data_Type_Access :=
           new Data_Type_Record'
             (Func     => To_Handler (Marsh.Func),
              Proxy    => Marsh.Proxy,
              Object   => Acc (Slot_Object));

      begin
         return Do_Signal_Connect
           (Glib.Object.GObject (Widget),
            Name,
            First_Marshaller'Address,
            To_Address (Marsh.Proxy),
            Convert (D),
            Free_Data'Address,
            After,
            Get_Object (Slot_Object),
            Expect_Return_Value => True);
      end Object_Connect;

      -------------
      -- Connect --
      -------------

      function Connect
        (Widget  : access Widget_Type'Class;
         Name    : Glib.Signal_Name;
         Cb      : Handler;
         After   : Boolean := False) return Handler_Id
      is
         D : constant Data_Type_Access :=
           new Data_Type_Record'
             (Func     => Cb,
              Proxy    => null,
              Object   => null);

      begin
         return Do_Signal_Connect
           (Glib.Object.GObject (Widget),
            Name,
            First_Marshaller'Address,
            To_Address (Cb),
            Convert (D),
            Free_Data'Address,
            After,
            Expect_Return_Value => True);
      end Connect;

      --------------------
      -- Object_Connect --
      --------------------

      function Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Cb          : Handler;
         Slot_Object : access Widget_Type'Class;
         After       : Boolean := False) return Handler_Id
      is
         D : constant Data_Type_Access :=
           new Data_Type_Record'
             (Func     => Cb,
              Proxy    => null,
              Object   => Acc (Slot_Object));

      begin
         return Do_Signal_Connect
           (Glib.Object.GObject (Widget),
            Name,
            First_Marshaller'Address,
            To_Address (Cb),
            Convert (D),
            Free_Data'Address,
            After,
            Get_Object (Slot_Object),
            Expect_Return_Value => True);
      end Object_Connect;

      ------------------
      -- Emit_By_Name --
      ------------------

      function Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Param  : Gdk.Event.Gdk_Event) return Return_Type
      is
         procedure Internal
           (Object : System.Address;
            Name   : Glib.Signal_Name;
            Param  : System.Address;
            Ret    : out Gint);
         pragma Import (C, Internal, "ada_g_signal_emit_by_name_ptr_ptr");

         B : Gint;

      begin
         pragma Assert (Count_Arguments (Object, Name) = 1);
         Internal
           (Get_Object (Object), Name & ASCII.NUL,
            Gdk.Event.To_Address (Param), B);
         return Return_Type'Val (B);
      end Emit_By_Name;

   end Return_Callback;

   --------------------------
   -- User_Return_Callback --
   --------------------------

   package body User_Return_Callback is

      function To_Handler is new Unchecked_Conversion
        (Gtk.Marshallers.General_Handler, Handler);
      function To_General_Handler is new Unchecked_Conversion
        (Handler, Gtk.Marshallers.General_Handler);
      function To_Address is new Unchecked_Conversion
        (Handler, System.Address);
      function To_Address is new Unchecked_Conversion
        (Marshallers.Handler_Proxy, System.Address);

      ---------------
      -- Free_Data --
      ---------------

      procedure Free_Data (Data : Data_Type_Access) is
         procedure Internal is new Unchecked_Deallocation
           (Data_Type_Record, Data_Type_Access);
         procedure Internal2 is new Unchecked_Deallocation
           (User_Type, User_Access);
         D : Data_Type_Access := Data;

      begin
         Internal2 (D.User);
         Internal (D);
      end Free_Data;

      ----------------------
      -- First_Marshaller --
      ----------------------

      procedure First_Marshaller
        (Closure         : GClosure;
         Return_Value    : GValue;
         N_Params        : Guint;
         Params          : System.Address;
         Invocation_Hint : System.Address;
         User_Data       : System.Address)
      is
         pragma Unreferenced (Invocation_Hint, User_Data);

         use type Marshallers.Handler_Proxy;

         Data   : constant Data_Type_Access := Convert (Get_Data (Closure));
         Stub   : Widget_Type;
         pragma Warnings (Off, Stub);
         Value  : aliased Return_Type;
         Values : GValues;

      begin
         if Data.Func = null then
            return;
         end if;

         Values := Make_Values (N_Params, Params);

         if Data.Object /= null then
            if Data.Proxy /= null then
               Value := Data.Proxy
                 (Data.Object, Values, To_General_Handler (Data.Func),
                  Data.User.all);
            else
               Value := Data.Func (Data.Object, Values, Data.User.all);
            end if;

         elsif Data.Proxy /= null then
            Value := Data.Proxy
              (Acc (Get_User_Data (Get_Address (Nth (Values, 0)), Stub)),
               Values, To_General_Handler (Data.Func), Data.User.all);
         else
            Value := Data.Func
              (Acc (Get_User_Data (Get_Address (Nth (Values, 0)), Stub)),
               Values, Data.User.all);
         end if;

         Set_Value (Return_Value, Value'Address);
      end First_Marshaller;

      -------------
      -- Connect --
      -------------

      procedure Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Marsh     : Marshallers.Marshaller;
         User_Data : User_Type;
         After     : Boolean := False)
      is
         Id : Handler_Id;
         pragma Warnings (Off, Id);
      begin
         Id := Connect (Widget, Name, Marsh, User_Data, After);
      end Connect;

      --------------------
      -- Object_Connect --
      --------------------

      procedure Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Marsh       : Marshallers.Marshaller;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False)
      is
         Id : Handler_Id;
         pragma Warnings (Off, Id);
      begin
         Id := Object_Connect
           (Widget, Name, Marsh, Slot_Object, User_Data, After);
      end Object_Connect;

      -------------
      -- Connect --
      -------------

      procedure Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Cb        : Handler;
         User_Data : User_Type;
         After     : Boolean := False)
      is
         Id : Handler_Id;
         pragma Warnings (Off, Id);
      begin
         Id := Connect (Widget, Name, Cb, User_Data, After);
      end Connect;

      --------------------
      -- Object_Connect --
      --------------------

      procedure Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Cb          : Handler;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False)
      is
         Id : Handler_Id;
         pragma Warnings (Off, Id);
      begin
         Id := Object_Connect
           (Widget, Name, Cb, Slot_Object, User_Data, After);
      end Object_Connect;

      -------------
      -- Connect --
      -------------

      procedure Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Cb        : Simple_Handler;
         User_Data : User_Type;
         After     : Boolean := False)
      is
         Id : Handler_Id;
         pragma Warnings (Off, Id);
      begin
         Id := Connect
           (Widget, Name,
            To_Marshaller (Marshallers.Void_Marshaller.Handler (Cb)),
            User_Data, After);
      end Connect;

      --------------------
      -- Object_Connect --
      --------------------

      procedure Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Cb          : Simple_Handler;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False)
      is
         Id : Handler_Id;
         pragma Warnings (Off, Id);
      begin
         Id := Object_Connect
           (Widget, Name,
            To_Marshaller (Marshallers.Void_Marshaller.Handler (Cb)),
            Slot_Object, User_Data, After);
      end Object_Connect;

      -------------
      -- Connect --
      -------------

      function Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Marsh     : Marshallers.Marshaller;
         User_Data : User_Type;
         After     : Boolean := False)
        return Handler_Id
      is
         D : constant Data_Type_Access := new Data_Type_Record'
           (Func     => To_Handler (Marsh.Func),
            Proxy    => Marsh.Proxy,
            User     => new User_Type'(User_Data),
            Object   => null);
      begin
         return Do_Signal_Connect
           (Glib.Object.GObject (Widget),
            Name,
            First_Marshaller'Address,
            To_Address (Marsh.Proxy),
            Convert (D),
            Free_Data'Address,
            After,
            Expect_Return_Value => True);
      end Connect;

      --------------------
      -- Object_Connect --
      --------------------

      function Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Marsh       : Marshallers.Marshaller;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False) return Handler_Id
      is
         D : constant Data_Type_Access := new Data_Type_Record'
           (Func     => To_Handler (Marsh.Func),
            Proxy    => Marsh.Proxy,
            User     => new User_Type'(User_Data),
            Object   => Acc (Slot_Object));
      begin
         return Do_Signal_Connect
           (Glib.Object.GObject (Widget),
            Name,
            First_Marshaller'Address,
            To_Address (Marsh.Proxy),
            Convert (D),
            Free_Data'Address,
            After,
            Get_Object (Slot_Object),
            Expect_Return_Value => True);
      end Object_Connect;

      -------------
      -- Connect --
      -------------

      function Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Cb        : Handler;
         User_Data : User_Type;
         After     : Boolean := False) return Handler_Id
      is
         D : constant Data_Type_Access := new Data_Type_Record'
           (Func     => Cb,
            Proxy    => null,
            User     => new User_Type'(User_Data),
            Object   => null);
      begin
         return Do_Signal_Connect
           (Glib.Object.GObject (Widget),
            Name,
            First_Marshaller'Address,
            To_Address (Cb),
            Convert (D),
            Free_Data'Address,
            After,
            Expect_Return_Value => True);
      end Connect;

      --------------------
      -- Object_Connect --
      --------------------

      function Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Cb          : Handler;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False) return Handler_Id
      is
         D : constant Data_Type_Access := new Data_Type_Record'
           (Func     => Cb,
            Proxy    => null,
            User     => new User_Type'(User_Data),
            Object   => Acc (Slot_Object));
      begin
         return Do_Signal_Connect
           (Glib.Object.GObject (Widget),
            Name,
            First_Marshaller'Address,
            To_Address (Cb),
            Convert (D),
            Free_Data'Address,
            After,
            Get_Object (Slot_Object),
            Expect_Return_Value => True);
      end Object_Connect;

      ------------------
      -- Emit_By_Name --
      ------------------

      function Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Param  : Gdk.Event.Gdk_Event) return Return_Type
      is
         procedure Internal
           (Object : System.Address;
            Name   : Glib.Signal_Name;
            Param  : System.Address;
            Ret    : out Gint);
         pragma Import (C, Internal, "ada_g_signal_emit_by_name_ptr_ptr");

         B : Gint;

      begin
         pragma Assert (Count_Arguments (Object, Name) = 1);
         Internal
           (Get_Object (Object), Name & ASCII.NUL,
            Gdk.Event.To_Address (Param), B);
         return Return_Type'Val (B);
      end Emit_By_Name;

   end User_Return_Callback;

   --------------
   -- Callback --
   --------------

   package body Callback is

      function To_Handler is new Unchecked_Conversion
        (Gtk.Marshallers.General_Handler, Handler);
      function To_General_Handler is new Unchecked_Conversion
        (Handler, Gtk.Marshallers.General_Handler);
      function To_Address is new Unchecked_Conversion
        (Handler, System.Address);
      function To_Address is new Unchecked_Conversion
        (Marshallers.Handler_Proxy, System.Address);

      ---------------
      -- Free_Data --
      ---------------

      procedure Free_Data (Data : Data_Type_Access) is
         procedure Internal is new Unchecked_Deallocation
           (Data_Type_Record, Data_Type_Access);
         D : Data_Type_Access := Data;

      begin
         Internal (D);
      end Free_Data;

      ----------------------
      -- First_Marshaller --
      ----------------------

      procedure First_Marshaller
        (Closure         : GClosure;
         Return_Value    : GValue;
         N_Params        : Guint;
         Params          : System.Address;
         Invocation_Hint : System.Address;
         User_Data       : System.Address)
      is
         pragma Unreferenced (Invocation_Hint, User_Data, Return_Value);

         use type Marshallers.Handler_Proxy;

         Data   : constant Data_Type_Access := Convert (Get_Data (Closure));
         Stub   : Widget_Type;
         pragma Warnings (Off, Stub);

         Values : GValues;

      begin
         if Data.Func = null then
            return;
         end if;

         Values := Make_Values (N_Params, Params);

         if Data.Object = null then
            if Data.Proxy /= null then
               Data.Proxy
                 (Acc (Get_User_Data (Get_Address (Nth (Values, 0)), Stub)),
                  Values, To_General_Handler (Data.Func));
            else
               Data.Func
                 (Acc (Get_User_Data (Get_Address (Nth (Values, 0)), Stub)),
                  Values);
            end if;
         else
            if Data.Proxy /= null then
               Data.Proxy
                 (Data.Object, Values, To_General_Handler (Data.Func));
            else
               Data.Func (Data.Object, Values);
            end if;
         end if;
      end First_Marshaller;

      -------------
      -- Connect --
      -------------

      procedure Connect
        (Widget : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Marsh  : Marshallers.Marshaller;
         After  : Boolean := False)
      is
         Id : Handler_Id;
         pragma Warnings (Off, Id);
      begin
         Id := Connect (Widget, Name, Marsh, After);
      end Connect;

      --------------------
      -- Object_Connect --
      --------------------

      procedure Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Marsh       : Marshallers.Marshaller;
         Slot_Object : access Widget_Type'Class;
         After       : Boolean := False)
      is
         Id : Handler_Id;
         pragma Warnings (Off, Id);
      begin
         Id := Object_Connect (Widget, Name, Marsh, Slot_Object, After);
      end Object_Connect;

      -------------
      -- Connect --
      -------------

      procedure Connect
        (Widget : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Cb     : Handler;
         After  : Boolean := False)
      is
         Id : Handler_Id;
         pragma Warnings (Off, Id);
      begin
         Id := Connect (Widget, Name, Cb, After);
      end Connect;

      --------------------
      -- Object_Connect --
      --------------------

      procedure Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Cb          : Handler;
         Slot_Object : access Widget_Type'Class;
         After       : Boolean := False)
      is
         Id : Handler_Id;
         pragma Warnings (Off, Id);
      begin
         Id := Object_Connect (Widget, Name, Cb, Slot_Object, After);
      end Object_Connect;

      -------------
      -- Connect --
      -------------

      procedure Connect
        (Widget : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Cb     : Simple_Handler;
         After  : Boolean := False)
      is
         Id : Handler_Id;
         pragma Warnings (Off, Id);
      begin
         Id := Connect
           (Widget, Name,
            To_Marshaller (Marshallers.Void_Marshaller.Handler (Cb)),
            After);
      end Connect;

      --------------------
      -- Object_Connect --
      --------------------

      procedure Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Cb          : Simple_Handler;
         Slot_Object : access Widget_Type'Class;
         After       : Boolean := False)
      is
         Id : Handler_Id;
         pragma Warnings (Off, Id);
      begin
         Id := Object_Connect
           (Widget, Name,
            To_Marshaller (Marshallers.Void_Marshaller.Handler (Cb)),
            Slot_Object, After);
      end Object_Connect;

      -------------
      -- Connect --
      -------------

      function Connect
        (Widget  : access Widget_Type'Class;
         Name    : Glib.Signal_Name;
         Marsh   : Marshallers.Marshaller;
         After   : Boolean := False)
        return Handler_Id
      is
         D : constant Data_Type_Access :=
           new Data_Type_Record'
             (Func  => To_Handler (Marsh.Func),
              Proxy => Marsh.Proxy,
              Object => null);

      begin
         return Do_Signal_Connect
           (Glib.Object.GObject (Widget),
            Name,
            First_Marshaller'Address,
            To_Address (Marsh.Proxy),
            Convert (D),
            Free_Data'Address,
            After,
            Expect_Return_Value => False);
      end Connect;

      --------------------
      -- Object_Connect --
      --------------------

      function Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Marsh       : Marshallers.Marshaller;
         Slot_Object : access Widget_Type'Class;
         After       : Boolean := False) return Handler_Id
      is
         D : constant Data_Type_Access :=
           new Data_Type_Record'
             (Func  => To_Handler (Marsh.Func),
              Proxy => Marsh.Proxy,
              Object => Acc (Slot_Object));

      begin
         return Do_Signal_Connect
           (Glib.Object.GObject (Widget),
            Name,
            First_Marshaller'Address,
            To_Address (Marsh.Proxy),
            Convert (D),
            Free_Data'Address,
            After,
            Get_Object (Slot_Object),
            Expect_Return_Value => False);
      end Object_Connect;

      -------------
      -- Connect --
      -------------

      function Connect
        (Widget  : access Widget_Type'Class;
         Name    : Glib.Signal_Name;
         Cb      : Handler;
         After   : Boolean := False)
        return Handler_Id
      is
         D : constant Data_Type_Access :=
           new Data_Type_Record'(Func => Cb, Proxy => null, Object => null);

      begin
         return Do_Signal_Connect
           (Glib.Object.GObject (Widget),
            Name,
            First_Marshaller'Address,
            To_Address (Cb),
            Convert (D),
            Free_Data'Address,
            After,
            Expect_Return_Value => False);
      end Connect;

      --------------------
      -- Object_Connect --
      --------------------

      function Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Cb          : Handler;
         Slot_Object : access Widget_Type'Class;
         After       : Boolean := False) return Handler_Id
      is
         D : constant Data_Type_Access :=
           new Data_Type_Record'
             (Func   => Cb,
              Proxy  => null,
              Object => Acc (Slot_Object));

      begin
         return Do_Signal_Connect
           (Glib.Object.GObject (Widget),
            Name,
            First_Marshaller'Address,
            To_Address (Cb),
            Convert (D),
            Free_Data'Address,
            After,
            Get_Object (Slot_Object),
            Expect_Return_Value => False);
      end Object_Connect;

      ------------------
      -- Emit_By_Name --
      ------------------

      procedure Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Param  : Gdk.Event.Gdk_Event)
      is
         procedure Internal
           (Object : System.Address;
            Name   : Glib.Signal_Name;
            Param  : System.Address);
         pragma Import (C, Internal, "ada_g_signal_emit_by_name_ptr");

      begin
         pragma Assert (Count_Arguments (Object, Name) = 1);
         Internal
           (Get_Object (Object), Name & ASCII.NUL,
            Gdk.Event.To_Address (Param));
      end Emit_By_Name;

   end Callback;

   -------------------
   -- User_Callback --
   -------------------

   package body User_Callback is

      function To_Handler is new Unchecked_Conversion
        (Gtk.Marshallers.General_Handler, Handler);
      function To_General_Handler is new Unchecked_Conversion
        (Handler, Gtk.Marshallers.General_Handler);
      function To_Address is new Unchecked_Conversion
        (Handler, System.Address);
      function To_Address is new Unchecked_Conversion
        (Marshallers.Handler_Proxy, System.Address);

      ---------------
      -- Free_Data --
      ---------------

      procedure Free_Data (Data : Data_Type_Access) is
         procedure Internal is new Unchecked_Deallocation
           (Data_Type_Record, Data_Type_Access);
         procedure Internal2 is new Unchecked_Deallocation
           (User_Type, User_Access);
         D : Data_Type_Access := Data;
      begin
         Internal2 (D.User);
         Internal (D);
      end Free_Data;

      ----------------------
      -- First_Marshaller --
      ----------------------

      procedure First_Marshaller
        (Closure         : GClosure;
         Return_Value    : GValue;
         N_Params        : Guint;
         Params          : System.Address;
         Invocation_Hint : System.Address;
         User_Data       : System.Address)
      is
         pragma Unreferenced (Invocation_Hint, User_Data, Return_Value);

         use type Marshallers.Handler_Proxy;

         Data   : constant Data_Type_Access := Convert (Get_Data (Closure));
         Stub   : Widget_Type;
         pragma Warnings (Off, Stub);
         Values : GValues;

      begin
         if Data.Func = null then
            return;
         end if;

         Values := Make_Values (N_Params, Params);

         if Data.Object /= null then
            if Data.Proxy /= null then
               Data.Proxy
                 (Data.Object, Values, To_General_Handler (Data.Func),
                  Data.User.all);
            else
               Data.Func (Data.Object, Values, Data.User.all);
            end if;

         elsif Data.Proxy /= null then
            Data.Proxy
              (Acc (Get_User_Data (Get_Address (Nth (Values, 0)), Stub)),
               Values, To_General_Handler (Data.Func), Data.User.all);
         else
            Data.Func
              (Acc (Get_User_Data (Get_Address (Nth (Values, 0)), Stub)),
               Values, Data.User.all);
         end if;
      end First_Marshaller;

      -------------
      -- Connect --
      -------------

      procedure Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Marsh     : Marshallers.Marshaller;
         User_Data : User_Type;
         After     : Boolean := False)
      is
         Id : Handler_Id;
         pragma Warnings (Off, Id);
      begin
         Id := Connect (Widget, Name, Marsh, User_Data, After);
      end Connect;

      --------------------
      -- Object_Connect --
      --------------------

      procedure Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Marsh       : Marshallers.Marshaller;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False)
      is
         Id : Handler_Id;
         pragma Warnings (Off, Id);
      begin
         Id := Object_Connect
           (Widget, Name, Marsh, Slot_Object, User_Data, After);
      end Object_Connect;

      -------------
      -- Connect --
      -------------

      procedure Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Cb        : Handler;
         User_Data : User_Type;
         After     : Boolean := False)
      is
         Id : Handler_Id;
         pragma Warnings (Off, Id);
      begin
         Id := Connect (Widget, Name, Cb, User_Data, After);
      end Connect;

      --------------------
      -- Object_Connect --
      --------------------

      procedure Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Cb          : Handler;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False)
      is
         Id : Handler_Id;
         pragma Warnings (Off, Id);
      begin
         Id := Object_Connect
           (Widget, Name, Cb, Slot_Object, User_Data, After);
      end Object_Connect;

      -------------
      -- Connect --
      -------------

      procedure Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Cb        : Simple_Handler;
         User_Data : User_Type;
         After     : Boolean := False)
      is
         Id : Handler_Id;
         pragma Warnings (Off, Id);
      begin
         Id := Connect
           (Widget, Name,
            To_Marshaller (Marshallers.Void_Marshaller.Handler (Cb)),
            User_Data, After);
      end Connect;

      --------------------
      -- Object_Connect --
      --------------------

      procedure Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Cb          : Simple_Handler;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False)
      is
         Id : Handler_Id;
         pragma Warnings (Off, Id);
      begin
         Id := Object_Connect
           (Widget, Name,
            To_Marshaller (Marshallers.Void_Marshaller.Handler (Cb)),
            Slot_Object, User_Data, After);
      end Object_Connect;

      -------------
      -- Connect --
      -------------

      function Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Marsh     : Marshallers.Marshaller;
         User_Data : User_Type;
         After     : Boolean := False) return Handler_Id
      is
         D : constant Data_Type_Access := new Data_Type_Record'
           (Func   => To_Handler (Marsh.Func),
            Proxy  => Marsh.Proxy,
            User   => new User_Type'(User_Data),
            Object => null);
      begin
         return Do_Signal_Connect
           (Glib.Object.GObject (Widget),
            Name,
            First_Marshaller'Address,
            To_Address (Marsh.Proxy),
            Convert (D),
            Free_Data'Address,
            After,
            Expect_Return_Value => False);
      end Connect;

      --------------------
      -- Object_Connect --
      --------------------

      function Object_Connect
        (Widget      : access GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Marsh       : Marshallers.Marshaller;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False) return Handler_Id
      is
         D : constant Data_Type_Access := new Data_Type_Record'
           (Func  => To_Handler (Marsh.Func),
            Proxy => Marsh.Proxy,
            User  => new User_Type'(User_Data),
            Object => Acc (Slot_Object));
      begin
         return Do_Signal_Connect
           (Glib.Object.GObject (Widget),
            Name,
            First_Marshaller'Address,
            To_Address (Marsh.Proxy),
            Convert (D),
            Free_Data'Address,
            After,
            Get_Object (Slot_Object),
            Expect_Return_Value => False);
      end Object_Connect;

      -------------
      -- Connect --
      -------------

      function Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Cb        : Handler;
         User_Data : User_Type;
         After     : Boolean := False) return Handler_Id
      is
         D : constant Data_Type_Access := new Data_Type_Record'
           (Func   => Cb,
            Proxy  => null,
            User   => new User_Type'(User_Data),
            Object => null);

      begin
         return Do_Signal_Connect
           (Glib.Object.GObject (Widget),
            Name,
            First_Marshaller'Address,
            To_Address (Cb),
            Convert (D),
            Free_Data'Address,
            After,
            Expect_Return_Value => False);
      end Connect;

      --------------------
      -- Object_Connect --
      --------------------

      function Object_Connect
        (Widget      : access GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Cb          : Handler;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False) return Handler_Id
      is
         D : constant Data_Type_Access := new Data_Type_Record'
           (Func   => Cb,
            Proxy  => null,
            User   => new User_Type'(User_Data),
            Object => Acc (Slot_Object));
      begin
         return Do_Signal_Connect
           (Glib.Object.GObject (Widget),
            Name,
            First_Marshaller'Address,
            To_Address (Cb),
            Convert (D),
            Free_Data'Address,
            After,
            Get_Object (Slot_Object),
            Expect_Return_Value => False);
      end Object_Connect;

      ------------------
      -- Emit_By_Name --
      ------------------

      procedure Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : Glib.Signal_Name;
         Param  : Gdk.Event.Gdk_Event)
      is
         procedure Internal
           (Object : System.Address;
            Name   : Glib.Signal_Name;
            Param  : System.Address);
         pragma Import (C, Internal, "ada_g_signal_emit_by_name_ptr");

      begin
         pragma Assert (Count_Arguments (Object, Name) = 1);
         Internal
           (Get_Object (Object), Name & ASCII.NUL,
            Gdk.Event.To_Address (Param));
      end Emit_By_Name;

   end User_Callback;

   -------------------------------------
   -- User_Return_Callback_With_Setup --
   -------------------------------------

   package body User_Return_Callback_With_Setup is

      -------------
      -- Connect --
      -------------

      procedure Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Marsh     : Internal_Cb.Marshallers.Marshaller;
         User_Data : User_Type;
         After     : Boolean := False)
      is
         Id : Handler_Id;
         pragma Warnings (Off, Id);
      begin
         Id := Connect (Widget, Name, Marsh, User_Data, After);
      end Connect;

      --------------------
      -- Object_Connect --
      --------------------

      procedure Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Marsh       : Internal_Cb.Marshallers.Marshaller;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False)
      is
         Id : Handler_Id;
         pragma Warnings (Off, Id);
      begin
         Id := Object_Connect
           (Widget, Name, Marsh, Slot_Object, User_Data, After);
      end Object_Connect;

      -------------
      -- Connect --
      -------------

      procedure Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Cb        : Handler;
         User_Data : User_Type;
         After     : Boolean := False)
      is
         Id : Handler_Id;
         pragma Warnings (Off, Id);
      begin
         Id := Connect (Widget, Name, Cb, User_Data, After);
      end Connect;

      --------------------
      -- Object_Connect --
      --------------------

      procedure Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Cb          : Handler;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False)
      is
         Id : Handler_Id;
         pragma Warnings (Off, Id);
      begin
         Id := Object_Connect
           (Widget, Name, Cb, Slot_Object, User_Data, After);
      end Object_Connect;

      -------------
      -- Connect --
      -------------

      procedure Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Cb        : Simple_Handler;
         User_Data : User_Type;
         After     : Boolean := False)
      is
         Id : Handler_Id;
         pragma Warnings (Off, Id);
      begin
         Id := Connect
           (Widget, Name,
            To_Marshaller (Marshallers.Void_Marshaller.Handler (Cb)),
            User_Data, After);
      end Connect;

      --------------------
      -- Object_Connect --
      --------------------

      procedure Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Cb          : Simple_Handler;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False)
      is
         Id : Handler_Id;
         pragma Warnings (Off, Id);
      begin
         Id := Object_Connect
           (Widget, Name,
            To_Marshaller (Marshallers.Void_Marshaller.Handler (Cb)),
            Slot_Object, User_Data, After);
      end Object_Connect;

      -------------
      -- Connect --
      -------------

      function Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Marsh     : Internal_Cb.Marshallers.Marshaller;
         User_Data : User_Type;
         After     : Boolean := False) return Handler_Id
      is
         Id : constant Handler_Id := Internal_Cb.Connect
           (Widget, Name, Marsh, User_Data, After);
      begin
         Setup (User_Data, Id);
         return Id;
      end Connect;

      --------------------
      -- Object_Connect --
      --------------------

      function Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Marsh       : Internal_Cb.Marshallers.Marshaller;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False) return Handler_Id
      is
         Id : constant Handler_Id := Internal_Cb.Object_Connect
           (Widget, Name, Marsh, Slot_Object, User_Data, After);
      begin
         Setup (User_Data, Id);
         return Id;
      end Object_Connect;

      -------------
      -- Connect --
      -------------

      function Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Cb        : Handler;
         User_Data : User_Type;
         After     : Boolean := False) return Handler_Id
      is
         Id : constant Handler_Id := Internal_Cb.Connect
           (Widget, Name, Cb, User_Data, After);
      begin
         Setup (User_Data, Id);
         return Id;
      end Connect;

      --------------------
      -- Object_Connect --
      --------------------

      function Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Cb          : Handler;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False) return Handler_Id
      is
         Id : constant Handler_Id := Internal_Cb.Object_Connect
           (Widget, Name, Cb, Slot_Object, User_Data, After);
      begin
         Setup (User_Data, Id);
         return Id;
      end Object_Connect;

   end User_Return_Callback_With_Setup;

   ------------------------------
   -- User_Callback_With_Setup --
   ------------------------------

   package body User_Callback_With_Setup is

      -------------
      -- Connect --
      -------------

      procedure Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Marsh     : Internal_Cb.Marshallers.Marshaller;
         User_Data : User_Type;
         After     : Boolean := False)
      is
         Id : Handler_Id;
         pragma Warnings (Off, Id);
      begin
         Id := Connect (Widget, Name, Marsh, User_Data, After);
      end Connect;

      --------------------
      -- Object_Connect --
      --------------------

      procedure Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Marsh       : Internal_Cb.Marshallers.Marshaller;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False)
      is
         Id : Handler_Id;
         pragma Warnings (Off, Id);
      begin
         Id := Object_Connect
           (Widget, Name, Marsh, Slot_Object, User_Data, After);
      end Object_Connect;

      -------------
      -- Connect --
      -------------

      procedure Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Cb        : Handler;
         User_Data : User_Type;
         After     : Boolean := False)
      is
         Id : Handler_Id;
         pragma Warnings (Off, Id);
      begin
         Id := Connect (Widget, Name, Cb, User_Data, After);
      end Connect;

      --------------------
      -- Object_Connect --
      --------------------

      procedure Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Cb          : Handler;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False)
      is
         Id : Handler_Id;
         pragma Warnings (Off, Id);
      begin
         Id := Object_Connect
           (Widget, Name, Cb, Slot_Object, User_Data, After);
      end Object_Connect;

      -------------
      -- Connect --
      -------------

      procedure Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Cb        : Simple_Handler;
         User_Data : User_Type;
         After     : Boolean := False)
      is
         Id : Handler_Id;
         pragma Warnings (Off, Id);
      begin
         Id := Connect
           (Widget, Name,
            To_Marshaller (Marshallers.Void_Marshaller.Handler (Cb)),
            User_Data, After);
      end Connect;

      --------------------
      -- Object_Connect --
      --------------------

      procedure Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Cb          : Simple_Handler;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False)
      is
         Id : Handler_Id;
         pragma Warnings (Off, Id);
      begin
         Id := Object_Connect
           (Widget, Name,
            To_Marshaller (Marshallers.Void_Marshaller.Handler (Cb)),
            Slot_Object, User_Data, After);
      end Object_Connect;

      -------------
      -- Connect --
      -------------

      function Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Marsh     : Internal_Cb.Marshallers.Marshaller;
         User_Data : User_Type;
         After     : Boolean := False) return Handler_Id
      is
         Id : constant Handler_Id := Internal_Cb.Connect
           (Widget, Name, Marsh, User_Data, After);
      begin
         Setup (User_Data, Id);
         return Id;
      end Connect;

      --------------------
      -- Object_Connect --
      --------------------

      function Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Marsh       : Internal_Cb.Marshallers.Marshaller;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False) return Handler_Id
      is
         Id : constant Handler_Id := Internal_Cb.Object_Connect
           (Widget, Name, Marsh, Slot_Object, User_Data, After);
      begin
         Setup (User_Data, Id);
         return Id;
      end Object_Connect;

      -------------
      -- Connect --
      -------------

      function Connect
        (Widget    : access Widget_Type'Class;
         Name      : Glib.Signal_Name;
         Cb        : Handler;
         User_Data : User_Type;
         After     : Boolean := False) return Handler_Id
      is
         Id : constant Handler_Id := Internal_Cb.Connect
           (Widget, Name, Cb, User_Data, After);
      begin
         Setup (User_Data, Id);
         return Id;
      end Connect;

      --------------------
      -- Object_Connect --
      --------------------

      function Object_Connect
        (Widget      : access Glib.Object.GObject_Record'Class;
         Name        : Glib.Signal_Name;
         Cb          : Handler;
         Slot_Object : access Widget_Type'Class;
         User_Data   : User_Type;
         After       : Boolean := False) return Handler_Id
      is
         Id : constant Handler_Id := Internal_Cb.Object_Connect
           (Widget, Name, Cb, Slot_Object, User_Data, After);
      begin
         Setup (User_Data, Id);
         return Id;
      end Object_Connect;

   end User_Callback_With_Setup;

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect
     (Object : access Glib.Object.GObject_Record'Class;
      Id     : in out Handler_Id) is
   begin
      if Id.Id /= Null_Handler_Id then
         Disconnect_Internal (Obj => Get_Object (Object), Id => Id.Id);
         Id.Id := Null_Handler_Id;
      end if;
   end Disconnect;

   -----------------------
   -- Emit_Stop_By_Name --
   -----------------------

   procedure Emit_Stop_By_Name
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : Glib.Signal_Name)
   is
      procedure Internal
        (Object : System.Address;
         Id     : Signal_Id;
         Detail : GQuark := Unknown_Quark);
      pragma Import (C, Internal, "g_signal_stop_emission");
   begin
      Internal
        (Get_Object (Object),
         Signal_Lookup (Name & ASCII.NUL, Get_Type (Object)));
   end Emit_Stop_By_Name;

   -------------------
   -- Handler_Block --
   -------------------

   procedure Handler_Block
     (Obj : access Glib.Object.GObject_Record'Class;
      Id  : Handler_Id)
   is
      procedure Internal (Obj : System.Address; Id : Gulong);
      pragma Import (C, Internal, "g_signal_handler_block");

   begin
      Internal (Obj => Get_Object (Obj), Id => Id.Id);
   end Handler_Block;

   ----------------------
   -- Handlers_Destroy --
   ----------------------

   procedure Handlers_Destroy (Obj : access Glib.Object.GObject_Record'Class)
   is
      procedure Internal (Obj : System.Address);
      pragma Import (C, Internal, "g_signal_handlers_destroy");

   begin
      Internal (Obj => Get_Object (Obj));
   end Handlers_Destroy;

   ---------------------
   -- Handler_Unblock --
   ---------------------

   procedure Handler_Unblock
     (Obj : access Glib.Object.GObject_Record'Class;
      Id  : Handler_Id)
   is
      procedure Internal (Obj : System.Address; Id : Gulong);
      pragma Import (C, Internal, "g_signal_handler_unblock");
   begin
      Internal (Obj => Get_Object (Obj), Id => Id.Id);
   end Handler_Unblock;

end Gtk.Handlers;

--  Design of the package:
--
--  The callback package should accomodate the following:
--  * Sometimes, the user does not want to get the handler_id.  Thus, we give
--    the two versions of Connect, either as a procedure or a function.
--  * Some gtk+ signals expect a handler that returns a value, whereas others
--    return nothing. The simplest here is to have two kinds of packages, one
--    for each case.  The standard gtk+ signals (up to 1.2) have the following
--    possible return values: gboolean, gchar*, gint, guint, void
--  * The first argument to a handler is always a Gtk Object.  since GtkAda is
--    full object oriented, we could simply have the first parameter be a
--    GObject'Class, but this would require in most cases an explicit cast
--    from the user. It is nice to give the opportunity to have a specific
--    type.
--  * The standard callbacks can have any number of arguments (or even
--    none). These arguments depend on the signal type, and the widget type.
--    The only possibility to cover all the cases is to pass these arguments as
--    an array. The package Marshallers provides a convenient interface between
--    these general callbacks and a more usual implementation.
--  * The user can specify some Data to be passed to the handler.  Or there
--    can be none. Once again, we create two versions of the standard packages.
--
--  How the callbacks work:
--     --------      --------------------      ---------------------
--     | Gtk+ |  ->  | First_Marshaller |  ->  | User's Marshaller |
--     --------      --------------------      ---------------------
--                                   \                 |
--                                    \  or            V
--                                     \       -------------------
--                                      \--->  | User's Callback |
--                                             -------------------
--
--  First_Marshaller is an internal function defined in this package. It
--  creates its own user data to memorize both the User's Marshaller (if any),
--  User's Callback, and the User's Data (if any). It then calls the
--  appropriate function in the user's code.
--  The user_data of First_Marshaller is automatically deallocated when the
--  callback is disconnected or the widget is killed, using some capabilities
--  of gtk+ itself.
