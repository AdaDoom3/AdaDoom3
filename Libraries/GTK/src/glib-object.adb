-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
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

with Interfaces.C.Strings;
with Unchecked_Conversion;
with Unchecked_Deallocation;

with Glib.Type_Conversion_Hooks;
with Gtkada.Bindings;  use Gtkada.Bindings;
with Gtkada.C;         use Gtkada.C;
with Gtkada.Types;     use Gtkada.Types;

package body Glib.Object is
   package Signal_Id_Arrays is new Gtkada.C.Unbounded_Arrays
     (Glib.Signal_Id, Glib.Null_Signal_Id, Glib.Guint,
      Glib.Object.Signal_Id_Array);

   procedure Free_User_Data (Data : System.Address);
   --  Free the user data Data. This function should not be called directly

   procedure Set_User_Data
     (Obj     : System.Address;
      Quark   : Glib.GQuark;
      Data    : System.Address;
      Destroy : System.Address);
   pragma Import (C, Set_User_Data, "g_object_set_qdata_full");

   function To_Object is new Ada.Unchecked_Conversion
     (System.Address, GObject);

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate (Object : access GObject_Record) is
      procedure Free is new Unchecked_Deallocation
        (GObject_Record'Class, GObject);

      Obj : GObject := GObject (Object);

   begin
      Free (Obj);
   end Deallocate;

   --------------------
   -- Free_User_Data --
   --------------------

   procedure Free_User_Data (Data : System.Address) is
      pragma Warnings (Off);
      --  This UC is safe aliasing-wise, so kill warning
      function Convert is new Unchecked_Conversion (System.Address, GObject);
      pragma Warnings (On);

   begin
      Deallocate (Convert (Data));
   end Free_User_Data;

   -----------
   -- G_New --
   -----------

   procedure G_New (Object : out GObject) is
   begin
      Object := new GObject_Record;
      Initialize (Object);
   end G_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Object : access GObject_Record'Class) is
      function Internal (Typ : GType) return System.Address;
      pragma Import (C, Internal, "ada_g_object_new");
   begin
      Set_Object (Object, Internal (GType_Object));
   end Initialize;

   ----------------
   -- Get_Object --
   ----------------

   function Get_Object
     (Object : access GObject_Record'Class) return System.Address is
   begin
      return Object.Ptr;
   end Get_Object;

   ------------------------
   -- Get_Object_Or_Null --
   ------------------------

   function Get_Object_Or_Null (Object : GObject) return System.Address is
   begin
      if Object = null then
         return System.Null_Address;
      else
         return Object.Ptr;
      end if;
   end Get_Object_Or_Null;

   --------------
   -- Get_Type --
   --------------

   function Get_Type (Object : access GObject_Record) return GType is
      function Internal (Object : System.Address) return GType;
      pragma Import (C, Internal, "ada_gobject_get_type");

   begin
      return Internal (Get_Object (Object));
   end Get_Type;

   -------------------
   -- Get_User_Data --
   -------------------

   function Get_User_Data
     (Obj  : System.Address;
      Stub : GObject_Record'Class) return GObject
   is
      function Internal
        (Object : System.Address;
         Quark  : Glib.GQuark) return System.Address;
      pragma Import (C, Internal, "g_object_get_qdata");

      use type System.Address;

      R : GObject;

   begin
      if Obj = System.Null_Address then
         return null;
      end if;

      if GtkAda_String_Quark = Glib.Unknown_Quark then
         GtkAda_String_Quark := Glib.Quark_From_String (GtkAda_String);
      end if;

      R := To_Object (Internal (Obj, GtkAda_String_Quark));

      if R = null then
         R := Glib.Type_Conversion_Hooks.Conversion_Function (Obj, Stub);

         --  This function will either simply return what we expect (Stub), or
         --  try to create the exact Ada type corresponding to the C type.

         Set_Object (R, Obj);
      end if;

      return R;
   end Get_User_Data;

   ------------------------
   -- Get_User_Data_Fast --
   ------------------------

   function Get_User_Data_Fast
     (Obj  : System.Address;
      Stub : GObject_Record'Class) return GObject
   is
      pragma Suppress (All_Checks);

      function Internal
        (Object : System.Address;
         Quark  : Glib.GQuark) return System.Address;
      pragma Import (C, Internal, "g_object_get_qdata");

      use type System.Address;

      R : GObject;

   begin
      if Obj = System.Null_Address then
         return null;
      end if;

      if GtkAda_String_Quark = Glib.Unknown_Quark then
         GtkAda_String_Quark := Glib.Quark_From_String (GtkAda_String);
      end if;

      R := To_Object (Internal (Obj, GtkAda_String_Quark));

      if R = null then
         R := new GObject_Record'Class'(Stub);
         Set_Object (R, Obj);
      end if;

      return R;
   end Get_User_Data_Fast;

   ----------------
   -- Is_Created --
   ----------------

   function Is_Created (Object : GObject_Record'Class) return Boolean is
      use type System.Address;
   begin
      return Object.Ptr /= System.Null_Address;
   end Is_Created;

   ----------------
   -- Set_Object --
   ----------------

   procedure Set_Object
     (Object : access GObject_Record'Class;
      Value  : System.Address)
   is
      use type System.Address;

   begin
      Object.Ptr := Value;

      --  Sets a user data field for the C object associated with Obj.
      --  This field will be used so that it is possible, knowing a
      --  C object, to get the full ada object.

      if GtkAda_String_Quark = Glib.Unknown_Quark then
         GtkAda_String_Quark := Glib.Quark_From_String (GtkAda_String);
      end if;

      --  If the user_data was already set in C, gtk+ will automatically call
      --  the finalization on it, and thus indirectly destroy the Ada
      --  object. which is exactly what we want.

      --  Special case for Null_Address, since this means we are already
      --  destroying the widget, and Set_User_Data would be invalid.

      if Value /= System.Null_Address then
         Set_User_Data (Value, GtkAda_String_Quark,
                        Object.all'Address, Free_User_Data'Address);
      end if;
   end Set_Object;

   --------------------
   -- Unchecked_Cast --
   --------------------

   function Unchecked_Cast
     (Obj  : access GObject_Record'Class;
      Stub : GObject_Record'Class) return GObject
   is
      Result : constant GObject := new GObject_Record'Class'(Stub);

      procedure Set_User_Data
        (Obj     : System.Address;
         Quark   : Glib.GQuark;
         Data    : System.Address;
         Destroy : System.Address);
      pragma Import (C, Set_User_Data, "g_object_set_qdata_full");

   begin
      Result.Ptr := Obj.Ptr;
      Set_User_Data
        (Obj.Ptr, GtkAda_String_Quark, Result'Address, Free_User_Data'Address);
      Deallocate (Obj);
      return Result;
   end Unchecked_Cast;

   -----------------------------
   -- Initialize_Class_Record --
   -----------------------------

   procedure Initialize_Class_Record
     (Object       : access GObject_Record'Class;
      Signals      : Gtkada.Types.Chars_Ptr_Array;
      Class_Record : in out GObject_Class;
      Type_Name    : String;
      Parameters   : Signal_Parameter_Types := Null_Parameter_Types)
   is
      function Internal
        (Object         : System.Address;
         NSignals       : Gint;
         Signals        : System.Address;
         Parameters     : System.Address;
         Max_Parameters : Gint;
         Class_Record   : GObject_Class;
         Type_Name      : String;
         Num_Virtual    : Gint;
         Virtual        : System.Address) return GObject_Class;
      pragma Import (C, Internal, "ada_initialize_class_record");

      Default_Params : Signal_Parameter_Types (1 .. Signals'Length, 1 .. 0) :=
        (others => (others => GType_None));
      Pa  : System.Address := Default_Params'Address;
      Num : Gint := 0;

   begin
      if Parameters /= Null_Parameter_Types then
         pragma Assert (Parameters'Length (1) = Signals'Length);
         Pa := Parameters'Address;
         Num := Parameters'Length (2);
      end if;

      Class_Record := Internal
        (Get_Object (Object),
         Signals'Length,
         Signals'Address,
         Pa,
         Num,
         Class_Record,
         Type_Name & ASCII.NUL,
         0, System.Null_Address);
   end Initialize_Class_Record;

   --------------
   -- List_Ids --
   --------------

   function List_Ids (Typ : Glib.GType) return Signal_Id_Array is
      use Signal_Id_Arrays;
      function Internal
        (Typ : GType; N_Ids : access Guint) return Unbounded_Array_Access;
      pragma Import (C, Internal, "g_signal_list_ids");

      N      : aliased Guint;
      Output : constant Unbounded_Array_Access := Internal (Typ, N'Access);
      Result : constant Signal_Id_Array := To_Array (Output, N);

   begin
      G_Free (Output);
      return Result;
   end List_Ids;

   -----------------
   -- Signal_Name --
   -----------------

   function Signal_Name (Q : Signal_Query) return Glib.Signal_Name is
      function Internal
        (Q : Signal_Query) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "ada_gsignal_query_signal_name");

   begin
      return Glib.Signal_Name
        (String'(Interfaces.C.Strings.Value (Internal (Q))));
   end Signal_Name;

   ------------
   -- Params --
   ------------

   function Params (Q : Signal_Query) return GType_Array is
      use GType_Arrays;
      function Internal
        (Q     : Signal_Query;
         N_Ids : access Guint) return Unbounded_Array_Access;
      pragma Import (C, Internal, "ada_gsignal_query_params");

      N      : aliased Guint;
      Output : constant Unbounded_Array_Access := Internal (Q, N'Access);
      Result : constant GType_Array := To_Array (Output, N);

   begin
      --  Do not free Output, it belongs to gtk+
      --  G_Free (Output);
      return Result;
   end Params;

   ------------
   -- Lookup --
   ------------

   function Lookup (Object : GType; Signal : String) return Glib.Signal_Id is
      function Internal (Signal : String; Object : GType) return Signal_Id;
      pragma Import (C, Internal, "g_signal_lookup");

   begin
      return Internal (Signal & ASCII.NUL, Object);
   end Lookup;

   ------------
   -- Notify --
   ------------

   procedure Notify
     (Object        : access GObject_Record;
      Property_Name : String)
   is
      procedure Internal (Object : System.Address; Name : String);
      pragma Import (C, Internal, "g_object_notify");

   begin
      Internal (Get_Object (Object), Property_Name & ASCII.NUL);
   end Notify;

   ---------
   -- Ref --
   ---------

   procedure Ref (Object : access GObject_Record) is
      procedure Internal (Object : System.Address);
      pragma Import (C, Internal, "g_object_ref");
   begin
      Internal (Get_Object (Object));
   end Ref;

   --------------
   -- Ref_Sink --
   --------------

   procedure Ref_Sink (Object : access GObject_Record) is
      procedure Internal (Object : System.Address);
      pragma Import (C, Internal, "g_object_ref_sink");
   begin
      Internal (Get_Object (Object));
   end Ref_Sink;

   -----------
   -- Unref --
   -----------

   procedure Unref (Object : access GObject_Record) is
      procedure Internal (Object : System.Address);
      pragma Import (C, Internal, "g_object_unref");
   begin
      Internal (Get_Object (Object));
   end Unref;

   ---------------
   -- User_Data --
   ---------------

   package body User_Data is
      type Data_Access is access all Data_Type;

      type Cb_Record is record
         Ptr          : Data_Access;
         On_Destroyed : On_Destroyed_Callback;
      end record;
      type Cb_Record_Access is access all Cb_Record;

      function Convert is new
        Unchecked_Conversion (System.Address, Cb_Record_Access);

      procedure Set_Data_Internal
        (Object  : System.Address;
         Key     : String;
         Data    : System.Address;
         Destroy : System.Address);
      pragma Import (C, Set_Data_Internal, "g_object_set_data_full");

      procedure Set_Data_Internal_Id
        (Object  : System.Address;
         Key     : Glib.GQuark;
         Data    : System.Address;
         Destroy : System.Address);
      pragma Import (C, Set_Data_Internal_Id, "g_object_set_qdata_full");

      function Get_Data_Internal
        (Object : System.Address;
         Key    : String) return System.Address;
      pragma Import (C, Get_Data_Internal, "g_object_get_data");

      function Get_Data_Internal_Id
        (Object : System.Address;
         Key    : Glib.GQuark) return System.Address;
      pragma Import (C, Get_Data_Internal_Id, "g_object_get_qdata");

      ----------
      -- Free --
      ----------

      procedure Free_Data (Data : System.Address) is
         procedure Internal is new
           Unchecked_Deallocation (Cb_Record, Cb_Record_Access);

         procedure Internal2 is new
           Unchecked_Deallocation (Data_Type, Data_Access);

         D : Cb_Record_Access := Convert (Data);

      begin
         if D.On_Destroyed /= null then
            D.On_Destroyed (D.Ptr.all);
         end if;
         Internal2 (D.Ptr);
         Internal (D);
      end Free_Data;

      ---------
      -- Get --
      ---------

      function Get
        (Object : access GObject_Record'Class;
         Id     : String := "user_data") return Data_Type
      is
         D : constant Cb_Record_Access :=
           Convert (Get_Data_Internal (Get_Object (Object), Id & ASCII.NUL));
      begin
         if D = null or else D.Ptr = null then
            raise Gtkada.Types.Data_Error;
         else
            return D.Ptr.all;
         end if;
      end Get;

      ---------
      -- Get --
      ---------

      function Get
        (Object  : access GObject_Record'Class;
         Id      : String := "user_data";
         Default : Data_Type) return Data_Type
      is
         D : constant Cb_Record_Access :=
           Convert (Get_Data_Internal (Get_Object (Object), Id & ASCII.NUL));
      begin
         if D = null or else D.Ptr = null then
            return Default;
         else
            return D.Ptr.all;
         end if;
      end Get;

      ---------
      -- Get --
      ---------

      function Get
        (Object : access GObject_Record'Class;
         Id     : Glib.GQuark) return Data_Type
      is
         D : constant Cb_Record_Access :=
           Convert (Get_Data_Internal_Id (Get_Object (Object), Id));
      begin
         if D = null or else D.Ptr = null then
            raise Gtkada.Types.Data_Error;
         else
            return D.Ptr.all;
         end if;
      end Get;

      ---------
      -- Get --
      ---------

      function Get
        (Object  : access GObject_Record'Class;
         Id      : Glib.GQuark;
         Default : Data_Type) return Data_Type
      is
         D : constant Cb_Record_Access :=
           Convert (Get_Data_Internal_Id (Get_Object (Object), Id));
      begin
         if D = null or else D.Ptr = null then
            return Default;
         else
            return D.Ptr.all;
         end if;
      end Get;

      ---------
      -- Set --
      ---------

      procedure Set
        (Object : access GObject_Record'Class;
         Data   : Data_Type;
         Id     : String := "user_data";
         On_Destroyed : On_Destroyed_Callback := null)
      is
         function Convert is new
           Unchecked_Conversion (Cb_Record_Access, System.Address);
         D : constant Cb_Record_Access :=
           new Cb_Record'(Ptr => new Data_Type'(Data),
                          On_Destroyed => On_Destroyed);

      begin
         Set_Data_Internal
           (Get_Object (Object),
            Id & ASCII.NUL,
            Convert (D),
            Free_Data'Address);
      end Set;

      ---------
      -- Set --
      ---------

      procedure Set
        (Object : access GObject_Record'Class;
         Data   : Data_Type;
         Id     : Glib.GQuark;
         On_Destroyed : On_Destroyed_Callback := null)
      is
         function Convert is new
           Unchecked_Conversion (Cb_Record_Access, System.Address);
         D : constant Cb_Record_Access :=
           new Cb_Record'(Ptr => new Data_Type'(Data),
                          On_Destroyed => On_Destroyed);
      begin
         Set_Data_Internal_Id
           (Get_Object (Object),
            Id,
            Convert (D),
            Free_Data'Address);
      end Set;

      ------------
      -- Remove --
      ------------

      procedure Remove
        (Object : access GObject_Record'Class;
         Id     : String := "user_data")
      is
         procedure Internal (Object : System.Address; Id : String);
         pragma Import (C, Internal, "g_object_steal_data");

      begin
         --  First make sure that the destroy callback is called, so that
         --  memory can be freed

         Set_Data_Internal
           (Get_Object (Object),
            Id & ASCII.NUL, System.Null_Address, System.Null_Address);
         Internal (Get_Object (Object), Id & ASCII.NUL);
      end Remove;

      ------------
      -- Remove --
      ------------

      procedure Remove
        (Object : access GObject_Record'Class;
         Id     : Glib.GQuark)
      is
         procedure Internal (Object : System.Address; Id : Glib.GQuark);
         pragma Import (C, Internal, "g_object_steal_qdata");

      begin
         --  First make sure that the destroy callback is called, so that
         --  memory can be freed

         Set_Data_Internal_Id
           (Get_Object (Object), Id, System.Null_Address, System.Null_Address);
         Internal (Get_Object (Object), Id);
      end Remove;
   end User_Data;

   -------------
   -- Convert --
   -------------

   function Convert (W : GObject) return System.Address is
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

   function Convert (W : System.Address) return GObject is
      Stub : GObject_Record;
   begin
      return Get_User_Data (W, Stub);
   end Convert;

   --------------
   -- Weak_Ref --
   --------------

   procedure Weak_Ref
     (Object : access GObject_Record'Class;
      Notify : Weak_Notify;
      Data   : System.Address := System.Null_Address)
   is
      procedure Internal (Object : System.Address;
                          Notify : Weak_Notify;
                          Data   : System.Address);
      pragma Import (C, Internal, "g_object_weak_ref");
   begin
      Internal (Get_Object (Object), Notify, Data);
   end Weak_Ref;

   ----------------
   -- Weak_Unref --
   ----------------

   procedure Weak_Unref
     (Object : access GObject_Record'Class;
      Notify : Weak_Notify;
      Data   : System.Address := System.Null_Address)
   is
      procedure Internal (Object : System.Address;
                          Notify : Weak_Notify;
                          Data   : System.Address);
      pragma Import (C, Internal, "g_object_weak_unref");
   begin
      Internal (Get_Object (Object), Notify, Data);
   end Weak_Unref;

   -------------------------------
   -- Interface_List_Properties --
   -------------------------------

   function Interface_List_Properties
     (Vtable : Interface_Vtable) return Glib.Param_Spec_Array
   is
      use Pspec_Arrays;
      function Internal
        (Vtable  : Interface_Vtable;
         N_Props : access Guint) return Unbounded_Array_Access;
      pragma Import (C, Internal, "g_object_interface_list_properties");

      N       : aliased Guint;
      Output  : constant Unbounded_Array_Access := Internal (Vtable, N'Access);
      Result  : constant Param_Spec_Array := To_Array (Output, Integer (N));

   begin
      --  Doc says we should free, but that results in double-deallocation...
--     G_Free (Output);
      return Result;
   end Interface_List_Properties;

   ---------------------------
   -- Class_List_Properties --
   ---------------------------

   function Class_List_Properties
     (Class : GObject_Class) return Glib.Param_Spec_Array
   is
      use Pspec_Arrays;
      function Internal
        (Class   : GObject_Class;
         N_Props : access Guint) return Unbounded_Array_Access;
      pragma Import (C, Internal, "g_object_class_list_properties");

      N      : aliased Guint;
      Output : constant Unbounded_Array_Access := Internal (Class, N'Access);
      Result : constant Param_Spec_Array := To_Array (Output, Integer (N));
   begin
      --  Doc says we should free, but that results in double-deallocation...
--      G_Free (Output);
      return Result;
   end Class_List_Properties;

end Glib.Object;
