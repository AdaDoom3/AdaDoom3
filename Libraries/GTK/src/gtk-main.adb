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

with Gdk.Types;               use Gdk.Types;
with Interfaces.C.Strings;    use Interfaces.C.Strings;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

package body Gtk.Main is

   package C renames Interfaces.C;

   function Timeout_Marshaller (Func : Timeout_Callback) return Gint;
   --  Marshaller for Timeout_Callbacks.

   function Idle_Marshaller (Func : Idle_Callback) return Gint;
   --  Marshaller for Idle_Callbacks.

   function Quit_Marshaller (Func : Quit_Function) return Gint;
   --  Marshaller for Quit_Function.

   --------------
   -- Do_Event --
   --------------

   procedure Do_Event (Event : Gdk.Event.Gdk_Event) is
      procedure Internal (Event : System.Address);
      pragma Import (C, Internal, "gtk_main_do_event");

   begin
      Internal (Gdk.Event.To_Address (Event));
   end Do_Event;

   --------------------
   -- Events_Pending --
   --------------------

   function Events_Pending return Boolean is
      function Internal return Gint;
      pragma Import (C, Internal, "gtk_events_pending");
   begin
      return Boolean'Val (Internal);
   end Events_Pending;

   --------------
   -- Quit_Add --
   --------------

   function Quit_Add
     (Main_Level : Guint; Func : Quit_Function) return Quit_Handler_Id
   is
      function Internal
        (Main_Level : Guint;
         Func       : System.Address;
         Marshal    : System.Address;
         Data       : System.Address := System.Null_Address;
         Destroy    : System.Address := System.Null_Address)
         return Quit_Handler_Id;
      pragma Import (C, Internal, "gtk_quit_add_full");

   begin
      return Internal (Main_Level, Quit_Marshaller'Address, Func'Address);
   end Quit_Add;

   ----------
   -- Quit --
   ----------

   package body Quit is
      type Data_Type_Access is access Data_Type;

      type Cb_Record is record
         Func : Quit_Function;
         Data : Data_Type_Access;
      end record;
      type Cb_Record_Access is access Cb_Record;

      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Cb_Record_Access);

      ----------
      -- Free --
      ----------

      procedure Free_Data (D : System.Address) is
         procedure Internal is new Ada.Unchecked_Deallocation
           (Cb_Record, Cb_Record_Access);
         procedure Internal2 is new Ada.Unchecked_Deallocation
           (Data_Type, Data_Type_Access);

         Data : Cb_Record_Access := Convert (D);
      begin
         Internal2 (Data.Data);
         Internal (Data);
      end Free_Data;

      ----------------
      -- General_Cb --
      ----------------

      function General_Cb (D : System.Address) return Gint is
         Data : constant Cb_Record_Access := Convert (D);
      begin
         return Boolean'Pos (Data.Func (Data.Data.all));
      end General_Cb;

      ---------
      -- Add --
      ---------

      function Quit_Add
        (Main_Level : Guint;
         Func       : Quit_Function;
         Data       : Data_Type) return Quit_Handler_Id
      is
         function Internal
           (Main_Level : Guint;
            Func       : System.Address;
            Marshal    : System.Address;
            Data       : System.Address;
            Destroy    : System.Address) return Quit_Handler_Id;
         pragma Import (C, Internal, "gtk_quit_add_full");

         function Convert is new Ada.Unchecked_Conversion
           (Cb_Record_Access, System.Address);

         D : constant Cb_Record_Access :=
           new Cb_Record'(Func => Func, Data => new Data_Type'(Data));

      begin
         return Internal (Main_Level, General_Cb'Address, System.Null_Address,
                          Convert (D), Free_Data'Address);
      end Quit_Add;
   end Quit;

   ----------------------
   -- Quit_Add_Destroy --
   ----------------------

   function Quit_Add_Destroy
     (Main_Level : Guint;
      Object     : access Glib.Object.GObject_Record'Class)
      return Quit_Handler_Id
   is
      function Internal
        (Main_Level : Guint;
         Object     : System.Address) return Quit_Handler_Id;
      pragma Import (C, Internal, "gtk_quit_add_destroy");

   begin
      return Internal (Main_Level, Get_Object (Object));
   end Quit_Add_Destroy;

   ---------------------
   -- Idle_Marshaller --
   ---------------------

   function Idle_Marshaller (Func : Idle_Callback) return Gint is
   begin
      if Func = null then
         return Boolean'Pos (False);
      else
         return Boolean'Pos (Func.all);
      end if;
   end Idle_Marshaller;

   ---------------------
   -- Quit_Marshaller --
   ---------------------

   function Quit_Marshaller (Func : Quit_Function) return Gint is
   begin
      if Func = null then
         return Boolean'Pos (False);
      else
         return Boolean'Pos (Func.all);
      end if;
   end Quit_Marshaller;

   --------------
   -- Idle_Add --
   --------------

   function Idle_Add
     (Cb       : Idle_Callback;
      Priority : Idle_Priority := Priority_Default_Idle) return Idle_Handler_Id
   is
      function To_Address is new Ada.Unchecked_Conversion
        (Idle_Callback, System.Address);
      function Internal
        (Priority : Idle_Priority;
         Func     : System.Address;
         Marshal  : System.Address := System.Null_Address;
         Data     : System.Address;
         Destroy  : System.Address := System.Null_Address)
         return Idle_Handler_Id;
      pragma Import (C, Internal, "gtk_idle_add_full");

   begin
      return Internal
        (Priority, Idle_Marshaller'Address, System.Null_Address,
         To_Address (Cb));
   end Idle_Add;

   ----------
   -- Init --
   ----------

   gnat_argc : Interfaces.C.int;
   pragma Import (C, gnat_argc);

   gnat_argv : System.Address;
   pragma Import (C, gnat_argv);

   procedure Init is
      procedure Internal (argc : System.Address; argv : System.Address);
      pragma Import (C, Internal, "gtk_init");

   begin
      Internal (gnat_argc'Address, gnat_argv'Address);
   end Init;

   ----------------
   -- Init_Check --
   ----------------

   function Init_Check return Boolean is
      function Internal
        (argc : System.Address; argv : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_init_check");

   begin
      return Boolean'Val (Internal (gnat_argc'Address, gnat_argv'Address));
   end Init_Check;

   ----------------------
   -- Get_Event_Widget --
   ----------------------

   function Get_Event_Widget
     (Event : Gdk.Event.Gdk_Event) return Gtk.Widget.Gtk_Widget
   is
      function Internal (Event : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_get_event_widget");

   begin
      return Gtk.Widget.Convert (Internal (Gdk.Event.To_Address (Event)));
   end Get_Event_Widget;

   --------------
   -- Grab_Add --
   --------------

   procedure Grab_Add (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_grab_add");

   begin
      Internal (Get_Object (Widget));
   end Grab_Add;

   -----------------
   -- Grab_Remove --
   -----------------

   procedure Grab_Remove
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_grab_remove");

   begin
      Internal (Get_Object (Widget));
   end Grab_Remove;

   ----------------------
   -- Grab_Get_Current --
   ----------------------

   function Grab_Get_Current return Gtk.Widget.Gtk_Widget is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_grab_get_current");

   begin
      return Gtk.Widget.Convert (Internal);
   end Grab_Get_Current;

   --------------------
   -- Main_Iteration --
   --------------------

   function Main_Iteration (Blocking : Boolean := True) return Boolean is
      function Internal (Blocking : Gboolean) return Gint;
      pragma Import (C, Internal, "gtk_main_iteration_do");

   begin
      return Boolean'Val (Internal (Boolean'Pos (Blocking)));
   end Main_Iteration;

   ----------------
   -- Set_Locale --
   ----------------

   function Set_Locale return String is
      function Internal return C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_set_locale");

   begin
      return C.Strings.Value (Internal);
   end Set_Locale;

   procedure Set_Locale is
      procedure Internal;
      pragma Import (C, Internal, "gtk_set_locale");

   begin
      Internal;
   end Set_Locale;

   ------------------------
   -- Timeout_Marshaller --
   ------------------------

   function Timeout_Marshaller (Func : Timeout_Callback) return Gint is
   begin
      if Func = null then
         return Boolean'Pos (False);
      else
         return Boolean'Pos (Func.all);
      end if;
   end Timeout_Marshaller;

   -----------------
   -- Timeout_Add --
   -----------------

   function Timeout_Add
     (Interval : Guint32;
      Func     : Timeout_Callback) return Timeout_Handler_Id
   is
      function To_Address is new Ada.Unchecked_Conversion
        (Timeout_Callback, System.Address);
      function Internal
        (Interval : Guint32;
         Func     : System.Address;
         Marshal  : System.Address := System.Null_Address;
         Data     : System.Address;
         Destroy  : System.Address := System.Null_Address)
         return Timeout_Handler_Id;
      pragma Import (C, Internal, "gtk_timeout_add_full");

   begin
      return Internal
        (Interval, Timeout_Marshaller'Address, System.Null_Address,
         To_Address (Func));
   end Timeout_Add;

   ----------
   -- Idle --
   ----------

   package body Idle is

      type Data_Type_Access is access Data_Type;
      type Cb_Record is record
         Func : Callback;
         User_Destroy : Destroy_Callback;
         Data : Data_Type_Access;
      end record;
      type Cb_Record_Access is access Cb_Record;

      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Cb_Record_Access);

      ----------
      -- Free --
      ----------

      procedure Free_Data (D : System.Address) is
         procedure Internal is new Ada.Unchecked_Deallocation
           (Cb_Record, Cb_Record_Access);
         procedure Internal2 is new Ada.Unchecked_Deallocation
           (Data_Type, Data_Type_Access);
         Data : Cb_Record_Access := Convert (D);

      begin
         if Data.User_Destroy /= null then
            Data.User_Destroy (Data.Data.all);
         end if;
         Internal2 (Data.Data);
         Internal (Data);
      end Free_Data;

      ----------------
      -- General_Cb --
      ----------------

      function General_Cb (D : System.Address) return Gint is
         Data : constant Cb_Record_Access := Convert (D);
      begin
         return Boolean'Pos (Data.Func (Data.Data.all));
      end General_Cb;

      ---------
      -- Add --
      ---------

      function Add
        (Cb       : Callback;
         D        : Data_Type;
         Priority : Idle_Priority := Priority_Default_Idle;
         Destroy  : Destroy_Callback := null)
         return Idle_Handler_Id
      is
         function Internal
           (Priority : Idle_Priority;
            Func     : System.Address;
            Marshal  : System.Address;
            Data     : System.Address;
            Destroy  : System.Address) return Idle_Handler_Id;
         pragma Import (C, Internal, "gtk_idle_add_full");

         function Convert is new Ada.Unchecked_Conversion
           (Cb_Record_Access, System.Address);

         Data : constant Cb_Record_Access := new Cb_Record'
           (Func => Cb,
            User_Destroy => Destroy,
            Data => new Data_Type'(D));
      begin
         return Internal (Priority, General_Cb'Address, System.Null_Address,
                          Convert (Data), Free_Data'Address);
      end Add;

   end Idle;

   -------------
   -- Timeout --
   -------------

   package body Timeout is

      type Data_Type_Access is access Data_Type;
      type Cb_Record is record
         Func : Callback;
         Data : Data_Type_Access;
         User_Destroy : Destroy_Callback;
      end record;
      type Cb_Record_Access is access Cb_Record;

      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Cb_Record_Access);

      ----------
      -- Free --
      ----------

      procedure Free_Data (D : System.Address) is
         procedure Internal is new Ada.Unchecked_Deallocation
           (Cb_Record, Cb_Record_Access);
         procedure Internal2 is new Ada.Unchecked_Deallocation
           (Data_Type, Data_Type_Access);

         Data : Cb_Record_Access := Convert (D);
      begin
         if Data.User_Destroy /= null then
            Data.User_Destroy (Data.Data.all);
         end if;

         Internal2 (Data.Data);
         Internal (Data);
      end Free_Data;

      ----------------
      -- General_Cb --
      ----------------

      function General_Cb (D : System.Address) return Gint is
         Data : constant Cb_Record_Access := Convert (D);
      begin
         return Boolean'Pos (Data.Func (Data.Data.all));
      end General_Cb;

      ---------
      -- Add --
      ---------

      function Add
        (Interval : Guint32;
         Func     : Callback;
         D        : Data_Type;
         Destroy  : Destroy_Callback := null) return Timeout_Handler_Id
      is
         function Internal
           (Interval : Guint32;
            Func     : System.Address;
            Marshal  : System.Address;
            Data     : System.Address;
            Destroy  : System.Address) return Timeout_Handler_Id;
         pragma Import (C, Internal, "gtk_timeout_add_full");

         function Convert is new Ada.Unchecked_Conversion
           (Cb_Record_Access, System.Address);

         Data : constant Cb_Record_Access := new Cb_Record'
           (Func => Func,
            Data => new Data_Type'(D),
            User_Destroy => Destroy);
      begin
         return Internal (Interval, General_Cb'Address, System.Null_Address,
                          Convert (Data), Free_Data'Address);
      end Add;

   end Timeout;

   -------------------
   -- Check_Version --
   -------------------

   function Check_Version
     (Required_Major : Guint := Gtk.Major_Version;
      Required_Minor : Guint := Gtk.Minor_Version;
      Required_Micro : Guint := Gtk.Micro_Version)
      return String
   is
      function Internal
        (Required_Major : Guint;
         Required_Minor : Guint;
         Required_Micro : Guint)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_check_version");
      C : constant Interfaces.C.Strings.chars_ptr :=
         Internal (Required_Major, Required_Minor, Required_Micro);
   begin
      if C = Null_Ptr then
         return "";
      else
         return Value (C);
      end if;
   end Check_Version;

   -----------------------------
   -- Get_Current_Event_State --
   -----------------------------

   procedure Get_Current_Event_State
     (State             : out Gdk_Modifier_Type;
      Had_Current_Event : out Boolean)
   is
      function Internal (State : access Gdk_Modifier_Type) return Gboolean;
      pragma Import (C, Internal, "gtk_get_current_event_state");
      St : aliased Gdk_Modifier_Type;
   begin
      Had_Current_Event := Boolean'Val (Internal (St'Unchecked_Access));
      State := St;
   end Get_Current_Event_State;

   ---------------------
   -- Propagate_Event --
   ---------------------

   procedure Propagate_Event
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event)
   is
      procedure Internal
        (Widget : System.Address; Event : Gdk.Event.Gdk_Event);
      pragma Import (C, Internal, "gtk_propagate_event");
   begin
      Internal (Get_Object (Widget), Event);
   end Propagate_Event;

end Gtk.Main;
