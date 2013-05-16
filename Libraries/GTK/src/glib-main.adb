-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                Copyright (C) 2006-2013, AdaCore                   --
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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

package body Glib.Main is

   function Low_Level_Source_Func (Func : G_Source_Func) return Gboolean;
   pragma Convention (C, Low_Level_Source_Func);
   --  Low-level wrapper around G_Source_Func, which can be called directly
   --  from C. Func is the Ada function that this wraps.

   function To_Address is new Ada.Unchecked_Conversion
     (G_Source_Func, System.Address);

   -----------------------
   -- Find_Source_By_Id --
   -----------------------

   function Find_Source_By_Id
     (Id : G_Source_Id; Context : G_Main_Context := null) return G_Source
   is
      function Internal
        (Context : G_Main_Context; Id : G_Source_Id) return G_Source;
      pragma Import (C, Internal, "g_main_context_find_source_by_id");
   begin
      return Internal (Context, Id);
   end Find_Source_By_Id;

   ---------------------------
   -- Low_Level_Source_Func --
   ---------------------------

   function Low_Level_Source_Func (Func : G_Source_Func) return Gboolean is
   begin
      if Func = null then
         return Boolean'Pos (False);
      else
         return Boolean'Pos (Func.all);
      end if;
   end Low_Level_Source_Func;

   --------------
   -- Idle_Add --
   --------------

   function Idle_Add (Func : G_Source_Func) return G_Source_Id is
      function Internal
        (Func : System.Address; Data : System.Address) return G_Source_Id;
      pragma Import (C, Internal, "g_idle_add");
   begin
      return Internal (Low_Level_Source_Func'Address, To_Address (Func));
   end Idle_Add;

   -----------------
   -- Timeout_Add --
   -----------------

   function Timeout_Add
     (Interval : Guint;
      Func     : G_Source_Func) return G_Source_Id
   is
      function Internal
        (Interval : Guint;
         Func     : System.Address;
         Data     : System.Address) return G_Source_Id;
      pragma Import (C, Internal, "g_timeout_add");
   begin
      return Internal
        (Interval, Low_Level_Source_Func'Address, To_Address (Func));
   end Timeout_Add;

   ---------------------
   -- Generic_Sources --
   ---------------------

   package body Generic_Sources is

      type Data_Type_Access is access Data_Type;

      type Cb_Record is record
         Func   : G_Source_Func;
         Notify : Destroy_Notify;
         Data   : Data_Type_Access;
      end record;
      type Cb_Record_Access is access Cb_Record;

      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Cb_Record_Access);

      function Convert is new Ada.Unchecked_Conversion
        (Cb_Record_Access, System.Address);

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
         if Data.Notify /= null then
            Data.Notify (Data.Data.all);
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

      --------------
      -- Idle_Add --
      --------------

      function Idle_Add
        (Func     : G_Source_Func;
         Data     : Data_Type;
         Priority : G_Priority := Priority_Default_Idle;
         Notify   : Destroy_Notify := null)
         return G_Source_Id
      is
         function Internal
           (Priority : G_Priority;
            Func     : System.Address;
            Data     : System.Address;
            Notify   : System.Address) return G_Source_Id;
         pragma Import (C, Internal, "g_idle_add_full");

         D : constant Cb_Record_Access := new Cb_Record'
           (Func   => Func,
            Notify => Notify,
            Data   => new Data_Type'(Data));
      begin
         return Internal
           (Priority, General_Cb'Address, Convert (D), Free_Data'Address);
      end Idle_Add;

      -----------------
      -- Timeout_Add --
      -----------------

      function Timeout_Add
        (Interval : Guint;
         Func     : G_Source_Func;
         Data     : Data_Type;
         Priority : G_Priority := Priority_Default;
         Notify   : Destroy_Notify := null) return G_Source_Id
      is
         function Internal
           (Priority : G_Priority;
            Interval : Guint;
            Func     : System.Address;
            Data     : System.Address;
            Notify   : System.Address) return G_Source_Id;
         pragma Import (C, Internal, "g_timeout_add_full");

         D : constant Cb_Record_Access := new Cb_Record'
           (Func   => Func,
            Notify => Notify,
            Data   => new Data_Type'(Data));
      begin
         return Internal
           (Priority, Interval,
            General_Cb'Address, Convert (D), Free_Data'Address);
      end Timeout_Add;

      ------------------
      -- Set_Callback --
      ------------------

      procedure Set_Callback
        (Source   : G_Source;
         Func     : G_Source_Func;
         Data     : Data_Type;
         Notify   : Destroy_Notify := null)
      is
         procedure Internal
           (Source : G_Source;
            Func   : System.Address;
            Data   : System.Address;
            Notify : System.Address);
         pragma Import (C, Internal, "g_source_set_callback");

         D : constant Cb_Record_Access := new Cb_Record'
           (Func   => Func,
            Notify => Notify,
            Data   => new Data_Type'(Data));
      begin
         Internal (Source, General_Cb'Address, Convert (D), Free_Data'Address);
      end Set_Callback;

   end Generic_Sources;

   ------------
   -- Remove --
   ------------

   function Remove (Id : G_Source_Id) return Boolean is
      function Internal (Id : G_Source_Id) return Gboolean;
      pragma Import (C, Internal, "g_source_remove");
   begin
      return Boolean'Val (Internal (Id));
   end Remove;

   ------------
   -- Remove --
   ------------

   procedure Remove (Id : G_Source_Id) is
      Tmp : Boolean;
      pragma Unreferenced (Tmp);
   begin
      Tmp := Remove (Id);
   end Remove;

   ----------------------
   -- Default_Dispatch --
   ----------------------

   function Default_Dispatch
     (Source : G_Source; Cb : G_Source_Func_User_Data; Data : System.Address)
      return Gboolean
   is
      pragma Unreferenced (Source);
   begin
      if Cb /= null then
         return Cb (Data);
      else
         --  No callback set => No need to call this g_source again, even.
         return 0;
      end if;
   end Default_Dispatch;

   -------------
   -- Acquire --
   -------------

   function Acquire (Context : G_Main_Context) return Boolean is
      function Internal (Context : G_Main_Context) return Gboolean;
      pragma Import (C, Internal, "g_main_context_acquire");
   begin
      return Boolean'Val (Internal (Context));
   end Acquire;

   --------------
   -- Is_Owner --
   --------------

   function Is_Owner (Context : G_Main_Context) return Boolean is
      function Internal (Context : G_Main_Context) return Gboolean;
      pragma Import (C, Internal, "g_main_context_is_owner");
   begin
      return Boolean'Val (Internal (Context));
   end Is_Owner;

   ---------------------
   -- Set_Can_Recurse --
   ---------------------

   procedure Set_Can_Recurse (Source : G_Source; Can_Recurse : Boolean) is
      procedure Internal (Source : G_Source; Can_Recurse : Gboolean);
      pragma Import (C, Internal, "g_source_set_can_recurse");
   begin
      Internal (Source, Boolean'Pos (Can_Recurse));
   end Set_Can_Recurse;

   ---------------------
   -- Get_Can_Recurse --
   ---------------------

   function Get_Can_Recurse (Source : G_Source) return Boolean is
      function Internal (Source : G_Source) return Gboolean;
      pragma Import (C, Internal, "g_source_get_can_recurse");
   begin
      return Boolean'Val (Internal (Source));
   end Get_Can_Recurse;

end Glib.Main;
