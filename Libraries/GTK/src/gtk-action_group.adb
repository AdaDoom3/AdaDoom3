-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                 Copyright (C) 2006-2013, AdaCore                  --
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
with Gtkada.Bindings;      use Gtkada.Bindings;
with Gtk.Action;           use Gtk.Action;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Glib.Type_Conversion_Hooks;

package body Gtk.Action_Group is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Action_Group_Record);
   pragma Warnings (Off, Type_Conversion);

   procedure Local_Radio_Action_Callback
     (Group, Current, User_Data : System.Address);
   pragma Convention (C, Local_Radio_Action_Callback);
   --  Local proxy for Radio_Action_Callback

   procedure Local_Radio_Action_Destroy
     (Data : in out System.Address);
   pragma Convention (C, Local_Radio_Action_Destroy);
   --  Local proxy for the Destroy notify for Radio_Action

   type Local_Radio_Action_User_Data is record
      Callback  : Radio_Action_Callback;
      User_Data : System.Address;
      Destroy   : G_Destroy_Notify_Address;
   end record;
   type Local_Radio_Action_User_Data_Access is
     access Local_Radio_Action_User_Data;

   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Local_Radio_Action_User_Data_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Local_Radio_Action_User_Data_Access, System.Address);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Local_Radio_Action_User_Data, Local_Radio_Action_User_Data_Access);

   ----------------
   -- Add_Action --
   ----------------

   procedure Add_Action
     (Action_Group : access Gtk_Action_Group_Record;
      Action       : access Gtk_Action_Record'Class)
   is
      procedure Internal
        (Action_Group : System.Address;
         Action       : System.Address);
      pragma Import (C, Internal, "gtk_action_group_add_action");
   begin
      Internal (Get_Object (Action_Group), Get_Object (Action));
   end Add_Action;

   ---------------------------
   -- Add_Action_With_Accel --
   ---------------------------

   procedure Add_Action_With_Accel
     (Action_Group : access Gtk_Action_Group_Record;
      Action       : access Gtk_Action_Record'Class;
      Accelerator  : String := "")
   is
      procedure Internal
        (Action_Group : System.Address;
         Action       : System.Address;
         Accelerator  : chars_ptr);
      pragma Import (C, Internal, "gtk_action_group_add_action_with_accel");
      A : chars_ptr := String_Or_Null (Accelerator);
   begin
      Internal (Get_Object (Action_Group), Get_Object (Action), A);
      Free (A);
   end Add_Action_With_Accel;

   ------------
   -- Create --
   ------------

   function Create
     (Name        : String;
      Label       : String := "";
      Stock_Id    : String := "";
      Accelerator : String := "";
      Tooltip     : String := "";
      Callback    : Action_Callback := null) return Action_Entry is
   begin
      return (Name        => String_Or_Null (Name),
              Label       => String_Or_Null (Label),
              Stock_Id    => String_Or_Null (Stock_Id),
              Accelerator => String_Or_Null (Accelerator),
              Tooltip     => String_Or_Null (Tooltip),
              Callback    => Callback);
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Name        : String;
      Label       : String := "";
      Stock_Id    : String := "";
      Accelerator : String := "";
      Tooltip     : String := "";
      Callback    : Action_Callback := null;
      Is_Active   : Boolean := True) return Toggle_Action_Entry is
   begin
      return (Name        => String_Or_Null (Name),
              Label       => String_Or_Null (Label),
              Stock_Id    => String_Or_Null (Stock_Id),
              Accelerator => String_Or_Null (Accelerator),
              Tooltip     => String_Or_Null (Tooltip),
              Callback    => Callback,
              Is_Active   => Boolean'Pos (Is_Active));
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Name        : String;
      Label       : String;
      Stock_Id    : String := "";
      Accelerator : String := "";
      Tooltip     : String := "";
      Value       : Glib.Gint) return Radio_Action_Entry is
   begin
      return (Name        => String_Or_Null (Name),
              Label       => String_Or_Null (Label),
              Stock_Id    => String_Or_Null (Stock_Id),
              Accelerator => String_Or_Null (Accelerator),
              Tooltip     => String_Or_Null (Tooltip),
              Value       => Value);
   end Create;

   ----------
   -- Free --
   ----------

   procedure Free (Action  : in out Action_Entry) is
   begin
      Free (Action.Name);
      Free (Action.Label);
      Free (Action.Stock_Id);
      Free (Action.Accelerator);
      Free (Action.Tooltip);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Actions : in out Action_Entry_Array) is
   begin
      for A in Actions'Range loop
         Free (Actions (A));
      end loop;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Action  : in out Radio_Action_Entry) is
   begin
      Free (Action.Name);
      Free (Action.Label);
      Free (Action.Stock_Id);
      Free (Action.Accelerator);
      Free (Action.Tooltip);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Actions : in out Radio_Action_Entry_Array) is
   begin
      for A in Actions'Range loop
         Free (Actions (A));
      end loop;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Action  : in out Toggle_Action_Entry) is
   begin
      Free (Action.Name);
      Free (Action.Label);
      Free (Action.Stock_Id);
      Free (Action.Accelerator);
      Free (Action.Tooltip);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Actions : in out Toggle_Action_Entry_Array) is
   begin
      for A in Actions'Range loop
         Free (Actions (A));
      end loop;
   end Free;

   -----------------
   -- Add_Actions --
   -----------------

   procedure Add_Actions
     (Action_Group : access Gtk_Action_Group_Record;
      Entries      : Action_Entry_Array;
      User_Data    : System.Address := System.Null_Address;
      Destroy      : Glib.G_Destroy_Notify_Address := null)
   is
      procedure Internal
        (Action_Group : System.Address;
         Entries      : System.Address;
         N_Entries    : Guint;
         User_Data    : System.Address;
         Destroy      : G_Destroy_Notify_Address);
      pragma Import (C, Internal, "gtk_action_group_add_actions_full");
   begin
      Internal (Get_Object (Action_Group),
                Entries (Entries'First)'Address,
                Entries'Length,
                User_Data, Destroy);
   end Add_Actions;

   ---------------------------------
   -- Local_Radio_Action_Callback --
   ---------------------------------

   procedure Local_Radio_Action_Callback
     (Group, Current, User_Data : System.Address)
   is
      Data : constant Local_Radio_Action_User_Data_Access :=
        Convert (User_Data);
   begin
      Data.Callback (Convert (Group), Convert (Current), Data.User_Data);
   end Local_Radio_Action_Callback;

   --------------------------------
   -- Local_Radio_Action_Destroy --
   --------------------------------

   procedure Local_Radio_Action_Destroy
     (Data : in out System.Address)
   is
      D : Local_Radio_Action_User_Data_Access := Convert (Data);
   begin
      D.Destroy (D.User_Data);
      Unchecked_Free (D);
   end Local_Radio_Action_Destroy;

   -----------------------
   -- Add_Radio_Actions --
   -----------------------

   procedure Add_Radio_Actions
     (Action_Group : access Gtk_Action_Group_Record;
      Entries      : Radio_Action_Entry_Array;
      Value        : Glib.Gint;
      On_Change    : Radio_Action_Callback;
      User_Data    : System.Address := System.Null_Address;
      Destroy      : Glib.G_Destroy_Notify_Address := null)
   is
      procedure Internal
        (Action_Group : System.Address;
         Entries      : System.Address;
         N_Entries    : Guint;
         Value        : Gint;
         On_Change    : System.Address;
         User_Data    : System.Address;
         Destroy      : System.Address);
      pragma Import (C, Internal, "gtk_action_group_add_radio_actions_full");
      Data : constant Local_Radio_Action_User_Data_Access :=
        new Local_Radio_Action_User_Data'
          (Callback  => On_Change,
           User_Data => User_Data,
           Destroy   => Destroy);
   begin
      Internal (Get_Object (Action_Group),
                Entries (Entries'First)'Address, Entries'Length,
                Value,
                Local_Radio_Action_Callback'Address,
                Convert (Data),
                Local_Radio_Action_Destroy'Address);
   end Add_Radio_Actions;

   ------------------------
   -- Add_Toggle_Actions --
   ------------------------

   procedure Add_Toggle_Actions
     (Action_Group : access Gtk_Action_Group_Record;
      Entries      : Toggle_Action_Entry_Array;
      User_Data    : System.Address := System.Null_Address;
      Destroy      : G_Destroy_Notify_Address := null)
   is
      procedure Internal
        (Action_Group : System.Address;
         Entries      : System.Address;
         N_Entries    : Guint;
         User_Data    : System.Address;
         Destroy      : G_Destroy_Notify_Address);
      pragma Import (C, Internal, "gtk_action_group_add_toggle_actions_full");
   begin
      Internal (Get_Object (Action_Group),
                Entries (Entries'First)'Address, Entries'Length, User_Data,
                Destroy);
   end Add_Toggle_Actions;

   ----------------
   -- Get_Action --
   ----------------

   function Get_Action
     (Action_Group : access Gtk_Action_Group_Record;
      Action_Name  : String)
      return Gtk_Action
   is
      function Internal
        (Action_Group : System.Address;
         Action_Name  : String)
         return System.Address;
      pragma Import (C, Internal, "gtk_action_group_get_action");
      Stub : Gtk_Action_Record;
   begin
      return Gtk_Action
        (Get_User_Data
           (Internal (Get_Object (Action_Group), Action_Name & ASCII.NUL),
            Stub));
   end Get_Action;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Action_Group : access Gtk_Action_Group_Record) return String
   is
      function Internal (Action_Group : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_action_group_get_name");
   begin
      --  Do not free returned value, it belongs to gtk+
      return Value (Internal (Get_Object (Action_Group)));
   end Get_Name;

   -------------------
   -- Get_Sensitive --
   -------------------

   function Get_Sensitive
     (Action_Group : access Gtk_Action_Group_Record) return Boolean
   is
      function Internal (Action_Group : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_action_group_get_sensitive");
   begin
      return Boolean'Val (Internal (Get_Object (Action_Group)));
   end Get_Sensitive;

   -----------------
   -- Get_Visible --
   -----------------

   function Get_Visible
     (Action_Group : access Gtk_Action_Group_Record) return Boolean
   is
      function Internal (Action_Group : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_action_group_get_visible");
   begin
      return Boolean'Val (Internal (Get_Object (Action_Group)));
   end Get_Visible;

   ------------------
   -- List_Actions --
   ------------------

   function List_Actions
     (Action_Group : access Gtk_Action_Group_Record)
      return Object_Simple_List.Glist
   is
      function Internal (Action_Group : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_action_group_list_actions");
      L : Object_Simple_List.Glist;
   begin
      Object_Simple_List.Set_Object
        (L, Internal (Get_Object (Action_Group)));
      return L;
   end List_Actions;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Group : out Gtk_Action_Group; Name : String) is
   begin
      Group := new Gtk_Action_Group_Record;
      Initialize (Group, Name);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Group : access Gtk_Action_Group_Record'Class;
      Name  : String)
   is
      function Internal (Name : String) return System.Address;
      pragma Import (C, Internal, "gtk_action_group_new");
   begin
      Set_Object (Group, Internal (Name & ASCII.NUL));
   end Initialize;

   -------------------
   -- Remove_Action --
   -------------------

   procedure Remove_Action
     (Action_Group : access Gtk_Action_Group_Record;
      Action       : access Gtk_Action_Record'Class)
   is
      procedure Internal
        (Action_Group : System.Address;
         Action       : System.Address);
      pragma Import (C, Internal, "gtk_action_group_remove_action");
   begin
      Internal (Get_Object (Action_Group), Get_Object (Action));
   end Remove_Action;

   -------------------
   -- Set_Sensitive --
   -------------------

   procedure Set_Sensitive
     (Action_Group : access Gtk_Action_Group_Record;
      Sensitive    : Boolean)
   is
      procedure Internal
        (Action_Group : System.Address;
         Sensitive    : Gboolean);
      pragma Import (C, Internal, "gtk_action_group_set_sensitive");
   begin
      Internal (Get_Object (Action_Group), Boolean'Pos (Sensitive));
   end Set_Sensitive;

   ----------------------------
   -- Set_Translation_Domain --
   ----------------------------

   procedure Set_Translation_Domain
     (Action_Group : access Gtk_Action_Group_Record;
      Domain       : String)
   is
      procedure Internal
        (Action_Group : System.Address;
         Domain       : String);
      pragma Import (C, Internal, "gtk_action_group_set_translation_domain");
   begin
      Internal (Get_Object (Action_Group), Domain & ASCII.NUL);
   end Set_Translation_Domain;

   -----------------
   -- Set_Visible --
   -----------------

   procedure Set_Visible
     (Action_Group : access Gtk_Action_Group_Record;
      Visible      : Boolean)
   is
      procedure Internal
        (Action_Group : System.Address;
         Visible      : Gboolean);
      pragma Import (C, Internal, "gtk_action_group_set_visible");
   begin
      Internal (Get_Object (Action_Group), Boolean'Pos (Visible));
   end Set_Visible;

end Gtk.Action_Group;
