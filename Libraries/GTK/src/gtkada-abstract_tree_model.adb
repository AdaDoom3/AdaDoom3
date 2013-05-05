-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                  Copyright (C) 2008-2013, AdaCore                 --
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
with System;

with Glib.Object;

package body Gtkada.Abstract_Tree_Model is

   use type Gtk.Tree_Model.Gtk_Tree_Iter;

   type Gtk_Tree_Iter_Access is access all Gtk.Tree_Model.Gtk_Tree_Iter;
   pragma Convention (C, Gtk_Tree_Iter_Access);

   type GInterface_Info is record
      interface_init     : System.Address := System.Null_Address;
      interface_finalize : System.Address := System.Null_Address;
      interface_data     : System.Address := System.Null_Address;
   end record;
   pragma Convention (C, GInterface_Info);

   type GTypeInterface is record
      g_type          : Glib.GType;
      g_instance_type : Glib.GType;
   end record;
   pragma Convention (C, GTypeInterface);

   type GtkTreeModelInterface is record
      g_type                : GTypeInterface;
      row_changed           : System.Address := System.Null_Address;
      row_inserted          : System.Address := System.Null_Address;
      row_has_child_toggled : System.Address := System.Null_Address;
      row_deleted           : System.Address := System.Null_Address;
      rows_reordered        : System.Address := System.Null_Address;
      get_flags             : System.Address := System.Null_Address;
      get_n_columns         : System.Address := System.Null_Address;
      get_column_type       : System.Address := System.Null_Address;
      get_iter              : System.Address := System.Null_Address;
      get_path              : System.Address := System.Null_Address;
      get_value             : System.Address := System.Null_Address;
      iter_next             : System.Address := System.Null_Address;
      iter_children         : System.Address := System.Null_Address;
      iter_has_child        : System.Address := System.Null_Address;
      iter_n_children       : System.Address := System.Null_Address;
      iter_nth_child        : System.Address := System.Null_Address;
      iter_parent           : System.Address := System.Null_Address;
      ref_node              : System.Address := System.Null_Address;
      unref_node            : System.Address := System.Null_Address;
   end record;
   pragma Convention (C, GtkTreeModelInterface);

   procedure Tree_Model_Interface_Init (Iface : in out GtkTreeModelInterface);
   pragma Convention (C, Tree_Model_Interface_Init);

   function Dispatch_Get_Flags
     (Tree_Model : System.Address) return Gtk.Tree_Model.Tree_Model_Flags;
   pragma Convention (C, Dispatch_Get_Flags);

   function Dispatch_Get_N_Columns
     (Tree_Model : System.Address) return Glib.Gint;
   pragma Convention (C, Dispatch_Get_N_Columns);

   function Dispatch_Get_Column_Type
     (Tree_Model : System.Address;
      Index      : Glib.Gint) return Glib.GType;
   pragma Convention (C, Dispatch_Get_Column_Type);

   function Dispatch_Get_Iter
     (Tree_Model : System.Address;
      Iter       : access Gtk.Tree_Model.Gtk_Tree_Iter;
      Path       : Gtk.Tree_Model.Gtk_Tree_Path) return Glib.Gboolean;
   pragma Convention (C, Dispatch_Get_Iter);

   function Dispatch_Get_Path
     (Tree_Model : System.Address;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Path;
   pragma Convention (C, Dispatch_Get_Path);

   procedure Dispatch_Get_Value
     (Tree_Model : System.Address;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Glib.Gint;
      Value      : out Glib.Values.GValue);
   pragma Convention (C, Dispatch_Get_Value);

   function Dispatch_Iter_Next
     (Tree_Model : System.Address;
      Iter       : access Gtk.Tree_Model.Gtk_Tree_Iter) return Glib.Gboolean;
   pragma Convention (C, Dispatch_Iter_Next);

   function Dispatch_Iter_Children
     (Tree_Model : System.Address;
      Iter       : access Gtk.Tree_Model.Gtk_Tree_Iter;
      Parent     : Gtk_Tree_Iter_Access)
      return Glib.Gboolean;
   pragma Convention (C, Dispatch_Iter_Children);

   function Dispatch_Iter_Has_Child
     (Tree_Model : System.Address;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Glib.Gboolean;
   pragma Convention (C, Dispatch_Iter_Has_Child);

   function Dispatch_Iter_N_Children
     (Tree_Model : System.Address;
      Iter       : Gtk_Tree_Iter_Access)
      return Glib.Gint;
   pragma Convention (C, Dispatch_Iter_N_Children);

   function Dispatch_Iter_Nth_Child
     (Tree_Model : System.Address;
      Iter       : access Gtk.Tree_Model.Gtk_Tree_Iter;
      Parent     : Gtk_Tree_Iter_Access;
      N          : Glib.Gint) return Glib.Gboolean;
   pragma Convention (C, Dispatch_Iter_Nth_Child);

   function Dispatch_Iter_Parent
     (Tree_Model : System.Address;
      Iter       : access Gtk.Tree_Model.Gtk_Tree_Iter;
      Child      : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Glib.Gboolean;
   pragma Convention (C, Dispatch_Iter_Parent);

   procedure Dispatch_Ref_Node
     (Tree_Model : System.Address;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Convention (C, Dispatch_Ref_Node);

   procedure Dispatch_Unref_Node
     (Tree_Model : System.Address;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter);
   pragma Convention (C, Dispatch_Unref_Node);

   Class_Record : Glib.Object.GObject_Class := Glib.Object.Uninitialized_Class;
   Info         : constant GInterface_Info  :=
     (interface_init => Tree_Model_Interface_Init'Address,
      others         => System.Null_Address);

   ------------------------------
   -- Dispatch_Get_Column_Type --
   ------------------------------

   function Dispatch_Get_Column_Type
     (Tree_Model : System.Address;
      Index      : Glib.Gint)
      return Glib.GType
   is
      Stub : Gtk.Tree_Model.Gtk_Tree_Model_Record;

   begin
      return
        Gtk.Tree_Model.Get_Column_Type
          (Gtk.Tree_Model.Gtk_Tree_Model
               (Glib.Object.Get_User_Data (Tree_Model, Stub)),
           Index);
   end Dispatch_Get_Column_Type;

   ------------------------
   -- Dispatch_Get_Flags --
   ------------------------

   function Dispatch_Get_Flags
     (Tree_Model : System.Address)
      return Gtk.Tree_Model.Tree_Model_Flags
   is
      Stub : Gtk.Tree_Model.Gtk_Tree_Model_Record;

   begin
      return
        Gtk.Tree_Model.Get_Flags
          (Gtk.Tree_Model.Gtk_Tree_Model
               (Glib.Object.Get_User_Data (Tree_Model, Stub)));
   end Dispatch_Get_Flags;

   -----------------------
   -- Dispatch_Get_Iter --
   -----------------------

   function Dispatch_Get_Iter
     (Tree_Model : System.Address;
      Iter       : access Gtk.Tree_Model.Gtk_Tree_Iter;
      Path       : Gtk.Tree_Model.Gtk_Tree_Path) return Glib.Gboolean
   is
      Stub : Gtk.Tree_Model.Gtk_Tree_Model_Record;

   begin
      Iter.all :=
        Gtk.Tree_Model.Get_Iter
          (Gtk.Tree_Model.Gtk_Tree_Model
               (Glib.Object.Get_User_Data (Tree_Model, Stub)),
           Path);

      if Iter.all = Gtk.Tree_Model.Null_Iter then
         return 0;

      else
         return 1;
      end if;
   end Dispatch_Get_Iter;

   -----------------------
   -- Dispatch_Get_Path --
   -----------------------

   function Dispatch_Get_Path
     (Tree_Model : System.Address;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Path
   is
      Stub : Gtk.Tree_Model.Gtk_Tree_Model_Record;

   begin
      return
        Gtk.Tree_Model.Get_Path
          (Gtk.Tree_Model.Gtk_Tree_Model
               (Glib.Object.Get_User_Data (Tree_Model, Stub)),
           Iter);
   end Dispatch_Get_Path;

   ----------------------------
   -- Dispatch_Get_N_Columns --
   ----------------------------

   function Dispatch_Get_N_Columns
     (Tree_Model : System.Address)
      return Glib.Gint
   is
      Stub : Gtk.Tree_Model.Gtk_Tree_Model_Record;

   begin
      return
        Gtk.Tree_Model.Get_N_Columns
          (Gtk.Tree_Model.Gtk_Tree_Model
               (Glib.Object.Get_User_Data (Tree_Model, Stub)));
   end Dispatch_Get_N_Columns;

   ------------------------
   -- Dispatch_Get_Value --
   ------------------------

   procedure Dispatch_Get_Value
     (Tree_Model : System.Address;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Glib.Gint;
      Value      : out Glib.Values.GValue)
   is
      Stub : Gtk.Tree_Model.Gtk_Tree_Model_Record;

   begin
      Gtk.Tree_Model.Get_Value
        (Gtk.Tree_Model.Gtk_Tree_Model
           (Glib.Object.Get_User_Data (Tree_Model, Stub)),
         Iter,
         Column,
         Value);
   end Dispatch_Get_Value;

   ----------------------------
   -- Dispatch_Iter_Children --
   ----------------------------

   function Dispatch_Iter_Children
     (Tree_Model : System.Address;
      Iter       : access Gtk.Tree_Model.Gtk_Tree_Iter;
      Parent     : Gtk_Tree_Iter_Access)
      return Glib.Gboolean
   is
      Stub : Gtk.Tree_Model.Gtk_Tree_Model_Record;
      Real_Parent : Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      --  Gtk should normally never pass null to that function. Unfortunately,
      --  When using e.g. PyGtk, Parent may be null, probably due to a bug.
      --  Using a null parent and passing Null_Iter in this case is a
      --  workaround to that problem.
      if Parent /= null then
         Real_Parent := Parent.all;
      else
         Real_Parent := Gtk.Tree_Model.Null_Iter;
      end if;

      Iter.all :=
        Gtk.Tree_Model.Children
          (Gtk.Tree_Model.Gtk_Tree_Model
               (Glib.Object.Get_User_Data (Tree_Model, Stub)),
           Real_Parent);

      if Iter.all = Gtk.Tree_Model.Null_Iter then
         return 0;

      else
         return 1;
      end if;
   end Dispatch_Iter_Children;

   -----------------------------
   -- Dispatch_Iter_Has_Child --
   -----------------------------

   function Dispatch_Iter_Has_Child
     (Tree_Model : System.Address;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Glib.Gboolean
   is
      Stub : Gtk.Tree_Model.Gtk_Tree_Model_Record;

   begin
      if Gtk.Tree_Model.Has_Child
        (Gtk.Tree_Model.Gtk_Tree_Model
           (Glib.Object.Get_User_Data (Tree_Model, Stub)),
         Iter)
      then
         return 1;

      else
         return 0;
      end if;
   end Dispatch_Iter_Has_Child;

   ------------------------------
   -- Dispatch_Iter_N_Children --
   ------------------------------

   function Dispatch_Iter_N_Children
     (Tree_Model : System.Address;
      Iter       : Gtk_Tree_Iter_Access)
      return Glib.Gint
   is
      Stub : Gtk.Tree_Model.Gtk_Tree_Model_Record;

   begin
      if Iter = null then
         return
           Gtk.Tree_Model.N_Children
             (Gtk.Tree_Model.Gtk_Tree_Model
                  (Glib.Object.Get_User_Data (Tree_Model, Stub)),
              Gtk.Tree_Model.Null_Iter);

      else
         return
           Gtk.Tree_Model.N_Children
             (Gtk.Tree_Model.Gtk_Tree_Model
                  (Glib.Object.Get_User_Data (Tree_Model, Stub)),
              Iter.all);
      end if;
   end Dispatch_Iter_N_Children;

   ------------------------
   -- Dispatch_Iter_Next --
   ------------------------

   function Dispatch_Iter_Next
     (Tree_Model : System.Address;
      Iter       : access Gtk.Tree_Model.Gtk_Tree_Iter) return Glib.Gboolean
   is
      Stub : Gtk.Tree_Model.Gtk_Tree_Model_Record;

   begin
      Gtk.Tree_Model.Next
        (Gtk.Tree_Model.Gtk_Tree_Model
           (Glib.Object.Get_User_Data (Tree_Model, Stub)),
         Iter.all);

      if Iter.all = Gtk.Tree_Model.Null_Iter then
         return 0;

      else
         return 1;
      end if;
   end Dispatch_Iter_Next;

   -----------------------------
   -- Dispatch_Iter_Nth_Child --
   -----------------------------

   function Dispatch_Iter_Nth_Child
     (Tree_Model : System.Address;
      Iter       : access Gtk.Tree_Model.Gtk_Tree_Iter;
      Parent     : Gtk_Tree_Iter_Access;
      N          : Glib.Gint) return Glib.Gboolean
   is
      Stub : Gtk.Tree_Model.Gtk_Tree_Model_Record;

   begin
      --  Parent can be null, GtkAda assume it can't.

      if Parent = null then
         Iter.all :=
           Gtk.Tree_Model.Nth_Child
             (Gtk.Tree_Model.Gtk_Tree_Model
                  (Glib.Object.Get_User_Data (Tree_Model, Stub)),
              Gtk.Tree_Model.Null_Iter,
              N);

      else
         Iter.all :=
           Gtk.Tree_Model.Nth_Child
             (Gtk.Tree_Model.Gtk_Tree_Model
                  (Glib.Object.Get_User_Data (Tree_Model, Stub)),
              Parent.all,
              N);
      end if;

      if Iter.all = Gtk.Tree_Model.Null_Iter then
         return 0;

      else
         return 1;
      end if;
   end Dispatch_Iter_Nth_Child;

   --------------------------
   -- Dispatch_Iter_Parent --
   --------------------------

   function Dispatch_Iter_Parent
     (Tree_Model : System.Address;
      Iter       : access Gtk.Tree_Model.Gtk_Tree_Iter;
      Child      : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Glib.Gboolean
   is
      Stub : Gtk.Tree_Model.Gtk_Tree_Model_Record;

   begin
      Iter.all :=
        Gtk.Tree_Model.Parent
          (Gtk.Tree_Model.Gtk_Tree_Model
               (Glib.Object.Get_User_Data (Tree_Model, Stub)),
           Child);

      if Iter.all = Gtk.Tree_Model.Null_Iter then
         return 0;

      else
         return 1;
      end if;
   end Dispatch_Iter_Parent;

   -----------------------
   -- Dispatch_Ref_Node --
   -----------------------

   procedure Dispatch_Ref_Node
     (Tree_Model : System.Address;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      Stub : Gtk.Tree_Model.Gtk_Tree_Model_Record;

   begin
      Gtk.Tree_Model.Ref_Node
        (Gtk.Tree_Model.Gtk_Tree_Model
           (Glib.Object.Get_User_Data (Tree_Model, Stub)),
         Iter);
   end Dispatch_Ref_Node;

   -------------------------
   -- Dispatch_Unref_Node --
   -------------------------

   procedure Dispatch_Unref_Node
     (Tree_Model : System.Address;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      Stub : Gtk.Tree_Model.Gtk_Tree_Model_Record;

   begin
      Gtk.Tree_Model.Unref_Node
        (Gtk.Tree_Model.Gtk_Tree_Model
           (Glib.Object.Get_User_Data (Tree_Model, Stub)),
         Iter);
   end Dispatch_Unref_Node;

   ---------------
   -- Get_Flags --
   ---------------

   function Get_Flags
     (Self : access Gtk_Abstract_Tree_Model_Record)
      return Gtk.Tree_Model.Tree_Model_Flags
   is
      pragma Unreferenced (Self);
      --  This parameter is used for call dispatching.

   begin
      return 0;
   end Get_Flags;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : access Gtk_Abstract_Tree_Model_Record'Class) is
      use type Glib.Object.GObject_Class;
      Empty : Interfaces.C.Strings.chars_ptr_array (1 .. 0);

      procedure Init_Interface
        (Class : Glib.GType;
         Iface : Glib.GType;
         Info  : GInterface_Info);
      pragma Import (C, Init_Interface, "g_type_add_interface_static");

      Initialized : constant Boolean :=
        Class_Record /= Glib.Object.Uninitialized_Class;

   begin
      Glib.Object.Initialize (Self);

      Glib.Object.Initialize_Class_Record
        (Self,
         Empty,
         Class_Record,
         "GtkAdaAbstractTreeModel");

      if not Initialized then
         Init_Interface
           (Glib.Object.Type_From_Class (Class_Record),
            Gtk.Tree_Model.Get_Type,
            Info);
      end if;
   end Initialize;

   --------------
   -- Ref_Node --
   --------------

   procedure Ref_Node
     (Self : access Gtk_Abstract_Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
   begin
      null;
   end Ref_Node;

   -------------------------------
   -- Tree_Model_Interface_Init --
   -------------------------------

   procedure Tree_Model_Interface_Init
     (Iface : in out GtkTreeModelInterface)
   is
   begin
      Iface.get_flags       := Dispatch_Get_Flags'Address;
      Iface.get_n_columns   := Dispatch_Get_N_Columns'Address;
      Iface.get_column_type := Dispatch_Get_Column_Type'Address;
      Iface.get_iter        := Dispatch_Get_Iter'Address;
      Iface.get_path        := Dispatch_Get_Path'Address;
      Iface.get_value       := Dispatch_Get_Value'Address;
      Iface.iter_next       := Dispatch_Iter_Next'Address;
      Iface.iter_children   := Dispatch_Iter_Children'Address;
      Iface.iter_has_child  := Dispatch_Iter_Has_Child'Address;
      Iface.iter_n_children := Dispatch_Iter_N_Children'Address;
      Iface.iter_nth_child  := Dispatch_Iter_Nth_Child'Address;
      Iface.iter_parent     := Dispatch_Iter_Parent'Address;
      Iface.ref_node        := Dispatch_Ref_Node'Address;
      Iface.unref_node      := Dispatch_Unref_Node'Address;
   end Tree_Model_Interface_Init;

   ----------------
   -- Unref_Node --
   ----------------

   procedure Unref_Node
     (Self : access Gtk_Abstract_Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
   begin
      null;
   end Unref_Node;

end Gtkada.Abstract_Tree_Model;
