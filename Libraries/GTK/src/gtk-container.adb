-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                 Copyright (C) 2000-2013, AdaCore                  --
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

with Gtkada.Bindings; use Gtkada.Bindings;
with System;          use System;
with Glib.Values;     use Glib.Values;
with Gtk.Adjustment;  use Gtk.Adjustment;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Widget;      use Gtk.Widget;

with Glib.Type_Conversion_Hooks;

package body Gtk.Container is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Container_Record);
   pragma Warnings (Off, Type_Conversion);

   procedure Internal_Gtk_Callback
     (Widget : System.Address; Data : Gtk_Callback);
   pragma Convention (C, Internal_Gtk_Callback);
   --  Proxy for a Gtk callback

   ---------
   -- Add --
   ---------

   procedure Add
     (Container : access Gtk_Container_Record;
      Widget    : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
        (Container : System.Address;
         Widget    : System.Address);
      pragma Import (C, Internal, "gtk_container_add");

   begin
      Internal (Get_Object (Container), Get_Object (Widget));
   end Add;

   ------------------
   -- Check_Resize --
   ------------------

   procedure Check_Resize (Container : access Gtk_Container_Record) is
      procedure Internal (Container : System.Address);
      pragma Import (C, Internal, "gtk_container_check_resize");

   begin
      Internal (Get_Object (Container));
   end Check_Resize;

   ----------------
   -- Child_Type --
   ----------------

   function Child_Type
     (Container : access Gtk_Container_Record) return Gtk.Gtk_Type
   is
      function Internal (Container : System.Address) return Gtk.Gtk_Type;
      pragma Import (C, Internal, "gtk_container_child_type");

   begin
      return Internal (Get_Object (Container));
   end Child_Type;

   ---------------------------
   -- Internal_Gtk_Callback --
   ---------------------------

   procedure Internal_Gtk_Callback
     (Widget : System.Address;
      Data   : Gtk_Callback)
   is
      Stub : Gtk_Widget_Record;
   begin
      Data (Gtk_Widget (Get_User_Data (Widget, Stub)));
   end Internal_Gtk_Callback;

   ------------
   -- Forall --
   ------------

   procedure Forall
     (Container : access Gtk_Container_Record;
      Func      : Gtk_Callback)
   is
      procedure Internal
        (Container : System.Address;
         Func      : System.Address;
         Data      : System.Address);
      pragma Import (C, Internal, "gtk_container_forall");
   begin
      Internal
        (Get_Object (Container), Internal_Gtk_Callback'Address,
         Func.all'Address);
   end Forall;

   -------------
   -- Foreach --
   -------------

   procedure Foreach
     (Container : access Gtk_Container_Record;
      Func      : Gtk_Callback)
   is
      procedure Internal
        (Container : System.Address;
         Func      : System.Address;
         Data      : System.Address);
      pragma Import (C, Internal, "gtk_container_foreach");
   begin
      Internal
        (Get_Object (Container), Internal_Gtk_Callback'Address,
         Func.all'Address);
   end Foreach;

   -------------
   -- For_Pkg --
   -------------

   package body For_Pkg is

      type Data_Type_Access is access all Data_Type;

      type Internal_Data is record
         Data : Data_Type_Access;
         Func : Gtk_Callback;
      end record;
      type Internal_Data_Access is access all Internal_Data;

      procedure Internal_Func
        (Widget : System.Address; Data : Internal_Data_Access);
      pragma Convention (C, Internal_Func);

      -------------------
      -- Internal_Func --
      -------------------

      procedure Internal_Func
        (Widget : System.Address; Data : Internal_Data_Access)
      is
         Stub : Gtk_Widget_Record;
      begin
         Data.Func (Gtk_Widget (Get_User_Data (Widget, Stub)), Data.Data.all);
      end Internal_Func;

      ------------
      -- Forall --
      ------------

      procedure Forall
        (Container : access Gtk_Container_Record;
         Func      : Gtk_Callback;
         Data      : Data_Type)
      is
         procedure Internal
           (Container : System.Address;
            Func      : System.Address;
            Data      : System.Address);
         pragma Import (C, Internal, "gtk_container_forall");
         Local : aliased Data_Type := Data;
         D : aliased Internal_Data := (Local'Unchecked_Access, Func);
      begin
         Internal (Get_Object (Container), Internal_Func'Address, D'Address);
      end Forall;

      -------------
      -- Foreach --
      -------------

      procedure Foreach
        (Container : access Gtk_Container_Record;
         Func      : Gtk_Callback;
         Data      : Data_Type)
      is
         procedure Internal
           (Container : System.Address;
            Func      : System.Address;
            Data      : System.Address);
         pragma Import (C, Internal, "gtk_container_foreach");
         Local : aliased Data_Type := Data;
         D : aliased Internal_Data := (Local'Unchecked_Access, Func);
      begin
         Internal (Get_Object (Container), Internal_Func'Address, D'Address);
      end Foreach;
   end For_Pkg;

   ----------------------
   -- Get_Border_Width --
   ----------------------

   function Get_Border_Width
     (Container : access Gtk_Container_Record) return Guint
   is
      function Internal (Container : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_container_get_border_width");

   begin
      return Internal (Get_Object (Container));
   end Get_Border_Width;

   ------------------
   -- Get_Children --
   ------------------

   function Get_Children
     (Container : access Gtk_Container_Record)
      return Gtk.Widget.Widget_List.Glist
   is
      function Internal (Container : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_container_get_children");

      List : Gtk.Widget.Widget_List.Glist;

   begin
      Gtk.Widget.Widget_List.Set_Object
        (List, Internal (Get_Object (Container)));
      return List;
   end Get_Children;

   ---------------------
   -- Get_Resize_Mode --
   ---------------------

   function Get_Resize_Mode
     (Container : access Gtk_Container_Record)
      return Gtk_Resize_Mode
   is
      function Internal (Container : System.Address) return Gtk_Resize_Mode;
      pragma Import (C, Internal, "gtk_container_get_resize_mode");

   begin
      return Internal (Get_Object (Container));
   end Get_Resize_Mode;

   ----------------------
   -- Propagate_Expose --
   ----------------------

   procedure Propagate_Expose
     (Container : access Gtk_Container_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event     : Gdk.Event.Gdk_Event_Expose)
   is
      procedure Internal
        (Container : System.Address;
         Child     : System.Address;
         Event     : Gdk.Event.Gdk_Event_Expose);
      pragma Import (C, Internal, "gtk_container_propagate_expose");

   begin
      Internal (Get_Object (Container), Get_Object (Child), Event);
   end Propagate_Expose;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Container : access Gtk_Container_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Container : System.Address; Widget : System.Address);
      pragma Import (C, Internal, "gtk_container_remove");

   begin
      Internal (Get_Object (Container), Get_Object (Widget));
   end Remove;

   ---------------------
   -- Resize_Children --
   ---------------------

   procedure Resize_Children (Container : access Gtk_Container_Record) is
      procedure Internal (Container : System.Address);
      pragma Import (C, Internal, "gtk_container_resize_children");

   begin
      Internal (Get_Object (Container));
   end Resize_Children;

   ----------------------
   -- Set_Border_Width --
   ----------------------

   procedure Set_Border_Width
     (Container    : access Gtk_Container_Record;
      Border_Width : Guint)
   is
      procedure Internal
        (Container     : System.Address;
         Border_Widget : Guint);
      pragma Import (C, Internal, "gtk_container_set_border_width");

   begin
      Internal (Get_Object (Container), Border_Width);
   end Set_Border_Width;

   ---------------------
   -- Set_Focus_Chain --
   ---------------------

   procedure Set_Focus_Chain
     (Container         : access Gtk_Container_Record;
      Focusable_Widgets : Gtk.Widget.Widget_List.Glist)
   is
      procedure Internal
        (Container         : System.Address;
         Focusable_Widgets : System.Address);
      pragma Import (C, Internal, "gtk_container_set_focus_chain");

      use Gtk.Widget.Widget_List;

   begin
      Internal (Get_Object (Container), Get_Object (Focusable_Widgets));
   end Set_Focus_Chain;

   ---------------------
   -- Get_Focus_Chain --
   ---------------------

   procedure Get_Focus_Chain
     (Container         : access Gtk_Container_Record;
      Focusable_Widgets : out Gtk.Widget.Widget_List.Glist;
      Success           : out Boolean)
   is
      use Widget_List;
      function Internal
        (Container : System.Address;
         List      : access System.Address)
         return Gboolean;
      pragma Import (C, Internal, "gtk_container_get_focus_chain");
      L : aliased System.Address;
   begin
      Success := Boolean'Val (Internal (Get_Object (Container), L'Access));
      Set_Object (Focusable_Widgets, L);
   end Get_Focus_Chain;

   ---------------------
   -- Set_Focus_Child --
   ---------------------

   procedure Set_Focus_Child
     (Container : access Gtk_Container_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
        (Container : System.Address;
         Child     : System.Address);
      pragma Import (C, Internal, "gtk_container_set_focus_child");

   begin
      Internal (Get_Object (Container), Get_Object (Child));
   end Set_Focus_Child;

   ---------------------------
   -- Set_Focus_Hadjustment --
   ---------------------------

   procedure Set_Focus_Hadjustment
     (Container  : access Gtk_Container_Record;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment)
   is
      procedure Internal
        (Container  : System.Address;
         Adjustment : System.Address);
      pragma Import (C, Internal, "gtk_container_set_focus_hadjustment");

   begin
      if Adjustment = null then
         Internal (Get_Object (Container), System.Null_Address);
      else
         Internal (Get_Object (Container), Get_Object (Adjustment));
      end if;
   end Set_Focus_Hadjustment;

   ---------------------------
   -- Set_Focus_Vadjustment --
   ---------------------------

   procedure Set_Focus_Vadjustment
     (Container  : access Gtk_Container_Record;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment)
   is
      procedure Internal
        (Container  : System.Address;
         Adjustment : System.Address);
      pragma Import (C, Internal, "gtk_container_set_focus_vadjustment");

   begin
      if Adjustment = null then
         Internal (Get_Object (Container), System.Null_Address);
      else
         Internal (Get_Object (Container), Get_Object (Adjustment));
      end if;
   end Set_Focus_Vadjustment;

   ----------------------------
   -- Set_Reallocate_Redraws --
   ----------------------------

   procedure Set_Reallocate_Redraws
     (Container : access Gtk_Container_Record;
      Needs_Redraws : Boolean := False)
   is
      procedure Internal
        (Container     : System.Address;
         Needs_Redraws : Gint);
      pragma Import (C, Internal, "gtk_container_set_reallocate_redraws");

   begin
      Internal (Get_Object (Container), Boolean'Pos (Needs_Redraws));
   end Set_Reallocate_Redraws;

   ---------------------
   -- Set_Resize_Mode --
   ---------------------

   procedure Set_Resize_Mode
     (Container   : access Gtk_Container_Record;
      Resize_Mode : Gtk_Resize_Mode)
   is
      procedure Internal
        (Container : System.Address; Mode : Gtk_Resize_Mode);
      pragma Import (C, Internal, "gtk_container_set_resize_mode");

   begin
      Internal (Get_Object (Container), Resize_Mode);
   end Set_Resize_Mode;

   -----------------------
   -- Unset_Focus_Chain --
   -----------------------

   procedure Unset_Focus_Chain (Container : access Gtk_Container_Record) is
      procedure Internal (Container : System.Address);
      pragma Import (C, Internal, "gtk_container_unset_focus_chain");

   begin
      Internal (Get_Object (Container));
   end Unset_Focus_Chain;

   ---------------------
   -- Get_Focus_Child --
   ---------------------

   function Get_Focus_Child
     (Container : access Gtk_Container_Record) return Gtk.Widget.Gtk_Widget
   is
      function Internal (Container : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_container_get_focus_child");

      Child : constant System.Address := Internal (Get_Object (Container));
   begin
      if Child = System.Null_Address then
         return null;
      else
         return Convert (Child);
      end if;
   end Get_Focus_Child;

   ---------------------------
   -- Get_Focus_Hadjustment --
   ---------------------------

   function Get_Focus_Hadjustment
     (Container : access Gtk_Container_Record)
      return Gtk_Adjustment
   is
      function Internal (Container : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_container_get_focus_hadjustment");
      Stub : Gtk_Adjustment_Record;
   begin
      return Gtk_Adjustment
        (Get_User_Data (Internal (Get_Object (Container)), Stub));
   end Get_Focus_Hadjustment;

   ---------------------------
   -- Get_Focus_Vadjustment --
   ---------------------------

   function Get_Focus_Vadjustment
     (Container : access Gtk_Container_Record)
      return Gtk_Adjustment
   is
      function Internal (Container : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_container_get_focus_vadjustment");
      Stub : Gtk_Adjustment_Record;
   begin
      return Gtk_Adjustment
        (Get_User_Data (Internal (Get_Object (Container)), Stub));
   end Get_Focus_Vadjustment;

   -------------------------------
   -- Class_Find_Child_Property --
   -------------------------------

   function Class_Find_Child_Property
     (Cclass        : GObject_Class;
      Property_Name : String)
      return Glib.Param_Spec
   is
      function Internal
        (Cclass        : GObject_Class;
         Property_Name : String)
         return Param_Spec;
      pragma Import (C, Internal, "gtk_container_class_find_child_property");
   begin
      return Internal (Cclass, Property_Name & ASCII.NUL);
   end Class_Find_Child_Property;

   ----------------------------------
   -- Class_Install_Child_Property --
   ----------------------------------

   procedure Class_Install_Child_Property
     (Cclass      : GObject_Class;
      Property_Id : Guint;
      Pspec       : Param_Spec)
   is
      procedure Internal
        (Cclass      : GObject_Class;
         Property_Id : Guint;
         Pspec       : Param_Spec);
      pragma Import
        (C, Internal, "gtk_container_class_install_child_property");
   begin
      Internal (Cclass, Property_Id, Pspec);
   end Class_Install_Child_Property;

   ------------------------
   -- Child_Get_Property --
   ------------------------

   procedure Child_Get_Property
     (Container     : access Gtk_Container_Record;
      Child         : access Gtk_Widget_Record'Class;
      Property_Name : String;
      Value         : out GValue)
   is
      procedure Internal
        (Container     : System.Address;
         Child         : System.Address;
         Property_Name : String;
         Value         : out GValue);
      pragma Import (C, Internal, "gtk_container_child_get_property");
   begin
      Internal (Get_Object (Container), Get_Object (Child),
                Property_Name & ASCII.NUL, Value);
   end Child_Get_Property;

   ------------------------
   -- Child_Set_Property --
   ------------------------

   procedure Child_Set_Property
     (Container     : access Gtk_Container_Record;
      Child         : access Gtk_Widget_Record'Class;
      Property_Name : String;
      Value         : GValue)
   is
      procedure Internal
        (Container     : System.Address;
         Child         : System.Address;
         Property_Name : String;
         Value         : GValue);
      pragma Import (C, Internal, "gtk_container_child_set_property");
   begin
      Internal (Get_Object (Container), Get_Object (Child),
                Property_Name & ASCII.NUL, Value);
   end Child_Set_Property;

   ---------------------------------
   -- Class_List_Child_Properties --
   ---------------------------------

   function Class_List_Child_Properties
     (Cclass : GObject_Class) return Param_Spec_Array
   is
      use Pspec_Arrays;
      function Internal
        (Cclass       : GObject_Class;
         N_Properties : access Guint) return Unbounded_Array_Access;
      pragma Import (C, Internal, "gtk_container_class_list_child_properties");

      Num     : aliased Guint;
      C_Array : constant Unbounded_Array_Access :=
        Internal (Cclass, Num'Access);
      Result  : constant Param_Spec_Array := To_Array (C_Array, Integer (Num));

   begin
      --  Do says we should free, but doing so results in double-deallocation
--      G_Free (C_Array);
      return Result;
   end Class_List_Child_Properties;

end Gtk.Container;
