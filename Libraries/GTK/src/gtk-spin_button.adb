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

with System;

with Glib.Type_Conversion_Hooks;

package body Gtk.Spin_Button is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Spin_Button_Record);
   pragma Warnings (Off, Type_Conversion);

   --------------------
   -- Get_Adjustment --
   --------------------

   function Get_Adjustment
     (Spin_Button : access Gtk_Spin_Button_Record)
      return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Spin_Button : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_spin_button_get_adjustment");

      Stub : Adjustment.Gtk_Adjustment_Record;

   begin
      return Gtk.Adjustment.Gtk_Adjustment
        (Get_User_Data (Internal (Get_Object (Spin_Button)), Stub));
   end Get_Adjustment;

   ----------------
   -- Get_Digits --
   ----------------

   function Get_Digits
     (Spin_Button : access Gtk_Spin_Button_Record) return Guint
   is
      function Internal
        (Spin_Button : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_spin_button_get_digits");

   begin
      return Internal (Get_Object (Spin_Button));
   end Get_Digits;

   --------------------
   -- Get_Increments --
   --------------------

   procedure Get_Increments
     (Spin_Button : access Gtk_Spin_Button_Record;
      Step        : out Gdouble;
      Page        : out Gdouble)
   is
      procedure Internal
        (Spin_Button : System.Address;
         Step        : out Gdouble;
         Page        : out Gdouble);
      pragma Import (C, Internal, "gtk_spin_button_get_increments");

   begin
      Internal (Get_Object (Spin_Button), Step, Page);
   end Get_Increments;

   ---------------
   -- Get_Range --
   ---------------

   procedure Get_Range
     (Spin_Button : access Gtk_Spin_Button_Record;
      Min         : out Gdouble;
      Max         : out Gdouble)
   is
      procedure Internal
        (Spin_Button : System.Address;
         Min         : out Gdouble;
         Max         : out Gdouble);
      pragma Import (C, Internal, "gtk_spin_button_get_range");

   begin
      Internal (Get_Object (Spin_Button), Min, Max);
   end Get_Range;

   -----------------------
   -- Get_Update_Policy --
   -----------------------

   function Get_Update_Policy
     (Spin_Button : access Gtk_Spin_Button_Record)
      return Gtk_Spin_Button_Update_Policy
   is
      function Internal
        (Spin_Button : System.Address) return Gtk_Spin_Button_Update_Policy;
      pragma Import (C, Internal, "gtk_spin_button_get_update_policy");

   begin
      return Internal (Get_Object (Spin_Button));
   end Get_Update_Policy;

   -----------------
   -- Get_Numeric --
   -----------------

   function Get_Numeric
     (Spin_Button : access Gtk_Spin_Button_Record) return Boolean
   is
      function Internal (Spin_Button : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_spin_button_get_numeric");

   begin
      return Internal (Get_Object (Spin_Button)) /= 0;
   end Get_Numeric;

   --------------
   -- Get_Wrap --
   --------------

   function Get_Wrap
     (Spin_Button : access Gtk_Spin_Button_Record) return Boolean
   is
      function Internal (Spin_Button : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_spin_button_get_wrap");

   begin
      return Internal (Get_Object (Spin_Button)) /= 0;
   end Get_Wrap;

   -----------------------
   -- Get_Snap_To_Ticks --
   -----------------------

   function Get_Snap_To_Ticks
     (Spin_Button : access Gtk_Spin_Button_Record) return Boolean
   is
      function Internal (Spin_Button : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_spin_button_get_snap_to_ticks");

   begin
      return Internal (Get_Object (Spin_Button)) /= 0;
   end Get_Snap_To_Ticks;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Spin_Button : access Gtk_Spin_Button_Record) return Gdouble
   is
      function Internal (Spin_Button : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_spin_button_get_value");

   begin
      return Internal (Get_Object (Spin_Button));
   end Get_Value;

   ----------------------
   -- Get_Value_As_Int --
   ----------------------

   function Get_Value_As_Int
     (Spin_Button : access Gtk_Spin_Button_Record) return Gint
   is
      function Internal (Spin_Button : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_spin_button_get_value_as_int");

   begin
      return Internal (Get_Object (Spin_Button));
   end Get_Value_As_Int;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Spin_Button : out Gtk_Spin_Button;
      Adjustment  : Gtk.Adjustment.Gtk_Adjustment;
      Climb_Rate  : Gdouble;
      The_Digits  : Gint) is
   begin
      Spin_Button := new Gtk_Spin_Button_Record;
      Initialize (Spin_Button, Adjustment, Climb_Rate, The_Digits);
   end Gtk_New;

   procedure Gtk_New
     (Spin_Button : out Gtk_Spin_Button;
      Min         : Gdouble;
      Max         : Gdouble;
      Step        : Gdouble) is
   begin
      Spin_Button := new Gtk_Spin_Button_Record;
      Initialize (Spin_Button, Min, Max, Step);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Spin_Button : access Gtk_Spin_Button_Record'Class;
      Adjustment  : Gtk.Adjustment.Gtk_Adjustment;
      Climb_Rate  : Gdouble;
      The_Digits  : Gint)
   is
      function Internal
        (Adjustment : System.Address;
         Climb_Rate : Gdouble;
         The_Digits : Gint) return System.Address;
      pragma Import (C, Internal, "gtk_spin_button_new");

      use type Gtk.Adjustment.Gtk_Adjustment;

   begin
      if Adjustment = null then
         Set_Object
           (Spin_Button,
            Internal (System.Null_Address, Climb_Rate, The_Digits));
      else
         Set_Object
           (Spin_Button,
            Internal (Get_Object (Adjustment), Climb_Rate, The_Digits));
      end if;
   end Initialize;

   procedure Initialize
     (Spin_Button : access Gtk_Spin_Button_Record'Class;
      Min         : Gdouble;
      Max         : Gdouble;
      Step        : Gdouble)
   is
      function Internal (Min, Max, Step : Gdouble) return System.Address;
      pragma Import (C, Internal, "gtk_spin_button_new_with_range");

   begin
      Set_Object (Spin_Button, Internal (Min, Max, Step));
   end Initialize;

   --------------------
   -- Set_Adjustment --
   --------------------

   procedure Set_Adjustment
     (Spin_Button : access Gtk_Spin_Button_Record;
      Adjustment  : Gtk.Adjustment.Gtk_Adjustment)
   is
      procedure Internal
        (Spin_Button : System.Address;
         Adjustment  : System.Address);
      pragma Import (C, Internal, "gtk_spin_button_set_adjustment");

      use type Gtk.Adjustment.Gtk_Adjustment;

   begin
      if Adjustment = null then
         Internal (Get_Object (Spin_Button), System.Null_Address);
      else
         Internal (Get_Object (Spin_Button), Get_Object (Adjustment));
      end if;
   end Set_Adjustment;

   ----------------
   -- Set_Digits --
   ----------------

   procedure Set_Digits
     (Spin_Button : access Gtk_Spin_Button_Record;
      The_Digits  : Guint)
   is
      procedure Internal
        (Spin_Button : System.Address;
         The_Digits  : Guint);
      pragma Import (C, Internal, "gtk_spin_button_set_digits");

   begin
      Internal (Get_Object (Spin_Button), The_Digits);
   end Set_Digits;

   --------------------
   -- Set_Increments --
   --------------------

   procedure Set_Increments
     (Spin_Button : access Gtk_Spin_Button_Record;
      Step        : Gdouble;
      Page        : Gdouble)
   is
      procedure Internal
        (Spin_Button : System.Address;
         Step        : Gdouble;
         Page        : Gdouble);
      pragma Import (C, Internal, "gtk_spin_button_set_increments");

   begin
      Internal (Get_Object (Spin_Button), Step, Page);
   end Set_Increments;

   -----------------
   -- Set_Numeric --
   -----------------

   procedure Set_Numeric
     (Spin_Button : access Gtk_Spin_Button_Record; Numeric : Boolean)
   is
      procedure Internal (Spin_Button : System.Address; Numeric : Gint);
      pragma Import (C, Internal, "gtk_spin_button_set_numeric");

   begin
      Internal (Get_Object (Spin_Button), Boolean'Pos (Numeric));
   end Set_Numeric;

   ---------------
   -- Set_Range --
   ---------------

   procedure Set_Range
     (Spin_Button : access Gtk_Spin_Button_Record;
      Min         : Gdouble;
      Max         : Gdouble)
   is
      procedure Internal
        (Spin_Button : System.Address;
         Min         : Gdouble;
         Max         : Gdouble);
      pragma Import (C, Internal, "gtk_spin_button_set_range");

   begin
      Internal (Get_Object (Spin_Button), Min, Max);
   end Set_Range;

   -----------------------
   -- Set_Snap_To_Ticks --
   -----------------------

   procedure Set_Snap_To_Ticks
    (Spin_Button   : access Gtk_Spin_Button_Record;
     Snap_To_Ticks : Boolean)
   is
      procedure Internal (Spin_Button : System.Address; Snap_To_Ticks : Gint);
      pragma Import (C, Internal, "gtk_spin_button_set_snap_to_ticks");

   begin
      Internal (Get_Object (Spin_Button), Boolean'Pos (Snap_To_Ticks));
   end Set_Snap_To_Ticks;

   -----------------------
   -- Set_Update_Policy --
   -----------------------

   procedure Set_Update_Policy
     (Spin_Button : access Gtk_Spin_Button_Record;
      Policy      : Gtk_Spin_Button_Update_Policy)
   is
      procedure Internal
        (Spin_Button : System.Address; Policy : Gtk_Spin_Button_Update_Policy);
      pragma Import (C, Internal, "gtk_spin_button_set_update_policy");

   begin
      Internal (Get_Object (Spin_Button), Policy);
   end Set_Update_Policy;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Spin_Button : access Gtk_Spin_Button_Record; Value : Gdouble)
   is
      procedure Internal (Spin_Button : System.Address; Value : Gdouble);
      pragma Import (C, Internal, "gtk_spin_button_set_value");

   begin
      Internal (Get_Object (Spin_Button), Value);
   end Set_Value;

   --------------
   -- Set_Wrap --
   --------------

   procedure Set_Wrap
     (Spin_Button : access Gtk_Spin_Button_Record; Wrap : Boolean)
   is
      procedure Internal (Spin_Button : System.Address; Wrap : Gint);
      pragma Import (C, Internal, "gtk_spin_button_set_wrap");

   begin
      Internal (Get_Object (Spin_Button), Boolean'Pos (Wrap));
   end Set_Wrap;

   ----------
   -- Spin --
   ----------

   procedure Spin
     (Spin_Button : access Gtk_Spin_Button_Record;
      Direction   : Gtk_Spin_Type;
      Step        : Gdouble)
   is
      procedure Internal
        (Spin_Button : System.Address;
         Direction   : Gtk_Spin_Type;
         Step        : Gdouble);
      pragma Import (C, Internal, "gtk_spin_button_spin");

   begin
      Internal (Get_Object (Spin_Button), Direction, Step);
   end Spin;

   ------------
   -- Update --
   ------------

   procedure Update (Spin_Button : access Gtk_Spin_Button_Record) is
      procedure Internal (Spin_Button : System.Address);
      pragma Import (C, Internal, "gtk_spin_button_update");

   begin
      Internal (Get_Object (Spin_Button));
   end Update;

end Gtk.Spin_Button;
