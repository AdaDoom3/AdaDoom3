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

with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Glib.Values is

   ----------
   -- Free --
   ----------

   procedure Free (Val : in out GValues) is
      procedure Internal (Value_Array : System.Address);
      pragma Import (C, Internal, "g_value_array_free");
   begin
      Internal (Val.Arr);
      Val := (Nb => 0, Arr => System.Null_Address);
   end Free;

   -----------------
   -- Get_Boolean --
   -----------------

   function Get_Boolean (Value : GValue) return Boolean is
      function Internal (Value : GValue) return Gboolean;
      pragma Import (C, Internal, "g_value_get_boolean");
   begin
      return Boolean'Val (Internal (Value));
   end Get_Boolean;

   ----------------
   -- Get_Object --
   ----------------

   function Get_Object (Value : GValue) return Glib.Object.GObject is
      function Internal (Value : GValue) return System.Address;
      pragma Import (C, Internal, "g_value_get_object");

   begin
      return Glib.Object.Convert (Internal (Value));
   end Get_Object;

   ----------------
   -- Get_String --
   ----------------

   function Get_String (Value : GValue) return String is
      function Internal (Value : GValue) return chars_ptr;
      pragma Import (C, Internal, "g_value_get_string");
      C : constant chars_ptr := Internal (Value);
   begin
      if C = Null_Ptr then
         return "";
      else
         return Interfaces.C.Strings.Value (C);
      end if;
   end Get_String;

   ----------------
   -- Get_String --
   ----------------

   function Get_String (Value : GValue; Length : Gint) return String is
      function Internal (Value : GValue) return chars_ptr;
      pragma Import (C, Internal, "g_value_get_string");
      C : constant chars_ptr := Internal (Value);
   begin
      if C = Null_Ptr then
         return "";
      else
         return Interfaces.C.Strings.Value
           (C, Interfaces.C.size_t (Length));
      end if;
   end Get_String;

   -----------------
   -- Make_Values --
   -----------------

   function Make_Values (Nb : Guint) return GValues is
      function Internal (N_Prealloced : Guint) return System.Address;
      pragma Import (C, Internal, "g_value_array_new");
   begin
      return (Nb => Nb, Arr => Internal (Nb));
   end Make_Values;

   function Make_Values (Nb : Guint; Val : System.Address) return GValues is
   begin
      return (Nb => Nb, Arr => Val);
   end Make_Values;

   ---------
   -- Nth --
   ---------

   function Nth (Val : GValues; Num : Guint) return GValue is
      procedure Internal
        (Val : System.Address; Num : Guint; V : in out GValue);
      pragma Import (C, Internal, "ada_gvalue_nth");
      V : GValue;
   begin
      --   Should this be greater or greater or equal ???
      if Num > Val.Nb then
         raise Constraint_Error;
      end if;

      Internal (Val.Arr, Num, V);
      return V;
   end Nth;

   -----------------
   -- Set_Boolean --
   -----------------

   procedure Set_Boolean (Value : in out GValue; V_Boolean : Boolean) is
      procedure Internal (Value : GValue; V_Boolean : Gboolean);
      pragma Import (C, Internal, "g_value_set_boolean");
   begin
      Internal (Value, Boolean'Pos (V_Boolean));
   end Set_Boolean;

   ----------------
   -- Set_Object --
   ----------------

   procedure Set_Object (Value : in out GValue; To : Glib.Object.GObject) is
      procedure Internal
        (Value : in out Glib.Values.GValue;
         To    : System.Address);
      pragma Import (C, Internal, "g_value_set_object");

   begin
      Internal (Value, Glib.Object.Convert (To));
   end Set_Object;

   ----------------
   -- Set_String --
   ----------------

   procedure Set_String (Value : in out GValue; V_String : String) is
      procedure Internal (Value : in out GValue; V_String : String);
      pragma Import (C, Internal, "g_value_set_string");
   begin
      Internal (Value, V_String & ASCII.NUL);
   end Set_String;

   ----------
   -- Init --
   ----------

   procedure Init (Value : in out GValue; G_Type : Glib.GType) is
      procedure Internal (Value : in out GValue; G_Type : Glib.GType);
      pragma Import (C, Internal, "g_value_init");
   begin
      Value := (g_type => 0, data => (others => 0));
      Internal (Value, G_Type);
   end Init;

   function C_Gvalue_Size return Natural;
   pragma Import (C, C_Gvalue_Size, "ada_c_gvalue_size");

begin
   if GValue'Size /= C_Gvalue_Size * 8 then
      raise Program_Error;
   end if;
end Glib.Values;
