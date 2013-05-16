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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;

package body Gtk.Adjustment is
   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Adjustment_Record);
   pragma Unreferenced (Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Adjustment     : out Gtk_Adjustment;
       Value          : Gdouble;
       Lower          : Gdouble;
       Upper          : Gdouble;
       Step_Increment : Gdouble;
       Page_Increment : Gdouble;
       Page_Size      : Gdouble := 0.0)
   is
   begin
      Adjustment := new Gtk_Adjustment_Record;
      Gtk.Adjustment.Initialize (Adjustment, Value, Lower, Upper, Step_Increment, Page_Increment, Page_Size);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Adjustment     : access Gtk_Adjustment_Record'Class;
       Value          : Gdouble;
       Lower          : Gdouble;
       Upper          : Gdouble;
       Step_Increment : Gdouble;
       Page_Increment : Gdouble;
       Page_Size      : Gdouble := 0.0)
   is
      function Internal
         (Value          : Gdouble;
          Lower          : Gdouble;
          Upper          : Gdouble;
          Step_Increment : Gdouble;
          Page_Increment : Gdouble;
          Page_Size      : Gdouble) return System.Address;
      pragma Import (C, Internal, "gtk_adjustment_new");
   begin
      Set_Object (Adjustment, Internal (Value, Lower, Upper, Step_Increment, Page_Increment, Page_Size));
   end Initialize;

   -------------
   -- Changed --
   -------------

   procedure Changed (Adjustment : access Gtk_Adjustment_Record) is
      procedure Internal (Adjustment : System.Address);
      pragma Import (C, Internal, "gtk_adjustment_changed");
   begin
      Internal (Get_Object (Adjustment));
   end Changed;

   ----------------
   -- Clamp_Page --
   ----------------

   procedure Clamp_Page
      (Adjustment : access Gtk_Adjustment_Record;
       Lower      : Gdouble;
       Upper      : Gdouble)
   is
      procedure Internal
         (Adjustment : System.Address;
          Lower      : Gdouble;
          Upper      : Gdouble);
      pragma Import (C, Internal, "gtk_adjustment_clamp_page");
   begin
      Internal (Get_Object (Adjustment), Lower, Upper);
   end Clamp_Page;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
      (Adjustment     : access Gtk_Adjustment_Record;
       Value          : Gdouble;
       Lower          : Gdouble;
       Upper          : Gdouble;
       Step_Increment : Gdouble;
       Page_Increment : Gdouble;
       Page_Size      : Gdouble)
   is
      procedure Internal
         (Adjustment     : System.Address;
          Value          : Gdouble;
          Lower          : Gdouble;
          Upper          : Gdouble;
          Step_Increment : Gdouble;
          Page_Increment : Gdouble;
          Page_Size      : Gdouble);
      pragma Import (C, Internal, "gtk_adjustment_configure");
   begin
      Internal (Get_Object (Adjustment), Value, Lower, Upper, Step_Increment, Page_Increment, Page_Size);
   end Configure;

   ---------------
   -- Get_Lower --
   ---------------

   function Get_Lower
      (Adjustment : access Gtk_Adjustment_Record) return Gdouble
   is
      function Internal (Adjustment : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_adjustment_get_lower");
   begin
      return Internal (Get_Object (Adjustment));
   end Get_Lower;

   ------------------------
   -- Get_Page_Increment --
   ------------------------

   function Get_Page_Increment
      (Adjustment : access Gtk_Adjustment_Record) return Gdouble
   is
      function Internal (Adjustment : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_adjustment_get_page_increment");
   begin
      return Internal (Get_Object (Adjustment));
   end Get_Page_Increment;

   -------------------
   -- Get_Page_Size --
   -------------------

   function Get_Page_Size
      (Adjustment : access Gtk_Adjustment_Record) return Gdouble
   is
      function Internal (Adjustment : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_adjustment_get_page_size");
   begin
      return Internal (Get_Object (Adjustment));
   end Get_Page_Size;

   ------------------------
   -- Get_Step_Increment --
   ------------------------

   function Get_Step_Increment
      (Adjustment : access Gtk_Adjustment_Record) return Gdouble
   is
      function Internal (Adjustment : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_adjustment_get_step_increment");
   begin
      return Internal (Get_Object (Adjustment));
   end Get_Step_Increment;

   ---------------
   -- Get_Upper --
   ---------------

   function Get_Upper
      (Adjustment : access Gtk_Adjustment_Record) return Gdouble
   is
      function Internal (Adjustment : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_adjustment_get_upper");
   begin
      return Internal (Get_Object (Adjustment));
   end Get_Upper;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
      (Adjustment : access Gtk_Adjustment_Record) return Gdouble
   is
      function Internal (Adjustment : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_adjustment_get_value");
   begin
      return Internal (Get_Object (Adjustment));
   end Get_Value;

   ---------------
   -- Set_Lower --
   ---------------

   procedure Set_Lower
      (Adjustment : access Gtk_Adjustment_Record;
       Lower      : Gdouble)
   is
      procedure Internal (Adjustment : System.Address; Lower : Gdouble);
      pragma Import (C, Internal, "gtk_adjustment_set_lower");
   begin
      Internal (Get_Object (Adjustment), Lower);
   end Set_Lower;

   ------------------------
   -- Set_Page_Increment --
   ------------------------

   procedure Set_Page_Increment
      (Adjustment     : access Gtk_Adjustment_Record;
       Page_Increment : Gdouble)
   is
      procedure Internal
         (Adjustment     : System.Address;
          Page_Increment : Gdouble);
      pragma Import (C, Internal, "gtk_adjustment_set_page_increment");
   begin
      Internal (Get_Object (Adjustment), Page_Increment);
   end Set_Page_Increment;

   -------------------
   -- Set_Page_Size --
   -------------------

   procedure Set_Page_Size
      (Adjustment : access Gtk_Adjustment_Record;
       Page_Size  : Gdouble)
   is
      procedure Internal (Adjustment : System.Address; Page_Size : Gdouble);
      pragma Import (C, Internal, "gtk_adjustment_set_page_size");
   begin
      Internal (Get_Object (Adjustment), Page_Size);
   end Set_Page_Size;

   ------------------------
   -- Set_Step_Increment --
   ------------------------

   procedure Set_Step_Increment
      (Adjustment     : access Gtk_Adjustment_Record;
       Step_Increment : Gdouble)
   is
      procedure Internal
         (Adjustment     : System.Address;
          Step_Increment : Gdouble);
      pragma Import (C, Internal, "gtk_adjustment_set_step_increment");
   begin
      Internal (Get_Object (Adjustment), Step_Increment);
   end Set_Step_Increment;

   ---------------
   -- Set_Upper --
   ---------------

   procedure Set_Upper
      (Adjustment : access Gtk_Adjustment_Record;
       Upper      : Gdouble)
   is
      procedure Internal (Adjustment : System.Address; Upper : Gdouble);
      pragma Import (C, Internal, "gtk_adjustment_set_upper");
   begin
      Internal (Get_Object (Adjustment), Upper);
   end Set_Upper;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
      (Adjustment : access Gtk_Adjustment_Record;
       Value      : Gdouble)
   is
      procedure Internal (Adjustment : System.Address; Value : Gdouble);
      pragma Import (C, Internal, "gtk_adjustment_set_value");
   begin
      Internal (Get_Object (Adjustment), Value);
   end Set_Value;

   -------------------
   -- Value_Changed --
   -------------------

   procedure Value_Changed (Adjustment : access Gtk_Adjustment_Record) is
      procedure Internal (Adjustment : System.Address);
      pragma Import (C, Internal, "gtk_adjustment_value_changed");
   begin
      Internal (Get_Object (Adjustment));
   end Value_Changed;

end Gtk.Adjustment;
