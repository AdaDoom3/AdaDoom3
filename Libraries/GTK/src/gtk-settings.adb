-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
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

with Glib.Values;          use Glib.Values;
with Gtk.Style;            use Gtk.Style;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Glib.Type_Conversion_Hooks;

package body Gtk.Settings is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Settings_Record);
   pragma Warnings (Off, Type_Conversion);

   -----------------
   -- Get_Default --
   -----------------

   function Get_Default return Gtk_Settings is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_settings_get_default");
      Stub : Gtk_Settings_Record;
   begin
      return Gtk_Settings (Get_User_Data (Internal, Stub));
   end Get_Default;

   --------------------
   -- Get_For_Screen --
   --------------------

   function Get_For_Screen (Screen : Gdk.Gdk_Screen) return Gtk_Settings is
      function Internal (Screen : Gdk.Gdk_Screen) return System.Address;
      pragma Import (C, Internal, "gtk_settings_get_for_screen");
      Stub : Gtk_Settings_Record;
   begin
      return Gtk_Settings (Get_User_Data (Internal (Screen), Stub));
   end Get_For_Screen;

   -------------------------
   -- Set_Double_Property --
   -------------------------

   procedure Set_Double_Property
     (Settings : access Gtk_Settings_Record;
      Name     : String;
      Value    : Gdouble;
      Origin   : String)
   is
      procedure Internal
        (Settings : System.Address;
         Name     : String;
         V_Double : Gdouble;
         Origin   : String);
      pragma Import (C, Internal, "gtk_settings_set_double_property");
   begin
      Internal
        (Get_Object (Settings), Name & ASCII.NUL, Value, Origin & ASCII.NUL);
   end Set_Double_Property;

   -----------------------
   -- Set_Long_Property --
   -----------------------

   procedure Set_Long_Property
     (Settings : access Gtk_Settings_Record;
      Name     : String;
      Value    : Glong;
      Origin   : String)
   is
      procedure Internal
        (Settings : System.Address;
         Name     : String;
         V_Long   : Glong;
         Origin   : String);
      pragma Import (C, Internal, "gtk_settings_set_long_property");
   begin
      Internal
        (Get_Object (Settings), Name & ASCII.NUL, Value, Origin & ASCII.NUL);
   end Set_Long_Property;

   ------------------------
   -- Set_Property_Value --
   ------------------------

   procedure Set_Property_Value
     (Settings : access Gtk_Settings_Record;
      Name     : String;
      Value    : GValue;
      Origin   : String)
   is
      type Property_Value is record
         Origin : Interfaces.C.Strings.chars_ptr;
         Value  : GValue;
      end record;
      pragma Convention (C, Property_Value);

      procedure Internal
        (Settings : System.Address;
         Name     : String;
         Svalue   : System.Address);
      pragma Import (C, Internal, "gtk_settings_set_property_value");

      Val : aliased Property_Value :=
        (Origin => New_String (Origin),
         Value  => Value);
   begin
      Internal (Get_Object (Settings), Name & ASCII.NUL, Val'Address);
      Free (Val.Origin);
   end Set_Property_Value;

   -------------------------
   -- Set_String_Property --
   -------------------------

   procedure Set_String_Property
     (Settings : access Gtk_Settings_Record;
      Name     : String;
      Value    : String;
      Origin   : String)
   is
      procedure Internal
        (Settings : System.Address;
         Name     : String;
         V_String : String;
         Origin   : String);
      pragma Import (C, Internal, "gtk_settings_set_string_property");
   begin
      Internal (Get_Object (Settings), Name & ASCII.NUL,
                Value & ASCII.NUL, Origin & ASCII.NUL);
   end Set_String_Property;

end Gtk.Settings;
