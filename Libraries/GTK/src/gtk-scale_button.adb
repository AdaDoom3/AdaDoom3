-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                    Copyright (C) 2010-2013, AdaCore               --
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

package body Gtk.Scale_Button is

   --------------------
   -- Get_Adjustment --
   --------------------

   function Get_Adjustment
     (Button : access Gtk_Scale_Button_Record)
      return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Button : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scale_button_get_adjustment");
      Stub : Gtk.Adjustment.Gtk_Adjustment_Record;
   begin
      return Gtk.Adjustment.Gtk_Adjustment
        (Get_User_Data (Internal (Get_Object (Button)), Stub));
   end Get_Adjustment;

   ----------------------
   -- Get_Minus_Button --
   ----------------------

   function Get_Minus_Button
     (Button : access Gtk_Scale_Button_Record) return Gtk.Widget.Gtk_Widget
   is
      function Internal (Button : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scale_button_get_minus_button");
      Stub : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget
        (Get_User_Data (Internal (Get_Object (Button)), Stub));
   end Get_Minus_Button;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
     (Button : access Gtk_Scale_Button_Record)
      return Gtk.Enums.Gtk_Orientation
   is
      function Internal (Button : System.Address)
        return Gtk.Enums.Gtk_Orientation;
      pragma Import (C, Internal, "gtk_scale_button_get_orientation");
   begin
      return Internal (Get_Object (Button));
   end Get_Orientation;

   ---------------------
   -- Get_Plus_Button --
   ---------------------

   function Get_Plus_Button
     (Button : access Gtk_Scale_Button_Record) return Gtk.Widget.Gtk_Widget
   is
      function Internal (Button : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scale_button_get_plus_button");
      Stub : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget
        (Get_User_Data (Internal (Get_Object (Button)), Stub));
   end Get_Plus_Button;

   ---------------
   -- Get_Popup --
   ---------------

   function Get_Popup
     (Button : access Gtk_Scale_Button_Record) return Gtk.Widget.Gtk_Widget
   is
      function Internal (Button : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scale_button_get_popup");
      Stub : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget
        (Get_User_Data (Internal (Get_Object (Button)), Stub));
   end Get_Popup;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Button : access Gtk_Scale_Button_Record) return Gdouble
   is
      function Internal (Button : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_scale_button_get_value");
   begin
      return Internal (Get_Object (Button));
   end Get_Value;

   -------------
   -- Gtk_New --
   -------------

   function Gtk_New
     (Size  : Gtk.Enums.Gtk_Icon_Size;
      Min   : Gdouble;
      Max   : Gdouble;
      Step  : Gdouble;
      Icons : Gtkada.Types.Chars_Ptr_Array)
      return Gtk_Scale_Button
   is
      use Gtkada.Types;

      function Internal
        (Size  : Gtk.Enums.Gtk_Icon_Size;
         Min   : Gdouble;
         Max   : Gdouble;
         Step  : Gdouble;
         Icons : System.Address)
         return System.Address;
      pragma Import (C, Internal, "gtk_scale_button_new");
      Stub : Gtk_Scale_Button_Record;
   begin
      if Icons = Null_Array then
         return Gtk_Scale_Button (Get_User_Data
           (Internal (Size, Min, Max, Step, System.Null_Address), Stub));
      else
         return Gtk_Scale_Button (Get_User_Data
           (Internal (Size, Min, Max, Step, Icons'Address), Stub));
      end if;
   end Gtk_New;

   --------------------
   -- Set_Adjustment --
   --------------------

   procedure Set_Adjustment
     (Button     : access Gtk_Scale_Button_Record;
      Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Internal (Button, Adjustment : System.Address);
      pragma Import (C, Internal, "gtk_scale_button_set_adjustment");
   begin
      Internal (Get_Object (Button), Get_Object (Adjustment));
   end Set_Adjustment;

   ---------------
   -- Set_Icons --
   ---------------

   procedure Set_Icons
     (Button : access Gtk_Scale_Button_Record;
      Icons  : Gtkada.Types.Chars_Ptr_Array)
   is
      use Gtkada.Types;
      procedure Internal (Button, Icons : System.Address);
      pragma Import (C, Internal, "gtk_scale_button_set_icons");
   begin
      if Icons = Null_Array then
         Internal (Get_Object (Button), System.Null_Address);
      else
         Internal (Get_Object (Button), Icons'Address);
      end if;
   end Set_Icons;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
     (Button      : access Gtk_Scale_Button_Record;
      Orientation : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal
        (Button      : System.Address;
         Orientation : Gtk.Enums.Gtk_Orientation);
      pragma Import (C, Internal, "gtk_scale_button_set_orientation");
   begin
      Internal (Get_Object (Button), Orientation);
   end Set_Orientation;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Button : access Gtk_Scale_Button_Record;
      Value  : Gdouble)
   is
      procedure Internal
        (Button : System.Address;
         Value  : Gdouble);
      pragma Import (C, Internal, "gtk_scale_button_set_value");
   begin
      Internal (Get_Object (Button), Value);
   end Set_Value;

end Gtk.Scale_Button;
