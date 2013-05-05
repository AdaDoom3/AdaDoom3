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
with Gtk.Enums;     use Gtk.Enums;
with Gtk.Scrollbar; use Gtk.Scrollbar;

with Glib.Type_Conversion_Hooks;

package body Gtk.Scrolled_Window is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Scrolled_Window_Record);
   pragma Warnings (Off, Type_Conversion);

   -----------------------
   -- Add_With_Viewport --
   -----------------------

   procedure Add_With_Viewport
     (Scrolled_Window : access Gtk_Scrolled_Window_Record;
      Child           : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
        (Scrolled_Window : System.Address;
         Child           : System.Address);
      pragma Import (C, Internal, "gtk_scrolled_window_add_with_viewport");

   begin
      Internal (Get_Object (Scrolled_Window), Get_Object (Child));
   end Add_With_Viewport;

   ----------------
   -- Get_Policy --
   ----------------

   procedure Get_Policy
     (Scrolled_Window    : access Gtk_Scrolled_Window_Record;
      H_Scrollbar_Policy : out Enums.Gtk_Policy_Type;
      V_Scrollbar_Policy : out Enums.Gtk_Policy_Type)
   is
      procedure Internal
        (Scrolled_Window    : System.Address;
         H_Scrollbar_Policy : out Enums.Gtk_Policy_Type;
         V_Scrollbar_Policy : out Enums.Gtk_Policy_Type);
      pragma Import (C, Internal, "gtk_scrolled_window_get_policy");

   begin
      Internal
        (Get_Object (Scrolled_Window), H_Scrollbar_Policy, V_Scrollbar_Policy);
   end Get_Policy;

   -------------------
   -- Get_Placement --
   -------------------

   function Get_Placement
     (Scrolled_Window  : access Gtk_Scrolled_Window_Record)
      return Gtk.Enums.Gtk_Corner_Type
   is
      function Internal
        (Scrolled_Window : System.Address) return Gtk.Enums.Gtk_Corner_Type;
      pragma Import (C, Internal, "gtk_scrolled_window_get_placement");

   begin
      return Internal (Get_Object (Scrolled_Window));
   end Get_Placement;

   ---------------------
   -- Get_Shadow_Type --
   ---------------------

   function Get_Shadow_Type
     (Scrolled_Window : access Gtk_Scrolled_Window_Record)
      return Gtk.Enums.Gtk_Shadow_Type
   is
      function Internal
        (Scrolled_Window : System.Address) return Gtk.Enums.Gtk_Shadow_Type;
      pragma Import (C, Internal, "gtk_scrolled_window_get_shadow_type");

   begin
      return Internal (Get_Object (Scrolled_Window));
   end Get_Shadow_Type;

   ----------------------
   -- Get_Hadjustement --
   ----------------------

   function Get_Hadjustment
     (Scrolled_Window : access Gtk_Scrolled_Window_Record)
      return Adjustment.Gtk_Adjustment
   is
      function Internal
        (Scrolled_Window : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scrolled_window_get_hadjustment");

      Stub : Adjustment.Gtk_Adjustment_Record;

   begin
      return Adjustment.Gtk_Adjustment
        (Get_User_Data (Internal (Get_Object (Scrolled_Window)), Stub));
   end Get_Hadjustment;

   ----------------------
   -- Get_Vadjustement --
   ----------------------

   function Get_Vadjustment
     (Scrolled_Window : access Gtk_Scrolled_Window_Record)
      return Adjustment.Gtk_Adjustment
   is
      function Internal
        (Scrolled_Window : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scrolled_window_get_vadjustment");

      Stub : Adjustment.Gtk_Adjustment_Record;

   begin
      return Adjustment.Gtk_Adjustment
        (Get_User_Data (Internal (Get_Object (Scrolled_Window)), Stub));
   end Get_Vadjustment;

   --------------------
   -- Get_Hscrollbar --
   --------------------

   function Get_Hscrollbar
     (Scrolled_Window : access Gtk_Scrolled_Window_Record)
      return Gtk_Scrollbar
   is
      function Internal (Scrolled : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scrolled_window_get_hscrollbar");
      Stub : Gtk_Scrollbar_Record;
   begin
      return Gtk_Scrollbar
        (Get_User_Data (Internal (Get_Object (Scrolled_Window)), Stub));
   end Get_Hscrollbar;

   --------------------
   -- Get_Vscrollbar --
   --------------------

   function Get_Vscrollbar
     (Scrolled_Window : access Gtk_Scrolled_Window_Record)
      return Gtk_Scrollbar
   is
      function Internal (Scrolled : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scrolled_window_get_vscrollbar");
      Stub : Gtk_Scrollbar_Record;
   begin
      return Gtk_Scrollbar
        (Get_User_Data (Internal (Get_Object (Scrolled_Window)), Stub));
   end Get_Vscrollbar;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Scrolled_Window : out Gtk_Scrolled_Window;
      Hadjustment     : Gtk_Adjustment := null;
      Vadjustment     : Gtk_Adjustment := null) is
   begin
      Scrolled_Window := new Gtk_Scrolled_Window_Record;
      Initialize (Scrolled_Window, Hadjustment, Vadjustment);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Scrolled_Window : access Gtk_Scrolled_Window_Record'Class;
      Hadjustment     : Gtk_Adjustment := null;
      Vadjustment     : Gtk_Adjustment := null)
   is
      function Internal
        (Hadjustment, Vadjustment : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scrolled_window_new");

      Hadj, Vadj : System.Address;

   begin
      if Hadjustment = null then
         Hadj := System.Null_Address;
      else
         Hadj := Get_Object (Hadjustment);
      end if;

      if Vadjustment = null then
         Vadj := System.Null_Address;
      else
         Vadj := Get_Object (Vadjustment);
      end if;

      Set_Object (Scrolled_Window, Internal (Hadj, Vadj));
   end Initialize;

   ---------------------
   -- Set_Hadjustment --
   ---------------------

   procedure Set_Hadjustment
     (Scrolled_Window : access Gtk_Scrolled_Window_Record;
      Hadjustment     : Gtk_Adjustment)
   is
      procedure Internal
        (Scrolled_Window : System.Address;
         Hadjustment     : System.Address);
      pragma Import (C, Internal, "gtk_scrolled_window_set_hadjustment");

   begin
      if Hadjustment = null then
         Internal (Get_Object (Scrolled_Window), System.Null_Address);
      else
         Internal (Get_Object (Scrolled_Window), Get_Object (Hadjustment));
      end if;
   end Set_Hadjustment;

   -------------------
   -- Set_Placement --
   -------------------

   procedure Set_Placement
     (Scrolled_Window  : access Gtk_Scrolled_Window_Record;
      Window_Placement : Gtk_Corner_Type)
   is
      procedure Internal
        (Scrolled_Window  : System.Address;
         Window_Placement : Gtk_Corner_Type);
      pragma Import (C, Internal, "gtk_scrolled_window_set_placement");

   begin
      Internal (Get_Object (Scrolled_Window), Window_Placement);
   end Set_Placement;

   ----------------
   -- Set_Policy --
   ----------------

   procedure Set_Policy
     (Scrolled_Window    : access Gtk_Scrolled_Window_Record;
      H_Scrollbar_Policy : Enums.Gtk_Policy_Type;
      V_Scrollbar_Policy : Enums.Gtk_Policy_Type)
   is
      procedure Internal
        (Scrolled_Window    : System.Address;
         H_Scrollbar_Policy : Enums.Gtk_Policy_Type;
         V_Scrollbar_Policy : Enums.Gtk_Policy_Type);
      pragma Import (C, Internal, "gtk_scrolled_window_set_policy");

   begin
      Internal (Get_Object (Scrolled_Window),
                H_Scrollbar_Policy, V_Scrollbar_Policy);
   end Set_Policy;

   ---------------------
   -- Set_Shadow_Type --
   ---------------------

   procedure Set_Shadow_Type
     (Scrolled_Window : access Gtk_Scrolled_Window_Record;
      Shadow_Type     : Gtk.Enums.Gtk_Shadow_Type)
   is
      procedure Internal
        (Scrolled_Window : System.Address;
         Shadow_Type     : Gtk.Enums.Gtk_Shadow_Type);
      pragma Import (C, Internal, "gtk_scrolled_window_set_shadow_type");

   begin
      Internal (Get_Object (Scrolled_Window), Shadow_Type);
   end Set_Shadow_Type;

   ---------------------
   -- Set_Vadjustment --
   ---------------------

   procedure Set_Vadjustment
     (Scrolled_Window : access Gtk_Scrolled_Window_Record;
      Vadjustment     : Gtk_Adjustment)
   is
      procedure Internal
        (Scrolled_Window : System.Address;
         Vadjustment     : System.Address);
      pragma Import (C, Internal, "gtk_scrolled_window_set_vadjustment");

   begin
      if Vadjustment = null then
         Internal (Get_Object (Scrolled_Window), System.Null_Address);
      else
         Internal (Get_Object (Scrolled_Window), Get_Object (Vadjustment));
      end if;
   end Set_Vadjustment;

   ---------------------
   -- Unset_Placement --
   ---------------------

   procedure Unset_Placement
     (Scrolled_Window : access Gtk_Scrolled_Window_Record)
   is
      procedure Internal (Scrolled_Window : System.Address);
      pragma Import (C, Internal, "gtk_scrolled_window_unset_placement");
   begin
      Internal (Get_Object (Scrolled_Window));
   end Unset_Placement;

end Gtk.Scrolled_Window;
