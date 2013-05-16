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

package body Gtk.Viewport is
   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Viewport_Record);
   pragma Unreferenced (Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Viewport    : out Gtk_Viewport;
       Hadjustment : Gtk.Adjustment.Gtk_Adjustment := null;
       Vadjustment : Gtk.Adjustment.Gtk_Adjustment := null)
   is
   begin
      Viewport := new Gtk_Viewport_Record;
      Gtk.Viewport.Initialize (Viewport, Hadjustment, Vadjustment);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Viewport    : access Gtk_Viewport_Record'Class;
       Hadjustment : Gtk.Adjustment.Gtk_Adjustment := null;
       Vadjustment : Gtk.Adjustment.Gtk_Adjustment := null)
   is
      function Internal
         (Hadjustment : System.Address;
          Vadjustment : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_viewport_new");
   begin
      Set_Object (Viewport, Internal (Get_Object_Or_Null (GObject (Hadjustment)), Get_Object_Or_Null (GObject (Vadjustment))));
   end Initialize;

   --------------------
   -- Get_Bin_Window --
   --------------------

   function Get_Bin_Window
      (Viewport : access Gtk_Viewport_Record) return Gdk.Window.Gdk_Window
   is
      function Internal
         (Viewport : System.Address) return Gdk.Window.Gdk_Window;
      pragma Import (C, Internal, "gtk_viewport_get_bin_window");
   begin
      return Internal (Get_Object (Viewport));
   end Get_Bin_Window;

   ---------------------
   -- Get_Hadjustment --
   ---------------------

   function Get_Hadjustment
      (Viewport : access Gtk_Viewport_Record)
       return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Viewport : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_viewport_get_hadjustment");
      Stub : Gtk.Adjustment.Gtk_Adjustment_Record;
   begin
      return Gtk.Adjustment.Gtk_Adjustment (Get_User_Data (Internal (Get_Object (Viewport)), Stub));
   end Get_Hadjustment;

   ---------------------
   -- Get_Shadow_Type --
   ---------------------

   function Get_Shadow_Type
      (Viewport : access Gtk_Viewport_Record)
       return Gtk.Enums.Gtk_Shadow_Type
   is
      function Internal (Viewport : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_viewport_get_shadow_type");
   begin
      return Gtk.Enums.Gtk_Shadow_Type'Val (Internal (Get_Object (Viewport)));
   end Get_Shadow_Type;

   ---------------------
   -- Get_Vadjustment --
   ---------------------

   function Get_Vadjustment
      (Viewport : access Gtk_Viewport_Record)
       return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Viewport : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_viewport_get_vadjustment");
      Stub : Gtk.Adjustment.Gtk_Adjustment_Record;
   begin
      return Gtk.Adjustment.Gtk_Adjustment (Get_User_Data (Internal (Get_Object (Viewport)), Stub));
   end Get_Vadjustment;

   ---------------------
   -- Get_View_Window --
   ---------------------

   function Get_View_Window
      (Viewport : access Gtk_Viewport_Record) return Gdk.Window.Gdk_Window
   is
      function Internal
         (Viewport : System.Address) return Gdk.Window.Gdk_Window;
      pragma Import (C, Internal, "gtk_viewport_get_view_window");
   begin
      return Internal (Get_Object (Viewport));
   end Get_View_Window;

   ---------------------
   -- Set_Hadjustment --
   ---------------------

   procedure Set_Hadjustment
      (Viewport   : access Gtk_Viewport_Record;
       Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Internal
         (Viewport   : System.Address;
          Adjustment : System.Address);
      pragma Import (C, Internal, "gtk_viewport_set_hadjustment");
   begin
      Internal (Get_Object (Viewport), Get_Object (Adjustment));
   end Set_Hadjustment;

   ---------------------
   -- Set_Shadow_Type --
   ---------------------

   procedure Set_Shadow_Type
      (Viewport : access Gtk_Viewport_Record;
       The_Type : Gtk.Enums.Gtk_Shadow_Type)
   is
      procedure Internal (Viewport : System.Address; The_Type : Integer);
      pragma Import (C, Internal, "gtk_viewport_set_shadow_type");
   begin
      Internal (Get_Object (Viewport), Gtk.Enums.Gtk_Shadow_Type'Pos (The_Type));
   end Set_Shadow_Type;

   ---------------------
   -- Set_Vadjustment --
   ---------------------

   procedure Set_Vadjustment
      (Viewport   : access Gtk_Viewport_Record;
       Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Internal
         (Viewport   : System.Address;
          Adjustment : System.Address);
      pragma Import (C, Internal, "gtk_viewport_set_vadjustment");
   begin
      Internal (Get_Object (Viewport), Get_Object (Adjustment));
   end Set_Vadjustment;

end Gtk.Viewport;
