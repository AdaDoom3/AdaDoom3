-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2006-2013, AdaCore                   --
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

with Gdk;                      use Gdk;
with Gdk.Display;              use Gdk.Display;
with Gdk.Rectangle;            use Gdk.Rectangle;
with Gdk.Types;                use Gdk.Types;
with Glib;                     use Glib;
with Glib.Object;              use Glib.Object;
with Glib.Values;              use Glib.Values;
with Interfaces.C.Strings;     use Interfaces.C.Strings;
with System;                   use System;

package body Gdk.Screen is

   ----------------
   -- Get_Screen --
   ----------------

   function Get_Screen
     (Display    : access Gdk_Display_Record'Class;
      Screen_Num : Gint)
      return Gdk_Screen
   is
      function Internal
        (Display    : System.Address;
         Screen_Num : Gint)
         return System.Address;
      pragma Import (C, Internal, "gdk_display_get_screen");
      --  External binding: gdk_display_get_screen
      Stub : Gdk_Screen_Record;
   begin
      return Gdk_Screen
        (Get_User_Data
          (Internal (Get_Object (Display), Screen_Num), Stub));
   end Get_Screen;

   ------------------------
   -- Get_Default_Screen --
   ------------------------

   function Get_Default_Screen
     (Display : access Gdk_Display_Record'Class)
      return Gdk_Screen
   is
      function Internal
        (Display : System.Address)
         return System.Address;
      pragma Import (C, Internal, "gdk_display_get_default_screen");
      --  External binding: gdk_display_get_default_screen
      Stub : Gdk_Screen_Record;
   begin
      return Gdk_Screen
        (Get_User_Data
          (Internal (Get_Object (Display)), Stub));
   end Get_Default_Screen;

   -----------------
   -- Get_Pointer --
   -----------------

   procedure Get_Pointer
     (Display : access Gdk_Display_Record'Class;
      Screen  : out Gdk_Screen;
      X       : out Gint;
      Y       : out Gint;
      Mask    : out Gdk_Modifier_Type)
   is
      procedure Internal
        (Display : System.Address;
         Screen  : out System.Address;
         X       : out Gint;
         Y       : out Gint;
         Mask    : out Gdk_Modifier_Type);
      pragma Import (C, Internal, "gdk_display_get_pointer");
      --  External binding: gdk_display_get_pointer

      S    : System.Address;
      Stub : Gdk_Screen_Record;

   begin
      Internal (Get_Object (Display), S, X, Y, Mask);
      Screen := Gdk_Screen (Get_User_Data (S, Stub));
   end Get_Pointer;

   -----------------
   -- Get_Default --
   -----------------

   function Get_Default return Gdk_Screen is
      function Internal return System.Address;
      pragma Import (C, Internal, "gdk_screen_get_default");
      Stub : Gdk_Screen_Record;
   begin
      return Gdk_Screen (Get_User_Data (Internal, Stub));
   end Get_Default;

   --------------------------
   -- Get_Default_Colormap --
   --------------------------

   function Get_Default_Colormap
     (Screen : access Gdk_Screen_Record)
      return Gdk_Colormap
   is
      function Internal (Screen : System.Address) return Gdk_Colormap;
      pragma Import (C, Internal, "gdk_screen_get_default_colormap");
   begin
      return Internal (Get_Object (Screen));
   end Get_Default_Colormap;

   -----------------
   -- Get_Display --
   -----------------

   function Get_Display
     (Screen : access Gdk_Screen_Record)
      return Gdk_Display
   is
      function Internal (Screen : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_screen_get_display");
      Stub : Gdk_Display_Record;
   begin
      return Gdk_Display
        (Get_User_Data (Internal (Get_Object (Screen)), Stub));
   end Get_Display;

   ----------------
   -- Get_Height --
   ----------------

   function Get_Height (Screen : access Gdk_Screen_Record) return Gint is
      function Internal (Screen : System.Address) return Gint;
      pragma Import (C, Internal, "gdk_screen_get_height");
   begin
      return Internal (Get_Object (Screen));
   end Get_Height;

   -------------------
   -- Get_Height_Mm --
   -------------------

   function Get_Height_Mm (Screen : access Gdk_Screen_Record) return Gint is
      function Internal (Screen : System.Address) return Gint;
      pragma Import (C, Internal, "gdk_screen_get_height_mm");
   begin
      return Internal (Get_Object (Screen));
   end Get_Height_Mm;

   --------------------------
   -- Get_Monitor_At_Point --
   --------------------------

   function Get_Monitor_At_Point
     (Screen : access Gdk_Screen_Record;
      X      : Gint;
      Y      : Gint)
      return Gint
   is
      function Internal
        (Screen : System.Address;
         X      : Gint;
         Y      : Gint)
         return Gint;
      pragma Import (C, Internal, "gdk_screen_get_monitor_at_point");
   begin
      return Internal (Get_Object (Screen), X, Y);
   end Get_Monitor_At_Point;

   ---------------------------
   -- Get_Monitor_At_Window --
   ---------------------------

   function Get_Monitor_At_Window
     (Screen : access Gdk_Screen_Record;
      Window : Gdk_Window)
      return Gint
   is
      function Internal
        (Screen : System.Address;
         Window : Gdk_Window)
         return Gint;
      pragma Import (C, Internal, "gdk_screen_get_monitor_at_window");
   begin
      return Internal (Get_Object (Screen), Window);
   end Get_Monitor_At_Window;

   --------------------------
   -- Get_Monitor_Geometry --
   --------------------------

   procedure Get_Monitor_Geometry
     (Screen      : access Gdk_Screen_Record;
      Monitor_Num : Gint;
      Dest        : out Gdk_Rectangle)
   is
      procedure Internal
        (Screen      : System.Address;
         Monitor_Num : Gint;
         Dest        : out Gdk_Rectangle);
      pragma Import (C, Internal, "gdk_screen_get_monitor_geometry");
   begin
      Internal (Get_Object (Screen), Monitor_Num, Dest);
   end Get_Monitor_Geometry;

   --------------------
   -- Get_N_Monitors --
   --------------------

   function Get_N_Monitors (Screen : access Gdk_Screen_Record) return Gint is
      function Internal (Screen : System.Address) return Gint;
      pragma Import (C, Internal, "gdk_screen_get_n_monitors");
   begin
      return Internal (Get_Object (Screen));
   end Get_N_Monitors;

   ----------------
   -- Get_Number --
   ----------------

   function Get_Number (Screen : access Gdk_Screen_Record) return Gint is
      function Internal (Screen : System.Address) return Gint;
      pragma Import (C, Internal, "gdk_screen_get_number");
   begin
      return Internal (Get_Object (Screen));
   end Get_Number;

   ----------------------
   -- Get_Rgb_Colormap --
   ----------------------

   function Get_Rgb_Colormap
     (Screen : access Gdk_Screen_Record) return Gdk_Colormap
   is
      function Internal (Screen : System.Address) return Gdk_Colormap;
      pragma Import (C, Internal, "gdk_screen_get_rgb_colormap");
   begin
      return Internal (Get_Object (Screen));
   end Get_Rgb_Colormap;

   --------------------
   -- Get_Rgb_Visual --
   --------------------

   function Get_Rgb_Visual
     (Screen : access Gdk_Screen_Record) return Gdk_Visual
   is
      function Internal (Screen : System.Address) return Gdk_Visual;
      pragma Import (C, Internal, "gdk_screen_get_rgb_visual");
   begin
      return Internal (Get_Object (Screen));
   end Get_Rgb_Visual;

   -----------------------
   -- Get_Rgba_Colormap --
   -----------------------

   function Get_Rgba_Colormap
     (Screen : access Gdk_Screen_Record)
      return Gdk_Colormap
   is
      function Internal (Screen : System.Address) return Gdk_Colormap;
      pragma Import (C, Internal, "gdk_screen_get_rgba_colormap");
   begin
      return Internal (Get_Object (Screen));
   end Get_Rgba_Colormap;

   ---------------------
   -- Get_Rgba_Visual --
   ---------------------

   function Get_Rgba_Visual
     (Screen : access Gdk_Screen_Record)
      return Gdk_Visual
   is
      function Internal (Screen : System.Address) return Gdk_Visual;
      pragma Import (C, Internal, "gdk_screen_get_rgba_visual");
   begin
      return Internal (Get_Object (Screen));
   end Get_Rgba_Visual;

   ---------------------
   -- Get_Root_Window --
   ---------------------

   function Get_Root_Window
     (Screen : access Gdk_Screen_Record)
      return Gdk_Window
   is
      function Internal (Screen : System.Address) return Gdk_Window;
      pragma Import (C, Internal, "gdk_screen_get_root_window");
   begin
      return Internal (Get_Object (Screen));
   end Get_Root_Window;

   -----------------
   -- Get_Setting --
   -----------------

   procedure Get_Setting
     (Screen : access Gdk_Screen_Record;
      Name   : String;
      Value  : out GValue;
      Found  : out Boolean)
   is
      function Internal
        (Screen : System.Address;
         Name   : String;
         Value  : access GValue)
         return Gboolean;
      pragma Import (C, Internal, "gdk_screen_get_setting");
      Val : aliased GValue;
   begin
      Found := Boolean'Val
        (Internal (Get_Object (Screen), Name & ASCII.NUL, Val'Access));
      Value := Val;
   end Get_Setting;

   -------------------------
   -- Get_System_Colormap --
   -------------------------

   function Get_System_Colormap
     (Screen : access Gdk_Screen_Record)
      return Gdk_Colormap
   is
      function Internal (Screen : System.Address) return Gdk_Colormap;
      pragma Import (C, Internal, "gdk_screen_get_system_colormap");
   begin
      return Internal (Get_Object (Screen));
   end Get_System_Colormap;

   -----------------------
   -- Get_System_Visual --
   -----------------------

   function Get_System_Visual
     (Screen : access Gdk_Screen_Record)
      return Gdk_Visual
   is
      function Internal (Screen : System.Address) return Gdk_Visual;
      pragma Import (C, Internal, "gdk_screen_get_system_visual");
   begin
      return Internal (Get_Object (Screen));
   end Get_System_Visual;

   ---------------
   -- Get_Width --
   ---------------

   function Get_Width
     (Screen : access Gdk_Screen_Record)
      return Gint
   is
      function Internal (Screen : System.Address) return Gint;
      pragma Import (C, Internal, "gdk_screen_get_width");
   begin
      return Internal (Get_Object (Screen));
   end Get_Width;

   ------------------
   -- Get_Width_Mm --
   ------------------

   function Get_Width_Mm (Screen : access Gdk_Screen_Record) return Gint is
      function Internal (Screen : System.Address) return Gint;
      pragma Import (C, Internal, "gdk_screen_get_width_mm");
   begin
      return Internal (Get_Object (Screen));
   end Get_Width_Mm;

   -----------------------
   -- Make_Display_Name --
   -----------------------

   function Make_Display_Name
     (Screen : access Gdk_Screen_Record)
      return String
   is
      function Internal (Screen : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gdk_screen_make_display_name");
      Val : chars_ptr := Internal (Get_Object (Screen));
      Result : constant String := Value (Val);
   begin
      Free (Val);
      return Result;
   end Make_Display_Name;

   --------------------------
   -- Set_Default_Colormap --
   --------------------------

   procedure Set_Default_Colormap
     (Screen   : access Gdk_Screen_Record;
      Colormap : Gdk_Colormap)
   is
      procedure Internal
        (Screen   : System.Address;
         Colormap : Gdk_Colormap);
      pragma Import (C, Internal, "gdk_screen_set_default_colormap");
   begin
      Internal (Get_Object (Screen), Colormap);
   end Set_Default_Colormap;

   ------------------
   -- Warp_Pointer --
   ------------------

   procedure Warp_Pointer
     (Display : access Gdk.Display.Gdk_Display_Record'Class;
      Screen  : access Gdk_Screen_Record;
      X       : Glib.Gint;
      Y       : Glib.Gint)
   is
      procedure Internal (D, S : System.Address; X, Y : Gint);
      pragma Import (C, Internal, "gdk_display_warp_pointer");
      --  External binding: gdk_display_warp_pointer
   begin
      Internal (Get_Object (Display), Get_Object (Screen), X, Y);
   end Warp_Pointer;

   -------------------
   -- Is_Composited --
   -------------------

   function Is_Composited (Screen : access Gdk_Screen_Record) return Boolean is
      function Internal (Screen : System.Address) return Gint;
      pragma Import (C, Internal, "gdk_screen_is_composited");
   begin
      return Internal (Get_Object (Screen)) /= 0;
   end Is_Composited;

end Gdk.Screen;
