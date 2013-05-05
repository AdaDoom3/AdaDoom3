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

with Ada.Unchecked_Conversion;
with System;
with System.Address_To_Access_Conversions;
with Glib.Type_Conversion_Hooks;

with Gdk.Rectangle;    use Gdk.Rectangle;
with Pango.Font;       use Pango.Font;

package body Gtk.Style is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Style_Record);
   pragma Warnings (Off, Type_Conversion);

   package Border_Address_Access_Conversions is
     new System.Address_To_Access_Conversions (Gtk_Border_Record);

   type Gdk_Color_Access is access Gdk_Color;
   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Gdk_Color_Access);

   ---------------
   --  Gtk_New  --
   ---------------

   procedure Gtk_New (Style : out Gtk_Style) is
   begin
      Style := new Gtk_Style_Record;
      Gtk.Style.Initialize (Style);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Style : access Gtk_Style_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_style_new");
   begin
      Set_Object (Style, Internal);
   end Initialize;

   ----------------
   -- Draw_Arrow --
   ----------------

   procedure Draw_Arrow (Style       : Gtk_Style;
                         Window      : Gdk.Window.Gdk_Window;
                         State_Type  : Enums.Gtk_State_Type;
                         Shadow_Type : Enums.Gtk_Shadow_Type;
                         Arrow_Type  : Enums.Gtk_Arrow_Type;
                         Fill        : Boolean;
                         X, Y        : Gint;
                         Width       : Gint;
                         Height      : Gint)
   is
      procedure Internal (Style       : System.Address;
                          Window      : Gdk.Window.Gdk_Window;
                          State_Type  : Enums.Gtk_State_Type;
                          Shadow_Type : Enums.Gtk_Shadow_Type;
                          Arrow_Type  : Enums.Gtk_Arrow_Type;
                          Fill        : Gint;
                          X, Y        : Gint;
                          Width       : Gint;
                          Height      : Gint);
      pragma Import (C, Internal, "gtk_draw_arrow");
   begin
      Internal (Get_Object (Style), Window, State_Type,
                Shadow_Type, Arrow_Type, Boolean'Pos (Fill), X, Y,
                Width, Height);
   end Draw_Arrow;

   ------------------
   -- Draw_Polygon --
   ------------------

   procedure Draw_Polygon
     (Style       : Gtk_Style;
      Window      : Gdk.Window.Gdk_Window;
      State_Type  : Enums.Gtk_State_Type;
      Shadow_Type : Enums.Gtk_Shadow_Type;
      Points      : Gdk.Types.Gdk_Points_Array;
      Fill        : Boolean) is

      procedure Internal
        (Style         : System.Address;
         Window        : Gdk.Window.Gdk_Window;
         State_Type    : Enums.Gtk_State_Type;
         Shadow_Type   : Enums.Gtk_Shadow_Type;
         Points        : Gdk.Types.Gdk_Points_Array;
         Npoints       : Gint;
         Fill          : Gint);
      pragma Import (C, Internal, "gtk_draw_polygon");

   begin
      Internal
        (Get_Object (Style), Window, State_Type, Shadow_Type,
         Points, Points'Length, Boolean'Pos (Fill));
   end Draw_Polygon;

   -----------------
   -- Draw_String --
   -----------------

   procedure Draw_String
     (Style       : Gtk_Style;
      Window      : Gdk.Window.Gdk_Window;
      State_Type  : Enums.Gtk_State_Type;
      X, Y        : Gint;
      Str         : UTF8_String)
   is
      procedure Internal
        (Style         : System.Address;
         Window        : Gdk.Window.Gdk_Window;
         State_Type    : Enums.Gtk_State_Type;
         X, Y          : Gint;
         Str           : UTF8_String);
      pragma Import (C, Internal, "gtk_draw_string");

   begin
      Internal
        (Get_Object (Style), Window, State_Type, X, Y, Str & ASCII.NUL);
   end Draw_String;

   --------------------
   -- Get_Foreground --
   --------------------

   function Get_Foreground
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.Color.Gdk_Color
   is
      function Internal
        (Style      : System.Address;
         State_Type : Enums.Gtk_State_Type) return System.Address;
      pragma Import (C, Internal, "ada_style_get_fg");
   begin
      return Convert (Internal (Get_Object (Style), State_Type)).all;
   end Get_Foreground;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background
     (Style      : Gtk_Style;
      Window     : Gdk.Window.Gdk_Window;
      State_Type : Enums.Gtk_State_Type)
   is
      procedure Internal
        (Style      : System.Address;
         Window     : Gdk.Window.Gdk_Window;
         State_Type : Enums.Gtk_State_Type);
      pragma Import (C, Internal, "gtk_style_set_background");

   begin
      Internal (Get_Object (Style), Window, State_Type);
   end Set_Background;

   --------------------
   -- Set_Foreground --
   --------------------

   procedure Set_Foreground
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Style : System.Address;
         State : Enums.Gtk_State_Type;
         Color : System.Address);
      pragma Import (C, Internal, "ada_style_set_fg");

      Col     : aliased Gdk.Color.Gdk_Color := Color;
      Color_A : System.Address := Col'Address;

   begin
      if Color = Gdk.Color.Null_Color then
         Color_A := System.Null_Address;
      end if;

      Internal (Get_Object (Style), State_Type, Color_A);
   end Set_Foreground;

   --------------------
   -- Get_Background --
   --------------------

   function Get_Background
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.Color.Gdk_Color
   is
      function Internal
        (Style      : System.Address;
         State_Type : Enums.Gtk_State_Type) return System.Address;
      pragma Import (C, Internal, "ada_style_get_bg");
   begin
      return Convert (Internal (Get_Object (Style), State_Type)).all;
   end Get_Background;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Style : System.Address;
         State : Enums.Gtk_State_Type;
         Color : System.Address);
      pragma Import (C, Internal, "ada_style_set_bg");

      Col     : aliased Gdk.Color.Gdk_Color := Color;
      Color_A : System.Address := Col'Address;

   begin
      if Color = Gdk.Color.Null_Color then
         Color_A := System.Null_Address;
      end if;

      Internal (Get_Object (Style), State_Type, Color_A);
   end Set_Background;

   ---------------
   -- Get_Light --
   ---------------

   function Get_Light
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.Color.Gdk_Color
   is
      function Internal
        (Style      : System.Address;
         State_Type : Enums.Gtk_State_Type) return System.Address;
      pragma Import (C, Internal, "ada_style_get_light");
   begin
      return Convert (Internal (Get_Object (Style), State_Type)).all;
   end Get_Light;

   ---------------
   -- Set_Light --
   ---------------

   procedure Set_Light
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Style : System.Address;
         State : Enums.Gtk_State_Type;
         Color : System.Address);
      pragma Import (C, Internal, "ada_style_set_light");

      Col     : aliased Gdk.Color.Gdk_Color := Color;
      Color_A : System.Address := Col'Address;

   begin
      if Color = Gdk.Color.Null_Color then
         Color_A := System.Null_Address;
      end if;

      Internal (Get_Object (Style), State_Type, Color_A);
   end Set_Light;

   --------------
   -- Get_Dark --
   --------------

   function Get_Dark
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.Color.Gdk_Color
   is
      function Internal
        (Style      : System.Address;
         State_Type : Enums.Gtk_State_Type) return System.Address;
      pragma Import (C, Internal, "ada_style_get_dark");
   begin
      return Convert (Internal (Get_Object (Style), State_Type)).all;
   end Get_Dark;

   --------------
   -- Set_Dark --
   --------------

   procedure Set_Dark
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Style : System.Address;
         State : Enums.Gtk_State_Type;
         Color : System.Address);
      pragma Import (C, Internal, "ada_style_set_dark");

      Col     : aliased Gdk.Color.Gdk_Color := Color;
      Color_A : System.Address := Col'Address;

   begin
      if Color = Gdk.Color.Null_Color then
         Color_A := System.Null_Address;
      end if;

      Internal (Get_Object (Style), State_Type, Color_A);
   end Set_Dark;

   ----------------
   -- Get_Middle --
   ----------------

   function Get_Middle
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.Color.Gdk_Color
   is
      function Internal
        (Style      : System.Address;
         State_Type : Enums.Gtk_State_Type) return System.Address;
      pragma Import (C, Internal, "ada_style_get_mid");
   begin
      return Convert (Internal (Get_Object (Style), State_Type)).all;
   end Get_Middle;

   ----------------
   -- Set_Middle --
   ----------------

   procedure Set_Middle
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Style : System.Address;
         State : Enums.Gtk_State_Type;
         Color : System.Address);
      pragma Import (C, Internal, "ada_style_set_mid");

      Col     : aliased Gdk.Color.Gdk_Color := Color;
      Color_A : System.Address := Col'Address;

   begin
      if Color = Gdk.Color.Null_Color then
         Color_A := System.Null_Address;
      end if;

      Internal (Get_Object (Style), State_Type, Color_A);
   end Set_Middle;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.Color.Gdk_Color
   is
      function Internal
        (Style      : System.Address;
         State_Type : Enums.Gtk_State_Type) return System.Address;
      pragma Import (C, Internal, "ada_style_get_text");
   begin
      return Convert (Internal (Get_Object (Style), State_Type)).all;
   end Get_Text;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Style : System.Address;
         State : Enums.Gtk_State_Type;
         Color : System.Address);
      pragma Import (C, Internal, "ada_style_set_text");

      Col     : aliased Gdk.Color.Gdk_Color := Color;
      Color_A : System.Address := Col'Address;

   begin
      if Color = Gdk.Color.Null_Color then
         Color_A := System.Null_Address;
      end if;

      Internal (Get_Object (Style), State_Type, Color_A);
   end Set_Text;

   --------------
   -- Get_Base --
   --------------

   function Get_Base
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.Color.Gdk_Color
   is
      function Internal
        (Style      : System.Address;
         State_Type : Enums.Gtk_State_Type) return System.Address;
      pragma Import (C, Internal, "ada_style_get_base");
   begin
      return Convert (Internal (Get_Object (Style), State_Type)).all;
   end Get_Base;

   --------------
   -- Set_Base --
   --------------

   procedure Set_Base
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Style : System.Address;
         State : Enums.Gtk_State_Type;
         Color : System.Address);
      pragma Import (C, Internal, "ada_style_set_base");

      Col     : aliased Gdk.Color.Gdk_Color := Color;
      Color_A : System.Address := Col'Address;

   begin
      if Color = Gdk.Color.Null_Color then
         Color_A := System.Null_Address;
      end if;

      Internal (Get_Object (Style), State_Type, Color_A);
   end Set_Base;

   ---------------
   -- Get_Black --
   ---------------

   function Get_Black (Style : Gtk_Style) return Gdk.Color.Gdk_Color is
      function Internal (Style : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_style_get_black");
   begin
      return Convert (Internal (Get_Object (Style))).all;
   end Get_Black;

   ---------------
   -- Set_Black --
   ---------------

   procedure Set_Black (Style : Gtk_Style; Color : Gdk.Color.Gdk_Color) is
      procedure Internal (Style : System.Address; Color : System.Address);
      pragma Import (C, Internal, "ada_style_set_black");

      Col     : aliased Gdk.Color.Gdk_Color := Color;
      Color_A : System.Address := Col'Address;

   begin
      if Color = Gdk.Color.Null_Color then
         Color_A := System.Null_Address;
      end if;

      Internal (Get_Object (Style), Color_A);
   end Set_Black;

   ---------------
   -- Get_White --
   ---------------

   function Get_White (Style : Gtk_Style) return Gdk.Color.Gdk_Color is
      function Internal (Style : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_style_get_white");
   begin
      return Convert (Internal (Get_Object (Style))).all;
   end Get_White;

   ---------------
   -- Set_White --
   ---------------

   procedure Set_White (Style : Gtk_Style; Color : Gdk.Color.Gdk_Color) is
      procedure Internal (Style : System.Address; Color : System.Address);
      pragma Import (C, Internal, "ada_style_set_white");

      Col     : aliased Gdk.Color.Gdk_Color := Color;
      Color_A : System.Address := Col'Address;

   begin
      if Color = Gdk.Color.Null_Color then
         Color_A := System.Null_Address;
      end if;

      Internal (Get_Object (Style), Color_A);
   end Set_White;

   -------------------------
   --  Get_Background_GC  --
   -------------------------

   function Get_Background_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC
   is
      function Internal
        (Style : System.Address; State : Gtk_State_Type) return Gdk.GC.Gdk_GC;
      pragma Import (C, Internal, "ada_style_get_bg_gc");

   begin
      return Internal (Get_Object (Style), State_Type);
   end Get_Background_GC;

   -----------------------
   -- Set_Background_GC --
   -----------------------

   procedure Set_Background_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC)
   is
      procedure Internal
        (Style : System.Address;
         State : Enums.Gtk_State_Type;
         GC    : Gdk.GC.Gdk_GC);
      pragma Import (C, Internal, "ada_style_set_bg_gc");

   begin
      Internal (Get_Object (Style), State_Type, GC);
   end Set_Background_GC;

   -----------------------
   -- Get_Foreground_GC --
   -----------------------

   function Get_Foreground_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC
   is
      function Internal
        (Style : System.Address; State : Gtk_State_Type) return Gdk_GC;
      pragma Import (C, Internal, "ada_style_get_fg_gc");

   begin
      return Internal (Get_Object (Style), State_Type);
   end Get_Foreground_GC;

   -----------------------
   -- Set_Foreground_GC --
   -----------------------

   procedure Set_Foreground_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC)
   is
      procedure Internal
        (Style : System.Address;
         State : Enums.Gtk_State_Type;
         GC    : Gdk.GC.Gdk_GC);
      pragma Import (C, Internal, "ada_style_set_fg_gc");

   begin
      Internal (Get_Object (Style), State_Type, GC);
   end Set_Foreground_GC;

   ------------------
   -- Get_Light_GC --
   ------------------

   function Get_Light_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC
   is
      function Internal
        (Style : System.Address; State : Gtk_State_Type) return Gdk.GC.Gdk_GC;
      pragma Import (C, Internal, "ada_style_get_light_gc");

   begin
      return Internal (Get_Object (Style), State_Type);
   end Get_Light_GC;

   ------------------
   -- Set_Light_GC --
   ------------------

   procedure Set_Light_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC)
   is
      procedure Internal
        (Style : System.Address;
         State : Enums.Gtk_State_Type;
         GC    : Gdk.GC.Gdk_GC);
      pragma Import (C, Internal, "ada_style_set_light_gc");

   begin
      Internal (Get_Object (Style), State_Type, GC);
   end Set_Light_GC;

   -----------------
   -- Get_Dark_GC --
   -----------------

   function Get_Dark_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC
   is
      function Internal
        (Style : System.Address; State : Gtk_State_Type) return Gdk.GC.Gdk_GC;
      pragma Import (C, Internal, "ada_style_get_dark_gc");

   begin
      return Internal (Get_Object (Style), State_Type);
   end Get_Dark_GC;

   -----------------
   -- Set_Dark_GC --
   -----------------

   procedure Set_Dark_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC)
   is
      procedure Internal
        (Style : System.Address;
         State : Enums.Gtk_State_Type;
         GC    : Gdk.GC.Gdk_GC);
      pragma Import (C, Internal, "ada_style_set_dark_gc");

   begin
      Internal (Get_Object (Style), State_Type, GC);
   end Set_Dark_GC;

   -------------------
   -- Get_Middle_GC --
   -------------------

   function Get_Middle_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC
   is
      function Internal
        (Style : System.Address; State : Gtk_State_Type) return Gdk.GC.Gdk_GC;
      pragma Import (C, Internal, "ada_style_get_mid_gc");

   begin
      return Internal (Get_Object (Style), State_Type);
   end Get_Middle_GC;

   -------------------
   -- Set_Middle_GC --
   -------------------

   procedure Set_Middle_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC)
   is
      procedure Internal
        (Style : System.Address;
         State : Enums.Gtk_State_Type;
         GC    : Gdk.GC.Gdk_GC);
      pragma Import (C, Internal, "ada_style_set_mid_gc");

   begin
      Internal (Get_Object (Style), State_Type, GC);
   end Set_Middle_GC;

   -----------------
   -- Get_Text_GC --
   -----------------

   function Get_Text_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC
   is
      function Internal
        (Style : System.Address; State : Gtk_State_Type) return Gdk.GC.Gdk_GC;
      pragma Import (C, Internal, "ada_style_get_text_gc");

   begin
      return Internal (Get_Object (Style), State_Type);
   end Get_Text_GC;

   -----------------
   -- Set_Text_GC --
   -----------------

   procedure Set_Text_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC)
   is
      procedure Internal
        (Style : System.Address;
         State : Enums.Gtk_State_Type;
         GC    : Gdk.GC.Gdk_GC);
      pragma Import (C, Internal, "ada_style_set_text_gc");

   begin
      Internal (Get_Object (Style), State_Type, GC);
   end Set_Text_GC;

   -----------------
   -- Get_Base_GC --
   -----------------

   function Get_Base_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC
   is
      function Internal
        (Style : System.Address; State : Gtk_State_Type) return Gdk.GC.Gdk_GC;
      pragma Import (C, Internal, "ada_style_get_base_gc");

   begin
      return Internal (Get_Object (Style), State_Type);
   end Get_Base_GC;

   -----------------
   -- Set_Base_GC --
   -----------------

   procedure Set_Base_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC)
   is
      procedure Internal
        (Style : System.Address;
         State : Enums.Gtk_State_Type;
         GC    : Gdk.GC.Gdk_GC);
      pragma Import (C, Internal, "ada_style_set_base_gc");

   begin
      Internal (Get_Object (Style), State_Type, GC);
   end Set_Base_GC;

   -------------------
   -- Set_Bg_Pixmap --
   -------------------

   procedure Set_Bg_Pixmap
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      Pixmap     : Gdk.Pixmap.Gdk_Pixmap)
   is
      procedure Internal
        (Style  : System.Address;
         State  : Enums.Gtk_State_Type;
         Pixmap : Gdk.Pixmap.Gdk_Pixmap);
      pragma Import (C, Internal, "ada_style_set_bg_pixmap");

   begin
      Internal (Get_Object (Style), State_Type, Pixmap);
   end Set_Bg_Pixmap;

   -------------------
   -- Get_Bg_Pixmap --
   -------------------

   function Get_Bg_Pixmap
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.Pixmap.Gdk_Pixmap
   is
      function Internal
        (Style : System.Address;
         State : Enums.Gtk_State_Type) return Gdk.Pixmap.Gdk_Pixmap;
      pragma Import (C, Internal, "ada_style_get_bg_pixmap");

   begin
      return Internal (Get_Object (Style), State_Type);
   end Get_Bg_Pixmap;

   ------------------
   -- Paint_Handle --
   ------------------

   procedure Paint_Handle
     (Style               : Gtk_Style;
      Window              : Gdk.Gdk_Window;
      State_Type          : Gtk.Enums.Gtk_State_Type;
      Shadow_Type         : Gtk.Enums.Gtk_Shadow_Type;
      Area                : Gdk_Rectangle;
      Widget              : access Glib.Object.GObject_Record'Class;
      Detail              : String := "paned";
      X, Y, Width, Height : Gint;
      Orientation         : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal
        (Style               : System.Address;
         Window              : Gdk.Gdk_Window;
         State_Type          : Gtk.Enums.Gtk_State_Type;
         Shadow_Type         : Gtk.Enums.Gtk_Shadow_Type;
         Area                : Gdk_Rectangle;
         Widget              : System.Address;
         Detail              : String;
         X, Y, Width, Height : Gint;
         Orientation         : Gtk.Enums.Gtk_Orientation);
      procedure Internal
        (Style               : System.Address;
         Window              : Gdk.Gdk_Window;
         State_Type          : Gtk.Enums.Gtk_State_Type;
         Shadow_Type         : Gtk.Enums.Gtk_Shadow_Type;
         Area                : System.Address;
         Widget              : System.Address;
         Detail              : String;
         X, Y, Width, Height : Gint;
         Orientation         : Gtk.Enums.Gtk_Orientation);
      pragma Import (C, Internal, "gtk_paint_handle");
      use type Gdk_Rectangle;
   begin
      if Area = Full_Area then
         Internal
           (Get_Object (Style), Window, State_Type, Shadow_Type,
            System.Null_Address, Get_Object (Widget), Detail & ASCII.NUL,
            X, Y, Width, Height, Orientation);
      else
         Internal
           (Get_Object (Style), Window, State_Type, Shadow_Type,
            Area, Get_Object (Widget), Detail & ASCII.NUL,
            X, Y, Width, Height, Orientation);
      end if;
   end Paint_Handle;

   ------------
   -- Attach --
   ------------

   function Attach
     (Style  : Gtk_Style; Window : Gdk.Window.Gdk_Window) return Gtk_Style
   is
      function Internal
        (Style  : System.Address;
         Window : Gdk_Window) return System.Address;
      pragma Import (C, Internal, "gtk_style_attach");
      Stub : Gtk_Style_Record;
   begin
      return Gtk_Style
        (Get_User_Data (Internal (Get_Object (Style), Window), Stub));
   end Attach;

   ------------
   -- Detach --
   ------------

   procedure Detach (Style : Gtk_Style) is
      procedure Internal (Style : System.Address);
      pragma Import (C, Internal, "gtk_style_detach");
   begin
      Internal (Get_Object (Style));
   end Detach;

   ----------
   -- Copy --
   ----------

   function Copy (Source : Gtk_Style) return Gtk_Style is
      function Internal (Source : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_style_copy");
      Stub : Gtk_Style_Record;
   begin
      return Gtk_Style (Get_User_Data (Internal (Get_Object (Source)), Stub));
   end Copy;

   ------------------
   -- Set_Black_GC --
   ------------------

   procedure Set_Black_GC (Style : Gtk_Style; GC : Gdk.GC.Gdk_GC) is
      procedure Internal (Style : System.Address; GC : Gdk_GC);
      pragma Import (C, Internal, "ada_style_set_black_gc");
   begin
      Internal (Get_Object (Style), GC);
   end Set_Black_GC;

   ------------------
   -- Get_Black_GC --
   ------------------

   function Get_Black_GC (Style : Gtk_Style) return Gdk.GC.Gdk_GC is
      function Internal (Style : System.Address) return Gdk_GC;
      pragma Import (C, Internal, "ada_style_get_black_gc");
   begin
      return Internal (Get_Object (Style));
   end Get_Black_GC;

   ------------------
   -- Set_White_GC --
   ------------------

   procedure Set_White_GC (Style : Gtk_Style; GC : Gdk.GC.Gdk_GC) is
      procedure Internal (Style : System.Address; GC : Gdk_GC);
      pragma Import (C, Internal, "ada_style_set_white_gc");
   begin
      Internal (Get_Object (Style), GC);
   end Set_White_GC;

   ------------------
   -- Get_White_GC --
   ------------------

   function Get_White_GC (Style : Gtk_Style) return Gdk.GC.Gdk_GC is
      function Internal (Style : System.Address) return Gdk_GC;
      pragma Import (C, Internal, "ada_style_get_white_gc");
   begin
      return Internal (Get_Object (Style));
   end Get_White_GC;

   -----------------
   -- X_Thickness --
   -----------------

   function X_Thickness (Style : Gtk_Style) return Gint is
      function Internal (Style : System.Address) return Gint;
      pragma Import (C, Internal, "ada_style_get_x_thickness");
   begin
      return Internal (Get_Object (Style));
   end X_Thickness;

   -----------------
   -- Y_Thickness --
   -----------------

   function Y_Thickness (Style : Gtk_Style) return Gint is
      function Internal (Style : System.Address) return Gint;
      pragma Import (C, Internal, "ada_style_get_y_thickness");
   begin
      return Internal (Get_Object (Style));
   end Y_Thickness;

   --------------------------
   -- Set_Font_Description --
   --------------------------

   procedure Set_Font_Description
     (Style : Gtk_Style; Desc : Pango.Font.Pango_Font_Description)
   is
      procedure Internal (S : System.Address; Desc : Pango_Font_Description);
      pragma Import (C, Internal, "ada_style_set_font_description");
   begin
      Internal (Get_Object (Style), Desc);
   end Set_Font_Description;

   --------------------------
   -- Get_Font_Description --
   --------------------------

   function Get_Font_Description
     (Style : Gtk_Style) return Pango.Font.Pango_Font_Description
   is
      function Internal (S : System.Address) return Pango_Font_Description;
      pragma Import (C, Internal, "ada_style_get_font_description");
   begin
      return Internal (Get_Object (Style));
   end Get_Font_Description;

   --------------
   -- Get_Font --
   --------------

   function Get_Font (Style : Gtk_Style) return Gdk.Font.Gdk_Font is
      function Internal (Style : System.Address) return Gdk.Font.Gdk_Font;
      pragma Import (C, Internal, "gtk_style_get_font");
   begin
      return Internal (Get_Object (Style));
   end Get_Font;

   ---------------------------
   -- Draw_Insertion_Cursor --
   ---------------------------

   procedure Draw_Insertion_Cursor
     (Widget     : access Glib.Object.GObject_Record'Class;
      Drawable   : Gdk_Drawable;
      Area       : Gdk.Rectangle.Gdk_Rectangle;
      Location   : Gdk.Rectangle.Gdk_Rectangle;
      Is_Primary : Boolean;
      Direction  : Gtk_Text_Direction;
      Draw_Arrow : Boolean)
   is
      procedure Internal
        (Widget     : System.Address;
         Drawable   : Gdk_Drawable;
         Area       : Gdk_Rectangle;
         Location   : Gdk_Rectangle;
         Is_Primary : Gboolean;
         Direction  : Gtk_Text_Direction;
         Draw_Arrow : Gboolean);
      pragma Import (C, Internal, "gtk_draw_insertion_cursor");
   begin
      Internal (Get_Object (Widget), Drawable, Area, Location,
                Boolean'Pos (Is_Primary), Direction, Boolean'Pos (Draw_Arrow));
   end Draw_Insertion_Cursor;

   ------------------------------
   -- Apply_Default_Background --
   ------------------------------

   procedure Apply_Default_Background
     (Style      : access Gtk_Style_Record;
      Window     : Gdk_Window;
      Set_Bg     : Boolean;
      State_Type : Gtk_State_Type;
      Area       : Gdk_Rectangle;
      X          : Gint;
      Y          : Gint;
      Width      : Gint;
      Height     : Gint)
   is
      procedure Internal
        (Style      : System.Address;
         Window     : Gdk_Window;
         Set_Bg     : Gboolean;
         State_Type : Gtk_State_Type;
         Area       : Gdk_Rectangle;
         X          : Gint;
         Y          : Gint;
         Width      : Gint;
         Height     : Gint);
      pragma Import (C, Internal, "gtk_style_apply_default_background");
   begin
      Internal (Get_Object (Style), Window, Boolean'Pos (Set_Bg),
                State_Type, Area, X, Y, Width, Height);
   end Apply_Default_Background;

   -----------------
   -- Paint_Arrow --
   -----------------

   procedure Paint_Arrow
     (Style       : access Gtk_Style_Record;
      Window      : Gdk_Window;
      State_Type  : Gtk_State_Type;
      Shadow_Type : Gtk_Shadow_Type;
      Area        : Gdk_Rectangle := Full_Area;
      Widget      : access GObject_Record'Class;
      Detail      : String := "";
      Arrow_Type  : Gtk_Arrow_Type;
      Fill        : Boolean;
      X           : Gint;
      Y           : Gint;
      Width       : Gint;
      Height      : Gint)
   is
      procedure Internal
        (Style       : System.Address;
         Window      : Gdk_Window;
         State_Type  : Gtk_State_Type;
         Shadow_Type : Gtk_Shadow_Type;
         Area        : Gdk_Rectangle;
         Widget      : System.Address;
         Detail      : String;
         Arrow_Type  : Gtk_Arrow_Type;
         Fill        : Gboolean;
         X           : Gint;
         Y           : Gint;
         Width       : Gint;
         Height      : Gint);
      pragma Import (C, Internal, "gtk_paint_arrow");
   begin
      Internal (Get_Object (Style), Window, State_Type, Shadow_Type, Area,
                Get_Object (Widget), Detail & ASCII.NUL, Arrow_Type,
                Boolean'Pos (Fill), X, Y, Width, Height);
   end Paint_Arrow;

   ---------------
   -- Paint_Box --
   ---------------

   procedure Paint_Box
     (Style       : access Gtk_Style_Record;
      Window      : Gdk_Window;
      State_Type  : Gtk_State_Type;
      Shadow_Type : Gtk_Shadow_Type;
      Area        : Gdk_Rectangle := Full_Area;
      Widget      : access GObject_Record'Class;
      Detail      : String := "";
      X           : Gint;
      Y           : Gint;
      Width       : Gint;
      Height      : Gint)
   is
      procedure Internal
        (Style       : System.Address;
         Window      : Gdk_Window;
         State_Type  : Gtk_State_Type;
         Shadow_Type : Gtk_Shadow_Type;
         Area        : Gdk_Rectangle;
         Widget      : System.Address;
         Detail      : String;
         X           : Gint;
         Y           : Gint;
         Width       : Gint;
         Height      : Gint);
      pragma Import (C, Internal, "gtk_paint_box");
   begin
      Internal (Get_Object (Style), Window, State_Type, Shadow_Type, Area,
                Get_Object (Widget), Detail & ASCII.NUL, X, Y, Width, Height);
   end Paint_Box;

   -------------------
   -- Paint_Box_Gap --
   -------------------

   procedure Paint_Box_Gap
     (Style       : access Gtk_Style_Record;
      Window      : Gdk_Window;
      State_Type  : Gtk_State_Type;
      Shadow_Type : Gtk_Shadow_Type;
      Area        : Gdk_Rectangle := Full_Area;
      Widget      : access GObject_Record'Class;
      Detail      : String := "";
      X           : Gint;
      Y           : Gint;
      Width       : Gint;
      Height      : Gint;
      Gap_Side    : Gtk_Position_Type;
      Gap_X       : Gint;
      Gap_Width   : Gint)
   is
      procedure Internal
        (Style       : System.Address;
         Window      : Gdk_Window;
         State_Type  : Gtk_State_Type;
         Shadow_Type : Gtk_Shadow_Type;
         Area        : Gdk_Rectangle;
         Widget      : System.Address;
         Detail      : String;
         X           : Gint;
         Y           : Gint;
         Width       : Gint;
         Height      : Gint;
         Gap_Side    : Gtk_Position_Type;
         Gap_X       : Gint;
         Gap_Width   : Gint);
      pragma Import (C, Internal, "gtk_paint_box_gap");
   begin
      Internal (Get_Object (Style), Window, State_Type, Shadow_Type, Area,
                Get_Object (Widget), Detail & ASCII.NUL, X, Y, Width, Height,
                Gap_Side, Gap_X, Gap_Width);
   end Paint_Box_Gap;

   -----------------
   -- Paint_Check --
   -----------------

   procedure Paint_Check
     (Style       : access Gtk_Style_Record;
      Window      : Gdk_Window;
      State_Type  : Gtk_State_Type;
      Shadow_Type : Gtk_Shadow_Type;
      Area        : Gdk_Rectangle := Full_Area;
      Widget      : access GObject_Record'Class;
      Detail      : String := "";
      X           : Gint;
      Y           : Gint;
      Width       : Gint;
      Height      : Gint)
   is
      procedure Internal
        (Style       : System.Address;
         Window      : Gdk_Window;
         State_Type  : Gtk_State_Type;
         Shadow_Type : Gtk_Shadow_Type;
         Area        : Gdk_Rectangle;
         Widget      : System.Address;
         Detail      : String;
         X           : Gint;
         Y           : Gint;
         Width       : Gint;
         Height      : Gint);
      pragma Import (C, Internal, "gtk_paint_check");
   begin
      Internal (Get_Object (Style), Window, State_Type, Shadow_Type, Area,
                Get_Object (Widget), Detail & ASCII.NUL, X, Y, Width, Height);
   end Paint_Check;

   -------------------
   -- Paint_Diamond --
   -------------------

   procedure Paint_Diamond
     (Style       : access Gtk_Style_Record;
      Window      : Gdk_Window;
      State_Type  : Gtk_State_Type;
      Shadow_Type : Gtk_Shadow_Type;
      Area        : Gdk_Rectangle := Full_Area;
      Widget      : access GObject_Record'Class;
      Detail      : String := "";
      X           : Gint;
      Y           : Gint;
      Width       : Gint;
      Height      : Gint)
   is
      procedure Internal
        (Style       : System.Address;
         Window      : Gdk_Window;
         State_Type  : Gtk_State_Type;
         Shadow_Type : Gtk_Shadow_Type;
         Area        : Gdk_Rectangle;
         Widget      : System.Address;
         Detail      : String;
         X           : Gint;
         Y           : Gint;
         Width       : Gint;
         Height      : Gint);
      pragma Import (C, Internal, "gtk_paint_diamond");
   begin
      Internal (Get_Object (Style), Window, State_Type, Shadow_Type, Area,
                Get_Object (Widget), Detail & ASCII.NUL, X, Y, Width, Height);
   end Paint_Diamond;

   --------------------
   -- Paint_Expander --
   --------------------

   procedure Paint_Expander
     (Style          : access Gtk_Style_Record;
      Window         : Gdk_Window;
      State_Type     : Gtk_State_Type;
      Area           : Gdk_Rectangle := Full_Area;
      Widget         : access GObject_Record'Class;
      Detail         : String := "";
      X              : Gint;
      Y              : Gint;
      Expander_Style : Gtk_Expander_Style)
   is
      procedure Internal
        (Style          : System.Address;
         Window         : Gdk_Window;
         State_Type     : Gtk_State_Type;
         Area           : Gdk_Rectangle;
         Widget         : System.Address;
         Detail         : String;
         X              : Gint;
         Y              : Gint;
         Expander_Style : Gtk_Expander_Style);
      pragma Import (C, Internal, "gtk_paint_expander");
   begin
      Internal (Get_Object (Style), Window, State_Type, Area,
                Get_Object (Widget), Detail & ASCII.NUL, X, Y, Expander_Style);
   end Paint_Expander;

   ---------------------
   -- Paint_Extension --
   ---------------------

   procedure Paint_Extension
     (Style       : access Gtk_Style_Record;
      Window      : Gdk_Window;
      State_Type  : Gtk_State_Type;
      Shadow_Type : Gtk_Shadow_Type;
      Area        : Gdk_Rectangle := Full_Area;
      Widget      : access GObject_Record'Class;
      Detail      : String := "";
      X           : Gint;
      Y           : Gint;
      Width       : Gint;
      Height      : Gint;
      Gap_Side    : Gtk_Position_Type)
   is
      procedure Internal
        (Style       : System.Address;
         Window      : Gdk_Window;
         State_Type  : Gtk_State_Type;
         Shadow_Type : Gtk_Shadow_Type;
         Area        : Gdk_Rectangle;
         Widget      : System.Address;
         Detail      : String;
         X           : Gint;
         Y           : Gint;
         Width       : Gint;
         Height      : Gint;
         Gap_Side    : Gtk_Position_Type);
      pragma Import (C, Internal, "gtk_paint_extension");
   begin
      Internal (Get_Object (Style), Window, State_Type, Shadow_Type, Area,
                Get_Object (Widget), Detail & ASCII.NUL, X, Y, Width, Height,
                Gap_Side);
   end Paint_Extension;

   --------------------
   -- Paint_Flat_Box --
   --------------------

   procedure Paint_Flat_Box
     (Style       : access Gtk_Style_Record;
      Window      : Gdk_Window;
      State_Type  : Gtk_State_Type;
      Shadow_Type : Gtk_Shadow_Type;
      Area        : Gdk_Rectangle := Full_Area;
      Widget      : access GObject_Record'Class;
      Detail      : String := "";
      X           : Gint;
      Y           : Gint;
      Width       : Gint;
      Height      : Gint)
   is
      procedure Internal
        (Style       : System.Address;
         Window      : Gdk_Window;
         State_Type  : Gtk_State_Type;
         Shadow_Type : Gtk_Shadow_Type;
         Area        : Gdk_Rectangle;
         Widget      : System.Address;
         Detail      : String;
         X           : Gint;
         Y           : Gint;
         Width       : Gint;
         Height      : Gint);
      pragma Import (C, Internal, "gtk_paint_flat_box");
   begin
      Internal (Get_Object (Style), Window, State_Type, Shadow_Type, Area,
                Get_Object (Widget), Detail & ASCII.NUL, X, Y, Width, Height);
   end Paint_Flat_Box;

   -----------------
   -- Paint_Focus --
   -----------------

   procedure Paint_Focus
     (Style      : access Gtk_Style_Record;
      Window     : Gdk_Window;
      State_Type : Gtk_State_Type;
      Area       : Gdk_Rectangle := Full_Area;
      Widget     : access GObject_Record'Class;
      Detail     : String := "";
      X          : Gint;
      Y          : Gint;
      Width      : Gint;
      Height     : Gint)
   is
      procedure Internal
        (Style      : System.Address;
         Window     : Gdk_Window;
         State_Type : Gtk_State_Type;
         Area       : Gdk_Rectangle;
         Widget     : System.Address;
         Detail     : String;
         X          : Gint;
         Y          : Gint;
         Width      : Gint;
         Height     : Gint);
      pragma Import (C, Internal, "gtk_paint_focus");
   begin
      Internal (Get_Object (Style), Window, State_Type, Area,
                Get_Object (Widget), Detail & ASCII.NUL, X, Y, Width, Height);
   end Paint_Focus;

   -----------------
   -- Paint_Hline --
   -----------------

   procedure Paint_Hline
     (Style      : access Gtk_Style_Record;
      Window     : Gdk_Window;
      State_Type : Gtk_State_Type;
      Area       : Gdk_Rectangle := Full_Area;
      Widget     : access GObject_Record'Class;
      Detail     : String := "";
      X1         : Gint;
      X2         : Gint;
      Y          : Gint)
   is
      procedure Internal
        (Style      : System.Address;
         Window     : Gdk_Window;
         State_Type : Gtk_State_Type;
         Area       : Gdk_Rectangle;
         Widget     : System.Address;
         Detail     : String;
         X1         : Gint;
         X2         : Gint;
         Y          : Gint);
      pragma Import (C, Internal, "gtk_paint_hline");
   begin
      Internal (Get_Object (Style), Window, State_Type, Area,
                Get_Object (Widget), Detail & ASCII.NUL, X1, X2, Y);
   end Paint_Hline;

   ------------------
   -- Paint_Layout --
   ------------------

   procedure Paint_Layout
     (Style      : access Gtk_Style_Record;
      Window     : Gdk_Window;
      State_Type : Gtk_State_Type;
      Use_Text   : Boolean;
      Area       : Gdk_Rectangle := Full_Area;
      Widget     : access GObject_Record'Class;
      Detail     : String := "";
      X          : Gint;
      Y          : Gint;
      Layout     : Pango.Layout.Pango_Layout)
   is
      procedure Internal
        (Style      : System.Address;
         Window     : Gdk_Window;
         State_Type : Gtk_State_Type;
         Use_Text   : Gboolean;
         Area       : Gdk_Rectangle;
         Widget     : System.Address;
         Detail     : String;
         X          : Gint;
         Y          : Gint;
         Layout     : System.Address);
      pragma Import (C, Internal, "gtk_paint_layout");
   begin
      Internal (Get_Object (Style), Window, State_Type, Boolean'Pos (Use_Text),
                Area, Get_Object (Widget), Detail & ASCII.NUL, X, Y,
                Get_Object (Layout));
   end Paint_Layout;

   ------------------
   -- Paint_Option --
   ------------------

   procedure Paint_Option
     (Style       : access Gtk_Style_Record;
      Window      : Gdk_Window;
      State_Type  : Gtk_State_Type;
      Shadow_Type : Gtk_Shadow_Type;
      Area        : Gdk_Rectangle := Full_Area;
      Widget      : access GObject_Record'Class;
      Detail      : String := "";
      X           : Gint;
      Y           : Gint;
      Width       : Gint;
      Height      : Gint)
   is
      procedure Internal
        (Style       : System.Address;
         Window      : Gdk_Window;
         State_Type  : Gtk_State_Type;
         Shadow_Type : Gtk_Shadow_Type;
         Area        : Gdk_Rectangle;
         Widget      : System.Address;
         Detail      : String;
         X           : Gint;
         Y           : Gint;
         Width       : Gint;
         Height      : Gint);
      pragma Import (C, Internal, "gtk_paint_option");
   begin
      Internal (Get_Object (Style), Window, State_Type, Shadow_Type, Area,
                Get_Object (Widget), Detail & ASCII.NUL, X, Y, Width, Height);
   end Paint_Option;

   -------------------
   -- Paint_Polygon --
   -------------------

   procedure Paint_Polygon
     (Style       : access Gtk_Style_Record;
      Window      : Gdk_Window;
      State_Type  : Gtk_State_Type;
      Shadow_Type : Gtk_Shadow_Type;
      Area        : Gdk_Rectangle := Full_Area;
      Widget      : access GObject_Record'Class;
      Detail      : String := "";
      Points      : Gdk.Types.Gdk_Points_Array;
      Fill        : Boolean)
   is
      procedure Internal
        (Style       : System.Address;
         Window      : Gdk_Window;
         State_Type  : Gtk_State_Type;
         Shadow_Type : Gtk_Shadow_Type;
         Area        : Gdk_Rectangle;
         Widget      : System.Address;
         Detail      : String;
         Points      : System.Address;
         Npoints     : Gint;
         Fill        : Gboolean);
      pragma Import (C, Internal, "gtk_paint_polygon");
   begin
      Internal (Get_Object (Style), Window, State_Type, Shadow_Type, Area,
                Get_Object (Widget), Detail & ASCII.NUL,
                Points (Points'First)'Address, Points'Length,
                Boolean'Pos (Fill));
   end Paint_Polygon;

   -----------------------
   -- Paint_Resize_Grip --
   -----------------------

   procedure Paint_Resize_Grip
     (Style      : access Gtk_Style_Record;
      Window     : Gdk_Window;
      State_Type : Gtk_State_Type;
      Area       : Gdk_Rectangle := Full_Area;
      Widget     : access GObject_Record'Class;
      Detail     : String := "";
      Edge       : Gdk.Window.Gdk_Window_Edge;
      X          : Gint;
      Y          : Gint;
      Width      : Gint;
      Height     : Gint)
   is
      procedure Internal
        (Style      : System.Address;
         Window     : Gdk_Window;
         State_Type : Gtk_State_Type;
         Area       : Gdk_Rectangle;
         Widget     : System.Address;
         Detail     : String;
         Edge       : Gdk.Window.Gdk_Window_Edge;
         X          : Gint;
         Y          : Gint;
         Width      : Gint;
         Height     : Gint);
      pragma Import (C, Internal, "gtk_paint_resize_grip");
   begin
      Internal (Get_Object (Style), Window, State_Type, Area,
                Get_Object (Widget), Detail & ASCII.NUL, Edge, X, Y, Width,
                Height);
   end Paint_Resize_Grip;

   ------------------
   -- Paint_Shadow --
   ------------------

   procedure Paint_Shadow
     (Style       : access Gtk_Style_Record;
      Window      : Gdk_Window;
      State_Type  : Gtk_State_Type;
      Shadow_Type : Gtk_Shadow_Type;
      Area        : Gdk_Rectangle := Full_Area;
      Widget      : access GObject_Record'Class;
      Detail      : String := "";
      X           : Gint;
      Y           : Gint;
      Width       : Gint;
      Height      : Gint)
   is
      procedure Internal
        (Style       : System.Address;
         Window      : Gdk_Window;
         State_Type  : Gtk_State_Type;
         Shadow_Type : Gtk_Shadow_Type;
         Area        : Gdk_Rectangle;
         Widget      : System.Address;
         Detail      : String;
         X           : Gint;
         Y           : Gint;
         Width       : Gint;
         Height      : Gint);
      pragma Import (C, Internal, "gtk_paint_shadow");
   begin
      Internal (Get_Object (Style), Window, State_Type, Shadow_Type, Area,
                Get_Object (Widget), Detail & ASCII.NUL, X, Y, Width, Height);
   end Paint_Shadow;

   ----------------------
   -- Paint_Shadow_Gap --
   ----------------------

   procedure Paint_Shadow_Gap
     (Style       : access Gtk_Style_Record;
      Window      : Gdk_Window;
      State_Type  : Gtk_State_Type;
      Shadow_Type : Gtk_Shadow_Type;
      Area        : Gdk_Rectangle := Full_Area;
      Widget      : access GObject_Record'Class;
      Detail      : String := "";
      X           : Gint;
      Y           : Gint;
      Width       : Gint;
      Height      : Gint;
      Gap_Side    : Gtk_Position_Type;
      Gap_X       : Gint;
      Gap_Width   : Gint)
   is
      procedure Internal
        (Style       : System.Address;
         Window      : Gdk_Window;
         State_Type  : Gtk_State_Type;
         Shadow_Type : Gtk_Shadow_Type;
         Area        : Gdk_Rectangle;
         Widget      : System.Address;
         Detail      : String;
         X           : Gint;
         Y           : Gint;
         Width       : Gint;
         Height      : Gint;
         Gap_Side    : Gtk_Position_Type;
         Gap_X       : Gint;
         Gap_Width   : Gint);
      pragma Import (C, Internal, "gtk_paint_shadow_gap");
   begin
      Internal (Get_Object (Style), Window, State_Type, Shadow_Type, Area,
                Get_Object (Widget), Detail & ASCII.NUL, X, Y, Width, Height,
                Gap_Side, Gap_X, Gap_Width);
   end Paint_Shadow_Gap;

   ------------------
   -- Paint_Slider --
   ------------------

   procedure Paint_Slider
     (Style       : access Gtk_Style_Record;
      Window      : Gdk_Window;
      State_Type  : Gtk_State_Type;
      Shadow_Type : Gtk_Shadow_Type;
      Area        : Gdk_Rectangle := Full_Area;
      Widget      : access GObject_Record'Class;
      Detail      : String := "";
      X           : Gint;
      Y           : Gint;
      Width       : Gint;
      Height      : Gint;
      Orientation : Gtk_Orientation)
   is
      procedure Internal
        (Style       : System.Address;
         Window      : Gdk_Window;
         State_Type  : Gtk_State_Type;
         Shadow_Type : Gtk_Shadow_Type;
         Area        : Gdk_Rectangle;
         Widget      : System.Address;
         Detail      : String;
         X           : Gint;
         Y           : Gint;
         Width       : Gint;
         Height      : Gint;
         Orientation : Gtk_Orientation);
      pragma Import (C, Internal, "gtk_paint_slider");
   begin
      Internal (Get_Object (Style), Window, State_Type, Shadow_Type, Area,
                Get_Object (Widget), Detail & ASCII.NUL, X, Y, Width, Height,
                Orientation);
   end Paint_Slider;

   ---------------
   -- Paint_Tab --
   ---------------

   procedure Paint_Tab
     (Style       : access Gtk_Style_Record;
      Window      : Gdk_Window;
      State_Type  : Gtk_State_Type;
      Shadow_Type : Gtk_Shadow_Type;
      Area        : Gdk_Rectangle := Full_Area;
      Widget      : access GObject_Record'Class;
      Detail      : String := "";
      X           : Gint;
      Y           : Gint;
      Width       : Gint;
      Height      : Gint)
   is
      procedure Internal
        (Style       : System.Address;
         Window      : Gdk_Window;
         State_Type  : Gtk_State_Type;
         Shadow_Type : Gtk_Shadow_Type;
         Area        : Gdk_Rectangle;
         Widget      : System.Address;
         Detail      : String;
         X           : Gint;
         Y           : Gint;
         Width       : Gint;
         Height      : Gint);
      pragma Import (C, Internal, "gtk_paint_tab");
   begin
      Internal (Get_Object (Style), Window, State_Type, Shadow_Type, Area,
                Get_Object (Widget), Detail & ASCII.NUL, X, Y, Width, Height);
   end Paint_Tab;

   -----------------
   -- Paint_Vline --
   -----------------

   procedure Paint_Vline
     (Style      : access Gtk_Style_Record;
      Window     : Gdk_Window;
      State_Type : Gtk_State_Type;
      Area       : Gdk_Rectangle := Full_Area;
      Widget     : access GObject_Record'Class;
      Detail     : String := "";
      Y1         : Gint;
      Y2         : Gint;
      X          : Gint)
   is
      procedure Internal
        (Style      : System.Address;
         Window     : Gdk_Window;
         State_Type : Gtk_State_Type;
         Area       : Gdk_Rectangle;
         Widget     : System.Address;
         Detail     : String;
         Y1         : Gint;
         Y2         : Gint;
         X          : Gint);
      pragma Import (C, Internal, "gtk_paint_vline");
   begin
      Internal (Get_Object (Style), Window, State_Type, Area,
                Get_Object (Widget), Detail & ASCII.NUL, Y1, Y2, X);
   end Paint_Vline;

   -----------------
   -- Draw_Shadow --
   -----------------

   procedure Draw_Shadow
     (Style       : access Gtk_Style_Record;
      Window      : Gdk_Window;
      State_Type  : Gtk_State_Type;
      Shadow_Type : Gtk_Shadow_Type;
      X           : Gint;
      Y           : Gint;
      Width       : Gint;
      Height      : Gint)
   is
      procedure Internal
        (Style       : System.Address;
         Window      : Gdk_Window;
         State_Type  : Gtk_State_Type;
         Shadow_Type : Gtk_Shadow_Type;
         X           : Gint;
         Y           : Gint;
         Width       : Gint;
         Height      : Gint);
      pragma Import (C, Internal, "gtk_draw_shadow");
   begin
      Internal (Get_Object (Style), Window, State_Type, Shadow_Type, X, Y,
                Width, Height);
   end Draw_Shadow;

   -----------------
   -- Border_Copy --
   -----------------

   function Border_Copy (Border : access Gtk_Border_Record)
      return Gtk_Border
   is
      use Border_Address_Access_Conversions;

      function Internal (Border : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_border_copy");
   begin
      return Gtk_Border (To_Pointer (Internal (Border.all'Address)));
   end Border_Copy;

   -----------------
   -- Border_Free --
   -----------------

   procedure Border_Free (Border : access Gtk_Border_Record) is
      procedure Internal (Border : System.Address);
      pragma Import (C, Internal, "gtk_border_free");
   begin
      Internal (Border.all'Address);
   end Border_Free;

   ---------------------
   -- Border_Get_Type --
   ---------------------

   function Border_Get_Type return GType is
      function Internal return GType;
      pragma Import (C, Internal, "gtk_border_get_type");
   begin
      return Internal;
   end Border_Get_Type;

end Gtk.Style;
