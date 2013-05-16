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

package body Gtk.Ruler is
   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Ruler_Record);
   pragma Unreferenced (Type_Conversion);

   --------------------
   -- Gtk_New_Hruler --
   --------------------

   procedure Gtk_New_Hruler (Ruler : out Gtk_Hruler) is
   begin
      Ruler := new Gtk_Hruler_Record;
      Gtk.Ruler.Initialize_Hruler (Ruler);
   end Gtk_New_Hruler;

   --------------------
   -- Gtk_New_Vruler --
   --------------------

   procedure Gtk_New_Vruler (Ruler : out Gtk_Vruler) is
   begin
      Ruler := new Gtk_Vruler_Record;
      Gtk.Ruler.Initialize_Vruler (Ruler);
   end Gtk_New_Vruler;

   -----------------------
   -- Initialize_Hruler --
   -----------------------

   procedure Initialize_Hruler (Ruler : access Gtk_Hruler_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_hruler_new");
   begin
      Set_Object (Ruler, Internal);
   end Initialize_Hruler;

   -----------------------
   -- Initialize_Vruler --
   -----------------------

   procedure Initialize_Vruler (Ruler : access Gtk_Vruler_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_vruler_new");
   begin
      Set_Object (Ruler, Internal);
   end Initialize_Vruler;

   --------------
   -- Draw_Pos --
   --------------

   procedure Draw_Pos (Ruler : access Gtk_Ruler_Record) is
      procedure Internal (Ruler : System.Address);
      pragma Import (C, Internal, "gtk_ruler_draw_pos");
   begin
      Internal (Get_Object (Ruler));
   end Draw_Pos;

   ----------------
   -- Draw_Ticks --
   ----------------

   procedure Draw_Ticks (Ruler : access Gtk_Ruler_Record) is
      procedure Internal (Ruler : System.Address);
      pragma Import (C, Internal, "gtk_ruler_draw_ticks");
   begin
      Internal (Get_Object (Ruler));
   end Draw_Ticks;

   ----------------
   -- Get_Metric --
   ----------------

   function Get_Metric
      (Ruler : access Gtk_Ruler_Record) return Gtk.Enums.Gtk_Metric_Type
   is
      function Internal (Ruler : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_ruler_get_metric");
   begin
      return Gtk.Enums.Gtk_Metric_Type'Val (Internal (Get_Object (Ruler)));
   end Get_Metric;

   ---------------
   -- Get_Range --
   ---------------

   procedure Get_Range
      (Ruler    : access Gtk_Ruler_Record;
       Lower    : out Gdouble;
       Upper    : out Gdouble;
       Position : out Gdouble;
       Max_Size : out Gdouble)
   is
      procedure Internal
         (Ruler    : System.Address;
          Lower    : out Gdouble;
          Upper    : out Gdouble;
          Position : out Gdouble;
          Max_Size : out Gdouble);
      pragma Import (C, Internal, "gtk_ruler_get_range");
   begin
      Internal (Get_Object (Ruler), Lower, Upper, Position, Max_Size);
   end Get_Range;

   ----------------
   -- Set_Metric --
   ----------------

   procedure Set_Metric
      (Ruler  : access Gtk_Ruler_Record;
       Metric : Gtk.Enums.Gtk_Metric_Type)
   is
      procedure Internal (Ruler : System.Address; Metric : Integer);
      pragma Import (C, Internal, "gtk_ruler_set_metric");
   begin
      Internal (Get_Object (Ruler), Gtk.Enums.Gtk_Metric_Type'Pos (Metric));
   end Set_Metric;

   ---------------
   -- Set_Range --
   ---------------

   procedure Set_Range
      (Ruler    : access Gtk_Ruler_Record;
       Lower    : Gdouble;
       Upper    : Gdouble;
       Position : Gdouble;
       Max_Size : Gdouble)
   is
      procedure Internal
         (Ruler    : System.Address;
          Lower    : Gdouble;
          Upper    : Gdouble;
          Position : Gdouble;
          Max_Size : Gdouble);
      pragma Import (C, Internal, "gtk_ruler_set_range");
   begin
      Internal (Get_Object (Ruler), Lower, Upper, Position, Max_Size);
   end Set_Range;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : access Gtk_Ruler_Record) return Gtk.Enums.Gtk_Orientation
   is
      function Internal (Self : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_orientable_get_orientation");
   begin
      return Gtk.Enums.Gtk_Orientation'Val (Internal (Get_Object (Self)));
   end Get_Orientation;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
      (Self        : access Gtk_Ruler_Record;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal (Self : System.Address; Orientation : Integer);
      pragma Import (C, Internal, "gtk_orientable_set_orientation");
   begin
      Internal (Get_Object (Self), Gtk.Enums.Gtk_Orientation'Pos (Orientation));
   end Set_Orientation;

end Gtk.Ruler;
