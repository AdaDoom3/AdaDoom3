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
with Interfaces.C.Strings;       use Interfaces.C.Strings;

package body Gtk.Scale is
   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Scale_Record);
   pragma Unreferenced (Type_Conversion);

   --------------------
   -- Gtk_New_Hscale --
   --------------------

   procedure Gtk_New_Hscale
      (Scale      : out Gtk_Hscale;
       Adjustment : Gtk.Adjustment.Gtk_Adjustment := null)
   is
   begin
      Scale := new Gtk_Hscale_Record;
      Gtk.Scale.Initialize_Hscale (Scale, Adjustment);
   end Gtk_New_Hscale;

   --------------------
   -- Gtk_New_Hscale --
   --------------------

   procedure Gtk_New_Hscale
      (Scale : out Gtk_Hscale;
       Min   : Gdouble;
       Max   : Gdouble;
       Step  : Gdouble)
   is
   begin
      Scale := new Gtk_Hscale_Record;
      Gtk.Scale.Initialize_Hscale (Scale, Min, Max, Step);
   end Gtk_New_Hscale;

   --------------------
   -- Gtk_New_Vscale --
   --------------------

   procedure Gtk_New_Vscale
      (Scale      : out Gtk_Vscale;
       Adjustment : Gtk.Adjustment.Gtk_Adjustment := null)
   is
   begin
      Scale := new Gtk_Vscale_Record;
      Gtk.Scale.Initialize_Vscale (Scale, Adjustment);
   end Gtk_New_Vscale;

   --------------------
   -- Gtk_New_Vscale --
   --------------------

   procedure Gtk_New_Vscale
      (Scale : out Gtk_Vscale;
       Min   : Gdouble;
       Max   : Gdouble;
       Step  : Gdouble)
   is
   begin
      Scale := new Gtk_Vscale_Record;
      Gtk.Scale.Initialize_Vscale (Scale, Min, Max, Step);
   end Gtk_New_Vscale;

   -----------------------
   -- Initialize_Hscale --
   -----------------------

   procedure Initialize_Hscale
      (Scale      : access Gtk_Hscale_Record'Class;
       Adjustment : Gtk.Adjustment.Gtk_Adjustment := null)
   is
      function Internal (Adjustment : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_hscale_new");
   begin
      Set_Object (Scale, Internal (Get_Object_Or_Null (GObject (Adjustment))));
   end Initialize_Hscale;

   -----------------------
   -- Initialize_Hscale --
   -----------------------

   procedure Initialize_Hscale
      (Scale : access Gtk_Hscale_Record'Class;
       Min   : Gdouble;
       Max   : Gdouble;
       Step  : Gdouble)
   is
      function Internal
         (Min  : Gdouble;
          Max  : Gdouble;
          Step : Gdouble) return System.Address;
      pragma Import (C, Internal, "gtk_hscale_new_with_range");
   begin
      Set_Object (Scale, Internal (Min, Max, Step));
   end Initialize_Hscale;

   -----------------------
   -- Initialize_Vscale --
   -----------------------

   procedure Initialize_Vscale
      (Scale      : access Gtk_Vscale_Record'Class;
       Adjustment : Gtk.Adjustment.Gtk_Adjustment := null)
   is
      function Internal (Adjustment : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_vscale_new");
   begin
      Set_Object (Scale, Internal (Get_Object_Or_Null (GObject (Adjustment))));
   end Initialize_Vscale;

   -----------------------
   -- Initialize_Vscale --
   -----------------------

   procedure Initialize_Vscale
      (Scale : access Gtk_Vscale_Record'Class;
       Min   : Gdouble;
       Max   : Gdouble;
       Step  : Gdouble)
   is
      function Internal
         (Min  : Gdouble;
          Max  : Gdouble;
          Step : Gdouble) return System.Address;
      pragma Import (C, Internal, "gtk_vscale_new_with_range");
   begin
      Set_Object (Scale, Internal (Min, Max, Step));
   end Initialize_Vscale;

   --------------
   -- Add_Mark --
   --------------

   procedure Add_Mark
      (Scale    : access Gtk_Scale_Record;
       Value    : Gdouble;
       Position : Gtk.Enums.Gtk_Position_Type;
       Markup   : UTF8_String)
   is
      procedure Internal
         (Scale    : System.Address;
          Value    : Gdouble;
          Position : Integer;
          Markup   : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_scale_add_mark");
      Tmp_Markup : Interfaces.C.Strings.chars_ptr := New_String (Markup);
   begin
      Internal (Get_Object (Scale), Value, Gtk.Enums.Gtk_Position_Type'Pos (Position), Tmp_Markup);
      Free (Tmp_Markup);
   end Add_Mark;

   -----------------
   -- Clear_Marks --
   -----------------

   procedure Clear_Marks (Scale : access Gtk_Scale_Record) is
      procedure Internal (Scale : System.Address);
      pragma Import (C, Internal, "gtk_scale_clear_marks");
   begin
      Internal (Get_Object (Scale));
   end Clear_Marks;

   ----------------
   -- Get_Digits --
   ----------------

   function Get_Digits (Scale : access Gtk_Scale_Record) return Gint is
      function Internal (Scale : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_scale_get_digits");
   begin
      return Internal (Get_Object (Scale));
   end Get_Digits;

   --------------------
   -- Get_Draw_Value --
   --------------------

   function Get_Draw_Value (Scale : access Gtk_Scale_Record) return Boolean is
      function Internal (Scale : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_scale_get_draw_value");
   begin
      return Boolean'Val (Internal (Get_Object (Scale)));
   end Get_Draw_Value;

   ----------------
   -- Get_Layout --
   ----------------

   function Get_Layout
      (Scale : access Gtk_Scale_Record) return Pango.Layout.Pango_Layout
   is
      function Internal (Scale : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scale_get_layout");
      Stub : Pango.Layout.Pango_Layout_Record;
   begin
      return Pango.Layout.Pango_Layout (Get_User_Data (Internal (Get_Object (Scale)), Stub));
   end Get_Layout;

   ------------------------
   -- Get_Layout_Offsets --
   ------------------------

   procedure Get_Layout_Offsets
      (Scale : access Gtk_Scale_Record;
       X     : out Gint;
       Y     : out Gint)
   is
      procedure Internal
         (Scale : System.Address;
          X     : out Gint;
          Y     : out Gint);
      pragma Import (C, Internal, "gtk_scale_get_layout_offsets");
   begin
      Internal (Get_Object (Scale), X, Y);
   end Get_Layout_Offsets;

   -------------------
   -- Get_Value_Pos --
   -------------------

   function Get_Value_Pos
      (Scale : access Gtk_Scale_Record) return Gtk.Enums.Gtk_Position_Type
   is
      function Internal (Scale : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_scale_get_value_pos");
   begin
      return Gtk.Enums.Gtk_Position_Type'Val (Internal (Get_Object (Scale)));
   end Get_Value_Pos;

   ----------------
   -- Set_Digits --
   ----------------

   procedure Set_Digits
      (Scale            : access Gtk_Scale_Record;
       Number_Of_Digits : Gint)
   is
      procedure Internal (Scale : System.Address; Number_Of_Digits : Gint);
      pragma Import (C, Internal, "gtk_scale_set_digits");
   begin
      Internal (Get_Object (Scale), Number_Of_Digits);
   end Set_Digits;

   --------------------
   -- Set_Draw_Value --
   --------------------

   procedure Set_Draw_Value
      (Scale      : access Gtk_Scale_Record;
       Draw_Value : Boolean)
   is
      procedure Internal (Scale : System.Address; Draw_Value : Integer);
      pragma Import (C, Internal, "gtk_scale_set_draw_value");
   begin
      Internal (Get_Object (Scale), Boolean'Pos (Draw_Value));
   end Set_Draw_Value;

   -------------------
   -- Set_Value_Pos --
   -------------------

   procedure Set_Value_Pos
      (Scale : access Gtk_Scale_Record;
       Pos   : Gtk.Enums.Gtk_Position_Type)
   is
      procedure Internal (Scale : System.Address; Pos : Integer);
      pragma Import (C, Internal, "gtk_scale_set_value_pos");
   begin
      Internal (Get_Object (Scale), Gtk.Enums.Gtk_Position_Type'Pos (Pos));
   end Set_Value_Pos;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : access Gtk_Scale_Record) return Gtk.Enums.Gtk_Orientation
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
      (Self        : access Gtk_Scale_Record;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal (Self : System.Address; Orientation : Integer);
      pragma Import (C, Internal, "gtk_orientable_set_orientation");
   begin
      Internal (Get_Object (Self), Gtk.Enums.Gtk_Orientation'Pos (Orientation));
   end Set_Orientation;

end Gtk.Scale;
