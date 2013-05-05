-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                 Copyright (C) 2001-2013, AdaCore                  --
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
with Gdk.Color;           use Gdk.Color;
with Gtk;                 use Gtk;
with Gtk.Extra.Plot;      use Gtk.Extra.Plot;
with Gtk.Extra.Plot_Data; use Gtk.Extra.Plot_Data;

with Glib.Type_Conversion_Hooks;

package body Gtk.Extra.Plot_3D is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Plot_3D_Record);
   pragma Warnings (Off, Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget   : out Gtk_Plot_3D;
      Drawable : Gdk.Drawable.Gdk_Drawable;
      Width, Height : Gdouble := 0.0) is
   begin
      Widget := new Gtk_Plot_3D_Record;
      Gtk.Extra.Plot_3D.Initialize (Widget, Drawable, Width, Height);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget   : access Gtk_Plot_3D_Record'Class;
      Drawable : Gdk.Drawable.Gdk_Drawable;
      Width    : Gdouble;
      Height   : Gdouble)
   is
      function Internal (Drawable : Gdk_Drawable; Width, Height : Gdouble)
         return System.Address;
      pragma Import (C, Internal, "gtk_plot3d_new_with_size");

      function Internal2 (Drawable : Gdk_Drawable) return System.Address;
      pragma Import (C, Internal2, "gtk_plot3d_new");

   begin
      if Width = 0.0 and then Height = 0.0 then
         Set_Object (Widget, Internal2 (Drawable));
      else
         Set_Object (Widget, Internal (Drawable, Width, Height));
      end if;
   end Initialize;

   ---------------
   -- Autoscale --
   ---------------

   procedure Autoscale (Plot : access Gtk_Plot_3D_Record)
   is
      procedure Internal (Plot : System.Address);
      pragma Import (C, Internal, "gtk_plot3d_autoscale");
   begin
      Internal (Get_Object (Plot));
   end Autoscale;

   ----------------
   -- Hide_Title --
   ----------------

   procedure Hide_Title
     (Plot : access Gtk_Plot_3D_Record; Side : Plot_Side)
   is
      procedure Internal (Plot : System.Address; Side : Plot_Side);
      pragma Import (C, Internal, "gtk_plot3d_hide_title");

   begin
      Internal (Get_Object (Plot), Side);
   end Hide_Title;

   ---------------------
   -- Set_Major_Ticks --
   ---------------------

   procedure Set_Major_Ticks
     (Plot       : access Gtk_Plot_3D_Record;
      Axis       : Plot_Orientation;
      Major_Step : Gdouble)
   is
      procedure Internal
        (Plot : System.Address; Axis : Plot_Orientation; Major_Step : Gdouble);
      pragma Import (C, Internal, "gtk_plot3d_set_major_ticks");

   begin
      Internal (Get_Object (Plot), Axis, Major_Step);
   end Set_Major_Ticks;

   ---------------------
   -- Set_Minor_Ticks --
   ---------------------

   procedure Set_Minor_Ticks
     (Plot   : access Gtk_Plot_3D_Record;
      Axis   : Plot_Orientation;
      Nminor : Gint)
   is
      procedure Internal
        (Plot : System.Address; Axis : Plot_Orientation; Nminor : Gint);
      pragma Import (C, Internal, "gtk_plot3d_set_minor_ticks");

   begin
      Internal (Get_Object (Plot), Axis, Nminor);
   end Set_Minor_Ticks;

   ---------------
   -- Set_Ticks --
   ---------------

   procedure Set_Ticks
     (Plot       : access Gtk_Plot_3D_Record;
      Axis       : Plot_Orientation;
      Major_Step : Gdouble;
      Nminor     : Gint)
   is
      procedure Internal
        (Plot       : System.Address;
         Axis       : Plot_Orientation;
         Major_Step : Gdouble;
         Nminor     : Gint);
      pragma Import (C, Internal, "gtk_plot3d_set_ticks");

   begin
      Internal
        (Get_Object (Plot), Axis, Major_Step, Nminor);
   end Set_Ticks;

   ----------------------
   -- Set_Ticks_Length --
   ----------------------

   procedure Set_Ticks_Length
     (Plot : access Gtk_Plot_3D_Record; Axis : Plot_Orientation; Length : Gint)
   is
      procedure Internal
        (Plot : System.Address; Axis : Plot_Orientation; Length : Gint);
      pragma Import (C, Internal, "gtk_plot3d_set_ticks_length");

   begin
      Internal (Get_Object (Plot), Axis, Length);
   end Set_Ticks_Length;

   ---------------------
   -- Set_Ticks_Width --
   ---------------------

   procedure Set_Ticks_Width
     (Plot  : access Gtk_Plot_3D_Record;
      Axis  : Plot_Orientation;
      Width : Gfloat)
   is
      procedure Internal
        (Plot : System.Address; Axis : Plot_Orientation; Width : Gfloat);
      pragma Import (C, Internal, "gtk_plot3d_set_ticks_width");

   begin
      Internal (Get_Object (Plot), Axis, Width);
   end Set_Ticks_Width;

   -----------------
   -- Show_Labels --
   -----------------

   procedure Show_Labels
     (Plot : access Gtk_Plot_3D_Record; Side : Plot_Side; Label_Mask : Gint)
   is
      procedure Internal
        (Plot : System.Address; Side : Plot_Side;  Label_Mask : Gint);
      pragma Import (C, Internal, "gtk_plot3d_show_labels");

   begin
      Internal (Get_Object (Plot), Side, Label_Mask);
   end Show_Labels;

   ----------------
   -- Show_Ticks --
   ----------------

   procedure Show_Ticks
     (Plot       : access Gtk_Plot_3D_Record;
      Side       : Plot_Side;
      Major_Mask : Plot_Ticks_Pos;
      Minor_Mask : Plot_Ticks_Pos)
   is
      procedure Internal
        (Plot       : System.Address;
         Side       : Plot_Side;
         Major_Mask : Plot_Ticks_Pos;
         Minor_Mask : Plot_Ticks_Pos);
      pragma Import (C, Internal, "gtk_plot3d_show_ticks");

   begin
      Internal
        (Get_Object (Plot), Side, Major_Mask, Minor_Mask);
   end Show_Ticks;

   ----------------
   -- Show_Title --
   ----------------

   procedure Show_Title
     (Plot : access Gtk_Plot_3D_Record; Side : Plot_Side)
   is
      procedure Internal (Plot : System.Address; Side : Plot_Side);
      pragma Import (C, Internal, "gtk_plot3d_show_title");

   begin
      Internal (Get_Object (Plot), Side);
   end Show_Title;

   ---------------------------
   -- Corner_Get_Attributes --
   ---------------------------

   procedure Corner_Get_Attributes
     (Plot  : access Gtk_Plot_3D_Record;
      Style : out Plot_Line_Style;
      Width : out Gfloat;
      Color : out Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Plot  : System.Address;
         Style : out Plot_Line_Style;
         Width : out Gfloat;
         Color : System.Address);
      pragma Import (C, Internal, "gtk_plot3d_corner_get_attributes");
      C : aliased Gdk_Color;
   begin
      Internal (Get_Object (Plot), Style, Width, C'Address);
      Color := C;
   end Corner_Get_Attributes;

   ---------------------------
   -- Corner_Set_Attributes --
   ---------------------------

   procedure Corner_Set_Attributes
     (Plot  : access Gtk_Plot_3D_Record;
      Style : Plot_Line_Style;
      Width : Gfloat;
      Color : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Plot  : System.Address;
         Style : Plot_Line_Style;
         Width : Gfloat;
         Color : System.Address);
      pragma Import (C, Internal, "gtk_plot3d_corner_set_attributes");

      C : aliased Gdk_Color := Color;

   begin
      Internal (Get_Object (Plot), Style, Width, C'Address);
   end Corner_Set_Attributes;

   ------------------------
   -- Corner_Set_Visible --
   ------------------------

   procedure Corner_Set_Visible
     (Plot : access Gtk_Plot_3D_Record; Visible : Boolean)
   is
      procedure Internal (Plot    : System.Address; Visible : Gint);
      pragma Import (C, Internal, "gtk_plot3d_corner_set_visible");
   begin
      Internal (Get_Object (Plot), Boolean'Pos (Visible));
   end Corner_Set_Visible;

   --------------------
   -- Corner_Visible --
   --------------------

   function Corner_Visible (Plot   : access Gtk_Plot_3D_Record) return Boolean
   is
      function Internal (Plot   : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_plot3d_corner_visible");
   begin
      return Boolean'Val (Internal (Get_Object (Plot)));
   end Corner_Visible;

   --------------------------
   -- Frame_Get_Attributes --
   --------------------------

   procedure Frame_Get_Attributes
     (Plot  : access Gtk_Plot_3D_Record;
      Style : out Plot_Line_Style;
      Width : out Gfloat;
      Color : out Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Plot  : System.Address;
         Style : out Plot_Line_Style;
         Width : out Gfloat;
         Color : System.Address);
      pragma Import (C, Internal, "gtk_plot3d_frame_get_attributes");
      C : aliased Gdk_Color;
   begin
      Internal (Get_Object (Plot), Style, Width, C'Address);
      Color := C;
   end Frame_Get_Attributes;

   --------------------------
   -- Frame_Set_Attributes --
   --------------------------

   procedure Frame_Set_Attributes
     (Plot  : access Gtk_Plot_3D_Record;
      Style : Plot_Line_Style;
      Width : Gfloat;
      Color : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Plot  : System.Address;
         Style : Plot_Line_Style;
         Width : Gfloat;
         Color : System.Address);
      pragma Import (C, Internal, "gtk_plot3d_frame_set_attributes");

      C : aliased Gdk_Color := Color;

   begin
      Internal (Get_Object (Plot), Style, Width, C'Address);
   end Frame_Set_Attributes;

   --------------
   -- Get_Axis --
   --------------

   function Get_Axis
     (Plot        : access Gtk_Plot_3D_Record;
      Orientation : Plot_Orientation)
      return Gtk_Plot_Axis
   is
      function Internal
        (Plot        : System.Address;
         Orientation : Plot_Orientation) return System.Address;
      pragma Import (C, Internal, "gtk_plot3d_get_axis");

      Stub : Gtk_Plot_Axis_Record;

   begin
      return Gtk_Plot_Axis (Get_User_Data
        (Internal (Get_Object (Plot), Orientation), Stub));
   end Get_Axis;

   ---------------
   -- Get_Pixel --
   ---------------

   procedure Get_Pixel
     (Plot       : access Gtk_Plot_3D_Record;
      X, Y, Z    : Gdouble;
      Px, Py, Pz : out Gdouble)
   is
      procedure Internal
        (Plot       : System.Address;
         X, Y, Z    : Gdouble;
         Px, Py, Pz : out Gdouble);
      pragma Import (C, Internal, "gtk_plot3d_get_pixel");
   begin
      Internal (Get_Object (Plot), X, Y, Z, Px, Py, Pz);
   end Get_Pixel;

   --------------
   -- Get_Side --
   --------------

   function Get_Side
     (Plot   : access Gtk_Plot_3D_Record;
      Side   : Plot_Side) return Gtk_Plot_Axis
   is
      function Internal
        (Plot : System.Address; Side : Plot_Side) return System.Address;
      pragma Import (C, Internal, "gtk_plot3d_get_side");

      Stub : Gtk_Plot_Axis_Record;

   begin
      return Gtk_Plot_Axis (Get_User_Data
         (Internal (Get_Object (Plot), Side), Stub));
   end Get_Side;

   -----------------------
   -- Get_Titles_Offset --
   -----------------------

   function Get_Titles_Offset (Plot : access Gtk_Plot_3D_Record) return Gint is
      function Internal (Plot   : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_plot3d_get_titles_offset");
   begin
      return Internal (Get_Object (Plot));
   end Get_Titles_Offset;

   -----------------
   -- Get_Xfactor --
   -----------------

   function Get_Xfactor (Plot   : access Gtk_Plot_3D_Record) return Gdouble
   is
      function Internal (Plot   : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_plot3d_get_xfactor");
   begin
      return Internal (Get_Object (Plot));
   end Get_Xfactor;

   -----------------
   -- Get_Yfactor --
   -----------------

   function Get_Yfactor (Plot   : access Gtk_Plot_3D_Record) return Gdouble
   is
      function Internal (Plot   : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_plot3d_get_yfactor");
   begin
      return Internal (Get_Object (Plot));
   end Get_Yfactor;

   -----------------
   -- Get_Zfactor --
   -----------------

   function Get_Zfactor (Plot   : access Gtk_Plot_3D_Record) return Gdouble
   is
      function Internal (Plot   : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_plot3d_get_zfactor");
   begin
      return Internal (Get_Object (Plot));
   end Get_Zfactor;

   -----------------------------
   -- Major_Grids_Set_Visible --
   -----------------------------

   procedure Major_Grids_Set_Visible
     (Plot : access Gtk_Plot_3D_Record; X, Y, Z : Boolean)
   is
      procedure Internal (Plot : System.Address; X, Y, Z : Gint);
      pragma Import (C, Internal, "gtk_plot3d_major_grids_set_visible");
   begin
      Internal
        (Get_Object (Plot), Boolean'Pos (X), Boolean'Pos (Y), Boolean'Pos (Z));
   end Major_Grids_Set_Visible;

   -------------------------
   -- Major_Grids_Visible --
   -------------------------

   procedure Major_Grids_Visible
     (Plot : access Gtk_Plot_3D_Record; X, Y, Z : out Boolean)
   is
      procedure Internal (Plot : System.Address; X, Y, Z : out Gboolean);
      pragma Import (C, Internal, "gtk_plot3d_major_grids_visible");
      Xb, Yb, Zb : Gboolean;
   begin
      Internal (Get_Object (Plot), Xb, Yb, Zb);
      X := Boolean'Val (Xb);
      Y := Boolean'Val (Yb);
      Z := Boolean'Val (Zb);
   end Major_Grids_Visible;

   --------------------------------
   -- Major_Zgrid_Get_Attributes --
   --------------------------------

   procedure Major_Zgrid_Get_Attributes
     (Plot  : access Gtk_Plot_3D_Record;
      Style : out Plot_Line_Style;
      Width : out Gfloat;
      Color : out Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Plot  : System.Address;
         Style : out Plot_Line_Style;
         Width : out Gfloat;
         Color : System.Address);
      pragma Import (C, Internal, "gtk_plot3d_major_zgrid_get_attributes");
      C : aliased Gdk_Color;
   begin
      Internal (Get_Object (Plot), Style, Width, C'Address);
      Color := C;
   end Major_Zgrid_Get_Attributes;

   --------------------------------
   -- Major_Zgrid_Set_Attributes --
   --------------------------------

   procedure Major_Zgrid_Set_Attributes
     (Plot  : access Gtk_Plot_3D_Record;
      Style : Plot_Line_Style;
      Width : Gfloat;
      Color : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Plot  : System.Address;
         Style : Plot_Line_Style;
         Width : Gfloat;
         Color : System.Address);
      pragma Import (C, Internal, "gtk_plot3d_major_zgrid_set_attributes");

      C : aliased Gdk_Color := Color;

   begin
      Internal (Get_Object (Plot), Style, Width, C'Address);
   end Major_Zgrid_Set_Attributes;

   -----------------------------
   -- Minor_Grids_Set_Visible --
   -----------------------------

   procedure Minor_Grids_Set_Visible
     (Plot : access Gtk_Plot_3D_Record; X, Y, Z : Boolean)
   is
      procedure Internal (Plot : System.Address; X, Y, Z : Gint);
      pragma Import (C, Internal, "gtk_plot3d_minor_grids_set_visible");
   begin
      Internal
        (Get_Object (Plot), Boolean'Pos (X), Boolean'Pos (Y), Boolean'Pos (Z));
   end Minor_Grids_Set_Visible;

   -------------------------
   -- Minor_Grids_Visible --
   -------------------------

   procedure Minor_Grids_Visible
     (Plot : access Gtk_Plot_3D_Record; X, Y, Z : out Boolean)
   is
      procedure Internal (Plot : System.Address; X, Y, Z : out Gboolean);
      pragma Import (C, Internal, "gtk_plot3d_minor_grids_visible");
      Xb, Yb, Zb : Gboolean;
   begin
      Internal (Get_Object (Plot), Xb, Yb, Zb);
      X := Boolean'Val (Xb);
      Y := Boolean'Val (Yb);
      Z := Boolean'Val (Zb);
   end Minor_Grids_Visible;

   --------------------------------
   -- Minor_Zgrid_Get_Attributes --
   --------------------------------

   procedure Minor_Zgrid_Get_Attributes
     (Plot  : access Gtk_Plot_3D_Record;
      Style : out Plot_Line_Style;
      Width : out Gfloat;
      Color : out Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Plot  : System.Address;
         Style : out Plot_Line_Style;
         Width : out Gfloat;
         Color : System.Address);
      pragma Import (C, Internal, "gtk_plot3d_minor_zgrid_get_attributes");
      C : aliased Gdk_Color;
   begin
      Internal (Get_Object (Plot), Style, Width, C'Address);
      Color := C;
   end Minor_Zgrid_Get_Attributes;

   --------------------------------
   -- Minor_Zgrid_Set_Attributes --
   --------------------------------

   procedure Minor_Zgrid_Set_Attributes
     (Plot  : access Gtk_Plot_3D_Record;
      Style : Plot_Line_Style;
      Width : Gfloat;
      Color : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Plot  : System.Address;
         Style : Plot_Line_Style;
         Width : Gfloat;
         Color : System.Address);
      pragma Import (C, Internal, "gtk_plot3d_minor_zgrid_set_attributes");

      C : aliased Gdk_Color := Color;

   begin
      Internal (Get_Object (Plot), Style, Width, C'Address);
   end Minor_Zgrid_Set_Attributes;

   ---------------------
   -- Plane_Set_Color --
   ---------------------

   procedure Plane_Set_Color
     (Plot  : access Gtk_Plot_3D_Record;
      Plane : Plot_Plane;
      Color : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Plot  : System.Address;
         Plane : Plot_Plane;
         Color : System.Address);
      pragma Import (C, Internal, "gtk_plot3d_plane_set_color");

      C : aliased Gdk_Color := Color;

   begin
      Internal (Get_Object (Plot), Plane, C'Address);
   end Plane_Set_Color;

   -----------------------
   -- Plane_Set_Visible --
   -----------------------

   procedure Plane_Set_Visible
     (Plot    : access Gtk_Plot_3D_Record;
      Plane   : Plot_Plane;
      Visible : Boolean)
   is
      procedure Internal
        (Plot : System.Address; Plane : Plot_Plane; Visible : Gint);
      pragma Import (C, Internal, "gtk_plot3d_plane_set_visible");

   begin
      Internal
        (Get_Object (Plot), Plane, Boolean'Pos (Visible));
   end Plane_Set_Visible;

   -------------------
   -- Plane_Visible --
   -------------------

   function Plane_Visible
     (Plot : access Gtk_Plot_3D_Record; Plane : Plot_Plane) return Boolean
   is
      function Internal
        (Plot : System.Address; Plane : Plot_Plane) return Gint;
      pragma Import (C, Internal, "gtk_plot3d_plane_visible");

   begin
      return Boolean'Val
        (Internal (Get_Object (Plot), Plane));
   end Plane_Visible;

   ------------------
   -- Reset_Angles --
   ------------------

   procedure Reset_Angles (Plot : access Gtk_Plot_3D_Record) is
      procedure Internal (Plot : System.Address);
      pragma Import (C, Internal, "gtk_plot3d_reset_angles");
   begin
      Internal (Get_Object (Plot));
   end Reset_Angles;

   ------------
   -- Rotate --
   ------------

   procedure Rotate
     (Plot    : access Gtk_Plot_3D_Record; Angle_X, Angle_Y, Angle_Z : Gdouble)
   is
      procedure Internal
        (Plot    : System.Address; Angle_X, Angle_Y, Angle_Z : Gdouble);
      pragma Import (C, Internal, "gtk_plot3d_rotate");
   begin
      Internal (Get_Object (Plot), Angle_X, Angle_Y, Angle_Z);
   end Rotate;

   -------------------
   -- Rotate_Vector --
   -------------------

   procedure Rotate_Vector
     (Plot   : access Gtk_Plot_3D_Record;
      Vector : Plot_Vector;
      A1, A2, A3 : Gdouble)
   is
      procedure Internal
        (Plot, Vector : System.Address; A1, A2, A3 : Gdouble);
      pragma Import (C, Internal, "gtk_plot3d_rotate_vector");
      V : aliased Plot_Vector := Vector;
   begin
      Internal (Get_Object (Plot), V'Address, A1, A2, A3);
   end Rotate_Vector;

   --------------
   -- Rotate_X --
   --------------

   procedure Rotate_X (Plot  : access Gtk_Plot_3D_Record; Angle : Gdouble) is
      procedure Internal (Plot  : System.Address; Angle : Gdouble);
      pragma Import (C, Internal, "gtk_plot3d_rotate_x");
   begin
      Internal (Get_Object (Plot), Angle);
   end Rotate_X;

   --------------
   -- Rotate_Y --
   --------------

   procedure Rotate_Y (Plot  : access Gtk_Plot_3D_Record; Angle : Gdouble) is
      procedure Internal (Plot  : System.Address; Angle : Gdouble);
      pragma Import (C, Internal, "gtk_plot3d_rotate_y");
   begin
      Internal (Get_Object (Plot), Angle);
   end Rotate_Y;

   --------------
   -- Rotate_Z --
   --------------

   procedure Rotate_Z (Plot  : access Gtk_Plot_3D_Record;  Angle : Gdouble) is
      procedure Internal (Plot  : System.Address; Angle : Gdouble);
      pragma Import (C, Internal, "gtk_plot3d_rotate_z");
   begin
      Internal (Get_Object (Plot), Angle);
   end Rotate_Z;

   -----------------------
   -- Set_Titles_Offset --
   -----------------------

   procedure Set_Titles_Offset
     (Plot : access Gtk_Plot_3D_Record; Offset : Gint)
   is
      procedure Internal (Plot : System.Address; Offset : Gint);
      pragma Import (C, Internal, "gtk_plot3d_set_titles_offset");
   begin
      Internal (Get_Object (Plot), Offset);
   end Set_Titles_Offset;

   -----------------
   -- Set_Xfactor --
   -----------------

   procedure Set_Xfactor (Plot : access Gtk_Plot_3D_Record; Xfactor : Gdouble)
   is
      procedure Internal (Plot    : System.Address; Xfactor : Gdouble);
      pragma Import (C, Internal, "gtk_plot3d_set_xfactor");
   begin
      Internal (Get_Object (Plot), Xfactor);
   end Set_Xfactor;

   ----------------
   -- Set_Xrange --
   ----------------

   procedure Set_Xrange (Plot : access Gtk_Plot_3D_Record; Min, Max : Gdouble)
   is
      procedure Internal (Plot : System.Address; Min, Max  : Gdouble);
      pragma Import (C, Internal, "gtk_plot3d_set_xrange");
   begin
      Internal (Get_Object (Plot), Min,  Max);
   end Set_Xrange;

   -----------------
   -- Set_Yfactor --
   -----------------

   procedure Set_Yfactor (Plot : access Gtk_Plot_3D_Record; Yfactor : Gdouble)
   is
      procedure Internal (Plot : System.Address; Yfactor : Gdouble);
      pragma Import (C, Internal, "gtk_plot3d_set_yfactor");
   begin
      Internal (Get_Object (Plot), Yfactor);
   end Set_Yfactor;

   ----------------
   -- Set_Yrange --
   ----------------

   procedure Set_Yrange (Plot : access Gtk_Plot_3D_Record; Min, Max : Gdouble)
   is
      procedure Internal (Plot : System.Address; Min, Max  : Gdouble);
      pragma Import (C, Internal, "gtk_plot3d_set_yrange");
   begin
      Internal (Get_Object (Plot), Min,  Max);
   end Set_Yrange;

   -----------------
   -- Set_Zfactor --
   -----------------

   procedure Set_Zfactor (Plot : access Gtk_Plot_3D_Record; Zfactor : Gdouble)
   is
      procedure Internal (Plot : System.Address; Zfactor : Gdouble);
      pragma Import (C, Internal, "gtk_plot3d_set_zfactor");
   begin
      Internal (Get_Object (Plot), Zfactor);
   end Set_Zfactor;

   ----------------
   -- Set_Zrange --
   ----------------

   procedure Set_Zrange (Plot : access Gtk_Plot_3D_Record; Min, Max : Gdouble)
   is
      procedure Internal (Plot : System.Address; Min, Max  : Gdouble);
      pragma Import (C, Internal, "gtk_plot3d_set_zrange");
   begin
      Internal (Get_Object (Plot), Min,  Max);
   end Set_Zrange;

   ---------------
   -- Set_Scale --
   ---------------

   procedure Set_Scale
     (Plot  : access Gtk_Plot_3D_Record;
      Axis  : Plot_Orientation;
      Scale : Plot_Scale)
   is
      procedure Internal
        (Plot  : System.Address;
         Axis  : Plot_Orientation;
         Scale : Plot_Scale);
      pragma Import (C, Internal, "gtk_plot3d_set_scale");
   begin
      Internal (Get_Object (Plot), Axis, Scale);
   end Set_Scale;

   ---------------
   -- Get_Scale --
   ---------------

   function Get_Scale
     (Plot  : access Gtk_Plot_3D_Record;
      Axis  : Gtk.Extra.Plot.Plot_Orientation)
      return Plot_Scale
   is
      function Internal
        (Plot : System.Address; Axis : Plot_Orientation)
         return Plot_Scale;
      pragma Import (C, Internal, "gtk_plot3d_get_scale");
   begin
      return Internal (Get_Object (Plot), Axis);
   end Get_Scale;

end Gtk.Extra.Plot_3D;
