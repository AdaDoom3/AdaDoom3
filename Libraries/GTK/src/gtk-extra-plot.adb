-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
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

with Gdk.Color;            use Gdk.Color;
with Gdk.Rectangle;        use Gdk.Rectangle;
with Gdk.Pixmap;           use Gdk.Pixmap;
with Gtk.Enums;            use Gtk.Enums;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Glib.Type_Conversion_Hooks;

package body Gtk.Extra.Plot is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Plot_Record);
   pragma Warnings (Off, Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Plot     : out Gtk_Plot;
                      Drawable : Gdk.Drawable.Gdk_Drawable
                        :=  Gdk.Drawable.Null_Drawable)
   is
   begin
      Plot := new Gtk_Plot_Record;
      Initialize (Plot, Drawable);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Plot     : access Gtk_Plot_Record'Class;
                         Drawable : Gdk.Drawable.Gdk_Drawable)
   is
      function Internal (Drawable : Gdk.Drawable.Gdk_Drawable)
                         return        System.Address;
      pragma Import (C, Internal, "gtk_plot_new");
   begin
      Set_Object (Plot, Internal (Drawable));
   end Initialize;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Plot     : out Gtk_Plot;
                      Width    : Gdouble;
                      Height   : Gdouble;
                      Drawable : Gdk.Drawable.Gdk_Drawable
                        :=  Gdk.Drawable.Null_Drawable)
   is
   begin
      Plot := new Gtk_Plot_Record;
      Initialize (Plot, Drawable, Width, Height);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Plot     : access Gtk_Plot_Record'Class;
                         Drawable : Gdk.Drawable.Gdk_Drawable;
                         Width    : Gdouble;
                         Height   : Gdouble)
   is
      function Internal (Drawable : Gdk.Drawable.Gdk_Drawable;
                         Width    : Gdouble;
                         Height   : Gdouble)
                        return        System.Address;
      pragma Import (C, Internal, "gtk_plot_new_with_size");
   begin
      Set_Object (Plot, Internal (Drawable, Width, Height));
   end Initialize;

   ------------------
   -- Set_Drawable --
   ------------------

   procedure Set_Drawable (Plot     : access Gtk_Plot_Record;
                           Drawable : Gdk.Drawable.Gdk_Drawable)
   is
      procedure Internal (Plot     : System.Address;
                          Drawable : Gdk.Drawable.Gdk_Drawable);
      pragma Import (C, Internal, "gtk_plot_set_drawable");
   begin
      Internal (Get_Object (Plot), Drawable);
   end Set_Drawable;

   ------------------
   -- Get_Drawable --
   ------------------

   function Get_Drawable (Plot   : access Gtk_Plot_Record)
                          return      Gdk.Drawable.Gdk_Drawable
   is
      function Internal (Plot : System.Address)
                        return Gdk.Drawable.Gdk_Drawable;
      pragma Import (C, Internal, "gtk_plot_get_drawable");
   begin
      return Internal (Get_Object (Plot));
   end Get_Drawable;

   ------------------
   -- Get_Position --
   ------------------

   procedure Get_Position (Plot : access Gtk_Plot_Record;
                           X    : out Gdouble;
                           Y    : out Gdouble)
   is
      procedure Internal (Plot : System.Address;
                          X    : out Gdouble;
                          Y    : out Gdouble);
      pragma Import (C, Internal, "gtk_plot_get_position");
   begin
      Internal (Get_Object (Plot), X, Y);
   end Get_Position;

   --------------
   -- Get_Size --
   --------------

   procedure Get_Size (Plot   : access Gtk_Plot_Record;
                       Width  : out Gdouble;
                       Height : out Gdouble)
   is
      procedure Internal (Plot   : System.Address;
                          Width  : out Gdouble;
                          Height : out Gdouble);
      pragma Import (C, Internal, "gtk_plot_get_size");
   begin
      Internal (Get_Object (Plot), Width, Height);
   end Get_Size;

   -----------------------------
   -- Get_Internal_Allocation --
   -----------------------------

   function Get_Internal_Allocation (Plot   : access Gtk_Plot_Record)
                                     return      Gtk.Widget.Gtk_Allocation
   is
      function Internal (Plot   : System.Address)
                         return      Gtk.Widget.Gtk_Allocation;
      pragma Import (C, Internal, "gtk_plot_get_internal_allocation");
   begin
      return Internal (Get_Object (Plot));
   end Get_Internal_Allocation;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background (Plot       : access Gtk_Plot_Record;
                             Background : Gdk.Color.Gdk_Color)
   is
      procedure Internal (Plot       : System.Address;
                          Background : System.Address);
      pragma Import (C, Internal, "gtk_plot_set_background");

      Back : aliased Gdk.Color.Gdk_Color := Background;
      Backa : System.Address := Back'Address;
   begin
      if Background = Gdk.Color.Null_Color then
         Backa := System.Null_Address;
      end if;

      Internal (Get_Object (Plot), Backa);
   end Set_Background;

   -----------
   -- Paint --
   -----------

   procedure Paint (Plot : access Gtk_Plot_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_plot_paint");
   begin
      Internal (Get_Object (Plot));
   end Paint;

   -------------
   -- Refresh --
   -------------

   procedure Refresh
      (Plot : access Gtk_Plot_Record;
       Area : Gdk.Rectangle.Gdk_Rectangle)
   is
      procedure Internal (Plot : System.Address;
                          Area : System.Address);
      pragma Import (C, Internal, "gtk_plot_refresh");

      Rec : aliased Gdk.Rectangle.Gdk_Rectangle := Area;
      R   : System.Address := Rec'Address;
      use type Gdk_Rectangle;
   begin
      if Rec = Full_Area then
         R := System.Null_Address;
      end if;
      Internal (Get_Object (Plot), R);
   end Refresh;

   ----------
   -- Move --
   ----------

   procedure Move (Plot : access Gtk_Plot_Record;
                   X    : Gdouble;
                   Y    : Gdouble)
   is
      procedure Internal (Plot : System.Address;
                          X    : Gdouble;
                          Y    : Gdouble);
      pragma Import (C, Internal, "gtk_plot_move");
   begin
      Internal (Get_Object (Plot), X, Y);
   end Move;

   ------------
   -- Resize --
   ------------

   procedure Resize (Plot   : access Gtk_Plot_Record;
                     Width  : Gdouble;
                     Height : Gdouble)
   is
      procedure Internal (Plot   : System.Address;
                          Width  : Gdouble;
                          Height : Gdouble);
      pragma Import (C, Internal, "gtk_plot_resize");
   begin
      Internal (Get_Object (Plot), Width, Height);
   end Resize;

   -----------------
   -- Move_Resize --
   -----------------

   procedure Move_Resize (Plot   : access Gtk_Plot_Record;
                          X      : Gdouble;
                          Y      : Gdouble;
                          Width  : Gdouble;
                          Height : Gdouble)
   is
      procedure Internal (Plot   : System.Address;
                          X      : Gdouble;
                          Y      : Gdouble;
                          Width  : Gdouble;
                          Height : Gdouble);
      pragma Import (C, Internal, "gtk_plot_move_resize");
   begin
      Internal (Get_Object (Plot), X, Y, Width, Height);
   end Move_Resize;

   ---------------
   -- Get_Pixel --
   ---------------

   procedure Get_Pixel (Plot : access Gtk_Plot_Record;
                        Xx   : Gdouble;
                        Yy   : Gdouble;
                        X    : out Gdouble;
                        Y    : out Gdouble)
   is
      procedure Internal (Plot : System.Address;
                          Xx   : Gdouble;
                          Yy   : Gdouble;
                          X    : out Gdouble;
                          Y    : out Gdouble);
      pragma Import (C, Internal, "gtk_plot_get_pixel");
   begin
      Internal (Get_Object (Plot), Xx, Yy, X, Y);
   end Get_Pixel;

   ---------------
   -- Clip_Data --
   ---------------

   procedure Clip_Data (Plot : access Gtk_Plot_Record; Clip : Boolean) is
      procedure Internal (Plot : System.Address; Clip : Gint);
      pragma Import (C, Internal, "gtk_plot_clip_data");
   begin
      Internal (Get_Object (Plot), Boolean'Pos (Clip));
   end Clip_Data;

   ---------------
   -- Get_Point --
   ---------------

   procedure Get_Point (Plot : access Gtk_Plot_Record;
                        X    : Gint;
                        Y    : Gint;
                        Xx   : out Gdouble;
                        Yy   : out Gdouble)
   is
      procedure Internal (Plot : System.Address;
                          X    : Gint;
                          Y    : Gint;
                          Xx   : out Gdouble;
                          Yy   : out Gdouble);
      pragma Import (C, Internal, "gtk_plot_get_point");
   begin
      Internal (Get_Object (Plot), X, Y, Xx, Yy);
   end Get_Point;

   ----------------
   -- Set_Xrange --
   ----------------

   procedure Set_Xrange (Plot : access Gtk_Plot_Record;
                         Xmin : Gdouble := 0.0;
                         Xmax : Gdouble := 1.0)
   is
      procedure Internal (Plot : System.Address;
                          Xmin : Gdouble;
                          Xmax : Gdouble);
      pragma Import (C, Internal, "gtk_plot_set_xrange");
   begin
      Internal (Get_Object (Plot), Xmin, Xmax);
   end Set_Xrange;

   ----------------
   -- Set_Yrange --
   ----------------

   procedure Set_Yrange (Plot : access Gtk_Plot_Record;
                         Ymin : Gdouble := 0.0;
                         Ymax : Gdouble := 1.0)
   is
      procedure Internal (Plot : System.Address;
                          Ymin : Gdouble;
                          Ymax : Gdouble);
      pragma Import (C, Internal, "gtk_plot_set_yrange");
   begin
      Internal (Get_Object (Plot), Ymin, Ymax);
   end Set_Yrange;

   ---------------
   -- Set_Range --
   ---------------

   procedure Set_Range (Plot : access Gtk_Plot_Record;
                        Xmin : Gdouble := 0.0;
                        Xmax : Gdouble := 1.0;
                        Ymin : Gdouble := 0.0;
                        Ymax : Gdouble := 1.0)
   is
      procedure Internal (Plot : System.Address;
                          Xmin : Gdouble;
                          Xmax : Gdouble;
                          Ymin : Gdouble;
                          Ymax : Gdouble);
      pragma Import (C, Internal, "gtk_plot_set_range");
   begin
      Internal (Get_Object (Plot), Xmin, Xmax, Ymin, Ymax);
   end Set_Range;

   ---------------
   -- Autoscale --
   ---------------

   procedure Autoscale (Plot : access Gtk_Plot_Record) is
      procedure Internal (Plot : System.Address);
      pragma Import (C, Internal, "gtk_plot_autoscale");
   begin
      Internal (Get_Object (Plot));
   end Autoscale;

   ----------------
   -- Get_Xrange --
   ----------------

   procedure Get_Xrange (Plot : access Gtk_Plot_Record;
                         Xmin : out Gdouble;
                         Xmax : out Gdouble)
   is
      procedure Internal (Plot : System.Address;
                          Xmin : out Gdouble;
                          Xmax : out Gdouble);
      pragma Import (C, Internal, "gtk_plot_get_xrange");
   begin
      Internal (Get_Object (Plot), Xmin, Xmax);
   end Get_Xrange;

   ----------------
   -- Get_Yrange --
   ----------------

   procedure Get_Yrange (Plot : access Gtk_Plot_Record;
                         Ymin : out Gdouble;
                         Ymax : out Gdouble)
   is
      procedure Internal (Plot : System.Address;
                          Ymin : out Gdouble;
                          Ymax : out Gdouble);
      pragma Import (C, Internal, "gtk_plot_get_yrange");
   begin
      Internal (Get_Object (Plot), Ymin, Ymax);
   end Get_Yrange;

   ----------------
   -- Set_Xscale --
   ----------------

   procedure Set_Xscale (Plot       : access Gtk_Plot_Record;
                         Scale_Type : Plot_Scale)
   is
      procedure Internal
        (Plot       : System.Address;
         Scale_Type : Plot_Scale);
      pragma Import (C, Internal, "gtk_plot_set_xscale");

   begin
      Internal (Get_Object (Plot), Scale_Type);
   end Set_Xscale;

   ----------------
   -- Set_Yscale --
   ----------------

   procedure Set_Yscale (Plot       : access Gtk_Plot_Record;
                         Scale_Type : Plot_Scale)
   is
      procedure Internal
        (Plot       : System.Address;
         Scale_Type : Plot_Scale);
      pragma Import (C, Internal, "gtk_plot_set_yscale");

   begin
      Internal (Get_Object (Plot), Scale_Type);
   end Set_Yscale;

   ----------------
   -- Get_Xscale --
   ----------------

   function Get_Xscale (Plot   : access Gtk_Plot_Record)
                        return      Plot_Scale
   is
      function Internal (Plot   : System.Address)
                         return      Gint;
      pragma Import (C, Internal, "gtk_plot_get_xscale");
   begin
      return Plot_Scale'Val (Internal (Get_Object (Plot)));
   end Get_Xscale;

   ----------------
   -- Get_Yscale --
   ----------------

   function Get_Yscale (Plot   : access Gtk_Plot_Record)
                        return      Plot_Scale
   is
      function Internal (Plot   : System.Address)
                         return      Gint;
      pragma Import (C, Internal, "gtk_plot_get_yscale");
   begin
      return Plot_Scale'Val (Internal (Get_Object (Plot)));
   end Get_Yscale;

   --------------
   -- Put_Text --
   --------------

   function Put_Text
      (Plot          : access Gtk_Plot_Record;
       X             : Gdouble;
       Y             : Gdouble;
       Font          : String := "";
       Font_Height   : Gint := 10;
       Angle         : Plot_Angle;
       Foreground    : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
       Background    : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
       Transparent   : Boolean := False;
       Justification : Gtk.Enums.Gtk_Justification := Gtk.Enums.Justify_Center;
       Text          : String := "") return Gtk_Plot_Text
   is
      function Internal
        (Plot          : System.Address;
         X             : Gdouble;
         Y             : Gdouble;
         Font          : System.Address;
         Height        : Gint;
         Angle         : Plot_Angle;
         Foreground    : System.Address;
         Background    : System.Address;
         Transparent   : Gint;
         Justification : Gtk.Enums.Gtk_Justification;
         Text          : String) return Gtk_Plot_Text;
      pragma Import (C, Internal, "gtk_plot_put_text");

      Back  : aliased Gdk.Color.Gdk_Color := Background;
      Fore  : aliased Gdk.Color.Gdk_Color := Foreground;
      Backa : System.Address := Back'Address;
      Forea : System.Address := Fore'Address;
      F     : aliased String := Font & ASCII.NUL;

   begin
      if Foreground = Gdk.Color.Null_Color then
         Forea := System.Null_Address;
      end if;

      if Background = Gdk.Color.Null_Color then
         Backa := System.Null_Address;
      end if;

      if Font = "" then
         return Internal
           (Get_Object (Plot), X, Y,
            System.Null_Address, Font_Height,
            Angle, Forea, Backa,
            Boolean'Pos (Transparent),
            Justification,
            Text & ASCII.NUL);
      else
         return Internal
           (Get_Object (Plot), X, Y,
            F'Address, Font_Height,
            Angle, Forea, Backa,
            Boolean'Pos (Transparent),
            Justification,
            Text & ASCII.NUL);
      end if;
   end Put_Text;

   ----------------------
   -- Axis_Set_Visible --
   ----------------------

   procedure Axis_Set_Visible
     (Axis    : access Gtk_Plot_Axis_Record;
      Visible : Boolean)
   is
      procedure Internal
        (Axis    : System.Address;
         Visible : Gint);
      pragma Import (C, Internal, "gtk_plot_axis_set_visible");

   begin
      Internal (Get_Object (Axis), Boolean'Pos (Visible));
   end Axis_Set_Visible;

   ------------------
   -- Axis_Visible --
   ------------------

   function Axis_Visible
     (Axis   : access Gtk_Plot_Axis_Record) return Boolean
   is
      function Internal
        (Axis : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_plot_axis_visible");

   begin
      return Boolean'Val (Internal (Get_Object (Axis)));
   end Axis_Visible;

   --------------------
   -- Axis_Set_Title --
   --------------------

   procedure Axis_Set_Title
     (Axis  : access Gtk_Plot_Axis_Record;
      Title : String)
   is
      procedure Internal
        (Axis  : System.Address;
         Title : String);
      pragma Import (C, Internal, "gtk_plot_axis_set_title");

   begin
      Internal (Get_Object (Axis), Title & ASCII.NUL);
   end Axis_Set_Title;

   ---------------------
   -- Axis_Show_Title --
   ---------------------

   procedure Axis_Show_Title (Axis : access Gtk_Plot_Axis_Record) is
      procedure Internal (Axis : System.Address);
      pragma Import (C, Internal, "gtk_plot_axis_show_title");

   begin
      Internal (Get_Object (Axis));
   end Axis_Show_Title;

   ---------------------
   -- Axis_Hide_Title --
   ---------------------

   procedure Axis_Hide_Title (Axis : access Gtk_Plot_Axis_Record) is
      procedure Internal (Axis : System.Address);
      pragma Import (C, Internal, "gtk_plot_axis_hide_title");
   begin
      Internal (Get_Object (Axis));
   end Axis_Hide_Title;

   ---------------------
   -- Axis_Move_Title --
   ---------------------

   procedure Axis_Move_Title
     (Axis  : access Gtk_Plot_Axis_Record;
      Angle : Plot_Angle;
      X     : Gdouble;
      Y     : Gdouble)
   is
      procedure Internal
        (Axis  : System.Address;
         Angle : Plot_Angle;
         X     : Gdouble;
         Y     : Gdouble);
      pragma Import (C, Internal, "gtk_plot_axis_move_title");

   begin
      Internal (Get_Object (Axis), Angle, X, Y);
   end Axis_Move_Title;

   ------------------------
   -- Axis_Justify_Title --
   ------------------------

   procedure Axis_Justify_Title
     (Axis          : access Gtk_Plot_Axis_Record;
      Justification : Gtk.Enums.Gtk_Justification)
   is
      procedure Internal
        (Axis          : System.Address;
         Justification : Gtk.Enums.Gtk_Justification);
      pragma Import (C, Internal, "gtk_plot_axis_justify_title");

   begin
      Internal (Get_Object (Axis), Justification);
   end Axis_Justify_Title;

   -------------------------
   -- Axis_Set_Attributes --
   -------------------------

   procedure Axis_Set_Attributes
     (Axis  : access Gtk_Plot_Axis_Record;
      Width : Gfloat;
      Color : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Axis  : System.Address;
         Width : Gfloat;
         Color : System.Address);
      pragma Import (C, Internal, "gtk_plot_axis_set_attributes");

      C  : aliased Gdk.Color.Gdk_Color := Color;
      Ca : System.Address := C'Address;

   begin
      if C = Gdk.Color.Null_Color then
         Ca := System.Null_Address;
      end if;

      Internal (Get_Object (Axis), Width, Ca);
   end Axis_Set_Attributes;

   -------------------------
   -- Axis_Get_Attributes --
   -------------------------

   procedure Axis_Get_Attributes
     (Axis  : access Gtk_Plot_Axis_Record;
      Width : out    Gfloat;
      Color : out    Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Axis  : System.Address;
         Width : out Gfloat;
         Color : out Gdk.Color.Gdk_Color);
      pragma Import (C, Internal, "gtk_plot_axis_get_attributes");

   begin
      Internal (Get_Object (Axis), Width, Color);
   end Axis_Get_Attributes;

   --------------------
   -- Axis_Set_Ticks --
   --------------------

   procedure Axis_Set_Ticks
     (Axis        : access Gtk_Plot_Axis_Record;
      Major_Step  : Gdouble;
      Num_Minor   : Gint)
   is
      procedure Internal
        (Axis        : System.Address;
         Major_Step  : Gdouble;
         Num_Minor   : Gint);
      pragma Import (C, Internal, "gtk_plot_axis_set_ticks");

   begin
      Internal (Get_Object (Axis), Major_Step, Num_Minor);
   end Axis_Set_Ticks;

   --------------------------
   -- Axis_Set_Major_Ticks --
   --------------------------

   procedure Axis_Set_Major_Ticks
     (Axis        : access Gtk_Plot_Axis_Record;
      Major_Step  : Gdouble)
   is
      procedure Internal
        (Axis        : System.Address;
         Major_Step  : Gdouble);
      pragma Import (C, Internal, "gtk_plot_axis_set_major_ticks");

   begin
      Internal (Get_Object (Axis), Major_Step);
   end Axis_Set_Major_Ticks;

   --------------------------
   -- Axis_Set_Minor_Ticks --
   --------------------------

   procedure Axis_Set_Minor_Ticks
     (Axis        : access Gtk_Plot_Axis_Record;
      Num_Minor   : Gint)
   is
      procedure Internal
        (Axis        : System.Address;
         Num_Minor   : Gint);
      pragma Import (C, Internal, "gtk_plot_axis_set_minor_ticks");

   begin
      Internal (Get_Object (Axis), Num_Minor);
   end Axis_Set_Minor_Ticks;

   ---------------------------
   -- Axis_Set_Ticks_Length --
   ---------------------------

   procedure Axis_Set_Ticks_Length
     (Axis   : access Gtk_Plot_Axis_Record;
      Length : Gint)
   is
      procedure Internal
        (Axis   : System.Address;
         Length : Gint);
      pragma Import (C, Internal, "gtk_plot_axis_set_ticks_length");

   begin
      Internal (Get_Object (Axis), Length);
   end Axis_Set_Ticks_Length;

   --------------------------
   -- Axis_Set_Ticks_Width --
   --------------------------

   procedure Axis_Set_Ticks_Width
     (Axis  : access Gtk_Plot_Axis_Record;
      Width : Gfloat)
   is
      procedure Internal
        (Axis  : System.Address;
         Width : Gfloat);
      pragma Import (C, Internal, "gtk_plot_axis_set_ticks_width");

   begin
      Internal (Get_Object (Axis), Width);
   end Axis_Set_Ticks_Width;

   ---------------------
   -- Axis_Show_Ticks --
   ---------------------

   procedure Axis_Show_Ticks
     (Axis       : access Gtk_Plot_Axis_Record;
      Major_Mask : Plot_Ticks_Pos;
      Minor_Mask : Plot_Ticks_Pos)
   is
      procedure Internal
         (Axis       : System.Address;
          Major_Mask : Plot_Ticks_Pos;
          Minor_Mask : Plot_Ticks_Pos);
      pragma Import (C, Internal, "gtk_plot_axis_show_ticks");

   begin
      Internal (Get_Object (Axis), Major_Mask, Minor_Mask);
   end Axis_Show_Ticks;

   ---------------------------
   -- Axis_Set_Ticks_Limits --
   ---------------------------

   procedure Axis_Set_Ticks_Limits
     (Axis        : access Gtk_Plot_Axis_Record;
      Ticks_Begin : Gdouble;
      Ticks_End   : Gdouble)
   is
      procedure Internal
        (Axis        : System.Address;
         Beg         : Gdouble;
         The_End     : Gdouble);
      pragma Import (C, Internal, "gtk_plot_axis_set_ticks_limits");

   begin
      Internal (Get_Object (Axis), Ticks_Begin, Ticks_End);
   end Axis_Set_Ticks_Limits;

   -----------------------------
   -- Axis_Unset_Ticks_Limits --
   -----------------------------

   procedure Axis_Unset_Ticks_Limits
     (Axis        : access Gtk_Plot_Axis_Record)
   is
      procedure Internal (Axis : System.Address);
      pragma Import (C, Internal, "gtk_plot_axis_unset_ticks_limits");

   begin
      Internal (Get_Object (Axis));
   end Axis_Unset_Ticks_Limits;

   ----------------------
   -- Axis_Show_Labels --
   ----------------------

   procedure Axis_Show_Labels
     (Axis        : access Gtk_Plot_Axis_Record;
      Labels_Mask : Plot_Label_Pos)
   is
      procedure Internal
        (Axis        : System.Address;
         Labels_Mask : Plot_Label_Pos);
      pragma Import (C, Internal, "gtk_plot_axis_show_labels");

   begin
      Internal (Get_Object (Axis), Labels_Mask);
   end Axis_Show_Labels;

   -------------------------------
   -- Axis_Title_Set_Attributes --
   -------------------------------

   procedure Axis_Title_Set_Attributes
     (Axis       : access Gtk_Plot_Axis_Record;
      Font       : String;
      Height     : Gint;
      Angle      : Plot_Angle;
      Foreground : Gdk.Color.Gdk_Color;
      Background : Gdk.Color.Gdk_Color;
      Transparent   : Boolean;
      Justification : Gtk.Enums.Gtk_Justification)
   is
      procedure Internal
        (Axis          : System.Address;
         Font          : String;
         Height        : Gint;
         Angle         : Plot_Angle;
         Foreground    : System.Address;
         Background    : System.Address;
         Transparent   : Gint;
         Justification : Gtk.Enums.Gtk_Justification);
      pragma Import (C, Internal, "gtk_plot_axis_title_set_attributes");

      Fore : aliased Gdk.Color.Gdk_Color := Foreground;
      Fa   : System.Address := Fore'Address;
      Back : aliased Gdk.Color.Gdk_Color := Background;
      Ba   : System.Address := Back'Address;
   begin
      if Fore = Gdk.Color.Null_Color then
         Fa := System.Null_Address;
      end if;

      if Back = Gdk.Color.Null_Color then
         Ba := System.Null_Address;
      end if;

      Internal
        (Get_Object (Axis), Font & ASCII.NUL,
         Height, Angle, Fa, Ba,
         Boolean'Pos (Transparent), Justification);
   end Axis_Title_Set_Attributes;

   --------------------------------
   -- Axis_Set_Labels_Attributes --
   --------------------------------

   procedure Axis_Set_Labels_Attributes
     (Axis          : access Gtk_Plot_Axis_Record;
      Font          : String;
      Height        : Gint;
      Angle         : Plot_Angle;
      Foreground    : Gdk.Color.Gdk_Color;
      Background    : Gdk.Color.Gdk_Color;
      Transparent   : Boolean;
      Justification : Gtk.Enums.Gtk_Justification)
   is
      procedure Internal
        (Axis          : System.Address;
         Font          : String;
         Height        : Gint;
         Angle         : Plot_Angle;
         Foreground    : System.Address;
         Background    : System.Address;
         Transparent   : Gint;
         Justification : Gtk.Enums.Gtk_Justification);
      pragma Import (C, Internal, "gtk_plot_axis_set_labels_attributes");

      Fore : aliased Gdk.Color.Gdk_Color := Foreground;
      Fa   : System.Address := Fore'Address;
      Back : aliased Gdk.Color.Gdk_Color := Background;
      Ba   : System.Address := Back'Address;
   begin
      if Fore = Gdk.Color.Null_Color then
         Fa := System.Null_Address;
      end if;

      if Back = Gdk.Color.Null_Color then
         Ba := System.Null_Address;
      end if;

      Internal
        (Get_Object (Axis), Font & ASCII.NUL,
         Height, Angle, Fa, Ba, Boolean'Pos (Transparent), Justification);
   end Axis_Set_Labels_Attributes;

   ---------------------------------
   -- Axis_Use_Custom_Tick_Labels --
   ---------------------------------

   procedure Axis_Use_Custom_Tick_Labels
     (Axis   : access Gtk_Plot_Axis_Record;
      Custom : Boolean := True)
   is
      procedure Internal
        (Axis   : System.Address;
         Custom : Gint);
      pragma Import (C, Internal, "gtk_plot_axis_use_custom_tick_labels");

   begin
      Internal (Get_Object (Axis), Boolean'Pos (Custom));
   end Axis_Use_Custom_Tick_Labels;

   ----------------
   -- X0_Visible --
   ----------------

   function X0_Visible (Plot   : access Gtk_Plot_Record)
                       return Boolean
   is
      function Internal (Plot   : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_plot_x0_visible");
   begin
      return Boolean'Val (Internal (Get_Object (Plot)));
   end X0_Visible;

   --------------------
   -- X0_Set_Visible --
   --------------------

   procedure X0_Set_Visible (Plot    : access Gtk_Plot_Record;
                             Visible : Boolean)
   is
      procedure Internal
         (Plot    : System.Address;
          Visible : Gint);
      pragma Import (C, Internal, "gtk_plot_x0_set_visible");

   begin
      Internal (Get_Object (Plot), Boolean'Pos (Visible));
   end X0_Set_Visible;

   ----------------
   -- Y0_Visible --
   ----------------

   function Y0_Visible (Plot   : access Gtk_Plot_Record)
                       return Boolean
   is
      function Internal (Plot   : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_plot_y0_visible");
   begin
      return Boolean'Val (Internal (Get_Object (Plot)));
   end Y0_Visible;

   --------------------
   -- Y0_Set_Visible --
   --------------------

   procedure Y0_Set_Visible (Plot    : access Gtk_Plot_Record;
                             Visible : Boolean)
   is
      procedure Internal
         (Plot    : System.Address;
          Visible : Gint);
      pragma Import (C, Internal, "gtk_plot_y0_set_visible");
   begin
      Internal (Get_Object (Plot), Boolean'Pos (Visible));
   end Y0_Set_Visible;

   -------------------
   -- Grids_Visible --
   -------------------

   procedure Grids_Visible (Plot   : access Gtk_Plot_Record;
                            Vmajor : out Boolean;
                            Vminor : out Boolean;
                            Hmajor : out Boolean;
                            Hminor : out Boolean)
   is
      procedure Internal (Plot   : System.Address;
                          Vmajor : out Gboolean;
                          Vminor : out Gboolean;
                          Hmajor : out Gboolean;
                          Hminor : out Gboolean);
      pragma Import (C, Internal, "gtk_plot_grids_visible");

      Vma, Vmi, Hma, Hmi : Gboolean;
   begin
      Internal (Get_Object (Plot), Vma, Vmi, Hma, Hmi);
      Vmajor := Boolean'Val (Vma);
      Vminor := Boolean'Val (Vmi);
      Hmajor := Boolean'Val (Hma);
      Hminor := Boolean'Val (Hmi);
   end Grids_Visible;

   -----------------------
   -- Grids_Set_Visible --
   -----------------------

   procedure Grids_Set_Visible (Plot   : access Gtk_Plot_Record;
                                Vmajor : Boolean;
                                Vminor : Boolean;
                                Hmajor : Boolean;
                                Hminor : Boolean)
   is
      procedure Internal
         (Plot   : System.Address;
          Vmajor : Gint;
          Vminor : Gint;
          Hmajor : Gint;
          Hminor : Gint);
      pragma Import (C, Internal, "gtk_plot_grids_set_visible");
   begin
      Internal (Get_Object (Plot),
                Boolean'Pos (Vmajor),
                Boolean'Pos (Vminor),
                Boolean'Pos (Hmajor),
                Boolean'Pos (Hminor));
   end Grids_Set_Visible;

   ---------------------------
   -- X0line_Set_Attributes --
   ---------------------------

   procedure X0line_Set_Attributes (Plot  : access Gtk_Plot_Record;
                                    Style : Plot_Line_Style;
                                    Width : Gfloat;
                                    Color : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Plot  : System.Address;
         Style : Plot_Line_Style;
         Width : Gfloat;
         Color : System.Address);
      pragma Import (C, Internal, "gtk_plot_x0line_set_attributes");

      C : aliased Gdk.Color.Gdk_Color := Color;
      Ca : System.Address := C'Address;

   begin
      if C = Gdk.Color.Null_Color then
         Ca := System.Null_Address;
      end if;

      Internal (Get_Object (Plot), Style, Width, Ca);
   end X0line_Set_Attributes;

   ---------------------------
   -- Y0line_Set_Attributes --
   ---------------------------

   procedure Y0line_Set_Attributes (Plot  : access Gtk_Plot_Record;
                                    Style : Plot_Line_Style;
                                    Width : Gfloat;
                                    Color : Gdk.Color.Gdk_Color)
   is
      procedure Internal
         (Plot  : System.Address;
          Style : Plot_Line_Style;
          Width : Gfloat;
          Color : System.Address);
      pragma Import (C, Internal, "gtk_plot_y0line_set_attributes");

      C : aliased Gdk.Color.Gdk_Color := Color;
      Ca : System.Address := C'Address;

   begin
      if C = Gdk.Color.Null_Color then
         Ca := System.Null_Address;
      end if;

      Internal (Get_Object (Plot), Style, Width, Ca);
   end Y0line_Set_Attributes;

   --------------------------------
   -- Major_Hgrid_Set_Attributes --
   --------------------------------

   procedure Major_Hgrid_Set_Attributes (Plot  : access Gtk_Plot_Record;
                                         Style : Plot_Line_Style;
                                         Width : Gfloat;
                                         Color : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Plot  : System.Address;
         Style : Plot_Line_Style;
         Width : Gfloat;
         Color : System.Address);
      pragma Import (C, Internal, "gtk_plot_major_hgrid_set_attributes");

      C : aliased Gdk.Color.Gdk_Color := Color;
      Ca : System.Address := C'Address;

   begin
      if C = Gdk.Color.Null_Color then
         Ca := System.Null_Address;
      end if;

      Internal (Get_Object (Plot), Style, Width, Ca);
   end Major_Hgrid_Set_Attributes;

   --------------------------------
   -- Major_Vgrid_Set_Attributes --
   --------------------------------

   procedure Major_Vgrid_Set_Attributes (Plot  : access Gtk_Plot_Record;
                                         Style : Plot_Line_Style;
                                         Width : Gfloat;
                                         Color : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Plot  : System.Address;
         Style : Plot_Line_Style;
         Width : Gfloat;
         Color : System.Address);
      pragma Import (C, Internal, "gtk_plot_major_vgrid_set_attributes");

      C : aliased Gdk.Color.Gdk_Color := Color;
      Ca : System.Address := C'Address;

   begin
      if C = Gdk.Color.Null_Color then
         Ca := System.Null_Address;
      end if;

      Internal (Get_Object (Plot), Style, Width, Ca);
   end Major_Vgrid_Set_Attributes;

   --------------------------------
   -- Minor_Hgrid_Set_Attributes --
   --------------------------------

   procedure Minor_Hgrid_Set_Attributes (Plot  : access Gtk_Plot_Record;
                                         Style : Plot_Line_Style;
                                         Width : Gfloat;
                                         Color : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Plot  : System.Address;
         Style : Plot_Line_Style;
         Width : Gfloat;
         Color : System.Address);
      pragma Import (C, Internal, "gtk_plot_minor_hgrid_set_attributes");

      C : aliased Gdk.Color.Gdk_Color := Color;
      Ca : System.Address := C'Address;

   begin
      if C = Gdk.Color.Null_Color then
         Ca := System.Null_Address;
      end if;

      Internal (Get_Object (Plot), Style, Width, Ca);
   end Minor_Hgrid_Set_Attributes;

   --------------------------------
   -- Minor_Vgrid_Set_Attributes --
   --------------------------------

   procedure Minor_Vgrid_Set_Attributes (Plot  : access Gtk_Plot_Record;
                                         Style : Plot_Line_Style;
                                         Width : Gfloat;
                                         Color : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Plot  : System.Address;
         Style : Plot_Line_Style;
         Width : Gfloat;
         Color : System.Address);
      pragma Import (C, Internal, "gtk_plot_minor_vgrid_set_attributes");

      C : aliased Gdk.Color.Gdk_Color := Color;
      Ca : System.Address := C'Address;

   begin
      if C = Gdk.Color.Null_Color then
         Ca := System.Null_Address;
      end if;

      Internal (Get_Object (Plot), Style, Width, Ca);
   end Minor_Vgrid_Set_Attributes;

   ------------------
   -- Show_Legends --
   ------------------

   procedure Show_Legends (Plot : access Gtk_Plot_Record)
   is
      procedure Internal (Plot : System.Address);
      pragma Import (C, Internal, "gtk_plot_show_legends");
   begin
      Internal (Get_Object (Plot));
   end Show_Legends;

   ------------------
   -- Hide_Legends --
   ------------------

   procedure Hide_Legends (Plot : access Gtk_Plot_Record)
   is
      procedure Internal (Plot : System.Address);
      pragma Import (C, Internal, "gtk_plot_hide_legends");
   begin
      Internal (Get_Object (Plot));
   end Hide_Legends;

   ------------------------
   -- Set_Legends_Border --
   ------------------------

   procedure Set_Legends_Border (Plot         : access Gtk_Plot_Record;
                                 Border       : Plot_Border_Style;
                                 Shadow_Width : Gint)
   is
      procedure Internal
        (Plot         : System.Address;
         Border       : Plot_Border_Style;
         Shadow_Width : Gint);
      pragma Import (C, Internal, "gtk_plot_set_legends_border");

   begin
      Internal (Get_Object (Plot), Border, Shadow_Width);
   end Set_Legends_Border;

   ------------------
   -- Legends_Move --
   ------------------

   procedure Legends_Move (Plot : access Gtk_Plot_Record;
                           X    : Gdouble;
                           Y    : Gdouble)
   is
      procedure Internal (Plot : System.Address;
                          X    : Gdouble;
                          Y    : Gdouble);
      pragma Import (C, Internal, "gtk_plot_legends_move");
   begin
      Internal (Get_Object (Plot), X, Y);
   end Legends_Move;

   --------------------------
   -- Legends_Get_Position --
   --------------------------

   procedure Legends_Get_Position (Plot : access Gtk_Plot_Record;
                                   X    : out Gdouble;
                                   Y    : out Gdouble)
   is
      procedure Internal (Plot : System.Address;
                          X    : out Gdouble;
                          Y    : out Gdouble);
      pragma Import (C, Internal, "gtk_plot_legends_get_position");
   begin
      Internal (Get_Object (Plot), X, Y);
   end Legends_Get_Position;

   ----------------------------
   -- Legends_Get_Allocation --
   ----------------------------

   function Legends_Get_Allocation (Plot   : access Gtk_Plot_Record)
                                    return      Gtk.Widget.Gtk_Allocation
   is
      function Internal (Plot   : System.Address)
                         return      Gtk.Widget.Gtk_Allocation;
      pragma Import (C, Internal, "gtk_plot_legends_get_allocation");
   begin
      return Internal (Get_Object (Plot));
   end Legends_Get_Allocation;

   ----------------------------
   -- Legends_Set_Attributes --
   ----------------------------

   procedure Legends_Set_Attributes (Plot       : access Gtk_Plot_Record;
                                     Ps_Font    : String;
                                     Height     : Gint;
                                     Foreground : Gdk.Color.Gdk_Color;
                                     Background : Gdk.Color.Gdk_Color)
   is
      procedure Internal (Plot       : System.Address;
                          Font       : System.Address;
                          Height     : Gint;
                          Foreground : System.Address;
                          Background : System.Address);
      pragma Import (C, Internal, "gtk_plot_legends_set_attributes");

      Fore  : aliased Gdk.Color.Gdk_Color := Foreground;
      Forea : System.Address := Fore'Address;
      Back  : aliased Gdk.Color.Gdk_Color := Background;
      Backa : System.Address := Back'Address;
      Font  : String := Ps_Font & ASCII.NUL;
      F     : System.Address := Font'Address;

   begin
      if Foreground = Gdk.Color.Null_Color then
         Forea := System.Null_Address;
      end if;

      if Background = Gdk.Color.Null_Color then
         Backa := System.Null_Address;
      end if;

      if Ps_Font = "" then
         F := System.Null_Address;
      end if;

      Internal (Get_Object (Plot), F, Height, Forea, Backa);
   end Legends_Set_Attributes;

   --------------
   -- Add_Data --
   --------------

   procedure Add_Data
     (Plot : access Gtk_Plot_Record;
      Data : access Gtk_Plot_Data_Record'Class)
   is
      procedure Internal (Plot : System.Address;
                          Data : System.Address);
      pragma Import (C, Internal, "gtk_plot_add_data");
   begin
      Internal (Get_Object (Plot), Get_Object (Data));
   end Add_Data;

   ------------------
   -- Add_Function --
   ------------------

   function Add_Function (Plot   : access Gtk_Plot_Record;
                          Func   : Plot_Function)
                         return      Gtk_Plot_Data
   is
      function Internal (Plot   : System.Address;
                         Func   : Plot_Function)
                        return System.Address;
      pragma Import (C, Internal, "gtk_plot_add_function");
      Stub : Gtk_Plot_Data_Record;
   begin
      return Gtk_Plot_Data
        (Get_User_Data (Internal (Get_Object (Plot), Func), Stub));
   end Add_Function;

   -----------------
   -- Remove_Data --
   -----------------

   function Remove_Data
     (Plot : access Gtk_Plot_Record;
      Data : access Gtk_Plot_Data_Record'Class) return Boolean
   is
      function Internal
        (Plot : System.Address; Data : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_plot_remove_data");

   begin
      return Boolean'Val (Internal (Get_Object (Plot), Get_Object (Data)));
   end Remove_Data;

   --------------
   -- Get_Axis --
   --------------

   function Get_Axis
     (Plot   : access Gtk_Plot_Record;
      Axis   : Plot_Axis_Pos) return Gtk_Plot_Axis
   is
      function Internal
        (Plot : System.Address; Axis : Plot_Axis_Pos) return System.Address;
      pragma Import (C, Internal, "gtk_plot_get_axis");

      Stub : Gtk_Plot_Axis_Record;

   begin
      return Gtk_Plot_Axis (Get_User_Data
         (Internal (Get_Object (Plot), Axis), Stub));
   end Get_Axis;

   ---------------------------
   -- Generic_Plot_Function --
   ---------------------------

   function Generic_Plot_Function
     (Plot  : System.Address;
      Set   : Gtk_Plot_Data;
      X     : Gdouble;
      Error : access Gboolean) return Gdouble
   is
      Stub : Gtk_Plot_Record;
      B    : aliased Boolean;
      Y    : Gdouble;

   begin
      Y := Func (Gtk_Plot (Get_User_Data (Plot, Stub)), Set, X, B'Access);
      Error.all := Boolean'Pos (B);

      return Y;
   end Generic_Plot_Function;

   -----------------------------
   -- Generic_Plot3D_Function --
   -----------------------------

   function Generic_Plot3D_Function
     (Plot  : System.Address;
      Set   : Gtk_Plot_Data;
      X     : Gdouble;
      Y     : Gdouble;
      Error : access Gboolean) return Gdouble
   is
      Stub : Gtk_Plot_Record;
      B    : aliased Boolean;
      Z    : Gdouble;

   begin
      Z := Func (Gtk_Plot (Get_User_Data (Plot, Stub)), Set, X, Y, B'Access);
      Error.all := Boolean'Pos (B);
      return Z;
   end Generic_Plot3D_Function;

   -----------------
   -- Remove_Text --
   -----------------

   procedure Remove_Text (Plot : access Gtk_Plot_Record;
                          Text : Gtk_Plot_Text)
   is
      procedure Internal (Plot : System.Address;
                          Text : Gtk_Plot_Text);
      pragma Import (C, Internal, "gtk_plot_remove_text");
   begin
      Internal (Get_Object (Plot), Text);
   end Remove_Text;

   -----------------------
   -- Set_Magnification --
   -----------------------

   procedure Set_Magnification (Plot          : access Gtk_Plot_Record;
                                Magnification : Gdouble)
   is
      procedure Internal (Plot : System.Address;
                          Magnification : Gdouble);
      pragma Import (C, Internal, "gtk_plot_set_magnification");
   begin
      Internal (Get_Object (Plot), Magnification);
   end Set_Magnification;

   -------------------------
   -- Text_Set_Attributes --
   -------------------------

   procedure Text_Set_Attributes
     (Text          : Gtk_Plot_Text;
      Font          : String;
      Height        : Gint;
      Angle         : Plot_Angle;
      Fg            : Gdk.Color.Gdk_Color;
      Bg            : Gdk.Color.Gdk_Color;
      Transparent   : Boolean := False;
      Justification : Gtk.Enums.Gtk_Justification :=
        Gtk.Enums.Justify_Center;
      Str           : String := "")
   is
      procedure Internal
        (Text          : Gtk_Plot_Text;
         Font          : String;
         Height        : Gint;
         Angle         : Plot_Angle;
         Fg            : System.Address;
         Bg            : System.Address;
         Transparent   : Gint;
         Justification : Gtk.Enums.Gtk_Justification;
         Str           : String);
      pragma Import (C, Internal, "gtk_plot_text_set_attributes");

      Back  : aliased Gdk.Color.Gdk_Color := Bg;
      Fore  : aliased Gdk.Color.Gdk_Color := Fg;
      Backa : System.Address := Back'Address;
      Forea : System.Address := Fore'Address;

   begin
      if Fg = Gdk.Color.Null_Color then
         Forea := System.Null_Address;
      end if;

      if Bg = Gdk.Color.Null_Color then
         Backa := System.Null_Address;
      end if;

      Internal (Text, Font & ASCII.NUL, Height, Angle,
                Forea, Backa, Boolean'Pos (Transparent),
                Justification, Str & ASCII.NUL);
   end Text_Set_Attributes;

   -------------------
   -- Text_Get_Size --
   -------------------

   procedure Text_Get_Size (Text          : Gtk_Plot_Text;
                            Angle         : Plot_Angle;
                            Font_Name     : String;
                            Font_Size     : Gint;
                            Width         : out Gint;
                            Height        : out Gint;
                            Ascent        : out Gint;
                            Descent       : out Gint)
   is
      procedure Internal (Text          : Gtk_Plot_Text;
                          Angle         : Plot_Angle;
                          Font_Name     : String;
                          Font_Size     : Gint;
                          Width         : out Gint;
                          Height        : out Gint;
                          Ascent        : out Gint;
                          Descent       : out Gint);
      pragma Import (C, Internal, "gtk_plot_text_get_size");
   begin
      Internal (Text, Angle, Font_Name & ASCII.NUL, Font_Size, Width, Height,
                Ascent, Descent);
   end Text_Get_Size;

   -------------------
   -- Text_Get_Area --
   -------------------

   procedure Text_Get_Area (Text          : Gtk_Plot_Text;
                            Angle         : Plot_Angle;
                            Just          : Gtk_Justification;
                            Font_Name     : String;
                            Font_Size     : Gint;
                            X             : out Gint;
                            Y             : out Gint;
                            Width         : out Gint;
                            Height        : out Gint)
   is
      procedure Internal (Text          : Gtk_Plot_Text;
                          Angle         : Plot_Angle;
                          Just          : Gtk_Justification;
                          Font_Name     : String;
                          Font_Size     : Gint;
                          X             : out Gint;
                          Y             : out Gint;
                          Width         : out Gint;
                          Height        : out Gint);
      pragma Import (C, Internal, "gtk_plot_text_get_area");
   begin
      Internal (Text, Angle, Just, Font_Name & ASCII.NUL, Font_Size, X, Y,
                Width, Height);
   end Text_Get_Area;

   ---------------
   -- Draw_Line --
   ---------------

   procedure Draw_Line (Plot : access Gtk_Plot_Record;
                        Line : Gtk_Plot_Line;
                        X1, Y1, X2, Y2 : Gdouble)
   is
      procedure Internal (Plot : System.Address;
                          Line : Gtk_Plot_Line;
                          X1, Y1, X2, Y2 : Gdouble);
      pragma Import (C, Internal, "gtk_plot_draw_line");
   begin
      Internal (Get_Object (Plot), Line, X1, Y1, X2, Y2);
   end Draw_Line;

   ---------------
   -- Draw_Text --
   ---------------

   procedure Draw_Text (Plot : access Gtk_Plot_Record;
                        Text : Gtk_Plot_Text)
   is
      procedure Internal (Plot : System.Address; Text : Gtk_Plot_Text);
      pragma Import (C, Internal, "gtk_plot_draw_text");
   begin
      Internal (Get_Object (Plot), Text);
   end Draw_Text;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Axis     : out Gtk_Plot_Axis;
                      Orientation : Plot_Orientation) is
   begin
      Axis := new Gtk_Plot_Axis_Record;
      Initialize (Axis, Orientation);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Axis     : access Gtk_Plot_Axis_Record'Class;
                         Orientation : Plot_Orientation) is
      function Internal (Orientation : Plot_Orientation) return System.Address;
      pragma Import (C, Internal, "gtk_plot_axis_new");
   begin
      Set_Object (Axis, Internal (Orientation));
   end Initialize;

   ----------------------------
   -- Axis_Set_Labels_Suffix --
   ----------------------------

   procedure Axis_Set_Labels_Suffix
     (Axis : access Gtk_Plot_Axis_Record;
      Text : String)
   is
      procedure Internal
        (Axis : System.Address;
         Text : String);
      pragma Import (C, Internal, "gtk_plot_axis_set_labels_suffix");

   begin
      Internal (Get_Object (Axis), Text & ASCII.NUL);
   end Axis_Set_Labels_Suffix;

   ----------------------------
   -- Axis_Set_Labels_Prefix --
   ----------------------------

   procedure Axis_Set_Labels_Prefix
     (Axis : access Gtk_Plot_Axis_Record;
      Text : String)
   is
      procedure Internal
        (Axis : System.Address;
         Text : String);
      pragma Import (C, Internal, "gtk_plot_axis_set_labels_prefix");

   begin
      Internal (Get_Object (Axis), Text & ASCII.NUL);
   end Axis_Set_Labels_Prefix;

   ----------------------------
   -- Axis_Get_Labels_Suffix --
   ----------------------------

   function Axis_Get_Labels_Suffix
     (Axis : access Gtk_Plot_Axis_Record) return String
   is
      function Internal (Axis : System.Address)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_plot_axis_get_labels_suffix");

   begin
      return Value (Internal (Get_Object (Axis)));
   end Axis_Get_Labels_Suffix;

   ----------------------------
   -- Axis_Get_Labels_Prefix --
   ----------------------------

   function Axis_Get_Labels_Prefix
     (Axis : access Gtk_Plot_Axis_Record) return String
   is
      function Internal (Axis : System.Address)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_plot_axis_get_labels_prefix");

   begin
      return Value (Internal (Get_Object (Axis)));
   end Axis_Get_Labels_Prefix;

   ---------------------------
   -- Set_Background_Pixmap --
   ---------------------------

   procedure Set_Background_Pixmap
     (Plot : access Gtk_Plot_Record; Pixmap : Gdk.Pixmap.Gdk_Pixmap)
   is
      procedure Internal (Plot : System.Address; Pixmap : Gdk.Gdk_Pixmap);
      pragma Import (C, Internal, "gtk_plot_set_background_pixmap");
   begin
      Internal (Get_Object (Plot), Pixmap);
   end Set_Background_Pixmap;

   ---------------------
   -- Set_Transparent --
   ---------------------

   procedure Set_Transparent
     (Plot : access Gtk_Plot_Record; Transparent : Boolean)
   is
      procedure Internal (Plot : System.Address; Transparent : Integer);
      pragma Import (C, Internal, "gtk_plot_set_transparent");
   begin
      Internal (Get_Object (Plot), Boolean'Pos (Transparent));
   end Set_Transparent;

   --------------------
   -- Is_Transparent --
   --------------------

   function Is_Transparent (Plot : access Gtk_Plot_Record) return Boolean is
      function Internal (Plot : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_plot_is_transparent");
   begin
      return Boolean'Val (Internal (Get_Object (Plot)));
   end Is_Transparent;

   ---------------
   -- Reflect_X --
   ---------------

   procedure Reflect_X (Plot : access Gtk_Plot_Record; Reflect : Boolean) is
      procedure Internal (Plot : System.Address; Reflect : Integer);
      pragma Import (C, Internal, "gtk_plot_reflect_x");
   begin
      Internal (Get_Object (Plot), Boolean'Pos (Reflect));
   end Reflect_X;

   --------------------
   -- Is_X_Reflected --
   --------------------

   function Is_X_Reflected (Plot : access Gtk_Plot_Record) return Boolean is
      function Internal (Plot : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_plot_is_x_reflected");
   begin
      return Boolean'Val (Internal (Get_Object (Plot)));
   end Is_X_Reflected;

   ---------------
   -- Reflect_Y --
   ---------------

   procedure Reflect_Y (Plot : access Gtk_Plot_Record; Reflect : Boolean) is
      procedure Internal (Plot : System.Address; Reflect : Integer);
      pragma Import (C, Internal, "gtk_plot_reflect_y");
   begin
      Internal (Get_Object (Plot), Boolean'Pos (Reflect));
   end Reflect_Y;

   --------------------
   -- Is_Y_Reflected --
   --------------------

   function Is_Y_Reflected (Plot : access Gtk_Plot_Record) return Boolean is
      function Internal (Plot : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_plot_is_y_reflected");
   begin
      return Boolean'Val (Internal (Get_Object (Plot)));
   end Is_Y_Reflected;

   --------------------
   -- Axis_Set_Break --
   --------------------

   procedure Axis_Set_Break
     (Axis         : access Gtk_Plot_Axis_Record;
      Min, Max     : Gdouble;
      Step_After   : Gdouble;
      Nminor_After : Gint;
      Scale_After  : Plot_Scale;
      Pos          : Gdouble)
   is
      procedure Internal (Axis : System.Address;
                          Min, Max, Step : Gdouble;
                          Nminor : Gint;
                          Scale : Plot_Scale;
                          Pos   : Gdouble);
      pragma Import (C, Internal, "gtk_plot_axis_set_break");
   begin
      Internal (Get_Object (Axis), Min, Max, Step_After,
                Nminor_After, Scale_After, Pos);
   end Axis_Set_Break;

   -----------------------
   -- Axis_Remove_Break --
   -----------------------

   procedure Axis_Remove_Break
     (Axis : access Gtk_Plot_Axis_Record)
   is
      procedure Internal (Axis : System.Address);
      pragma Import (C, Internal, "gtk_plot_axis_remove_break");
   begin
      Internal (Get_Object (Axis));
   end Axis_Remove_Break;

   ----------------------------
   -- Axis_Set_Labels_Offset --
   ----------------------------

   procedure Axis_Set_Labels_Offset
     (Axis   : access Gtk_Plot_Axis_Record;
      Offset : Gint)
   is
      procedure Internal (Axis : System.Address; O : Gint);
      pragma Import (C, Internal, "gtk_plot_axis_set_labels_offset");
   begin
      Internal (Get_Object (Axis), Offset);
   end Axis_Set_Labels_Offset;

   ----------------------------
   -- Axis_Get_Labels_Offset --
   ----------------------------

   function Axis_Get_Labels_Offset
     (Axis : access Gtk_Plot_Axis_Record) return Gint
   is
      function Internal (Axis : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_plot_axis_get_labels_offset");
   begin
      return Internal (Get_Object (Axis));
   end Axis_Get_Labels_Offset;

   ---------------------------
   -- Axis_Set_Labels_Style --
   ---------------------------

   procedure Axis_Set_Labels_Style
     (Axis      : access Gtk_Plot_Axis_Record;
      Style     : Plot_Label_Style;
      Precision : Gint)
   is
      procedure Internal
        (Axis         : System.Address;
         Style     : Plot_Label_Style;
         Precision : Gint);
      pragma Import (C, Internal, "gtk_plot_axis_set_labels_style");

   begin
      Internal (Get_Object (Axis), Style, Precision);
   end Axis_Set_Labels_Style;

   ----------------------
   -- Grids_Set_On_Top --
   ----------------------

   procedure Grids_Set_On_Top
     (Plot : access Gtk_Plot_Record; On_Top : Boolean)
   is
      procedure Internal (Plot : System.Address; On_Top : Integer);
      pragma Import (C, Internal, "gtk_plot_grids_set_on_top");
   begin
      Internal (Get_Object (Plot), Boolean'Pos (On_Top));
   end Grids_Set_On_Top;

   ------------------
   -- Grids_On_Top --
   ------------------

   function Grids_On_Top (Plot : access Gtk_Plot_Record) return Boolean is
      function Internal (Plot : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_plot_grids_on_top");
   begin
      return Boolean'Val (Internal (Get_Object (Plot)));
   end Grids_On_Top;

   -------------------------
   -- Set_Line_Attributes --
   -------------------------

   procedure Set_Line_Attributes
     (Plot : access Gtk_Plot_Record;
      Line : Gtk_Plot_Line)
   is
      procedure Internal (Plot : System.Address; Line : Gtk_Plot_Line);
      pragma Import (C, Internal, "gtk_plot_set_line_attributes");
   begin
      Internal (Get_Object (Plot), Line);
   end Set_Line_Attributes;

   ---------------
   -- Set_Ticks --
   ---------------

   procedure Set_Ticks
     (Plot        : access Gtk_Plot_Record;
      Orientation : Plot_Orientation;
      Major_Step  : Gdouble;
      Num_Minor   : Gint)
   is
      procedure Internal
        (Plot : System.Address;
         Orientation : Plot_Orientation;
         Major : Gdouble;
         Minor : Gint);
      pragma Import (C, Internal, "gtk_plot_set_ticks");
   begin
      Internal (Get_Object (Plot), Orientation, Major_Step, Num_Minor);
   end Set_Ticks;

   ---------------------
   -- Set_Major_Ticks --
   ---------------------

   procedure Set_Major_Ticks
     (Plot        : access Gtk_Plot_Record;
      Orientation : Plot_Orientation;
      Major_Step  : Gdouble)
   is
      procedure Internal
        (Plot : System.Address;
         Orientation : Plot_Orientation;
         Major : Gdouble);
      pragma Import (C, Internal, "gtk_plot_set_major_ticks");
   begin
      Internal (Get_Object (Plot), Orientation, Major_Step);
   end Set_Major_Ticks;

   ---------------------
   -- Set_Minor_Ticks --
   ---------------------

   procedure Set_Minor_Ticks
     (Plot        : access Gtk_Plot_Record;
      Orientation : Plot_Orientation;
      Num_Minor   : Gint)
   is
      procedure Internal
        (Plot : System.Address; Orient : Plot_Orientation; Minor : Gint);
      pragma Import (C, Internal, "gtk_plot_set_minor_ticks");
   begin
      Internal (Get_Object (Plot), Orientation, Num_Minor);
   end Set_Minor_Ticks;

   ----------------------
   -- Set_Ticks_Limits --
   ----------------------

   procedure Set_Ticks_Limits
     (Plot        : access Gtk_Plot_Record;
      Orientation : Plot_Orientation;
      Ticks_Begin : Gdouble;
      Ticks_End   : Gdouble)
   is
      procedure Internal
        (Plot : System.Address; Orient : Plot_Orientation; B, E : Gdouble);
      pragma Import (C, Internal, "gtk_plot_set_ticks_limits");
   begin
      Internal (Get_Object (Plot), Orientation, Ticks_Begin, Ticks_End);
   end Set_Ticks_Limits;

   ------------------------
   -- Unset_Ticks_Limits --
   ------------------------

   procedure Unset_Ticks_Limits
     (Plot        : access Gtk_Plot_Record;
      Orientation : Plot_Orientation)
   is
      procedure Internal (Plot : System.Address; Orient : Plot_Orientation);
      pragma Import (C, Internal, "gtk_plot_unset_ticks_limits");
   begin
      Internal (Get_Object (Plot), Orientation);
   end Unset_Ticks_Limits;

   ---------------
   -- Set_Break --
   ---------------

   procedure Set_Break
     (Plot         : access Gtk_Plot_Record;
      Orient       : Plot_Orientation;
      Min, Max     : Gdouble;
      Step_After   : Gdouble;
      Nminor_After : Gint;
      Scale_After  : Plot_Scale;
      Pos          : Gdouble)
   is
      procedure Internal
        (Plot : System.Address;
         Orien : Plot_Orientation;
         Min, Max, Step : Gdouble;
         Minor : Gint;
         Scale : Plot_Scale;
         Pos   : Gdouble);
      pragma Import (C, Internal, "gtk_plot_set_break");
   begin
      Internal (Get_Object (Plot), Orient, Min, Max, Step_After,
                Nminor_After, Scale_After, Pos);
   end Set_Break;

   ------------------
   -- Remove_Break --
   ------------------

   procedure Remove_Break
     (Plot : access Gtk_Plot_Record; Orient : Plot_Orientation)
   is
      procedure Internal (Plot : System.Address; Orient : Plot_Orientation);
      pragma Import (C, Internal, "gtk_plot_remove_break");
   begin
      Internal (Get_Object (Plot), Orient);
   end Remove_Break;

   -----------------------
   -- Axis_Ticks_Recalc --
   -----------------------

   procedure Axis_Ticks_Recalc (Axis : access Gtk_Plot_Axis_Record) is
      procedure Internal (Axis : System.Address);
      pragma Import (C, Internal, "gtk_plot_axis_ticks_recalc");
   begin
      Internal (Get_Object (Axis));
   end Axis_Ticks_Recalc;

   --------------------------
   -- Axis_Ticks_Transform --
   --------------------------

   function Axis_Ticks_Transform
     (Axis : access Gtk_Plot_Axis_Record;
      Y    : Gdouble) return Gdouble
   is
      function Internal (Axis : System.Address; Y : Gdouble) return Gdouble;
      pragma Import (C, Internal, "gtk_plot_axis_ticks_transform");
   begin
      return Internal (Get_Object (Axis), Y);
   end Axis_Ticks_Transform;

   ------------------------
   -- Axis_Ticks_Inverse --
   ------------------------

   function Axis_Ticks_Inverse
     (Axis : access Gtk_Plot_Axis_Record;
      X    : Gdouble) return Gdouble
   is
      function Internal (Axis : System.Address; X : Gdouble) return Gdouble;
      pragma Import (C, Internal, "gtk_plot_axis_ticks_inverse");
   begin
      return Internal (Get_Object (Axis), X);
   end Axis_Ticks_Inverse;

   ----------------------
   -- Axis_Parse_Label --
   ----------------------

   procedure Axis_Parse_Label
     (Axis      : access Gtk_Plot_Axis_Record;
      Val       : Gdouble;
      Precision : Gint;
      Style     : Gint;
      Label     : String)
   is
      procedure Internal
        (Axis : System.Address;
         Val : Gdouble;
         Precision, Style : Gint;
         Label : String);
      pragma Import (C, Internal, "gtk_plot_axis_parse_label");
   begin
      Internal (Get_Object (Axis), Val, Precision, Style, Label);
   end Axis_Parse_Label;

   --------------
   -- Gradient --
   --------------

   function Gradient
     (Data : access Gtk_Plot_Data_Record'Class) return Gtk_Plot_Axis
   is
      function Internal (Data : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_plot_data_gradient");
      Stub : Gtk_Plot_Axis_Record;
   begin
      return Gtk_Plot_Axis
        (Get_User_Data (Internal (Get_Object (Data)), Stub));
   end Gradient;

end Gtk.Extra.Plot;
