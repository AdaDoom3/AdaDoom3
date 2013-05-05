-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001-2006                       --
--                         AdaCore                                   --
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

--  <description>
--  A special plot that draws its data in three dimension. The data associated
--  with such plots should either be a function or a Gtk.Extra.Plot_Surface.
--  </description>
--  <c_version>gtkextra 2.1.1</c_version>
--  <group>Plotting Data</group>
--  <testgtk>create_plot_3d.adb</testgtk>
--  <screenshot>gtk-plot_3d</screenshot>

with Gdk.Color;
with Gdk.Drawable;
with Gtk.Extra.Plot;
with Gtk.Extra.Plot_Data;

package Gtk.Extra.Plot_3D is

   type Gtk_Plot_3D_Record is new Gtk.Extra.Plot.Gtk_Plot_Record with private;
   type Gtk_Plot_3D is access all Gtk_Plot_3D_Record'Class;

   type Plot_Plane is new Integer;
   Plot_Plane_Xy : constant Plot_Plane := 0;
   Plot_Plane_Yx : constant Plot_Plane := 0;
   Plot_Plane_Xz : constant Plot_Plane := 1;
   Plot_Plane_Zx : constant Plot_Plane := 1;
   Plot_Plane_Yz : constant Plot_Plane := 2;
   Plot_Plane_Zy : constant Plot_Plane := 2;

   type Plot_Side is mod 2 ** 32;
   Plot_Side_Xy : constant Plot_Side := 2 ** 0;
   Plot_Side_Xz : constant Plot_Side := 2 ** 1;
   Plot_Side_Yx : constant Plot_Side := 2 ** 2;
   Plot_Side_Yz : constant Plot_Side := 2 ** 3;
   Plot_Side_Zx : constant Plot_Side := 2 ** 4;
   Plot_Side_Zy : constant Plot_Side := 2 ** 5;

   procedure Gtk_New
     (Widget        : out Gtk_Plot_3D;
      Drawable      : Gdk.Drawable.Gdk_Drawable;
      Width, Height : Gdouble := 0.0);
   --  Create a new 3D plot.

   procedure Initialize
     (Widget        : access Gtk_Plot_3D_Record'Class;
      Drawable      : Gdk.Drawable.Gdk_Drawable;
      Width, Height : Gdouble);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   procedure Autoscale (Plot : access Gtk_Plot_3D_Record);
   --  Chooses the best ranges for all coordinates depending on the data sets
   --  put in Plot. Note that only data sets whose points you have set
   --  explicitely are taken into account, not the ones based on functions.

   function Get_Xfactor (Plot : access Gtk_Plot_3D_Record) return Gdouble;
   function Get_Yfactor (Plot : access Gtk_Plot_3D_Record) return Gdouble;
   function Get_Zfactor (Plot : access Gtk_Plot_3D_Record) return Gdouble;
   --  Get the scaling factor along each of the coordinates.

   procedure Set_Xfactor (Plot : access Gtk_Plot_3D_Record; Xfactor : Gdouble);
   procedure Set_Yfactor (Plot : access Gtk_Plot_3D_Record; Yfactor : Gdouble);
   procedure Set_Zfactor (Plot : access Gtk_Plot_3D_Record; Zfactor : Gdouble);
   --  Set the scaling factor along each of the coordinates

   procedure Set_Xrange (Plot : access Gtk_Plot_3D_Record; Min, Max : Gdouble);
   procedure Set_Yrange (Plot : access Gtk_Plot_3D_Record; Min, Max : Gdouble);
   procedure Set_Zrange (Plot : access Gtk_Plot_3D_Record; Min, Max : Gdouble);
   --  Set the minimal and maximal values for each axis.

   ----------
   -- Axis --
   ----------

   procedure Show_Title
     (Plot : access Gtk_Plot_3D_Record; Side : Plot_Side);
   --  Show the title associated with the axis.

   procedure Hide_Title
     (Plot : access Gtk_Plot_3D_Record; Side : Plot_Side);
   --  Hide the title associated with the axis.

   procedure Set_Major_Ticks
     (Plot       : access Gtk_Plot_3D_Record;
      Axis       : Gtk.Extra.Plot.Plot_Orientation;
      Major_Step : Gdouble);
   --  Modify the step for major ticks.
   --  This is a percentage value that indicates how many major ticks are
   --  drawn along the axis. See also Axis_Set_Ticks.

   procedure Set_Minor_Ticks
     (Plot   : access Gtk_Plot_3D_Record;
      Axis   : Gtk.Extra.Plot.Plot_Orientation;
      Nminor : Gint);
   --  Modify the number of minor ticks between each major one.
   --  See also Axis_Set_Ticks.

   procedure Set_Ticks
     (Plot       : access Gtk_Plot_3D_Record;
      Axis       : Gtk.Extra.Plot.Plot_Orientation;
      Major_Step : Gdouble;
      Nminor     : Gint);
   --  Set up ticks for a specific orientation.
   --  A horizontal orientation will match the left and right sides, whereas
   --  a vertical orientation will match the top and bottom sides.
   --  Major_Step is a percentage value of the widget size, and indicate the
   --  step between each big ticks. For instance, if Major_Step has a value
   --  of 0.2, there will be 5 big ticks drawn along the axis.
   --  Num_Minor is the number of minor ticks between each major one.

   procedure Set_Ticks_Length
     (Plot   : access Gtk_Plot_3D_Record;
      Axis   : Gtk.Extra.Plot.Plot_Orientation;
      Length : Gint);
   --  Set the length (in pixels) of the big ticks.
   --  The small ticks will have half this length.

   procedure Set_Ticks_Width
     (Plot  : access Gtk_Plot_3D_Record;
      Axis  : Gtk.Extra.Plot.Plot_Orientation;
      Width : Gfloat);
   --  Set the width (in pixels) of the ticks.
   --  This width is common to both the long and short ticks.

   procedure Show_Labels
     (Plot       : access Gtk_Plot_3D_Record;
      Side       : Plot_Side;
      Label_Mask : Gint);
   --  Indicate whether a label should be drawn at each ticks to indicate
   --  its value.
   --  Not all values of Labels_Mask are relevant for all axis. For instance,
   --  for a vertical axis, the relevant values are Label_Right and Label_Left.

   procedure Show_Ticks
     (Plot       : access Gtk_Plot_3D_Record;
      Side       : Plot_Side;
      Major_Mask : Gtk.Extra.Plot.Plot_Ticks_Pos;
      Minor_Mask : Gtk.Extra.Plot.Plot_Ticks_Pos);
   --  Set the style of the ticks.

   function Get_Axis
     (Plot        : access Gtk_Plot_3D_Record;
      Orientation : Gtk.Extra.Plot.Plot_Orientation)
      return Gtk.Extra.Plot.Gtk_Plot_Axis;
   --  Return a handle to a specific axis.

   function Get_Side
     (Plot : access Gtk_Plot_3D_Record; Side : Plot_Side)
      return Gtk.Extra.Plot.Gtk_Plot_Axis;
   --  Get the axis for a specific side.

   procedure Set_Scale
     (Plot  : access Gtk_Plot_3D_Record;
      Axis  : Gtk.Extra.Plot.Plot_Orientation;
      Scale : Gtk.Extra.Plot_Data.Plot_Scale);
   --  Set the scale for the axis

   function Get_Scale
     (Plot  : access Gtk_Plot_3D_Record;
      Axis  : Gtk.Extra.Plot.Plot_Orientation)
      return Gtk.Extra.Plot_Data.Plot_Scale;
   --  Get the current sale for the axis

   ----------
   -- Grid --
   ----------

   procedure Major_Grids_Set_Visible
     (Plot : access Gtk_Plot_3D_Record; X, Y, Z : Boolean);
   --  Indicate whether the grid should be displayed for each coordinate

   procedure Major_Grids_Visible
     (Plot : access Gtk_Plot_3D_Record; X, Y, Z : out Boolean);
   --  Indicate whether the grid is currently displayed.

   procedure Minor_Grids_Set_Visible
     (Plot : access Gtk_Plot_3D_Record; X, Y, Z : Boolean);
   --  Indicate whether the grid should be displayed for each coordinate

   procedure Minor_Grids_Visible
     (Plot : access Gtk_Plot_3D_Record; X, Y, Z : out Boolean);
   --  Indicate whether the grid is currently displayed.

   procedure Major_Zgrid_Get_Attributes
     (Plot  : access Gtk_Plot_3D_Record;
      Style : out Gtk.Extra.Plot_Data.Plot_Line_Style;
      Width : out Gfloat;
      Color : out Gdk.Color.Gdk_Color);
   --  Get the attributes of the major grid

   procedure Major_Zgrid_Set_Attributes
     (Plot  : access Gtk_Plot_3D_Record;
      Style : Gtk.Extra.Plot_Data.Plot_Line_Style;
      Width : Gfloat;
      Color : Gdk.Color.Gdk_Color);
   --  Set the attributes of the major grid

   procedure Minor_Zgrid_Get_Attributes
     (Plot  : access Gtk_Plot_3D_Record;
      Style : out Gtk.Extra.Plot_Data.Plot_Line_Style;
      Width : out Gfloat;
      Color : out Gdk.Color.Gdk_Color);
   --  Get the attributes of the minor grid

   procedure Minor_Zgrid_Set_Attributes
     (Plot  : access Gtk_Plot_3D_Record;
      Style : Gtk.Extra.Plot_Data.Plot_Line_Style;
      Width : Gfloat;
      Color : Gdk.Color.Gdk_Color);
   --  Set the attributes of the minor grid

   --------------
   -- Rotating --
   --------------

   procedure Reset_Angles (Plot : access Gtk_Plot_3D_Record);
   --  reset all the angles to their default values

   procedure Rotate
     (Plot    : access Gtk_Plot_3D_Record;
      Angle_X, Angle_Y, Angle_Z : Gdouble);
   --  Rotate the plot along the three axis at the same time.
   --  The angles are specified in degrees.

   procedure Rotate_Vector
     (Plot       : access Gtk_Plot_3D_Record;
      Vector     : Gtk.Extra.Plot.Plot_Vector;
      A1, A2, A3 : Gdouble);
   --  Rotate Vector along the three axis.
   --  The three angles A1, A2 and A3 are specified in degrees.

   procedure Rotate_X (Plot  : access Gtk_Plot_3D_Record; Angle : Gdouble);
   procedure Rotate_Y (Plot  : access Gtk_Plot_3D_Record; Angle : Gdouble);
   procedure Rotate_Z (Plot  : access Gtk_Plot_3D_Record; Angle : Gdouble);
   --  Rotate the plot along a specific axis.
   --  Angle is specific in degrees.

   ------------
   -- Planes --
   ------------
   --  A 3D plot is associated, as usual, with three axis (one per coordinate
   --  X, Y and Z). These three axis, together, define 3 planes that can be
   --  shown or hidden, and on which a grid can be displayed to make it easy
   --  to visualize the value of the data.

   procedure Plane_Set_Color
     (Plot : access Gtk_Plot_3D_Record;
      Plane : Plot_Plane;
      Color : Gdk.Color.Gdk_Color);
   --  Define the background color to use for one of the planes. Each plane
   --  can have its own color.

   procedure Plane_Set_Visible
     (Plot : access Gtk_Plot_3D_Record; Plane : Plot_Plane; Visible : Boolean);
   --  Indicate whether each plane should be displayed or not.

   function Plane_Visible
     (Plot : access Gtk_Plot_3D_Record; Plane : Plot_Plane) return Boolean;
   --  Indicate whether a plane is currently visible or not.

   -------------
   -- Corners --
   -------------
   --  In addition to drawing the three planes defined by the axis, a 3D plot
   --  can also draw some lines to draw a cube around the plot (although the
   --  three new planes defined by these lines are left transparent so that
   --  the plot is visible.

   procedure Corner_Get_Attributes
     (Plot  : access Gtk_Plot_3D_Record;
      Style : out Gtk.Extra.Plot_Data.Plot_Line_Style;
      Width : out Gfloat;
      Color : out Gdk.Color.Gdk_Color);
   --  Get the style of the corner lines.

   procedure Corner_Set_Attributes
     (Plot  : access Gtk_Plot_3D_Record;
      Style : Gtk.Extra.Plot_Data.Plot_Line_Style;
      Width : Gfloat;
      Color : Gdk.Color.Gdk_Color);
   --  Define the style of the corner lines.

   procedure Corner_Set_Visible
     (Plot : access Gtk_Plot_3D_Record; Visible : Boolean);
   --  Whether corners should be visible

   function Corner_Visible (Plot : access Gtk_Plot_3D_Record) return Boolean;
   --  Indicate whether corners are visible

   ----------
   -- Misc --
   ----------

   procedure Frame_Get_Attributes
     (Plot  : access Gtk_Plot_3D_Record;
      Style : out Gtk.Extra.Plot_Data.Plot_Line_Style;
      Width : out Gfloat;
      Color : out Gdk.Color.Gdk_Color);

   procedure Frame_Set_Attributes
     (Plot  : access Gtk_Plot_3D_Record;
      Style : Gtk.Extra.Plot_Data.Plot_Line_Style;
      Width : Gfloat;
      Color : Gdk.Color.Gdk_Color);

   procedure Get_Pixel
     (Plot       : access Gtk_Plot_3D_Record;
      X, Y, Z    : Gdouble;
      Px, Py, Pz : out Gdouble);

   function Get_Titles_Offset (Plot : access Gtk_Plot_3D_Record) return Gint;

   procedure Set_Titles_Offset
     (Plot : access Gtk_Plot_3D_Record; Offset : Gint);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  </signals>

private
   type Gtk_Plot_3D_Record is new Gtk.Extra.Plot.Gtk_Plot_Record
     with null record;
   pragma Import (C, Get_Type, "gtk_plot3d_get_type");
end Gtk.Extra.Plot_3D;
