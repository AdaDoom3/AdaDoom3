-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--      Copyright (C) 2000 E. Briot, J. Brobecker and A. Charlet     --
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

--  <description>
--
--  This package implements a high-level, general purpose plotting widget.
--  You can display any set of data (set of points, curve defined by a
--  parametric function, ...). This widget can automatically display them
--  as a curve, along with labelled axis, axis tic marks, legends,...
--
--  This is the base class, that provides 2D graphics. Some children provide
--  polar-coordinates and 3D graphics in addition.
--
--  It fully supports the drag-and-drop protocol for all of its children,
--  which means that the user can interactively move them in the Gtk_Plot
--  area.
--
--  A Gtk_Plot is closely associated with a Gdk_Drawable, on which all the
--  drawings are done. It can be done anywhere within that drawable, its
--  "position" is indicated by a tuple (X, Y), which are two values between
--  0.0 and 1.0 (from left to right, or from top to bottom).
--  Its size is also given as a ratio other the drawable's size.
--
--  Most points in the plot have also this relative coordinates systems, which
--  makes it really easy to handle resizing of a plot window.
--
--  See the package Gtk.Extra.Plot_Ps for a way to easily print a Gtk_Plot to
--  a postscript file.
--
--  In this package, font parameters are sometimes required. Here is the
--  list of possible fonts used by Gtk.Extra:
--
--   - "Times-Roman",
--   - "Times-Italic",
--   - "Times-Bold",
--   - "Times-BoldItalic",
--   - "AvantGarde-Book",
--   - "AvantGarde-BookOblique",
--   - "AvantGarde-Demi",
--   - "AvantGarde-DemiOblique",
--   - "Bookman-Light",
--   - "Bookman-LightItalic",
--   - "Bookman-Demi",
--   - "Bookman-DemiItalic",
--   - "Courier",
--   - "Courier-Oblique",
--   - "Courier-Bold",
--   - "Courier-BoldOblique",
--   - "Helvetica",
--   - "Helvetica-Oblique",
--   - "Helvetica-Bold",
--   - "Helvetica-BoldOblique",
--   - "Helvetica-Narrow",
--   - "Helvetica-Narrow-Oblique",
--   - "Helvetica-Narrow-Bold",
--   - "Helvetica-Narrow-BoldOblique",
--   - "NewCenturySchoolbook-Roman",
--   - "NewCenturySchoolbook-Italic",
--   - "NewCenturySchoolbook-Bold",
--   - "NewCenturySchoolbook-BoldItalic",
--   - "Palatino-Roman",
--   - "Palatino-Italic",
--   - "Palatino-Bold",
--   - "Palatino-BoldItalic",
--   - "Symbol",
--   - "ZapfChancery-MediumItalic",
--   - "ZapfDingbats",
--
--  </description>
--  <c_version>gtkextra 2.1.1</c_version>
--  <group>Plotting Data</group>
--  <testgtk>create_plot.adb</testgtk>
--  <screenshot>gtk-plot</screenshot>

with Glib.Object;

with System;
with Gdk.Color;
with Gdk.Drawable;
with Gdk.Pixmap;
with Gdk.Rectangle;
with Gtk.Enums;
with Gtk.Extra.Plot_Data;   use Gtk.Extra.Plot_Data;
with Gtk.Widget;

package Gtk.Extra.Plot is

   type Gtk_Plot_Record is new Gtk.Widget.Gtk_Widget_Record with private;
   type Gtk_Plot is access all Gtk_Plot_Record'Class;

   type Gtk_Plot_Axis_Record is new Glib.Object.GObject_Record with private;
   type Gtk_Plot_Axis is access all Gtk_Plot_Axis_Record'Class;
   --  One of the axis of the plot.
   --  There are up to six axis for each plot, one on each side. They can have
   --  ticks, labels, etc.

   type Gtk_Plot_Text is new Gdk.C_Proxy;
   --  A text that can be displayed anywhere on the plot.

   type Gtk_Plot_Line is new Gdk.C_Proxy;
   --  A simple line drawn on the plot.

   type Gtk_Plot_Symbol is new Gdk.C_Proxy;
   type Gtk_Plot_Tick   is new Gdk.C_Proxy;
   type Gtk_Plot_Ticks  is new Gdk.C_Proxy;

   type Plot_Vector is record
      X, Y, Z : Gdouble;
   end record;

   ----------------
   -- Enum types --
   ----------------

   type Plot_Border_Style is
     (Border_None,
      --  No border is drawn

      Border_Line,
      --  A simple line on each side

      Border_Shadow
      --  The right and bottom lines are
      --  thicker
     );
   --  Border types used for legends.
   pragma Convention (C, Plot_Border_Style);

   --     subtype Plot_Scale        is Gtk.Extra.Plot_Data.Plot_Scale;
   --     subtype Plot_Label_Style  is Gtk.Extra.Plot_Data.Plot_Label_Style;
   --     subtype Plot_Symbol_Style is Gtk.Extra.Plot_Data.Plot_Symbol_Style;
   --     subtype Plot_Symbol_Type  is Gtk.Extra.Plot_Data.Plot_Symbol_Type;
   --     subtype Plot_Line_Style   is Gtk.Extra.Plot_Data.Plot_Line_Style;
   --     subtype Plot_Connector    is Gtk.Extra.Plot_Data.Plot_Connector;

   --  In C, these types are declared in gtkplot.h. However, because of type
   --  circularity, we need to define them in Gtk.Extra.Plot_Data, and have
   --  subtypes here. This would lead to unnecessary required qualification in
   --  user code though...

   type Plot_Label_Pos is new Integer;
   --  Position of labels along an axis.

   Label_None   : constant Plot_Label_Pos;
   Label_In     : constant Plot_Label_Pos;
   Label_Out    : constant Plot_Label_Pos;

   type Plot_Error is (Error_Div_Zero, Error_Log_Neg);
   --  Errors that can be encountered while calculating a graph.
   pragma Convention (C, Plot_Error);

   type Plot_Axis_Pos is (Axis_Left, Axis_Right, Axis_Top, Axis_Bottom);
   --  Where the axis should be put
   pragma Convention (C, Plot_Axis_Pos);

   type Plot_Orientation is (Axis_X, Axis_Y, Axis_Z);
   --  How to reference axis in 3D plots
   pragma Convention (C, Plot_Orientation);

   type Plot_Ticks_Pos is new Integer;
   --  The position and orientation of the ticks along an axis.
   --  See the constants below for the possible values.
   --  Note also that not all the values are valid with all types of axis.

   Ticks_None  : constant Plot_Ticks_Pos;
   Ticks_In    : constant Plot_Ticks_Pos;
   Ticks_Out   : constant Plot_Ticks_Pos;

   ---------------------
   -- Creating a plot --
   ---------------------

   procedure Gtk_New
     (Plot     : out Gtk_Plot;
      Drawable : Gdk.Drawable.Gdk_Drawable := Gdk.Drawable.Null_Drawable);
   --  Create a new plot, that will be displayed in Drawable.
   --  All the dataset, labels, axis,... associated with the plot will be drawn
   --  in that drawable, which must have been created beforehand.
   --  Note that the drawable can also be set later with Set_Drawable.

   procedure Gtk_New
     (Plot     : out Gtk_Plot;
      Width    : Gdouble;
      Height   : Gdouble;
      Drawable : Gdk.Drawable.Gdk_Drawable := Gdk.Drawable.Null_Drawable);
   --  Create a new plot with a specific size.

   procedure Initialize
     (Plot     : access Gtk_Plot_Record'Class;
      Drawable : Gdk.Drawable.Gdk_Drawable);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Initialize
     (Plot     : access Gtk_Plot_Record'Class;
      Drawable : Gdk.Drawable.Gdk_Drawable;
      Width    : Gdouble;
      Height   : Gdouble);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Plot.

   procedure Set_Drawable
     (Plot     : access Gtk_Plot_Record;
      Drawable : Gdk.Drawable.Gdk_Drawable);
   --  Modify the drawable on which the graphs are displayed.
   --  From now on, all the drawings will be done on that drawable. Note that
   --  they are not automatically copied to the new Drawable until the Plot
   --  needs to be redrawn.

   function Get_Drawable
     (Plot : access Gtk_Plot_Record) return Gdk.Drawable.Gdk_Drawable;
   --  Return the drawable on which the graphs are plotted.

   procedure Set_Background
     (Plot       : access Gtk_Plot_Record;
      Background : Gdk.Color.Gdk_Color);
   --  Change the background for the plot.
   --  Note that this has no effect if the plot has been set to transparent
   --  (see the flags below).
   --  The Plot is also redrawn as soon as you modify this color.

   procedure Set_Background_Pixmap
     (Plot : access Gtk_Plot_Record; Pixmap : Gdk.Pixmap.Gdk_Pixmap);
   --  Specificy a background pixmap to use for the plot

   procedure Set_Transparent
     (Plot : access Gtk_Plot_Record; Transparent : Boolean);
   --  Whether the plot is transparent. If Transparent is True, all background
   --  attributes are ignored (pixmap, color,...)

   function Is_Transparent (Plot : access Gtk_Plot_Record) return Boolean;
   --  Whether the plot is current transparent

   procedure Paint (Plot : access Gtk_Plot_Record);
   --  Force an immediate repaint of the widget in its pixmap.
   --  The modification won't appear on the screen until you call Refresh.
   --  It is probably not a good idea to call this function directly, and it
   --  is more efficient to queue a draw request (see the Gtk.Widget package
   --  for related functions).

   procedure Refresh
     (Plot : access Gtk_Plot_Record;
      Area : Gdk.Rectangle.Gdk_Rectangle);
   --  Copy the plot's pixmap to the screen.
   --  The same comment as for Paint applies here, and you probably don't
   --  have to call this function yourself, since queuing a draw request is
   --  more efficient.

   ----------------------------
   --  Coordinates and sizes --
   ----------------------------

   procedure Get_Position
     (Plot : access Gtk_Plot_Record;
      X    : out Gdouble;
      Y    : out Gdouble);
   --  Return the position of the Plot within its drawable.
   --  X and Y are in the range 0.0 .. 1.0, where (0.0, 0.0) is the top-left
   --  corner and (1.0, 1.0) the bottom-right corner. The position can be
   --  modified by Move below.

   procedure Get_Size
     (Plot   : access Gtk_Plot_Record;
      Width  : out Gdouble;
      Height : out Gdouble);
   --  Return the size of the Plot.
   --  Width and Height are both in the range 0.0 .. 1.0, where 1.0 means they
   --  occupy all the space available in the Drawable, 0.5 means they only
   --  occupy half of it.

   function Get_Internal_Allocation
     (Plot : access Gtk_Plot_Record) return Gtk.Widget.Gtk_Allocation;
   --  Return the real position/size of the plot inside its parent container.
   --  You should use this function instead of converting yourself the result
   --  of Get_Position and Get_Size.

   procedure Set_Magnification
     (Plot          : access Gtk_Plot_Record;
      Magnification : Gdouble);
   --  Change the magnification level of the plot.
   --  1.0 is the default magnification, higher values will zoom in while lower
   --  values will zoom out.

   procedure Move
     (Plot : access Gtk_Plot_Record;
      X    : Gdouble;
      Y    : Gdouble);
   --  Move the plot widget inside its drawable.
   --  X and Y should both be in the range 0.0 .. 1.0 (from top-left corner
   --  to bottom-right corner).

   procedure Resize
     (Plot   : access Gtk_Plot_Record;
      Width  : Gdouble;
      Height : Gdouble);
   --  Resize the widget.
   --  Width and Height should both be in the range 0.0 .. 1.0, this indicates
   --  which ratio of the drawable's screen real-estate they should use.

   procedure Move_Resize
     (Plot   : access Gtk_Plot_Record;
      X      : Gdouble;
      Y      : Gdouble;
      Width  : Gdouble;
      Height : Gdouble);
   --  Move and resize the widget in a single operation.
   --  This is faster than doing each operation separately.

   procedure Get_Pixel
     (Plot : access Gtk_Plot_Record;
      Xx   : Gdouble;
      Yy   : Gdouble;
      X    : out Gdouble;
      Y    : out Gdouble);
   --  Get the screen coordinate (relative to Plot's parent) of a point.
   --  The initial coordinates (Xx, Yy) should be in the range 0.0 .. 1.0.

   procedure Clip_Data (Plot : access Gtk_Plot_Record; Clip : Boolean);
   --  If Clip is True, any drawing of a Gtk_Plot_Data will be limited to the
   --  area occupied by Plot. Otherwise, it might draw outside of Plot.

   procedure Get_Point
     (Plot : access Gtk_Plot_Record;
      X    : Gint;
      Y    : Gint;
      Xx   : out Gdouble;
      Yy   : out Gdouble);
   --  Convert from an absolute screen coordinate to a relative one.
   --  (X, Y) should be relative to Plot's parent.
   --  This function is the opposite of Get_Pixel.

   procedure Set_Xrange
     (Plot : access Gtk_Plot_Record;
      Xmin : Gdouble := 0.0;
      Xmax : Gdouble := 1.0);
   --  Set the range of visible points for this plot.
   --  Only the points of the graph those coordinates are in the range
   --  Xmin .. Xmax will be visible.

   procedure Set_Yrange
     (Plot : access Gtk_Plot_Record;
      Ymin : Gdouble := 0.0;
      Ymax : Gdouble := 1.0);
   --  Set the range of visible points for this plot.
   --  Only the points of the graph those coordinates are in the range
   --  Xmin .. Xmax will be visible.

   procedure Set_Range
     (Plot : access Gtk_Plot_Record;
      Xmin : Gdouble := 0.0;
      Xmax : Gdouble := 1.0;
      Ymin : Gdouble := 0.0;
      Ymax : Gdouble := 1.0);
   --  Set both ranges at the same time

   procedure Autoscale (Plot : access Gtk_Plot_Record);
   --  Calculate automically the appropriate ranges for the plot.

   procedure Get_Xrange
     (Plot : access Gtk_Plot_Record;
      Xmin : out Gdouble;
      Xmax : out Gdouble);
   --  Get the current range for the X axis.

   procedure Get_Yrange
     (Plot : access Gtk_Plot_Record;
      Ymin : out Gdouble;
      Ymax : out Gdouble);
   --  Get the current range for the X axis.

   procedure Set_Xscale
     (Plot       : access Gtk_Plot_Record;
      Scale_Type : Plot_Scale);
   --  Set the type of the X axis (logarithmic, linear, ...).

   procedure Set_Yscale
     (Plot       : access Gtk_Plot_Record;
      Scale_Type : Plot_Scale);
   --  Set the type of the Y axis (logarithmic, linear, ...).

   function Get_Xscale
     (Plot : access Gtk_Plot_Record) return Plot_Scale;
   --  Get the type of the X axis.

   function Get_Yscale
     (Plot : access Gtk_Plot_Record) return Plot_Scale;
   --  Get the type of the Y axis.

   procedure Reflect_X (Plot : access Gtk_Plot_Record; Reflect : Boolean);
   --  Reverse the direction of the X axis

   function Is_X_Reflected (Plot : access Gtk_Plot_Record) return Boolean;
   --  Whether the X axis is currently reflected

   procedure Reflect_Y (Plot : access Gtk_Plot_Record; Reflect : Boolean);
   --  Reverse the direction of the Y axis

   function Is_Y_Reflected (Plot : access Gtk_Plot_Record) return Boolean;
   --  Whether the Y axis is currently reflected

   ----------
   -- Text --
   ----------

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
      Text          : String := "") return Gtk_Plot_Text;
   --  Print some text in Plot.
   --  The text will be drawn at the relative coordinates (X, Y), with a
   --  specified Angle.
   --  If Font is the empty string, a default font and default Font_Height
   --  will be used. Likewise, default colors will be used if you don't
   --  specify any. Font should be the name of a postscript font, the list of
   --  which can be found in Gtk.Plot.Psfont.
   --  If Transparent is True, then no background will be drawn for the text.

   procedure Remove_Text
     (Plot : access Gtk_Plot_Record;
      Text : Gtk_Plot_Text);
   --  Remove some text that is currently visible on the plot.
   --  Nothing is done if Text is currently not visible.

   procedure Text_Get_Area
     (Text          : Gtk_Plot_Text;
      Angle         : Plot_Angle;
      Just          : Gtk.Enums.Gtk_Justification;
      Font_Name     : String;
      Font_Size     : Gint;
      X             : out Gint;
      Y             : out Gint;
      Width         : out Gint;
      Height        : out Gint);
   --  Return the area currently occupied by a text.
   --  The coordinates are relative to the top-left corner of the plot in
   --  which the text was put.

   procedure Text_Get_Size
     (Text          : Gtk_Plot_Text;
      Angle         : Plot_Angle;
      Font_Name     : String;
      Font_Size     : Gint;
      Width         : out Gint;
      Height        : out Gint;
      Ascent        : out Gint;
      Descent       : out Gint);
   --  Return the size in pixels occupied by a text in the plot.
   --  See Gtk.Extra.Plot_Canvas for a function that returns a Gtk_Plot_Text.

   procedure Text_Set_Attributes
     (Text          : Gtk_Plot_Text;
      Font          : String;
      Height        : Gint;
      Angle         : Plot_Angle;
      Fg            : Gdk.Color.Gdk_Color;
      Bg            : Gdk.Color.Gdk_Color;
      Transparent   : Boolean := False;
      Justification : Gtk.Enums.Gtk_Justification := Gtk.Enums.Justify_Center;
      Str           : String := "");
   --  Change the attributes of Text.

   procedure Text_Set_Border
     (Text         : Gtk_Plot_Text;
      Border       : Plot_Border_Style;
      Border_Space : Gint;
      Border_Width : Gint;
      Shadow_Width : Gint);
   --  Set the border attributes for the text

   procedure Draw_Text
     (Plot : access Gtk_Plot_Record;
      Text : Gtk_Plot_Text);
   --  Draw the text

   -----------
   -- Lines --
   -----------

   procedure Draw_Line
     (Plot           : access Gtk_Plot_Record;
      Line           : Gtk_Plot_Line;
      X1, Y1, X2, Y2 : Gdouble);
   --  Draw a line on the plot

   procedure Set_Line_Attributes
     (Plot : access Gtk_Plot_Record;
      Line : Gtk_Plot_Line);

   ----------
   -- Axis --
   ----------
   --  A Gtk_Plot has four axis, one one each of its sides. These axis can
   --  have ticks, labels for ticks, titles, ... associated with them.

   procedure Set_Ticks
     (Plot        : access Gtk_Plot_Record;
      Orientation : Plot_Orientation;
      Major_Step  : Gdouble;
      Num_Minor   : Gint);
   --  Set up ticks for a specific orientation.
   --  A horizontal orientation will match the left and right sides, whereas
   --  a vertical orientation will match the top and bottom sides.
   --  Major_Step is a value between 0.0 and 1.0 which indicates the
   --  proportion of the total axis length between successive big ticks.
   --  For instance, if Major_Step has a value of 0.2, there will be 5 big
   --  ticks drawn along the axis.
   --  Num_Minor is the number of minor ticks between each major one.

   procedure Set_Major_Ticks
     (Plot        : access Gtk_Plot_Record;
      Orientation : Plot_Orientation;
      Major_Step  : Gdouble);
   --  Modify the step for major ticks.
   --  Major_Step is a value between 0.0 and 1.0 which indicates the
   --  proportion of the total axis length between successive big ticks.
   --  For instance, if Major_Step has a value of 0.2, there will be 5 big
   --  ticks drawn along the axis.
   --  See also Set_Ticks.

   procedure Set_Minor_Ticks
     (Plot        : access Gtk_Plot_Record;
      Orientation : Plot_Orientation;
      Num_Minor   : Gint);
   --  Modify the number of minor ticks between each major one.
   --  See also Axis_Set_Ticks.

   procedure Set_Ticks_Limits
     (Plot        : access Gtk_Plot_Record;
      Orientation : Plot_Orientation;
      Ticks_Begin : Gdouble;
      Ticks_End   : Gdouble);
   --  Indicate the area of the axis that should have ticks.
   --  Ticks will be displayed only from Ticks_Beg to Ticks_End.

   procedure Unset_Ticks_Limits
     (Plot        : access Gtk_Plot_Record;
      Orientation : Plot_Orientation);
   --  Cancel the ticks limits set by a previous call to
   --  Axis_Set_Ticks_Limits.

   procedure Set_Break
     (Plot         : access Gtk_Plot_Record;
      Orient       : Plot_Orientation;
      Min, Max     : Gdouble;
      Step_After   : Gdouble;
      Nminor_After : Gint;
      Scale_After  : Plot_Scale;
      Pos          : Gdouble);
   --  ???

   procedure Remove_Break
     (Plot : access Gtk_Plot_Record; Orient : Plot_Orientation);
   --  ???

   procedure Gtk_New
     (Axis        : out Gtk_Plot_Axis;
      Orientation : Plot_Orientation);
   --  Create a new axis

   procedure Initialize
     (Axis        : access Gtk_Plot_Axis_Record'Class;
      Orientation : Plot_Orientation);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Axis_Get_Type return Gtk_Type;
   --  Return the internal value associated with a Gtk_Plot_Axis.

   function Get_Axis
     (Plot   : access Gtk_Plot_Record;
      Axis   : Plot_Axis_Pos) return Gtk_Plot_Axis;
   --  Get a pointer to an axis.

   function Gradient
     (Data : access Gtk_Plot_Data_Record'Class) return Gtk_Plot_Axis;
   --  Return the gradient associated with Data.
   --
   --  This function cannot be defined in Gtk.Plot_Data, since Gtk_Plot_Axis
   --  must be defined in the same package as its primitive operations, ie
   --  Gtk.Plot

   procedure Axis_Set_Visible
     (Axis    : access Gtk_Plot_Axis_Record;
      Visible : Boolean);
   --  Indicate whether the axis should be visible or not.

   function Axis_Visible
     (Axis    : access Gtk_Plot_Axis_Record) return Boolean;
   --  Return the visibility state of the axis

   procedure Axis_Set_Title
     (Axis  : access Gtk_Plot_Axis_Record;
      Title : String);
   --  Modify the title of the axis.
   --  Each axis has a title that is displayed along its line (vertically
   --  for the left and right sides).

   procedure Axis_Show_Title
     (Axis : access Gtk_Plot_Axis_Record);
   --  Show the title associated with the axis.

   procedure Axis_Hide_Title
     (Axis : access Gtk_Plot_Axis_Record);
   --  Hide the title associated with the axis.

   procedure Axis_Move_Title
     (Axis  : access Gtk_Plot_Axis_Record;
      Angle : Plot_Angle;
      X     : Gdouble;
      Y     : Gdouble);
   --  Modify the position and orientation of the axis' title.
   --  X and Y indicate a position relative to the location of the axis (0.0
   --  to display it to the left (resp. top) of the axis, 1.0 to display it
   --  to the right (resp. bottom) of the axis.

   procedure Axis_Justify_Title
     (Axis          : access Gtk_Plot_Axis_Record;
      Justification : Gtk.Enums.Gtk_Justification);
   --  Modify the justification for the axis.

   procedure Axis_Set_Attributes
     (Axis  : access Gtk_Plot_Axis_Record;
      Width : Gfloat;
      Color : Gdk.Color.Gdk_Color);
   --  Modify the attributes of the lines of the axis.

   procedure Axis_Get_Attributes
     (Axis  : access Gtk_Plot_Axis_Record;
      Width : out    Gfloat;
      Color : out    Gdk.Color.Gdk_Color);
   --  Get the attributes of the axis.

   procedure Axis_Set_Ticks
     (Axis        : access Gtk_Plot_Axis_Record;
      Major_Step  : Gdouble;
      Num_Minor   : Gint);
   --  Set up ticks for a specific orientation.
   --  A horizontal orientation will match the left and right sides, whereas
   --  a vertical orientation will match the top and bottom sides.
   --  Major_Step is a value between 0.0 and 1.0 which indicates the
   --  proportion of the total axis length between successive big ticks.
   --  For instance, if Major_Step has a value of 0.2, there will be 5 big
   --  ticks drawn along the axis.

   procedure Axis_Set_Major_Ticks
     (Axis        : access Gtk_Plot_Axis_Record;
      Major_Step  : Gdouble);
   --  Modify the step for major ticks.
   --  Major_Step is a value between 0.0 and 1.0 which indicates the
   --  proportion of the total axis length between successive big ticks.
   --  For instance, if Major_Step has a value of 0.2, there will be 5 big
   --  ticks drawn along the axis.
   --  See also Axis_Set_Ticks.

   procedure Axis_Set_Minor_Ticks
     (Axis        : access Gtk_Plot_Axis_Record;
      Num_Minor   : Gint);
   --  Modify the number of minor ticks between each major one.
   --  See also Axis_Set_Ticks.

   procedure Axis_Set_Ticks_Length
     (Axis   : access Gtk_Plot_Axis_Record;
      Length : Gint);
   --  Set the length (in pixels) of the big ticks.
   --  The small ticks will have half this length.

   procedure Axis_Set_Ticks_Width
     (Axis  : access Gtk_Plot_Axis_Record;
      Width : Gfloat);
   --  Set the width (in pixels) of the ticks.
   --  This width is common to both the long and short ticks.

   procedure Axis_Show_Ticks
     (Axis       : access Gtk_Plot_Axis_Record;
      Major_Mask : Plot_Ticks_Pos;
      Minor_Mask : Plot_Ticks_Pos);
   --  Set the style of the ticks.

   procedure Axis_Set_Ticks_Limits
     (Axis        : access Gtk_Plot_Axis_Record;
      Ticks_Begin : Gdouble;
      Ticks_End   : Gdouble);
   --  Indicate the area of the axis that should have ticks.
   --  Ticks will be displayed only from Ticks_Beg to Ticks_End.

   procedure Axis_Unset_Ticks_Limits
     (Axis        : access Gtk_Plot_Axis_Record);
   --  Cancel the ticks limits set by a previous call to
   --  Axis_Set_Ticks_Limits.

   procedure Axis_Set_Break
     (Axis         : access Gtk_Plot_Axis_Record;
      Min, Max     : Gdouble;
      Step_After   : Gdouble;
      Nminor_After : Gint;
      Scale_After  : Plot_Scale;
      Pos          : Gdouble);
   --  ???

   procedure Axis_Remove_Break (Axis : access Gtk_Plot_Axis_Record);
   --  ???

   procedure Axis_Show_Labels
     (Axis        : access Gtk_Plot_Axis_Record;
      Labels_Mask : Plot_Label_Pos);
   --  Indicate whether a label should be drawn at each ticks to indicate
   --  its value.
   --  Not all values of Labels_Mask are relevant for all axis. For instance,
   --  for a vertical axis, the relevant values are Axis_Right and Axis_Left.

   procedure Axis_Title_Set_Attributes
     (Axis          : access Gtk_Plot_Axis_Record;
      Font          : String;
      Height        : Gint;
      Angle         : Plot_Angle;
      Foreground    : Gdk.Color.Gdk_Color;
      Background    : Gdk.Color.Gdk_Color;
      Transparent   : Boolean;
      Justification : Gtk.Enums.Gtk_Justification);
   --  Set the attributes to be used for the title of the axis.
   --  Font is a postscript font name (as listed in the beginning of this
   --  package).

   procedure Axis_Set_Labels_Attributes
     (Axis          : access Gtk_Plot_Axis_Record;
      Font          : String;
      Height        : Gint;
      Angle         : Plot_Angle;
      Foreground    : Gdk.Color.Gdk_Color;
      Background    : Gdk.Color.Gdk_Color;
      Transparent   : Boolean;
      Justification : Gtk.Enums.Gtk_Justification);
   --  Set the attributes to be used for the ticks labels.

   procedure Axis_Set_Labels_Offset
     (Axis   : access Gtk_Plot_Axis_Record;
      Offset : Gint);
   --  Set the distance between the axis and its labels

   function Axis_Get_Labels_Offset
     (Axis : access Gtk_Plot_Axis_Record) return Gint;
   --  Get the distance between the axis and its labels.

   procedure Axis_Set_Labels_Style
     (Axis      : access Gtk_Plot_Axis_Record;
      Style     : Plot_Label_Style;
      Precision : Gint);
   --  Set the style of labels.
   --  This indicates whether the labels should be displayed as floating
   --  point values or in the scientific notation.
   --  Precision is the number of digits to be printed.

   procedure Axis_Use_Custom_Tick_Labels
     (Axis   : access Gtk_Plot_Axis_Record;
      Custom : Boolean := True);
   --  Indicate which kind of labels should be used for major ticks.
   --  If Custom is True, then the labels set by Axis_Set_Tick_Labels will
   --  be used.

   procedure Axis_Set_Labels_Suffix
     (Axis : access Gtk_Plot_Axis_Record;
      Text : String);
   --  Defines a suffix to add after each label on the axis

   procedure Axis_Set_Labels_Prefix
     (Axis : access Gtk_Plot_Axis_Record;
      Text : String);
   --  Defines a prefix to add before each label on the axis

   function Axis_Get_Labels_Suffix
     (Axis : access Gtk_Plot_Axis_Record) return String;
   --  Return the suffix added to each label.

   function Axis_Get_Labels_Prefix
     (Axis : access Gtk_Plot_Axis_Record) return String;
   --  Return the prefix added to each label.

   procedure Axis_Ticks_Recalc (Axis : access Gtk_Plot_Axis_Record);

   function Axis_Ticks_Transform
     (Axis : access Gtk_Plot_Axis_Record;
      Y    : Gdouble) return Gdouble;

   function Axis_Ticks_Inverse
     (Axis : access Gtk_Plot_Axis_Record;
      X    : Gdouble) return Gdouble;

   procedure Axis_Parse_Label
     (Axis      : access Gtk_Plot_Axis_Record;
      Val       : Gdouble;
      Precision : Gint;
      Style     : Gint;
      Label     : String);

   -----------
   -- Grids --
   -----------
   --  A grid can be displayed in the graph.
   --  This makes it easier to understand a graphics in some situations.
   --  The grid has two simultaneous line styles, each with its own specific
   --  step (minor and major steps).
   --
   --  There are two special lines in the grid, that you can display even if
   --  you don't display the rest of the line. These are the origin of the
   --  coordinates system, ie the lines at X=0 and Y=0.

   procedure X0_Set_Visible
     (Plot    : access Gtk_Plot_Record;
      Visible : Boolean);
   --  Indicate whether the line at X=0 should be displayed.

   function X0_Visible
     (Plot : access Gtk_Plot_Record) return Boolean;
   --  Return the visibility state of the line at X=0

   procedure Y0_Set_Visible
     (Plot    : access Gtk_Plot_Record;
      Visible : Boolean);
   --  Indicate whether the line at Y=0 should be displayed.

   function Y0_Visible
     (Plot   : access Gtk_Plot_Record) return Boolean;
   --  Return the visibility state of the line at Y=0

   procedure X0line_Set_Attributes
     (Plot  : access Gtk_Plot_Record;
      Style : Plot_Line_Style;
      Width : Gfloat;
      Color : Gdk.Color.Gdk_Color);
   --  Set the attributes of the line at X=0

   procedure Y0line_Set_Attributes
     (Plot  : access Gtk_Plot_Record;
      Style : Plot_Line_Style;
      Width : Gfloat;
      Color : Gdk.Color.Gdk_Color);
   --  Set the attributes of the line at Y=0

   procedure Grids_Set_On_Top
     (Plot : access Gtk_Plot_Record; On_Top : Boolean);
   --  Whether the grid should be displayed on top of the plots

   function Grids_On_Top (Plot : access Gtk_Plot_Record) return Boolean;
   --  Whether the gris is currently displayed on top of the plots

   procedure Grids_Set_Visible
     (Plot   : access Gtk_Plot_Record;
      Vmajor : Boolean;
      Vminor : Boolean;
      Hmajor : Boolean;
      Hminor : Boolean);
   --  Indicate whether the lines of the grids should be displayed.
   --  You can decide separately whether the major and minor lines should
   --  be displayed.

   procedure Grids_Visible
     (Plot   : access Gtk_Plot_Record;
      Vmajor : out Boolean;
      Vminor : out Boolean;
      Hmajor : out Boolean;
      Hminor : out Boolean);
   --  Return the visibility state of the grid.

   procedure Major_Hgrid_Set_Attributes
     (Plot  : access Gtk_Plot_Record;
      Style : Plot_Line_Style;
      Width : Gfloat;
      Color : Gdk.Color.Gdk_Color);
   --  Set the attributes for the major horizontal lines in the grid.

   procedure Major_Vgrid_Set_Attributes
     (Plot  : access Gtk_Plot_Record;
      Style : Plot_Line_Style;
      Width : Gfloat;
      Color : Gdk.Color.Gdk_Color);
   --  Set the attributes for the major vertical lines in the grid.

   procedure Minor_Hgrid_Set_Attributes
     (Plot  : access Gtk_Plot_Record;
      Style : Plot_Line_Style;
      Width : Gfloat;
      Color : Gdk.Color.Gdk_Color);
   --  Set the attributes for the minor horizontal lines in the grid.

   procedure Minor_Vgrid_Set_Attributes
     (Plot  : access Gtk_Plot_Record;
      Style : Plot_Line_Style;
      Width : Gfloat;
      Color : Gdk.Color.Gdk_Color);
   --  Set the attributes for the minor vertical lines in the grid.

   -------------
   -- Legends --
   -------------
   --  Each graph is associated with one legend, that is supposed to
   --  indicate what the plot represents.

   procedure Show_Legends (Plot : access Gtk_Plot_Record);
   --  Indicate that the legend should be displayed.

   procedure Hide_Legends (Plot : access Gtk_Plot_Record);
   --  Indicate that the legend should not be displayed.

   procedure Set_Legends_Border
     (Plot         : access Gtk_Plot_Record;
      Border       : Plot_Border_Style;
      Shadow_Width : Gint);
   --  Modify the way the borders of the legend look like.

   procedure Legends_Move
     (Plot : access Gtk_Plot_Record;
      X    : Gdouble;
      Y    : Gdouble);
   --  Move the legend relative to the widget's area.
   --  X and Y are percentage values. (0.0, 0.0) indicates the top-left
   --  corner of the plot, (1.0, 1.0) indicates the bottom-right corner.

   procedure Legends_Get_Position
     (Plot : access Gtk_Plot_Record;
      X    : out Gdouble;
      Y    : out Gdouble);
   --  Return the current position of the legend.

   function Legends_Get_Allocation
     (Plot   : access Gtk_Plot_Record) return Gtk.Widget.Gtk_Allocation;
   --  Return the exact coordinates and size in pixels of the legend.
   --  The coordinates are relative to the widget's parent container.

   procedure Legends_Set_Attributes
     (Plot       : access Gtk_Plot_Record;
      Ps_Font    : String;
      Height     : Gint;
      Foreground : Gdk.Color.Gdk_Color;
      Background : Gdk.Color.Gdk_Color);
   --  Set the attributes to use when displaying the legend.

   --------------
   -- Datasets --
   --------------
   --  A dataset is a set of points, either given explicitly by your
   --  application or calculated with a specific function, and that can be
   --  plotted on the screen.
   --  In Gtk_Plot, such a set is represented with symbols (special points in
   --  the graph, that can be manipulated interactively if you so wish), linked
   --  by connectors, which are either straight lines, splines, sets, ...
   --  Multiple data sets can of course be printed on a single graph.

   --  <doc_ignore>
   generic
      with function Func (Plot  : access Gtk_Plot_Record'Class;
                          Set   :        Gtk_Plot_Data;
                          X     :        Gdouble;
                          Error : access Boolean)
                         return Gdouble;
   function Generic_Plot_Function (Plot  : System.Address;
                                   Set   : Gtk_Plot_Data;
                                   X     : Gdouble;
                                   Error : access Gboolean)
                                  return Gdouble;
   --  Generic function that can be instantiated for Plot_Function below.
   --  </doc_ignore>

   --  <doc_ignore>
   generic
      with function Func (Plot  : access Gtk_Plot_Record'Class;
                          Set   :        Gtk_Plot_Data;
                          X     :        Gdouble;
                          Y     :        Gdouble;
                          Error : access Boolean)
                         return Gdouble;
   function Generic_Plot3D_Function (Plot  : System.Address;
                                     Set   : Gtk_Plot_Data;
                                     X     : Gdouble;
                                     Y     : Gdouble;
                                     Error : access Gboolean)
                                    return Gdouble;
   --  Generic function that can be instanciated for Plot3D_Function below.
   --  </doc_ignore>

   type Plot3D_Function is access function
     (Plot  : System.Address;
      Set   : Gtk_Plot_Data;
      X     : Gdouble;
      Y     : Gdouble;
      Error : access Gboolean) return Gdouble;
   --  Function used for plotting 3D graphs.
   --  It should return the value associated with (X, Y) in its graph, and set
   --  Error to True if there was an error while calculating the value.

   pragma Convention (C, Generic_Plot_Function);
   pragma Convention (C, Generic_Plot3D_Function);
   pragma Convention (C, Plot3D_Function);

   procedure Add_Data
     (Plot : access Gtk_Plot_Record;
      Data : access Gtk_Plot_Data_Record'Class);
   --  Add an existing set of data to the plot.
   --  This set will automatically be drawn the next time the Plot itself is
   --  drawn.

   function Remove_Data
     (Plot : access Gtk_Plot_Record;
      Data : access Gtk_Plot_Data_Record'Class)
      return Boolean;
   --  Remove the dataset from Plot.
   --  This function returns True if the dataset was indeed found and could be
   --  removed, False otherwise.

   function Add_Function
     (Plot   : access Gtk_Plot_Record;
      Func   : Plot_Function)
      return  Gtk_Plot_Data;
   --  Allocate a new dataset, whose point are automatically calculated.
   --  Func is a function that takes the X coordinate value, and should return
   --  the Y coordinate value.
   --  The newly allocated set should be freed by calling Free above.
   --  The set is automatically added to the plot, so you don't need to
   --  explicitly call Add_Dataset.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "changed"
   --    procedure Handler (Plot : access Gtk_Plot_Record'Class);
   --
   --    Called every time some property of the widget is changed, or the
   --    widget is moved or resized.
   --
   --  - "moved"
   --    function Handler (Plot : access Gtk_Plot_Record'Class;
   --                      X    : Gdouble;
   --                      Y    : Gdouble)
   --                     return Boolean;
   --
   --    Called when the widget has been moved relative to its drawable.
   --    Its new position is given in parameters.
   --
   --  - "resized"
   --    function Handler (Plot   : access Gtk_Plot_Record'Class;
   --                      Width  : Gdouble;
   --                      Height : Gdouble)
   --                     return Boolean;
   --
   --    Called when the widget has been resized relative to its drawable.
   --    Its new size is given in parameters.
   --
   --  - "tick_label"
   --    function Handler (Axis  : access Gtk_Plot_Axis_Record'Class;
   --                      Tick  : Gdouble_Access;
   --                      Label : Interfaces.C.Strings.chars_ptr)
   --                     return Boolean;
   --
   --     Called when a label should be drawn. You can modify the contents
   --     of Label (up to 100 characters) a
   --
   --  </signals>

private
   type Gtk_Plot_Record is new Gtk.Widget.Gtk_Widget_Record with null record;
   type Gtk_Plot_Axis_Record is new Glib.Object.GObject_Record with
     null record;

   Label_None   : constant Plot_Label_Pos := 0;
   Label_In     : constant Plot_Label_Pos := 1;
   Label_Out    : constant Plot_Label_Pos := 2;

   Ticks_None  : constant Plot_Ticks_Pos := 0;
   Ticks_In    : constant Plot_Ticks_Pos := 1;
   Ticks_Out   : constant Plot_Ticks_Pos := 2;

   pragma Import (C, Get_Type, "gtk_plot_get_type");
   pragma Import (C, Axis_Get_Type, "gtk_plot_axis_get_type");
   pragma Import (C, Text_Set_Border, "gtk_plot_text_set_border");
end Gtk.Extra.Plot;

--  Unbound:
--    gtk_plot_set_pc
--    gtk_plot_axis_set_tick_labels
--    gtk_plot_axis_ticks_autoscale
