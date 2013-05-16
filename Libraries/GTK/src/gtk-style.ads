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

--  <description>
--  This package contains various functions to draw widget parts on the screen.
--  Whenever possible, you should use these functions instead of directly
--  the ones from Gdk.Drawable, since this package will properly take into
--  account the user's theme and color choice.
--
--  Consider also using directly the function Gtk.Widget.Modify_Font,
--  Gtk.Widget.Modify_Background,... rather than use the lower level Gtk_Style
--  object.
--
--  See Gtk.RC to learn how styles can be defined in external configuration
--  files by the end-user of your application.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Configuration and Themes</group>

with Glib.Object;
with Glib.Values;
with Gdk.Color;        use Gdk.Color;
with Gdk.Font;         use Gdk.Font;
with Gdk.GC;
with Gdk.Pixmap;
with Gdk.Rectangle;
with Gdk.Types;
with Gdk.Window;
with Gtk.Enums;        use Gtk.Enums;
with Pango.Font;
with Pango.Layout;
with Interfaces.C.Strings;

package Gtk.Style is

   ------------
   -- Styles --
   ------------

   type Gtk_Style_Record is new Glib.Object.GObject_Record with null record;
   type Gtk_Style is access all Gtk_Style_Record'Class;

   Null_Style : constant Gtk_Style := null;

   type Gtk_Rc_Property_Parser is access function
     (Pspec     : Glib.Param_Spec;
      Rc_String : Interfaces.C.Strings.chars_ptr;
      Value     : access Glib.Values.GValue) return Gboolean;
   pragma Convention (C, Gtk_Rc_Property_Parser);
   --  A function used to parse a string into the value of a property.
   --  Pspec is the property that is being parsed, Rc_String is the value as
   --  read in a gtk configuration file (any string, really), and Value should
   --  be set to the value read.
   --  See also Gtk.Settings

   procedure Gtk_New (Style : out Gtk_Style);
   procedure Initialize (Style : access Gtk_Style_Record'Class);
   --  Creates or initializes a new style

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Style.

   function Copy (Source : Gtk_Style) return Gtk_Style;
   --  Copy a Gtk_Style

   function Attach
     (Style  : Gtk_Style; Window : Gdk.Window.Gdk_Window) return Gtk_Style;
   procedure Detach (Style : Gtk_Style);
   --  Attaches a style to a window; this process allocates the colors and
   --  creates the GC's for the style - it specializes it to a particular
   --  visual and colormap. The process may involve the creation of a new style
   --  if the style has already been attached to a window with a different
   --  style and colormap.
   --  It returns either Style or a newly allocated style. If a new one is
   --  created, the parameter will be Unref once, and the new one Ref once.

   procedure Set_Background
     (Style      : Gtk_Style;
      Window     : Gdk.Window.Gdk_Window;
      State_Type : Gtk_State_Type);
   --  Set the background color of Window to the background color specified
   --  by Style.

   procedure Apply_Default_Background
     (Style      : access Gtk_Style_Record;
      Window     : Gdk.Window.Gdk_Window;
      Set_Bg     : Boolean;
      State_Type : Gtk_State_Type;
      Area       : Gdk.Rectangle.Gdk_Rectangle;
      X          : Gint;
      Y          : Gint;
      Width      : Gint;
      Height     : Gint);
   --  Applies the default background from style to the given area in Window

   -----------------
   --  Properties --
   -----------------
   --  The style contains a number of properties. Each of these can be set to
   --  multiple values simulatenously, that will be applied depending on the
   --  widget's current state (highlighted, active, inactive,...)

   procedure Set_Background
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color);
   procedure Set_Bg
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color) renames Set_Background;
   function Get_Background
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.Color.Gdk_Color;
   function Get_Bg
     (Style      : Gtk_Style;
      State_Type : Gtk_State_Type) return Gdk_Color renames Get_Background;
   --  Set or get the background color that this style uses in the given state

   procedure Set_Background_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC);
   procedure Set_Background
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk_GC) renames Set_Background_GC;
   procedure Set_Bg
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC) renames Set_Background_GC;
   procedure Set_Bg_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC) renames Set_Background_GC;
   function Get_Background_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC;
   function Get_Bg
     (Style      : Gtk_Style;
      State_Type : Gtk_State_Type) return Gdk_GC renames Get_Background_GC;
   function Get_Background
     (Style      : Gtk_Style;
      State_Type : Gtk_State_Type) return Gdk_GC renames Get_Background_GC;
   function Get_Bg_GC
     (Style      : Gtk_Style;
      State_Type : Gtk_State_Type) return Gdk_GC renames Get_Background_GC;
   --  Set or get the graphic context that the style is using for the
   --  background

   procedure Set_Foreground
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color);
   procedure Set_Fg
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color) renames Set_Foreground;
   function Get_Foreground
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.Color.Gdk_Color;
   function Get_Fg
     (Style      : Gtk_Style;
      State_Type : Gtk_State_Type) return Gdk_Color renames Get_Foreground;
   --  Set or get the foreground color that the style is using

   procedure Set_Foreground_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC);
   procedure Set_Fg_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC) renames Set_Foreground_GC;
   procedure Set_Foreground
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC) renames Set_Foreground_GC;
   procedure Set_Fg
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC) renames Set_Foreground_GC;
   function Get_Foreground_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC;
   function Get_Foreground
     (Style      : Gtk_Style;
      State_Type : Gtk_State_Type) return Gdk_GC renames Get_Foreground_GC;
   function Get_Fg
     (Style      : Gtk_Style;
      State_Type : Gtk_State_Type) return Gdk_GC renames Get_Foreground_GC;
   function Get_Fg_GC
     (Style      : Gtk_Style;
      State_Type : Gtk_State_Type) return Gdk_GC renames Get_Foreground_GC;
   --  Set or get the graphic context used by this style for the foreground

   procedure Set_Light
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color);
   function Get_Light
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.Color.Gdk_Color;
   procedure Set_Light_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC);
   function Get_Light_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC;
   procedure Set_Light
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC) renames Set_Light_GC;
   function Get_Light
     (Style      : Gtk_Style;
      State_Type : Gtk_State_Type) return Gdk_GC renames Get_Light_GC;
   --  Set or get the lighter color or graphic context that this style is
   --  using. This color is used to draw the shadows around rectangles for
   --  instance

   procedure Set_Dark
     (Style      : Gtk_Style;
      State_Type : Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color);
   procedure Set_Dark_GC
     (Style      : Gtk_Style;
      State_Type : Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC);
   procedure Set_Dark
     (Style      : Gtk_Style;
      State_Type : Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC) renames Set_Dark_GC;
   function Get_Dark
     (Style      : Gtk_Style;
      State_Type : Gtk_State_Type) return Gdk.Color.Gdk_Color;
   function Get_Dark_GC
     (Style      : Gtk_Style;
      State_Type : Gtk_State_Type) return Gdk.GC.Gdk_GC;
   function Get_Dark
     (Style      : Gtk_Style;
      State_Type : Gtk_State_Type) return Gdk_GC renames Get_Dark_GC;
   --  Set or get the darker color or graphic context that this style is using.
   --  This color is used to draw the shadows around rectangles for instance.

   procedure Set_Middle
     (Style      : Gtk_Style;
      State_Type : Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color);
   procedure Set_Mid
     (Style      : Gtk_Style;
      State_Type : Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color) renames Set_Middle;
   function Get_Middle
     (Style      : Gtk_Style;
      State_Type : Gtk_State_Type) return Gdk.Color.Gdk_Color;
   function Get_Mid
     (Style      : Gtk_Style;
      State_Type : Gtk_State_Type) return Gdk_Color renames Get_Middle;
   procedure Set_Middle_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC);
   function Get_Middle_GC
     (Style      : Gtk_Style;
      State_Type : Gtk_State_Type) return Gdk_GC;
   procedure Set_Middle
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk_GC) renames Set_Middle_GC;
   function Get_Middle
     (Style      : Gtk_Style;
      State_Type : Gtk_State_Type) return Gdk_GC renames Get_Middle_GC;
   procedure Set_Mid_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk_GC) renames Set_Middle_GC;
   function Get_Mid_GC
     (Style      : Gtk_Style;
      State_Type : Gtk_State_Type) return Gdk_GC renames Get_Middle_GC;
   procedure Set_Mid
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC) renames Set_Middle_GC;
   function Get_Mid
     (Style      : Gtk_Style;
      State_Type : Gtk_State_Type) return Gdk_GC renames Get_Middle_GC;
   --  Set or get the middle color. This color should be between the light and
   --  dark colors set above.

   procedure Set_Text
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color);
   function Get_Text
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.Color.Gdk_Color;
   procedure Set_Text_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC);
   function Get_Text_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC;
   procedure Set_Text
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC) renames Set_Text_GC;
   function Get_Text
     (Style      : Gtk_Style;
      State_Type : Gtk_State_Type) return Gdk_GC renames Get_Text_GC;
   --  Set or get the color to use when drawing text.

   procedure Set_Base
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color);
   function Get_Base
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.Color.Gdk_Color;
   procedure Set_Base_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC);
   function Get_Base_GC
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.GC.Gdk_GC;
   procedure Set_Base
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      GC         : Gdk.GC.Gdk_GC) renames Set_Base_GC;
   function Get_Base
     (Style      : Gtk_Style;
      State_Type : Gtk_State_Type) return Gdk_GC renames Get_Base_GC;
   --  Set or get the base color

   procedure Set_Black
     (Style : Gtk_Style;
      Color : Gdk.Color.Gdk_Color);
   function Get_Black (Style : Gtk_Style) return Gdk.Color.Gdk_Color;
   procedure Set_Black_GC (Style : Gtk_Style; GC : Gdk.GC.Gdk_GC);
   function Get_Black_GC (Style : Gtk_Style) return Gdk.GC.Gdk_GC;
   procedure Set_Black
     (Style : Gtk_Style; GC : Gdk.GC.Gdk_GC) renames Set_Black_GC;
   function Get_Black
     (Style : Gtk_Style) return Gdk.GC.Gdk_GC renames Get_Black_GC;
   --  Set or get the "black" color. It isn't necessarily black, although most
   --  themes will want to use black here.

   procedure Set_White (Style : Gtk_Style; Color : Gdk.Color.Gdk_Color);
   function Get_White (Style : Gtk_Style) return Gdk.Color.Gdk_Color;
   procedure Set_White_GC (Style : Gtk_Style; GC : Gdk.GC.Gdk_GC);
   function Get_White_GC (Style : Gtk_Style) return Gdk.GC.Gdk_GC;
   procedure Set_White
     (Style : Gtk_Style; GC : Gdk.GC.Gdk_GC) renames Set_White_GC;
   function Get_White
     (Style : Gtk_Style) return Gdk.GC.Gdk_GC renames Get_White_GC;
   --  Set or get the "white" color. It isn't necessarily white, although most
   --  themes will want to use white here.

   procedure Set_Font_Description
     (Style : Gtk_Style; Desc : Pango.Font.Pango_Font_Description);
   function Get_Font_Description
     (Style : Gtk_Style) return Pango.Font.Pango_Font_Description;
   --  Set or get the font to use for this style

   --  <doc_ignore>
   function Get_Font (Style : Gtk_Style) return Gdk.Font.Gdk_Font;
   pragma Obsolescent ("Use Get_Font_Description");  --  Get_Font
   --  </doc_ignore>

   procedure Set_Bg_Pixmap
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type;
      Pixmap     : Gdk.Pixmap.Gdk_Pixmap);
   function Get_Bg_Pixmap
     (Style      : Gtk_Style;
      State_Type : Enums.Gtk_State_Type) return Gdk.Pixmap.Gdk_Pixmap;
   --  Set or get the pixmap to use for background

   function X_Thickness (Style : Gtk_Style) return Gint;
   --  Width of the vertical scrollbars and ranges when Style is applied.
   --  In fact, this thickness is used for a lot of widgets whose width
   --  does not depend on their content, such as rulers,...

   function Y_Thickness (Style : Gtk_Style) return Gint;
   --  Height of the horizontal scrollbars and ranges when Style is applied.

   --------------
   -- Painting --
   --------------
   --  All the subprograms below have similar profiles.
   --  Area is always a clipping area. Drawing only takes place within that
   --  area, and pixels outside of it are not affected.
   --  Detail is a theme-specific detail string. This is generally provided by
   --  the application (or rather gtk+ itself) to indicate that the drawing
   --  should be slightly different, and Detail describes the exact contact.
   --  All drawings are done on Window. Widget is used to draw specific
   --  things depending on the widget type.

   procedure Draw_Insertion_Cursor
     (Widget     : access Glib.Object.GObject_Record'Class;
      Drawable   : Gdk_Drawable;
      Area       : Gdk.Rectangle.Gdk_Rectangle;
      Location   : Gdk.Rectangle.Gdk_Rectangle;
      Is_Primary : Boolean;
      Direction  : Gtk_Text_Direction;
      Draw_Arrow : Boolean);
   --  Draws a text caret on Drawable at Location. This is not a style function
   --  but merely a convenience function for drawing the standard cursor shape.
   --  Is_Primary indicates whether the cursor should be the primary cursor
   --  color. Direction is the text direction. Draw_Arrow should be true to
   --  draw a directional arrow on the cursor. Should be False unless the
   --  cursor is split.

   procedure Paint_Handle
     (Style               : Gtk_Style;
      Window              : Gdk.Gdk_Window;
      State_Type          : Gtk.Enums.Gtk_State_Type;
      Shadow_Type         : Gtk.Enums.Gtk_Shadow_Type;
      Area                : Gdk.Rectangle.Gdk_Rectangle;
      Widget              : access Glib.Object.GObject_Record'Class;
      Detail              : String := "paned";
      X, Y, Width, Height : Gint;
      Orientation         : Gtk.Enums.Gtk_Orientation);
   --  Paint the handles as is done in the Gtk_Paned widget (ie a series of
   --  small dots in Window, that indicate that Window can be manipulated and
   --  resized.
   --  If Detail is "paned", only a few dots are painted in the middle of
   --  window (aligned either horizontally or vertically depending on
   --  Orientation). Any other value for Detail draws points on the whole
   --  length of Window.
   --  (X, Y, Width, Height) is the area in which the dots should be painted.
   --  For the whole window, use (0, 0, -1, -1).
   --  Only the area that intersect Area is drawn.

   procedure Paint_Arrow
     (Style       : access Gtk_Style_Record;
      Window      : Gdk_Window;
      State_Type  : Gtk_State_Type;
      Shadow_Type : Gtk_Shadow_Type;
      Area        : Gdk.Rectangle.Gdk_Rectangle := Gdk.Rectangle.Full_Area;
      Widget      : access Glib.Object.GObject_Record'Class;
      Detail      : String := "";
      Arrow_Type  : Gtk_Arrow_Type;
      Fill        : Boolean;
      X           : Gint;
      Y           : Gint;
      Width       : Gint;
      Height      : Gint);
   --  Draws an arrow in the given rectangle on Window using the given
   --  parameters. Arrow_Type determines the direction of the arrow.
   --  The default theme engine only recognazied "menu_scroll_arrow_up" for
   --  Detail.

   procedure Paint_Box
     (Style       : access Gtk_Style_Record;
      Window      : Gdk_Window;
      State_Type  : Gtk_State_Type;
      Shadow_Type : Gtk_Shadow_Type;
      Area        : Gdk.Rectangle.Gdk_Rectangle := Gdk.Rectangle.Full_Area;
      Widget      : access Glib.Object.GObject_Record'Class;
      Detail      : String := "";
      X           : Gint;
      Y           : Gint;
      Width       : Gint;
      Height      : Gint);
   --  Draws a box on Window with the given parameters.
   --  The default theme engine recognizes the following for Detail:
   --  "spinbutton_up", "spinbutton_down", "paned", "optionmenu"

   procedure Paint_Box_Gap
     (Style       : access Gtk_Style_Record;
      Window      : Gdk_Window;
      State_Type  : Gtk_State_Type;
      Shadow_Type : Gtk_Shadow_Type;
      Area        : Gdk.Rectangle.Gdk_Rectangle := Gdk.Rectangle.Full_Area;
      Widget      : access Glib.Object.GObject_Record'Class;
      Detail      : String := "";
      X           : Gint;
      Y           : Gint;
      Width       : Gint;
      Height      : Gint;
      Gap_Side    : Gtk_Position_Type;
      Gap_X       : Gint;
      Gap_Width   : Gint);
   --  Draws a box in Window using the given style and state and shadow type,
   --  leaving a gap in one side.

   procedure Paint_Check
     (Style       : access Gtk_Style_Record;
      Window      : Gdk_Window;
      State_Type  : Gtk_State_Type;
      Shadow_Type : Gtk_Shadow_Type;
      Area        : Gdk.Rectangle.Gdk_Rectangle := Gdk.Rectangle.Full_Area;
      Widget      : access Glib.Object.GObject_Record'Class;
      Detail      : String := "";
      X           : Gint;
      Y           : Gint;
      Width       : Gint;
      Height      : Gint);
   --  Draws a check button indicator in the given rectangle on Window with
   --  the given parameters. The default theme handles the following values for
   --  detail: "cellcheck", "check"

   procedure Paint_Diamond
     (Style       : access Gtk_Style_Record;
      Window      : Gdk_Window;
      State_Type  : Gtk_State_Type;
      Shadow_Type : Gtk_Shadow_Type;
      Area        : Gdk.Rectangle.Gdk_Rectangle := Gdk.Rectangle.Full_Area;
      Widget      : access Glib.Object.GObject_Record'Class;
      Detail      : String := "";
      X           : Gint;
      Y           : Gint;
      Width       : Gint;
      Height      : Gint);
   --  Draws a diamond in the given rectangle on Window using the given
   --  parameters.

   procedure Paint_Expander
     (Style          : access Gtk_Style_Record;
      Window         : Gdk_Window;
      State_Type     : Gtk_State_Type;
      Area           : Gdk.Rectangle.Gdk_Rectangle := Gdk.Rectangle.Full_Area;
      Widget         : access Glib.Object.GObject_Record'Class;
      Detail         : String := "";
      X              : Gint;
      Y              : Gint;
      Expander_Style : Gtk_Expander_Style);
   --  Draws an expander as used in Gtk_Tree_View. X and Y specify the center
   --  the expander. The size of the expander is determined by the
   --  "expander-size" style property of Widget. (If widget is not specified or
   --  doesn't have an "expander-size" property, an unspecified default size
   --  will be used, since the caller doesn't have sufficient information to
   --  position the expander, this is likely not useful.) The expander is
   --  expander_size pixels tall in the collapsed position and expander_size
   --  pixels wide in the expanded position.

   procedure Paint_Extension
     (Style       : access Gtk_Style_Record;
      Window      : Gdk_Window;
      State_Type  : Gtk_State_Type;
      Shadow_Type : Gtk_Shadow_Type;
      Area        : Gdk.Rectangle.Gdk_Rectangle := Gdk.Rectangle.Full_Area;
      Widget      : access Glib.Object.GObject_Record'Class;
      Detail      : String := "";
      X           : Gint;
      Y           : Gint;
      Width       : Gint;
      Height      : Gint;
      Gap_Side    : Gtk_Position_Type);
   --  Draws an extension, i.e. a notebook tab.

   procedure Paint_Flat_Box
     (Style       : access Gtk_Style_Record;
      Window      : Gdk_Window;
      State_Type  : Gtk_State_Type;
      Shadow_Type : Gtk_Shadow_Type;
      Area        : Gdk.Rectangle.Gdk_Rectangle := Gdk.Rectangle.Full_Area;
      Widget      : access Glib.Object.GObject_Record'Class;
      Detail      : String := "";
      X           : Gint;
      Y           : Gint;
      Width       : Gint;
      Height      : Gint);
   --  Draws a flat box on Window with the given parameters.

   procedure Paint_Focus
     (Style      : access Gtk_Style_Record;
      Window     : Gdk_Window;
      State_Type : Gtk_State_Type;
      Area       : Gdk.Rectangle.Gdk_Rectangle := Gdk.Rectangle.Full_Area;
      Widget     : access Glib.Object.GObject_Record'Class;
      Detail     : String := "";
      X          : Gint;
      Y          : Gint;
      Width      : Gint;
      Height     : Gint);
   --  Draws a focus indicator around the given rectangle on Window using the
   --  given style.

   procedure Paint_Hline
     (Style      : access Gtk_Style_Record;
      Window     : Gdk_Window;
      State_Type : Gtk_State_Type;
      Area       : Gdk.Rectangle.Gdk_Rectangle := Gdk.Rectangle.Full_Area;
      Widget     : access Glib.Object.GObject_Record'Class;
      Detail     : String := "";
      X1         : Gint;
      X2         : Gint;
      Y          : Gint);
   --  Draws a horizontal line from (X1, Y) to (X2, Y) in Window
   --  using the given style and state.

   procedure Paint_Layout
     (Style      : access Gtk_Style_Record;
      Window     : Gdk_Window;
      State_Type : Gtk_State_Type;
      Use_Text   : Boolean;
      Area       : Gdk.Rectangle.Gdk_Rectangle := Gdk.Rectangle.Full_Area;
      Widget     : access Glib.Object.GObject_Record'Class;
      Detail     : String := "";
      X          : Gint;
      Y          : Gint;
      Layout     : Pango.Layout.Pango_Layout);
   --  Draws a layout on Window using the given parameters.

   procedure Paint_Option
     (Style       : access Gtk_Style_Record;
      Window      : Gdk_Window;
      State_Type  : Gtk_State_Type;
      Shadow_Type : Gtk_Shadow_Type;
      Area        : Gdk.Rectangle.Gdk_Rectangle := Gdk.Rectangle.Full_Area;
      Widget      : access Glib.Object.GObject_Record'Class;
      Detail      : String := "";
      X           : Gint;
      Y           : Gint;
      Width       : Gint;
      Height      : Gint);
   --  Draws a radio button indicator in the given rectangle on Window with
   --  the given parameters.

   procedure Paint_Polygon
     (Style       : access Gtk_Style_Record;
      Window      : Gdk_Window;
      State_Type  : Gtk_State_Type;
      Shadow_Type : Gtk_Shadow_Type;
      Area        : Gdk.Rectangle.Gdk_Rectangle := Gdk.Rectangle.Full_Area;
      Widget      : access Glib.Object.GObject_Record'Class;
      Detail      : String := "";
      Points      : Gdk.Types.Gdk_Points_Array;
      Fill        : Boolean);
   --  Draws a polygon on Window with the given parameters.

   procedure Paint_Resize_Grip
     (Style      : access Gtk_Style_Record;
      Window     : Gdk_Window;
      State_Type : Gtk_State_Type;
      Area       : Gdk.Rectangle.Gdk_Rectangle := Gdk.Rectangle.Full_Area;
      Widget     : access Glib.Object.GObject_Record'Class;
      Detail     : String := "";
      Edge       : Gdk.Window.Gdk_Window_Edge;
      X          : Gint;
      Y          : Gint;
      Width      : Gint;
      Height     : Gint);
   --  Draws a resize grip in the given rectangle on Window using the given
   --  parameters.

   procedure Paint_Shadow
     (Style       : access Gtk_Style_Record;
      Window      : Gdk_Window;
      State_Type  : Gtk_State_Type;
      Shadow_Type : Gtk_Shadow_Type;
      Area        : Gdk.Rectangle.Gdk_Rectangle := Gdk.Rectangle.Full_Area;
      Widget      : access Glib.Object.GObject_Record'Class;
      Detail      : String := "";
      X           : Gint;
      Y           : Gint;
      Width       : Gint;
      Height      : Gint);
   --  Draws a shadow around the given rectangle in Window using the given
   --  style and state and shadow type.

   procedure Paint_Shadow_Gap
     (Style       : access Gtk_Style_Record;
      Window      : Gdk_Window;
      State_Type  : Gtk_State_Type;
      Shadow_Type : Gtk_Shadow_Type;
      Area        : Gdk.Rectangle.Gdk_Rectangle := Gdk.Rectangle.Full_Area;
      Widget      : access Glib.Object.GObject_Record'Class;
      Detail      : String := "";
      X           : Gint;
      Y           : Gint;
      Width       : Gint;
      Height      : Gint;
      Gap_Side    : Gtk_Position_Type;
      Gap_X       : Gint;
      Gap_Width   : Gint);
   --  Draws a shadow around the given rectangle in Window
   --  using the given style and state and shadow type, leaving a
   --  gap in one side.

   procedure Paint_Slider
     (Style       : access Gtk_Style_Record;
      Window      : Gdk_Window;
      State_Type  : Gtk_State_Type;
      Shadow_Type : Gtk_Shadow_Type;
      Area        : Gdk.Rectangle.Gdk_Rectangle := Gdk.Rectangle.Full_Area;
      Widget      : access Glib.Object.GObject_Record'Class;
      Detail      : String := "";
      X           : Gint;
      Y           : Gint;
      Width       : Gint;
      Height      : Gint;
      Orientation : Gtk_Orientation);
   --  Draws a slider in the given rectangle on Window using the given style
   --  and orientation.

   procedure Paint_Tab
     (Style       : access Gtk_Style_Record;
      Window      : Gdk_Window;
      State_Type  : Gtk_State_Type;
      Shadow_Type : Gtk_Shadow_Type;
      Area        : Gdk.Rectangle.Gdk_Rectangle := Gdk.Rectangle.Full_Area;
      Widget      : access Glib.Object.GObject_Record'Class;
      Detail      : String := "";
      X           : Gint;
      Y           : Gint;
      Width       : Gint;
      Height      : Gint);
   --  Draws an option menu tab (i.e. the up and down pointing arrows) in the
   --  given rectangle on Window using the given parameters.

   procedure Paint_Vline
     (Style      : access Gtk_Style_Record;
      Window     : Gdk_Window;
      State_Type : Gtk_State_Type;
      Area       : Gdk.Rectangle.Gdk_Rectangle := Gdk.Rectangle.Full_Area;
      Widget     : access Glib.Object.GObject_Record'Class;
      Detail     : String := "";
      Y1         : Gint;
      Y2         : Gint;
      X          : Gint);
   --  Draws a vertical line from (X, Y1) to (X, Y2) in Window
   --  using the given style and state.

   -------------
   -- Drawing --
   -------------
   --  All these procedures are obsolescent, and have been replaced by the
   --  Paint_* versions below.
   --  <doc_ignore>

   procedure Draw_Polygon
     (Style       : Gtk_Style;
      Window      : Gdk.Window.Gdk_Window;
      State_Type  : Enums.Gtk_State_Type;
      Shadow_Type : Enums.Gtk_Shadow_Type;
      Points      : Gdk.Types.Gdk_Points_Array;
      Fill        : Boolean);
   pragma Obsolescent ("Use Paint_Polygon");  --  Draw_Polygon
   --  Draws a polygon in Window

   procedure Draw_Arrow
     (Style       : Gtk_Style;
      Window      : Gdk.Window.Gdk_Window;
      State_Type  : Enums.Gtk_State_Type;
      Shadow_Type : Enums.Gtk_Shadow_Type;
      Arrow_Type  : Enums.Gtk_Arrow_Type;
      Fill        : Boolean;
      X, Y        : Gint;
      Width       : Gint;
      Height      : Gint);
   pragma Obsolescent ("Use Pain_Arrow");  --  Draw_Arrow
   --  Draws an arrow in Window, within the given rectangle

   procedure Draw_Shadow
     (Style       : access Gtk_Style_Record;
      Window      : Gdk_Window;
      State_Type  : Gtk_State_Type;
      Shadow_Type : Gtk_Shadow_Type;
      X           : Gint;
      Y           : Gint;
      Width       : Gint;
      Height      : Gint);
   pragma Obsolescent;  --  Draw_Shadow
   --  Draws a shadow around the given rectangle in @window
   --  using the given style and state and shadow type.

   procedure Draw_String
     (Style       : Gtk_Style;
      Window      : Gdk.Window.Gdk_Window;
      State_Type  : Enums.Gtk_State_Type;
      X, Y        : Gint;
      Str         : UTF8_String);
   pragma Obsolescent ("Use Paint_String");  --  Draw_String
   --  Draws a text on Window

   --  </doc_ignore>

   -------------
   -- Borders --
   -------------

   type Gtk_Border_Record is record
      Left   : Gint;
      Right  : Gint;
      Top    : Gint;
      Bottom : Gint;
   end record;
   pragma Convention (C, Gtk_Border_Record);
   type Gtk_Border is access all Gtk_Border_Record;

   function Border_Copy (Border : access Gtk_Border_Record) return Gtk_Border;
   --  Copies a Gtk_Border structure.

   procedure Border_Free (Border : access Gtk_Border_Record);
   --  Frees a Gtk_Border structure.

   function Border_Get_Type return GType;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "realize"
   --    procedure Handler (Style : access Gtk_Style_Record'Class);
   --    Emitted when the style has been initialized for a particular colormap
   --    and depth. Connecting to this signal is probably seldom useful since
   --    most of the time applications and widgets only deal with styles that
   --    have been already realized.
   --
   --  - "unrealize"
   --    procedure Handler (Style : access Gtk_Style_Record'Class);
   --    Emitted when the aspects of the style specific to a particular
   --    colormap and depth are being cleaned up. A connection to this signal
   --    can be useful if a widget wants to cache objects like a Gdk_GC as
   --    object data on Gtk_Style. This signal provides a convenient place to
   --    free such cached objects.
   --
   --  </signals>

   Signal_Realize   : constant Glib.Signal_Name := "realize";
   Signal_Unrealize : constant Glib.Signal_Name := "unrealize";

private
   pragma Import (C, Get_Type, "gtk_style_get_type");
end Gtk.Style;

--  The following subprograms never had a binding, and are now obsolescent:
--  No binding: gtk_draw_box
--  No binding: gtk_draw_box_gap
--  No binding: gtk_draw_check
--  No binding: gtk_draw_diamond
--  No binding: gtk_draw_expander
--  No binding: gtk_draw_extension
--  No binding: gtk_draw_flat_box
--  No binding: gtk_draw_focus
--  No binding: gtk_draw_handle
--  No binding: gtk_draw_hline
--  No binding: gtk_draw_layout
--  No binding: gtk_draw_option
--  No binding: gtk_draw_resize_grip
--  No binding: gtk_draw_shadow_gap
--  No binding: gtk_draw_slider
--  No binding: gtk_draw_tab
--  No binding: gtk_draw_vline
--  No binding: gtk_paint_string
--  No binding: gtk_style_set_font

--  These are obsolescent, and inherited anyway
--  No binding: gtk_style_ref
--  No binding: gtk_style_unref

--  Binding is in gtk-icon_factory.ads
--  No binding: gtk_style_lookup_icon_set
--  No binding: gtk_style_render_icon
