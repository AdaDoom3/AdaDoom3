-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2007 AdaCore                    --
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
--  A Gtk_Color_Selection widget is a complex dialog that allows the user
--  to select a color based either on its (Red, Green, Blue) or its
--  (Hue, Saturation, Value).
--  An additional field is provided to select the opacity of the color (this
--  is usually called the alpha channel).
--
--  See Gtk.Color_Selection_Dialog for a version of this widget that comes with
--  its own dialog.
--
--  See Gtk.Extra.Color_Combo for a different way to select colors.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Drawing</group>
--  <testgtk>create_color_selection.adb</testgtk>
--  <screenshot>gtk-colorsel</screenshot>

with Glib.Properties;
with Gdk.Color;
with Gtk.Enums;
with Gtk.Box;

package Gtk.Color_Selection is

   type Gtk_Color_Selection_Record is new Gtk.Box.Gtk_Box_Record with private;
   type Gtk_Color_Selection is access all Gtk_Color_Selection_Record'Class;

   type Color_Index is (Red, Green, Blue, Opacity);
   --  Used as an index to the table used to set and get the currently
   --  selected color.

   type Color_Array is array (Color_Index) of Gdouble;
   --  Array that indicates the currently selected color.
   --  All the values are between 0.0 and 1.0 (a percentage value).
   --  They should be converted to absolute values before using them to create
   --  a new color, with the following piece of code:
   --    Absolute := To_Absolute (Color_Array (Index))

   procedure Gtk_New (Widget : out Gtk_Color_Selection);
   procedure Initialize (Widget : access Gtk_Color_Selection_Record'Class);
   --  Creates or initiailizes a new color selection widget.

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Gtk_Color_Selection.

   procedure Set_Current_Color
     (Colorsel : access Gtk_Color_Selection_Record;
      Color    : Gdk.Color.Gdk_Color);
   procedure Get_Current_Color
     (Colorsel : access Gtk_Color_Selection_Record;
      Color    : out Gdk.Color.Gdk_Color);
   --  Set the current color of the Colorsel. When called for the first time,
   --  the original color will the set to Color as well.

   procedure Set_Previous_Color
     (Colorsel : access Gtk_Color_Selection_Record;
      Color    : Gdk.Color.Gdk_Color);
   procedure Get_Previous_Color
     (Colorsel : access Gtk_Color_Selection_Record;
      Color    : out Gdk.Color.Gdk_Color);
   --  Set the previous color. This procedure should not be called without
   --  analysis, as it might seem confusing to see that color change.
   --  Calling Set_Current_Color for the first time will also set this
   --  color.

   function Is_Adjusting
      (Colorsel : access Gtk_Color_Selection_Record) return Boolean;
   --  Get the current state of the Colorsel.
   --  Return TRue if the user is currently dragging a color around, False if
   --  the selection has stopped.

   function To_Absolute (Color : Gdouble) return Gushort;
   --  Convert from a percentage value as returned by Get_Color to an
   --  absolute value as can be used with Gdk_Color.

   function To_Percent (Color : Gushort) return Gdouble;
   --  Convert from an absolute value as used in Gdk_Color to a percentage
   --  value as used in Set_Color.

   -------------
   -- Opacity --
   -------------
   --  The color selection widget allows you optionally to select the opacity
   --  of the color

   procedure Set_Has_Opacity_Control
     (Colorsel    : access Gtk_Color_Selection_Record;
      Has_Opacity : Boolean);
   function Get_Has_Opacity_Control
     (Colorsel : access Gtk_Color_Selection_Record) return Boolean;
   --  Set the Colorsel to use or not use opacity. An additional field is
   --  displayed to select the opacity if needed.

   procedure Set_Previous_Alpha
     (Colorsel : access Gtk_Color_Selection_Record;
      Alpha    : Guint16);
   function Get_Previous_Alpha
     (Colorsel : access Gtk_Color_Selection_Record) return Guint16;
   --  Set the previous opacity to Alpha. This procedure should not be called
   --  without analysis, as it might seem confusing to see that value change.

   procedure Set_Current_Alpha
     (Colorsel : access Gtk_Color_Selection_Record;
      Alpha    : Guint16);
   function Get_Current_Alpha
     (Colorsel : access Gtk_Color_Selection_Record) return Guint16;
   --  Set the current opacity to be Alpha. When called for the first time,
   --  the original opacity will be set too.

   -------------
   -- Palette --
   -------------
   --  The color selection widget can optionally display a palette, which the
   --  user can change dynamically. This palette helps selecting colors for the
   --  user, who can chose faster among a limited set of colors.

   procedure Set_Has_Palette
     (Colorsel    : access Gtk_Color_Selection_Record;
      Has_Palette : Boolean);
   function Get_Has_Palette
     (Colorsel : access Gtk_Color_Selection_Record) return Boolean;
   --  If Has_Palette is True, then set the Colorsel to show the palette.
   --  Hide the palette otherwise.

   function Palette_From_String
     (Str : String) return Gdk.Color.Gdk_Color_Array;
   --  Parses a color palette string; this string is a colon-separated list of
   --  color names readable by Gdk.Color.Parse.
   --  An empty array is returned if Str couldn't be parsed

   function Palette_To_String
     (Colors   : Gdk.Color.Gdk_Color_Array) return String;
   --  Encodes a palette as a string, useful for persistent storage.

   type Gtk_Color_Selection_Change_Palette_With_Screen_Func is access
     procedure (Screen : Gdk.Gdk_Screen;
                Colors : Gdk.Color.Gdk_Color_Array);
   --  This function should save the new palette contents, and update the
   --  Gtk_Settings property "gtk-color-palette" so all Gtk_Color_Selection
   --  widgets will be modified, including the current one. For instance, you
   --  would do:
   --    Set_String_Property
   --      (Get_Default, Gtk_Color_Palette, Palette_To_String (Colors), "Foo");

   function Set_Change_Palette_With_Screen_Hook
     (Func : Gtk_Color_Selection_Change_Palette_With_Screen_Func)
      return Gtk_Color_Selection_Change_Palette_With_Screen_Func;
   --  Installs a global function to be called whenever the user tries to
   --  modify the palette in a color selection.
   --  Return value: the previous change palette hook (that was replaced).

   -----------------
   -- Obsolescent --
   -----------------
   --  All subprograms below are now obsolescent in gtk+. They might be removed
   --  from future versions of gtk+ (and therefore GtkAda).
   --  To find out whether your code uses any of these, we recommend compiling
   --  with the -gnatwj switch
   --  <doc_ignore>

   procedure Set_Update_Policy
     (Colorsel : access Gtk_Color_Selection_Record;
      Policy   : Enums.Gtk_Update_Type);
   pragma Obsolescent;  --  Set_Update_Policy
   --  Set the behavior of the scales used to select a value (red, green,...)
   --  Set Policy to Update_Continuous if you want to update the color
   --  continuously as the slider is mode, Update_Discontinuous to update the
   --  color only when the mouse is released and Update_Delayed to update when
   --  the mouse is released or has been motionless for a while.

   procedure Set_Color
     (Colorsel : access Gtk_Color_Selection_Record;
      Color    : Color_Array);
   pragma Obsolescent ("Use Set_Current_Color");  --  Set_Color
   --  Modify the current color.
   --  Note that Color is an array of percentages, between 0.0 and 1.0, not
   --  absolute values.

   procedure Get_Color
     (Colorsel : access Gtk_Color_Selection_Record;
      Color    : out Color_Array);
   pragma Obsolescent ("Use Get_Current_Color");  --  Get_Color
   --  Get the current color.
   --  Note that Color is an array of percentages, between 0.0 and 1.0, not
   --  absolute values.

   --  </doc_ignore>

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Current_Alpha_Property
   --  Type:  Uint
   --  Descr: The current opacity value (0 fully transparent, 65535 fully
   --         opaque)
   --
   --  Name:  Current_Color_Property
   --  Type:  Boxed
   --  Descr: The current color
   --
   --  Name:  Has_Opacity_Control_Property
   --  Type:  Boolean
   --  Descr: Whether the color selector should allow setting opacity
   --
   --  Name:  Has_Palette_Property
   --  Type:  Boolean
   --  Descr: Whether a palette should be used
   --
   --  </properties>

   Current_Alpha_Property       : constant Glib.Properties.Property_Uint;
   --  Current_Color_Property       : constant Glib.Properties.Property_Boxed;
   Has_Opacity_Control_Property : constant Glib.Properties.Property_Boolean;
   Has_Palette_Property         : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "color_changed"
   --    procedure Handler
   --       (Selection : access Gtk_Color_Selection_Record'Class);
   --    Called every time a new color is selected in the dialog
   --  </signals>

   Signal_Color_Changed : constant Glib.Signal_Name := "color_changed";

private
   type Gtk_Color_Selection_Record is new Gtk.Box.Gtk_Box_Record
     with null record;

   Current_Alpha_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("current-alpha");
   --  Current_Color_Property : constant Glib.Properties.Property_Boxed :=
   --    Glib.Properties.Build ("current-color");
   Has_Opacity_Control_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-opacity-control");
   Has_Palette_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-palette");

   pragma Import (C, Get_Type, "gtk_color_selection_get_type");
end Gtk.Color_Selection;

--  These subprograms never had a binding and are now obsolescent:
--  No binding: gtk_color_selection_set_change_palette_hook
