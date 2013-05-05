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
--  This widget provides a low level graphical representation of a range of
--  values. It is used by other widgets such as Gtk_Scale and Gtk_Scrollbar.
--
--  </description>
--  <screenshot>gtk-range</screenshot>
--  <testgtk>create_range.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Rectangle;   use Gdk.Rectangle;
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Adjustment;  use Gtk.Adjustment;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Orientable;  use Gtk.Orientable;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.GRange is

   type Gtk_Range_Record is new Gtk_Widget_Record with null record;
   type Gtk_Range is access all Gtk_Range_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_range_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Adjustment
      (The_Range : access Gtk_Range_Record)
       return Gtk.Adjustment.Gtk_Adjustment;
   procedure Set_Adjustment
      (The_Range  : access Gtk_Range_Record;
       Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --  Sets the adjustment to be used as the "model" object for this range
   --  widget. The adjustment indicates the current range value, the minimum
   --  and maximum range values, the step/page increments used for keybindings
   --  and scrolling, and the page size. The page size is normally 0 for
   --  Gtk.Scale.Gtk_Scale and nonzero for Gtk.Scrollbar.Gtk_Scrollbar, and
   --  indicates the size of the visible area of the widget being scrolled. The
   --  page size affects the size of the scrollbar slider.
   --  "adjustment": a Gtk.Adjustment.Gtk_Adjustment

   function Get_Fill_Level
      (The_Range : access Gtk_Range_Record) return Gdouble;
   procedure Set_Fill_Level
      (The_Range  : access Gtk_Range_Record;
       Fill_Level : Gdouble);
   --  Set the new position of the fill level indicator. The "fill level" is
   --  probably best described by its most prominent use case, which is an
   --  indicator for the amount of pre-buffering in a streaming media player.
   --  In that use case, the value of the range would indicate the current play
   --  position, and the fill level would be the position up to which the
   --  file/stream has been downloaded. This amount of prebuffering can be
   --  displayed on the range's trough and is themeable separately from the
   --  trough. To enable fill level display, use
   --  Gtk.GRange.Set_Show_Fill_Level. The range defaults to not showing the
   --  fill level. Additionally, it's possible to restrict the range's slider
   --  position to values which are smaller than the fill level. This is
   --  controller by Gtk.GRange.Set_Restrict_To_Fill_Level and is by default
   --  enabled.
   --  Since: gtk+ 2.12
   --  "fill_level": the new position of the fill level indicator

   function Get_Flippable
      (The_Range : access Gtk_Range_Record) return Boolean;
   procedure Set_Flippable
      (The_Range : access Gtk_Range_Record;
       Flippable : Boolean);
   --  If a range is flippable, it will switch its direction if it is
   --  horizontal and its direction is %GTK_TEXT_DIR_RTL. See
   --  Gtk.Widget.Get_Direction.
   --  Since: gtk+ 2.18
   --  "flippable": True to make the range flippable

   function Get_Inverted
      (The_Range : access Gtk_Range_Record) return Boolean;
   procedure Set_Inverted
      (The_Range : access Gtk_Range_Record;
       Setting   : Boolean);
   --  Ranges normally move from lower to higher values as the slider moves
   --  from top to bottom or left to right. Inverted ranges have higher values
   --  at the top or on the right rather than on the bottom or left.
   --  "setting": True to invert the range

   function Get_Lower_Stepper_Sensitivity
      (The_Range : access Gtk_Range_Record)
       return Gtk.Enums.Gtk_Sensitivity_Type;
   procedure Set_Lower_Stepper_Sensitivity
      (The_Range   : access Gtk_Range_Record;
       Sensitivity : Gtk.Enums.Gtk_Sensitivity_Type);
   --  Sets the sensitivity policy for the stepper that points to the 'lower'
   --  end of the GtkRange's adjustment.
   --  Since: gtk+ 2.10
   --  "sensitivity": the lower stepper's sensitivity policy.

   function Get_Min_Slider_Size
      (The_Range : access Gtk_Range_Record) return Gint;
   procedure Set_Min_Slider_Size
      (The_Range : access Gtk_Range_Record;
       Min_Size  : Boolean);
   --  Sets the minimum size of the range's slider. This function is useful
   --  mainly for Gtk.GRange.Gtk_Range subclasses.
   --  Since: gtk+ 2.20
   --  "min_size": The slider's minimum size

   procedure Get_Range_Rect
      (The_Range  : access Gtk_Range_Record;
       Range_Rect : out Gdk.Rectangle.Gdk_Rectangle);
   --  This function returns the area that contains the range's trough and its
   --  steppers, in widget->window coordinates. This function is useful mainly
   --  for Gtk.GRange.Gtk_Range subclasses.
   --  Since: gtk+ 2.20
   --  "range_rect": return location for the range rectangle

   function Get_Restrict_To_Fill_Level
      (The_Range : access Gtk_Range_Record) return Boolean;
   procedure Set_Restrict_To_Fill_Level
      (The_Range              : access Gtk_Range_Record;
       Restrict_To_Fill_Level : Boolean);
   --  Sets whether the slider is restricted to the fill level. See
   --  Gtk.GRange.Set_Fill_Level for a general description of the fill level
   --  concept.
   --  Since: gtk+ 2.12
   --  "restrict_to_fill_level": Whether the fill level restricts slider
   --  movement.

   function Get_Round_Digits
      (The_Range : access Gtk_Range_Record) return Gint;
   procedure Set_Round_Digits
      (The_Range    : access Gtk_Range_Record;
       Round_Digits : Gint);
   --  Sets the number of digits to round the value to when it changes. See
   --  Gtk.GRange.Gtk_Range::change-value.
   --  Since: gtk+ 2.24
   --  "round_digits": the precision in digits, or -1

   function Get_Show_Fill_Level
      (The_Range : access Gtk_Range_Record) return Boolean;
   procedure Set_Show_Fill_Level
      (The_Range       : access Gtk_Range_Record;
       Show_Fill_Level : Boolean);
   --  Sets whether a graphical fill level is show on the trough. See
   --  Gtk.GRange.Set_Fill_Level for a general description of the fill level
   --  concept.
   --  Since: gtk+ 2.12
   --  "show_fill_level": Whether a fill level indicator graphics is shown.

   procedure Get_Slider_Range
      (The_Range    : access Gtk_Range_Record;
       Slider_Start : out Gint;
       Slider_End   : out Gint);
   --  This function returns sliders range along the long dimension, in
   --  widget->window coordinates. This function is useful mainly for
   --  Gtk.GRange.Gtk_Range subclasses.
   --  Since: gtk+ 2.20
   --  "slider_start": return location for the slider's start, or null
   --  "slider_end": return location for the slider's end, or null

   function Get_Slider_Size_Fixed
      (The_Range : access Gtk_Range_Record) return Boolean;
   procedure Set_Slider_Size_Fixed
      (The_Range  : access Gtk_Range_Record;
       Size_Fixed : Boolean);
   --  Sets whether the range's slider has a fixed size, or a size that
   --  depends on it's adjustment's page size. This function is useful mainly
   --  for Gtk.GRange.Gtk_Range subclasses.
   --  Since: gtk+ 2.20
   --  "size_fixed": True to make the slider size constant

   function Get_Update_Policy
      (The_Range : access Gtk_Range_Record) return Gtk.Enums.Gtk_Update_Type;
   pragma Obsolescent (Get_Update_Policy);
   procedure Set_Update_Policy
      (The_Range : access Gtk_Range_Record;
       Policy    : Gtk.Enums.Gtk_Update_Type);
   pragma Obsolescent (Set_Update_Policy);
   --  Sets the update policy for the range. GTK_UPDATE_CONTINUOUS means that
   --  anytime the range slider is moved, the range value will change and the
   --  value_changed signal will be emitted. GTK_UPDATE_DELAYED means that the
   --  value will be updated after a brief timeout where no slider motion
   --  occurs, so updates are spaced by a short time rather than continuous.
   --  GTK_UPDATE_DISCONTINUOUS means that the value will only be updated when
   --  the user releases the button and ends the slider drag operation.
   --  updates, you need to code it yourself.
   --  Deprecated since 2.24, There is no replacement. If you require delayed
   --  "policy": update policy

   function Get_Upper_Stepper_Sensitivity
      (The_Range : access Gtk_Range_Record)
       return Gtk.Enums.Gtk_Sensitivity_Type;
   procedure Set_Upper_Stepper_Sensitivity
      (The_Range   : access Gtk_Range_Record;
       Sensitivity : Gtk.Enums.Gtk_Sensitivity_Type);
   --  Sets the sensitivity policy for the stepper that points to the 'upper'
   --  end of the GtkRange's adjustment.
   --  Since: gtk+ 2.10
   --  "sensitivity": the upper stepper's sensitivity policy.

   function Get_Value (The_Range : access Gtk_Range_Record) return Gdouble;
   procedure Set_Value
      (The_Range : access Gtk_Range_Record;
       Value     : Gdouble);
   --  Sets the current value of the range; if the value is outside the
   --  minimum or maximum range values, it will be clamped to fit inside them.
   --  The range emits the Gtk.GRange.Gtk_Range::value-changed signal if the
   --  value changes.
   --  "value": new value of the range

   procedure Set_Increments
      (The_Range : access Gtk_Range_Record;
       Step      : Gdouble;
       Page      : Gdouble);
   --  Sets the step and page sizes for the range. The step size is used when
   --  the user clicks the Gtk.Scrollbar.Gtk_Scrollbar arrows or moves
   --  Gtk.Scale.Gtk_Scale via arrow keys. The page size is used for example
   --  when moving via Page Up or Page Down keys.
   --  "step": step size
   --  "page": page size

   procedure Set_Range
      (The_Range : access Gtk_Range_Record;
       Min       : Gdouble;
       Max       : Gdouble);
   --  Sets the allowable values in the Gtk.GRange.Gtk_Range, and clamps the
   --  range value to be between Min and Max. (If the range has a non-zero page
   --  size, it is clamped between Min and Max - page-size.)
   --  "min": minimum range value
   --  "max": maximum range value

   ---------------------
   -- Interfaces_Impl --
   ---------------------

   function Get_Orientation
      (Self : access Gtk_Range_Record) return Gtk.Enums.Gtk_Orientation;
   procedure Set_Orientation
      (Self        : access Gtk_Range_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "Orientable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Range_Record, Gtk_Range);
   function "+"
     (Widget : access Gtk_Range_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Range
   renames Implements_Buildable.To_Object;

   package Implements_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Range_Record, Gtk_Range);
   function "+"
     (Widget : access Gtk_Range_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Range
   renames Implements_Orientable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Adjustment_Property
   --  Type: Gtk.Adjustment.Gtk_Adjustment
   --  Flags: read-write
   --
   --  Name: Fill_Level_Property
   --  Type: Gdouble
   --  Flags: read-write
   --  The fill level (e.g. prebuffering of a network stream). See
   --  Gtk.GRange.Set_Fill_Level.
   --
   --  Name: Inverted_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Lower_Stepper_Sensitivity_Property
   --  Type: Gtk.Enums.Gtk_Sensitivity_Type
   --  Flags: read-write
   --
   --  Name: Restrict_To_Fill_Level_Property
   --  Type: Boolean
   --  Flags: read-write
   --  The restrict-to-fill-level property controls whether slider movement is
   --  restricted to an upper boundary set by the fill level. See
   --  Gtk.GRange.Set_Restrict_To_Fill_Level.
   --
   --  Name: Round_Digits_Property
   --  Type: Gint
   --  Flags: read-write
   --  The number of digits to round the value to when it changes, or -1. See
   --  Gtk.GRange.Gtk_Range::change-value.
   --
   --  Name: Show_Fill_Level_Property
   --  Type: Boolean
   --  Flags: read-write
   --  The show-fill-level property controls whether fill level indicator
   --  graphics are displayed on the trough. See
   --  Gtk.GRange.Set_Show_Fill_Level.
   --
   --  Name: Update_Policy_Property
   --  Type: Gtk.Enums.Gtk_Update_Type
   --  Flags: read-write
   --
   --  Name: Upper_Stepper_Sensitivity_Property
   --  Type: Gtk.Enums.Gtk_Sensitivity_Type
   --  Flags: read-write

   Adjustment_Property : constant Glib.Properties.Property_Object;
   Fill_Level_Property : constant Glib.Properties.Property_Double;
   Inverted_Property : constant Glib.Properties.Property_Boolean;
   Lower_Stepper_Sensitivity_Property : constant Gtk.Enums.Property_Gtk_Sensitivity_Type;
   Restrict_To_Fill_Level_Property : constant Glib.Properties.Property_Boolean;
   Round_Digits_Property : constant Glib.Properties.Property_Int;
   Show_Fill_Level_Property : constant Glib.Properties.Property_Boolean;
   Update_Policy_Property : constant Gtk.Enums.Property_Gtk_Update_Type;
   Upper_Stepper_Sensitivity_Property : constant Gtk.Enums.Property_Gtk_Sensitivity_Type;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "adjust-bounds"
   --     procedure Handler
   --       (Self   : access Gtk_Range_Record'Class;
   --        Object : Gdouble);
   --
   --  "change-value"
   --     function Handler
   --       (Self   : access Gtk_Range_Record'Class;
   --        Scroll : Gtk.Enums.Gtk_Scroll_Type;
   --        Value  : Gdouble) return Boolean;
   --    --  "scroll": the type of scroll action that was performed
   --    --  "value": the new value resulting from the scroll action
   --  The ::change-value signal is emitted when a scroll action is performed
   --  on a range. It allows an application to determine the type of scroll
   --  event that occurred and the resultant new value. The application can
   --  handle the event itself and return True to prevent further processing.
   --  Or, by returning False, it can pass the event to other handlers until
   --  the default GTK+ handler is reached. The value parameter is unrounded.
   --  An application that overrides the ::change-value signal is responsible
   --  for clamping the value to the desired number of decimal digits; the
   --  default GTK+ handler clamps the value based on
   --  Gtk.GRange.Gtk_Range:round_digits. It is not possible to use delayed
   --  update policies in an overridden ::change-value handler.
   --  Returns True to prevent other handlers from being invoked for the
   --  signal, False to propagate the signal further
   --
   --  "move-slider"
   --     procedure Handler
   --       (Self : access Gtk_Range_Record'Class;
   --        Step : Gtk.Enums.Gtk_Scroll_Type);
   --    --  "step": how to move the slider
   --  Virtual function that moves the slider. Used for keybindings.
   --
   --  "value-changed"
   --     procedure Handler (Self : access Gtk_Range_Record'Class);
   --  Emitted when the range value changes.

   Signal_Adjust_Bounds : constant Glib.Signal_Name := "adjust-bounds";
   Signal_Change_Value : constant Glib.Signal_Name := "change-value";
   Signal_Move_Slider : constant Glib.Signal_Name := "move-slider";
   Signal_Value_Changed : constant Glib.Signal_Name := "value-changed";

private
   Adjustment_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("adjustment");
   Fill_Level_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("fill-level");
   Inverted_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("inverted");
   Lower_Stepper_Sensitivity_Property : constant Gtk.Enums.Property_Gtk_Sensitivity_Type :=
     Gtk.Enums.Build ("lower-stepper-sensitivity");
   Restrict_To_Fill_Level_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("restrict-to-fill-level");
   Round_Digits_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("round-digits");
   Show_Fill_Level_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-fill-level");
   Update_Policy_Property : constant Gtk.Enums.Property_Gtk_Update_Type :=
     Gtk.Enums.Build ("update-policy");
   Upper_Stepper_Sensitivity_Property : constant Gtk.Enums.Property_Gtk_Sensitivity_Type :=
     Gtk.Enums.Build ("upper-stepper-sensitivity");
end Gtk.GRange;
