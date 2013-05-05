-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2006 AdaCore                    --
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
--  The progress bar provides a convenient way of displaying a state of
--  completion for typically lengthy tasks.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Display widgets</group>
--  <testgtk>create_progress.adb</testgtk>
--  <screenshot>gtk-progress_bar</screenshot>

with Glib.Properties;
with Gtk.Enums;    use Gtk.Enums;
with Pango.Layout;

pragma Warnings (Off);  --  Gtk.Progress is obsolete
with Gtk.Progress;
pragma Warnings (On);

package Gtk.Progress_Bar is

   type Gtk_Progress_Bar_Orientation is
     (Progress_Left_To_Right,
      Progress_Right_To_Left,
      Progress_Bottom_To_Top,
      Progress_Top_To_Bottom);
   pragma Convention (C, Gtk_Progress_Bar_Orientation);

   pragma Warnings (Off);  --  Gtk.Progress is obsolete
   type Gtk_Progress_Bar_Record is new
     Gtk.Progress.Gtk_Progress_Record with private;
   pragma Warnings (On);

   type Gtk_Progress_Bar is access all Gtk_Progress_Bar_Record'Class;

   procedure Gtk_New (Progress_Bar : out Gtk_Progress_Bar);
   procedure Initialize (Progress_Bar : access Gtk_Progress_Bar_Record'Class);
   --  Creates or initializes a new progress bar.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Progress_Bar.

   procedure Pulse (Progress_Bar : access Gtk_Progress_Bar_Record);
   --  Indicate that some progress is made, but you don't know how much.
   --  Causes the progress bar to enter "activity mode," where a block
   --  bounces back and forth. Each call to Pulse causes the block to move by
   --  a little bit (the amount of movement per pulse is determined by
   --  Set_Pulse_Step).

   procedure Set_Text
     (Progress_Bar : access Gtk_Progress_Bar_Record; Text : UTF8_String);
   function Get_Text
     (Progress_Bar : access Gtk_Progress_Bar_Record) return UTF8_String;
   --  Causes the given Text to appear superimposed on the progress bar.
   --  Text: a UTF-8 string.

   procedure Set_Fraction
     (Progress_Bar : access Gtk_Progress_Bar_Record;
      Fraction     : Gdouble);
   function Get_Fraction
     (Progress_Bar : access Gtk_Progress_Bar_Record) return Gdouble;
   --  Cause the progress bar to "fill in" the given fraction of the bar.
   --  The fraction should be between 0.0 and 1.0, inclusive.

   procedure Set_Pulse_Step
     (Progress_Bar : access Gtk_Progress_Bar_Record;
      Step         : Gdouble);
   function Get_Pulse_Step
     (Progress_Bar : access Gtk_Progress_Bar_Record) return Gdouble;
   --  Set the fraction of total progress bar length to move the
   --  bouncing block for each call to Pulse.

   procedure Set_Orientation
     (Progress_Bar : access Gtk_Progress_Bar_Record;
      Orientation  : Gtk_Progress_Bar_Orientation);
   function Get_Orientation
     (Progress_Bar : access Gtk_Progress_Bar_Record)
      return Gtk_Progress_Bar_Orientation;
   --  Cause the progress bar to switch to a different orientation
   --  (left-to-right, right-to-left, top-to-bottom, or bottom-to-top).

   procedure Set_Ellipsize
     (Pbar : access Gtk_Progress_Bar_Record;
      Mode : Pango.Layout.Pango_Ellipsize_Mode);
   function Get_Ellipsize
     (Pbar : access Gtk_Progress_Bar_Record)
      return Pango.Layout.Pango_Ellipsize_Mode;
   --  Sets the mode used to ellipsize (add an ellipsis: "...") the text
   --  if there is not enough space to render the entire string.

   -----------------
   -- Obsolescent --
   -----------------
   --  All subprograms below are now obsolescent in gtk+. They might be removed
   --  from future versions of gtk+ (and therefore GtkAda).
   --  To find out whether your code uses any of these, we recommend compiling
   --  with the -gnatwj switch
   --  <doc_ignore>

   --  </doc_ignore>

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Orientation_Property
   --  Type:  Gtk_Orientation
   --  Flags: read-write
   --  Descr: Orientation and growth of the progress bar
   --  See also: Set_Orientation and Get_Orientation
   --
   --  Name:  Discrete_Blocks_Property
   --  Type:  Guint
   --  Flags: read-write
   --  Descr: The number of discrete blocks in a progress bar (when shown
   --         in the discrete Style)
   --
   --  Name:  Fraction_Property
   --  Type:  Gdouble
   --  Flags: read-write
   --  Descr: The fraction of total work that has been completed
   --  See also: Set_Fraction and Get_Fraction
   --
   --  Name:  Pulse_Step_Property
   --  Type:  Gdouble
   --  Flags: read-write
   --  Descr: The fraction of total progress to move the bouncing block when
   --         pulsed
   --  See also: Set_Pulse_Step and Get_Pulse_Step
   --
   --  Name:  Text_Property
   --  Type:  UTF8_String
   --  Flags: read-write
   --  Descr: Text to be displayed in the progress bar
   --  See also: Set_Text and Get_Text
   --
   --  Name:  Activity_Blocks_Property
   --  Type:  Uint
   --  Descr: The number of blocks which can fit in the progress bar area in
   --         activity mode (Deprecated)
   --
   --  Name:  Activity_Step_Property
   --  Type:  Uint
   --  Descr: The increment used for each iteration in activity mode
   --        (Deprecated)
   --
   --  Name:  Adjustment_Property
   --  Type:  Object
   --  Descr: The GtkAdjustment connected to the progress bar (Deprecated)
   --
   --  Name:  Bar_Style_Property
   --  Type:  Enum
   --  Descr: Specifies the visual style of the bar in percentage mode
   --         (Deprecated)
   --
   --  Name:  Ellipsize_Property
   --  Type:  Enum
   --  Descr: The preferred place to ellipsize the string, if the progressbar
   --         does not have enough room to display the entire string, if at all
   --
   --  </properties>

   Orientation_Property     : constant Gtk.Enums.Property_Gtk_Orientation;
   Discrete_Blocks_Property : constant Glib.Properties.Property_Uint;
   Fraction_Property        : constant Glib.Properties.Property_Double;
   Pulse_Step_Property      : constant Glib.Properties.Property_Double;
   Text_Property            : constant Glib.Properties.Property_String;

   --  This requires a new type in Pango.Layout.Ellipsize_Mode
   --  Ellipsize_Property       : constant Glib.Properties.Property_Enum;

   --  These are deprecated, and never had a binding, no need to add them
   --  Activity_Blocks_Property : constant Glib.Properties.Property_Uint;
   --  Activity_Step_Property   : constant Glib.Properties.Property_Uint;
   --  Adjustment_Property      : constant Glib.Properties.Property_Object;
   --  Bar_Style_Property       : constant Glib.Properties.Property_Enum;

private
   pragma Warnings (Off);
   type Gtk_Progress_Bar_Record is new Gtk.Progress.Gtk_Progress_Record
     with null record;
   pragma Warnings (On);
   --  Gtk.Progress is obsolete

   Orientation_Property     : constant Gtk.Enums.Property_Gtk_Orientation :=
     Gtk.Enums.Build ("orientation");
   Discrete_Blocks_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("discrete_blocks");
   Fraction_Property        : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("fraction");
   Pulse_Step_Property      : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("pulse_step");
   Text_Property            : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("text");

   pragma Import (C, Get_Type, "gtk_progress_bar_get_type");
end Gtk.Progress_Bar;

--  The following subprograms are now obsolescent, and never had a binding
--  No binding: gtk_progress_bar_set_activity_blocks
--  No binding: gtk_progress_bar_set_activity_step
--  No binding: gtk_progress_bar_set_bar_style
--  No binding: gtk_progress_bar_set_discrete_blocks
--  No binding: gtk_progress_bar_update
--  No binding: gtk_progress_bar_new_with_adjustment
