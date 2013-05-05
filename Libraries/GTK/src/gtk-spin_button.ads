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
--  A Gtk_Spin_Button is a single line text editing widget for text that
--  represents a number. At the right hand side of the text line there are
--  small up- and down arrow buttons for incrementing or decrementing
--  (spinning) the number within a given range.
--  It allows the value to have zero or a number of decimal places and
--  to be incremented/decremented in configurable steps.
--  The action of holding down one of the buttons optionally results in an
--  acceleration of change in the value according to how long it is
--  depressed.
--
--  See Gtk.GEntry for a text editing widget without spin buttons.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Numeric/Text Data Entry</group>
--  <screenshot>gtk-spin_button</screenshot>
--  <testgtk>create_spin.adb</testgtk>

with Glib.Generic_Properties;
with Glib.Properties;
with Gtk.Adjustment;
with Gtk.GEntry;

package Gtk.Spin_Button is

   type Gtk_Spin_Button_Record is new Gtk.GEntry.Gtk_Entry_Record with private;
   type Gtk_Spin_Button is access all Gtk_Spin_Button_Record'Class;

   type Gtk_Spin_Button_Update_Policy is
     (Update_Always,
      --  Update always, errors are ignored while converting text into a
      --  numeric value.

      Update_If_Valid
      --  The spin button's value gets changed if the text input is a numeric
      --  value that is within the range specified by the adjustment.
     );
   --  Determine the update policy of the spin button which affects the
   --  behaviour when parsing inserted text and syncing its value with the
   --  values of the adjustment.
   pragma Convention (C, Gtk_Spin_Button_Update_Policy);

   type Gtk_Spin_Type is
     (Spin_Step_Forward,
      Spin_Step_Backward,
      Spin_Page_Forward,
      Spin_Page_Backward,
      Spin_Home,
      Spin_End,
      Spin_User_Defined);
   --  Determine how manual spinning should be done.
   --  See also the Spin procedure.
   pragma Convention (C, Gtk_Spin_Type);

   procedure Gtk_New
     (Spin_Button : out Gtk_Spin_Button;
      Adjustment  : Gtk.Adjustment.Gtk_Adjustment;
      Climb_Rate  : Gdouble;
      The_Digits  : Gint);
   --  Create a spin button with the given parameters.
   --  Adjustment contains the range, current value, step value and
   --  "page" value. The step value is the increment/decrement when pressing
   --  mouse button 1 on a button; the page value when mouse button 2 is
   --  pressed. Additionally, mouse button 3 can be used to jump directly to
   --  the or lower values when used to select one of the buttons.
   --  Climb_Rate takes a value between 0.0 and 1.0 and indicates the
   --  amount of acceleration that the Spin Button has.
   --  The_Digits is the number of digits behind the decimal point to be
   --  displayed for the value.

   procedure Gtk_New
     (Spin_Button : out Gtk_Spin_Button;
      Min         : Gdouble;
      Max         : Gdouble;
      Step        : Gdouble);
   --  Same as above, but with explicit range instead of an adjustment.
   --  The adjustment associated with Spin_Button is created internally.

   procedure Initialize
     (Spin_Button : access Gtk_Spin_Button_Record'Class;
      Adjustment  : Gtk.Adjustment.Gtk_Adjustment;
      Climb_Rate  : Gdouble;
      The_Digits  : Gint);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Initialize
     (Spin_Button : access Gtk_Spin_Button_Record'Class;
      Min         : Gdouble;
      Max         : Gdouble;
      Step        : Gdouble);
   --  Internal initialization function.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Spin_Button.

   procedure Set_Adjustment
     (Spin_Button : access Gtk_Spin_Button_Record;
      Adjustment  : Gtk.Adjustment.Gtk_Adjustment);
   function Get_Adjustment
     (Spin_Button : access Gtk_Spin_Button_Record)
      return Gtk.Adjustment.Gtk_Adjustment;
   --  Set or Get the adjustment settings of the spin button.

   procedure Set_Digits
     (Spin_Button : access Gtk_Spin_Button_Record;
      The_Digits  : Guint);
   function Get_Digits
     (Spin_Button : access Gtk_Spin_Button_Record) return Guint;
   --  Set or Get number of decimals of the spin button.

   procedure Set_Increments
     (Spin_Button : access Gtk_Spin_Button_Record;
      Step        : Gdouble;
      Page        : Gdouble);
   procedure Get_Increments
     (Spin_Button : access Gtk_Spin_Button_Record;
      Step        : out Gdouble;
      Page        : out Gdouble);
   --  Set or Get the increments for a single step and a page move.

   procedure Set_Range
     (Spin_Button : access Gtk_Spin_Button_Record;
      Min         : Gdouble;
      Max         : Gdouble);
   procedure Get_Range
     (Spin_Button : access Gtk_Spin_Button_Record;
      Min         : out Gdouble;
      Max         : out Gdouble);
   --  Set or Get range of the spin button.

   procedure Set_Value
     (Spin_Button : access Gtk_Spin_Button_Record;
      Value       : Gdouble);
   function Get_Value
     (Spin_Button : access Gtk_Spin_Button_Record) return Gdouble;
   --  Set or Get the current value of the spin button in a double.

   function Get_Value_As_Int
     (Spin_Button : access Gtk_Spin_Button_Record) return Gint;
   --  Return the current value of the spin button in an integer.

   procedure Set_Update_Policy
     (Spin_Button : access Gtk_Spin_Button_Record;
      Policy      : Gtk_Spin_Button_Update_Policy);
   function Get_Update_Policy
     (Spin_Button : access Gtk_Spin_Button_Record)
      return Gtk_Spin_Button_Update_Policy;
   --  Set the update policy of the spin button.
   --  See Gtk_Spin_Button_Update_Policy for the meaning of Policy.

   procedure Set_Numeric
     (Spin_Button : access Gtk_Spin_Button_Record;
      Numeric     : Boolean);
   function Get_Numeric
     (Spin_Button : access Gtk_Spin_Button_Record) return Boolean;
   --  If Numeric is True, then only a numeric value can be typed in the
   --  text entry, otherwise also nonnumeric text.

   procedure Spin
     (Spin_Button : access Gtk_Spin_Button_Record;
      Direction   : Gtk_Spin_Type;
      Step        : Gdouble);
   --  Set the value of the spin button relative to its current value.
   --  Depending on Direction, it will be incremented or decremented with
   --  the step value.

   procedure Set_Wrap
     (Spin_Button : access Gtk_Spin_Button_Record; Wrap : Boolean);
   function Get_Wrap
     (Spin_Button : access Gtk_Spin_Button_Record) return Boolean;
   --  Set whether the spin button should "wrap around" when exceeding the
   --  upper and lower limits.

   procedure Set_Snap_To_Ticks
    (Spin_Button   : access Gtk_Spin_Button_Record;
     Snap_To_Ticks : Boolean);
   function Get_Snap_To_Ticks
    (Spin_Button : access Gtk_Spin_Button_Record) return Boolean;
   --  Set the spin button to round the value to the nearest step value
   --  which is set within its adjustment settings.

   procedure Update (Spin_Button : access Gtk_Spin_Button_Record);
   --  Manually force an update of the spin button.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Adjustment_Property
   --  Type:  Object
   --  Descr: The adjustment that holds the value of the spinbutton
   --
   --  Name:  Climb_Rate_Property
   --  Type:  Double
   --  Descr: The acceleration rate when you hold down a button
   --
   --  Name:  Digits_Property
   --  Type:  Uint
   --  Descr: The number of decimal places to display
   --
   --  Name:  Numeric_Property
   --  Type:  Boolean
   --  Descr: Whether non-numeric characters should be ignored
   --
   --  Name:  Snap_To_Ticks_Property
   --  Type:  Boolean
   --  Descr: Whether erroneous values are automatically changed to a spin
   --         button's nearest step increment
   --
   --  Name:  Update_Policy_Property
   --  Type:  Enum
   --  Descr: Whether the spin button should update always, or only when the
   --         value is legal
   --
   --  Name:  Value_Property
   --  Type:  Double
   --  Descr: Reads the current value, or sets a new value
   --
   --  Name:  Wrap_Property
   --  Type:  Boolean
   --  Descr: Whether a spin button should wrap upon reaching its limits
   --
   --  </properties>

   package Spin_Button_Update_Policy_Properties is new
     Glib.Generic_Properties.Generic_Internal_Discrete_Property
       (Gtk_Spin_Button_Update_Policy);
   type Property_Spin_Button_Update_Policy_Type is new
     Spin_Button_Update_Policy_Properties.Property;

   Adjustment_Property    : constant Glib.Properties.Property_Object;
   Climb_Rate_Property    : constant Glib.Properties.Property_Double;
   Digits_Property        : constant Glib.Properties.Property_Uint;
   Numeric_Property       : constant Glib.Properties.Property_Boolean;
   Snap_To_Ticks_Property : constant Glib.Properties.Property_Boolean;
   Update_Policy_Property : constant Property_Spin_Button_Update_Policy_Type;
   Value_Property         : constant Glib.Properties.Property_Double;
   Wrap_Property          : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "change_value"
   --    procedure Handler
   --      (Spin : access Gtk_Spin_Button_Record'Class;
   --       Typ  : Gtk_Scroll_Type);
   --    You should emit this signal to request a change in the value of the
   --    spin button. This is mostly useful as a keybinding, and is bound, by
   --    default, to the arrow keys, PageUp, PageDown, Home and End keys.
   --
   --  - "input"
   --    procedure Handler
   --       (Spin  : access Gtk_Spin_Button_Record'Class;
   --        Value : out Gint);
   --    ???
   --
   --  - "output"
   --    procedure Handler (Spin : access Gtk_Spin_Button_Record'Class);
   --    ???
   --
   --  - "value_changed"
   --    procedure Handler (Spin : access Gtk_Spin_Button_Record'Class);
   --    Emitted when the value of the spin button has changed.
   --
   --  </signals>

   Signal_Change_Value  : constant Glib.Signal_Name := "change_value";
   Signal_Input         : constant Glib.Signal_Name := "input";
   Signal_Output        : constant Glib.Signal_Name := "output";
   Signal_Value_Changed : constant Glib.Signal_Name := "value_changed";

private
   type Gtk_Spin_Button_Record is new Gtk.GEntry.Gtk_Entry_Record
   with null record;

   Adjustment_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("adjustment");
   Climb_Rate_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("climb-rate");
   Digits_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("digits");
   Numeric_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("numeric");
   Snap_To_Ticks_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("snap-to-ticks");
   Update_Policy_Property : constant Property_Spin_Button_Update_Policy_Type :=
     Build ("update-policy");
   Value_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("value");
   Wrap_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("wrap");

   pragma Import (C, Get_Type, "gtk_spin_button_get_type");
end Gtk.Spin_Button;

--  The following function is for the sake of the C++ binding only:
--  No binding: gtk_spin_button_configure
