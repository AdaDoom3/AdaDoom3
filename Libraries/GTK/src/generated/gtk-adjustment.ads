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
--  This object represents an adjustable bounded value. It is used in many
--  other widgets that have such internal values, like Gtk_Scrollbar,
--  Gtk_Spin_Button, Gtk_Range, ... Modifying the value of these widgets is
--  done through their associated adjustments.
--
--  The modification of the value is left to the user, who should call
--  Value_Changed or Changed to emit the relevant signals.
--
--  The meaning of the most important fields can be explained on the following
--  figure (imagine this is a scrollbar):
--
--  [-------|=================|-------------------]
--
--  lower value value + page_size upper
--
--  </description>
--  <group>Scrolling</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;

package Gtk.Adjustment is

   type Gtk_Adjustment_Record is new GObject_Record with null record;
   type Gtk_Adjustment is access all Gtk_Adjustment_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Adjustment     : out Gtk_Adjustment;
       Value          : Gdouble;
       Lower          : Gdouble;
       Upper          : Gdouble;
       Step_Increment : Gdouble;
       Page_Increment : Gdouble;
       Page_Size      : Gdouble := 0.0);
   procedure Initialize
      (Adjustment     : access Gtk_Adjustment_Record'Class;
       Value          : Gdouble;
       Lower          : Gdouble;
       Upper          : Gdouble;
       Step_Increment : Gdouble;
       Page_Increment : Gdouble;
       Page_Size      : Gdouble := 0.0);
   --  Create a new adjustment. Value is the initial value of the adjustment.
   --  It must be in the range (Lower .. Upper) and the adjustment's value will
   --  never be outside this range. Step_Increment is the value used to make
   --  minor adjustments, such as when the user clicks on the arrows of a
   --  scrollbar. Page_Increment is used to make major adjustments, such as
   --  when the user clicks in the through on a scrollbar. Page_Size is
   --  deprecated, use the default value.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_adjustment_get_type");

   -------------
   -- Methods --
   -------------

   procedure Changed (Adjustment : access Gtk_Adjustment_Record);

   procedure Clamp_Page
      (Adjustment : access Gtk_Adjustment_Record;
       Lower      : Gdouble;
       Upper      : Gdouble);
   --  Update the Adjustment value to ensure that the range between Lower and
   --  Upper is in the current page (i.e. between value and value + page_size).
   --  If the range is larger than the page size, then only the start of it
   --  will be in the current page. A "value_changed" signal will be emitted if
   --  the value is changed.

   procedure Configure
      (Adjustment     : access Gtk_Adjustment_Record;
       Value          : Gdouble;
       Lower          : Gdouble;
       Upper          : Gdouble;
       Step_Increment : Gdouble;
       Page_Increment : Gdouble;
       Page_Size      : Gdouble);
   --  Sets all properties of the adjustment at once. Use this function to
   --  avoid multiple emissions of the "changed" signal. See
   --  Gtk.Adjustment.Set_Lower for an alternative way of compressing multiple
   --  emissions of "changed" into one.
   --  Since: gtk+ 2.14
   --  "value": the new value
   --  "lower": the new minimum value
   --  "upper": the new maximum value
   --  "step_increment": the new step increment
   --  "page_increment": the new page increment
   --  "page_size": the new page size

   function Get_Lower
      (Adjustment : access Gtk_Adjustment_Record) return Gdouble;
   procedure Set_Lower
      (Adjustment : access Gtk_Adjustment_Record;
       Lower      : Gdouble);
   --  Sets the minimum value of the adjustment. When setting multiple
   --  adjustment properties via their individual setters, multiple "changed"
   --  signals will be emitted. However, since the emission of the "changed"
   --  signal is tied to the emission of the "GObject::notify" signals of the
   --  changed properties, it's possible to compress the "changed" signals into
   --  one by calling g_object_freeze_notify and g_object_thaw_notify around
   --  the calls to the individual setters. Alternatively, using a single
   --  g_object_set for all the properties to change, or using
   --  Gtk.Adjustment.Configure has the same effect of compressing "changed"
   --  emissions.
   --  Since: gtk+ 2.14
   --  "lower": the new minimum value

   function Get_Page_Increment
      (Adjustment : access Gtk_Adjustment_Record) return Gdouble;
   procedure Set_Page_Increment
      (Adjustment     : access Gtk_Adjustment_Record;
       Page_Increment : Gdouble);
   --  Sets the page increment of the adjustment. See Gtk.Adjustment.Set_Lower
   --  about how to compress multiple emissions of the "changed" signal when
   --  setting multiple adjustment properties.
   --  Since: gtk+ 2.14
   --  "page_increment": the new page increment

   function Get_Page_Size
      (Adjustment : access Gtk_Adjustment_Record) return Gdouble;
   procedure Set_Page_Size
      (Adjustment : access Gtk_Adjustment_Record;
       Page_Size  : Gdouble);
   --  Sets the page size of the adjustment. See Gtk.Adjustment.Set_Lower
   --  about how to compress multiple emissions of the "changed" signal when
   --  setting multiple adjustment properties.
   --  Since: gtk+ 2.14
   --  "page_size": the new page size

   function Get_Step_Increment
      (Adjustment : access Gtk_Adjustment_Record) return Gdouble;
   procedure Set_Step_Increment
      (Adjustment     : access Gtk_Adjustment_Record;
       Step_Increment : Gdouble);
   --  Sets the step increment of the adjustment. See Gtk.Adjustment.Set_Lower
   --  about how to compress multiple emissions of the "changed" signal when
   --  setting multiple adjustment properties.
   --  Since: gtk+ 2.14
   --  "step_increment": the new step increment

   function Get_Upper
      (Adjustment : access Gtk_Adjustment_Record) return Gdouble;
   procedure Set_Upper
      (Adjustment : access Gtk_Adjustment_Record;
       Upper      : Gdouble);
   --  Sets the maximum value of the adjustment. Note that values will be
   --  restricted by <literal>upper - page-size</literal> if the page-size
   --  property is nonzero. See Gtk.Adjustment.Set_Lower about how to compress
   --  multiple emissions of the "changed" signal when setting multiple
   --  adjustment properties.
   --  Since: gtk+ 2.14
   --  "upper": the new maximum value

   function Get_Value
      (Adjustment : access Gtk_Adjustment_Record) return Gdouble;
   procedure Set_Value
      (Adjustment : access Gtk_Adjustment_Record;
       Value      : Gdouble);

   procedure Value_Changed (Adjustment : access Gtk_Adjustment_Record);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Lower_Property
   --  Type: Gdouble
   --  Flags: read-write
   --  The minimum value of the adjustment.
   --
   --  Name: Page_Increment_Property
   --  Type: Gdouble
   --  Flags: read-write
   --  The page increment of the adjustment.
   --
   --  Name: Page_Size_Property
   --  Type: Gdouble
   --  Flags: read-write
   --  The page size of the adjustment. Note that the page-size is irrelevant
   --  and should be set to zero if the adjustment is used for a simple scalar
   --  value, e.g. in a Gtk.Spinbutton.Gtk_Spinbutton.
   --
   --  Name: Step_Increment_Property
   --  Type: Gdouble
   --  Flags: read-write
   --  The step increment of the adjustment.
   --
   --  Name: Upper_Property
   --  Type: Gdouble
   --  Flags: read-write
   --  The maximum value of the adjustment. Note that values will be
   --  restricted by <literal>upper - page-size</literal> if the page-size
   --  property is nonzero.
   --
   --  Name: Value_Property
   --  Type: Gdouble
   --  Flags: read-write
   --  The value of the adjustment.

   Lower_Property : constant Glib.Properties.Property_Double;
   Page_Increment_Property : constant Glib.Properties.Property_Double;
   Page_Size_Property : constant Glib.Properties.Property_Double;
   Step_Increment_Property : constant Glib.Properties.Property_Double;
   Upper_Property : constant Glib.Properties.Property_Double;
   Value_Property : constant Glib.Properties.Property_Double;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "changed"
   --     procedure Handler (Self : access Gtk_Adjustment_Record'Class);
   --
   --  "value-changed"
   --     procedure Handler (Self : access Gtk_Adjustment_Record'Class);

   Signal_Changed : constant Glib.Signal_Name := "changed";
   Signal_Value_Changed : constant Glib.Signal_Name := "value-changed";

private
   Lower_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("lower");
   Page_Increment_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("page-increment");
   Page_Size_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("page-size");
   Step_Increment_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("step-increment");
   Upper_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("upper");
   Value_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("value");
end Gtk.Adjustment;
