-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                    Copyright (C) 2010-2013, AdaCore               --
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
--  Gtk_Scale_Button provides a button which pops up a scale widget. This kind
--  of widget is commonly used for volume controls in multimedia applications,
--  and GTK+ provides a Gtk_Volume_Button subclass that is tailored for this
--  use case.
--  </description>
--  <c_version>2.16.6</c_version>
--  <group>Numeric/Text Data Entry</group>

with Glib.Properties;
with Gtk.Adjustment;
with Gtk.Button;
with Gtk.Enums;
with Gtk.Widget;
with Gtkada.Types;

package Gtk.Scale_Button is

   type Gtk_Scale_Button_Record is
     new Gtk.Button.Gtk_Button_Record with private;
   type Gtk_Scale_Button is access all Gtk_Scale_Button_Record'Class;

   function Gtk_New
     (Size  : Gtk.Enums.Gtk_Icon_Size;
      Min   : Gdouble;
      Max   : Gdouble;
      Step  : Gdouble;
      Icons : Gtkada.Types.Chars_Ptr_Array)
      return Gtk_Scale_Button;
   --  Creates a Gtk_Scale_Button, with a range between Min and Max (usually
   --  0 .. 100), with a stepping of Step (usually 2).  Invoke with a list of
   --  icon names, or Null_Array if you want to set the list later with
   --  Set_Icons.

   function Get_Type return GType;

   function Get_Adjustment
     (Button : access Gtk_Scale_Button_Record)
      return Gtk.Adjustment.Gtk_Adjustment;
   --  Gets the Gtk_Adjustment associated with the Gtk_Scale_Button's scale.
   --  See Gtk.Range.Get_Adjustment for details.

   function Get_Minus_Button
     (Button : access Gtk_Scale_Button_Record) return Gtk.Widget.Gtk_Widget;
   function Get_Plus_Button
     (Button : access Gtk_Scale_Button_Record) return Gtk.Widget.Gtk_Widget;
   --  Retrieves the minus/plus button of the Gtk_Scale_Button.

   function Get_Popup
     (Button : access Gtk_Scale_Button_Record) return Gtk.Widget.Gtk_Widget;
   --  Retrieves the popup of the Gtk_Scale_Button.

   function Get_Value (Button : access Gtk_Scale_Button_Record) return Gdouble;
   procedure Set_Value
     (Button : access Gtk_Scale_Button_Record;
      Value  : Gdouble);
   --  Gets/Sets the current value of the scale; if the value is outside
   --  the minimum or maximum range values, it will be clamped to fit
   --  inside them. The scale button emits the GtkScaleButton::value-changed
   --  signal if the value changes.

   procedure Set_Adjustment
     (Button     : access Gtk_Scale_Button_Record;
      Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --  Sets the Gtk_Adjustment to be used as a model for the Gtk_Scale_Button's
   --  scale.  See Gtk.Range.Set_Adjustment for details.

   procedure Set_Icons
     (Button : access Gtk_Scale_Button_Record;
      Icons  : Gtkada.Types.Chars_Ptr_Array);
   --  Sets the icons to be used by the scale button.
   --  For details, see the icons property.

   -----------------
   -- Obsolescent --
   -----------------

   function Get_Orientation
     (Button : access Gtk_Scale_Button_Record)
      return Gtk.Enums.Gtk_Orientation;
   pragma Obsolescent; --  Get_Orientation
   procedure Set_Orientation
     (Button      : access Gtk_Scale_Button_Record;
      Orientation : Gtk.Enums.Gtk_Orientation);
   pragma Obsolescent; --  Set_Orientation
   --  Gets/Sets the orientation of the Gtk_Scale_Button's popup window.
   --
   --  Deprecated: 2.16: Use Gtk.Orientable.Get_Orientation and
   --  Gtk.Orientable.Set_Orientation instead.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  Name:  Adjustment_Property
   --  Type:  Object
   --  Descr: The Gtk_Adjustment that contains the current value of this scale
   --         button object
   --
   --  Name:  Icons_Property
   --  Type:  Boxed
   --  Descr: List of icon names
   --
   --  Name:  Size_Property
   --  Type:  Enum
   --  Descr: The icon size
   --
   --  Name:  Value_Property
   --  Type:  Double
   --  Descr: The value of the scale
   --
   --  </properties>

   Adjustment_Property : constant Glib.Properties.Property_Object;
   Icons_Property      : constant Glib.Properties.Property_Boxed;
   Size_Property       : constant Glib.Properties.Property_Enum;
   Value_Property      : constant Glib.Properties.Property_Double;

private

   type Gtk_Scale_Button_Record is
     new Gtk.Button.Gtk_Button_Record with null record;

   Adjustment_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("adjustment");
   Icons_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("icons");
   Size_Property : constant Glib.Properties.Property_Enum :=
     Glib.Properties.Build ("size");
   Value_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("value");

   pragma Import (C, Get_Type, "gtk_scale_button_get_type");

end Gtk.Scale_Button;
