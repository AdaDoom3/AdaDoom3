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
--  The Gtk_Curve widget allows the user to edit a curve covering a range of
--  values. It is typically used to fine-tune color balances in graphics
--  applications like the Gimp.
--
--  The Gtk_Curve widget has 3 modes of operation: spline, linear and free. In
--  spline mode the user places points on the curve which are automatically
--  connected together into a smooth curve. In linear mode the user places
--  points on the curve which are connected by straight lines. In free mode the
--  user can draw the points of the curve freely, and they are not connected at
--  all.
--
--  </description>
--  <group>Drawing</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;             use Glib;
with Glib.Properties;  use Glib.Properties;
with Glib.Types;       use Glib.Types;
with Gtk.Buildable;    use Gtk.Buildable;
with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Widget;       use Gtk.Widget;

package Gtk.Curve is

   type Gtk_Curve_Record is new Gtk_Drawing_Area_Record with null record;
   type Gtk_Curve is access all Gtk_Curve_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Curve : out Gtk_Curve);
   procedure Initialize (Curve : access Gtk_Curve_Record'Class);

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_curve_get_type");

   -------------
   -- Methods --
   -------------

   procedure Reset (Curve : access Gtk_Curve_Record);
   --  Reset the curve. Reset to a straight line from the minimum x & y values
   --  to the maximum x & y values (i.e. from the bottom-left to the top-right
   --  corners). The curve type is not changed.

   procedure Set_Curve_Type
      (Curve    : access Gtk_Curve_Record;
       The_Type : Gtk.Enums.Gtk_Curve_Type);
   --  Set the type of the curve. The curve will remain unchanged except when
   --  changing from a free curve to a linear or spline curve, in which case
   --  the curve will be changed as little as possible.

   procedure Set_Gamma (Curve : access Gtk_Curve_Record; Gamma : Gfloat);
   --  Recompute the entire curve using the given gamma value. A gamma value
   --  of 1.0 results in a straight line. Values greater than 1.0 result in a
   --  curve above the straight line. Values less than 1.0 result in a curve
   --  below the straight line. The curve type is changed to Curve_Type_Free.

   procedure Set_Range
      (Curve : access Gtk_Curve_Record;
       Min_X : Gfloat;
       Max_X : Gfloat;
       Min_Y : Gfloat;
       Max_Y : Gfloat);
   --  Set the minimum and maximum x & y values of the curve. The curve is
   --  also reset with a call to Reset.

   ----------------------
   -- GtkAda additions --
   ----------------------

   procedure Set_Vector
     (Curve  : access Gtk_Curve_Record;
      Vector : Gfloat_Array);
   procedure Get_Vector
     (Curve  : access Gtk_Curve_Record;
      Vector : out Gfloat_Array);
   --  Set the vector of points on the curve.
   --  The curve type is set to Curve_Type_Free.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Curve_Record, Gtk_Curve);
   function "+"
     (Widget : access Gtk_Curve_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Curve
   renames Implements_Buildable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Curve_Type_Property
   --  Type: Gtk.Enums.Gtk_Curve_Type
   --  Flags: read-write
   --
   --  Name: Max_X_Property
   --  Type: Gfloat
   --  Flags: read-write
   --
   --  Name: Max_Y_Property
   --  Type: Gfloat
   --  Flags: read-write
   --
   --  Name: Min_X_Property
   --  Type: Gfloat
   --  Flags: read-write
   --
   --  Name: Min_Y_Property
   --  Type: Gfloat
   --  Flags: read-write

   Curve_Type_Property : constant Gtk.Enums.Property_Gtk_Curve_Type;
   Max_X_Property : constant Glib.Properties.Property_Float;
   Max_Y_Property : constant Glib.Properties.Property_Float;
   Min_X_Property : constant Glib.Properties.Property_Float;
   Min_Y_Property : constant Glib.Properties.Property_Float;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "curve-type-changed"
   --     procedure Handler (Self : access Gtk_Curve_Record'Class);

   Signal_Curve_Type_Changed : constant Glib.Signal_Name := "curve-type-changed";

private
   Curve_Type_Property : constant Gtk.Enums.Property_Gtk_Curve_Type :=
     Gtk.Enums.Build ("curve-type");
   Max_X_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("max-x");
   Max_Y_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("max-y");
   Min_X_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("min-x");
   Min_Y_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("min-y");
end Gtk.Curve;
