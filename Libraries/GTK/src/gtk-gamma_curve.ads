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
--  The Gtk_Gamma_Curve widget is a child of Gtk_Curve specifically for editing
--  gamma curves, which are used in graphics applications such as the Gimp.
--
--  The Gamma_Curve widget shows a curve which the user can edit with the mouse
--  just like a Gtk_Curve widget. On the right of the curve it also displays 5
--  buttons, 3 of which change between the 3 curve modes (spline, linear and
--  free), and the other 2 set the curve to a particular gamma value, or reset
--  it to a straight line.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Drawing</group>
--  <testgtk>create_gamma_curve.adb</testgtk>
--  <screenshot>gtk-gamma</screenshot>

with Gtk.Box;
with Gtk.Curve;

package Gtk.Gamma_Curve is

   type Gtk_Gamma_Curve_Record is new Gtk.Box.Gtk_Box_Record with private;
   type Gtk_Gamma_Curve is access all Gtk_Gamma_Curve_Record'Class;

   procedure Gtk_New (Gamma_Curve : out Gtk_Gamma_Curve);
   --  Create a new Gtk_Gamma_Curve.

   procedure Initialize (Gamma_Curve : access Gtk_Gamma_Curve_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Gamma_Curve.

   function Get_Curve
     (Gamma_Curve : access Gtk_Gamma_Curve_Record) return Gtk.Curve.Gtk_Curve;
   --  Return the Curve widget associated with a Gamma_Curve.

   function Get_Gamma
     (Gamma_Curve : access Gtk_Gamma_Curve_Record) return Gfloat;
   --  Return the Gamma value associated with a Gamma_Curve.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  </properties>

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  </signals>

private

   type Gtk_Gamma_Curve_Record is new Gtk.Box.Gtk_Box_Record with null record;

   pragma Import (C, Get_Type, "gtk_gamma_curve_get_type");

end Gtk.Gamma_Curve;
