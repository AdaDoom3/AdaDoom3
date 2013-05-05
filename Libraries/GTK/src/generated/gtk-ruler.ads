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
--  This widget is generally put on the sides of a drawing area to help the
--  user measure distances. It indicates the current position of the mouse
--  cursor within the drawing area, and can be graduated in multiple units.
--
--  </description>
--  <screenshot>gtk-rulers</screenshot>
--  <group>Drawing</group>
--  <testgtk>create_rulers.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Orientable;  use Gtk.Orientable;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Ruler is

   type Gtk_Ruler_Record is new Gtk_Widget_Record with null record;
   type Gtk_Ruler is access all Gtk_Ruler_Record'Class;

   subtype Gtk_Vruler_Record is Gtk_Ruler_Record;
   subtype Gtk_Vruler is Gtk_Ruler;

   subtype Gtk_Hruler_Record is Gtk_Ruler_Record;
   subtype Gtk_Hruler is Gtk_Ruler;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_ruler_get_type");

   procedure Gtk_New_Vruler (Ruler : out Gtk_Vruler);
   procedure Initialize_Vruler (Ruler : access Gtk_Vruler_Record'Class);
   --  Creates a new vertical ruler unmaintained and too specialized. There is
   --  no replacement.

   function Vruler_Get_Type return Glib.GType;
   pragma Import (C, Vruler_Get_Type, "gtk_vruler_get_type");

   procedure Gtk_New_Hruler (Ruler : out Gtk_Hruler);
   procedure Initialize_Hruler (Ruler : access Gtk_Hruler_Record'Class);

   function Hruler_Get_Type return Glib.GType;
   pragma Import (C, Hruler_Get_Type, "gtk_hruler_get_type");

   -------------
   -- Methods --
   -------------

   procedure Draw_Pos (Ruler : access Gtk_Ruler_Record);

   procedure Draw_Ticks (Ruler : access Gtk_Ruler_Record);

   function Get_Metric
      (Ruler : access Gtk_Ruler_Record) return Gtk.Enums.Gtk_Metric_Type;
   procedure Set_Metric
      (Ruler  : access Gtk_Ruler_Record;
       Metric : Gtk.Enums.Gtk_Metric_Type);
   --  Set or get the units used for a Gtk_Ruler. See Set_Metric

   procedure Get_Range
      (Ruler    : access Gtk_Ruler_Record;
       Lower    : out Gdouble;
       Upper    : out Gdouble;
       Position : out Gdouble;
       Max_Size : out Gdouble);
   procedure Set_Range
      (Ruler    : access Gtk_Ruler_Record;
       Lower    : Gdouble;
       Upper    : Gdouble;
       Position : Gdouble;
       Max_Size : Gdouble);
   --  This sets the range of the ruler. unmaintained and too specialized.
   --  There is no replacement.
   --  "lower": the lower limit of the ruler
   --  "upper": the upper limit of the ruler
   --  "position": the mark on the ruler
   --  "max_size": the maximum size of the ruler used when calculating the
   --  space to leave for the text

   ---------------------
   -- Interfaces_Impl --
   ---------------------

   function Get_Orientation
      (Self : access Gtk_Ruler_Record) return Gtk.Enums.Gtk_Orientation;
   procedure Set_Orientation
      (Self        : access Gtk_Ruler_Record;
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
     (Gtk.Buildable.Gtk_Buildable, Gtk_Ruler_Record, Gtk_Ruler);
   function "+"
     (Widget : access Gtk_Ruler_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Ruler
   renames Implements_Buildable.To_Object;

   package Implements_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Ruler_Record, Gtk_Ruler);
   function "+"
     (Widget : access Gtk_Ruler_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Ruler
   renames Implements_Orientable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Lower_Property
   --  Type: Gdouble
   --  Flags: read-write
   --
   --  Name: Max_Size_Property
   --  Type: Gdouble
   --  Flags: read-write
   --
   --  Name: Metric_Property
   --  Type: Gtk.Enums.Gtk_Metric_Type
   --  Flags: read-write
   --  The metric used for the ruler.
   --
   --  Name: Position_Property
   --  Type: Gdouble
   --  Flags: read-write
   --
   --  Name: Upper_Property
   --  Type: Gdouble
   --  Flags: read-write
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Lower_Property : constant Glib.Properties.Property_Double;
   Max_Size_Property : constant Glib.Properties.Property_Double;
   Metric_Property : constant Gtk.Enums.Property_Metric_Type;
   Position_Property : constant Glib.Properties.Property_Double;
   Upper_Property : constant Glib.Properties.Property_Double;

private
   Lower_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("lower");
   Max_Size_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("max-size");
   Metric_Property : constant Gtk.Enums.Property_Metric_Type :=
     Gtk.Enums.Build ("metric");
   Position_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("position");
   Upper_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("upper");
end Gtk.Ruler;
