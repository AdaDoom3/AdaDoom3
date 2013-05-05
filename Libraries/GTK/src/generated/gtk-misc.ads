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
--  This widget is a base class for all the widgets that require an alignment
--  and padding. This widget can not be instantiated directly.
--
--  </description>
--  <group>Abstract base classes</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Misc is

   type Gtk_Misc_Record is new Gtk_Widget_Record with null record;
   type Gtk_Misc is access all Gtk_Misc_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_misc_get_type");

   -------------
   -- Methods --
   -------------

   procedure Get_Alignment
      (Misc   : access Gtk_Misc_Record;
       Xalign : out Gfloat;
       Yalign : out Gfloat);
   procedure Set_Alignment
      (Misc   : access Gtk_Misc_Record;
       Xalign : Gfloat;
       Yalign : Gfloat);
   --  Modify the alignment for the widget. Xalign and Yalign are both values
   --  between 0.0 and 1.0 that specify the alignment: if Xalign is 0.0, the
   --  widget will be left aligned; if it is 0.5, the widget will be centered;
   --  if it is 1.0 the widget will be right aligned. Yalign is from top (0.0)
   --  to bottom (1.0). Both Xalign and Yalign will be constrained to the range
   --  0.0 .. 1.0 Note that if the widget fills its allocated area, these two
   --  parameters won't have any effect.

   procedure Get_Padding
      (Misc : access Gtk_Misc_Record;
       Xpad : out Gint;
       Ypad : out Gint);
   procedure Set_Padding
      (Misc : access Gtk_Misc_Record;
       Xpad : Gint;
       Ypad : Gint);
   --  Set the padding (i.e. the extra spaces on the side of the widget). If
   --  Xpad or Ypad is negative, they will be changed to 0.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Misc_Record, Gtk_Misc);
   function "+"
     (Widget : access Gtk_Misc_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Misc
   renames Implements_Buildable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Xalign_Property
   --  Type: Gfloat
   --  Flags: read-write
   --
   --  Name: Xpad_Property
   --  Type: Gint
   --  Flags: read-write
   --
   --  Name: Yalign_Property
   --  Type: Gfloat
   --  Flags: read-write
   --
   --  Name: Ypad_Property
   --  Type: Gint
   --  Flags: read-write

   Xalign_Property : constant Glib.Properties.Property_Float;
   Xpad_Property : constant Glib.Properties.Property_Int;
   Yalign_Property : constant Glib.Properties.Property_Float;
   Ypad_Property : constant Glib.Properties.Property_Int;

private
   Xalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("xalign");
   Xpad_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("xpad");
   Yalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("yalign");
   Ypad_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("ypad");
end Gtk.Misc;
