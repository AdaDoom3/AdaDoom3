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
--  Gtk_Arrow should be used to draw simple arrows that need to point in one
--  of the four cardinal directions (up, down, left, or right). The style of
--  the arrow can be one of shadow in, shadow out, etched in, or etched out.
--  Note that these directions and style types may be ammended in versions of
--  Gtk to come.
--
--  Gtk_Arrow will fill any space alloted to it, but since it is inherited
--  from Gtk_Misc, it can be padded and/or aligned, to fill exactly the space
--  you desire.
--
--  Arrows are created with a call to Gtk_New. The direction or style of an
--  arrow can be changed after creation by using Set.
--
--  </description>
--  <screenshot>gtk-arrow</screenshot>
--  <testgtk>create_arrow.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;          use Glib;
with Glib.Types;    use Glib.Types;
with Gtk.Buildable; use Gtk.Buildable;
with Gtk.Enums;     use Gtk.Enums;
with Gtk.Misc;      use Gtk.Misc;
with Gtk.Widget;    use Gtk.Widget;

package Gtk.Arrow is

   type Gtk_Arrow_Record is new Gtk_Misc_Record with null record;
   type Gtk_Arrow is access all Gtk_Arrow_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Arrow       : out Gtk_Arrow;
       Arrow_Type  : Gtk.Enums.Gtk_Arrow_Type;
       Shadow_Type : Gtk.Enums.Gtk_Shadow_Type);
   procedure Initialize
      (Arrow       : access Gtk_Arrow_Record'Class;
       Arrow_Type  : Gtk.Enums.Gtk_Arrow_Type;
       Shadow_Type : Gtk.Enums.Gtk_Shadow_Type);
   --  Creates a new Gtk.Arrow.Gtk_Arrow widget.
   --  "arrow_type": a valid Gtk.Enums.Gtk_Arrow_Type.
   --  "shadow_type": a valid Gtk.Enums.Gtk_Shadow_Type.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_arrow_get_type");

   -------------
   -- Methods --
   -------------

   procedure Set
      (Arrow       : access Gtk_Arrow_Record;
       Arrow_Type  : Gtk.Enums.Gtk_Arrow_Type;
       Shadow_Type : Gtk.Enums.Gtk_Shadow_Type);
   --  Sets the direction and style of the Gtk.Arrow.Gtk_Arrow, Arrow.
   --  "arrow_type": a valid Gtk.Enums.Gtk_Arrow_Type.
   --  "shadow_type": a valid Gtk.Enums.Gtk_Shadow_Type.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Arrow_Record, Gtk_Arrow);
   function "+"
     (Widget : access Gtk_Arrow_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Arrow
   renames Implements_Buildable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Arrow_Type_Property
   --  Type: Gtk.Enums.Gtk_Arrow_Type
   --  Flags: read-write
   --
   --  Name: Shadow_Type_Property
   --  Type: Gtk.Enums.Gtk_Shadow_Type
   --  Flags: read-write

   Arrow_Type_Property : constant Gtk.Enums.Property_Gtk_Arrow_Type;
   Shadow_Type_Property : constant Gtk.Enums.Property_Gtk_Shadow_Type;

private
   Arrow_Type_Property : constant Gtk.Enums.Property_Gtk_Arrow_Type :=
     Gtk.Enums.Build ("arrow-type");
   Shadow_Type_Property : constant Gtk.Enums.Property_Gtk_Shadow_Type :=
     Gtk.Enums.Build ("shadow-type");
end Gtk.Arrow;
