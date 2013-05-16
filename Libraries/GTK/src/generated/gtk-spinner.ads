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
--  A Gtk_Spinner widget displays an icon-size spinning animation. It is often
--  used as an alternative to a Gtk_Progress for displaying indefinite
--  activity, instead of actual progress. To start the animation, use
--  Gtk.Spinner.Start; to stop it use Gtk.Spinner.Stop.
--
--  </description>
--  <group>Ornaments</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;             use Glib;
with Glib.Properties;  use Glib.Properties;
with Glib.Types;       use Glib.Types;
with Gtk.Buildable;    use Gtk.Buildable;
with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.Widget;       use Gtk.Widget;

package Gtk.Spinner is

   type Gtk_Spinner_Record is new Gtk_Drawing_Area_Record with null record;
   type Gtk_Spinner is access all Gtk_Spinner_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Spinner : out Gtk_Spinner);
   procedure Initialize (Spinner : access Gtk_Spinner_Record'Class);
   --  Returns a new spinner widget. Not yet started.
   --  Since: gtk+ 2.20

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_spinner_get_type");

   -------------
   -- Methods --
   -------------

   procedure Start (Spinner : access Gtk_Spinner_Record);
   --  Starts the animation of the spinner.
   --  Since: gtk+ 2.20

   procedure Stop (Spinner : access Gtk_Spinner_Record);
   --  Stops the animation of the spinner.
   --  Since: gtk+ 2.20

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Spinner_Record, Gtk_Spinner);
   function "+"
     (Widget : access Gtk_Spinner_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Spinner
   renames Implements_Buildable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Active_Property
   --  Type: Boolean
   --  Flags: read-write

   Active_Property : constant Glib.Properties.Property_Boolean;

private
   Active_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("active");
end Gtk.Spinner;
