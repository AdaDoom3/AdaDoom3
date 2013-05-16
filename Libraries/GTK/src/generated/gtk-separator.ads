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
--  A separator is a vertical or horizontal line that can be displayed between
--  widgets, to provide visual grouping of the widgets into meaningful groups.
--  It is for instance used in dialogs to isolate the actual contents of the
--  dialogs and the various buttons to acknowledge the dialog (OK, Cancel,...)
--
--  </description>
--  <screenshot>gtk-separator</screenshot>
--  <group>Ornaments</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;           use Glib;
with Glib.Types;     use Glib.Types;
with Gtk.Buildable;  use Gtk.Buildable;
with Gtk.Enums;      use Gtk.Enums;
with Gtk.Orientable; use Gtk.Orientable;
with Gtk.Widget;     use Gtk.Widget;

package Gtk.Separator is

   type Gtk_Separator_Record is new Gtk_Widget_Record with null record;
   type Gtk_Separator is access all Gtk_Separator_Record'Class;

   subtype Gtk_Vseparator_Record is Gtk_Separator_Record;
   subtype Gtk_Vseparator is Gtk_Separator;

   subtype Gtk_Hseparator_Record is Gtk_Separator_Record;
   subtype Gtk_Hseparator is Gtk_Separator;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_separator_get_type");

   procedure Gtk_New_Vseparator (Separator : out Gtk_Vseparator);
   procedure Initialize_Vseparator
      (Separator : access Gtk_Vseparator_Record'Class);
   --  Creates a new Gtk.Separator.Gtk_Vseparator.

   function Vseparator_Get_Type return Glib.GType;
   pragma Import (C, Vseparator_Get_Type, "gtk_vseparator_get_type");

   procedure Gtk_New_Hseparator (Separator : out Gtk_Hseparator);
   procedure Initialize_Hseparator
      (Separator : access Gtk_Hseparator_Record'Class);

   function Hseparator_Get_Type return Glib.GType;
   pragma Import (C, Hseparator_Get_Type, "gtk_hseparator_get_type");

   ---------------------
   -- Interfaces_Impl --
   ---------------------

   function Get_Orientation
      (Self : access Gtk_Separator_Record) return Gtk.Enums.Gtk_Orientation;
   procedure Set_Orientation
      (Self        : access Gtk_Separator_Record;
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
     (Gtk.Buildable.Gtk_Buildable, Gtk_Separator_Record, Gtk_Separator);
   function "+"
     (Widget : access Gtk_Separator_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Separator
   renames Implements_Buildable.To_Object;

   package Implements_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Separator_Record, Gtk_Separator);
   function "+"
     (Widget : access Gtk_Separator_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Separator
   renames Implements_Orientable.To_Object;

end Gtk.Separator;
