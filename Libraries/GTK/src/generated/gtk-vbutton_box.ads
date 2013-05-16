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
--  A Gtk_Vbutton_Box is a specific Gtk_Button_Box that organizes its children
--  vertically. The beginning of the box (when you add children with
--  Gtk.Box.Pack_Start) is on the top of the box. Its end (for
--  Gtk.Box.Pack_End) is on the bottom.
--
--  </description>
--  <group>Layout containers</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;           use Glib;
with Glib.Types;     use Glib.Types;
with Gtk.Buildable;  use Gtk.Buildable;
with Gtk.Button_Box; use Gtk.Button_Box;
with Gtk.Enums;      use Gtk.Enums;
with Gtk.Orientable; use Gtk.Orientable;
with Gtk.Widget;     use Gtk.Widget;

package Gtk.Vbutton_Box is

   type Gtk_Vbutton_Box_Record is new Gtk_Button_Box_Record with null record;
   type Gtk_Vbutton_Box is access all Gtk_Vbutton_Box_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Widget : out Gtk_Vbutton_Box);
   procedure Initialize (Widget : access Gtk_Vbutton_Box_Record'Class);
   --  Creates a new vertical button box.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_vbutton_box_get_type");

   ---------------
   -- Functions --
   ---------------

   function Get_Layout_Default return Gtk.Enums.Gtk_Button_Box_Style;
   pragma Obsolescent (Get_Layout_Default);
   procedure Set_Layout_Default (Layout : Gtk.Enums.Gtk_Button_Box_Style);
   pragma Obsolescent (Set_Layout_Default);
   --  Sets a new layout mode that will be used by all button boxes.
   --  Deprecated since 2.0, Use Gtk.Button_Box.Set_Layout instead.
   --  "layout": a new Gtk.Enums.Gtk_Button_Box_Style.

   function Get_Spacing_Default return Gint;
   pragma Obsolescent (Get_Spacing_Default);
   procedure Set_Spacing_Default (Spacing : Gint);
   pragma Obsolescent (Set_Spacing_Default);
   --  Changes the default spacing that is placed between widgets in an
   --  vertical button box.
   --  Deprecated since 2.0, Use Gtk.Box.Set_Spacing instead.
   --  "spacing": an integer value.

   ---------------------
   -- Interfaces_Impl --
   ---------------------

   function Get_Orientation
      (Self : access Gtk_Vbutton_Box_Record)
       return Gtk.Enums.Gtk_Orientation;
   procedure Set_Orientation
      (Self        : access Gtk_Vbutton_Box_Record;
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
     (Gtk.Buildable.Gtk_Buildable, Gtk_Vbutton_Box_Record, Gtk_Vbutton_Box);
   function "+"
     (Widget : access Gtk_Vbutton_Box_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Vbutton_Box
   renames Implements_Buildable.To_Object;

   package Implements_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Vbutton_Box_Record, Gtk_Vbutton_Box);
   function "+"
     (Widget : access Gtk_Vbutton_Box_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Vbutton_Box
   renames Implements_Orientable.To_Object;

end Gtk.Vbutton_Box;
