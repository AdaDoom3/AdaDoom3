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
--  A Gtk_Cell_Renderer_Accel displays a keyboard accelerator (i.e. a key
--  combination like <Control>-a). If the cell renderer is editable, the
--  accelerator can be changed by simply typing the new combination.
--
--  The Gtk_Cell_Renderer_Accel cell renderer was added in GTK+ 2.10.
--  </description>
--  <c_version>2.16.6</c_version>

with Glib.Properties;
with Gtk.Cell_Renderer_Text;

package Gtk.Cell_Renderer_Accel is

   type Gtk_Cell_Renderer_Accel_Record is
     new Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text_Record with private;
   type Gtk_Cell_Renderer_Accel is
     access all Gtk_Cell_Renderer_Accel_Record'Class;

   procedure Gtk_New (Widget : out Gtk_Cell_Renderer_Accel);
   --  Creates a new Gtk_Cell_Renderer_Accel.

   procedure Initialize
     (Widget : access Gtk_Cell_Renderer_Accel_Record'Class);
   --  Creates a new Gtk_Cell_Renderer_Accel.

   function Get_Type return GType;
   --  Return the internal value associated with this widget.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  Name:  Accel_Key_Property
   --  Type:  Uint
   --  Descr: The keyval of the accelerator
   --
   --  Name:  Accel_Mode_Property
   --  Type:  Enum
   --  Descr: The type of accelerators
   --
   --  Name:  Accel_Mods_Property
   --  Type:  Flags
   --  Descr: The modifier mask of the accelerator
   --
   --  Name:  Keycode_Property
   --  Type:  Uint
   --  Descr: The hardware keycode of the accelerator
   --
   --  </properties>

   Accel_Key_Property  : constant Glib.Properties.Property_Uint;
   Accel_Mode_Property : constant Glib.Properties.Property_Enum;
   --  Accel_Mods_Property : constant Glib.Properties.Property_Flags;
   Keycode_Property    : constant Glib.Properties.Property_Uint;

private
   type Gtk_Cell_Renderer_Accel_Record is
     new Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text_Record with null record;

   Accel_Key_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("accel-key");
   Accel_Mode_Property : constant Glib.Properties.Property_Enum :=
     Glib.Properties.Build ("accel-mode");
   --  Accel_Mods_Property : constant Glib.Properties.Property_Flags :=
   --    Glib.Properties.Build ("accel-mods");
   Keycode_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("keycode");

   pragma Import (C, Get_Type, "gtk_cell_renderer_accel_get_type");
end Gtk.Cell_Renderer_Accel;
