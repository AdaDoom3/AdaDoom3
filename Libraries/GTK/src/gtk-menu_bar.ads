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
--  Gtk_Menu_Bar is a subclass of Gtk_Menu_Shell which contains one to many
--  Gtk_Menu_Item. The result is a standard menu bar which can hold many menu
--  items. Gtk_Menu_Bar allows for a shadow type to be set for aesthetic
--  purposes.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Menus and Toolbars</group>
--  <screenshot>gtk-menu_bar</screenshot>

with Glib.Properties;
with Gtk.Enums;
with Gtk.Menu_Shell;

package Gtk.Menu_Bar is

   type Gtk_Menu_Bar_Record is new
     Gtk.Menu_Shell.Gtk_Menu_Shell_Record with private;
   type Gtk_Menu_Bar is access all Gtk_Menu_Bar_Record'Class;

   procedure Gtk_New (Menu_Bar : out Gtk_Menu_Bar);
   --  Create a menu bar.

   procedure Initialize (Menu_Bar : access Gtk_Menu_Bar_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Menu_Bar.

   procedure Set_Child_Pack_Direction
     (Menubar        : access Gtk_Menu_Bar_Record;
      Child_Pack_Dir : Gtk.Enums.Gtk_Pack_Direction);
   function Get_Child_Pack_Direction
     (Menubar : access Gtk_Menu_Bar_Record)
      return Gtk.Enums.Gtk_Pack_Direction;
   --  Sets how widgets should be packed inside the children of a menubar.

   procedure Set_Pack_Direction
     (Menubar  : access Gtk_Menu_Bar_Record;
      Pack_Dir : Gtk.Enums.Gtk_Pack_Direction);
   function Get_Pack_Direction
     (Menubar : access Gtk_Menu_Bar_Record)
      return Gtk.Enums.Gtk_Pack_Direction;
   --  Sets how items should be packed inside a menubar.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.

   --  <properties>
   --  Name:  Child_Pack_Direction_Property
   --  Type:  Enum
   --  Descr: The child pack direction of the menubar
   --
   --  Name:  Pack_Direction_Property
   --  Type:  Enum
   --  Descr: The pack direction of the menubar
   --  </properties>

   Child_Pack_Direction_Property : constant Gtk.Enums.Property_Pack_Direction;
   Pack_Direction_Property       : constant Gtk.Enums.Property_Pack_Direction;

   ----------------------
   -- Style Properties --
   ----------------------
   --  The following properties can be changed through the gtk theme and
   --  configuration files, and retrieved through Gtk.Widget.Style_Get_Property

   --  <style_properties>
   --  Name:  Internal_Padding_Property
   --  Type:  Int
   --  Descr: Amount of border space between the menubar shadow and the menu
   --         items
   --
   --  Name:  Shadow_Type_Property
   --  Type:  Enum
   --  Descr: Style of bevel around the menubar
   --  </style_properties>

   Internal_Padding_Property : constant Glib.Properties.Property_Int;
   Shadow_Type_Property      : constant Gtk.Enums.Property_Gtk_Shadow_Type;

private
   type Gtk_Menu_Bar_Record is new Gtk.Menu_Shell.Gtk_Menu_Shell_Record
     with null record;

   Child_Pack_Direction_Property : constant Gtk.Enums.Property_Pack_Direction
     := Gtk.Enums.Build ("child-pack-direction");
   Pack_Direction_Property : constant Gtk.Enums.Property_Pack_Direction :=
     Gtk.Enums.Build ("pack-direction");

   Internal_Padding_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("internal-padding");
   Shadow_Type_Property : constant Gtk.Enums.Property_Gtk_Shadow_Type :=
     Gtk.Enums.Build ("shadow-type");

   pragma Import (C, Get_Type, "gtk_menu_bar_get_type");

end Gtk.Menu_Bar;
