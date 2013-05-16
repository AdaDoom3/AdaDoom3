-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--               Copyright (C) 2006-2013, AdaCore                    --
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
--  A Gtk_Cell_Renderer_Combo can be used to render a combobox in a cell.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Trees and Lists</group>

with Glib.Properties;
with Gtk.Cell_Renderer_Text;

package Gtk.Cell_Renderer_Combo is

   type Gtk_Cell_Renderer_Combo_Record is
     new Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text_Record with null record;
   type Gtk_Cell_Renderer_Combo is
     access all Gtk_Cell_Renderer_Combo_Record'Class;

   procedure Gtk_New    (Render : out Gtk_Cell_Renderer_Combo);
   procedure Initialize (Render : access Gtk_Cell_Renderer_Combo_Record'Class);
   --  Creates or initializes a new renderer.
   --  Adjust how text is drawn using object properties.
   --  Properties can be set either directly on the renderer, or in combination
   --  with a Gtk_Tree_View_Column so that the property is bound to a value
   --  in the Gtk_Tree_Model. For example, you can bind the "text" property
   --  on the cell renderer to a string value in the model, thus rendering
   --  a different string in each row of the Gtk_Tree_View.

   function Get_Type return GType;
   --  Returns the internal value associated with this renderer

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Has_Entry_Property
   --  Type:  Boolean
   --  Descr: If FALSE, don't allow to enter strings other than the chosen ones
   --
   --  Name:  Model_Property
   --  Type:  Object
   --  Descr: The model containing the possible values for the combo box
   --
   --  Name:  Text_Column_Property
   --  Type:  Int
   --  Descr: A column in the data source model to get the strings from
   --
   --  </properties>

   Has_Entry_Property   : constant Glib.Properties.Property_Boolean;
   Model_Property       : constant Glib.Properties.Property_Object;
   Text_Column_Property : constant Glib.Properties.Property_Int;

private
   pragma Import (C, Get_Type, "gtk_cell_renderer_combo_get_type");

   Has_Entry_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-entry");
   Model_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("model");
   Text_Column_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("text-column");

end Gtk.Cell_Renderer_Combo;
