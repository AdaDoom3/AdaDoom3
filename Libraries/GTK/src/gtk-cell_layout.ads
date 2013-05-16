-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2006-2013, AdaCore                   --
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
--  Gtk_Cell_Layout is an interface to be implemented by all objects which want
--  to provide a Gtk_Tree_View_Column like API for packing cells, setting
--  attributes and data funcs.
--  The rendering of the widget is done through various Gtk_Cell_Renderer, and
--  by reading data from a Gtk_Tree_Model.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Trees and Lists</group>
--  <testgtk>create_cell_view.adb</testgtk>

with Glib.Types;
with Gtk.Cell_Renderer;
with Gtk.Tree_Model;

package Gtk.Cell_Layout is

   type Gtk_Cell_Layout is new Glib.Types.GType_Interface;
   --  An interface (similar to Java's interfaces) that can be implemented by
   --  tagged types derived from Glib.Object.GObject.

   function Get_Type return Glib.GType;
   --  Returns the internal type used for a Gtk_Cell_Layout interface

   procedure Pack_Start
     (Cell_Layout : Gtk_Cell_Layout;
      Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Expand      : Boolean);
   procedure Pack_End
     (Cell_Layout : Gtk_Cell_Layout;
      Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Expand      : Boolean);
   --  Adds Cell to the beginning or end of Cell_Layout. If Expand is False,
   --  then the Cell is allocated no more space than it needs. Any unused space
   --  is divided evenly between cells for which Expand is True. Note that
   --  reusing the same cell renderer is not supported.

   procedure Add_Attribute
     (Cell_Layout : Gtk_Cell_Layout;
      Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Attribute   : String;
      Column      : Gint);
   --  Adds an attribute mapping to the list in Cell_Layout. Column is the
   --  column of the model to get a value from, and Attribute is the parameter
   --  on Cell to be set from the value. So for example if column of the model
   --  contains strings, you could have the "text" attribute of
   --  Gtk_Cell_Renderer_Text get its values from column 2.
   --  To mark rows as insensitive, create a column containing booleans in
   --  your model, and add an attribute "sensitive" to your renderer that
   --  points to this column.

   procedure Clear (Cell_Layout : Gtk_Cell_Layout);
   --  Unsets all the mappings on all renderers on Cell_Layout and
   --  removes all renderers from Cell_Layout.

   procedure Clear_Attributes
     (Cell_Layout : Gtk_Cell_Layout;
      Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);
   --  Clears all existing attributes previously set with Add_Attribute.

   procedure Reorder
     (Cell_Layout : Gtk_Cell_Layout;
      Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Position    : Gint);
   --  Re-inserts Cell at Position. Note that Cell has already to be packed
   --  into Cell_layout for this to function properly.

   type Cell_Data_Func is access procedure
     (Cell_Layout : Gtk_Cell_Layout;
      Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Model       : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  This subprogram can be used to globally modify an attribute of the
   --  Cell renderer.
   --  It should set the attributes of Cell as appropriate for this tree iter.

   procedure Set_Cell_Data_Func
     (Cell_Layout : Gtk_Cell_Layout;
      Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Func        : Cell_Data_Func);
   --  Sets the Cell_Data_Func to use for Cell_Layout. This function is used
   --  instead of the standard attributes mapping for setting the column value,
   --  and should set the value of Cell_layout's cell renderer(s) as
   --  appropriate. Func may be null to remove and older one.
   --  This allows you to compute the attributes dynamically from several
   --  columns of the model for instance

   generic
      type Data_Type (<>) is private;
   package Cell_Data_Functions is
      type Cell_Data_Func is access procedure
        (Cell_Layout : Gtk_Cell_Layout;
         Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
         Model       : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
         Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
         Data        : Data_Type);

      type Destroy_Notify is access procedure (Data : in out Data_Type);
      --  Free the memory used by Data

      procedure Set_Cell_Data_Func
        (Cell_Layout : Gtk_Cell_Layout;
         Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
         Func        : Cell_Data_Func;
         Data        : Data_Type;
         Destroy     : Destroy_Notify := null);
      --  Same as the other Set_Cell_Data_Func, but passes an addition user
      --  data to the callback.

   private
      --  <doc_ignore>
      type Data_Type_Access is access Data_Type;

      type Data_Type_Record is record
         Func    : Cell_Data_Func;
         Destroy : Destroy_Notify;
         Data    : Data_Type_Access;
      end record;
      type Data_Type_Record_Access is access Data_Type_Record;
      pragma Convention (C, Data_Type_Record_Access);

      procedure Internal_Destroy_Notify (Data : Data_Type_Record_Access);
      pragma Convention (C, Internal_Destroy_Notify);

      procedure Internal_Data_Cell_Data_Func
        (Cell_Layout       : Gtk_Cell_Layout;
         Cell, Model, Iter : System.Address;
         Data              : Data_Type_Record_Access);
      pragma Convention (C, Internal_Data_Cell_Data_Func);
      --  </doc_ignore>
   end Cell_Data_Functions;

private
   pragma Import (C, Get_Type, "gtk_cell_layout_get_type");
   pragma Import (C, Clear, "gtk_cell_layout_clear");
end Gtk.Cell_Layout;

--  No binding: gtk_cell_layout_set_attributes
