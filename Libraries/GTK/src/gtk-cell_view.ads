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
--  A Gtk_Cell_View displays a single row of a Gtk_Tree_Model, using cell
--  renderers just like Gtk_Tree_View. Gtk_Cell_View doesn't support some of
--  the more complex features of Gtk_Tree_View, like cell editing and drag and
--  drop.
--  </description>
--  <c_version>2.14</c_version>
--  <group>Trees and Lists</group>
--  <testgtk>create_cell_view.adb</testgtk>

with Glib.Properties;
with Glib.Types;
with Gdk.Color;
with Gdk.Pixbuf;
with Gtk.Cell_Renderer;
with Gtk.Cell_Layout;
with Gtk.Tree_Model;
with Gtk.Widget;

package Gtk.Cell_View is
   type Gtk_Cell_View_Record is new Gtk.Widget.Gtk_Widget_Record
      with null record;
   type Gtk_Cell_View is access all Gtk_Cell_View_Record'Class;

   procedure Gtk_New    (View : out Gtk_Cell_View);
   procedure Initialize (View : access Gtk_Cell_View_Record'Class);
   --  Creates or initializes a new Gtk_Cell_View

   procedure Gtk_New_With_Text (View : out Gtk_Cell_View; Text : String);
   procedure Initialize_With_Text
     (View : access Gtk_Cell_View_Record'Class; Text : String);
   --  Creates a new Gtk_Cell_View widget, adds a Gtk_Cell_Renderer_Text to it,
   --  and makes it show Text.

   procedure Gtk_New_With_Markup (View : out Gtk_Cell_View; Markup : String);
   procedure Initialize_With_Markup
     (View : access Gtk_Cell_View_Record'Class; Markup : String);
   --  Creates a new Gtk_Cell_View widget, adds a Gtk_Cell_Renderer_Text to it,
   --  and makes its show Markup. The text can be marked up with the Pango text
   --  markup language.

   procedure Gtk_New_With_Pixbuf
     (View : out Gtk_Cell_View; Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf);
   procedure Initialize_With_Pixbuf
     (View   : access Gtk_Cell_View_Record'Class;
      Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf);
   --  Creates a new Gtk_Cell_View widget, adds a Gtk_Cell_Renderer_Pixbuf
   --  to it, and makes its show Pixbuf.

   function Get_Type return Glib.GType;
   --  Returns the internal value used for Gtk_Cell_View

   procedure Set_Displayed_Row
     (Cell_View : access Gtk_Cell_View_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path);
   --  Sets the row of the model that is currently displayed by the
   --  Gtk_Cell_View. If the path is unset, then the contents of the cellview
   --  "stick" at their last value; this is not normally a desired result, but
   --  may be a needed intermediate state if say, the model for the
   --  Gtk_Cell_View becomes temporarily empty.

   function Get_Displayed_Row
     (Cell_View : access Gtk_Cell_View_Record)
      return Gtk.Tree_Model.Gtk_Tree_Path;
   --  Returns a Gtk_Tree_Path referring to the currently displayed row. If no
   --  row is currently displayed, null is returned.

   function Get_Size_Of_Row
     (Cell_View   : access Gtk_Cell_View_Record;
      Path        : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Widget.Gtk_Requisition;
   --  Return the size needed by Cell_View to display the model row pointed to
   --  by Path.

   procedure Set_Background_Color
     (Cell_View : access Gtk_Cell_View_Record;
      Color     : Gdk.Color.Gdk_Color);
   --  Sets the background color of View.

   procedure Set_Model
     (Cell_View : access Gtk_Cell_View_Record;
      Model     : Gtk.Tree_Model.Gtk_Tree_Model);
   --  Sets the model for Cell_View.  If Cell_View already has a model
   --  set, it will remove it before setting the new model.  If Model is
   --  null, then it will unset the old model.

   function Get_Model
     (Cell_View : access Gtk_Cell_View_Record)
      return Gtk.Tree_Model.Gtk_Tree_Model;
   --  Returns the model for Cell_View.  If no model is used, null is
   --  returned.

   function Get_Cell_Renderers
     (Cell_View : access Gtk_Cell_View_Record)
     return Gtk.Cell_Renderer.Cell_Renderer_List.Glist;
   --  Returns the cell renderers which have been added to Cell_View.
   --  Return value: a list of cell renderers.
   --  The list must be freed by the caller.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Cell_Layout"
   --    This interface should be used to add new renderers to the view, to
   --    render various columns of the model

   package Implements_Cell_Layout is new Glib.Types.Implements
     (Gtk.Cell_Layout.Gtk_Cell_Layout, Gtk_Cell_View_Record, Gtk_Cell_View);
   function "+"
     (View : access Gtk_Cell_View_Record'Class)
      return Gtk.Cell_Layout.Gtk_Cell_Layout
      renames Implements_Cell_Layout.To_Interface;
   function "-"
     (Layout : Gtk.Cell_Layout.Gtk_Cell_Layout)
      return Gtk_Cell_View
      renames Implements_Cell_Layout.To_Object;
   --  Converts to and from the Gtk_Cell_Layout interface

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.

   --  <properties>
   --  Name:  Background_Property
   --  Type:  String`
   --  Descr: Background color as a string
   --
   --  Name:  Background_Gdk_Property
   --  Type:  Boxed
   --  Descr: Background color as a GdkColor
   --
   --  Name:  Model_Property
   --  Type:  Object
   --  Descr: The model for cell view
   --  </properties>

   Background_Property     : constant Glib.Properties.Property_String;
   --  Background_Gdk_Property : constant Glib.Properties.Property_Boxed;
   Model_Property : constant Glib.Properties.Property_Object;

private
   pragma Import (C, Get_Type, "gtk_cell_view_get_type");

   Background_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("background");
   Model_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("model");
--     Background_Gdk_Property : constant Glib.Properties.Property_Boxed :=
--       Glib.Properties.Build ("background-gdk");

end Gtk.Cell_View;
