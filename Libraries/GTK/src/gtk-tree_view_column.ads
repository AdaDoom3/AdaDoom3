-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                 Copyright (C) 2001-2013, AdaCore                  --
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
--  General organization of the tree_view widgets:
--
--  <example>
--    ______________Tree_View___________________________________
--   |   _________________________     ________________________|
--   |  |_____Tree_View_Column1__|    |___Tree_View_Column2 __||
--   |  |                        |    |                       ||
--   |  |  ----------- ---------||    |                       ||
--   |  |  |Renderer1| |render2 ||    |                       ||
--   |  |  |         | |        ||    |                       ||
--   |  |  |         | |        ||    |                       ||
--   |  |  |         | |        ||    |                       ||
--   |  |  |---------| |--------||    |                       ||
--   |  |________________________|    |_______________________||
--   |_________________________________________________________|
--
--  </example>
--  A tree view can contain multiple physical columns on the screen. These
--  columns can have a button at the top, typically to force an ordering
--  of the tree). They can also be reorganized interactively by the user.
--
--  Each physical column can display several information, like buttons,
--  strings, ... Each of this display comes from a cell_renderer, that displays
--  some data it reads from the model associated with the tree_view.
--
--  The renderers are then divided into lines, which are typically pointed to
--  by iterators (Gtk_Tree_Iter).
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Trees and Lists</group>

with Glib.Properties;
with Glib.Generic_Properties;
with Glib.Object;
with Gdk.Rectangle;
with Gtk;
with Gtk.Cell_Renderer;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Tree_Model;
with Gtk.Widget;

with Glib.Glist;
pragma Elaborate_All (Glib.Glist);
with Ada.Unchecked_Conversion;

package Gtk.Tree_View_Column is

   type Gtk_Tree_View_Column_Record is
     new Glib.Object.GObject_Record with private;
   type Gtk_Tree_View_Column is access all Gtk_Tree_View_Column_Record'Class;

   function Convert is new Ada.Unchecked_Conversion
     (Gtk_Tree_View_Column, System.Address);
   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Tree_View_Column);
   package Column_List is new Glib.Glist.Generic_List (Gtk_Tree_View_Column);

   type Gtk_Tree_View_Column_Sizing is
     (Tree_View_Column_Grow_Only,
      Tree_View_Column_Autosize,
      Tree_View_Column_Fixed);
   pragma Convention (C, Gtk_Tree_View_Column_Sizing);

   procedure Gtk_New (Widget : out Gtk_Tree_View_Column);
   procedure Initialize (Widget : access Gtk_Tree_View_Column_Record'Class);
   --  Creates or initializes a new Gtk_Tree_View_Column.

   function Get_Type return Glib.GType;
   --  Return the internal value associated with this widget.

   --  function Get_Tree_View
   --    (Tree_Column : access Gtk_Tree_View_Column_Record)
   --     return Gtk_Tree_View;
   --  Returns the Gtk_Tree_View wherein Tree_Column has been inserted.
   --  This function has been relocated to the Gtk.Tree_View package to
   --  avoid a dependency circularity.

   ---------------------------------------
   -- Visual representation of the data --
   ---------------------------------------
   --  All the cells in a column have a similar graphical representation. This
   --  could be either a simple text, an editable text, a toggle button, ...
   --  This visual representation is independent from the actual data to
   --  represent. For instance, the same data from the model could be used for
   --  two different columns, once for a text and once for a button.
   --
   --  The visual representation is specified through a "renderer". See the
   --  various Gtk.Cell_Renderer* packages for more information on the
   --  available renderers.
   --
   --  Note that the same renderer can be used for multiple columns, even
   --  though its properties can be different each time. This means that for
   --  instance you can instantiate only one Gtk_Cell_Renderer_Text, and use it
   --  for all the columns that need to display text.

   procedure Pack_Start
     (Tree_Column : access Gtk_Tree_View_Column_Record;
      Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Expand      : Boolean);
   --  Add a renderer to the Tree_Column.
   --  Multiple renderers can be put in a specific column, and each of them can
   --  be associated with different data from the model. This provides a very
   --  powerful way to display different data in the same column.

   procedure Pack_End
     (Tree_Column : access Gtk_Tree_View_Column_Record;
      Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Expand      : Boolean);
   --  Same as the above. See the description of Pack_Start and Pack_End in
   --  Gtk.Box for the precise difference between the two

   procedure Clear (Tree_Column : access Gtk_Tree_View_Column_Record);
   --  Remove all the renderers set in the column.
   --  The column will always be empty until you put some new renderers.

   function Get_Cell_Renderers
     (Tree_Column : access Gtk_Tree_View_Column_Record)
      return Gtk.Cell_Renderer.Cell_Renderer_List.Glist;
   pragma Obsolescent (Get_Cell_Renderers);
   --  Return the list of cell renderers set in the column. The returned list
   --  must be freed by the caller.

   ------------------------------------
   -- Specifying the data to display --
   ------------------------------------
   --  The data to display in a column is always read from the model associated
   --  with the tree. In some cases (like if you are using the Gtk_Tree_Store
   --  model), this means that is has to be physically stored in a data
   --  structure. However, if you define your own models, you could also
   --  compute it on the fly.
   --
   --  For instance, if you have a database that contains some distance and
   --  time information, and you want to display the speed in a tree view: if
   --  you are using a Gtk_Tree_Store model, you have to create a third column
   --  in the model to store the string, and have a renderer point to that
   --  third column.
   --
   --  However, if you are using your own model, it is conceivable that the
   --  speed is computed on the fly from the distance and time.
   --
   --  The subprograms below use two or three parameters to precisely identify
   --  the part of the tree they impact: the column, the renderer in the
   --  column, and in some cases the specific line.
   --
   --  A renderer is always associated with a column in the model (even if that
   --  is a virtual column not associated with physical data). This is done
   --  through the Add_Attribute subprogram. This will read the data from the
   --  model. The type of the data read depends on the type of the column in
   --  the model.
   --  The type of data that Add_Attribute excepts to find in the column is
   --  documented in the packages for each of the renderer.

   procedure Add_Attribute
     (Tree_Column   : access Gtk_Tree_View_Column_Record;
      Cell_Renderer : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Attribute     : String;
      Column        : Gint);
   --  Add an attribute mapping to the list in Tree_Column.
   --  The Column is the column of the model to get a value from, and the
   --  Attribute is the parameter on Cell_Renderer to be set from the value. So
   --  for example if column 2 of the model contains strings, you could have
   --  the "text" attribute of a Gtk_Cell_Renderer_Text get its values from
   --  column 2.
   --
   --  For a list of properties available for each Cell_Renderer, please
   --  refer to the corresponding package specifications.
   --
   --  See also the function Set_Cell_Data_Func for another way to query the
   --  data to display in the tree.

   type Cell_Data_Func is access procedure
     (Tree_Column : access Gtk_Tree_View_Column_Record'Class;
      Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Model       : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  This subprogram can be used to globally modify an attribute of the
   --  Cell renderer.
   --  It is called every time some event happens in the tree (a line was
   --  clicked, the mouse moved into or out of a line,...). Iter and
   --  Tree_Column point to the location in the tree that received the event.

   procedure Set_Cell_Data_Func
     (Tree_Column : access Gtk_Tree_View_Column_Record;
      Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Func        : Cell_Data_Func);
   --  Set the function to use for the column.
   --  This function is used instead of the standard attributes mapping for
   --  setting the column value, and should set the value of Tree_Column's cell
   --  renderer as appropriate.
   --  Func may be null to remove an older one.
   --  It should be used when values from the model should be computed from
   --  application-specific data structures rather than stored in the model.

   generic
      type Data_Type (<>) is private;
   package Cell_Data_Functions is
      type Cell_Data_Func is access procedure
        (Tree_Column : access Gtk_Tree_View_Column_Record'Class;
         Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
         Model       : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
         Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
         Data        : Data_Type);

      procedure Set_Cell_Data_Func
        (Tree_Column : access Gtk_Tree_View_Column_Record'Class;
         Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
         Func        : Cell_Data_Func;
         Data        : Data_Type);

   private
      --  <doc_ignore>
      type Data_Type_Access is access Data_Type;

      type Data_Type_Record is record
         Func : Cell_Data_Func;
         Data : Data_Type_Access;
      end record;
      type Data_Type_Record_Access is access Data_Type_Record;
      pragma Convention (C, Data_Type_Record_Access);

      procedure Internal_Destroy_Notify (Data : Data_Type_Record_Access);
      pragma Convention (C, Internal_Destroy_Notify);

      procedure Internal_Data_Cell_Data_Func
        (Tree_Column, Cell, Model, Iter : System.Address;
         Data : Data_Type_Record_Access);
      pragma Convention (C, Internal_Data_Cell_Data_Func);
      --  </doc_ignore>
   end Cell_Data_Functions;

   procedure Clear_Attributes
     (Tree_Column   : access Gtk_Tree_View_Column_Record;
      Cell_Renderer : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);
   --  Clear all existing attributes previously set with
   --  Gtk.Tree_View_Column.Set_Attributes.

   ------------------------------------------
   -- Options for manipulating the columns --
   ------------------------------------------

   procedure Set_Spacing
     (Tree_Column : access Gtk_Tree_View_Column_Record;
      Spacing     : Gint);
   function Get_Spacing
     (Tree_Column : access Gtk_Tree_View_Column_Record) return Gint;
   --  Set the spacing field of Tree_Column.
   --  The spacing field is the number of pixels to place between cell
   --  renderers packed into it.

   procedure Set_Visible
     (Tree_Column : access Gtk_Tree_View_Column_Record;
      Visible     : Boolean);
   function Get_Visible
     (Tree_Column : access Gtk_Tree_View_Column_Record) return Boolean;
   --  Set the visibility of Tree_Column.

   procedure Set_Resizable
     (Tree_Column : access Gtk_Tree_View_Column_Record;
      Resizable   : Boolean);
   function Get_Resizable
     (Tree_Column : access Gtk_Tree_View_Column_Record) return Boolean;
   --  Set whether the Tree_Column is resizable.

   procedure Set_Sizing
     (Tree_Column : access Gtk_Tree_View_Column_Record;
      The_Type    : Gtk_Tree_View_Column_Sizing);
   function Get_Sizing
     (Tree_Column : access Gtk_Tree_View_Column_Record)
      return Gtk_Tree_View_Column_Sizing;
   --  Set the growth behavior of Tree_Column to The_Type.

   function Get_Width
     (Tree_Column : access Gtk_Tree_View_Column_Record) return Gint;
   --  Return the current size of the Tree_Column in pixels.

   procedure Queue_Resize
     (Tree_Column : access Gtk_Tree_View_Column_Record);
   --  Flags the column, and the cell renderers added to this column, to have
   --  their sizes renegotiated.

   procedure Set_Fixed_Width
     (Tree_Column : access Gtk_Tree_View_Column_Record;
      Fixed_Width : Gint);
   function Get_Fixed_Width
     (Tree_Column : access Gtk_Tree_View_Column_Record) return Gint;
   --  Set the size of the column in pixels.
   --  This is meaningful only if the sizing type is
   --  Gtk_Tree_View_Column_Fixed. In this case, the value is discarded as the
   --  size of the column is based on the calculated width of the column. The
   --  width is clamped to the min/max width for the column.
   --  The value returned by Get_Fixed_width may not be the actual width of the
   --  column on the screen, just what is requested.

   procedure Set_Min_Width
     (Tree_Column : access Gtk_Tree_View_Column_Record;
      Min_Width   : Gint);
   function Get_Min_Width
     (Tree_Column : access Gtk_Tree_View_Column_Record) return Gint;
   --  Set the minimum width of the Tree_Column.
   --  If Min_Width is -1, then the minimum width is unset.

   procedure Set_Max_Width
     (Tree_Column : access Gtk_Tree_View_Column_Record;
      Max_Width   : Gint);
   function Get_Max_Width
     (Tree_Column : access Gtk_Tree_View_Column_Record) return Gint;
   --  Set the maximum width of the Tree_Column.
   --  If Max_Width is -1, then the maximum width is unset.
   --  Note, the column can actually be wider than max width if it's the last
   --  column in a view. In this case, the column expands to fill the view.

   procedure Clicked (Tree_Column : access Gtk_Tree_View_Column_Record);
   --  Emit the "clicked" signal on the column.
   --  This function will only work if the user could have conceivably clicked
   --  on the button.

   procedure Set_Expand
     (Tree_Column : access Gtk_Tree_View_Column_Record; Expand : Boolean);
   function Get_Expand
     (Tree_Column : access Gtk_Tree_View_Column_Record) return Boolean;
   --  Sets the column to take available extra space. This space is shared
   --  equally amongst all columns that have the expand set to TRUE. If no
   --  column has this option set, then the last column gets all extra space.
   --  By default, every column is created with this FALSE.

   procedure Set_Title
     (Tree_Column : access Gtk_Tree_View_Column_Record;
      Title       : UTF8_String);
   function Get_Title
     (Tree_Column : access Gtk_Tree_View_Column_Record) return UTF8_String;
   --  Set the title of the Tree_Column.
   --  If a custom widget has been set, then this value is ignored.

   procedure Set_Clickable
     (Tree_Column : access Gtk_Tree_View_Column_Record;
      Clickable   : Boolean);
   function Get_Clickable
     (Tree_Column : access Gtk_Tree_View_Column_Record) return Boolean;
   --  Set the header to be active if Active is True.
   --  When the header is active, then it can take keyboard focus, and can be
   --  clicked.

   procedure Set_Widget
     (Tree_Column : access Gtk_Tree_View_Column_Record;
      Widget      : access Gtk.Widget.Gtk_Widget_Record'Class);
   function Get_Widget
     (Tree_Column : access Gtk_Tree_View_Column_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Return the Gtk_Widget in the button in the column header.
   --  If a custom widget has not been set, then this will be a Gtk_Alignment
   --  with a Gtk_Label in it.

   procedure Set_Alignment
     (Tree_Column : access Gtk_Tree_View_Column_Record;
      Xalign      : Gfloat);
   function Get_Alignment
     (Tree_Column : access Gtk_Tree_View_Column_Record) return Gfloat;
   --  Set the alignment of the title or custom widget inside the column header
   --  The alignment determines its location inside the button
   --  0.0 for left, 0.5 for center, 1.0 for right.

   procedure Set_Reorderable
     (Tree_Column : access Gtk_Tree_View_Column_Record;
      Reorderable : Boolean);
   function Get_Reorderable
     (Tree_Column : access Gtk_Tree_View_Column_Record) return Boolean;
   --  Whether this column can be drag-and-dropped to some other place in the
   --  tree.

   procedure Set_Sort_Column_Id
     (Tree_Column    : access Gtk_Tree_View_Column_Record;
      Sort_Column_Id : Gint);
   function Get_Sort_Column_Id
     (Tree_Column : access Gtk_Tree_View_Column_Record) return Gint;
   --  Set the logical model columns that this column sorts on when this
   --  column is selected for sorting. Doing so makes the column header
   --  clickable.
   --  Get_Sort_Column_Id returns -1 if this column can't be used for sorting.

   procedure Set_Sort_Indicator
     (Tree_Column : access Gtk_Tree_View_Column_Record;
      Setting     : Boolean);
   function Get_Sort_Indicator
     (Tree_Column : access Gtk_Tree_View_Column_Record) return Boolean;
   --  Call this function with a Setting of True to display an arrow in
   --  the header button indicating the column is sorted. Call
   --  Set_Sort_Order to change the direction of the arrow.

   procedure Set_Sort_Order
     (Tree_Column : access Gtk_Tree_View_Column_Record;
      Order       : Gtk_Sort_Type);
   function Get_Sort_Order
     (Tree_Column : access Gtk_Tree_View_Column_Record) return Gtk_Sort_Type;
   --  Change the appearance of the sort indicator.
   --  This does *not* actually sort the model. Use
   --  Gtk.Tree_View_Column.Set_Sort_Column_Id if you want automatic sorting
   --  support. This function is primarily for custom sorting behavior, and
   --  should be used in conjunction with Gtk.Tree_Sortable.Set_Sort_Column
   --  to do that. For custom models, the mechanism will vary. The sort
   --  indicator changes direction to indicate normal sort or reverse sort.
   --  Note that you must have the sort indicator enabled to see anything
   --  when calling this function; see Set_Sort_Indicator.

   procedure Cell_Set_Cell_Data
     (Tree_Column : access Gtk_Tree_View_Column_Record;
      Tree_Model  : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
      Is_Expander : Boolean;
      Is_Expanded : Boolean);
   --  Set the cell renderer based on the Tree_Model and Tree_Node.
   --  That is, for every attribute mapping in Tree_Column, it will get a
   --  value from the set column on the Tree_Node, and use that value to
   --  set the attribute on the cell renderer.  This is used primarily by
   --  the Gtk_Tree_View.

   procedure Cell_Get_Size
     (Tree_Column : access Gtk_Tree_View_Column_Record;
      Cell_Area   : Gdk.Rectangle.Gdk_Rectangle;
      X_Offset    : out Gint;
      Y_Offset    : out Gint;
      Width       : out Gint;
      Height      : out Gint);
   --  Obtain the width and height needed to render the column.
   --  This is used primarily by the Gtk_Tree_View.

   function Cell_Is_Visible
     (Tree_Column : access Gtk_Tree_View_Column_Record) return Boolean;
   --  Returns true if any of the cells packed in the column is visible

   procedure Cell_Get_Position
     (Tree_Column   : access Gtk_Tree_View_Column_Record;
      Cell_Renderer : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Start_Pos     : out Gint;
      Width         : out Gint;
      Success       : out Boolean);
   --  Obtains the horizontal position and size of a cell in a column. If the
   --  cell is not found in the column, start_pos and width are not changed
   --  and FALSE is returned.

   procedure Focus_Cell
     (Tree_Column : access Gtk_Tree_View_Column_Record;
      Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);
   --  Sets the current keyboard focus to be at Cell, if the column contains
   --  2 or more editable and activatable cells.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Alignment_Property
   --  Type:  Float
   --  Descr: X Alignment of the column header text or widget
   --
   --  Name:  Clickable_Property
   --  Type:  Boolean
   --  Descr: Whether the header can be clicked
   --
   --  Name:  Expand_Property
   --  Type:  Boolean
   --  Descr: Column gets share of extra width allocated to the widget
   --
   --  Name:  Fixed_Width_Property
   --  Type:  Int
   --  Descr: Current fixed width of the column
   --
   --  Name:  Max_Width_Property
   --  Type:  Int
   --  Descr: Maximum allowed width of the column
   --
   --  Name:  Min_Width_Property
   --  Type:  Int
   --  Descr: Minimum allowed width of the column
   --
   --  Name:  Reorderable_Property
   --  Type:  Boolean
   --  Descr: Whether the column can be reordered around the headers
   --
   --  Name:  Resizable_Property
   --  Type:  Boolean
   --  Descr: Column is user-resizable
   --
   --  Name:  Sizing_Property
   --  Type:  Enum
   --  Descr: Resize mode of the column
   --
   --  Name:  Sort_Indicator_Property
   --  Type:  Boolean
   --  Descr: Whether to show a sort indicator
   --
   --  Name:  Sort_Order_Property
   --  Type:  Enum
   --  Descr: Sort direction the sort indicator should indicate
   --
   --  Name:  Spacing_Property
   --  Type:  Int
   --  Descr: Space which is inserted between cells
   --
   --  Name:  Title_Property
   --  Type:  String
   --  Descr: Title to appear in column header
   --
   --  Name:  Visible_Property
   --  Type:  Boolean
   --  Descr: Whether to display the column
   --
   --  Name:  Widget_Property
   --  Type:  Object
   --  Descr: Widget to put in column header button instead of column title
   --
   --  Name:  Width_Property
   --  Type:  Int
   --  Descr: Current width of the column
   --
   --  </properties>

   package Column_Sizing_Properties is new
     Glib.Generic_Properties.Generic_Internal_Discrete_Property
       (Gtk_Tree_View_Column_Sizing);
   type Property_Column_Sizing is new Column_Sizing_Properties.Property;

   Alignment_Property      : constant Glib.Properties.Property_Float;
   Clickable_Property      : constant Glib.Properties.Property_Boolean;
   Expand_Property         : constant Glib.Properties.Property_Boolean;
   Fixed_Width_Property    : constant Glib.Properties.Property_Int;
   Max_Width_Property      : constant Glib.Properties.Property_Int;
   Min_Width_Property      : constant Glib.Properties.Property_Int;
   Reorderable_Property    : constant Glib.Properties.Property_Boolean;
   Resizable_Property      : constant Glib.Properties.Property_Boolean;
   Sizing_Property         : constant Property_Column_Sizing;
   Sort_Indicator_Property : constant Glib.Properties.Property_Boolean;
   Sort_Order_Property     : constant Gtk.Enums.Property_Sort_Type;
   Spacing_Property        : constant Glib.Properties.Property_Int;
   Title_Property          : constant Glib.Properties.Property_String;
   Visible_Property        : constant Glib.Properties.Property_Boolean;
   Widget_Property         : constant Glib.Properties.Property_Object;
   Width_Property          : constant Glib.Properties.Property_Int;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "clicked"
   --    procedure Handler (Widget : access Gtk_Tree_View_Column_Record'Class);
   --
   --  </signals>

   Signal_Clicked : constant Glib.Signal_Name := "clicked";

private
   type Gtk_Tree_View_Column_Record is
     new Glib.Object.GObject_Record with null record;

      Alignment_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("alignment");
   Clickable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("clickable");
   Expand_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("expand");
   Fixed_Width_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("fixed-width");
   Max_Width_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("max-width");
   Min_Width_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("min-width");
   Reorderable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("reorderable");
   Resizable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("resizable");
   Sizing_Property : constant Property_Column_Sizing :=
     Build ("sizing");
   Sort_Indicator_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("sort-indicator");
   Sort_Order_Property : constant Gtk.Enums.Property_Sort_Type :=
     Gtk.Enums.Build ("sort-order");
   Spacing_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("spacing");
   Title_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("title");
   Visible_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("visible");
   Widget_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("widget");
   Width_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("width");

   pragma Import (C, Get_Type, "gtk_tree_view_column_get_type");
end Gtk.Tree_View_Column;

--  No binding: gtk_tree_view_column_new_with_attributes
--  No binding: gtk_tree_view_column_set_attributes

--  Implemented in Gtk.Tree_View:
--  No binding: gtk_tree_view_column_get_tree_view
