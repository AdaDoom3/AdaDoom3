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
--  A Gtk_Combo_Box is a widget that allows the user to choose from a list of
--  valid choices. The Gtk_Combo_Box displays the selected choice. When
--  activated, the Gtk_Combo_Box displays a popup which allows the user to make
--  new choice. The style in which the selected value is displayed, and the
--  style of the popup is determined by the current theme. It may be similar to
--  a Gtk_Option_Menu, or similar to a Windows-style combo box.
--
--  Unlike its predecessors Gtk.Combo.Gtk_Combo and
--  Gtk.Option_Menu.Gtk_Option_Menu, the Gtk_Combo_Box uses the model-view
--  pattern; the list of valid choices is specified in the form of a tree
--  model, and the display of the choices can be adapted to the data in the
--  model by using cell renderers, as you would in a tree view. This is
--  possible since Gtk_Combo_Box implements the Gtk_Cell_Layout interface. The
--  tree model holding the valid choices is not restricted to a flat list, it
--  can be a real tree, and the popup will reflect the tree structure.
--
--  In addition to the model-view API, Gtk_Combo_Box offers a simple API which
--  is suitable for text-only combo boxes, and hides the complexity of managing
--  the data in a model.
--  </description>
--  <c_version>2.16.6</c_version>
--  <group>Trees and Lists</group>
--  <see>Gtk.Combo_Box_Entry</see>
--  <screenshot>gtk-combo_box</screenshot>

with Glib.Properties;
with Gtk.Bin;
with Gtk.Cell_Editable;
with Gtk.Cell_Layout;
with Gtk.Enums;
with Gtk.Tree_Model;
with Gtk.Tree_View;
with Glib.Types;

package Gtk.Combo_Box is
   type Gtk_Combo_Box_Record is new Gtk.Bin.Gtk_Bin_Record with null record;
   type Gtk_Combo_Box is access all Gtk_Combo_Box_Record'Class;

   procedure Gtk_New    (Combo : out Gtk_Combo_Box);
   procedure Initialize (Combo : access Gtk_Combo_Box_Record'Class);
   --  Creates or initializes a new empty combo

   procedure Gtk_New_With_Model
     (Combo : out Gtk_Combo_Box;
      Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class);
   procedure Initialize_With_Model
     (Combo : access Gtk_Combo_Box_Record'Class;
      Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class);
   --  Creates or initializes a new combo initializes to Model.

   procedure Gtk_New_With_Entry (Combo : out Gtk_Combo_Box);
   procedure Initialize_With_Entry (Combo : access Gtk_Combo_Box_Record'Class);
   --  Creates or initializes a new combo with entry

   procedure Gtk_New_With_Model_And_Entry
     (Combo : out Gtk_Combo_Box;
      Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class);
   procedure Initialize_With_Model_And_Entry
     (Combo : access Gtk_Combo_Box_Record'Class;
      Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class);
   --  Creates or initializes a new combo with entry, initialized with Model

   function Get_Type return Glib.GType;
   --  Returns the internal value used for Gtk_Combo_Box widgets

   procedure Set_Model
     (Combo_Box : access Gtk_Combo_Box_Record;
      Model     : Gtk.Tree_Model.Gtk_Tree_Model := null);
   function Get_Model
     (Combo_Box : access Gtk_Combo_Box_Record)
      return Gtk.Tree_Model.Gtk_Tree_Model;
   --  Sets the model used by Combo_Box to be Model. Will unset a previously
   --  set model (if applicable). If model is null, then it will unset the
   --  model. Note that this function does not clear the cell renderers, you
   --  have to call Gtk.Cell_Layout.Clear yourself if you need to set up
   --  different cell renderers for the new model.

   procedure Set_Active
     (Combo_Box : access Gtk_Combo_Box_Record; Index : Gint);
   function Get_Active
     (Combo_Box : access Gtk_Combo_Box_Record) return Gint;
   --  Returns the index of the currently active item, or -1 if there's no
   --  active item. If the model is a non-flat treemodel, and the active item
   --  is not an immediate child of the root of the tree, this function returns
   --  Gtk.Tree_Model.Get_Indices (Path)[0], where Path is the Gtk_Tree_Path of
   --  the active model.

   procedure Set_Active_Iter
     (Combo_Box : access Gtk_Combo_Box_Record;
      Iter      : Gtk.Tree_Model.Gtk_Tree_Iter);
   function Get_Active_Iter
     (Combo_Box : access Gtk_Combo_Box_Record)
      return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Sets the current active item to be the one referenced by Iter.
   --  Iter must correspond to a path of depth one.

   procedure Set_Wrap_Width
     (Combo_Box : access Gtk_Combo_Box_Record; Width : Gint);
   function Get_Wrap_Width
     (Combo_Box : access Gtk_Combo_Box_Record) return Gint;
   --  Returns the wrap width which is used to determine the number
   --  of columns for the popup menu. If the wrap width is larger than
   --  1, the combo box is in table mode. This can be used for instance to
   --  display a matrix of color (a color palette to choose from).
   --  See also Set_Column_Span_Column

   procedure Set_Add_Tearoffs
     (Combo_Box : access Gtk_Combo_Box_Record; Add_Tearoffs : Boolean);
   function Get_Add_Tearoffs
     (Combo_Box : access Gtk_Combo_Box_Record) return Boolean;
   --  Sets whether the popup menu should have a tearoff menu item.
   --  Clicking on this menu will detach the combo into a floating window that
   --  the user can put anywhere on the screen.

   procedure Set_Button_Sensitivity
     (Combo_Box   : access Gtk_Combo_Box_Record;
      Sensitivity : Gtk.Enums.Gtk_Sensitivity_Type);
   function Get_Button_Sensitivity
     (Combo_Box : access Gtk_Combo_Box_Record)
      return Gtk.Enums.Gtk_Sensitivity_Type;
   --  Sets whether the dropdown button of the combo box should be always
   --  sensitive (Gtk_Sensitivity_On), never sensitive (Gtk_Sensitivity_Off)
   --  or only if there is at least one item to display (Gtk_Sensitivity_Auto).

   function Get_Has_Entry
     (Combo_Box : access Gtk_Combo_Box_Record) return Boolean;
   --  Returns whether the combo box has an entry

   procedure Set_Entry_Text_Column
     (Combo_Box   : access Gtk_Combo_Box_Record;
      Text_Column : Gint);
   --  Sets the model column which combo_box should use to get strings from to
   --  be Text_Column. The column Text_Column in the model of Combo_Box must be
   --  of type G_TYPE_STRING.

   function Get_Entry_Text_Column
     (Combo_Box : access Gtk_Combo_Box_Record) return Gint;
   --  Returns the column which combo_box is using to get the strings from to
   --  display in the internal entry.

   procedure Set_Column_Span_Column
     (Combo_Box : access Gtk_Combo_Box_Record; Column_Span : Gint);
   function Get_Column_Span_Column
     (Combo_Box : access Gtk_Combo_Box_Record) return Gint;
   --  Sets the column with column span information for Combo_Box to be
   --  Column_Span. The column span column contains integers which indicate
   --  how many columns an item should span. This applies to grid combos, see
   --  also Set_Wrap_Width.

   procedure Set_Row_Span_Column
     (Combo_Box : access Gtk_Combo_Box_Record; Row_Span : Gint);
   function Get_Row_Span_Column
     (Combo_Box : access Gtk_Combo_Box_Record) return Gint;
   --  Sets the column with row span information for Combo_Box to be Row_Span.
   --  The row span column contains integers which indicate how many rows
   --  an item should span.

   procedure Set_Focus_On_Click
     (Combo          : access Gtk_Combo_Box_Record;
      Focus_On_Click : Boolean);
   function Get_Focus_On_Click
     (Combo : access Gtk_Combo_Box_Record) return Boolean;
   --  Sets whether the combo box will grab focus when it is clicked with
   --  the mouse. Making mouse clicks not grab focus is useful in places
   --  like toolbars where you don't want the keyboard focus removed from
   --  the main area of the application.

   procedure Set_Row_Separator_Func
     (Combo_Box : access Gtk_Combo_Box_Record;
      Func      : Gtk.Tree_View.Gtk_Tree_View_Row_Separator_Func;
      Data      : System.Address;
      Destroy   : Glib.G_Destroy_Notify_Address := null);
   function Get_Row_Separator_Func
     (Combo_Box : access Gtk_Combo_Box_Record)
      return Gtk.Tree_View.Gtk_Tree_View_Row_Separator_Func;
   --  Sets the row separator function, which is used to determine
   --  whether a row should be drawn as a separator. If the row separator
   --  function is null, no separators are drawn. This is the default value.

   procedure Set_Title
     (Combo_Box : access Gtk_Combo_Box_Record;
      Title     : String);
   function Get_Title
     (Combo_Box : access Gtk_Combo_Box_Record)
      return String;
   --  Sets or Gets the menu's title in tearoff mode.

   ---------------------------
   -- Text-only combo boxes --
   ---------------------------
   --  If your combo box only contains text, you do not necessarily have to go
   --  through the more complex use of a Gtk_Tree_Model.

   procedure Gtk_New_Text    (Combo : out Gtk_Combo_Box);
   procedure Initialize_Text (Combo : access Gtk_Combo_Box_Record'Class);
   --  Convenience function which constructs a new text combo box, which is
   --  Gtk_Combo_Box just displaying strings. If you use this function to
   --  create a text combo box, you should only manipulate its data source with
   --  the following convenience functions: Append_Text, Insert_Text,
   --  Prepend_Text and Remove_Text

   procedure Append_Text
     (Combo_Box : access Gtk_Combo_Box_Record; Text : String);
   procedure Prepend_Text
     (Combo_Box : access Gtk_Combo_Box_Record; Text : String);
   procedure Insert_Text
     (Combo_Box : access Gtk_Combo_Box_Record;
      Position  : Gint;
      Text      : String);
   --  Adds Text to the list of strings stored in Combo_Box. Note that
   --  you can only use this function with combo boxes constructed with
   --  Gtk_New_Text.

   procedure Remove_Text
     (Combo_Box : access Gtk_Combo_Box_Record; Position : Gint);
   --  Removes the string at Position from Combo_Box. Note that you can only
   --  use this function with combo boxes constructed with Gtk_New_Text.

   function Get_Active_Text
     (Combo_Box : access Gtk_Combo_Box_Record) return String;
   --  Returns the currently active string in Combo_Box or "" if none
   --  is selected.  Note that you can only use this function with combo
   --  boxes constructed with Gtk_New_Text.

   --------------------------
   -- Programmatic Control --
   --------------------------

   procedure Popdown (Combo_Box : access Gtk_Combo_Box_Record);
   procedure Popup   (Combo_Box : access Gtk_Combo_Box_Record);
   --  Hides or pops up the menu or dropdown list of Combo_Box.
   --  This function is mostly intended for use by accessibility technologies;
   --  applications should have little use for it.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Cell_Layout"
   --    This interface should be used to add new renderers to the view, to
   --    render various columns of the model
   --  - "Gtk_Cell_Editable"
   --    This interface should be used to edit the contents of a tree model
   --    cell

   package Implements_Cell_Layout is new Glib.Types.Implements
     (Gtk.Cell_Layout.Gtk_Cell_Layout, Gtk_Combo_Box_Record, Gtk_Combo_Box);
   function "+"
     (Box : access Gtk_Combo_Box_Record'Class)
      return Gtk.Cell_Layout.Gtk_Cell_Layout
      renames Implements_Cell_Layout.To_Interface;
   function "-"
     (Layout : Gtk.Cell_Layout.Gtk_Cell_Layout)
      return Gtk_Combo_Box
      renames Implements_Cell_Layout.To_Object;
   --  Converts to and from the Gtk_Cell_Layout interface

   package Implements_Cell_Editable is new Glib.Types.Implements
     (Gtk.Cell_Editable.Gtk_Cell_Editable,
      Gtk_Combo_Box_Record, Gtk_Combo_Box);
   function "+"
     (Box : access Gtk_Combo_Box_Record'Class)
      return Gtk.Cell_Editable.Gtk_Cell_Editable
      renames Implements_Cell_Editable.To_Interface;
   function "-"
     (Editable : Gtk.Cell_Editable.Gtk_Cell_Editable)
      return Gtk_Combo_Box
      renames Implements_Cell_Editable.To_Object;
   --  Converts to and from the Gtk_Cell_Editable interface

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.

   --  <properties>
   --  Name:  Active_Property
   --  Type:  Int
   --  Descr: The item which is currently active
   --
   --  Name:  Add_Tearoffs_Property
   --  Type:  Boolean
   --  Descr: Whether dropdowns should have a tearoff menu item
   --
   --  Name:  Button_Sensitivity_Property
   --  Type:  Enum
   --  Descr: Whether the dropdown button is sensitive when the model is empty
   --
   --  Name:  Column_Span_Column_Property
   --  Type:  Int
   --  Descr: TreeModel column containing the column span values
   --
   --  Name:  Focus_On_Click_Property
   --  Type:  Boolean
   --  Descr: Whether the combo box grabs focus when it is clicked with the
   --         mouse
   --
   --  Name:  Has_Frame_Property
   --  Type:  Boolean
   --  Descr: Whether the combo box draws a frame around the child
   --
   --  Name:  Model_Property
   --  Type:  Object
   --  Descr: The model for the combo box
   --
   --  Name:  Popup_Shown_Property
   --  Type:  Boolean
   --  Descr: Whether the combo's dropdown is shown
   --
   --  Name:  Row_Span_Column_Property
   --  Type:  Int
   --  Descr: TreeModel column containing the row span values
   --
   --  Name:  Tearoff_Title_Property
   --  Type:  String
   --  Descr: A title that may be displayed by the window manager when the
   --         popup is torn-off
   --
   --  Name:  Wrap_Width_Property
   --  Type:  Int
   --  Descr: Wrap width for layouting the items in a grid
   --  </properties>

   Active_Property             : constant Glib.Properties.Property_Int;
   Add_Tearoffs_Property       : constant Glib.Properties.Property_Boolean;
   Button_Sensitivity_Property : constant Glib.Properties.Property_Enum;
   Column_Span_Column_Property : constant Glib.Properties.Property_Int;
   Focus_On_Click_Property     : constant Glib.Properties.Property_Boolean;
   Has_Frame_Property          : constant Glib.Properties.Property_Boolean;
   Model_Property              : constant Glib.Properties.Property_Object;
   Popup_Shown_Property        : constant Glib.Properties.Property_Boolean;
   Row_Span_Column_Property    : constant Glib.Properties.Property_Int;
   Tearoff_Title_Property      : constant Glib.Properties.Property_String;
   Wrap_Width_Property         : constant Glib.Properties.Property_Int;

   ----------------------
   -- Style Properties --
   ----------------------
   --  The following properties can be changed through the gtk theme and
   --  configuration files, and retrieved through Gtk.Widget.Style_Get_Property

   --  <style_properties>
   --  Name:  Appears_As_List_Property
   --  Type:  Boolean
   --  Descr: Whether dropdowns should look like lists rather than menus
   --
   --  Name:  Arrow_Size_Property
   --  Type:  Int
   --  Descr: The minimum size of the arrow in the combo box
   --
   --  Name:  Shadow_Type_Property
   --  Type:  Enum
   --  Descr: Which kind of shadow to draw around the combo box
   --  </style_properties>

   Appears_As_List_Property : constant Glib.Properties.Property_Boolean;
   Arrow_Size_Property      : constant Glib.Properties.Property_Int;
   Shadow_Type_Property     : constant Glib.Properties.Property_Enum;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget

   --  <signals>
   --  - "changed"
   --    procedure Handler (Combo : access Gtk_Combo_Box_Record'Class);
   --    Emitted when the active item is changed. The can be due to the user
   --    selecting a different item from the list, or due to a call to
   --    Set_Active_Iter. It will also be emitted while typing into a
   --    Gtk_Combo_Box_Entry, as well as when selecting an item from the
   --    Gtk_Combo_Box_Entry's list.
   --  </signals>

   Signal_Changed : constant Glib.Signal_Name := "changed";

private
   pragma Import (C, Get_Type, "gtk_combo_box_get_type");

   Active_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("active");
   Add_Tearoffs_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("add-tearoffs");
   Arrow_Size_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("arrow-size");
   Button_Sensitivity_Property : constant Glib.Properties.Property_Enum :=
     Glib.Properties.Build ("button-sensitivity");
   Column_Span_Column_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("column-span-column");
   Focus_On_Click_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("focus-on-click");
   Has_Frame_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-frame");
   Model_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("model");
   Popup_Shown_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("popup-shown");
   Row_Span_Column_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("row-span-column");
   Shadow_Type_Property : constant Glib.Properties.Property_Enum :=
     Glib.Properties.Build ("shadow-type");
   Tearoff_Title_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("tearoff-title");
   Wrap_Width_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("wrap-width");

   Appears_As_List_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("appears-as-list");
end Gtk.Combo_Box;

--  No binding: gtk_combo_box_get_popup_accessible
