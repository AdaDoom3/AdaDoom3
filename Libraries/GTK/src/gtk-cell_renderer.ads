-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2001-2013, AdaCore                   --
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
--  The Gtk_Cell_Renderer is a base class of a set of objects used for
--  rendering a cell to a Gdk_Drawable. These objects are used primarily by the
--  Gtk_Tree_View widget, though they aren't tied to them in any specific way.
--  It is worth noting that Gtk_Cell_Renderer is not a Gtk_Widget and cannot be
--  treated as such.
--
--  The primary use of a Gtk_Cell_Renderer is for drawing a certain graphical
--  elements on a Gdk_Drawable. Typically, one cell renderer is used to draw
--  many cells on the screen. To this extent, it isn't expected that
--  Cell_Renderer keep any permanent state around. Instead, any state is set
--  just prior to use using GObjects property system. Then, the cell is
--  measured using Get_Size(). Finally, the cell is rendered in the correct
--  location using Render().
--
--  There are a number of rules that must be followed when writing a new
--  Gtk_Cell_Renderer. First and formost, it's important that a certain set of
--  properties will always yield a cell renderer of the same size, barring
--  GtkStyle change. The Gtk_Cell_Renderer also has a number of generic
--  properties that are expected to be honored by all children.
--
--  Beyond merely rendering a cell, cell renderers can optionally provide
--  active user interface elements. A cell renderer can be activatable like
--  Gtk_Cell_Renderer_Toggle, which toggles when it gets activated by a mouse
--  click, or it can be editable like Gtk_Cell_Renderer_Text, which allows the
--  user to edit the text using a Gtk_Entry. To make a cell renderer
--  activatable or editable, you have to implement the activate or
--  start_editing virtual functions, respectively.
--  </description>
--  <c_version>2.14</c_version>
--  <group>Trees and Lists</group>

with Glib.Object;

with Gdk.Event;
with Gdk.Rectangle;
with Gdk.Window;
with Gtk;
with Gtk.Cell_Editable;
with Gtk.Widget;
with Glib.Properties;
with Glib.Generic_Properties;
with Glib.Glist;
pragma Elaborate_All (Glib.Glist);

package Gtk.Cell_Renderer is

   type Gtk_Cell_Renderer_Record is
     new Glib.Object.GObject_Record with private;
   type Gtk_Cell_Renderer is access all Gtk_Cell_Renderer_Record'Class;

   function Convert (R : Gtk_Cell_Renderer) return System.Address;
   function Convert (R : System.Address) return Gtk_Cell_Renderer;
   package Cell_Renderer_List is
      new Glib.Glist.Generic_List (Gtk_Cell_Renderer);

   type Gtk_Cell_Renderer_State is mod 2 ** 32;
   Cell_Renderer_Selected    : constant Gtk_Cell_Renderer_State;
   Cell_Renderer_Prelit      : constant Gtk_Cell_Renderer_State;
   Cell_Renderer_Insensitive : constant Gtk_Cell_Renderer_State;
   Cell_Renderer_Sorted      : constant Gtk_Cell_Renderer_State;
   Cell_Renderer_Focused     : constant Gtk_Cell_Renderer_State;
   --  Identifies how a cell should be renderer. Prelit is used when the mouse
   --  is hovering over a particular cell. Sorted is used when the cell is in
   --  a sort row.

   type Gtk_Cell_Renderer_Mode is
     (Cell_Renderer_Mode_Inert,
      Cell_Renderer_Mode_Activatable,
      Cell_Renderer_Mode_Editable);
   for Gtk_Cell_Renderer_Mode'Size use Glib.Gint'Size;
   --  Identifies how the user can interact with a particular cell. If
   --  Activatable, the cell can be clicked. If Editable, the cell can be
   --  modified

   function Get_Type return GType;
   --  Return the internal value associated with Gtk_Cell_Renderer

   procedure Get_Size
     (Cell      : access Gtk_Cell_Renderer_Record;
      Widget    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Cell_Area : out Gdk.Rectangle.Gdk_Rectangle;
      X_Offset  : out Gint;
      Y_Offset  : out Gint;
      Width     : out Gint;
      Height    : out Gint);
   --  Obtain the width and height needed to render the cell.
   --  Used by view widgets to determine the appropriate size for the Cell_Area
   --  passed to Render. Fill in the x and y offsets (if set) of the cell
   --  relative to this location. Please note that the values set in Width and
   --  Height, as well as those in X_Offset and Y_Offset are inclusive of the
   --  Xpad and Ypad properties.
   --  Widget: the widget the renderer is rendering to.
   --  Cell_Area: The area a cell will be allocated.
   --  X_Offset: X offset of cell relative to Cell_Area.
   --  Y_Offset: Y offset of cell relative to Cell_Area.
   --  Width: Width needed to render a cell.
   --  Height: Height needed to render a cell.

   procedure Render
     (Cell            : access Gtk_Cell_Renderer_Record;
      Window          : Gdk.Window.Gdk_Window;
      Widget          : access Gtk.Widget.Gtk_Widget_Record'Class;
      Background_Area : Gdk.Rectangle.Gdk_Rectangle;
      Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
      Expose_Area     : Gdk.Rectangle.Gdk_Rectangle;
      Flags           : Gtk_Cell_Renderer_State);
   --  Invokes the virtual render function of the Gtk_Cell_Renderer. The three
   --  passed-in rectangles are areas of Window. Most renderers will draw
   --  within Cell_Area; the Xalign, Yalign, Xpad, and Ypad fields of the
   --  GtkCellRenderer should be honored with respect to Cell_Area.
   --  Background_Area includes the blank space around the cell, and also the
   --  area containing the tree expander; so the Background_Area rectangles
   --  for all cells tile to cover the entire Window.  Expose_Area is a clip
   --  rectangle.

   function Activate
     (Cell            : access Gtk_Cell_Renderer_Record;
      Event           : Gdk.Event.Gdk_Event;
      Widget          : access Gtk.Widget.Gtk_Widget_Record'Class;
      Path            : UTF8_String;
      Background_Area : Gdk.Rectangle.Gdk_Rectangle;
      Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
      Flags           : Gtk_Cell_Renderer_State) return Boolean;
   --  Passes an activate event to the cell renderer for possible processing.
   --  Some cell renderers may use events;
   --  for example, Gtk_Cell_Renderer_Toggle toggles when it gets a
   --  mouse click.

   function Start_Editing
     (Cell            : access Gtk_Cell_Renderer_Record;
      Event           : Gdk.Event.Gdk_Event;
      Widget          : access Gtk.Widget.Gtk_Widget_Record'Class;
      Path            : UTF8_String;
      Background_Area : Gdk.Rectangle.Gdk_Rectangle;
      Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
      Flags           : Gtk_Cell_Renderer_State)
      return Gtk.Cell_Editable.Gtk_Cell_Editable;
   --  Passes an activate event to the cell renderer for possible processing.
   --  Cell: a Gtk_Cell_Renderer
   --  Event: a Gdk_Event
   --  Widget: widget that received the event
   --  Path: widget-dependent string representation of the event location;
   --  e.g. for Gtk_Tree_View, a string representation of Gtk_Tree_Path
   --  Background_Area: background area as passed to Render
   --  Cell_Area: cell area as passed to Render

   procedure Set_Fixed_Size
     (Cell   : access Gtk_Cell_Renderer_Record;
      Width  : Gint;
      Height : Gint);
   procedure Get_Fixed_Size
     (Cell   : access Gtk_Cell_Renderer_Record;
      Width  : out Gint;
      Height : out Gint);
   --  Sets the renderer size to be explicit, independent of the
   --  properties set.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "editing-canceled"
   --    procedure Handler (Cell : access Gtk_Cell_Renderer_Record'Class);
   --    This signal gets emitted when the user cancels the process of editing
   --    cell. For example, an editable cell renderer could be written to
   --    cancel editing when the user presses Escape.
   --
   --  - "editing-started"
   --    procedure Handler
   --       (Cell     : access Gtk_Cell_Renderer_Record'Class;
   --        Editable : Gtk_Cell_Editable
   --        Path     : String)
   --    This signal gets emitted when a cell starts to be edited. The indended
   --    use of this signal is to do special setup on editable, e.g. adding a
   --    GtkEntryCompletion or setting up additional columns in a GtkComboBox.
   --    Note that GTK+ doesn't guarantee that cell renderers will continue to
   --    use the same kind of widget for editing in future releases, therefore
   --    you should check the type of editable before doing any specific setup,
   --    as in the following example:
   --  </signals>

   Signal_Editing_Canceled : constant Glib.Signal_Name := "editing-canceled";
   Signal_Editing_Started  : constant Glib.Signal_Name := "editing-started";

   ----------------
   -- Properties --
   ----------------

   --  The following properties are defined for this cell_renderer and its
   --  children:
   --  <properties>
   --
   --   Attribute             Type in Model             Mode
   --   =========             =============             ====
   --
   --   "mode"                Gtk_Cell_Renderer_Mode    Read / Write
   --   "visible"             Boolean                   Read / Write
   --   "xalign"              Gfloat                    Read / Write
   --   "yalign"              Gfloat                    Read / Write
   --   "xpad"                Guint                     Read / Write
   --   "ypad"                Guint                     Read / Write
   --   "width"               Gint                      Read / Write
   --   "height"              Gint                      Read / Write
   --   "is_expander"         Boolean                   Read / Write
   --   "is_expanded"         Boolean                   Read / Write
   --   "cell_background_gdk" Gdk_Color                 Read / Write
   --   "cell_background"     String                    Write
   --
   --  Name:  Cell_Background_Property
   --  Type:  String
   --  Descr: Cell background color as a string
   --
   --  Name:  Cell_Background_Gdk_Property
   --  Type:  Boxed
   --  Descr: Cell background color as a GdkColor
   --
   --  Name:  Editing_Property
   --  Type:  Boolean
   --  Descr: Whether the cell renderer is currently in editing mode
   --
   --  Name:  Height_Property
   --  Type:  Int
   --  Descr: The fixed height
   --
   --  Name:  Is_Expanded_Property
   --  Type:  Boolean
   --  Descr: Row is an expander row, and is expanded
   --
   --  Name:  Is_Expander_Property
   --  Type:  Boolean
   --  Descr: Row has children
   --
   --  Name:  Mode_Property
   --  Type:  Enum
   --  Descr: Editable mode of the CellRenderer
   --
   --  Name:  Sensitive_Property
   --  Type:  Boolean
   --  Descr: Display the cell sensitive
   --
   --  Name:  Visible_Property
   --  Type:  Boolean
   --  Descr: Display the cell
   --
   --  Name:  Width_Property
   --  Type:  Int
   --  Descr: The fixed width
   --
   --  Name:  Xalign_Property
   --  Type:  Float
   --  Descr: The x-align
   --
   --  Name:  Xpad_Property
   --  Type:  Uint
   --  Descr: The xpad
   --
   --  Name:  Yalign_Property
   --  Type:  Float
   --  Descr: The y-align
   --
   --  Name:  Ypad_Property
   --  Type:  Uint
   --  Descr: The ypad

   --   </properties>

   package Cell_Renderer_Mode_Properties is new
     Glib.Generic_Properties.Generic_Internal_Discrete_Property
       (Gtk_Cell_Renderer_Mode);
   type Property_Cell_Renderer_Mode is
     new Cell_Renderer_Mode_Properties.Property;

   Cell_Background_Property     : constant Glib.Properties.Property_String;
   --  Cell_Background_Gdk_Property : constant Glib.Properties.Property_Boxed;
   Editing_Property             : constant Glib.Properties.Property_Boolean;
   Height_Property              : constant Glib.Properties.Property_Int;
   Is_Expanded_Property         : constant Glib.Properties.Property_Boolean;
   Is_Expander_Property         : constant Glib.Properties.Property_Boolean;
   Mode_Property                : constant Property_Cell_Renderer_Mode;
   Sensitive_Property           : constant Glib.Properties.Property_Boolean;
   Visible_Property             : constant Glib.Properties.Property_Boolean;
   Width_Property               : constant Glib.Properties.Property_Int;
   Xalign_Property              : constant Glib.Properties.Property_Float;
   Xpad_Property                : constant Glib.Properties.Property_Uint;
   Yalign_Property              : constant Glib.Properties.Property_Float;
   Ypad_Property                : constant Glib.Properties.Property_Uint;

private
   type Gtk_Cell_Renderer_Record is
     new Glib.Object.GObject_Record with null record;

   pragma Import (C, Get_Type, "gtk_cell_renderer_get_type");

   Cell_Background_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("cell-background");
   --  Cell_Background_Gdk_Property : constant Glib.Properties.Property_Boxed
   --    := Glib.Properties.Build ("cell-background-gdk");
   Editing_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("editing");
   Height_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("height");
   Is_Expanded_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("is-expanded");
   Is_Expander_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("is-expander");
   Mode_Property : constant Property_Cell_Renderer_Mode :=
     Build ("mode");
   Sensitive_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("sensitive");
   Visible_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("visible");
   Width_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("width");
   Xalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("xalign");
   Xpad_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("xpad");
   Yalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("yalign");
   Ypad_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("ypad");

   Cell_Renderer_Selected    : constant Gtk_Cell_Renderer_State := 2 ** 0;
   Cell_Renderer_Prelit      : constant Gtk_Cell_Renderer_State := 2 ** 1;
   Cell_Renderer_Insensitive : constant Gtk_Cell_Renderer_State := 2 ** 2;
   Cell_Renderer_Sorted      : constant Gtk_Cell_Renderer_State := 2 ** 3;
   Cell_Renderer_Focused     : constant Gtk_Cell_Renderer_State := 2 ** 4;
end Gtk.Cell_Renderer;

--  The following subprograms never had a binding, are now obsolescent
--  No binding: gtk_cell_renderer_editing_canceled
--  No binding: gtk_cell_renderer_stop_editing
