-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--      Copyright (C) 2000 E. Briot, J. Brobecker and A. Charlet     --
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
--  A Gtk_Plot_Canvas is a special kind of drawing area used with Gtk_Plot
--  widgets.
--  It provides drag-and-drop capabilities for the texts, legends, points...
--  available in a Gtk_Plot.
--  Note that this widget is specifically designed for Gtk_Plot widgets, and
--  won't provide any other capability for other kinds of widgets.
--
--  Like any child of Gtk_Layout, this widget can have an almost unlimited
--  size for its children, and provides scrolling.
--  </description>
--  <c_version>gtkextra 2.1.1</c_version>
--  <group>Plotting Data</group>

with Glib.Object;
with Gdk;
with Gtk.Extra.Plot_Data;
with Gtk.Fixed;
with Gdk.Color;

package Gtk.Extra.Plot_Canvas is

   type Gtk_Plot_Canvas_Record is new Gtk.Fixed.Gtk_Fixed_Record with private;
   type Gtk_Plot_Canvas is access all Gtk_Plot_Canvas_Record'Class;

   type Gtk_Plot_Canvas_Child_Record is
     new Glib.Object.GObject_Record with private;
   type Gtk_Plot_Canvas_Child is access all Gtk_Plot_Canvas_Child_Record'Class;

   ----------------
   -- Enum types --
   ----------------

   type Plot_Canvas_Action is
     (Action_Inactive,
      Action_Selection,
      Action_Drag,
      Action_Resize);
   --  The action being performed on the canvas.
   pragma Convention (C, Plot_Canvas_Action);

   type Plot_Canvas_Flag is new Gint;
   Frozen     : constant Plot_Canvas_Flag;
   Can_Move   : constant Plot_Canvas_Flag;
   Can_Resize : constant Plot_Canvas_Flag;

   type Plot_Canvas_Selection is
     (Select_None,
      Select_Markers,
      Select_Target);
   pragma Convention (C, Plot_Canvas_Selection);

   type Plot_Canvas_Pos is
     (Canvas_Out,
      Canvas_In,
      Canvas_Left,
      Canvas_Right,
      Canvas_Top,
      Canvas_Bottom,
      Canvas_Top_Left,
      Canvas_Top_Right,
      Canvas_Bottom_Left,
      Canvas_Bottom_Right);
   --  The position of the items in the canvas.
   pragma Convention (C, Plot_Canvas_Pos);

   type Plot_Canvas_Selection_Mode is
     (Select_Click_1,
      Select_Click_2);
   pragma Convention (C, Plot_Canvas_Selection_Mode);

   ------------------------------------------
   -- Creating and manipulating the canvas --
   ------------------------------------------

   procedure Gtk_New
     (Widget        : out Gtk_Plot_Canvas;
      Width         : Gint;
      Height        : Gint;
      Magnification : Gdouble := 1.0);
   --  Create a new Gtk_Plot_Canvas, with a specific screen size.
   --  Since the widget can have an unlimited internal size, it does not try
   --  to set its size to accommodate all of its children.

   procedure Initialize
     (Widget        : access Gtk_Plot_Canvas_Record'Class;
      Width         : Gint;
      Height        : Gint;
      Magnification : Gdouble := 1.0);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Plot_Canvas.

   function Child_Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Plot_Canvas_Child.

   procedure Refresh (Canvas : access Gtk_Plot_Canvas_Record);
   --  Force a refresh of the canvas on the screen. The screen is updated from
   --  the contents of the double-buffer.

   procedure Paint (Canvas : access Gtk_Plot_Canvas_Record);
   --  Redraw each of the items included in the canvas. The painting is done
   --  in the double-buffer, and must be drawn on the screen with Refresh.

   procedure Freeze (Canvas : access Gtk_Plot_Canvas_Record);
   --  Freeze all graphical updates to the screen. This significanly speeds up
   --  the updates to the plot

   procedure Thaw (Canvas : access Gtk_Plot_Canvas_Record);
   --  Reactivate all graphical updates to the screen

   procedure Grid_Set_Visible
     (Canvas  : access Gtk_Plot_Canvas_Record;
      Visible : Boolean);
   --  Indicate whether the grid should be visible or not.

   procedure Grid_Set_Step
     (Canvas : access Gtk_Plot_Canvas_Record;
      Step   : Gdouble);
   --  Set the space between two lines of the grid.

   procedure Grid_Set_Attributes
     (Canvas : access Gtk_Plot_Canvas_Record;
      Style  : Gtk.Extra.Plot_Data.Plot_Line_Style;
      Width  : Gint;
      Color  : Gdk.Color.Gdk_Color);
   --  Set the attributes of the grid.

   procedure Cancel_Action (Plot_Canvas : access Gtk_Plot_Canvas_Record);
   --  Cancel the current action.
   --  This can be called in the user callbacks to ignore temporarily some of
   --  the signals below.

   procedure Unselect (Canvas : access Gtk_Plot_Canvas_Record);
   --  Unselect the currently selected item.

   function Get_Active_Item
     (Canvas  : access Gtk_Plot_Canvas_Record)
     return Gtk_Plot_Canvas_Child;
   --  Return the currently selected item.

   procedure Set_Size
     (Canvas  : access Gtk_Plot_Canvas_Record;
      Width   : Gint;
      Height  : Gint);
   --  Modify the size allocated for the canvas, and the size of the pixmap
   --  the plots are displayed on.

   procedure Set_Magnification
     (Canvas        : access Gtk_Plot_Canvas_Record;
      Magnification : Gdouble := 1.0);
   --  Changes the magnification for the canvas.
   --  1.0 is the default value. Higher values will zoom in, while lower values
   --  will zoom out.

   procedure Set_Transparent
     (Canvas : access Gtk_Plot_Canvas_Record; Transparent : Boolean);
   --  Whether the canvas should be transparent. If Transparent is True, all
   --  background attributes are ignored

   function Transparent
     (Canvas : access Gtk_Plot_Canvas_Record) return Boolean;
   --  Whether the canvas is currently transparent

   procedure Set_Background
     (Canvas     : access Gtk_Plot_Canvas_Record;
      Background : Gdk.Color.Gdk_Color);
   --  Set the background color for the canvas.

   procedure Get_Pixel
     (Canvas : access Gtk_Plot_Canvas_Record;
      Px     : Gdouble;
      Py     : Gdouble;
      X      : out Gint;
      Y      : out Gint);
   --  Convert from relative coordinates to absolute ones.

   procedure Get_Position
     (Canvas : access Gtk_Plot_Canvas_Record;
      X      : Gint;
      Y      : Gint;
      Px     : out Gdouble;
      Py     : out Gdouble);
   --  Convert from absolute coordinates to relative ones.

   procedure Put_Child
     (Canvas : access Gtk_Plot_Canvas_Record;
      Child  : access Gtk_Plot_Canvas_Child_Record'Class;
      X1     : Gdouble;
      Y1     : Gdouble;
      X2     : Gdouble := 0.0;
      Y2     : Gdouble := 0.0);
   --  Insert a new item in the canvas. It will occupy the area defined by
   --  the four coordinates.
   --  See the various packages Gtk.Extra.Plot_Canvas.* on how to create
   --  such children.
   --  Leaving X2 and Y2 to their default value will ensure that the child uses
   --  as much space as it needs

   procedure Remove_Child
     (Canvas : access Gtk_Plot_Canvas_Record;
      Child  : access Gtk_Plot_Canvas_Child_Record'Class);
   --  Remove a child from the canvas

   procedure Child_Move
     (Canvas : access Gtk_Plot_Canvas_Record;
      Child  : access Gtk_Plot_Canvas_Child_Record'Class;
      X1     : Gdouble;
      Y1     : Gdouble);
   --  Move an item, but does not change its size.

   procedure Child_Move_Resize
     (Canvas : access Gtk_Plot_Canvas_Record;
      Child  : access Gtk_Plot_Canvas_Child_Record'Class;
      X1     : Gdouble;
      Y1     : Gdouble;
      X2     : Gdouble;
      Y2     : Gdouble);
   --  Move an resize an item in the canvas.

   procedure Set_Selection
     (Child     : access Gtk_Plot_Canvas_Child_Record;
      Selection : Plot_Canvas_Selection);

   procedure Set_Selection_Mode
     (Child     : access Gtk_Plot_Canvas_Child_Record;
      Mode      : Plot_Canvas_Selection_Mode);

   procedure Get_Position
     (Canvas    : access Gtk_Plot_Canvas_Record;
      Child     : access Gtk_Plot_Canvas_Child_Record'Class;
      X1, Y1    : out Gdouble;
      X2, Y2    : out Gdouble);

   ---------------------
   -- Custom children --
   ---------------------
   --  You can insert your own items in a canvas.
   --  While the canvas will take care of moving the item, it is your
   --  responsability to provide a visual rendering for it.

   -----------
   -- Flags --
   -----------
   --  Some flags are defined for this widget. You can not access them through
   --  the usual interface in Gtk.Widget.Flag_Is_Set since this widget is not
   --  part of the standard gtk+ packages. Instead, use the functions below.
   --
   --  - "can_select"
   --    True if it is possible to select a region of the canvas
   --
   --  - "can_select_item"
   --    True if it is possible to select any of the item on the canvas.
   --
   --  - "can_dnd"
   --    True if it is possible to drag an item on the canvas.
   --

   Can_Select       : constant := 2 ** 0;
   Can_Select_Item  : constant := 2 ** 1;
   Can_Dnd          : constant := 2 ** 2;
   Dnd_Flags        : constant := Can_Select_Item + Can_Dnd;

   function Plot_Canvas_Flag_Is_Set
     (Plot_Canvas : access Gtk_Plot_Canvas_Record;
      Flag        : Guint16)
     return Boolean;
   --  Test whether one of the flags for a Gtk_Plot_Canvas widget or its
   --  children is set.

   procedure Plot_Canvas_Set_Flags
     (Plot_Canvas  : access Gtk_Plot_Canvas_Record;
      Flags        : Guint16);
   --  Set the flags for a Gtk_Plot_Canvas widget or its children.
   --  Note that the flags currently set are not touched by this function.
   --  This can only be used for the flags defined in the
   --  Gtk.Extra.Gtk_Plot_Canvas package.

   procedure Plot_Canvas_Unset_Flags
     (Plot_Canvas  : access Gtk_Plot_Canvas_Record;
      Flags        : Guint16);
   --  Unset the flags for a Gtk_Plot_Canvas.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "select_item"
   --    function Handler (Canvas : access Gtk_Plot_Canvas_Record'Class;
   --                      Event  : Gdk_Button_Event;
   --                      Item   : Gtk_Plot_Canvas_Child)
   --                     return Boolean;
   --
   --    Called when an item was selected.
   --    An item can be anything, from a text to a plot
   --    When this signal is called, the item was simply selected, but not
   --    dragged.
   --    The handler should return False if the item can not be selected.
   --
   --  - "move_item"
   --    function Handler (Canvas : access Gtk_Plot_Canvas_Record'Class;
   --                      Item   : Gtk_Plot_Canvas_Child;
   --                      New_X  : Gdouble;
   --                      New_Y  : Gdouble)
   --                     return Boolean;
   --
   --    An item was moved on the canvas.
   --    Its coordinates have not changed yet, but if the handler returns True
   --    they will become (New_X, New_Y). If the handler returns False,
   --    nothing happens.
   --
   --  - "resize_item"
   --    function Handler (Canvas     : access Gtk_Plot_Canvas_Record'Class;
   --                      Item       : Gtk_Plot_Canvas_Child;
   --                      New_Width  : Gdouble;
   --                      New_Height : Gdouble)
   --                     return Boolean;
   --
   --    An item is being resized.
   --    Its size has not changed yet, but if the handler returns True
   --    it will become (New_Width, New_Height). If the handler returns False,
   --    nothing happens.
   --
   --  - "add_item"
   --    procedure Handler (Canvas : access Gtk_Plot_Canvas_Record'Class;
   --                       Item   : Gtk_Plot_Canvas_Child);
   --    Called when a new child is added into the canvas
   --
   --  - "delete_item"
   --    function Handler (Canvas : access Gtk_Plot_Canvas_Record'Class;
   --                      Item   : Gtk_Plot_Canvas_Child) return GBoolean;
   --    Called when an item is being removed from the canvas
   --
   --  - "select_region"
   --    procedure Handler (Canvas : access Gtk_Plot_Canvas_Record'Class;
   --                       X_Min  : Gdouble;
   --                       Y_Min  : Gdouble;
   --                       X_Max  : Gdouble;
   --                       Y_Max  : Gdouble);
   --    A region of the canvas was selected by the user.
   --
   --  - "changed"
   --    procedure Handler (Canvas : access Gtk_Plot_Canvas_Record'Class);
   --    Called when the contents of the canvas has changed (an item was
   --    moved interactively by the user).
   --  </signals>

private
   type Gtk_Plot_Canvas_Record is new Gtk.Fixed.Gtk_Fixed_Record
     with null record;
   type Gtk_Plot_Canvas_Child_Record is
     new Glib.Object.GObject_Record with null record;
   pragma Import (C, Get_Type, "gtk_plot_canvas_get_type");
   pragma Import (C, Child_Get_Type, "gtk_plot_canvas_child_get_type");

   Frozen     : constant Plot_Canvas_Flag := 0;
   Can_Move   : constant Plot_Canvas_Flag := 1;
   Can_Resize : constant Plot_Canvas_Flag := 2;

end Gtk.Extra.Plot_Canvas;

--  Unbound
--    gtk_plot_canvas_set_pc
--    gtk_plot_canvas_set_line_attributes
