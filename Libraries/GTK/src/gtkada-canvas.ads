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
--  This package provides an interactive canvas, on which the user can put
--  items, move them with the mouse, etc. The items can be connected together,
--  and the connections remain active while the items are moved.
--
--  It also supports scrolling if put in a Gtk_Scrolled_Window.
--  The canvas will be scrolled (and the selected items moved) if an item is
--  selected and the mouse is dragged on a small area on the side of the canvas
--  or even directly outside of the canvas. Scrolling will continue until the
--  mouse is either released or moved back inside the canvas.
--
--  The scrolling speed will slightly increase over time if the mouse is kept
--  outside of the canvas. This makes the canvas much more comfortable to use
--  for the user.
--
--  All items put in this canvas must inherit from the type Canvas_Item_Record.
--  However, it is your responsability, as a programmer, to provide drawing
--  routines. In fact, all these items should draw in a pixmap, which is then
--  copied automatically to the screen whenever the canvas needs to redraw
--  itself.
--
--  The items can also react to mouse events: mouse clicks are transmitted to
--  the item if the mouse did not move more than a given amount of pixels.
--  To decide what their reaction should be, you should override the
--  On_Button_Click subprogram.
--
--  This canvas is not intended for cases where you want to put hundreds of
--  items on the screen. For instance, it does not provide any smart
--  double-buffering other than the one provided by gtk+ itself, and thus you
--  would get some flicker if there are too many items.
--
--  There are three coordinate systems used by widget. All the subprograms
--  expect a specific coordinate system as input or output. Here are the three
--  systems:
--    - World coordinates
--      The position of an item is reported in pixels, as if the canvas
--      currently had a zoom level of 100%. This is fully independent, at any
--      time, from the current zoom level of the canvas.
--      Since the canvas is considered to expand ad infinitum, the top-left
--      corner doesn't have any specific fixed coordinates. It can be known by
--      checking the current lower value of the adjustments (aka scrollbars).
--
--    - Canvas coordinates
--      This is similar to world coordinates, except these depend on the
--      current zoom level of the canvas. This also affect the width and height
--      of the objects in the canvas.
--      The subprograms To_Canvas_Coordinates and To_World_Coordinates can be
--      used to convert lengths from world to canvas coordinates.
--      The same behavior as world coordinates applies for the top-left corner.
--      All drawing to the screen, in particular for Draw_Background, must be
--      done using this coordinate systems
--
--    - Item coordinates
--      The position of a point is relative to the top-left corner of the
--      current item. This corner therefore has coordinates (0, 0).
--      This coordinate systems assumes a zoom-level of 100%
--
--  Items are selected automatically when they are clicked. If Control is
--  pressed at the same time, multiple items can be selected.
--  If the background is clicked (and control is not pressed), then all items
--  are unselected.
--  Pressing and dragging the mouse in the backgroudn draws a virtual box on
--  the screen. All the items fully included in this box when it is released
--  will be selected (this will replace the current selection if Control was
--  not pressed).
--
--  </description>
--  <group>Drawing</group>
--  <testgtk>create_canvas.adb</testgtk>
--  <screenshot>gtkada-canvas</screenshot>

with Ada.Calendar;

with Cairo;
with Cairo.Region;

with Gdk.Color;
with Gdk.Event;

with Glib;
with Glib.Graphs;
with Glib.Main;

with Gtk.Adjustment;
with Gtk.Drawing_Area;

with Pango.Font;
with Pango.Layout;

package Gtkada.Canvas is

   type Interactive_Canvas_Record is new
     Gtk.Drawing_Area.Gtk_Drawing_Area_Record with private;
   type Interactive_Canvas is access all Interactive_Canvas_Record'Class;
   --  A canvas on which items are put.
   --  Each item can be moved interactively by the user, and links can be
   --  drawn automatically from an item to another.
   --  This widget can be inserted directly in a scrolled window to provide
   --  support for scrolling.

   type Canvas_Item_Record is abstract new Glib.Graphs.Vertex with private;
   type Canvas_Item is access all Canvas_Item_Record'Class;
   --  An item that can be put on the canvas.
   --  This is an abstract type, as it does not provide any default drawing
   --  routine. You must override the abstract Draw subprogram.

   type Canvas_Link_Record is new Glib.Graphs.Edge with private;
   type Canvas_Link is access all Canvas_Link_Record'Class;
   type Canvas_Link_Access is access all Canvas_Link_Record;
   --  A link between two items in the canvas.
   --  The implementation provided in this package provides links that can
   --  be either straight links or curved links.
   --  This type is provided as a tagged type so that you can associated your
   --  own user data with it.

   -------------------
   -- Customization --
   -------------------
   --  These are the default configuration values for the canvas. All the
   --  values can be changed by the Configure subprogram.

   Default_Annotation_Font  : constant String := "Helvetica 8";
   --  Font used when displaying link annotation. See Pango.Font for the
   --  format.

   Default_Grid_Size        : constant := 15;
   --  Number of pixels between two dots on the grid.
   --  This is used for both horizontal and vertical orientation.

   Default_Arc_Link_Offset  : constant := 25;
   --  Distance between two parallel arcs for two links. This is not the exact
   --  distance, and it only used to compute the control points for the bezier
   --  curves.

   Default_Arrow_Angle      : constant := 30;
   --  Half angle for the arrows in degres

   Default_Arrow_Length     : constant := 6;
   --  Length of the arrows in pixels.

   Default_Motion_Threshold : constant := 4.0;
   --  Mimimum motion the mouse must have before we start moving the selected
   --  item. If the mouse has moved less than that amount of pixels in any
   --  direction, then the mouse click is considered as being a selection
   --  only and is transfered to the item itself.
   --  This is in screen coordinates

   ----------------
   -- Enum types --
   ----------------

   type Arrow_Type is
     (No_Arrow,
      --  the link does not have an arrow

      Start_Arrow,
      --  the link has an arrow at its beginning

      End_Arrow,
      --  the link has an arrow at the end

      Both_Arrow
      --  the link has an arrow on both sides
     );
   --  Indicate whether the links have an arrow or not.

   -----------------------
   -- Creating a canvas --
   -----------------------

   procedure Gtk_New
     (Canvas : out Interactive_Canvas; Auto_Layout : Boolean := True);
   --  Create a new empty Canvas.
   --  If Auto_Layout is True, then the items are automatically positioned as
   --  they are put in the canvas, if no coordinates are specified.

   procedure Initialize
     (Canvas      : access Interactive_Canvas_Record'Class;
      Auto_Layout : Boolean := True);
   --  Internal function used to initialize the canvas.

   procedure Configure
     (Canvas : access Interactive_Canvas_Record;
      Grid_Size        : Glib.Guint := Default_Grid_Size;
      Annotation_Font  : Pango.Font.Pango_Font_Description :=
                           Pango.Font.From_String (Default_Annotation_Font);
      Arc_Link_Offset  : Glib.Gint := Default_Arc_Link_Offset;
      Arrow_Angle      : Glib.Gint := Default_Arrow_Angle;
      Arrow_Length     : Glib.Gint := Default_Arrow_Length;
      Motion_Threshold : Glib.Gdouble := Default_Motion_Threshold);
   --  Change the parameters for the canvas.
   --  A Grid_Size of 0 means than no grid should be drawn in the background of
   --  canvas. Note that in that case you can never activate Align_On_Grid.
   --  This setting doesn't apply if you have redefined Draw_Background, which
   --  may not draw a grid.

   function Get_Vadj
     (Canvas : access Interactive_Canvas_Record'Class)
      return Gtk.Adjustment.Gtk_Adjustment;
   --  Return the vertical adjustment associated with Canvas

   function Get_Hadj
     (Canvas : access Interactive_Canvas_Record'Class)
      return Gtk.Adjustment.Gtk_Adjustment;
   --  Return the horizontal adjustment associated with Canva

   procedure Get_Bounding_Box
     (Canvas : access Interactive_Canvas_Record'Class;
      Width  : out Glib.Gdouble;
      Height : out Glib.Gdouble);
   --  Return the size occupied by the items drawn on the canvas.

   procedure Draw_Area
     (Canvas : access Interactive_Canvas_Record'Class;
      Rect   : Cairo.Region.Cairo_Rectangle_Int);
   --  Draw in Canvas the specified area.

   procedure Draw_All
     (Canvas : access Interactive_Canvas_Record'Class;
      Cr     : Cairo.Cairo_Context);
   --  Draws the whole canvas in Cr. Useful to print the canvas on an SVG or
   --  PNG surface.

   procedure Draw_Background
     (Canvas : access Interactive_Canvas_Record;
      Cr     : Cairo.Cairo_Context);
   --  Draw the background of the canvas. This procedure should be overriden if
   --  you want to draw something else on the background. It must first clear
   --  the area on the screen.
   --
   --  The default implementation draws a grid.
   --
   --  An example implementation that draws a background image is shown at the
   --  end of this file.

   procedure Draw_Grid
     (Canvas : access Interactive_Canvas_Record;
      Cr     : Cairo.Cairo_Context);
   --  Helper function that can be called from Draw_Background. It cannot be
   --  used directly as Draw_Background, since it doesn't clear the area first.

   procedure Set_Orthogonal_Links
     (Canvas : access Interactive_Canvas_Record;
      Orthogonal : Boolean);
   --  If Orthogonal is True, then all the links will be drawn only with
   --  vertical and horizontal lines. This is not applied for the second or
   --  more link between two items.

   function Get_Orthogonal_Links
     (Canvas : access Interactive_Canvas_Record) return Boolean;
   --  Return True if the links are only drawn horizontally and vertically.

   procedure Align_On_Grid
     (Canvas : access Interactive_Canvas_Record;
      Align  : Boolean := True);
   --  Choose whether the items should be aligned on the grid when moved.
   --  Existing items are not moved even if you set this parameter to True,
   --  this will only take effect the next time the items are moved.

   function Get_Align_On_Grid
     (Canvas : access Interactive_Canvas_Record) return Boolean;
   --  Return True if items are currently aligned on grid.

   procedure Move_To
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class;
      X, Y   : Glib.Gint := Glib.Gint'First);
   --  Move the item in the canvas, to world coordinates (X, Y).
   --  Item is assumed to be already in the canvas.
   --  If you leave both coordinates X and Y to their default value, then the
   --  item's location will be automatically computed when you layout the
   --  canvas (it is your responsability to call Layout).

   procedure Set_Items
     (Canvas : access Interactive_Canvas_Record;
      Items  : Glib.Graphs.Graph);
   --  Set the items and links to display in the canvas from Items.
   --  All items previously in the canvas are removed, and replaced by the
   --  vertices in Items.
   --  Note that the vertices in Items must be in Canvas_Item_Record'Class, and
   --  the links must be in Canvas_Link_Record'Class.
   --  If you do not have an automatic layout set up in Canvas, you need to set
   --  the coordinates of all the vertices by calling Move_To separately.
   --
   --  You mustn't destroy items yourself, this is done automatically when the
   --  canvas is destroyed.

   procedure Put
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class;
      X, Y   : Glib.Gint := Glib.Gint'First);
   --  Add a new item to the canvas, at world coordinates (X, Y).
   --  The item is added at a specific location.
   --  If you leave both X and Y to their default value, the item's location
   --  will be computed automatically when you call Layout on the canvas,
   --  unless Auto_Layout has been set, in which case the position will be
   --  computed immediately.

   function Item_At_Coordinates
     (Canvas : access Interactive_Canvas_Record;
      X, Y : Glib.Gint) return Canvas_Item;
   --  Return the item at world coordinates (X, Y) which is on top of all
   --  others.
   --  null is returned if there is no such item.

   function Item_At_Coordinates
     (Canvas : access Interactive_Canvas_Record; Event : Gdk.Event.Gdk_Event)
      return Canvas_Item;
   --  Same as above, but using the canvas coordinates of the event, taking
   --  into account the current zoom level and current scrolling

   procedure Item_At_Coordinates
     (Canvas : access Interactive_Canvas_Record;
      Event  : Gdk.Event.Gdk_Event;
      Item   : out Canvas_Item;
      X, Y   : out Glib.Gint);
   --  Same as above, but also returns the coordinates (X, Y) within the item.
   --  The coordinates are not set if Item is null on exit.

   procedure Clear (Canvas : access Interactive_Canvas_Record);
   --  Remove all items from the canvas

   procedure Remove
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class);
   --  Remove an item and all the links to and from it from the canvas.
   --  The item itself is not freed, but the links are.
   --  Nothing is done if the item is not part of the canvas.

   procedure Item_Updated
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class);
   --  This should be called when Item has changed the contents of its
   --  pixmap, and thus the Canvas should be updated.

   procedure Refresh_Canvas (Canvas : access Interactive_Canvas_Record);
   --  Redraw the whole canvas (both in the double buffer and on the screen).

   procedure Raise_Item
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class);
   --  Raise the item so that it is displayed on top of all the others
   --  The canvas is refreshed as needed to reflect the change.
   --  Nothing happens if Item is not part of the canvas.

   procedure Lower_Item
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class);
   --  Lower the item so that it is displayed below all the others.
   --  The canvas is refreshed as needed to reflect the change.
   --  Nothing happens if Item is not part of the canvas.

   function Is_On_Top
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class) return Boolean;
   --  Return True if Item is displayed on top of all the others in the canvas.

   procedure Show_Item
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class);
   --  Scroll the canvas so that Item is visible. Nothing is done if the item
   --  is already visible

   procedure Align_Item
     (Canvas  : access Interactive_Canvas_Record;
      Item    : access Canvas_Item_Record'Class;
      X_Align : Float := 0.5;
      Y_Align : Float := 0.5);
   --  Scroll the canvas so that the Item appears at the given location in the
   --  canvas. If X_Align is 0.0, the item is align on the left. With 0.5, it
   --  is centered horizontally. If 1.0, it is aligned on the right.

   function Get_Arrow_Angle
     (Canvas : access Interactive_Canvas_Record'Class) return Glib.Gdouble;
   --  Return the angle of arrows in the canvas.

   function Get_Arrow_Length
     (Canvas : access Interactive_Canvas_Record'Class) return Glib.Gint;
   --  Return the length of arrows in the canvas.

   --------------------------
   -- Iterating over items --
   --------------------------

   type Item_Processor is access function
     (Canvas : access Interactive_Canvas_Record'Class;
      Item   : access Canvas_Item_Record'Class) return Boolean;

   procedure For_Each_Item
     (Canvas            : access Interactive_Canvas_Record;
      Execute           : Item_Processor;
      Linked_From_Or_To : Canvas_Item := null);
   --  Execute an action on each of the items contained in the canvas.
   --  If Execute returns False, we stop traversing the list of children.
   --  It is safe to remove the items in Item_Processor.
   --
   --  If Linked_From_Or_To is not null, then only the items linked to this one
   --  will be processed. It is possible that a given item will be returned
   --  twice, if it is both linked to and from the item.

   type Item_Iterator is private;

   function Start
     (Canvas            : access Interactive_Canvas_Record;
      Linked_From_Or_To : Canvas_Item := null;
      Selected_Only     : Boolean := False) return Item_Iterator;
   --  Return the first item in the canvas.
   --  The same restriction as above applies if Linked_From_Or_To is not null.

   procedure Next (Iter : in out Item_Iterator);
   function Next (Iter : Item_Iterator) return Item_Iterator;
   --  Move the iterator to the next item.
   --  All items will eventually be returned if you do not add new items during
   --  the iteration and none are removed. However, it is safe to remove items
   --  at any time, except the current item

   function Get (Iter : Item_Iterator) return Canvas_Item;
   --  Return the item pointed to by the iterator.
   --  null is returned when there are no more item in the canvas.

   function Is_Linked_From (Iter : Item_Iterator) return Boolean;
   --  Return True if there is a link from:
   --     Get (Iter) -> Linked_From_Or_To
   --  Linked_From_Or_To is the item passed to Start. False is returned if this
   --  item was null.

   -------------
   -- Zooming --
   -------------

   procedure Zoom
     (Canvas  : access Interactive_Canvas_Record;
      Percent : Glib.Gdouble := 1.0;
      Length  : Duration := 0.0);
   --  Zoom in or out in the canvas.
   --
   --  Length is the length of the zooming animation.
   --
   --  Note that one possible use for this function is to refresh the canvas
   --  and emit the "zoomed" signal, which might redraw all the items. This can
   --  be accomplished by keeping the default 1.0 value for Percent.

   function Get_Zoom
     (Canvas : access Interactive_Canvas_Record) return Glib.Gdouble;
   --  Return the current zoom level

   procedure Get_World_Coordinates
     (Canvas : access Interactive_Canvas_Record'Class;
      X, Y   : out Glib.Gdouble;
      Width  : out Glib.Gdouble;
      Height : out Glib.Gdouble);
   --  Return the world coordinates of Canvas.

   ---------------------
   -- Layout of items --
   ---------------------

   type Layout_Algorithm is access procedure
     (Canvas          : access Interactive_Canvas_Record'Class;
      Graph           : Glib.Graphs.Graph;
      Force           : Boolean;
      Vertical_Layout : Boolean);
   --  A general layout algorithm. It should compute the position of all the
   --  vertices of the graph, and set them directly in the graph itself.
   --  Note: all the vertices in the graph are of type Canvas_Item_Record'Class
   --  and you should use that to set the coordinates through a call to
   --  Move_To.
   --
   --  Algorithms are encouraged to preserve the current layout as much as
   --  possible, taking into account items that have been moved manually by
   --  the user, so that the latter can preserver his mental map of the graph.
   --  However, if Force is set to True, then the whole layout should be
   --  recomputed as if all items had just been inserted.
   --
   --  Items that have just been inserted in the graph, but whose position has
   --  never been computed, are set at coordinates (Gint'First, Gint'First).
   --  Check the result of Get_Coord.
   --
   --  This function doesn't need to align items, this is done automatically by
   --  the canvas if necessary.

   procedure Set_Layout_Algorithm
     (Canvas    : access Interactive_Canvas_Record;
      Algorithm : Layout_Algorithm);
   --  Set the layout algorithm to use to compute the position of the items.
   --  Algorithm mustn't be null.

   procedure Default_Layout_Algorithm
     (Canvas          : access Interactive_Canvas_Record'Class;
      Graph           : Glib.Graphs.Graph;
      Force           : Boolean;
      Vertical_Layout : Boolean);
   --  The default algorithm used in the canvas.
   --  Basically, items are put next to each other, unless there is a link
   --  between two items. In that case, the second item is put below the first,
   --  as space allows.

   procedure Set_Auto_Layout
     (Canvas      : access Interactive_Canvas_Record;
      Auto_Layout : Boolean);
   --  If Auto_Layout is true, then every time an item is inserted in the
   --  canvas, the layout algorithm is called. If set to False, it is the
   --  responsability of the caller to call Layout below to force a
   --  recomputation of the layout, preferably after inserting a number of
   --  items.

   procedure Set_Layout_Orientation
     (Canvas          : access Interactive_Canvas_Record;
      Vertical_Layout : Boolean := False);
   --  Specify the layout orientation to use for this canvas. The setting is
   --  passed as a parameter to the layout algorithm

   procedure Layout
     (Canvas : access Interactive_Canvas_Record;
      Force  : Boolean := False);
   --  Recompute the layout of the canvas.
   --  Force can be used to control the layout algorithm, as described above
   --  for Layout_Algorithm.

   -----------
   -- Links --
   -----------

   procedure Configure
     (Link  : access Canvas_Link_Record;
      Arrow : Arrow_Type := End_Arrow;
      Descr : Glib.UTF8_String := "");
   --  Configure a link.
   --  The link is an oriented bound between two items on the canvas.
   --  If Descr is not the empty string, it will be displayed in the middle
   --  of the link, and should indicate what the link means.
   --  Arrow indicates whether some arrows should be printed as well.

   function Get_Descr
     (Link : access Canvas_Link_Record) return Glib.UTF8_String;
   --  Return the description for the link, or "" if there is none

   function Get_Arrow_Type
     (Link : access Canvas_Link_Record) return Arrow_Type;
   --  Return the location of the arrows on Link

   procedure Set_Src_Pos
     (Link : access Canvas_Link_Record; X_Pos, Y_Pos : Glib.Gfloat := 0.5);
   --  Set the position of the link's attachment in its source item.
   --  X_Pos and Y_Pos should be given between 0.0 and 1.0 (from left to right
   --  or top to bottom)..
   --  By default, all links are considered to be attached to the center of
   --  items. However, in some cases it is more convenient to attach it to a
   --  specific part of the item. For instance, you can force a link to always
   --  start from the top of the item by setting Y_Pos to 0.0.

   procedure Set_Dest_Pos
     (Link : access Canvas_Link_Record; X_Pos, Y_Pos : Glib.Gfloat := 0.5);
   --  Same as Set_Src_Pos for the destination item

   procedure Get_Src_Pos
     (Link : access Canvas_Link_Record; X, Y : out Glib.Gfloat);
   --  Return the attachment position of the link along its source item

   procedure Get_Dest_Pos
     (Link : access Canvas_Link_Record; X, Y : out Glib.Gfloat);
   --  Return the attachment position of the link along its destination item

   function Has_Link
     (Canvas   : access Interactive_Canvas_Record;
      From, To : access Canvas_Item_Record'Class;
      Name     : Glib.UTF8_String := "") return Boolean;
   --  Test whether there is a link from From to To, with the same name.
   --  If Name is the empty string "", then no check is done on the name,
   --  and True if returned if there is any link between the two items.

   procedure Add_Link
     (Canvas : access Interactive_Canvas_Record;
      Link   : access Canvas_Link_Record'Class;
      Src    : access Canvas_Item_Record'Class;
      Dest   : access Canvas_Item_Record'Class;
      Arrow  : Arrow_Type := End_Arrow;
      Descr  : Glib.UTF8_String := "");
   --  Add Link in the canvas. This connects the two items Src and Dest.
   --  Simpler procedure to add a standard link.
   --  This takes care of memory allocation, as well as adding the link to
   --  the canvas.

   procedure Remove_Link
     (Canvas : access Interactive_Canvas_Record;
      Link   : access Canvas_Link_Record'Class);
   --  Remove a link from the canvas.
   --  It also destroys the link itself, and free the memory allocated to it.
   --  Nothing is done if Link does not belong to canvas.

   type Link_Processor is access function
     (Canvas : access Interactive_Canvas_Record'Class;
      Link   : access Canvas_Link_Record'Class) return Boolean;

   procedure For_Each_Link
     (Canvas   : access Interactive_Canvas_Record;
      Execute  : Link_Processor;
      From, To : Canvas_Item := null);
   --  Execute an action on each of the links contained in the canvas.
   --  If Execute returns False, we stop traversing the list of links.
   --  It is safe to remove the link from the list in Link_Processor.
   --
   --  (From, To) can be used to limit what links are looked for.
   --
   --  ??? Would be nicer to give direct access to the Graph iterators

   procedure Destroy (Link : in out Canvas_Link_Record);
   --  Method called every time a link is destroyed. You should override this
   --  if you define your own link types.
   --  Note that the link might already have been removed from the canvas
   --  when this subprogram is called.
   --  This shouldn't free the link itself, only its fields.

   -------------------
   -- Drawing links --
   -------------------
   --  Drawing of links can be controlled at several levels:
   --    - Redefining Update_Links gives control at the canvas level. This can
   --      be used to implement routing algorithms for the links where the
   --      routes must be computed before any link is actually drawn (otherwise
   --      it is better to redefine Draw_Link). It can also be used to control
   --      in what order the links should be drawn.
   --    - Redefining Draw_Link gives the opportunity to draw links any way you
   --      need (several bends, ...). It can be used to control the routing of
   --      this specific link, for routing algorithms that only rely on the
   --      items layout and not on other links. Otherwise see Update_Links.
   --    - Redefining Draw_Straight_Line if slightly lower-level. This is
   --      called by the default Draw_Link procedure, once the ends of the
   --      links have been computed.

   procedure Update_Links
     (Canvas         : access Interactive_Canvas_Record;
      Cr             : Cairo.Cairo_Context;
      Invert_Mode    : Boolean;
      From_Selection : Boolean);
   --  Redraw all the links in the canvas, after the items have been laid out.
   --
   --  If From_Selection is true, then only the links to or from one of the
   --  selected items need to be drawn.

   procedure Draw_Link
     (Canvas      : access Interactive_Canvas_Record'Class;
      Link        : access Canvas_Link_Record;
      Cr          : Cairo.Cairo_Context;
      Edge_Number : Glib.Gint;
      Show_Annotation : Boolean := True);
   --  Redraw the link on the canvas.
   --  Note that this is a primitive procedure of Link, not of Canvas, and thus
   --  can easily be overrided for specific links. The default version draws
   --  either straight or arc links (the latter when there are multiple links
   --  between two given items).
   --  This function shouldn't be called if one of the two ends of the link is
   --  invisible.
   --
   --  Cr is the Cairo_Context that is used to draw the link.
   --  The link is drawn using the current cairo brush, so if you need to
   --  specify some particular color, you can do it directly in the
   --  Cairo_Context
   --
   --  Edge_Number indicates the index of link in the list of links that join
   --  the same source to the same destination. It should be used so that two
   --  links do not overlap (for instance, the default is to draw the first
   --  link straight, and the others as arcs).

   type Item_Side is (East, West, North, South);
   --  Each side of an item, along its rectangle bounding box

   procedure Clip_Line
     (Src   : access Canvas_Item_Record;
      Canvas : access Interactive_Canvas_Record'Class;
      To_X  : Glib.Gint;
      To_Y  : Glib.Gint;
      X_Pos : Glib.Gfloat;
      Y_Pos : Glib.Gfloat;
      Side  : out Item_Side;
      X_Out : out Glib.Gint;
      Y_Out : out Glib.Gint);
   --  Clip the line that goes from Src at pos (X_Pos, Y_Pos) to (To_X, To_Y)
   --  in world coordinates.
   --  The intersection between that line and the border of Rect is returned
   --  in (X_Out, Y_Out). The result should be in world coordinates.
   --  X_Pos and Y_Pos have the same meaning as Src_X_Pos and Src_Y_Pos in the
   --  link record.
   --  This procedure is called when computing the position for the links
   --  within the default Draw_Link procedure. The default implementation only
   --  works with rectangular items. The computed coordinates are then passed
   --  on directly to Draw_Straight_Line.

   procedure Draw_Straight_Line
     (Link      : access Canvas_Link_Record;
      Cr        : Cairo.Cairo_Context;
      Src_Side  : Item_Side;
      X1, Y1    : Glib.Gdouble;
      Dest_Side : Item_Side;
      X2, Y2    : Glib.Gdouble);
   --  Draw a straight link between two points. This could be overriden if you
   --  need to draw an something along the link.
   --  The links goes from (Src, X1, Y1) to (Dest, X2, Y2), in canvas
   --  coordinates. The coordinates have already been clipped so that they do
   --  not override the item.

   ---------------
   -- Selection --
   ---------------

   procedure Clear_Selection (Canvas : access Interactive_Canvas_Record);
   --  Clear the list of currently selected items.

   procedure Add_To_Selection
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class);
   --  Add Item to the selection.  This is only meaningful during a drag
   --  operation (ie during a button press and the matching button
   --  release). Item will be moved at the same time that the selection is
   --  moved.
   --  Item is not added again if it is already in the selection.
   --  This function can be called from the Button_Click subprogram to force
   --  moving items.
   --  This emits the "item_selected" signal.

   procedure Remove_From_Selection
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class);
   --  Remove Item from the selection.
   --  This emits the "item_unselected" signal.

   procedure Select_All (Canvas : access Interactive_Canvas_Record);
   --  Select all the Item in the canvas.

   function Is_Selected
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class) return Boolean;
   --  Return True if the item is currently selected

   ------------------------
   -- Items manipulation --
   ------------------------

   function Canvas
     (Item : access Canvas_Item_Record) return Interactive_Canvas;
   --  Retrieve the canvas this item is attached to, or null if it does not
   --  belong to a canvas.

   procedure Selected
     (Item        : access Canvas_Item_Record;
      Canvas      : access Interactive_Canvas_Record'Class;
      Is_Selected : Boolean);
   --  Called when the item is selected or unselected.
   --  The default is to do nothing.

   function Point_In_Item
     (Item : access Canvas_Item_Record;
      X, Y : Glib.Gint) return Boolean;
   --  This function should return True if (X, Y) is inside the item. X and Y
   --  are in world coordinates.
   --  This function is meant to be overriden for non-rectangular items, since
   --  the default behavior works for rectangular items.
   --  This function is never called for invisible items

   procedure Set_Screen_Size
     (Item   : access Canvas_Item_Record;
      Width  : Glib.Gint;
      Height : Glib.Gint);
   --  Set the size of bounding box for the item in world coordinates.
   --  The item itself needn't occupy the whole area of this bounding box,
   --  see Point_In_Item.
   --  You need to redraw the item, and call Item_Updated to force the canvas
   --  to refresh the screen.

   procedure Draw_Selected
     (Item : access Canvas_Item_Record;
      Cr   : Cairo.Cairo_Context);
   --  Draws a selected item. By default, this adds a semi-transparent overlay
   --  above the item, drawn using the below call to Draw

   procedure Draw
     (Item : access Canvas_Item_Record;
      Cr   : Cairo.Cairo_Context) is abstract;
   --  This subprogram, that must be overridden, should draw the item on
   --  Cr. The Item is drawn from coordinates (0,0), and does not need to take
   --  care of the zoom level.
   --  If you need to change the contents of the item, you should call
   --  Item_Updated after having done the drawing.

   procedure Destroy (Item : in out Canvas_Item_Record);
   --  Free the memory occupied by the item (not the item itself). You should
   --  override this function if you define your own widget type, but always
   --  call the parent's Destroy subprogram.

   function On_Button_Click
     (Item  : access Canvas_Item_Record;
      Event : Gdk.Event.Gdk_Event_Button) return Boolean;
   --  Function called whenever mouse events occured.
   --  The following mouse events may be received:
   --    Mouse_Press,
   --    Motion_Notify
   --      (only once the mouse is pressed, and On_Button_Click returned True),
   --    Mouse_Release
   --      (only once the mouse is pressed, and On_Button_Click returned True),
   --  Returns whether the event was handled or not.
   --
   --  The coordinates (X, Y) in the Event are relative to the top-left corner
   --  of Item.

   function Get_Coord
     (Item : access Canvas_Item_Record)
      return Cairo.Region.Cairo_Rectangle_Int;
   --  Return the coordinates and size of the bounding box for item, in world
   --  coordinates.
   --  If the item has never been resized, it initially has a width and height
   --  of 1.

   procedure Set_Visibility
     (Item    : access Canvas_Item_Record;
      Visible : Boolean);
   --  Set the visibility status of the item. An invisible item will not be
   --  visible on the screen, and will not take part in the computation of the
   --  the scrollbars for the canvas.
   --  The canvas is not refreshed (this is your responsibility to do it after
   --  you have finished doing all the modifications).

   function Is_Visible (Item : access Canvas_Item_Record) return Boolean;
   --  Return True if the item is currently visible

   function Is_From_Auto_Layout
     (Item : access Canvas_Item_Record) return Boolean;
   --  Return True if the current location of the item is the result from the
   --  auto layout algorithm.
   --  False is returned if the item was moved manually by the user.

   --------------------
   -- Buffered items --
   --------------------

   type Buffered_Item_Record is new Canvas_Item_Record with private;
   type Buffered_Item is access all Buffered_Item_Record'Class;
   --  A widget that has a double-buffer associated. You should use this one
   --  when drawing items can take a long time, or you do not want to handle
   --  the zoom yourself.
   --  You only need to update the contents of the double pixmap when the
   --  contents of the item changes, since all the drawing and zooming is
   --  taken care of automatically. Once the drawing is done, call Item_Updated
   --  to force the canvas to refresh the screen.
   --  This buffered_item is meant to handle rectangular items. However, it can
   --  be used for polygonal items by overriding Draw. The new version should
   --  set the clip mask for the GC, then call Draw for the buffered item, and
   --  finally reset the clip mask. The clip mask must take into account the
   --  current zoom level.

   function Surface (Item : access Buffered_Item_Record)
      return Cairo.Cairo_Surface;
   --  Return the double-buffer.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "background_click"
   --  procedure Handler (Canvas : access Interactive_Canvas_Record'Class;
   --                     Event  : Gdk.Event.Gdk_Event);
   --
   --  Called every time the user clicks in the background (ie not on an item,
   --  or On_Button_Click would be called).
   --  This is called both on Button_Release and Button_Press events.
   --  The coordinates (X, Y) in the Event are relative to the top-left corner
   --  of Canvas.
   --
   --  - "item_selected"
   --  procedure Handler (Canvas : access Interactive_Canvas_Record'Class;
   --                     Item   : Canvas_Item);
   --
   --  Emitted when the user has clicked on an item to select it, ie before any
   --  drag even has occured. This is a good time to add other items to the
   --  selection if you need. At thee same time, the primitive operation
   --  Selected is called for the item.
   --
   --  - "item_unselected"
   --  procedure Handler (Canvas : access Interactive_Canvas_Record'Class;
   --                     Item   : Canvas_Item);
   --
   --  Emitted when the Item was unselected. At the same time, the primitive
   --  operation Selected is called for the item.
   --
   --  - "item_moved"
   --  procedure Handler (Canvas : access Interactive_Canvas_Record'Class;
   --                     Item   : Canvas_Item);
   --
   --  Emitted when Item has been moved. New coordinates have been assigned to
   --  Item. However, the canvas hasn't been refreshed yet. This signal might
   --  be called multiple time when the user finishes a drag action, in case
   --  there were several selected items.
   --
   --  - "zoomed"
   --  procedure Handler (Canvas : access Interactive_Canvas_Record'Class);
   --
   --  Emitted when the canvas has been zoomed in or out. You do not need to
   --  redraw the items yourself, since this will be handled by calls to Draw
   --
   --  - "set_scroll_adjustments"
   --  procedure Handler (Canvas : access Interactive_Canvas_Record'Class);
   --
   --  Emitted when the canvas has scrolled.
   --
   --  </signals>

   Signal_Background_Click       : constant Glib.Signal_Name :=
                                     "background_click";
   Signal_Item_Selected          : constant Glib.Signal_Name :=
                                     "item_selected";
   Signal_Item_Unselected        : constant Glib.Signal_Name :=
                                     "item_unselected";
   Signal_Item_Moved             : constant Glib.Signal_Name :=
                                     "item_moved";
   Signal_Zoomed                 : constant Glib.Signal_Name :=
                                     "zoomed";
   Signal_Set_Scroll_Adjustments : constant Glib.Signal_Name :=
                                     "set_scroll_adjustments";

private

   type String_Access is access Glib.UTF8_String;

   type Canvas_Link_Record is new Glib.Graphs.Edge with record
      Descr      : String_Access;
      Arrow      : Arrow_Type := End_Arrow;

      Src_X_Pos  : Glib.Gfloat := 0.5;
      Src_Y_Pos  : Glib.Gfloat := 0.5;
      Dest_X_Pos : Glib.Gfloat := 0.5;
      Dest_Y_Pos : Glib.Gfloat := 0.5;
      --  Position of the link's attachment in each of the src and dest items.
   end record;

   type Interactive_Canvas_Record is new
     Gtk.Drawing_Area.Gtk_Drawing_Area_Record
   with record
      Children          : Glib.Graphs.Graph;
      World_X, World_Y  : Glib.Gdouble;
      --  The World coordinates at canvas (0,0)

      Layout            : Layout_Algorithm := Default_Layout_Algorithm'Access;
      Auto_Layout       : Boolean := True;
      Vertical_Layout   : Boolean := False;
      --  The algorithm to use when laying out items on the canvas.

      World_X_At_Click  : Glib.Gdouble := 0.0;
      World_Y_At_Click  : Glib.Gdouble := 0.0;
      --  Coordinates of the last button_press event in the canvas.
      --  These are world-coordinates, so that even if the canvas is scrolled
      --  they remain valid

      Selected_Count    : Natural := 0;
      --  Number of selected items

      Offset_X_World    : Glib.Gint := 0;
      Offset_Y_World    : Glib.Gint := 0;
      --  How much world-coordinates have we moved the mouse since the last
      --  button press event ?

      Mouse_Has_Moved   : Boolean;
      --  True if mouse has moved while the button was clicked. This is used
      --  to distinguish between item motion and item selection.

      Background_Press  : Boolean;
      --  True if the mouse press event occured in the background

      Item_Press        : Canvas_Item;
      --  Points to the canvas item that received the press event

      Show_Item                    : Canvas_Item;
      Show_Canvas_X, Show_Canvas_Y : Glib.Gdouble;
      --  The item that should be made visible when the canvas is resized.
      --  This is required since the canvas doesn't necessarily have a size yet
      --  when Show_Item() is called the first time.

      Grid_Size         : Glib.Guint := Default_Grid_Size;
      --  The current number of pixels between each dot of the grid. If this
      --  is strictly below 2, the grid is not drawn.

      Arc_Link_Offset   : Glib.Gint := Default_Arc_Link_Offset;
      Arrow_Angle       : Glib.Gdouble;
      Arrow_Length      : Glib.Gint := Default_Arrow_Length;
      Motion_Threshold  : Glib.Gdouble := Default_Motion_Threshold;
      Align_On_Grid     : Boolean := False;

      Black_Color     : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Sel_Color       : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;

      Annotation_Layout : Pango.Layout.Pango_Layout;
      --  Layout used to draw the annotations

      Hadj, Vadj        : Gtk.Adjustment.Gtk_Adjustment;
      Scrolling_Timeout_Id : Glib.Main.G_Source_Id := 0;

      Orthogonal_Links : Boolean := False;
      --  True if the links should be orthogonal

      Surround_Box_Scroll : Glib.Gdouble;
      --  Amount of scrolling for each step while the cursor is left in the
      --  surrounding box.

      Zoom                : Glib.Gdouble := 1.0;
      --  Zoom level in percent (100% is normal size)

      Initial_Zoom        : Glib.Gdouble := 1.0;
      Target_Zoom         : Glib.Gdouble := 1.0;
      Zoom_Duration       : Duration := 0.0;
      Zoom_Start          : Ada.Calendar.Time;
      Zoom_X              : Glib.Gdouble := 0.0;
      Zoom_Y              : Glib.Gdouble := 0.0;
      --  Variables used while smooth-scrolling the canvas

      Freeze           : Boolean := False;
   end record;

   type Canvas_Item_Record is abstract new Glib.Graphs.Vertex with record
      Canvas           : Interactive_Canvas := null;
      Coord            : aliased Cairo.Region.Cairo_Rectangle_Int :=
        (0, 0, 0, 0);
      --  This is the bounding box of the item

      Visible          : Boolean := True;
      Selected         : Boolean := False;

      From_Auto_Layout : Boolean := True;
      --  True if the item's current location is the result of the automatic
      --  layout algorithm.
   end record;

   type Buffered_Item_Record is new Canvas_Item_Record with record
      Pixmap : Cairo.Cairo_Surface := Cairo.Null_Surface;
   end record;

   procedure Set_Screen_Size
     (Item   : access Buffered_Item_Record;
      Width, Height  : Glib.Gint);
   --  See documentation from inherited subprogram

   procedure Draw
     (Item : access Buffered_Item_Record;
      Cr   : Cairo.Cairo_Context);
   --  Draw the item's double-buffer onto Dest.

   procedure Destroy (Item : in out Buffered_Item_Record);
   --  Free the double-buffer allocated for the item

   type Item_Iterator is record
      Vertex            : Glib.Graphs.Vertex_Iterator;
      Edge              : Glib.Graphs.Edge_Iterator;
      Linked_From_Or_To : Canvas_Item;
      Selected_Only     : Boolean;
   end record;

   pragma Inline (Get_Arrow_Type);

end Gtkada.Canvas;

--  <example>
--  --  The following example shows a possible Draw_Background procedure,
--  --  that draws a background image on the canvas's background. It fully
--  --  handles zooming and tiling of the image. Note that drawing a large
--  --  image will dramatically slow down the performances.
--
--  Bg_Image : constant String := "my_background.png";
--
--  procedure Draw_Background
--    (Canvas : access Image_Canvas_Record;
--     Cr     : Cairo.Cairo_Context)
--  is
--     Surface    : Cairo.Cairo_Surface;
--     Background : Cairo.Cairo_Pattern;
--  begin
--     Surface := Cairo.Png.Create_From_Png (Bg_Image);
--     Background := Cairo.Pattern.Create_For_Surface (Surface);
--     Cairo.Pattern.Set_Extend (Canvas.Background, Cairo_Extend_Repeat);
--     Destroy (Surface);

--     Cairo.Save (Cr);
--     Cairo.Set_Source (Cr, Canvas.Background);
--     Cairo.Paint (Cr);
--     Cairo.Restore (Cr);
--  end Draw_Background;
--  </example>
