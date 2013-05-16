-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000, E. Briot, J. Brobecker and A. Charlet  --
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

with Ada.Numerics;                       use Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;
with Interfaces.C.Strings;               use Interfaces.C.Strings;
with System;
with Unchecked_Deallocation;
with GNAT.IO;                            use GNAT.IO;

with Cairo;                              use Cairo;
with Cairo.Image_Surface;                use Cairo.Image_Surface;
with Cairo.Pattern;                      use Cairo.Pattern;
with Cairo.Region;                       use Cairo.Region;
with Cairo.Surface;                      use Cairo.Surface;
with Pango.Cairo;                        use Pango.Cairo;

with Glib;                               use Glib;
with Glib.Graphs;                        use Glib.Graphs;
with Glib.Main;                          use Glib.Main;
with Glib.Object;                        use Glib.Object;
with Glib.Values;                        use Glib.Values;

with Gdk.Cairo;                          use Gdk.Cairo;
with Gdk.Color;                          use Gdk.Color;
with Gdk.Cursor;                         use Gdk.Cursor;
with Gdk.Event;                          use Gdk.Event;
with Gdk.Rectangle;                      use Gdk.Rectangle;
with Gdk.Window;                         use Gdk.Window;
with Gdk.Types;                          use Gdk.Types;
with Gdk.Types.Keysyms;                  use Gdk.Types.Keysyms;

with Gtk.Adjustment;                     use Gtk.Adjustment;
with Gtk.Arguments;                      use Gtk.Arguments;
with Gtk.Drawing_Area;                   use Gtk.Drawing_Area;
with Gtk.Enums;                          use Gtk.Enums;
with Gtk.Handlers;
with Gtk.Main;                           use Gtk.Main;
pragma Elaborate_All (Gtk.Main);
with Gtk.Style;                          use Gtk.Style;
with Gtk.Widget;                         use Gtk.Widget;

with Gtkada.Handlers;                    use Gtkada.Handlers;

with Pango.Font;                         use Pango.Font;
with Pango.Layout;                       use Pango.Layout;

--  TODO:
--   - would be nice to have a pixbuf item directly (for alpha layers)

package body Gtkada.Canvas is

   package Double_Elementary_Functions is new
     Ada.Numerics.Generic_Elementary_Functions (Gdouble);
   use Double_Elementary_Functions;

   use type Gdk_Window;
   use type System.Address;

   Traces : constant Boolean := False;

   Class_Record : GObject_Class := Uninitialized_Class;
   --  This pointer will keep a pointer to the C 'class record' for
   --  gtk. To avoid allocating memory for each widget, this may be done
   --  only once, and reused.
   --  ??? This is a global variable.

   Timeout_Between_Scrolls : constant := 50;
   --  Time between two scrollings when the mouse is in the bounding box.

   Scrolling_Margin : constant := 10;
   --  Width and height of the surrounding box in which "infinite"
   --  scrolling is started (it will continue while the mouse is kept in this
   --  area or moved outside of the canvas)

   Scrolling_Amount_Min      : constant Gdouble := 10.0;
   Scrolling_Amount_Max      : constant Gdouble := 20.0;
   Scrolling_Amount_Increase : constant Gdouble := 1.05;  --  +5% every step
   --  Number of pixels to scroll while the mouse is in the surrounding
   --  box. This is the initial value, and will keep increasing while the mouse
   --  is left in the box.

   Links_Threshold_While_Moving : constant := 20;
   --  Maximal number of links that are drawn while moving an item. This is
   --  used to make the canvas still usable when there are lots of links to a
   --  given item.

   Signals : constant chars_ptr_array :=
               (1 => New_String (String (Signal_Background_Click)),
                2 => New_String (String (Signal_Item_Selected)),
                3 => New_String (String (Signal_Zoomed)),
                4 => New_String (String (Signal_Set_Scroll_Adjustments)),
                5 => New_String (String (Signal_Item_Unselected)),
                6 => New_String (String (Signal_Item_Moved)));
   --  Array of the signals created for this widget

   -----------------
   -- Subprograms --
   -----------------
   --  Note: Some callbacks take Gtk_Widget_Record parameters, so that we can
   --  reuse the callbacks in Gtkada.Handlers, and thus save a lot of space
   --  in the GtkAda library.

   procedure Free is new Unchecked_Deallocation (String, String_Access);

   package Canvas_Timeout is
     new Glib.Main.Generic_Sources (Interactive_Canvas);

   function Expose
     (Canv  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event) return Boolean;
   --  Handle the expose events for a canvas.

   procedure Canvas_Destroyed (Canvas : access Gtk_Widget_Record'Class);
   --  Called when the canvas is being destroyed. All the items and links
   --  are removed, and the double-buffer is freed

   procedure Size_Allocate
     (Canv : access Gtk_Widget_Record'Class; Args : Gtk_Args);
   --  When the item is resized.

   function Button_Pressed
     (Canv  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Called when the user has pressed the mouse button in the canvas.
   --  This tests whether an item was selected.

   function Button_Release
     (Canv  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Called when the user has released the mouse button.
   --  If an item was selected, this refreshed the canvas.

   function Button_Motion
     (Canv  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Called when the user moves the mouse while a button is pressed.
   --  If an item was selected, the item is moved.

   function Key_Press
     (Canv  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Handle key events, to provide scrolling through Page Up, Page Down, and
   --  arrow keys.

   procedure To_World_Coordinates
     (Canvas  : access Interactive_Canvas_Record'Class;
      X       : Gint;
      Y       : Gint;
      X_World : out Gdouble;
      Y_World : out Gdouble);

   procedure To_World_Coordinates
     (Canvas  : access Interactive_Canvas_Record'Class;
      Event   : Gdk_Event;
      X_World : out Gdouble;
      Y_World : out Gdouble);

   procedure Draw_Area
     (Canvas : access Interactive_Canvas_Record'Class;
      Rect   : Cairo_Rectangle_Int;
      Cr     : Cairo_Context);

   procedure Draw_Orthogonal_Link
     (Canvas          : access Interactive_Canvas_Record'Class;
      Cr              : Cairo_Context;
      Link            : access Canvas_Link_Record'Class;
      Show_Annotation : Boolean);
   --  Draw a link on the screen, as possibly several orthogonal lines.
   --  This link includes both an arrow head on its destination, and an
   --  optional text displayed approximatively in its middle.

   procedure Draw_Straight_Link
     (Canvas          : access Interactive_Canvas_Record'Class;
      Cr              : Cairo_Context;
      Link            : access Canvas_Link_Record'Class;
      Show_Annotation : Boolean);
   --  Draw Link on the screen as a straight line.
   --  This link includes both an arrow head on its destination, and an
   --  optional text displayed approximatively in its middle.

   procedure Draw_Arc_Link
     (Canvas          : access Interactive_Canvas_Record'Class;
      Cr              : Cairo_Context;
      Link            : access Canvas_Link_Record'Class;
      Offset          : Gint;
      Show_Annotation : Boolean);
   --  Draw Link on the screen.
   --  The link is drawn as a curved link (ie there is an extra handle in its
   --  middle).
   --  This link includes both an arrow head on its destination, and an
   --  optional text displayed approximatively in its middle.

   procedure Draw_Self_Link
     (Canvas          : access Interactive_Canvas_Record'Class;
      Cr              : Cairo_Context;
      Link            : access Canvas_Link_Record'Class;
      Offset          : Gint;
      Show_Annotation : Boolean);
   --  Draw a link when its source and destination items are the same

   procedure Update_Adjustments
     (Canvas       : access Interactive_Canvas_Record'Class;
      Min_Max      : Boolean := True;
      Page_Size    : Boolean := True;
      Clip_Value   : Boolean := True;
      Send_Changed : Boolean := True);
   --  Update the adjustments of the canvas.
   --  The bounds for the adjustments are automatically computed, given the
   --  list of items in it.
   --  If Clip_Value is set, this also makes sure that the adjustment values
   --  remain in the adjustment range

   procedure Draw_Arrow_Head
     (Canvas : access Interactive_Canvas_Record'Class;
      Cr     : Cairo_Context;
      X, Y   : Gint;
      Angle  : Gdouble);
   --  Draw an arrow head at the position (X, Y) on the canvas. The position
   --  is given in pixels, and should include zoom processing.
   --  Angle is the angle of the main axis of the arrow.

   procedure Draw_Annotation
     (Canvas : access Interactive_Canvas_Record'Class;
      Cr     : Cairo_Context;
      X, Y   : Gint;
      Link   : access Canvas_Link_Record'Class);
   --  Print an annotation on the canvas.
   --  The annotation is centered around (X, Y), in pixels. These coordinates
   --  should already include zoom processing.

   procedure Set_Scroll_Adjustments
     (Canvas : access Gtk_Widget_Record'Class;
      Args   : GValues);
   --  Change the two adjustments used for the canvas (in a callback)

   procedure Scrolled (Canvas : access Gtk_Widget_Record'Class);
   --  Called everytime the value of one of the adjustments is changed.

   procedure Get_Bounding_Box
     (Canvas : access Interactive_Canvas_Record'Class;
      X_Min, X_Max, Y_Min, Y_Max : out Gdouble);
   --  Find the smallest bounding box for all the items in the canvas.
   --  Note that this does not include links, which might thus be found
   --  outside of this box.
   --  The returned values are in world coordinates

   procedure Test_Scrolling_Box
     (Canvas : access Interactive_Canvas_Record'Class;
      Mouse_X_In_Canvas, Mouse_Y_In_Canvas : Gint;
      X_Scroll                             : out Gdouble;
      Y_Scroll                             : out Gdouble);
   --  We keep moving the selection (and scrolling the canvas) as long as the
   --  mouse remains in a surrounding box around the canvas, or even outside
   --  the canvas. This is done even if the mouse doesn't move, so at to make
   --  it easier to move items.  This subprogram tests whether the pointer is
   --  found in that box, and returns the extra scrolling that should be
   --  done. (0, 0) is returned if the mouse is not in that box.
   --  (Mouse_X_In_Canvas, Mouse_Y_In_Canvas) are the screen coordinates of the
   --  mouse in the canvas.

   function Scrolling_Timeout (Canvas : Interactive_Canvas) return Boolean;
   --  Function called repeatedly while the mouse is in the scrolling box.
   --  This provides scrolling even when the mouse doesn't move

   procedure Scroll_Canvas_To_Area
     (Canvas             : access Interactive_Canvas_Record'Class;
      X1, Y1, X2, Y2     : Gdouble;
      Canvas_X, Canvas_Y : Gdouble := 0.5;
      Ignore_If_Visible  : Boolean := True;
      Report_Adj_Changed : Boolean := True);
   --  Scroll the visible area of the canvas so that the given area
   --  (X1, Y1) .. (X2, Y2) is made visible.
   --  These are in world coordinates.
   --  If Ignore_If_Visible is true and the area is already visible, do nothing
   --  (Canvas_X, Canvas_Y) indicates at which part of the canvas the region
   --  should be centered. If these are greater than 1.0, minimal scrolling is
   --  done.

   function Move_Selection
     (Canvas : access Interactive_Canvas_Record'Class;
      New_Offset_X_World, New_Offset_Y_World : Gdouble) return Boolean;
   --  Moves all selected items by a specific amount.
   --  The move is relative to the initial position of the items, and
   --  (Delta_X_World, Delta_Y_World) are given in world coordinates.
   --  Return True if the selection was actually moved, False if for some
   --  reason nothing happened.
   --  (Mouse_X_In_Canvas, Mouse_Y_In_Canvas) are the screen coordinates of the
   --  mouse in the canvas.

   procedure Show_Item
     (Canvas             : access Interactive_Canvas_Record'Class;
      Item               : access Canvas_Item_Record'Class;
      Canvas_X, Canvas_Y : Gdouble;
      Report_Adj_Changed : Boolean := True);
   --  Like Show_Item.
   --  (Canvas_X, Canvas_Y) are the position in the canvas where the center of
   --  the item should be put. (0,0) is on the top-left, (1,1) is bottom-right.
   --
   --  Nothing is done if the item is already visible.
   --
   --  If Report_Adj_Changed is true, the "changed" signal might be sent if the
   --  adjustments are changed. However, this might result in flickering.

   procedure Draw_Dashed_Selection
     (Canvas : access Interactive_Canvas_Record'Class;
      Cr     : Cairo_Context);
   --  Draw all the selected items and links with dashed-lines.

   function Zoom_Timeout (Canvas : Interactive_Canvas) return Boolean;
   --  Timeout function used to provide smooth zooming.

   procedure Zoom_Internal
     (Canvas : access Interactive_Canvas_Record'Class; Percent : Gdouble);
   --  Internal function to implement zooming

   function Get_Background_Selection_Rectangle
     (Canvas : access Interactive_Canvas_Record'Class) return Gdk_Rectangle;
   --  Return the coordinates of the rectangle representing the background
   --  selection (when the user clicks in the background and drags the mouse).
   --  Return coordinates are in world coordinates

   procedure Emit_By_Name_Item
     (Object : System.Address;
      Name   : String;
      Param  : access Canvas_Item_Record'Class);
   --  ???

   function Compute_Line_Pos
     (Canvas : access Interactive_Canvas_Record'Class) return Gint_Array;
   --  ???

   procedure Scroll_Canvas_To_Item
     (Canvas             : access Interactive_Canvas_Record'Class;
      Item               : access Canvas_Item_Record'Class;
      Canvas_X, Canvas_Y : Gdouble := 0.5;
      Report_Adj_Changed : Boolean := True);
   --  Scroll the canvas to the item. This function tries to scroll the canvas
   --  as less as possible, typically used when the item is moving out of the
   --  window.

   function Create
     (Canvas : access Interactive_Canvas_Record'Class) return Cairo_Context;

   ------------
   -- Create --
   ------------

   function Create
     (Canvas : access Interactive_Canvas_Record'Class) return Cairo_Context
   is
      Cr             : constant Cairo_Context := Create (Get_Window (Canvas));
      X_Base, Y_Base : Gdouble;
   begin
      To_World_Coordinates
        (Canvas, 0, 0, X_Base, Y_Base);
      Cairo.Translate (Cr, -X_Base * Canvas.Zoom, -Y_Base * Canvas.Zoom);
      Cairo.Scale (Cr, Canvas.Zoom, Canvas.Zoom);
      Set_Source_Color (Cr, Get_Fg (Get_Style (Canvas), State_Normal));
      Set_Line_Width (Cr, 1.0);

      return Cr;
   end Create;

   -----------------------
   -- Emit_By_Name_Item --
   -----------------------

   procedure Emit_By_Name_Item
     (Object : System.Address;
      Name   : String;
      Param  : access Canvas_Item_Record'Class)
   is
      procedure Internal
        (Object : System.Address;
         Name   : String;
         Param  : System.Address);
      pragma Import (C, Internal, "ada_g_signal_emit_by_name_ptr");
   begin
      Internal (Object, Name, Param.all'Address);
   end Emit_By_Name_Item;

   --------------------------
   -- To_World_Coordinates --
   --------------------------

   procedure To_World_Coordinates
     (Canvas  : access Interactive_Canvas_Record'Class;
      X       : Gint;
      Y       : Gint;
      X_World : out Gdouble;
      Y_World : out Gdouble)
   is
   begin
      X_World := Canvas.World_X + Gdouble (X) / Canvas.Zoom;
      Y_World := Canvas.World_Y + Gdouble (Y) / Canvas.Zoom;
   end To_World_Coordinates;

   --------------------------
   -- To_World_Coordinates --
   --------------------------

   procedure To_World_Coordinates
     (Canvas  : access Interactive_Canvas_Record'Class;
      Event   : Gdk_Event;
      X_World : out Gdouble;
      Y_World : out Gdouble)
   is
   begin
      X_World := Canvas.World_X + Get_X (Event) / Canvas.Zoom;
      Y_World := Canvas.World_Y + Get_Y (Event) / Canvas.Zoom;
   end To_World_Coordinates;

   ---------------------------
   -- Get_World_Coordinates --
   ---------------------------

   procedure Get_World_Coordinates
     (Canvas : access Interactive_Canvas_Record'Class;
      X, Y   : out Glib.Gdouble;
      Width  : out Glib.Gdouble;
      Height : out Glib.Gdouble)
   is
   begin
      X := Get_Lower (Canvas.Hadj);
      Y := Get_Lower (Canvas.Vadj);
      Width := Get_Upper (Canvas.Hadj) - X;
      Height := Get_Lower (Canvas.Vadj) - Y;
   end Get_World_Coordinates;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Canvas : out Interactive_Canvas; Auto_Layout : Boolean := True)
   is
   begin
      Canvas := new Interactive_Canvas_Record;
      Gtkada.Canvas.Initialize (Canvas, Auto_Layout);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Canvas      : access Interactive_Canvas_Record'Class;
      Auto_Layout : Boolean := True)
   is
      Signal_Parameters : constant Signal_Parameter_Types :=
        (1 => (1 => Gdk.Event.Get_Type,      2 => GType_None),
         2 => (1 => GType_Pointer,           2 => GType_None),
         3 => (1 => GType_Uint,              2 => GType_None),
         4 => (1 => Gtk.Adjustment.Get_Type, 2 => Gtk.Adjustment.Get_Type),
         5 => (1 => GType_Pointer,           2 => GType_None),
         6 => (1 => GType_Pointer,           2 => GType_None));
      --  the parameters for the above signals.
      --  This must be defined in this function rather than at the
      --  library-level, or the value of Gdk_Event.Get_Type is not yet
      --  initialized.

      Style : Gtk_Style;

   begin
      Gtk.Drawing_Area.Initialize (Canvas);
      Canvas.Offset_X_World := 0;
      Canvas.Offset_Y_World := 0;
      Canvas.World_X := 0.0;
      Canvas.World_Y := 0.0;

      Set_Directed (Canvas.Children, True);
      Canvas.Auto_Layout := Auto_Layout;

      --  The following call is required to initialize the class record,
      --  and the new signals created for this widget.
      --  Note also that we keep Class_Record, so that the memory allocation
      --  is done only once.
      Initialize_Class_Record
        (Canvas, Signals, Class_Record,
         "GtkAdaCanvas", Signal_Parameters);
      Set_Scroll_Adjustments_Signal
        (Class_Record, "set_scroll_adjustments");

      Return_Callback.Connect
        (Canvas, "expose_event",
         Return_Callback.To_Marshaller (Expose'Access));
      Return_Callback.Connect
        (Canvas, "button_press_event",
         Return_Callback.To_Marshaller (Button_Pressed'Access));
      Return_Callback.Connect
        (Canvas, "button_release_event",
         Return_Callback.To_Marshaller (Button_Release'Access));
      Return_Callback.Connect
        (Canvas, "motion_notify_event",
         Return_Callback.To_Marshaller (Button_Motion'Access));
      Return_Callback.Connect
        (Canvas, "key_press_event",
         Return_Callback.To_Marshaller (Key_Press'Access));
      Widget_Callback.Connect
        (Canvas, "size_allocate", Size_Allocate'Access);
      Widget_Callback.Connect
        (Canvas, "set_scroll_adjustments", Set_Scroll_Adjustments'Access);
      Widget_Callback.Connect
        (Canvas, "destroy",
         Widget_Callback.To_Marshaller (Canvas_Destroyed'Access));

      Style := Get_Style (Canvas);
      Set_Bg (Style, State_Normal, Gdk_Color'(Get_White (Style)));
      Set_Style (Canvas, Style);

      Canvas.Annotation_Layout := Create_Pango_Layout (Canvas);

      --  We want to be sure to get all the mouse events, that are required
      --  for the animation.

      Add_Events
        (Canvas,
         Button_Press_Mask
           or Button_Motion_Mask
           or Button_Release_Mask
           or Key_Press_Mask
           or Key_Release_Mask);
      Set_Flags (Canvas, Can_Focus);

      --  Configure with default values
      Configure (Canvas);
      Set_Scroll_Adjustments (Canvas, null, null);
   end Initialize;

   --------------
   -- Get_Vadj --
   --------------

   function Get_Vadj
     (Canvas : access Interactive_Canvas_Record'Class) return Gtk_Adjustment is
   begin
      return Canvas.Vadj;
   end Get_Vadj;

   --------------
   -- Get_Hadj --
   --------------

   function Get_Hadj
     (Canvas : access Interactive_Canvas_Record'Class) return Gtk_Adjustment is
   begin
      return Canvas.Hadj;
   end Get_Hadj;

   ----------------------
   -- Canvas_Destroyed --
   ----------------------

   procedure Canvas_Destroyed (Canvas : access Gtk_Widget_Record'Class) is
      C : constant Interactive_Canvas := Interactive_Canvas (Canvas);
   begin
      if C.Scrolling_Timeout_Id /= 0 then
         Remove (C.Scrolling_Timeout_Id);
      end if;

      Clear (C);

      Unref (C.Annotation_Layout);

      Unref (C.Hadj);
      Unref (C.Vadj);
   end Canvas_Destroyed;

   ----------------------------
   -- Set_Scroll_Adjustments --
   ----------------------------

   procedure Set_Scroll_Adjustments
     (Canvas : access Gtk_Widget_Record'Class;
      Args   : GValues)
   is
      Hadj : constant System.Address := Get_Address (Nth (Args, 1));
      Vadj : constant System.Address := Get_Address (Nth (Args, 2));
      Canv : constant Interactive_Canvas := Interactive_Canvas (Canvas);
      Stub : Gtk_Adjustment_Record;

   begin
      if Canv.Hadj /= null then
         Unref (Canv.Hadj);
      end if;

      if Hadj /= System.Null_Address then
         Canv.Hadj := Gtk_Adjustment (Get_User_Data (Hadj, Stub));
      else
         Gtk_New (Canv.Hadj, 0.0, 0.0, 0.0, 1.0, 1.0);
      end if;
      Ref (Canv.Hadj);
      Ref_Sink (Canv.Hadj);

      if Canv.Vadj /= null then
         Unref (Canv.Vadj);
      end if;

      if Vadj /= System.Null_Address then
         Canv.Vadj := Gtk_Adjustment (Get_User_Data (Vadj, Stub));
      else
         Gtk_New (Canv.Vadj, 0.0, 0.0, 0.0, 1.0, 1.0);
      end if;
      Ref (Canv.Vadj);
      Ref_Sink (Canv.Vadj);

      Scrolled (Canvas);

      Widget_Callback.Object_Connect
        (Canv.Hadj, "value_changed",
         Widget_Callback.To_Marshaller (Scrolled'Access), Canv);
      Widget_Callback.Object_Connect
        (Canv.Vadj, "value_changed",
         Widget_Callback.To_Marshaller (Scrolled'Access), Canv);

      Update_Adjustments (Canv);
   end Set_Scroll_Adjustments;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Canvas            : access Interactive_Canvas_Record;
      Grid_Size         : Guint := Default_Grid_Size;
      Annotation_Font   : Pango.Font.Pango_Font_Description :=
        Pango.Font.From_String (Default_Annotation_Font);
      Arc_Link_Offset   : Gint := Default_Arc_Link_Offset;
      Arrow_Angle       : Gint := Default_Arrow_Angle;
      Arrow_Length      : Gint := Default_Arrow_Length;
      Motion_Threshold  : Gdouble := Default_Motion_Threshold) is
   begin
      Canvas.Grid_Size := Grid_Size;

      if Grid_Size < 2 then
         Canvas.Align_On_Grid := False;
      end if;

      Set_Font_Description (Canvas.Annotation_Layout, Annotation_Font);

      Canvas.Arc_Link_Offset := Arc_Link_Offset;
      Canvas.Arrow_Angle := Gdouble (Arrow_Angle) * Pi / 180.0;
      Canvas.Arrow_Length := Arrow_Length;
      Canvas.Motion_Threshold := Motion_Threshold;
   end Configure;

   -------------------
   -- Size_Allocate --
   -------------------

   procedure Size_Allocate
     (Canv : access Gtk_Widget_Record'Class;
      Args : Gtk_Args)
   is
      Canvas : constant Interactive_Canvas := Interactive_Canvas (Canv);
      pragma Unreferenced (Args);
   begin
      Update_Adjustments (Canvas);

      if Canvas.Show_Item /= null then
         Show_Item (Canvas, Canvas.Show_Item,
                    Canvas.Show_Canvas_X, Canvas.Show_Canvas_Y);
         Canvas.Show_Item := null;
      end if;
   end Size_Allocate;

   -------------------
   -- Align_On_Grid --
   -------------------

   procedure Align_On_Grid
     (Canvas : access Interactive_Canvas_Record;
      Align  : Boolean := True) is
   begin
      Canvas.Align_On_Grid := (Canvas.Grid_Size >= 2) and then Align;
   end Align_On_Grid;

   ----------------------
   -- Get_Bounding_Box --
   ----------------------

   procedure Get_Bounding_Box
     (Canvas                     : access Interactive_Canvas_Record'Class;
      X_Min, X_Max, Y_Min, Y_Max : out Gdouble)
   is
      Current    : Vertex_Iterator := First (Canvas.Children);
      Item       : Canvas_Item;

   begin
      if At_End (Current) then
         X_Min := 0.0;
         X_Max := 0.0;
         Y_Min := 0.0;
         Y_Max := 0.0;

      else
         X_Min := Gdouble'Last;
         X_Max := Gdouble'First;
         Y_Min := Gdouble'Last;
         Y_Max := Gdouble'First;

         while not At_End (Current) loop
            Item := Canvas_Item (Get (Current));
            if Item.Visible and then Item.Coord.X /= Gint'First then
               X_Min := Gdouble'Min
                 (X_Min, Gdouble (Item.Coord.X));
               X_Max := Gdouble'Max
                 (X_Max, Gdouble (Item.Coord.X + Item.Coord.Width));
               Y_Min := Gdouble'Min
                 (Y_Min, Gdouble (Item.Coord.Y));
               Y_Max := Gdouble'Max
                 (Y_Max, Gdouble (Item.Coord.Y + Item.Coord.Height));

               --  If the item is selected, also include its new position in
               --  the computation (while we are moving items)

               if (Canvas.Offset_X_World /= 0
                   or else Canvas.Offset_Y_World /= 0)
                 and then Item.Selected
               then
                  X_Min := Gdouble'Min
                    (X_Min, Gdouble (Item.Coord.X + Canvas.Offset_X_World));
                  X_Max := Gdouble'Max
                    (X_Max, Gdouble (Item.Coord.X + Item.Coord.Width +
                                       Canvas.Offset_X_World));
                  Y_Min := Gdouble'Min
                    (Y_Min, Gdouble (Item.Coord.Y + Canvas.Offset_Y_World));
                  Y_Max := Gdouble'Max
                    (Y_Max, Gdouble (Item.Coord.Y + Item.Coord.Height +
                                       Canvas.Offset_Y_World));
               end if;
            end if;

            Next (Current);
         end loop;
      end if;
   end Get_Bounding_Box;

   ------------------------
   -- Update_Adjustments --
   ------------------------

   procedure Update_Adjustments
     (Canvas       : access Interactive_Canvas_Record'Class;
      Min_Max      : Boolean := True;
      Page_Size    : Boolean := True;
      Clip_Value   : Boolean := True;
      Send_Changed : Boolean := True)
   is
      procedure Update_Axis
        (Adj      : access Gtk_Adjustment_Record'Class;
         Min, Max : Gdouble;
         Size     : Gdouble);
      --  Takes care of one of the axis

      -----------------
      -- Update_Axis --
      -----------------

      procedure Update_Axis
        (Adj      : access Gtk_Adjustment_Record'Class;
         Min, Max : Gdouble;
         Size     : Gdouble)
      is
         Changed_Sent : Boolean := False;
         Vals_Changed : Boolean := False;
         Tmp          : Gdouble;

      begin
         if Min_Max then
            Tmp := Min;
            if Tmp > Get_Value (Adj) then
               Tmp := Get_Value (Adj);
            end if;
            if Get_Lower (Adj) /= Tmp then
               Set_Lower          (Adj, Tmp);
               Vals_Changed := True;
            end if;
            Tmp := Max;
            if Tmp < Get_Value (Adj) + Size then
               Tmp := Get_Value (Adj) + Size - 1.0;
            end if;
            if Get_Upper (Adj) /= Tmp then
               Set_Upper          (Adj, Tmp);
               Vals_Changed := True;
            end if;
         end if;

         if Page_Size then
            if Get_Page_Size (Adj) /= Size then
               Set_Page_Size      (Adj, Size);
               Set_Step_Increment (Adj, Size / 10.0);
               Set_Page_Increment (Adj, Size / 2.0);
               Vals_Changed := True;
            end if;
         end if;

         if Clip_Value then
            if Get_Value (Adj) < Min then
               Set_Value (Adj, Min);
               --  this call emits the changed signal
               Changed_Sent := True;
            end if;

            if Get_Value (Adj) + Size > Max then
               Set_Value (Adj, Max - Size);
               --  this call emits the changed signal
               Changed_Sent := True;
            end if;
         end if;

         if Send_Changed and then Vals_Changed and then not Changed_Sent then
            --  If the changed signal was not sent, then we manually send it
            Changed (Adj);
         end if;
      end Update_Axis;

      X_Max, Y_Max, X_Min, Y_Min : Gdouble;

   begin
      --  If the canvas was properly initialized
      if Realized_Is_Set (Canvas)
        and then Get_Allocation_Width (Canvas) /= 1
      then
         Get_Bounding_Box (Canvas, X_Min, X_Max, Y_Min, Y_Max);
         --  Add some space around this bounding box
         --  ??? Use a constant
         X_Min := X_Min - 20.0;
         Y_Min := Y_Min - 20.0;
         X_Max := X_Max + 20.0;
         Y_Max := Y_Max + 20.0;

         Update_Axis
           (Canvas.Hadj, X_Min, X_Max,
            Gdouble (Get_Allocation_Width (Canvas)) / Canvas.Zoom);
         Update_Axis
           (Canvas.Vadj, Y_Min, Y_Max,
            Gdouble (Get_Allocation_Height (Canvas)) / Canvas.Zoom);
      end if;
   end Update_Adjustments;

   ------------------------------
   -- Default_Layout_Algorithm --
   ------------------------------

   procedure Default_Layout_Algorithm
     (Canvas          : access Interactive_Canvas_Record'Class;
      Graph           : Glib.Graphs.Graph;
      Force           : Boolean;
      Vertical_Layout : Boolean)
   is
      pragma Unreferenced (Force);
      Step       : Gint := Gint (Canvas.Grid_Size);
      Region     : Cairo_Region;
      Items      : Vertex_Iterator;
      Item       : Canvas_Item;
      Links      : Edge_Iterator;
      Src_Item   : Canvas_Item := null;
      X1, X3, Y1, Y3 : Gint;
      Num        : Gint;
      Coord      : aliased Cairo_Rectangle_Int;
      Status     : Cairo_Status;
      pragma Unreferenced (Status);

   begin
      if Step = 0 then
         Step := Gint (Default_Grid_Size);
      end if;

      --  First, check every item that won't be moved

      Region := Create;
      Items := First (Graph);
      while not At_End (Items) loop
         Item := Canvas_Item (Get (Items));
         if Item.Coord.X /= Gint'First
           or else Item.Coord.Y /= Gint'First
         then
            Status := Union_Rectangle (Region, Item.Coord'Access);
         end if;

         Next (Items);
      end loop;

      Items := First (Graph);
      while not At_End (Items) loop
         Item := Canvas_Item (Get (Items));
         if Item.Coord.X = Gint'First or else Item.Coord.Y = Gint'First then
            --  Check if there is any link that has for destination or source
            --  the widget we are adding.

            Links := First (Canvas.Children, Src => Vertex_Access (Item));
            while not At_End (Links) loop
               Src_Item := Canvas_Item (Get_Dest (Get (Links)));
               exit when Src_Item /= Item;
               Src_Item := null;
               Next (Links);
            end loop;

            if Src_Item = null then
               Links := First (Canvas.Children, Dest => Vertex_Access (Item));
               while not At_End (Links) loop
                  Src_Item := Canvas_Item (Get_Src (Get (Links)));
                  exit when Src_Item /= Item;
                  Src_Item := null;
                  Next (Links);
               end loop;
            end if;

            --  The rule is the following when we have a link to an existing
            --  item: We first try to put the new item below the old one, then,
            --  if that place is already occupied, to the bottom-right, then
            --  the bottom-left, then two down, ...

            if Src_Item /= null then
               Num := 0;

               if Vertical_Layout then
                  X1 := Src_Item.Coord.X + Src_Item.Coord.Width + Step;
                  Y3 := Src_Item.Coord.Y;

                  loop
                     case Num mod 3 is
                        when 0 =>
                           Y1 := Y3;
                        when 1 =>
                           Y1 := Y3 - Step - Item.Coord.Height;
                        when 2 =>
                           Y1 := Y3 + Step + Item.Coord.Height;
                        when others =>
                           null;
                     end case;

                     Coord := (X1, Y1, Item.Coord.Width, Item.Coord.Height);
                     exit when
                       Contains_Rectangle
                         (Region, Coord'Access) = Cairo_Region_Overlap_Out;

                     Num := Num + 1;
                     if Num mod 3 = 0 then
                        X1 := X1 + 2 * Step;
                     end if;
                  end loop;

               else
                  X3 := Src_Item.Coord.X;
                  Y1 := Src_Item.Coord.Y + Src_Item.Coord.Height + Step;

                  loop
                     case Num mod 3 is
                        when 0 =>
                           X1 := X3;
                        when 1 =>
                           X1 := X3 - Step - Item.Coord.Width;
                        when 2 =>
                           X1 := X3 + Step + Item.Coord.Width;
                        when others =>
                           null;
                     end case;

                     Coord := (X1, Y1, Item.Coord.Width, Item.Coord.Height);
                     exit when
                       Contains_Rectangle
                         (Region, Coord'Access) = Cairo_Region_Overlap_Out;

                     Num := Num + 1;
                     if Num mod 3 = 0 then
                        Y1 := Y1 + 2 * Step;
                     end if;
                  end loop;
               end if;

            else
               --  Else put the item in the first line, at the first possible
               --  location
               X1 := Gint (Get_Lower (Canvas.Hadj)) + Step;
               Y1 := Gint (Get_Lower (Canvas.Vadj)) + Step;

               loop
                  Coord := (X1, Y1, Item.Coord.Width, Item.Coord.Height);
                  exit when
                    Contains_Rectangle
                      (Region, Coord'Access) = Cairo_Region_Overlap_Out;

                  if Vertical_Layout then
                     Y1 := Y1 + 2 * Step;
                  else
                     X1 := X1 + 2 * Step;
                  end if;
               end loop;
            end if;

            Item.Coord.X := X1;
            Item.Coord.Y := Y1;

            Status := Union_Rectangle (Region, Item.Coord'Access);
         end if;

         Next (Items);
      end loop;

      Destroy (Region);
   end Default_Layout_Algorithm;

   ---------------------
   -- Set_Auto_Layout --
   ---------------------

   procedure Set_Auto_Layout
     (Canvas      : access Interactive_Canvas_Record;
      Auto_Layout : Boolean) is
   begin
      Canvas.Auto_Layout := Auto_Layout;
   end Set_Auto_Layout;

   ----------------------------
   -- Set_Layout_Orientation --
   ----------------------------

   procedure Set_Layout_Orientation
     (Canvas          : access Interactive_Canvas_Record;
      Vertical_Layout : Boolean := False)
   is
   begin
      Canvas.Vertical_Layout := Vertical_Layout;
   end Set_Layout_Orientation;

   ------------
   -- Layout --
   ------------

   procedure Layout
     (Canvas : access Interactive_Canvas_Record;
      Force  : Boolean := False)
   is
      Step         : constant Gint := Gint (Canvas.Grid_Size);
      Items        : Vertex_Iterator;
      Item         : Canvas_Item;
      Min_X, Min_Y : Gint := Gint'Last;
      Max_X, Max_Y : Gint := Gint'First;

   begin
      Canvas.Layout
        (Canvas, Canvas.Children,
         Force           => Force,
         Vertical_Layout => Canvas.Vertical_Layout);

      Items := First (Canvas.Children);

      while not At_End (Items) loop
         Item := Canvas_Item (Get (Items));
         Min_X := Gint'Min (Min_X, Item.Coord.X);
         Min_Y := Gint'Min (Min_Y, Item.Coord.Y);
         Max_X := Gint'Max (Max_X, Item.Coord.X + Item.Coord.Width);
         Max_Y := Gint'Max (Max_Y, Item.Coord.Y + Item.Coord.Height);

         if Force then
            Item.From_Auto_Layout := True;
         end if;

         Next (Items);
      end loop;

      Items := First (Canvas.Children);

      while not At_End (Items) loop
         Item := Canvas_Item (Get (Items));

         --  Normalize the coordinates, so that we stay within Integer'Range.
         --  Since this causes unwanted scrolling when new boxes are added, we
         --  only do it to keep a safe margin when the user moves a box around,
         --  and thus only when absolutly needed.

         if Max_X > Gint'Last - 5000
           or else Max_Y > Gint'Last - 5000
           or else Min_X < Gint'First + 5000
           or else Min_Y < Gint'First + 5000
         then
            if Traces then
               Put_Line ("Layout: Changing all items: Min="
                 & Gint'Image (Min_X) & Gint'Image (Min_Y)
                         & " Max=" & Gint'Image (Max_X) & Gint'Image (Max_Y));
            end if;

            Item.Coord.X := Item.Coord.X - Min_X;
            Item.Coord.Y := Item.Coord.Y - Min_Y;
         end if;

         if Item.From_Auto_Layout then
            if Canvas.Align_On_Grid then
               Item.Coord.X := Item.Coord.X - Item.Coord.X mod Step;
               Item.Coord.Y := Item.Coord.Y - Item.Coord.Y mod Step;
            end if;
         end if;

         Next (Items);
      end loop;

      Update_Adjustments (Canvas);
   end Layout;

   --------------------------
   -- Set_Layout_Algorithm --
   --------------------------

   procedure Set_Layout_Algorithm
     (Canvas    : access Interactive_Canvas_Record;
      Algorithm : Layout_Algorithm) is
   begin
      if Algorithm /= null then
         Canvas.Layout := Algorithm;
      end if;
   end Set_Layout_Algorithm;

   -------------
   -- Move_To --
   -------------

   procedure Move_To
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class;
      X, Y   : Glib.Gint := Glib.Gint'First)
   is
      pragma Unreferenced (Canvas);
   begin
      Item.Coord.X := X;
      Item.Coord.Y := Y;
   end Move_To;

   ---------
   -- Put --
   ---------

   procedure Put
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class;
      X, Y   : Gint := Gint'First) is
   begin
      Add_Vertex (Canvas.Children, Item);
      Item.Canvas := Interactive_Canvas (Canvas);
      Move_To (Canvas, Item, X, Y);

      --  Make sure that the item will be properly moved by the layout
      --  algorithm.
      Item.From_Auto_Layout :=
        X = Gint'First and then Y = Gint'First;

      if Canvas.Auto_Layout
        and then Item.From_Auto_Layout
      then
         Layout (Canvas);
      else
         Update_Adjustments (Canvas);
      end if;
   end Put;

   ---------------
   -- Set_Items --
   ---------------

   procedure Set_Items
     (Canvas : access Interactive_Canvas_Record;
      Items  : Glib.Graphs.Graph) is
   begin
      Destroy (Canvas.Children);
      Canvas.Children := Items;
   end Set_Items;

   -------------------
   -- For_Each_Item --
   -------------------

   procedure For_Each_Item
     (Canvas            : access Interactive_Canvas_Record;
      Execute           : Item_Processor;
      Linked_From_Or_To : Canvas_Item := null)
   is
      Iter : Item_Iterator := Start (Canvas, Linked_From_Or_To);
      It   : Canvas_Item;
   begin
      loop
         It := Get (Iter);
         exit when It = null;

         Next (Iter);
         exit when not Execute (Canvas, It);
      end loop;
   end For_Each_Item;

   -----------
   -- Start --
   -----------

   function Start
     (Canvas            : access Interactive_Canvas_Record;
      Linked_From_Or_To : Canvas_Item := null;
      Selected_Only     : Boolean := False) return Item_Iterator
   is
      Iter : Item_Iterator;
   begin
      if Linked_From_Or_To = null then
         Iter := (Vertex            => First (Canvas.Children),
                  Edge              => Null_Edge_Iterator,
                  Selected_Only     => Selected_Only,
                  Linked_From_Or_To => null);
      else
         Iter := (Vertex => Null_Vertex_Iterator,
                  Edge   => First (Canvas.Children,
                    Vertex_Access (Linked_From_Or_To),
                    Directed => False),
                  Selected_Only     => Selected_Only,
                  Linked_From_Or_To => Linked_From_Or_To);
      end if;

      if Iter.Selected_Only
        and then Get (Iter) /= null
        and then not Get (Iter).Selected
      then
         Next (Iter);
      end if;

      return Iter;
   end Start;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Item_Iterator) is
   begin
      loop
         if Iter.Linked_From_Or_To = null then
            Next (Iter.Vertex);
         else
            Next (Iter.Edge);
         end if;

         exit when not Iter.Selected_Only
           or else Get (Iter) = null
           or else Get (Iter).Selected;
      end loop;
   end Next;

   ----------
   -- Next --
   ----------

   function Next (Iter : Item_Iterator) return Item_Iterator is
      It : Item_Iterator := Iter;
   begin
      Next (It);
      return It;
   end Next;

   --------------------
   -- Is_Linked_From --
   --------------------

   function Is_Linked_From (Iter : Item_Iterator) return Boolean is
   begin
      return Iter.Linked_From_Or_To /= null
        and then not At_End (Iter.Edge)
        and then Canvas_Item (Get_Src (Get (Iter.Edge))) /=
          Iter.Linked_From_Or_To;
   end Is_Linked_From;

   ---------
   -- Get --
   ---------

   function Get (Iter : Item_Iterator) return Canvas_Item is
      Item : Canvas_Item;
   begin
      if Iter.Linked_From_Or_To = null then
         if At_End (Iter.Vertex) then
            return null;
         else
            return Canvas_Item (Get (Iter.Vertex));
         end if;

      else
         if At_End (Iter.Edge) then
            return null;
         end if;

         Item  := Canvas_Item (Get_Src (Get (Iter.Edge)));
         if Item /= Iter.Linked_From_Or_To then
            return Item;
         end if;

         --  If Get_Src was the item, we want to return Dest (which might
         --  actually be the item itself).
         --  Else, if Get_Src wasn't the item, then Get_Dest is the item, and
         --  we do not want to return it.
         return Canvas_Item (Get_Dest (Get (Iter.Edge)));
      end if;
   end Get;

   ---------------
   -- Clip_Line --
   ---------------

   procedure Clip_Line
     (Src    : access Canvas_Item_Record;
      Canvas : access Interactive_Canvas_Record'Class;
      To_X   : Gint;
      To_Y   : Gint;
      X_Pos  : Gfloat;
      Y_Pos  : Gfloat;
      Side   : out Item_Side;
      X_Out  : out Gint;
      Y_Out  : out Gint)
   is
      Rect    : aliased Cairo_Rectangle_Int;
      Src_X   : Gint;
      Src_Y   : Gint;
      Delta_X : Gint;
      Delta_Y : Gint;
      Offset  : Gint;
   begin
      if Src.Selected then
         Rect := (Src.Coord.X + Canvas.Offset_X_World,
                  Src.Coord.Y + Canvas.Offset_Y_World,
                  Src.Coord.Width,
                  Src.Coord.Height);
      else
         Rect := Src.Coord;
      end if;

      Src_X    := Rect.X + Gint (Gfloat (Rect.Width) * X_Pos);
      Src_Y    := Rect.Y + Gint (Gfloat (Rect.Height) * Y_Pos);
      Delta_X  := To_X - Src_X;
      Delta_Y  := To_Y - Src_Y;

      --  Intersection with horizontal sides

      if Delta_Y /= 0 then
         Offset := (Src_X * To_Y - To_X * Src_Y);

         if Delta_Y < 0 then
            Side := North;
            Y_Out := Rect.Y;
         else
            Side := South;
            Y_Out := Rect.Y + Rect.Height;
         end if;

         X_Out := (Delta_X * Y_Out + Offset) / Delta_Y;

         if Rect.X <= X_Out
           and then X_Out <= Rect.X + Rect.Width
         then
            return;
         end if;
      end if;

      --  Intersection with vertical sides

      if Delta_X /= 0 then
         Offset := (To_X * Src_Y - Src_X * To_Y);

         if Delta_X < 0 then
            Side := West;
            X_Out := Rect.X;
         else
            Side := East;
            X_Out := Rect.X + Rect.Width;
         end if;

         Y_Out := (Delta_Y * X_Out + Offset) / Delta_X;

         if Rect.Y <= Y_Out
           and then Y_Out <= Rect.Y + Rect.Height
         then
            return;
         end if;
      end if;

      X_Out := 0;
      Y_Out := 0;
      Side := East;
   end Clip_Line;

   ---------------------
   -- Draw_Arrow_Head --
   ---------------------

   procedure Draw_Arrow_Head
     (Canvas : access Interactive_Canvas_Record'Class;
      Cr     : Cairo_Context;
      X, Y   : Gint;
      Angle  : Gdouble)
   is
      Length : constant Gdouble := Gdouble (Canvas.Arrow_Length);

   begin
      Move_To (Cr, Gdouble (X), Gdouble (Y));
      Line_To
        (Cr,
         Gdouble (X) + Length * Cos (Angle + Canvas.Arrow_Angle),
         Gdouble (Y) + Length * Sin (Angle + Canvas.Arrow_Angle));
      Line_To
        (Cr,
         Gdouble (X) + Length * Cos (Angle - Canvas.Arrow_Angle),
         Gdouble (Y) + Length * Sin (Angle - Canvas.Arrow_Angle));
      Close_Path (Cr);
      Cairo.Fill (Cr);
   end Draw_Arrow_Head;

   ---------------------
   -- Draw_Annotation --
   ---------------------

   procedure Draw_Annotation
     (Canvas : access Interactive_Canvas_Record'Class;
      Cr     : Cairo_Context;
      X, Y   : Gint;
      Link   : access Canvas_Link_Record'Class)
   is
      W, H : Gint;

   begin
      if Link.Descr /= null
        and then Link.Descr.all /= ""
        and then Canvas.Annotation_Layout /= null
      then
         Set_Text (Canvas.Annotation_Layout, Link.Descr.all);
         Get_Pixel_Size (Canvas.Annotation_Layout, W, H);

         Cairo.Save (Cr);
         Gdk.Cairo.Set_Source_Color
           (Cr, Get_Bg (Get_Style (Canvas), State_Normal));
         Cairo.Set_Line_Width (Cr, 1.0);
         Cairo.Rectangle
           (Cr,
            Gdouble (X) - 0.5,
            Gdouble (Y) - 0.5,
            Gdouble (W) + 1.0,
            Gdouble (H) + 1.0);
         Cairo.Fill (Cr);
         Cairo.Restore (Cr);

         Cairo.Move_To (Cr, Gdouble (X), Gdouble (Y));
         Pango.Cairo.Show_Layout  (Cr, Canvas.Annotation_Layout);

      end if;
   end Draw_Annotation;

   ----------------------
   -- Compute_Line_Pos --
   ----------------------

   function Compute_Line_Pos
     (Canvas : access Interactive_Canvas_Record'Class) return Gint_Array
   is
      type Graph_Range is record
         From, To : Gint;
      end record;

      type Range_Array is array (Positive range <>) of Graph_Range;
      type Range_Array_Access is access all Range_Array;

      procedure Free is new Unchecked_Deallocation
        (Range_Array, Range_Array_Access);

      Free_Ranges : Range_Array_Access := new Range_Array (1 .. 1000);
      Tmp         : Range_Array_Access;
      Last_Range  : Positive := Free_Ranges'First;
      Iter        : Vertex_Iterator := First (Canvas.Children);
      E           : Canvas_Item;
      Right       : Gint;
   begin
      Free_Ranges (Free_Ranges'First) := (From => Gint'First, To => Gint'Last);

      while not At_End (Iter) loop
         E := Canvas_Item (Get (Iter));
         Right := E.Coord.X + E.Coord.Width;

         for R in Free_Ranges'First .. Last_Range loop
            if Free_Ranges (R).From <= E.Coord.X
              and then Free_Ranges (R).To >= E.Coord.X
              and then Free_Ranges (R).To <= Right
            then
               Free_Ranges (R) :=
                 (From => Free_Ranges (R).From, To => E.Coord.X - 1);

            elsif Free_Ranges (R).From <= E.Coord.X
              and then Free_Ranges (R).To >= Right
            then
               if Last_Range >= Free_Ranges'Last then
                  Tmp := new Range_Array (1 .. Free_Ranges'Last * 2);
                  Tmp (1 .. Free_Ranges'Last) := Free_Ranges.all;
                  Free (Free_Ranges);
                  Free_Ranges := Tmp;
               end if;

               Free_Ranges (R + 1 .. Last_Range + 1) :=
                 Free_Ranges (R .. Last_Range);
               Free_Ranges (R + 1) :=
                 (From => Right + 1, To => Free_Ranges (R).To);
               Free_Ranges (R) :=
                 (From => Free_Ranges (R).From, To => E.Coord.X - 1);
               Last_Range := Last_Range + 1;

            elsif Free_Ranges (R).From >= E.Coord.X
              and then Free_Ranges (R).From <= Right
              and then Free_Ranges (R).To >= Right
            then
               Free_Ranges (R) :=
                 (From => Right + 1, To => Free_Ranges (R).To);
            end if;

            exit when Free_Ranges (R).From > Right;
         end loop;

         Next (Iter);
      end loop;

      declare
         Result : Gint_Array (1 .. Last_Range);
      begin
         for R in Result'Range loop
            --  ??? Should handle vertical layout and horizontal layout
            Result (R) :=
              (Free_Ranges (R).From + Free_Ranges (R).To) / 2;
         end loop;

         Free (Free_Ranges);
         return Result;
      end;
   end Compute_Line_Pos;

   ---------------------------
   -- Scroll_Canvas_To_Item --
   ----------------------------

   procedure Scroll_Canvas_To_Item
     (Canvas             : access Interactive_Canvas_Record'Class;
      Item               : access Canvas_Item_Record'Class;
      Canvas_X, Canvas_Y : Gdouble := 0.5;
      Report_Adj_Changed : Boolean := True)
   is
      X1, Y1 : Gint;
   begin
      --  If no size was allocated yet, memorize the item for later (see
      --  the callback for size_allocate)

      if Get_Allocation_Width (Canvas) = 1
        or else Get_Allocation_Height (Canvas) = 1
      then
         Canvas.Show_Item     := Canvas_Item (Item);
         Canvas.Show_Canvas_X := Canvas_X;
         Canvas.Show_Canvas_Y := Canvas_Y;
      else
         X1 := Item.Coord.X + Canvas.Offset_X_World;
         Y1 := Item.Coord.Y + Canvas.Offset_Y_World;
         Scroll_Canvas_To_Area
           (Canvas,
            Gdouble (X1),
            Gdouble (Y1),
            Gdouble (X1 + Item.Coord.Width),
            Gdouble (Y1 + Item.Coord.Height),
            Canvas_X, Canvas_Y, Report_Adj_Changed);
      end if;
   end Scroll_Canvas_To_Item;

   ---------------------------
   -- Scroll_Canvas_To_Area --
   ---------------------------

   procedure Scroll_Canvas_To_Area
     (Canvas             : access Interactive_Canvas_Record'Class;
      X1, Y1, X2, Y2     : Gdouble;
      Canvas_X, Canvas_Y : Gdouble := 0.5;
      Ignore_If_Visible  : Boolean := True;
      Report_Adj_Changed : Boolean := True)
   is
      procedure Center_On_Coordinate
        (Adj        : access Gtk_Adjustment_Record'Class;
         Min, Max   : Gdouble;
         Canvas_Pos : Gdouble);
      --  Takes care of one of the axis

      --------------------------
      -- Center_On_Coordinate --
      --------------------------

      procedure Center_On_Coordinate
        (Adj        : access Gtk_Adjustment_Record'Class;
         Min, Max   : Gdouble;
         Canvas_Pos : Gdouble)
      is
         Adj_Changed : Boolean := False;
         Val         : constant Gdouble := Get_Value (Adj);
         Size        : constant Gdouble := Get_Page_Size (Adj);
         N_Val       : Gdouble;

      begin
         if Ignore_If_Visible
           and then Min >= Val and then Max <= Val + Size
         then
            return;
         end if;

         --  Calculate the new scroll value

         if Canvas_Pos > 1.0 then
            --  Minimal scrolling is needed
            if Min < Val then
               N_Val := Min;
            elsif Max > Val + Size then
               N_Val := Max - Size;
            end if;

         else
            --  Align the center of the region with the given canvas location
            N_Val := (Min + Max) / 2.0 - Size * Canvas_Pos;
         end if;

         --  Do we need to extend the canvas to show the region ?

         if Max > Get_Upper (Adj) then
            Set_Upper (Adj, Max);
            Adj_Changed := True;
         end if;

         if N_Val + Size > Get_Upper (Adj) then
            Set_Upper (Adj, N_Val + Size - 1.0);
            Adj_Changed := True;
         end if;

         if Min < Get_Lower (Adj) then
            Set_Lower (Adj, Min);
            Adj_Changed := True;
         end if;

         if N_Val < Get_Lower (Adj) then
            Set_Lower (Adj, N_Val);
            Adj_Changed := True;
         end if;

         if Report_Adj_Changed and then Adj_Changed then
            Changed (Adj);
         end if;

         --  Now scroll appropriately
         Set_Value (Adj, N_Val);
      end Center_On_Coordinate;

   begin
      Center_On_Coordinate (Canvas.Hadj, X1, X2, Canvas_X);
      Center_On_Coordinate (Canvas.Vadj, Y1, Y2, Canvas_Y);
   end Scroll_Canvas_To_Area;

   --------------------------
   -- Draw_Orthogonal_Link --
   --------------------------

   procedure Draw_Orthogonal_Link
     (Canvas          : access Interactive_Canvas_Record'Class;
      Cr              : Cairo_Context;
      Link            : access Canvas_Link_Record'Class;
      Show_Annotation : Boolean)
   is
      X1, Y1, Xp1, Yp1, X2, Y2, Xp2, Yp2, X3, Y3 : Gint;
      Xc1, Xc2, Yc1, Yc2 : Gint;
      Xarr_End, Yarr_End, Xarr_Start, Yarr_Start : Gint;
      Angle_Arr_End, Angle_Arr_Start : Gdouble;
      Src      : constant Canvas_Item := Canvas_Item (Get_Src (Link));
      Dest     : constant Canvas_Item := Canvas_Item (Get_Dest (Link));
      Line_Pos : constant Gint_Array := Compute_Line_Pos (Canvas);

   begin
      if Src.Selected then
         X1 := Src.Coord.X + Canvas.Offset_X_World;
         Y1 := Src.Coord.Y + Canvas.Offset_Y_World;
      else
         X1 := Src.Coord.X;
         Y1 := Src.Coord.Y;
      end if;

      Xp1 := X1 + Src.Coord.Width;
      Yp1 := Y1 + Src.Coord.Height;

      if Dest.Selected then
         X2 := Dest.Coord.X + Canvas.Offset_X_World;
         Y2 := Dest.Coord.Y + Canvas.Offset_Y_World;
      else
         X2 := Dest.Coord.X;
         Y2 := Dest.Coord.Y;
      end if;

      Xp2 := X2 + Dest.Coord.Width;
      Yp2 := Y2 + Dest.Coord.Height;

      Xc1 := (X1 + Xp1) / 2;

      if Canvas.Grid_Size > 0 then
         Xc1 := Xc1 - Xc1 mod Gint (Canvas.Grid_Size);
      end if;

      Xc2 := (X2 + Xp2) / 2;
      if Canvas.Grid_Size > 0 then
         Xc2 := Xc2 - Xc2 mod Gint (Canvas.Grid_Size);
      end if;

      Yc1 := (Y1 + Yp1) / 2;
      Yc2 := (Y2 + Yp2) / 2;

      --  The preferred case will be
      --     A ---
      --         |____ B
      --  The separation line should be at equal distance of the center of A
      --  and the center of B, so that multiple items lined up in a column
      --  above B all have the vertical line at the same location.
      --
      --  If the vertical line can be drawn at exact distance of the centers,
      --  then we try and display the vertical line at equal distance of the
      --  adjacent edges of A and B

      X3 := Gint'First;

      for L in Line_Pos'Range loop
         if Line_Pos (L) >= Xp1
           and then Line_Pos (L) <= X2
         then
            X3 := Line_Pos (L);
            exit;

         elsif Line_Pos (L) >= Xp2
           and then Line_Pos (L) <= X1
         then
            X3 := Line_Pos (L);
            exit;
         end if;
      end loop;

      --  X3 := (X1 + Xp1 + X2 + Xp2) / 4;
      --  X3 := X3 - X3 mod Gint (Canvas.Grid_Size);

      --  if ((X1 <= X3 and then X3 <= Xp1)
      --      or else (X2 <= X3 and then X3 <= Xp2))
      --    and then (Xp1 <= X2 or else Xp2 <= X1)
      --  then
      --     X3 := (Xp1 + X2) / 2;
      --     X3 := X3 - X3 mod Gint (Canvas.Grid_Size);
      --  end if;

      if X3 /= Gint'First then
      --  if (X3 >= Xp1 and then X3 <= X2)
      --    or else (X3 <= X1 and then X3 >= Xp2)
      --  then

         Yarr_Start := Yc1;
         Yarr_End := Yc2;

         if X3 >= Xp1 then
            Cairo.Move_To (Cr, Gdouble (Xp1), Gdouble (Yc1) + 0.5);
            Line_To (Cr, Gdouble (X3) + 0.5, Gdouble (Yc1) + 0.5);
            Line_To (Cr, Gdouble (X3) + 0.5, Gdouble (Yc2) + 0.5);
            Line_To (Cr, Gdouble (X2), Gdouble (Yc2) + 0.5);
            Cairo.Stroke (Cr);

            Xarr_Start := Xp1;
            Xarr_End := X2;
            Angle_Arr_Start := 0.0;
            Angle_Arr_End := -Ada.Numerics.Pi;
         else
            Move_To (Cr, Gdouble (X1), Gdouble (Yc1) + 0.5);
            Line_To (Cr, Gdouble (X3) + 0.5, Gdouble (Yc1) + 0.5);
            Line_To (Cr, Gdouble (X3) + 0.5, Gdouble (Yc2) + 0.5);
            Line_To (Cr, Gdouble (Xp2), Gdouble (Yc2) + 0.5);
            Cairo.Stroke (Cr);

            Xarr_Start := X1;
            Xarr_End := Xp2;
            Angle_Arr_Start := -Ada.Numerics.Pi;
            Angle_Arr_End := 0.0;
         end if;

      --  Third case is when we didn't have enough space to draw the
      --  intermediate line. In that case, the layout is similar to
      --      A ----
      --           |
      --           B
      --  with the vertical line drawn at the same location as in the first
      --  algorithm.

      --  elsif X3 >= Xp1 or else X3 <= X1 then
      --     if X3 >= Xp1 then
      --        Draw_Line (Window, GC, Xp1, Yc1, X3, Yc1);
      --        Xarr_Start := Xp1;
      --        Angle_Arr_Start := -Ada.Numerics.Pi;
      --     else
      --        Draw_Line (Window, GC, X1, Yc1, X3, Yc1);
      --        Xarr_Start := X1;
      --        Angle_Arr_Start := 0.0;
      --     end if;

      --     Yarr_Start := Yc1;
      --     Xarr_End := X3;

      --     if Y2 < Yc1 then
      --        Draw_Line (Window, GC, X3, Yc1, X3, Yp2);
      --        Yarr_End := Yp2;
      --        Angle_Arr_End := Ada.Numerics.Pi / 2.0;
      --     else
      --        Draw_Line (Window, GC, X3, Yc1, X3, Y2);
      --        Yarr_End := Y2;
      --        Angle_Arr_End := -Ada.Numerics.Pi / 2.0;
      --     end if;

      --  Second case is when one of the item is below the other one. In that
      --  case, the layout should look like
      --       AAA
      --       |_
      --         |
      --        BB
      --  ie the link connects the top side of one item and the bottom side of
      --  the other item.

      else
      --  elsif (X1 <= X2 and then X2 <= Xp1)
      --    or else (X2 <= X1 and then X1 <= Xp2)
      --  then
         Y3 := (Y1 + Yp1 + Y2 + Yp2) / 4;
         if Canvas.Grid_Size > 0 then
            Y3 := Y3 - Y3 mod Gint (Canvas.Grid_Size);
         end if;
         Xarr_Start := Xc1;
         Xarr_End := Xc2;
         X3 := (Xc1 + Xc2) / 2;

         if Y2 > Y3 then
            Move_To (Cr, Gdouble (Xc1), Gdouble (Yp1));
            Line_To (Cr, Gdouble (Xc1), Gdouble (Y3));
            Line_To (Cr, Gdouble (Xc2), Gdouble (Y3));
            Line_To (Cr, Gdouble (Xc2), Gdouble (Y2));
            Cairo.Stroke (Cr);

            Yarr_Start := Yp1;
            Yarr_End := Y2;
            Angle_Arr_End := -Ada.Numerics.Pi / 2.0;
            Angle_Arr_Start := Ada.Numerics.Pi / 2.0;

         else
            Move_To (Cr, Gdouble (Xc1), Gdouble (Y1));
            Line_To (Cr, Gdouble (Xc1), Gdouble (Y3));
            Line_To (Cr, Gdouble (Xc2), Gdouble (Y3));
            Line_To (Cr, Gdouble (Xc2), Gdouble (Yp2));
            Cairo.Stroke (Cr);

            Yarr_Start := Y1;
            Yarr_End := Yp2;
            Angle_Arr_End := Ada.Numerics.Pi / 2.0;
            Angle_Arr_Start := -Ada.Numerics.Pi / 2.0;
         end if;
      end if;

      if Link.Arrow = End_Arrow or else Link.Arrow = Both_Arrow then
         Draw_Arrow_Head (Canvas, Cr, Xarr_End, Yarr_End, Angle_Arr_End);
      end if;

      if Link.Arrow = Start_Arrow or else Link.Arrow = Both_Arrow then
         Draw_Arrow_Head (Canvas, Cr, Xarr_Start, Yarr_Start, Angle_Arr_Start);
      end if;

      --  Draw the text if any

      if Link.Descr /= null and then Show_Annotation then
         Draw_Annotation
           (Canvas, Cr, X3, (Y1 + Y2) / 2, Link);
      end if;
   end Draw_Orthogonal_Link;

   ------------------------
   -- Draw_Straight_Line --
   ------------------------

   procedure Draw_Straight_Line
     (Link      : access Canvas_Link_Record;
      Cr        : Cairo_Context;
      Src_Side  : Item_Side;
      X1, Y1    : Glib.Gdouble;
      Dest_Side : Item_Side;
      X2, Y2    : Glib.Gdouble)
   is
      pragma Unreferenced (Link, Src_Side, Dest_Side);
   begin
      Cairo.Move_To (Cr, X1, Y1);
      Cairo.Line_To (Cr, X2, Y2);
      Cairo.Stroke (Cr);
   end Draw_Straight_Line;

   ------------------------
   -- Draw_Straight_Link --
   ------------------------

   procedure Draw_Straight_Link
     (Canvas          : access Interactive_Canvas_Record'Class;
      Cr              : Cairo_Context;
      Link            : access Canvas_Link_Record'Class;
      Show_Annotation : Boolean)
   is
      X1, Y1, X2, Y2, Xs, Ys, Xd, Yd : Gint;
      Src   : constant Canvas_Item := Canvas_Item (Get_Src (Link));
      Dest  : constant Canvas_Item := Canvas_Item (Get_Dest (Link));
      Src_Side, Dest_Side : Item_Side;

   begin
      if Src.Selected then
         Xs := Src.Coord.X + Canvas.Offset_X_World;
         Ys := Src.Coord.Y + Canvas.Offset_Y_World;
      else
         Xs := Src.Coord.X;
         Ys := Src.Coord.Y;
      end if;

      if Dest.Selected then
         Xd := Dest.Coord.X + Canvas.Offset_X_World;
         Yd := Dest.Coord.Y + Canvas.Offset_Y_World;
      else
         Xd := Dest.Coord.X;
         Yd := Dest.Coord.Y;
      end if;

      Clip_Line
        (Src, Canvas,
         Xd + Gint (Gfloat (Dest.Coord.Width) * Link.Dest_X_Pos),
         Yd + Gint (Gfloat (Dest.Coord.Height) * Link.Dest_Y_Pos),
         X_Pos => Link.Src_X_Pos, Y_Pos => Link.Src_Y_Pos,
         Side => Src_Side, X_Out => X1, Y_Out => Y1);
      Clip_Line
        (Dest, Canvas,
         Xs + Gint (Gfloat (Src.Coord.Width) * Link.Src_X_Pos),
         Ys + Gint (Gfloat (Src.Coord.Height) * Link.Src_Y_Pos),
         X_Pos => Link.Dest_X_Pos, Y_Pos => Link.Dest_Y_Pos,
         Side => Dest_Side, X_Out => X2, Y_Out => Y2);

      Draw_Straight_Line
        (Link, Cr, Src_Side, Gdouble (X1), Gdouble (Y1),
         Dest_Side, Gdouble (X2), Gdouble (Y2));

      --  Draw the end arrow head

      if Link.Arrow = End_Arrow or else Link.Arrow = Both_Arrow then
         if X1 /= X2 then
            Draw_Arrow_Head
              (Canvas, Cr, X2, Y2,
               Arctan (Gdouble (Y1 - Y2), Gdouble (X1 - X2)));
         elsif Y1 > Y2 then
            Draw_Arrow_Head
              (Canvas, Cr, X2, Y2, Pi / 2.0);
         else
            Draw_Arrow_Head
              (Canvas, Cr, X2, Y2, -Pi / 2.0);
         end if;
      end if;

      --  Draw the start arrow head

      if Link.Arrow = Start_Arrow or else Link.Arrow = Both_Arrow then
         if X1 /= X2 then
            Draw_Arrow_Head
              (Canvas, Cr, X1, Y1,
               Arctan (Gdouble (Y2 - Y1), Gdouble (X2 - X1)));
         elsif Y1 > Y2 then
            Draw_Arrow_Head (Canvas, Cr, X1, Y1, -Pi / 2.0);
         else
            Draw_Arrow_Head (Canvas, Cr, X1, Y1, Pi / 2.0);
         end if;
      end if;

      --  Draw the text if any

      if Link.Descr /= null and then Show_Annotation then
         Draw_Annotation (Canvas, Cr, (X1 + X2) / 2, (Y1 + Y2) / 2, Link);
      end if;
   end Draw_Straight_Link;

   --------------------
   -- Draw_Self_Link --
   --------------------

   procedure Draw_Self_Link
     (Canvas          : access Interactive_Canvas_Record'Class;
      Cr              : Cairo_Context;
      Link            : access Canvas_Link_Record'Class;
      Offset          : Gint;
      Show_Annotation : Boolean)
   is
      Right_Angle : constant Gdouble := Pi / 2.0;
      Src         : constant Canvas_Item := Canvas_Item (Get_Src (Link));
      X1, Y1, X3, Y3, Xc, Yc, Radius : Gint;

   begin
      pragma Assert (Src = Canvas_Item (Get_Dest (Link)));

      Xc := Src.Coord.X + Src.Coord.Width;
      Yc := Src.Coord.Y;

      if Src.Selected then
         Xc := Xc + Canvas.Offset_X_World;
         Yc := Yc + Canvas.Offset_Y_World;
      end if;

      Radius := Canvas.Arc_Link_Offset / 2 * Offset;

      --  Location of the arrow and the annotation
      X3 := Xc - Radius;
      Y3 := Yc;
      X1 := Xc;
      Y1 := Yc + Radius;

      Cairo.Move_To (Cr, Gdouble (X3), Gdouble (Y3));
      Cairo.Arc
        (Cr,
         Gdouble (Xc), Gdouble (Yc),
         Gdouble (Radius), Pi, Pi * 0.5);
      Cairo.Stroke (Cr);

      --  Draw the arrows

      if Link.Arrow /= No_Arrow then
         Draw_Arrow_Head (Canvas, Cr, X3, Y3, -Right_Angle);
      end if;

      if Link.Arrow = Both_Arrow then
         Draw_Arrow_Head (Canvas, Cr, X1, Y1, 0.0);
      end if;

      --  Draw the annotations
      if Link.Descr /= null and then Show_Annotation then
         Draw_Annotation
           (Canvas, Cr, Xc + Radius / 2, Yc + Radius / 2, Link);
      end if;
   end Draw_Self_Link;

   -------------------
   -- Draw_Arc_Link --
   -------------------

   procedure Draw_Arc_Link
     (Canvas          : access Interactive_Canvas_Record'Class;
      Cr              : Cairo_Context;
      Link            : access Canvas_Link_Record'Class;
      Offset          : Gint;
      Show_Annotation : Boolean)
   is
      Angle       : Gdouble;
      X1, Y1, X2, Y2, X3, Y3 : Gint;
      Right_Angle : constant Gdouble := Pi / 2.0;
      Arc_Offset  : constant Gdouble := Gdouble (Canvas.Arc_Link_Offset);
      Src         : constant Canvas_Item := Canvas_Item (Get_Src (Link));
      Dest        : constant Canvas_Item := Canvas_Item (Get_Dest (Link));
      Src_Side, Dest_Side : Item_Side;

   begin
      if Src.Selected then
         X1 := Src.Coord.X + Canvas.Offset_X_World;
         Y1 := Src.Coord.Y + Canvas.Offset_Y_World;
      else
         X1 := Src.Coord.X;
         Y1 := Src.Coord.Y;
      end if;

      if Dest.Selected then
         X3 := Dest.Coord.X + Canvas.Offset_X_World;
         Y3 := Dest.Coord.Y + Canvas.Offset_Y_World;
      else
         X3 := Dest.Coord.X;
         Y3 := Dest.Coord.Y;
      end if;

      --  We will first compute the extra intermediate point between the
      --  center of the two items. Once we have this intermediate point, we
      --  will be able to use the intersection point between the two items
      --  and the two lines from the centers to the middle point. This extra
      --  point is used as a control point for the Bezier curve.

      X1 := X1 + Gint (Gfloat (Src.Coord.Width) * Link.Src_X_Pos);
      Y1 := Y1 + Gint (Gfloat (Src.Coord.Height) * Link.Src_Y_Pos);
      X3 := X3 + Gint (Gfloat (Dest.Coord.Width) * Link.Dest_X_Pos);
      Y3 := Y3 + Gint (Gfloat (Dest.Coord.Height) * Link.Dest_Y_Pos);

      --  Compute the middle point for the arc, and create a dummy item for it
      --  that the user can move.

      if X1 /= X3 then
         Angle := Arctan (Gdouble (Y3 - Y1), Gdouble (X3 - X1));
      elsif Y3 > Y1 then
         Angle := Right_Angle;
      else
         Angle := -Right_Angle;
      end if;

      if Offset < 0 then
         Angle := Angle - Right_Angle;
      else
         Angle := Angle + Right_Angle;
      end if;

      X2 := (X1 + X3) / 2 + abs (Offset) * Gint (Arc_Offset * Cos (Angle));
      Y2 := (Y1 + Y3) / 2 + abs (Offset) * Gint (Arc_Offset * Sin (Angle));

      --  Clip to the border of the boxes

      Clip_Line
        (Src, Canvas,
         X2, Y2, Link.Src_X_Pos, Link.Src_Y_Pos, Src_Side, X1, Y1);
      Clip_Line
        (Dest, Canvas, X2, Y2, Link.Dest_X_Pos, Link.Dest_Y_Pos,
         Dest_Side, X3, Y3);

      Cairo.Move_To (Cr, Gdouble (X1), Gdouble (Y1));
      Cairo.Curve_To
        (Cr, Gdouble (X1), Gdouble (Y1),
         Gdouble (X2), Gdouble (Y2),
         Gdouble (X3), Gdouble (Y3));
      Cairo.Stroke (Cr);

      --  Draw the arrows

      if Link.Arrow = End_Arrow or else Link.Arrow = Both_Arrow then
         if X3 /= X2 then
            Angle := Arctan (Gdouble (Y2 - Y3), Gdouble (X2 - X3));
         elsif Y3 > Y2 then
            Angle := Right_Angle;
         else
            Angle := -Right_Angle;
         end if;

         Draw_Arrow_Head (Canvas, Cr, X3, Y3, Angle);
      end if;

      if Link.Arrow = Start_Arrow or else Link.Arrow = Both_Arrow then
         if X1 /= X2 then
            Angle := Arctan (Gdouble (Y2 - Y1), Gdouble (X2 - X1));
         elsif Y2 > Y1 then
            Angle := Right_Angle;
         else
            Angle := -Right_Angle;
         end if;

         Draw_Arrow_Head (Canvas,  Cr, X1, Y1, Angle);
      end if;

      --  Draw the annotations, if any, in the middle of the link
      if Link.Descr /= null and then Show_Annotation then
         X2 := Gint (0.25 * Float (X1) + 0.5 * Float (X2) + 0.25 * Float (X3));
         Y2 := Gint (0.25 * Float (Y1) + 0.5 * Float (Y2) + 0.25 * Float (Y3));
         Draw_Annotation (Canvas, Cr, X2, Y2, Link);
      end if;
   end Draw_Arc_Link;

   ---------------
   -- Draw_Link --
   ---------------

   procedure Draw_Link
     (Canvas          : access Interactive_Canvas_Record'Class;
      Link            : access Canvas_Link_Record;
      Cr              : Cairo_Context;
      Edge_Number     : Gint;
      Show_Annotation : Boolean := True)
   is
   begin
      --  Self-referencing links
      if Get_Src (Link) = Get_Dest (Link) then
         Draw_Self_Link
           (Canvas, Cr, Link, Edge_Number, Show_Annotation);

      elsif Edge_Number = 1 then
         --  The first link in the list is always straight
         if Canvas.Orthogonal_Links then
            Draw_Orthogonal_Link (Canvas, Cr, Link, Show_Annotation);
         else
            Draw_Straight_Link (Canvas, Cr, Link, Show_Annotation);
         end if;

      elsif Edge_Number mod 2 = 1 then
         Draw_Arc_Link
           (Canvas, Cr, Link, Edge_Number / 2, Show_Annotation);

      else
         Draw_Arc_Link
           (Canvas, Cr, Link, -(Edge_Number / 2), Show_Annotation);

      end if;
   end Draw_Link;

   ------------------
   -- Update_Links --
   ------------------

   procedure Update_Links
     (Canvas         : access Interactive_Canvas_Record;
      Cr             : Cairo_Context;
      Invert_Mode    : Boolean;
      From_Selection : Boolean)
   is
      Current : Edge_Iterator := First (Canvas.Children);
      Count   : Natural := 0;
      L       : Canvas_Link;

   begin
      while not At_End (Current) loop
         L := Canvas_Link (Get (Current));
         if Is_Visible (Canvas_Item (Get_Src (L)))
           and then Is_Visible (Canvas_Item (Get_Dest (L)))
         then
            if not From_Selection
              or else Canvas_Item (Get_Src (L)).Selected
              or else Canvas_Item (Get_Dest (L)).Selected
            then
               Set_Line_Width (Cr, 1.0);
               Draw_Link
                 (Canvas, L, Cr,
                  Gint (Repeat_Count (Current)),
                  Show_Annotation => not Invert_Mode);
            end if;

            --  To save time, we limit the number of links that are drawn
            --  while moving items.
            Count := Count + 1;
            exit when From_Selection
              and then Count > Links_Threshold_While_Moving;
         end if;

         Next (Current);
      end loop;
   end Update_Links;

   ---------------
   -- Draw_Grid --
   ---------------

   procedure Draw_Grid
     (Canvas        : access Interactive_Canvas_Record;
      Cr            : Cairo_Context)
   is
      Grid    : Gint := Gint (Canvas.Grid_Size);
      Ptrn    : Cairo_Pattern;
      Surface : Cairo_Surface;
      Tmp_Cr  : Cairo_Context;

   begin
      if Grid = 0 then
         return;
      end if;

      while Gdouble (Grid) * Canvas.Zoom < 5.0 loop
         Grid := Grid * 2;
      end loop;

      --  First create a surface that will contain the pattern to duplicate
      Surface := Cairo.Surface.Create_Similar
        (Cairo.Get_Group_Target (Cr),
         Cairo_Content_Color_Alpha,
         Grid, Grid);

      --  We create a context from the surface
      Tmp_Cr := Cairo.Create (Surface);

      --  Copy the source color
      Set_Source (Tmp_Cr, Cairo.Get_Source (Cr));
      Cairo.Set_Line_Width (Tmp_Cr, 1.0);
      Cairo.Set_Line_Cap (Tmp_Cr, Cairo_Line_Cap_Round);

      --  Let's draw a single point on the surface
      Move_To (Tmp_Cr, 0.5, 0.5);
      Line_To (Tmp_Cr, 0.5, 0.5);
      Stroke (Tmp_Cr);
      Destroy (Tmp_Cr);

      --  Now that the surface is drawn, let's create a pattern from it
      Ptrn := Cairo.Pattern.Create_For_Surface (Surface);
      Cairo.Pattern.Set_Extend (Ptrn, Cairo_Extend_Repeat);

      --  And paint the background
      Cairo.Save (Cr);
      Cairo.Set_Source (Cr, Ptrn);
      Cairo.Paint_With_Alpha (Cr, 0.5);
      Cairo.Restore (Cr);

      Destroy (Ptrn);
      Destroy (Surface);
   end Draw_Grid;

   ---------------------
   -- Draw_Background --
   ---------------------

   procedure Draw_Background
     (Canvas : access Interactive_Canvas_Record;
      Cr     : Cairo_Context)
   is
   begin
      Cairo.Save (Cr);
      Set_Source_Color (Cr, Get_Bg (Get_Style (Canvas), State_Normal));
      Paint (Cr);
      Cairo.Restore (Cr);
   end Draw_Background;

   ----------------------
   -- Get_Bounding_Box --
   ----------------------

   procedure Get_Bounding_Box
     (Canvas : access Interactive_Canvas_Record'Class;
      Width  : out Gdouble;
      Height : out Gdouble)
   is
      X_Min, X_Max, Y_Min, Y_Max : Gdouble;
   begin
      Get_Bounding_Box (Canvas, X_Min, X_Max, Y_Min, Y_Max);
      Width := X_Max - X_Min + 40.0;
      Height := Y_Max - Y_Min + 40.0;
   end Get_Bounding_Box;

   --------------
   -- Draw_All --
   --------------

   procedure Draw_All
     (Canvas : access Interactive_Canvas_Record'Class;
      Cr     : Cairo_Context)
   is
      Area : Cairo_Rectangle_Int;
      X_Min, X_Max, Y_Min, Y_Max : Gdouble;
   begin
      Get_Bounding_Box (Canvas, X_Min, X_Max, Y_Min, Y_Max);
      Area :=
        (X      => Gint (X_Min - 20.0),
         Y      => Gint (Y_Min - 20.0),
         Width  => Gint (X_Max - X_Min + 40.0),
         Height => Gint (Y_Max - Y_Min + 40.0));
      Cairo.Translate (Cr, -X_Min + 20.0, -Y_Min + 20.0);

      Draw_Area (Canvas, Area, Cr);
   end Draw_All;

   ---------------
   -- Draw_Area --
   ---------------

   procedure Draw_Area
     (Canvas : access Interactive_Canvas_Record'Class;
      Rect   : Cairo_Rectangle_Int)
   is
      Cr     : Cairo_Context;
   begin
      if not Realized_Is_Set (Canvas) then
         return;
      end if;

      Cr := Create (Canvas);
      Draw_Area (Canvas, Rect, Cr);
      Destroy (Cr);
   end Draw_Area;

   ---------------
   -- Draw_Area --
   ---------------

   procedure Draw_Area
     (Canvas : access Interactive_Canvas_Record'Class;
      Rect   : Cairo_Rectangle_Int;
      Cr     : Cairo_Context)
   is
      Item   : Canvas_Item;
      Tmp    : Vertex_Iterator := First (Canvas.Children);
      Dest   : Cairo_Rectangle_Int;
      Inters : Boolean;

   begin
      --  If the GC was not created, do not do anything

      if not Realized_Is_Set (Canvas) then
         return;
      end if;

      --  Clear the canvas

      Draw_Background (Canvas, Cr);
      Draw_Grid (Canvas, Cr);

      --  Draw the links first, so that they appear to be below the items.
      --  ??? Should redraw only the required links

      declare
         OX : constant Gint := Canvas.Offset_X_World;
         OY : constant Gint := Canvas.Offset_Y_World;
      begin
         Canvas.Offset_X_World := 0;
         Canvas.Offset_Y_World := 0;

         Update_Links
           (Canvas,
            Cr,
            Invert_Mode    => False,
            From_Selection => False);

         --  Draw each of the items.

         while not At_End (Tmp) loop
            Item := Canvas_Item (Get (Tmp));

            if Item.Visible then
               Intersect
                 (Rect,
                  (Item.Coord.X,
                   Item.Coord.Y,
                   Item.Coord.Width,
                   Item.Coord.Height),
                  Dest, Inters);

               if Inters then
                  Cairo.Save (Cr);

                  --  Translate the coordinates to the item's coordinates
                  Cairo.Translate
                    (Cr, Gdouble (Item.Coord.X), Gdouble (Item.Coord.Y));

                  --  Clip to the item's area
                  Cairo.Rectangle
                    (Cr, 0.0, 0.0,
                     Gdouble (Item.Coord.Width),
                     Gdouble (Item.Coord.Height));
                  Clip (Cr);

                  if Item.Selected then
                     Draw_Selected (Item, Cr);
                  else
                     Draw (Item, Cr);
                  end if;

                  Cairo.Restore (Cr);
               end if;
            end if;

            Next (Tmp);
         end loop;

         Canvas.Offset_X_World := OX;
         Canvas.Offset_Y_World := OY;
      end;

      if Canvas.Offset_X_World /= 0
        or else Canvas.Offset_Y_World /= 0
      then
         Draw_Dashed_Selection (Canvas, Cr);
      end if;
   end Draw_Area;

   ------------
   -- Expose --
   ------------

   function Expose
     (Canv  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event) return Boolean
   is
      Canvas : constant Interactive_Canvas := Interactive_Canvas (Canv);
      Rect   : Cairo_Rectangle_Int renames Get_Area (Event);
      X_W, Y_W : Gdouble;
   begin
      To_World_Coordinates
        (Canvas, Rect.X, Rect.Y, X_W, Y_W);

      if X_W >= Gdouble (Gint'First)
        and then X_W <= Gdouble (Gint'Last)
        and then Y_W >= Gdouble (Gint'First)
        and then Y_W <= Gdouble (Gint'Last)
      then
         Draw_Area
           (Canvas,
            (X      => Gint (X_W),
             Y      => Gint (Y_W),
             Width  => Gint (Gdouble (Rect.Width) / Canvas.Zoom),
             Height => Gint (Gdouble (Rect.Height) / Canvas.Zoom)));
      end if;

      return False;
   end Expose;

   ---------------------
   -- Set_Screen_Size --
   ---------------------

   procedure Set_Screen_Size
     (Item          : access Canvas_Item_Record;
      Width, Height : Gint)
   is
      Old_W, Old_H : Gint;
   begin
      Old_W := Item.Coord.Width;
      Old_H := Item.Coord.Height;

      Item.Coord.Width  := Width;
      Item.Coord.Height := Height;

      if Item.Canvas /= null
        and then (Width /= Old_W or else Height /= Old_H)
      then
         Refresh_Canvas (Item.Canvas);
      end if;
   end Set_Screen_Size;

   -------------------
   -- Draw_Selected --
   -------------------

   procedure Draw_Selected
     (Item : access Canvas_Item_Record;
      Cr   : Cairo.Cairo_Context)
   is
      Sel : constant Gdk_Color :=
              Get_Bg (Get_Style (Item.Canvas), State_Selected);
      P   : Cairo_Pattern;

   begin
      --  Use an intermediate group to allow proper compositing
      Cairo.Push_Group (Cr);

      --  Draw the item
      Cairo.Save (Cr);
      Draw (Canvas_Item (Item), Cr);
      Cairo.Restore (Cr);

      Cairo.Set_Operator (Cr, Cairo_Operator_Atop);
      Cairo.Rectangle
        (Cr, 0.0, 0.0,
         Gdouble (Item.Coord.Width),
         Gdouble (Item.Coord.Height));

      Cairo.Set_Source_Rgba
        (Cr,
         Gdouble (Red (Sel)) / 65535.0,
         Gdouble (Green (Sel)) / 65535.0,
         Gdouble (Blue (Sel)) / 65535.0,
         0.5);

      Cairo.Fill (Cr);

      P := Cairo.Pop_Group (Cr);
      Set_Source (Cr, P);
      Paint (Cr);
   end Draw_Selected;

   ---------------
   -- Key_Press --
   ---------------

   function Key_Press
     (Canv  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      Canvas    : constant Interactive_Canvas := Interactive_Canvas (Canv);
      Value     : constant Gdouble := Canvas.World_Y;
      Upper     : constant Gdouble := Get_Upper (Canvas.Vadj);
      Lower     : constant Gdouble := Get_Lower (Canvas.Vadj);
      Page_Incr : constant Gdouble := Scrolling_Amount_Max;
      Page_Size : constant Gdouble := Get_Page_Size (Canvas.Vadj);
      Step_Incr : constant Gdouble := Scrolling_Amount_Min;

   begin
      --  Note: we do not need to call Changed on the adjustments below, since
      --  we are only modifying the value, not the bounds.

      case Get_Key_Val (Event) is
         when GDK_Home =>
            Set_Value (Canvas.Vadj, Lower);
            return True;

         when GDK_End =>
            Set_Value (Canvas.Vadj, Upper - Page_Size);
            return True;

         when GDK_Page_Up =>
            if Value >= Lower + Page_Incr then
               Set_Value (Canvas.Vadj, Value - Page_Incr);
            else
               Set_Value (Canvas.Vadj, Lower);
            end if;
            return True;

         when GDK_Page_Down =>
            if Value + Page_Incr + Page_Size <= Upper then
               Set_Value (Canvas.Vadj, Value + Page_Incr);
            else
               Set_Value (Canvas.Vadj, Upper - Page_Size);
            end if;
            return True;

         when GDK_Up | GDK_KP_Up =>
            if Value - Step_Incr >= Lower then
               Set_Value (Canvas.Vadj, Value - Step_Incr);
            else
               Set_Value (Canvas.Vadj, Lower);
            end if;
            Gtk.Handlers.Emit_Stop_By_Name (Canvas, "key_press_event");
            return True;

         when GDK_Down | GDK_KP_Down =>
            if Value + Step_Incr + Page_Size <= Upper then
               Set_Value (Canvas.Vadj, Value + Step_Incr);
            else
               Set_Value (Canvas.Vadj, Upper - Page_Size);
            end if;
            Gtk.Handlers.Emit_Stop_By_Name (Canvas, "key_press_event");
            return True;

         when GDK_Left | GDK_KP_Left =>
            if Canvas.World_X -
              Get_Step_Increment (Canvas.Hadj) >=
                Get_Lower (Canvas.Hadj)
            then
               Set_Value (Canvas.Hadj,
                          Canvas.World_X
                          - Get_Step_Increment (Canvas.Hadj));
            else
               Set_Value (Canvas.Hadj,
                          Get_Lower (Canvas.Hadj));
            end if;
            Gtk.Handlers.Emit_Stop_By_Name (Canvas, "key_press_event");
            return True;

         when GDK_Right | GDK_KP_Right =>
            if Canvas.World_X +
              Get_Step_Increment (Canvas.Hadj) +
              Get_Page_Size (Canvas.Hadj) <=
                Get_Upper (Canvas.Hadj)
            then
               Set_Value (Canvas.Hadj,
                          Canvas.World_X +
                            Get_Step_Increment (Canvas.Hadj));
            else
               Set_Value (Canvas.Hadj,
                          Get_Upper (Canvas.Hadj) -
                            Get_Page_Size (Canvas.Hadj));
            end if;
            Gtk.Handlers.Emit_Stop_By_Name (Canvas, "key_press_event");
            return True;

         when others =>
            null;
      end case;

      return False;

   exception
      when others =>
         return False;
   end Key_Press;

   -------------------
   -- Point_In_Item --
   -------------------

   function Point_In_Item
     (Item : access Canvas_Item_Record;
      X, Y : Gint) return Boolean is
   begin
      return X >= Item.Coord.X
        and then X <= Item.Coord.X + Item.Coord.Width
        and then Y >= Item.Coord.Y
        and then Y <= Item.Coord.Y + Item.Coord.Height;
   end Point_In_Item;

   -------------------------
   -- Item_At_Coordinates --
   -------------------------

   function Item_At_Coordinates
     (Canvas : access Interactive_Canvas_Record;
      X, Y   : Glib.Gint) return Canvas_Item
   is
      Tmp    : Vertex_Iterator := First (Canvas.Children);
      Result : Canvas_Item := null;
      Item   : Canvas_Item;
   begin
      --  Keep the last item found, since this is the one on top.
      --  ??? Not the most efficient way to search, since we have to traverse
      --  the whole list every time.

      while not At_End (Tmp) loop
         Item := Canvas_Item (Get (Tmp));

         if Item.Visible and then Point_In_Item (Item, X, Y) then
            Result := Item;
         end if;

         Next (Tmp);
      end loop;

      return Result;
   end Item_At_Coordinates;

   -------------------------
   -- Item_At_Coordinates --
   -------------------------

   function Item_At_Coordinates
     (Canvas : access Interactive_Canvas_Record; Event : Gdk_Event)
      return Canvas_Item
   is
      X_World, Y_World : Gdouble;
      Item             : Canvas_Item;

   begin
      To_World_Coordinates (Canvas, Event, X_World, Y_World);
      Item := Item_At_Coordinates (Canvas, Gint (X_World), Gint (Y_World));
      return Item;
   end Item_At_Coordinates;

   -------------------------
   -- Item_At_Coordinates --
   -------------------------

   procedure Item_At_Coordinates
     (Canvas : access Interactive_Canvas_Record;
      Event  : Gdk.Event.Gdk_Event;
      Item   : out Canvas_Item;
      X, Y   : out Glib.Gint)
   is
      X_World, Y_World : Gdouble;

   begin
      To_World_Coordinates (Canvas, Event, X_World, Y_World);
      Item := Item_At_Coordinates (Canvas, Gint (X_World), Gint (Y_World));
      if Item /= null then
         X := Gint (X_World) - Item.Coord.X;
         Y := Gint (Y_World) - Item.Coord.Y;
      end if;
   end Item_At_Coordinates;

   --------------------
   -- Button_Pressed --
   --------------------

   function Button_Pressed
     (Canv  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      Canvas  : constant Interactive_Canvas := Interactive_Canvas (Canv);
      Cursor  : Gdk.Cursor.Gdk_Cursor;
      Handled : Boolean;

   begin
      if Get_Window (Event) /= Get_Window (Canvas) then
         return False;
      end if;

      Grab_Focus (Canvas);
      Set_Flags (Canvas, Has_Focus);

      To_World_Coordinates
        (Canvas, Event, Canvas.World_X_At_Click, Canvas.World_Y_At_Click);

      --  Find the selected item.

      Canvas.Item_Press := Item_At_Coordinates (Canvas, Event);

      if Traces then
         if Canvas.Item_Press /= null then
            Put_Line ("Clicked on Item at world coordinates ("
                      & Gdouble'Image (Canvas.World_X_At_Click)
                      & Gdouble'Image (Canvas.World_Y_At_Click)
                      & ") item=("
                      & Gint'Image (Canvas.Item_Press.Coord.X)
                      & Gint'Image (Canvas.Item_Press.Coord.Y)
                      & Gint'Image (Canvas.Item_Press.Coord.Width)
                      & Gint'Image (Canvas.Item_Press.Coord.Height)
                      & ") mouse=" & Gint'Image (Gint (Get_X (Event)))
                      & Gint'Image (Gint (Get_Y (Event))));
         else
            Put_Line ("Clicked outside of item at world coordinates "
                      & Gdouble'Image (Canvas.World_X_At_Click)
                      & " " & Gdouble'Image (Canvas.World_Y_At_Click));
         end if;
      end if;

      --  Button press on the background: clear the selection
      if Canvas.Item_Press = null then
         if (Get_State (Event) and Default_Modifier_Mask) = 0 then
            Clear_Selection (Canvas);
         end if;

         Widget_Callback.Emit_By_Name (Canvas, "background_click", Event);
         Canvas.Background_Press := True;

      else
         Canvas.Background_Press := False;

         if (Get_State (Event) and Default_Modifier_Mask) /= 0 then
            if Is_Selected (Canvas, Canvas.Item_Press) then
               Remove_From_Selection (Canvas, Canvas.Item_Press);
            else
               Add_To_Selection (Canvas, Canvas.Item_Press);
            end if;
         else
            Set_X
              (Event,
               Canvas.World_X_At_Click - Gdouble (Canvas.Item_Press.Coord.X));
            Set_Y
              (Event,
               Canvas.World_Y_At_Click - Gdouble (Canvas.Item_Press.Coord.Y));
            Handled := On_Button_Click (Canvas.Item_Press, Event);

            if not Handled then
               --  If not handled, then:
               --  if the iter was part of a selection, do nothing,
               --  if the iter was not part of a selection, clear the selection
               --  and select this iter.

               if not Canvas.Item_Press.Selected then
                  Clear_Selection (Canvas);

                  Add_To_Selection (Canvas, Canvas.Item_Press);
               end if;

               Canvas.Item_Press := null;
            else
               return True;
            end if;
         end if;
      end if;

      --  Clicks other than left mouse button are transmitted directly

      if Get_Button (Event) /= 1 then
         return False;
      end if;

      --  Change the cursor to give visual feedback

      Gdk_New (Cursor, Fleur);
      Set_Cursor (Get_Window (Canvas), Cursor);
      Unref (Cursor);

      --  Initialize the move

      Canvas.Offset_X_World      := 0;
      Canvas.Offset_Y_World      := 0;
      Canvas.Mouse_Has_Moved     := False;
      Canvas.Surround_Box_Scroll := Scrolling_Amount_Min;

      --  Make sure that no other widget steals the events while we are
      --  moving an item.

      Grab_Add (Canvas);

      return False;

   exception
      when others =>
         return False;
   end Button_Pressed;

   -------------------
   -- Button_Motion --
   -------------------

   function Button_Motion
     (Canv  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      Canvas             : constant Interactive_Canvas :=
                             Interactive_Canvas (Canv);
      X_Scroll, Y_Scroll : Gdouble;
      Dead               : Boolean;
      pragma Unreferenced (Dead);

   begin
      if Get_Window (Event) /= Get_Window (Canvas) then
         return False;
      end if;

      if Canvas.Item_Press /= null then
         declare
            New_X, New_Y : Gdouble;
         begin
            To_World_Coordinates (Canvas, Event, New_X, New_Y);
            Set_X (Event, New_X - Gdouble (Canvas.Item_Press.Coord.X));
            Set_Y (Event, New_Y - Gdouble (Canvas.Item_Press.Coord.Y));
         end;

         return On_Button_Click (Canvas.Item_Press, Event);
      end if;

      --  Are we in the scrolling box ? If yes, do not move the item
      --  directly, but establish the timeout callbacks that will take care
      --  of the scrolling

      Test_Scrolling_Box
        (Canvas            => Canvas,
         Mouse_X_In_Canvas => Gint (Get_X (Event)),
         Mouse_Y_In_Canvas => Gint (Get_Y (Event)),
         X_Scroll          => X_Scroll,
         Y_Scroll          => Y_Scroll);

      if X_Scroll /= 0.0 or else Y_Scroll /= 0.0 then
         if Canvas.Scrolling_Timeout_Id = 0 then
            if Traces then
               Put_Line ("Button_Motion, within the scrolling box,"
                         & " starting timeout");
            end if;
            Canvas.Scrolling_Timeout_Id := Canvas_Timeout.Timeout_Add
              (Timeout_Between_Scrolls, Scrolling_Timeout'Access, Canvas);
         end if;
         return False;
      end if;

      if Canvas.Scrolling_Timeout_Id /= 0 then
         if Traces then
            Put_Line ("Button_Motion, cancel timeout");
         end if;
         Remove (Canvas.Scrolling_Timeout_Id);
         Canvas.Surround_Box_Scroll := Scrolling_Amount_Min;
         Canvas.Scrolling_Timeout_Id := 0;
      end if;

      --  Find the current mouse position in world coordinates, to find out
      --  where to draw the dashed outline.

      To_World_Coordinates
        (Canvas, Event, X_Scroll, Y_Scroll);

      Dead := Move_Selection
        (Canvas,
         New_Offset_X_World => X_Scroll - Canvas.World_X_At_Click,
         New_Offset_Y_World => Y_Scroll - Canvas.World_Y_At_Click);

      return False;
   end Button_Motion;

   --------------------
   -- Button_Release --
   --------------------

   function Button_Release
     (Canv  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      Canvas       : constant Interactive_Canvas := Interactive_Canvas (Canv);
      Rect, Coord  : Gdk_Rectangle;
      Iter         : Item_Iterator;
      Item         : Canvas_Item;
      Handled      : Boolean;

   begin
      Grab_Remove (Canvas);

      --  Restore the standard cursor
      Set_Cursor (Get_Window (Canvas), null);

      if Get_Window (Event) /= Get_Window (Canvas) then
         return False;
      end if;

      if Canvas.Scrolling_Timeout_Id /= 0 then
         Remove (Canvas.Scrolling_Timeout_Id);
         Canvas.Scrolling_Timeout_Id := 0;
         Canvas.Surround_Box_Scroll := Scrolling_Amount_Min;
      end if;

      if Canvas.Item_Press /= null then

         --  Translate the button's coordinates
         declare
            New_X, New_Y : Gdouble;
         begin
            To_World_Coordinates (Canvas, Event, New_X, New_Y);
            Set_X (Event, New_X - Gdouble (Canvas.Item_Press.Coord.X));
            Set_Y (Event, New_Y - Gdouble (Canvas.Item_Press.Coord.Y));
         end;

         Handled := On_Button_Click (Canvas.Item_Press, Event);
         Canvas.Item_Press := null;

         return Handled;

      elsif Canvas.Selected_Count = 0
        and then Canvas.Background_Press
      then
         Widget_Callback.Emit_By_Name (Canvas, "background_click", Event);

         --  Select all the items inside the rectangle

         Rect := Get_Background_Selection_Rectangle (Canvas);

         Iter := Start (Canvas, Selected_Only => False);
         while Get (Iter) /= null loop
            Coord := Get_Coord (Get (Iter));

            --  Only items fully contained in the rectangle are selected
            if Rect.X <= Coord.X
              and then Coord.X + Coord.Width <= Rect.X + Rect.Width
              and then Rect.Y <= Coord.Y
              and then Coord.Y + Coord.Height <= Rect.Y + Rect.Height
            then
               Add_To_Selection (Canvas, Get (Iter));
            end if;

            Next (Iter);
         end loop;

         Canvas.Offset_X_World := 0;
         Canvas.Offset_Y_World := 0;

         Queue_Draw (Canvas);

         return True;

      elsif Canvas.Mouse_Has_Moved then
         Iter := Start (Canvas, Selected_Only => True);
         loop
            Item := Get (Iter);
            exit when Item = null;

            Item.Coord.X := Item.Coord.X + Canvas.Offset_X_World;
            Item.Coord.Y := Item.Coord.Y + Canvas.Offset_Y_World;

            if Canvas.Align_On_Grid then
               Item.Coord.X := Item.Coord.X
                 - Item.Coord.X mod Gint (Canvas.Grid_Size);
               Item.Coord.Y := Item.Coord.Y
                 - Item.Coord.Y mod Gint (Canvas.Grid_Size);
            end if;
            Item.From_Auto_Layout := False;

            Emit_By_Name_Item
              (Get_Object (Canvas), "item_moved" & ASCII.NUL, Item);

            Next (Iter);
         end loop;

         Canvas.Offset_X_World := 0;
         Canvas.Offset_Y_World := 0;

         --  Scroll the canvas so as to show the first item from the selection
         Refresh_Canvas (Canvas);

      else
         --  If we are reaching this point, this means that there wasn't an
         --  item being pressed, and we didn't perform a button pressed move.
         --  So if there is an item under the cursor, if this item wasn't
         --  already selected, clear the selection.
         Item := Item_At_Coordinates (Canvas, Event);
         if not Item.Selected then
            Clear_Selection (Canvas);
         end if;
      end if;

      Canvas.Item_Press := null;

      return False;

   exception
      when others =>
         return False;
   end Button_Release;

   ----------------------------------------
   -- Get_Background_Selection_Rectangle --
   ----------------------------------------

   function Get_Background_Selection_Rectangle
     (Canvas : access Interactive_Canvas_Record'Class) return Gdk_Rectangle
   is
      X : Gint := Gint (Canvas.World_X_At_Click);
      Y : Gint := Gint (Canvas.World_Y_At_Click);
      W : Gint := Canvas.Offset_X_World;
      H : Gint := Canvas.Offset_Y_World;

   begin
      if W < 0 then
         W := -W;
         X := X - W;
      end if;

      if H < 0 then
         H := -H;
         Y := Y - H;
      end if;

      return (X, Y, W, H);
   end Get_Background_Selection_Rectangle;

   ------------------------
   -- Test_Scrolling_Box --
   ------------------------

   procedure Test_Scrolling_Box
     (Canvas   : access Interactive_Canvas_Record'Class;
      Mouse_X_In_Canvas, Mouse_Y_In_Canvas : Gint;
      X_Scroll : out Gdouble;
      Y_Scroll : out Gdouble)
   is
   begin
      if Mouse_X_In_Canvas < Scrolling_Margin then
         X_Scroll := -Canvas.Surround_Box_Scroll / Canvas.Zoom;
      elsif Mouse_X_In_Canvas >
        Gint (Get_Allocation_Width (Canvas)) - Scrolling_Margin
      then
         X_Scroll := Canvas.Surround_Box_Scroll / Canvas.Zoom;
      else
         X_Scroll := 0.0;
      end if;

      if Mouse_Y_In_Canvas < Scrolling_Margin then
         Y_Scroll := -Canvas.Surround_Box_Scroll / Canvas.Zoom;
      elsif Mouse_Y_In_Canvas >
        Gint (Get_Allocation_Height (Canvas)) - Scrolling_Margin
      then
         Y_Scroll := Canvas.Surround_Box_Scroll / Canvas.Zoom;
      else
         Y_Scroll := 0.0;
      end if;

      if Traces then
         Put_Line ("Test_Scrolling_Box, world delta="
                   & Gdouble'Image (X_Scroll) & " "
                   & Gdouble'Image (Y_Scroll)
                   & " mouse canvas="
                   & Gint'Image (Mouse_X_In_Canvas)
                   & Gint'Image (Mouse_Y_In_Canvas));
      end if;
   end Test_Scrolling_Box;

   -----------------------
   -- Scrolling_Timeout --
   -----------------------

   function Scrolling_Timeout (Canvas : Interactive_Canvas) return Boolean is
      Mouse_X_Canvas, Mouse_Y_Canvas : Gint;
      Mask                           : Gdk_Modifier_Type;
      W                              : Gdk_Window;
      X_Scroll, Y_Scroll             : Gdouble;
   begin
      if Traces then
         Put_Line ("Scrolling timeout");
      end if;

      Get_Pointer
        (Get_Window (Canvas), Mouse_X_Canvas, Mouse_Y_Canvas, Mask, W);
      Test_Scrolling_Box
        (Canvas, Mouse_X_Canvas, Mouse_Y_Canvas, X_Scroll, Y_Scroll);

      if (X_Scroll /= 0.0 or else Y_Scroll /= 0.0)
        and then Move_Selection
          (Canvas,
           New_Offset_X_World => X_Scroll + Gdouble (Canvas.Offset_X_World),
           New_Offset_Y_World => Y_Scroll + Gdouble (Canvas.Offset_Y_World))
      then
         --  Keep increasing the speed
         if Canvas.Surround_Box_Scroll < Scrolling_Amount_Max then
            Canvas.Surround_Box_Scroll := Canvas.Surround_Box_Scroll
              * Scrolling_Amount_Increase;
         end if;

         --  Force an immediate draw, since Queue_Draw would only redraw in
         --  an idle event, and thus might not happen before the next timeout.
         --  With lots of items, this would break the scrolling.
         Draw (Canvas);
         return True;
      else
         Canvas.Surround_Box_Scroll := Scrolling_Amount_Min;
         Canvas.Scrolling_Timeout_Id := 0;
         return False;
      end if;
   end Scrolling_Timeout;

   ---------------------------
   -- Draw_Dashed_Selection --
   ---------------------------

   procedure Draw_Dashed_Selection
     (Canvas : access Interactive_Canvas_Record'Class;
      Cr     : Cairo_Context)
   is
      Iter  : Item_Iterator;
      Item  : Canvas_Item;
      X, Y  : Gint;
      Rect  : Gdk_Rectangle;
      Sel   : constant Gdk_Color :=
                Get_Bg (Get_Style (Canvas), State_Selected);

   begin
      if Canvas.Selected_Count = 0 then
         Rect := Get_Background_Selection_Rectangle (Canvas);
         Rectangle
           (Cr,
            Gdouble (Rect.X) + 0.5,
            Gdouble (Rect.Y) + 0.5,
            Gdouble (Rect.Width) - 1.0,
            Gdouble (Rect.Height) - 1.0);

         Cairo.Set_Source_Rgba
           (Cr,
            Gdouble (Red (Sel)) / 65535.0,
            Gdouble (Green (Sel)) / 65535.0,
            Gdouble (Blue (Sel)) / 65535.0,
            0.3);
         Fill_Preserve (Cr);
         Set_Source_Color (Cr, Sel);
         Stroke (Cr);

      else
         Iter := Start (Canvas, Selected_Only => True);
         Cairo.Set_Operator (Cr, Cairo_Operator_Xor);
         Set_Source_Rgba
           (Cr,
            Gdouble (Red (Sel)) / 65535.0,
            Gdouble (Green (Sel)) / 65535.0,
            Gdouble (Blue (Sel)) / 65535.0,
            0.5);

         loop
            Item := Get (Iter);
            exit when Item = null;

            if Item.Visible then
               X := Item.Coord.X + Canvas.Offset_X_World;
               Y := Item.Coord.Y + Canvas.Offset_Y_World;

               if Canvas.Align_On_Grid then
                  X := X - X mod Gint (Canvas.Grid_Size);
                  Y := Y - Y mod Gint (Canvas.Grid_Size);
               end if;

               Rectangle
                 (Cr,
                  Gdouble (X) + 0.5,
                  Gdouble (Y) + 0.5,
                  Gdouble (Item.Coord.Width),
                  Gdouble (Item.Coord.Height));
               Cairo.Fill (Cr);
            end if;
            Next (Iter);
         end loop;

         Update_Links
           (Canvas, Cr, Invert_Mode => True, From_Selection => True);
      end if;
   end Draw_Dashed_Selection;

   --------------------
   -- Move_Selection --
   --------------------

   function Move_Selection
     (Canvas : access Interactive_Canvas_Record'Class;
      New_Offset_X_World, New_Offset_Y_World : Gdouble) return Boolean
   is
      Z : Gdouble renames Canvas.Zoom;
   begin
      if not Canvas.Mouse_Has_Moved then
         --  Is this a motion, or simply a selection ?

         if abs (New_Offset_X_World) <= Canvas.Motion_Threshold / Z
           and then abs (New_Offset_Y_World) <= Canvas.Motion_Threshold / Z
         then
            return False;
         end if;
      end if;

      Canvas.Mouse_Has_Moved := True;

      if Traces then
         Put_Line ("Move_Selection, delta world="
                   & Gdouble'Image (New_Offset_X_World)
                   & " " & Gdouble'Image (New_Offset_Y_World));
      end if;

      Canvas.Offset_X_World := Gint (New_Offset_X_World);
      Canvas.Offset_Y_World := Gint (New_Offset_Y_World);

      Update_Adjustments (Canvas, Clip_Value => False);

      Scroll_Canvas_To_Area
        (Canvas,
         Canvas.World_X_At_Click +
           Gdouble (Canvas.Offset_X_World - Scrolling_Margin),
         Canvas.World_Y_At_Click +
           Gdouble (Canvas.Offset_Y_World - Scrolling_Margin),
         Canvas.World_X_At_Click +
           Gdouble (Canvas.Offset_X_World + Scrolling_Margin),
         Canvas.World_Y_At_Click +
           Gdouble (Canvas.Offset_Y_World + Scrolling_Margin),
         Canvas_X => 2.0,
         Canvas_Y => 2.0,
         Ignore_If_Visible => True,
         Report_Adj_Changed => True);

      Queue_Draw (Canvas);

      return True;
   end Move_Selection;

   ------------------
   -- Item_Updated --
   ------------------

   procedure Item_Updated
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class)
   is
   begin
      if Item.Visible then
         Queue_Draw_Area
           (Canvas,
            Item.Coord.X,
            Item.Coord.Y,
            Item.Coord.Width,
            Item.Coord.Height);
      end if;
   end Item_Updated;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class) is
   begin
      Remove_From_Selection (Canvas, Item);
      Remove (Canvas.Children, Item);

      --  Have to redraw everything, since there might have been some
      --  links.
      --  ??? Note very efficient when removing several items.
      Refresh_Canvas (Canvas);
   end Remove;

   -----------
   -- Clear --
   -----------

   procedure Clear (Canvas : access Interactive_Canvas_Record) is
   begin
      Clear_Selection (Canvas);
      Clear (Canvas.Children);
      Refresh_Canvas (Canvas);
   end Clear;

   ---------------------
   -- On_Button_Click --
   ---------------------

   function On_Button_Click
     (Item  : access Canvas_Item_Record;
      Event : Gdk.Event.Gdk_Event_Button) return Boolean
   is
      pragma Unreferenced (Item, Event);
   begin
      return False;
   end On_Button_Click;

   ---------------
   -- Get_Coord --
   ---------------

   function Get_Coord
     (Item : access Canvas_Item_Record) return Gdk.Rectangle.Gdk_Rectangle is
   begin
      return Item.Coord;
   end Get_Coord;

   --------------
   -- Has_Link --
   --------------

   function Has_Link
     (Canvas   : access Interactive_Canvas_Record;
      From, To : access Canvas_Item_Record'Class;
      Name     : UTF8_String := "") return Boolean
   is
      Current   : Edge_Iterator := First
        (Canvas.Children,
         Src  => Vertex_Access (From),
         Dest => Vertex_Access (To),
         Directed => False);
      E         : Canvas_Link;
      Candidate : Boolean;
   begin
      --  We need to examine both links from FROM to TO and from TO to FROM,
      --  since the layout algorithm might sometimes transparently revert links
      --  to get an acyclic graph

      while not At_End (Current) loop
         E := Canvas_Link (Get (Current));
         if Get_Arrow_Type (E) = End_Arrow then
            Candidate := Get_Src (E) = Vertex_Access (From)
              and then Get_Dest (E) = Vertex_Access (To);
         elsif Get_Arrow_Type (E) = Start_Arrow then
            Candidate := Get_Src (E) = Vertex_Access (To)
              and then Get_Dest (E) = Vertex_Access (From);
         else
            Candidate := True;
         end if;

         if Candidate
           and then
             (Name = ""
              or else (Canvas_Link (Get (Current)).Descr /= null
                       and then Canvas_Link (Get (Current)).Descr.all = Name))
         then
            return True;
         end if;
         Next (Current);
      end loop;
      return False;
   end Has_Link;

   ----------------
   -- Lower_Item --
   ----------------

   procedure Lower_Item
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class) is
   begin
      Move_To_Front (Canvas.Children, Item);

      --  Redraw just the part of the canvas that is impacted.
      Item_Updated (Canvas, Item);
   end Lower_Item;

   ----------------
   -- Raise_Item --
   ----------------

   procedure Raise_Item
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class) is
   begin
      Move_To_Back (Canvas.Children, Item);

      --  Redraw just the part of the canvas that is impacted.
      Item_Updated (Canvas, Item);
   end Raise_Item;

   ---------------
   -- Is_On_Top --
   ---------------

   function Is_On_Top
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class) return Boolean
   is
      Iter : Vertex_Iterator := First (Canvas.Children);
      Last : Canvas_Item := null;
   begin
      while not At_End (Iter) loop
         Last := Canvas_Item (Get (Iter));
         Next (Iter);
      end loop;
      return Last = Canvas_Item (Item);
   end Is_On_Top;

   ---------------
   -- Show_Item --
   ---------------

   procedure Show_Item
     (Canvas             : access Interactive_Canvas_Record'Class;
      Item               : access Canvas_Item_Record'Class;
      Canvas_X, Canvas_Y : Gdouble;
      Report_Adj_Changed : Boolean := True)
   is
   begin
      Scroll_Canvas_To_Item
        (Canvas, Item, Canvas_X, Canvas_Y, Report_Adj_Changed);
   end Show_Item;

   ----------------
   -- Align_Item --
   ----------------

   procedure Align_Item
     (Canvas  : access Interactive_Canvas_Record;
      Item    : access Canvas_Item_Record'Class;
      X_Align : Float := 0.5;
      Y_Align : Float := 0.5) is
   begin
      Show_Item (Canvas, Item, Gdouble (X_Align), Gdouble (Y_Align));
   end Align_Item;

   ---------------
   -- Show_Item --
   ---------------

   procedure Show_Item
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class) is
   begin
      Show_Item (Canvas, Item, 0.5, 0.5);
   end Show_Item;

   -----------------------
   -- Get_Align_On_Grid --
   -----------------------

   function Get_Align_On_Grid
     (Canvas : access Interactive_Canvas_Record) return Boolean is
   begin
      return Canvas.Align_On_Grid;
   end Get_Align_On_Grid;

   --------------------
   -- Set_Visibility --
   --------------------

   procedure Set_Visibility
     (Item    : access Canvas_Item_Record;
      Visible : Boolean) is
   begin
      Item.Visible := Visible;
   end Set_Visibility;

   ----------------
   -- Is_Visible --
   ----------------

   function Is_Visible (Item : access Canvas_Item_Record) return Boolean is
   begin
      return Item.Visible;
   end Is_Visible;

   --------------------
   -- Refresh_Canvas --
   --------------------

   procedure Refresh_Canvas (Canvas : access Interactive_Canvas_Record) is
   begin
      Update_Adjustments (Canvas);
      Queue_Draw (Canvas);
   end Refresh_Canvas;

   ---------------------
   -- Clear_Selection --
   ---------------------

   procedure Clear_Selection (Canvas : access Interactive_Canvas_Record) is
      Iter : Item_Iterator := Start (Canvas, Selected_Only => True);
   begin
      while Get (Iter) /= null loop
         Remove_From_Selection (Canvas, Get (Iter));
         Next (Iter);
      end loop;
   end Clear_Selection;

   ----------------------
   -- Add_To_Selection --
   ----------------------

   procedure Add_To_Selection
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class)
   is
   begin
      if not Item.Selected then
         Canvas.Selected_Count := Canvas.Selected_Count + 1;
         Item.Selected := True;
         Selected (Item, Canvas, Is_Selected => True);
         Emit_By_Name_Item
           (Get_Object (Canvas), "item_selected" & ASCII.NUL, Item);
      end if;
   end Add_To_Selection;

   ---------------------------
   -- Remove_From_Selection --
   ---------------------------

   procedure Remove_From_Selection
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class)
   is
   begin
      if Item.Selected then
         Canvas.Selected_Count := Canvas.Selected_Count - 1;
         Item.Selected := False;
         if not In_Destruction_Is_Set (Canvas) then
            Selected (Item, Canvas, Is_Selected => False);
         end if;

         Emit_By_Name_Item
           (Get_Object (Canvas), "item_unselected" & ASCII.NUL, Item);
      end if;
   end Remove_From_Selection;

   ----------------
   -- Select_All --
   ----------------

   procedure Select_All (Canvas : access Interactive_Canvas_Record) is
      Iter : Item_Iterator := Start (Canvas, Selected_Only => False);
      Item : Canvas_Item;
   begin
      loop
         Item := Get (Iter);
         exit when Item = null;
         Add_To_Selection (Canvas, Item);
         Next (Iter);
      end loop;
   end Select_All;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Link   : access Canvas_Link_Record;
      Arrow  : Arrow_Type := End_Arrow;
      Descr  : UTF8_String := "") is
   begin
      Link.Arrow := Arrow;
      Free (Link.Descr);
      Link.Descr := new String'(Descr);
   end Configure;

   --------------
   -- Add_Link --
   --------------

   procedure Add_Link
     (Canvas : access Interactive_Canvas_Record;
      Link   : access Canvas_Link_Record'Class;
      Src    : access Canvas_Item_Record'Class;
      Dest   : access Canvas_Item_Record'Class;
      Arrow  : Arrow_Type := End_Arrow;
      Descr  : UTF8_String := "") is
   begin
      Configure (Link, Arrow, Descr);
      Add_Edge (Canvas.Children, Link, Src, Dest);
   end Add_Link;

   -----------------
   -- Remove_Link --
   -----------------

   procedure Remove_Link
     (Canvas : access Interactive_Canvas_Record;
      Link   : access Canvas_Link_Record'Class) is
   begin
      Remove (Canvas.Children, Link);
   end Remove_Link;

   -------------------
   -- For_Each_Link --
   -------------------

   procedure For_Each_Link
     (Canvas  : access Interactive_Canvas_Record;
      Execute : Link_Processor;
      From, To : Canvas_Item := null)
   is
      Iter : Edge_Iterator := First
        (Canvas.Children, Vertex_Access (From), Vertex_Access (To));
      Link : Canvas_Link;
   begin
      while not At_End (Iter) loop
         Link := Canvas_Link (Get (Iter));
         Next (Iter);
         exit when not Execute (Canvas, Link);
      end loop;
   end For_Each_Link;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Link : in out Canvas_Link_Record) is
   begin
      Free (Link.Descr);
   end Destroy;

   procedure Destroy (Item : in out Canvas_Item_Record) is
      pragma Unreferenced (Item);
   begin
      null;
   end Destroy;

   ---------------
   -- Get_Descr --
   ---------------

   function Get_Descr (Link : access Canvas_Link_Record) return UTF8_String is
   begin
      if Link.Descr = null then
         return "";
      else
         return Link.Descr.all;
      end if;
   end Get_Descr;

   -----------------
   -- Set_Src_Pos --
   -----------------

   procedure Set_Src_Pos
     (Link : access Canvas_Link_Record; X_Pos, Y_Pos : Gfloat := 0.5) is
   begin
      Link.Src_X_Pos := X_Pos;
      Link.Src_Y_Pos := Y_Pos;
   end Set_Src_Pos;

   ------------------
   -- Set_Dest_Pos --
   ------------------

   procedure Set_Dest_Pos
     (Link : access Canvas_Link_Record; X_Pos, Y_Pos : Gfloat := 0.5) is
   begin
      Link.Dest_X_Pos := X_Pos;
      Link.Dest_Y_Pos := Y_Pos;
   end Set_Dest_Pos;

   ------------------
   -- Zoom_Timeout --
   ------------------

   function Zoom_Timeout (Canvas : Interactive_Canvas) return Boolean is
      Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Z   : Gdouble;
      dT  : Gdouble;
      use type Ada.Calendar.Time;

   begin
      if Canvas.Zoom_Start + Canvas.Zoom_Duration < Now then
         Zoom_Internal (Canvas, Canvas.Target_Zoom);
         return False;
      else
         dT := Gdouble (Now - Canvas.Zoom_Start);
         Z  := Canvas.Initial_Zoom +
           (Canvas.Target_Zoom - Canvas.Initial_Zoom) *
             dT / Gdouble (Canvas.Zoom_Duration);
         Zoom_Internal (Canvas, Z);

         return True;
      end if;
   end Zoom_Timeout;

   -------------------
   -- Zoom_Internal --
   -------------------

   procedure Zoom_Internal
     (Canvas : access Interactive_Canvas_Record'Class; Percent : Gdouble)
   is
   begin
      --  Display the proper area in the canvas
      --  When zooming out, we want to keep the old area centered into the
      --  new one.
      --  When zooming in, we want to keep the same center as before
      --  (reverse of zoom out)

      --  Apply the zoom
      Canvas.Zoom := Percent;
      Canvas.Freeze := True;
      --  Only update the page size, other values will be updated when
      --  centering the zoom area
      Update_Adjustments
        (Canvas,
         Min_Max      => False,
         Page_Size    => True,
         Clip_Value   => False,
         Send_Changed => False);

      --  Display the proper area in the canvas
      --  When zooming out, we want to keep the old area centered into the
      --  new one.
      --  When zooming in, we want to keep the same center as before
      --  (reverse of zoom out)
      Scroll_Canvas_To_Area
        (Canvas,
         Canvas.Zoom_X, Canvas.Zoom_Y, Canvas.Zoom_X, Canvas.Zoom_Y,
         Canvas_X => 0.5, Canvas_Y => 0.5,
         Ignore_If_Visible => False,
         Report_Adj_Changed => True);
      Canvas.Freeze := False;

      Queue_Draw (Canvas);

      Widget_Callback.Emit_By_Name (Canvas, "zoomed");
   end Zoom_Internal;

   ----------
   -- Zoom --
   ----------

   procedure Zoom
     (Canvas : access Interactive_Canvas_Record;
      Percent : Gdouble  := 1.0;
      Length  : Duration := 0.0)
   is
      Id : G_Source_Id;
      pragma Unreferenced (Id);
   begin
      if Canvas.Zoom = Percent then
         return;
      end if;
      Canvas.Target_Zoom := Percent;
      Canvas.Initial_Zoom := Canvas.Zoom;
      Canvas.Zoom_X :=
        Canvas.World_X +
        Gdouble (Get_Allocation_Width (Canvas)) / Canvas.Zoom / 2.0;
      Canvas.Zoom_Y :=
        Canvas.World_Y +
          Gdouble (Get_Allocation_Height (Canvas)) / Canvas.Zoom / 2.0;
      Canvas.Zoom_Start := Ada.Calendar.Clock;

      --  Do we want smooth scrolling ?
      if Length > 0.0 then
         Canvas.Zoom_Duration := Length;

         Id := Canvas_Timeout.Idle_Add
           (Zoom_Timeout'Access, Interactive_Canvas (Canvas));

      else
         Zoom_Internal (Canvas, Percent);
      end if;
   end Zoom;

   --------------
   -- Get_Zoom --
   --------------

   function Get_Zoom
     (Canvas : access Interactive_Canvas_Record) return Glib.Gdouble is
   begin
      return Canvas.Zoom;
   end Get_Zoom;

   --------------
   -- Scrolled --
   --------------

   procedure Scrolled (Canvas : access Gtk_Widget_Record'Class) is
      C : constant Interactive_Canvas := Interactive_Canvas (Canvas);
   begin
      C.World_X := Get_Value (C.Hadj);
      C.World_Y := Get_Value (C.Vadj);
      Queue_Draw (Canvas);
   end Scrolled;

   ----------
   -- Draw --
   ----------

   procedure Draw
     (Item : access Buffered_Item_Record;
      Cr   : Cairo.Cairo_Context) is
   begin
      Cairo.Set_Source_Surface (Cr, Item.Pixmap, 0.0, 0.0);
      Cairo.Rectangle
        (Cr, 0.0, 0.0,
         Gdouble (Item.Coord.Width), Gdouble (Item.Coord.Height));
      Cairo.Fill (Cr);

      if Status (Cr) /= Cairo_Status_Success then
         Put_Line ("??? Cannot draw buffered item: " &
                     Cairo_Status'Image (Status (Cr)));
      end if;
   end Draw;

   ---------------------
   -- Set_Screen_Size --
   ---------------------

   procedure Set_Screen_Size
     (Item   : access Buffered_Item_Record;
      Width, Height  : Glib.Gint)
   is
   begin
      if Item.Pixmap /= Null_Surface then
         Cairo.Surface.Destroy (Item.Pixmap);
      end if;

      --  Always pass a drawable, so that the colormap for Item.Pixmap is
      --  set correctly. Otherwise, on setups where colormaps are used we
      --  get a crash
      Item.Pixmap := Create (Cairo_Format_ARGB32, Width, Height);

      Set_Screen_Size (Canvas_Item_Record (Item.all)'Access, Width, Height);
   end Set_Screen_Size;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Item : in out Buffered_Item_Record) is
   begin
      if Item.Pixmap /= Null_Surface then
         Destroy (Item.Pixmap);
         Item.Pixmap := Null_Surface;
      end if;

      Destroy (Canvas_Item_Record (Item));
   end Destroy;

   -------------
   -- Surface --
   -------------

   function Surface (Item : access Buffered_Item_Record)
                    return Cairo_Surface is
   begin
      return Item.Pixmap;
   end Surface;

   --------------------
   -- Get_Arrow_Type --
   --------------------

   function Get_Arrow_Type
     (Link : access Canvas_Link_Record) return Arrow_Type is
   begin
      return Link.Arrow;
   end Get_Arrow_Type;

   --------------------------
   -- Set_Orthogonal_Links --
   --------------------------

   procedure Set_Orthogonal_Links
     (Canvas : access Interactive_Canvas_Record;
      Orthogonal : Boolean) is
   begin
      Canvas.Orthogonal_Links := Orthogonal;
   end Set_Orthogonal_Links;

   --------------------------
   -- Get_Orthogonal_Links --
   --------------------------

   function Get_Orthogonal_Links
     (Canvas : access Interactive_Canvas_Record) return Boolean is
   begin
      return Canvas.Orthogonal_Links;
   end Get_Orthogonal_Links;

   -------------------------
   -- Is_From_Auto_Layout --
   -------------------------

   function Is_From_Auto_Layout
     (Item : access Canvas_Item_Record) return Boolean is
   begin
      return Item.From_Auto_Layout;
   end Is_From_Auto_Layout;

   -----------------
   -- Is_Selected --
   -----------------

   function Is_Selected
     (Canvas : access Interactive_Canvas_Record;
      Item   : access Canvas_Item_Record'Class) return Boolean
   is
      pragma Unreferenced (Canvas);
   begin
      return Item.Selected;
   end Is_Selected;

   ------------
   -- Canvas --
   ------------

   function Canvas
     (Item : access Canvas_Item_Record) return Interactive_Canvas is
   begin
      return Item.Canvas;
   end Canvas;

   --------------
   -- Selected --
   --------------

   procedure Selected
     (Item        : access Canvas_Item_Record;
      Canvas      : access Interactive_Canvas_Record'Class;
      Is_Selected : Boolean)
   is
      pragma Unreferenced (Item, Is_Selected);
   begin
      Queue_Draw (Canvas);
   end Selected;

   -----------------
   -- Get_Src_Pos --
   -----------------

   procedure Get_Src_Pos
     (Link : access Canvas_Link_Record; X, Y : out Glib.Gfloat) is
   begin
      X := Link.Src_X_Pos;
      Y := Link.Src_Y_Pos;
   end Get_Src_Pos;

   ------------------
   -- Get_Dest_Pos --
   ------------------

   procedure Get_Dest_Pos
     (Link : access Canvas_Link_Record; X, Y : out Glib.Gfloat) is
   begin
      X := Link.Dest_X_Pos;
      Y := Link.Dest_Y_Pos;
   end Get_Dest_Pos;

   ---------------------
   -- Get_Arrow_Angle --
   ---------------------

   function Get_Arrow_Angle
     (Canvas : access Interactive_Canvas_Record'Class) return Gdouble is
   begin
      return Canvas.Arrow_Angle;
   end Get_Arrow_Angle;

   ----------------------
   -- Get_Arrow_Length --
   ----------------------

   function Get_Arrow_Length
     (Canvas : access Interactive_Canvas_Record'Class) return Glib.Gint is
   begin
      return Canvas.Arrow_Length;
   end Get_Arrow_Length;

end Gtkada.Canvas;
