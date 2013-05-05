-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2002 ACT-Europe                 --
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
--
--  A graphic context is a structure that describes all the attributes
--  used by the drawing functions in Gdk.
--  The colors, line styles, Fill styles and so on are defined through
--  this structure.
--
--  On X11 systems, this structure is stored directly on the XServer,
--  which speeds up the transfer of the drawing attributes a lot. Instead
--  of transferring all of them every time you call one of the drawing
--  functions, you simply specify which GC you want to use.
--
--  Thus, it is recommended to create as many GCs as you need, instead
--  of creating a single one that is modified every time you need to
--  modify one of the attributes.
--
--  On Unix machines, you should have a look at the external utility 'xgc'
--  which demonstrates all the basic settings of the graphic contexts.
--
--  </description>
--  <c_version>1.3.6</c_version>
--  <group>Gdk, the low-level API</group>
--  <testgtk>create_gc.adb</testgtk>
--  <see>Gtk.GC</see>

with Glib; use Glib;
with Gdk.Color;
with Gdk.Font;
with Gdk.Rectangle;
with Gdk.Region;

package Gdk.GC is

   subtype Gdk_GC is Gdk.Gdk_GC;
   --  A graphic context that contain all the information to draw graphics
   --  on the screen.
   --  Creating these GC is more efficient than passing a lot of parameters
   --  to each of the drawing functions, since these GC are stored on the
   --  server side and do not need to be pass through the network.

   type Gdk_GC_Values is new Gdk.C_Proxy;
   --  A structure used on the client side to store the same information
   --  as the GC. Creating a GC from this structure is more efficient than
   --  calling a lot of functions to modify the GC directly, since there is
   --  a single call to the server.

   type Gdk_Cap_Style is (Cap_Not_Last, Cap_Butt, Cap_Round, Cap_Projecting);
   pragma Convention (C, Gdk_Cap_Style);

   type Gdk_Fill is (Solid, Tiled, Stippled, Opaque_Stippled);
   pragma Convention (C, Gdk_Fill);

   type Gdk_Function is
     (Copy,
      Invert,
      Gdk_Xor,
      Clear,
      Gdk_And,
      And_Reverse,
      And_Invert,
      Noop,
      Gdk_Or,
      Equiv,
      Or_Reverse,
      Copy_Invert,
      Or_Invert,
      Nand,
      Set);
   pragma Convention (C, Gdk_Function);

   type Gdk_Join_Style is (Join_Miter, Join_Round, Join_Bevel);
   pragma Convention (C, Gdk_Join_Style);

   type Gdk_Line_Style is (Line_Solid, Line_On_Off_Dash, Line_Double_Dash);
   pragma Convention (C, Gdk_Line_Style);

   type Gdk_Subwindow_Mode is (Clip_By_Children, Include_Inferiors);
   pragma Convention (C, Gdk_Subwindow_Mode);

   type Gdk_GC_Values_Mask is mod 2 ** 32;
   GC_Foreground    : constant Gdk_GC_Values_Mask := 2 ** 0;
   GC_Background    : constant Gdk_GC_Values_Mask := 2 ** 1;
   GC_Font          : constant Gdk_GC_Values_Mask := 2 ** 2;
   GC_Function      : constant Gdk_GC_Values_Mask := 2 ** 3;
   GC_Fill          : constant Gdk_GC_Values_Mask := 2 ** 4;
   GC_Tile          : constant Gdk_GC_Values_Mask := 2 ** 5;
   GC_Stipple       : constant Gdk_GC_Values_Mask := 2 ** 6;
   GC_Clip_Mask     : constant Gdk_GC_Values_Mask := 2 ** 7;
   GC_Subwindow     : constant Gdk_GC_Values_Mask := 2 ** 8;
   GC_Ts_X_Origin   : constant Gdk_GC_Values_Mask := 2 ** 9;
   GC_Tx_Y_Origin   : constant Gdk_GC_Values_Mask := 2 ** 10;
   GC_Clip_X_Origin : constant Gdk_GC_Values_Mask := 2 ** 11;
   GC_Clip_Y_Origin : constant Gdk_GC_Values_Mask := 2 ** 12;
   GC_Exposures     : constant Gdk_GC_Values_Mask := 2 ** 13;
   GC_Line_Width    : constant Gdk_GC_Values_Mask := 2 ** 14;
   GC_Line_Style    : constant Gdk_GC_Values_Mask := 2 ** 15;
   GC_Cap_Style     : constant Gdk_GC_Values_Mask := 2 ** 16;
   GC_Join_Style    : constant Gdk_GC_Values_Mask := 2 ** 17;

   Null_GC : constant Gdk_GC := null;
   Null_GC_Values : constant Gdk_GC_Values := null;

   ------------
   -- Gdk_GC --
   ------------

   procedure Gdk_New
     (GC       : out Gdk_GC;
      Drawable : Gdk.Gdk_Drawable);
   --  Create a new graphic context.
   --  The window must have been realized first (so that it is associated
   --  with some resources on the Xserver).
   --  The GC can then be used for any window that has the same root window,
   --  and same color depth as Window.
   --  See the manual page for XCreateGC on Unix systems for more information.

   procedure Gdk_New
     (GC          : out Gdk_GC;
      Drawable    : Gdk.Gdk_Drawable;
      Values      : Gdk_GC_Values;
      Values_Mask : Gdk_GC_Values_Mask);
   --  Create a new graphic context.
   --  It is directly created with the values set in Values, and whose
   --  associated field has been set in Values_Mask.
   --  This is faster than calling the simple Gdk_New function and each of
   --  other functions in this package, since each of them requires a call
   --  to the server.

   function Get_Type return Glib.GType;
   --  Return the internal value associated with Gdk_GC.

   procedure Destroy (GC : Gdk_GC);
   --  Free the memory allocated on the server for the graphic context.
   --  Graphic contexts are never freed automatically by GtkAda, this is
   --  the user responsibility to do so.
   --  This procedure is deprecated. Use Unref instead.

   procedure Ref (GC : Gdk_GC);
   --  Increment the reference counting for the graphic context.

   procedure Unref (GC : Gdk_GC);
   --  Decrement the reference counting for the graphic context.
   --  When this reaches 0, the graphic context is destroyed.

   procedure Get_Values (GC : Gdk_GC; Values : Gdk_GC_Values);
   --  Get the values set in the GC.
   --  This copies the values from the server to client, allowing faster
   --  modifications. Values can then be copied back to the server by
   --  creating a new graphic context with the function Gdk_New above.
   --  Values should have been allocated first with a call to Gdk_New.

   procedure Set_Values
     (GC     : Gdk_GC;
      Values : Gdk_GC_Values;
      Mask   : Gdk_GC_Values_Mask);
   --  Set the values in the GC.
   --  Mask indicates which values should be taken from Values and set in GC.

   procedure Set_Foreground (GC : Gdk_GC; Color : Gdk.Color.Gdk_Color);
   --  Set the foreground color for the graphic context.
   --  This color is the one that is used by most drawing functions.

   procedure Set_Background (GC : Gdk_GC; Color : Gdk.Color.Gdk_Color);
   --  Set the background color for the graphic context.

   procedure Set_Font (GC : Gdk_GC; Font : Gdk.Font.Gdk_Font);
   --  Set the font used by the graphic context.
   --  This font is used by the function Gdk.Drawable.Draw_Text.

   procedure Set_Function (GC : Gdk_GC; Func : Gdk_Function);
   --  Set the function in the graphic context.
   --  This function specifies how the points are put on the screen, ie
   --  if GtkAda how GtkAda should mix the point already on the screen
   --  and the new point being put.
   --  Note that setting the function to Gdk_Xor is not the right way
   --  to do animation. You should instead save the background pixmap,
   --  put the image, and then restore the background.
   --
   --  In general, there are three basic steps to drawing: reading the source
   --  pixels, reading the destination pixels, and writing the destination
   --  pixels.  Some functions only perform the third step (Set and Clear),
   --  some do not need the middle step (Copy), whereas most require the three
   --  steps, and thus can be much slower.

   procedure Set_Fill (GC : Gdk_GC; Fill : Gdk_Fill);
   --  Set the pattern used for filling the polygons.

   procedure Set_Tile (GC : Gdk_GC; Tile : Gdk.Gdk_Pixmap);

   procedure Set_Stipple (GC : Gdk_GC; Stipple : Gdk.Gdk_Pixmap);

   procedure Set_Ts_Origin (GC : Gdk_GC; X, Y : Gint);
   --  Set the Tile and Stipple origin in the graphic context.

   procedure Set_Clip_Origin (GC : Gdk_GC; X, Y : Gint);
   --  Set the origin of the clip mask.
   --  See the functions Set_Clip_Rectangle, Set_Clip_Region and
   --  Gdk.Bitmap.Set_Clip_Mask for more explanation.

   procedure Set_Clip_Mask (GC : Gdk.GC.Gdk_GC; Mask : Gdk.Gdk_Bitmap);
   --  If Mask is set to Null_Bitmap, then no clip_mask is used for drawing.
   --  Points will be drawn through this GC only where the bits are set to 1
   --  in the mask. See also the function Set_Clip_Origin for
   --  how to move the mask inside the GC.

   procedure Set_Clip_Rectangle
     (GC : Gdk_GC; Rectangle : Gdk.Rectangle.Gdk_Rectangle);
   procedure Set_Clip_Rectangle
     (GC : Gdk_GC; Rectangle : Gdk.Rectangle.Gdk_Rectangle_Access := null);
   --  Set the clip rectangle.
   --  Only the points that are drawn inside this rectangle will be displayed
   --  on the screen. The clip origin is modified automatically.
   --  See Set_Clip_Mask to delete the current clip mask.

   procedure Set_Clip_Region (GC : Gdk_GC; Region : Gdk.Region.Gdk_Region);
   --  Define a clip region on the screen.
   --  This is just like Set_Clip_Rectangle, except that a region is a more
   --  complex region, that can be the intersection or union of multiple
   --  rectangles. Note that the Clip_Origin can have an influence on this
   --  function.

   procedure Set_Subwindow (GC : Gdk_GC; Mode : Gdk_Subwindow_Mode);
   --  Set the subwindow mode for the graphic context.
   --  This specifies whether the drawing routines should be clipped to
   --  the specific window they are drawn into, or if they should extend
   --  to subwindows as well.

   procedure Set_Exposures (GC : Gdk_GC; Exposures : Boolean);
   --  Exposures indicates whether you want "expose" and "noexpose" events to
   --  be reported when calling Copy_Area and Copy_Plane with this GC.
   --  You should disable this if you don't need the event and want to optimize
   --  your application.
   --  If Exposures is True, then any call to Copy_Area or Draw_Pixmap will
   --  generate an expose event. Otherwise, these will generate a no_expose
   --  event.

   procedure Set_Line_Attributes
     (GC         : Gdk_GC;
      Line_Width : Gint;
      Line_Style : Gdk_Line_Style;
      Cap_Style  : Gdk_Cap_Style;
      Join_Style : Gdk_Join_Style);
   --  Set the line attributes for this GC.
   --  Line_Width is the width of the line. If its value is 0, the line is as
   --  thin as possible, possibly even more so than if the width is 1. It is
   --  also faster to draw a line with width 0 than any other line width.
   --
   --  Line_Style specifies whether the line should be solid or dashed.
   --  With Line_On_Off_Dash, the colors are alternatively the foreground
   --  color, and blank. With Line_Double_Dash, the colors are
   --  alternatively the foreground and background colors.
   --
   --  Cap_Style specifies how the line should end, either flat or rounded.
   --
   --  Join_Style specifies how two consecutive lines drawn by Draw_Lines are
   --  connected.

   procedure Set_Dashes
     (Gc          : Gdk_GC;
      Dash_Offset : Gint;
      Dash_List   : Guchar_Array);
   --  Specify the dash pattern when the line's style is anything but solid.
   --  The values in the array alternatively give the length (in pixels) of
   --  the plain dash, the empty dash, the second plain dash, ... None of
   --  these values can be 0. If there is an odd number of items in Dash_List,
   --  this is equivalent to giving the array concatenated with itself.
   --  Dash_Offset specifies the phase of the pattern to start with.

   procedure Copy (Dst_GC : Gdk_GC; Src_GC : Gdk_GC);
   --  Copy a Src_GC to Dst_GC.

   procedure Set_Colormap (Gc : Gdk_GC; Colormap : Gdk.Gdk_Colormap);

   function Get_Colormap (Gc : Gdk_GC) return Gdk.Gdk_Colormap;

   procedure Set_Rgb_Fg_Color (Gc : Gdk_GC; Color : Gdk.Color.Gdk_Color);

   procedure Set_Rgb_Bg_Color (Gc : Gdk_GC; Color : Gdk.Color.Gdk_Color);

   ----------------------
   -- Gdk_Color_Values --
   ----------------------

   function Gdk_New return Gdk_GC_Values;
   --  Allocate a new Values structure on the client.
   --  Note that this function allocates a C structure, and thus needs to
   --  be freed with a call to Free below.

   procedure Free (Values : Gdk_GC_Values);
   --  Free the C structure associated with Values.

   procedure Set_Foreground
     (Values : Gdk_GC_Values; Color : Gdk.Color.Gdk_Color);
   --  Same as Set_Foreground, but on the client side

   procedure Set_Background
     (Values : Gdk_GC_Values; Color : Gdk.Color.Gdk_Color);
   --  Same as Set_Background, but on the client side

   procedure Set_Font (Values : Gdk_GC_Values; Font : Gdk.Font.Gdk_Font);
   --  Same as Set_Font, but on the client side

   procedure Set_Function (Values : Gdk_GC_Values; Func : Gdk_Function);
   --  Same as Set_Function, but on the client side

   procedure Set_Fill (Values : Gdk_GC_Values; Fill : Gdk_Fill);
   --  Same as Set_Fill, but on the client side

   procedure Set_Ts_Origin
     (Values : Gdk_GC_Values;
      X, Y   : Gint);
   --  Same as Set_Ts_Origin, but on the client side

   procedure Set_Clip_Origin
     (Values : Gdk_GC_Values;
      X, Y   : Gint);
   --  Same as Set_Clip_Origin, but on the client side

   procedure Set_Subwindow
     (Values : Gdk_GC_Values;
      Mode   : Gdk_Subwindow_Mode);
   --  Same as Set_Subwindow, but on the client side

   procedure Set_Exposures (Values : Gdk_GC_Values; Exposures : Boolean);
   --  Same as Set_Exposures, but on the client side

   procedure Set_Line_Attributes
     (Values     : Gdk_GC_Values;
      Line_Width : Gint;
      Line_Style : Gdk_Line_Style;
      Cap_Style  : Gdk_Cap_Style;
      Join_Style : Gdk_Join_Style);
   --  Same as Set_Line_Attributes, but on the client side

private
   pragma Import (C, Get_Type, "gdk_gc_get_type");
   pragma Import (C, Copy, "gdk_gc_copy");
   pragma Import (C, Destroy, "gdk_gc_unref");
   pragma Import (C, Free, "ada_gdk_gc_free_values");
   pragma Import (C, Get_Values, "gdk_gc_get_values");
   pragma Import (C, Set_Values, "gdk_gc_set_values");
   pragma Import (C, Ref, "gdk_gc_ref");
   pragma Import (C, Unref, "gdk_gc_unref");
   pragma Import (C, Set_Clip_Rectangle, "gdk_gc_set_clip_rectangle");
   pragma Import (C, Set_Clip_Region, "gdk_gc_set_clip_region");
   pragma Import (C, Set_Stipple, "gdk_gc_set_stipple");
   pragma Import (C, Set_Tile, "gdk_gc_set_tile");
   pragma Import (C, Set_Clip_Mask, "gdk_gc_set_clip_mask");
   pragma Import (C, Set_Colormap, "gdk_gc_set_colormap");
   pragma Import (C, Get_Colormap, "gdk_gc_get_colormap");
   pragma Import (C, Set_Rgb_Fg_Color, "gdk_gc_set_rgb_fg_color");
   pragma Import (C, Set_Rgb_Bg_Color, "gdk_gc_set_rgb_bg_color");
end Gdk.GC;
