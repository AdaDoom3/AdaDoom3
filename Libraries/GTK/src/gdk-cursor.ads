-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2007 AdaCore                    --
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
--  This package provides the capability to create predefined mouse cursors
--  as well as user defined ones.
--  </description>
--  <c_version>1.3.6</c_version>
--  <group>Gdk, the low-level API</group>
--  <testgtk>create_cursors.adb</testgtk>

with Glib;
with Gdk.Color;

package Gdk.Cursor is

   type Gdk_Cursor is new Gdk.C_Proxy;
   Null_Cursor : constant Gdk_Cursor;

   type Gdk_Cursor_Type is
     (X_Cursor,
      Arrow,
      Based_Arrow_Down,
      Based_Arrow_Up,
      Boat,
      Bogosity,
      Bottom_Left_Corner,
      Bottom_Right_Corner,
      Bottom_Side,
      Bottom_Tee,
      Box_Spiral,
      Center_Ptr,
      Circle,
      Clock,
      Coffee_Mug,
      Cross,
      Cross_Reverse,
      Crosshair,
      Diamond_Cross,
      Dot,
      Dotbox,
      Double_Arrow,
      Draft_Large,
      Draft_Small,
      Draped_Box,
      Exchange,
      Fleur,
      Gobbler,
      Gumby,
      Hand1,
      Hand2,
      Heart,
      Icon,
      Iron_Cross,
      Left_Ptr,
      Left_Side,
      Left_Tee,
      Leftbutton,
      Ll_Angle,
      Lr_Angle,
      Man,
      Middlebutton,
      Mouse,
      Pencil,
      Pirate,
      Plus,
      Question_Arrow,
      Right_Ptr,
      Right_Side,
      Right_Tee,
      Rightbutton,
      Rtl_Logo,
      Sailboat,
      Sb_Down_Arrow,
      Sb_H_Double_Arrow,
      Sb_Left_Arrow,
      Sb_Right_Arrow,
      Sb_Up_Arrow,
      Sb_V_Double_Arrow,
      Shuttle,
      Sizing,
      Spider,
      Spraycan,
      Star,
      Target,
      Tcross,
      Top_Left_Arrow,
      Top_Left_Corner,
      Top_Right_Corner,
      Top_Side,
      Top_Tee,
      Trek,
      Ul_Angle,
      Umbrella,
      Ur_Angle,
      Watch,
      Xterm);

   procedure Gdk_New
     (Widget      : out Gdk_Cursor;
      Cursor_Type : Gdk_Cursor_Type);
   --  Create a new standard cursor.

   procedure Gdk_New
     (Widget : out Gdk_Cursor;
      Source : Gdk.Gdk_Pixmap;
      Mask   : Gdk.Gdk_Pixmap;
      Fg     : Gdk.Color.Gdk_Color;
      Bg     : Gdk.Color.Gdk_Color;
      X      : Glib.Gint;
      Y      : Glib.Gint);
   --  Create a new cursor from a given pixmap and mask.
   --  See also Gdk.Pixbuf.Gdk_New_From_Pixbuf.
   --  Both the pixmap and mask must have a depth of 1 (i.e. each pixel has
   --  only 2 values - on or off). The standard cursor size is 16 by 16 pixels.
   --   - Source is the pixmap specifying the cursor.
   --   - Mask is the pixmap specifying the mask, which must be the same size
   --   as source.
   --   - Fg is the foreground color, used for the bits in the source which are
   --   enabled. The color does not have to be allocated first.
   --   - Bg is the background color, used for the bits in the source which are
   --   disabled. The color does not have to be allocated first.
   --   - X is the horizontal offset of the 'hotspot' of the cursor.
   --   - Y is the vertical offset of the 'hotspot' of the cursor.

   --  procedure Gdk_New_From_Pixbuf (...)
   --  This function is declared in Gdk.Pixbuf, for dependency circularity
   --  reasons. It can be used to create a cursor directly from a pixbuf.

   procedure Gdk_New
     (Cursor  : out Gdk_Cursor;
      Name    : String);
   --  Create a cursor from a name

   procedure Destroy (Cursor : Gdk_Cursor);
   pragma Obsolescent;  --  Destroy
   --  Destroy a cursor, freeing any resources allocated for it.
   --  Deprecated, use Unref instead.

   procedure Ref (Cursor : Gdk_Cursor);
   --  Increment the reference counting for the cursor.

   procedure Unref (Cursor : Gdk_Cursor);
   --  Decrement the reference counting for the cursor.
   --  When this reaches 0, the cursor is destroyed.

private
   Null_Cursor : constant Gdk_Cursor := null;
   pragma Import (C, Destroy, "gdk_cursor_unref");
   pragma Import (C, Ref, "gdk_cursor_ref");
   pragma Import (C, Unref, "gdk_cursor_unref");

   for Gdk_Cursor_Type'Size use Glib.Gint'Size;
   for Gdk_Cursor_Type use
     (X_Cursor => 0,
      Arrow    => 2,
      Based_Arrow_Down => 4,
      Based_Arrow_Up => 6,
      Boat => 8,
      Bogosity => 10,
      Bottom_Left_Corner => 12,
      Bottom_Right_Corner => 14,
      Bottom_Side => 16,
      Bottom_Tee => 18,
      Box_Spiral => 20,
      Center_Ptr => 22,
      Circle => 24,
      Clock => 26,
      Coffee_Mug => 28,
      Cross => 30,
      Cross_Reverse => 32,
      Crosshair => 34,
      Diamond_Cross => 36,
      Dot => 38,
      Dotbox => 40,
      Double_Arrow => 42,
      Draft_Large => 44,
      Draft_Small => 46,
      Draped_Box => 48,
      Exchange => 50,
      Fleur => 52,
      Gobbler => 54,
      Gumby => 56,
      Hand1 => 58,
      Hand2 => 60,
      Heart => 62,
      Icon => 64,
      Iron_Cross => 66,
      Left_Ptr => 68,
      Left_Side => 70,
      Left_Tee => 72,
      Leftbutton => 74,
      Ll_Angle => 76,
      Lr_Angle => 78,
      Man => 80,
      Middlebutton => 82,
      Mouse => 84,
      Pencil => 86,
      Pirate => 88,
      Plus => 90,
      Question_Arrow => 92,
      Right_Ptr => 94,
      Right_Side => 96,
      Right_Tee => 98,
      Rightbutton => 100,
      Rtl_Logo => 102,
      Sailboat => 104,
      Sb_Down_Arrow => 106,
      Sb_H_Double_Arrow => 108,
      Sb_Left_Arrow => 110,
      Sb_Right_Arrow => 112,
      Sb_Up_Arrow => 114,
      Sb_V_Double_Arrow => 116,
      Shuttle => 118,
      Sizing => 120,
      Spider => 122,
      Spraycan => 124,
      Star => 126,
      Target => 128,
      Tcross => 130,
      Top_Left_Arrow => 132,
      Top_Left_Corner => 134,
      Top_Right_Corner => 136,
      Top_Side => 138,
      Top_Tee => 140,
      Trek => 142,
      Ul_Angle => 144,
      Umbrella => 146,
      Ur_Angle => 148,
      Watch => 150,
      Xterm => 152);

end Gdk.Cursor;
