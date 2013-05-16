-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
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

with System;

package body Gdk.Pixmap is

   ----------------------
   -- Create_From_Data --
   ----------------------

   procedure Create_From_Data
     (Pixmap : out Gdk_Pixmap;
      Window : Gdk.Window.Gdk_Window;
      Data   : String;
      Width  : Gint;
      Height : Gint;
      Depth  : Gint;
      Fg     : Color.Gdk_Color;
      Bg     : Color.Gdk_Color)
   is
      function Internal
        (Window : Gdk.Window.Gdk_Window;
         Data   : String;
         Width  : Gint;
         Height : Gint;
         Depth  : Gint;
         Fg     : System.Address;
         Bg     : System.Address) return Gdk_Pixmap;
      pragma Import (C, Internal, "gdk_pixmap_create_from_data");

      use type Gdk.Color.Gdk_Color;

      Fg_Col : aliased Gdk.Color.Gdk_Color := Fg;
      Bg_Col : aliased Gdk.Color.Gdk_Color := Bg;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.

      Fg_A : System.Address := Fg_Col'Address;
      Bg_A : System.Address := Bg_Col'Address;

   begin
      if Fg = Gdk.Color.Null_Color then
         Fg_A := System.Null_Address;
      end if;

      if Bg = Gdk.Color.Null_Color then
         Bg_A := System.Null_Address;
      end if;

      Pixmap := Internal
        (Window, Data & ASCII.NUL,
         Width, Height, Depth, Fg_A, Bg_A);
   end Create_From_Data;

   ---------------------
   -- Create_From_Xpm --
   ---------------------

   procedure Create_From_Xpm
     (Pixmap      : out Gdk_Pixmap;
      Window      : Gdk.Window.Gdk_Window;
      Mask        : in out Gdk.Bitmap.Gdk_Bitmap;
      Transparent : Gdk.Color.Gdk_Color;
      Filename    : String)
   is
      function Internal
        (Window      : Gdk.Window.Gdk_Window;
         Mask        : System.Address;
         Transparent : System.Address;
         Filename    : String) return Gdk_Pixmap;
      pragma Import (C, Internal, "gdk_pixmap_create_from_xpm");

      use type Gdk.Color.Gdk_Color;

      Tmp : aliased Gdk.Bitmap.Gdk_Bitmap := Mask;

      Transp_Col : aliased Gdk.Color.Gdk_Color := Transparent;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.

      Transparent_A : System.Address := Transp_Col'Address;

   begin
      if Transparent = Gdk.Color.Null_Color then
         Transparent_A := System.Null_Address;
      end if;

      Pixmap :=
        Internal
          (Window, Tmp'Address, Transparent_A, Filename & ASCII.NUL);
      Mask := Tmp;
   end Create_From_Xpm;

   ---------------------
   -- Create_From_Xpm --
   ---------------------

   procedure Create_From_Xpm
     (Pixmap      : out Gdk_Pixmap;
      Window      : Gdk.Window.Gdk_Window;
      Colormap    : Gdk.Color.Gdk_Colormap;
      Mask        : in out Gdk.Bitmap.Gdk_Bitmap;
      Transparent : Gdk.Color.Gdk_Color;
      Filename    : String)
   is
      function Internal
        (Window      : Gdk.Window.Gdk_Window;
         Colormap    : Gdk.Color.Gdk_Colormap;
         Mask        : System.Address;
         Transparent : System.Address;
         Filename    : String) return Gdk_Pixmap;
      pragma Import (C, Internal, "gdk_pixmap_colormap_create_from_xpm");

      use type Gdk.Color.Gdk_Color;

      Tmp : aliased Gdk.Bitmap.Gdk_Bitmap := Mask;

      Transp_Col : aliased Gdk.Color.Gdk_Color := Transparent;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.

      Transparent_A : System.Address := Transp_Col'Address;

   begin
      if Transparent = Gdk.Color.Null_Color then
         Transparent_A := System.Null_Address;
      end if;

      Pixmap :=
        Internal
          (Window => Window,
           Colormap => Colormap,
           Mask => Tmp'Address,
           Transparent => Transparent_A,
           Filename => Filename & ASCII.NUL);
      Mask := Tmp;
   end Create_From_Xpm;

   -----------------------
   -- Create_From_Xpm_D --
   -----------------------

   procedure Create_From_Xpm_D
     (Pixmap      : out Gdk_Pixmap;
      Window      : Gdk.Window.Gdk_Window;
      Mask        : in out Gdk.Bitmap.Gdk_Bitmap;
      Transparent : Gdk.Color.Gdk_Color;
      Data        : Gtkada.Types.Chars_Ptr_Array)
   is
      function Internal
        (Window      : Gdk.Window.Gdk_Window;
         Mask        : System.Address;
         Transparent : System.Address;
         Data        : Gtkada.Types.Chars_Ptr_Array) return Gdk_Pixmap;
      pragma Import (C, Internal, "gdk_pixmap_create_from_xpm_d");

      use type Gdk.Color.Gdk_Color;

      Tmp : aliased Gdk.Bitmap.Gdk_Bitmap := Mask;
      Transp_Col : aliased Gdk.Color.Gdk_Color := Transparent;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.

      Transparent_A : System.Address := Transp_Col'Address;

   begin
      if Transparent = Gdk.Color.Null_Color then
         Transparent_A := System.Null_Address;
      end if;

      Pixmap := Internal (Window, Tmp'Address, Transparent_A, Data);
      Mask := Tmp;
   end Create_From_Xpm_D;

   -----------------------
   -- Create_From_Xpm_D --
   -----------------------

   procedure Create_From_Xpm_D
     (Pixmap      : out Gdk_Pixmap;
      Window      : Gdk.Window.Gdk_Window;
      Colormap    : Gdk.Color.Gdk_Colormap;
      Mask        : in out Gdk.Bitmap.Gdk_Bitmap;
      Transparent : Gdk.Color.Gdk_Color;
      Data        : Gtkada.Types.Chars_Ptr_Array)
   is
      function Internal
        (Window      : Gdk.Window.Gdk_Window;
         Colormap    : Gdk.Color.Gdk_Colormap;
         Mask        : System.Address;
         Transparent : System.Address;
         Data        : Gtkada.Types.Chars_Ptr_Array) return Gdk_Pixmap;
      pragma Import (C, Internal, "gdk_pixmap_colormap_create_from_xpm_d");

      use type Gdk.Color.Gdk_Color;

      Tmp : aliased Gdk.Bitmap.Gdk_Bitmap := Mask;
      Transp_Col : aliased Gdk.Color.Gdk_Color := Transparent;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.

      Transparent_A : System.Address := Transp_Col'Address;

   begin
      if Transparent = Gdk.Color.Null_Color then
         Transparent_A := System.Null_Address;
      end if;

      Pixmap := Internal (Window, Colormap, Tmp'Address, Transparent_A, Data);
      Mask := Tmp;
   end Create_From_Xpm_D;

   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New
     (Pixmap : out Gdk_Pixmap;
      Window : Gdk.Window.Gdk_Window;
      Width  : Gint;
      Height : Gint;
      Depth  : Gint := -1)
   is
      function Internal
        (Window : Gdk.Window.Gdk_Window;
         Width  : Gint;
         Height : Gint;
         Depth  : Gint) return Gdk_Pixmap;
      pragma Import (C, Internal, "gdk_pixmap_new");

   begin
      Pixmap := Internal (Window, Width, Height, Depth);
   end Gdk_New;

end Gdk.Pixmap;
