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

package body Gdk.Rectangle is

   ---------------
   -- Intersect --
   ---------------

   procedure Intersect
     (Src1      : Gdk_Rectangle;
      Src2      : Gdk_Rectangle;
      Dest      : out Gdk_Rectangle;
      Intersect : out Boolean)
   is
      function Internal
        (Src1, Src2 : Gdk_Rectangle;
         Dest       : System.Address) return Gint;
      pragma Import (C, Internal, "gdk_rectangle_intersect");

      Dest_Rec : aliased Gdk_Rectangle;

   begin
      Intersect := Internal (Src1, Src2, Dest_Rec'Address) /= 0;
      Dest := Dest_Rec;
   end Intersect;

end Gdk.Rectangle;
