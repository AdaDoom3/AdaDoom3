-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                    Copyright (C) 2011-2013, AdaCore               --
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

package body Cairo.Region is

   -----------
   -- Equal --
   -----------

   function "=" (A, B : Cairo_Region) return Boolean is
      function Internal (A, B : Cairo_Region) return Gboolean;
      pragma Import (C, Internal, "cairo_region_equal");
   begin
      return Internal (A, B) /= 0;
   end "=";

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Region : Cairo_Region) return Boolean is
      function Internal (Region : Cairo_Region) return Gboolean;
      pragma Import (C, Internal, "cairo_region_is_empty");
   begin
      return Internal (Region) /= 0;
   end Is_Empty;

   --------------------
   -- Contains_Point --
   --------------------

   function Contains_Point
     (Region : Cairo_Region;
      X      : Gint;
      Y      : Gint) return Boolean
   is
      function Internal (R : Cairo_Region; X, Y : Gint) return Gboolean;
      pragma Import (C, Internal, "cairo_region_contains_point");
   begin
      return Internal (Region, X, Y) /= 0;
   end Contains_Point;

end Cairo.Region;
