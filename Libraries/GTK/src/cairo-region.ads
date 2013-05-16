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

--  Regions -- Representing a pixel-aligned area
--  <description>
--  Bindings to the Cairo 2D graphics library.
--  Regions are a simple graphical data type representing an area of integer-
--  aligned rectangles. Thay are often used on raster surfaces to track areas
--  of interest, such as change or clip areas.
--  </description>
--
--  <c_version>1.10</c_version>
--  <group>Cairo</group>

package Cairo.Region is

   type Cairo_Region is private;
   --  A Cairo_Region represents a set of integer-aligned rectangles.
   --
   --  It allows set-theoretical operations like Union and Intersect to be
   --  performed on them.
   --
   --  Memory management of Cairo_Region is done with Reference and
   --  Destroy.
   --
   --  Since: 1.10

   Null_Region : constant Cairo_Region;

   type Cairo_Rectangle_Int is record
      X, Y, Width, Height : aliased Gint;
   end record;

   type Cairo_Region_Overlap is
     (Cairo_Region_Overlap_In,    --  Completely inside region
      Cairo_Region_Overlap_Out,   --  Completely outside region
      Cairo_Region_Overlap_Part   --  Partly inside region
     );
   --  Used as the return value for Contains_Rectangle.
   pragma Convention (C, Cairo_Region_Overlap);

   function Create return Cairo_Region;
   --  Allocates a new empty region object.
   --
   --  Returns: A newly allocated Cairo_Region. Free with Destroy. This
   --  function always returns a valid Cairo_Region; if memory cannot be
   --  allocated, then a special error object is returned where all operations
   --  on the object do nothing. You can check for this with Status.

   function Create_Rectangle
     (Rectangle : access Cairo_Rectangle_Int) return Cairo_Region;
   --  Allocates a new region object containing Rectangle.
   --
   --  Returns: A newly allocated Cairo_Region. Free with Destroy. This
   --  function always returns a valid Cairo_Region; if memory cannot be
   --  allocated, then a special error object is returned where all operations
   --  on the object do nothing. You can check for this with Status.

   function Copy (Original : Cairo_Region) return Cairo_Region;
   --  Allocates a new Cairo_Region object copying the area from Original.
   --
   --  Returns: A newly allocated Cairo_Region. Free with Destroy. This
   --  function always returns a valid Cairo_Region; if memory cannot be
   --  allocated, then a special error object is returned where all operations
   --  on the object do nothing. You can check for this with Status.

   function Reference (Region : Cairo_Region) return Cairo_Region;
   --  Increases the reference count on Region by one. This prefents Region
   --  from being destroyed until a matching call to Destroy is made.

   procedure Destroy (Region : Cairo_Region);
   --  Destroys a Cairo_Region object created with Create, Copy or
   --  Create_Rectangle.

   function "=" (A, B : Cairo_Region) return Boolean;
   --  Compares whether A is equivalent to B. Null_Region as an argument is
   --  equal to itself, but not to any non-Null_Region region.

   function Status (Region : Cairo_Region) return Cairo_Status;
   --  Checks whether an error has occured for this region object.
   --
   --  Returns: Cairo_Status_Success or Cairo_Status_No_Memory

   procedure Get_Extents
     (Region  : Cairo_Region;
      Extents : out Cairo_Rectangle_Int);
   --  Gets the bounding rectangle of Region as a Cairo_Rectangle_Int

   function Num_Rectangles (Region : Cairo_Region) return Gint;
   --  Returns the number of rectangle contained in Region

   procedure Get_Rectangle
     (Region    : Cairo_Region;
      Nth       : Gint;
      Rectangle : out Cairo_Rectangle_Int);
   --  Stores the Nth rectangle from the region in Rectangle.

   function Is_Empty (Region : Cairo_Region) return Boolean;
   --  Checks whether Region is empty.

   function Contains_Rectangle
     (Region    : Cairo_Region;
      Rectangle : access Cairo_Rectangle_Int) return Cairo_Region_Overlap;
   --  Checks whether Rectangle is inside, outside or partially contained in
   --  Region

   function Contains_Point
     (Region : Cairo_Region;
      X      : Gint;
      Y      : Gint) return Boolean;
   --  Checks whether (X,Y) is contained in Region.

   procedure Translate
     (Region : Cairo_Region;
      dX     : Gint;
      dY     : Gint);
   --  Translates Region by (dX,dY).

   function Subtract
     (Dst   : Cairo_Region;
      Other : Cairo_Region) return Cairo_Status;
   --  Subtracts Other from Dst and places the result in Dst.
   --
   --  Returns: Cairo_Status_Success or Cairo_Status_No_Memory.

   function Subtract_Rectangle
     (Dst       : Cairo_Region;
      Rectangle : access Cairo_Rectangle_Int) return Cairo_Status;
   --  Subtracts Rectangle from Dst and places the result in Dst.
   --
   --  Returns: Cairo_Status_Success or Cairo_Status_No_Memory.

   function Intersect
     (Dst   : Cairo_Region;
      Other : Cairo_Region) return Cairo_Status;
   --  Computes the intersection of Dst with Other and places the result in Dst
   --
   --  Returns: Cairo_Status_Success or Cairo_Status_No_Memory.

   function Intersect_Rectangle
     (Dst       : Cairo_Region;
      Rectangle : access Cairo_Rectangle_Int) return Cairo_Status;
   --  Computes the intersection of Dst with Rectangle and places the result in
   --  Dst.
   --
   --  Returns: Cairo_Status_Success or Cairo_Status_No_Memory.

   function Union
     (Dst   : Cairo_Region;
      Other : Cairo_Region) return Cairo_Status;
   --  Computes the union of Dst with Other and places the result in Dst.
   --
   --  Returns: Cairo_Status_Success or Cairo_Status_No_Memory.

   function Union_Rectangle
     (Dst       : Cairo_Region;
      Rectangle : access Cairo_Rectangle_Int) return Cairo_Status;
   --  Computes the union of Dst with Rectangle and places the result in Dst.
   --
   --  Returns: Cairo_Status_Success or Cairo_Status_No_Memory.

   function Do_Xor
     (Dst   : Cairo_Region;
      Other : Cairo_Region) return Cairo_Status;
   --  Computes the exclusive difference of Dst with Other and places the
   --  result in Dst.
   --
   --  Returns: Cairo_Status_Success or Cairo_Status_No_Memory.

   function Xor_Rectangle
     (Dst       : Cairo_Region;
      Rectangle : access Cairo_Rectangle_Int) return Cairo_Status;
   --  Computes the exclusive difference of Dst with Rectangle and places the
   --  result in Dst.
   --
   --  Returns: Cairo_Status_Success or Cairo_Status_No_Memory.

private

   pragma Convention (C, Cairo_Rectangle_Int);

   type Cairo_Region is new System.Address;
   Null_Region : constant Cairo_Region := Cairo_Region (System.Null_Address);

   pragma Import (C, Create, "cairo_region_create");
   pragma Import (C, Create_Rectangle, "cairo_region_create_rectangle");
   pragma Import (C, Copy, "cairo_region_copy");
   pragma Import (C, Reference, "cairo_region_reference");
   pragma Import (C, Destroy, "cairo_region_destroy");
   pragma Import (C, Status, "cairo_region_status");
   pragma Import (C, Get_Extents, "cairo_region_get_extents");
   pragma Import (C, Num_Rectangles, "cairo_region_num_rectangles");
   pragma Import (C, Get_Rectangle, "cairo_region_get_rectangle");
   pragma Import (C, Contains_Rectangle, "cairo_region_contains_rectangle");
   pragma Import (C, Translate, "cairo_region_translate");
   pragma Import (C, Subtract, "cairo_region_subtract");
   pragma Import (C, Subtract_Rectangle, "cairo_region_subtract_rectangle");
   pragma Import (C, Intersect, "cairo_region_intersect");
   pragma Import (C, Intersect_Rectangle, "cairo_region_intersect_rectangle");
   pragma Import (C, Union, "cairo_region_union");
   pragma Import (C, Union_Rectangle, "cairo_region_union_rectangle");
   pragma Import (C, Do_Xor, "cairo_region_xor");
   pragma Import (C, Xor_Rectangle, "cairo_region_xor_rectangle");
   pragma Inline ("=");
   pragma Inline (Is_Empty);

end Cairo.Region;
