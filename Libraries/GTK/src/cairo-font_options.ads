-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                    Copyright (C) 2010-2013, AdaCore               --
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
--  A set of utilities to manipulate font options.
--  </description>
--
--  <c_version>1.8.8</c_version>
--  <group>Cairo</group>

package Cairo.Font_Options is

   function Create return Cairo_Font_Options;
   --  Allocates a new font options object with all options initialized
   --  to default values.
   --
   --  Return value: a newly allocated Cairo_Font_Options. Free with
   --  Cairo.Font_Options.Destroy. If memory cannot be allocated, then a
   --  special error object is returned where all operations on the object do
   --  nothing. You can check for this with Cairo.Font_Options.Status.

   function Copy
     (Original : Cairo_Font_Options)
      return     Cairo_Font_Options;
   --  Original: a Cairo_Font_Options
   --
   --  Allocates a new font options object copying the option values from
   --  original.
   --
   --  Return value: a newly allocated Cairo_Font_Options. Free with
   --  Cairo.Font_Options.Destroy. If memory cannot be allocated, then a
   --  special error object is returned where all operations on the object do
   --  nothing. You can check for this with Cairo.Font_Options.Status.

   procedure Destroy (Options : Cairo_Font_Options);
   --  Options: a Cairo_Font_Options
   --
   --  Destroys a Cairo_Font_Options object created with
   --  Cairo.Font_Options.Create or Cairo.Font_Options.Copy.

   function Status (Options : Cairo_Font_Options) return Cairo_Status;
   --  Options: a Cairo_Font_Options
   --
   --  Checks whether an error has previously occurred for this
   --  font options object
   --
   --  Return value: Cairo_Status_Success or Cairo_Status_No_Memory

   procedure Merge
     (Options : Cairo_Font_Options;
      Other   : Cairo_Font_Options);
   --  Options: a Cairo_Font_Options
   --  Other: anOther Cairo_Font_Options
   --
   --  Merges non-default options from other into options, replacing
   --  existing values. This operation can be thought of as somewhat
   --  similar to compositing other onto options with the operation
   --  of Cairo_Operation_Over.

   function Equal
     (Options : Cairo_Font_Options;
      Other   : Cairo_Font_Options)
      return    Cairo_Bool;
   --  Options: a Cairo_Font_Options
   --  Other: another Cairo_Font_Options
   --
   --  Compares two font options objects for equality.
   --
   --  Return value: True if all fields of the two font options objects match.
   --  Note that this function will return False if either object is in
   --  error.

   function Hash
     (Options : Cairo_Font_Options)
      return    Gulong;
   --  Options: a Cairo_Font_Options
   --
   --  Compute a hash for the font options object; this value will
   --  be useful when storing an object containing a Cairo_Font_Options
   --  in a hash table.
   --
   --  Return value: the hash value for the font options object.
   --  The return value can be cast to a 32-bit type if a
   --  32-bit hash value is needed.

   procedure Set_Antialias
     (Options   : Cairo_Font_Options;
      Antialias : Cairo_Antialias);
   --  Options: a Cairo_Font_Options
   --  Antialias: the new Antialiasing mode
   --
   --  Sets the antialiasing mode for the font options object. This
   --  specifies the type of antialiasing to do when rendering text.

   function Get_Antialias
     (Options : Cairo_Font_Options)
      return    Cairo_Antialias;
   --  Options: a Cairo_Font_Options
   --
   --  Gets the antialiasing mode for the font options object.
   --
   --  Return value: the antialiasing mode

   procedure Set_Subpixel_Order
     (Options        : Cairo_Font_Options;
      Subpixel_Order : Cairo_Subpixel_Order);
   --  Options: a Cairo_Font_Options
   --  Subpixel_Order: the new subpixel order
   --
   --  Sets the subpixel order for the font options object. The subpixel
   --  order specifies the order of color elements within each pixel on
   --  the display device when rendering with an antialiasing mode of
   --  Cairo_Antialias_Subpixel. See the documentation for
   --  Cairo_Subpixel_Order for full details.

   function Get_Subpixel_Order
     (Options : Cairo_Font_Options)
      return    Cairo_Subpixel_Order;
   --  Options: a Cairo_Font_Options
   --
   --  Gets the subpixel order for the font options object.
   --  See the documentation for Cairo_Subpixel_Order for full details.
   --
   --  Return value: the subpixel order for the font options object

   procedure Set_Hint_Style
     (Options    : Cairo_Font_Options;
      Hint_Style : Cairo_Hint_Style);
   --  Options: a Cairo_Font_Options
   --  Hint_Style: the new hint style
   --
   --  Sets the hint style for font outlines for the font options object.
   --  This controls whether to fit font outlines to the pixel grid,
   --  and if so, whether to optimize for fidelity or contrast.
   --  See the documentation for Cairo_Hint_Style for full details.

   function Get_Hint_Style
     (Options : Cairo_Font_Options)
      return    Cairo_Hint_Style;
   --  Options: a Cairo_Font_Options
   --
   --  Gets the hint style for font outlines for the font options object.
   --  See the documentation for Cairo_Hint_Style for full details.
   --
   --  Return value: the hint style for the font options object

   procedure Set_Hint_Metrics
     (Options      : Cairo_Font_Options;
      Hint_Metrics : Cairo_Hint_Metrics);
   --  Options: a Cairo_Font_Options
   --  Hint_Metrics: the new metrics hinting mode
   --
   --  Sets the metrics hinting mode for the font options object. This
   --  controls whether metrics are quantized to integer values in
   --  device units.
   --  See the documentation for Cairo_Hint_Metrics for full details.

   function Get_Hint_Metrics
     (Options : Cairo_Font_Options)
      return    Cairo_Hint_Metrics;
   --  Options: a Cairo_Font_Options
   --
   --  Gets the metrics hinting mode for the font options object.
   --  See the documentation for Cairo_Hint_Metrics for full details.
   --
   --  Return value: the metrics hinting mode for the font options object

private

   pragma Import (C, Create, "cairo_font_options_create");
   pragma Import (C, Copy, "cairo_font_options_copy");
   pragma Import (C, Destroy, "cairo_font_options_destroy");
   pragma Import (C, Status, "cairo_font_options_status");
   pragma Import (C, Merge, "cairo_font_options_merge");
   pragma Import (C, Equal, "cairo_font_options_equal");
   pragma Import (C, Hash, "cairo_font_options_hash");
   pragma Import (C, Set_Antialias, "cairo_font_options_set_antialias");
   pragma Import (C, Get_Antialias, "cairo_font_options_get_antialias");
   pragma Import
     (C,
      Set_Subpixel_Order,
      "cairo_font_options_set_subpixel_order");
   pragma Import
     (C,
      Get_Subpixel_Order,
      "cairo_font_options_get_subpixel_order");
   pragma Import (C, Set_Hint_Style, "cairo_font_options_set_hint_style");
   pragma Import (C, Get_Hint_Style, "cairo_font_options_get_hint_style");
   pragma Import (C, Set_Hint_Metrics, "cairo_font_options_set_hint_metrics");
   pragma Import (C, Get_Hint_Metrics, "cairo_font_options_get_hint_metrics");

end Cairo.Font_Options;
