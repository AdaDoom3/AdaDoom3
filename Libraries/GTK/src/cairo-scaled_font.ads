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
--  Utilities for manipulating font faces.
--  </description>
--
--  <c_version>1.8.8</c_version>
--  <group>Cairo</group>

with Interfaces.C.Strings;

package Cairo.Scaled_Font is

   function Reference
     (Scaled_Font : Cairo_Scaled_Font)
      return        Cairo_Scaled_Font;
   --  Scaled_Font: a Cairo.Scaled_Font.T, (may be null in which case
   --  this function does nothing)
   --
   --  Increases the reference count on scaled_font by one. This prevents
   --  scaled_font from being destroyed until a matching call to
   --  Cairo.Scaled_Font.Destroy is made.
   --
   --  The number of references to a Cairo_Scaled_Font can be get using
   --  Cairo.Scaled_Font.Get_Reference_Count.
   --
   --  Returns: the referenced Cairo_Scaled_Font

   procedure Destroy (Scaled_Font : Cairo_Scaled_Font);
   --  Scaled_Font: a Cairo_Scaled_Font
   --
   --  Decreases the reference count on font by one. If the result
   --  is zero, then font and all associated resources are freed.
   --  See Cairo.Scaled_Font.Reference.

   function Get_Reference_Count
     (Scaled_Font : Cairo_Scaled_Font)
      return        Guint;
   --  Scaled_Font: a Cairo_Scaled_Font
   --
   --  Returns the current reference count of scaled_font.
   --
   --  Return value: the current reference count of scaled_font.  If the
   --  object is a nil object, 0 will be returned.
   --
   --  Since: 1.4

   function Status (Scaled_Font : Cairo_Scaled_Font) return Cairo_Status;
   --  Scaled_Font: a Cairo_Scaled_Font
   --
   --  Checks whether an error has previously occurred for this
   --  scaled_font.
   --
   --  Return value: Cairo_Status_Success or another error such as
   --    Cairo_Status_No_Memory.

   function Get_Type
     (Scaled_Font : Cairo_Scaled_Font)
      return        Cairo_Font_Type;
   --  Scaled_Font: a Cairo_Scaled_Font
   --
   --  This function returns the type of the backend used to create
   --  a scaled font. See Cairo_Font_Type for available types.
   --
   --  Return value: The type of scaled_font.
   --
   --  Since: 1.2

   procedure Extents
     (Scaled_Font : Cairo_Scaled_Font;
      Extents     : access Cairo_Font_Extents);
   --  Scaled_Font: a Cairo_Scaled_Font
   --  Extents: a Cairo_Font_Extents which to store the retrieved Extents.
   --
   --  Gets the metrics for a Cairo_Scaled_Font.

   procedure Text_Extents
     (Scaled_Font : Cairo_Scaled_Font;
      Utf8        : Interfaces.C.Strings.chars_ptr;
      Extents     : Cairo_Text_Extents);
   --  Scaled_Font: a Cairo_Scaled_Font
   --  Utf8: a NUL-terminated string of text, encoded in UTF-8
   --  Extents: a Cairo_Text_Extents which to store the retrieved Extents.
   --
   --  Gets the extents for a string of text. The extents describe a
   --  user-space rectangle that encloses the "inked" portion of the text
   --  drawn at the origin (0,0) (as it would be drawn by Cairo_Show_Text
   --  if the cairo graphics state were set to the same font_face,
   --  font_matrix, ctm, and font_options as scaled_font).  Additionally,
   --  the x_advance and y_advance values indicate the amount by which the
   --  current point would be advanced by Cairo_Show_Text.
   --
   --  Note that whitespace characters do not directly contribute to the
   --  size of the rectangle (extents.width and extents.height). They do
   --  contribute indirectly by changing the position of non-whitespace
   --  characters. In particular, trailing whitespace characters are
   --  likely to not affect the size of the rectangle, though they will
   --  affect the x_advance and y_advance values.
   --
   --  Since: 1.2

   procedure Glyph_Extents
     (Scaled_Font : Cairo_Scaled_Font;
      Glyphs      : access Cairo_Glyph;
      Num_Glyphs  : Gint;
      Extents     : Cairo_Text_Extents);
   --  Scaled_Font: a Cairo_Scaled_Font
   --  Glyphs: an array of glyph IDs with X and Y offsets.
   --  Num_Glyphs: the number of glyphs in the glyphs array
   --  Extents: a Cairo_Text_Extents which to store the retrieved Extents.
   --
   --  Gets the extents for an array of glyphs. The extents describe a
   --  user-space rectangle that encloses the "inked" portion of the
   --  glyphs, (as they would be drawn by Cairo.Show_Glyphs if the cairo
   --  graphics state were set to the same Font_Face, Font_Matrix, Ctm,
   --  and Font_Options as Scaled_Font).  Additionally, the x_advance and
   --  Y_Advance values indicate the amount by which the current point
   --  would be advanced by Cairo.Show_Glyphs.
   --
   --  Note that whitespace glyphs do not contribute to the size of the
   --  rectangle (extents.width and extents.height).

   function Get_Font_Face
     (Scaled_Font : Cairo_Scaled_Font)
      return        Cairo_Font_Face;
   --  Scaled_Font: a Cairo_Scaled_Font
   --
   --  Gets the font face that this scaled font was created for.
   --
   --  Return value: The Cairo_Font_Face with which Scaled_Font was
   --  created.
   --
   --  Since: 1.2

   procedure Get_Font_Matrix
     (Scaled_Font : Cairo_Scaled_Font;
      Font_Matrix : access Cairo_Matrix);
   --  Scaled_Font: a Cairo_Scaled_Font
   --  Font_Matrix: return value for the matrix
   --
   --  Stores the font matrix with which Scaled_Font was created into
   --  matrix.
   --
   --  Since: 1.2

   procedure Get_Ctm
     (Scaled_Font : Cairo_Scaled_Font;
      Ctm         : access Cairo_Matrix);
   --  Scaled_Font: a Cairo_Scaled_Font
   --  Ctm: return value for the CTM
   --
   --  Stores the CTM with which Scaled_Font was created into Ctm.
   --
   --  Since: 1.2

   procedure Get_Scale_Matrix
     (Scaled_Font  : Cairo_Scaled_Font;
      Scale_Matrix : access Cairo_Matrix);
   --  Scaled_Font: a Cairo_Scaled_Font
   --  Scale_Matrix: return value for the matrix
   --
   --  Stores the scale matrix of scaled_font into matrix.
   --  The scale matrix is product of the font matrix and the ctm
   --  associated with the scaled font, and hence is the matrix mapping from
   --  font space to device space.
   --
   --  Since: 1.8

   procedure Get_Font_Options
     (Scaled_Font : Cairo_Scaled_Font;
      Options     : Cairo_Font_Options);
   --  Scaled_Font: a Cairo_Scaled_Font
   --  Options: return value for the font Options
   --
   --  Stores the font options with which scaled_font was created into
   --  options.
   --
   --  Since: 1.2

private

   pragma Import (C, Reference, "cairo_scaled_font_reference");
   pragma Import (C, Destroy, "cairo_scaled_font_destroy");
   pragma Import
     (C,
      Get_Reference_Count,
      "cairo_scaled_font_get_reference_count");
   pragma Import (C, Status, "cairo_scaled_font_status");
   pragma Import (C, Get_Type, "cairo_scaled_font_get_type");
   pragma Import (C, Extents, "cairo_scaled_font_extents");
   pragma Import (C, Text_Extents, "cairo_scaled_font_text_extents");
   pragma Import (C, Glyph_Extents, "cairo_scaled_font_glyph_extents");
   pragma Import (C, Get_Font_Face, "cairo_scaled_font_get_font_face");
   pragma Import (C, Get_Font_Matrix, "cairo_scaled_font_get_font_matrix");
   pragma Import (C, Get_Ctm, "cairo_scaled_font_get_ctm");
   pragma Import (C, Get_Scale_Matrix, "cairo_scaled_font_get_scale_matrix");
   pragma Import (C, Get_Font_Options, "cairo_scaled_font_get_font_options");

   --  Not bound :
--     pragma Import (C, Text_To_Glyphs, "cairo_scaled_font_text_to_glyphs");
--     pragma Import (C, Create, "cairo_scaled_font_create");
--     pragma Import (C, Get_User_Data, "cairo_scaled_font_get_user_data");
--     pragma Import (C, Set_User_Data, "cairo_scaled_font_set_user_data");
end Cairo.Scaled_Font;
