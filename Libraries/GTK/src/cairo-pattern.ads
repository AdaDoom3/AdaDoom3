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
--  A Cairo_Pattern is the paintbrush with which cairo draws. The primary use
--  of patterns is as the source for all cairo drawing operations.
--
--  A cairo pattern is created by using one of the many constructors, of the
--  form Cairo_Pattern.Create_<type> or implicitly through
--  Cairo.Set_Source_<type> subprograms.
--  </description>
--
--  <c_version>1.8.8</c_version>
--  <group>Cairo</group>

with System;

package Cairo.Pattern is

   --------------------------------
   -- Pattern creation functions --
   --------------------------------

   --  Cairo_Pattern_Type is used to describe the type of a given pattern.
   --
   --  The type of a pattern is determined by the function used to create
   --  it. The Cairo.Pattern.Create_Rgb and Cairo.Pattern.Create_Rgba
   --  functions create Solid patterns. The remaining
   --  Cairo.Pattern.Create_<> functions map to pattern types in obvious
   --  ways.
   --
   --  The pattern type can be queried with Cairo.Pattern.Get_Type
   --
   --  Most Cairo_Pattern functions can be called with a pattern of any type,
   --  (though trying to change the extend or filter for a solid pattern will
   --  have no effect). A notable exception is Cairo.Pattern.Add_Color_Stop_Rgb
   --  and Cairo.Pattern.Add_Color_Stop_Rgba which must only be called with
   --  gradient patterns (either Linear or Radial). Otherwise the pattern will
   --  be shutdown and put into an error state.
   --
   --  New entries may be added in future versions.
   --
   --  Since: 1.2
   type Cairo_Pattern_Type is
     (Cairo_Pattern_Type_Solid,
      --  The pattern is a solid (uniform) color. It may be opaque or
      --  translucent.

      Cairo_Pattern_Type_Surface,
      --  The pattern is a based on a surface (an image).

      Cairo_Pattern_Type_Linear,
      --  The pattern is a linear gradient.

      Cairo_Pattern_Type_Radial
      --  The pattern is a radial gradient.
     );
   pragma Convention (C, Cairo_Pattern_Type);

   --  Cairo_extend is used to describe how pattern color/alpha will be
   --  determined for areas "outside" the pattern's natural area, (for
   --  example, outside the surface bounds or outside the gradient
   --  geometry).
   --
   --  The default extend mode is CAIRO_EXTEND_NONE for surface patterns
   --  and CAIRO_EXTEND_PAD for gradient patterns.
   --
   --  New entries may be added in future versions.
   type Cairo_Extend is
     (Cairo_Extend_None,
      --  Pixels outside of the source pattern are fully transparent

      Cairo_Extend_Repeat,
      --  The pattern is tiled by repeating

      Cairo_Extend_Reflect,
      --  The pattern is tiled by reflecting at the edges (Implemented for
      --  surface patterns since 1.6)

      Cairo_Extend_Pad
      --  Pixels outside of the pattern copy
      --  the closest pixel from the source (Since 1.2; but only
      --  implemented for surface patterns since 1.6)
     );
   pragma Convention (C, Cairo_Extend);

   --  Cairo_filter is used to indicate what filtering should be
   --  applied when reading pixel values from patterns. See
   --  Cairo.Pattern.Set_Source for indicating the desired filter to be
   --  used with a particular pattern.
   type Cairo_Filter is
     (Cairo_Filter_Fast,
      --  A high-performance filter, with quality similar to
      --  Cairo_Filter_Nearest

      Cairo_Filter_Good,
      --  A reasonable-performance filter, with quality similar to
      --  Cairo_Filter_Bilinear

      Cairo_Filter_Best,
      --  The highest-quality available, performance may
      --  not be suitable for interactive use.

      Cairo_Filter_Nearest,
      --  Nearest-neighbor filtering

      Cairo_Filter_Bilinear,
      --  Linear interpolation in two dimensions

      Cairo_Filter_Gaussian
      --  This filter value is currently unimplemented, and should not be used
      --  in current code.
     );
   pragma Convention (C, Cairo_Filter);

   function Create_Rgb
     (Red   : Gdouble;
      Green : Gdouble;
      Blue  : Gdouble)
      return  Cairo_Pattern;
   --  Red:   Red component of the color
   --  Green: Green component of the color
   --  Blue:  Blue component of the color
   --
   --  Creates a new Cairo_Pattern corresponding to an opaque color.  The
   --  color components are floating point numbers in the range 0 to 1.
   --  If the values passed in are outside that range, they will be
   --  clamped.
   --
   --  Return value: the newly created Cairo_Pattern if successful, or
   --  an error pattern in case of no memory.  The caller owns the
   --  returned object and should call Cairo.Pattern.Destroy when
   --  finished with it.
   --
   --  This function will always return a valid pointer, but if an error
   --  occurred the pattern status will be set to an error. To inspect
   --  the status of a pattern use Cairo.Pattern.Status.

   function Create_Rgba
     (Red   : Gdouble;
      Green : Gdouble;
      Blue  : Gdouble;
      Alpha : Gdouble)
      return  Cairo_Pattern;
   --  Red: Red component of the color
   --  Green: Green component of the color
   --  Blue: Blue component of the color
   --  Alpha: Alpha component of the color
   --
   --  Creates a new Cairo_Pattern corresponding to a translucent color.
   --  The color components are floating point numbers in the range 0 to
   --  1.  If the values passed in are outside that range, they will be
   --  clamped.
   --
   --  Return value: the newly created Cairo_Pattern if successful, or
   --  an error pattern in case of no memory.  The caller owns the
   --  returned object and should call Cairo.Pattern.Destroy when
   --  finished with it.
   --
   --  This function will always return a valid pointer, but if an error
   --  occurred the pattern status will be set to an error. To inspect
   --  the status of a pattern use Cairo.Pattern.Status.

   function Create_For_Surface
     (Surface : Cairo_Surface)
      return    Cairo_Pattern;
   --  Surface: the Surface
   --
   --  Create a new Cairo_Pattern for the given surface.
   --
   --  Return value: the newly created Cairo_Pattern if successful, or
   --  an error pattern in case of no memory.  The caller owns the
   --  returned object and should call Cairo.Pattern.Destroy when
   --  finished with it.
   --
   --  This function will always return a valid pointer, but if an error
   --  occurred the pattern status will be set to an error. To inspect
   --  the status of a pattern use Cairo.Pattern.Status.

   function Create_Linear
     (X0   : Gdouble;
      Y0   : Gdouble;
      X1   : Gdouble;
      Y1   : Gdouble)
      return Cairo_Pattern;
   --  X0: x coordinate of the start point
   --  Y0: y coordinate of the start point
   --  X1: x coordinate of the end point
   --  Y1: y coordinate of the end point
   --
   --  Create a new linear gradient Cairo_Pattern along the line defined
   --  by (X0, Y0) and (X1, Y1).  Before using the gradient pattern, a
   --  number of color stops should be defined using
   --  Cairo.Pattern.Add_Color_Stop_Rgb or
   --  Cairo.Pattern.Add_Color_Stop_Rgba.
   --
   --  Note: The coordinates here are in pattern space. For a new pattern,
   --  pattern space is identical to user space, but the relationship
   --  between the spaces can be changed with Cairo.Pattern.Set_Matrix.
   --
   --  Return value: the newly created Cairo_Pattern if successful, or
   --  an error pattern in case of no memory.  The caller owns the
   --  returned object and should call Cairo.Pattern.Destroy when
   --  finished with it.
   --
   --  This function will always return a valid pointer, but if an error
   --  occurred the pattern status will be set to an error.  To inspect
   --  the status of a pattern use Cairo.Pattern.Status.

   function Create_Radial
     (Cx0     : Gdouble;
      Cy0     : Gdouble;
      Radius0 : Gdouble;
      Cx1     : Gdouble;
      Cy1     : Gdouble;
      Radius1 : Gdouble)
      return    Cairo_Pattern;
   --  Cx0: X coordinate for the center of the start circle
   --  Cy0: Y coordinate for the center of the start circle
   --  Radius0: radius of the start circle
   --  Cx1: X coordinate for the center of the end circle
   --  Cy1: Y coordinate for the center of the end circle
   --  Radius1: radius of the end circle
   --
   --  Creates a new radial gradient Cairo_Pattern between the two circles
   --  defined by (Cx0, Cy0, Radius0) and (Cx1, Cy1, Radius1). Before using the
   --  gradient pattern, a number of color stops should be defined using
   --  Cairo.Pattern.Add_Color_Stop_Rgb or Cairo.Pattern.Add_Color_Stop_Rgba.
   --
   --  Note: The coordinates here are in pattern space. For a new pattern,
   --  pattern space is identical to user space, but the relationship
   --  between the spaces can be changed with Cairo.Pattern.Set_Matrix.
   --
   --  Return value: the newly created Cairo_Pattern if successful, or
   --  an error pattern in case of no memory.  The caller owns the
   --  returned object and should call Cairo.Pattern.Destroy when
   --  finished with it.
   --
   --  This function will always return a valid pointer, but if an error
   --  occurred the pattern status will be set to an error.  To inspect
   --  the status of a pattern use Cairo.Pattern.Status.

   function Reference (Pattern : Cairo_Pattern) return Cairo_Pattern;
   --  Pattern: a Cairo_Pattern
   --
   --  Increases the reference count on pattern by one. This prevents
   --  pattern from being destroyed until a matching call to
   --  Cairo.Pattern.Destroy is made.
   --
   --  The number of references to a Cairo_Pattern can be get using
   --  Cairo.Pattern.Get_Reference_Count.
   --
   --  Return value: the referenced Cairo_Pattern.

   procedure Destroy (Pattern : Cairo_Pattern);
   --  Pattern: a Cairo_Pattern
   --
   --  Decreases the reference count on pattern by one. If the result is
   --  zero, then pattern and all associated resources are freed.  See
   --  Cairo.Pattern.Reference.

   function Get_Reference_Count (Pattern : Cairo_Pattern) return Guint;
   --  Pattern: a Cairo_Pattern
   --
   --  Returns the current reference count of pattern.
   --
   --  Return value: the current reference count of pattern.  If the
   --  object is a nil object, 0 will be returned.
   --
   --  Since: 1.4

   function Status (Pattern : Cairo_Pattern) return Cairo_Status;
   --  Pattern: a Cairo_Pattern
   --
   --  Checks whether an error has previously occurred for this
   --  pattern.
   --
   --  Return value: Cairo_Status_Success, Cairo_Status_No_Memory, or
   --  Cairo_Status_Pattern_Type_Mismatch.

   function Get_User_Data
     (Pattern : Cairo_Pattern;
      Key     : access Cairo_User_Data_Key) return System.Address;
   --  Pattern: a Cairo_Pattern
   --  Key: the address of the Cairo_User_Data_Key the user data was
   --  attached to
   --
   --  Return user data previously attached to pattern using the
   --  specified key.  If no user data has been attached with the given
   --  key this function returns System.Null_Address.
   --
   --  Return value: the user data previously attached or System.Null_Address.
   --
   --  Since: 1.4

   function Set_User_Data
     (Pattern   : Cairo_Pattern;
      Key       : access Cairo_User_Data_Key;
      User_Data : System.Address;
      Destroy   : Cairo_Destroy_Func) return Cairo_Status;
   --  Pattern: a Cairo_Pattern
   --  Key: the address of a Cairo_User_Data_Key to attach the user data to
   --  User_Data: the user data to attach to the Cairo_Pattern
   --  Destroy: a Cairo_Destroy_Func which will be called when the
   --  Cairo_Context is destroyed or when new user data is attached using the
   --  same key.
   --
   --  Attach user data to pattern.  To remove user data from a surface,
   --  call this function with the key that was used to set it and Null_Address
   --  for data.
   --
   --  Return value: Cairo_Status_Success or Cairo_Status_No_Memory if a
   --  slot could not be allocated for the user data.
   --
   --  Since: 1.4

   function Get_Type (Pattern : Cairo_Pattern) return Cairo_Pattern_Type;
   --  Pattern: a Cairo_Pattern
   --
   --  This function returns the type a pattern.
   --  See Cairo_Pattern_Type for available types.
   --
   --  Return value: The type of pattern.
   --
   --  Since: 1.2

   procedure Add_Color_Stop_Rgb
     (Pattern : Cairo_Pattern;
      Offset  : Gdouble;
      Red     : Gdouble;
      Green   : Gdouble;
      Blue    : Gdouble);
   --  Pattern: a Cairo_Pattern
   --  Offset: an Offset in the range [0.0 .. 1.0]
   --  Red: Red component of color
   --  Green: Green component of color
   --  Blue: Blue component of color
   --
   --  Adds an opaque color stop to a gradient pattern. The offset
   --  specifies the location along the gradient's control vector. For
   --  example, a linear gradient's control vector is from (X0,Y0) to
   --  (X1,Y1) while a radial gradient's control vector is from any point
   --  on the start circle to the corresponding point on the end circle.
   --
   --  The color is specified in the same way as in Cairo.Set_Source_Rgb.
   --
   --  If two (or more) stops are specified with identical offset values,
   --  they will be sorted according to the order in which the stops are
   --  added, (stops added earlier will compare less than stops added
   --  later). This can be useful for reliably making sharp color
   --  transitions instead of the typical blend.
   --
   --
   --  Note: If the pattern is not a gradient pattern, (eg. a linear or
   --  radial pattern), then the pattern will be put into an error status
   --  with a status of Cairo_Status_Pattern_Type_Mismatch.

   procedure Add_Color_Stop_Rgba
     (Pattern : Cairo_Pattern;
      Offset  : Gdouble;
      Red     : Gdouble;
      Green   : Gdouble;
      Blue    : Gdouble;
      Alpha   : Gdouble);
   --  Pattern: a Cairo_Pattern
   --  Offset: an Offset in the range [0.0 .. 1.0]
   --  Red: Red component of color
   --  Green: Green component of color
   --  Blue: Blue component of color
   --  Alpha: Alpha component of color
   --
   --  Adds a translucent color stop to a gradient pattern. The offset
   --  specifies the location along the gradient's control vector. For
   --  example, a linear gradient's control vector is from (x0,y0) to
   --  (x1,y1) while a radial gradient's control vector is from any point
   --  on the start circle to the corresponding point on the end circle.
   --
   --  The color is specified in the same way as in Cairo_Set_Source_Rgba.
   --
   --  If two (or more) stops are specified with identical offset values,
   --  they will be sorted according to the order in which the stops are
   --  added, (stops added earlier will compare less than stops added
   --  later). This can be useful for reliably making sharp color
   --  transitions instead of the typical blend.
   --
   --  Note: If the pattern is not a gradient pattern, (eg. a linear or
   --  radial pattern), then the pattern will be put into an error status
   --  with a status of Cairo_Status_Pattern_Type_Mismatch.

   procedure Set_Matrix
     (Pattern : Cairo_Pattern;
      Matrix  : access Cairo_Matrix);
   --  Pattern: a Cairo_Pattern
   --  Matrix: a Cairo_Matrix
   --
   --  Sets the pattern's transformation matrix to matrix. This matrix is
   --  a transformation from user space to pattern space.
   --
   --  When a pattern is first created it always has the identity matrix
   --  for its transformation matrix, which means that pattern space is
   --  initially identical to user space.
   --
   --  Important: Please note that the direction of this transformation
   --  matrix is from user space to pattern space. This means that if you
   --  imagine the flow from a pattern to user space (and on to device
   --  space), then coordinates in that flow will be transformed by the
   --  inverse of the pattern matrix.
   --
   --  For example, if you want to make a pattern appear twice as large as
   --  it does by default the correct code to use is:
   --
   --  Cairo.Matrix.Init_Scale (Matrix, 0.5, 0.5);
   --  Cairo.Pattern.Set_Matrix (Pattern, Matrix);
   --
   --  Meanwhile, using values of 2.0 rather than 0.5 in the code above
   --  would cause the pattern to appear at half of its default size.
   --
   --  Also, please note the discussion of the user-space locking
   --  semantics of Cairo_Set_Source.

   procedure Get_Matrix
     (Pattern : Cairo_Pattern;
      Matrix  : access Cairo_Matrix);
   --  Pattern: a Cairo_Pattern
   --  Matrix: return value for the Matrix
   --
   --  Stores the pattern's transformation matrix into matrix.

   procedure Set_Extend (Pattern : Cairo_Pattern; Extend : Cairo_Extend);
   --  Pattern: a Cairo_Pattern
   --  Extend: a Cairo_Extend describing how the area outside of the
   --  pattern will be drawn
   --
   --  Sets the mode to be used for drawing outside the area of a pattern.
   --  See Cairo_Extend for details on the semantics of each extend
   --  strategy.
   --
   --  The default extend mode is Cairo_Extend_None for surface patterns
   --  and Cairo_Extend_PAd for gradient patterns.

   function Get_Extend (Pattern : Cairo_Pattern) return Cairo_Extend;
   --  Pattern: a Cairo_Pattern
   --
   --  Gets the current extend mode for a pattern.  See Cairo_Extend
   --  for details on the semantics of each extend strategy.
   --
   --  Return value: the current extend strategy used for drawing the
   --  pattern.

   procedure Set_Filter (Pattern : Cairo_Pattern; Filter : Cairo_Filter);
   --  Pattern: a Cairo_Pattern
   --  Filter: a Cairo_Filter describing the Filter to use for resizing
   --  the pattern
   --
   --  Sets the filter to be used for resizing when using this pattern.
   --  See Cairo_Filter for details on each filter.
   --
   --  Note that you might want to control filtering even when you do not
   --  have an explicit Cairo_Pattern object, (for example when using
   --  Cairo_Set_Source_Surface). In these cases, it is convenient to
   --  use Cairo_Get_Source to get access to the pattern that cairo
   --  creates implicitly. For example:
   --
   --  Cairo_Set_Source_Surface (Cr, Image, X, Y);
   --  Cairo.Pattern.Set_Filter (Cairo_Get_Source (Cr), Cairo_Filter_Nearest);

   function Get_Filter (Pattern : Cairo_Pattern) return Cairo_Filter;
   --  Pattern: a Cairo_Pattern
   --
   --  Gets the current filter for a pattern.  See Cairo_Filter
   --  for details on each filter.
   --
   --  Return value: the current filter used for resizing the pattern.

   function Get_Rgba
     (Pattern : Cairo_Pattern;
      Red     : access Gdouble;
      Green   : access Gdouble;
      Blue    : access Gdouble;
      Alpha   : access Gdouble)
      return    Cairo_Status;
   --  Pattern: a Cairo_Pattern
   --  Red: return value for Red component of color, or null
   --  Green: return value for Green component of color, or null
   --  Blue: return value for Blue component of color, or null
   --  Alpha: return value for Alpha component of color, or null
   --
   --  Gets the solid color for a solid color pattern.
   --
   --  Return value: Cairo_Status_Success, or
   --  Cairo_Status_Pattern_Type_Mismatch if the pattern is not a solid
   --  color pattern.
   --
   --  Since: 1.4

   function Get_Surface
     (Pattern : Cairo_Pattern;
      Surface : Cairo_Surface)
      return    Cairo_Status;
   --  Pattern: a Cairo_Pattern
   --  Surface: return value for Surface of pattern, or null
   --
   --  Gets the surface of a surface pattern.  The reference returned in
   --  surface is owned by the pattern; the caller should call
   --  Cairo.Surface.Reference if the surface is to be retained.
   --
   --  Return value: Cairo_Status_Success, or
   --  Cairo_Status_Pattern_Type_Mismatch if the pattern is not a surface
   --  pattern.
   --
   --  Since: 1.4

   function Get_Color_Stop_Rgba
     (Pattern : Cairo_Pattern;
      Index   : Gint;
      Offset  : access Gdouble;
      Red     : access Gdouble;
      Green   : access Gdouble;
      Blue    : access Gdouble;
      Alpha   : access Gdouble)
      return    Cairo_Status;
   --  Pattern: a Cairo_Pattern
   --  Index: Index of the stop to return data for
   --  Offset: return value for the Offset of the stop, or null
   --  Red: return value for Red component of color, or null
   --  Green: return value for Green component of color, or null
   --  Blue: return value for Blue component of color, or null
   --  Alpha: return value for Alpha component of color, or null
   --
   --  Gets the color and offset information at the given index for a
   --  gradient pattern.  Values of index are 0 to 1 less than the number
   --  returned by Cairo.Pattern.Get_Color_Stop_Count.
   --
   --  Return value: Cairo_Status_Success, or Cairo_Status_Invalid_Index
   --  if index is not valid for the given pattern.  If the pattern is
   --  not a gradient pattern, Cairo_Status_Pattern_Type_Mismatch is
   --  returned.
   --
   --  Since: 1.4

   function Get_Color_Stop_Count
     (Pattern : Cairo_Pattern;
      Count   : access Gint)
      return    Cairo_Status;
   --  Pattern: a Cairo_Pattern
   --  Count: return value for the number of color stops, or NULL
   --
   --  Gets the number of color stops specified in the given gradient
   --  pattern.
   --
   --  Return value: Cairo_Status_Success, or
   --  Cairo_Status_Pattern_Type_Mismatch if pattern is not a gradient
   --  pattern.
   --
   --  Since: 1.4

   function Get_Linear_Points
     (Pattern : Cairo_Pattern;
      X0      : access Gdouble;
      Y0      : access Gdouble;
      X1      : access Gdouble;
      Y1      : access Gdouble)
      return    Cairo_Status;
   --  Pattern: a Cairo_Pattern
   --  X0: return value for the x coordinate of the first point, or null
   --  Y0: return value for the y coordinate of the first point, or null
   --  X1: return value for the x coordinate of the second point, or null
   --  Y1: return value for the y coordinate of the second point, or null
   --
   --  Gets the gradient endpoints for a linear gradient.
   --
   --  Return value: Cairo_Status_Success, or
   --  Cairo_Status_Pattern_Type_Mismatch if pattern is not a linear
   --  gradient pattern.
   --
   --  Since: 1.4

   function Get_Radial_Circles
     (Pattern : Cairo_Pattern;
      X0      : access Gdouble;
      Y0      : access Gdouble;
      R0      : access Gdouble;
      X1      : access Gdouble;
      Y1      : access Gdouble;
      R1      : access Gdouble)
      return    Cairo_Status;
   --  Pattern: a Cairo_Pattern
   --  X0: return value for the x coordinate of the center of the first
   --  circle, or null
   --  Y0: return value for the y coordinate of the center of the first
   --  circle, or null
   --  R0: return value for the radius of the first circle, or null
   --  X1: return value for the x coordinate of the center of the second
   --  circle, or null
   --  Y1: return value for the y coordinate of the center of the second
   --  circle, or null
   --  R1: return value for the radius of the second circle, or null
   --
   --  Gets the gradient endpoint circles for a radial gradient, each
   --  specified as a center coordinate and a radius.
   --
   --  Return value: Cairo_Status_Success, or
   --  Cairo_Status_Pattern_Type_Mismatch if pattern is not a radial
   --  gradient pattern.
   --
   --  Since: 1.4

private

   pragma Import (C, Create_Rgb, "cairo_pattern_create_rgb");
   pragma Import (C, Create_Rgba, "cairo_pattern_create_rgba");
   pragma Import (C, Create_For_Surface, "cairo_pattern_create_for_surface");
   pragma Import (C, Create_Linear, "cairo_pattern_create_linear");
   pragma Import (C, Create_Radial, "cairo_pattern_create_radial");
   pragma Import (C, Reference, "cairo_pattern_reference");
   pragma Import (C, Destroy, "cairo_pattern_destroy");
   pragma Import
     (C,
      Get_Reference_Count,
      "cairo_pattern_get_reference_count");
   pragma Import (C, Status, "cairo_pattern_status");
   pragma Import (C, Get_User_Data, "cairo_pattern_get_user_data");
   pragma Import (C, Set_User_Data, "cairo_pattern_set_user_data");
   pragma Import (C, Get_Type, "cairo_pattern_get_type");
   pragma Import (C, Add_Color_Stop_Rgb, "cairo_pattern_add_color_stop_rgb");
   pragma Import
     (C,
      Add_Color_Stop_Rgba,
      "cairo_pattern_add_color_stop_rgba");
   pragma Import (C, Set_Matrix, "cairo_pattern_set_matrix");
   pragma Import (C, Get_Matrix, "cairo_pattern_get_matrix");
   pragma Import (C, Set_Extend, "cairo_pattern_set_extend");
   pragma Import (C, Get_Extend, "cairo_pattern_get_extend");
   pragma Import (C, Set_Filter, "cairo_pattern_set_filter");
   pragma Import (C, Get_Filter, "cairo_pattern_get_filter");
   pragma Import (C, Get_Rgba, "cairo_pattern_get_rgba");
   pragma Import (C, Get_Surface, "cairo_pattern_get_surface");
   pragma Import
     (C,
      Get_Color_Stop_Rgba,
      "cairo_pattern_get_color_stop_rgba");
   pragma Import
     (C,
      Get_Color_Stop_Count,
      "cairo_pattern_get_color_stop_count");
   pragma Import (C, Get_Linear_Points, "cairo_pattern_get_linear_points");
   pragma Import (C, Get_Radial_Circles, "cairo_pattern_get_radial_circles");

end Cairo.Pattern;
