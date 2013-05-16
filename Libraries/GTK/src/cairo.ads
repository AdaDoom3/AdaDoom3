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
--  Bindings to the Cairo 2D graphics library.
--  The Cairo_Context is the main object used when drawing with cairo. To draw
--  with Cairo, you create a Context, set the target surface, and drawing
--  options for the Cairo_Context, create shapes with functions like Move_To
--  and Line_To, and then draw shapes with Stroke or Fill.
--
--  All drawing in Cairo is done on a Cairo_Context.
--
--  Drawing on on-screen Gtk widgets should be done in a callback to the
--  "expose" event:
--
--  When the widget has been created, connect a drawing function:
--
--  <code>
--     declare
--        Area : Gtk_Drawing_Area;
--
--        package Event_Cb is new Gtk.Handlers.Return_Callback
--            (Gtk_Drawing_Area_Record, Boolean);
--     begin
--        Gtk_New (Area);
--        Event_Cb.Connect (Area, "expose_event",
--                          Event_Cb.To_Marshaller (Expose_Cb'Access));
--     end;
--  </code>
--
--  In the callback, first get the context of the drawable on which you
--  need to draw, using Gdk.Cairo.Create. Then do the drawing operations, and
--  release the memory allocated to Cr using Cairo.Destroy.
--
--  In addition to drawing on on-screen widgets, drawing can also be done using
--  the same Cairo calls to pixbufs (see Gdk.Cairo) to memory
--  (see Cairo.Image_Surface), and to PNG files (see Cairo.Png).
--
--  Code samples demonstrating how to use various functionalities of Cairo
--  can be found in the testcairo example, shipped with GtkAda.
--  </description>
--
--  <c_version>1.8.8</c_version>
--  <group>Cairo</group>

with Ada.Unchecked_Deallocation;

with System;
with Interfaces.C.Strings;

with Glib; use Glib;

package Cairo is

   type Cairo_Context is private;
   --  A Cairo_Context contains the current state of the rendering device,
   --  including coordinates of yet to be drawn shapes.
   --
   --  Cairo contexts, as Cairo_Context objects are named, are central to
   --  cairo and all drawing with cairo is always done to a Cairo_Context
   --  object.
   --
   --  Memory management of Cairo_Context is done with subprograms
   --  Reference and Destroy, see below.

   type Cairo_Surface is private;
   --  A Cairo_Surface represents an image, either as the destination
   --  of a drawing operation or as source when drawing onto another
   --  surface.  To draw to a Cairo_Surface, create a cairo context
   --  with the surface as the target, using Create.
   --
   --  There are different subtypes of Cairo_Surface for
   --  different drawing backends; for example, Cairo.Image_Surface.Create
   --  creates a bitmap image in memory.
   --  The type of a surface can be queried with Cairo.Surface.Get_Type.
   --
   --  Memory management of Cairo_Surface is done with
   --  Cairo.Surface.Reference and Cairo.Surface.Destroy.

   type Cairo_Matrix is record
      Xx : aliased Gdouble;
      Yx : aliased Gdouble;
      Xy : aliased Gdouble;
      Yy : aliased Gdouble;
      X0 : aliased Gdouble;
      Y0 : aliased Gdouble;
   end record;
   --  Xx: Xx component of the affine transformation
   --  Yx: Yx component of the affine transformation
   --  Xy: Xy component of the affine transformation
   --  Yy: Yy component of the affine transformation
   --  X0: X translation component of the affine transformation
   --  Y0: Y translation component of the affine transformation
   --
   --  A Cairo_Matrix holds an affine transformation, such as a scale,
   --  rotation, shear, or a combination of those. The transformation of
   --  a point (X, Y) is given by:
   --
   --      X_New = Xx * X + Xy * Y + X0;
   --      Y_New = Yx * X + Yy * Y + Y0;
   pragma Convention (C_Pass_By_Copy, Cairo_Matrix);

   type Cairo_Matrix_Access is access Cairo_Matrix;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Cairo_Matrix, Cairo_Matrix_Access);

   type Cairo_Pattern is private;
   --  A Cairo_Pattern represents a source when drawing onto a
   --  surface. There are different subtypes of Cairo_Pattern,
   --  for different types of sources; for example,
   --  Cairo.Pattern.Create_Rgb creates a pattern for a solid
   --  opaque color.
   --
   --  Other than various Cairo.Pattern.Create_<type>
   --  functions, some of the pattern types can be implicitly created
   --  using various Set_Source_<type> functions; for example Set_Source_Rgb.
   --
   --  The type of a pattern can be queried with Cairo.Pattern.Get_Type.
   --
   --  Memory management of Cairo_Pattern is done with
   --  Cairo.Pattern.Reference and Cairo.Pattern.Destroy.

   type Cairo_Destroy_Func is access procedure (Arg1 : System.Address);
   --  Data: The Data element being destroyed.
   --
   --  Cairo_destroy_func the type of function which is called when a
   --  data element is destroyed. It is passed the pointer to the data
   --  element and should free any memory and resources allocated for it.

   type Cairo_User_Data_Key is record
      Unused : aliased Gint;
   end record;
   --  Unused: not used; ignore.
   --
   --  Cairo_User_Data_Key is used for attaching user data to cairo
   --  data structures.  The actual contents of the struct is never used,
   --  and there is no need to initialize the object; only the unique
   --  address of a Cairo_User_Data_Key object is used.  Typically, you
   --  would just use the address of a static Cairo_User_Data_Key object.

   pragma Convention (C_Pass_By_Copy, Cairo_User_Data_Key);

   --  Cairo_Status is used to indicate errors that can occur when
   --  using Cairo. In some cases it is returned directly by functions.
   --  but when using Cairo_T, the last error, if any, is stored in
   --  the context and can be retrieved with Cairo_Status.
   --
   --  New entries may be added in future versions.  Use
   --  Cairo_Status_To_String
   --  to get a human-readable representation of an error message.
   type Cairo_Status is
     (
      Cairo_Status_Success,
      --  no error has occurred

      Cairo_Status_No_Memory,
      --  out of memory

      Cairo_Status_Invalid_Restore,
      --  Cairo_Restore called without matching Cairo_Save

      Cairo_Status_Invalid_Pop_Group,
      --  no saved group to pop

      Cairo_Status_No_Current_Point,
      --  no current point defined

      Cairo_Status_Invalid_Matrix,
      --  invalid matrix (not invertible)

      Cairo_Status_Invalid_Status,
      --  invalid value for an input Cairo_status

      Cairo_Status_Null_Pointer,
      --  NULL pointer

      Cairo_Status_Invalid_String,
      --  input string not valid UTF-8

      Cairo_Status_Invalid_Path_Data,
      --  input path data not valid

      Cairo_Status_Read_Error,
      --  error while reading from input stream

      Cairo_Status_Write_Error,
      --  error while writing to output stream

      Cairo_Status_Surface_Finished,
      --  target surface has been finished

      Cairo_Status_Surface_Type_Mismatch,
      --  the surface type is not appropriate for the operation

      Cairo_Status_Pattern_Type_Mismatch,
      --  the pattern type is not appropriate for the operation

      Cairo_Status_Invalid_Content,
      --  invalid value for an input Cairo_content

      Cairo_Status_Invalid_Format,
      --  invalid value for an input Cairo_format

      Cairo_Status_Invalid_Visual,
      --  invalid value for an input Visual*

      Cairo_Status_File_Not_Found,
      --  file not found

      Cairo_Status_Invalid_Dash,
      --  invalid value for a dash setting

      Cairo_Status_Invalid_Dsc_Comment,
      --  invalid value for a DSC comment (Since 1.2)

      Cairo_Status_Invalid_Index,
      --  invalid index passed to getter (Since 1.4)

      Cairo_Status_Clip_Not_Representable,
      --  clip region not representable in desired format (Since 1.4)

      Cairo_Status_Temp_File_Error,
      --  error creating or writing to a temporary file (Since 1.6)

      Cairo_Status_Invalid_Stride,
      --  invalid value for stride (Since 1.6)

      Cairo_Status_Font_Type_Mismatch,
      --  the font type is not appropriate for the operation (Since 1.8)

      Cairo_Status_User_Font_Immutable,
      --  the user-font is immutable (Since 1.8)

      Cairo_Status_User_Font_Error,
      --  error occurred in a user-font callback function (Since 1.8)

      Cairo_Status_Negative_Count,
      --  negative number used where it is not allowed (Since 1.8)

      Cairo_Status_Invalid_Clusters,
      --  input clusters do not represent the accompanying text and glyph
      --  array (Since 1.8)

      Cairo_Status_Invalid_Slant,
      --  invalid value for an input Cairo_Font_Slant (Since 1.8)

      Cairo_Status_Invalid_Weight
      --  invalid value for an input Cairo_Font_Weight (Since 1.8)
     );

   subtype Cairo_Content is Guint;
   --  Cairo_content is used to describe the content that a surface will
   --  contain, whether color information, alpha information (translucence
   --  vs. opacity), or both.
   --
   --  Note: The large values here are designed to keep Cairo_Content
   --  values distinct from Cairo_Format values so that the
   --  implementation can detect the error if users confuse the two types.

   Cairo_Content_Color       : constant Cairo_Content := 4096;
   --  The surface will hold color content only.

   Cairo_Content_Alpha       : constant Cairo_Content := 8192;
   --  CAIRO_CONTENT_ALPHA: The surface will hold alpha content only.

   Cairo_Content_Color_Alpha : constant Cairo_Content := 12288;
   --  CAIRO_CONTENT_COLOR_ALPHA: The surface will hold color and alpha
   --  content.

   function Create (Target : Cairo_Surface) return Cairo_Context;
   --  Target: Target surface for the context
   --
   --  Creates a new Cairo_Context with all graphics state parameters set to
   --  default values and with target as a target surface. The target
   --  surface should be constructed with a backend-specific function such
   --  as Cairo.Image_Surface.Create.
   --
   --  This function references target, so you can immediately
   --  call Cairo.Surface.Destroy on it if you don't need to
   --  maintain a separate reference to it.
   --
   --  Return value: a newly allocated Cairo_Context with a reference
   --  count of 1. The initial reference count should be released
   --  with Destroy when you are done using the Cairo_Context.
   --  This function never returns NULL. If memory cannot be
   --  allocated, a special Cairo_Context object will be returned on
   --  which Status returns Cairo_Status_No_Memory.
   --  You can use this object normally, but no drawing will
   --  be done.

   function Reference (Cr : Cairo_Context) return Cairo_Context;
   --  Cr: a Cairo_Context
   --
   --  Increases the reference count on cr by one. This prevents
   --  cr from being destroyed until a matching call to Destroy
   --  is made.
   --
   --  The number of references to a Cairo_Context can be retrieved using
   --  Get_Reference_Count.
   --
   --  Return value: the referenced Cairo_Context.

   procedure Destroy (Cr : Cairo_Context);
   --  Cr: a Cairo_Context
   --
   --  Decreases the reference count on cr by one. If the result
   --  is zero, then cr and all associated resources are freed.
   --  See Reference.

   function Get_Reference_Count (Cr : Cairo_Context) return Guint;
   --  Cr: a Cairo_Context
   --
   --  Returns the current reference count of cr.
   --
   --  Return value: the current reference count of cr.  If the
   --  object is a nil object, 0 will be returned.
   --
   --  Since: 1.4

   function Get_User_Data
     (Cr   : Cairo_Context;
      Key  : access Cairo_User_Data_Key)
      return System.Address;
   --  Cr: a Cairo_Context
   --  Key: the address of the Cairo_User_Data_Key the user data was
   --  attached to
   --
   --  Return user data previously attached to cr using the specified
   --  key.  If no user data has been attached with the given key this
   --  function returns NULL.
   --
   --  Return value: the user data previously attached or NULL.
   --
   --  Since: 1.4

   function Set_User_Data
     (Cr        : Cairo_Context;
      Key       : access Cairo_User_Data_Key;
      User_Data : System.Address;
      Destroy   : Cairo_Destroy_Func)
      return      Cairo_Status;
   --  Cr: a Cairo_Context
   --  Key: the address of a Cairo_User_Data_Key to attach the user data to
   --  User_Data: the user data to attach to the Cairo_Context
   --  Destroy: a Cairo_Destroy_Func which will be called when the
   --  Cairo_Context is destroyed or when new user data is attached using the
   --  same key.
   --
   --  Attach user data to cr.  To remove user data from a surface,
   --  call this function with the key that was used to set it and NULL
   --  for data.
   --
   --  Return value: CAIRO_STATUS_SUCCESS or CAIRO_STATUS_NO_MEMORY if a
   --  slot could not be allocated for the user data.
   --
   --  Since: 1.4

   procedure Save (Cr : Cairo_Context);
   --  Cr: a Cairo_Context
   --
   --  Makes a copy of the current state of cr and saves it
   --  on an internal stack of saved states for cr. When
   --  Cairo_Restore is called, cr will be restored to
   --  the saved state. Multiple calls to Cairo_Save and
   --  Cairo_Restore can be nested; each call to Cairo_Restore
   --  restores the state from the matching paired Cairo_Save.
   --
   --  It isn't necessary to clear all saved states before
   --  a Cairo_Context is freed. If the reference count of a Cairo_Context
   --  drops to zero in response to a call to Cairo_Destroy,
   --  any saved states will be freed along with the Cairo_Context.

   procedure Restore (Cr : Cairo_Context);
   --  Cr: a Cairo_Context
   --
   --  Restores cr to the state saved by a preceding call to
   --  Cairo_Save and removes that state from the stack of
   --  saved states.

   procedure Push_Group (Cr : Cairo_Context);
   --  Cr: a cairo context
   --
   --  Temporarily redirects drawing to an intermediate surface known as a
   --  group. The redirection lasts until the group is completed by a call
   --  to Pop_Group or Pop_Group_To_Source.
   --
   --  These calls provide the result of any drawing to the group as a pattern,
   --  (either as an explicit object, or set as the source pattern).
   --
   --  This group functionality can be convenient for performing
   --  intermediate compositing. One common use of a group is to render
   --  objects as opaque within the group, (so that they occlude each
   --  other), and then blend the result with translucence onto the
   --  destination.
   --
   --  Groups can be nested arbitrarily deep by making balanced calls to
   --  Push_Group/Pop_Group. Each call pushes/pops the new target group
   --  onto/from a stack.
   --
   --  The Push_Group function calls Save so that any changes to the graphics
   --  state will not be visible outside the group, (the pop_group functions
   --  call Restore).
   --
   --  By default the intermediate group will have a content type of
   --  Cairo_Content_Color_Alphe. Other content types can be chosen for
   --  the group by using Push_Group_With_Content instead.
   --
   --  As an example, here is how one might fill and stroke a path with
   --  translucence, but without any portion of the fill being visible
   --  under the stroke:
   --
   --      Push_Group (Cr);
   --      Set_Source (Cr, Fill_Pattern);
   --      Fill_Preserve (Cr);
   --      Set_Source (Cr, Stroke_Pattern);
   --      Stroke (Cr);
   --      Pop_Group_To_Source (Cr);
   --      Paint_With_Alpha (Cr, Alpha);
   --
   --  Since: 1.2

   procedure Push_Group_With_Content
     (Cr      : Cairo_Context;
      Content : Cairo_Content);
   --  Cr: a cairo context
   --  Content: a Cairo_Content indicating the type of group that
   --            will be created
   --
   --  Temporarily redirects drawing to an intermediate surface known as a
   --  group. The redirection lasts until the group is completed by a call to
   --  Pop_Group or Pop_Group_To_Source. These calls provide the result of any
   --  drawing to the group as a pattern, (either as an explicit object, or set
   --  as the source pattern).
   --
   --  The group will have a content type of content. The ability to control
   --  this content type is the only distinction between this function and
   --  Push_Group which you should see for a more detailed description of group
   --  rendering.
   --
   --  Since: 1.2

   function Pop_Group (Cr : Cairo_Context) return Cairo_Pattern;
   --  Cr: a cairo context
   --
   --  Terminates the redirection begun by a call to Push_Group or
   --  Push_Group_With_Content and returns a new pattern containing the results
   --  of all drawing operations performed to the group.
   --
   --  The Pop_Group function calls Restore, (balancing a call to Save by the
   --  Push_Group function), so that any changes to the graphics state will not
   --  be visible outside the group.
   --
   --  Return value: a newly created (surface) pattern containing the
   --  results of all drawing operations performed to the group. The
   --  caller owns the returned object and should call
   --  Cairo.Pattern.Destroy when finished with it.
   --
   --  Since: 1.2

   procedure Pop_Group_To_Source (Cr : Cairo_Context);
   --  Cr: a cairo context
   --
   --  Terminates the redirection begun by a call to Push_Group or
   --  Push_Group_With_Content and installs the resulting pattern as the source
   --  pattern in the given cairo context.
   --
   --  The behavior of this function is equivalent to the sequence of
   --  operations:
   --
   --  declare
   --     Group: Cairo_Pattern := Pop_Group (Cr);
   --  begin
   --     Set_Source (Cr, Group);
   --     Cairo.Pattern.Destroy (Group);
   --  end;
   --
   --  but is more convenient as their is no need for a variable to store
   --  the short-lived pointer to the pattern.
   --
   --  The Pop_Group function calls Restore, (balancing a call to Save by the
   --  push_group function), so that any changes to the graphics state will not
   --  be visible outside the group.
   --
   --  Since: 1.2

   --  Cairo_operator is used to set the compositing operator for all cairo
   --  drawing operations.
   --
   --  The default operator is Cairo_Operator_Over.
   --
   --  The operators marked as "unbounded" modify their destination even
   --  outside of the mask layer (that is, their effect is not bound by the
   --  mask layer). However, their effect can still be limited by way of
   --  clipping.
   --
   --  To keep things simple, the operator descriptions here document the
   --  behavior for when both source and destination are either fully
   --  transparent or fully opaque. The actual implementation works for
   --  translucent layers too.
   --
   --  For a more detailed explanation of the effects of each operator,
   --  including the mathematical definitions, see
   --  http://cairographics.org/operators/
   type Cairo_Operator is
     (Cairo_Operator_Clear,
      --  clear destination layer (bounded)

      Cairo_Operator_Source,
      --  replace destination layer (bounded)

      Cairo_Operator_Over,
      --  draw source layer on top of destination layer (bounded)

      Cairo_Operator_In,
      --  draw source where there was destination content (unbounded)

      Cairo_Operator_Out,
      --  draw source where there was no destination content (unbounded)

      Cairo_Operator_Atop,
      --  draw source on top of destination content and only there

      Cairo_Operator_Dest,
      --  ignore the source

      Cairo_Operator_Dest_Over,
      --  draw destination on top of source

      Cairo_Operator_Dest_In,
      --  leave destination only where there was source content (unbounded)

      Cairo_Operator_Dest_Out,
      --  leave destination only where there was no source content

      Cairo_Operator_Dest_Atop,
      --  leave destination on top of source content and only there (unbounded)

      Cairo_Operator_Xor,
      --  source and destination are shown where there is only one of them

      Cairo_Operator_Add,
      --  source and destination layers are accumulated

      Cairo_Operator_Saturate
      --  like over, but assuming source and dest are disjoint geometries
     );

   procedure Set_Operator (Cr : Cairo_Context; Op : Cairo_Operator);
   --  Cr: a Cairo_Context
   --  Op: a compositing Operator, specified as a Cairo_Operator
   --
   --  Sets the compositing operator to be used for all drawing
   --  operations. See Cairo_Operator for details on the semantics of
   --  each available compositing operator.
   --
   --  The default operator is Cairo_Operator_Over.

   procedure Set_Source (Cr : Cairo_Context; Source : Cairo_Pattern);
   --  Cr: a cairo context
   --  Source: a Cairo_Pattern to be used as the Source for
   --  subsequent drawing operations.
   --
   --  Sets the source pattern within Cr to source. This pattern
   --  will then be used for any subsequent drawing operation until a new
   --  source pattern is set.
   --
   --  Note: The pattern's transformation matrix will be locked to the user
   --  space in effect at the time of Set_Source. This means that further
   --  modifications of the current transformation matrix will not affect the
   --  source pattern. See Cairo.Pattern.Set_Matrix.
   --
   --  The default source pattern is a solid pattern that is opaque black,
   --  (that is, it is equivalent to Set_Source_Rgb (Cr, 0.0, 0.0, 0.0)).

   procedure Set_Source_Rgb
     (Cr    : Cairo_Context;
      Red   : Gdouble;
      Green : Gdouble;
      Blue  : Gdouble);
   --  Cr    : a cairo context
   --  Red   : Red component of color
   --  Green : Green component of color
   --  Blue  : Blue component of color
   --
   --  Sets the source pattern within Cr to an opaque color. This opaque
   --  color will then be used for any subsequent drawing operation until
   --  a new source pattern is set.
   --
   --  The color components are floating point numbers in the range 0 to
   --  1. If the values passed in are outside that range, they will be
   --  clamped.
   --
   --  The default source pattern is opaque black, (that is, it is
   --  equivalent to Set_Source_Rgb (Cr, 0.0, 0.0, 0.0)).

   procedure Set_Source_Rgba
     (Cr    : Cairo_Context;
      Red   : Gdouble;
      Green : Gdouble;
      Blue  : Gdouble;
      Alpha : Gdouble);
   --  Cr    : a cairo context
   --  Red   : Red component of color
   --  Green : Green component of color
   --  Blue  : Blue component of color
   --  Alpha : Alpha component of color
   --
   --  Sets the source pattern within Cr to a translucent color. This
   --  color will then be used for any subsequent drawing operation until
   --  a new source pattern is set.
   --
   --  The color and alpha components are floating point numbers in the
   --  range 0 to 1. If the values passed in are outside that range, they
   --  will be clamped.
   --
   --  The default source pattern is opaque black, (that is, it is
   --  equivalent to Set_Source_Rgba (Cr, 0.0, 0.0, 0.0, 1.0)).

   procedure Set_Source_Surface
     (Cr      : Cairo_Context;
      Surface : Cairo_Surface;
      X       : Gdouble;
      Y       : Gdouble);
   --  Cr      : a cairo context
   --  Surface : a Surface to be used to set the source pattern
   --  X       : User-space X coordinate for surface origin
   --  Y       : User-space Y coordinate for surface origin
   --
   --  This is a convenience function for creating a pattern from surface
   --  and setting it as the source in Cr with Set_Source.
   --
   --  The X and Y parameters give the user-space coordinate at which
   --  the surface origin should appear. (The surface origin is its
   --  upper-left corner before any transformation has been applied.) The
   --  X and Y patterns are negated and then set as translation values
   --  in the pattern matrix.
   --
   --  Other than the initial translation pattern matrix, as described
   --  above, all other pattern attributes, (such as its extend mode), are
   --  set to the default values as in Cairo.Pattern.Create_For_Surface.
   --  The resulting pattern can be queried with Get_Source so that these
   --  attributes can be modified if desired, (eg. to create a
   --  repeating pattern with Cairo.Pattern.Set_Extend).

   procedure Set_Tolerance (Cr : Cairo_Context; Tolerance : Gdouble);
   --  Cr: a Cairo_Context
   --  Tolerance: the Tolerance, in device units (typically pixels)
   --
   --  Sets the tolerance used when converting paths into trapezoids.
   --  Curved segments of the path will be subdivided until the maximum
   --  deviation between the original path and the polygonal approximation
   --  is less than tolerance. The default value is 0.1. A larger
   --  value will give better performance, a smaller value, better
   --  appearance. (Reducing the value from the default value of 0.1
   --  is unlikely to improve appearance significantly.)  The accuracy of paths
   --  within Cairo is limited by the precision of its internal arithmetic, and
   --  the prescribed tolerance is restricted to the smallest
   --  representable internal value.

   --  Specifies the type of antialiasing to do when rendering text or shapes.
   type Cairo_Antialias is
     (
      Cairo_Antialias_Default,
      --  Use the default antialiasing for the subsystem and target device

      Cairo_Antialias_None,
      --  Use a bilevel alpha mask

      Cairo_Antialias_Gray,
      --  Perform single-color antialiasing (using shades of gray for black
      --  text on a white background, for example).

      Cairo_Antialias_Subpixel
      --  Perform antialiasing by taking advantage of the order of subpixel
      --  elements on devices such as LCD panels
     );

   procedure Set_Antialias
     (Cr        : Cairo_Context;
      Antialias : Cairo_Antialias);
   --  Cr: a Cairo_Context
   --  Antialias: the new Antialiasing mode
   --
   --  Set the antialiasing mode of the rasterizer used for drawing shapes.
   --  This value is a hint, and a particular backend may or may not support
   --  a particular value.  At the current time, no backend supports
   --  Cairo_Antialias_Subpixel when drawing shapes.
   --
   --  Note that this option does not affect text rendering, instead see
   --  Cairo.Font_Options.Set_Antialias.

   --  Cairo_Fill_Rule is used to select how paths are filled. For both
   --  fill rules, whether or not a point is included in the fill is
   --  determined by taking a ray from that point to infinity and looking
   --  at intersections with the path. The ray can be in any direction,
   --  as long as it doesn't pass through the end point of a segment
   --  or have a tricky intersection such as intersecting tangent to the path.
   --  (Note that filling is not actually implemented in this way. This
   --  is just a description of the rule that is applied.)
   --
   --  The default fill rule is Cairo_Fill_Rule_Winding.
   --
   --  New entries may be added in future versions.
   type Cairo_Fill_Rule is
     (Cairo_Fill_Rule_Winding,
      --  If the path crosses the ray from left-to-right, counts +1. If the
      --  path crosses the ray from right to left, counts -1. (Left and right
      --  are determined from the perspective of looking along the ray from
      --  the starting point). If the total count is non-zero, the point will
      --  be filled.

      Cairo_Fill_Rule_Even_Odd
      --  Counts the total number of
      --  intersections, without regard to the orientation of the contour. If
      --  the total number of intersections is odd, the point will be filled.
     );

   procedure Set_Fill_Rule
     (Cr        : Cairo_Context;
      Fill_Rule : Cairo_Fill_Rule);
   --  Cr: a Cairo_Context
   --  Fill_Rule: a fill rule
   --
   --  Set the current fill rule within the cairo context. The fill rule is
   --  used to determine which regions are inside or outside a complex
   --  (potentially self-intersecting) path. The current fill rule affects both
   --  Fill and Clip. See Cairo_Fill_Rule for details on the semantics of each
   --  available fill rule.
   --
   --  The default fill rule is Cairo_Fill_Rule_Winding.

   procedure Set_Line_Width (Cr : Cairo_Context; Width : Gdouble);
   --  Cr: a Cairo_Context
   --  Width: a line Width
   --
   --  Sets the current line width within the cairo context. The line
   --  width value specifies the diameter of a pen that is circular in
   --  user space, (though device-space pen may be an ellipse in general
   --  due to scaling/shear/rotation of the CTM).
   --
   --  Note: When the description above refers to user space and CTM it
   --  refers to the user space and CTM in effect at the time of the
   --  stroking operation, not the user space and CTM in effect at the
   --  time of the call to Set_Line_Width. The simplest usage
   --  makes both of these spaces identical. That is, if there is no
   --  change to the CTM between a call to Set_Line_Width and the
   --  stroking operation, then one can just pass user-space values to
   --  Set_Line_Width and ignore this note.
   --
   --  As with the other stroke parameters, the current line width is examined
   --  by Stroke, Stroke_Extents, and Stroke_To_Path, but does not have any
   --  effect during path construction.
   --
   --  The default line width value is 2.0.

   type Cairo_Line_Cap is
     (Cairo_Line_Cap_Butt,
      --  start(stop) the line exactly at the start(end) point

      Cairo_Line_Cap_Round,
      --  use a round ending, the center of the circle is the end point

      Cairo_Line_Cap_Square
      --  use squared ending, the center of the square is the end point
     );
   --  Specifies how to render the endpoints of the path when stroking.
   --
   --  The default line cap style is Cairo_Line_Cap_Butt.

   procedure Set_Line_Cap (Cr : Cairo_Context; Line_Cap : Cairo_Line_Cap);
   --  Cr: a cairo context
   --  Line_Cap: a line cap style
   --
   --  Sets the current line cap style within the cairo context. See
   --  Cairo_Line_Cap for details about how the available line cap
   --  styles are drawn.
   --
   --  As with the other stroke parameters, the current line cap style is
   --  examined by Stroke, Stroke_Extents, and Stroke_To_Path, but does not
   --  have any effect during path construction.
   --
   --  The default line cap style is Cairo_Line_Cap_Butt.

   type Cairo_Line_Join is
     (Cairo_Line_Join_Miter,
      --  use a sharp (angled) corner, see Set_Miter_Limit

      Cairo_Line_Join_Round,
      --  use a rounded join, the center of the circle is the joint point

      Cairo_Line_Join_Bevel
      --  use a cut-off join, the join is cut off at half the line width from
      --  the joint point
     );
   --  Specifies how to render the junction of two lines when stroking.
   --
   --  The default line join style is Cairo_Line_Join_Miter.

   procedure Set_Line_Join
     (Cr        : Cairo_Context;
      Line_Join : Cairo_Line_Join);
   --  Cr: a cairo context
   --  Line_Join: a line join style
   --
   --  Sets the current line join style within the cairo context. See
   --  Cairo_Line_Join for details about how the available line join styles are
   --  drawn.
   --
   --  As with the other stroke parameters, the current line join style is
   --  examined by Stroke, Stroke_Extents, and Stroke_To_Path, but does
   --  not have any effect during path construction.
   --
   --  The default line join style is Cairo_Line_Join_Miter.

   type Dash_Array is array (Natural range <>) of Gdouble;

   No_Dashes : constant Dash_Array (1 .. 0) := (others => 0.0);

   procedure Set_Dash
     (Cr         : Cairo_Context;
      Dashes     : Dash_Array;
      Offset     : Gdouble);
   --  Cr: a cairo context
   --  Dashes: an array specifying alternate lengths of on and off stroke
   --  portions
   --  Offset: an Offset into the dash pattern at which the stroke should start
   --
   --  Sets the dash pattern to be used by Stroke. A dash pattern
   --  is specified by dashes, an array of positive values. Each value
   --  provides the length of alternate "on" and "off" portions of the
   --  stroke. The offset specifies an offset into the pattern at which
   --  the stroke begins.
   --
   --  Each "on" segment will have caps applied as if the segment were a
   --  separate sub-path. In particular, it is valid to use an "on" length
   --  of 0.0 with Cairo_Line_Cap_Round or Cairo_Line_Cap_Square in order
   --  to distributed dots or squares along a path.
   --
   --  Note: The length values are in user-space units as evaluated at the
   --  time of stroking. This is not necessarily the same as the user
   --  space at the time of Set_Dash.
   --
   --  If the array is No_Dashes, dashing is disabled.
   --
   --  If the array contains only one element symmetric pattern is assumed with
   --  alternating on and off portions of the size specified by the single
   --  value in dashes.
   --
   --  If any value in dashes is negative, or if all values are 0, then
   --  cr will be put into an error state with a status of
   --  Cairo_Status_Invalid_Dash.

   procedure Set_Miter_Limit (Cr : Cairo_Context; Limit : Gdouble);
   --  Cr: a cairo context
   --  Limit: miter Limit to set
   --
   --  Sets the current miter limit within the cairo context.
   --
   --  If the current line join style is set to Cairo_Line_Join_Miter
   --  (see Cairo_Set_Line_Join), the miter limit is used to determine
   --  whether the lines should be joined with a bevel instead of a miter.
   --  Cairo divides the length of the miter by the line width.
   --  If the result is greater than the miter limit, the style is
   --  converted to a bevel.
   --
   --  As with the other stroke parameters, the current line miter limit is
   --  examined by Stroke, Stroke_Extents, and Stroke_To_Path, but does not
   --  have any effect during path construction.
   --
   --  The default miter limit value is 10.0, which will convert joins
   --  with interior angles less than 11 degrees to bevels instead of
   --  miters. For reference, a miter limit of 2.0 makes the miter cutoff
   --  at 60 degrees, and a miter limit of 1.414 makes the cutoff at 90
   --  degrees.
   --
   --  A miter limit for a desired angle can be computed as: miter limit =
   --  1/sin(angle/2)

   procedure Translate (Cr : Cairo_Context; Tx : Gdouble; Ty : Gdouble);
   --  Cr: a cairo context
   --  Tx: amount to translate in the X direction
   --  Ty: amount to translate in the Y direction
   --
   --  Modifies the current transformation matrix (CTM) by translating the
   --  user-space origin by (tx, ty). This offset is interpreted as a
   --  user-space coordinate according to the CTM in place before the new
   --  call to Translate. In other words, the translation of the
   --  user-space origin takes place after any existing transformation.

   procedure Scale (Cr : Cairo_Context; Sx : Gdouble; Sy : Gdouble);
   --  Cr: a cairo context
   --  Sx: scale factor for the X dimension
   --  Sy: scale factor for the Y dimension
   --
   --  Modifies the current transformation matrix (CTM) by scaling the X
   --  and Y user-space axes by sx and sy respectively. The scaling of
   --  the axes takes place after any existing transformation of user
   --  space.

   procedure Rotate (Cr : Cairo_Context; Angle : Gdouble);
   --  Cr: a cairo context
   --  Angle: Angle (in radians) by which the user-space axes will be
   --  rotated
   --
   --  Modifies the current transformation matrix (CTM) by rotating the
   --  user-space axes by angle radians. The rotation of the axes takes
   --  places after any existing transformation of user space. The
   --  rotation direction for positive angles is from the positive X axis
   --  toward the positive Y axis.

   procedure Transform
     (Cr     : Cairo_Context;
      Matrix : access Cairo_Matrix);
   --  Cr: a cairo context
   --  Matrix: a transformation to be applied to the user-space axes
   --
   --  Modifies the current transformation matrix (CTM) by applying
   --  matrix as an additional transformation. The new transformation of
   --  user space takes place after any existing transformation.

   procedure Set_Matrix
     (Cr     : Cairo_Context;
      Matrix : access Cairo_Matrix);
   --  Cr: a cairo context
   --  Matrix: a transformation Matrix from user space to device space
   --
   --  Modifies the current transformation matrix (CTM) by setting it
   --  equal to matrix.

   procedure Identity_Matrix (Cr : Cairo_Context);
   --  Cr: a cairo context
   --
   --  Resets the current transformation matrix (CTM) by setting it equal
   --  to the identity matrix. That is, the user-space and device-space
   --  axes will be aligned and one user-space unit will transform to one
   --  device-space unit.

   procedure User_To_Device
     (Cr : Cairo_Context;
      X  : access Gdouble;
      Y  : access Gdouble);
   --  Cr: a cairo context
   --  X: X value of coordinate (in/out parameter)
   --  Y: Y value of coordinate (in/out parameter)
   --
   --  Transform a coordinate from user space to device space by
   --  multiplying the given point by the current transformation matrix
   --  (CTM).

   procedure User_To_Device_Distance
     (Cr : Cairo_Context;
      Dx : access Gdouble;
      Dy : access Gdouble);
   --  Cr: a cairo context
   --  Dx: X component of a distance vector (in/out parameter)
   --  Dy: Y component of a distance vector (in/out parameter)
   --
   --  Transform a distance vector from user space to device space. This
   --  function is similar to User_To_Device except that the translation
   --  components of the CTM will be ignored when transforming (Dx,Dy).

   procedure Device_To_User
     (Cr : Cairo_Context;
      X  : access Gdouble;
      Y  : access Gdouble);
   --  Cr: a cairo
   --  X: X value of coordinate (in/out parameter)
   --  Y: Y value of coordinate (in/out parameter)
   --
   --  Transform a coordinate from device space to user space by
   --  multiplying the given point by the inverse of the current
   --  transformation matrix (CTM).

   procedure Device_To_User_Distance
     (Cr : Cairo_Context;
      Dx : access Gdouble;
      Dy : access Gdouble);
   --  Cr: a cairo context
   --  Dx: X component of a distance vector (in/out parameter)
   --  Dy: Y component of a distance vector (in/out parameter)
   --
   --  Transform a distance vector from device space to user space. This
   --  function is similar to Device_To_User except that the
   --  translation components of the inverse CTM will be ignored when
   --  transforming (Dx,dy).

   -------------------
   -- Path creation --
   -------------------

   procedure New_Path (Cr : Cairo_Context);
   --  Cr: a cairo context
   --
   --  Clears the current path. After this call there will be no path and
   --  no current point.

   procedure Move_To (Cr : Cairo_Context; X : Gdouble; Y : Gdouble);
   --  Cr: a cairo context
   --  X: the X coordinate of the new position
   --  Y: the Y coordinate of the new position
   --
   --  Begin a new sub-path. After this call the current point will be (X, Y).

   procedure New_Sub_Path (Cr : Cairo_Context);
   --  Cr: a cairo context
   --
   --  Begin a new sub-path. Note that the existing path is not
   --  affected. After this call there will be no current point.
   --
   --  In many cases, this call is not needed since new sub-paths are
   --  frequently started with Move_To.
   --
   --  A call to New_Sub_Path is particularly useful when
   --  beginning a new sub-path with one of the Arc calls. This
   --  makes things easier as it is no longer necessary to manually
   --  compute the arc's initial coordinates for a call to
   --  Move_To.
   --
   --  Since: 1.2

   procedure Line_To (Cr : Cairo_Context; X : Gdouble; Y : Gdouble);
   --  Cr: a cairo context
   --  X: the X coordinate of the end of the new line
   --  Y: the Y coordinate of the end of the new line
   --
   --  Adds a line to the path from the current point to position (X, Y)
   --  in user-space coordinates. After this call the current point
   --  will be (X, Y).
   --
   --  If there is no current point before the call to Line_To
   --  this function will behave as Move_To (Cr, X, Y).

   procedure Curve_To
     (Cr : Cairo_Context;
      X1 : Gdouble;
      Y1 : Gdouble;
      X2 : Gdouble;
      Y2 : Gdouble;
      X3 : Gdouble;
      Y3 : Gdouble);
   --  Cr: a cairo context
   --  X1: the X coordinate of the first control point
   --  Y1: the Y coordinate of the first control point
   --  X2: the X coordinate of the second control point
   --  Y2: the Y coordinate of the second control point
   --  X3: the X coordinate of the end of the curve
   --  Y3: the Y coordinate of the end of the curve
   --
   --  Adds a cubic Bézier spline to the path from the current point to
   --  position (X3, Y3) in user-space coordinates, using (X1, Y1) and
   --  (X2, Y2) as the control points. After this call the current point
   --  will be (X3, Y3).
   --
   --  If there is no current point before the call to Curve_To
   --  this function will behave as if preceded by a call to
   --  Move_To (Cr, X1, Y1).

   procedure Arc
     (Cr     : Cairo_Context;
      Xc     : Gdouble;
      Yc     : Gdouble;
      Radius : Gdouble;
      Angle1 : Gdouble;
      Angle2 : Gdouble);
   --  Cr: a cairo context
   --  Xc: X position of the center of the arc
   --  Yc: Y position of the center of the arc
   --  Radius: the Radius of the arc
   --  Angle1: the start angle, in radians
   --  Angle2: the end angle, in radians
   --
   --  Adds a circular arc of the given radius to the current path.  The
   --  arc is centered at (Xc, Yc), begins at Angle1 and proceeds in
   --  the direction of increasing angles to end at Angle2. If Angle2 is
   --  less than Angle1 it will be progressively increased by 2*M_PI
   --  until it is greater than Angle1.
   --
   --  If there is a current point, an initial line segment will be added
   --  to the path to connect the current point to the beginning of the
   --  arc. If this initial line is undesired, it can be avoided by
   --  calling New_Sub_Path before calling Arc.
   --
   --  Angles are measured in radians. An angle of 0.0 is in the direction
   --  of the positive X axis (in user space). An angle of M_PI/2.0 radians
   --  (90 degrees) is in the direction of the positive Y axis (in
   --  user space). Angles increase in the direction from the positive X
   --  axis toward the positive Y axis. So with the default transformation
   --  matrix, angles increase in a clockwise direction.
   --
   --  (To convert from degrees to radians, use degrees * (Pi / 180.0))
   --
   --  This function gives the arc in the direction of increasing angles;
   --  see Cairo_Arc_Negative to get the arc in the direction of
   --  decreasing angles.
   --
   --  The arc is circular in user space. To achieve an elliptical arc,
   --  you can scale the current transformation matrix by different
   --  amounts in the X and Y directions. For example, to draw an ellipse
   --  in the box given by X, Y, Width, Height:
   --
   --  Cairo_Save (Cr);
   --  Cairo_Translate (Cr, X + Width / 2.0, Y + Height / 2.0);
   --  Cairo_Scale (Cr, Width / 2.0, Height / 2.0);
   --  Cairo_Arc (Cr, 0.0, 0.0, 1.0, 0.0, 2 * Pi);
   --  Cairo_Restore (Cr);

   procedure Arc_Negative
     (Cr     : Cairo_Context;
      Xc     : Gdouble;
      Yc     : Gdouble;
      Radius : Gdouble;
      Angle1 : Gdouble;
      Angle2 : Gdouble);
   --  Cr: a cairo context
   --  Xc: X position of the center of the arc
   --  Yc: Y position of the center of the arc
   --  Radius: the Radius of the arc
   --  Angle1: the start angle, in radians
   --  Angle2: the end angle, in radians
   --
   --  Adds a circular arc of the given radius to the current path.  The
   --  arc is centered at (Xc, Yc), begins at Angle1 and proceeds in
   --  the direction of decreasing angles to end at Angle2. If Angle2 is
   --  greater than Angle1 it will be progressively decreased by 2*Pi
   --  until it is less than Angle1.
   --
   --  See Arc for more details. This function differs only in the
   --  direction of the arc between the two angles.

   procedure Rel_Move_To (Cr : Cairo_Context; Dx : Gdouble; Dy : Gdouble);
   --  Cr: a cairo context
   --  Dx: the X offset
   --  Dy: the Y offset
   --
   --  Begin a new sub-path. After this call the current point will offset
   --  by (X, Y).
   --
   --  Given a current point of (X, Y), Rel_Move_To (Cr, Dx, Dy)
   --  is logically equivalent to Move_To (Cr, X + Dx, Y + Dy).
   --
   --  It is an error to call this function with no current point. Doing
   --  so will cause cr to shutdown with a status of
   --  Cairo_Status_No_Current_Point.

   procedure Rel_Line_To (Cr : Cairo_Context; Dx : Gdouble; Dy : Gdouble);
   --  Cr: a cairo context
   --  Dx: the X offset to the end of the new line
   --  Dy: the Y offset to the end of the new line
   --
   --  Relative-coordinate version of Line_To. Adds a line to the
   --  path from the current point to a point that is offset from the
   --  current point by (Dx, Dy) in user space. After this call the
   --  current point will be offset by (Dx, Dy).
   --
   --  Given a current point of (X, Y), Rel_Line_To (Cr, Dx, Dy)
   --  is logically equivalent to Cairo_Line_To(Cr, X + Dx, Y + Dy).
   --
   --  It is an error to call this function with no current point. Doing
   --  so will cause cr to shutdown with a status of
   --  Cairo_Status_No_Current_Point.

   procedure Rel_Curve_To
     (Cr  : Cairo_Context;
      Dx1 : Gdouble;
      Dy1 : Gdouble;
      Dx2 : Gdouble;
      Dy2 : Gdouble;
      Dx3 : Gdouble;
      Dy3 : Gdouble);
   --  Cr: a cairo context
   --  Dx1: the X offset to the first control point
   --  Dy1: the Y offset to the first control point
   --  Dx2: the X offset to the second control point
   --  Dy2: the Y offset to the second control point
   --  Dx3: the X offset to the end of the curve
   --  Dy3: the Y offset to the end of the curve
   --
   --  Relative-coordinate version of Cairo_Curve_To. All offsets are
   --  relative to the current point. Adds a cubic Bézier spline to the
   --  path from the current point to a point offset from the current
   --  point by (Dx3, Dy3), using points offset by (Dx1, Dy1) and
   --  (Dx2, Dy2) as the control points. After this call the current
   --  point will be offset by (Dx3, Dy3).
   --
   --  Given a current point of (X, Y), Cairo_Rel_Curve_To(Cr, Dx1,
   --  Dy1, Dx2, Dy2, Dx3, Dy3) is logically equivalent to
   --  Cairo_Curve_To(Cr, X+Dx1, Y+Dy1, X+Dx2, Y+Dy2, X+Dx3, Y+Dy3).
   --
   --  It is an error to call this function with no current point. Doing
   --  so will cause cr to shutdown with a status of
   --  Cairo_Status_No_Current_Point.

   procedure Rectangle
     (Cr     : Cairo_Context;
      X      : Gdouble;
      Y      : Gdouble;
      Width  : Gdouble;
      Height : Gdouble);
   --  Cr: a cairo context
   --  X: the X coordinate of the top left corner of the rectangle
   --  Y: the Y coordinate to the top left corner of the rectangle
   --  Width: the Width of the rectangle
   --  Height: the Height of the rectangle
   --
   --  Adds a closed sub-path rectangle of the given size to the current
   --  path at position (X, Y) in user-space coordinates.
   --
   --  This function is logically equivalent to:
   --
   --  Move_To (Cr, x, Y);
   --  Rel_Line_To (Cr, Width, 0);
   --  Rel_Line_To (Cr, 0, Height);
   --  Rel_Line_To (Cr, -Width, 0);
   --  Close_Path (Cr);

   procedure Close_Path (Cr : Cairo_Context);
   --  Cr: a cairo context
   --
   --  Adds a line segment to the path from the current point to the
   --  beginning of the current sub-path, (the most recent point passed to
   --  Move_To), and closes this sub-path. After this call the
   --  current point will be at the joined endpoint of the sub-path.
   --
   --  The behavior of Close_Path is distinct from simply calling
   --  Line_To with the equivalent coordinate in the case of
   --  stroking. When a closed sub-path is stroked, there are no caps on
   --  the ends of the sub-path. Instead, there is a line join connecting
   --  the final and initial segments of the sub-path.
   --
   --  If there is no current point before the call to Close_Path,
   --  this function will have no effect.
   --
   --  Note: As of cairo version 1.2.4 any call to Close_Path will
   --  place an explicit MOVE_TO element into the path immediately after
   --  the CLOSE_PATH element, (which can be seen in Copy_Path for
   --  example). This can simplify path processing in some cases as it may
   --  not be necessary to save the "last move_to point" during processing
   --  as the MOVE_TO immediately after the CLOSE_PATH will provide that
   --  point.

   procedure Path_Extents
     (Cr : Cairo_Context;
      X1 : access Gdouble;
      Y1 : access Gdouble;
      X2 : access Gdouble;
      Y2 : access Gdouble);
   --  Cr: a cairo context
   --  X1: left of the resulting extents
   --  Y1: top of the resulting extents
   --  X2: right of the resulting extents
   --  Y2: bottom of the resulting extents
   --
   --  Computes a bounding box in user-space coordinates covering the
   --  points on the current path. If the current path is empty, returns
   --  an empty rectangle ((0,0), (0,0)). Stroke parameters, fill rule,
   --  surface dimensions and clipping are not taken into account.
   --
   --  Contrast with Fill_Extents and Stroke_Extents which
   --  return the extents of only the area that would be "inked" by
   --  the corresponding drawing operations.
   --
   --  The result of Path_Extents is defined as equivalent to the
   --  limit of Stroke_Extents with Cairo_Line_Cap_Round as the
   --  line width approaches 0.0, (but never reaching the empty-rectangle
   --  returned by Cairo_Stroke_Extents for a line width of 0.0).
   --
   --  Specifically, this means that zero-area sub-paths such as
   --  Move_To;Line_To segments, (even degenerate cases where the coordinates
   --  to both calls are identical), will be considered as contributing to the
   --  extents. However, a lone Move_To will not contribute to the results of
   --  Path_Extents.
   --
   --  Since: 1.6

   --------------
   -- Painting --
   --------------

   procedure Paint (Cr : Cairo_Context);
   --  Cr: a cairo context
   --
   --  A drawing operator that paints the current source everywhere within
   --  the current clip region.

   procedure Paint_With_Alpha (Cr : Cairo_Context; Alpha : Gdouble);
   --  Cr: a cairo context
   --  Alpha: Alpha value, between 0 (transparent) and 1 (opaque)
   --
   --  A drawing operator that paints the current source everywhere within the
   --  current clip region using a mask of constant alpha value alpha. The
   --  effect is similar to Paint, but the drawing is faded out using the alpha
   --  value.

   procedure Mask (Cr : Cairo_Context; Pattern : Cairo_Pattern);
   --  Cr: a cairo context
   --  Pattern: a Cairo_Pattern
   --
   --  A drawing operator that paints the current source using the alpha
   --  channel of pattern as a mask. (Opaque areas of pattern are painted with
   --  the source, transparent areas are not painted.)

   procedure Mask_Surface
     (Cr        : Cairo_Context;
      Surface   : Cairo_Surface;
      Surface_X : Gdouble;
      Surface_Y : Gdouble);
   --  Cr: a cairo context
   --  Surface: a Cairo_Surface
   --  Surface_X: X coordinate at which to place the origin of surface
   --  Surface_Y: Y coordinate at which to place the origin of surface
   --
   --  A drawing operator that paints the current source
   --  using the alpha channel of surface as a mask. (Opaque
   --  areas of surface are painted with the source, transparent
   --  areas are not painted.)

   procedure Stroke (Cr : Cairo_Context);
   --  Cr: a cairo context
   --
   --  A drawing operator that strokes the current path according to the
   --  current line width, line join, line cap, and dash settings. After
   --  Cairo_Stroke, the current path will be cleared from the cairo
   --  context. See Set_Line_Width, Set_Line_Join, Set_Line_Cap, Set_Dash,
   --  and Stroke_Preserve.
   --
   --  Note: Degenerate segments and sub-paths are treated specially and
   --  provide a useful result. These can result in two different
   --  situations:
   --
   --  1. Zero-length "on" segments set in Set_Dash. If the cap
   --  style is Cairo_Line_Cap_Round or Cairo_Line_Cap_Square then these
   --  segments will be drawn as circular dots or squares respectively. In
   --  the case of Cairo_Line_Cap_Square, the orientation of the squares
   --  is determined by the direction of the underlying path.
   --
   --  2. A sub-path created by Cairo_Move_To followed by either a
   --  Cairo_Close_Path or one or more calls to Cairo_Line_To to the
   --  same coordinate as the Cairo_Move_To. If the cap style is
   --  Cairo_Line_Cap_Round then these sub-paths will be drawn as circular
   --  dots. Note that in the case of Cairo_Line_Cap_Square a degenerate
   --  sub-path will not be drawn at all, (since the correct orientation
   --  is indeterminate).
   --
   --  In no case will a cap style of Cairo_Line_Cap_Butt cause anything
   --  to be drawn in the case of either degenerate segments or sub-paths.

   procedure Stroke_Preserve (Cr : Cairo_Context);
   --  Cr: a cairo context
   --
   --  A drawing operator that strokes the current path according to the
   --  current line width, line join, line cap, and dash settings. Unlike
   --  Cairo_Stroke, Cairo_Stroke_Preserve preserves the path within the
   --  cairo context.
   --
   --  See Set_Line_Width, Set_Line_Join, Set_Line_Cap, Set_Dash, and
   --  Stroke_Preserve.

   procedure Fill (Cr : Cairo_Context);
   --  Cr: a cairo context
   --
   --  A drawing operator that fills the current path according to the
   --  current fill rule, (each sub-path is implicitly closed before being
   --  filled). After Fill, the current path will be cleared from
   --  the cairo context. See Set_Fill_Rule and Fill_Preserve.

   procedure Fill_Preserve (Cr : Cairo_Context);
   --  Cr: a cairo context
   --
   --  A drawing operator that fills the current path according to the
   --  current fill rule, (each sub-path is implicitly closed before being
   --  filled). Unlike Fill, Fill_Preserve preserves the path within the
   --  cairo context.
   --
   --  See Set_Fill_Rule and Fill.

   procedure Copy_Page (Cr : Cairo_Context);
   --  Cr: a cairo context
   --
   --  Emits the current page for backends that support multiple pages, but
   --  doesn't clear it, so, the contents of the current page will be retained
   --  for the next page too.  Use Show_Page if you want to get an
   --  empty page after the emission.
   --
   --  This is a convenience function that simply calls
   --  Cairo.Surface.Copy_Page on Cr's target.

   procedure Show_Page (Cr : Cairo_Context);
   --  Cr: a cairo context
   --
   --  Emits and clears the current page for backends that support multiple
   --  pages.  Use Copy_Page if you don't want to clear the page.
   --
   --  This is a convenience function that simply calls
   --  Cairo.Surface.Show_Page on cr's target.

   ------------------------
   -- Insideness testing --
   ------------------------

   type Cairo_Bool is new Boolean;

   function In_Stroke
     (Cr   : Cairo_Context;
      X    : Gdouble;
      Y    : Gdouble)
      return Cairo_Bool;
   --  Cr: a cairo context
   --  X: X coordinate of the point to test
   --  Y: Y coordinate of the point to test
   --
   --  Tests whether the given point is inside the area that would be
   --  affected by a Stroke operation given the current path and
   --  stroking parameters. Surface dimensions and clipping are not taken
   --  into account.
   --
   --  See Stroke, Set_Line_Width, Set_Line_Join,
   --  Set_Line_Cap, Set_Dash, and Stroke_Preserve.
   --
   --  Return value: A non-zero value if the point is inside, or zero if
   --  outside.

   function In_Fill
     (Cr   : Cairo_Context;
      X    : Gdouble;
      Y    : Gdouble)
      return Cairo_Bool;
   --  Cr: a cairo context
   --  X: X coordinate of the point to test
   --  Y: Y coordinate of the point to test
   --
   --  Tests whether the given point is inside the area that would be
   --  affected by a Fill operation given the current path and
   --  filling parameters. Surface dimensions and clipping are not taken
   --  into account.
   --
   --  See Fill, Set_Fill_Rule and Fill_Preserve.
   --
   --  Return value: A non-zero value if the point is inside, or zero if
   --  outside.

   -------------------------
   -- Rectangular extents --
   -------------------------

   procedure Stroke_Extents
     (Cr : Cairo_Context;
      X1 : access Gdouble;
      Y1 : access Gdouble;
      X2 : access Gdouble;
      Y2 : access Gdouble);
   --  Cr: a cairo context
   --  X1: left of the resulting extents
   --  Y1: top of the resulting extents
   --  X2: right of the resulting extents
   --  Y2: bottom of the resulting extents
   --
   --  Computes a bounding box in user coordinates covering the area that
   --  would be affected, (the "inked" area), by a Stroke
   --  operation given the current path and stroke parameters.
   --  If the current path is empty, returns an empty rectangle ((0,0), (0,0)).
   --  Surface dimensions and clipping are not taken into account.
   --
   --  Note that if the line width is set to exactly zero, then
   --  Stroke_Extents will return an empty rectangle. Contrast with
   --  Path_Extents which can be used to compute the non-empty
   --  bounds as the line width approaches zero.
   --
   --  Note that Stroke_Extents must necessarily do more work to
   --  compute the precise inked areas in light of the stroke parameters,
   --  so Path_Extents may be more desirable for sake of
   --  performance if non-inked path extents are desired.
   --
   --  See Stroke, Set_Line_Width, Set_Line_Join, Set_Line_Cap, Set_Dash, and
   --  Stroke_Preserve.

   procedure Fill_Extents
     (Cr : Cairo_Context;
      X1 : access Gdouble;
      Y1 : access Gdouble;
      X2 : access Gdouble;
      Y2 : access Gdouble);
   --  Cr: a cairo context
   --  X1: left of the resulting extents
   --  Y1: top of the resulting extents
   --  X2: right of the resulting extents
   --  Y2: bottom of the resulting extents
   --
   --  Computes a bounding box in user coordinates covering the area that
   --  would be affected, (the "inked" area), by a Fill operation
   --  given the current path and fill parameters. If the current path is
   --  empty, returns an empty rectangle ((0,0), (0,0)). Surface
   --  dimensions and clipping are not taken into account.
   --
   --  Contrast with Path_Extents, which is similar, but returns
   --  non-zero extents for some paths with no inked area, (such as a
   --  simple line segment).
   --
   --  Note that Fill_Extents must necessarily do more work to
   --  compute the precise inked areas in light of the fill rule, so
   --  Path_Extents may be more desirable for sake of performance
   --  if the non-inked path extents are desired.
   --
   --  See Fill, Set_Fill_Rule and Fill_Preserve.

   --------------
   -- Clipping --
   --------------

   procedure Reset_Clip (Cr : Cairo_Context);
   --  Cr: a cairo context
   --
   --  Reset the current clip region to its original, unrestricted
   --  state. That is, set the clip region to an infinitely large shape
   --  containing the target surface. Equivalently, if infinity is too
   --  hard to grasp, one can imagine the clip region being reset to the
   --  exact bounds of the target surface.
   --
   --  Note that code meant to be reusable should not call
   --  Reset_Clip as it will cause results unexpected by higher-level code
   --  which calls Clip. Consider using Save and Restore around Clip as a more
   --  robust means of temporarily restricting the clip region.

   procedure Clip (Cr : Cairo_Context);
   --  Cr: a cairo context
   --
   --  Establishes a new clip region by intersecting the current clip
   --  region with the current path as it would be filled by Fill
   --  and according to the current fill rule (see Set_Fill_Rule).
   --
   --  After Clip, the current path will be cleared from the cairo
   --  context.
   --
   --  The current clip region affects all drawing operations by
   --  effectively masking out any changes to the surface that are outside
   --  the current clip region.
   --
   --  Calling Clip can only make the clip region smaller, never
   --  larger. But the current clip is part of the graphics state, so a
   --  temporary restriction of the clip region can be achieved by
   --  calling Clip within a Save/Restore
   --  pair. The only other means of increasing the size of the clip
   --  region is Reset_Clip.

   procedure Clip_Preserve (Cr : Cairo_Context);
   --  Cr: a cairo context
   --
   --  Establishes a new clip region by intersecting the current clip
   --  region with the current path as it would be filled by Fill
   --  and according to the current fill rule (see Set_Fill_Rule).
   --
   --  Unlike Clip, Clip_Preserve preserves the path within
   --  the cairo context.
   --
   --  The current clip region affects all drawing operations by
   --  effectively masking out any changes to the surface that are outside
   --  the current clip region.
   --
   --  Calling Clip_Preserve can only make the clip region smaller, never
   --  larger. But the current clip is part of the graphics state, so a
   --  temporary restriction of the clip region can be achieved by
   --  calling Clip_Preserve within a Save/Restore
   --  pair. The only other means of increasing the size of the clip
   --  region is Reset_Clip.

   procedure Clip_Extents
     (Cr : Cairo_Context;
      X1 : access Gdouble;
      Y1 : access Gdouble;
      X2 : access Gdouble;
      Y2 : access Gdouble);
   --  Cr: a cairo context
   --  X1: left of the resulting extents
   --  Y1: top of the resulting extents
   --  X2: right of the resulting extents
   --  Y2: bottom of the resulting extents
   --
   --  Computes a bounding box in user coordinates covering the area inside the
   --  current clip.
   --
   --  Since: 1.4

   type Cairo_Rectangle is record
      X      : aliased Gdouble;
      Y      : aliased Gdouble;
      Width  : aliased Gdouble;
      Height : aliased Gdouble;
   end record;
   --  X: X coordinate of the left side of the rectangle
   --  Y: Y coordinate of the the top side of the rectangle
   --  Width: Width of the rectangle
   --  Height: Height of the rectangle
   --
   --  A data structure for holding a rectangle.
   --
   --  Since: 1.4

   type Cairo_Rectangle_Array is array (Natural) of Cairo_Rectangle;
   type Cairo_Rectangle_Array_Access is access all Cairo_Rectangle_Array;

   type Cairo_Rectangle_List is record
      Status         : aliased Cairo_Status;
      Rectangles     : Cairo_Rectangle_Array_Access;
      --  Warning: for efficiency reasons, Rectangles is a direct mapping to
      --  the C structure. Therefore, there is no bounds checking on this
      --  array, user needs to make sure only to access data between indexes 0
      --  and Num_Rectanges-1.
      Num_Rectangles : aliased Gint;
   end record;
   --  Status: Error Status of the rectangle list
   --  Rectangles: Array containing the Rectangles
   --  Num_Rectangles: Number of rectangles in this list
   --
   --  A data structure for holding a Dynamically allocated
   --  array of rectangles.
   --
   --  Since: 1.4
   pragma Convention (C_Pass_By_Copy, Cairo_Rectangle_List);

   type Cairo_Rectangle_List_Access is access all Cairo_Rectangle_List;

   function Copy_Clip_Rectangle_List
     (Cr   : Cairo_Context)
      return Cairo_Rectangle_List_Access;
   --  Cr: a cairo context
   --
   --  Gets the current clip region as a list of rectangles in user
   --  coordinates.
   --  Never returns NULL.
   --
   --  The status in the list may be Cairo_Status_Clip_Not_Representable to
   --  indicate that the clip region cannot be represented as a list of
   --  user-space rectangles. The status may have other values to indicate
   --  other errors.
   --
   --  Returns: the current clip region as a list of rectangles in user
   --  coordinates,
   --  which should be destroyed using Rectangle_List_Destroy.
   --
   --  Since: 1.4

   procedure Rectangle_List_Destroy
     (Rectangle_List : access Cairo_Rectangle_List);
   --  Rectangle_List: a rectangle list, as obtained from Copy_Clip_Rectangles
   --
   --  Unconditionally frees Rectangle_List and all associated
   --  references. After this call, the Rectangle_List pointer must not
   --  be dereferenced.
   --
   --  Since: 1.4

   -------------------------
   -- Font/Text functions --
   -------------------------

   type Cairo_Scaled_Font is private;
   --  A Cairo_Scaled_Font is a font scaled to a particular size and device
   --  resolution. A Cairo_Scaled_Font is most useful for low-level font
   --  usage where a library or application wants to cache a reference
   --  to a scaled font to speed up the computation of metrics.
   --
   --  There are various types of scaled fonts, depending on the font backend
   --  they use. The type of a scaled font can be queried using
   --  Cairo.Scaled_Font.Get_Type.
   --
   --  Memory management of Cairo_Scaled_Font is done with
   --  Cairo.Scaled_Font.Reference and Cairo.Scaled_Font.Destroy.

   type Cairo_Font_Face is private;
   --  A Cairo_Font_Face specifies all aspects of a font other
   --  than the size or font matrix (a font matrix is used to distort
   --  a font by sheering it or scaling it unequally in the two
   --  directions) . A font face can be set on a Cairo_Context by using
   --  Set_Font_Face; the size and font matrix are set with
   --  Set_Font_Size and Set_Font_Matrix.
   --
   --  There are various types of font faces, depending on the font backend
   --  they use. The type of a font face can be queried using
   --  Cairo.Font_Face.Get_Type.
   --
   --  Memory management of Cairo_Font_Face is done with
   --  Cairo.Font_Face.Reference and Cairo.Font_Face.Destroy.
   --
   --  The Cairo_glyph structure holds information about a single glyph when
   --  drawing or measuring text. A font is (in simple terms) a collection of
   --  shapes used to draw text. A glyph is one of these shapes. There can be
   --  multiple glyphs for a single character (alternates to be used in
   --  different contexts, for example), or a glyph can be a ligature of
   --  multiple characters. Cairo doesn't expose any way of converting input
   --  text into glyphs, so in order to use the Cairo interfaces that take
   --  arrays of glyphs, you must directly access the appropriate underlying
   --  font system.
   --
   --  Note that the offsets given by X and Y are not cumulative. When
   --  drawing or measuring text, each glyph is individually positioned
   --  with respect to the overall origin
   type Cairo_Glyph is record
      Index : aliased Gulong;
      --  Glyph Index in the font. The exact interpretation of the
      --  glyph index depends on the font technology being used.

      X     : aliased Gdouble;
      --  The offset in the X direction between the origin used for
      --  drawing or measuring the string and the origin of this glyph.

      Y     : aliased Gdouble;
      --  The offset in the Y direction between the origin used for drawing or
      --  measuring the string and the origin of this glyph.
   end record;
   pragma Convention (C_Pass_By_Copy, Cairo_Glyph);

   type Cairo_Text_Cluster is record
      Num_Bytes  : aliased Gint;
      --  The number of bytes of UTF-8 text covered by cluster

      Num_Glyphs : aliased Gint;
      --  The number of glyphs covered by cluster
   end record;
   --  The Cairo_text_cluster structure holds information about a single text
   --  cluster. A text cluster is a minimal mapping of some glyphs
   --  corresponding to some UTF-8 text.
   --
   --  For a cluster to be valid, both num_bytes and num_glyphs should
   --  be non-negative, and at least one should be non-zero.
   --  Note that clusters with zero glyphs are not as well supported as
   --  normal clusters.  For example, PDF rendering applications typically
   --  ignore those clusters when PDF text is being selected.
   --
   --  See Show_Text_Glyphs for how clusters are used in advanced
   --  text operations.
   --
   --  Since: 1.8
   pragma Convention (C_Pass_By_Copy, Cairo_Text_Cluster);

   subtype Cairo_Text_Cluster_Flags is Guint;
   --  Specifies properties of a text cluster mapping.
   --
   --  Since: 1.8

   Cairo_Text_Cluster_Flag_Backward : constant Cairo_Text_Cluster_Flags := 1;
   --  The clusters in the cluster array map to glyphs in the glyph array from
   --  end to start.

   type Cairo_Text_Extents is record
      X_Bearing : aliased Gdouble;
      --  The horizontal distance from the origin to the
      --  leftmost part of the glyphs as drawn. Positive if the
      --  glyphs lie entirely to the right of the origin.

      Y_Bearing : aliased Gdouble;
      --  The vertical distance from the origin to the
      --  topmost part of the glyphs as drawn. Positive only if the
      --  glyphs lie completely below the origin; will usually be
      --  negative.

      Width     : aliased Gdouble;
      --  Width of the glyphs as drawn

      Height    : aliased Gdouble;
      --  Height of the glyphs as drawn

      X_Advance : aliased Gdouble;
      --  Distance to advance in the X direction after drawing these glyphs

      Y_Advance : aliased Gdouble;
      --  Distance to advance in the Y direction
      --  after drawing these glyphs. Will typically be zero except
      --  for vertical text layout as found in East-Asian languages.
   end record;
   --  The Cairo_text_extents structure stores the extents of a single glyph
   --  or a string of glyphs in user-space coordinates. Because text extents
   --  are in user-space coordinates, they are mostly, but not entirely,
   --  independent of the current transformation matrix. If you call
   --  Scale (Cr, 2.0, 2.0), text will be drawn twice as big, but the reported
   --  text extents will not be doubled. They will change slightly due to
   --  hinting (so you can't assume that metrics are independent of the
   --  transformation matrix), but otherwise will remain unchanged.
   pragma Convention (C_Pass_By_Copy, Cairo_Text_Extents);

   type Cairo_Font_Extents is record
      Ascent        : aliased Gdouble;
      --  The distance that the font extends above the baseline.
      --  Note that this is not always exactly equal to the maximum
      --  of the extents of all the glyphs in the font, but rather
      --  is picked to express the font designer's intent as to
      --  how the font should align with elements above it.

      Descent       : aliased Gdouble;
      --  The distance that the font extends below the baseline.
      --  This value is positive for typical fonts that include
      --  portions below the baseline. Note that this is not always
      --  exactly equal to the maximum of the extents of all the
      --  glyphs in the font, but rather is picked to express the
      --  font designer's intent as to how the the font should
      --  align with elements below it.

      Height        : aliased Gdouble;
      --  The recommended vertical distance between baselines when
      --  setting consecutive lines of text with the font. This
      --  is greater than ascent+descent by a
      --  quantity known as the line spacing or external leading. When space is
      --  at a premium, most fonts can be set with only a distance of
      --  ascent+descent between lines.

      Max_X_Advance : aliased Gdouble;
      --  The maximum distance in the X direction that
      --  the the origin is advanced for any glyph in the font.

      Max_Y_Advance : aliased Gdouble;
      --  The maximum distance in the Y direction that
      --  the the origin is advanced for any glyph in the font.
      --  this will be zero for normal fonts used for horizontal
      --  writing. (The scripts of East Asia are sometimes written
      --  vertically.)
   end record;
   --  The Cairo_font_extents structure stores metric information for
   --  a font. Values are given in the current user-space coordinate
   --  system.
   --
   --  Because font metrics are in user-space coordinates, they are
   --  mostly, but not entirely, independent of the current transformation
   --  matrix. If you call Scale (Cr, 2.0, 2.0), text will be drawn twice as
   --  big, but the reported text extents will not be doubled. They will
   --  change slightly due to hinting (so you can't assume that metrics are
   --  independent of the transformation matrix), but otherwise will remain
   --  unchanged.
   pragma Convention (C_Pass_By_Copy, Cairo_Font_Extents);

   type Cairo_Font_Slant is (
                             Cairo_Font_Slant_Normal,
                             Cairo_Font_Slant_Italic,
                             Cairo_Font_Slant_Oblique);
   --  Specifies variants of a font face based on their slant.
   pragma Convention (C, Cairo_Font_Slant);

   type Cairo_Font_Weight is (
                              Cairo_Font_Weight_Normal,
                              Cairo_Font_Weight_Bold);
   --  Specifies variants of a font face based on their weight.
   pragma Convention (C, Cairo_Font_Weight);

   type Cairo_Subpixel_Order is
     (Cairo_Subpixel_Order_Default,
      --  Use the default subpixel order for the target device

      Cairo_Subpixel_Order_Rgb,
      --  Subpixel elements are arranged horizontally with red at the left

      Cairo_Subpixel_Order_Bgr,
      --  Subpixel elements are arranged horizontally with blue at the left

      Cairo_Subpixel_Order_Vrgb,
      --  Subpixel elements are arranged vertically with red at the top

      Cairo_Subpixel_Order_Vbgr
      --  Subpixel elements are arranged vertically with blue at the top
     );
   --  The subpixel order specifies the order of color elements within
   --  each pixel on the display device when rendering with an
   --  antialiasing mode of CAIRO_ANTIALIAS_SUBPIXEL.
   pragma Convention (C, Cairo_Subpixel_Order);

   type Cairo_Hint_Style is
     (Cairo_Hint_Style_Default,
      --  Use the default hint style for font backend and target device

      Cairo_Hint_Style_None,
      --  Do not hint outlines

      Cairo_Hint_Style_Slight,
      --  Hint outlines slightly to improve contrast while retaining good
      --  fidelity to the original shapes.

      Cairo_Hint_Style_Medium,
      --  Hint outlines with medium strength giving a compromise between
      --  fidelity to the original shapes and contrast

      Cairo_Hint_Style_Full
      --  Hint outlines to maximize contrast
     );
   --  Specifies the type of hinting to do on font outlines. Hinting
   --  is the process of fitting outlines to the pixel grid in order
   --  to improve the appearance of the result. Since hinting outlines
   --  involves distorting them, it also reduces the faithfulness
   --  to the original outline shapes. Not all of the outline hinting
   --  styles are supported by all font backends.
   --
   --  New entries may be added in future versions.
   pragma Convention (C, Cairo_Hint_Style);

   type Cairo_Hint_Metrics is
     (Cairo_Hint_Metrics_Default,
      --  Hint metrics in the default manner for the font backend and target
      --  device

      Cairo_Hint_Metrics_Off,
      --  Do not hint font metrics

      Cairo_Hint_Metrics_On
      --  Hint font metrics
     );
   --  Specifies whether to hint font metrics; hinting font metrics
   --  means quantizing them so that they are integer values in
   --  device space. Doing this improves the consistency of
   --  letter and line spacing, however it also means that text
   --  will be laid out differently at different zoom factors.
   pragma Convention (C, Cairo_Hint_Metrics);

   type Cairo_Font_Options is private;
   --  An opaque structure holding all options that are used when
   --  rendering fonts.
   --
   --  Individual features of a Cairo_Font_Options can be set or
   --  accessed using functions named
   --  Cairo.Font_Options.Set_<feature_Name> and
   --  Cairo.Font_Options.Get_<feature_Name>, like
   --  Cairo.Font_Options.Set_Antialias and
   --  Cairo.Font_Options.Get_Antialias.
   --
   --  New features may be added to a Cairo_font_options in the
   --  future.  For this reason, Cairo.Font_Options.Copy,
   --  Cairo.Font_Options.Equal, Cairo.Font_Options.Merge, and
   --  Cairo.Font_Options.Hash should be used to copy, check
   --  for equality, merge, or compute a hash value of
   --  Cairo_Font_Options objects.

   --  This interface is for dealing with text as text, not caring about the
   --  font object inside the the Cairo_Context.

   procedure Select_Font_Face
     (Cr     : Cairo_Context;
      Family : String;
      Slant  : Cairo_Font_Slant;
      Weight : Cairo_Font_Weight);
   --  Cr: a Cairo_Context
   --  Family: a font Family name, encoded in UTF-8
   --  Slant: the Slant for the font
   --  Weight: the Weight for the font
   --
   --  Note: The Select_Font_Face function call is part of what
   --  the cairo designers call the "toy" text API. It is convenient for
   --  short demos and simple programs, but it is not expected to be
   --  adequate for serious text-using applications.
   --
   --  Selects a family and style of font from a simplified description as
   --  a family name, slant and weight. Cairo provides no operation to
   --  list available family names on the system (this is a "toy",
   --  remember), but the standard CSS2 generic family names, ("serif",
   --  "sans-serif", "cursive", "fantasy", "monospace"), are likely to
   --  work as expected.
   --
   --  It is expected that most applications will need to use a more
   --  comprehensive font handling and text layout library, (for example,
   --  pango), in conjunction with cairo.
   --
   --  If text is drawn without a call to Select_Font_Face, (nor
   --  Set_Font_Face nor Set_Scaled_Font), the default
   --  family is platform-specific, but is essentially "sans-serif".
   --  Default slant is Cairo_Font_Slant_Normal, and default weight is
   --  Cairo_Font_Weight_Normal.
   --
   --  This function is equivalent to a call to
   --  Cairo.Font_Face.Toy_Font_Face_Create followed by Set_Font_Face.

   procedure Set_Font_Size (Cr : Cairo_Context; Size : Gdouble);
   --  Cr: a Cairo_Context
   --  Size: the new font Size, in user space units
   --
   --  Sets the current font matrix to a scale by a factor of size, replacing
   --  any font matrix previously set with Set_Font_Size or
   --  Set_Font_Matrix. This results in a font size of size user space
   --  units. (More precisely, this matrix will result in the font's
   --  em-square being a size by size square in user space.)
   --
   --  If text is drawn without a call to Set_Font_Size, (nor Set_Font_Matrix
   --  nor Set_Scaled_Font), the default font size is 10.0.

   procedure Set_Font_Matrix
     (Cr     : Cairo_Context;
      Matrix : access Cairo_Matrix);
   --  Cr: a Cairo_Context
   --  Matrix: a Cairo_Matrix describing a transform to be applied to
   --  the current font.
   --
   --  Sets the current font matrix to matrix. The font matrix gives a
   --  transformation from the design space of the font (in this space,
   --  the em-square is 1 unit by 1 unit) to user space. Normally, a
   --  simple scale is used (see Set_Font_Size), but a more
   --  complex font matrix can be used to shear the font
   --  or stretch it unequally along the two axes

   procedure Get_Font_Matrix
     (Cr     : Cairo_Context;
      Matrix : access Cairo_Matrix);
   --  Cr: a Cairo_Context
   --  Matrix: return value for the Matrix
   --
   --  Stores the current font matrix into matrix. See Set_Font_Matrix.

   procedure Set_Font_Options
     (Cr      : Cairo_Context;
      Options : Cairo_Font_Options);
   --  Cr: a Cairo_Context
   --  Options: font Options to use
   --
   --  Sets a set of custom font rendering options for the Cairo_Context.
   --  Rendering options are derived by merging these options with the
   --  options derived from underlying surface; if the value in options
   --  has a default value (like Cairo_Antialias_Default), then the value
   --  from the surface is used.

   procedure Get_Font_Options
     (Cr      : Cairo_Context;
      Options : Cairo_Font_Options);
   --  Cr: a Cairo_Context
   --  Options: a Cairo_Font_Options object into which to store
   --    the retrieved options. All existing values are overwritten
   --
   --  Retrieves font rendering options set via Set_Font_Options.
   --  Note that the returned options do not include any options derived
   --  from the underlying surface; they are literally the options
   --  passed to Set_Font_Options.

   procedure Set_Font_Face
     (Cr        : Cairo_Context;
      Font_Face : Cairo_Font_Face);
   --  Cr: a Cairo_Context
   --  Font_Face: a Cairo_Font_Face, or Null_Font_Face to restore to the
   --  default font
   --
   --  Replaces the current Cairo_Font_Face object in the Cairo_Context with
   --  font_face. The replaced font face in the Cairo_Context will be
   --  destroyed if there are no other references to it.

   function Get_Font_Face (Cr : Cairo_Context) return Cairo_Font_Face;
   --  Cr: a Cairo_Context
   --
   --  Gets the current font face for a Cairo_Context.
   --
   --  Return value: the current font face.  This object is owned by
   --  cairo. To keep a reference to it, you must call
   --  Cairo.Font_Face.Reference.
   --
   --  This function never returns Null_Font_Face. If memory cannot be
   --  allocated, a special "nil" Cairo_Font_Face object will be returned on
   --  which Cairo.Font_Face.Status returns Cairo_Status_No_Memory. Using this
   --  nil object will cause its error state to propagate to other objects it
   --  is passed to, (for example, calling Set_Font_Face with a nil font
   --  will trigger an error that will shutdown the Cairo_Context object).

   procedure Set_Scaled_Font
     (Cr          : Cairo_Context;
      Scaled_Font : access Cairo_Scaled_Font);
   --  Cr: a Cairo_Context
   --  Scaled_Font: a Cairo_Scaled_Font
   --
   --  Replaces the current font face, font matrix, and font options in
   --  the Cairo_Context with those of the Cairo_Scaled_Font.  Except for
   --  some translation, the current CTM of the Cairo_Context should be the
   --  same as that of the Cairo_Scaled_Font, which can be accessed
   --  using Cairo.Scaled_Font.Get_Ctm.
   --
   --  Since: 1.2

   function Get_Scaled_Font (Cr : Cairo_Context) return Cairo_Scaled_Font;
   --  Cr: a Cairo_Context
   --
   --  Gets the current scaled font for a Cairo_Context.
   --
   --  Return value: the current scaled font. This object is owned by
   --  cairo. To keep a reference to it, you must call
   --  Cairo.Scaled_Font.Reference.
   --
   --  This function never returns Null_Font_Face. If memory cannot be
   --  allocated, a special "nil" Cairo_Scaled_Font object will be returned on
   --  which Cairo.Font_Face.Status returns Cairo_Status_No_Memory. Using this
   --  nil object will cause its error state to propagate to other objects it
   --  is passed to, (for example, calling Set_Font_Face with a nil font
   --  will trigger an error that will shutdown the Cairo_Context object).
   --
   --  Since: 1.4

   procedure Show_Text
     (Cr   : Cairo_Context;
      Utf8 : String);
   --  Cr: a cairo context
   --  Utf8: a NUL-terminated string of text encoded in UTF-8, or Null_Ptr
   --
   --  A drawing operator that generates the shape from a string of UTF-8
   --  characters, rendered according to the current Font_Face, Font_Size
   --  (Font_Matrix), and Font_Options.
   --
   --  This function first computes a set of glyphs for the string of
   --  text. The first glyph is placed so that its origin is at the
   --  current point. The origin of each subsequent glyph is offset from
   --  that of the previous glyph by the advance values of the previous
   --  glyph.
   --
   --  After this call the current point is moved to the origin of where
   --  the next glyph would be placed in this same progression. That is,
   --  the current point will be at the origin of the final glyph offset
   --  by its advance values. This allows for easy display of a single
   --  logical string with multiple calls to Show_Text.
   --
   --  Note: The Show_Text function call is part of what the cairo
   --  designers call the "toy" text API. It is convenient for short demos
   --  and simple programs, but it is not expected to be adequate for
   --  serious text-using applications. See Show_Glyphs for the
   --  "real" text display API in cairo.

   procedure Show_Glyphs
     (Cr         : Cairo_Context;
      Glyphs     : access Cairo_Glyph;
      Num_Glyphs : Gint);
   --  Cr: a cairo context
   --  Glyphs: array of Glyphs to show
   --  Num_Glyphs: number of glyphs to show
   --
   --  A drawing operator that generates the shape from an array of glyphs,
   --  rendered according to the current font face, font size
   --  (font matrix), and font options.

   procedure Text_Path
     (Cr   : Cairo_Context;
      Utf8 : String);
   --  Cr: a cairo context
   --  Utf8: a NUL-terminated string of text encoded in UTF-8, or Null_Ptr
   --
   --  Adds closed paths for text to the current path.  The generated
   --  path if filled, achieves an effect similar to that of
   --  Show_Text.
   --
   --  Text conversion and positioning is done similar to Show_Text.
   --
   --  Like Show_Text, After this call the current point is
   --  moved to the origin of where the next glyph would be placed in
   --  this same progression.  That is, the current point will be at
   --  the origin of the final glyph offset by its advance values.
   --  This allows for chaining multiple calls to to Cairo_Text_Path
   --  without having to set current point in between.
   --
   --  Note: The Text_Path function call is part of what the cairo
   --  designers call the "toy" text API. It is convenient for short demos
   --  and simple programs, but it is not expected to be adequate for
   --  serious text-using applications. See Glyph_Path for the
   --  "real" text path API in cairo.

   procedure Text_Extents
     (Cr      : Cairo_Context;
      Utf8    : Interfaces.C.Strings.chars_ptr;
      Extents : access Cairo_Text_Extents);
   --  Cr: a Cairo_Context
   --  Utf8: a NUL-terminated string of text encoded in UTF-8, or Null_Ptr
   --  Extents: a Cairo_Text_Extents object into which the results
   --  will be stored
   --
   --  Gets the extents for a string of text. The extents describe a
   --  user-space rectangle that encloses the "inked" portion of the text,
   --  (as it would be drawn by Show_Text). Additionally, the
   --  x_advance and y_advance values indicate the amount by which the
   --  current point would be advanced by Cairo_Show_Text.
   --
   --  Note that whitespace characters do not directly contribute to the
   --  size of the rectangle (extents.width and extents.height). They do
   --  contribute indirectly by changing the position of non-whitespace
   --  characters. In particular, trailing whitespace characters are
   --  likely to not affect the size of the rectangle, though they will
   --  affect the x_advance and y_advance values.

   procedure Glyph_Extents
     (Cr         : Cairo_Context;
      Glyphs     : access Cairo_Glyph;
      Num_Glyphs : Gint;
      Extents    : access Cairo_Text_Extents);
   --  Cr: a Cairo_Context
   --  Glyphs: an array of Cairo_Glyph objects
   --  Num_Glyphs: the number of elements in glyphs
   --  Extents: a Cairo_Text_Extents object into which the results
   --  will be stored
   --
   --  Gets the extents for an array of glyphs. The extents describe a
   --  user-space rectangle that encloses the "inked" portion of the
   --  glyphs, (as they would be drawn by Show_Glyphs).
   --  Additionally, the X_Advance and Y_Advance values indicate the
   --  amount by which the current point would be advanced by
   --  Show_Glyphs.
   --
   --  Note that whitespace glyphs do not contribute to the size of the
   --  rectangle (Extents.Width and Extents.Height).

   procedure Font_Extents
     (Cr      : Cairo_Context;
      Extents : access Cairo_Font_Extents);
   --  Cr: a Cairo_Context
   --  Extents: a Cairo_Font_Extents object into which the results
   --  will be stored.
   --
   --  Gets the font extents for the currently selected font.

   type Cairo_Font_Type is
     (Cairo_Font_Type_Toy,
      --  The font was created using cairo's toy font api (Since: 1.8)

      Cairo_Font_Type_Ft,
      --  The font is of type FreeType

      Cairo_Font_Type_Win32,
      --  The font is of type Win32

      Cairo_Font_Type_Quartz,
      --  The font is of type Quartz (Since: 1.6)

      Cairo_Font_Type_User
      --  The font was create using cairo's user font api
     );
   --  Cairo_font_type is used to describe the type of a given font
   --  face or scaled font. The font types are also known as "font
   --  backends" within Cairo.
   --
   --  The type of a font face is determined by the function used to
   --  create it, which will generally be of the form
   --  <type>_Font_Face_Create. The font face type
   --  can be queried
   --  with Cairo.Font_Face.Get_Type
   --
   --  The various Cairo_Font_Face functions can be used with a font face
   --  of any type.
   --
   --  The type of a scaled font is determined by the type of the font
   --  face passed to Cairo.Scaled_Font.Create. The scaled font type can
   --  be queried with Cairo.Scaled_Font.Get_Type
   --
   --  The various Cairo_scaled_font functions can be used with scaled
   --  fonts of any type, but some font backends also provide
   --  type-specific functions that must only be called with a scaled font
   --  of the appropriate type. These functions have names that begin with
   --  <type>_Scaled_Font such as Ft_Scaled_Font_Lock_Face.
   --
   --  The behavior of calling a type-specific function with a scaled font
   --  of the wrong type is undefined.
   --
   --  New entries may be added in future versions.
   --
   --  Since: 1.2
   pragma Convention (C, Cairo_Font_Type);

   ---------------------
   -- Query functions --
   ---------------------

   function Get_Operator (Cr : Cairo_Context) return Cairo_Operator;
   --  Cr: a cairo context
   --
   --  Gets the current compositing operator for a cairo context.
   --
   --  Return value: the current compositing operator.

   function Get_Source (Cr : Cairo_Context) return Cairo_Pattern;
   --  Cr: a cairo context
   --
   --  Gets the current source pattern for Cr.
   --
   --  Return value: the current source pattern. This object is owned by
   --  cairo. To keep a reference to it, you must call
   --  Cairo.Pattern.Reference.

   function Get_Tolerance (Cr : Cairo_Context) return Gdouble;
   --  Cr: a cairo context
   --
   --  Gets the current tolerance value, as set by Set_Tolerance.
   --
   --  Return value: the current tolerance value.

   function Get_Antialias (Cr : Cairo_Context) return Cairo_Antialias;
   --  Cr: a cairo context
   --
   --  Gets the current shape antialiasing mode, as set by Set_Antialias.
   --
   --  Return value: the current shape antialiasing mode.

   function Has_Current_Point (Cr : Cairo_Context) return Cairo_Bool;
   --  Cr: a cairo context
   --
   --  Returns whether a current point is defined on the current path.
   --  See Get_Current_Point for details on the current point.
   --
   --  Return value: whether a current point is defined.
   --
   --  Since: 1.6

   procedure Get_Current_Point
     (Cr : Cairo_Context;
      X  : access Gdouble;
      Y  : access Gdouble);
   --  Cr: a cairo context
   --  X: return value for X coordinate of the current point
   --  Y: return value for Y coordinate of the current point
   --
   --  Gets the current point of the current path, which is
   --  conceptually the final point reached by the path so far.
   --
   --  The current point is returned in the user-space coordinate
   --  system. If there is no defined current point or if cr is in an
   --  error status, X and Y will both be set to 0.0. It is possible to
   --  check this in advance with Has_Current_Point.
   --
   --  Most path construction functions alter the current point. See the
   --  following for details on how they affect the current point:
   --  New_Path, New_Sub_Path, Append_Path, Close_Path, Move_To, Line_To,
   --  Curve_To, Rel_Move_To, Rel_Line_To, Rel_Curve_To, Arc, Arc_Negative,
   --  Rectangle, Text_Path, Glyph_Path, Stroke_To_Path.
   --
   --  Some functions use and alter the current point but do not otherwise
   --  change current path: Show_Text.
   --
   --  Some functions unset the current path and as a result, current point:
   --  Fill, Stroke.

   function Get_Fill_Rule (Cr : Cairo_Context) return Cairo_Fill_Rule;
   --  Cr: a cairo context
   --
   --  Gets the current fill rule, as set by Set_Fill_Rule.
   --
   --  Return value: the current fill rule.

   function Get_Line_Width (Cr : Cairo_Context) return Gdouble;
   --  Cr: a cairo context
   --
   --  This function returns the current line width value exactly as set by
   --  Set_Line_Width. Note that the value is unchanged even if
   --  the CTM has changed between the calls to Set_Line_Width and
   --  Get_Line_Width.
   --
   --  Return value: the current line width.

   function Get_Line_Cap (Cr : Cairo_Context) return Cairo_Line_Cap;
   --  Cr: a cairo context
   --
   --  Gets the current line cap style, as set by Set_Line_Cap.
   --
   --  Return value: the current line cap style.

   function Get_Line_Join (Cr : Cairo_Context) return Cairo_Line_Join;
   --  Cr: a cairo context
   --
   --  Gets the current line join style, as set by Set_Line_Join.
   --
   --  Return value: the current line join style.

   function Get_Miter_Limit (Cr : Cairo_Context) return Gdouble;
   --  Cr: a cairo context
   --
   --  Gets the current miter limit, as set by Set_Miter_Limit.
   --
   --  Return value: the current miter limit.

   function Get_Dash_Count (Cr : Cairo_Context) return Gint;
   --  Cr: a Cairo_Context
   --
   --  This function returns the length of the dash array in cr (0 if dashing
   --  is not currently in effect).
   --
   --  See also Set_Dash and Get_Dash.
   --
   --  Return value: the length of the dash array, or 0 if no dash array set.
   --
   --  Since: 1.4

   type Dash_Array_Access is access all Dash_Array;

   procedure Get_Dash
     (Cr     : Cairo_Context;
      Dashes : out Dash_Array_Access;
      Offset : out Gdouble);
   --  Cr: a Cairo_Context
   --  Dashes: return value for the dash array, or null
   --  Offset: return value for the current dash Offset, or null
   --
   --  Gets the current dash array.
   --
   --  Since: 1.4

   procedure Get_Matrix (Cr : Cairo_Context; Matrix : access Cairo_Matrix);
   --  Cr: a cairo context
   --  Matrix: return value for the Matrix
   --
   --  Stores the current transformation matrix (CTM) into matrix.

   function Get_Target (Cr : Cairo_Context) return Cairo_Surface;
   --  Cr: a cairo context
   --
   --  Gets the target surface for the cairo context as passed to Create.
   --
   --  This function will always return a valid pointer, but the result
   --  can be a "nil" surface if cr is already in an error state,
   --  (ie. Cairo_Status /= Cairo_Status_Success).
   --  A nil surface is indicated by
   --  Cairo.Surface.Status/= Cairo_Status_Success.
   --
   --  Return value: the target surface. This object is owned by cairo. To
   --  keep a reference to it, you must call Cairo.Surface.Reference.

   function Get_Group_Target (Cr : Cairo_Context) return Cairo_Surface;
   --  Cr: a cairo context
   --
   --  Gets the current destination surface for the context. This is either
   --  the original target surface as passed to Create or the target
   --  surface for the current group as started by the most recent call to
   --  Push_Group or Push_Group_With_Content.
   --
   --  This function will always return a valid pointer, but the result
   --  can be a "nil" surface if cr is already in an error state,
   --  (ie. Cairo_Status /= Cairo_Status_Success).
   --  A nil surface is indicated by Cairo_Status /= Cairo_Status_Success.
   --
   --  Return value: the target surface. This object is owned by cairo. To
   --  keep a reference to it, you must call Cairo.Surface.Reference.
   --
   --  Since: 1.2

   type Cairo_Path_Data_Type is
     (Cairo_Path_Move_To,    --  A move-to operation
      Cairo_Path_Line_To,    --  A line-to operation
      Cairo_Path_Curve_To,   --  A curve-to operation
      Cairo_Path_Close_Path  --  A close-path operation
     );
   --  Cairo_path_data is used to describe the type of one portion
   --  of a path when represented as a Cairo_Path.
   --  See Cairo_Path_Data for details.
   pragma Convention (C, Cairo_Path_Data_Type);

   type Header_Type is record
      Path_Type : aliased Cairo_Path_Data_Type;
      Length    : aliased Gint;
   end record;
   --  A Path header. See Cairo_Path_Data for details.

   type Point_Type is record
      X : aliased Gdouble;
      Y : aliased Gdouble;
   end record;
   --  A geometrical point. See Cairo_Path_Data for details.

   type Cairo_Path_Data (Discr : Guint := 0) is record
      case Discr is
         when 0 =>
            Header : aliased Header_Type;
         when others =>
            Point : aliased Point_Type;
      end case;
   end record;
   --  Cairo_path_data is used to represent the path data inside a
   --  Cairo_path.
   --
   --  The data structure is designed to try to balance the demands of
   --  efficiency and ease-of-use. A path is represented as an array of
   --  Cairo_Path_Data, which is a union of headers and points.
   --
   --  Each portion of the path is represented by one or more elements in
   --  the array, (one header followed by 0 or more points). The length
   --  value of the header is the number of array elements for the current
   --  portion including the header, (ie. length == 1 +  of points), and
   --  where the number of points for each element type is as follows:
   --
   --      Cairo_Path_Move_To:     1 point
   --      Cairo_Path_Line_To:     1 point
   --      Cairo_Path_Curve_To:    3 points
   --      Cairo_Path_Close_Path:  0 points
   --
   --  The semantics and ordering of the coordinate values are consistent
   --  with Move_To, Line_To, Curve_To, and Close_Path.
   --
   --  Here is sample code for iterating through a Cairo_Path:
   --
   --    declare
   --       J    : Gint;
   --       Path : Cairo_Path;
   --       Data : Cairo_Path_Data;
   --     begin
   --       Path = Copy_Path (Cr);
   --
   --       J := 0;
   --       while J < Path.Num_Data loop
   --          Data := Path.Data(J);
   --
   --          case Data.Header.Path_Type is
   --
   --              when Cairo_Path_Move_To =>
   --                 Do_Move_To_Things (Data(1).Point.X, Data(1).Point.Y);
   --
   --              when Cairo_Path_Line_To =>
   --                 Do_Line_To_Things (Data(1).Point.X, Data(1).Point.Y);
   --
   --              when Cairo_Path_Curve_To =>
   --                 Do_Curve_To_Things (Data(1).Point.X, Data(1).Point.Y,
   --                                     Data(2).Point.X, Data(2).Point.Y,
   --                                     Data(3).Point.X, Data(3).Point.Y);
   --
   --              when Cairo_Path_Curve_To =>
   --                 Do_Close_Path_Things;
   --          end case;
   --
   --          J := J + Path.Data[J].Header.Length;
   --       end loop;
   --
   --       Path_Destroy (Path);
   --    end;
   --
   --  As of cairo 1.4, cairo does not mind if there are more elements in
   --  a portion of the path than needed.  Such elements can be used by
   --  users of the cairo API to hold extra values in the path data
   --  structure.  For this reason, it is recommended that applications
   --  always use Data.Header.Length to iterate over the path data, instead of
   --  hardcoding the number of elements for each element type.

   type Path_Data_Array is array (Natural) of Cairo_Path_Data;
   type Path_Data_Array_Access is access all Path_Data_Array;

   type Cairo_Path is record
      Status   : aliased Cairo_Status;
      Data     : Path_Data_Array_Access;
      --  Warning: for efficiency reasons, Data is a direct mapping to the C
      --  structure. Therefore, there is no bounds checking on this array,
      --  the user needs to make sure only to access data between indexes
      --  0 and Num_Data-1.
      Num_Data : aliased Gint;
   end record;
   type Cairo_Path_Access is access all Cairo_Path;
   --  Status: the current error Status
   --  Data: the elements in the path
   --  Num_Data: the number of elements in the data array
   --
   --  A data structure for holding a path. This data structure serves as the
   --  return value for Copy_Path and Copy_Path_Flat as well the input value
   --  for Append_Path.
   --
   --  See Cairo_Path_Data for hints on how to iterate over the
   --  actual data within the path.
   --
   --  The num_data member gives the number of elements in the data
   --  array. This number is larger than the number of independent path
   --  portions (defined in Cairo_Path_Data_Type), since the data
   --  includes both headers and coordinates for each portion.

   function Copy_Path (Cr : Cairo_Context) return Cairo_Path_Access;
   --  Cr: a cairo context
   --
   --  Creates a copy of the current path and returns it to the user as a
   --  Cairo_Path. See Cairo_Path_Data for hints on how to iterate
   --  over the returned data structure.
   --
   --  This function will always return a valid pointer, but the result
   --  will have no data (Data = null and Num_Data = 0), if
   --  either of the following conditions hold:
   --
   --  -> If there is insufficient memory to copy the path. In this
   --      case Path.Status will be set to Cairo_Status_No_Memory
   --
   --  -> If Cr is already in an error state. In this case
   --     Path.Status will contain the same status that
   --     would be returned by Status.
   --
   --  Return value: the copy of the current path. The caller owns the
   --  returned object and should call Path_Destroy when finished with it.

   function Copy_Path_Flat (Cr : Cairo_Context) return Cairo_Path_Access;
   --  Cr: a cairo context
   --
   --  Gets a flattened copy of the current path and returns it to the
   --  user as a Cairo_Path. See Cairo_Path_Data for hints on
   --  how to iterate over the returned data structure.
   --
   --  This function is like Copy_Path except that any curves
   --  in the path will be approximated with piecewise-linear
   --  approximations, (accurate to within the current tolerance
   --  value). That is, the result is guaranteed to not have any elements
   --  of type Cairo_Path_Curve_To which will instead be replaced by a
   --  series of Cairo_Path_Line_To elements.
   --
   --  This function will always return a valid pointer, but the result will
   --  have no data (Data = null and Num_Data = 0), if either of the following
   --  conditions hold:
   --
   --  -> If there is insufficient memory to copy the path. In this
   --      case Path.Status will be set to Cairo_Status_No_Memory
   --
   --  -> If Cr is already in an error state. In this case
   --     Path.Status will contain the same status that
   --     would be returned by Status.
   --
   --  Return value: the copy of the current path. The caller owns the
   --  returned object and should call Path_Destroy when finished
   --  with it.

   procedure Append_Path
     (Cr   : Cairo_Context;
      Path : access Cairo_Path);
   --  Cr: a cairo context
   --  Path: Path to be appended
   --
   --  Append the path onto the current path. The path may be either the return
   --  value from one of Copy_Path or Copy_Path_Flat or it may be constructed
   --  manually. See Cairo_Path for details on how the path data structure
   --  should be initialized, and note that Path.Status must be initialized to
   --  Cairo_Status_Success.

   procedure Path_Destroy (Path : access Cairo_Path);
   --  Path: a path previously returned by either Copy_Path or Copy_Path_Flat.
   --
   --  Immediately releases all memory associated with Path. After a call
   --  to Path_Destroy the Path pointer is no longer valid and should not be
   --  used further.
   --
   --  Note: Path_Destroy should only be called with an access to a
   --  Cairo_Path returned by a cairo function. Any path that is created
   --  manually (ie. outside of cairo) should be destroyed manually as well.

   --------------------------
   -- Error status queries --
   --------------------------

   function Status (Cr : Cairo_Context) return Cairo_Status;
   --  Cr: a cairo context
   --
   --  Checks whether an error has previously occurred for this context.
   --
   --  Returns: the current status of this context, see Cairo_Status

   Null_Context      : constant Cairo_Context;
   Null_Surface      : constant Cairo_Surface;
   Null_Pattern      : constant Cairo_Pattern;
   Null_Scaled_Font  : constant Cairo_Scaled_Font;
   Null_Font_Face    : constant Cairo_Font_Face;
   Null_Font_Options : constant Cairo_Font_Options;

private

   pragma Convention (C, Cairo_Destroy_Func);
   pragma Convention (C, Cairo_Bool);
   pragma Convention (C, Cairo_Status);
   pragma Convention (C, Cairo_Operator);
   pragma Convention (C, Cairo_Antialias);
   pragma Convention (C, Cairo_Fill_Rule);
   pragma Convention (C, Cairo_Line_Cap);
   pragma Convention (C, Cairo_Line_Join);
   pragma Convention (C, Path_Data_Array_Access);
   pragma Convention (C_Pass_By_Copy, Cairo_Path);
   pragma Convention (C_Pass_By_Copy, Cairo_Rectangle);
   pragma Convention (C, Cairo_Rectangle_Array_Access);

   pragma Convention (C_Pass_By_Copy, Header_Type);
   pragma Convention (C_Pass_By_Copy, Point_Type);
   pragma Convention (C_Pass_By_Copy, Cairo_Path_Data);
   pragma Unchecked_Union (Cairo_Path_Data);

   type Cairo_Context is new System.Address;
   Null_Context : constant Cairo_Context :=
     Cairo_Context (System.Null_Address);
   type Cairo_Surface is new System.Address;
   Null_Surface : constant Cairo_Surface :=
     Cairo_Surface (System.Null_Address);
   type Cairo_Pattern is new System.Address;
   Null_Pattern : constant Cairo_Pattern :=
     Cairo_Pattern (System.Null_Address);
   type Cairo_Scaled_Font is new System.Address;
   Null_Scaled_Font : constant Cairo_Scaled_Font :=
     Cairo_Scaled_Font (System.Null_Address);
   type Cairo_Font_Face is new System.Address;
   Null_Font_Face : constant Cairo_Font_Face :=
     Cairo_Font_Face (System.Null_Address);
   type Cairo_Font_Options is new System.Address;
   Null_Font_Options : constant Cairo_Font_Options :=
     Cairo_Font_Options (System.Null_Address);
   pragma Import (C, Create, "cairo_create");
   pragma Import (C, Reference, "cairo_reference");
   pragma Import (C, Destroy, "cairo_destroy");
   pragma Import (C, Get_Reference_Count, "cairo_get_reference_count");
   pragma Import (C, Get_User_Data, "cairo_get_user_data");
   pragma Import (C, Set_User_Data, "cairo_set_user_data");
   pragma Import (C, Save, "cairo_save");
   pragma Import (C, Restore, "cairo_restore");
   pragma Import (C, Push_Group, "cairo_push_group");
   pragma Import
     (C,
      Push_Group_With_Content,
      "cairo_push_group_with_content");
   pragma Import (C, Pop_Group, "cairo_pop_group");
   pragma Import (C, Pop_Group_To_Source, "cairo_pop_group_to_source");
   pragma Import (C, Set_Operator, "cairo_set_operator");
   pragma Import (C, Set_Source, "cairo_set_source");
   pragma Import (C, Set_Source_Rgb, "cairo_set_source_rgb");
   pragma Import (C, Set_Source_Rgba, "cairo_set_source_rgba");
   pragma Import (C, Set_Source_Surface, "cairo_set_source_surface");
   pragma Import (C, Set_Tolerance, "cairo_set_tolerance");
   pragma Import (C, Set_Antialias, "cairo_set_antialias");
   pragma Import (C, Set_Fill_Rule, "cairo_set_fill_rule");
   pragma Import (C, Set_Line_Width, "cairo_set_line_width");
   pragma Import (C, Set_Line_Cap, "cairo_set_line_cap");
   pragma Import (C, Set_Line_Join, "cairo_set_line_join");
   pragma Import (C, Set_Miter_Limit, "cairo_set_miter_limit");
   pragma Import (C, Translate, "cairo_translate");
   pragma Import (C, Scale, "cairo_scale");
   pragma Import (C, Rotate, "cairo_rotate");
   pragma Import (C, Transform, "cairo_transform");
   pragma Import (C, Set_Matrix, "cairo_set_matrix");
   pragma Import (C, Identity_Matrix, "cairo_identity_matrix");
   pragma Import (C, User_To_Device, "cairo_user_to_device");
   pragma Import
     (C,
      User_To_Device_Distance,
      "cairo_user_to_device_distance");
   pragma Import (C, Device_To_User, "cairo_device_to_user");
   pragma Import
     (C,
      Device_To_User_Distance,
      "cairo_device_to_user_distance");
   pragma Import (C, New_Path, "cairo_new_path");
   pragma Import (C, Move_To, "cairo_move_to");
   pragma Import (C, New_Sub_Path, "cairo_new_sub_path");
   pragma Import (C, Line_To, "cairo_line_to");
   pragma Import (C, Curve_To, "cairo_curve_to");
   pragma Import (C, Arc, "cairo_arc");
   pragma Import (C, Arc_Negative, "cairo_arc_negative");
   pragma Import (C, Rel_Move_To, "cairo_rel_move_to");
   pragma Import (C, Rel_Line_To, "cairo_rel_line_to");
   pragma Import (C, Rel_Curve_To, "cairo_rel_curve_to");
   pragma Import (C, Rectangle, "cairo_rectangle");
   pragma Import (C, Close_Path, "cairo_close_path");
   pragma Import (C, Path_Extents, "cairo_path_extents");
   pragma Import (C, Paint, "cairo_paint");
   pragma Import (C, Paint_With_Alpha, "cairo_paint_with_alpha");
   pragma Import (C, Mask, "cairo_mask");
   pragma Import (C, Mask_Surface, "cairo_mask_surface");
   pragma Import (C, Stroke, "cairo_stroke");
   pragma Import (C, Stroke_Preserve, "cairo_stroke_preserve");
   pragma Import (C, Fill, "cairo_fill");
   pragma Import (C, Fill_Preserve, "cairo_fill_preserve");
   pragma Import (C, Copy_Page, "cairo_copy_page");
   pragma Import (C, Show_Page, "cairo_show_page");
   pragma Import (C, In_Stroke, "cairo_in_stroke");
   pragma Import (C, In_Fill, "cairo_in_fill");
   pragma Import (C, Stroke_Extents, "cairo_stroke_extents");
   pragma Import (C, Fill_Extents, "cairo_fill_extents");
   pragma Import (C, Reset_Clip, "cairo_reset_clip");
   pragma Import (C, Clip, "cairo_clip");
   pragma Import (C, Clip_Preserve, "cairo_clip_preserve");
   pragma Import (C, Clip_Extents, "cairo_clip_extents");
   pragma Import
     (C,
      Copy_Clip_Rectangle_List,
      "cairo_copy_clip_rectangle_list");
   pragma Import (C, Rectangle_List_Destroy, "cairo_rectangle_list_destroy");
   pragma Import (C, Set_Font_Size, "cairo_set_font_size");
   pragma Import (C, Set_Font_Matrix, "cairo_set_font_matrix");
   pragma Import (C, Get_Font_Matrix, "cairo_get_font_matrix");
   pragma Import (C, Set_Font_Options, "cairo_set_font_options");
   pragma Import (C, Get_Font_Options, "cairo_get_font_options");
   pragma Import (C, Set_Font_Face, "cairo_set_font_face");
   pragma Import (C, Get_Font_Face, "cairo_get_font_face");
   pragma Import (C, Set_Scaled_Font, "cairo_set_scaled_font");
   pragma Import (C, Get_Scaled_Font, "cairo_get_scaled_font");
   pragma Import (C, Show_Glyphs, "cairo_show_glyphs");
   --     pragma Import (C, Show_Text_Glyphs, "cairo_show_text_glyphs");
   --     pragma Import (C, Glyph_Path, "cairo_glyph_path");
   pragma Import (C, Text_Extents, "cairo_text_extents");
   pragma Import (C, Glyph_Extents, "cairo_glyph_extents");
   pragma Import (C, Font_Extents, "cairo_font_extents");
   pragma Import (C, Get_Operator, "cairo_get_operator");
   pragma Import (C, Get_Source, "cairo_get_source");
   pragma Import (C, Get_Tolerance, "cairo_get_tolerance");
   pragma Import (C, Get_Antialias, "cairo_get_antialias");
   pragma Import (C, Has_Current_Point, "cairo_has_current_point");
   pragma Import (C, Get_Current_Point, "cairo_get_current_point");
   pragma Import (C, Get_Fill_Rule, "cairo_get_fill_rule");
   pragma Import (C, Get_Line_Width, "cairo_get_line_width");
   pragma Import (C, Get_Line_Cap, "cairo_get_line_cap");
   pragma Import (C, Get_Line_Join, "cairo_get_line_join");
   pragma Import (C, Get_Miter_Limit, "cairo_get_miter_limit");
   pragma Import (C, Get_Dash_Count, "cairo_get_dash_count");
   pragma Import (C, Get_Matrix, "cairo_get_matrix");
   pragma Import (C, Get_Target, "cairo_get_target");
   pragma Import (C, Get_Group_Target, "cairo_get_group_target");
   pragma Import (C, Copy_Path, "cairo_copy_path");
   pragma Import (C, Copy_Path_Flat, "cairo_copy_path_flat");
   pragma Import (C, Append_Path, "cairo_append_path");
   pragma Import (C, Path_Destroy, "cairo_path_destroy");
   pragma Import (C, Status, "cairo_status");

end Cairo;
