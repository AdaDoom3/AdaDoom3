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
--  A Cairo_Surface is the abstract type representing all different drawing
--  targets that cairo can render to. The actual drawings are performed using a
--  Cairo_Context.
--  </description>
--
--  <c_version>1.8.8</c_version>
--  <group>Cairo</group>

with System;

package Cairo.Surface is

   --------------------------
   -- Surface manipulation --
   --------------------------

   type Cairo_Surface_Type is
     (Cairo_Surface_Type_Image,
      Cairo_Surface_Type_Pdf,
      Cairo_Surface_Type_Ps,
      Cairo_Surface_Type_Xlib,
      Cairo_Surface_Type_Xcb,
      Cairo_Surface_Type_Glitz,
      Cairo_Surface_Type_Quartz,
      Cairo_Surface_Type_Win32,
      Cairo_Surface_Type_Beos,
      Cairo_Surface_Type_Directfb,
      Cairo_Surface_Type_Svg,
      Cairo_Surface_Type_Os2,
      Cairo_Surface_Type_Win32_Printing,
      Cairo_Surface_Type_Quartz_Image);
   pragma Convention (C, Cairo_Surface_Type);
   --  Cairo_Surface_Type is used to describe the type of a given
   --  surface. The surface types are also known as "backends" or "surface
   --  backends" within cairo.
   --
   --  The type of a surface is determined by the function used to create
   --  it, which will generally be of the form
   --  Cairo.<type>_Surface_Create,
   --  (though see Cairo.Surface.Create_Similar as well).
   --
   --  The surface type can be queried with Cairo.Surface.Get_Type
   --
   --  The various Cairo_surface functions can be used with surfaces of
   --  any type, but some backends also provide type-specific functions
   --  that must only be called with a surface of the appropriate
   --  type. These functions have names that begin with
   --  Cairo.<type>_Surface.* such as
   --  Cairo.Image_Surface.Get_Width.
   --
   --  The behavior of calling a type-specific function with a surface of
   --  the wrong type is undefined.
   --
   --  New entries may be added in future versions.
   --
   --  Since: 1.2

   function Create_Similar
     (Other   : Cairo_Surface;
      Content : Cairo_Content;
      Width   : Gint;
      Height  : Gint)
      return    Cairo_Surface;
   --  Other: an existing surface used to select the backend of the new surface
   --  Content: the Content for the new surface
   --  Width: Width of the new surface, (in device-space units)
   --  Height: Height of the new surface (in device-space units)
   --
   --  Create a new surface that is as compatible as possible with an
   --  existing surface. For example the new surface will have the same
   --  fallback resolution and font options as other. Generally, the new
   --  surface will also use the same backend as other, unless that is
   --  not possible for some reason. The type of the returned surface may
   --  be examined with Cairo.Surface.Get_Type.
   --
   --  Initially the surface contents are all 0 (transparent if contents
   --  have transparency, black otherwise.)
   --
   --  Return value: a pointer to the newly allocated surface. The caller
   --  owns the surface and should call Cairo.Surface.Destroy when done
   --  with it.
   --
   --  This function always returns a valid pointer, but it will return a
   --  pointer to a "nil" surface if other is already in an error state
   --  or any other error occurs.

   function Reference (Surface : Cairo_Surface) return Cairo_Surface;
   --  Surface: a Cairo_Surface
   --
   --  Increases the reference count on surface by one. This prevents
   --  surface from being destroyed until a matching call to
   --  Cairo.Surface.Destroy is made.
   --
   --  The number of references to a Cairo_Surface can be get using
   --  Cairo.Surface.Get_Reference_Count.
   --
   --  Return value: the referenced Cairo_Surface.

   procedure Finish (Surface : Cairo_Surface);
   --  Surface: the Cairo_Surface to finish
   --
   --  This function finishes the surface and drops all references to
   --  external resources.  For example, for the Xlib backend it means
   --  that cairo will no longer access the drawable, which can be freed.
   --  After calling Cairo.Surface.Finish the only valid operations on a
   --  surface are getting and setting user, referencing and
   --  destroying, and flushing and finishing it.
   --  Further drawing to the surface will not affect the
   --  surface but will instead trigger a Cairo_Status_Surface_Finished
   --  error.
   --
   --  When the last call to Cairo.Surface.Destroy decreases the
   --  reference count to zero, cairo will call Cairo.Surface.Finish if
   --  it hasn't been called already, before freeing the resources
   --  associated with the surface.

   procedure Destroy (Surface : Cairo_Surface);
   --  Surface: a Cairo_Surface
   --
   --  Decreases the reference count on surface by one. If the result is
   --  zero, then surface and all associated resources are freed.  See
   --  Cairo.Surface.Reference.

   function Get_Reference_Count (Surface : Cairo_Surface) return Guint;
   --  Surface: a Cairo_Surface
   --
   --  Returns the current reference count of surface.
   --
   --  Return value: the current reference count of surface.  If the
   --  object is a nil object, 0 will be returned.
   --
   --  Since: 1.4

   function Status (Surface : Cairo_Surface) return Cairo_Status;
   --  Surface: a Cairo_Surface
   --
   --  Checks whether an error has previously occurred for this
   --  surface.
   --
   --  Return value: Cairo_Status_Success, Cairo_Status_Null_Pointer,
   --     Cairo_Status_No_Memory, Cairo_Status_Read_Error,
   --     Cairo_Status_Invalid_Content, Cairo_Status_Invalid_Format, or
   --     Cairo_Status_Invalid_Visual.

   function Get_Type (Surface : Cairo_Surface) return Cairo_Surface_Type;
   --  Surface: a Cairo_Surface
   --
   --  This function returns the type of the backend used to create
   --  a surface. See Cairo_Surface_Type for available types.
   --
   --  Return value: The type of surface.
   --
   --  Since: 1.2

   function Get_Content (Surface : Cairo_Surface) return Cairo_Content;
   --  Surface: a Cairo_Surface
   --
   --  This function returns the content type of surface which indicates
   --  whether the surface contains color and/or alpha information. See
   --  Cairo_Content.
   --
   --  Return value: The content type of surface.
   --
   --  Since: 1.2

   function Get_User_Data
     (Surface : Cairo_Surface;
      Key     : access Cairo_User_Data_Key) return System.Address;
   --  Surface: a Cairo_Surface
   --  Key: the address of the Cairo_User_Data_Key the user data was
   --  attached to
   --
   --  Return user data previously attached to surface using the specified
   --  key.  If no user data has been attached with the given key this
   --  function returns null.
   --
   --  Return value: the user data previously attached or null.

   function Set_User_Data
     (Surface   : Cairo_Surface;
      Key       : access Cairo_User_Data_Key;
      User_Data : System.Address;
      Destroy   : Cairo_Destroy_Func) return Cairo_Status;
   --  Surface: a Cairo_Surface
   --  Key: the address of a Cairo_User_Data_Key to attach the user data to
   --  User_Data: the user data to attach to the surface
   --  Destroy: a Cairo_Destroy_Func which will be called when the
   --  surface is destroyed or when new user data is attached using the
   --  same key.
   --
   --  Attach user data to surface.  To remove user data from a surface,
   --  call this function with the key that was used to set it and null
   --  for data.
   --
   --  Return value: Cairo_Status_Success or Cairo_Status_No_Memory if a
   --  slot could not be allocated for the user data.

   procedure Get_Font_Options
     (Surface : Cairo_Surface;
      Options : access Cairo_Font_Options);
   --  Surface: a Cairo_Surface
   --  Options: a Cairo_Font_Options object into which to store
   --    the retrieved options. All existing values are overwritten
   --
   --  Retrieves the default font rendering options for the surface.
   --  This allows display surfaces to report the correct subpixel order
   --  for rendering on them, print surfaces to disable hinting of
   --  metrics and so forth. The result can then be used with
   --  Cairo.Scaled_Font.Create.

   procedure Flush (Surface : Cairo_Surface);
   --  Surface: a Cairo_Surface
   --
   --  Do any pending drawing for the surface and also restore any
   --  temporary modification's cairo has made to the surface's
   --  state. This function must be called before switching from
   --  drawing on the surface with cairo to drawing on it directly
   --  with native APIs. If the surface doesn't support direct access,
   --  then this function does nothing.

   procedure Mark_Dirty (Surface : Cairo_Surface);
   --  Surface: a Cairo_Surface
   --
   --  Tells cairo that drawing has been done to surface using means other
   --  than cairo, and that cairo should reread any cached areas. Note
   --  that you must call Cairo.Surface.Flush before doing such drawing.

   procedure Mark_Dirty_Rectangle
     (Surface : Cairo_Surface;
      X       : Gint;
      Y       : Gint;
      Width   : Gint;
      Height  : Gint);
   --  Surface: a Cairo_Surface
   --  X: X coordinate of dirty rectangle
   --  Y: Y coordinate of dirtY rectangle
   --  Width: Width of dirty rectangle
   --  Height: Height of dirty rectangle
   --
   --  Like Cairo.Surface.Mark_Dirty, but drawing has been done only to
   --  the specified rectangle, so that cairo can retain cached contents
   --  for other parts of the surface.
   --
   --  Any cached clip set on the surface will be reset by this function,
   --  to make sure that future cairo calls have the clip set that they
   --  expect.

   procedure Set_Device_Offset
     (Surface  : Cairo_Surface;
      X_Offset : Gdouble;
      Y_Offset : Gdouble);
   --  Surface: a Cairo_Surface
   --  X_Offset: the offset in the X direction, in device units
   --  Y_Offset: the offset in the Y direction, in device units
   --
   --  Sets an offset that is added to the device coordinates determined
   --  by the CTM when drawing to surface. One use case for this function
   --  is when we want to create a Cairo_Surface that redirects drawing
   --  for a portion of an onscreen surface to an offscreen surface in a
   --  way that is completely invisible to the user of the cairo
   --  API. Setting a transformation via Cairo.Translate isn't
   --  sufficient to do this, since functions like
   --  Cairo.Device_To_User will expose the hidden offset.
   --
   --  Note that the offset affects drawing to the surface as well as
   --  using the surface in a source pattern.

   procedure Get_Device_Offset
     (Surface  : Cairo_Surface;
      X_Offset : access Gdouble;
      Y_Offset : access Gdouble);
   --  Surface: a Cairo_Surface
   --  X_Offset: the offset in the X direction, in device units
   --  Y_Offset: the offset in the Y direction, in device units
   --
   --  This function returns the previous device offset set by
   --  Cairo.Surface.Set_Device_Offset.
   --
   --  Since: 1.2

   procedure Set_Fallback_Resolution
     (Surface           : Cairo_Surface;
      X_Pixels_Per_Inch : Gdouble;
      Y_Pixels_Per_Inch : Gdouble);
   --  Surface: a Cairo_Surface
   --  X_Pixels_Per_Inch: horizontal setting for pixels per inch
   --  Y_Pixels_Per_Inch: vertical setting for pixels per inch
   --
   --  Set the horizontal and vertical resolution for image fallbacks.
   --
   --  When certain operations aren't supported natively by a backend,
   --  cairo will fallback by rendering operations to an image and then
   --  overlaying that image onto the output. For backends that are
   --  natively vector-oriented, this function can be used to set the
   --  resolution used for these image fallbacks, (larger values will
   --  result in more detailed images, but also larger file sizes).
   --
   --  Some examples of natively vector-oriented backends are the ps, pdf,
   --  and svg backends.
   --
   --  For backends that are natively raster-oriented, image fallbacks are
   --  still possible, but they are always performed at the native
   --  device resolution. So this function has no effect on those
   --  backends.
   --
   --  Note: The fallback resolution only takes effect at the time of
   --  completing a page (with Cairo.Show_Page or Cairo.Copy_Page) so
   --  there is currently no way to have more than one fallback resolution
   --  in effect on a single page.
   --
   --  The default fallback resoultion is 300 pixels per inch in both
   --  dimensions.
   --
   --  Since: 1.2

   procedure Get_Fallback_Resolution
     (Surface           : Cairo_Surface;
      X_Pixels_Per_Inch : access Gdouble;
      Y_Pixels_Per_Inch : access Gdouble);
   --  Surface: a Cairo_Surface
   --  X_Pixels_Per_Inch: horizontal pixels per inch
   --  Y_Pixels_Per_Inch: vertical pixels per inch
   --
   --  This function returns the previous fallback resolution set by
   --  Cairo.Surface.Set_Fallback_Resolution, or default fallback
   --  resolution if never set.
   --
   --  Since: 1.8

   procedure Copy_Page (Surface : Cairo_Surface);
   --  Surface: a Cairo_Surface
   --
   --  Emits the current page for backends that support multiple pages,
   --  but doesn't clear it, so that the contents of the current page will
   --  be retained for the next page.  Use Cairo.Surface.Show_Page if you
   --  want to get an empty page after the emission.
   --
   --  There is a convenience function for this that takes a Cairo_Context,
   --  namely Cairo.Copy_Page.
   --
   --  Since: 1.6

   procedure Show_Page (Surface : Cairo_Surface);
   --  Surface: a Cairo_Surface
   --
   --  Emits and clears the current page for backends that support multiple
   --  pages.  Use Cairo.Surface.Copy_Page if you don't want to clear the page.
   --
   --  There is a convenience function for this that takes a Cairo_Context,
   --  namely Cairo_Show_Page.
   --
   --  Since: 1.6

   function Has_Show_Text_Glyphs
     (Surface : Cairo_Surface)
      return    Cairo_Bool;
   --  Surface: a Cairo_Surface
   --
   --  Returns whether the surface supports
   --  sophisticated Cairo_Show_Text_Glyphs operations.  That is,
   --  whether it actually uses the provided text and cluster data
   --  to a Cairo_Show_Text_Glyphs call.
   --
   --  Note: Even if this function returns FALSE, a
   --  Cairo_Show_Text_Glyphs operation targeted at surface will
   --  still succeed.  It just will
   --  act like a Cairo_Show_Glyphs operation.  Users can use this
   --  function to avoid computing UTF-8 text and cluster mapping if the
   --  target surface does not use it.
   --
   --  Return value: TRUE if surface supports
   --                Cairo_Show_Text_Glyphs, FALSE otherwise
   --
   --  Since: 1.8

private

   pragma Import (C, Create_Similar, "cairo_surface_create_similar");
   pragma Import (C, Reference, "cairo_surface_reference");
   pragma Import (C, Finish, "cairo_surface_finish");
   pragma Import (C, Destroy, "cairo_surface_destroy");
   pragma Import
     (C,
      Get_Reference_Count,
      "cairo_surface_get_reference_count");
   pragma Import (C, Status, "cairo_surface_status");
   pragma Import (C, Get_Type, "cairo_surface_get_type");
   pragma Import (C, Get_Content, "cairo_surface_get_content");
   pragma Import (C, Get_User_Data, "cairo_surface_get_user_data");
   pragma Import (C, Set_User_Data, "cairo_surface_set_user_data");
   pragma Import (C, Get_Font_Options, "cairo_surface_get_font_options");
   pragma Import (C, Flush, "cairo_surface_flush");
   pragma Import (C, Mark_Dirty, "cairo_surface_mark_dirty");
   pragma Import
     (C,
      Mark_Dirty_Rectangle,
      "cairo_surface_mark_dirty_rectangle");
   pragma Import (C, Set_Device_Offset, "cairo_surface_set_device_offset");
   pragma Import (C, Get_Device_Offset, "cairo_surface_get_device_offset");
   pragma Import
     (C,
      Set_Fallback_Resolution,
      "cairo_surface_set_fallback_resolution");
   pragma Import
     (C,
      Get_Fallback_Resolution,
      "cairo_surface_get_fallback_resolution");
   pragma Import (C, Copy_Page, "cairo_surface_copy_page");
   pragma Import (C, Show_Page, "cairo_surface_show_page");
   pragma Import
     (C,
      Has_Show_Text_Glyphs,
      "cairo_surface_has_show_text_glyphs");

end Cairo.Surface;
