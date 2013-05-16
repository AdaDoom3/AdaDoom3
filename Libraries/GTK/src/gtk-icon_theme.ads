-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2006-2013, AdaCore                   --
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
--  Gtk_Icon_Theme provides a facility for looking up icons by name and size.
--  The main reason for using a name rather than simply providing a filename is
--  to allow different icons to be used depending on what icon theme is
--  selecetd by the user. The operation of icon themes on Linux and Unix
--  follows the Icon Theme Specification. There is a default icon theme, named
--  hicolor where applications should install their icons, but more additional
--  application themes can be installed as operating system vendors and users
--  choose.
--
--  Named icons are similar to the Themeable Stock Images(3) facility, and the
--  distinction between the two may be a bit confusing. A few things to keep in
--  mind:
--
--  - Stock images usually are used in conjunction with Stock Items(3)., such
--    as STOCK_OK or STOCK_OPEN. Named icons are easier to set up and
--    therefore are more useful for new icons that an application wants to add,
--    such as application icons or window icons.
--
--  - Stock images can only be loaded at the symbolic sizes defined by the
--    Gtk_Icon_Size enumeration, or by custom sizes defined by
--    Gtk.Icon_Factory.Icon_Size_Register, while named icons are more flexible
--    and any pixel size can be specified.
--
--  - Because stock images are closely tied to stock items, and thus to actions
--    in the user interface, stock images may come in multiple variants for
--    different widget states or writing directions.
--
--  A good rule of thumb is that if there is a stock image for what you want to
--  use, use it, otherwise use a named icon. It turns out that internally stock
--  images are generally defined in terms of one or more named icons. (An
--  example of the more than one case is icons that depend on writing
--  direction; STOCK_GO_FORWARD uses the two themed icons
--  "gtk-stock-go-forward-ltr" and "gtk-stock-go-forward-rtl".)
--
--  In many cases, named themes are used indirectly, via Gtk_Image or stock
--  items, rather than directly, but looking up icons directly is also simple.
--  The Gtk_Icon_Theme object acts as a database of all the icons in the
--  current theme. You can create new Gtk_Icon_Theme objects, but its much more
--  efficient to use the standard icon theme for the Gdk_Screen so that the
--  icon information is shared with other people looking up icons. In the case
--  where the default screen is being used, looking up an icon can be as simple
--  as:
--      Theme := Get_Default;
--      Pixbuf := Load_Icon (Theme, "my-icon-name", 48, 0, Error);
--      if Pixbuf = null then
--         Put_Line ("Error " & Get_Message (Error);
--         Error_Free (Error);
--      end if;
--  </description>
--  <c_version>2.16.6</c_version>
--  <group>Display widgets</group>
--  <see>Gtk.Icon_Factory</see>

with Gdk.Pixbuf;
with Gdk.Rectangle;
with Gdk.Types;
with Glib.Error;
with Glib.G_Icon;
with Glib.Object;
with Gtk.Enums;
with GNAT.Strings;

package Gtk.Icon_Theme is

   type Gtk_Icon_Theme_Record is new Glib.Object.GObject_Record with
     null record;
   type Gtk_Icon_Theme is access all Gtk_Icon_Theme_Record'Class;

   type Gtk_Icon_Lookup_Flags is mod Gint'Last;
   Icon_Lookup_No_Svg      : constant Gtk_Icon_Lookup_Flags := 2 ** 0;
   Icon_Lookup_Force_Svg   : constant Gtk_Icon_Lookup_Flags := 2 ** 1;
   Icon_Lookup_Use_Builtin : constant Gtk_Icon_Lookup_Flags := 2 ** 2;
   --  Used to specify options for Lookup_Icon.
   --  No_Svg: never return SVG icons, even if gdk-pixbuf supports them.
   --  Force_Svg: return SVG even if gdk-pixbuf doesn't support them.
   --  Use_Builtin: including builtin icons as well as files.

   ---------------
   -- Icon_Info --
   ---------------

   type Gtk_Icon_Info is new Glib.C_Proxy;

   function Icon_Info_Get_Type return GType;

   function New_For_Pixbuf
     (Icon_Theme : access Gtk_Icon_Theme_Record;
      Pixbuf     : Gdk.Pixbuf.Gdk_Pixbuf)
      return Gtk_Icon_Info;
   --  Creates a Gtk_Icon_Info for a Gtk_Pixbuf.

   function Copy (Icon_Info : Gtk_Icon_Info) return Gtk_Icon_Info;
   --  Make a copy of a Icon_Info

   procedure Free (Icon_Info : Gtk_Icon_Info);
   --  Free a Gtk_Icon_Info and associated information

   function Get_Attach_Points
     (Icon_Info : Gtk_Icon_Info) return Gdk.Types.Gdk_Points_Array;
   --  Fetches the set of attach points for an icon. An attach point is a
   --  location in the icon that can be used as anchor points for attaching
   --  emblems or overlays to the icon.

   function Get_Base_Size (Icon_Info : Gtk_Icon_Info) return Gint;
   --  Gets the base size for the icon. The base size is a size for the icon
   --  that was specified by the icon theme creator. This may be different than
   --  the actual size of image; an example of this is small emblem icons that
   --  can be attached to a larger icon. These icons will be given the same
   --  base size as the larger icons to which they are attached.
   --
   --  Return value: the base size, or 0, if no base size is known for the
   --  icon.

   function Get_Builtin_Pixbuf
     (Icon_Info : Gtk_Icon_Info) return Gdk.Pixbuf.Gdk_Pixbuf;
   --  Gets the built-in image for this icon, if any. To allow GTK+ to use
   --  built in icon images, you must pass the %GTK_ICON_LOOKUP_USE_BUILTIN to
   --  Gtk.Icon_Theme.Lookup_Icon.
   --
   --  Return value: the built-in image pixbuf, or null. No extra reference is
   --  added to the returned pixbuf, so if you want to keep it around, you must
   --  use Ref. The returned image must not be modified.

   function Get_Display_Name (Icon_Info : Gtk_Icon_Info) return String;
   --  Gets the display name for an icon. A display name is a string to be used
   --  in place of the icon name in a user visible context like a list of
   --  icons.

   procedure Get_Embedded_Rect
     (Icon_Info              : Gtk_Icon_Info;
      Rectangle              : in out Gdk.Rectangle.Gdk_Rectangle;
      Has_Embedded_Rectangle : out Boolean);
   --  Gets the coordinates of a rectangle within the icon that can be used for
   --  display of information such as a preview of the contents of a text file.
   --  See Set_Raw_Coordinates for further information about the coordinate
   --  system.
   --  Rectangle is only set if Has_Embedded_Rectangle is set to True.

   function Get_Filename (Icon_Info : Gtk_Icon_Info) return String;
   --  Gets the filename for the icon. If the %GTK_ICON_LOOKUP_USE_BUILTIN flag
   --  was passed to Lookup_Icon, there may be no filename if a builtin icon is
   --  returned; in this case, you should use Get_Builtin_Pixbuf.
   --  Return value: the filename for the icon, or "" if Get_Builtin_Pixbuf()
   --  should be used instead.

   function Load_Icon
     (Icon_Info : Gtk_Icon_Info;
      Error     : Glib.Error.GError_Access := null)
      return Gdk.Pixbuf.Gdk_Pixbuf;
   --  Renders an icon previously looked up in an icon theme using Lookup_Icon;
   --  the size will be based on the size passed to Lookup_Icon. Note that the
   --  resulting pixbuf may not be exactly this size; an icon theme may have
   --  icons that differ slightly from their nominal sizes, and in addition
   --  GTK+ will avoid scaling icons that it considers sufficiently close to
   --  the requested size or for which the source image would have to be scaled
   --  up too far. (This maintains sharpness.)
   --  Return value: the rendered icon; this may be a newly created icon or a
   --  new reference to an internal icon, so you must not modify the icon. Use
   --  Unref to release your reference to the icon.

   procedure Set_Raw_Coordinates
     (Icon_Info       : Gtk_Icon_Info;
      Raw_Coordinates : Boolean);
   --  Sets whether the coordinates returned by Get_Embedded_Rect and
   --  Get_Attach_Points should be returned in their original form as specified
   --  in the icon theme, instead of scaled appropriately for the pixbuf
   --  returned by Load_Icon.
   --
   --  Raw coordinates are somewhat strange; they are specified to be with
   --  respect to the unscaled pixmap for PNG and XPM icons, but for SVG icons,
   --  they are in a 1000x1000 coordinate space that is scaled to the final
   --  size of the icon. You can determine if the icon is an SVG icon by using
   --  Get_Filename, and seeing if it is non-empty and ends in '.svg'.
   --
   --  This function is provided primarily to allow compatibility wrappers
   --  for older API's, and is not expected to be useful for applications.

   -----------------
   -- Search path --
   -----------------

   Default_Theme_Name : constant String := "hicolor";

   procedure Set_Search_Path
     (Icon_Theme : access Gtk_Icon_Theme_Record;
      Path       : GNAT.Strings.String_List);
   --  Sets the search path for the icon theme object. When looking for an icon
   --  theme, GTK+ will search for a subdirectory of one or more of the
   --  directories in Path with the same name as the icon theme. (Themes from
   --  multiple of the path elements are combined to allow themes to be
   --  extended by adding icons in the user's home directory.)
   --
   --  In addition if an icon found isn't found either in the current icon
   --  theme or the default icon theme, and an image file with the right name
   --  is found directly in one of the elements of Path, then that image will
   --  be used for the icon name. (This is legacy feature, and new icons should
   --  be put into the default icon theme, which is called DEFAULT_THEME_NAME,
   --  rather than directly on the icon path.)

   procedure Append_Search_Path
     (Icon_Theme : access Gtk_Icon_Theme_Record; Path : String);
   procedure Prepend_Search_Path
     (Icon_Theme : access Gtk_Icon_Theme_Record;
      Path       : String);
   --  Appends or prepends a directory to the search path.
   --  Set_Search_Path.

   function Get_Search_Path
     (Icon_Theme : access Gtk_Icon_Theme_Record)
      return GNAT.Strings.String_List;
   --  Gets the current search path

   -----------------
   -- Icon themes --
   -----------------

   procedure Gtk_New    (Theme : out Gtk_Icon_Theme);
   procedure Initialize (Theme : access Gtk_Icon_Theme_Record'Class);
   --  Creates a new icon theme object. Icon theme objects are used to lookup
   --  up an icon by name in a particular icon theme. Usually, you'll want to
   --  use Get_Default or Get_For_Screen rather than creating a new icon theme
   --  object for scratch.

   function Get_Type return GType;
   --  Return the internal type associated with icon themes

   procedure Add_Builtin_Icon
     (Icon_Name : String;
      Size      : Gint;
      Pixbuf    : Gdk.Pixbuf.Gdk_Pixbuf);
   --  Registers a built-in icon for icon theme lookups. The idea of built-in
   --  icons is to allow an application or library that uses themed icons to
   --  function requiring files to be present in the file system. For instance,
   --  the default images for all of GTK+'s stock icons are registered as
   --  built-icons.
   --
   --  In general, if you use Add_Builtin_Icon you should also install the icon
   --  in the icon theme, so that the icon is generally available.
   --
   --  This function will generally be used with pixbufs loaded via
   --  Gdk.Pixbuf.Gdk_New_From-Inline.

   function Choose_Icon
     (Icon_Theme : access Gtk_Icon_Theme_Record;
      Icon_Names : GNAT.Strings.String_List;
      Size       : Gint;
      Flags      : Gtk_Icon_Lookup_Flags)
      return Gtk_Icon_Info;
   --  Looks up a named icon and returns a structure containing
   --  information such as the filename of the icon.  Returns null if
   --  the icon wasn't found.  The returned value can then be rendered
   --  into a pixbuf using Load_Icon.  There is also a Load_Icon
   --  subprogram that combines these two steps if all you need is the
   --  pixbuf.
   --
   --  If Icon_Names contains more than one name, this function
   --  tries them all in the given order before falling back to
   --  inherited icon themes.
   --
   --  Free the returned value with Free.

   function Get_Default return Gtk_Icon_Theme;
   --  Gets the icon theme for the default screen. See Get_For_Screen. Return
   --  value: A unique Gtk_Icon_Theme associated with the default screen. This
   --  icon theme is associated with the screen and can be used as long as the
   --  screen is open. Do not ref or unref it.

   function Get_Example_Icon_Name
     (Icon_Theme : access Gtk_Icon_Theme_Record) return String;
   --  Gets the name of an icon that is representative of the current theme
   --  (for instance, to use when presenting a list of themes to the user.)

   function Get_Icon_Sizes
     (Icon_Theme : access Gtk_Icon_Theme_Record;
      Icon_Name  : String) return Glib.Gint_Array;
   --  Returns an array of integers describing the sizes at which the icon is
   --  available without scaling. A size of -1 means that the icon is available
   --  in a scalable format.

   function Has_Icon
     (Icon_Theme : access Gtk_Icon_Theme_Record;
      Icon_Name  : String) return Boolean;
   --  Checks whether an icon theme includes an icon for a particular name.

   function List_Icons
     (Icon_Theme : access Gtk_Icon_Theme_Record;
      Context    : String := "") return Gtk.Enums.String_List.Glist;
   --  Lists the icons in the current icon theme. Only a subset of the icons
   --  can be listed by providing a context string. The set of values for the
   --  context string is system dependent, but will typically include such
   --  values as "Applications" and "MimeTypes".
   --  You must free the list with Gtk.Enums.Free_String_List.

   function Load_Icon
     (Icon_Theme : access Gtk_Icon_Theme_Record;
      Icon_Name  : String;
      Size       : Glib.Gint;
      Flags      : Gtk_Icon_Lookup_Flags)
      return Gdk.Pixbuf.Gdk_Pixbuf;
   --  Looks up an icon in an icon theme, scales it to the given size and
   --  renders it into a pixbuf. This is a convenience function; if more
   --  details about the icon are needed, use Lookup_Icon or Choose_Icon
   --  followed by Load_Icon.
   --
   --  Note that you probably want to listen for icon theme changes and update
   --  the icon. This is usually done by connecting to the GtkWidget::style-set
   --  signal. If for some reason you do not want to update the icon when the
   --  icon theme changes, you should consider using Gdk.Pixbuf.Copy to make a
   --  private copy of the pixbuf returned by this function. Otherwise GTK+ may
   --  need to keep the old icon theme loaded, which would be a waste of
   --  memory.
   --
   --  Return value: the rendered icon; this may be a newly created icon or a
   --  new reference to an internal icon, so you must not modify the icon. Use
   --  Unref to release your reference to the icon.

   function Lookup_By_Gicon
     (Icon_Theme : access Gtk_Icon_Theme_Record;
      Icon       : Glib.G_Icon.G_Icon;
      Size       : Gint;
      Flags      : Gtk_Icon_Lookup_Flags)
      return Gtk_Icon_Info;
   --  Looks up an icon and returns a structure containing
   --  information such as the filename of the icon.
   --  The icon can then be rendered into a pixbuf using
   --  Load_Icon.  Returns null if the icon wasn't found.
   --  Free the returned value with Free.

   function Lookup_Icon
     (Icon_Theme : access Gtk_Icon_Theme_Record;
      Icon_Name  : String;
      Size       : Glib.Gint;
      Flags      : Gtk_Icon_Lookup_Flags)
      return Gtk_Icon_Info;
   --  Looks up a named icon and returns a structure containing information
   --  such as the filename of the icon. The icon can then be rendered into a
   --  pixbuf using Load_Icon. (Load_Icon combines these two steps if all you
   --  need is the pixbuf)
   --  Free the returned value with Free.

   function Rescan_If_Needed
     (Icon_Theme : access Gtk_Icon_Theme_Record) return Boolean;
   --  Checks to see if the icon theme has changed; if it has, any
   --  currently cached information is discarded and will be reloaded
   --  next time Icon_Theme is accessed.

   procedure Set_Custom_Theme
     (Icon_Theme : access Gtk_Icon_Theme_Record;
      Theme_Name : String);
   --  Sets the name of the icon theme that the Gtk_Icon_Theme object uses
   --  overriding system configuration. This function cannot be called on the
   --  icon theme objects returned from Get_Default and Get_For_Screen.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "changed"
   --    procedure Handler (Theme : access Gtk_Icon_Theme_Record'Class);
   --    Emitted when the current icon theme is switched or GTK+ detects that a
   --    change has occurred in the contents of the current icon theme.
   --  </signals>

   Signal_Changed : constant Glib.Signal_Name := "changed";

private
   pragma Import (C, Free, "gtk_icon_info_free");
   pragma Import (C, Get_Base_Size, "gtk_icon_info_get_base_size");
   pragma Import (C, Copy, "gtk_icon_info_copy");
   pragma Import (C, Get_Type, "gtk_icon_theme_get_type");
   pragma Import (C, Icon_Info_Get_Type, "gtk_icon_info_get_type");
end Gtk.Icon_Theme;

--  Binding will be provided later:
--  No binding: gtk_icon_theme_error_quark
--  No binding: gtk_icon_theme_get_for_screen
--  No binding: gtk_icon_theme_set_screen
