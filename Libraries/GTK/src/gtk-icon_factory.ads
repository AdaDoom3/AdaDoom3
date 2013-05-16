-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                   Copyright (C) 2004-2013, AdaCore                --
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
--  Browse the available stock icons in the list of stock IDs found here. You
--  can also use the gtk-demo application for this purpose.
--
--  An icon factory manages a collection of Gtk_Icon_Set; a Gtk_Icon_Set
--  manages set of variants of a particular icon (i.e. a Gtk_Icon_Set contains
--  variants for different sizes and widget states). Icons in an icon factory
--  are named by a stock ID, which is a simple string identifying the icon.
--  Each Gtk_Style has a list of Gtk_Icon_Factory derived from the current
--  theme; those icon factories are consulted first when searching for an icon.
--  If the theme doesn't set a particular icon, GTK+ looks for the icon in a
--  list of default icon factories, maintained by gtk.icon_factory.add_default
--  and gtk.icon_factory.remove_default. Applications with icons should add
--  default icon factory with their icons, which will allow themes to override
--  the icons for the application.
--
--  To display an icon, always use Lookup_Icon_Set on the widget that
--  will display the icon, or the convenience function Gtk.Widget.Render_Icon.
--  These functions take the theme into account when looking up the icon to use
--  for a given stock ID.
--  </description>
--  <c_version>2.8.17</c_version>
--  <see>Gtk.Icon_Theme</see>

with Glib.Object;
with Gdk.Pixbuf;
with Gtk.Enums;
with Gtk.Settings;
with Gtk.Style;
with Gtk.Widget;

package Gtk.Icon_Factory is

   --------------------
   -- Icon factories --
   --------------------

   type Gtk_Icon_Factory_Record is new Glib.Object.GObject_Record with private;
   type Gtk_Icon_Factory is access all Gtk_Icon_Factory_Record'Class;

   procedure Gtk_New (Widget : out Gtk_Icon_Factory);
   procedure Initialize (Widget : access Gtk_Icon_Factory_Record'Class);
   --  Creates or initializes a new Icon_Factory.

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Icon_Factory.

   procedure Add_Default (Factory : access Gtk_Icon_Factory_Record);
   --  Adds an icon factory to the list of icon factories searched by
   --  Lookup_Icon_Set. This means that, for example,
   --  Gtk.Image.New_From_Stock will be able to find icons in Factory.
   --  There will normally be an icon factory added for each library or
   --  application that comes with icons. The default icon factories
   --  can be overridden by themes.

   procedure Remove_Default (Factory : access Gtk_Icon_Factory_Record);
   --  Removes an icon factory from the list of default icon
   --  factories. Not normally used; you might use it for a library that
   --  can be unloaded or shut down.

   ---------------
   -- Icon sets --
   ---------------

   --  An icon set represents a single icon in various sizes and widget states.
   --  It can provide a Gdk_Pixbuf for a given size and state on request, and
   --  automatically caches some of the rendered Gdk_Pixbuf objects.
   --
   --  Normally you would use Gtk.Widget.Render_Icon instead of using icon sets
   --  directly. The one case where you'd use an icon set is to create
   --  application-specific icon sets to place in an icon factory.

   type Gtk_Icon_Set is new Glib.C_Proxy;

   function Gtk_New return Gtk_Icon_Set;
   --  Create an empty Gtk_Icon_Set.

   function Gtk_New (Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf) return Gtk_Icon_Set;
   --  Creates a new icon set with Pixbuf as the default/fallback source
   --  image. If you don't add any additional icon sources (see below) to the
   --  icon set, all variants of the icon will be created from Pixbuf,
   --  using scaling, pixelation, etc. as required to adjust the icon size
   --  or make the icon look insensitive/prelighted.

   procedure Add
     (Factory  : access Gtk_Icon_Factory_Record;
      Stock_Id : String;
      Set      : Gtk_Icon_Set);
   --  Adds the given icon set to the icon factory, under the name Stock_Id.
   --  Stock_Id should be namespaced for your application, e.g.
   --  "myapp-whatever-icon".  Normally applications create an icon factory,
   --  then add it to the list of default factories with Add_Default. Then they
   --  pass the Stock_Id to widgets such as Gtk_Image to display the icon.
   --  Themes can provide an icon with the same name (such as
   --  "myapp-whatever-icon") to override your application's default icons. If
   --  an icon already existed in Factory for Stock_Id, it is unreferenced and
   --  replaced with the new icon set.

   function Lookup_Icon_Set
     (Style  : access Gtk.Style.Gtk_Style_Record'Class; Stock_Id : String)
      return Gtk_Icon_Set;
   --  Retrieve an icon set by its name. The icon might exist in various sizes,
   --  that can be manipulated through the result set

   function Lookup
     (Factory  : access Gtk_Icon_Factory_Record;
      Stock_Id : String) return Gtk_Icon_Set;
   --  Looks up Stock_Id in the icon factory, returning an icon set if found,
   --  otherwise null. For display to the user, you should use
   --  Lookup_Icon_Set on the Gtk_Style for the widget that will
   --  display the icon, instead of using this function directly, so that
   --  themes are taken into account.

   function Lookup_Default (Stock_Id : String) return Gtk_Icon_Set;
   --  Looks for an icon in the list of default icon factories.  For
   --  display to the user, you should use Lookup_Icon_Set on
   --  the Gtk_Style for the widget that will display the icon, instead of
   --  using this function directly, so that themes are taken into
   --  account.

   function Icon_Set_Get_Type return Glib.GType;
   --  Return the internal type value for Gtk_Icon_Set

   function Copy (Icon_Set : Gtk_Icon_Set) return Gtk_Icon_Set;
   pragma Import (C, Copy, "gtk_icon_set_copy");
   --  Copies Icon_Set by value.

   function Get_Sizes (Icon_Set : Gtk_Icon_Set) return Gint_Array;
   --  Obtains a list of icon sizes this icon set can render.

   procedure Ref   (Icon_Set : Gtk_Icon_Set);
   procedure Unref (Icon_Set : Gtk_Icon_Set);
   pragma Import (C, Ref,   "gtk_icon_set_ref");
   pragma Import (C, Unref, "gtk_icon_set_unref");
   --  Increase or decrease the reference counting for the icon set. When this
   --  reaches 0, the memory is reclaimed

   function Render_Icon
     (Icon_Set  : Gtk_Icon_Set;
      Style     : access Gtk.Style.Gtk_Style_Record'Class;
      Direction : Gtk.Enums.Gtk_Text_Direction;
      State     : Gtk.Enums.Gtk_State_Type;
      Size      : Gtk.Enums.Gtk_Icon_Size;
      Widget    : Gtk.Widget.Gtk_Widget := null;
      Detail    : String := "")
      return Gdk.Pixbuf.Gdk_Pixbuf;
   --  Renders an icon using Render_Icon below. In most cases, the other
   --  version of Render_Icon is better, since it automatically provides most
   --  of the arguments from the current widget settings. This function never
   --  returns null; if the icon can't be rendered (perhaps because an image
   --  file fails to load), a default "missing image" icon will be returned
   --  instead.
   --  Widget is the widget that will display the icon, or null. This is
   --  typically used to determine the screen (and thus the colormap depth).
   --  Detail is the string to pass to the theme engine to provide more
   --  information. Passing anything but the empty string will disable
   --  caching.

   ------------------
   -- Icon sources --
   ------------------

   --  An icon source contains a Gdk_Pixbuf (or image filename) that serves as
   --  the base image for one or more of the icons in an icon set, along with a
   --  specification for which icons in the icon set will be based on that
   --  pixbuf or image file. An icon set contains a set of icons that represent
   --  "the same" logical concept in different states, different global text
   --  directions, and different sizes.
   --
   --  So for example a web browser's "Back to Previous Page" icon might point
   --  in a different direction in Hebrew and in English; it might look
   --  different when insensitive; and it might change size depending on
   --  toolbar mode (small/large icons). So a single icon set would contain all
   --  those variants of the icon. An icon set contains a list of icon sources
   --  from which it can derive specific icon variants in the set.
   --
   --  In the simplest case, an icon set contains one source pixbuf from which
   --  it derives all variants.
   --
   --  If you want to use a different base pixbuf for different icon variants,
   --  you create multiple icon sources, mark which variants they'll be used to
   --  create, and add them to the icon set with Add_Source (see below).
   --
   --  By default, the icon source has all parameters wildcarded. That is, the
   --  icon source will be used as the base icon for any desired text
   --  direction, widget state, or icon size.

   type Gtk_Icon_Source is new Glib.C_Proxy;

   function Gtk_New return Gtk_Icon_Source;
   --  Create a new icon source.

   function Icon_Source_Copy (Source : Gtk_Icon_Source) return Gtk_Icon_Source;
   --  Returns a copy of Gtk_Icon_Source. This function is generally not
   --  useful

   function Icon_Source_Get_Type return Glib.GType;
   --  Return the internal type used for Gtk_Icon_Source

   function Render_Icon
     (Style     : access Gtk.Style.Gtk_Style_Record'Class;
      Source    : Gtk.Icon_Factory.Gtk_Icon_Source;
      Direction : Gtk.Enums.Gtk_Text_Direction;
      State     : Gtk.Enums.Gtk_State_Type;
      Size      : Gtk.Enums.Gtk_Icon_Size;
      Widget    : Gtk.Widget.Gtk_Widget := null;
      Detail    : String := "")
      return Gdk.Pixbuf.Gdk_Pixbuf;
   --  Renders the icon specified by Source at the given Size according to the
   --  given parameters and returns the result in pixbuf.

   procedure Add_Source
     (Set    : Gtk_Icon_Set;
      Source : Gtk_Icon_Source);
   --  Icon sets have a list of icon sources, which they use as base
   --  icons for rendering icons in different states and sizes. Icons are
   --  scaled, made to look insensitive, etc. in Gtk.Icon.Set_Render_Icon,
   --  but an icon set needs base images to work with. The base images and
   --  when to use them are described by an icon source.
   --  This function copies Source, so you can reuse the same source
   --  immediately without affecting the icon set.

   procedure Free (Source : Gtk_Icon_Source);
   --  Free memory allocated to Source.

   procedure Set_Filename
     (Source : Gtk_Icon_Source; Filename : String);
   function Get_Filename (Source : Gtk_Icon_Source) return String;
   --  Set the name of an image file to use as a base image when creating
   --  icon variants for an icon set. The filename must be absolute.

   procedure Set_Pixbuf
     (Source : Gtk_Icon_Source; Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf);
   function Get_Pixbuf
     (Source : Gtk_Icon_Source) return Gdk.Pixbuf.Gdk_Pixbuf;
   --  Set a pixbuf to use as a base image when creating icon variants
   --  for an icon set. If an icon source has both a filename and a pixbuf
   --  set, the pixbuf will take priority.

   procedure Set_Size
     (Source : Gtk_Icon_Source; Size : Gtk.Enums.Gtk_Icon_Size);
   function Get_Size
     (Source : Gtk_Icon_Source) return Gtk.Enums.Gtk_Icon_Size;
   --  Set the icon size this icon source is intended to be used with

   procedure Set_Icon_Name (Source : Gtk_Icon_Source; Icon_Name : String);
   function  Get_Icon_Name (Source : Gtk_Icon_Source) return String;
   --  Retrieves the source icon name, or "" if none is set. The icon comes
   --  from an icon theme in this case.

   procedure Set_Size_Wildcarded
     (Source : Gtk_Icon_Source; Wildcarded : Boolean);
   function Get_Size_Wildcarded (Source : Gtk_Icon_Source) return Boolean;
   --  Change the wilcarded state of the size for the icon source.
   --
   --  If the icon size is wildcarded, this source can be used as the base
   --  image for an icon of any size.  if the size is not wildcarded, then
   --  the size the source applies to should be set with Set_Size,
   --  and the icon source will only be used with that specific size.
   --
   --  Gtk prefers non-wildcarded sources (exact matches) over wildcarded
   --  sources, and will use an exact match when possible.
   --
   --  Gtk will normally scale wildcarded source images to produce
   --  an appropriate icon at a given size, but will not change the size
   --  of source images that match exactly.

   procedure Set_Direction_Wildcarded
     (Source  : Gtk_Icon_Source;  Setting : Boolean);
   function Get_Direction_Wildcarded
     (Source : Gtk_Icon_Source) return Boolean;
   --  If the text direction is wildcarded, this source can be used as the base
   --  image for an icon in any Gtk_Text_Direction.
   --  If the text direction is not wildcarded, then the text direction the
   --  icon source applies to should be set with Set_Direction(), and the icon
   --  source will only be used with that text direction.
   --  Gtk_Icon_Set prefers non-wildcarded sources (exact matches) over
   --  wildcarded sources, and will use an exact match when possible

   procedure Set_Direction
     (Source : Gtk_Icon_Source; Direction : Gtk.Enums.Gtk_Text_Direction);
   function Get_Direction
     (Source : Gtk_Icon_Source) return Gtk.Enums.Gtk_Text_Direction;
   --  Sets the text direction this icon source is intended to be used
   --  with.
   --  Setting the text direction on an icon source makes no difference if the
   --  text direction is wildcarded. Therefore, you should usually call
   --  Set_Direction_Wildcarded() to un-wildcard it in addition to calling this
   --  function.

   procedure Set_State_Wildcarded
     (Source  : Gtk_Icon_Source;  Setting : Boolean);
   function Get_State_Wildcarded (Source  : Gtk_Icon_Source) return Boolean;
   --  If the widget state is wildcarded, this source can be used as the base
   --  image for an icon in any Gtk_State_Type. If the widget state is not
   --  wildcarded, then the state the source applies to should be set with
   --  Set_State and the icon source will only be used with that specific
   --  state.
   --  Gtk_Icon_Set prefers non-wildcarded sources (exact matches) over
   --  wildcarded sources, and will use an exact match when possible.
   --  Gtk_Icon_Set will normally transform wildcarded source images to produce
   --  an appropriate icon for a given state, for example lightening an image
   --  on prelight, but will not modify source images that match exactly.

   procedure Set_State
     (Source : Gtk_Icon_Source; State  : Gtk.Enums.Gtk_State_Type);
   function Get_State
     (Source : Gtk_Icon_Source) return Gtk.Enums.Gtk_State_Type;
   --  Sets the widget state this icon source is intended to be used
   --  with.
   --  Setting the widget state on an icon source makes no difference if the
   --  state is wildcarded. Therefore, you should usually call
   --  Set_State_Wildcarded to un-wildcard it in addition to calling this
   --  function.

   ----------------
   -- Icon sizes --
   ----------------
   --  There are a number of predefined icon sizes (see gtk-enums.ads). These
   --  are used in the various gtk+ contexts. However, you can also define your
   --  own icon sizes to use in your application.

   function Icon_Size_From_Name (Name : String) return Gtk.Enums.Gtk_Icon_Size;
   --  Looks up the icon size associated with Name.
   --  Predefined icon sizes are associated with the following names:
   --    Icon_Size_Menu          => "gtk-menu"          (16x16)
   --    Icon_Size_Button        => "gtk-button"        (20x20)
   --    Icon_Size_Small_Toolbar => "gtk-small-toolbar" (18x18)
   --    Icon_Size_Large_Toolbar => "gtk-large-toolbar" (24x24)
   --    Icon_Size_Dnd           => "gtk-dnd"           (32x32)
   --    Icon_Size_Dialog        => "gtk-dialog"        (48x48)
   --  You can also define your own names (see Icon_Size_Register)

   function Icon_Size_Get_Name (Size : Gtk.Enums.Gtk_Icon_Size) return String;
   --  Gets the canonical name of the given icon size

   procedure Icon_Size_Lookup
     (Size          : Gtk.Enums.Gtk_Icon_Size;
      Width, Height : out Gint);
   --  Obtains the pixel size of a semantic icon size, possibly
   --  modified by user preferences for the default Gtk_Settings.
   --  (See Icon_Size_Lookup_For_Settings).
   --  This function isn't normally needed, Render_Icon is the usual way to get
   --  an icon for rendering, then just look at the size of the rendered
   --  pixbuf. The rendered pixbuf may not even correspond to the width/height
   --  returned by Icon_Size_Lookup, because themes are free to render the
   --  pixbuf however they like, including changing the usual size.
   --  Sizes are set to -1 if Size is invalid.

   procedure Icon_Size_Lookup_For_Settings
     (Settings : access Gtk.Settings.Gtk_Settings_Record'Class;
      Size     : Gtk.Enums.Gtk_Icon_Size;
      Width    : out Gint;
      Height   : out Gint);
   --  Obtains the pixel size of a semantic icon size, possibly modified by
   --  user preferences for a particular Gtk_Settings.

   function Icon_Size_Register
     (Name   : String;
      Width  : Gint;
      Height : Gint)
      return Gtk.Enums.Gtk_Icon_Size;
   --  Registers a new icon size, along the same lines as Icon_Size_Menu,
   --  etc. Returns the integer value for the size.

   procedure Icon_Size_Register_Alias
     (Alias : String;  Target : Gtk.Enums.Gtk_Icon_Size);
   --  Registers Alias as another name for Target.
   --  So calling Icon_Size_From_Name with Alias as argument will return Target

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  </signals>

private
   type Gtk_Icon_Factory_Record is
     new Glib.Object.GObject_Record with null record;

   pragma Import (C, Get_Type, "gtk_icon_factory_get_type");
   pragma Import (C, Set_Size, "gtk_icon_source_set_size");
   pragma Import (C, Get_Size, "gtk_icon_source_get_size");
   pragma Import (C, Icon_Set_Get_Type, "gtk_icon_set_get_type");
   pragma Import (C, Icon_Source_Get_Type, "gtk_icon_source_get_type");
   pragma Import (C, Set_Direction, "gtk_icon_source_set_direction");
   pragma Import (C, Get_Direction, "gtk_icon_source_get_direction");
   pragma Import (C, Set_State, "gtk_icon_source_set_state");
   pragma Import (C, Get_State, "gtk_icon_source_get_state");
   pragma Import (C, Icon_Source_Copy, "gtk_icon_source_copy");

end Gtk.Icon_Factory;
