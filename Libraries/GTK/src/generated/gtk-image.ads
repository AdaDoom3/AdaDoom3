-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2013, AdaCore                   --
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
--  The Gtk_Image widget displays a graphical image. The image is typically
--  created using Gdk.Image.Gdk_New.
--
--  The pixels in a Gtk_Image may be manipulated by the application after
--  creation, as Gtk_Image store the pixel data on the client side. If you wish
--  to store the pixel data on the server side (thus not allowing manipulation
--  of the data after creation) you should use Gtk_Pixmap.
--
--  </description>
--  <screenshot>gtk-image</screenshot>
--  <group>Display widgets</group>

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;     use GNAT.Strings;
with Gdk.Bitmap;       use Gdk.Bitmap;
with Gdk.Image;        use Gdk.Image;
with Gdk.Pixbuf;       use Gdk.Pixbuf;
with Gdk.Pixmap;       use Gdk.Pixmap;
with Glib;             use Glib;
with Glib.G_Icon;      use Glib.G_Icon;
with Glib.Properties;  use Glib.Properties;
with Glib.Types;       use Glib.Types;
with Gtk.Buildable;    use Gtk.Buildable;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Icon_Factory; use Gtk.Icon_Factory;
with Gtk.Misc;         use Gtk.Misc;
with Gtk.Widget;       use Gtk.Widget;

package Gtk.Image is

   type Gtk_Image_Record is new Gtk_Misc_Record with null record;
   type Gtk_Image is access all Gtk_Image_Record'Class;

   type Gtk_Image_Type is
        (Image_Empty,
         Image_Pixmap,
         Image_Image,
         Image_Pixbuf,
         Image_Stock,
         Image_Icon_Set,
         Image_Animation,
         Image_Icon_Name,
         Image_Gicon);
      pragma Convention (C, Gtk_Image_Type);

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Image : out Gtk_Image);
   procedure Initialize (Image : access Gtk_Image_Record'Class);
   --  Creates a new empty Gtk.Image.Gtk_Image widget.

   procedure Gtk_New
      (Image     : out Gtk_Image;
       Animation : Gdk.Pixbuf.Gdk_Pixbuf_Animation);
   procedure Initialize
      (Image     : access Gtk_Image_Record'Class;
       Animation : Gdk.Pixbuf.Gdk_Pixbuf_Animation);
   --  Creates a Gtk.Image.Gtk_Image displaying the given animation. The
   --  Gtk.Image.Gtk_Image does not assume a reference to the animation; you
   --  still need to unref it if you own references. Gtk.Image.Gtk_Image will
   --  add its own reference rather than adopting yours. Note that the
   --  animation frames are shown using a timeout with G_PRIORITY_DEFAULT. When
   --  using animations to indicate busyness, keep in mind that the animation
   --  will only be shown if the main loop is not busy with something that has
   --  a higher priority.
   --  "animation": an animation

   procedure Gtk_New (Image : out Gtk_Image; Filename : UTF8_String);
   procedure Initialize
      (Image    : access Gtk_Image_Record'Class;
       Filename : UTF8_String);
   --  Creates a new Gtk.Image.Gtk_Image displaying the file Filename. If the
   --  file isn't found or can't be loaded, the resulting Gtk.Image.Gtk_Image
   --  will display a "broken image" icon. This function never returns null, it
   --  always returns a valid Gtk.Image.Gtk_Image widget. If the file contains
   --  an animation, the image will contain an animation. If you need to detect
   --  failures to load the file, use Gdk.Pixbuf.Gdk_New_From_File to load the
   --  file yourself, then create the Gtk.Image.Gtk_Image from the pixbuf. (Or
   --  for animations, use Gdk.Pixbuf.Gdk_New_From_File). The storage type
   --  (gtk_image_get_storage_type) of the returned image is not defined, it
   --  will be whatever is appropriate for displaying the file.
   --  "filename": a filename

   procedure Gtk_New_From_Gicon
      (Image : out Gtk_Image;
       Icon  : Glib.G_Icon.G_Icon;
       Size  : Gtk.Enums.Gtk_Icon_Size);
   procedure Initialize_From_Gicon
      (Image : access Gtk_Image_Record'Class;
       Icon  : Glib.G_Icon.G_Icon;
       Size  : Gtk.Enums.Gtk_Icon_Size);
   --  Creates a Gtk.Image.Gtk_Image displaying an icon from the current icon
   --  theme. If the icon name isn't known, a "broken image" icon will be
   --  displayed instead. If the current icon theme is changed, the icon will
   --  be updated appropriately.
   --  Since: gtk+ 2.14
   --  "icon": an icon
   --  "size": a stock icon size

   procedure Gtk_New_From_Icon_Name
      (Image     : out Gtk_Image;
       Icon_Name : UTF8_String;
       Size      : Gtk.Enums.Gtk_Icon_Size);
   procedure Initialize_From_Icon_Name
      (Image     : access Gtk_Image_Record'Class;
       Icon_Name : UTF8_String;
       Size      : Gtk.Enums.Gtk_Icon_Size);
   --  Creates a Gtk.Image.Gtk_Image displaying an icon from the current icon
   --  theme. If the icon name isn't known, a "broken image" icon will be
   --  displayed instead. If the current icon theme is changed, the icon will
   --  be updated appropriately.
   --  Since: gtk+ 2.6
   --  "icon_name": an icon name
   --  "size": a stock icon size

   procedure Gtk_New
      (Image    : out Gtk_Image;
       Icon_Set : Gtk.Icon_Factory.Gtk_Icon_Set;
       Size     : Gtk.Enums.Gtk_Icon_Size);
   procedure Initialize
      (Image    : access Gtk_Image_Record'Class;
       Icon_Set : Gtk.Icon_Factory.Gtk_Icon_Set;
       Size     : Gtk.Enums.Gtk_Icon_Size);
   --  Creates a Gtk.Image.Gtk_Image displaying an icon set. Sample stock
   --  sizes are GTK_ICON_SIZE_MENU, GTK_ICON_SIZE_SMALL_TOOLBAR. Instead of
   --  using this function, usually it's better to create a
   --  Gtk.Iconfactory.Gtk_Iconfactory, put your icon sets in the icon factory,
   --  add the icon factory to the list of default factories with
   --  Gtk.Icon_Factory.Add_Default, and then use Gtk.Image.Gtk_New. This will
   --  allow themes to override the icon you ship with your application. The
   --  Gtk.Image.Gtk_Image does not assume a reference to the icon set; you
   --  still need to unref it if you own references. Gtk.Image.Gtk_Image will
   --  add its own reference rather than adopting yours.
   --  "icon_set": a GtkIconSet
   --  "size": a stock icon size

   procedure Gtk_New
      (Image : out Gtk_Image;
       Val   : Gdk.Image.Gdk_Image;
       Mask  : Gdk.Bitmap.Gdk_Bitmap);
   procedure Initialize
      (Image : access Gtk_Image_Record'Class;
       Val   : Gdk.Image.Gdk_Image;
       Mask  : Gdk.Bitmap.Gdk_Bitmap);
   --  Creates a Gtk.Image.Gtk_Image widget displaying a Image with a Mask. A
   --  GdkImage is a client-side image buffer in the pixel format of the
   --  current display. The Gtk.Image.Gtk_Image does not assume a reference to
   --  the image or mask; you still need to unref them if you own references.
   --  Gtk.Image.Gtk_Image will add its own reference rather than adopting
   --  yours.
   --  "Val": a GdkImage, or null
   --  "mask": a GdkBitmap, or null

   procedure Gtk_New
      (Image  : out Gtk_Image;
       Pixbuf : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);
   procedure Initialize
      (Image  : access Gtk_Image_Record'Class;
       Pixbuf : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);
   --  Creates a new Gtk.Image.Gtk_Image displaying Pixbuf. The
   --  Gtk.Image.Gtk_Image does not assume a reference to the pixbuf; you still
   --  need to unref it if you own references. Gtk.Image.Gtk_Image will add its
   --  own reference rather than adopting yours. Note that this function just
   --  creates an Gtk.Image.Gtk_Image from the pixbuf. The Gtk.Image.Gtk_Image
   --  created will not react to state changes. Should you want that, you
   --  should use Gtk.Image.Gtk_New.
   --  "pixbuf": a Gdk.Pixbuf.Gdk_Pixbuf, or null

   procedure Gtk_New
      (Image  : out Gtk_Image;
       Pixmap : Gdk.Pixmap.Gdk_Pixmap;
       Mask   : Gdk.Bitmap.Gdk_Bitmap);
   procedure Initialize
      (Image  : access Gtk_Image_Record'Class;
       Pixmap : Gdk.Pixmap.Gdk_Pixmap;
       Mask   : Gdk.Bitmap.Gdk_Bitmap);
   --  Creates a Gtk.Image.Gtk_Image widget displaying Pixmap with a Mask. A
   --  GdkPixmap is a server-side image buffer in the pixel format of the
   --  current display. The Gtk.Image.Gtk_Image does not assume a reference to
   --  the pixmap or mask; you still need to unref them if you own references.
   --  Gtk.Image.Gtk_Image will add its own reference rather than adopting
   --  yours.
   --  "pixmap": a GdkPixmap, or null
   --  "mask": a GdkBitmap, or null

   procedure Gtk_New
      (Image    : out Gtk_Image;
       Stock_Id : UTF8_String;
       Size     : Gtk.Enums.Gtk_Icon_Size);
   procedure Initialize
      (Image    : access Gtk_Image_Record'Class;
       Stock_Id : UTF8_String;
       Size     : Gtk.Enums.Gtk_Icon_Size);
   --  Creates a Gtk.Image.Gtk_Image displaying a stock icon. Sample stock
   --  icon names are GTK_STOCK_OPEN, GTK_STOCK_QUIT. Sample stock sizes are
   --  GTK_ICON_SIZE_MENU, GTK_ICON_SIZE_SMALL_TOOLBAR. If the stock icon name
   --  isn't known, the image will be empty. You can register your own stock
   --  icon names, see Gtk.Icon_Factory.Add_Default and Gtk.Icon_Factory.Add.
   --  "stock_id": a stock icon name
   --  "size": a stock icon size

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_image_get_type");

   -------------
   -- Methods --
   -------------

   procedure Clear (Image : access Gtk_Image_Record);
   --  Resets the image to be empty.
   --  Since: gtk+ 2.8

   function Get
      (Image : access Gtk_Image_Record)
       return Gdk.Pixbuf.Gdk_Pixbuf_Animation;
   procedure Get
      (Image : access Gtk_Image_Record;
       Gicon : out Glib.G_Icon.G_Icon;
       Size  : out Gtk.Enums.Gtk_Icon_Size);
   procedure Get
      (Image    : access Gtk_Image_Record;
       Icon_Set : out Gtk.Icon_Factory.Gtk_Icon_Set;
       Size     : out Gtk.Enums.Gtk_Icon_Size);
   procedure Get
      (Image     : access Gtk_Image_Record;
       Gdk_Image : out Gdk.Image.Gdk_Image;
       Mask      : out Gdk.Bitmap.Gdk_Bitmap);
   function Get
      (Image : access Gtk_Image_Record) return Gdk.Pixbuf.Gdk_Pixbuf;
   procedure Get
      (Image  : access Gtk_Image_Record;
       Pixmap : out Gdk.Pixmap.Gdk_Pixmap;
       Mask   : out Gdk.Bitmap.Gdk_Bitmap);
   --  Gets the pixmap and mask being displayed by the Gtk.Image.Gtk_Image.
   --  The storage type of the image must be %GTK_IMAGE_EMPTY or
   --  %GTK_IMAGE_PIXMAP (see Gtk.Image.Get_Storage_Type). The caller of this
   --  function does not own a reference to the returned pixmap and mask.
   --  "pixmap": location to store the pixmap, or null
   --  "mask": location to store the mask, or null

   function Get_Pixel_Size (Image : access Gtk_Image_Record) return Gint;
   procedure Set_Pixel_Size
      (Image      : access Gtk_Image_Record;
       Pixel_Size : Gint);
   --  Sets the pixel size to use for named icons. If the pixel size is set to
   --  a value != -1, it is used instead of the icon size set by
   --  Gtk.Image.Set_From_Icon_Name.
   --  Since: gtk+ 2.6
   --  "pixel_size": the new pixel size

   function Get_Storage_Type
      (Image : access Gtk_Image_Record) return Gtk_Image_Type;
   --  Gets the type of representation being used by the Gtk.Image.Gtk_Image
   --  to store image data. If the Gtk.Image.Gtk_Image has no image data, the
   --  return value will be %GTK_IMAGE_EMPTY.

   procedure Set
      (Image     : access Gtk_Image_Record;
       Animation : Gdk.Pixbuf.Gdk_Pixbuf_Animation);
   procedure Set (Image : access Gtk_Image_Record; Filename : UTF8_String);
   procedure Set
      (Image : access Gtk_Image_Record;
       Icon  : Glib.G_Icon.G_Icon;
       Size  : Gtk.Enums.Gtk_Icon_Size);
   procedure Set
      (Image    : access Gtk_Image_Record;
       Icon_Set : Gtk.Icon_Factory.Gtk_Icon_Set;
       Size     : Gtk.Enums.Gtk_Icon_Size);
   procedure Set
      (Image     : access Gtk_Image_Record;
       Gdk_Image : Gdk.Image.Gdk_Image;
       Mask      : Gdk.Bitmap.Gdk_Bitmap);
   procedure Set
      (Image  : access Gtk_Image_Record;
       Pixbuf : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);
   procedure Set
      (Image  : access Gtk_Image_Record;
       Pixmap : Gdk.Pixmap.Gdk_Pixmap;
       Mask   : Gdk.Bitmap.Gdk_Bitmap);
   procedure Set
      (Image    : access Gtk_Image_Record;
       Stock_Id : UTF8_String;
       Size     : Gtk.Enums.Gtk_Icon_Size);
   --  See Gtk.Image.Gtk_New for details.
   --  "stock_id": a stock icon name
   --  "size": a stock icon size

   procedure Set_From_Icon_Name
      (Image     : access Gtk_Image_Record;
       Icon_Name : UTF8_String;
       Size      : Gtk.Enums.Gtk_Icon_Size);
   --  See Gtk.Image.Gtk_New_From_Icon_Name for details.
   --  Since: gtk+ 2.6
   --  "icon_name": an icon name
   --  "size": an icon size

   ----------------------
   -- GtkAda additions --
   ----------------------

   function Get
     (Image : access Gtk_Image_Record;
      Size  : access Gtk.Enums.Gtk_Icon_Size) return String;
   --  Get the stock_id for the image displayed

   procedure Get_Icon_Name
     (Image : access Gtk_Image_Record;
      Name  : out GNAT.Strings.String_Access;
      Size  : out Gtk.Enums.Gtk_Icon_Size);

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Image_Record, Gtk_Image);
   function "+"
     (Widget : access Gtk_Image_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Image
   renames Implements_Buildable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: File_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --
   --  Name: Gicon_Property
   --  Type: Glib.G_Icon.G_Icon
   --  Flags: read-write
   --  The GIcon displayed in the GtkImage. For themed icons, If the icon
   --  theme is changed, the image will be updated automatically.
   --
   --  Name: Icon_Name_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  The name of the icon in the icon theme. If the icon theme is changed,
   --  the image will be updated automatically.
   --
   --  Name: Icon_Set_Property
   --  Type: Gtk.Icon_Factory.Gtk_Icon_Set
   --  Flags: read-write
   --
   --  Name: Icon_Size_Property
   --  Type: Gint
   --  Flags: read-write
   --
   --  Name: Image_Property
   --  Type: Gdk.Image.Gdk_Image
   --  Flags: read-write
   --
   --  Name: Mask_Property
   --  Type: Gdk.Pixmap.Gdk_Pixmap
   --  Flags: read-write
   --
   --  Name: Pixbuf_Property
   --  Type: Gdk.Pixbuf.Gdk_Pixbuf
   --  Flags: read-write
   --
   --  Name: Pixbuf_Animation_Property
   --  Type: Gdk.Pixbuf.Gdk_Pixbuf_Animation
   --  Flags: read-write
   --
   --  Name: Pixel_Size_Property
   --  Type: Gint
   --  Flags: read-write
   --  The "pixel-size" property can be used to specify a fixed size
   --  overriding the Gtk.Image.Gtk_Image:icon-size property for images of type
   --  %GTK_IMAGE_ICON_NAME.
   --
   --  Name: Pixmap_Property
   --  Type: Gdk.Pixmap.Gdk_Pixmap
   --  Flags: read-write
   --
   --  Name: Stock_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --
   --  Name: Storage_Type_Property
   --  Type: Gtk_Image_Type
   --  Flags: read-write

   File_Property : constant Glib.Properties.Property_String;
   Gicon_Property : constant Glib.Properties.Property_Boxed;
   Icon_Name_Property : constant Glib.Properties.Property_String;
   Icon_Set_Property : constant Glib.Properties.Property_Boxed;
   Icon_Size_Property : constant Glib.Properties.Property_Int;
   Image_Property : constant Glib.Properties.Property_Boxed;
   Mask_Property : constant Glib.Properties.Property_Boxed;
   Pixbuf_Property : constant Glib.Properties.Property_Object;
   Pixbuf_Animation_Property : constant Glib.Properties.Property_Boxed;
   Pixel_Size_Property : constant Glib.Properties.Property_Int;
   Pixmap_Property : constant Glib.Properties.Property_Boxed;
   Stock_Property : constant Glib.Properties.Property_String;
   Storage_Type_Property : constant Glib.Properties.Property_Boxed;

private
   File_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("file");
   Gicon_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("gicon");
   Icon_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("icon-name");
   Icon_Set_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("icon-set");
   Icon_Size_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("icon-size");
   Image_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("image");
   Mask_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("mask");
   Pixbuf_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("pixbuf");
   Pixbuf_Animation_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("pixbuf-animation");
   Pixel_Size_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("pixel-size");
   Pixmap_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("pixmap");
   Stock_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("stock");
   Storage_Type_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("storage-type");
end Gtk.Image;
