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
--  The "system tray" or notification area is normally used for transient icons
--  that indicate some special state. For example, a system tray icon might
--  appear to tell the user that they have new mail, or have an incoming
--  instant message, or something along those lines. The basic idea is that
--  creating an icon in the notification area is less annoying than popping up
--  a dialog.
--
--  A Gtk_Status_Icon object can be used to display an icon in a "system tray".
--  The icon can have a tooltip, and the user can interact with it by
--  activating it or popping up a context menu. Critical information should not
--  solely be displayed in a Gtk_Status_Icon, since it may not be visible
--  (e.g. when the user doesn't have a notification area on his panel). This
--  can be checked with Is_Embedded.
--
--  On X11, the implementation follows the freedesktop.org "System Tray"
--  specification. Implementations of the "tray" side of this specification can
--  be found e.g. in the GNOME and KDE panel applications.
--
--  Note that a Gtk_Status_Icon is not a widget, but just a GObject. Making it
--  a widget would be impractical, since the system tray on Win32 doesn't allow
--  to embed arbitrary widgets.
--  </description>
--  <c_version>2.16.6</c_version>

with Glib.G_Icon;
with Glib.Object;
with Glib.Properties;
with Gdk.Pixbuf;
with Gdk.Rectangle;
with Gdk.Screen;
with Gtk.Enums;
with Gtk.Image;

package Gtk.Status_Icon is

   type Gtk_Status_Icon_Record is new Glib.Object.GObject_Record with private;
   type Gtk_Status_Icon is access all Gtk_Status_Icon_Record'Class;

   function Get_Type return GType;

   procedure Gtk_New (Widget : out Gtk_Status_Icon);
   procedure Initialize (Widget : access Gtk_Status_Icon_Record'Class);
   --  Creates an empty status icon object.

   function Get_Storage_Type
     (Status_Icon : access Gtk_Status_Icon_Record)
      return Gtk.Image.Gtk_Image_Type;
   --  Gets the type of representation being used by the Gtk_Status_Icon
   --  to store image data. If the Gtk_Status_Icon has no image data,
   --  the return value will be Image_Empty.

   function Is_Embedded
     (Status_Icon : access Gtk_Status_Icon_Record) return Boolean;
   --  Returns whether the status icon is embedded in a notification
   --  area.

   ----------
   -- File --
   ----------

   procedure Gtk_New_From_File
     (Widget   : out Gtk_Status_Icon;
      Filename : String);
   procedure Initialize_From_File
     (Widget   : access Gtk_Status_Icon_Record'Class;
      Filename : String);
   --  Creates a status icon displaying the file Filename.
   --
   --  The image will be scaled down to fit in the available
   --  space in the notification area, if necessary.

   procedure Set_From_File
     (Status_Icon : access Gtk_Status_Icon_Record;
      Filename    : String);
   --  Makes Status_Icon display the file Filename.
   --  See Gtk_New_From_File for details.

   ------------
   -- G_Icon --
   ------------

   procedure Gtk_New_From_Gicon
     (Widget : out Gtk_Status_Icon;
      Icon   : Glib.G_Icon.G_Icon);
   procedure Initialize_From_Gicon
     (Widget : access Gtk_Status_Icon_Record'Class;
      Icon   : Glib.G_Icon.G_Icon);
   --  Creates a status icon displaying a G_Icon. If the icon is a
   --  themed icon, it will be updated when the theme changes.

   function Get_Gicon
     (Status_Icon : access Gtk_Status_Icon_Record) return Glib.G_Icon.G_Icon;
   --  Retrieves the G_Icon being displayed by the Gtk_Status_Icon.
   --  The storage type of the status icon must be Image_Empty or
   --  Image_Gicon (see Get_Storage_Type).
   --  The caller of this function does not own a reference to the
   --  returned G_Icon.
   --
   --  Status_Icon is left unchanged if this function fails.  Returns
   --  null if the image is empty.

   procedure Set_From_Gicon
     (Status_Icon : access Gtk_Status_Icon_Record;
      Icon        : Glib.G_Icon.G_Icon);
   --  Makes Status_Icon display the G_Icon.
   --  See Gtk_New_From_G_Icon for details.

   ---------------
   -- Icon_Name --
   ---------------

   procedure Gtk_New_From_Icon_Name
     (Widget    : out Gtk_Status_Icon;
      Icon_Name : String);
   procedure Initialize_From_Icon_Name
     (Widget    : access Gtk_Status_Icon_Record'Class;
      Icon_Name : String);
   --  Creates a status icon displaying an icon from the current icon theme.
   --  If the current icon theme is changed, the icon will be updated
   --  appropriately.

   function Get_Icon_Name
     (Status_Icon : access Gtk_Status_Icon_Record) return String;
   --  Gets the name of the icon being displayed by the Gtk_Status_Icon.
   --  The storage type of the status icon must be Image_Empty or
   --  Image_Icon_Name (see Get_Storage_Type).  Returns "" if the image
   --  is empty.

   procedure Set_From_Icon_Name
     (Status_Icon : access Gtk_Status_Icon_Record;
      Icon_Name   : String);
   --  Makes Status_Icon display the icon named Icon_Name from the
   --  current icon theme.
   --  See Gtk_New_From_Icon_Name for details.

   ------------
   -- Pixbuf --
   ------------

   procedure Gtk_New_From_Pixbuf
     (Widget : out Gtk_Status_Icon;
      Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf);
   procedure Initialize_From_Pixbuf
     (Widget : access Gtk_Status_Icon_Record'Class;
      Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf);
   --  Creates a status icon displaying Pixbuf.
   --
   --  The image will be scaled down to fit in the available
   --  space in the notification area, if necessary.

   function Get_Pixbuf
     (Status_Icon : access Gtk_Status_Icon_Record)
      return Gdk.Pixbuf.Gdk_Pixbuf;
   --  Gets the Gdk_Pixbuf being displayed by the Gtk_Status_Icon.
   --  The storage type of the status icon must be Gtk_Image_Empty
   --  (in which case this function will return null) or Gtk_Image_Pixbuf
   --  (see Get_Storage_Type).  The caller of this function does not own
   --  a reference to the returned pixbuf.

   procedure Set_From_Pixbuf
     (Status_Icon : access Gtk_Status_Icon_Record;
      Pixbuf      : Gdk.Pixbuf.Gdk_Pixbuf);
   --  Makes Status_Icon display Pixbuf.
   --  See Gtk_New_From_Pixbuf for details.

   -----------
   -- Stock --
   -----------

   procedure Gtk_New_From_Stock
     (Widget   : out Gtk_Status_Icon;
      Stock_Id : String);
   procedure Initialize_From_Stock
     (Widget   : access Gtk_Status_Icon_Record'Class;
      Stock_Id : String);
   --  Creates a status icon displaying a stock icon. Sample stock icon
   --  names are "GTK_STOCK_OPEN", "GTK_STOCK_QUIT". You can register your
   --  own stock icon names, see Gtk.Icon_Factory.Add_Default and
   --  Gtk.Icon_Factory.Add.

   function Get_Stock
     (Status_Icon : access Gtk_Status_Icon_Record) return String;
   --  Gets the id of the stock icon being displayed by the Gtk_Status_Icon.
   --  The storage type of the status icon must be Image_Empty or
   --  Image_Stock (see Get_Storage_Type).  Returns "" if the image is empty.

   procedure Set_From_Stock
     (Status_Icon : access Gtk_Status_Icon_Record;
      Stock_Id    : String);
   --  Makes Status_Icon display the stock icon with the id Stock_Id.
   --  See Gtk_New_From_Stock for details.

   --------------
   -- Tooltips --
   --------------

   function Get_Has_Tooltip
     (Status_Icon : access Gtk_Status_Icon_Record) return Boolean;
   procedure Set_Has_Tooltip
     (Status_Icon : access Gtk_Status_Icon_Record;
      Has_Tooltip : Boolean);
   --  Gets/Sets the has-tooltip property on Status_Icon.
   --  See has-tooltip for more information.

   function Get_Tooltip_Markup
     (Status_Icon : access Gtk_Status_Icon_Record) return String;
   procedure Set_Tooltip_Markup
     (Status_Icon : access Gtk_Status_Icon_Record;
      Markup      : String);
   --  Gets/Sets Markup as the contents of the tooltip, which is marked up with
   --  the Pango text markup language.  "" means no tooltip.
   --
   --  Set_Tooltip_Markup function will take care of setting has-tooltip to
   --  True and of the default handler for the query-tooltip signal.
   --
   --  See also the tooltip-markup property.

   function Get_Tooltip_Text
     (Status_Icon : access Gtk_Status_Icon_Record) return String;
   procedure Set_Tooltip_Text
     (Status_Icon : access Gtk_Status_Icon_Record;
      Text        : String);
   --  Gets/Sets the contents of the tooltip.  "" means no tooltip.
   --
   --  This function will take care of setting has-tooltip to
   --  True and of the default handler for the query-tooltip
   --  signal.
   --
   --  See also the tooltip-text property.

   -------------
   -- Display --
   -------------

   procedure Get_Geometry
     (Status_Icon : access Gtk_Status_Icon_Record;
      Screen      : in out Gdk.Screen.Gdk_Screen;
      Area        : out Gdk.Rectangle.Gdk_Rectangle;
      Orientation : out Gtk.Enums.Gtk_Orientation;
      Success     : out Boolean);
   --  Status_Icon: a Gtk_Status_Icon
   --  Screen:      a valid Gdk_Screen or null if the information is not needed
   --  Area:        area occupied by the status icon
   --  Orientation: the orientation of the panel in which the status icon is
   --               embedded. A panel at the top or bottom of the screen is
   --               horizontal, a panel at the left or right is vertical.
   --  Success:     True if the location information has been filled in.
   --
   --  Obtains information about the location of the status icon
   --  on screen. This information can be used to e.g. position
   --  popups like notification bubbles.
   --
   --  See Position_Menu for a more convenient alternative for positioning
   --  menus.
   --
   --  Note that some platforms do not allow GTK+ to provide
   --  this information, and even on platforms that do allow it,
   --  the information is not reliable unless the status icon
   --  is embedded in a notification area, see Is_Embedded.

   function Get_Blinking
     (Status_Icon : access Gtk_Status_Icon_Record) return Boolean;
   procedure Set_Blinking
     (Status_Icon : access Gtk_Status_Icon_Record;
      Blinking    : Boolean);
   --  Makes the status icon start or stop blinking.
   --  Note that blinking user interface elements may be problematic
   --  for some users, and thus may be turned off, in which case
   --  this setting has no effect.

   function Get_Screen
     (Status_Icon : access Gtk_Status_Icon_Record)
      return Gdk.Screen.Gdk_Screen;
   procedure Set_Screen
     (Status_Icon : access Gtk_Status_Icon_Record;
      Screen      : access Gdk.Screen.Gdk_Screen_Record'Class);
   --  Gets/Sets the Gdk_Screen where Status_Icon is displayed.  When invoking
   --  Set_Screen, if the icon is already mapped, it will be unmapped, and then
   --  remapped on the new screen.

   function Get_Size (Status_Icon : access Gtk_Status_Icon_Record) return Gint;
   --  Gets the size in pixels that is available for the image.
   --  Stock icons and named icons adapt their size automatically
   --  if the size of the notification area changes. For other
   --  storage types, the size-changed signal can be used to
   --  react to size changes.
   --
   --  Note that the returned size is only meaningful while the
   --  status icon is embedded (see Is_Embedded).

   function Get_Visible
     (Status_Icon : access Gtk_Status_Icon_Record) return Boolean;
   procedure Set_Visible
     (Status_Icon : access Gtk_Status_Icon_Record;
      Visible     : Boolean);
   --  Whether the status icon is visible or not.
   --  Note that being visible does not guarantee that
   --  the user can actually see the icon, see also
   --  Is_Embedded.

   function Get_X11_Window_Id
     (Status_Icon : access Gtk_Status_Icon_Record) return Guint32;
   --  This function is only useful on the X11/freedesktop.org platform.
   --  It returns a window ID for the widget in the underlying
   --  status icon implementation.  This is useful for the Galago
   --  notification service, which can send a window ID in the protocol
   --  in order for the server to position notification windows
   --  pointing to a status icon reliably.
   --
   --  This function is not intended for other use cases which are
   --  more likely to be met by one of the non-X11 specific methods, such
   --  as Position_Menu.

   procedure Position_Menu
     (Menu        : System.Address;
      X           : out Gint;
      Y           : out Gint;
      Push_In     : out Gboolean;
      Status_Icon : System.Address);
   --  Menu positioning function to use with Gtk.Menu.Popup to position
   --  Menu aligned to Status_Icon.  Pass Get_Object (Your_Status_Icon) as
   --  User_Data when calling Gtk.Menu.Popup.

   -----------------
   -- Obsolescent --
   -----------------

   procedure Set_Tooltip
     (Status_Icon  : access Gtk_Status_Icon_Record;
      Tooltip_Text : String);
   pragma Obsolescent; --  Set_Tooltip
   --  Sets the tooltip of the status icon.
   --
   --  Deprecated: 2.16: Use Set_Tooltip_Text instead.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  Name:  Blinking_Property
   --  Type:  Boolean
   --  Descr: Whether or not the status icon is blinking
   --
   --  Name:  Embedded_Property
   --  Type:  Boolean
   --  Descr: Whether or not the status icon is embedded
   --
   --  Name:  File_Property
   --  Type:  String
   --  Descr: Filename to load and display
   --
   --  Name:  Gicon_Property
   --  Type:  Object
   --  Descr: The GIcon being displayed
   --
   --  Name:  Has_Tooltip_Property
   --  Type:  Boolean
   --  Descr: Whether this tray icon has a tooltip
   --
   --  Name:  Icon_Name_Property
   --  Type:  String
   --  Descr: The name of the icon from the icon theme
   --
   --  Name:  Orientation_Property
   --  Type:  Enum
   --  Descr: The orientation of the tray
   --
   --  Name:  Pixbuf_Property
   --  Type:  Object
   --  Descr: A Gdk_Pixbuf to display
   --
   --  Name:  Screen_Property
   --  Type:  Object
   --  Descr: The screen where this status icon will be displayed
   --
   --  Name:  Size_Property
   --  Type:  Int
   --  Descr: The size of the icon
   --
   --  Name:  Stock_Property
   --  Type:  String
   --  Descr: Stock ID for a stock image to display
   --
   --  Name:  Storage_Type_Property
   --  Type:  Enum
   --  Descr: The representation being used for image data
   --
   --  Name:  Tooltip_Markup_Property
   --  Type:  String
   --  Descr: The contents of the tooltip for this tray icon
   --
   --  Name:  Tooltip_Text_Property
   --  Type:  String
   --  Descr: The contents of the tooltip for this widget
   --
   --  Name:  Visible_Property
   --  Type:  Boolean
   --  Descr: Whether or not the status icon is visible
   --
   --  </properties>

   Blinking_Property       : constant Glib.Properties.Property_Boolean;
   Embedded_Property       : constant Glib.Properties.Property_Boolean;
   File_Property           : constant Glib.Properties.Property_String;
   Gicon_Property          : constant Glib.Properties.Property_Object;
   Has_Tooltip_Property    : constant Glib.Properties.Property_Boolean;
   Icon_Name_Property      : constant Glib.Properties.Property_String;
   Orientation_Property    : constant Glib.Properties.Property_Enum;
   Pixbuf_Property         : constant Glib.Properties.Property_Object;
   Screen_Property         : constant Glib.Properties.Property_Object;
   Size_Property           : constant Glib.Properties.Property_Int;
   Stock_Property          : constant Glib.Properties.Property_String;
   Storage_Type_Property   : constant Glib.Properties.Property_Enum;
   Tooltip_Markup_Property : constant Glib.Properties.Property_String;
   Tooltip_Text_Property   : constant Glib.Properties.Property_String;
   Visible_Property        : constant Glib.Properties.Property_Boolean;

private

   type Gtk_Status_Icon_Record is
     new Glib.Object.GObject_Record with null record;

   Blinking_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("blinking");
   Embedded_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("embedded");
   File_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("file");
   Gicon_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("gicon");
   Has_Tooltip_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-tooltip");
   Icon_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("icon-name");
   Orientation_Property : constant Glib.Properties.Property_Enum :=
     Glib.Properties.Build ("orientation");
   Pixbuf_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("pixbuf");
   Screen_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("screen");
   Size_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("size");
   Stock_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("stock");
   Storage_Type_Property : constant Glib.Properties.Property_Enum :=
     Glib.Properties.Build ("storage-type");
   Tooltip_Markup_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("tooltip-markup");
   Tooltip_Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("tooltip-text");
   Visible_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("visible");

   pragma Import (C, Get_Type, "gtk_status_icon_get_type");
   pragma Import (C, Position_Menu, "gtk_status_icon_position_menu");

end Gtk.Status_Icon;
