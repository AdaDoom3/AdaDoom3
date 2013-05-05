-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2006 AdaCore                    --
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
--  This package provides an interface to Gtk's configuration files.
--  GTK+ provides resource file mechanism for configuring various aspects of
--  the operation of a GTK+ program at runtime.
--
--  Default files
--  =============
--
--  An application can cause GTK+ to parse a specific RC file by calling
--  Gtk.RC.Parse. In addition to this, certain files will be read at the end
--  of Gtk.Main.Init. Unless modified, the files looked for will be
--  <SYSCONFDIR>/gtk-2.0/gtkrc and .gtkrc-2.0 in the users home directory.
--  (<SYSCONFDIR> defaults to /usr/local/etc. It can be changed with the
--  --prefix or --sysconfdir options when configuring GTK+.) Note that although
--  the filenames contain the version number 2.0, all 2.x versions of GTK+ look
--  for these files.
--
--  The set of these default files can be retrieved with
--  Gtk.RC.Get_Default_Files and modified with Gtk.RC.Add_Default_File and
--  Gtk.RC.Set_Default_Files. Additionally, the GTK2_RC_FILES environment
--  variable can be set to a G_SEARCHPATH_SEPARATOR_S-separated list of files
--  in order to overwrite the set of default files at runtime.
--
--  For each RC file, in addition to the file itself, GTK+ will look for a
--  locale-specific file that will be parsed after the main file. For instance,
--  if LANG is set to ja_JP.ujis, when loading the default file ~/.gtkrc then
--  GTK+ looks for ~/.gtkrc.ja_JP and ~/.gtkrc.ja, and parses the first of
--  those that exists.
--
--  Pathnames and patterns
--  ======================
--
--  A resource file defines a number of styles and key bindings and attaches
--  them to particular widgets. The attachment is done by the widget,
--  widget_class, and class declarations. As an example of such a statement:
--          widget "mywindow.*.GtkEntry" style "my-entry-class"
--  attaches the style "my-entry-class" to all widgets whose widget class
--  matches the pattern "mywindow.*.GtkEntry".
--
--  The patterns here are given in the standard shell glob syntax. The "?"
--  wildcard matches any character, while "*" matches zero or more of any
--  character. The three types of matching are against the widget path, the
--  class path and the class hierarchy. Both the widget and the class paths
--  consists of a "." separated list of all the parents of the widget and the
--  widget itself from outermost to innermost. The difference is that in the
--  widget path, the name assigned by Gtk.Widget.Set_Name is used if present,
--  otherwise the class name of the widget, while for the class path, the class
--  name is always used.
--
--  So, if you have a GtkEntry named "myentry", inside of a of a window named
--  "mywindow", then the widget path is: "mwindow.GtkHBox.myentry" while the
--  class path is: "GtkWindow.GtkHBox.GtkEntry".
--
--  Matching against class is a little different. The pattern match is done
--  against all class names in the widgets class hierarchy (not the layout
--  hierarchy) in sequence, so the pattern:
--         class "GtkButton" style "my-style"
--  will match not just Gtk_Button widgets, but also Gtk_Toggle_Button and
--  Gtk_Check_Button widgets, since those classes derive from Gtk_Button.
--
--  Additionally, a priority can be specified for each pattern, and styles
--  override other styles first by priority, then by pattern type and then by
--  order of specification (later overrides earlier). The priorities that can
--  be specified are (highest to lowest):
--        highest
--        rc
--        theme
--        application
--        gtk
--        lowest
--
--  rc is the default for styles read from an RC file, theme is the default for
--  styles read from theme RC files, application should be used for styles an
--  application sets up, and gtk is used for styles that GTK+ creates
--  internally.
--
--  Toplevel declarations
--  =====================
--
--  An RC file is a text file which is composed of a sequence of declarations.
--  '#' characters delimit comments and the portion of a line after a '#' is
--  ignored when parsing an RC file.
--
--  The possible toplevel declarations are:
--     binding name { ... }
--         Declares a binding set.
--     class pattern [ style | binding ][ : priority ] name
--         Specifies a style or binding set for a particular branch of the
--         inheritance hierarchy.
--     include filename
--         Parses another file at this point. If filename is not an absolute
--         filename, it is searched in the directories of the currently open RC
--         files.
--         GTK+ also tries to load a locale-specific variant of the included
--         file.
--     module_path path
--         Sets a path (a list of directories separated by colons) that will be
--         searched for theme engines referenced in RC files.
--     pixmap_path path
--         Sets a path (a list of directories separated by colons) that will be
--         searched for pixmaps referenced in RC files.
--     im_module_file pathname
--         Sets the pathname for the IM modules file. Setting this from RC
--         files is deprecated; you should use the environment variable
--         GTK_IM_MODULE_FILE instead.
--     style name [ = parent ] { ... }
--         Declares a style.
--     widget pattern [ style | binding ][ : priority ] name
--         Specifies a style or binding set for a particular group of widgets
--         by matching on the widget pathname.
--     widget_class pattern [ style | binding ][ : priority ] name
--         Specifies a style or binding set for a particular group of widgets
--         by matching on the class pathname.
--     setting = value
--         Specifies a value for a setting. Note that settings in RC files are
--         overwritten by system-wide settings which are managed by an
--         XSettings manager. See Gtk.Settings.
--
--  Styles
--  ======
--
--  A RC style is specified by a style declaration in a RC file, and then bound
--  to widgets with a widget, widget_class, or class declaration. All styles
--  applying to a particular widget are composited together with widget
--  declarations overriding widget_class declarations which, in turn, override
--  class declarations. Within each type of declaration, later declarations
--  override earlier ones.
--
--  Within a style declaration, the possible elements are:
--     bg[state] = color
--       Sets the color used for the background of most widgets.
--     fg[state] = color
--       Sets the color used for the foreground of most widgets.
--     base[state] = color
--       Sets the color used for the background of widgets displaying editable
--       text. This color is used for the background of, among others,
--       Gtk_Text, Gtk_Entry, Gtk_List, and Gtk_CList.
--     text[state] = color
--       Sets the color used for foreground of widgets using base for the
--       background color.
--     xthickness = number
--       Sets the xthickness, which is used for various horizontal padding
--       values in GTK+.
--     ythickness = number
--       Sets the ythickness, which is used for various vertical padding
--       values in GTK+.
--     bg_pixmap[state] = pixmap
--       Sets a background pixmap to be used in place of the bg color (or for
--       GtkText, in place of the base color. The special value "<parent>" may
--       be used to indicate that the widget should use the same background
--       pixmap as its parent. The special value "<none>" may be used to
--       indicate no background pixmap.
--     font = font
--     fontset = font
--       Starting with GTK+ 2.0, the "font" and "fontset" declarations are
--       ignored; use "font_name" declarations instead.
--     font_name = font
--       Sets the font for a widget. font must be a Pango font name, e.g. "Sans
--       Italic 10". For details about Pango font names, see
--       Pango.Font.Font_Description_From_String.
--     stock["stock-id"] = { icon source specifications }
--       Defines the icon for a stock item.
--     engine "engine" { engine-specific settings }
--       Defines the engine to be used when drawing with this style.
--     class::property = value
--       Sets a style property for a widget class.
--
--  The colors and background pixmaps are specified as a function of the state
--  of the widget. The states are:
--     NORMAL
--       A color used for a widget in its normal state.
--     ACTIVE
--       A variant of the NORMAL color used when the widget is in the
--       GTK_STATE_ACTIVE state, and also for the trough of a ScrollBar, tabs
--       of a NoteBook other than the current tab and similar areas.
--       Frequently, this should be a darker variant of the NORMAL color.
--     PRELIGHT
--       A color used for widgets in the GTK_STATE_PRELIGHT state. This state
--       is the used for Buttons and MenuItems that have the mouse cursor over
--       them, and for their children.
--     SELECTED
--       A color used to highlight data selected by the user. for instance, the
--       selected items in a list widget, and the selection in an editable
--       widget.
--     INSENSITIVE
--       A color used for the background of widgets that have been set
--       insensitive with Gtk.Widget.Set_Sensitive().
--
--  Colors can be specified as a string containing a color name (GTK+ knows all
--  names from the X color database /usr/lib/X11/rgb.txt), in one of the
--  hexadecimal forms #rrrrggggbbbb, #rrrgggbbb, #rrggbb, or #rgb, where r, g
--  and b are hex digits, or they can be specified as a triplet { r, g, b},
--  where r, g and b are either integers in the range 0-65535 or floats in the
--  range 0.0-1.0.
--
--  In a stock definition, icon sources are specified as a 4-tuple of image
--  filename or icon name, text direction, widget state, and size, in that
--  order. Each icon source specifies an image filename or icon name to use
--  with a given direction, state, and size. Filenames are specified as a
--  string such as "itemltr.png", while icon names (looked up in the current
--  icon theme), are specified with a leading @, such as @"item-ltr". The *
--  character can be used as a wildcard, and if direction/state/size are
--  omitted they default to *. So for example, the following specifies
--  different icons to use for left-to-right and right-to-left languages:
--
--     stock["my-stock-item"] = {
--        { "itemltr.png", LTR, *, * },
--        { "itemrtl.png", RTL, *, * }}
--
--  This could be abbreviated as follows:
--
--     stock["my-stock-item"] = {
--        { "itemltr.png", LTR },
--        { "itemrtl.png", RTL }}
--
--  You can specify custom icons for specific sizes, as follows:
--
--     stock["my-stock-item"] = {
--        { "itemmenusize.png", *, *, "gtk-menu" },
--        { "itemtoolbarsize.png", *, *, "gtk-large-toolbar" }
--        { "itemgeneric.png" }} /* implicit *, *, * as a fallback */
--
--  The sizes that come with GTK+ itself are "gtk-menu", "gtk-small-toolbar",
--  "gtk-large-toolbar", "gtk-button", "gtk-dialog". Applications can define
--  other sizes (see also Gtk.Icon_Factory to learn more about this)
--
--  It's also possible to use custom icons for a given state, for example:
--
--     stock["my-stock-item"] = {
--        { "itemprelight.png", *, PRELIGHT },
--        { "iteminsensitive.png", *, INSENSITIVE },
--        { "itemgeneric.png" }} /* implicit *, *, * as a fallback */
--
--  When selecting an icon source to use, GTK+ will consider text direction
--  most important, state second, and size third. It will select the best match
--  based on those criteria. If an attribute matches exactly (e.g. you
--  specified PRELIGHT or specified the size), GTK+ won't modify the image; if
--  the attribute matches with a wildcard, GTK+ will scale or modify the image
--  to match the state and size the user requested.
--
--  Key bindings
--  ============
--
--  Key bindings allow the user to specify actions to be taken on particular
--  key presses. The form of a binding set declaration is:
--
--    binding name {
--       bind key {
--         signalname (param, ...)
--         ...
--       }
--       ...
--    }
--
--  key is a string consisting of a series of modifiers followed by the name of
--  a key. The modifiers can be:
--     <alt>, <control>, <mod1>, <mod2>, <mod3>, <mod4>, <mod5>
--     <release>, <shft>, <shift>
--  <shft> is an alias for <shift> and <alt> is an alias for <mod1>.
--
--  The action that is bound to the key is a sequence of signal names (strings)
--  followed by parameters for each signal. The signals must be action signals.
--  Each parameter can be a float, integer, string, or unquoted string
--  representing an enumeration value. The types of the parameters specified
--  must match the types of the parameters of the signal.
--
--  Binding sets are connected to widgets in the same manner as styles, with
--  one difference: Binding sets override other binding sets first by pattern
--  type, then by priority and then by order of specification. The priorities
--  that can be specified and their default values are the same as for styles.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Configuration and Themes</group>
--  <see>gtk-bindings.ads</see>

with Glib.Object;
with Gtk.Settings;
with Gtk.Style; use Gtk.Style;
with Gtk.Widget;
with Gtkada.Types; use Gtkada.Types;

package Gtk.Rc is

   type Gtk_Rc_Style_Record is new Glib.Object.GObject_Record with private;
   type Gtk_Rc_Style is access all Gtk_Rc_Style_Record'Class;

   procedure Gtk_New (Rc_Style : out Gtk_Rc_Style);

   procedure Initialize (Rc_Style : access Gtk_Rc_Style_Record'Class);

   function Get_Type return Glib.GType;
   --  Return the internal value associated with Gtk_Rc_Style.

   function Copy (Orig : access Gtk_Rc_Style_Record) return Gtk_Rc_Style;
   --  Make a copy of the specified Gtk_Rc.Style.
   --  This function will correctly copy an rc style that is a member of a
   --  class derived from Gtk_Rc_Style.

   procedure Add_Default_File (Filename : String);
   --  Add a file to the list of files to be parsed at the end of Gtk.Main.Init

   procedure Set_Default_Files (Filenames : Chars_Ptr_Array);
   function Get_Default_Files return Chars_Ptr_Array;
   --  Set the list of files that GtkAda will read at the end of Gtk.Main.Init

   function Get_Style
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) return Gtk_Style;
   --  Find all matching RC styles for a given widget, composites them
   --  together, and then create a Gtk_Style representing the composite
   --  appearance. (GtkAda actually keeps a cache of previously created styles,
   --  so a new style may not be created)
   --  Return the resulting style. No refcount is added to the returned style,
   --  so if you want to save this style around, you should add a reference
   --  yourself.

   procedure Parse (Filename : String);
   procedure Parse_String (Rc_String : String);
   --  Parse either a file or a string containing a gtk+ configuration (see the
   --  description at the top of this package).

   function Reparse_All return Boolean;
   --  If the modification time on any previously read file for the
   --  default Gtk_Settings has changed, discard all style information
   --  and then reread all previously read RC files.
   --  Return True if the files were reread.

   function Find_Module_In_Path (Module_File : String) return String;

   function Get_Theme_Dir return String;
   --  Returns the standard directory in which themes should be installed.
   --  (GTK+ does not actually use this directory itself.)

   function Get_Module_Dir return String;
   --  Returns a directory in which GTK+ looks for theme engines. This is a
   --  dynamic library loaded by gtk+ that will be responsible for drawing
   --  parts of the application (ie implement all the functions in Gtk.Style)

   function Get_Im_Module_Path return String;
   --  Obtains the path in which to look for IM modules. See the documentation
   --  of the GTK_PATH environment variable for more details about looking up
   --  modules. This function is useful solely for utilities supplied with GTK+
   --  and should not be used by applications under normal circumstances.

   function Get_Im_Module_File return String;
   --  Obtains the path to the IM modules file. See the documentation of the
   --  GTK_IM_MODULE_FILE environment variable for more details.

   function Reparse_All_For_Settings
     (Settings   : access Gtk.Settings.Gtk_Settings_Record'Class;
      Force_Load : Boolean)
      return Boolean;
   --  If the modification time on any previously read file
   --  for the given Gtk_Settings has changed, discard all style information
   --  and then reread all previously read RC files.
   --  If Force_Load is true, the files are reloaded even if unmodified.
   --  Return True if some files have been reparsed

   procedure Reset_Styles
     (Settings : access Gtk.Settings.Gtk_Settings_Record'Class);
   --  This function recomputes the styles for all widgets that use a
   --  particular Gtk_Settings object. (There is one Gtk_Settings object
   --  per Gdk_Screen, see Gtk.Settings.Get_For_Screen); It is useful
   --  when some global parameter has changed that affects the appearance
   --  of all widgets, because when a widget gets a new style, it will
   --  both redraw and recompute any cached information about its
   --  appearance. As an example, it is used when the default font size
   --  set by the operating system changes. Note that this function
   --  doesn't affect widgets that have a style set explicitely on them
   --  with Gtk.Widget.Set_Style.

   function Get_Style_By_Paths
     (Settings    : access Gtk.Settings.Gtk_Settings_Record'Class;
      Widget_Path : String := "";
      Class_Path  : String := "";
      Typ         : Glib.GType := Glib.GType_None)
     return Gtk.Style.Gtk_Style;
   --  Creates up a Gtk_Style from styles defined in a RC file by providing
   --  the raw components used in matching. This function may be useful
   --  when creating pseudo-widgets that should be themed like widgets but
   --  don't actually have corresponding GTK+ widgets. An example of this
   --  would be items inside a GNOME canvas widget.
   --  Returns null if nothing matching was found and the default style should
   --  be used. You must call Ref if you intend to keep a reference on the
   --  style.

   ------------------------------
   -- Widget related functions --
   ------------------------------

   procedure Modify_Style
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Style  : access Gtk_Rc_Style_Record'Class);
   --  Modifies style values on the widget. Modifications made using this
   --  technique take precedence over style values set via an RC file,
   --  however, they will be overriden if a style is explicitely set on
   --  the widget using gtk_widget_set_style(). The #GtkRcStyle structure
   --  is designed so each field can either be set or unset, so it is
   --  possible, using this function, to modify some style values and
   --  leave the others unchanged.
   --
   --  Note that modifications made with this function are not cumulative
   --  with previous calls to gtk_widget_modify_style() or with such
   --  functions as gtk_widget_modify_fg(). If you wish to retain
   --  previous values, you must first call gtk_widget_get_modifier_style(),
   --  make your modifications to the returned style, then call
   --  gtk_widget_modify_style() with that style. On the other hand,
   --  if you first call gtk_widget_modify_style(), subsequent calls
   --  to such functions gtk_widget_modify_fg() will have a cumulative
   --  effect with the initial modifications.

   function Get_Modifier_Style
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) return Gtk_Rc_Style;
   --  Return the current modifier style for the widget.
   --  (As set by Modify_Style.) If no style has previously set, a new
   --  Gtk_Rc_Style will be created with all values unset, and set as the
   --  modifier style for the widget. If you make changes to this rc
   --  style, you must call Modify_Style, passing in the
   --  returned rc style, to make sure that your changes take effect.
   --
   --  Return value: the modifier style for the widget. This rc style is
   --    owned by the widget. If you want to keep a pointer to value this
   --    around, you must add a refcount using Ref.

private
   type Gtk_Rc_Style_Record is new Glib.Object.GObject_Record with null record;

   pragma Import (C, Get_Type, "gtk_rc_style_get_type");
end Gtk.Rc;

--  The following functions never had a binding and are now obsolescent:
--  No binding: gtk_rc_add_class_style
--  No binding: gtk_rc_add_widget_class_style
--  No binding: gtk_rc_add_widget_name_style

--  These functions do not seem to be needed for normal applications:
--  No binding: gtk_rc_parse_color
--  No binding: gtk_rc_parse_priority
--  No binding: gtk_rc_parse_state
--  No binding: gtk_rc_scanner_new
--  No binding: gtk_rc_style_ref
--  No binding: gtk_rc_style_unref
--  No binding: gtk_rc_find_pixmap_in_path
