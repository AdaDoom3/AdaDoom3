-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
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
--  This package contains various subprograms to easily share settings between
--  applications, or even between various parts of your application.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Configuration and Themes</group>

with Gdk;
with Glib.Object;
with Glib.Properties;
with Glib.Values;
with Gtk.Style;
with Interfaces.C.Strings;

package Gtk.Settings is

   type Gtk_Settings_Record is new Glib.Object.GObject_Record with null record;
   type Gtk_Settings is access all Gtk_Settings_Record'Class;

   function Get_Default return Gtk_Settings;
   --  Gets the settings object for the default GDK screen, creating
   --  it if necessary.

   function Get_For_Screen (Screen : Gdk.Gdk_Screen) return Gtk_Settings;
   --  Gets the settings object for Screen, creating it if necessary.

   function Get_Type return Glib.GType;
   --  Return the internal type used to identify a Gtk_Settings

   procedure Install_Property (Pspec : Glib.Param_Spec);
   --  Declares a property that can be shared among various parts of the
   --  application

   procedure Install_Property_Parser
     (Pspec  : Glib.Param_Spec;
      Parser : Gtk.Style.Gtk_Rc_Property_Parser);
   --  Install a new parser for the given property. This parser is responsible
   --  for reading the property's value in a gtk configuration file, and
   --  convert it to a suitable value.

   --------------------------------
   -- Precoded parsing functions --
   --------------------------------

   function Parse_Color
     (Pspec     : Glib.Param_Spec;
      Rc_String : Interfaces.C.Strings.chars_ptr;
      Value     : access Glib.Values.GValue) return Gboolean;
   function Parse_Enum
     (Pspec     : Glib.Param_Spec;
      Rc_String : Interfaces.C.Strings.chars_ptr;
      Value     : access Glib.Values.GValue) return Gboolean;
   function Parse_Flags
     (Pspec     : Glib.Param_Spec;
      Rc_String : Interfaces.C.Strings.chars_ptr;
      Value     : access Glib.Values.GValue) return Gboolean;
   function Parse_Requisition
     (Pspec     : Glib.Param_Spec;
      Rc_String : Interfaces.C.Strings.chars_ptr;
      Value     : access Glib.Values.GValue) return Gboolean;
   function Parse_Border
     (Pspec     : Glib.Param_Spec;
      Rc_String : Interfaces.C.Strings.chars_ptr;
      Value     : access Glib.Values.GValue) return Gboolean;
   --  These functions parse some of the predefined property types

   -----------------------------------
   -- Setting predefined properties --
   -----------------------------------

   procedure Set_Property_Value
     (Settings : access Gtk_Settings_Record;
      Name     : String;
      Value    : Glib.Values.GValue;
      Origin   : String);
   procedure Set_String_Property
     (Settings : access Gtk_Settings_Record;
      Name     : String;
      Value    : String;
      Origin   : String);
   procedure Set_Long_Property
     (Settings : access Gtk_Settings_Record;
      Name     : String;
      Value    : Glong;
      Origin   : String);
   procedure Set_Double_Property
     (Settings : access Gtk_Settings_Record;
      Name     : String;
      Value    : Gdouble;
      Origin   : String);
   --  Set the value of a property. This automatically propagates the new
   --  value to all listeners, so that they can refresh themselves.
   --  Origin should be something like "filename:line" for rc files, or the
   --  name of the function that sets it otherwise

   ----------------
   -- Properties --
   ----------------
   --  The following settings are predefined:

   use Glib.Properties;

   Gtk_Alternative_Button_Order     : constant Property_Boolean;
   --  Whether buttons in dialogs should use the alternative button order.
   --
   --  Default value: FALSE

   Gtk_Alternative_Sort_Arrows      : constant Property_Boolean;
   --  Controls the direction of the sort indicators in sorted list and tree
   --  views. By default an arrow pointing down means the column is sorted in
   --  ascending order. When set to TRUE, this order will be inverted.
   --
   --  Default value: FALSE

   Gtk_Auto_Mnemonics               : constant Property_Boolean;
   --  Controls the direction of the sort indicators in sorted list and tree
   --  views. By default an arrow pointing down means the column is sorted in
   --  ascending order. When set to TRUE, this order will be inverted.
   --  Default value: FALSE

   Gtk_Button_Images                : constant Property_Boolean;
   --  Whether images should be shown on buttons.
   --  Default value: TRUE

   Gtk_Can_Change_Accels            : constant Property_Boolean;
   --  Whether menu accelerators can be changed by pressing a key over the menu
   --  item.
   --  Default value: FALSE

   Gtk_Color_Palette                : constant Property_String;
   --  Palette to use in the color selector.
   --
   --  Default value: "black:white:gray50:red:purple:blue:light
   --  blue:green:yellow:orange:lavender:brown:goldenrod4:dodger
   --  blue:pink:light green:gray10:gray30:gray75:gray90"

   Gtk_Color_Scheme                 : constant Property_String;
   --  A palette of named colors for use in themes. The format of the string is
   --    name1: color1
   --    name2: color2
   --    ...
   --  Color names must be acceptable as identifiers in the gtkrc syntax, and
   --  color specifications must be in the format accepted by Gdk.Color.Parse.
   --
   --  Note that due to the way the color tables from different sources are
   --  merged, color specifications will be converted to hexadecimal form
   --  when getting this property.
   --
   --  Starting with GTK+ 2.12, the entries can alternatively be separated by
   --  ';' instead of newlines:
   --    name1: color1; name2: color2; ...
   --
   --  Default value: ""

   Gtk_Cursor_Blink                 : constant Property_Boolean;
   --  Whether the cursor should blink.
   --
   --  Also see the Gtk_Cursor_Blink_Timeout setting, which allows more
   --  flexible control over cursor blinking.
   --
   --  Default value: TRUE

   Gtk_Cursor_Blink_Time            : constant Property_Int;
   --  Length of the cursor blink cycle, in milliseconds.
   --
   --  Allowed values: >= 100
   --
   --  Default value: 1200

   Gtk_Cursor_Blink_Timeout         : constant Property_Int;
   --  Time after which the cursor stops blinking, in seconds. The timer is
   --  reset after each user interaction.
   --
   --  Setting this to zero has the same effect as setting Gtk_Cursor_Blink to
   --  FALSE.
   --
   --  Allowed values: >= 1
   --
   --  Default value: 2147483647

   Gtk_Cursor_Theme_Name            : constant Property_String;
   --  Name of the cursor theme to use, or NULL to use the default theme.
   --
   --  Default value: NULL

   Gtk_Cursor_Theme_Size            : constant Property_Int;
   --  Size to use for cursors, or 0 to use the default size.
   --
   --  Allowed values: [0,128]
   --
   --  Default value: 0

   Gtk_Dnd_Drag_Threshold           : constant Property_Int;
   --  Number of pixels the cursor can move before dragging.
   --
   --  Allowed values: >= 1
   --
   --  Default value: 8

   Gtk_Double_Click_Distance        : constant Property_Int;
   --  Maximum distance allowed between two clicks for them to be considered a
   --  double click (in pixels).
   --
   --  Allowed values: >= 0
   --
   --  Default value: 5

   Gtk_Double_Click_Time            : constant Property_Int;
   --  Maximum time allowed between two clicks for them to be considered a
   --  double click (in milliseconds).
   --
   --  Allowed values: >= 0
   --
   --  Default value: 250

   Gtk_Enable_Accels                : constant Property_Boolean;
   --  Whether menu items should have visible accelerators which can be
   --  activated.
   --
   --  Default value: TRUE

   Gtk_Enable_Animations            : constant Property_Boolean;
   --  Whether to enable toolkit-wide animations.
   --
   --  Default value: TRUE

   Gtk_Enable_Event_Sounds          : constant Property_Boolean;
   --  Whether to play any event sounds at all.
   --
   --  See the Sound Theme spec for more information on event sounds and sound
   --  themes.
   --
   --  GTK+ itself does not support event sounds, you have to use a loadable
   --  module like the one that comes with libcanberra.
   --
   --  Default value: TRUE

   Gtk_Enable_Input_Feedback_Sounds : constant Property_Boolean;
   --  Whether to play event sounds as feedback to user input.
   --
   --  See the Sound Theme spec for more information on event sounds and sound
   --  themes.
   --
   --  GTK+ itself does not support event sounds, you have to use a loadable
   --  module like the one that comes with libcanberra.
   --
   --  Default value: TRUE

   Gtk_Enable_Mnemonics             : constant Property_Boolean;
   --  Whether labels and menu items should have visible mnemonics which can be
   --  activated.
   --
   --  Default value: TRUE

   Gtk_Enable_Tooltips              : constant Property_Boolean;
   --  Whether tooltips should be shown on widgets.
   --
   --  Default value: TRUE

   Gtk_Entry_Password_Hint_Timeout  : constant Property_Uint;
   --  How long to show the last input character in hidden entries. This value
   --  is in milliseconds. 0 disables showing the last char. 600 is a good
   --  value for enabling it.
   --
   --  Default value: 0

   Gtk_Entry_Select_On_Focus        : constant Property_Boolean;
   --  Whether to select the contents of an entry when it is focused.
   --
   --  Default value: TRUE

   Gtk_Error_Bell                   : constant Property_Boolean;
   --  When TRUE, keyboard navigation and other input-related errors will cause
   --  a beep. Since the error bell is implemented using gdk_window_beep(), the
   --  windowing system may offer ways to configure the error bell in many
   --  ways, such as flashing the window or similar visual effects.
   --
   --  Default value: TRUE

   Gtk_Fallback_Icon_Theme          : constant Property_String;
   --  Name of a icon theme to fall back to.
   --
   --  Default value: NULL

   Gtk_File_Chooser_Backend         : constant Property_String;
   --  Name of the GtkFileChooser backend to use by default.
   --
   --  Default value: NULL

   Gtk_Font_Name                    : constant Property_String;
   --  Name of default font to use.
   --
   --  Default value: "Sans 10"

   Gtk_Fontconfig_Timestamp         : constant Property_Uint;
   --  Timestamp of current fontconfig configuration.
   --
   --  Default value: 0

   Gtk_Icon_Sizes                   : constant Property_String;
   --  A list of icon sizes. The list is separated by colons, and item has the
   --  form:
   --
   --  size-name = width , height
   --
   --  E.g. "gtk-menu=16,16:gtk-button=20,20:gtk-dialog=48,48". GTK+ itself use
   --  the following named icon sizes: gtk-menu, gtk-button, gtk-small-toolbar,
   --  gtk-large-toolbar, gtk-dnd, gtk-dialog. Applications can register their
   --  own named icon sizes with gtk_icon_size_register().
   --
   --  Default value: NULL

   Gtk_Icon_Theme_Name              : constant Property_String;
   --  Name of icon theme to use.
   --
   --  Default value: "hicolor"

   Gtk_Im_Module                    : constant Property_String;
   --  Which IM (input method) module should be used by default. This is the
   --  input method that will be used if the user has not explicitly chosen
   --  another input method from the IM context menu.
   --
   --  See GtkIMContext and see the "gtk-show-input-method-menu" property.
   --
   --  Default value: NULL

   Gtk_Key_Theme_Name               : constant Property_String;
   --  Name of key theme RC file to load.
   --
   --  Default value: NULL

   Gtk_Keynav_Cursor_Only           : constant Property_Boolean;
   --  When TRUE, keyboard navigation should be able to reach all widgets by
   --  using the cursor keys only. Tab, Shift etc. keys can't be expected to
   --  be present on the used input device.
   --
   --  Default value: FALSE

   Gtk_Keynav_Wrap_Around           : constant Property_Boolean;
   --  When TRUE, some widgets will wrap around when doing keyboard navigation,
   --  such as menus, menubars and notebooks.
   --
   --  Default value: TRUE

   Gtk_Label_Select_On_Focus        : constant Property_Boolean;
   --  Whether to select the contents of a selectable label when it is focused.
   --
   --  Default value: TRUE

   Gtk_Menu_Bar_Accel               : constant Property_String;
   --  Keybinding to activate the menu bar.
   --
   --  Default value: "F10"

   Gtk_Menu_Bar_Popup_Delay         : constant Property_Int;
   --  Delay before the submenus of a menu bar appear.
   --
   --  Allowed values: >= 0
   --
   --  Default value: 0

   Gtk_Menu_Images                  : constant Property_Boolean;
   --  Whether images should be shown in menus.
   --
   --  Default value: TRUE

   Gtk_Menu_Popdown_Delay           : constant Property_Int;
   --  The time before hiding a submenu when the pointer is moving towards the
   --  submenu.
   --
   --  Allowed values: >= 0
   --
   --  Default value: 1000

   Gtk_Menu_Popup_Delay             : constant Property_Int;
   --  Minimum time the pointer must stay over a menu item before the submenu
   --  appear.
   --
   --  Allowed values: >= 0
   --
   --  Default value: 225

   Gtk_Modules                      : constant Property_String;
   --  List of currently active GTK modules.
   --
   --  Default value: NULL

   Gtk_Print_Backend                : constant Property_String;
   --  A comma-separated list of print backends to use in the print dialog.
   --  Available print backends depend on the GTK+ installation, and may
   --  include "file", "cups", "lpr" or "papi".
   --
   --  Default value: "file,cups"

   Gtk_Print_Preview_Command        : constant Property_String;
   --  A command to run for displaying the print preview. The command should
   --  contain a f placeholder, which will get replaced by the path to the
   --  pdf file. The command may also contain a s placeholder, which will get
   --  replaced by the path to a file containing the print settings in the
   --  format produced by Gtk.Print_Settings.To_File.
   --
   --  The preview application is responsible for removing the pdf file and the
   --  print settings file when it is done.
   --
   --  Default value:
   --    "evince --unlink-tempfile --preview --print-settings %s %f"

   Gtk_Recent_Files_Limit           : constant Property_Int;
   --  The number of recently used files that should be displayed by default by
   --  GtkRecentChooser implementations and by the GtkFileChooser. A value of
   --  -1 means every recently used file stored.
   --
   --  Allowed values: >= G_MAXULONG
   --
   --  Default value: 50

   Gtk_Recent_Files_Max_Age         : constant Property_Int;
   --  The maximum age, in days, of the items inside the recently used
   --  resources list. Items older than this setting will be excised from the
   --  list. If set to 0, the list will always be empty; if set to -1, no item
   --  will be removed.
   --
   --  Allowed values: >= G_MAXULONG
   --
   --  Default value: 30

   Gtk_Show_Input_Method_Menu       : constant Property_Boolean;
   --  Whether the context menus of entries and text views should offer to
   --  change the input method.
   --
   --  Default value: TRUE

   Gtk_Show_Unicode_Menu            : constant Property_Boolean;
   --  Whether the context menus of entries and text views should offer to
   --  insert control characters.
   --
   --  Default value: TRUE

   Gtk_Sound_Theme_Name             : constant Property_String;
   --  The XDG sound theme to use for event sounds.
   --
   --  See the Sound Theme spec for more information on event sounds and sound
   --  themes.
   --
   --  GTK+ itself does not support event sounds, you have to use a loadable
   --  module like the one that comes with libcanberra.
   --
   --  Default value: "freedesktop"

   Gtk_Split_Cursor                 : constant Property_Boolean;
   --  Whether two cursors should be displayed for mixed left-to-right and
   --  right-to-left text.
   --
   --  Default value: TRUE

   Gtk_Theme_Name                   : constant Property_String;
   --  Name of theme RC file to load.
   --
   --  Default value: "Raleigh"

   Gtk_Timeout_Expand               : constant Property_Int;
   --  Expand value for timeouts, when a widget is expanding a new region.
   --
   --  Allowed values: >= 0
   --
   --  Default value: 500

   Gtk_Timeout_Initial              : constant Property_Int;
   --  Starting value for timeouts, when button is pressed.
   --
   --  Allowed values: >= 0
   --
   --  Default value: 200

   Gtk_Timeout_Repeat               : constant Property_Int;
   --  Repeat value for timeouts, when button is pressed.
   --
   --  Allowed values: >= 0
   --
   --  Default value: 20

   Gtk_Toolbar_Icon_Size            : constant Property_Enum;
   --  The size of icons in default toolbars.
   --
   --  Default value: GTK_ICON_SIZE_LARGE_TOOLBAR

   Gtk_Toolbar_Style                : constant Property_Enum;
   --  Whether default toolbars have text only, text and icons, icons only,
   --  etc.
   --
   --  Default value: GTK_TOOLBAR_BOTH

   Gtk_Tooltip_Browse_Mode_Timeout  : constant Property_Int;
   --  Amount of time, in milliseconds, after which the browse mode will be
   --  disabled.
   --
   --  See Gtk_Tooltip_Browse_Timeout" for more information about browse mode.
   --
   --  Allowed values: >= 0
   --
   --  Default value: 500

   Gtk_Tooltip_Browse_Timeout       : constant Property_Int;
   --  Controls the time after which tooltips will appear when browse mode is
   --  enabled, in milliseconds.
   --
   --  Browse mode is enabled when the mouse pointer moves off an object
   --  where a tooltip was currently being displayed. If the mouse pointer
   --  hits another object before the browse mode timeout expires (see
   --  Gtk_Tooltip_Browse_Mode_Timeout), it will take the amount of
   --  milliseconds specified by this setting to popup the tooltip for the
   --  new object.
   --
   --  Allowed values: >= 0
   --
   --  Default value: 60

   Gtk_Tooltip_Timeout              : constant Property_Int;
   --  Time, in milliseconds, after which a tooltip could appear if the cursor
   --  is hovering on top of a widget.
   --
   --  Allowed values: >= 0
   --
   --  Default value: 500

   Gtk_Touchscreen_Mode             : constant Property_Boolean;
   --  When TRUE, there are no motion notify events delivered on this screen,
   --  and widgets can't use the pointer hovering them for any essential
   --  functionality.
   --
   --  Default value: FALSE

   Gtk_Xft_Antialias                : constant Property_Int;
   --  Whether to antialias Xft fonts; 0=no, 1=yes, -1=default.
   --
   --  Allowed values: [G_MAXULONG,1]
   --
   --  Default value: -1

   Gtk_Xft_Dpi                      : constant Property_Int;
   --  Resolution for Xft, in 1024 * dots/inch. -1 to use default value.
   --
   --  Allowed values: [G_MAXULONG,1048576]
   --
   --  Default value: -1

   Gtk_Xft_Hinting                  : constant Property_Int;
   --  Whether to hint Xft fonts; 0=no, 1=yes, -1=default.
   --
   --  Allowed values: [G_MAXULONG,1]
   --
   --  Default value: -1

   Gtk_Xft_Hintstyle                : constant Property_String;
   --  What degree of hinting to use; hintnone, hintslight, hintmedium, or
   --  hintfull.
   --
   --  Default value: NULL

   Gtk_Xft_Rgba                     : constant Property_String;
   --  Type of subpixel antialiasing; none, rgb, bgr, vrgb, vbgr.
   --
   --  Default value: NULL

private

   Gtk_Alternative_Button_Order     : constant Property_Boolean :=
                                        Build ("gtk-alternative-button-order");
   Gtk_Alternative_Sort_Arrows      : constant Property_Boolean :=
                                        Build ("gtk-alternative-sort-arrows");
   Gtk_Auto_Mnemonics               : constant Property_Boolean :=
                                        Build ("gtk-auto-mnemonics");
   Gtk_Button_Images                : constant Property_Boolean :=
                                        Build ("gtk-button-images");
   Gtk_Can_Change_Accels            : constant Property_Boolean :=
                                        Build ("gtk-can-change-accels");
   Gtk_Color_Palette                : constant Property_String :=
                                        Build ("gtk-color-palette");
   Gtk_Color_Scheme                 : constant Property_String :=
                                        Build ("gtk-color-scheme");
   Gtk_Cursor_Blink                 : constant Property_Boolean :=
                                        Build ("gtk-cursor-blink");
   Gtk_Cursor_Blink_Time            : constant Property_Int :=
                                        Build ("gtk-cursor-blink-time");
   Gtk_Cursor_Blink_Timeout         : constant Property_Int :=
                                        Build ("gtk-cursor-blink-timeout");
   Gtk_Cursor_Theme_Name            : constant Property_String :=
                                        Build ("gtk-cursor-theme-name");
   Gtk_Cursor_Theme_Size            : constant Property_Int :=
                                        Build ("gtk-cursor-theme-size");
   Gtk_Dnd_Drag_Threshold           : constant Property_Int :=
                                        Build ("gtk-dnd-drag-threshold");
   Gtk_Double_Click_Distance        : constant Property_Int :=
                                        Build ("gtk-double-click-distance");
   Gtk_Double_Click_Time            : constant Property_Int :=
                                        Build ("gtk-double-click-time");
   Gtk_Enable_Accels                : constant Property_Boolean :=
                                        Build ("gtk-enable-accels");
   Gtk_Enable_Animations            : constant Property_Boolean :=
                                        Build ("gtk-enable-animations");
   Gtk_Enable_Event_Sounds          : constant Property_Boolean :=
                                        Build ("gtk-enable-event-sounds");
   Gtk_Enable_Input_Feedback_Sounds : constant Property_Boolean :=
                                    Build ("gtk-enable-input-feedback-sounds");
   Gtk_Enable_Mnemonics             : constant Property_Boolean :=
                                        Build ("gtk-enable-mnemonics");
   Gtk_Enable_Tooltips              : constant Property_Boolean :=
                                        Build ("gtk-enable-tooltips");
   Gtk_Entry_Password_Hint_Timeout  : constant Property_Uint :=
                                    Build ("gtk-entry-password-hint-timeout");
   Gtk_Entry_Select_On_Focus        : constant Property_Boolean :=
                                        Build ("gtk-entry-select-on-focus");
   Gtk_Error_Bell                   : constant Property_Boolean :=
                                        Build ("gtk-error-bell");
   Gtk_Fallback_Icon_Theme          : constant Property_String :=
                                        Build ("gtk-fallback-icon-theme");
   Gtk_File_Chooser_Backend         : constant Property_String :=
                                        Build ("gtk-file-chooser-backend");
   Gtk_Font_Name                    : constant Property_String :=
                                        Build ("gtk-font-name");
   Gtk_Fontconfig_Timestamp         : constant Property_Uint :=
                                        Build ("gtk-fontconfig-timestamp");
   Gtk_Icon_Sizes                   : constant Property_String :=
                                        Build ("gtk-icon-sizes");
   Gtk_Icon_Theme_Name              : constant Property_String :=
                                        Build ("gtk-icon-theme-name");
   Gtk_Im_Module                    : constant Property_String :=
                                        Build ("gtk-im-module");
   Gtk_Key_Theme_Name               : constant Property_String :=
                                        Build ("gtk-key-theme-name");
   Gtk_Keynav_Cursor_Only           : constant Property_Boolean :=
                                        Build ("gtk-keynav-cursor-only");
   Gtk_Keynav_Wrap_Around           : constant Property_Boolean :=
                                        Build ("gtk-keynav-wrap-around");
   Gtk_Label_Select_On_Focus        : constant Property_Boolean :=
                                        Build ("gtk-label-select-on-focus");
   Gtk_Menu_Bar_Accel               : constant Property_String :=
                                        Build ("gtk-menu-bar-accel");
   Gtk_Menu_Bar_Popup_Delay         : constant Property_Int :=
                                        Build ("gtk-menu-bar-popup-delay");
   Gtk_Menu_Images                  : constant Property_Boolean :=
                                        Build ("gtk-menu-images");
   Gtk_Menu_Popdown_Delay           : constant Property_Int :=
                                        Build ("gtk-menu-popdown-delay");
   Gtk_Menu_Popup_Delay             : constant Property_Int :=
                                        Build ("gtk-menu-popup-delay");
   Gtk_Modules                      : constant Property_String :=
                                        Build ("gtk-modules");
   Gtk_Print_Backend                : constant Property_String :=
                                        Build ("gtk-print-backend");
   Gtk_Print_Preview_Command        : constant Property_String :=
                                        Build ("gtk-print-preview-command");
   Gtk_Recent_Files_Limit           : constant Property_Int :=
                                        Build ("gtk-recent-files-limit");
   Gtk_Recent_Files_Max_Age         : constant Property_Int :=
                                        Build ("gtk-recent-files-max-age");
   Gtk_Show_Input_Method_Menu       : constant Property_Boolean :=
                                        Build ("gtk-show-input-method-menu");
   Gtk_Show_Unicode_Menu            : constant Property_Boolean :=
                                        Build ("gtk-show-unicode-menu");
   Gtk_Sound_Theme_Name             : constant Property_String :=
                                        Build ("gtk-sound-theme-name");
   Gtk_Split_Cursor                 : constant Property_Boolean :=
                                        Build ("gtk-split-cursor");
   Gtk_Theme_Name                   : constant Property_String :=
                                        Build ("gtk-theme-name");
   Gtk_Timeout_Expand               : constant Property_Int :=
                                        Build ("gtk-timeout-expand");
   Gtk_Timeout_Initial              : constant Property_Int :=
                                        Build ("gtk-timeout-initial");
   Gtk_Timeout_Repeat               : constant Property_Int :=
                                        Build ("gtk-timeout-repeat");
   Gtk_Toolbar_Icon_Size            : constant Property_Enum :=
                                        Build ("gtk-toolbar-icon-size");
   Gtk_Toolbar_Style                : constant Property_Enum :=
                                        Build ("gtk-toolbar-style");
   Gtk_Tooltip_Browse_Mode_Timeout  : constant Property_Int :=
                                    Build ("gtk-tooltip-browse-mode-timeout");
   Gtk_Tooltip_Browse_Timeout       : constant Property_Int :=
                                        Build ("gtk-tooltip-browse-timeout");
   Gtk_Tooltip_Timeout              : constant Property_Int :=
                                        Build ("gtk-tooltip-timeout");
   Gtk_Touchscreen_Mode             : constant Property_Boolean :=
                                        Build ("gtk-touchscreen-mode");
   Gtk_Xft_Antialias                : constant Property_Int :=
                                        Build ("gtk-xft-antialias");
   Gtk_Xft_Dpi                      : constant Property_Int :=
                                        Build ("gtk-xft-dpi");
   Gtk_Xft_Hinting                  : constant Property_Int :=
                                        Build ("gtk-xft-hinting");
   Gtk_Xft_Hintstyle                : constant Property_String :=
                                        Build ("gtk-xft-hintstyle");
   Gtk_Xft_Rgba                     : constant Property_String :=
                                        Build ("gtk-xft-rgba");

   pragma Import (C, Get_Type,          "gtk_settings_get_type");
   pragma Import (C, Install_Property_Parser,
                  "gtk_settings_install_property_parser");
   pragma Import (C, Install_Property,  "gtk_settings_install_property");
   pragma Import (C, Parse_Color,       "gtk_rc_property_parse_color");
   pragma Import (C, Parse_Enum,        "gtk_rc_property_parse_enum");
   pragma Import (C, Parse_Flags,       "gtk_rc_property_parse_flags");
   pragma Import (C, Parse_Requisition, "gtk_rc_property_parse_requisition");
   pragma Import (C, Parse_Border,      "gtk_rc_property_parse_border");
end Gtk.Settings;
