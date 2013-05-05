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
--  The Gtk_About_Dialog offers a simple way to display information about a
--  program like its logo, name, copyright, website and license. It is also
--  possible to give credits to the authors, documenters, translators and
--  artists who have worked on the program. An about dialog is typically opened
--  when the user selects the About option from the Help menu. All parts of the
--  dialog are optional.
--
--  About dialog often contain links and email addresses. Gtk_About_Dialog
--  supports this by offering global hooks, which are called when the user
--  clicks on a link or email address, see Set_Email_Hook and Set_Url_Hook.
--  Email addresses in the authors, documenters and artists properties are
--  recognized by looking for <user@host>, URLs are recognized by looking for
--  http://url, with url extending to the next space, tab or line break.
--
--  To make constructing a Gtk_About_Dialog as convenient as possible, you can
--  use the function gtk_show_about_dialog which constructs and shows a dialog
--  and keeps it around so that it can be shown again.
--
--  </description>
--  <group>Windows</group>
--  <testgtk>create_about.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;         use GNAT.Strings;
with Gdk.Pixbuf;           use Gdk.Pixbuf;
with Glib;                 use Glib;
with Glib.Properties;      use Glib.Properties;
with Glib.Types;           use Glib.Types;
with Gtk.Buildable;        use Gtk.Buildable;
with Gtk.Dialog;           use Gtk.Dialog;
with Gtk.Widget;           use Gtk.Widget;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package Gtk.About_Dialog is

   type Gtk_About_Dialog_Record is new Gtk_Dialog_Record with null record;
   type Gtk_About_Dialog is access all Gtk_About_Dialog_Record'Class;

   type Activate_Link_Func is access procedure
     (About : System.Address;
      Link  : Interfaces.C.Strings.chars_ptr;
      Data  : System.Address);
   pragma Convention (C, Activate_Link_Func);
   --  A callback called when the user presses an hyper link in the about
   --  dialog. This is a low-level function, and you'll need to convert the
   --  parameters to more useful types with:
   --     Stub : Gtk_About_Dialog_Record;
   --     A    : constant Gtk_About_Dialog :=
   --       Gtk_About_Dialog (Get_User_Data (About, Stub));
   --     L    : constant String := Interfaces.C.Strings.Value (Link);

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (About : out Gtk_About_Dialog);
   procedure Initialize (About : access Gtk_About_Dialog_Record'Class);
   --  Creates a new Gtk.About_Dialog.Gtk_About_Dialog.
   --  Since: gtk+ 2.6

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_about_dialog_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Artists
      (About : access Gtk_About_Dialog_Record)
       return GNAT.Strings.String_List;
   procedure Set_Artists
      (About   : access Gtk_About_Dialog_Record;
       Artists : GNAT.Strings.String_List);
   --  Sets the strings which are displayed in the artists tab of the
   --  secondary credits dialog.
   --  Since: gtk+ 2.6
   --  "artists": a null-terminated array of strings

   function Get_Authors
      (About : access Gtk_About_Dialog_Record)
       return GNAT.Strings.String_List;
   procedure Set_Authors
      (About   : access Gtk_About_Dialog_Record;
       Authors : GNAT.Strings.String_List);
   --  Sets the strings which are displayed in the authors tab of the
   --  secondary credits dialog.
   --  Since: gtk+ 2.6
   --  "authors": a null-terminated array of strings

   function Get_Comments
      (About : access Gtk_About_Dialog_Record) return UTF8_String;
   procedure Set_Comments
      (About    : access Gtk_About_Dialog_Record;
       Comments : UTF8_String);
   --  Sets the comments string to display in the about dialog. This should be
   --  a short string of one or two lines.
   --  Since: gtk+ 2.6
   --  "comments": a comments string

   function Get_Copyright
      (About : access Gtk_About_Dialog_Record) return UTF8_String;
   procedure Set_Copyright
      (About     : access Gtk_About_Dialog_Record;
       Copyright : UTF8_String);
   --  Sets the copyright string to display in the about dialog. This should
   --  be a short string of one or two lines.
   --  Since: gtk+ 2.6
   --  "copyright": (allow-none) the copyright string

   function Get_Documenters
      (About : access Gtk_About_Dialog_Record)
       return GNAT.Strings.String_List;
   procedure Set_Documenters
      (About       : access Gtk_About_Dialog_Record;
       Documenters : GNAT.Strings.String_List);
   --  Sets the strings which are displayed in the documenters tab of the
   --  secondary credits dialog.
   --  Since: gtk+ 2.6
   --  "documenters": a null-terminated array of strings

   function Get_License
      (About : access Gtk_About_Dialog_Record) return UTF8_String;
   procedure Set_License
      (About   : access Gtk_About_Dialog_Record;
       License : UTF8_String);
   --  Sets the license information to be displayed in the secondary license
   --  dialog. If License is null, the license button is hidden.
   --  Since: gtk+ 2.6
   --  "license": the license information or null

   function Get_Logo
      (About : access Gtk_About_Dialog_Record) return Gdk.Pixbuf.Gdk_Pixbuf;
   procedure Set_Logo
      (About : access Gtk_About_Dialog_Record;
       Logo  : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);
   --  Sets the pixbuf to be displayed as logo in the about dialog. If it is
   --  null, the default window icon set with Gtk.Window.Set_Default_Icon will
   --  be used.
   --  Since: gtk+ 2.6
   --  "logo": a Gdk.Pixbuf.Gdk_Pixbuf, or null

   function Get_Logo_Icon_Name
      (About : access Gtk_About_Dialog_Record) return UTF8_String;
   procedure Set_Logo_Icon_Name
      (About     : access Gtk_About_Dialog_Record;
       Icon_Name : UTF8_String);
   --  Sets the pixbuf to be displayed as logo in the about dialog. If it is
   --  null, the default window icon set with Gtk.Window.Set_Default_Icon will
   --  be used.
   --  Since: gtk+ 2.6
   --  "icon_name": an icon name, or null

   function Get_Name
      (About : access Gtk_About_Dialog_Record) return UTF8_String;
   pragma Obsolescent (Get_Name);
   procedure Set_Name
      (About : access Gtk_About_Dialog_Record;
       Name  : UTF8_String);
   pragma Obsolescent (Set_Name);
   --  Sets the name to display in the about dialog. If this is not set, it
   --  defaults to g_get_application_name.
   --  Since: gtk+ 2.6
   --  Deprecated since 2.12, Use Gtk.About_Dialog.Set_Program_Name instead.
   --  "name": the program name

   function Get_Program_Name
      (About : access Gtk_About_Dialog_Record) return UTF8_String;
   procedure Set_Program_Name
      (About : access Gtk_About_Dialog_Record;
       Name  : UTF8_String);
   --  Sets the name to display in the about dialog. If this is not set, it
   --  defaults to g_get_application_name.
   --  Since: gtk+ 2.12
   --  "name": the program name

   function Get_Translator_Credits
      (About : access Gtk_About_Dialog_Record) return UTF8_String;
   procedure Set_Translator_Credits
      (About              : access Gtk_About_Dialog_Record;
       Translator_Credits : UTF8_String);
   --  Sets the translator credits string which is displayed in the
   --  translators tab of the secondary credits dialog. The intended use for
   --  this string is to display the translator of the language which is
   --  currently used in the user interface. Using gettext, a simple way to
   --  achieve that is to mark the string for translation: |[
   --  gtk_about_dialog_set_translator_credits (about,
   --  _("translator-credits")); ]| It is a good idea to use the customary
   --  msgid "translator-credits" for this purpose, since translators will
   --  already know the purpose of that msgid, and since
   --  Gtk.About_Dialog.Gtk_About_Dialog will detect if "translator-credits" is
   --  untranslated and hide the tab.
   --  Since: gtk+ 2.6
   --  "translator_credits": the translator credits

   function Get_Version
      (About : access Gtk_About_Dialog_Record) return UTF8_String;
   procedure Set_Version
      (About   : access Gtk_About_Dialog_Record;
       Version : UTF8_String);
   --  Sets the version string to display in the about dialog.
   --  Since: gtk+ 2.6
   --  "version": the version string

   function Get_Website
      (About : access Gtk_About_Dialog_Record) return UTF8_String;
   procedure Set_Website
      (About   : access Gtk_About_Dialog_Record;
       Website : UTF8_String);
   --  Sets the URL to use for the website link. Note that that the hook
   --  functions need to be set up before calling this function.
   --  Since: gtk+ 2.6
   --  "website": a URL string starting with "http://"

   function Get_Website_Label
      (About : access Gtk_About_Dialog_Record) return UTF8_String;
   procedure Set_Website_Label
      (About         : access Gtk_About_Dialog_Record;
       Website_Label : UTF8_String);
   --  Sets the label to be used for the website link. It defaults to the
   --  website URL.
   --  Since: gtk+ 2.6
   --  "website_label": the label used for the website link

   function Get_Wrap_License
      (About : access Gtk_About_Dialog_Record) return Boolean;
   procedure Set_Wrap_License
      (About        : access Gtk_About_Dialog_Record;
       Wrap_License : Boolean);
   --  Sets whether the license text in About is automatically wrapped.
   --  Since: gtk+ 2.8
   --  "wrap_license": whether to wrap the license

   ---------------
   -- Functions --
   ---------------

   function Set_Email_Hook
      (Func    : Activate_Link_Func;
       Data    : System.Address;
       Destroy : Glib.G_Destroy_Notify_Address) return Activate_Link_Func;
   pragma Obsolescent (Set_Email_Hook);
   --  Installs a global function to be called whenever the user activates an
   --  email link in an about dialog. Since 2.18 there exists a default
   --  function which uses gtk_show_uri(). To deactivate it, you can pass null
   --  for Func.
   --  Since: gtk+ 2.6
   --  Deprecated since 2.24, Use the
   --  Gtk.About_Dialog.Gtk_About_Dialog::activate-link signal
   --  "func": a function to call when an email link is activated.
   --  "data": data to pass to Func
   --  "destroy": Glib.G_Destroy_Notify_Address for Data

   function Set_Url_Hook
      (Func    : Activate_Link_Func;
       Data    : System.Address;
       Destroy : Glib.G_Destroy_Notify_Address) return Activate_Link_Func;
   pragma Obsolescent (Set_Url_Hook);
   --  Installs a global function to be called whenever the user activates a
   --  URL link in an about dialog. Since 2.18 there exists a default function
   --  which uses gtk_show_uri(). To deactivate it, you can pass null for Func.
   --  Since: gtk+ 2.6
   --  Deprecated since 2.24, Use the
   --  Gtk.About_Dialog.Gtk_About_Dialog::activate-link signal
   --  "func": a function to call when a URL link is activated.
   --  "data": data to pass to Func
   --  "destroy": Glib.G_Destroy_Notify_Address for Data

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_About_Dialog_Record, Gtk_About_Dialog);
   function "+"
     (Widget : access Gtk_About_Dialog_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_About_Dialog
   renames Implements_Buildable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Comments_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  Comments about the program. This string is displayed in a label in the
   --  main dialog, thus it should be a short explanation of the main purpose
   --  of the program, not a detailed list of features.
   --
   --  Name: Copyright_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  Copyright information for the program.
   --
   --  Name: License_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  The license of the program. This string is displayed in a text view in
   --  a secondary dialog, therefore it is fine to use a long multi-paragraph
   --  text. Note that the text is only wrapped in the text view if the
   --  "wrap-license" property is set to True; otherwise the text itself must
   --  contain the intended linebreaks.
   --
   --  Name: Logo_Property
   --  Type: Gdk.Pixbuf.Gdk_Pixbuf
   --  Flags: read-write
   --  A logo for the about box. If this is not set, it defaults to
   --  gtk_window_get_default_icon_list.
   --
   --  Name: Logo_Icon_Name_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  A named icon to use as the logo for the about box. This property
   --  overrides the Gtk.About_Dialog.Gtk_About_Dialog:logo property.
   --
   --  Name: Program_Name_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  The name of the program. If this is not set, it defaults to
   --  g_get_application_name.
   --
   --  Name: Translator_Credits_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  Credits to the translators. This string should be marked as
   --  translatable. The string may contain email addresses and URLs, which
   --  will be displayed as links, see the introduction for more details.
   --
   --  Name: Version_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  The version of the program.
   --
   --  Name: Website_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  The URL for the link to the website of the program. This should be a
   --  string starting with "http://.
   --
   --  Name: Website_Label_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  The label for the link to the website of the program. If this is not
   --  set, it defaults to the URL specified in the
   --  Gtk.About_Dialog.Gtk_About_Dialog:website property.
   --
   --  Name: Wrap_License_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Whether to wrap the text in the license dialog.

   Comments_Property : constant Glib.Properties.Property_String;
   Copyright_Property : constant Glib.Properties.Property_String;
   License_Property : constant Glib.Properties.Property_String;
   Logo_Property : constant Glib.Properties.Property_Object;
   Logo_Icon_Name_Property : constant Glib.Properties.Property_String;
   Program_Name_Property : constant Glib.Properties.Property_String;
   Translator_Credits_Property : constant Glib.Properties.Property_String;
   Version_Property : constant Glib.Properties.Property_String;
   Website_Property : constant Glib.Properties.Property_String;
   Website_Label_Property : constant Glib.Properties.Property_String;
   Wrap_License_Property : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "activate-link"
   --     function Handler
   --       (Self : access Gtk_About_Dialog_Record'Class;
   --        Uri  : UTF8_String) return Boolean;
   --    --  "uri": the URI that is activated
   --  The signal which gets emitted to activate a URI. Applications may
   --  connect to it to override the default behaviour, which is to call
   --  gtk_show_uri().
   --  Returns True if the link has been activated

   Signal_Activate_Link : constant Glib.Signal_Name := "activate-link";

private
   Comments_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("comments");
   Copyright_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("copyright");
   License_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("license");
   Logo_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("logo");
   Logo_Icon_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("logo-icon-name");
   Program_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("program-name");
   Translator_Credits_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("translator-credits");
   Version_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("version");
   Website_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("website");
   Website_Label_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("website-label");
   Wrap_License_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("wrap-license");
end Gtk.About_Dialog;
