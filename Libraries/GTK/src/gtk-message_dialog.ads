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
--  Gtk_Message_Dialog presents a dialog with an image representing the type of
--  message (Error, Question, etc.) alongside some message text. It's simply a
--  convenience widget; you could construct the equivalent of
--  Gtk_Message_Dialog from Gtk_Dialog without too much effort, but
--  Gtk_Message_Dialog saves typing.
--
--  The easiest way to do a modal message dialog is to use Gtk.Dialog.Run,
--  though you can also pass in the MODAL flag, Gtk.Dialog.Run automatically
--  makes the dialog modal and waits for the user to respond to it.
--  Gtk.Dialog.Run returns when any dialog button is clicked.
--  </description>
--  <c_version>2.16.6</c_version>
--  <group>Windows</group>
--  <screenshot>messagedialog.png</screenshot>
--  <see>Gtkada.Dialogs</see>

with Glib.Properties;
with Gtk.Dialog;
with Gtk.Widget;
with Gtk.Window;

package Gtk.Message_Dialog is

   type Gtk_Message_Dialog_Record is new Gtk.Dialog.Gtk_Dialog_Record with
     null record;
   type Gtk_Message_Dialog is access all Gtk_Message_Dialog_Record'Class;

   type Gtk_Message_Type is
     (Message_Info,
      Message_Warning,
      Message_Question,
      Message_Error);

   type Gtk_Buttons_Type is
     (Buttons_None,
      Buttons_Ok,
      Buttons_Close,
      Buttons_Cancel,
      Buttons_Yes_No,
      Buttons_Ok_Cancel);

   procedure Gtk_New
     (Dialog         : out Gtk_Message_Dialog;
      Parent         : Gtk.Window.Gtk_Window := null;
      Flags          : Gtk.Dialog.Gtk_Dialog_Flags := 0;
      Typ            : Gtk_Message_Type := Message_Info;
      Buttons        : Gtk_Buttons_Type := Buttons_Close;
      Message        : String);
   procedure Initialize
     (Dialog         : access Gtk_Message_Dialog_Record'Class;
      Parent         : Gtk.Window.Gtk_Window := null;
      Flags          : Gtk.Dialog.Gtk_Dialog_Flags := 0;
      Typ            : Gtk_Message_Type := Message_Info;
      Buttons        : Gtk_Buttons_Type := Buttons_Close;
      Message        : String);
   --  Creates a new message dialog, which is a simple dialog with an icon
   --  indicating the dialog type (error, warning, etc.) and some text the user
   --  may want to see. When the user clicks a button a "response" signal is
   --  emitted with response IDs from Gtk.Dialog.Gtk_Response_Type. See
   --  Gtk_Dialog for more details.

   procedure Gtk_New_With_Markup
     (Dialog         : out Gtk_Message_Dialog;
      Parent         : Gtk.Window.Gtk_Window := null;
      Flags          : Gtk.Dialog.Gtk_Dialog_Flags := 0;
      Typ            : Gtk_Message_Type := Message_Info;
      Buttons        : Gtk_Buttons_Type := Buttons_Close;
      Message        : String);
   procedure Initialize_With_Markup
     (Dialog         : access Gtk_Message_Dialog_Record'Class;
      Parent         : Gtk.Window.Gtk_Window := null;
      Flags          : Gtk.Dialog.Gtk_Dialog_Flags := 0;
      Typ            : Gtk_Message_Type := Message_Info;
      Buttons        : Gtk_Buttons_Type := Buttons_Close;
      Message        : String);
   --  Same as Gtk_New and Initialize, but Message might contain special markup
   --  like <b>, <i>, <big>,...

   function Get_Type return GType;
   --  Return the internal type used for a Gtk_Message_Dialog

   procedure Set_Markup
     (Message_Dialog : access Gtk_Message_Dialog_Record;
      Str            : String);
   --  Sets the text of the message dialog to be Str, which is marked
   --  up with the >Pango text markup language. This means that you can for
   --  instance <b> to get bold text.

   procedure Format_Secondary_Markup
     (Message_Dialog : access Gtk_Message_Dialog_Record;
      Message        : String);
   procedure Format_Secondary_Text
     (Message_Dialog : access Gtk_Message_Dialog_Record;
      Message        : String);
   --  Sets the secondary text of the message dialog to be Message. When using
   --  markup, special marks are interpreted (<b> for bold, <i> for italic,...)
   --  Note that setting a secondary text makes the primary text become bold,
   --  unless you have provided explicit markup.

   function Get_Image
     (Dialog : access Gtk_Message_Dialog_Record)
      return Gtk.Widget.Gtk_Widget;
   procedure Set_Image
     (Dialog : access Gtk_Message_Dialog_Record;
      Image  : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Get and set the dialog's image.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  <properties>
   --  Name:  Buttons_Property
   --  Type:  Enum
   --  Descr: The buttons shown in the message dialog
   --
   --  Name:  Image_Property
   --  Type:  Object
   --  Descr: The image
   --
   --  Name:  Message_Type_Property
   --  Type:  Enum
   --  Descr: The type of message
   --
   --  Name:  Secondary_Text_Property
   --  Type:  String
   --  Descr: The secondary text of the message dialog
   --
   --  Name:  Secondary_Use_Markup_Property
   --  Type:  Boolean
   --  Descr: The secondary text includes Pango markup.
   --
   --  Name:  Text_Property
   --  Type:  String
   --  Descr: The primary text of the message dialog
   --
   --  Name:  Use_Markup_Property
   --  Type:  Boolean
   --  Descr: The primary text of the title includes Pango markup.
   --
   --  </properties>

--     Buttons_Property      : constant Glib.Properties.Property_Enum;
--     Message_Type_Property : constant Glib.Properties.Property_Enum;
   Image_Property                : constant Glib.Properties.Property_Object;
   Secondary_Text_Property       : constant Glib.Properties.Property_String;
   Secondary_Use_Markup_Property : constant Glib.Properties.Property_Boolean;
   Text_Property                 : constant Glib.Properties.Property_String;
   Use_Markup_Property           : constant Glib.Properties.Property_Boolean;

   ----------------------
   -- Style Properties --
   ----------------------
   --  The following properties can be changed through the gtk theme and
   --  configuration files, and retrieved through Gtk.Widget.Style_Get_Property
   --
   --  <style_properties>
   --  Name:  Message_Border_Property
   --  Type:  Int
   --  Descr: Width of border around the label and image in the message dialog
   --
   --  Name:  Use_Separator_Property
   --  Type:  Boolean
   --  Descr: Whether to put a separator between the message dialog's text and
   --         the buttons
   --
   --  </style_properties>

   Message_Border_Property : constant Glib.Properties.Property_Int;
   Use_Separator_Property  : constant Glib.Properties.Property_Boolean;

private
--     Buttons_Property : constant Glib.Properties.Property_Enum :=
--       Glib.Properties.Build ("buttons");
--     Message_Type_Property : constant Glib.Properties.Property_Enum :=
--       Glib.Properties.Build ("message-type");
   Image_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("image");
   Secondary_Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("secondary-text");
   Secondary_Use_Markup_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("secondary-use-markup");
   Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("text");
   Use_Markup_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-markup");

   Message_Border_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("message-border");
   Use_Separator_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-separator");

   pragma Import (C, Get_Type, "gtk_message_dialog_get_type");

end Gtk.Message_Dialog;

--  Implemented through our own C wrappers:
--  No binding: gtk_message_dialog_format_secondary_markup
--  No binding: gtk_message_dialog_format_secondary_text
--  No binding: gtk_message_dialog_new
--  No binding: gtk_message_dialog_new_with_markup
