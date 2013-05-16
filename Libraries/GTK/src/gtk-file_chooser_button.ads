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
--  The Gtk_File_Chooser_Button is a widget that lets the user select a file.
--  It implements the Gtk_File_Chooser interface. Visually, it is a file name
--  with a button to bring up a Gtk_File_Chooser_Dialog. The user can then use
--  that dialog to change the file associated with that button. This widget
--  does not support setting the "select-multiple" property to TRUE.
--
--  The Gtk_File_Chooser_Button supports the File_Chooser_Actions
--  Action_Open and Action_Select_Folder.
--
--  The Gtk_File_Chooser_Button will ellipsize the label, and thus will request
--  little horizontal space. To give the button more space, you should call
--  Gtk.Widget.Size_Request, Set_Width_Chars, or pack the button in such a way
--  that other interface elements give space to the widget.
--  </description>
--  <c_version>2.16.6</c_version>
--  <group>Selectors</group>
--  <screenshot>file-button.png</screenshot>
--  <testgtk>create_file_chooser.adb</testgtk>

with Glib.Properties;
with Glib.Types;
with Gtk.Box;
with Gtk.File_Chooser;
with Gtk.File_Chooser_Dialog; use Gtk.File_Chooser_Dialog;

package Gtk.File_Chooser_Button is

   type Gtk_File_Chooser_Button_Record is
     new Gtk.Box.Gtk_Hbox_Record with null record;
   type Gtk_File_Chooser_Button is
     access all Gtk_File_Chooser_Button_Record'Class;

   function Get_Type return GType;
   --  Return the internal value associated with a Gtk_File_Chooser_Button

   procedure Gtk_New
     (Button : out Gtk_File_Chooser_Button;
      Title  : String;
      Action : Gtk.File_Chooser.File_Chooser_Action);
   procedure Initialize
     (Button : access Gtk_File_Chooser_Button_Record'Class;
      Title  : String;
      Action : Gtk.File_Chooser.File_Chooser_Action);
   --  Creates a new file-selecting button widget.
   --  Title is the title of the browse dialog

   procedure Gtk_New_With_Backend
     (Button  : out Gtk_File_Chooser_Button;
      Title   : String;
      Action  : Gtk.File_Chooser.File_Chooser_Action;
      Backend : String);
   pragma Obsolescent; --  Gtk_New_With_Backend
   procedure Initialize_With_Backend
     (Button  : access Gtk_File_Chooser_Button_Record'Class;
      Title   : String;
      Action  : Gtk.File_Chooser.File_Chooser_Action;
      Backend : String);
   --  Creates a new file-selecting button widget using Backend. See also
   --  Gtk.File_System.

   procedure Gtk_New_With_Dialog
     (Button : out Gtk_File_Chooser_Button;
      Dialog : access Gtk_File_Chooser_Dialog_Record'Class);
   procedure Initialize_With_Dialog
     (Button : access Gtk_File_Chooser_Button_Record'Class;
      Dialog : access Gtk_File_Chooser_Dialog_Record'Class);
   --  Creates a button widget which uses Dialog as it's file-picking window.
   --  Note that dialog must not have Destroy_With_Parent set.

   procedure Set_Title
     (Button : access Gtk_File_Chooser_Button_Record; Title  : String);
   function Get_Title
     (Button : access Gtk_File_Chooser_Button_Record) return String;
   --  Modifies the Title of the browse dialog used by Button.

   procedure Set_Width_Chars
     (Button  : access Gtk_File_Chooser_Button_Record;
      N_Chars : Gint);
   function Get_Width_Chars
     (Button : access Gtk_File_Chooser_Button_Record) return Gint;
   --  Sets the width (in characters) that Button will use.

   function Get_Focus_On_Click
     (Button : access Gtk_File_Chooser_Button_Record)
      return Boolean;
   procedure Set_Focus_On_Click
     (Button         : access Gtk_File_Chooser_Button_Record;
      Focus_On_Click : Boolean);
   --  Controls whether the button will grab focus when it is clicked with
   --  the mouse.  Making mouse clicks not grab focus is useful in places
   --  like toolbars where you don't want the keyboard focus removed from
   --  the main area of the application.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_File_Chooser"

   package Implements_File_Chooser is new Glib.Types.Implements
     (Gtk.File_Chooser.Gtk_File_Chooser,
      Gtk_File_Chooser_Button_Record, Gtk_File_Chooser_Button);
   function "+"
     (Button : access Gtk_File_Chooser_Button_Record'Class)
      return Gtk.File_Chooser.Gtk_File_Chooser
      renames Implements_File_Chooser.To_Interface;
   function "-"
     (File : Gtk.File_Chooser.Gtk_File_Chooser)
      return Gtk_File_Chooser_Button
      renames Implements_File_Chooser.To_Object;
   --  Converts to and from the Gtk_File_Chooser interface

   ----------------
   -- Properties --
   ----------------
   --  <properties>
   --  Name:  Dialog_Property
   --  Type:  Object
   --  Descr: The file chooser dialog to use.
   --
   --  Name:  Focus_On_Click_Property
   --  Type:  Boolean
   --  Descr: Whether the button grabs focus when it is clicked with the mouse
   --
   --  Name:  Title_Property
   --  Type:  String
   --  Descr: The title of the file chooser dialog.
   --
   --  Name:  Width_Chars_Property
   --  Type:  Int
   --  Descr: The desired width of the button widget, in characters.
   --
   --  </properties>

   Dialog_Property         : constant Glib.Properties.Property_Object;
   Focus_On_Click_Property : constant Glib.Properties.Property_Boolean;
   Title_Property          : constant Glib.Properties.Property_String;
   Width_Chars_Property    : constant Glib.Properties.Property_Int;

private
   pragma Import (C, Get_Type, "gtk_file_chooser_button_get_type");

   Dialog_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("dialog");
   Focus_On_Click_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("focus-on-click");
   Title_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("title");
   Width_Chars_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("width-chars");

end Gtk.File_Chooser_Button;
