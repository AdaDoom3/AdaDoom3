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
--  A Gtk_File_Selection is a general widget to interactively select file.
--  It displays a dialog in which the user can navigate through directories,
--  select a file, and even manipulate files with operations like removing,
--  renaming,...
--  Currently, only one file can be selected in the dialog.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Selectors</group>
--  <testgtk>create_file_selection.adb</testgtk>
--  <screenshot>gtk-file_selection</screenshot>

with Glib.Properties;
with Gtk.Box;
with Gtk.Button;
with Gtk.Widget;
with Gtk.Dialog;
with GNAT.Strings;

package Gtk.File_Selection is

   type Gtk_File_Selection_Record is new
     Gtk.Dialog.Gtk_Dialog_Record with private;
   type Gtk_File_Selection is access all Gtk_File_Selection_Record'Class;

   ------------------------------
   -- Operations on the dialog --
   ------------------------------

   procedure Gtk_New
     (File_Selection : out Gtk_File_Selection; Title : UTF8_String);
   procedure Initialize
     (File_Selection : access Gtk_File_Selection_Record'Class;
      Title          : UTF8_String);
   --  Creates or initializes a new file selection dialog.
   --  Title is the name of the dialog, as displayed in its title bar.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_File_Selection.

   procedure Set_Filename
     (File_Selection : access Gtk_File_Selection_Record;
      Filename       : UTF8_String);
   function Get_Filename
     (File_Selection : access Gtk_File_Selection_Record) return UTF8_String;
   --  Highlight the given file in the dialog.
   --  Note that this does not close the dialog.
   --  You can also use this procedure to select the directory to be displayed
   --  in the dialog. Along with Complete, this allows you to set some filters
   --  in the dialog.

   function Get_Selections
     (Filesel : access Gtk_File_Selection_Record)
      return GNAT.Strings.String_List;
   --  Retrieves the list of file selections the user has made in the dialog
   --  box. This function is intended for use when the user can select multiple
   --  files in the file list.
   --  The filenames are in the GLib file name encoding. To convert to UTF-8,
   --  call g_filename_to_utf8() on each string.
   --  The returned value must be freed by the caller

   procedure Complete
     (File_Selection : access Gtk_File_Selection_Record;
      Pattern        : UTF8_String);
   --  Set the filter used to display the files.
   --  The pattern is displayed in the entry at the bottom of the dialog, and
   --  the list of files displayed in the list.

   procedure Show_Fileop_Buttons
     (File_Selection : access Gtk_File_Selection_Record);
   procedure Hide_Fileop_Buttons
     (File_Selection : access Gtk_File_Selection_Record);
   --  When this function is called, the dialog includes a series of buttons
   --  for file operations (create directory, rename a file, delete a file).

   procedure Set_Show_File_Op_Buttons
     (File_Selection : access Gtk_File_Selection_Record;
      Flag           : Boolean);
   --  Choose whether to display or not the file operation buttons.
   --  If Flag is true, calls Show_Fileop_Buttons, otherwise calls
   --  Hide_Fileop_Buttons.

   procedure Set_Select_Multiple
     (Filesel         : access Gtk_File_Selection_Record;
      Select_Multiple : Boolean);
   function Get_Select_Multiple
     (Filesel : access Gtk_File_Selection_Record)
      return Boolean;
   --  Sets whether the user is allowed to select multiple files in the file
   --  list.
   --  Use Get_selections to get the list of selected files.

   ------------------------
   -- Getting the fields --
   ------------------------
   --  The following functions are provided to access the fields of the
   --  file selection dialog.
   --  This dialog is divided into two main areas, the Action_Area which is
   --  the top part that contains the list of files, and the button area which
   --  is the bottom part that contains the OK and Cancel buttons.

   function Get_Action_Area
     (File_Selection : access Gtk_File_Selection_Record)
      return Gtk.Box.Gtk_Box;
   --  Return the action area.
   --  This is the area that contains the list of files, the filter entry,etc.

   function Get_Button_Area
     (File_Selection : access Gtk_File_Selection_Record)
      return Gtk.Box.Gtk_Box;
   --  Return the button area.
   --  This is the area that contains the OK and Cancel buttons.

   function Get_Dir_List
     (File_Selection : access Gtk_File_Selection_Record)
      return Gtk.Widget.Gtk_Widget;
   --  Return the list that display the list of directories.

   function Get_File_List
     (File_Selection : access Gtk_File_Selection_Record)
      return Gtk.Widget.Gtk_Widget;
   --  Return the list that display the list of files in the selected directory

   function Get_Cancel_Button
     (File_Selection : access Gtk_File_Selection_Record)
      return Gtk.Button.Gtk_Button;
   --  Return the Cancel button.
   --  To remove this button from the dialog, call Hide on the return value.
   --  The callbacks on this button should simply close the dialog, but should
   --  ignore the file selected by the user.

   function Get_Help_Button
     (File_Selection : access Gtk_File_Selection_Record)
      return Gtk.Button.Gtk_Button;
   --  Return the Help button.
   --  To remove this button from the dialog, call Hide on the return value.
   --  The callbacks on this button should display a new dialog that explain
   --  what file the user should select.

   function Get_Ok_Button
     (File_Selection : access Gtk_File_Selection_Record)
      return Gtk.Button.Gtk_Button;
   --  Return the OK button.
   --  The callbacks on this button should close the dialog and do something
   --  with the file selected by the user.

   function Get_History_Pulldown
     (File_Selection : access Gtk_File_Selection_Record)
      return Gtk.Widget.Gtk_Widget;
   --  Return the menu that display the history of directories
   --  for easy access by the user.

   function Get_Selection_Entry
     (File_Selection : access Gtk_File_Selection_Record)
      return Gtk.Widget.Gtk_Widget;
   --  Return the entry used to set the filter on the list of directories.
   --  The simplest way to insert text in this entry is to use the
   --  Complete procedure above.

   function Get_Selection_Text
     (File_Selection : access Gtk_File_Selection_Record)
      return Gtk.Widget.Gtk_Widget;
   --  Return the text displayed just above the Selection_Entry.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Filename_Property
   --  Type:  UTF8_String
   --  Flags: read-write
   --  Descr: The currently selected filename.
   --  See also: Set_Filename and Get_Filename
   --
   --  Name:  Show_Fileops_Property
   --  Type:  Boolean
   --  Flags: read-write
   --  Descr: Whether buttons for creating/manipulating files should
   --         be displayed.
   --  See also: Show_Fileop_Buttons and Hide_Fileop_Buttons
   --
   --  Name:  Select_Multiple_Property
   --  Type:  Boolean
   --  Descr: Whether to allow multiple files to be selected
   --
   --  </properties>

   Filename_Property        : constant Glib.Properties.Property_String;
   Show_Fileops_Property    : constant Glib.Properties.Property_Boolean;
   Select_Multiple_Property : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  </signals>

private
   type Gtk_File_Selection_Record is new
     Gtk.Dialog.Gtk_Dialog_Record with null record;

   Filename_Property     : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("filename");
   Show_Fileops_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show_fileops");
   Select_Multiple_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("select-multiple");

   pragma Import (C, Get_Type, "gtk_file_selection_get_type");
end Gtk.File_Selection;
