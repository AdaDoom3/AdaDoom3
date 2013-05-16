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
--  Gtk_File_Chooser_Dialog is a dialog box suitable for use with "File/Open"
--  or "File/Save as" commands. This widget works by putting a
--  Gtk_File_Chooser_Widget inside a Gtk_Dialog. It exposes the
--  Gtk_File_Chooser interface, so you can use all of the Gtk_File_Chooser
--  functions on the file chooser dialog as well as those for GtkDialog.
--
--  Note that Gtk_File_Chooser_Dialog does not have any methods of its own.
--  Instead, you should use the functions that work on a Gtk_File_Chooser.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Selectors</group>
--  <screenshot>filechooser.png</screenshot>

with Glib.Types;
with Gtk.Dialog;
with Gtk.File_Chooser;
with Gtk.Window;

package Gtk.File_Chooser_Dialog is

   type Gtk_File_Chooser_Dialog_Record is
     new Gtk.Dialog.Gtk_Dialog_Record with null record;
   type Gtk_File_Chooser_Dialog is
     access all Gtk_File_Chooser_Dialog_Record'Class;

   function Get_Type return GType;
   --  Return the internal value associated with a Gtk_File_Chooser_Button

   procedure Gtk_New
     (Dialog            : out Gtk_File_Chooser_Dialog;
      Title             : String;
      Parent            : access Gtk.Window.Gtk_Window_Record'Class;
      Action            : Gtk.File_Chooser.File_Chooser_Action);
   procedure Initialize
     (Dialog            : access Gtk_File_Chooser_Dialog_Record'Class;
      Title             : String;
      Parent            : access Gtk.Window.Gtk_Window_Record'Class;
      Action            : Gtk.File_Chooser.File_Chooser_Action);
   --  Creates a new file chooser dialog

   procedure Gtk_New_With_Backend
     (Dialog            : out Gtk_File_Chooser_Dialog;
      Title             : String;
      Parent            : access Gtk.Window.Gtk_Window_Record'Class;
      Action            : Gtk.File_Chooser.File_Chooser_Action;
      Backend           : String);
   procedure Initialize_With_Backend
     (Dialog            : access Gtk_File_Chooser_Dialog_Record'Class;
      Title             : String;
      Parent            : access Gtk.Window.Gtk_Window_Record'Class;
      Action            : Gtk.File_Chooser.File_Chooser_Action;
      Backend           : String);
   --  Creates a new file chooser dialog

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_File_Chooser"

   package Implements_File_Chooser is new Glib.Types.Implements
     (Gtk.File_Chooser.Gtk_File_Chooser,
      Gtk_File_Chooser_Dialog_Record, Gtk_File_Chooser_Dialog);
   function "+"
     (Dialog : access Gtk_File_Chooser_Dialog_Record'Class)
      return Gtk.File_Chooser.Gtk_File_Chooser
      renames Implements_File_Chooser.To_Interface;
   function "-"
     (File : Gtk.File_Chooser.Gtk_File_Chooser)
      return Gtk_File_Chooser_Dialog
      renames Implements_File_Chooser.To_Object;
   --  Converts to and from the Gtk_File_Chooser interface

private
   pragma Import (C, Get_Type, "gtk_file_chooser_dialog_get_type");
end Gtk.File_Chooser_Dialog;

--  Binding is done through our own C functions:
--  No binding: gtk_file_chooser_dialog_new
--  No binding: gtk_file_chooser_dialog_new_with_backend
