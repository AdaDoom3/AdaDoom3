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
--  Gtk_File_Chooser_Widget is a widget suitable for selecting files. It is the
--  main building block of a Gtk_File_Chooser_Dialog. Most applications will
--  only need to use the latter; you can use Gtk_File_Chooser_Widget as part of
--  a larger window if you have special needs.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Selectors</group>

with Glib.Types;
with Gtk.Box;
with Gtk.File_Chooser;

package Gtk.File_Chooser_Widget is

   type Gtk_File_Chooser_Widget_Record is
     new Gtk.Box.Gtk_Vbox_Record with null record;
   type Gtk_File_Chooser_Widget is
     access all Gtk_File_Chooser_Widget_Record'Class;

   function Get_Type return GType;
   --  Return the internal value associated with a Gtk_File_Chooser_Widget

   procedure Gtk_New
     (Widget : out Gtk_File_Chooser_Widget;
      Action : Gtk.File_Chooser.File_Chooser_Action);
   procedure Initialize
     (Widget : access Gtk_File_Chooser_Widget_Record'Class;
      Action : Gtk.File_Chooser.File_Chooser_Action);
   --  Creates a new file chooser.  This is a file chooser widget that can
   --  be embedded in custom windows, and it is the same widget that is used by
   --  Gtk_File_Chooser_Dialog.

   procedure Gtk_New_With_Backend
     (Widget  : out Gtk_File_Chooser_Widget;
      Action  : Gtk.File_Chooser.File_Chooser_Action;
      Backend : String);
   pragma Obsolescent (Gtk_New_With_Backend);
   procedure Initialize_With_Backend
     (Widget  : access Gtk_File_Chooser_Widget_Record'Class;
      Action  : Gtk.File_Chooser.File_Chooser_Action;
      Backend : String);
   pragma Obsolescent (Initialize_With_Backend);
   --  Creates a new file chooser with a specified backend.  This is
   --  especially useful if you use Gtk.File_Chooser.Set_Local_Only to allow
   --  non-local files.  This is a file chooser widget that can be embedded in
   --  custom windows and it is the same widget that is used by
   --  Gtk_File_Chooser_Dialog.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_File_Chooser"

   package Implements_File_Chooser is new Glib.Types.Implements
     (Gtk.File_Chooser.Gtk_File_Chooser,
      Gtk_File_Chooser_Widget_Record, Gtk_File_Chooser_Widget);
   function "+"
     (Widget : access Gtk_File_Chooser_Widget_Record'Class)
      return Gtk.File_Chooser.Gtk_File_Chooser
      renames Implements_File_Chooser.To_Interface;
   function "-"
     (File : Gtk.File_Chooser.Gtk_File_Chooser)
      return Gtk_File_Chooser_Widget
      renames Implements_File_Chooser.To_Object;
   --  Converts to and from the Gtk_File_Chooser interface

private
   pragma Import (C, Get_Type, "gtk_file_chooser_widget_get_type");

end Gtk.File_Chooser_Widget;
