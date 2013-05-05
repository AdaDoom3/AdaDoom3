-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001                            --
--                         ACT-Europe                                --
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

with Gtk;
with Gtk.Box;
with Gtk.Widget;

package Gnome.File_Entry is

   type Gnome_File_Entry_Record is new Gtk.Box.Gtk_Hbox_Record with private;
   type Gnome_File_Entry is access all Gnome_File_Entry_Record'Class;

   procedure Gnome_New
     (Widget              : out Gnome_File_Entry;
      History_Id          : String;
      Browse_Dialog_Title : String);

   procedure Initialize
     (Widget              : access Gnome_File_Entry_Record'Class;
      History_Id          : String;
      Browse_Dialog_Title : String);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   function Gtk_Entry
     (Fentry : access Gnome_File_Entry_Record) return Gtk.Widget.Gtk_Widget;

   function Gnome_Entry
     (Fentry : access Gnome_File_Entry_Record) return Gtk.Widget.Gtk_Widget;

   function Get_Full_Path
     (Fentry          : access Gnome_File_Entry_Record;
      File_Must_Exist : Boolean)
      return String;

   procedure Set_Default_Path
     (Fentry : access Gnome_File_Entry_Record;
      Path   : String);

   procedure Set_Directory
     (Fentry          : access Gnome_File_Entry_Record;
      Directory_Entry : Boolean);

   procedure Set_Modal
     (Fentry   : access Gnome_File_Entry_Record;
      Is_Modal : Boolean);

   procedure Set_Title
     (Fentry              : access Gnome_File_Entry_Record;
      Browse_Dialog_Title : String);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "browse_clicked"
   --    procedure Handler (Widget : access Gnome_File_Entry_Record'Class);
   --
   --  </signals>

private
   type Gnome_File_Entry_Record is new
     Gtk.Box.Gtk_Hbox_Record with null record;

   pragma Import (C, Get_Type, "gnome_file_entry_get_type");
end Gnome.File_Entry;
