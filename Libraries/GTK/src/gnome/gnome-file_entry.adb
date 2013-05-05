-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001-2006                       --
--                           AdaCore                                 --
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

with Glib; use Glib;
with Gtk.Widget; use Gtk.Widget;
with Gtk; use Gtk;
with Interfaces.C.Strings;
with System;

package body Gnome.File_Entry is

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New
     (Widget              : out Gnome_File_Entry;
      History_Id          : String;
      Browse_Dialog_Title : String)
   is
   begin
      Widget := new Gnome_File_Entry_Record;
      Initialize (Widget, History_Id, Browse_Dialog_Title);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget              : access Gnome_File_Entry_Record'Class;
      History_Id          : String;
      Browse_Dialog_Title : String)
   is
      function Internal
        (History_Id          : String;
         Browse_Dialog_Title : String)
         return System.Address;
      pragma Import (C, Internal, "gnome_file_entry_new");
   begin
      Set_Object (Widget, Internal (History_Id & ASCII.NUL,
                                    Browse_Dialog_Title & ASCII.NUL));
   end Initialize;

   -----------------
   -- Gnome_Entry --
   -----------------

   function Gnome_Entry
     (Fentry : access Gnome_File_Entry_Record) return Gtk.Widget.Gtk_Widget
   is
      function Internal (Fentry : System.Address) return System.Address;
      pragma Import (C, Internal, "gnome_file_entry_gnome_entry");
   begin
      return Widget.Convert (Internal (Get_Object (Fentry)));
   end Gnome_Entry;

   ---------------
   -- Gtk_Entry --
   ---------------

   function Gtk_Entry
     (Fentry : access Gnome_File_Entry_Record) return Gtk.Widget.Gtk_Widget
   is
      function Internal (Fentry : System.Address) return System.Address;
      pragma Import (C, Internal, "gnome_file_entry_gtk_entry");
   begin
      return Widget.Convert (Internal (Get_Object (Fentry)));
   end Gtk_Entry;

   -------------------
   -- Get_Full_Path --
   -------------------

   function Get_Full_Path
     (Fentry          : access Gnome_File_Entry_Record;
      File_Must_Exist : Boolean)
      return String
   is
      function Internal
        (Fentry          : System.Address;
         File_Must_Exist : Gint)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gnome_file_entry_get_full_path");
   begin
      return Interfaces.C.Strings.Value (Internal
        (Get_Object (Fentry), Boolean'Pos (File_Must_Exist)));
   end Get_Full_Path;

   ----------------------
   -- Set_Default_Path --
   ----------------------

   procedure Set_Default_Path
     (Fentry : access Gnome_File_Entry_Record;
      Path   : String)
   is
      procedure Internal
        (Fentry : System.Address;
         Path   : String);
      pragma Import (C, Internal, "gnome_file_entry_set_default_path");
   begin
      Internal (Get_Object (Fentry),
                Path & ASCII.NUL);
   end Set_Default_Path;

   -------------------
   -- Set_Directory --
   -------------------

   procedure Set_Directory
     (Fentry          : access Gnome_File_Entry_Record;
      Directory_Entry : Boolean)
   is
      procedure Internal
        (Fentry          : System.Address;
         Directory_Entry : Gint);
      pragma Import (C, Internal, "gnome_file_entry_set_directory");
   begin
      Internal (Get_Object (Fentry),
                Boolean'Pos (Directory_Entry));
   end Set_Directory;

   ---------------
   -- Set_Modal --
   ---------------

   procedure Set_Modal
     (Fentry   : access Gnome_File_Entry_Record;
      Is_Modal : Boolean)
   is
      procedure Internal
        (Fentry   : System.Address;
         Is_Modal : Gint);
      pragma Import (C, Internal, "gnome_file_entry_set_modal");
   begin
      Internal (Get_Object (Fentry),
                Boolean'Pos (Is_Modal));
   end Set_Modal;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title
     (Fentry              : access Gnome_File_Entry_Record;
      Browse_Dialog_Title : String)
   is
      procedure Internal
        (Fentry              : System.Address;
         Browse_Dialog_Title : String);
      pragma Import (C, Internal, "gnome_file_entry_set_title");
   begin
      Internal (Get_Object (Fentry),
                Browse_Dialog_Title & ASCII.NUL);
   end Set_Title;

end Gnome.File_Entry;
