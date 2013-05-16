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

with Gtk.Widget; use Gtk.Widget;
with Gtk; use Gtk;
with Interfaces.C.Strings;
with System;

package body Gnome.Pixmap_Entry is

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New
     (Widget              : out Gnome_Pixmap_Entry;
      History_Id          : String;
      Browse_Dialog_Title : String;
      Do_Preview          : Boolean)
   is
   begin
      Widget := new Gnome_Pixmap_Entry_Record;
      Initialize (Widget, History_Id, Browse_Dialog_Title, Do_Preview);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget              : access Gnome_Pixmap_Entry_Record'Class;
      History_Id          : String;
      Browse_Dialog_Title : String;
      Do_Preview          : Boolean)
   is
      function Internal
        (History_Id          : String;
         Browse_Dialog_Title : String;
         Do_Preview          : Gint)
         return System.Address;
      pragma Import (C, Internal, "gnome_pixmap_entry_new");
   begin
      Set_Object (Widget, Internal (History_Id & ASCII.NUL,
                                    Browse_Dialog_Title & ASCII.NUL,
                                    Boolean'Pos (Do_Preview)));
   end Initialize;

   ----------------
   -- File_Entry --
   ----------------

   function File_Entry (Pentry : access Gnome_Pixmap_Entry_Record)
                        return Gtk.Widget.Gtk_Widget
   is
      function Internal (Pentry : System.Address)
                         return System.Address;
      pragma Import (C, Internal, "gnome_pixmap_entry_gnome_file_entry");
   begin
      return Widget.Convert (Internal (Get_Object (Pentry)));
   end File_Entry;

   -----------------
   -- Gnome_Entry --
   -----------------

   function Gnome_Entry
     (Pentry : access Gnome_Pixmap_Entry_Record) return Gtk.Widget.Gtk_Widget
   is
      function Internal (Pentry : System.Address) return System.Address;
      pragma Import (C, Internal, "gnome_pixmap_entry_gnome_entry");
   begin
      return Widget.Convert (Internal (Get_Object (Pentry)));
   end Gnome_Entry;

   ---------------
   -- Gtk_Entry --
   ---------------

   function Gtk_Entry
     (Pentry : access Gnome_Pixmap_Entry_Record) return Gtk.Widget.Gtk_Widget
   is
      function Internal (Pentry : System.Address) return System.Address;
      pragma Import (C, Internal, "gnome_pixmap_entry_gtk_entry");
   begin
      return Widget.Convert (Internal (Get_Object (Pentry)));
   end Gtk_Entry;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename (Pentry : access Gnome_Pixmap_Entry_Record)
                          return String
   is
      function Internal (Pentry : System.Address)
                         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gnome_pixmap_entry_get_filename");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Pentry)));
   end Get_Filename;

   -----------------------
   -- Set_Pixmap_Subdir --
   -----------------------

   procedure Set_Pixmap_Subdir
     (Pentry : access Gnome_Pixmap_Entry_Record;
      Subdir : String)
   is
      procedure Internal
        (Pentry : System.Address;
         Subdir : String);
      pragma Import (C, Internal, "gnome_pixmap_entry_set_pixmap_subdir");
   begin
      Internal (Get_Object (Pentry),
                Subdir & ASCII.NUL);
   end Set_Pixmap_Subdir;

   -----------------
   -- Set_Preview --
   -----------------

   procedure Set_Preview
     (Pentry     : access Gnome_Pixmap_Entry_Record;
      Do_Preview : Boolean)
   is
      procedure Internal
        (Pentry     : System.Address;
         Do_Preview : Gint);
      pragma Import (C, Internal, "gnome_pixmap_entry_set_preview");
   begin
      Internal (Get_Object (Pentry),
                Boolean'Pos (Do_Preview));
   end Set_Preview;

   ----------------------
   -- Set_Preview_Size --
   ----------------------

   procedure Set_Preview_Size
     (Pentry    : access Gnome_Pixmap_Entry_Record;
      Preview_W : Gint;
      Preview_H : Gint)
   is
      procedure Internal
        (Pentry    : System.Address;
         Preview_W : Gint;
         Preview_H : Gint);
      pragma Import (C, Internal, "gnome_pixmap_entry_set_preview_size");
   begin
      Internal (Get_Object (Pentry),
                Preview_W,
                Preview_H);
   end Set_Preview_Size;

end Gnome.Pixmap_Entry;
