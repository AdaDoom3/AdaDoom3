-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                  Copyright (C) 2001-2002                          --
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

with Gtk; use Gtk;
with Interfaces.C.Strings;
with System;

package body Gnome.Icon_Entry is

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New
     (Widget              : out Gnome_Icon_Entry;
      History_Id          : String;
      Browse_Dialog_Title : String)
   is
   begin
      Widget := new Gnome_Icon_Entry_Record;
      Initialize (Widget, History_Id, Browse_Dialog_Title);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget              : access Gnome_Icon_Entry_Record'Class;
      History_Id          : String;
      Browse_Dialog_Title : String)
   is
      function Internal
        (History_Id          : String;
         Browse_Dialog_Title : String)
         return System.Address;
      pragma Import (C, Internal, "gnome_icon_entry_new");
   begin
      Set_Object (Widget, Internal (History_Id & ASCII.NUL,
                                    Browse_Dialog_Title & ASCII.NUL));
   end Initialize;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename
     (Ientry : access Gnome_Icon_Entry_Record) return String
   is
      function Internal
        (Ientry : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gnome_icon_entry_get_filename");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Ientry)));
   end Get_Filename;

   ------------------
   -- Set_Filename --
   ------------------

   procedure Set_Filename
     (Ientry   : access Gnome_Icon_Entry_Record;
      Filename : String)
   is
      procedure Internal
        (Ientry   : System.Address;
         Filename : String);
      pragma Import (C, Internal, "gnome_icon_entry_set_filename");
   begin
      Internal (Get_Object (Ientry), Filename & ASCII.NUL);
   end Set_Filename;

   -----------------------
   -- Set_Pixmap_Subdir --
   -----------------------

   procedure Set_Pixmap_Subdir
     (Ientry : access Gnome_Icon_Entry_Record;
      Subdir : String)
   is
      procedure Internal
        (Ientry : System.Address;
         Subdir : String);
      pragma Import (C, Internal, "gnome_icon_entry_set_pixmap_subdir");
   begin
      Internal (Get_Object (Ientry),
                Subdir & ASCII.NUL);
   end Set_Pixmap_Subdir;

end Gnome.Icon_Entry;
