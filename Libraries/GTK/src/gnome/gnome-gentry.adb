-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                  Copyright (C) 2001-2006                          --
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

with Gtk.Widget; use Gtk.Widget;
with Gtk; use Gtk;
with System;

package body Gnome.GEntry is

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New
     (Widget     : out Gnome_GEntry;
      History_Id : String) is
   begin
      Widget := new Gnome_GEntry_Record;
      Initialize (Widget, History_Id);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget     : access Gnome_GEntry_Record'Class;
      History_Id : String)
   is
      function Internal (History_Id : String) return System.Address;
      pragma Import (C, Internal, "gnome_entry_new");
   begin
      Set_Object (Widget, Internal (History_Id & ASCII.NUL));
   end Initialize;

   --------------------------
   -- Entry_Append_History --
   --------------------------

   procedure Entry_Append_History
     (Gentry : access Gnome_GEntry_Record;
      Save   : Gint;
      Text   : String)
   is
      procedure Internal
        (Gentry : System.Address;
         Save   : Gint;
         Text   : String);
      pragma Import (C, Internal, "gnome_entry_append_history");
   begin
      Internal (Get_Object (Gentry),
                Save,
                Text & ASCII.NUL);
   end Entry_Append_History;

   ---------------------
   -- Entry_Gtk_Entry --
   ---------------------

   function Entry_Gtk_Entry (Gentry : access Gnome_GEntry_Record)
                             return Gtk.Widget.Gtk_Widget
   is
      function Internal (Gentry : System.Address)
                         return System.Address;
      pragma Import (C, Internal, "gnome_entry_gtk_entry");
   begin
      return Widget.Convert (Internal (Get_Object (Gentry)));
   end Entry_Gtk_Entry;

   ---------------------------
   -- Entry_Prepend_History --
   ---------------------------

   procedure Entry_Prepend_History
     (Gentry : access Gnome_GEntry_Record;
      Save   : Gint;
      Text   : String)
   is
      procedure Internal
        (Gentry : System.Address;
         Save   : Gint;
         Text   : String);
      pragma Import (C, Internal, "gnome_entry_prepend_history");
   begin
      Internal (Get_Object (Gentry),
                Save,
                Text & ASCII.NUL);
   end Entry_Prepend_History;

   --------------------------
   -- Entry_Set_History_Id --
   --------------------------

   procedure Entry_Set_History_Id
     (Gentry     : access Gnome_GEntry_Record;
      History_Id : String)
   is
      procedure Internal
        (Gentry     : System.Address;
         History_Id : String);
      pragma Import (C, Internal, "gnome_entry_set_history_id");
   begin
      Internal (Get_Object (Gentry),
                History_Id & ASCII.NUL);
   end Entry_Set_History_Id;

   -------------------------
   -- Entry_Set_Max_Saved --
   -------------------------

   procedure Entry_Set_Max_Saved
     (Gentry    : access Gnome_GEntry_Record;
      Max_Saved : Guint)
   is
      procedure Internal
        (Gentry    : System.Address;
         Max_Saved : Guint);
      pragma Import (C, Internal, "gnome_entry_set_max_saved");
   begin
      Internal (Get_Object (Gentry),
                Max_Saved);
   end Entry_Set_Max_Saved;

end Gnome.GEntry;
