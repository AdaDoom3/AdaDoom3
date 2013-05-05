-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                  Copyright (C) 2001-2006 AdaCore                  --
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
with Gtk;
pragma Warnings (Off); --  Gtk.Combo is obsolescent
with Gtk.Combo;
pragma Warnings (On);
with Gtk.Widget;

package Gnome.GEntry is

   pragma Warnings (Off);  --  Gtk.Combo is obsolescent;
   type Gnome_GEntry_Record is new Gtk.Combo.Gtk_Combo_Record with private;
   pragma Warnings (On);
   type Gnome_GEntry is access all Gnome_GEntry_Record'Class;

   procedure Gnome_New (Widget     : out Gnome_GEntry;
                        History_Id : String);

   procedure Initialize (Widget     : access Gnome_GEntry_Record'Class;
                         History_Id : String);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   procedure Entry_Append_History
     (Gentry : access Gnome_GEntry_Record;
      Save   : Gint;
      Text   : String);

   function Entry_Gtk_Entry (Gentry : access Gnome_GEntry_Record)
                             return Gtk.Widget.Gtk_Widget;

   procedure Entry_Prepend_History
     (Gentry : access Gnome_GEntry_Record;
      Save   : Gint;
      Text   : String);

   procedure Entry_Set_History_Id
     (Gentry     : access Gnome_GEntry_Record;
      History_Id : String);

   procedure Entry_Set_Max_Saved
     (Gentry    : access Gnome_GEntry_Record;
      Max_Saved : Guint);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  </signals>

private
   pragma Warnings (Off);  --  Gtk.Combo is obsolescent;
   type Gnome_GEntry_Record is new Gtk.Combo.Gtk_Combo_Record with null record;
   pragma Warnings (On);

   pragma Import (C, Get_Type, "gnome_gentry_get_type");
end Gnome.GEntry;
