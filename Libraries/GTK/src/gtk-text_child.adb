-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                 Copyright (C) 2001-2013, AdaCore                  --

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

with System;

with Glib;

with Glib.Type_Conversion_Hooks;

package body Gtk.Text_Child is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Text_Child_Anchor_Record);
   pragma Warnings (Off, Type_Conversion);

   -----------------
   -- Get_Deleted --
   -----------------

   function Get_Deleted
     (Anchor : access Gtk_Text_Child_Anchor_Record) return Boolean
   is
      function Internal (Anchor : System.Address) return Gboolean;
      pragma Import  (C, Internal, "gtk_text_child_anchor_get_deleted");

   begin
      return Internal (Get_Object (Anchor)) /= 0;
   end Get_Deleted;

   -----------------
   -- Get_Widgets --
   -----------------

   function Get_Widgets
     (Anchor : access Gtk_Text_Child_Anchor_Record)
      return Gtk.Widget.Widget_List.Glist
   is
      use Gtk.Widget.Widget_List;
      function Internal (Anchor : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_text_child_anchor_get_widgets");
      List : Gtk.Widget.Widget_List.Glist;
   begin
      Set_Object (List, Internal (Get_Object (Anchor)));
      return List;
   end Get_Widgets;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Text_Child_Anchor) is
   begin
      Widget := new Gtk_Text_Child_Anchor_Record;
      Gtk.Text_Child.Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Text_Child_Anchor_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_text_child_anchor_new");

   begin
      Set_Object (Widget, Internal);
   end Initialize;

end Gtk.Text_Child;
