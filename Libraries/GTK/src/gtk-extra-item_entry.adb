-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--     Copyright (C) 2000 E. Briot, J. Brobecker and A. Charlet      --
--                 Copyright (C) 2000-2013, AdaCore                  --
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

with Gtk.Enums; use Gtk.Enums;
with System;
with Glib.Type_Conversion_Hooks;
pragma Elaborate_All (Glib.Type_Conversion_Hooks);

package body Gtk.Extra.Item_Entry is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Item_Entry_Record);
   pragma Warnings (Off, Type_Conversion);
   --  This package is used to implement a minimal automated type conversion
   --  without having to drag the whole Gtk.Type_Conversion package for the
   --  most common widgets.

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_IEntry;
                      Max    : Guint16 := 0)
   is
   begin
      Widget := new Gtk_IEntry_Record;
      Initialize (Widget, Max);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_IEntry_Record'Class;
                         Max    : Guint16)
   is
      function Internal (Max    : Guint16)
                         return      System.Address;
      pragma Import (C, Internal, "gtk_item_entry_new_with_max_length");
   begin
      Set_Object (Widget, Internal (Max));
   end Initialize;

   -----------------------
   -- Set_Justification --
   -----------------------

   procedure Set_Justification
      (Item_Entry    : access Gtk_IEntry_Record;
       Justification : Gtk.Enums.Gtk_Justification)
   is
      procedure Internal
        (Item_Entry    : System.Address;
         Justification : Gtk.Enums.Gtk_Justification);
      pragma Import (C, Internal, "gtk_item_entry_set_justification");

   begin
      Internal (Get_Object (Item_Entry), Justification);
   end Set_Justification;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
      (Item_Entry    : access Gtk_IEntry_Record;
       Text          : String;
       Justification : Gtk.Enums.Gtk_Justification)
   is
      procedure Internal
        (Item_Entry    : System.Address;
         Text          : String;
         Justification : Gtk.Enums.Gtk_Justification);
      pragma Import (C, Internal, "gtk_item_entry_set_text");

   begin
      Internal (Get_Object (Item_Entry),
                Text & ASCII.NUL,
                Justification);
   end Set_Text;

   ------------------------
   -- Set_Cursor_Visible --
   ------------------------

   procedure Set_Cursor_Visible
     (Item_Entry : access Gtk_IEntry_Record; Visible : Boolean)
   is
      procedure Internal (Ent : System.Address; Visible : Gboolean);
      pragma Import (C, Internal, "gtk_item_entry_set_cursor_visible");
   begin
      Internal (Get_Object (Item_Entry), Boolean'Pos (Visible));
   end Set_Cursor_Visible;

   ------------------------
   -- Get_Cursor_Visible --
   ------------------------

   function Get_Cursor_Visible
     (Item_Entry : access Gtk_IEntry_Record) return Boolean
   is
      function Internal (Ent : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_item_entry_get_cursor_visible");
   begin
      return Boolean'Val (Internal (Get_Object (Item_Entry)));
   end Get_Cursor_Visible;

end Gtk.Extra.Item_Entry;
