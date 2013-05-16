-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                 Copyright (C) 2006-2013, AdaCore                  --
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

with Gtk.Tree_Model; use Gtk.Tree_Model;

with Glib.Type_Conversion_Hooks;

package body Gtk.Combo_Box_Entry is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Combo_Box_Entry_Record);
   pragma Warnings (Off, Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Combo : out Gtk_Combo_Box_Entry) is
   begin
      Combo := new Gtk_Combo_Box_Entry_Record;
      Gtk.Combo_Box_Entry.Initialize (Combo);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Combo : access Gtk_Combo_Box_Entry_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_combo_box_entry_new");
   begin
      Set_Object (Combo, Internal);
   end Initialize;

   ------------------
   -- Gtk_New_Text --
   ------------------

   procedure Gtk_New_Text (Combo : out Gtk_Combo_Box_Entry) is
   begin
      Combo := new Gtk_Combo_Box_Entry_Record;
      Gtk.Combo_Box_Entry.Initialize_Text (Combo);
   end Gtk_New_Text;

   ---------------------
   -- Initialize_Text --
   ---------------------

   procedure Initialize_Text
     (Combo : access Gtk_Combo_Box_Entry_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_combo_box_entry_new_text");
   begin
      Set_Object (Combo, Internal);
   end Initialize_Text;

   ------------------------
   -- Gtk_New_With_Model --
   ------------------------

   procedure Gtk_New_With_Model
     (Combo       : out Gtk_Combo_Box_Entry;
      Model       : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Text_Column : Gint)
   is
   begin
      Combo := new Gtk_Combo_Box_Entry_Record;
      Initialize_With_Model (Combo, Model, Text_Column);
   end Gtk_New_With_Model;

   ---------------------------
   -- Initialize_With_Model --
   ---------------------------

   procedure Initialize_With_Model
     (Combo       : access Gtk_Combo_Box_Entry_Record'Class;
      Model       : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Text_Column : Gint)
   is
      function Internal
        (Model       : System.Address;
         Text_Column : Gint)
         return System.Address;
      pragma Import (C, Internal, "gtk_combo_box_entry_new_with_model");
   begin
      Set_Object
        (Combo, Internal (Get_Object (Model), Text_Column));
   end Initialize_With_Model;

   ---------------------
   -- Set_Text_Column --
   ---------------------

   procedure Set_Text_Column
     (Entry_Box : access Gtk_Combo_Box_Entry_Record; Text_Column : Gint)
   is
      procedure Internal
        (Entry_Box   : System.Address;
         Text_Column : Gint);
      pragma Import (C, Internal, "gtk_combo_box_entry_set_text_column");
   begin
      Internal (Get_Object (Entry_Box), Text_Column);
   end Set_Text_Column;

   ---------------------
   -- Get_Text_Column --
   ---------------------

   function Get_Text_Column
     (Entry_Box : access Gtk_Combo_Box_Entry_Record)  return Gint
   is
      function Internal (Entry_Box : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_combo_box_entry_get_text_column");
   begin
      return Internal (Get_Object (Entry_Box));
   end Get_Text_Column;

end Gtk.Combo_Box_Entry;
