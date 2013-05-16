-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
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

with Glib.Type_Conversion_Hooks;

package body Gtk.Toggle_Tool_Button is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Toggle_Tool_Button_Record);
   pragma Warnings (Off, Type_Conversion);

   ----------------
   -- Get_Active --
   ----------------

   function Get_Active
     (Button : access Gtk_Toggle_Tool_Button_Record)
      return Boolean
   is
      function Internal (Button : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_toggle_tool_button_get_active");
   begin
      return Boolean'Val (Internal (Get_Object (Button)));
   end Get_Active;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Button : out Gtk_Toggle_Tool_Button) is
   begin
      Button := new Gtk_Toggle_Tool_Button_Record;
      Gtk.Toggle_Tool_Button.Initialize (Button);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Button : access Gtk_Toggle_Tool_Button_Record'Class)
   is
      function Internal  return System.Address;
      pragma Import (C, Internal, "gtk_toggle_tool_button_new");
   begin
      Set_Object (Button, Internal);
   end Initialize;

   ------------------------
   -- Gtk_New_From_Stock --
   ------------------------

   procedure Gtk_New_From_Stock
     (Button   : out Gtk_Toggle_Tool_Button;
      Stock_Id : String) is
   begin
      Button := new Gtk_Toggle_Tool_Button_Record;
      Initialize_From_Stock (Button, Stock_Id);
   end Gtk_New_From_Stock;

   ---------------------------
   -- Initialize_From_Stock --
   ---------------------------

   procedure Initialize_From_Stock
     (Button   : access Gtk_Toggle_Tool_Button_Record'Class;
      Stock_Id : String)
   is
      function Internal (Stock_Id : String) return System.Address;
      pragma Import (C, Internal, "gtk_toggle_tool_button_new_from_stock");
   begin
      Set_Object (Button, Internal (Stock_Id & ASCII.NUL));
   end Initialize_From_Stock;

   ----------------
   -- Set_Active --
   ----------------

   procedure Set_Active
     (Button    : access Gtk_Toggle_Tool_Button_Record;
      Is_Active : Boolean)
   is
      procedure Internal
        (Button    : System.Address;
         Is_Active : Gboolean);
      pragma Import (C, Internal, "gtk_toggle_tool_button_set_active");
   begin
      Internal (Get_Object (Button), Boolean'Pos (Is_Active));
   end Set_Active;

end Gtk.Toggle_Tool_Button;
