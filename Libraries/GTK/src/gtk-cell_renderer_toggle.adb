-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                 Copyright (C) 2001-2013, AdaCore                  --
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
with System;

with Glib.Type_Conversion_Hooks;

package body Gtk.Cell_Renderer_Toggle is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Cell_Renderer_Toggle_Record);
   pragma Warnings (Off, Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Cell_Renderer_Toggle)
   is
   begin
      Widget := new Gtk_Cell_Renderer_Toggle_Record;
      Gtk.Cell_Renderer_Toggle.Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Cell_Renderer_Toggle_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_cell_renderer_toggle_new");
   begin
      Set_Object (Widget, Internal);
   end Initialize;

   ---------------
   -- Get_Radio --
   ---------------

   function Get_Radio (Toggle : access Gtk_Cell_Renderer_Toggle_Record)
                       return Boolean
   is
      function Internal (Toggle : System.Address)
                         return Gint;
      pragma Import (C, Internal, "gtk_cell_renderer_toggle_get_radio");
   begin
      return Boolean'Val (Internal (Get_Object (Toggle)));
   end Get_Radio;

   ---------------
   -- Set_Radio --
   ---------------

   procedure Set_Radio
     (Toggle : access Gtk_Cell_Renderer_Toggle_Record;
      Radio  : Boolean)
   is
      procedure Internal
        (Toggle : System.Address;
         Radio  : Gint);
      pragma Import (C, Internal, "gtk_cell_renderer_toggle_set_radio");
   begin
      Internal (Get_Object (Toggle),
                Boolean'Pos (Radio));
   end Set_Radio;

   ----------------
   -- Get_Active --
   ----------------

   function Get_Active (Toggle : access Gtk_Cell_Renderer_Toggle_Record)
                        return Boolean
   is
      function Internal (Toggle : System.Address)
                         return Gint;
      pragma Import (C, Internal, "gtk_cell_renderer_toggle_get_active");
   begin
      return Boolean'Val (Internal (Get_Object (Toggle)));
   end Get_Active;

   ----------------
   -- Set_Active --
   ----------------

   procedure Set_Active
     (Toggle  : access Gtk_Cell_Renderer_Toggle_Record;
      Setting : Boolean)
   is
      procedure Internal
        (Toggle  : System.Address;
         Setting : Gint);
      pragma Import (C, Internal, "gtk_cell_renderer_toggle_set_active");
   begin
      Internal (Get_Object (Toggle),
                Boolean'Pos (Setting));
   end Set_Active;

end Gtk.Cell_Renderer_Toggle;
