-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2013, AdaCore                   --
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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;

package body Gtk.Event_Box is
   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Event_Box_Record);
   pragma Unreferenced (Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Event_Box : out Gtk_Event_Box) is
   begin
      Event_Box := new Gtk_Event_Box_Record;
      Gtk.Event_Box.Initialize (Event_Box);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Event_Box : access Gtk_Event_Box_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_event_box_new");
   begin
      Set_Object (Event_Box, Internal);
   end Initialize;

   ---------------------
   -- Get_Above_Child --
   ---------------------

   function Get_Above_Child
      (Event_Box : access Gtk_Event_Box_Record) return Boolean
   is
      function Internal (Event_Box : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_event_box_get_above_child");
   begin
      return Boolean'Val (Internal (Get_Object (Event_Box)));
   end Get_Above_Child;

   ------------------------
   -- Get_Visible_Window --
   ------------------------

   function Get_Visible_Window
      (Event_Box : access Gtk_Event_Box_Record) return Boolean
   is
      function Internal (Event_Box : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_event_box_get_visible_window");
   begin
      return Boolean'Val (Internal (Get_Object (Event_Box)));
   end Get_Visible_Window;

   ---------------------
   -- Set_Above_Child --
   ---------------------

   procedure Set_Above_Child
      (Event_Box   : access Gtk_Event_Box_Record;
       Above_Child : Boolean)
   is
      procedure Internal (Event_Box : System.Address; Above_Child : Integer);
      pragma Import (C, Internal, "gtk_event_box_set_above_child");
   begin
      Internal (Get_Object (Event_Box), Boolean'Pos (Above_Child));
   end Set_Above_Child;

   ------------------------
   -- Set_Visible_Window --
   ------------------------

   procedure Set_Visible_Window
      (Event_Box      : access Gtk_Event_Box_Record;
       Visible_Window : Boolean)
   is
      procedure Internal
         (Event_Box      : System.Address;
          Visible_Window : Integer);
      pragma Import (C, Internal, "gtk_event_box_set_visible_window");
   begin
      Internal (Get_Object (Event_Box), Boolean'Pos (Visible_Window));
   end Set_Visible_Window;

end Gtk.Event_Box;
