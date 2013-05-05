-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
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

with System;

with Glib.Type_Conversion_Hooks;

package body Gtk.Plug is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Plug_Record);
   pragma Warnings (Off, Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Plug : out Gtk_Plug; Socket_Id : Guint32) is
   begin
      Plug := new Gtk_Plug_Record;
      Initialize (Plug, Socket_Id);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Plug : access Gtk_Plug_Record'Class; Socket_Id : Guint32)
   is
      function Internal (Socket_Id : Guint32) return System.Address;
      pragma Import (C, Internal, "gtk_plug_new");

   begin
      Set_Object (Plug, Internal (Socket_Id));
   end Initialize;

   ------------
   -- Get_Id --
   ------------

   function Get_Id (Plug : access Gtk_Plug_Record) return Guint32 is
      function Internal (Plug : System.Address) return Guint32;
      pragma Import (C, Internal, "gtk_plug_get_id");

   begin
      return Internal (Get_Object (Plug));
   end Get_Id;

end Gtk.Plug;
