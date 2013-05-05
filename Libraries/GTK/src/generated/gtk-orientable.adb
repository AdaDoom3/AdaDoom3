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
package body Gtk.Orientable is
   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : Gtk_Orientable) return Gtk.Enums.Gtk_Orientation
   is
      function Internal (Self : Gtk_Orientable) return Integer;
      pragma Import (C, Internal, "gtk_orientable_get_orientation");
   begin
      return Gtk.Enums.Gtk_Orientation'Val (Internal (Self));
   end Get_Orientation;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
      (Self        : Gtk_Orientable;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal (Self : Gtk_Orientable; Orientation : Integer);
      pragma Import (C, Internal, "gtk_orientable_set_orientation");
   begin
      Internal (Self, Gtk.Enums.Gtk_Orientation'Pos (Orientation));
   end Set_Orientation;

end Gtk.Orientable;
