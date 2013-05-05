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

package body Gtk.Separator is
   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Separator_Record);
   pragma Unreferenced (Type_Conversion);

   ------------------------
   -- Gtk_New_Hseparator --
   ------------------------

   procedure Gtk_New_Hseparator (Separator : out Gtk_Hseparator) is
   begin
      Separator := new Gtk_Hseparator_Record;
      Gtk.Separator.Initialize_Hseparator (Separator);
   end Gtk_New_Hseparator;

   ------------------------
   -- Gtk_New_Vseparator --
   ------------------------

   procedure Gtk_New_Vseparator (Separator : out Gtk_Vseparator) is
   begin
      Separator := new Gtk_Vseparator_Record;
      Gtk.Separator.Initialize_Vseparator (Separator);
   end Gtk_New_Vseparator;

   ---------------------------
   -- Initialize_Hseparator --
   ---------------------------

   procedure Initialize_Hseparator
      (Separator : access Gtk_Hseparator_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_hseparator_new");
   begin
      Set_Object (Separator, Internal);
   end Initialize_Hseparator;

   ---------------------------
   -- Initialize_Vseparator --
   ---------------------------

   procedure Initialize_Vseparator
      (Separator : access Gtk_Vseparator_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_vseparator_new");
   begin
      Set_Object (Separator, Internal);
   end Initialize_Vseparator;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : access Gtk_Separator_Record) return Gtk.Enums.Gtk_Orientation
   is
      function Internal (Self : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_orientable_get_orientation");
   begin
      return Gtk.Enums.Gtk_Orientation'Val (Internal (Get_Object (Self)));
   end Get_Orientation;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
      (Self        : access Gtk_Separator_Record;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal (Self : System.Address; Orientation : Integer);
      pragma Import (C, Internal, "gtk_orientable_set_orientation");
   begin
      Internal (Get_Object (Self), Gtk.Enums.Gtk_Orientation'Pos (Orientation));
   end Set_Orientation;

end Gtk.Separator;
