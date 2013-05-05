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

package body Gtk.Fixed is
   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Fixed_Record);
   pragma Unreferenced (Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Fixed : out Gtk_Fixed) is
   begin
      Fixed := new Gtk_Fixed_Record;
      Gtk.Fixed.Initialize (Fixed);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Fixed : access Gtk_Fixed_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_fixed_new");
   begin
      Set_Object (Fixed, Internal);
   end Initialize;

   --------------------
   -- Get_Has_Window --
   --------------------

   function Get_Has_Window (Fixed : access Gtk_Fixed_Record) return Boolean is
      function Internal (Fixed : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_fixed_get_has_window");
   begin
      return Boolean'Val (Internal (Get_Object (Fixed)));
   end Get_Has_Window;

   ----------
   -- Move --
   ----------

   procedure Move
      (Fixed  : access Gtk_Fixed_Record;
       Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
       X      : Gint;
       Y      : Gint)
   is
      procedure Internal
         (Fixed  : System.Address;
          Widget : System.Address;
          X      : Gint;
          Y      : Gint);
      pragma Import (C, Internal, "gtk_fixed_move");
   begin
      Internal (Get_Object (Fixed), Get_Object (Widget), X, Y);
   end Move;

   ---------
   -- Put --
   ---------

   procedure Put
      (Fixed  : access Gtk_Fixed_Record;
       Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
       X      : Gint;
       Y      : Gint)
   is
      procedure Internal
         (Fixed  : System.Address;
          Widget : System.Address;
          X      : Gint;
          Y      : Gint);
      pragma Import (C, Internal, "gtk_fixed_put");
   begin
      Internal (Get_Object (Fixed), Get_Object (Widget), X, Y);
   end Put;

   --------------------
   -- Set_Has_Window --
   --------------------

   procedure Set_Has_Window
      (Fixed      : access Gtk_Fixed_Record;
       Has_Window : Boolean := False)
   is
      procedure Internal (Fixed : System.Address; Has_Window : Integer);
      pragma Import (C, Internal, "gtk_fixed_set_has_window");
   begin
      Internal (Get_Object (Fixed), Boolean'Pos (Has_Window));
   end Set_Has_Window;

end Gtk.Fixed;
