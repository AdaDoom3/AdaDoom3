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

package body Gtk.Gamma_Curve is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Gamma_Curve_Record);
   pragma Warnings (Off, Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Gamma_Curve : out Gtk_Gamma_Curve) is
   begin
      Gamma_Curve := new Gtk_Gamma_Curve_Record;
      Gtk.Gamma_Curve.Initialize (Gamma_Curve);
   end Gtk_New;

   ---------------
   -- Get_Curve --
   ---------------

   function Get_Curve
     (Gamma_Curve : access Gtk_Gamma_Curve_Record) return Gtk.Curve.Gtk_Curve
   is
      function Internal (Widget : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_gamma_curve_get_curve");

      Stub : Gtk.Curve.Gtk_Curve_Record;

   begin
      return Gtk.Curve.Gtk_Curve
        (Get_User_Data (Internal (Get_Object (Gamma_Curve)), Stub));
   end Get_Curve;

   ---------------
   -- Get_Gamma --
   ---------------

   function Get_Gamma
     (Gamma_Curve : access Gtk_Gamma_Curve_Record) return Gfloat
   is
      function Internal (Widget : System.Address) return Gfloat;
      pragma Import (C, Internal, "ada_gamma_curve_get_gamma");

   begin
      return Internal (Get_Object (Gamma_Curve));
   end Get_Gamma;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Gamma_Curve : access Gtk_Gamma_Curve_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_gamma_curve_new");

   begin
      Set_Object (Gamma_Curve, Internal);
   end Initialize;

end Gtk.Gamma_Curve;
