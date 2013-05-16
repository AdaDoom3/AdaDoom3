-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                       Copyright (C) 2000                          --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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

with Glib.Type_Conversion_Hooks;

package body Gtk.Extra.Plot_Polar is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Plot_Polar_Record);
   pragma Warnings (Off, Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Polar       : out Gtk_Plot_Polar;
      Drawable      : Gdk.Drawable.Gdk_Drawable := null;
      Width, Height : Gdouble := 0.0) is
   begin
      Polar := new Gtk_Plot_Polar_Record;
      Initialize (Polar, Drawable, Width, Height);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Polar       : access Gtk_Plot_Polar_Record'Class;
      Drawable      : Gdk.Drawable.Gdk_Drawable;
      Width, Height : Gdouble := 0.0)
   is
      function Internal (Drawable : Gdk_Drawable) return System.Address;
      pragma Import (C, Internal, "gtk_plot_polar_new");

      function Internal2
        (Drawable : Gdk_Drawable; W, H : Gdouble) return System.Address;
      pragma Import (C, Internal2, "gtk_plot_polar_new_with_size");
   begin
      if Width = 0.0 and then Height = 0.0 then
         Set_Object (Polar, Internal (Drawable));
      else
         Set_Object (Polar, Internal2 (Drawable, Width, Height));
      end if;
   end Initialize;

   ------------
   -- Rotate --
   ------------

   procedure Rotate (Polar : access Gtk_Plot_Polar_Record; Angle : Gdouble) is
      procedure Internal (Polar : System.Address; Angle : Gdouble);
      pragma Import (C, Internal, "gtk_plot_polar_rotate");
   begin
      Internal (Get_Object (Polar), Angle);
   end Rotate;

   ---------------
   -- Get_Angle --
   ---------------

   function Get_Angle (Polar : access Gtk_Plot_Polar_Record) return Gdouble is
      function Internal (Polar : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_plot_polar_get_angle");
   begin
      return Internal (Get_Object (Polar));
   end Get_Angle;

end Gtk.Extra.Plot_Polar;
