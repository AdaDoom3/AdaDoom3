-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 2000                            --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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

with Gtk.Extra.Plot_Data;  use Gtk.Extra.Plot_Data;
with Gtk.Enums;            use Gtk.Enums;

with Glib.Type_Conversion_Hooks;

package body Gtk.Extra.Plot_Bar is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Plot_Bar_Record);
   pragma Warnings (Off, Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Bar         : out Gtk_Plot_Bar;
      Orientation : Gtk.Enums.Gtk_Orientation) is
   begin
      Bar := new Gtk_Plot_Bar_Record;
      Initialize (Bar, Orientation);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Bar         : access Gtk_Plot_Bar_Record'Class;
      Orientation : Gtk.Enums.Gtk_Orientation)
   is
      function Internal (Orientation : Gtk_Orientation) return System.Address;
      pragma Import (C, Internal, "gtk_plot_bar_new");
   begin
      Set_Object (Bar, Internal (Orientation));
   end Initialize;

   ---------------
   -- Set_Width --
   ---------------

   procedure Set_Width
     (Bar   : access Gtk_Plot_Bar_Record'Class;
      Width : Gdouble)
   is
      procedure Internal (Bar : System.Address; Width : Gdouble);
      pragma Import (C, Internal, "gtk_plot_bar_set_width");
   begin
      Internal (Get_Object (Bar), Width);
   end Set_Width;

   ---------------
   -- Get_Width --
   ---------------

   function Get_Width (Bar : access Gtk_Plot_Bar_Record'Class)
      return Gdouble
   is
      function Internal (Bar : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_plot_bar_get_width");
   begin
      return Internal (Get_Object (Bar));
   end Get_Width;
   --  Return the width used to draw the bars
end Gtk.Extra.Plot_Bar;
