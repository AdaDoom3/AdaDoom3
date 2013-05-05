-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
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

--  <description>
--  A special kind of child that can be put in a Gtk_Plot_Canvas.
--  </description>
--  <c_version>gtkextra 2.1.1</c_version>

with Gdk.Bitmap;           use Gdk.Bitmap;
with Gdk.Pixmap;           use Gdk.Pixmap;
with System;

with Glib.Type_Conversion_Hooks;

package body Gtk.Extra.Plot_Canvas.Pixmap is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Plot_Canvas_Pixmap_Record);
   pragma Warnings (Off, Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Child    : out Gtk_Plot_Canvas_Pixmap;
      Pixmap   : Gdk.Pixmap.Gdk_Pixmap;
      Mask     : Gdk.Bitmap.Gdk_Bitmap)
   is
      function Internal
        (Pixmap : Gdk.Pixmap.Gdk_Pixmap; Mask : Gdk.Bitmap.Gdk_Bitmap)
         return System.Address;
      pragma Import (C, Internal, "gtk_plot_canvas_pixmap_new");
   begin
      Child := new Gtk_Plot_Canvas_Pixmap_Record;
      Set_Object (Child, Internal (Pixmap, Mask));
   end Gtk_New;

end Gtk.Extra.Plot_Canvas.Pixmap;
