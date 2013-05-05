-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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

with Glib;        use Glib;
with Gtk.Box;     use Gtk.Box;
with Gtk.Enums;   use Gtk.Enums;
with Glib.Main;   use Glib.Main;
with Gtk.Preview; use Gtk.Preview;
with Gtk.Widget;  use Gtk.Widget;
with Gtk.Window;  use Gtk.Window;
with Gtk;         use Gtk;
with Common;      use Common;

package body Create_Preview_Color is

   package Preview_Idle is new Glib.Main.Generic_Sources (Gtk_Preview);

   Window : aliased Gtk.Window.Gtk_Window;

   Color_Idle : G_Source_Id := 0;
   Count      : Guchar := 1;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "The @bGtk_Preview@B widget displays an RGB image, which can be"
        & " easily manipulated through an array of @bGuchar@Bs. The image"
        & " can be either color or grayscale.";
   end Help;

   ---------------------
   -- Color_Idle_Func --
   ---------------------

   function Color_Idle_Func (Preview : Gtk_Preview) return Boolean is
      Buf : Guchar_Array (0 .. 767);
      K   : Natural;
   begin
      for I in 0 .. Guchar'(255) loop
         K := 0;
         for J in 0 .. Guchar'(255) loop
            Buf (K + 0) := I + Count;
            Buf (K + 1) := 0;
            Buf (K + 2) := J + Count;
            K := K + 3;
         end loop;
         Draw_Row (Preview, Buf, 0, Gint (I), 256);
      end loop;
      Count := Count + 1;
      Draw (Preview);
      return True;
   end Color_Idle_Func;

   ---------------------
   -- Preview_Destroy --
   ---------------------

   procedure Preview_Destroy (Dummy : access Gtk_Widget_Record'Class) is
      pragma Warnings (Off, Dummy);
   begin
      if Color_Idle > 0 then
         Remove (Color_Idle);
         Color_Idle := 0;
      end if;
      Window := null;
   end Preview_Destroy;

   ------------------
   -- Demo_Destroy --
   ------------------

   procedure Demo_Destroy (Dummy : access Gtk_Widget_Record'Class) is
   begin
      if Window /= null then
         Destroy (Window);
         Preview_Destroy (Dummy);
      end if;
   end Demo_Destroy;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Preview : Gtk_Preview;
      Box     : Gtk_Box;

   begin
      if Window = null then

         --  Create a dummy widget, that will tell us whenever the user
         --  selected a new demo (since the children of Frame are automatically
         --  deleted in that case). We can then close the dialog.

         Gtk_New_Vbox (Box, Homogeneous => False);
         Add (Frame, Box);
         Widget_Handler.Connect
           (Box, "destroy",
            Widget_Handler.To_Marshaller (Demo_Destroy'Access));

         --  Now create the real demo

         Gtk_New (Window, Window_Toplevel);
         Widget_Handler.Connect
           (Window, "destroy",
            Widget_Handler.To_Marshaller (Preview_Destroy'Access));
         Set_Title (Window, "test");
         Set_Border_Width (Window, Border_Width => 10);

         Gtk_New (Preview, Preview_Color);
         Size (Preview, 256, 256);
         Add (Window, Preview);

         Color_Idle := Preview_Idle.Idle_Add (Color_Idle_Func'Access, Preview);
         Show_All (Window);
      else
         Destroy (Window);
      end if;

   end Run;

end Create_Preview_Color;
