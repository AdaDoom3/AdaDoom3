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

with Glib;            use Glib;
with Gtk.Box;         use Gtk.Box;
with Gtk.Button;      use Gtk.Button;
with Gtk.Curve;       use Gtk.Curve;
with Gtk.Gamma_Curve; use Gtk.Gamma_Curve;
with Gtk.Handlers;    use Gtk.Handlers;
with Gtk;             use Gtk;

with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;

package body Create_Gamma_Curve is

   package Float_P is new Ada.Numerics.Generic_Elementary_Functions (Gfloat);
   package Gamma_Cb is new Handlers.Callback (Gtk_Button_Record);

   Count  : Gint := 0;
   Curve  : Gtk_Gamma_Curve;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A very specific widget to edit a curve. This is based on a "
        & " @bGtk_Curve@B, a modified version of the @bGtk_Drawing_Area@B."
        & " You problably won't need to use this...";
   end Help;

   ------------------
   -- Change_Curve --
   ------------------

   procedure Change_Curve (Button : access Gtk_Button_Record'Class) is
      pragma Warnings (Off, Button);
      Max    : constant Gint := 127 + (Count mod 4) * 128;
      Vec    : Gfloat_Array (1 .. Positive (Max));
   begin
      if (Count mod 4 /= 3) then
         Ada.Text_IO.Put_Line ("Redrawing the window with "
                               & Gint'Image (Max)
                               & " points");
      end if;
      Set_Range (Get_Curve (Curve), 0.0, Gfloat (Max), 0.0, Gfloat (Max));
      for J in Vec'Range loop
         Vec (J) := (127.0 / Float_P.Sqrt (Gfloat (Max)))
           * Float_P.Sqrt (Gfloat (J));
      end loop;
      Set_Vector (Get_Curve (Curve), Vec);
      Count := Count + 1;
   end Change_Curve;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Button : Gtk_Button;
      Box    : Gtk_Box;

   begin
      Set_Label (Frame, "Gamma Curve");

      Gtk_New_Vbox (Box, Homogeneous => False, Spacing => 0);
      Add (Frame, Box);

      Gtk_New (Curve);
      Pack_Start (Box, Curve, False, False);

      Gtk_New (Button, "Change mode");
      Pack_Start (Box, Button, False, False);
      Gamma_Cb.Connect (Button, "clicked",
                        Gamma_Cb.To_Marshaller (Change_Curve'Access));

      Gamma_Cb.Emit_By_Name (Button, "clicked");

      Show_All (Frame);
   end Run;

end Create_Gamma_Curve;

