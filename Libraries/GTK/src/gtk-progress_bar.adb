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
with Interfaces.C.Strings;
with Pango.Layout;         use Pango.Layout;

with Glib.Type_Conversion_Hooks;

package body Gtk.Progress_Bar is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Progress_Bar_Record);
   pragma Warnings (Off, Type_Conversion);

   package ICS renames Interfaces.C.Strings;

   ------------------
   -- Get_Fraction --
   ------------------

   function Get_Fraction
     (Progress_Bar : access Gtk_Progress_Bar_Record) return Gdouble
   is
      function Internal (Progress_Bar : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_progress_bar_get_fraction");

   begin
      return Internal (Get_Object (Progress_Bar));
   end Get_Fraction;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
     (Progress_Bar : access Gtk_Progress_Bar_Record)
      return Gtk_Progress_Bar_Orientation
   is
      function Internal
        (Progress_Bar : System.Address) return Gtk_Progress_Bar_Orientation;
      pragma Import (C, Internal, "gtk_progress_bar_get_orientation");

   begin
      return Internal (Get_Object (Progress_Bar));
   end Get_Orientation;

   --------------------
   -- Get_Pulse_Step --
   --------------------

   function Get_Pulse_Step
     (Progress_Bar : access Gtk_Progress_Bar_Record) return Gdouble
   is
      function Internal (Progress_Bar : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_progress_bar_get_pulse_step");

   begin
      return Internal (Get_Object (Progress_Bar));
   end Get_Pulse_Step;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
     (Progress_Bar : access Gtk_Progress_Bar_Record) return UTF8_String
   is
      function Internal
        (Progress_Bar : System.Address) return ICS.chars_ptr;
      pragma Import (C, Internal, "gtk_progress_bar_get_text");

   begin
      return ICS.Value (Internal (Get_Object (Progress_Bar)));
   end Get_Text;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Progress_Bar : out Gtk_Progress_Bar) is
   begin
      Progress_Bar := new Gtk_Progress_Bar_Record;
      Gtk.Progress_Bar.Initialize (Progress_Bar);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Progress_Bar : access Gtk_Progress_Bar_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_progress_bar_new");

   begin
      Set_Object (Progress_Bar, Internal);
   end Initialize;

   -----------
   -- Pulse --
   -----------

   procedure Pulse (Progress_Bar : access Gtk_Progress_Bar_Record) is
      procedure Internal (Progress_Bar : System.Address);
      pragma Import (C, Internal, "gtk_progress_bar_pulse");

   begin
      Internal (Get_Object (Progress_Bar));
   end Pulse;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
     (Progress_Bar : access Gtk_Progress_Bar_Record;
      Text         : UTF8_String)
   is
      procedure Internal
        (Progress_Bar : System.Address;
         Text         : UTF8_String);
      pragma Import (C, Internal, "gtk_progress_bar_set_text");

   begin
      Internal (Get_Object (Progress_Bar), Text & ASCII.NUL);
   end Set_Text;

   ------------------
   -- Set_Fraction --
   ------------------

   procedure Set_Fraction
     (Progress_Bar : access Gtk_Progress_Bar_Record; Fraction : Gdouble)
   is
      procedure Internal (Progress_Bar : System.Address; Fraction : Gdouble);
      pragma Import (C, Internal, "gtk_progress_bar_set_fraction");

   begin
      Internal (Get_Object (Progress_Bar), Fraction);
   end Set_Fraction;

   --------------------
   -- Set_Pulse_Step --
   --------------------

   procedure Set_Pulse_Step
     (Progress_Bar : access Gtk_Progress_Bar_Record; Step : Gdouble)
   is
      procedure Internal (Progress_Bar : System.Address; Step : Gdouble);
      pragma Import (C, Internal, "gtk_progress_bar_set_pulse_step");

   begin
      Internal (Get_Object (Progress_Bar), Step);
   end Set_Pulse_Step;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
     (Progress_Bar : access Gtk_Progress_Bar_Record;
      Orientation  : Gtk_Progress_Bar_Orientation)
   is
      procedure Internal
        (Progress_Bar : System.Address;
         Orientation  : Gtk_Progress_Bar_Orientation);
      pragma Import (C, Internal, "gtk_progress_bar_set_orientation");

   begin
      Internal (Get_Object (Progress_Bar), Orientation);
   end Set_Orientation;

   -------------------
   -- Get_Ellipsize --
   -------------------

   function Get_Ellipsize
     (Pbar : access Gtk_Progress_Bar_Record)
      return Pango_Ellipsize_Mode
   is
      function Internal
        (Pbar : System.Address)
         return Pango_Ellipsize_Mode;
      pragma Import (C, Internal, "gtk_progress_bar_get_ellipsize");
   begin
      return Internal (Get_Object (Pbar));
   end Get_Ellipsize;

   -------------------
   -- Set_Ellipsize --
   -------------------

   procedure Set_Ellipsize
     (Pbar : access Gtk_Progress_Bar_Record;
      Mode : Pango_Ellipsize_Mode)
   is
      procedure Internal
        (Pbar : System.Address;
         Mode : Pango_Ellipsize_Mode);
      pragma Import (C, Internal, "gtk_progress_bar_set_ellipsize");
   begin
      Internal (Get_Object (Pbar), Mode);
   end Set_Ellipsize;

end Gtk.Progress_Bar;
