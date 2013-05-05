-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                  Copyright (C) 2000-2013, AdaCore                 --
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
pragma Elaborate_All (Glib.Type_Conversion_Hooks);

package body Gtk.Accel_Label is

   use Gtk.Widget;

   -----------------------
   -- Local Subprograms --
   -----------------------

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Accel_Label_Record);
   pragma Warnings (Off, Type_Conversion);
   --  This package is used to implement a minimal automated type conversion
   --  without having to drag the whole Gtk.Type_Conversion package for the
   --  most common widgets.

   ----------------------
   -- Get_Accel_Widget --
   ----------------------

   function Get_Accel_Widget
     (Accel_Label : access Gtk_Accel_Label_Record) return Gtk_Widget
   is
      function Internal (Accel_Label : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_accel_label_get_accel_widget");

   begin
      return Convert (Internal (Get_Object (Accel_Label)));
   end Get_Accel_Widget;

   ---------------------
   -- Get_Accel_Width --
   ---------------------

   function Get_Accel_Width
     (Accel_Label : access Gtk_Accel_Label_Record) return Guint
   is
      function Internal (Accel_Label : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_accel_label_get_accel_width");

   begin
      return Internal (Get_Object (Accel_Label));
   end Get_Accel_Width;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Accel_Label : out Gtk_Accel_Label; Str : UTF8_String) is
   begin
      Accel_Label := new Gtk_Accel_Label_Record;
      Initialize (Accel_Label, Str);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Accel_Label : access Gtk_Accel_Label_Record'Class; Str : UTF8_String)
   is
      function Internal (Str : String) return System.Address;
      pragma Import (C, Internal, "gtk_accel_label_new");

   begin
      Set_Object (Accel_Label, Internal (Str & ASCII.NUL));
   end Initialize;

   -------------
   -- Refetch --
   -------------

   function Refetch
     (Accel_Label : access Gtk_Accel_Label_Record) return Boolean
   is
      function Internal (Accel_Label : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_accel_label_refetch");

   begin
      return Boolean'Val (Internal (Get_Object (Accel_Label)));
   end Refetch;

   ----------------------
   -- Set_Accel_Widget --
   ----------------------

   procedure Set_Accel_Widget
     (Accel_Label  : access Gtk_Accel_Label_Record;
      Accel_Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
        (Accel_Label  : System.Address;
         Accel_Widget : System.Address);
      pragma Import (C, Internal, "gtk_accel_label_set_accel_widget");

   begin
      Internal (Get_Object (Accel_Label), Get_Object (Accel_Widget));
   end Set_Accel_Widget;

end Gtk.Accel_Label;
