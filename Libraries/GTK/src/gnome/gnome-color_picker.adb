-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--               Copyright (C) 2000 Helix Code, Inc.                 --
--               Copyright (C) 2000-2001 ACT-Europe                  --
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
with Gtk; use Gtk;

package body Gnome.Color_Picker is

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New (Color_Picker : out Gnome_Color_Picker) is
   begin
      Color_Picker := new Gnome_Color_Picker_Record;
      Gnome.Color_Picker.Initialize (Color_Picker);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Color_Picker : access Gnome_Color_Picker_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gnome_color_picker_new");
   begin
      Set_Object (Color_Picker, Internal);
   end Initialize;

   ---------
   -- Set --
   ---------

   procedure Set
     (Cpicker : access Gnome_Color_Picker_Record;
      R       : in Gdouble;
      G       : in Gdouble;
      B       : in Gdouble;
      A       : in Gdouble := 0.0)
   is
      procedure Internal
        (Cpicker : System.Address;
         R       : Gdouble;
         G       : Gdouble;
         B       : Gdouble;
         A       : Gdouble);
      pragma Import (C, Internal, "gnome_color_picker_set_d");

   begin
      Internal (Get_Object (Cpicker), R, G, B, A);
   end Set;

   ---------
   -- Get --
   ---------

   procedure Get
     (Cpicker : access Gnome_Color_Picker_Record;
      R       : out Gdouble;
      G       : out Gdouble;
      B       : out Gdouble;
      A       : out Gdouble)
   is
      procedure Internal
        (Cpicker : System.Address;
         R       : out Gdouble;
         G       : out Gdouble;
         B       : out Gdouble;
         A       : out Gdouble);
      pragma Import (C, Internal, "gnome_color_picker_get_d");

   begin
      Internal (Get_Object (Cpicker), R, G, B, A);
   end Get;

   ---------
   -- Set --
   ---------

   procedure Set
     (Cpicker : access Gnome_Color_Picker_Record;
      R       : in Guint8;
      G       : in Guint8;
      B       : in Guint8;
      A       : in Guint8 := 0)
   is
      procedure Internal
        (Cpicker : System.Address;
         R : Guint8;
         G : Guint8;
         B : Guint8;
         A : Guint8);
      pragma Import (C, Internal, "gnome_color_picker_set_i8");

   begin
      Internal (Get_Object (Cpicker), R, G, B, A);
   end Set;

   ---------
   -- Get --
   ---------

   procedure Get
     (Cpicker : access Gnome_Color_Picker_Record;
      R       : out Guint8;
      G       : out Guint8;
      B       : out Guint8;
      A       : out Guint8)
   is
      procedure Internal
        (Cpicker : System.Address;
         R       : out Guint8;
         G       : out Guint8;
         B       : out Guint8;
         A       : out Guint8);
      pragma Import (C, Internal, "gnome_color_picker_get_i8");

   begin
      Internal (Get_Object (Cpicker), R, G, B, A);
   end Get;

   ---------
   -- Set --
   ---------

   procedure Set
     (Cpicker : access Gnome_Color_Picker_Record;
      R       : in Gushort;
      G       : in Gushort;
      B       : in Gushort;
      A       : in Gushort := 0)
   is
      procedure Internal
        (Cpicker : System.Address;
         R : Gushort;
         G : Gushort;
         B : Gushort;
         A : Gushort);
      pragma Import (C, Internal, "gnome_color_picker_set_i16");

   begin
      Internal (Get_Object (Cpicker), R, G, B, A);
   end Set;

   ---------
   -- Get --
   ---------

   procedure Get
     (Cpicker : access Gnome_Color_Picker_Record;
      R       : out Gushort;
      G       : out Gushort;
      B       : out Gushort;
      A       : out Gushort)
   is
      procedure Internal
        (Cpicker : System.Address;
         R       : out Gushort;
         G       : out Gushort;
         B       : out Gushort;
         A       : out Gushort);
      pragma Import (C, Internal, "gnome_color_picker_get_i16");

   begin
      Internal (Get_Object (Cpicker), R, G, B, A);
   end Get;

   ----------------
   -- Set_Dither --
   ----------------

   procedure Set_Dither
     (Cpicker : access Gnome_Color_Picker_Record;
      Dither  : in Boolean)
   is
      procedure Internal
        (Cpicker : System.Address;
         Dither  : Gint);
      pragma Import (C, Internal, "gnome_color_picker_set_dither");

   begin
      Internal (Get_Object (Cpicker), Boolean'Pos (Dither));
   end Set_Dither;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title
     (Cpicker : access Gnome_Color_Picker_Record;
      Title   : in String)
   is
      procedure Internal (Cpicker : System.Address; Title : String);
      pragma Import (C, Internal, "gnome_color_picker_set_title");

   begin
      Internal (Get_Object (Cpicker), Title & ASCII.NUL);
   end Set_Title;

   -------------------
   -- Set_Use_Alpha --
   -------------------

   procedure Set_Use_Alpha
     (Cpicker   : access Gnome_Color_Picker_Record;
      Use_Alpha : in Boolean)
   is
      procedure Internal (Cpicker : System.Address; Use_Alpha : Gint);
      pragma Import (C, Internal, "gnome_color_picker_set_use_alpha");

   begin
      Internal (Get_Object (Cpicker), Boolean'Pos (Use_Alpha));
   end Set_Use_Alpha;

end Gnome.Color_Picker;
