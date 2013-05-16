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

package body Gtk.Alignment is
   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Alignment_Record);
   pragma Unreferenced (Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Alignment : out Gtk_Alignment;
       Xalign    : Gfloat;
       Yalign    : Gfloat;
       Xscale    : Gfloat;
       Yscale    : Gfloat)
   is
   begin
      Alignment := new Gtk_Alignment_Record;
      Gtk.Alignment.Initialize (Alignment, Xalign, Yalign, Xscale, Yscale);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Alignment : access Gtk_Alignment_Record'Class;
       Xalign    : Gfloat;
       Yalign    : Gfloat;
       Xscale    : Gfloat;
       Yscale    : Gfloat)
   is
      function Internal
         (Xalign : Gfloat;
          Yalign : Gfloat;
          Xscale : Gfloat;
          Yscale : Gfloat) return System.Address;
      pragma Import (C, Internal, "gtk_alignment_new");
   begin
      Set_Object (Alignment, Internal (Xalign, Yalign, Xscale, Yscale));
   end Initialize;

   -----------------
   -- Get_Padding --
   -----------------

   procedure Get_Padding
      (Alignment      : access Gtk_Alignment_Record;
       Padding_Top    : out Guint;
       Padding_Bottom : out Guint;
       Padding_Left   : out Guint;
       Padding_Right  : out Guint)
   is
      procedure Internal
         (Alignment      : System.Address;
          Padding_Top    : out Guint;
          Padding_Bottom : out Guint;
          Padding_Left   : out Guint;
          Padding_Right  : out Guint);
      pragma Import (C, Internal, "gtk_alignment_get_padding");
   begin
      Internal (Get_Object (Alignment), Padding_Top, Padding_Bottom, Padding_Left, Padding_Right);
   end Get_Padding;

   ---------
   -- Set --
   ---------

   procedure Set
      (Alignment : access Gtk_Alignment_Record;
       Xalign    : Gfloat;
       Yalign    : Gfloat;
       Xscale    : Gfloat;
       Yscale    : Gfloat)
   is
      procedure Internal
         (Alignment : System.Address;
          Xalign    : Gfloat;
          Yalign    : Gfloat;
          Xscale    : Gfloat;
          Yscale    : Gfloat);
      pragma Import (C, Internal, "gtk_alignment_set");
   begin
      Internal (Get_Object (Alignment), Xalign, Yalign, Xscale, Yscale);
   end Set;

   -----------------
   -- Set_Padding --
   -----------------

   procedure Set_Padding
      (Alignment      : access Gtk_Alignment_Record;
       Padding_Top    : Guint;
       Padding_Bottom : Guint;
       Padding_Left   : Guint;
       Padding_Right  : Guint)
   is
      procedure Internal
         (Alignment      : System.Address;
          Padding_Top    : Guint;
          Padding_Bottom : Guint;
          Padding_Left   : Guint;
          Padding_Right  : Guint);
      pragma Import (C, Internal, "gtk_alignment_set_padding");
   begin
      Internal (Get_Object (Alignment), Padding_Top, Padding_Bottom, Padding_Left, Padding_Right);
   end Set_Padding;

   ----------------
   -- Get_Xalign --
   ----------------

   function Get_Xalign
      (Alignment : access Gtk_Alignment_Record) return Gfloat
   is
      function Internal (Alignment : System.Address) return Gfloat;
      pragma Import (C, Internal, "gtkada_GtkAlignment_get_xalign");
   begin
      return Internal (Get_Object (Alignment));
   end Get_Xalign;

   ----------------
   -- Get_Xscale --
   ----------------

   function Get_Xscale
      (Alignment : access Gtk_Alignment_Record) return Gfloat
   is
      function Internal (Alignment : System.Address) return Gfloat;
      pragma Import (C, Internal, "gtkada_GtkAlignment_get_xscale");
   begin
      return Internal (Get_Object (Alignment));
   end Get_Xscale;

   ----------------
   -- Get_Yalign --
   ----------------

   function Get_Yalign
      (Alignment : access Gtk_Alignment_Record) return Gfloat
   is
      function Internal (Alignment : System.Address) return Gfloat;
      pragma Import (C, Internal, "gtkada_GtkAlignment_get_yalign");
   begin
      return Internal (Get_Object (Alignment));
   end Get_Yalign;

   ----------------
   -- Get_Yscale --
   ----------------

   function Get_Yscale
      (Alignment : access Gtk_Alignment_Record) return Gfloat
   is
      function Internal (Alignment : System.Address) return Gfloat;
      pragma Import (C, Internal, "gtkada_GtkAlignment_get_yscale");
   begin
      return Internal (Get_Object (Alignment));
   end Get_Yscale;

end Gtk.Alignment;
