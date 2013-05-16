-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                    Copyright (C) 2010-2013, AdaCore               --
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

package body Gtk.Print_Context is

   --------------------------
   -- Create_Pango_Context --
   --------------------------

   function Create_Pango_Context
     (Context : access Gtk_Print_Context_Record)
      return Pango.Context.Pango_Context
   is
      function Internal (Context : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_print_context_create_pango_context");
      Stub : Pango.Context.Pango_Context_Record;
   begin
      return Pango.Context.Pango_Context
        (Get_User_Data (Internal (Get_Object (Context)), Stub));
   end Create_Pango_Context;

   -------------------------
   -- Create_Pango_Layout --
   -------------------------

   function Create_Pango_Layout
     (Context : access Gtk_Print_Context_Record)
      return Pango.Layout.Pango_Layout
   is
      function Internal (Context : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_print_context_create_pango_layout");
      Stub : Pango.Layout.Pango_Layout_Record;
   begin
      return Pango.Layout.Pango_Layout
        (Get_User_Data (Internal (Get_Object (Context)), Stub));
   end Create_Pango_Layout;

   -----------------------
   -- Get_Cairo_Context --
   -----------------------

   function Get_Cairo_Context
     (Context : access Gtk_Print_Context_Record)
      return Cairo.Cairo_Context
   is
      function Internal (Context : System.Address) return Cairo.Cairo_Context;
      pragma Import (C, Internal, "gtk_print_context_get_cairo_context");
   begin
      return Internal (Get_Object (Context));
   end Get_Cairo_Context;

   ---------------
   -- Get_Dpi_X --
   ---------------

   function Get_Dpi_X
     (Context : access Gtk_Print_Context_Record)
      return Gdouble
   is
      function Internal
        (Context : System.Address)
         return Gdouble;
      pragma Import (C, Internal, "gtk_print_context_get_dpi_x");
   begin
      return Internal (Get_Object (Context));
   end Get_Dpi_X;

   ---------------
   -- Get_Dpi_Y --
   ---------------

   function Get_Dpi_Y
     (Context : access Gtk_Print_Context_Record)
      return Gdouble
   is
      function Internal
        (Context : System.Address)
         return Gdouble;
      pragma Import (C, Internal, "gtk_print_context_get_dpi_y");
   begin
      return Internal (Get_Object (Context));
   end Get_Dpi_Y;

   ----------------
   -- Get_Height --
   ----------------

   function Get_Height
     (Context : access Gtk_Print_Context_Record)
      return Gdouble
   is
      function Internal
        (Context : System.Address)
         return Gdouble;
      pragma Import (C, Internal, "gtk_print_context_get_height");
   begin
      return Internal (Get_Object (Context));
   end Get_Height;

   --------------------
   -- Get_Page_Setup --
   --------------------

   function Get_Page_Setup
     (Context : access Gtk_Print_Context_Record)
      return Gtk.Page_Setup.Gtk_Page_Setup
   is
      function Internal (Context : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_print_context_get_page_setup");
      Stub : Gtk.Page_Setup.Gtk_Page_Setup_Record;
   begin
      return Gtk.Page_Setup.Gtk_Page_Setup
        (Get_User_Data (Internal (Get_Object (Context)), Stub));
   end Get_Page_Setup;

   ---------------
   -- Get_Width --
   ---------------

   function Get_Width
     (Context : access Gtk_Print_Context_Record)
      return Gdouble
   is
      function Internal (Context : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_print_context_get_width");
   begin
      return Internal (Get_Object (Context));
   end Get_Width;

   -----------------------
   -- Set_Cairo_Context --
   -----------------------

   procedure Set_Cairo_Context
     (Context : access Gtk_Print_Context_Record;
      Cr      : Cairo.Cairo_Context;
      Dpi_X   : Gdouble;
      Dpi_Y   : Gdouble)
   is
      procedure Internal
        (Context : System.Address;
         Cr      : Cairo.Cairo_Context;
         Dpi_X   : Gdouble;
         Dpi_Y   : Gdouble);
      pragma Import (C, Internal, "gtk_print_context_set_cairo_context");
   begin
      Internal (Get_Object (Context), Cr, Dpi_X, Dpi_Y);
   end Set_Cairo_Context;

end Gtk.Print_Context;
