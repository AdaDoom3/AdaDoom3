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

with Interfaces.C.Strings; use Interfaces.C.Strings;
with Gtkada.Bindings;      use Gtkada.Bindings;

package body Gtk.Page_Setup is

   ----------
   -- Copy --
   ----------

   function Copy
     (Other : access Gtk_Page_Setup_Record) return Gtk_Page_Setup
   is
      function Internal (Other : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_page_setup_copy");
      Stub : Gtk_Page_Setup_Record;
   begin
      return Gtk_Page_Setup
        (Get_User_Data (Internal (Get_Object (Other)), Stub));
   end Copy;

   -----------------------
   -- Get_Bottom_Margin --
   -----------------------

   function Get_Bottom_Margin
     (Setup : access Gtk_Page_Setup_Record;
      Unit  : Gtk.Enums.Gtk_Unit)
      return Gdouble
   is
      function Internal
        (Setup : System.Address;
         Unit  : Gtk.Enums.Gtk_Unit)
         return Gdouble;
      pragma Import (C, Internal, "gtk_page_setup_get_bottom_margin");
   begin
      return Internal (Get_Object (Setup), Unit);
   end Get_Bottom_Margin;

   ---------------------
   -- Get_Left_Margin --
   ---------------------

   function Get_Left_Margin
     (Setup : access Gtk_Page_Setup_Record;
      Unit  : Gtk.Enums.Gtk_Unit)
      return Gdouble
   is
      function Internal
        (Setup : System.Address;
         Unit  : Gtk.Enums.Gtk_Unit)
         return Gdouble;
      pragma Import (C, Internal, "gtk_page_setup_get_left_margin");
   begin
      return Internal (Get_Object (Setup), Unit);
   end Get_Left_Margin;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
     (Setup : access Gtk_Page_Setup_Record)
      return Gtk.Enums.Gtk_Page_Orientation
   is
      function Internal (Setup : System.Address)
        return Gtk.Enums.Gtk_Page_Orientation;
      pragma Import (C, Internal, "gtk_page_setup_get_orientation");
   begin
      return Internal (Get_Object (Setup));
   end Get_Orientation;

   ---------------------
   -- Get_Page_Height --
   ---------------------

   function Get_Page_Height
     (Setup : access Gtk_Page_Setup_Record;
      Unit  : Gtk.Enums.Gtk_Unit)
      return Gdouble
   is
      function Internal
        (Setup : System.Address;
         Unit  : Gtk.Enums.Gtk_Unit)
         return Gdouble;
      pragma Import (C, Internal, "gtk_page_setup_get_page_height");
   begin
      return Internal (Get_Object (Setup), Unit);
   end Get_Page_Height;

   --------------------
   -- Get_Page_Width --
   --------------------

   function Get_Page_Width
     (Setup : access Gtk_Page_Setup_Record;
      Unit  : Gtk.Enums.Gtk_Unit)
      return Gdouble
   is
      function Internal
        (Setup : System.Address;
         Unit  : Gtk.Enums.Gtk_Unit)
         return Gdouble;
      pragma Import (C, Internal, "gtk_page_setup_get_page_width");
   begin
      return Internal (Get_Object (Setup), Unit);
   end Get_Page_Width;

   ----------------------
   -- Get_Paper_Height --
   ----------------------

   function Get_Paper_Height
     (Setup : access Gtk_Page_Setup_Record;
      Unit  : Gtk.Enums.Gtk_Unit)
      return Gdouble
   is
      function Internal
        (Setup : System.Address;
         Unit  : Gtk.Enums.Gtk_Unit)
         return Gdouble;
      pragma Import (C, Internal, "gtk_page_setup_get_paper_height");
   begin
      return Internal (Get_Object (Setup), Unit);
   end Get_Paper_Height;

   --------------------
   -- Get_Paper_Size --
   --------------------

   function Get_Paper_Size
     (Setup : access Gtk_Page_Setup_Record)
      return Gtk.Paper_Size.Gtk_Paper_Size
   is
      function Internal (Setup : System.Address)
        return Gtk.Paper_Size.Gtk_Paper_Size;
      pragma Import (C, Internal, "gtk_page_setup_get_paper_size");
   begin
      return Internal (Get_Object (Setup));
   end Get_Paper_Size;

   ---------------------
   -- Get_Paper_Width --
   ---------------------

   function Get_Paper_Width
     (Setup : access Gtk_Page_Setup_Record;
      Unit  : Gtk.Enums.Gtk_Unit)
      return Gdouble
   is
      function Internal
        (Setup : System.Address;
         Unit  : Gtk.Enums.Gtk_Unit)
         return Gdouble;
      pragma Import (C, Internal, "gtk_page_setup_get_paper_width");
   begin
      return Internal (Get_Object (Setup), Unit);
   end Get_Paper_Width;

   ----------------------
   -- Get_Right_Margin --
   ----------------------

   function Get_Right_Margin
     (Setup : access Gtk_Page_Setup_Record;
      Unit  : Gtk.Enums.Gtk_Unit)
      return Gdouble
   is
      function Internal
        (Setup : System.Address;
         Unit  : Gtk.Enums.Gtk_Unit)
         return Gdouble;
      pragma Import (C, Internal, "gtk_page_setup_get_right_margin");
   begin
      return Internal (Get_Object (Setup), Unit);
   end Get_Right_Margin;

   --------------------
   -- Get_Top_Margin --
   --------------------

   function Get_Top_Margin
     (Setup : access Gtk_Page_Setup_Record;
      Unit  : Gtk.Enums.Gtk_Unit)
      return Gdouble
   is
      function Internal
        (Setup : System.Address;
         Unit  : Gtk.Enums.Gtk_Unit)
         return Gdouble;
      pragma Import (C, Internal, "gtk_page_setup_get_top_margin");
   begin
      return Internal (Get_Object (Setup), Unit);
   end Get_Top_Margin;

   ---------------
   -- Load_File --
   ---------------

   function Load_File
     (Setup     : access Gtk_Page_Setup_Record;
      File_Name : String;
      Error     : Glib.Error.GError := null)
      return Boolean
   is
      function Internal
        (Setup     : System.Address;
         File_Name : String;
         Error     : Glib.Error.GError)
         return Gboolean;
      pragma Import (C, Internal, "gtk_page_setup_load_file");
   begin
      return Boolean'Val (Internal
        (Get_Object (Setup), File_Name & ASCII.NUL, Error));
   end Load_File;

   -------------------
   -- Load_Key_File --
   -------------------

   function Load_Key_File
     (Setup      : access Gtk_Page_Setup_Record;
      Key_File   : Glib.Key_File.G_Key_File;
      Group_Name : String := "";
      Error     : Glib.Error.GError := null)
      return Boolean
   is
      function Internal
        (Setup      : System.Address;
         Key_File   : Glib.Key_File.G_Key_File;
         Group_Name : chars_ptr;
         Error      : Glib.Error.GError)
         return Gboolean;
      pragma Import (C, Internal, "gtk_page_setup_load_key_file");
      G : chars_ptr := String_Or_Null (Group_Name);
      Result : Gboolean;
   begin
      Result := Internal (Get_Object (Setup), Key_File, G, Error);
      Free (G);
      return Boolean'Val (Result);
   end Load_Key_File;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Page_Setup) is
   begin
      Widget := new Gtk_Page_Setup_Record;
      Gtk.Page_Setup.Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Page_Setup_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_page_setup_new");
   begin
      Set_Object (Widget, Internal);
   end Initialize;

   -----------------------
   -- Gtk_New_From_File --
   -----------------------

   procedure Gtk_New_From_File
     (Widget    : out Gtk_Page_Setup;
      File_Name : String;
      Error     : Glib.Error.GError := null)
   is
   begin
      Widget := new Gtk_Page_Setup_Record;
      Initialize_From_File (Widget, File_Name, Error);
   end Gtk_New_From_File;

   --------------------------
   -- Initialize_From_File --
   --------------------------

   procedure Initialize_From_File
     (Widget    : access Gtk_Page_Setup_Record'Class;
      File_Name : String;
      Error     : Glib.Error.GError := null)
   is
      function Internal
        (File_Name : String;
         Error     : Glib.Error.GError)
         return System.Address;
      pragma Import (C, Internal, "gtk_page_setup_new_from_file");
   begin
      Set_Object (Widget, Internal (File_Name & ASCII.NUL, Error));
   end Initialize_From_File;

   ---------------------------
   -- Gtk_New_From_Key_File --
   ---------------------------

   procedure Gtk_New_From_Key_File
     (Widget     : out Gtk_Page_Setup;
      Key_File   : Glib.Key_File.G_Key_File;
      Group_Name : String := "";
      Error      : Glib.Error.GError := null)
   is
   begin
      Widget := new Gtk_Page_Setup_Record;
      Initialize_From_Key_File (Widget, Key_File, Group_Name, Error);
   end Gtk_New_From_Key_File;

   ------------------------------
   -- Initialize_From_Key_File --
   ------------------------------

   procedure Initialize_From_Key_File
     (Widget     : access Gtk_Page_Setup_Record'Class;
      Key_File   : Glib.Key_File.G_Key_File;
      Group_Name : String := "";
      Error      : Glib.Error.GError := null)
   is
      function Internal
        (Key_File   : Glib.Key_File.G_Key_File;
         Group_Name : chars_ptr;
         Error      : Glib.Error.GError)
         return System.Address;
      pragma Import (C, Internal, "gtk_page_setup_new_from_key_file");

      G : chars_ptr := String_Or_Null (Group_Name);
   begin
      Set_Object (Widget, Internal (Key_File, G, Error));
      Free (G);
   end Initialize_From_Key_File;

   -----------------------
   -- Set_Bottom_Margin --
   -----------------------

   procedure Set_Bottom_Margin
     (Setup  : access Gtk_Page_Setup_Record;
      Margin : Gdouble;
      Unit   : Gtk.Enums.Gtk_Unit)
   is
      procedure Internal
        (Setup  : System.Address;
         Margin : Gdouble;
         Unit   : Gtk.Enums.Gtk_Unit);
      pragma Import (C, Internal, "gtk_page_setup_set_bottom_margin");
   begin
      Internal (Get_Object (Setup), Margin, Unit);
   end Set_Bottom_Margin;

   ---------------------
   -- Set_Left_Margin --
   ---------------------

   procedure Set_Left_Margin
     (Setup  : access Gtk_Page_Setup_Record;
      Margin : Gdouble;
      Unit   : Gtk.Enums.Gtk_Unit)
   is
      procedure Internal
        (Setup  : System.Address;
         Margin : Gdouble;
         Unit   : Gtk.Enums.Gtk_Unit);
      pragma Import (C, Internal, "gtk_page_setup_set_left_margin");
   begin
      Internal (Get_Object (Setup), Margin, Unit);
   end Set_Left_Margin;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
     (Setup       : access Gtk_Page_Setup_Record;
      Orientation : Gtk.Enums.Gtk_Page_Orientation)
   is
      procedure Internal
        (Setup       : System.Address;
         Orientation : Gtk.Enums.Gtk_Page_Orientation);
      pragma Import (C, Internal, "gtk_page_setup_set_orientation");
   begin
      Internal (Get_Object (Setup), Orientation);
   end Set_Orientation;

   --------------------
   -- Set_Paper_Size --
   --------------------

   procedure Set_Paper_Size
     (Setup : access Gtk_Page_Setup_Record;
      Size  : Gtk.Paper_Size.Gtk_Paper_Size)
   is
      procedure Internal
        (Setup : System.Address;
         Size  : Gtk.Paper_Size.Gtk_Paper_Size);
      pragma Import (C, Internal, "gtk_page_setup_set_paper_size");
   begin
      Internal (Get_Object (Setup), Size);
   end Set_Paper_Size;

   ----------------------------------------
   -- Set_Paper_Size_And_Default_Margins --
   ----------------------------------------

   procedure Set_Paper_Size_And_Default_Margins
     (Setup : access Gtk_Page_Setup_Record;
      Size  : Gtk.Paper_Size.Gtk_Paper_Size)
   is
      procedure Internal
        (Setup : System.Address;
         Size  : Gtk.Paper_Size.Gtk_Paper_Size);
      pragma Import
        (C, Internal, "gtk_page_setup_set_paper_size_and_default_margins");
   begin
      Internal (Get_Object (Setup), Size);
   end Set_Paper_Size_And_Default_Margins;

   ----------------------
   -- Set_Right_Margin --
   ----------------------

   procedure Set_Right_Margin
     (Setup  : access Gtk_Page_Setup_Record;
      Margin : Gdouble;
      Unit   : Gtk.Enums.Gtk_Unit)
   is
      procedure Internal
        (Setup  : System.Address;
         Margin : Gdouble;
         Unit   : Gtk.Enums.Gtk_Unit);
      pragma Import (C, Internal, "gtk_page_setup_set_right_margin");
   begin
      Internal (Get_Object (Setup), Margin, Unit);
   end Set_Right_Margin;

   --------------------
   -- Set_Top_Margin --
   --------------------

   procedure Set_Top_Margin
     (Setup  : access Gtk_Page_Setup_Record;
      Margin : Gdouble;
      Unit   : Gtk.Enums.Gtk_Unit)
   is
      procedure Internal
        (Setup  : System.Address;
         Margin : Gdouble;
         Unit   : Gtk.Enums.Gtk_Unit);
      pragma Import (C, Internal, "gtk_page_setup_set_top_margin");
   begin
      Internal (Get_Object (Setup), Margin, Unit);
   end Set_Top_Margin;

   -------------
   -- To_File --
   -------------

   function To_File
     (Setup     : access Gtk_Page_Setup_Record;
      File_Name : String;
      Error     : Glib.Error.GError := null)
      return Boolean
   is
      function Internal
        (Setup     : System.Address;
         File_Name : String;
         Error     : Glib.Error.GError)
         return Gboolean;
      pragma Import (C, Internal, "gtk_page_setup_to_file");
   begin
      return Boolean'Val (Internal
        (Get_Object (Setup), File_Name & ASCII.NUL, Error));
   end To_File;

   -----------------
   -- To_Key_File --
   -----------------

   procedure To_Key_File
     (Setup      : access Gtk_Page_Setup_Record;
      Key_File   : Glib.Key_File.G_Key_File;
      Group_Name : String := "")
   is
      procedure Internal
        (Setup      : System.Address;
         Key_File   : Glib.Key_File.G_Key_File;
         Group_Name : chars_ptr);
      pragma Import (C, Internal, "gtk_page_setup_to_key_file");
      G : chars_ptr := String_Or_Null (Group_Name);
   begin
      Internal (Get_Object (Setup), Key_File, G);
      Free (G);
   end To_Key_File;

end Gtk.Page_Setup;
