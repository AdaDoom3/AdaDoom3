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

with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Gtkada.Bindings;      use Gtkada.Bindings;

package body Gtk.Print_Settings is

   ----------
   -- Copy --
   ----------

   function Copy
     (Other : access Gtk_Print_Settings_Record) return Gtk_Print_Settings
   is
      function Internal (Other : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_print_settings_copy");
      Stub : Gtk_Print_Settings_Record;
   begin
      return Gtk_Print_Settings
        (Get_User_Data (Internal (Get_Object (Other)), Stub));
   end Copy;

   ---------
   -- Get --
   ---------

   function Get
     (Settings : access Gtk_Print_Settings_Record;
      Key      : String)
      return String
   is
      function Internal
        (Settings : System.Address;
         Key      : String)
         return chars_ptr;
      pragma Import (C, Internal, "gtk_print_settings_get");
      C : chars_ptr;
   begin
      C := Internal (Get_Object (Settings), Key & ASCII.NUL);
      if C = Null_Ptr then
         return "";
      else
         return Value (C);
      end if;
   end Get;

   --------------
   -- Get_Bool --
   --------------

   function Get_Bool
     (Settings : access Gtk_Print_Settings_Record;
      Key      : String)
      return Boolean
   is
      function Internal
        (Settings : System.Address;
         Key      : String)
         return Gboolean;
      pragma Import (C, Internal, "gtk_print_settings_get_bool");
   begin
      return Boolean'Val (Internal (Get_Object (Settings), Key & ASCII.NUL));
   end Get_Bool;

   -----------------
   -- Get_Collate --
   -----------------

   function Get_Collate
     (Settings : access Gtk_Print_Settings_Record) return Boolean
   is
      function Internal (Settings : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_print_settings_get_collate");
   begin
      return Boolean'Val (Internal (Get_Object (Settings)));
   end Get_Collate;

   ------------------------
   -- Get_Default_Source --
   ------------------------

   function Get_Default_Source
     (Settings : access Gtk_Print_Settings_Record) return String
   is
      function Internal (Settings : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_print_settings_get_default_source");
      C : chars_ptr;
   begin
      C := Internal (Get_Object (Settings));
      if C = Null_Ptr then
         return "";
      else
         return Value (C);
      end if;
   end Get_Default_Source;

   ----------------
   -- Get_Dither --
   ----------------

   function Get_Dither
     (Settings : access Gtk_Print_Settings_Record) return String
   is
      function Internal (Settings : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_print_settings_get_dither");
      C : chars_ptr;
   begin
      C := Internal (Get_Object (Settings));
      if C = Null_Ptr then
         return "";
      else
         return Value (C);
      end if;
   end Get_Dither;

   ----------------
   -- Get_Double --
   ----------------

   function Get_Double
     (Settings : access Gtk_Print_Settings_Record;
      Key      : String;
      Def      : Gdouble := 0.0)
      return Gdouble
   is
      function Internal
        (Settings : System.Address;
         Key      : String;
         Def      : Gdouble)
         return Gdouble;
      pragma Import
        (C, Internal, "gtk_print_settings_get_double_with_default");
   begin
      return Internal (Get_Object (Settings), Key & ASCII.NUL, Def);
   end Get_Double;

   ----------------
   -- Get_Duplex --
   ----------------

   function Get_Duplex
     (Settings : access Gtk_Print_Settings_Record)
      return Gtk.Enums.Gtk_Print_Duplex
   is
      function Internal
        (Settings : System.Address) return Gtk.Enums.Gtk_Print_Duplex;
      pragma Import (C, Internal, "gtk_print_settings_get_duplex");
   begin
      return Internal (Get_Object (Settings));
   end Get_Duplex;

   --------------------
   -- Get_Finishings --
   --------------------

   function Get_Finishings
     (Settings : access Gtk_Print_Settings_Record)
      return String
   is
      function Internal (Settings : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_print_settings_get_finishings");
   begin
      return Value (Internal (Get_Object (Settings)));
   end Get_Finishings;

   -------------
   -- Get_Int --
   -------------

   function Get_Int
     (Settings : access Gtk_Print_Settings_Record;
      Key      : String;
      Def      : Gint := 0)
      return Gint
   is
      function Internal
        (Settings : System.Address;
         Key      : String;
         Def      : Gint)
         return Gint;
      pragma Import (C, Internal, "gtk_print_settings_get_int_with_default");
   begin
      return Internal (Get_Object (Settings), Key & ASCII.NUL, Def);
   end Get_Int;

   ----------------
   -- Get_Length --
   ----------------

   function Get_Length
     (Settings : access Gtk_Print_Settings_Record;
      Key      : String;
      Unit     : Gtk.Enums.Gtk_Unit)
      return Gdouble
   is
      function Internal
        (Settings : System.Address;
         Key      : String;
         Unit     : Gtk.Enums.Gtk_Unit)
         return Gdouble;
      pragma Import (C, Internal, "gtk_print_settings_get_length");
   begin
      return Internal (Get_Object (Settings), Key & ASCII.NUL, Unit);
   end Get_Length;

   --------------------
   -- Get_Media_Type --
   --------------------

   function Get_Media_Type
     (Settings : access Gtk_Print_Settings_Record)
      return String
   is
      function Internal (Settings : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_print_settings_get_media_type");
   begin
      return Value (Internal (Get_Object (Settings)));
   end Get_Media_Type;

   ------------------
   -- Get_N_Copies --
   ------------------

   function Get_N_Copies
     (Settings : access Gtk_Print_Settings_Record) return Gint
   is
      function Internal (Settings : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_print_settings_get_n_copies");
   begin
      return Internal (Get_Object (Settings));
   end Get_N_Copies;

   -------------------
   -- Get_Number_Up --
   -------------------

   function Get_Number_Up
     (Settings : access Gtk_Print_Settings_Record) return Gint
   is
      function Internal (Settings : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_print_settings_get_number_up");
   begin
      return Internal (Get_Object (Settings));
   end Get_Number_Up;

   --------------------------
   -- Get_Number_Up_Layout --
   --------------------------

   function Get_Number_Up_Layout
     (Settings : access Gtk_Print_Settings_Record)
      return Gtk.Enums.Gtk_Number_Up_Layout
   is
      function Internal
        (Settings : System.Address) return Gtk.Enums.Gtk_Number_Up_Layout;
      pragma Import (C, Internal, "gtk_print_settings_get_number_up_layout");
   begin
      return Internal (Get_Object (Settings));
   end Get_Number_Up_Layout;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
     (Settings : access Gtk_Print_Settings_Record)
      return Gtk.Enums.Gtk_Page_Orientation
   is
      function Internal
        (Settings : System.Address) return Gtk.Enums.Gtk_Page_Orientation;
      pragma Import (C, Internal, "gtk_print_settings_get_orientation");
   begin
      return Internal (Get_Object (Settings));
   end Get_Orientation;

   --------------------
   -- Get_Output_Bin --
   --------------------

   function Get_Output_Bin
     (Settings : access Gtk_Print_Settings_Record) return String
   is
      function Internal (Settings : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_print_settings_get_output_bin");
   begin
      return Value (Internal (Get_Object (Settings)));
   end Get_Output_Bin;

   ---------------------
   -- Get_Page_Ranges --
   ---------------------

   function Get_Page_Ranges
     (Settings   : access Gtk_Print_Settings_Record)
      return Gtk_Page_Range_Array
   is
      type Internal_Page_Range_Array is
        array (size_t) of Gtk_Page_Range_Record;
      pragma Convention (C, Internal_Page_Range_Array);

      type Internal_Page_Range_Array_Access is
        access Internal_Page_Range_Array;
      pragma Convention (C, Internal_Page_Range_Array_Access);

      procedure Free (Item : in out Internal_Page_Range_Array_Access);
      pragma Import (C, Free, "g_free");

      function Internal
        (Settings   : System.Address;
         Num_Ranges : access Gint)
         return Internal_Page_Range_Array_Access;
      pragma Import (C, Internal, "gtk_print_settings_get_page_ranges");

      Len    : aliased Gint;
      Result : Internal_Page_Range_Array_Access;
   begin
      Result := Internal (Get_Object (Settings), Len'Access);
      declare
         Ranges : constant Gtk_Page_Range_Array :=
           Gtk_Page_Range_Array (Result (0 .. size_t (Len)));
      begin
         Free (Result);
         return Ranges;
      end;
   end Get_Page_Ranges;

   ------------------
   -- Get_Page_Set --
   ------------------

   function Get_Page_Set
     (Settings : access Gtk_Print_Settings_Record)
      return Gtk.Enums.Gtk_Page_Set
   is
      function Internal
        (Settings : System.Address) return Gtk.Enums.Gtk_Page_Set;
      pragma Import (C, Internal, "gtk_print_settings_get_page_set");
   begin
      return Internal (Get_Object (Settings));
   end Get_Page_Set;

   ----------------------
   -- Get_Paper_Height --
   ----------------------

   function Get_Paper_Height
     (Settings : access Gtk_Print_Settings_Record;
      Unit     : Gtk.Enums.Gtk_Unit)
      return Gdouble
   is
      function Internal
        (Settings : System.Address;
         Unit     : Gtk.Enums.Gtk_Unit)
         return Gdouble;
      pragma Import (C, Internal, "gtk_print_settings_get_paper_height");
   begin
      return Internal (Get_Object (Settings), Unit);
   end Get_Paper_Height;

   --------------------
   -- Get_Paper_Size --
   --------------------

   function Get_Paper_Size
     (Settings : access Gtk_Print_Settings_Record)
      return Gtk.Paper_Size.Gtk_Paper_Size
   is
      function Internal (Settings : System.Address)
        return Gtk.Paper_Size.Gtk_Paper_Size;
      pragma Import (C, Internal, "gtk_print_settings_get_paper_size");
   begin
      return Internal (Get_Object (Settings));
   end Get_Paper_Size;

   ---------------------
   -- Get_Paper_Width --
   ---------------------

   function Get_Paper_Width
     (Settings : access Gtk_Print_Settings_Record;
      Unit     : Gtk.Enums.Gtk_Unit)
      return Gdouble
   is
      function Internal
        (Settings : System.Address;
         Unit     : Gtk.Enums.Gtk_Unit)
         return Gdouble;
      pragma Import (C, Internal, "gtk_print_settings_get_paper_width");
   begin
      return Internal (Get_Object (Settings), Unit);
   end Get_Paper_Width;

   ---------------------
   -- Get_Print_Pages --
   ---------------------

   function Get_Print_Pages
     (Settings : access Gtk_Print_Settings_Record)
      return Gtk.Enums.Gtk_Print_Pages
   is
      function Internal
        (Settings : System.Address) return Gtk.Enums.Gtk_Print_Pages;
      pragma Import (C, Internal, "gtk_print_settings_get_print_pages");
   begin
      return Internal (Get_Object (Settings));
   end Get_Print_Pages;

   -----------------
   -- Get_Printer --
   -----------------

   function Get_Printer
     (Settings : access Gtk_Print_Settings_Record) return String
   is
      function Internal (Settings : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_print_settings_get_printer");
   begin
      return Value (Internal (Get_Object (Settings)));
   end Get_Printer;

   ---------------------
   -- Get_Printer_Lpi --
   ---------------------

   function Get_Printer_Lpi
     (Settings : access Gtk_Print_Settings_Record) return Gdouble
   is
      function Internal (Settings : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_print_settings_get_printer_lpi");
   begin
      return Internal (Get_Object (Settings));
   end Get_Printer_Lpi;

   -----------------
   -- Get_Quality --
   -----------------

   function Get_Quality
     (Settings : access Gtk_Print_Settings_Record)
      return Gtk.Enums.Gtk_Print_Quality
   is
      function Internal
        (Settings : System.Address) return Gtk.Enums.Gtk_Print_Quality;
      pragma Import (C, Internal, "gtk_print_settings_get_quality");
   begin
      return Internal (Get_Object (Settings));
   end Get_Quality;

   --------------------
   -- Get_Resolution --
   --------------------

   function Get_Resolution
     (Settings : access Gtk_Print_Settings_Record) return Gint
   is
      function Internal (Settings : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_print_settings_get_resolution");
   begin
      return Internal (Get_Object (Settings));
   end Get_Resolution;

   ----------------------
   -- Get_Resolution_X --
   ----------------------

   function Get_Resolution_X
     (Settings : access Gtk_Print_Settings_Record) return Gint
   is
      function Internal (Settings : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_print_settings_get_resolution_x");
   begin
      return Internal (Get_Object (Settings));
   end Get_Resolution_X;

   ----------------------
   -- Get_Resolution_Y --
   ----------------------

   function Get_Resolution_Y
     (Settings : access Gtk_Print_Settings_Record) return Gint
   is
      function Internal (Settings : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_print_settings_get_resolution_y");
   begin
      return Internal (Get_Object (Settings));
   end Get_Resolution_Y;

   -----------------
   -- Get_Reverse --
   -----------------

   function Get_Reverse
     (Settings : access Gtk_Print_Settings_Record) return Boolean
   is
      function Internal (Settings : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_print_settings_get_reverse");
   begin
      return Boolean'Val (Internal (Get_Object (Settings)));
   end Get_Reverse;

   ---------------
   -- Get_Scale --
   ---------------

   function Get_Scale
     (Settings : access Gtk_Print_Settings_Record) return Gdouble
   is
      function Internal (Settings : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_print_settings_get_scale");
   begin
      return Internal (Get_Object (Settings));
   end Get_Scale;

   -------------------
   -- Get_Use_Color --
   -------------------

   function Get_Use_Color
     (Settings : access Gtk_Print_Settings_Record) return Boolean
   is
      function Internal (Settings : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_print_settings_get_use_color");
   begin
      return Boolean'Val (Internal (Get_Object (Settings)));
   end Get_Use_Color;

   -------------
   -- Has_Key --
   -------------

   function Has_Key
     (Settings : access Gtk_Print_Settings_Record;
      Key      : String)
      return Boolean
   is
      function Internal
        (Settings : System.Address;
         Key      : String)
         return Gboolean;
      pragma Import (C, Internal, "gtk_print_settings_has_key");
   begin
      return Boolean'Val (Internal (Get_Object (Settings), Key & ASCII.NUL));
   end Has_Key;

   ---------------
   -- Load_File --
   ---------------

   function Load_File
     (Settings  : access Gtk_Print_Settings_Record;
      File_Name : String;
      Error     : Glib.Error.GError := null)
      return Boolean
   is
      function Internal
        (Settings  : System.Address;
         File_Name : String;
         Error     : Glib.Error.GError)
         return Gboolean;
      pragma Import (C, Internal, "gtk_print_settings_load_file");
   begin
      return Boolean'Val (Internal
        (Get_Object (Settings), File_Name & ASCII.NUL, Error));
   end Load_File;

   -------------------
   -- Load_Key_File --
   -------------------

   function Load_Key_File
     (Settings   : access Gtk_Print_Settings_Record;
      Key_File   : Glib.Key_File.G_Key_File;
      Group_Name : String := "";
      Error      : Glib.Error.GError := null)
      return Boolean
   is
      function Internal
        (Settings   : System.Address;
         Key_File   : Glib.Key_File.G_Key_File;
         Group_Name : chars_ptr;
         Error      : Glib.Error.GError)
         return Gboolean;
      pragma Import (C, Internal, "gtk_print_settings_load_key_file");

      G : chars_ptr := String_Or_Null (Group_Name);
      Result : Gboolean;
   begin
      Result := Internal (Get_Object (Settings), Key_File, G, Error);
      Free (G);
      return Boolean'Val (Result);
   end Load_Key_File;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Print_Settings) is
   begin
      Widget := new Gtk_Print_Settings_Record;
      Gtk.Print_Settings.Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Print_Settings_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_print_settings_new");
   begin
      Set_Object (Widget, Internal);
   end Initialize;

   -----------------------
   -- Gtk_New_From_File --
   -----------------------

   procedure Gtk_New_From_File
     (Widget    : out Gtk_Print_Settings;
      File_Name : String;
      Error     : Glib.Error.GError := null)
   is
   begin
      Widget := new Gtk_Print_Settings_Record;
      Initialize_From_File (Widget, File_Name, Error);
   end Gtk_New_From_File;

   --------------------------
   -- Initialize_From_File --
   --------------------------

   procedure Initialize_From_File
     (Widget    : access Gtk_Print_Settings_Record'Class;
      File_Name : String;
      Error     : Glib.Error.GError := null)
   is
      function Internal
        (File_Name : String;
         Error     : Glib.Error.GError)
         return System.Address;
      pragma Import (C, Internal, "gtk_print_settings_new_from_file");
   begin
      Set_Object (Widget, Internal (File_Name & ASCII.NUL, Error));
   end Initialize_From_File;

   ---------------------------
   -- Gtk_New_From_Key_File --
   ---------------------------

   procedure Gtk_New_From_Key_File
     (Widget     : out Gtk_Print_Settings;
      Key_File   : Glib.Key_File.G_Key_File;
      Group_Name : String := "";
      Error      : Glib.Error.GError := null)
   is
   begin
      Widget := new Gtk_Print_Settings_Record;
      Initialize_From_Key_File (Widget, Key_File, Group_Name, Error);
   end Gtk_New_From_Key_File;

   ------------------------------
   -- Initialize_From_Key_File --
   ------------------------------

   procedure Initialize_From_Key_File
     (Widget     : access Gtk_Print_Settings_Record'Class;
      Key_File   : Glib.Key_File.G_Key_File;
      Group_Name : String := "";
      Error      : Glib.Error.GError := null)
   is
      function Internal
        (Key_File   : Glib.Key_File.G_Key_File;
         Group_Name : chars_ptr;
         Error      : Glib.Error.GError)
         return System.Address;
      pragma Import (C, Internal, "gtk_print_settings_new_from_key_file");
      G : chars_ptr := String_Or_Null (Group_Name);
   begin
      Set_Object (Widget, Internal (Key_File, G, Error));
      Free (G);
   end Initialize_From_Key_File;

   ---------
   -- Set --
   ---------

   procedure Set
     (Settings : access Gtk_Print_Settings_Record;
      Key      : String;
      Value    : String := "")
   is
      procedure Internal
        (Settings : System.Address;
         Key      : String;
         Value    : chars_ptr);
      pragma Import (C, Internal, "gtk_print_settings_set");
      V : chars_ptr := String_Or_Null (Value);
   begin
      Internal (Get_Object (Settings), Key & ASCII.NUL, V);
      Free (V);
   end Set;

   --------------
   -- Set_Bool --
   --------------

   procedure Set_Bool
     (Settings : access Gtk_Print_Settings_Record;
      Key      : String;
      Value    : Boolean)
   is
      procedure Internal
        (Settings : System.Address;
         Key      : String;
         Value    : Gboolean);
      pragma Import (C, Internal, "gtk_print_settings_set_bool");
   begin
      Internal (Get_Object (Settings), Key & ASCII.NUL, Boolean'Pos (Value));
   end Set_Bool;

   -----------------
   -- Set_Collate --
   -----------------

   procedure Set_Collate
     (Settings : access Gtk_Print_Settings_Record;
      Collate  : Boolean)
   is
      procedure Internal
        (Settings : System.Address;
         Collate  : Gboolean);
      pragma Import (C, Internal, "gtk_print_settings_set_collate");
   begin
      Internal (Get_Object (Settings), Boolean'Pos (Collate));
   end Set_Collate;

   ------------------------
   -- Set_Default_Source --
   ------------------------

   procedure Set_Default_Source
     (Settings       : access Gtk_Print_Settings_Record;
      Default_Source : String)
   is
      procedure Internal
        (Settings       : System.Address;
         Default_Source : String);
      pragma Import (C, Internal, "gtk_print_settings_set_default_source");
   begin
      Internal (Get_Object (Settings), Default_Source & ASCII.NUL);
   end Set_Default_Source;

   ----------------
   -- Set_Dither --
   ----------------

   procedure Set_Dither
     (Settings : access Gtk_Print_Settings_Record;
      Dither   : String)
   is
      procedure Internal
        (Settings : System.Address;
         Dither   : String);
      pragma Import (C, Internal, "gtk_print_settings_set_dither");
   begin
      Internal (Get_Object (Settings), Dither & ASCII.NUL);
   end Set_Dither;

   ----------------
   -- Set_Double --
   ----------------

   procedure Set_Double
     (Settings : access Gtk_Print_Settings_Record;
      Key      : String;
      Value    : Gdouble)
   is
      procedure Internal
        (Settings : System.Address;
         Key      : String;
         Value    : Gdouble);
      pragma Import (C, Internal, "gtk_print_settings_set_double");
   begin
      Internal (Get_Object (Settings), Key & ASCII.NUL, Value);
   end Set_Double;

   ----------------
   -- Set_Duplex --
   ----------------

   procedure Set_Duplex
     (Settings : access Gtk_Print_Settings_Record;
      Duplex   : Gtk.Enums.Gtk_Print_Duplex)
   is
      procedure Internal
        (Settings : System.Address;
         Duplex   : Gtk.Enums.Gtk_Print_Duplex);
      pragma Import (C, Internal, "gtk_print_settings_set_duplex");
   begin
      Internal (Get_Object (Settings), Duplex);
   end Set_Duplex;

   --------------------
   -- Set_Finishings --
   --------------------

   procedure Set_Finishings
     (Settings   : access Gtk_Print_Settings_Record;
      Finishings : String)
   is
      procedure Internal
        (Settings   : System.Address;
         Finishings : String);
      pragma Import (C, Internal, "gtk_print_settings_set_finishings");
   begin
      Internal (Get_Object (Settings), Finishings & ASCII.NUL);
   end Set_Finishings;

   -------------
   -- Set_Int --
   -------------

   procedure Set_Int
     (Settings : access Gtk_Print_Settings_Record;
      Key      : String;
      Value    : Gint)
   is
      procedure Internal
        (Settings : System.Address;
         Key      : String;
         Value    : Gint);
      pragma Import (C, Internal, "gtk_print_settings_set_int");
   begin
      Internal (Get_Object (Settings), Key & ASCII.NUL, Value);
   end Set_Int;

   ----------------
   -- Set_Length --
   ----------------

   procedure Set_Length
     (Settings : access Gtk_Print_Settings_Record;
      Key      : String;
      Value    : Gdouble;
      Unit     : Gtk.Enums.Gtk_Unit)
   is
      procedure Internal
        (Settings : System.Address;
         Key      : String;
         Value    : Gdouble;
         Unit     : Gtk.Enums.Gtk_Unit);
      pragma Import (C, Internal, "gtk_print_settings_set_length");
   begin
      Internal (Get_Object (Settings), Key & ASCII.NUL, Value, Unit);
   end Set_Length;

   --------------------
   -- Set_Media_Type --
   --------------------

   procedure Set_Media_Type
     (Settings   : access Gtk_Print_Settings_Record;
      Media_Type : String)
   is
      procedure Internal
        (Settings   : System.Address;
         Media_Type : String);
      pragma Import (C, Internal, "gtk_print_settings_set_media_type");
   begin
      Internal (Get_Object (Settings), Media_Type & ASCII.NUL);
   end Set_Media_Type;

   ------------------
   -- Set_N_Copies --
   ------------------

   procedure Set_N_Copies
     (Settings   : access Gtk_Print_Settings_Record;
      Num_Copies : Gint)
   is
      procedure Internal
        (Settings   : System.Address;
         Num_Copies : Gint);
      pragma Import (C, Internal, "gtk_print_settings_set_n_copies");
   begin
      Internal (Get_Object (Settings), Num_Copies);
   end Set_N_Copies;

   -------------------
   -- Set_Number_Up --
   -------------------

   procedure Set_Number_Up
     (Settings  : access Gtk_Print_Settings_Record;
      Number_Up : Gint)
   is
      procedure Internal
        (Settings  : System.Address;
         Number_Up : Gint);
      pragma Import (C, Internal, "gtk_print_settings_set_number_up");
   begin
      Internal (Get_Object (Settings), Number_Up);
   end Set_Number_Up;

   --------------------------
   -- Set_Number_Up_Layout --
   --------------------------

   procedure Set_Number_Up_Layout
     (Settings         : access Gtk_Print_Settings_Record;
      Number_Up_Layout : Gtk.Enums.Gtk_Number_Up_Layout)
   is
      procedure Internal
        (Settings         : System.Address;
         Number_Up_Layout : Gtk.Enums.Gtk_Number_Up_Layout);
      pragma Import (C, Internal, "gtk_print_settings_set_number_up_layout");
   begin
      Internal (Get_Object (Settings), Number_Up_Layout);
   end Set_Number_Up_Layout;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
     (Settings    : access Gtk_Print_Settings_Record;
      Orientation : Gtk.Enums.Gtk_Page_Orientation)
   is
      procedure Internal
        (Settings    : System.Address;
         Orientation : Gtk.Enums.Gtk_Page_Orientation);
      pragma Import (C, Internal, "gtk_print_settings_set_orientation");
   begin
      Internal (Get_Object (Settings), Orientation);
   end Set_Orientation;

   --------------------
   -- Set_Output_Bin --
   --------------------

   procedure Set_Output_Bin
     (Settings   : access Gtk_Print_Settings_Record;
      Output_Bin : String)
   is
      procedure Internal
        (Settings   : System.Address;
         Output_Bin : String);
      pragma Import (C, Internal, "gtk_print_settings_set_output_bin");
   begin
      Internal (Get_Object (Settings), Output_Bin & ASCII.NUL);
   end Set_Output_Bin;

   ---------------------
   -- Set_Page_Ranges --
   ---------------------

   procedure Set_Page_Ranges
     (Settings    : access Gtk_Print_Settings_Record;
      Page_Ranges : access Gtk_Page_Range_Array)
   is
      procedure Internal
        (Settings    : System.Address;
         Page_Ranges : System.Address;
         Num_Ranges  : Gint);
      pragma Import (C, Internal, "gtk_print_settings_set_page_ranges");
   begin
      Internal
        (Get_Object (Settings),
         Page_Ranges (Page_Ranges'First)'Address,
         Page_Ranges'Length);
   end Set_Page_Ranges;

   ------------------
   -- Set_Page_Set --
   ------------------

   procedure Set_Page_Set
     (Settings : access Gtk_Print_Settings_Record;
      Page_Set : Gtk.Enums.Gtk_Page_Set)
   is
      procedure Internal
        (Settings : System.Address;
         Page_Set : Gtk.Enums.Gtk_Page_Set);
      pragma Import (C, Internal, "gtk_print_settings_set_page_set");
   begin
      Internal (Get_Object (Settings), Page_Set);
   end Set_Page_Set;

   ----------------------
   -- Set_Paper_Height --
   ----------------------

   procedure Set_Paper_Height
     (Settings : access Gtk_Print_Settings_Record;
      Height   : Gdouble;
      Unit     : Gtk.Enums.Gtk_Unit)
   is
      procedure Internal
        (Settings : System.Address;
         Height   : Gdouble;
         Unit     : Gtk.Enums.Gtk_Unit);
      pragma Import (C, Internal, "gtk_print_settings_set_paper_height");
   begin
      Internal (Get_Object (Settings), Height, Unit);
   end Set_Paper_Height;

   --------------------
   -- Set_Paper_Size --
   --------------------

   procedure Set_Paper_Size
     (Settings   : access Gtk_Print_Settings_Record;
      Paper_Size : Gtk.Paper_Size.Gtk_Paper_Size)
   is
      procedure Internal
        (Settings   : System.Address;
         Paper_Size : Gtk.Paper_Size.Gtk_Paper_Size);
      pragma Import (C, Internal, "gtk_print_settings_set_paper_size");
   begin
      Internal (Get_Object (Settings), Paper_Size);
   end Set_Paper_Size;

   ---------------------
   -- Set_Paper_Width --
   ---------------------

   procedure Set_Paper_Width
     (Settings : access Gtk_Print_Settings_Record;
      Width    : Gdouble;
      Unit     : Gtk.Enums.Gtk_Unit)
   is
      procedure Internal
        (Settings : System.Address;
         Width    : Gdouble;
         Unit     : Gtk.Enums.Gtk_Unit);
      pragma Import (C, Internal, "gtk_print_settings_set_paper_width");
   begin
      Internal (Get_Object (Settings), Width, Unit);
   end Set_Paper_Width;

   ---------------------
   -- Set_Print_Pages --
   ---------------------

   procedure Set_Print_Pages
     (Settings : access Gtk_Print_Settings_Record;
      Pages    : Gtk.Enums.Gtk_Print_Pages)
   is
      procedure Internal
        (Settings : System.Address;
         Pages    : Gtk.Enums.Gtk_Print_Pages);
      pragma Import (C, Internal, "gtk_print_settings_set_print_pages");
   begin
      Internal (Get_Object (Settings), Pages);
   end Set_Print_Pages;

   -----------------
   -- Set_Printer --
   -----------------

   procedure Set_Printer
     (Settings : access Gtk_Print_Settings_Record;
      Printer  : String)
   is
      procedure Internal
        (Settings : System.Address;
         Printer  : String);
      pragma Import (C, Internal, "gtk_print_settings_set_printer");
   begin
      Internal (Get_Object (Settings), Printer & ASCII.NUL);
   end Set_Printer;

   ---------------------
   -- Set_Printer_Lpi --
   ---------------------

   procedure Set_Printer_Lpi
     (Settings : access Gtk_Print_Settings_Record;
      Lpi      : Gdouble)
   is
      procedure Internal
        (Settings : System.Address;
         Lpi      : Gdouble);
      pragma Import (C, Internal, "gtk_print_settings_set_printer_lpi");
   begin
      Internal (Get_Object (Settings), Lpi);
   end Set_Printer_Lpi;

   -----------------
   -- Set_Quality --
   -----------------

   procedure Set_Quality
     (Settings : access Gtk_Print_Settings_Record;
      Quality  : Gtk.Enums.Gtk_Print_Quality)
   is
      procedure Internal
        (Settings : System.Address;
         Quality  : Gtk.Enums.Gtk_Print_Quality);
      pragma Import (C, Internal, "gtk_print_settings_set_quality");
   begin
      Internal (Get_Object (Settings), Quality);
   end Set_Quality;

   --------------------
   -- Set_Resolution --
   --------------------

   procedure Set_Resolution
     (Settings   : access Gtk_Print_Settings_Record;
      Resolution : Gint)
   is
      procedure Internal
        (Settings   : System.Address;
         Resolution : Gint);
      pragma Import (C, Internal, "gtk_print_settings_set_resolution");
   begin
      Internal (Get_Object (Settings), Resolution);
   end Set_Resolution;

   -----------------------
   -- Set_Resolution_XY --
   -----------------------

   procedure Set_Resolution_XY
     (Settings     : access Gtk_Print_Settings_Record;
      Resolution_X : Gint;
      Resolution_Y : Gint)
   is
      procedure Internal
        (Settings     : System.Address;
         Resolution_X : Gint;
         Resolution_Y : Gint);
      pragma Import (C, Internal, "gtk_print_settings_set_resolution_xy");
   begin
      Internal (Get_Object (Settings), Resolution_X, Resolution_Y);
   end Set_Resolution_XY;

   -----------------
   -- Set_Reverse --
   -----------------

   procedure Set_Reverse
     (Settings : access Gtk_Print_Settings_Record;
      Rev      : Boolean)
   is
      procedure Internal
        (Settings : System.Address;
         Rev      : Gboolean);
      pragma Import (C, Internal, "gtk_print_settings_set_reverse");
   begin
      Internal (Get_Object (Settings), Boolean'Pos (Rev));
   end Set_Reverse;

   ---------------
   -- Set_Scale --
   ---------------

   procedure Set_Scale
     (Settings : access Gtk_Print_Settings_Record;
      Scale    : Gdouble)
   is
      procedure Internal
        (Settings : System.Address;
         Scale    : Gdouble);
      pragma Import (C, Internal, "gtk_print_settings_set_scale");
   begin
      Internal (Get_Object (Settings), Scale);
   end Set_Scale;

   -------------------
   -- Set_Use_Color --
   -------------------

   procedure Set_Use_Color
     (Settings  : access Gtk_Print_Settings_Record;
      Use_Color : Boolean)
   is
      procedure Internal
        (Settings  : System.Address;
         Use_Color : Gboolean);
      pragma Import (C, Internal, "gtk_print_settings_set_use_color");
   begin
      Internal (Get_Object (Settings), Boolean'Pos (Use_Color));
   end Set_Use_Color;

   -------------
   -- To_File --
   -------------

   function To_File
     (Settings  : access Gtk_Print_Settings_Record;
      File_Name : String;
      Error     : Glib.Error.GError := null)
      return Boolean
   is
      function Internal
        (Settings  : System.Address;
         File_Name : String;
         Error     : Glib.Error.GError)
         return Gboolean;
      pragma Import (C, Internal, "gtk_print_settings_to_file");
   begin
      return Boolean'Val (Internal
        (Get_Object (Settings), File_Name & ASCII.NUL, Error));
   end To_File;

   -----------------
   -- To_Key_File --
   -----------------

   procedure To_Key_File
     (Settings   : access Gtk_Print_Settings_Record;
      Key_File   : Glib.Key_File.G_Key_File;
      Group_Name : String := "")
   is
      procedure Internal
        (Settings   : System.Address;
         Key_File   : Glib.Key_File.G_Key_File;
         Group_Name : chars_ptr);
      pragma Import (C, Internal, "gtk_print_settings_to_key_file");
      G : chars_ptr := String_Or_Null (Group_Name);
   begin
      Internal (Get_Object (Settings), Key_File, G);
      Free (G);
   end To_Key_File;

   -----------
   -- Unset --
   -----------

   procedure Unset
     (Settings : access Gtk_Print_Settings_Record;
      Key      : String)
   is
      procedure Internal
        (Settings : System.Address;
         Key      : String);
      pragma Import (C, Internal, "gtk_print_settings_unset");
   begin
      Internal (Get_Object (Settings), Key & ASCII.NUL);
   end Unset;

end Gtk.Print_Settings;
