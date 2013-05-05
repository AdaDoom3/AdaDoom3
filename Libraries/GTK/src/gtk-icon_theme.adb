-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                 Copyright (C) 2006-2013, AdaCore                  --
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

with System;                     use System;
with Interfaces.C.Strings;
with GNAT.Strings;               use GNAT.Strings;
with Glib.Error;                 use Glib.Error;
with Glib.Type_Conversion_Hooks;
with Gdk.Pixbuf;                 use Gdk.Pixbuf;
with Gdk.Rectangle;              use Gdk.Rectangle;
with Gdk.Types;                  use Gdk.Types;
with Gtk.Enums;                  use Gtk.Enums;
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.C;                   use Gtkada.C;
with Gtkada.Types;               use Gtkada.Types;

package body Gtk.Icon_Theme is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Icon_Theme_Record);
   pragma Warnings (Off, Type_Conversion);

   package ICS renames Interfaces.C.Strings;

   package Points_Arrays is new Gtkada.C.Unbounded_Arrays
     (Gdk.Types.Gdk_Point, (0, 0), Positive, Gdk.Types.Gdk_Points_Array);

   -----------------------
   -- Get_Attach_Points --
   -----------------------

   function Get_Attach_Points
     (Icon_Info : Gtk_Icon_Info) return Gdk.Types.Gdk_Points_Array
   is
      use Points_Arrays;
      function Internal
        (Icon_Info : Gtk_Icon_Info;
         Result    : access Unbounded_Array_Access;
         N_Points  : access Gint) return Gboolean;
      pragma Import (C, Internal, "gtk_icon_info_get_attach_points");

      R : aliased Unbounded_Array_Access;
      N : aliased Gint;
   begin
      if Internal (Icon_Info, R'Unchecked_Access, N'Unchecked_Access) = 0 then
         R := null;
      end if;

      declare
         Result : constant Gdk_Points_Array := To_Array (R, Integer (N));
      begin
         G_Free (R);
         return Result;
      end;
   end Get_Attach_Points;

   ------------------------
   -- Get_Builtin_Pixbuf --
   ------------------------

   function Get_Builtin_Pixbuf
     (Icon_Info : Gtk_Icon_Info) return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal
        (Icon_Info : Gtk_Icon_Info) return System.Address;
      pragma Import (C, Internal, "gtk_icon_info_get_builtin_pixbuf");

   begin
      return Convert (Internal (Icon_Info));
   end Get_Builtin_Pixbuf;

   ----------------------
   -- Get_Display_Name --
   ----------------------

   function Get_Display_Name (Icon_Info : Gtk_Icon_Info) return String is
      function Internal (Icon_Info : Gtk_Icon_Info) return Chars_Ptr;
      pragma Import (C, Internal, "gtk_icon_info_get_display_name");
   begin
      --  Return value owned by gtk+
      return ICS.Value (Internal (Icon_Info));
   end Get_Display_Name;

   -----------------------
   -- Get_Embedded_Rect --
   -----------------------

   procedure Get_Embedded_Rect
     (Icon_Info              : Gtk_Icon_Info;
      Rectangle              : in out Gdk.Rectangle.Gdk_Rectangle;
      Has_Embedded_Rectangle : out Boolean)
   is
      function Internal
        (Icon_Info : Gtk_Icon_Info;
         Rectangle : access Gdk_Rectangle)
         return Gboolean;
      pragma Import (C, Internal, "gtk_icon_info_get_embedded_rect");
      R : aliased Gdk_Rectangle;
   begin
      Has_Embedded_Rectangle := Boolean'Val
        (Internal (Icon_Info, R'Unchecked_Access));
      Rectangle := R;
   end Get_Embedded_Rect;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename (Icon_Info : Gtk_Icon_Info) return String is
      function Internal (Icon_Info : Gtk_Icon_Info) return Chars_Ptr;
      pragma Import (C, Internal, "gtk_icon_info_get_filename");
   begin
      --  returned value owned by gtk+
      return ICS.Value (Internal (Icon_Info));
   end Get_Filename;

   -------------------------
   -- Set_Raw_Coordinates --
   -------------------------

   procedure Set_Raw_Coordinates
     (Icon_Info       : Gtk_Icon_Info;
      Raw_Coordinates : Boolean)
   is
      procedure Internal
        (Icon_Info       : Gtk_Icon_Info;
         Raw_Coordinates : Gboolean);
      pragma Import (C, Internal, "gtk_icon_info_set_raw_coordinates");
   begin
      Internal (Icon_Info, Boolean'Pos (Raw_Coordinates));
   end Set_Raw_Coordinates;

   ----------------------
   -- Add_Builtin_Icon --
   ----------------------

   procedure Add_Builtin_Icon
     (Icon_Name : String;
      Size      : Gint;
      Pixbuf    : Gdk_Pixbuf)
   is
      procedure Internal
        (Icon_Name : String;
         Size      : Gint;
         Pixbuf    : System.Address);
      pragma Import (C, Internal, "gtk_icon_theme_add_builtin_icon");
   begin
      Internal (Icon_Name & ASCII.NUL, Size, Get_Object (Pixbuf));
   end Add_Builtin_Icon;

   ------------------------
   -- Append_Search_Path --
   ------------------------

   procedure Append_Search_Path
     (Icon_Theme : access Gtk_Icon_Theme_Record;
      Path       : String)
   is
      procedure Internal
        (Icon_Theme : System.Address;
         Path       : String);
      pragma Import (C, Internal, "gtk_icon_theme_append_search_path");
   begin
      Internal (Get_Object (Icon_Theme), Path & ASCII.NUL);
   end Append_Search_Path;

   -----------------
   -- Choose_Icon --
   -----------------

   function Choose_Icon
     (Icon_Theme : access Gtk_Icon_Theme_Record;
      Icon_Names : GNAT.Strings.String_List;
      Size       : Gint;
      Flags      : Gtk_Icon_Lookup_Flags)
      return Gtk_Icon_Info
   is
      function Internal
        (Icon_Theme : System.Address;
         Icon_Names : ICS.chars_ptr_array;
         Size       : Gint;
         Flags      : Gtk_Icon_Lookup_Flags)
         return System.Address;
      pragma Import (C, Internal, "gtk_icon_theme_choose_icon");

      C_Icon_Names : ICS.chars_ptr_array := From_String_List (Icon_Names);
      Retval       : System.Address;
   begin
      Retval := Internal (Get_Object (Icon_Theme), C_Icon_Names, Size, Flags);
      Free (C_Icon_Names);
      return To_Proxy (Retval);
   end Choose_Icon;

   -----------------
   -- Get_Default --
   -----------------

   function Get_Default return Gtk_Icon_Theme
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_icon_theme_get_default");
      Stub : Gtk_Icon_Theme_Record;
   begin
      return Gtk_Icon_Theme (Get_User_Data (Internal, Stub));
   end Get_Default;

   ---------------------------
   -- Get_Example_Icon_Name --
   ---------------------------

   function Get_Example_Icon_Name
     (Icon_Theme : access Gtk_Icon_Theme_Record)
      return String
   is
      function Internal (Icon_Theme : System.Address) return Chars_Ptr;
      pragma Import (C, Internal, "gtk_icon_theme_get_example_icon_name");
      Val : Chars_Ptr := Internal (Get_Object (Icon_Theme));
      Result : constant String := ICS.Value (Val);
   begin
      ICS.Free (Val);
      return Result;
   end Get_Example_Icon_Name;

   --------------------
   -- Get_Icon_Sizes --
   --------------------

   function Get_Icon_Sizes
     (Icon_Theme : access Gtk_Icon_Theme_Record;
      Icon_Name  : String)
      return Gint_Array
   is
      use Gint_Arrays;
      function Internal
        (Icon_Theme : System.Address;
         Icon_Name  : String) return Unbounded_Array_Access;
      pragma Import (C, Internal, "gtk_icon_theme_get_icon_sizes");

      Res    : constant Unbounded_Array_Access := Internal
        (Get_Object (Icon_Theme), Icon_Name & ASCII.NUL);
      Result : constant Gint_Array := To_Gint_Array_Zero_Terminated (Res);
   begin
      G_Free (Res);
      return Result;
   end Get_Icon_Sizes;

   ---------------------
   -- Get_Search_Path --
   ---------------------

   function Get_Search_Path
     (Icon_Theme : access Gtk_Icon_Theme_Record)
      return GNAT.Strings.String_List
   is
      procedure Internal
        (Icon_Theme : System.Address;
         Path       : out chars_ptr_array_access;
         N_Elements : out Gint);
      pragma Import (C, Internal, "gtk_icon_theme_get_search_path");

      P : chars_ptr_array_access;
      N : Gint;
   begin
      Internal (Get_Object (Icon_Theme), P, N);
      declare
         Result : constant GNAT.Strings.String_List :=
           To_String_List (P.all, N);
      begin
         Free (P.all);
         return Result;
      end;
   end Get_Search_Path;

   --------------
   -- Has_Icon --
   --------------

   function Has_Icon
     (Icon_Theme : access Gtk_Icon_Theme_Record;
      Icon_Name  : String)
      return Boolean
   is
      function Internal
        (Icon_Theme : System.Address;
         Icon_Name  : String)
         return Gboolean;
      pragma Import (C, Internal, "gtk_icon_theme_has_icon");
   begin
      return Boolean'Val
        (Internal (Get_Object (Icon_Theme), Icon_Name & ASCII.NUL));
   end Has_Icon;

   ----------------
   -- List_Icons --
   ----------------

   function List_Icons
     (Icon_Theme : access Gtk_Icon_Theme_Record;
      Context    : String := "")
      return Gtk.Enums.String_List.Glist
   is
      function Internal
        (Icon_Theme : System.Address;
         Context    : Chars_Ptr)
         return System.Address;
      pragma Import (C, Internal, "gtk_icon_theme_list_icons");

      C : Chars_Ptr := String_Or_Null (Context);
      L : Gtk.Enums.String_List.Glist;
   begin
      Gtk.Enums.String_List.Set_Object
        (L, Internal (Get_Object (Icon_Theme), C));
      Interfaces.C.Strings.Free (C);
      return L;
   end List_Icons;

   ---------------
   -- Load_Icon --
   ---------------

   function Load_Icon
     (Icon_Theme : access Gtk_Icon_Theme_Record;
      Icon_Name  : String;
      Size       : Gint;
      Flags      : Gtk_Icon_Lookup_Flags)
      return Gdk_Pixbuf
   is
      function Internal
        (Icon_Theme : System.Address;
         Icon_Name  : String;
         Size       : Gint;
         Flags      : Gtk_Icon_Lookup_Flags)
         return System.Address;
      pragma Import (C, Internal, "gtk_icon_theme_load_icon");
   begin
      return Convert
        (Internal
           (Get_Object (Icon_Theme), Icon_Name & ASCII.NUL, Size, Flags));
   end Load_Icon;

   function Load_Icon
     (Icon_Info : Gtk_Icon_Info;
      Error     : Glib.Error.GError_Access := null)
      return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal
        (Icon_Info : Gtk_Icon_Info;
         Error     : Glib.Error.GError_Access := null)
      return System.Address;
      pragma Import (C, Internal, "gtk_icon_info_load_icon");

   begin
      return Convert (Internal (Icon_Info, Error));
   end Load_Icon;

   -----------------
   -- Lookup_Icon --
   -----------------

   function Lookup_Icon
     (Icon_Theme : access Gtk_Icon_Theme_Record;
      Icon_Name  : String;
      Size       : Gint;
      Flags      : Gtk_Icon_Lookup_Flags)
      return Gtk_Icon_Info
   is
      function Internal
        (Icon_Theme : System.Address;
         Icon_Name  : String;
         Size       : Gint;
         Flags      : Gtk_Icon_Lookup_Flags)
         return Gtk_Icon_Info;
      pragma Import (C, Internal, "gtk_icon_theme_lookup_icon");
   begin
      return Internal
        (Get_Object (Icon_Theme), Icon_Name & ASCII.NUL, Size, Flags);
   end Lookup_Icon;

   ---------------------
   -- Lookup_By_Gicon --
   ---------------------

   function Lookup_By_Gicon
     (Icon_Theme : access Gtk_Icon_Theme_Record;
      Icon       : Glib.G_Icon.G_Icon;
      Size       : Gint;
      Flags      : Gtk_Icon_Lookup_Flags)
      return Gtk_Icon_Info
   is
      function Internal
        (Icon_Theme : System.Address;
         Icon       : Glib.G_Icon.G_Icon;
         Size       : Gint;
         Flags      : Gtk_Icon_Lookup_Flags)
         return System.Address;
      pragma Import (C, Internal, "gtk_icon_theme_lookup_by_gicon");
   begin
      return To_Proxy (Internal (Get_Object (Icon_Theme), Icon, Size, Flags));
   end Lookup_By_Gicon;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Theme : out Gtk_Icon_Theme) is
   begin
      Theme := new Gtk_Icon_Theme_Record;
      Gtk.Icon_Theme.Initialize (Theme);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Theme : access Gtk_Icon_Theme_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_icon_theme_new");
   begin
      Set_Object (Theme, Internal);
   end Initialize;

   --------------------
   -- New_For_Pixbuf --
   --------------------

   function New_For_Pixbuf
     (Icon_Theme : access Gtk_Icon_Theme_Record;
      Pixbuf     : Gdk_Pixbuf)
      return Gtk_Icon_Info
   is
      function Internal
        (Icon_Theme : System.Address;
         Pixbuf     : System.Address)
         return System.Address;
      pragma Import (C, Internal, "gtk_icon_info_new_for_pixbuf");
   begin
      return To_Proxy
        (Internal (Get_Object (Icon_Theme), Get_Object (Pixbuf)));
   end New_For_Pixbuf;

   -------------------------
   -- Prepend_Search_Path --
   -------------------------

   procedure Prepend_Search_Path
     (Icon_Theme : access Gtk_Icon_Theme_Record;
      Path       : String)
   is
      procedure Internal
        (Icon_Theme : System.Address;
         Path       : String);
      pragma Import (C, Internal, "gtk_icon_theme_prepend_search_path");
   begin
      Internal (Get_Object (Icon_Theme), Path & ASCII.NUL);
   end Prepend_Search_Path;

   ----------------------
   -- Rescan_If_Needed --
   ----------------------

   function Rescan_If_Needed
     (Icon_Theme : access Gtk_Icon_Theme_Record)
      return Boolean
   is
      function Internal
        (Icon_Theme : System.Address)
         return Gboolean;
      pragma Import (C, Internal, "gtk_icon_theme_rescan_if_needed");
   begin
      return Boolean'Val (Internal (Get_Object (Icon_Theme)));
   end Rescan_If_Needed;

   ----------------------
   -- Set_Custom_Theme --
   ----------------------

   procedure Set_Custom_Theme
     (Icon_Theme : access Gtk_Icon_Theme_Record;
      Theme_Name : String)
   is
      procedure Internal
        (Icon_Theme : System.Address;
         Theme_Name : String);
      pragma Import (C, Internal, "gtk_icon_theme_set_custom_theme");
   begin
      Internal (Get_Object (Icon_Theme), Theme_Name & ASCII.NUL);
   end Set_Custom_Theme;

   ---------------------
   -- Set_Search_Path --
   ---------------------

   procedure Set_Search_Path
     (Icon_Theme : access Gtk_Icon_Theme_Record;
      Path       : GNAT.Strings.String_List)
   is
      procedure Internal
        (Icon_Theme : System.Address;
         Path       : Chars_Ptr_Array;
         N_Elements : Gint);
      pragma Import (C, Internal, "gtk_icon_theme_set_search_path");

      C : Chars_Ptr_Array := From_String_List (Path);
   begin
      Internal (Get_Object (Icon_Theme), C, C'Length);
      Free (C);
   end Set_Search_Path;

end Gtk.Icon_Theme;
