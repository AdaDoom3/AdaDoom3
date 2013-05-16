-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                 Copyright (C) 2004-2013, AdaCore                  --
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

with Gtkada.Bindings;      use Gtkada.Bindings;
with System;               use System;
with Gdk.Pixbuf;           use Gdk.Pixbuf;
with Gtk;                  use Gtk;
with Gtk.Enums;            use Gtk.Enums;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Gtk.Widget;           use Gtk.Widget;

with Glib.Type_Conversion_Hooks;

package body Gtk.Icon_Factory is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Icon_Factory_Record);
   pragma Warnings (Off, Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   function Gtk_New return Gtk_Icon_Set is
      function Internal return Gtk_Icon_Set;
      pragma Import (C, Internal, "gtk_icon_set_new");
   begin
      return Internal;
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   function Gtk_New return Gtk_Icon_Source is
      function Internal return Gtk_Icon_Source;
      pragma Import (C, Internal, "gtk_icon_source_new");
   begin
      return Internal;
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Icon_Factory_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_icon_factory_new");
   begin
      Set_Object (Widget, Internal);
   end Initialize;

   ---------
   -- Add --
   ---------

   procedure Add
     (Factory  : access Gtk_Icon_Factory_Record;
      Stock_Id : String;
      Set      : Gtk_Icon_Set)
   is
      procedure Internal
        (Factory  : System.Address;
         Stock_Id : String;
         Set      : Gtk_Icon_Set);
      pragma Import (C, Internal, "gtk_icon_factory_add");

   begin
      Internal (Get_Object (Factory), Stock_Id & ASCII.NUL, Set);
   end Add;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (Factory  : access Gtk_Icon_Factory_Record;
      Stock_Id : String) return Gtk_Icon_Set
   is
      function Internal
        (Factory  : System.Address;
         Stock_Id : String) return Gtk_Icon_Set;
      pragma Import (C, Internal, "gtk_icon_factory_lookup");

   begin
      return Internal (Get_Object (Factory), Stock_Id & ASCII.NUL);
   end Lookup;

   -----------------
   -- Add_Default --
   -----------------

   procedure Add_Default (Factory : access Gtk_Icon_Factory_Record) is
      procedure Internal (Factory : System.Address);
      pragma Import (C, Internal, "gtk_icon_factory_add_default");
   begin
      Internal (Get_Object (Factory));
   end Add_Default;

   --------------------
   -- Remove_Default --
   --------------------

   procedure Remove_Default (Factory : access Gtk_Icon_Factory_Record) is
      procedure Internal (Factory : System.Address);
      pragma Import (C, Internal, "gtk_icon_factory_remove_default");
   begin
      Internal (Get_Object (Factory));
   end Remove_Default;

   --------------------
   -- Lookup_Default --
   --------------------

   function Lookup_Default (Stock_Id : String) return Gtk_Icon_Set is
      function Internal (Stock_Id : String) return Gtk_Icon_Set;
      pragma Import (C, Internal, "gtk_icon_factory_lookup_default");
   begin
      return Internal (Stock_Id & ASCII.NUL);
   end Lookup_Default;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Icon_Factory) is
   begin
      Widget := new Gtk_Icon_Factory_Record;
      Gtk.Icon_Factory.Initialize (Widget);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   function Gtk_New (Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf) return Gtk_Icon_Set is
      function Internal (Pixbuf : System.Address) return Gtk_Icon_Set;
      pragma Import (C, Internal, "gtk_icon_set_new_from_pixbuf");
   begin
      return Internal (Get_Object (Pixbuf));
   end Gtk_New;

   ----------------
   -- Add_Source --
   ----------------

   procedure Add_Source
     (Set    : Gtk_Icon_Set;
      Source : Gtk_Icon_Source)
   is
      procedure Internal
        (Set    : Gtk_Icon_Set;
         Source : Gtk_Icon_Source);
      pragma Import (C, Internal, "gtk_icon_set_add_source");

   begin
      Internal (Set, Source);
   end Add_Source;

   ----------
   -- Free --
   ----------

   procedure Free (Source : Gtk_Icon_Source) is
      procedure Internal (Source : Gtk_Icon_Source);
      pragma Import (C, Internal, "gtk_icon_source_free");
   begin
      Internal (Source);
   end Free;

   ------------------
   -- Set_Filename --
   ------------------

   procedure Set_Filename
     (Source   : Gtk_Icon_Source;
      Filename : String)
   is
      procedure Internal
        (Source   : Gtk_Icon_Source;
         Filename : String);
      pragma Import (C, Internal, "gtk_icon_source_set_filename");

   begin
      Internal (Source, Filename & ASCII.NUL);
   end Set_Filename;

   -------------------------
   -- Set_Size_Wildcarded --
   -------------------------

   procedure Set_Size_Wildcarded
     (Source     : Gtk_Icon_Source;
      Wildcarded : Boolean)
   is
      procedure Internal (Source : Gtk_Icon_Source; Wildcarded : Gboolean);
      pragma Import (C, Internal, "gtk_icon_source_set_size_wildcarded");
   begin
      Internal (Source, Boolean'Pos (Wildcarded));
   end Set_Size_Wildcarded;

   ---------------
   -- Get_Sizes --
   ---------------

   function Get_Sizes (Icon_Set : Gtk_Icon_Set) return Gint_Array is
      use Gint_Arrays;
      procedure Internal
        (Icon_Set : Gtk_Icon_Set;
         Result   : access Unbounded_Array_Access;
         N_Sizes  : access Gint);
      pragma Import (C, Internal, "gtk_icon_set_get_sizes");

      Count  : aliased Gint;
      Result : aliased Unbounded_Array_Access;
   begin
      Internal (Icon_Set, Result'Access, Count'Access);
      declare
         Output : constant Gint_Array := To_Array (Result, Integer (Count));
      begin
         G_Free (Result);
         return Output;
      end;
   end Get_Sizes;

   ---------------------
   -- Lookup_Icon_Set --
   ---------------------

   function Lookup_Icon_Set
     (Style    : access Gtk.Style.Gtk_Style_Record'Class;
      Stock_Id : String)
      return Gtk_Icon_Set
   is
      function Internal
        (Style    : System.Address;
         Stock_Id : String)
         return Gtk_Icon_Set;
      pragma Import (C, Internal, "gtk_style_lookup_icon_set");
      --  External binding: gtk_style_lookup_icon_set
   begin
      return Internal (Get_Object (Style), Stock_Id & ASCII.NUL);
   end Lookup_Icon_Set;

   -----------------
   -- Render_Icon --
   -----------------

   function Render_Icon
     (Icon_Set  : Gtk_Icon_Set;
      Style     : access Gtk.Style.Gtk_Style_Record'Class;
      Direction : Gtk.Enums.Gtk_Text_Direction;
      State     : Gtk.Enums.Gtk_State_Type;
      Size      : Gtk.Enums.Gtk_Icon_Size;
      Widget    : Gtk.Widget.Gtk_Widget := null;
      Detail    : String := "")
      return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal
        (Icon_Set  : Gtk_Icon_Set;
         Style     : System.Address;
         Direction : Gtk_Text_Direction;
         State     : Gtk_State_Type;
         Size      : Gtk_Icon_Size;
         Widget    : System.Address;
         Detail    : chars_ptr)
         return System.Address;
      pragma Import (C, Internal, "gtk_icon_set_render_icon");
      Str : chars_ptr := String_Or_Null (Detail);
      W   : System.Address := System.Null_Address;
      Result : Gdk_Pixbuf;

   begin
      if Widget /= null then
         W := Get_Object (Widget);
      end if;

      Result := Convert
        (Internal
           (Icon_Set, Get_Object (Style), Direction, State, Size, W, Str));
      Free (Str);

      return Result;
   end Render_Icon;

   -----------------
   -- Render_Icon --
   -----------------

   function Render_Icon
     (Style     : access Gtk.Style.Gtk_Style_Record'Class;
      Source    : Gtk_Icon_Source;
      Direction : Gtk_Text_Direction;
      State     : Gtk_State_Type;
      Size      : Gtk_Icon_Size;
      Widget    : Gtk_Widget := null;
      Detail    : String := "")
      return Gdk_Pixbuf
   is
      function Internal
        (Style     : System.Address;
         Source    : Gtk_Icon_Source;
         Direction : Gtk_Text_Direction;
         State     : Gtk_State_Type;
         Size      : Gtk_Icon_Size;
         Widget    : System.Address;
         Detail    : chars_ptr)
         return System.Address;
      pragma Import (C, Internal, "gtk_style_render_icon");
      --  External binding: gtk_style_render_icon

      Str    : chars_ptr := String_Or_Null (Detail);
      W      : System.Address := System.Null_Address;
      Result : Gdk_Pixbuf;

   begin
      if Widget /= null then
         W := Get_Object (Widget);
      end if;

      Result := Convert
        (Internal
           (Get_Object (Style), Source, Direction, State, Size, W, Str));
      Free (Str);

      return Result;
   end Render_Icon;

   -------------------------
   -- Icon_Size_From_Name --
   -------------------------

   function Icon_Size_From_Name (Name : String) return Gtk_Icon_Size is
      function Internal (Name : String) return Gtk_Icon_Size;
      pragma Import (C, Internal, "gtk_icon_size_from_name");
   begin
      return Internal (Name & ASCII.NUL);
   end Icon_Size_From_Name;

   ------------------------
   -- Icon_Size_Get_Name --
   ------------------------

   function Icon_Size_Get_Name (Size : Gtk_Icon_Size) return String is
      function Internal
        (Size : Gtk_Icon_Size) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_icon_size_get_name");
   begin
      return Value (Internal (Size));
   end Icon_Size_Get_Name;

   ----------------------
   -- Icon_Size_Lookup --
   ----------------------

   procedure Icon_Size_Lookup
     (Size   : Gtk_Icon_Size;
      Width  : out Gint;
      Height : out Gint)
   is
      function Internal
        (Size   : Gtk_Icon_Size;
         Width  : access Gint;
         Height : access Gint) return Gboolean;
      pragma Import (C, Internal, "gtk_icon_size_lookup");
      W, H : aliased Gint;
      Tmp  : Gboolean;
      pragma Unreferenced (Tmp);
   begin
      Tmp := Internal (Size, W'Access, H'Access);
      Width  := W;
      Height := H;
   end Icon_Size_Lookup;

   -----------------------------------
   -- Icon_Size_Lookup_For_Settings --
   -----------------------------------

   procedure Icon_Size_Lookup_For_Settings
     (Settings : access Gtk.Settings.Gtk_Settings_Record'Class;
      Size     : Gtk_Icon_Size;
      Width    : out Gint;
      Height   : out Gint)
   is
      function Internal
        (Settings : System.Address;
         Size     : Gtk_Icon_Size;
         Width    : access Gint;
         Height   : access Gint)
         return Gboolean;
      pragma Import (C, Internal, "gtk_icon_size_lookup_for_settings");
      W, H : aliased Gint;
      Tmp  : Gboolean;
      pragma Unreferenced (Tmp);
   begin
      Tmp := Internal (Get_Object (Settings), Size, W'Access, H'Access);
      Width  := W;
      Height := H;
   end Icon_Size_Lookup_For_Settings;

   ------------------------
   -- Icon_Size_Register --
   ------------------------

   function Icon_Size_Register
     (Name   : String;
      Width  : Gint;
      Height : Gint)
      return Gtk_Icon_Size
   is
      function Internal
        (Name   : String;
         Width  : Gint;
         Height : Gint)
         return Gtk_Icon_Size;
      pragma Import (C, Internal, "gtk_icon_size_register");
   begin
      return Internal (Name & ASCII.NUL, Width, Height);
   end Icon_Size_Register;

   ------------------------------
   -- Icon_Size_Register_Alias --
   ------------------------------

   procedure Icon_Size_Register_Alias
     (Alias  : String;
      Target : Gtk_Icon_Size)
   is
      procedure Internal
        (Alias  : String;
         Target : Gtk_Icon_Size);
      pragma Import (C, Internal, "gtk_icon_size_register_alias");
   begin
      Internal (Alias & ASCII.NUL, Target);
   end Icon_Size_Register_Alias;

   --------------------------
   -- Set_State_Wildcarded --
   --------------------------

   procedure Set_State_Wildcarded
     (Source : Gtk_Icon_Source; Setting : Boolean)
   is
      procedure Internal (Source : Gtk_Icon_Source; Setting : Gboolean);
      pragma Import (C, Internal, "gtk_icon_source_set_state_wildcarded");
   begin
      Internal (Source, Boolean'Pos (Setting));
   end Set_State_Wildcarded;

   ------------------------------
   -- Set_Direction_Wildcarded --
   ------------------------------

   procedure Set_Direction_Wildcarded
     (Source  : Gtk_Icon_Source; Setting : Boolean)
   is
      procedure Internal (Source : Gtk_Icon_Source;  Setting : Gboolean);
      pragma Import (C, Internal, "gtk_icon_source_set_direction_wildcarded");
   begin
      Internal (Source, Boolean'Pos (Setting));
   end Set_Direction_Wildcarded;

   --------------------------
   -- Get_State_Wildcarded --
   --------------------------

   function Get_State_Wildcarded
     (Source : Gtk_Icon_Source) return Boolean
   is
      function Internal (Source : Gtk_Icon_Source) return Gboolean;
      pragma Import (C, Internal, "gtk_icon_source_get_state_wildcarded");
   begin
      return Boolean'Val (Internal (Source));
   end Get_State_Wildcarded;

   ------------------------------
   -- Get_Direction_Wildcarded --
   ------------------------------

   function Get_Direction_Wildcarded
     (Source : Gtk_Icon_Source) return Boolean
   is
      function Internal (Source : Gtk_Icon_Source) return Gboolean;
      pragma Import (C, Internal, "gtk_icon_source_get_direction_wildcarded");
   begin
      return Boolean'Val (Internal (Source));
   end Get_Direction_Wildcarded;

   ----------------
   -- Get_Pixbuf --
   ----------------

   function Get_Pixbuf
     (Source : Gtk_Icon_Source) return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal (Source : Gtk_Icon_Source) return System.Address;
      pragma Import (C, Internal, "gtk_icon_source_get_pixbuf");

   begin
      return Convert (Internal (Source));
   end Get_Pixbuf;

   -------------------------
   -- Get_Size_Wildcarded --
   -------------------------

   function Get_Size_Wildcarded (Source : Gtk_Icon_Source) return Boolean is
      function Internal (Source : Gtk_Icon_Source) return Gboolean;
      pragma Import (C, Internal, "gtk_icon_source_get_size_wildcarded");
   begin
      return Boolean'Val (Internal (Source));
   end Get_Size_Wildcarded;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename (Source : Gtk_Icon_Source) return String is
      function Internal (Source : Gtk_Icon_Source)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_icon_source_get_filename");
      --  Return value must not be freed.
   begin
      return Value (Internal (Source));
   end Get_Filename;

   -------------------
   -- Get_Icon_Name --
   -------------------

   function Get_Icon_Name (Source : Gtk_Icon_Source) return String is
      function Internal
        (Source : Gtk_Icon_Source) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_icon_source_get_icon_name");
      --  Return value must not be freed
   begin
      return Value (Internal (Source));
   end Get_Icon_Name;

   -------------------
   -- Set_Icon_Name --
   -------------------

   procedure Set_Icon_Name
     (Source : Gtk_Icon_Source; Icon_Name : String)
   is
      procedure Internal
        (Source    : Gtk_Icon_Source;
         Icon_Name : String);
      pragma Import (C, Internal, "gtk_icon_source_set_icon_name");
   begin
      Internal (Source, Icon_Name & ASCII.NUL);
   end Set_Icon_Name;

   ----------------
   -- Set_Pixbuf --
   ----------------

   procedure Set_Pixbuf
     (Source : Gtk_Icon_Source; Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf)
   is
      procedure Internal (Source : Gtk_Icon_Source; Pixbuf : System.Address);
      pragma Import (C, Internal, "gtk_icon_source_set_pixbuf");

   begin
      Internal (Source, Get_Object (Pixbuf));
   end Set_Pixbuf;

end Gtk.Icon_Factory;
