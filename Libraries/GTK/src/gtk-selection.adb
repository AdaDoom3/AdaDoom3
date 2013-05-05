-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
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

with Gtkada.C;             use Gtkada.C;
with Gdk.Pixbuf;           use Gdk.Pixbuf;
with Gdk.Types;            use Gdk.Types;
with Interfaces.C.Strings; use Interfaces.C, Interfaces.C.Strings;
with System;               use System;
with GNAT.Strings;         use GNAT.Strings;

package body Gtk.Selection is

   package Atom_Arrays is new Gtkada.C.Unbounded_Arrays
     (Gdk.Types.Gdk_Atom, Gdk.Types.Gdk_None,
      Natural, Gdk.Types.Gdk_Atom_Array);

   ------------------------
   -- Get_Data_As_String --
   ------------------------

   function Get_Data_As_String
     (Selection : Selection_Data) return String
   is
      function Internal
        (Selection : Selection_Data) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_selection_data_get_data");

   begin
      return Interfaces.C.Strings.Value (Internal (Selection));
   end Get_Data_As_String;

   ----------------
   -- Get_Pixbuf --
   ----------------

   function Get_Pixbuf
     (Selection : Selection_Data) return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal
        (Selection : Selection_Data) return System.Address;
      pragma Import (C, Internal, "gtk_selection_data_get_pixbuf");
   begin
      return Gdk.Pixbuf.Convert (Internal (Selection));
   end Get_Pixbuf;

   ------------------------
   -- Selection_Data_Set --
   ------------------------

   procedure Selection_Data_Set
     (Selection : Selection_Data;
      The_Type  : Gdk.Types.Gdk_Atom;
      Format    : Gint;
      Data      : String) is
   begin
      Selection_Data_Set
        (Selection, The_Type, Format, Data'Address, Data'Length);
   end Selection_Data_Set;

   ---------------------
   -- Target_List_New --
   ---------------------

   function Target_List_New
     (Targets : Target_Entry_Array) return Target_List
   is
      function Internal
        (Targets : System.Address; N_Targets : Guint) return Target_List;
      pragma Import (C, Internal, "gtk_target_list_new");

   begin
      return Internal (Targets'Address, Targets'Length);
   end Target_List_New;

   ---------------------------
   -- Target_List_Add_Table --
   ---------------------------

   procedure Target_List_Add_Table
     (List    : Target_List;
      Targets : Target_Entry_Array)
   is
      procedure Internal
        (List      : Target_List;
         Targets   : System.Address;
         N_Targets : Guint);
      pragma Import (C, Internal, "gtk_target_list_add_table");

   begin
      Internal (List, Targets'Address, Targets'Length);
   end Target_List_Add_Table;

   ----------------------
   -- Target_List_Find --
   ----------------------

   procedure Target_List_Find
     (List   : Target_List;
      Target : Gdk.Types.Gdk_Atom;
      Info   : out Guint;
      Found  : out Boolean)
   is
      function Internal
        (List   : Target_List;
         Target : Gdk.Types.Gdk_Atom;
         Info   : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_target_list_find");

      J : aliased Guint;

   begin
      Found := Boolean'Val (Internal (List, Target, J'Address));
      Info := J;
   end Target_List_Find;

   ---------------
   -- Owner_Set --
   ---------------

   function Owner_Set
     (Widget    : Gtk.Widget.Gtk_Widget;
      Selection : Gdk_Selection := Selection_Primary;
      Time      : Guint32 := 0) return Boolean
   is
      function Internal
        (Widget    : System.Address;
         Selection : Gdk_Selection;
         Time      : Guint32) return Gint;
      pragma Import (C, Internal, "gtk_selection_owner_set");

   begin
      return Boolean'Val (Internal (Get_Object (Widget), Selection, Time));
   end Owner_Set;

   ----------------
   -- Add_Target --
   ----------------

   procedure Add_Target
     (Widget    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Selection : Gdk_Selection;
      Target    : Gdk.Types.Gdk_Atom;
      Info      : Guint)
   is
      procedure Internal
        (Widget    : System.Address;
         Selection : Gdk_Selection;
         Target    : Gdk.Types.Gdk_Atom;
         Info      : Guint);
      pragma Import (C, Internal, "gtk_selection_add_target");

   begin
      Internal (Get_Object (Widget), Selection, Target, Info);
   end Add_Target;

   -----------------
   -- Add_Targets --
   -----------------

   procedure Add_Targets
     (Widget    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Selection : Gdk_Selection;
      Targets   : Target_Entry_Array)
   is
      procedure Internal
        (Widget    : System.Address;
         Selection : Gdk_Selection;
         Targets   : System.Address;
         N_Targets : Guint);
      pragma Import (C, Internal, "gtk_selection_add_targets");

   begin
      Internal (Get_Object (Widget), Selection, Targets'Address,
                Targets'Length);
   end Add_Targets;

   -------------
   -- Convert --
   -------------

   function Convert
     (Widget    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Selection : Gdk_Selection := Selection_Primary;
      Target    : Gdk.Types.Gdk_Atom;
      Time      : Guint32 := 0) return Boolean
   is
      function Internal
        (Widget    : System.Address;
         Selection : Gdk_Selection;
         Target    : Gdk.Types.Gdk_Atom;
         Time      : Guint32) return Gint;
      pragma Import (C, Internal, "gtk_selection_convert");

   begin
      return Boolean'Val (Internal (Get_Object (Widget), Selection,
                                    Target, Time));
   end Convert;

   -------------------
   -- Clear_Targets --
   -------------------

   procedure Clear_Targets
     (Widget    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Selection : Gdk_Selection)
   is
      procedure Internal
        (Widget    : System.Address;
         Selection : Gdk_Selection);
      pragma Import (C, Internal, "gtk_selection_clear_targets");

   begin
      Internal (Get_Object (Widget), Selection);
   end Clear_Targets;

   ----------------
   -- Remove_All --
   ----------------

   procedure Remove_All (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_selection_remove_all");

   begin
      Internal (Get_Object (Widget));
   end Remove_All;

   ----------------
   -- Set_Pixbuf --
   ----------------

   function Set_Pixbuf
     (Selection : Selection_Data;
      Pixbuf    : Gdk_Pixbuf)
      return Boolean
   is
      function Internal
        (Selection : Selection_Data;
         Pixbuf    : System.Address)
         return Gboolean;
      pragma Import (C, Internal, "gtk_selection_data_set_pixbuf");
   begin
      return Boolean'Val (Internal (Selection, Get_Object (Pixbuf)));
   end Set_Pixbuf;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
     (Selection : Selection_Data) return String
   is
      function Internal
        (Selection : Selection_Data) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_selection_data_get_text");
      Val    : chars_ptr := Internal (Selection);
      Result : constant String := Value (Val);
   begin
      Free (Val);
      return Result;
   end Get_Text;

   --------------
   -- Set_Text --
   --------------

   function Set_Text
     (Selection : Selection_Data;
      Str       : UTF8_String)
      return Boolean
   is
      function Internal
        (Selection : Selection_Data;
         Str       : String;
         Len       : Gint)
         return Gboolean;
      pragma Import (C, Internal, "gtk_selection_data_set_text");
   begin
      return Boolean'Val
        (Internal (Selection, Str, Str'Length));
   end Set_Text;

   -----------------
   -- Get_Targets --
   -----------------

   function Get_Targets
     (Selection : Selection_Data) return Gdk.Types.Gdk_Atom_Array
   is
      use Atom_Arrays;
      function Internal
        (Selection : Selection_Data;
         Targets   : access Unbounded_Array_Access;
         N_Atoms   : access Gint) return Gboolean;
      pragma Import (C, Internal, "gtk_selection_data_get_targets");

      Output    : aliased Unbounded_Array_Access;
      N         : aliased Gint;
   begin
      if Internal
        (Selection, Output'Unchecked_Access, N'Unchecked_Access) = 0
      then
         Output := null;
      end if;

      declare
         Result : constant Gdk_Atom_Array := To_Array (Output, Integer (N));
      begin
         G_Free (Output);
         return Result;
      end;
   end Get_Targets;

   ---------------------------
   -- Targets_Include_Image --
   ---------------------------

   function Targets_Include_Image
     (Selection : Selection_Data; Writable : Boolean := True) return Boolean
   is
      function Internal
        (Selection : Selection_Data; Writable : Gboolean) return Gboolean;
      pragma Import (C, Internal, "gtk_selection_data_targets_include_image");
   begin
      return Boolean'Val (Internal (Selection, Boolean'Pos (Writable)));
   end Targets_Include_Image;

   --------------------------
   -- Targets_Include_Text --
   --------------------------

   function Targets_Include_Text (Selection : Selection_Data) return Boolean is
      function Internal (Selection : Selection_Data) return Gboolean;
      pragma Import (C, Internal, "gtk_selection_data_targets_include_text");
   begin
      return Boolean'Val (Internal (Selection));
   end Targets_Include_Text;

   -----------------------------------
   -- Target_List_Add_Image_Targets --
   -----------------------------------

   procedure Target_List_Add_Image_Targets
     (List      : Target_List;
      Info      : Guint;
      Writable  : Boolean)
   is
      procedure Internal
        (List     : Target_List;
         Info     : Guint;
         Writable : Gboolean);
      pragma Import (C, Internal, "gtk_target_list_add_image_targets");
   begin
      Internal (List, Info, Boolean'Pos (Writable));
   end Target_List_Add_Image_Targets;

   --------------
   -- Set_Uris --
   --------------

   function Set_Uris
     (Selection : Selection_Data;
      URIs      : GNAT.Strings.String_List)
      return Boolean
   is
      function Internal
        (Selection : Selection_Data; URIs : chars_ptr_array) return Gboolean;
      pragma Import (C, Internal, "gtk_selection_data_set_uris");

      List : chars_ptr_array (1 .. size_t (URIs'Length) + 1);
      Result : Gboolean;
   begin
      for U in URIs'Range loop
         List (size_t (U - URIs'First) + List'First) :=
           New_String (URIs (U).all);
      end loop;
      List (List'Last) := Null_Ptr;
      Result := Internal (Selection, List);

      Gtkada.Types.Free (List);
      return Boolean'Val (Result);
   end Set_Uris;

   --------------
   -- Get_Uris --
   --------------

   function Get_Uris
     (Selection : Selection_Data)
      return GNAT.Strings.String_List
   is
      function Internal (Selection : Selection_Data) return System.Address;
      pragma Import (C, Internal, "gtk_selection_data_get_uris");

      function Get_Length (S : System.Address) return Integer;
      pragma Import (C, Get_Length, "ada_string_array_length");

      function Get (S : System.Address; Index : Natural) return chars_ptr;
      pragma Import (C, Get, "ada_string_array_get");

      Result : constant System.Address := Internal (Selection);
   begin
      if Result = System.Null_Address then
         return (1 .. 0 => null);
      else
         declare
            Output : String_List (1 .. Get_Length (Result));
            Tmp    : chars_ptr;
         begin
            for L in Output'Range loop
               Tmp := Get (Result, L - Output'First);
               Output (L) := new String'(Value (Tmp));
               Free (Tmp);
            end loop;
            return Output;
         end;
      end if;
   end Get_Uris;

end Gtk.Selection;
