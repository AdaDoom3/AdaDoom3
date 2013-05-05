-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2006-2013, AdaCore                   --
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

with Gtk.File_Filter;      use Gtk.File_Filter;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Gtk.File_Chooser is

   ----------------
   -- Add_Filter --
   ----------------

   procedure Add_Filter
     (Chooser : Gtk_File_Chooser;
      Filter  : access Gtk.File_Filter.Gtk_File_Filter_Record'Class)
   is
      procedure Internal
        (Chooser : Gtk_File_Chooser;
         Filter  : System.Address);
      pragma Import (C, Internal, "gtk_file_chooser_add_filter");
   begin
      Internal (Chooser, Get_Object (Filter));
   end Add_Filter;

   -------------------------
   -- Add_Shortcut_Folder --
   -------------------------

   function Add_Shortcut_Folder
     (Chooser : Gtk_File_Chooser;
      Folder  : String) return Glib.Error.GError
   is
      function Internal
        (Chooser : Gtk_File_Chooser;
         Folder  : String;
         Error   : access Glib.Error.GError)
         return Gboolean;
      pragma Import (C, Internal, "gtk_file_chooser_add_shortcut_folder");
      Err : aliased Glib.Error.GError;
   begin
      if Internal (Chooser, Folder & ASCII.NUL, Err'Unchecked_Access) = 0 then
         return Err;
      else
         return null;
      end if;
   end Add_Shortcut_Folder;

   -----------------------------
   -- Add_Shortcut_Folder_Uri --
   -----------------------------

   function Add_Shortcut_Folder_Uri
     (Chooser : Gtk_File_Chooser;
      Uri     : String) return Glib.Error.GError
   is
      function Internal
        (Chooser : Gtk_File_Chooser;
         Folder  : String;
         Error   : access Glib.Error.GError)
         return Gboolean;
      pragma Import (C, Internal, "gtk_file_chooser_add_shortcut_folder_uri");
      Err : aliased Glib.Error.GError;
   begin
      if Internal (Chooser, Uri & ASCII.NUL, Err'Unchecked_Access) = 0 then
         return Err;
      else
         return null;
      end if;
   end Add_Shortcut_Folder_Uri;

   -----------------------------------
   -- Get_Do_Overwrite_Confirmation --
   -----------------------------------

   function Get_Do_Overwrite_Confirmation
     (Chooser : Gtk_File_Chooser) return Boolean
   is
      function Internal (Chooser : Gtk_File_Chooser) return Gboolean;
      pragma Import
        (C, Internal, "gtk_file_chooser_get_do_overwrite_confirmation");
   begin
      return Boolean'Val (Internal (Chooser));
   end Get_Do_Overwrite_Confirmation;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename (Chooser : Gtk_File_Chooser) return String is
      function Internal
        (Chooser : Gtk_File_Chooser) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_file_chooser_get_filename");

      Result : chars_ptr := Internal (Chooser);
   begin
      if Result = Null_Ptr then
         return "";
      else
         declare
            File   : constant String := Value (Result);
         begin
            Free (Result);
            return File;
         end;
      end if;
   end Get_Filename;

   -------------------
   -- Get_Filenames --
   -------------------

   function Get_Filenames
     (Chooser : Gtk_File_Chooser) return Gtk.Enums.String_SList.GSlist
   is
      function Internal (Chooser : Gtk_File_Chooser) return System.Address;
      pragma Import (C, Internal, "gtk_file_chooser_get_filenames");
      L : Gtk.Enums.String_SList.GSlist;
   begin
      Gtk.Enums.String_SList.Set_Object (L, Internal (Chooser));
      return L;
   end Get_Filenames;

   ------------------------
   -- Get_Current_Folder --
   ------------------------

   function Get_Current_Folder (Chooser : Gtk_File_Chooser) return String is
      function Internal (Chooser : Gtk_File_Chooser) return chars_ptr;
      pragma Import (C, Internal, "gtk_file_chooser_get_current_folder");

      Result : chars_ptr := Internal (Chooser);
   begin
      if Result = Null_Ptr then
         return "";
      else
         declare
            File : constant String := Value (Result);
         begin
            Free (Result);
            return File;
         end;
      end if;
   end Get_Current_Folder;

   ----------------------------
   -- Get_Current_Folder_Uri --
   ----------------------------

   function Get_Current_Folder_Uri
     (Chooser : Gtk_File_Chooser) return String
   is
      function Internal (Chooser : Gtk_File_Chooser) return chars_ptr;
      pragma Import (C, Internal, "gtk_file_chooser_get_current_folder_uri");

      Result : chars_ptr := Internal (Chooser);
   begin
      if Result = Null_Ptr then
         return "";
      else
         declare
            Uri : constant String := Value (Result);
         begin
            Free (Result);
            return Uri;
         end;
      end if;
   end Get_Current_Folder_Uri;

   ----------------------
   -- Get_Extra_Widget --
   ----------------------

   function Get_Extra_Widget (Chooser : Gtk_File_Chooser)
      return Gtk.Widget.Gtk_Widget
   is
      function Internal (Chooser : Gtk_File_Chooser) return System.Address;
      pragma Import (C, Internal, "gtk_file_chooser_get_extra_widget");
      Stub : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Chooser), Stub));
   end Get_Extra_Widget;

   ----------------
   -- Get_Filter --
   ----------------

   function Get_Filter
     (Chooser : Gtk_File_Chooser)
      return Gtk_File_Filter
   is
      function Internal (Chooser : Gtk_File_Chooser) return System.Address;
      pragma Import (C, Internal, "gtk_file_chooser_get_filter");
      Stub : Gtk_File_Filter_Record;
   begin
      return Gtk_File_Filter (Get_User_Data (Internal (Chooser), Stub));
   end Get_Filter;

   --------------------
   -- Get_Local_Only --
   --------------------

   function Get_Local_Only (Chooser : Gtk_File_Chooser) return Boolean is
      function Internal (Chooser : Gtk_File_Chooser) return Gboolean;
      pragma Import (C, Internal, "gtk_file_chooser_get_local_only");
   begin
      return Boolean'Val (Internal (Chooser));
   end Get_Local_Only;

   --------------------------
   -- Get_Preview_Filename --
   --------------------------

   function Get_Preview_Filename (Chooser : Gtk_File_Chooser) return String
   is
      function Internal (Chooser : Gtk_File_Chooser) return chars_ptr;
      pragma Import (C, Internal, "gtk_file_chooser_get_preview_filename");

      Result : chars_ptr := Internal (Chooser);
   begin
      if Result = Null_Ptr then
         return "";
      else
         declare
            File : constant String := Value (Result);
         begin
            Free (Result);
            return File;
         end;
      end if;
   end Get_Preview_Filename;

   ---------------------
   -- Get_Preview_Uri --
   ---------------------

   function Get_Preview_Uri (Chooser : Gtk_File_Chooser) return String is
      function Internal (Chooser : Gtk_File_Chooser) return chars_ptr;
      pragma Import (C, Internal, "gtk_file_chooser_get_preview_uri");

      Result : chars_ptr := Internal (Chooser);
   begin
      if Result = Null_Ptr then
         return "";
      else
         declare
            Uri : constant String := Value (Result);
         begin
            Free (Result);
            return Uri;
         end;
      end if;
   end Get_Preview_Uri;

   ------------------------
   -- Get_Preview_Widget --
   ------------------------

   function Get_Preview_Widget
     (Chooser : Gtk_File_Chooser) return Gtk.Widget.Gtk_Widget
   is
      function Internal (Chooser : Gtk_File_Chooser) return System.Address;
      pragma Import (C, Internal, "gtk_file_chooser_get_preview_widget");
      Stub : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Chooser), Stub));
   end Get_Preview_Widget;

   -------------------------------
   -- Get_Preview_Widget_Active --
   -------------------------------

   function Get_Preview_Widget_Active
     (Chooser : Gtk_File_Chooser) return Boolean
   is
      function Internal (Chooser : Gtk_File_Chooser) return Gboolean;
      pragma Import
        (C, Internal, "gtk_file_chooser_get_preview_widget_active");
   begin
      return Boolean'Val (Internal (Chooser));
   end Get_Preview_Widget_Active;

   -------------------------
   -- Get_Select_Multiple --
   -------------------------

   function Get_Select_Multiple (Chooser : Gtk_File_Chooser) return Boolean is
      function Internal (Chooser : Gtk_File_Chooser) return Gboolean;
      pragma Import (C, Internal, "gtk_file_chooser_get_select_multiple");
   begin
      return Boolean'Val (Internal (Chooser));
   end Get_Select_Multiple;

   ---------------------
   -- Get_Show_Hidden --
   ---------------------

   function Get_Show_Hidden (Chooser : Gtk_File_Chooser) return Boolean is
      function Internal (Chooser : Gtk_File_Chooser) return Gboolean;
      pragma Import (C, Internal, "gtk_file_chooser_get_show_hidden");
   begin
      return Boolean'Val (Internal (Chooser));
   end Get_Show_Hidden;

   -------------
   -- Get_Uri --
   -------------

   function Get_Uri (Chooser : Gtk_File_Chooser) return String
   is
      function Internal (Chooser : Gtk_File_Chooser) return chars_ptr;
      pragma Import (C, Internal, "gtk_file_chooser_get_uri");

      Result : chars_ptr := Internal (Chooser);
   begin
      if Result = Null_Ptr then
         return "";
      else
         declare
            Uri : constant String := Value (Result);
         begin
            Free (Result);
            return Uri;
         end;
      end if;
   end Get_Uri;

   --------------
   -- Get_Uris --
   --------------

   function Get_Uris
     (Chooser : Gtk_File_Chooser) return Gtk.Enums.String_SList.GSlist
   is
      function Internal (Chooser : Gtk_File_Chooser) return System.Address;
      pragma Import (C, Internal, "gtk_file_chooser_get_uris");
      L : Gtk.Enums.String_SList.GSlist;
   begin
      Gtk.Enums.String_SList.Set_Object (L, Internal (Chooser));
      return L;
   end Get_Uris;

   ---------------------------
   -- Get_Use_Preview_Label --
   ---------------------------

   function Get_Use_Preview_Label
     (Chooser : Gtk_File_Chooser) return Boolean
   is
      function Internal (Chooser : Gtk_File_Chooser) return Gboolean;
      pragma Import (C, Internal, "gtk_file_chooser_get_use_preview_label");
   begin
      return Boolean'Val (Internal (Chooser));
   end Get_Use_Preview_Label;

   ------------------
   -- List_Filters --
   ------------------

   function List_Filters
     (Chooser : Gtk_File_Chooser)
      return Glib.Object.Object_List.GSlist
   is
      function Internal (Chooser : Gtk_File_Chooser) return System.Address;
      pragma Import (C, Internal, "gtk_file_chooser_list_filters");
      L : Glib.Object.Object_List.GSlist;
   begin
      Glib.Object.Object_List.Set_Object (L, Internal (Chooser));
      return L;
   end List_Filters;

   ---------------------------
   -- List_Shortcut_Folders --
   ---------------------------

   function List_Shortcut_Folders
     (Chooser : Gtk_File_Chooser) return Gtk.Enums.String_SList.GSlist
   is
      function Internal (Chooser : Gtk_File_Chooser) return System.Address;
      pragma Import (C, Internal, "gtk_file_chooser_list_shortcut_folders");
      L : Gtk.Enums.String_SList.GSlist;
   begin
      Gtk.Enums.String_SList.Set_Object (L, Internal (Chooser));
      return L;
   end List_Shortcut_Folders;

   -------------------------------
   -- List_Shortcut_Folder_Uris --
   -------------------------------

   function List_Shortcut_Folder_Uris
     (Chooser : Gtk_File_Chooser) return Gtk.Enums.String_SList.GSlist
   is
      function Internal (Chooser : Gtk_File_Chooser) return System.Address;
      pragma Import
        (C, Internal, "gtk_file_chooser_list_shortcut_folder_uris");
      L : Gtk.Enums.String_SList.GSlist;
   begin
      Gtk.Enums.String_SList.Set_Object (L, Internal (Chooser));
      return L;
   end List_Shortcut_Folder_Uris;

   -------------------
   -- Remove_Filter --
   -------------------

   procedure Remove_Filter
     (Chooser : Gtk_File_Chooser;
      Filter  : access Gtk_File_Filter_Record'Class)
   is
      procedure Internal
        (Chooser : Gtk_File_Chooser;
         Filter  : System.Address);
      pragma Import (C, Internal, "gtk_file_chooser_remove_filter");
   begin
      Internal (Chooser, Get_Object (Filter));
   end Remove_Filter;

   ----------------------------
   -- Remove_Shortcut_Folder --
   ----------------------------

   function Remove_Shortcut_Folder
     (Chooser : Gtk_File_Chooser;
      Folder  : String)
      return Glib.Error.GError
   is
      function Internal
        (Chooser : Gtk_File_Chooser;
         Folder  : String;
         Error   : access Glib.Error.GError)
         return Gboolean;
      pragma Import (C, Internal, "gtk_file_chooser_remove_shortcut_folder");

      Err : aliased Glib.Error.GError;
   begin
      if Internal (Chooser, Folder & ASCII.NUL, Err'Unchecked_Access) = 0 then
         return Err;
      else
         return null;
      end if;
   end Remove_Shortcut_Folder;

   --------------------------------
   -- Remove_Shortcut_Folder_Uri --
   --------------------------------

   function Remove_Shortcut_Folder_Uri
     (Chooser : Gtk_File_Chooser;
      Uri     : String)
      return Glib.Error.GError
   is
      function Internal
        (Chooser : Gtk_File_Chooser;
         Uri     : String;
         Error   : access Glib.Error.GError)
         return Gboolean;
      pragma Import
        (C, Internal, "gtk_file_chooser_remove_shortcut_folder_uri");

      Err : aliased Glib.Error.GError;
   begin
      if Internal (Chooser, Uri & ASCII.NUL, Err'Unchecked_Access) = 0 then
         return Err;
      else
         return null;
      end if;
   end Remove_Shortcut_Folder_Uri;

   ---------------------
   -- Select_Filename --
   ---------------------

   function Select_Filename
     (Chooser  : Gtk_File_Chooser; Filename : String) return Boolean
   is
      function Internal
        (Chooser  : Gtk_File_Chooser; Filename : String) return Gboolean;
      pragma Import (C, Internal, "gtk_file_chooser_select_filename");
   begin
      return Boolean'Val (Internal (Chooser, Filename & ASCII.NUL));
   end Select_Filename;

   ----------------
   -- Select_Uri --
   ----------------

   function Select_Uri
     (Chooser : Gtk_File_Chooser; Uri : String) return Boolean
   is
      function Internal
        (Chooser : Gtk_File_Chooser; Uri : String) return Gboolean;
      pragma Import (C, Internal, "gtk_file_chooser_select_uri");
   begin
      return Boolean'Val (Internal (Chooser, Uri & ASCII.NUL));
   end Select_Uri;

   ------------------------
   -- Set_Current_Folder --
   ------------------------

   function Set_Current_Folder
     (Chooser  : Gtk_File_Chooser; Filename : String) return Boolean
   is
      function Internal
        (Chooser  : Gtk_File_Chooser; Filename : String) return Gboolean;
      pragma Import (C, Internal, "gtk_file_chooser_set_current_folder");
   begin
      return Boolean'Val (Internal (Chooser, Filename & ASCII.NUL));
   end Set_Current_Folder;

   ----------------------------
   -- Set_Current_Folder_Uri --
   ----------------------------

   function Set_Current_Folder_Uri
     (Chooser : Gtk_File_Chooser; Uri : String) return Boolean
   is
      function Internal
        (Chooser : Gtk_File_Chooser; Uri : String) return Gboolean;
      pragma Import (C, Internal, "gtk_file_chooser_set_current_folder_uri");
   begin
      return Boolean'Val (Internal (Chooser, Uri & ASCII.NUL));
   end Set_Current_Folder_Uri;

   ----------------------
   -- Set_Current_Name --
   ----------------------

   procedure Set_Current_Name (Chooser : Gtk_File_Chooser; Name : String) is
      procedure Internal (Chooser : Gtk_File_Chooser; Name : String);
      pragma Import (C, Internal, "gtk_file_chooser_set_current_name");
   begin
      Internal (Chooser, Name & ASCII.NUL);
   end Set_Current_Name;

   -----------------------------------
   -- Set_Do_Overwrite_Confirmation --
   -----------------------------------

   procedure Set_Do_Overwrite_Confirmation
     (Chooser                   : Gtk_File_Chooser;
      Do_Overwrite_Confirmation : Boolean)
   is
      procedure Internal
        (Chooser                   : Gtk_File_Chooser;
         Do_Overwrite_Confirmation : Gboolean);
      pragma Import
        (C, Internal, "gtk_file_chooser_set_do_overwrite_confirmation");
   begin
      Internal (Chooser, Boolean'Pos (Do_Overwrite_Confirmation));
   end Set_Do_Overwrite_Confirmation;

   ----------------------
   -- Set_Extra_Widget --
   ----------------------

   procedure Set_Extra_Widget
     (Chooser      : Gtk_File_Chooser;
      Extra_Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
        (Chooser      : Gtk_File_Chooser;
         Extra_Widget : System.Address);
      pragma Import (C, Internal, "gtk_file_chooser_set_extra_widget");
   begin
      Internal (Chooser, Get_Object (Extra_Widget));
   end Set_Extra_Widget;

   ------------------
   -- Set_Filename --
   ------------------

   function Set_Filename
     (Chooser  : Gtk_File_Chooser; Filename : String) return Boolean
   is
      function Internal
        (Chooser  : Gtk_File_Chooser; Filename : String) return Gboolean;
      pragma Import (C, Internal, "gtk_file_chooser_set_filename");
   begin
      return Boolean'Val (Internal (Chooser, Filename & ASCII.NUL));
   end Set_Filename;

   ----------------
   -- Set_Filter --
   ----------------

   procedure Set_Filter
     (Chooser : Gtk_File_Chooser;
      Filter  : access Gtk_File_Filter_Record'Class)
   is
      procedure Internal
        (Chooser : Gtk_File_Chooser;
         Filter  : System.Address);
      pragma Import (C, Internal, "gtk_file_chooser_set_filter");
   begin
      Internal (Chooser, Get_Object (Filter));
   end Set_Filter;

   --------------------
   -- Set_Local_Only --
   --------------------

   procedure Set_Local_Only
     (Chooser : Gtk_File_Chooser; Local_Only : Boolean := True)
   is
      procedure Internal (Chooser : Gtk_File_Chooser; Local_Only : Gboolean);
      pragma Import (C, Internal, "gtk_file_chooser_set_local_only");
   begin
      Internal (Chooser, Boolean'Pos (Local_Only));
   end Set_Local_Only;

   ------------------------
   -- Set_Preview_Widget --
   ------------------------

   procedure Set_Preview_Widget
     (Chooser        : Gtk_File_Chooser;
      Preview_Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
        (Chooser        : Gtk_File_Chooser;
         Preview_Widget : System.Address);
      pragma Import (C, Internal, "gtk_file_chooser_set_preview_widget");
   begin
      Internal (Chooser, Get_Object (Preview_Widget));
   end Set_Preview_Widget;

   -------------------------------
   -- Set_Preview_Widget_Active --
   -------------------------------

   procedure Set_Preview_Widget_Active
     (Chooser : Gtk_File_Chooser; Active : Boolean)
   is
      procedure Internal (Chooser : Gtk_File_Chooser; Active  : Gboolean);
      pragma Import
        (C, Internal, "gtk_file_chooser_set_preview_widget_active");
   begin
      Internal (Chooser, Boolean'Pos (Active));
   end Set_Preview_Widget_Active;

   -------------------------
   -- Set_Select_Multiple --
   -------------------------

   procedure Set_Select_Multiple
     (Chooser : Gtk_File_Chooser; Select_Multiple : Boolean)
   is
      procedure Internal (Chooser : Gtk_File_Chooser; Multiple : Gboolean);
      pragma Import (C, Internal, "gtk_file_chooser_set_select_multiple");
   begin
      Internal (Chooser, Boolean'Pos (Select_Multiple));
   end Set_Select_Multiple;

   ---------------------
   -- Set_Show_Hidden --
   ---------------------

   procedure Set_Show_Hidden
     (Chooser     : Gtk_File_Chooser;
      Show_Hidden : Boolean)
   is
      procedure Internal (Chooser : Gtk_File_Chooser; Show_Hidden : Gboolean);
      pragma Import (C, Internal, "gtk_file_chooser_set_show_hidden");
   begin
      Internal (Chooser, Boolean'Pos (Show_Hidden));
   end Set_Show_Hidden;

   -------------
   -- Set_Uri --
   -------------

   function Set_Uri
     (Chooser : Gtk_File_Chooser; Uri : String) return Boolean
   is
      function Internal (C : Gtk_File_Chooser; U : String) return Gboolean;
      pragma Import (C, Internal, "gtk_file_chooser_set_uri");
   begin
      return Boolean'Val (Internal (Chooser, Uri & ASCII.NUL));
   end Set_Uri;

   ---------------------------
   -- Set_Use_Preview_Label --
   ---------------------------

   procedure Set_Use_Preview_Label
     (Chooser   : Gtk_File_Chooser;
      Use_Label : Boolean)
   is
      procedure Internal (Chooser : Gtk_File_Chooser; Use_Label : Gboolean);
      pragma Import (C, Internal, "gtk_file_chooser_set_use_preview_label");
   begin
      Internal (Chooser, Boolean'Pos (Use_Label));
   end Set_Use_Preview_Label;

   -----------------------
   -- Unselect_Filename --
   -----------------------

   procedure Unselect_Filename
     (Chooser  : Gtk_File_Chooser; Filename : String)
   is
      procedure Internal (Chooser  : Gtk_File_Chooser; Filename : String);
      pragma Import (C, Internal, "gtk_file_chooser_unselect_filename");
   begin
      Internal (Chooser, Filename & ASCII.NUL);
   end Unselect_Filename;

   ------------------
   -- Unselect_Uri --
   ------------------

   procedure Unselect_Uri (Chooser : Gtk_File_Chooser; Uri : String) is
      procedure Internal (Chooser : Gtk_File_Chooser; Uri : String);
      pragma Import (C, Internal, "gtk_file_chooser_unselect_uri");
   begin
      Internal (Chooser, Uri & ASCII.NUL);
   end Unselect_Uri;

end Gtk.File_Chooser;
