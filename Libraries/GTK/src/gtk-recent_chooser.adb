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

with Interfaces.C.Strings;       use Interfaces.C.Strings;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Gtkada.Bindings;

package body Gtk.Recent_Chooser is

   ----------------
   -- Add_Filter --
   ----------------

   procedure Add_Filter
     (Chooser : Gtk_Recent_Chooser;
      Filter  : access Gtk.Recent_Filter.Gtk_Recent_Filter_Record'Class)
   is
      procedure Internal
        (Chooser : Gtk_Recent_Chooser;
         Filter  : System.Address);
      pragma Import (C, Internal, "gtk_recent_chooser_add_filter");
   begin
      Internal (Chooser, Get_Object (Filter));
   end Add_Filter;

   ----------------------
   -- Get_Current_Item --
   ----------------------

   function Get_Current_Item
     (Chooser : Gtk_Recent_Chooser) return Gtk.Recent_Manager.Gtk_Recent_Info
   is
      function Internal (Chooser : Gtk_Recent_Chooser) return System.Address;
      pragma Import (C, Internal, "gtk_recent_chooser_get_current_item");
      Stub : Gtk.Recent_Manager.Gtk_Recent_Info_Record;
   begin
      return Gtk.Recent_Manager.Gtk_Recent_Info
        (Get_User_Data (Internal (Chooser), Stub));
   end Get_Current_Item;

   ---------------------
   -- Get_Current_Uri --
   ---------------------

   function Get_Current_Uri (Chooser : Gtk_Recent_Chooser) return String is
      function Internal (Chooser : Gtk_Recent_Chooser) return chars_ptr;
      pragma Import (C, Internal, "gtk_recent_chooser_get_current_uri");
      C : chars_ptr;
   begin
      C := Internal (Chooser);
      if C = Null_Ptr then
         return "";
      else
         return Value (C);
      end if;
   end Get_Current_Uri;

   ----------------
   -- Get_Filter --
   ----------------

   function Get_Filter
     (Chooser : Gtk_Recent_Chooser) return Gtk.Recent_Filter.Gtk_Recent_Filter
   is
      function Internal (Chooser : Gtk_Recent_Chooser) return System.Address;
      pragma Import (C, Internal, "gtk_recent_chooser_get_filter");
      Stub : Gtk.Recent_Filter.Gtk_Recent_Filter_Record;
   begin
      return Gtk.Recent_Filter.Gtk_Recent_Filter
        (Get_User_Data (Internal (Chooser), Stub));
   end Get_Filter;

   ---------------
   -- Get_Items --
   ---------------

   function Get_Items
     (Chooser : Gtk_Recent_Chooser)
      return Gtk.Recent_Manager.Gtk_Recent_Info_List.Glist
   is
      use Gtk.Recent_Manager.Gtk_Recent_Info_List;
      function Internal (Chooser : Gtk_Recent_Chooser) return System.Address;
      pragma Import (C, Internal, "gtk_recent_chooser_get_items");
      List : Glist;
   begin
      Set_Object (List, Internal (Chooser));
      return List;
   end Get_Items;

   ---------------
   -- Get_Limit --
   ---------------

   function Get_Limit (Chooser : Gtk_Recent_Chooser) return Gint is
      function Internal (Chooser : Gtk_Recent_Chooser) return Gint;
      pragma Import (C, Internal, "gtk_recent_chooser_get_limit");
   begin
      return Internal (Chooser);
   end Get_Limit;

   --------------------
   -- Get_Local_Only --
   --------------------

   function Get_Local_Only (Chooser : Gtk_Recent_Chooser) return Boolean is
      function Internal (Chooser : Gtk_Recent_Chooser) return Gboolean;
      pragma Import (C, Internal, "gtk_recent_chooser_get_local_only");
   begin
      return Boolean'Val (Internal (Chooser));
   end Get_Local_Only;

   -------------------------
   -- Get_Select_Multiple --
   -------------------------

   function Get_Select_Multiple
     (Chooser : Gtk_Recent_Chooser) return Boolean
   is
      function Internal (Chooser : Gtk_Recent_Chooser) return Gboolean;
      pragma Import (C, Internal, "gtk_recent_chooser_get_select_multiple");
   begin
      return Boolean'Val (Internal (Chooser));
   end Get_Select_Multiple;

   --------------------
   -- Get_Show_Icons --
   --------------------

   function Get_Show_Icons (Chooser : Gtk_Recent_Chooser) return Boolean is
      function Internal (Chooser : Gtk_Recent_Chooser) return Gboolean;
      pragma Import (C, Internal, "gtk_recent_chooser_get_show_icons");
   begin
      return Boolean'Val (Internal (Chooser));
   end Get_Show_Icons;

   ------------------------
   -- Get_Show_Not_Found --
   ------------------------

   function Get_Show_Not_Found (Chooser : Gtk_Recent_Chooser) return Boolean is
      function Internal (Chooser : Gtk_Recent_Chooser) return Gboolean;
      pragma Import (C, Internal, "gtk_recent_chooser_get_show_not_found");
   begin
      return Boolean'Val (Internal (Chooser));
   end Get_Show_Not_Found;

   ----------------------
   -- Get_Show_Numbers --
   ----------------------

   function Get_Show_Numbers (Chooser : Gtk_Recent_Chooser) return Boolean is
      function Internal (Chooser : Gtk_Recent_Chooser) return Gboolean;
      pragma Import (C, Internal, "gtk_recent_chooser_get_show_numbers");
   begin
      return Boolean'Val (Internal (Chooser));
   end Get_Show_Numbers;

   ----------------------
   -- Get_Show_Private --
   ----------------------

   function Get_Show_Private (Chooser : Gtk_Recent_Chooser) return Boolean is
      function Internal (Chooser : Gtk_Recent_Chooser) return Gboolean;
      pragma Import (C, Internal, "gtk_recent_chooser_get_show_private");
   begin
      return Boolean'Val (Internal (Chooser));
   end Get_Show_Private;

   -------------------
   -- Get_Show_Tips --
   -------------------

   function Get_Show_Tips (Chooser : Gtk_Recent_Chooser) return Boolean is
      function Internal (Chooser : Gtk_Recent_Chooser) return Gboolean;
      pragma Import (C, Internal, "gtk_recent_chooser_get_show_tips");
   begin
      return Boolean'Val (Internal (Chooser));
   end Get_Show_Tips;

   -------------------
   -- Get_Sort_Type --
   -------------------

   function Get_Sort_Type
     (Chooser : Gtk_Recent_Chooser) return Gtk_Recent_Sort_Type
   is
      function Internal
        (Chooser : Gtk_Recent_Chooser) return Gtk_Recent_Sort_Type;
      pragma Import (C, Internal, "gtk_recent_chooser_get_sort_type");
   begin
      return Internal (Chooser);
   end Get_Sort_Type;

   --------------
   -- Get_Uris --
   --------------

   function Get_Uris
     (Chooser : Gtk_Recent_Chooser) return GNAT.Strings.String_List
   is
      use Gtkada.Bindings;

      function Internal
        (Chooser : Gtk_Recent_Chooser;
         Length  : access Gsize)
         return chars_ptr_array_access;
      pragma Import (C, Internal, "gtk_recent_chooser_get_uris");

      Length : aliased Gsize;
      Tmp : chars_ptr_array_access;
   begin
      Tmp := Internal (Chooser, Length'Access);
      declare
         String_List : constant GNAT.Strings.String_List :=
           To_String_List (Tmp.all, Gint (Length));
      begin
         g_strfreev (Tmp);
         return String_List;
      end;
   end Get_Uris;

   ------------------
   -- List_Filters --
   ------------------

   function List_Filters
     (Chooser : Gtk_Recent_Chooser)
      return Gtk.Recent_Filter.Gtk_Recent_Filter_List.GSlist
   is
      use Gtk.Recent_Filter.Gtk_Recent_Filter_List;

      function Internal (Chooser : Gtk_Recent_Chooser) return System.Address;
      pragma Import (C, Internal, "gtk_recent_chooser_list_filters");
      Result : GSlist;
   begin
      Set_Object (Result, Internal (Chooser));
      return Result;
   end List_Filters;

   -------------------
   -- Remove_Filter --
   -------------------

   procedure Remove_Filter
     (Chooser : Gtk_Recent_Chooser;
      Filter  : access Gtk.Recent_Filter.Gtk_Recent_Filter_Record'Class)
   is
      procedure Internal
        (Chooser : Gtk_Recent_Chooser;
         Filter  : System.Address);
      pragma Import (C, Internal, "gtk_recent_chooser_remove_filter");
   begin
      Internal (Chooser, Get_Object (Filter));
   end Remove_Filter;

   ----------------
   -- Select_All --
   ----------------

   procedure Select_All (Chooser : Gtk_Recent_Chooser) is
      procedure Internal (Chooser : Gtk_Recent_Chooser);
      pragma Import (C, Internal, "gtk_recent_chooser_select_all");
   begin
      Internal (Chooser);
   end Select_All;

   ----------------
   -- Select_Uri --
   ----------------

   function Select_Uri
     (Chooser : Gtk_Recent_Chooser;
      Uri     : String;
      Error   : Glib.Error.GError := null)
      return Boolean
   is
      function Internal
        (Chooser : Gtk_Recent_Chooser;
         Uri     : String;
         Error   : Glib.Error.GError)
         return Gboolean;
      pragma Import (C, Internal, "gtk_recent_chooser_select_uri");
   begin
      return Boolean'Val (Internal (Chooser, Uri & ASCII.NUL, Error));
   end Select_Uri;

   ---------------------
   -- Set_Current_Uri --
   ---------------------

   function Set_Current_Uri
     (Chooser : Gtk_Recent_Chooser;
      Uri     : String;
      Error   : Glib.Error.GError := null)
      return Boolean
   is
      function Internal
        (Chooser : Gtk_Recent_Chooser;
         Uri     : String;
         Error   : Glib.Error.GError)
         return Gboolean;
      pragma Import (C, Internal, "gtk_recent_chooser_set_current_uri");
   begin
      return Boolean'Val (Internal (Chooser, Uri & ASCII.NUL, Error));
   end Set_Current_Uri;

   ----------------
   -- Set_Filter --
   ----------------

   procedure Set_Filter
     (Chooser : Gtk_Recent_Chooser;
      Filter  : access Gtk.Recent_Filter.Gtk_Recent_Filter_Record'Class)
   is
      procedure Internal
        (Chooser : Gtk_Recent_Chooser;
         Filter  : System.Address);
      pragma Import (C, Internal, "gtk_recent_chooser_set_filter");
   begin
      Internal (Chooser, Get_Object (Filter));
   end Set_Filter;

   ---------------
   -- Set_Limit --
   ---------------

   procedure Set_Limit
     (Chooser : Gtk_Recent_Chooser;
      Limit   : Gint)
   is
      procedure Internal
        (Chooser : Gtk_Recent_Chooser;
         Limit   : Gint);
      pragma Import (C, Internal, "gtk_recent_chooser_set_limit");
   begin
      Internal (Chooser, Limit);
   end Set_Limit;

   --------------------
   -- Set_Local_Only --
   --------------------

   procedure Set_Local_Only
     (Chooser    : Gtk_Recent_Chooser;
      Local_Only : Boolean)
   is
      procedure Internal
        (Chooser    : Gtk_Recent_Chooser;
         Local_Only : Gboolean);
      pragma Import (C, Internal, "gtk_recent_chooser_set_local_only");
   begin
      Internal (Chooser, Boolean'Pos (Local_Only));
   end Set_Local_Only;

   -------------------------
   -- Set_Select_Multiple --
   -------------------------

   procedure Set_Select_Multiple
     (Chooser         : Gtk_Recent_Chooser;
      Select_Multiple : Boolean)
   is
      procedure Internal
        (Chooser         : Gtk_Recent_Chooser;
         Select_Multiple : Gboolean);
      pragma Import (C, Internal, "gtk_recent_chooser_set_select_multiple");
   begin
      Internal (Chooser, Boolean'Pos (Select_Multiple));
   end Set_Select_Multiple;

   --------------------
   -- Set_Show_Icons --
   --------------------

   procedure Set_Show_Icons
     (Chooser    : Gtk_Recent_Chooser;
      Show_Icons : Boolean)
   is
      procedure Internal
        (Chooser    : Gtk_Recent_Chooser;
         Show_Icons : Gboolean);
      pragma Import (C, Internal, "gtk_recent_chooser_set_show_icons");
   begin
      Internal (Chooser, Boolean'Pos (Show_Icons));
   end Set_Show_Icons;

   ------------------------
   -- Set_Show_Not_Found --
   ------------------------

   procedure Set_Show_Not_Found
     (Chooser        : Gtk_Recent_Chooser;
      Show_Not_Found : Boolean)
   is
      procedure Internal
        (Chooser        : Gtk_Recent_Chooser;
         Show_Not_Found : Gboolean);
      pragma Import (C, Internal, "gtk_recent_chooser_set_show_not_found");
   begin
      Internal (Chooser, Boolean'Pos (Show_Not_Found));
   end Set_Show_Not_Found;

   ----------------------
   -- Set_Show_Numbers --
   ----------------------

   procedure Set_Show_Numbers
     (Chooser      : Gtk_Recent_Chooser;
      Show_Numbers : Boolean)
   is
      procedure Internal
        (Chooser      : Gtk_Recent_Chooser;
         Show_Numbers : Gboolean);
      pragma Import (C, Internal, "gtk_recent_chooser_set_show_numbers");
   begin
      Internal (Chooser, Boolean'Pos (Show_Numbers));
   end Set_Show_Numbers;

   ----------------------
   -- Set_Show_Private --
   ----------------------

   procedure Set_Show_Private
     (Chooser      : Gtk_Recent_Chooser;
      Show_Private : Boolean)
   is
      procedure Internal
        (Chooser      : Gtk_Recent_Chooser;
         Show_Private : Gboolean);
      pragma Import (C, Internal, "gtk_recent_chooser_set_show_private");
   begin
      Internal (Chooser, Boolean'Pos (Show_Private));
   end Set_Show_Private;

   -------------------
   -- Set_Show_Tips --
   -------------------

   procedure Set_Show_Tips
     (Chooser   : Gtk_Recent_Chooser;
      Show_Tips : Boolean)
   is
      procedure Internal
        (Chooser   : Gtk_Recent_Chooser;
         Show_Tips : Gboolean);
      pragma Import (C, Internal, "gtk_recent_chooser_set_show_tips");
   begin
      Internal (Chooser, Boolean'Pos (Show_Tips));
   end Set_Show_Tips;

   --------------------
   -- User_Sort_Func --
   --------------------

   package body User_Sort_Func is

      type Internal_Data is record
         User_Func    : Gtk_Recent_Sort_Func;
         User_Data    : Data_Type_Access;
         Destroy_Func : Destroy_Notify;
      end record;
      type Internal_Data_Access is access Internal_Data;

      function Internal_Sort_Func
        (A, B : System.Address;
         Data : Internal_Data_Access)
         return Gint;
      pragma Convention (C, Internal_Sort_Func);
      --  This is the sorting function we'll pass to
      --  gtk_recent_chooser_set_sort_func().

      procedure Internal_Destroy (D : System.Address);
      pragma Convention (C, Internal_Destroy);
      --  This is a wrapper destroy function we'll pass to
      --  gtk_recent_chooser_set_sort_func().

      ----------------------
      -- Internal_Destroy --
      ----------------------

      procedure Internal_Destroy (D : System.Address) is
         function Convert is new Ada.Unchecked_Conversion
           (System.Address, Internal_Data_Access);
         procedure Free is new Ada.Unchecked_Deallocation
           (Internal_Data, Internal_Data_Access);
         procedure Free is new Ada.Unchecked_Deallocation
           (Data_Type, Data_Type_Access);
         Data : Internal_Data_Access := Convert (D);
      begin
         if Data.Destroy_Func /= null then
            Data.Destroy_Func (Data.User_Data.all);
         end if;
         Free (Data.User_Data);
         Free (Data);
      end Internal_Destroy;

      ------------------------
      -- Internal_Sort_Func --
      ------------------------

      function Internal_Sort_Func
        (A, B : System.Address;
         Data : Internal_Data_Access)
         return Gint
      is
         use Gtk.Recent_Manager;
         Left, Right : Gtk_Recent_Info;
         Stub : Gtk_Recent_Info_Record;
      begin
         Left  := Gtk_Recent_Info (Get_User_Data (A, Stub));
         Right := Gtk_Recent_Info (Get_User_Data (B, Stub));
         case Data.User_Func (Left, Right, Data.User_Data) is
            when Before => return  1;
            when Equal  => return  0;
            when After  => return -1;
         end case;
      end Internal_Sort_Func;

      -------------------
      -- Set_Sort_Func --
      -------------------

      procedure Set_Sort_Func
        (Chooser      : Gtk_Recent_Chooser;
         Sort_Func    : Gtk_Recent_Sort_Func;
         Sort_Data    : Data_Type_Access := null;
         Data_Destroy : Destroy_Notify := null)
      is
         procedure Internal
           (Chooser      : Gtk_Recent_Chooser;
            Sort_Func    : System.Address;
            Sort_Data    : Internal_Data_Access;
            Data_Destroy : System.Address);
         pragma Import (C, Internal, "gtk_recent_chooser_set_sort_func");

         Data : Internal_Data_Access;
      begin
         Data := new Internal_Data'
           (User_Func    => Sort_Func,
            User_Data    => Sort_Data,
            Destroy_Func => Data_Destroy);
         Internal
           (Chooser,
            Internal_Sort_Func'Address,
            Data,
            Internal_Destroy'Address);
      end Set_Sort_Func;

   end User_Sort_Func;

   -------------------
   -- Set_Sort_Type --
   -------------------

   procedure Set_Sort_Type
     (Chooser   : Gtk_Recent_Chooser;
      Sort_Type : Gtk_Recent_Sort_Type)
   is
      procedure Internal
        (Chooser   : Gtk_Recent_Chooser;
         Sort_Type : Gtk_Recent_Sort_Type);
      pragma Import (C, Internal, "gtk_recent_chooser_set_sort_type");
   begin
      Internal (Chooser, Sort_Type);
   end Set_Sort_Type;

   ------------------
   -- Unselect_All --
   ------------------

   procedure Unselect_All (Chooser : Gtk_Recent_Chooser) is
      procedure Internal (Chooser : Gtk_Recent_Chooser);
      pragma Import (C, Internal, "gtk_recent_chooser_unselect_all");
   begin
      Internal (Chooser);
   end Unselect_All;

   ------------------
   -- Unselect_Uri --
   ------------------

   procedure Unselect_Uri
     (Chooser : Gtk_Recent_Chooser;
      Uri     : String)
   is
      procedure Internal
        (Chooser : Gtk_Recent_Chooser;
         Uri     : String);
      pragma Import (C, Internal, "gtk_recent_chooser_unselect_uri");
   begin
      Internal (Chooser, Uri & ASCII.NUL);
   end Unselect_Uri;

end Gtk.Recent_Chooser;
