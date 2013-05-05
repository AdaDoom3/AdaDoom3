-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                 Copyright (C) 2001-2013, AdaCore                  --
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

with System;         use System;
with Gtk;            use Gtk;
with Gtk.Tree_Model; use Gtk.Tree_Model;

with Glib.Type_Conversion_Hooks;

package body Gtk.Tree_Selection is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Tree_Selection_Record);
   pragma Warnings (Off, Type_Conversion);

   package body Selection_Foreach is

      ----------------------
      -- Selected_Foreach --
      ----------------------

      type Data_Wrapper is record
         Data : Data_Type_Access;
         Func : Foreach_Func;
      end record;

      procedure C_Foreach_Func
        (Model : System.Address;
         Path  : Gtk_Tree_Path;
         Iter  : Gtk_Tree_Iter;
         Data  : access Data_Wrapper);
      pragma Convention (C, C_Foreach_Func);

      procedure C_Foreach_Func
        (Model : System.Address;
         Path  : Gtk_Tree_Path;
         Iter  : Gtk_Tree_Iter;
         Data  : access Data_Wrapper)
      is
         Stub : Gtk_Tree_Model_Record;
      begin
         Data.Func (Gtk_Tree_Model (Get_User_Data_Fast (Model, Stub)),
                    Path, Iter, Data.Data);
      end C_Foreach_Func;

      procedure Selected_Foreach
        (Selection : access Gtk_Tree_Selection_Record'Class;
         Func      : Foreach_Func;
         Data      : Data_Type_Access)
      is
         procedure Internal
           (Selection : System.Address;
            Func      : System.Address;
            Data      : access Data_Wrapper);
         pragma Import (C, Internal, "gtk_tree_selection_selected_foreach");

         D : aliased Data_Wrapper := (Data, Func);
      begin
         Internal (Get_Object (Selection), C_Foreach_Func'Address, D'Access);
      end Selected_Foreach;

   end Selection_Foreach;

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode
     (Selection : access Gtk_Tree_Selection_Record'Class;
      The_Type  : Gtk_Selection_Mode)
   is
      procedure Internal
        (Selection : System.Address;
         The_Type  : Gtk_Selection_Mode);
      pragma Import (C, Internal, "gtk_tree_selection_set_mode");

   begin
      Internal (Get_Object (Selection), The_Type);
   end Set_Mode;

   --------------
   -- Get_Mode --
   --------------

   function Get_Mode
     (Selection : access Gtk_Tree_Selection_Record'Class)
      return Gtk_Selection_Mode
   is
      function Internal (Selection : System.Address) return Gtk_Selection_Mode;
      pragma Import (C, Internal, "gtk_tree_selection_get_mode");

   begin
      return Internal (Get_Object (Selection));
   end Get_Mode;

--    -------------------------
--    -- Set_Select_Function --
--    -------------------------

--    procedure Set_Select_Function
--      (Selection : access Gtk_Tree_Selection_Record'Class;
--       Func      : Gtk_Tree_Selection_Func;
--       Data      : gpointer;
--       Destroy   : Gtk_Destroy_Notify)
--    is
--       procedure Internal
--         (Selection : System.Address;
--          Func      : Gtk_Tree_Selection_Func;
--          Data      : Integer;
--          Destroy   : Gtk_Destroy_Notify);
--       pragma Import (C, Internal, "gtk_tree_selection_set_select_function");
--
--    begin
--       Internal (Get_Object (Selection), Func, Data, Destroy);
--    end Set_Select_Function;

--    -------------------
--    -- Get_User_Data --
--    -------------------

--    function Get_User_Data
--     (Selection : access Gtk_Tree_Selection_Record'Class)
--                            return System.Address
--    is
--       function Internal (Selection : System.Address) return System.Address;
--       pragma Import (C, Internal, "gtk_tree_selection_get_user_data");
--    begin
--       return Internal (Get_Object (Selection));
--    end Get_User_Data;

   -------------------
   -- Get_Tree_View --
   -------------------

   function Get_Tree_View
     (Selection : access Gtk_Tree_Selection_Record'Class)
      return Gtk.Widget.Gtk_Widget
   is
      function Internal (Selection : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_selection_get_tree_view");

   begin
      return Widget.Convert (Internal (Get_Object (Selection)));
   end Get_Tree_View;

   ------------------
   -- Get_Selected --
   ------------------

   procedure Get_Selected
     (Selection : access Gtk_Tree_Selection_Record'Class;
      Model     : out Gtk_Tree_Model;
      Iter      : out Gtk_Tree_Iter)
   is
      function Internal
        (Selection : System.Address;
         Model     : access System.Address;
         Iter      : access Gtk_Tree_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_tree_selection_get_selected");

      M           : aliased System.Address;
      Local_Model : Gtk_Tree_Model_Record;
      It          : aliased Gtk_Tree_Iter;

   begin
      if Internal (Get_Object (Selection), M'Access, It'Access) = 0 then
         Iter  := Null_Iter;
         Model := null;
      else
         Model := Gtk_Tree_Model (Get_User_Data_Fast (M, Local_Model));
         Iter  := It;
      end if;
   end Get_Selected;

   -----------------
   -- Select_Path --
   -----------------

   procedure Select_Path
     (Selection : access Gtk_Tree_Selection_Record'Class;
      Path      : Gtk_Tree_Path)
   is
      procedure Internal
        (Selection : System.Address;
         Path      : Gtk_Tree_Path);
      pragma Import (C, Internal, "gtk_tree_selection_select_path");

   begin
      Internal (Get_Object (Selection), Path);
   end Select_Path;

   -------------------
   -- Unselect_Path --
   -------------------

   procedure Unselect_Path
     (Selection : access Gtk_Tree_Selection_Record'Class;
      Path      : Gtk_Tree_Path)
   is
      procedure Internal
        (Selection : System.Address;
         Path      : Gtk_Tree_Path);
      pragma Import (C, Internal, "gtk_tree_selection_unselect_path");

   begin
      Internal (Get_Object (Selection), Path);
   end Unselect_Path;

   -----------------
   -- Select_Iter --
   -----------------

   procedure Select_Iter
     (Selection : access Gtk_Tree_Selection_Record'Class;
      Iter      : Gtk_Tree_Iter)
   is
      procedure Internal
        (Selection : System.Address;
         Iter      : Gtk_Tree_Iter);
      pragma Import (C, Internal, "gtk_tree_selection_select_iter");

   begin
      Internal (Get_Object (Selection), Iter);
   end Select_Iter;

   -------------------
   -- Unselect_Iter --
   -------------------

   procedure Unselect_Iter
     (Selection : access Gtk_Tree_Selection_Record'Class;
      Iter      : Gtk_Tree_Iter)
   is
      procedure Internal
        (Selection : System.Address;
         Iter      : Gtk_Tree_Iter);
      pragma Import (C, Internal, "gtk_tree_selection_unselect_iter");

   begin
      Internal (Get_Object (Selection), Iter);
   end Unselect_Iter;

   ----------------------
   -- Path_Is_Selected --
   ----------------------

   function Path_Is_Selected
     (Selection : access Gtk_Tree_Selection_Record'Class;
      Path      : Gtk_Tree_Path)
      return Boolean
   is
      function Internal
        (Selection : System.Address;
         Path      : Gtk_Tree_Path) return Gboolean;
      pragma Import (C, Internal, "gtk_tree_selection_path_is_selected");

   begin
      return Internal (Get_Object (Selection), Path) /= 0;
   end Path_Is_Selected;

   ----------------------
   -- Iter_Is_Selected --
   ----------------------

   function Iter_Is_Selected
     (Selection : access Gtk_Tree_Selection_Record'Class;
      Iter      : Gtk_Tree_Iter) return Boolean
   is
      function Internal
        (Selection : System.Address;
         Iter      : Gtk_Tree_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_tree_selection_iter_is_selected");

   begin
      return Internal (Get_Object (Selection), Iter) /= 0;
   end Iter_Is_Selected;

   ----------------
   -- Select_All --
   ----------------

   procedure Select_All (Selection : access Gtk_Tree_Selection_Record'Class) is
      procedure Internal (Selection : System.Address);
      pragma Import (C, Internal, "gtk_tree_selection_select_all");

   begin
      Internal (Get_Object (Selection));
   end Select_All;

   ------------------
   -- Unselect_All --
   ------------------

   procedure Unselect_All
     (Selection : access Gtk_Tree_Selection_Record'Class)
   is
      procedure Internal (Selection : System.Address);
      pragma Import (C, Internal, "gtk_tree_selection_unselect_all");
   begin
      Internal (Get_Object (Selection));
   end Unselect_All;

   ------------------
   -- Select_Range --
   ------------------

   procedure Select_Range
     (Selection  : access Gtk_Tree_Selection_Record'Class;
      Start_Path : Gtk_Tree_Path;
      End_Path   : Gtk_Tree_Path)
   is
      procedure Internal
        (Selection  : System.Address;
         Start_Path : Gtk_Tree_Path;
         End_Path   : Gtk_Tree_Path);
      pragma Import (C, Internal, "gtk_tree_selection_select_range");

   begin
      Internal (Get_Object (Selection), Start_Path, End_Path);
   end Select_Range;

   -------------------------
   -- Count_Selected_Rows --
   -------------------------

   function Count_Selected_Rows
     (Selection : access Gtk_Tree_Selection_Record) return Gint
   is
      function Internal (Selection : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_tree_selection_count_selected_rows");
   begin
      return Internal (Get_Object (Selection));
   end Count_Selected_Rows;

   -----------------------
   -- Get_Selected_Rows --
   -----------------------

   procedure Get_Selected_Rows
     (Selection : access Gtk_Tree_Selection_Record;
      Model     : out Gtk_Tree_Model;
      Path_List : out Gtk_Tree_Path_List.Glist)
   is
      function Internal
        (Selection : System.Address;
         Model     : access System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_selection_get_selected_rows");

      M        : aliased System.Address;
      Stub     : Gtk_Tree_Model_Record;
      Selected : System.Address;

   begin
      Selected := Internal (Get_Object (Selection), M'Access);
      if Selected = System.Null_Address then
         Model := null;
         Path_List := Gtk_Tree_Path_List.Null_List;
      else
         Model := Gtk_Tree_Model (Get_User_Data_Fast (M, Stub));
         Gtk_Tree_Path_List.Set_Object (Path_List, Selected);
      end if;
   end Get_Selected_Rows;

   --------------------
   -- Unselect_Range --
   --------------------

   procedure Unselect_Range
     (Selection  : access Gtk_Tree_Selection_Record;
      Start_Path : Gtk_Tree_Path;
      End_Path   : Gtk_Tree_Path)
   is
      procedure Internal
        (Selection  : System.Address;
         Start_Path : Gtk_Tree_Path;
         End_Path   : Gtk_Tree_Path);
      pragma Import (C, Internal, "gtk_tree_selection_unselect_range");
   begin
      Internal (Get_Object (Selection), Start_Path, End_Path);
   end Unselect_Range;

end Gtk.Tree_Selection;
