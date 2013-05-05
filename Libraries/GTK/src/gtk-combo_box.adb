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

with Gtk.Tree_Model;       use Gtk.Tree_Model;
with Gtk.Tree_View;        use Gtk.Tree_View;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;               use System;

with Glib.Type_Conversion_Hooks;

package body Gtk.Combo_Box is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Combo_Box_Record);
   pragma Warnings (Off, Type_Conversion);

   ------------------
   -- Gtk_New_Text --
   ------------------

   procedure Gtk_New_Text (Combo : out Gtk_Combo_Box) is
   begin
      Combo := new Gtk_Combo_Box_Record;
      Initialize_Text (Combo);
   end Gtk_New_Text;

   ---------------------
   -- Initialize_Text --
   ---------------------

   procedure Initialize_Text (Combo : access Gtk_Combo_Box_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_combo_box_new_text");
   begin
      Set_Object (Combo, Internal);
   end Initialize_Text;

   -----------------
   -- Append_Text --
   -----------------

   procedure Append_Text
     (Combo_Box : access Gtk_Combo_Box_Record; Text : String)
   is
      procedure Internal (Combo_Box : System.Address; Text : String);
      pragma Import (C, Internal, "gtk_combo_box_append_text");
   begin
      Internal (Get_Object (Combo_Box), Text & ASCII.NUL);
   end Append_Text;

   ------------------
   -- Prepend_Text --
   ------------------

   procedure Prepend_Text
     (Combo_Box : access Gtk_Combo_Box_Record; Text : String)
   is
      procedure Internal (Combo_Box : System.Address; Text : String);
      pragma Import (C, Internal, "gtk_combo_box_prepend_text");
   begin
      Internal (Get_Object (Combo_Box), Text & ASCII.NUL);
   end Prepend_Text;

   -----------------
   -- Insert_Text --
   -----------------

   procedure Insert_Text
     (Combo_Box : access Gtk_Combo_Box_Record;
      Position  : Gint;
      Text      : String)
   is
      procedure Internal
        (Combo_Box : System.Address;
         Position  : Gint;
         Text      : String);
      pragma Import (C, Internal, "gtk_combo_box_insert_text");
   begin
      Internal (Get_Object (Combo_Box), Position, Text & ASCII.NUL);
   end Insert_Text;

   -----------------
   -- Remove_Text --
   -----------------

   procedure Remove_Text
     (Combo_Box : access Gtk_Combo_Box_Record; Position : Gint)
   is
      procedure Internal (Combo_Box : System.Address; Position  : Gint);
      pragma Import (C, Internal, "gtk_combo_box_remove_text");
   begin
      Internal (Get_Object (Combo_Box), Position);
   end Remove_Text;

   ---------------------
   -- Get_Active_Text --
   ---------------------

   function Get_Active_Text
     (Combo_Box : access Gtk_Combo_Box_Record) return String
   is
      function Internal (Combo_Box : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_combo_box_get_active_text");
      Result : chars_ptr := Internal (Get_Object (Combo_Box));
   begin
      if Result /= Null_Ptr then
         declare
            Output : constant String := Value (Result);
         begin
            Free (Result);

            return Output;
         end;
      else
         return "";
      end if;
   end Get_Active_Text;

   ---------------
   -- Set_Model --
   ---------------

   procedure Set_Model
     (Combo_Box : access Gtk_Combo_Box_Record;
      Model     : Gtk_Tree_Model := null)
   is
      procedure Internal
        (Combo_Box : System.Address;
         Model     : System.Address);
      pragma Import (C, Internal, "gtk_combo_box_set_model");
   begin
      if Model = null then
         Internal (Get_Object (Combo_Box), System.Null_Address);
      else
         Internal (Get_Object (Combo_Box), Get_Object (Model));
      end if;
   end Set_Model;

   ---------------
   -- Get_Model --
   ---------------

   function Get_Model
     (Combo_Box : access Gtk_Combo_Box_Record)
      return Gtk_Tree_Model
   is
      function Internal
        (Combo_Box : System.Address)
         return System.Address;
      pragma Import (C, Internal, "gtk_combo_box_get_model");
      Stub : Gtk_Tree_Model_Record;
   begin
      return Gtk_Tree_Model
        (Get_User_Data (Internal (Get_Object (Combo_Box)), Stub));
   end Get_Model;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Combo : out Gtk_Combo_Box) is
   begin
      Combo := new Gtk_Combo_Box_Record;
      Gtk.Combo_Box.Initialize (Combo);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Combo : access Gtk_Combo_Box_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_combo_box_new");
   begin
      Set_Object (Combo, Internal);
   end Initialize;

   ------------------------
   -- Gtk_New_With_Model --
   ------------------------

   procedure Gtk_New_With_Model
     (Combo  : out Gtk_Combo_Box;
      Model  : access Gtk_Tree_Model_Record'Class) is
   begin
      Combo := new Gtk_Combo_Box_Record;
      Initialize_With_Model (Combo, Model);
   end Gtk_New_With_Model;

   ---------------------------
   -- Initialize_With_Model --
   ---------------------------

   procedure Initialize_With_Model
     (Combo : access Gtk_Combo_Box_Record'Class;
      Model : access Gtk_Tree_Model_Record'Class)
   is
      function Internal (Model : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_combo_box_new_with_model");
   begin
      Set_Object (Combo,  Internal (Get_Object (Model)));
   end Initialize_With_Model;

   ------------------------
   -- Gtk_New_With_Entry --
   ------------------------

   procedure Gtk_New_With_Entry (Combo : out Gtk_Combo_Box) is
   begin
      Combo := new Gtk_Combo_Box_Record;
      Initialize_With_Entry (Combo);
   end Gtk_New_With_Entry;

   ---------------------------
   -- Initialize_With_Entry --
   ---------------------------

   procedure Initialize_With_Entry (Combo : access Gtk_Combo_Box_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_combo_box_new_with_entry");
   begin
      Set_Object (Combo, Internal);
   end Initialize_With_Entry;

   ----------------------------------
   -- Gtk_New_With_Model_And_Entry --
   ----------------------------------

   procedure Gtk_New_With_Model_And_Entry
     (Combo : out Gtk_Combo_Box;
      Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class) is
   begin
      Combo := new Gtk_Combo_Box_Record;
      Initialize_With_Model_And_Entry (Combo, Model);
   end Gtk_New_With_Model_And_Entry;

   -------------------------------------
   -- Initialize_With_Model_And_Entry --
   -------------------------------------

   procedure Initialize_With_Model_And_Entry
     (Combo : access Gtk_Combo_Box_Record'Class;
      Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class)
   is
      function Internal (Model : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_combo_box_new_with_model_and_entry");
   begin
      Set_Object (Combo,  Internal (Get_Object (Model)));
   end Initialize_With_Model_And_Entry;

   ----------------
   -- Get_Active --
   ----------------

   function Get_Active
     (Combo_Box : access Gtk_Combo_Box_Record) return Gint
   is
      function Internal (Combo_Box : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_combo_box_get_active");
   begin
      return Internal (Get_Object (Combo_Box));
   end Get_Active;

   ---------------------
   -- Get_Active_Iter --
   ---------------------

   function Get_Active_Iter
     (Combo_Box : access Gtk_Combo_Box_Record) return Gtk_Tree_Iter
   is
      function Internal
        (Combo_Box : System.Address;
         Iter      : System.Address)
         return Gboolean;
      pragma Import (C, Internal, "gtk_combo_box_get_active_iter");
      Iter : aliased Gtk_Tree_Iter;
      Tmp  : constant Gboolean := Internal
        (Get_Object (Combo_Box), Iter'Address);
   begin
      if Tmp /= 0 then
         return Iter;
      else
         return Null_Iter;
      end if;
   end Get_Active_Iter;

   --------------------
   -- Get_Wrap_Width --
   --------------------

   function Get_Wrap_Width
     (Combo_Box : access Gtk_Combo_Box_Record) return Gint
   is
      function Internal (Combo_Box : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_combo_box_get_wrap_width");
   begin
      return Internal (Get_Object (Combo_Box));
   end Get_Wrap_Width;

   ----------------------
   -- Get_Add_Tearoffs --
   ----------------------

   function Get_Add_Tearoffs
     (Combo_Box : access Gtk_Combo_Box_Record) return Boolean
   is
      function Internal (Combo_Box : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_combo_box_get_add_tearoffs");
   begin
      return Boolean'Val (Internal (Get_Object (Combo_Box)));
   end Get_Add_Tearoffs;

   ----------------------------
   -- Get_Button_Sensitivity --
   ----------------------------

   function Get_Button_Sensitivity
     (Combo_Box : access Gtk_Combo_Box_Record)
      return Gtk.Enums.Gtk_Sensitivity_Type
   is
      function Internal
        (Combo_Box : System.Address)
         return Gtk.Enums.Gtk_Sensitivity_Type;
      pragma Import (C, Internal, "gtk_combo_box_get_button_sensitivity");
   begin
      return Internal (Get_Object (Combo_Box));
   end Get_Button_Sensitivity;

   ----------------------------
   -- Get_Column_Span_Column --
   ----------------------------

   function Get_Column_Span_Column
     (Combo_Box : access Gtk_Combo_Box_Record) return Gint
   is
      function Internal (Combo_Box : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_combo_box_get_column_span_column");
   begin
      return Internal (Get_Object (Combo_Box));
   end Get_Column_Span_Column;

   ------------------------
   -- Get_Focus_On_Click --
   ------------------------

   function Get_Focus_On_Click
     (Combo : access Gtk_Combo_Box_Record) return Boolean
   is
      function Internal (Combo : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_combo_box_get_focus_on_click");
   begin
      return Boolean'Val (Internal (Get_Object (Combo)));
   end Get_Focus_On_Click;

   -------------------------
   -- Get_Row_Span_Column --
   -------------------------

   function Get_Row_Span_Column
     (Combo_Box : access Gtk_Combo_Box_Record) return Gint
   is
      function Internal (Combo_Box : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_combo_box_get_row_span_column");
   begin
      return Internal (Get_Object (Combo_Box));
   end Get_Row_Span_Column;

   ---------------
   -- Get_Title --
   ---------------

   function Get_Title
     (Combo_Box : access Gtk_Combo_Box_Record)
      return String
   is
      function Internal
        (Combo_Box : System.Address)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_combo_box_get_title");
   begin
      return Value (Internal (Get_Object (Combo_Box)));
   end Get_Title;

   -------------------
   -- Get_Has_Entry --
   -------------------

   function Get_Has_Entry
     (Combo_Box : access Gtk_Combo_Box_Record) return Boolean
   is
      function Internal (C : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_combo_box_get_has_entry");
   begin
      return Internal (Get_Object (Combo_Box)) /= 0;
   end Get_Has_Entry;

   ---------------------------
   -- Set_Entry_Text_Column --
   ---------------------------

   procedure Set_Entry_Text_Column
     (Combo_Box   : access Gtk_Combo_Box_Record;
      Text_Column : Gint)
   is
      procedure Internal (C : System.Address; Col : Gint);
      pragma Import (C, Internal, "gtk_combo_box_set_entry_text_column");
   begin
      Internal (Get_Object (Combo_Box), Text_Column);
   end Set_Entry_Text_Column;

   ---------------------------
   -- Get_Entry_Text_Column --
   ---------------------------

   function Get_Entry_Text_Column
     (Combo_Box : access Gtk_Combo_Box_Record) return Gint
   is
      function Internal (C : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_combo_box_get_entry_text_column");
   begin
      return Internal (Get_Object (Combo_Box));
   end Get_Entry_Text_Column;

   ----------------
   -- Set_Active --
   ----------------

   procedure Set_Active
     (Combo_Box : access Gtk_Combo_Box_Record; Index : Gint)
   is
      procedure Internal
        (Combo_Box : System.Address; Index : Gint);
      pragma Import (C, Internal, "gtk_combo_box_set_active");
   begin
      Internal (Get_Object (Combo_Box), Index);
   end Set_Active;

   ---------------------
   -- Set_Active_Iter --
   ---------------------

   procedure Set_Active_Iter
     (Combo_Box : access Gtk_Combo_Box_Record;
      Iter      : Gtk_Tree_Iter)
   is
      procedure Internal
        (Combo_Box : System.Address;
         Iter      : Gtk_Tree_Iter);
      pragma Import (C, Internal, "gtk_combo_box_set_active_iter");
   begin
      Internal (Get_Object (Combo_Box), Iter);
   end Set_Active_Iter;

   ----------------------
   -- Set_Add_Tearoffs --
   ----------------------

   procedure Set_Add_Tearoffs
     (Combo_Box    : access Gtk_Combo_Box_Record;
      Add_Tearoffs : Boolean)
   is
      procedure Internal
        (Combo_Box : System.Address; Add_Tearoffs : Gboolean);
      pragma Import (C, Internal, "gtk_combo_box_set_add_tearoffs");
   begin
      Internal (Get_Object (Combo_Box), Boolean'Pos (Add_Tearoffs));
   end Set_Add_Tearoffs;

   ----------------------------
   -- Set_Button_Sensitivity --
   ----------------------------

   procedure Set_Button_Sensitivity
     (Combo_Box   : access Gtk_Combo_Box_Record;
      Sensitivity : Gtk.Enums.Gtk_Sensitivity_Type)
   is
      procedure Internal
        (Combo_Box   : System.Address;
         Sensitivity : Gtk.Enums.Gtk_Sensitivity_Type);
      pragma Import (C, Internal, "gtk_combo_box_set_button_sensitivity");
   begin
      Internal (Get_Object (Combo_Box), Sensitivity);
   end Set_Button_Sensitivity;

   ----------------------------
   -- Set_Column_Span_Column --
   ----------------------------

   procedure Set_Column_Span_Column
     (Combo_Box   : access Gtk_Combo_Box_Record;
      Column_Span : Gint)
   is
      procedure Internal (Combo_Box   : System.Address;  Column_Span : Gint);
      pragma Import (C, Internal, "gtk_combo_box_set_column_span_column");
   begin
      Internal (Get_Object (Combo_Box), Column_Span);
   end Set_Column_Span_Column;

   ------------------------
   -- Set_Focus_On_Click --
   ------------------------

   procedure Set_Focus_On_Click
     (Combo          : access Gtk_Combo_Box_Record;
      Focus_On_Click : Boolean)
   is
      procedure Internal (Combo : System.Address; Focus_On_Click : Gboolean);
      pragma Import (C, Internal, "gtk_combo_box_set_focus_on_click");
   begin
      Internal (Get_Object (Combo), Boolean'Pos (Focus_On_Click));
   end Set_Focus_On_Click;

   -------------------------
   -- Set_Row_Span_Column --
   -------------------------

   procedure Set_Row_Span_Column
     (Combo_Box : access Gtk_Combo_Box_Record;
      Row_Span  : Gint)
   is
      procedure Internal (Combo_Box : System.Address;  Row_Span  : Gint);
      pragma Import (C, Internal, "gtk_combo_box_set_row_span_column");
   begin
      Internal (Get_Object (Combo_Box), Row_Span);
   end Set_Row_Span_Column;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title
     (Combo_Box : access Gtk_Combo_Box_Record;
      Title     : String)
   is
      procedure Internal
        (Combo_Box : System.Address;
         Title     : String);
      pragma Import (C, Internal, "gtk_combo_box_set_title");
   begin
      Internal (Get_Object (Combo_Box), Title & ASCII.NUL);
   end Set_Title;

   --------------------
   -- Set_Wrap_Width --
   --------------------

   procedure Set_Wrap_Width
     (Combo_Box : access Gtk_Combo_Box_Record;
      Width     : Gint)
   is
      procedure Internal (Combo_Box : System.Address; Width : Gint);
      pragma Import (C, Internal, "gtk_combo_box_set_wrap_width");
   begin
      Internal (Get_Object (Combo_Box), Width);
   end Set_Wrap_Width;

   -------------
   -- Popdown --
   -------------

   procedure Popdown
     (Combo_Box : access Gtk_Combo_Box_Record)
   is
      procedure Internal
        (Combo_Box : System.Address);
      pragma Import (C, Internal, "gtk_combo_box_popdown");
   begin
      Internal (Get_Object (Combo_Box));
   end Popdown;

   -----------
   -- Popup --
   -----------

   procedure Popup
     (Combo_Box : access Gtk_Combo_Box_Record)
   is
      procedure Internal
        (Combo_Box : System.Address);
      pragma Import (C, Internal, "gtk_combo_box_popup");
   begin
      Internal (Get_Object (Combo_Box));
   end Popup;

   ----------------------------
   -- Get_Row_Separator_Func --
   ----------------------------

   function Get_Row_Separator_Func
     (Combo_Box : access Gtk_Combo_Box_Record)
      return Gtk_Tree_View_Row_Separator_Func
   is
      function Internal
        (Combo_Box : System.Address) return Gtk_Tree_View_Row_Separator_Func;
      pragma Import (C, Internal, "gtk_combo_box_get_row_separator_func");
   begin
      return Internal (Get_Object (Combo_Box));
   end Get_Row_Separator_Func;

   ----------------------------
   -- Set_Row_Separator_Func --
   ----------------------------

   procedure Set_Row_Separator_Func
     (Combo_Box : access Gtk_Combo_Box_Record;
      Func      : Gtk_Tree_View_Row_Separator_Func;
      Data      : System.Address;
      Destroy   : Glib.G_Destroy_Notify_Address := null)
   is
      procedure Internal
        (Combo_Box : System.Address;
         Func      : Gtk_Tree_View_Row_Separator_Func;
         Data      : System.Address;
         Destroy   : Glib.G_Destroy_Notify_Address);
      pragma Import (C, Internal, "gtk_combo_box_set_row_separator_func");
   begin
      Internal (Get_Object (Combo_Box), Func, Data, Destroy);
   end Set_Row_Separator_Func;

end Gtk.Combo_Box;
