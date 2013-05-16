-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;

package body Gtk.Entry_Completion is
   package body Match_Functions is

      function Internal_Completion_Func
        (Completion : System.Address;
         Key        : Interfaces.C.Strings.chars_ptr;
         Iter       : Gtk_Tree_Iter;
         Data       : System.Address) return Gboolean;
      pragma Convention (C, Internal_Completion_Func);
      --  Internal callback

      procedure Internal_Notify (Data : System.Address);
      pragma Convention (C, Internal_Notify);
      --  Internal notification function

      type Data_Type_Access is access Data_Type;
      type Completion_User_Data_Record is record
         Callback  : Gtk_Entry_Completion_Match_Func;
         User_Data : Data_Type_Access;
         Notify    : Destroy_Notify;
      end record;
      type Completion_User_Data is access Completion_User_Data_Record;

      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Completion_User_Data_Record, Completion_User_Data);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Data_Type, Data_Type_Access);
      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Completion_User_Data);

      ------------------------------
      -- Internal_Completion_Func --
      ------------------------------

      function Internal_Completion_Func
        (Completion : System.Address;
         Key        : Interfaces.C.Strings.chars_ptr;
         Iter       : Gtk_Tree_Iter;
         Data       : System.Address) return Gboolean
      is
         D : constant Completion_User_Data := Convert (Data);
         Stub : Gtk_Entry_Completion_Record;
         Complete : Gtk_Entry_Completion;
      begin
         Complete := Gtk_Entry_Completion (Get_User_Data (Completion, Stub));
         return Boolean'Pos
           (D.Callback (Complete, Value (Key), Iter, D.User_Data.all));
      end Internal_Completion_Func;

      ---------------------
      -- Internal_Notify --
      ---------------------

      procedure Internal_Notify (Data : System.Address) is
         D : Completion_User_Data := Convert (Data);
      begin
         D.Notify (D.User_Data.all);
         Unchecked_Free (D.User_Data);
         Unchecked_Free (D);
      end Internal_Notify;

      --------------------
      -- Set_Match_Func --
      --------------------

      procedure Set_Match_Func
        (Completion  : access Gtk_Entry_Completion_Record;
         Func        : Gtk_Entry_Completion_Match_Func;
         Func_Data   : Data_Type;
         Func_Notify : Destroy_Notify)
      is
         procedure Internal
           (Completion  : System.Address;
            Func        : System.Address;
            Func_Data   : Completion_User_Data;
            Func_Notify : System.Address);
         pragma Import (C, Internal, "gtk_entry_completion_set_match_func");

         Data : constant Completion_User_Data :=
         new Completion_User_Data_Record'
           (Callback    => Func,
            User_Data   => new Data_Type'(Func_Data),
            Notify      => Func_Notify);
      begin
         Internal (Get_Object (Completion),
            Internal_Completion_Func'Address,
            Data, Internal_Notify'Address);
      end Set_Match_Func;
   end Match_Functions;

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Entry_Completion_Record);
   pragma Unreferenced (Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Completion : out Gtk_Entry_Completion) is
   begin
      Completion := new Gtk_Entry_Completion_Record;
      Gtk.Entry_Completion.Initialize (Completion);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Completion : access Gtk_Entry_Completion_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_entry_completion_new");
   begin
      Set_Object (Completion, Internal);
   end Initialize;

   --------------
   -- Complete --
   --------------

   procedure Complete (Completion : access Gtk_Entry_Completion_Record) is
      procedure Internal (Completion : System.Address);
      pragma Import (C, Internal, "gtk_entry_completion_complete");
   begin
      Internal (Get_Object (Completion));
   end Complete;

   -------------------
   -- Delete_Action --
   -------------------

   procedure Delete_Action
      (Completion : access Gtk_Entry_Completion_Record;
       Index      : Gint)
   is
      procedure Internal (Completion : System.Address; Index : Gint);
      pragma Import (C, Internal, "gtk_entry_completion_delete_action");
   begin
      Internal (Get_Object (Completion), Index);
   end Delete_Action;

   ---------------------------
   -- Get_Completion_Prefix --
   ---------------------------

   function Get_Completion_Prefix
      (Completion : access Gtk_Entry_Completion_Record) return UTF8_String
   is
      function Internal
         (Completion : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_entry_completion_get_completion_prefix");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Completion)));
   end Get_Completion_Prefix;

   ---------------
   -- Get_Entry --
   ---------------

   function Get_Entry
      (Completion : access Gtk_Entry_Completion_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Completion : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_entry_completion_get_entry");
      Stub : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Completion)), Stub));
   end Get_Entry;

   ---------------------------
   -- Get_Inline_Completion --
   ---------------------------

   function Get_Inline_Completion
      (Completion : access Gtk_Entry_Completion_Record) return Boolean
   is
      function Internal (Completion : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_entry_completion_get_inline_completion");
   begin
      return Boolean'Val (Internal (Get_Object (Completion)));
   end Get_Inline_Completion;

   --------------------------
   -- Get_Inline_Selection --
   --------------------------

   function Get_Inline_Selection
      (Completion : access Gtk_Entry_Completion_Record) return Boolean
   is
      function Internal (Completion : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_entry_completion_get_inline_selection");
   begin
      return Boolean'Val (Internal (Get_Object (Completion)));
   end Get_Inline_Selection;

   ----------------------------
   -- Get_Minimum_Key_Length --
   ----------------------------

   function Get_Minimum_Key_Length
      (Completion : access Gtk_Entry_Completion_Record) return Gint
   is
      function Internal (Completion : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_entry_completion_get_minimum_key_length");
   begin
      return Internal (Get_Object (Completion));
   end Get_Minimum_Key_Length;

   ---------------
   -- Get_Model --
   ---------------

   function Get_Model
      (Completion : access Gtk_Entry_Completion_Record)
       return Gtk.Tree_Model.Gtk_Tree_Model
   is
      function Internal (Completion : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_entry_completion_get_model");
      Stub : Gtk.Tree_Model.Gtk_Tree_Model_Record;
   begin
      return Gtk.Tree_Model.Gtk_Tree_Model (Get_User_Data (Internal (Get_Object (Completion)), Stub));
   end Get_Model;

   --------------------------
   -- Get_Popup_Completion --
   --------------------------

   function Get_Popup_Completion
      (Completion : access Gtk_Entry_Completion_Record) return Boolean
   is
      function Internal (Completion : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_entry_completion_get_popup_completion");
   begin
      return Boolean'Val (Internal (Get_Object (Completion)));
   end Get_Popup_Completion;

   -------------------------
   -- Get_Popup_Set_Width --
   -------------------------

   function Get_Popup_Set_Width
      (Completion : access Gtk_Entry_Completion_Record) return Boolean
   is
      function Internal (Completion : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_entry_completion_get_popup_set_width");
   begin
      return Boolean'Val (Internal (Get_Object (Completion)));
   end Get_Popup_Set_Width;

   ----------------------------
   -- Get_Popup_Single_Match --
   ----------------------------

   function Get_Popup_Single_Match
      (Completion : access Gtk_Entry_Completion_Record) return Boolean
   is
      function Internal (Completion : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_entry_completion_get_popup_single_match");
   begin
      return Boolean'Val (Internal (Get_Object (Completion)));
   end Get_Popup_Single_Match;

   ---------------------
   -- Get_Text_Column --
   ---------------------

   function Get_Text_Column
      (Completion : access Gtk_Entry_Completion_Record) return Gint
   is
      function Internal (Completion : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_entry_completion_get_text_column");
   begin
      return Internal (Get_Object (Completion));
   end Get_Text_Column;

   --------------------------
   -- Insert_Action_Markup --
   --------------------------

   procedure Insert_Action_Markup
      (Completion : access Gtk_Entry_Completion_Record;
       Index      : Gint;
       Markup     : UTF8_String)
   is
      procedure Internal
         (Completion : System.Address;
          Index      : Gint;
          Markup     : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_entry_completion_insert_action_markup");
      Tmp_Markup : Interfaces.C.Strings.chars_ptr := New_String (Markup);
   begin
      Internal (Get_Object (Completion), Index, Tmp_Markup);
      Free (Tmp_Markup);
   end Insert_Action_Markup;

   ------------------------
   -- Insert_Action_Text --
   ------------------------

   procedure Insert_Action_Text
      (Completion : access Gtk_Entry_Completion_Record;
       Index      : Gint;
       Text       : UTF8_String)
   is
      procedure Internal
         (Completion : System.Address;
          Index      : Gint;
          Text       : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_entry_completion_insert_action_text");
      Tmp_Text : Interfaces.C.Strings.chars_ptr := New_String (Text);
   begin
      Internal (Get_Object (Completion), Index, Tmp_Text);
      Free (Tmp_Text);
   end Insert_Action_Text;

   -------------------
   -- Insert_Prefix --
   -------------------

   procedure Insert_Prefix (Completion : access Gtk_Entry_Completion_Record) is
      procedure Internal (Completion : System.Address);
      pragma Import (C, Internal, "gtk_entry_completion_insert_prefix");
   begin
      Internal (Get_Object (Completion));
   end Insert_Prefix;

   ---------------------------
   -- Set_Inline_Completion --
   ---------------------------

   procedure Set_Inline_Completion
      (Completion        : access Gtk_Entry_Completion_Record;
       Inline_Completion : Boolean)
   is
      procedure Internal
         (Completion        : System.Address;
          Inline_Completion : Integer);
      pragma Import (C, Internal, "gtk_entry_completion_set_inline_completion");
   begin
      Internal (Get_Object (Completion), Boolean'Pos (Inline_Completion));
   end Set_Inline_Completion;

   --------------------------
   -- Set_Inline_Selection --
   --------------------------

   procedure Set_Inline_Selection
      (Completion       : access Gtk_Entry_Completion_Record;
       Inline_Selection : Boolean)
   is
      procedure Internal
         (Completion       : System.Address;
          Inline_Selection : Integer);
      pragma Import (C, Internal, "gtk_entry_completion_set_inline_selection");
   begin
      Internal (Get_Object (Completion), Boolean'Pos (Inline_Selection));
   end Set_Inline_Selection;

   --------------------
   -- Set_Match_Func --
   --------------------

   procedure Set_Match_Func
      (Completion  : access Gtk_Entry_Completion_Record;
       Func        : C_Gtk_Entry_Completion_Match_Func;
       Func_Data   : System.Address;
       Func_Notify : Glib.G_Destroy_Notify_Address)
   is
      procedure Internal
         (Completion  : System.Address;
          Func        : C_Gtk_Entry_Completion_Match_Func;
          Func_Data   : System.Address;
          Func_Notify : Glib.G_Destroy_Notify_Address);
      pragma Import (C, Internal, "gtk_entry_completion_set_match_func");
   begin
      Internal (Get_Object (Completion), Func, Func_Data, Func_Notify);
   end Set_Match_Func;

   ----------------------------
   -- Set_Minimum_Key_Length --
   ----------------------------

   procedure Set_Minimum_Key_Length
      (Completion : access Gtk_Entry_Completion_Record;
       Length     : Gint)
   is
      procedure Internal (Completion : System.Address; Length : Gint);
      pragma Import (C, Internal, "gtk_entry_completion_set_minimum_key_length");
   begin
      Internal (Get_Object (Completion), Length);
   end Set_Minimum_Key_Length;

   ---------------
   -- Set_Model --
   ---------------

   procedure Set_Model
      (Completion : access Gtk_Entry_Completion_Record;
       Model      : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class)
   is
      procedure Internal
         (Completion : System.Address;
          Model      : System.Address);
      pragma Import (C, Internal, "gtk_entry_completion_set_model");
   begin
      Internal (Get_Object (Completion), Get_Object (Model));
   end Set_Model;

   --------------------------
   -- Set_Popup_Completion --
   --------------------------

   procedure Set_Popup_Completion
      (Completion       : access Gtk_Entry_Completion_Record;
       Popup_Completion : Boolean)
   is
      procedure Internal
         (Completion       : System.Address;
          Popup_Completion : Integer);
      pragma Import (C, Internal, "gtk_entry_completion_set_popup_completion");
   begin
      Internal (Get_Object (Completion), Boolean'Pos (Popup_Completion));
   end Set_Popup_Completion;

   -------------------------
   -- Set_Popup_Set_Width --
   -------------------------

   procedure Set_Popup_Set_Width
      (Completion      : access Gtk_Entry_Completion_Record;
       Popup_Set_Width : Boolean)
   is
      procedure Internal
         (Completion      : System.Address;
          Popup_Set_Width : Integer);
      pragma Import (C, Internal, "gtk_entry_completion_set_popup_set_width");
   begin
      Internal (Get_Object (Completion), Boolean'Pos (Popup_Set_Width));
   end Set_Popup_Set_Width;

   ----------------------------
   -- Set_Popup_Single_Match --
   ----------------------------

   procedure Set_Popup_Single_Match
      (Completion         : access Gtk_Entry_Completion_Record;
       Popup_Single_Match : Boolean)
   is
      procedure Internal
         (Completion         : System.Address;
          Popup_Single_Match : Integer);
      pragma Import (C, Internal, "gtk_entry_completion_set_popup_single_match");
   begin
      Internal (Get_Object (Completion), Boolean'Pos (Popup_Single_Match));
   end Set_Popup_Single_Match;

   ---------------------
   -- Set_Text_Column --
   ---------------------

   procedure Set_Text_Column
      (Completion : access Gtk_Entry_Completion_Record;
       Column     : Gint)
   is
      procedure Internal (Completion : System.Address; Column : Gint);
      pragma Import (C, Internal, "gtk_entry_completion_set_text_column");
   begin
      Internal (Get_Object (Completion), Column);
   end Set_Text_Column;

end Gtk.Entry_Completion;
