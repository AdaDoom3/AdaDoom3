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

with Gtkada.Bindings;      use Gtkada.Bindings;
with Glib.Error;           use Glib.Error;
with Gtk.Accel_Group;      use Gtk.Accel_Group;
with Gtk.Action;           use Gtk.Action;
with Gtk.Action_Group;     use Gtk.Action_Group;
with Gtk.Widget;           use Gtk.Widget;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Glib.Type_Conversion_Hooks;

package body Gtk.UI_Manager is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_UI_Manager_Record);
   pragma Warnings (Off, Type_Conversion);

   ------------
   -- Add_UI --
   ------------

   procedure Add_UI
     (Self     : access Gtk_UI_Manager_Record;
      Merge_Id : Guint;
      Path     : String;
      Name     : String;
      Action   : String := "";
      Typ      : Manager_Item_Type := Manager_Auto;
      Top      : Boolean := False)
   is
      procedure Internal
        (Self     : System.Address;
         Merge_Id : Guint;
         Path     : String;
         Name     : String;
         Action   : chars_ptr;
         Typ      : Manager_Item_Type;
         Top      : Gboolean);
      pragma Import (C, Internal, "gtk_ui_manager_add_ui");
      A : chars_ptr := String_Or_Null (Action);
   begin
      Internal (Get_Object (Self), Merge_Id, Path & ASCII.NUL,
                Name & ASCII.NUL, A, Typ, Boolean'Pos (Top));
      Free (A);
   end Add_UI;

   ----------------------
   -- Add_UI_From_File --
   ----------------------

   function Add_UI_From_File
     (Self     : access Gtk_UI_Manager_Record;
      Filename : String;
      Error    : GError_Access := null)
      return Guint
   is
      function Internal
        (Self     : System.Address;
         Filename : String;
         Error    : GError_Access) return Guint;
      pragma Import (C, Internal, "gtk_ui_manager_add_ui_from_file");
   begin
      return Internal (Get_Object (Self), Filename & ASCII.NUL, Error);
   end Add_UI_From_File;

   ------------------------
   -- Add_UI_From_String --
   ------------------------

   function Add_UI_From_String
     (Self   : access Gtk_UI_Manager_Record;
      Buffer : String;
      Error  : GError_Access := null)
      return Guint
   is
      function Internal
        (Self   : System.Address;
         Buffer : String;
         Length : Gint;
         Error  : GError_Access)
         return Guint;
      pragma Import (C, Internal, "gtk_ui_manager_add_ui_from_string");
   begin
      return Internal (Get_Object (Self), Buffer, Buffer'Length, Error);
   end Add_UI_From_String;

   -------------------
   -- Ensure_Update --
   -------------------

   procedure Ensure_Update (Self : access Gtk_UI_Manager_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_ui_manager_ensure_update");
   begin
      Internal (Get_Object (Self));
   end Ensure_Update;

   ---------------------
   -- Get_Accel_Group --
   ---------------------

   function Get_Accel_Group
     (Self : access Gtk_UI_Manager_Record)
      return Gtk_Accel_Group
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_ui_manager_get_accel_group");
      Stub : Gtk_Accel_Group_Record;
   begin
      return Gtk_Accel_Group
        (Get_User_Data (Internal (Get_Object (Self)), Stub));
   end Get_Accel_Group;

   ----------------
   -- Get_Action --
   ----------------

   function Get_Action
     (Self : access Gtk_UI_Manager_Record;
      Path : String)
      return Gtk_Action
   is
      function Internal
        (Self : System.Address; Path : String) return System.Address;
      pragma Import (C, Internal, "gtk_ui_manager_get_action");
      Stub : Gtk_Action_Record;
   begin
      return Gtk_Action
        (Get_User_Data (Internal (Get_Object (Self), Path & ASCII.NUL), Stub));
   end Get_Action;

   -----------------------
   -- Get_Action_Groups --
   -----------------------

   function Get_Action_Groups
     (Self : access Gtk_UI_Manager_Record)
      return Glib.Object.Object_Simple_List.Glist
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_ui_manager_get_action_groups");
      L : Object_Simple_List.Glist;
   begin
      Object_Simple_List.Set_Object (L, Internal (Get_Object (Self)));
      return L;
   end Get_Action_Groups;

   ----------------------
   -- Get_Add_Tearoffs --
   ----------------------

   function Get_Add_Tearoffs
     (Self : access Gtk_UI_Manager_Record)
      return Boolean
   is
      function Internal
        (Self : System.Address)
         return Gboolean;
      pragma Import (C, Internal, "gtk_ui_manager_get_add_tearoffs");
   begin
      return Boolean'Val (Internal (Get_Object (Self)));
   end Get_Add_Tearoffs;

   -------------------
   -- Get_Toplevels --
   -------------------

   function Get_Toplevels
     (Self  : access Gtk_UI_Manager_Record;
      Types : Manager_Item_Type)
      return Widget_SList.GSlist
   is
      function Internal
        (Self  : System.Address;
         Types : Manager_Item_Type) return System.Address;
      pragma Import (C, Internal, "gtk_ui_manager_get_toplevels");
      L : Widget_SList.GSlist;
   begin
      Widget_SList.Set_Object (L, Internal (Get_Object (Self), Types));
      return L;
   end Get_Toplevels;

   ------------
   -- Get_UI --
   ------------

   function Get_UI
     (Self : access Gtk_UI_Manager_Record) return String
   is
      function Internal (Self : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_ui_manager_get_ui");
      S : chars_ptr := Internal (Get_Object (Self));
      Result : constant String := Value (S);
   begin
      Free (S);
      return Result;
   end Get_UI;

   ----------------
   -- Get_Widget --
   ----------------

   function Get_Widget
     (Self : access Gtk_UI_Manager_Record;
      Path : String) return Gtk_Widget
   is
      function Internal
        (Self : System.Address;
         Path : String) return System.Address;
      pragma Import (C, Internal, "gtk_ui_manager_get_widget");
      Stub : Gtk_Widget_Record;
   begin
      return Gtk_Widget
        (Get_User_Data (Internal (Get_Object (Self), Path & ASCII.NUL), Stub));
   end Get_Widget;

   -------------------------
   -- Insert_Action_Group --
   -------------------------

   procedure Insert_Action_Group
     (Self         : access Gtk_UI_Manager_Record;
      Action_Group : access Gtk_Action_Group_Record'Class;
      Pos          : Gint)
   is
      procedure Internal
        (Self         : System.Address;
         Action_Group : System.Address;
         Pos          : Gint);
      pragma Import (C, Internal, "gtk_ui_manager_insert_action_group");
   begin
      Internal (Get_Object (Self), Get_Object (Action_Group), Pos);
   end Insert_Action_Group;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (UI : out Gtk_UI_Manager) is
   begin
      UI := new Gtk_UI_Manager_Record;
      Gtk.UI_Manager.Initialize (UI);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (UI : access Gtk_UI_Manager_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_ui_manager_new");
   begin
      Set_Object (UI, Internal);
   end Initialize;

   ------------------
   -- New_Merge_Id --
   ------------------

   function New_Merge_Id
     (Self : access Gtk_UI_Manager_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_ui_manager_new_merge_id");
   begin
      return Internal (Get_Object (Self));
   end New_Merge_Id;

   -------------------------
   -- Remove_Action_Group --
   -------------------------

   procedure Remove_Action_Group
     (Self         : access Gtk_UI_Manager_Record;
      Action_Group : access Gtk_Action_Group_Record'Class)
   is
      procedure Internal
        (Self         : System.Address;
         Action_Group : System.Address);
      pragma Import (C, Internal, "gtk_ui_manager_remove_action_group");
   begin
      Internal (Get_Object (Self), Get_Object (Action_Group));
   end Remove_Action_Group;

   ---------------
   -- Remove_UI --
   ---------------

   procedure Remove_UI
     (Self     : access Gtk_UI_Manager_Record;
      Merge_Id : Guint)
   is
      procedure Internal (Self : System.Address; Merge_Id : Guint);
      pragma Import (C, Internal, "gtk_ui_manager_remove_ui");
   begin
      Internal (Get_Object (Self), Merge_Id);
   end Remove_UI;

   ----------------------
   -- Set_Add_Tearoffs --
   ----------------------

   procedure Set_Add_Tearoffs
     (Self         : access Gtk_UI_Manager_Record;
      Add_Tearoffs : Boolean)
   is
      procedure Internal
        (Self         : System.Address;
         Add_Tearoffs : Gboolean);
      pragma Import (C, Internal, "gtk_ui_manager_set_add_tearoffs");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Add_Tearoffs));
   end Set_Add_Tearoffs;

end Gtk.UI_Manager;
