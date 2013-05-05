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

with Interfaces.C.Strings; use Interfaces.C.Strings;
with Gtkada.Bindings;      use Gtkada.Bindings;
with Gtkada.Types;

package body Gtk.Recent_Manager is

   --------------
   -- Add_Full --
   --------------

   function Add_Full
     (Manager      : access Gtk_Recent_Manager_Record;
      Uri          : UTF8_String;
      Display_Name : UTF8_String := "";
      Description  : UTF8_String := "";
      Mime_Type    : UTF8_String;
      App_Name     : UTF8_String;
      App_Exec     : UTF8_String;
      Groups       : GNAT.Strings.String_List;
      Is_Private   : Boolean)
      return Boolean
   is
      function Internal
        (Manager     : System.Address;
         Uri         : String;
         Recent_Data : System.Address)
         return Gboolean;
      pragma Import (C, Internal, "gtk_recent_manager_add_full");

      type Gtk_Recent_Data_Record is record
         display_name : chars_ptr;
         description  : chars_ptr;
         mime_type    : chars_ptr;
         app_name     : chars_ptr;
         app_exec     : chars_ptr;
         groups       : System.Address;
         is_private   : Gboolean;
      end record;
      pragma Convention (C, Gtk_Recent_Data_Record);
      --  Internal record that matches struct _GtkRecentData in
      --  gtkrecentmanager.h

      C_Groups : aliased chars_ptr_array := From_String_List (Groups);
      --  Temporary variable to aid translation

      GRD : aliased Gtk_Recent_Data_Record;
      --  Data to feed in to gtk_recent_manager_add_full()

      Result : Gboolean;
   begin
      --  Set up.
      GRD.display_name := String_Or_Null (Display_Name);
      GRD.description  := String_Or_Null (Description);
      GRD.mime_type    := New_String (Mime_Type);
      GRD.app_name     := New_String (App_Name);
      GRD.app_exec     := New_String (App_Exec);
      GRD.is_private   := Boolean'Pos (Is_Private);

      if C_Groups'Length > 0 then
         GRD.groups := C_Groups (C_Groups'First)'Address;
      else
         GRD.groups := System.Null_Address;
      end if;

      --  Invoke function.
      Result := Internal (Get_Object (Manager), Uri & ASCII.NUL, GRD'Address);

      --  Clean up, making sure to avoid double-deallocations where such
      --  may be possible.
      if GRD.display_name /= Null_Ptr then
         Free (GRD.display_name);
      end if;
      if GRD.description /= Null_Ptr then
         Free (GRD.description);
      end if;
      Free (GRD.mime_type);
      Free (GRD.app_name);
      Free (GRD.app_exec);
      for I in C_Groups'Range loop
         if C_Groups (I) /= Null_Ptr then
            Free (C_Groups (I));
         end if;
      end loop;

      --  Return result.
      return Boolean'Val (Result);
   end Add_Full;

   --------------
   -- Add_Item --
   --------------

   function Add_Item
     (Manager : access Gtk_Recent_Manager_Record;
      Uri     : UTF8_String)
      return Boolean
   is
      function Internal
        (Manager : System.Address;
         Uri     : String)
         return Gboolean;
      pragma Import (C, Internal, "gtk_recent_manager_add_item");
   begin
      return Boolean'Val (Internal (Get_Object (Manager), Uri & ASCII.NUL));
   end Add_Item;

   -------------
   -- Convert --
   -------------

   function Convert (Widget : Gtk_Recent_Info) return System.Address is
   begin
      return Get_Object (Widget);
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert (Widget : System.Address) return Gtk_Recent_Info is
      Stub : Gtk_Recent_Info_Record;
   begin
      return Gtk_Recent_Info (Get_User_Data (Widget, Stub));
   end Convert;

   ---------------
   -- Get_Limit --
   ---------------

   function Get_Limit
     (Manager : access Gtk_Recent_Manager_Record) return Gint
   is
      function Internal (Manager : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_recent_manager_get_limit");
   begin
      return Internal (Get_Object (Manager));
   end Get_Limit;

   -----------------
   -- Get_Default --
   -----------------

   function Get_Default return Gtk_Recent_Manager is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_recent_manager_get_default");
      Stub : Gtk_Recent_Manager_Record;
   begin
      return Gtk_Recent_Manager (Get_User_Data (Internal, Stub));
   end Get_Default;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Recent_Manager) is
   begin
      Widget := new Gtk_Recent_Manager_Record;
      Gtk.Recent_Manager.Initialize (Widget);
   end Gtk_New;

   ------------
   -- Exists --
   ------------

   function Exists (Info : access Gtk_Recent_Info_Record) return Boolean is
      function Internal (Info : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_recent_info_exists");
   begin
      return Boolean'Val (Internal (Get_Object (Info)));
   end Exists;

   ---------------
   -- Get_Added --
   ---------------

   function Get_Added
     (Info : access Gtk_Recent_Info_Record) return time_t
   is
      function Internal (Info : System.Address) return time_t;
      pragma Import (C, Internal, "gtk_recent_info_get_added");
   begin
      return Internal (Get_Object (Info));
   end Get_Added;

   -------------
   -- Get_Age --
   -------------

   function Get_Age
     (Info : access Gtk_Recent_Info_Record) return Gint
   is
      function Internal (Info : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_recent_info_get_age");
   begin
      return Internal (Get_Object (Info));
   end Get_Age;

   --------------------------
   -- Get_Application_Info --
   --------------------------

   function Get_Application_Info
     (Info     : access Gtk_Recent_Info_Record;
      App_Name : String)
      return Application_Info_Record
   is
      function Internal
        (Info     : System.Address;
         App_Name : String;
         App_Exec : access chars_ptr;
         Count    : access Guint;
         Time     : access time_t)
         return Gboolean;
      pragma Import (C, Internal, "gtk_recent_info_get_application_info");

      AI       : Application_Info_Record;
      App_Exec : aliased chars_ptr;
      Count    : aliased Guint;
      Time     : aliased time_t;
   begin
      AI.Result := Boolean'Val (Internal
        (Get_Object (Info),
         App_Name & ASCII.NUL,
         App_Exec'Access,
         Count'Access,
         Time'Access));

      AI.App_Exec := new String'(Value (App_Exec));
      AI.Count    := Count;
      AI.Time     := Time;

      Gtkada.Types.g_free (App_Exec);
      return AI;
   end Get_Application_Info;

   ---------------------
   -- Get_Description --
   ---------------------

   function Get_Description
     (Info : access Gtk_Recent_Info_Record) return String
   is
      function Internal (Info : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_recent_info_get_description");
      C : chars_ptr;
   begin
      C := Internal (Get_Object (Info));
      if C = Null_Ptr then
         return "";
      else
         return Value (C);
      end if;
   end Get_Description;

   ----------------------
   -- Get_Display_Name --
   ----------------------

   function Get_Display_Name
     (Info : access Gtk_Recent_Info_Record) return String
   is
      function Internal (Info : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_recent_info_get_display_name");
      C : chars_ptr;
   begin
      C := Internal (Get_Object (Info));
      if C = Null_Ptr then
         return "";
      else
         return Value (C);
      end if;
   end Get_Display_Name;

   --------------------
   -- Get_For_Screen --
   --------------------

   function Get_For_Screen
     (Screen : access Gdk.Screen.Gdk_Screen_Record) return Gtk_Recent_Manager
   is
      function Internal (Screen : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_recent_manager_get_for_screen");
      Stub : Gtk_Recent_Manager_Record;
   begin
      return Gtk_Recent_Manager
        (Get_User_Data (Internal (Get_Object (Screen)), Stub));
   end Get_For_Screen;

   --------------
   -- Get_Icon --
   --------------

   function Get_Icon
     (Info : access Gtk_Recent_Info_Record;
      Size : Gint)
      return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal
        (Info : System.Address;
         Size : Gint)
         return System.Address;
      pragma Import (C, Internal, "gtk_recent_info_get_icon");

      Stub : Gdk.Pixbuf.Gdk_Pixbuf_Record;
   begin
      return Gdk.Pixbuf.Gdk_Pixbuf
        (Get_User_Data (Internal (Get_Object (Info), Size), Stub));
   end Get_Icon;

   ---------------
   -- Get_Items --
   ---------------

   function Get_Items
     (Manager : access Gtk_Recent_Manager_Record)
      return Gtk_Recent_Info_List.Glist
   is
      function Internal (Manager : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_recent_manager_get_items");

      Tmp : Gtk_Recent_Info_List.Glist;
   begin
      Gtk_Recent_Info_List.Set_Object (Tmp, Internal (Get_Object (Manager)));
      return Tmp;
   end Get_Items;

   -------------------
   -- Get_Mime_Type --
   -------------------

   function Get_Mime_Type
     (Info : access Gtk_Recent_Info_Record) return String
   is
      function Internal (Info : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_recent_info_get_mime_type");
      C : chars_ptr;
   begin
      C := Internal (Get_Object (Info));
      if C = Null_Ptr then
         return "";
      else
         return Value (C);
      end if;
   end Get_Mime_Type;

   ------------------
   -- Get_Modified --
   ------------------

   function Get_Modified
     (Info : access Gtk_Recent_Info_Record) return time_t
   is
      function Internal (Info : System.Address) return time_t;
      pragma Import (C, Internal, "gtk_recent_info_get_modified");
   begin
      return Internal (Get_Object (Info));
   end Get_Modified;

   ----------------------
   -- Get_Private_Hint --
   ----------------------

   function Get_Private_Hint
     (Info : access Gtk_Recent_Info_Record) return Boolean
   is
      function Internal (Info : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_recent_info_get_private_hint");
   begin
      return Boolean'Val (Internal (Get_Object (Info)));
   end Get_Private_Hint;

   -------------
   -- Get_Uri --
   -------------

   function Get_Uri (Info : access Gtk_Recent_Info_Record) return String is
      function Internal (Info : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_recent_info_get_uri");
      C : chars_ptr;
   begin
      C := Internal (Get_Object (Info));
      if C = Null_Ptr then
         return "";
      else
         return Value (Internal (Get_Object (Info)));
      end if;
   end Get_Uri;

   -----------------
   -- Get_Visited --
   -----------------

   function Get_Visited
     (Info : access Gtk_Recent_Info_Record) return time_t
   is
      function Internal (Info : System.Address) return time_t;
      pragma Import (C, Internal, "gtk_recent_info_get_visited");
   begin
      return Internal (Get_Object (Info));
   end Get_Visited;

   ---------------------
   -- Has_Application --
   ---------------------

   function Has_Application
     (Info     : access Gtk_Recent_Info_Record;
      App_Name : String)
      return Boolean
   is
      function Internal
        (Info     : System.Address;
         App_Name : String)
         return Gboolean;
      pragma Import (C, Internal, "gtk_recent_info_has_application");
   begin
      return Boolean'Val (Internal (Get_Object (Info), App_Name & ASCII.NUL));
   end Has_Application;

   ---------------
   -- Has_Group --
   ---------------

   function Has_Group
     (Info       : access Gtk_Recent_Info_Record;
      Group_Name : String)
      return Boolean
   is
      function Internal
        (Info       : System.Address;
         Group_Name : String)
         return Gboolean;
      pragma Import (C, Internal, "gtk_recent_info_has_group");
   begin
      return
        Boolean'Val (Internal (Get_Object (Info), Group_Name & ASCII.NUL));
   end Has_Group;

   --------------
   -- Has_Item --
   --------------

   function Has_Item
     (Manager : access Gtk_Recent_Manager_Record;
      Uri     : UTF8_String)
      return Boolean
   is
      function Internal
        (Manager : System.Address;
         Uri     : String)
         return Gboolean;
      pragma Import (C, Internal, "gtk_recent_manager_has_item");
   begin
      return Boolean'Val (Internal (Get_Object (Manager), Uri & ASCII.NUL));
   end Has_Item;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Recent_Manager_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_recent_manager_new");
   begin
      Set_Object (Widget, Internal);
   end Initialize;

   --------------
   -- Is_Local --
   --------------

   function Is_Local (Info : access Gtk_Recent_Info_Record) return Boolean is
      function Internal (Info : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_recent_info_is_local");
   begin
      return Boolean'Val (Internal (Get_Object (Info)));
   end Is_Local;

   -----------------
   -- Lookup_Item --
   -----------------

   function Lookup_Item
     (Manager : access Gtk_Recent_Manager_Record;
      Uri     : UTF8_String;
      Error   : Glib.Error.GError)
      return Gtk_Recent_Info
   is
      function Internal
        (Manager : System.Address;
         Uri     : String;
         Error   : Glib.Error.GError)
         return System.Address;
      pragma Import (C, Internal, "gtk_recent_manager_lookup_item");
      Stub : Gtk_Recent_Info_Record;
   begin
      return Gtk_Recent_Info
        (Get_User_Data
          (Internal (Get_Object (Manager), Uri & ASCII.NUL, Error), Stub));
   end Lookup_Item;

   -----------
   -- Match --
   -----------

   function Match
     (Info_A : access Gtk_Recent_Info_Record'Class;
      Info_B : access Gtk_Recent_Info_Record'Class)
      return Boolean
   is
      function Internal (Info_A, Info_B : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_recent_info_match");
   begin
      return Boolean'Val (Internal (Get_Object (Info_A), Get_Object (Info_B)));
   end Match;

   ---------------
   -- Move_Item --
   ---------------

   function Move_Item
     (Manager : access Gtk_Recent_Manager_Record;
      Uri     : UTF8_String;
      New_Uri : UTF8_String;
      Error   : Glib.Error.GError)
      return Boolean
   is
      function Internal
        (Manager : System.Address;
         Uri     : String;
         New_Uri : System.Address;
         Error   : Glib.Error.GError)
         return Gboolean;
      pragma Import (C, Internal, "gtk_recent_manager_move_item");

      U1 : constant String := Uri & ASCII.NUL;
      U2 : constant String := New_Uri & ASCII.NUL;
   begin
      if New_Uri = "" then
         return Boolean'Val
           (Internal (Get_Object (Manager), U1, System.Null_Address, Error));
      else
         return Boolean'Val
           (Internal (Get_Object (Manager), U1, U2'Address, Error));
      end if;
   end Move_Item;

   -----------------
   -- Purge_Items --
   -----------------

   function Purge_Items
     (Manager : access Gtk_Recent_Manager_Record;
      Error   : Glib.Error.GError)
      return Gint
   is
      function Internal
        (Manager : System.Address;
         Error   : Glib.Error.GError)
         return Gint;
      pragma Import (C, Internal, "gtk_recent_manager_purge_items");
   begin
      return Internal (Get_Object (Manager), Error);
   end Purge_Items;

   ---------
   -- Ref --
   ---------

   function Ref
     (Info : access Gtk_Recent_Info_Record) return Gtk_Recent_Info
   is
      function Internal (Info : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_recent_info_ref");
      Stub : Gtk_Recent_Info_Record;
   begin
      return Gtk_Recent_Info
        (Get_User_Data (Internal (Get_Object (Info)), Stub));
   end Ref;

   -----------------
   -- Remove_Item --
   -----------------

   function Remove_Item
     (Manager : access Gtk_Recent_Manager_Record;
      Uri     : String;
      Error   : Glib.Error.GError)
      return Boolean
   is
      function Internal
        (Manager : System.Address;
         Uri     : String;
         Error   : Glib.Error.GError)
         return Gboolean;
      pragma Import (C, Internal, "gtk_recent_manager_remove_item");
   begin
      return Boolean'Val (Internal
        (Get_Object (Manager),
         Uri & ASCII.NUL,
         Error));
   end Remove_Item;

   ---------------
   -- Set_Limit --
   ---------------

   procedure Set_Limit
     (Manager : access Gtk_Recent_Manager_Record;
      Limit   : Gint)
   is
      procedure Internal
        (Manager : System.Address;
         Limit   : Gint);
      pragma Import (C, Internal, "gtk_recent_manager_set_limit");
   begin
      Internal (Get_Object (Manager), Limit);
   end Set_Limit;

   ----------------
   -- Set_Screen --
   ----------------

   procedure Set_Screen
     (Manager : access Gtk_Recent_Manager_Record;
      Screen  : access Gdk.Screen.Gdk_Screen_Record'Class)
   is
      procedure Internal
        (Manager : System.Address;
         Screen  : System.Address);
      pragma Import (C, Internal, "gtk_recent_manager_set_screen");
   begin
      Internal (Get_Object (Manager), Get_Object (Screen));
   end Set_Screen;

   -----------
   -- Unref --
   -----------

   procedure Unref (Info : access Gtk_Recent_Info_Record) is
      procedure Internal (Info : System.Address);
      pragma Import (C, Internal, "gtk_recent_info_unref");
   begin
      Internal (Get_Object (Info));
   end Unref;

end Gtk.Recent_Manager;
