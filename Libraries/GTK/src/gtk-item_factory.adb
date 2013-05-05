-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                 Copyright (C) 2000-2013, AdaCore                  --
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

with Ada.Unchecked_Conversion;
with Interfaces.C.Strings;
with System;

with Gdk;
with Gtk.Widget; use Gtk.Widget;

with Glib.Type_Conversion_Hooks;

package body Gtk.Item_Factory is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Item_Factory_Record);
   pragma Warnings (Off, Type_Conversion);

   package ICS renames Interfaces.C.Strings;

   -----------------
   -- Add_Foreign --
   -----------------

   procedure Add_Foreign
     (Accel_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Full_Path    : UTF8_String;
      Accel_Group  : Gtk.Accel_Group.Gtk_Accel_Group;
      Keyval       : Guint;
      Modifiers    : Gdk.Types.Gdk_Modifier_Type)
   is
      procedure Internal
        (Accel_Widget : System.Address;
         Full_Path    : UTF8_String;
         Accel_Group  : System.Address;
         Keyval       : Guint;
         Modifiers    : Gdk.Types.Gdk_Modifier_Type);
      pragma Import (C, Internal, "gtk_item_factory_add_foreign");

   begin
      Internal (Get_Object (Accel_Widget),
                Full_Path & ASCII.NUL,
                Get_Object (Accel_Group),
                Keyval,
                Modifiers);
   end Add_Foreign;

   --------------------
   -- Delete_Entries --
   --------------------

   procedure Delete_Entries
     (Ifactory  : access Gtk_Item_Factory_Record;
      Entries   : Gtk_Item_Factory_Entry_Array)
   is
      procedure Internal
        (Ifactory  : System.Address;
         N_Entries : Guint;
         Entries   : System.Address);
      pragma Import (C, Internal, "gtk_item_factory_delete_entries");

   begin
      Internal (Get_Object (Ifactory), Entries'Length, Entries'Address);
   end Delete_Entries;

   ------------------
   -- Delete_Entry --
   ------------------

   procedure Delete_Entry
     (Ifactory : access Gtk_Item_Factory_Record;
      Ientry   : Gtk_Item_Factory_Entry)
   is
      procedure Internal
        (Ifactory : System.Address;
         Ientry   : Gtk_Item_Factory_Entry);
      pragma Import (C, Internal, "gtk_item_factory_delete_entry");

   begin
      Internal (Get_Object (Ifactory), Ientry);
   end Delete_Entry;

   -----------------
   -- Delete_Item --
   -----------------

   procedure Delete_Item
     (Ifactory : access Gtk_Item_Factory_Record;
      Path     : UTF8_String)
   is
      procedure Internal
        (Ifactory : System.Address;
         Path     : UTF8_String);
      pragma Import (C, Internal, "gtk_item_factory_delete_item");

   begin
      Internal (Get_Object (Ifactory), Path & ASCII.NUL);
   end Delete_Item;

   -----------------
   -- From_Widget --
   -----------------

   function From_Widget
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Gtk_Item_Factory
   is
      function Internal (Widget : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_item_factory_from_widget");

      Stub : Gtk_Item_Factory_Record;

   begin
      return Gtk_Item_Factory
        (Get_User_Data (Internal (Get_Object (Widget)), Stub));
   end From_Widget;

   --------------
   -- Get_Item --
   --------------

   function Get_Item
     (Ifactory : access Gtk_Item_Factory_Record;
      Path     : UTF8_String) return Gtk.Widget.Gtk_Widget
   is
      function Internal
        (Ifactory : System.Address;
         Path     : UTF8_String) return System.Address;
      pragma Import (C, Internal, "gtk_item_factory_get_item");

   begin
      return Convert (Internal (Get_Object (Ifactory), Path & ASCII.NUL));
   end Get_Item;

   ------------------------
   -- Get_Item_By_Action --
   ------------------------

   function Get_Item_By_Action
     (Ifactory : access Gtk_Item_Factory_Record;
      Action   : Guint) return Gtk.Widget.Gtk_Widget
   is
      function Internal
        (Ifactory : System.Address;
         Action   : Guint) return System.Address;
      pragma Import (C, Internal, "gtk_item_factory_get_item_by_action");

   begin
      return Convert (Internal (Get_Object (Ifactory), Action));
   end Get_Item_By_Action;

   ----------------
   -- Get_Widget --
   ----------------

   function Get_Widget
     (Ifactory : access Gtk_Item_Factory_Record;
      Path     : UTF8_String) return Gtk.Widget.Gtk_Widget
   is
      function Internal
        (Ifactory : System.Address;
         Path     : UTF8_String) return System.Address;
      pragma Import (C, Internal, "gtk_item_factory_get_widget");

   begin
      return Convert (Internal (Get_Object (Ifactory), Path & ASCII.NUL));
   end Get_Widget;

   --------------------------
   -- Get_Widget_By_Action --
   --------------------------

   function Get_Widget_By_Action
     (Ifactory : access Gtk_Item_Factory_Record;
      Action   : Guint) return Gtk.Widget.Gtk_Widget
   is
      function Internal
        (Ifactory : System.Address;
         Action   : Guint) return System.Address;
      pragma Import (C, Internal, "gtk_item_factory_get_widget_by_action");

   begin
      return Convert (Internal (Get_Object (Ifactory), Action));
   end Get_Widget_By_Action;

   ----------------------
   -- Path_From_Widget --
   ----------------------

   function Path_From_Widget
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) return UTF8_String
   is
      function Internal
        (Widget : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_item_factory_path_from_widget");

   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Widget)));
   end Path_From_Widget;

   -----------
   -- Popup --
   -----------

   procedure Popup
     (Ifactory     : access Gtk_Item_Factory_Record;
      X            : Guint;
      Y            : Guint;
      Mouse_Button : Guint;
      Time         : Guint32)
   is
      procedure Internal
        (Ifactory     : System.Address;
         X            : Guint;
         Y            : Guint;
         Mouse_Button : Guint;
         Time         : Guint32);
      pragma Import (C, Internal, "gtk_item_factory_popup");

   begin
      Internal (Get_Object (Ifactory), X, Y, Mouse_Button, Time);
   end Popup;

   ---------------
   -- Data_Item --
   ---------------

   package body Data_Item is

      -----------------
      -- Create_Item --
      -----------------

      procedure Create_Item
        (Ifactory      : access Gtk_Item_Factory_Record'Class;
         Ientry        : Gtk_Item_Factory_Entry;
         Callback_Data : Data_Type_Access;
         Callback_Type : Guint)
      is
         procedure Internal
           (Ifactory      : System.Address;
            Ientry        : Gtk_Item_Factory_Entry;
            Callback_Data : Data_Type_Access;
            Callback_Type : Guint);
         pragma Import (C, Internal, "gtk_item_factory_create_item");

      begin
         Internal (Get_Object (Ifactory),
                   Ientry,
                   Callback_Data,
                   Callback_Type);
      end Create_Item;

      ------------------
      -- Create_Items --
      ------------------

      procedure Create_Items
        (Ifactory      : access Gtk_Item_Factory_Record'Class;
         Entries       : Gtk_Item_Factory_Entry_Array;
         Callback_Data : Data_Type_Access)
      is
         procedure Internal
           (Ifactory      : System.Address;
            N_Entries     : Guint;
            Entries       : System.Address;
            Callback_Data : Data_Type_Access);
         pragma Import (C, Internal, "gtk_item_factory_create_items");

      begin
         Internal (Get_Object (Ifactory),
                   Entries'Length,
                   Entries'Address,
                   Callback_Data);
      end Create_Items;

      ----------
      -- Free --
      ----------

      procedure Free (Ientry : in out Gtk_Item_Factory_Entry) is
         use Interfaces.C.Strings;
      begin
         if Ientry.Item_Type /= Null_Ptr
           and then UTF8_String'(Value (Ientry.Item_Type)) /= "<ImageItem>"
         then
            ICS.Free (Ientry.Extra_Data);
         end if;

         ICS.Free (Ientry.Path);
         ICS.Free (Ientry.Accelerator);
         ICS.Free (Ientry.Item_Type);
      end Free;

      procedure Free (Ientries : in out Gtk_Item_Factory_Entry_Array) is
      begin
         for J in Ientries'Range loop
            Free (Ientries (J));
         end loop;
      end Free;

      -------------
      -- Gtk_New --
      -------------

      function Gtk_New
        (Path            : UTF8_String;
         Accelerator     : String := "";
         Callback        : Gtk_Item_Factory_Callback := null;
         Item_Type       : Item_Type_Enum;
         Callback_Action : Guint := 0) return Gtk_Item_Factory_Entry
      is
         function Item_Type_String (Item_Type : Item_Type_Enum) return String;

         function Item_Type_String
           (Item_Type : Item_Type_Enum) return String is
         begin
            case Item_Type is
               when Title       => return "<Title>";
               when Item        => return "<Item>";
               when Image_Item  => return "<ImageItem>";
               when Stock_Item  => return "<StockItem>";
               when Check_Item  => return "<CheckItem>";
               when Toggle_Item => return "<ToggleItem>";
               when Radio_Item  => return "<RadioItem>";
               when Separator   => return "<Separator>";
               when Tearoff     => return "<Tearoff>";
               when Branch      => return "<Branch>";
               when Last_Branch => return "<LastBranch>";
            end case;
         end Item_Type_String;

      begin
         return Gtk_New (Path, Accelerator, Callback,
           Item_Type_String (Item_Type), Callback_Action);
      end Gtk_New;

      function Gtk_New
        (Path            : UTF8_String;
         Accelerator     : String := "";
         Callback        : Gtk_Item_Factory_Callback := null;
         Item_Type       : String := "";
         Callback_Action : Guint := 0) return Gtk_Item_Factory_Entry
      is
         Ientry : Gtk_Item_Factory_Entry;
      begin
         Ientry.Path := ICS.New_String (Path);

         if Accelerator = "" then
            Ientry.Accelerator := ICS.Null_Ptr;
         else
            Ientry.Accelerator := ICS.New_String (Accelerator);
         end if;

         if Callback = null then
            Ientry.Callback := System.Null_Address;
         else
            Ientry.Callback := Callback.all'Address;
         end if;

         Ientry.Callback_Action := Callback_Action;

         if Item_Type = "" then
            Ientry.Item_Type := ICS.Null_Ptr;
         else
            Ientry.Item_Type := ICS.New_String (Item_Type);
         end if;

         return Ientry;
      end Gtk_New;

      function Gtk_New
        (Path            : UTF8_String;
         Accelerator     : String := "";
         Stock_Id        : String;
         Callback        : Gtk_Item_Factory_Callback := null;
         Callback_Action : Guint := 0) return Gtk_Item_Factory_Entry
      is
         Ientry : Gtk_Item_Factory_Entry;
      begin
         Ientry := Gtk_New
           (Path, Accelerator, Callback, "<StockItem>", Callback_Action);
         Ientry.Extra_Data := ICS.New_String (Stock_Id);
         return Ientry;
      end Gtk_New;

      function Gtk_New
        (Path            : UTF8_String;
         Accelerator     : String := "";
         Callback        : Gtk_Item_Factory_Callback := null;
         Pixbuf          : access Guchar_Array;
         Callback_Action : Guint := 0) return Gtk_Item_Factory_Entry
      is
         Ientry : Gtk_Item_Factory_Entry;

         pragma Warnings (Off);
         --  This UC is safe aliasing-wise, so kill warning
         function To_Chars_Ptr is new
           Ada.Unchecked_Conversion (System.Address, Gtkada.Types.Chars_Ptr);
         pragma Warnings (On);

      begin
         Ientry := Gtk_New
           (Path, Accelerator, Callback, "<ImageItem>", Callback_Action);
         Ientry.Extra_Data  := To_Chars_Ptr (Pixbuf.all'Address);
         return Ientry;
      end Gtk_New;

      ----------------
      -- Popup_Data --
      ----------------

      function Popup_Data
        (Ifactory : access Gtk_Item_Factory_Record'Class)
         return Data_Type_Access
      is
         function Internal (Ifactory : System.Address) return Data_Type_Access;
         pragma Import (C, Internal, "gtk_item_factory_popup_data");

      begin
         return Internal (Get_Object (Ifactory));
      end Popup_Data;

      ----------------------------
      -- Popup_Data_From_Widget --
      ----------------------------

      function Popup_Data_From_Widget
        (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
         return Data_Type_Access
      is
         function Internal (Widget : System.Address) return Data_Type_Access;
         pragma Import
           (C, Internal, "gtk_item_factory_popup_data_from_widget");

      begin
         return Internal (Get_Object (Widget));
      end Popup_Data_From_Widget;

      ---------------------
      -- Popup_With_Data --
      ---------------------

      procedure Popup_With_Data
        (Ifactory     : access Gtk_Item_Factory_Record'Class;
         Popup_Data   : Data_Type_Access;
         Destroy      : System.Address;  --  Gtk_Destroy_Notify ???
         X            : Guint;
         Y            : Guint;
         Mouse_Button : Guint;
         Time         : Guint32)
      is
         procedure Internal
           (Ifactory     : System.Address;
            Popup_Data   : Data_Type_Access;
            Destroy      : System.Address;
            X            : Guint;
            Y            : Guint;
            Mouse_Button : Guint;
            Time         : Guint32);
         pragma Import (C, Internal, "gtk_item_factory_popup_with_data");

      begin
         Internal (Get_Object (Ifactory),
                   Popup_Data,
                   Destroy,
                   X,
                   Y,
                   Mouse_Button,
                   Time);
      end Popup_With_Data;

      ------------------------
      -- Set_Translate_Func --
      ------------------------

      procedure Set_Translate_Func
        (Ifactory : access Gtk_Item_Factory_Record'Class;
         Func     : Gtk_Translate_Func;
         Data     : Data_Type_Access;
         Notify   : System.Address)  --  Gtk_Destroy_Notify ???
      is
         procedure Internal
           (Ifactory : System.Address;
            Func     : Gtk_Translate_Func;  --  ???
            Data     : Data_Type_Access;
            Notify   : System.Address);
         pragma Import (C, Internal, "gtk_item_factory_set_translate_func");

      begin
         Internal (Get_Object (Ifactory), Func, Data, Notify);
      end Set_Translate_Func;

      ---------------
      -- To_Widget --
      ---------------

      function To_Widget
        (Widget : Limited_Widget) return Gtk.Widget.Gtk_Widget is
      begin
         return Convert (System.Address (Widget));
      end To_Widget;

   end Data_Item;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Ifactory       : out Gtk_Item_Factory;
      Container_Type : Gtk_Type;
      Path           : UTF8_String;
      Accel_Group    : Gtk.Accel_Group.Gtk_Accel_Group) is
   begin
      Ifactory := new Gtk_Item_Factory_Record;
      Initialize (Ifactory, Container_Type, Path, Accel_Group);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Ifactory       : access Gtk_Item_Factory_Record'Class;
      Container_Type : Gtk_Type;
      Path           : UTF8_String := "";
      Accel_Group    : Gtk.Accel_Group.Gtk_Accel_Group)
   is
      function Internal
        (Container_Type : Gtk_Type;
         Path           : UTF8_String;
         Accel_Group    : System.Address)
         return System.Address;
      pragma Import (C, Internal, "gtk_item_factory_new");

   begin
      Set_Object
        (Ifactory,
         Internal (Container_Type,
                   Path & ASCII.NUL,
                   Get_Object (Accel_Group)));
   end Initialize;

end Gtk.Item_Factory;
