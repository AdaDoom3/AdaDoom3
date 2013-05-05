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
with Interfaces.C.Strings;       use Interfaces.C.Strings;

package body Gtk.Assistant is
   package body Generic_Assistant_Functions is

      type Data_Type_Access is access Data_Type;

      type Cb_Record is record
         Func      : Page_Func;
         Notify    : Destroy_Notify;
         User_Data : Data_Type_Access;
      end record;
      type Cb_Record_Access is access Cb_Record;

      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Cb_Record_Access);

      function Convert is new Ada.Unchecked_Conversion
        (Cb_Record_Access, System.Address);

      function General_Cb
        (Current_Page : Gint;
         D : System.Address)
      return Gint;
      pragma Convention (C, General_Cb);

      procedure Free_Data (D : System.Address);
      pragma Convention (C, Free_Data);
      --  Callback used to free data associated with Set_Forward_Page_Func

      function General_Cb
        (Current_Page : Gint;
         D : System.Address)
      return Gint
      is
         Data : constant Cb_Record_Access := Convert (D);
      begin
         return Data.Func (Current_Page, Data.User_Data.all);
      end General_Cb;

      procedure Free_Data (D : System.Address) is
         procedure Free is new Ada.Unchecked_Deallocation
           (Cb_Record, Cb_Record_Access);
         procedure Free is new Ada.Unchecked_Deallocation
           (Data_Type, Data_Type_Access);
         Data : Cb_Record_Access := Convert (D);
      begin
         if Data.Notify /= null then
            Data.Notify (Data.User_Data.all);
         end if;
         Free (Data.User_Data);
         Free (Data);
      end Free_Data;

      ---------------------------
      -- Set_Forward_Page_Func --
      ---------------------------

      procedure Set_Forward_Page_Func
        (Assistant : Gtk_Assistant;
         Func      : Page_Func;
         User_Data : Data_Type;
         Destroy   : Destroy_Notify := null)
      is
         procedure Internal
           (Assistant : System.Address;
            Page_Func : System.Address;
            Data      : System.Address;
            Destroy   : System.Address);
         pragma Import (C, Internal, "gtk_assistant_set_forward_page_func");

         D : constant Cb_Record_Access := new Cb_Record'
           (Func      => Func,
            Notify    => Destroy,
            User_Data => new Data_Type'(User_Data));
      begin
         Internal
           (Get_Object (Assistant),
            General_Cb'Address,
            Convert (D),
            Free_Data'Address);
      end Set_Forward_Page_Func;

   end Generic_Assistant_Functions;

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Assistant_Record);
   pragma Unreferenced (Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Assistant : out Gtk_Assistant) is
   begin
      Assistant := new Gtk_Assistant_Record;
      Gtk.Assistant.Initialize (Assistant);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Assistant : access Gtk_Assistant_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_assistant_new");
   begin
      Set_Object (Assistant, Internal);
   end Initialize;

   -----------------------
   -- Add_Action_Widget --
   -----------------------

   procedure Add_Action_Widget
      (Assistant : access Gtk_Assistant_Record;
       Child     : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Assistant : System.Address;
          Child     : System.Address);
      pragma Import (C, Internal, "gtk_assistant_add_action_widget");
   begin
      Internal (Get_Object (Assistant), Get_Object (Child));
   end Add_Action_Widget;

   -----------------
   -- Append_Page --
   -----------------

   function Append_Page
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class) return Gint
   is
      function Internal
         (Assistant : System.Address;
          Page      : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_assistant_append_page");
   begin
      return Internal (Get_Object (Assistant), Get_Object (Page));
   end Append_Page;

   ------------
   -- Commit --
   ------------

   procedure Commit (Assistant : access Gtk_Assistant_Record) is
      procedure Internal (Assistant : System.Address);
      pragma Import (C, Internal, "gtk_assistant_commit");
   begin
      Internal (Get_Object (Assistant));
   end Commit;

   ----------------------
   -- Get_Current_Page --
   ----------------------

   function Get_Current_Page
      (Assistant : access Gtk_Assistant_Record) return Gint
   is
      function Internal (Assistant : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_assistant_get_current_page");
   begin
      return Internal (Get_Object (Assistant));
   end Get_Current_Page;

   -----------------
   -- Get_N_Pages --
   -----------------

   function Get_N_Pages
      (Assistant : access Gtk_Assistant_Record) return Gint
   is
      function Internal (Assistant : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_assistant_get_n_pages");
   begin
      return Internal (Get_Object (Assistant));
   end Get_N_Pages;

   ------------------
   -- Get_Nth_Page --
   ------------------

   function Get_Nth_Page
      (Assistant : access Gtk_Assistant_Record;
       Page_Num  : Gint) return Gtk.Widget.Gtk_Widget
   is
      function Internal
         (Assistant : System.Address;
          Page_Num  : Gint) return System.Address;
      pragma Import (C, Internal, "gtk_assistant_get_nth_page");
      Stub : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Assistant), Page_Num), Stub));
   end Get_Nth_Page;

   -----------------------
   -- Get_Page_Complete --
   -----------------------

   function Get_Page_Complete
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class) return Boolean
   is
      function Internal
         (Assistant : System.Address;
          Page      : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_assistant_get_page_complete");
   begin
      return Boolean'Val (Internal (Get_Object (Assistant), Get_Object (Page)));
   end Get_Page_Complete;

   ---------------------------
   -- Get_Page_Header_Image --
   ---------------------------

   function Get_Page_Header_Image
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal
         (Assistant : System.Address;
          Page      : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_assistant_get_page_header_image");
      Stub : Gdk.Pixbuf.Gdk_Pixbuf_Record;
   begin
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Internal (Get_Object (Assistant), Get_Object (Page)), Stub));
   end Get_Page_Header_Image;

   -------------------------
   -- Get_Page_Side_Image --
   -------------------------

   function Get_Page_Side_Image
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal
         (Assistant : System.Address;
          Page      : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_assistant_get_page_side_image");
      Stub : Gdk.Pixbuf.Gdk_Pixbuf_Record;
   begin
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Internal (Get_Object (Assistant), Get_Object (Page)), Stub));
   end Get_Page_Side_Image;

   --------------------
   -- Get_Page_Title --
   --------------------

   function Get_Page_Title
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class)
       return UTF8_String
   is
      function Internal
         (Assistant : System.Address;
          Page      : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_assistant_get_page_title");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Assistant), Get_Object (Page)));
   end Get_Page_Title;

   -------------------
   -- Get_Page_Type --
   -------------------

   function Get_Page_Type
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gtk_Assistant_Page_Type
   is
      function Internal
         (Assistant : System.Address;
          Page      : System.Address) return Gtk_Assistant_Page_Type;
      pragma Import (C, Internal, "gtk_assistant_get_page_type");
   begin
      return Internal (Get_Object (Assistant), Get_Object (Page));
   end Get_Page_Type;

   -----------------
   -- Insert_Page --
   -----------------

   function Insert_Page
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class;
       Position  : Gint) return Gint
   is
      function Internal
         (Assistant : System.Address;
          Page      : System.Address;
          Position  : Gint) return Gint;
      pragma Import (C, Internal, "gtk_assistant_insert_page");
   begin
      return Internal (Get_Object (Assistant), Get_Object (Page), Position);
   end Insert_Page;

   ------------------
   -- Prepend_Page --
   ------------------

   function Prepend_Page
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class) return Gint
   is
      function Internal
         (Assistant : System.Address;
          Page      : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_assistant_prepend_page");
   begin
      return Internal (Get_Object (Assistant), Get_Object (Page));
   end Prepend_Page;

   --------------------------
   -- Remove_Action_Widget --
   --------------------------

   procedure Remove_Action_Widget
      (Assistant : access Gtk_Assistant_Record;
       Child     : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Assistant : System.Address;
          Child     : System.Address);
      pragma Import (C, Internal, "gtk_assistant_remove_action_widget");
   begin
      Internal (Get_Object (Assistant), Get_Object (Child));
   end Remove_Action_Widget;

   ----------------------
   -- Set_Current_Page --
   ----------------------

   procedure Set_Current_Page
      (Assistant : access Gtk_Assistant_Record;
       Page_Num  : Gint)
   is
      procedure Internal (Assistant : System.Address; Page_Num : Gint);
      pragma Import (C, Internal, "gtk_assistant_set_current_page");
   begin
      Internal (Get_Object (Assistant), Page_Num);
   end Set_Current_Page;

   ---------------------------
   -- Set_Forward_Page_Func --
   ---------------------------

   procedure Set_Forward_Page_Func
      (Assistant : access Gtk_Assistant_Record;
       Page_Func : Gtk_Assistant_Page_Func;
       Data      : System.Address;
       Destroy   : Glib.G_Destroy_Notify_Address)
   is
      procedure Internal
         (Assistant : System.Address;
          Page_Func : Gtk_Assistant_Page_Func;
          Data      : System.Address;
          Destroy   : Glib.G_Destroy_Notify_Address);
      pragma Import (C, Internal, "gtk_assistant_set_forward_page_func");
   begin
      Internal (Get_Object (Assistant), Page_Func, Data, Destroy);
   end Set_Forward_Page_Func;

   -----------------------
   -- Set_Page_Complete --
   -----------------------

   procedure Set_Page_Complete
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class;
       Complete  : Boolean)
   is
      procedure Internal
         (Assistant : System.Address;
          Page      : System.Address;
          Complete  : Integer);
      pragma Import (C, Internal, "gtk_assistant_set_page_complete");
   begin
      Internal (Get_Object (Assistant), Get_Object (Page), Boolean'Pos (Complete));
   end Set_Page_Complete;

   ---------------------------
   -- Set_Page_Header_Image --
   ---------------------------

   procedure Set_Page_Header_Image
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class;
       Pixbuf    : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
   is
      procedure Internal
         (Assistant : System.Address;
          Page      : System.Address;
          Pixbuf    : System.Address);
      pragma Import (C, Internal, "gtk_assistant_set_page_header_image");
   begin
      Internal (Get_Object (Assistant), Get_Object (Page), Get_Object (Pixbuf));
   end Set_Page_Header_Image;

   -------------------------
   -- Set_Page_Side_Image --
   -------------------------

   procedure Set_Page_Side_Image
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class;
       Pixbuf    : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
   is
      procedure Internal
         (Assistant : System.Address;
          Page      : System.Address;
          Pixbuf    : System.Address);
      pragma Import (C, Internal, "gtk_assistant_set_page_side_image");
   begin
      Internal (Get_Object (Assistant), Get_Object (Page), Get_Object (Pixbuf));
   end Set_Page_Side_Image;

   --------------------
   -- Set_Page_Title --
   --------------------

   procedure Set_Page_Title
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class;
       Title     : UTF8_String)
   is
      procedure Internal
         (Assistant : System.Address;
          Page      : System.Address;
          Title     : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_assistant_set_page_title");
      Tmp_Title : Interfaces.C.Strings.chars_ptr := New_String (Title);
   begin
      Internal (Get_Object (Assistant), Get_Object (Page), Tmp_Title);
      Free (Tmp_Title);
   end Set_Page_Title;

   -------------------
   -- Set_Page_Type --
   -------------------

   procedure Set_Page_Type
      (Assistant : access Gtk_Assistant_Record;
       Page      : access Gtk.Widget.Gtk_Widget_Record'Class;
       The_Type  : Gtk_Assistant_Page_Type)
   is
      procedure Internal
         (Assistant : System.Address;
          Page      : System.Address;
          The_Type  : Gtk_Assistant_Page_Type);
      pragma Import (C, Internal, "gtk_assistant_set_page_type");
   begin
      Internal (Get_Object (Assistant), Get_Object (Page), The_Type);
   end Set_Page_Type;

   --------------------------
   -- Update_Buttons_State --
   --------------------------

   procedure Update_Buttons_State (Assistant : access Gtk_Assistant_Record) is
      procedure Internal (Assistant : System.Address);
      pragma Import (C, Internal, "gtk_assistant_update_buttons_state");
   begin
      Internal (Get_Object (Assistant));
   end Update_Buttons_State;

end Gtk.Assistant;
