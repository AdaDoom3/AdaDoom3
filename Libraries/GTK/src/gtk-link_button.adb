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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Interfaces.C.Strings;       use Interfaces.C.Strings;
with System;

package body Gtk.Link_Button is

   -------------
   -- Get_Uri --
   -------------

   function Get_Uri
     (Link_Button : access Gtk_Link_Button_Record) return String
   is
      function Internal
        (Link_Button : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_link_button_get_uri");

   begin
      return Value (Internal (Get_Object (Link_Button)));
   end Get_Uri;

   -----------------
   -- Get_Visited --
   -----------------

   function Get_Visited
     (Link_Button : access Gtk_Link_Button_Record) return Boolean
   is
      function Internal
        (Link_Button : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_link_button_get_visited");

   begin
      return Boolean'Val (Internal (Get_Object (Link_Button)));
   end Get_Visited;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Link_Button; Uri : String) is
   begin
      Widget := new Gtk_Link_Button_Record;
      Initialize (Widget, Uri);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget : access Gtk_Link_Button_Record'Class;
      Uri    : String)
   is
      function Internal (Uri : String) return System.Address;
      pragma Import (C, Internal, "gtk_link_button_new");

   begin
      Set_Object (Widget, Internal (Uri & ASCII.NUL));
   end Initialize;

   ------------------------
   -- Gtk_New_With_Label --
   ------------------------

   procedure Gtk_New_With_Label
     (Widget : out Gtk_Link_Button;
      Uri    : String;
      Label  : String)
   is
   begin
      Widget := new Gtk_Link_Button_Record;
      Initialize_With_Label (Widget, Uri, Label);
   end Gtk_New_With_Label;

   ---------------------------
   -- Initialize_With_Label --
   ---------------------------

   procedure Initialize_With_Label
     (Widget : access Gtk_Link_Button_Record'Class;
      Uri    : String;
      Label  : String)
   is
      function Internal
        (Uri : String; Label : String) return System.Address;
      pragma Import (C, Internal, "gtk_link_button_new_with_label");

   begin
      Set_Object (Widget, Internal (Uri & ASCII.NUL, Label & ASCII.NUL));
   end Initialize_With_Label;

   -------------
   -- Set_Uri --
   -------------

   procedure Set_Uri
     (Link_Button : access Gtk_Link_Button_Record;
      Uri : String)
   is
      procedure Internal (Link_Button : System.Address; Uri : String);
      pragma Import (C, Internal, "gtk_link_button_set_uri");

   begin
      Internal (Get_Object (Link_Button), Uri & ASCII.NUL);
   end Set_Uri;

   ------------------
   -- Set_Uri_Hook --
   ------------------

   function Set_Uri_Hook
     (Func    : Uri_Func;
      Data    : System.Address;
      Destroy : G_Destroy_Notify)
      return Uri_Func
   is
      function Internal
        (Func    : Uri_Func;
         Data    : System.Address;
         Destroy : G_Destroy_Notify)
         return Uri_Func;
      pragma Import (C, Internal, "gtk_link_button_set_uri_hook");

   begin
      return Internal (Func, Data, Destroy);
   end Set_Uri_Hook;

   -----------------
   -- Set_Visited --
   -----------------

   procedure Set_Visited
     (Link_Button : access Gtk_Link_Button_Record;
      Visited     : Boolean)
   is
      procedure Internal (Link_Button : System.Address; Visited : Gboolean);
      pragma Import (C, Internal, "gtk_link_button_set_visited");

   begin
      Internal (Get_Object (Link_Button), Boolean'Pos (Visited));
   end Set_Visited;

   ----------------------
   -- Generic_Uri_Hook --
   ----------------------

   package body Generic_Uri_Hook is

      type Data_Type_Access is access Data_Type;

      type Cb_Record is record
         Handler   : Uri_Handler;
         Notify    : Destroy_Notify;
         User_Data : Data_Type_Access;
      end record;
      type Cb_Record_Access is access Cb_Record;

      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Cb_Record_Access);

      function Convert is new Ada.Unchecked_Conversion
        (Cb_Record_Access, System.Address);

      procedure Free_Data (D : System.Address);
      --  Proxy subprogram to be passed as a G_Destroy_Notify when we invoke
      --  gtk_link_button_set_uri_hook() in Set_Uri_Hook.

      procedure General_Cb
        (B : System.Address;
         L : Interfaces.C.Strings.chars_ptr;
         D : System.Address);
      pragma Convention (C, General_Cb);
      --  Wrapper for user callback.

      ---------------
      -- Free_Data --
      ---------------

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

      ----------------
      -- General_Cb --
      ----------------

      procedure General_Cb
        (B : System.Address;
         L : Interfaces.C.Strings.chars_ptr;
         D : System.Address)
      is
         Stub   : Gtk_Link_Button_Record;
         Button : constant Gtk_Link_Button :=
                    Gtk_Link_Button (Get_User_Data (B, Stub));
         Link   : constant String := Interfaces.C.Strings.Value (L);
         Data   : constant Cb_Record_Access := Convert (D);
      begin
         Data.Handler (Button, Link, Data.User_Data.all);
      end General_Cb;

      ------------------
      -- Set_Uri_Hook --
      ------------------

      procedure Set_Uri_Hook
        (Handler   : Uri_Handler;
         User_Data : Data_Type;
         Destroy   : Destroy_Notify)
      is
         function Internal
           (Func    : System.Address;
            Data    : System.Address;
            Destroy : System.Address)
            return Uri_Func;
         pragma Import (C, Internal, "gtk_link_button_set_uri_hook");

         D : constant Cb_Record_Access := new Cb_Record'
           (Handler   => Handler,
            Notify    => Destroy,
            User_Data => new Data_Type'(User_Data));

         Old_Handler : Uri_Func;
         pragma Warnings (Off, Old_Handler);
      begin
         Old_Handler := Internal
           (General_Cb'Address, Convert (D), Free_Data'Address);
      end Set_Uri_Hook;

   end Generic_Uri_Hook;

end Gtk.Link_Button;
